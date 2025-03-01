#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 11:55:53 2023

@author: emilio

This code has the necessary functions to run the whole model. 
"""

import numpy as np  ##numpy paquete que acelera todo el manejo de matrices y demás
import random
import pandas as pd
import math as m

"""
Set scenario

Receives:
    - initial arrangement of plants
    - the number of the repetition to create the same seed. (dic_Simulation["rep_"])
    - the density (dic_Lattice["num_Plants"])
    - the fruit charge (dic_Harvest[["hl_Plants"]])
    -initial infection (dic_Lattice["ini_Inf"]])
    
generates:
    a inital data frame, with the initial lattice in step 1:
        pd.DataFrame({"ID": IDplants, "X": X, "Y": Y,
                      "Rust": Rust, "WorkerID": WorkerID,
                      "HarvestStep":HarvestStep, 
                      "FruitLoad":FruitLoad, 
                      "TotalHarvest": TotalHarvest})
        
calls: NA
"""

def setScenario(dic_Lattice, dic_Simulation, dic_Harvest):
    if dic_Lattice["mode_Arr"] == "random":  #so if the model is random
        
    
        rstate = np.random.RandomState(dic_Simulation["rep_"])  #para cada repeticion mismo seed
        
    #harvesting conditions (will be filled or modified during simulation)
        WorkerID = np.repeat(0, dic_Lattice["num_Plants"])
        HarvestStep = np.repeat(0, dic_Lattice["num_Plants"])
        TotalHarvest = np.repeat(0, dic_Lattice["num_Plants"])
        
    #fruit load (will change in simulation)  #THIS IS DIFFERENTE
        hlPlants = dic_Harvest["hl_Plants"]
        nlPlants = dic_Lattice["num_Plants"]- hlPlants
        F2 = np.repeat(1, hlPlants)  #NO HAY 2, no hay productivity odel 
        F1 = np.repeat(0, nlPlants)
        FruitLoad = np.concatenate((F2, F1), axis= None)
        rstate.shuffle(FruitLoad)
        
       # print(FruitLoad)
        
     # plants and coordinae   
        IDplants = np.arange(0, dic_Lattice["num_Plants"], 1)  #add each ID 
    
        X = rstate.random_sample(dic_Lattice["num_Plants"])*dic_Lattice["dim_Ini"][0]
        Y = rstate.random_sample(dic_Lattice["num_Plants"])*dic_Lattice["dim_Ini"][1]

        
        
    #initial infection
        iniInfected = [round(i*dic_Lattice["num_Plants"]) for i in dic_Lattice["ini_Inf"]] #add the number of infected plants we round it up, so sometime we can have an aditional plant per situation of infection. But, that is why is better to put values of 10.
        L = np.repeat(0.5, iniInfected[1])
        I = np.repeat(1, iniInfected[2])
        numS = dic_Lattice["num_Plants"]- iniInfected[1] - iniInfected[2]
        S = np.repeat(0, numS) #so, depending on the initial value, we can have 2 S less plants, beacuse L and I wre reounded up to the unit
        Rust = np.concatenate((S, L, I), axis= None)
       # random.shuffle(Rust) creo que es inece

## y se junta todo, loque no es temporal, ya veremos si es necesario juntar el modelo desde aqui o desde fuera
        
        initialLattice = pd.DataFrame({"ID": IDplants, "X": X, "Y": Y,"Rust": Rust, "WorkerID": WorkerID, "HarvestStep":HarvestStep, "FruitLoad":FruitLoad, "TotalHarvest": TotalHarvest})
    else:
        pass
    
    return(initialLattice)


"""
generaldynamic

Receives:
    - dicLattice, dicSimulation, dicHarvest) (all the initial conditions9)
    
generates:
    - a general data frame that will store all timesteps dataframes
    pd.DataFrame(columns= ["ID", "X", "Y", 
                           "Rust", "WorkerID", "HarvestStep","FruitLoad", 
                           "TotalHarvest", "DistanceW", 
                           "Time"])

calls:
    - setScenario 
    loop per time step:
    - HM_general
    - contactModel
    
"""

def generalDynamic(dic_Lattice, dic_Simulation, dic_Harvest):
    
    generalDF = pd.DataFrame(columns= ["ID", "X", "Y", "Rust", "WorkerID", "HarvestStep","FruitLoad", "TotalHarvest", "DistanceW", "Time"]) #we create a general data frame thatwill store the whole dynamic. Here we have the time colu
    T = 0 #we set time to 0  
    
    initialDF= setScenario(dic_Lattice, dic_Simulation, dic_Harvest) #this creates the first sectio of the data frame
    temporalDF= initialDF.copy() #we copy it before including the Time (we willuse it dinamically)
    
    #we inclue the first T and add it to the general DF
    Ti = np.repeat(T, dic_Lattice["num_Plants"])
    initialDF["Time"] =Ti  #we add it 
    
    generalDF= pd.concat([generalDF, initialDF])
    
    tiempoCosecha = dic_Harvest["time_Harvest"]
    
    #whole loop simlation
    while T< dic_Simulation["T_max"]:
        print("Time", T)
        tau = 0
        while tau<1:
        
            
            if dic_Harvest["har_vest"] == "control":
                pass
            else:
                if T == tiempoCosecha:
                    newDF= HM_general(temporalDF, dic_Harvest, dic_Simulation)
                    temporalDF= newDF.copy()
                else:
                    pass

        
        #aplica el modulo contact
            
            newDF = contactModel(temporalDF, dic_Simulation) #we create next step
            temporalDF = newDF.copy() #now we set it to the old step 
            
            # we actualize the general dataframe
            tau = tau + 0.5
            T = T+ 0.5
            Tli = np.repeat(T, dic_Lattice["num_Plants"])
            newDF["Time"] = Tli
            generalDF= pd.concat([generalDF, newDF])
            
        
    return(generalDF)



"""
contactModel

Receives:
    - old_data frame (a time t)
    - the contact distance {dic_Simulation["contactDistance"]}
    
generates:
    - new data frame updated with the contact infectin (a time t+1)

calls:
 NA
    
"""



def contactModel(old_DF, dic_Simulation): #r_h is ruested trees durign harvest
    tempDF = old_DF 
    
    tempDF.loc[tempDF["Rust"] ==0.5, "Rust"] = 0.75 #this will save these data separately before infection 
    
     #now for the local infection
    IT = tempDF.loc[tempDF["Rust"] == 1]#this is a view
    ST = tempDF.loc[tempDF["Rust"] == 0] #this is a view
    maxDistance = dic_Simulation["contactDistance"]**2 #lo pongo así para no usar raices cuadrasd
    LC_total = pd.DataFrame(columns= ["ID", "X", "Y", "Rust", "WorkerID", "HarvestStep", "FruitLoad", "TotalHarvest", "DistanceW"]) #dataFRramevacio
    
    for row in range(1, len(IT)):
        
        LC = ST.copy()  #this takes the susceptibles at the moment and makes copy (rusted by contact)
        LC["Distance"] = (LC["X"] - IT.iloc[row]["X"])**2  + (LC["Y"] - IT.iloc[row]["Y"])**2 #this calculat distance between the each infected plants and all suceptibles
        LC = LC.loc[LC["Distance"]<maxDistance] #tis filters only the neighburs
        if len(LC) >0:
            LC.drop(columns= ["Distance"]) #quitarla para no tener una de más
            #print(RC)
            LC_total = pd.concat([LC_total, LC])
        else:
            pass

        LC_total.loc[LC_total["Rust"] ==0, "Rust"] = 0.5        
        
    #now the general actualization (this sets should no overlpa)
   
    tempDF.loc[tempDF.ID.isin(LC_total.ID), ["Rust"]] = 0.5 #esta se puede traslapar con la de arriba, pero no hay pedp
    
    
    tempDF.loc[tempDF["Rust"] ==0.25, "Rust"] = 0.5 #rusted during harvest
    
    tempDF.loc[tempDF["Rust"] ==0.75, "Rust"] = 1 # already infecred at the beignning

    
    return(tempDF)


"""
HM_general (harvesting model)

Receives:
    - old_data frame (a time t)
    - the numberof worjers and steps (dic_Harvest["harvest_Steps"], dic_Harvest["num_wrkers"])
    - the repetirion for the seed (dic_Simulation["rep_"])
    
generates:
    - new data frame updated with the harvst infectin (a time t+1)
calls:
    -NA
"""



def HM_general(old_DF, dic_Harvest, dic_Simulation):
    
    tempDF = old_DF
    hSteps= dic_Harvest["harvest_Steps"]-1 #hago el -1 para que sean n pasos contando el 0
    
    numW = dic_Harvest["num_Workers"]
    
    modeloH = dic_Harvest["model_harvest"]
    
    
    UN_HARV = tempDF.loc[tempDF["FruitLoad"] != 0]  ##IN both scenario, we star at a random plant, to have the same random stat
    
        
      
    
    rstate = np.random.RandomState(dic_Simulation["rep_"])  #para cada repeticion mismo seed

    
    initialPlants = rstate.choice(list(UN_HARV["ID"]), numW, replace= False)  ###ESTE ES EL PEDO!
    
    #print("iniPans", initialPlants)
    liWorkers = []
        
    for w in np.arange(0,numW,1):
        liWorkers.append("W_%d" % (w))
        tempDF.loc[tempDF["ID"] == initialPlants[w], "TotalHarvest"] = tempDF.loc[tempDF["ID"] == initialPlants[w], "FruitLoad"]
        tempDF.loc[tempDF["ID"] == initialPlants[w], "FruitLoad"] = 0
        tempDF.loc[tempDF["ID"] == initialPlants[w], "WorkerID"] = "W_%d" % (w)
        tempDF.loc[tempDF["ID"] == initialPlants[w], "HarvestStep"] = 1
        tempDF.loc[tempDF["ID"] == initialPlants[w], "DistanceW"] = 0
        
        

    #print("liW", liWorkers)   
    conteo = 0  #CUANDO CA;BIP ESTO NO FUCNIONA
    #print ("hSteps", hSteps)
    while conteo<hSteps:
        conteo= conteo +1
       # print("conteoPasos", conteo)
        for w in np.arange(0, numW,1):
            #print("Worker", w)
            
            #print("TEMPSTEP", tempDF["HarvestStep"].unique())
            #print("TEMPwOrker", tempDF["WorkerID"].unique())
            
            LAST_W = tempDF.loc[(tempDF["HarvestStep"] == conteo) & (tempDF["WorkerID"] == liWorkers[w])] #esto filtra solo los ultimos pasos, que deben tener 3 trabajadores
            
            royaOrigen = LAST_W.iloc[0]["Rust"]
            
            #print("lastw \n", LAST_W)
            #print("royaOrigen \n", royaOrigen)
            
            if dic_Harvest["har_vest"] == "closeness":
                UH_DIN = tempDF.loc[tempDF["FruitLoad"] != 0].copy()#this is a cooy
            elif dic_Harvest["har_vest"] == "productivity":
                UH_DIN = tempDF.loc[tempDF["FruitLoad"] == 2].copy() #this is a copy
            
            UH_DIN["Distance"] = (UH_DIN["X"] -LAST_W.iloc[0]["X"])**2  + (UH_DIN["Y"] - LAST_W.iloc[0]["Y"])**2
            UH_DIN= UH_DIN.loc[UH_DIN["Distance"] == min(UH_DIN["Distance"])]

          #  print("UHDIN", UH_DIN)
           # print("UHDINiloc", UH_DIN.iloc[0])
           # print("UHDINilocDisn", UH_DIN.iloc[0]["Distance"])
            
            royaDestino = UH_DIN.iloc[0]["Rust"]
 #           print("w", w, "UHDIN \n", UH_DIN)

            conteoTemp = conteo+1
    
                        
            tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["TotalHarvest"]] = UH_DIN.iloc[0]["FruitLoad"]  
            tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["FruitLoad"]] = 0
            tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["WorkerID"]] = "W_%d" % (w)  
            tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["HarvestStep"]] = conteoTemp 
            tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["DistanceW"]] = UH_DIN.iloc[0]["Distance"]  
            
            #esta es la linea para diferenciar los modelos
            
            
            infectar= 0
            
            if modeloH == "S_I":
                if conteoTemp <(hSteps/2):
                    infectar = 1
            elif modeloH == "S_F":
                if conteoTemp >(hSteps/2):
                    infectar = 1
            elif modeloH == "A":
                infectar =1
            
                
            if infectar ==1: #para que solo infecte después de haber cosechado la mitad (solo quiero ver el asincronico)
            
                if royaDestino == 0:  #esto para asegurar que no fuera una planta infectada (0.75 o 1 o 0.5)
                    if royaOrigen == 1:
                        tempDF.loc[tempDF.ID.isin(UH_DIN.ID), ["Rust"]] = 0.25 
                    else:
                        pass
                else:
                    pass
            else:
                pass
    return(tempDF)

    
"""
actualizeHarves

Receives:
    - old_data frame (a time t)

    
generates:
    - update the harvest status of each plant
    - new data frame (tim +1)
calls:
    -NA
"""





    
def actualizeHarvest(old_DF):
    tempDF = old_DF
    tempDF.loc[tempDF["FruitLoad"] == 1, "FruitLoad"] = 11 #truqiito para que no se sobreesciban
    tempDF.loc[tempDF["FruitLoad"] == 2, "FruitLoad"] = 1  
    tempDF.loc[tempDF["FruitLoad"] == 11, "FruitLoad"] = 2
    
    return(tempDF)

