#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 11:55:53 2023

@author: emilio
"""

import numpy as np  ##numpy paquete que acelera todo el manejo de matrices y demás
import random
import pandas as pd
import math as m


def setScenario(dic_Lattice, dic_Simulation, dic_Harvest):
    if dic_Lattice["mode_Arr"] == "random":  #so if the model is random
        HarvestModel = np.repeat(0, dic_Lattice["num_Plants"])
        #Jump = np.repeat(dic_Simulation["jumps_"], dic_Lattice["num_Plants"])#add initial value of jump for each ID of the plant
        Rep = np.repeat(dic_Simulation["rep_"], dic_Lattice["num_Plants"]) #add initial value of rep for each ID of the plant
        
        IDplants = np.arange(0, dic_Lattice["num_Plants"], 1)  #add each ID 
        X = np.random.randint(dic_Lattice["dim_Ini"][0], size= dic_Lattice["num_Plants"])  #add random coordinates
        Y = np.random.randint(dic_Lattice["dim_Ini"][1], size= dic_Lattice["num_Plants"])
        
        hlPlants = dic_Harvest["hl_Plants"]
        llPlants = dic_Lattice["num_Plants"]- hlPlants
        H2 = np.repeat(2, hlPlants)
        H1 = np.repeat(1, llPlants)
        Harvest = np.concatenate((H2, H1), axis= None)
        random.shuffle(Harvest) #important to randomize 
        iniInfected = [round(i*dic_Lattice["num_Plants"]) for i in dic_Lattice["ini_Inf"]] #add the number of infected plants we round it up, so sometime we can have an aditional plant per situation of infection. But, that is why is better to put values of 10.
        L = np.repeat(0.5, iniInfected[1])
        I = np.repeat(1, iniInfected[2])
        numS = dic_Lattice["num_Plants"]- iniInfected[1] - iniInfected[2]
        S = np.repeat(0, numS) #so, depending on the initial value, we can have 2 S less plants, beacuse L and I wre reounded up to the unit
        Rust = np.concatenate((S, L, I), axis= None)
       # Tcero= np.repeat(0, dic_Lattice["num_Plants"])  #esto es solo para crear la columna, se va a reemplrazar
        print("S", numS)
        initialLattice = pd.DataFrame({"HarvestModel": HarvestModel, "Rep": Rep, "ID": IDplants, "X": X, "Y": Y, "Harvest":Harvest, "Rust": Rust})
    else:
        pass
    
    return(initialLattice)


"""
This function generates all the dynamic
"""

def generalDynamic(dic_Lattice, dic_Simulation, dic_Harvest):
    
    generalDF = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust", "Time"]) #we create a general data frame thatwill store the whole dynamic
    T = 0 #we set time to 0
    
    initialDF= setScenario(dic_Lattice, dic_Simulation, dic_Harvest) #this creates the first sectio of the data frame
    print("initial", initialDF)
    temporalDF= initialDF.copy() #we copy it before including the Time (we willuse it dinamically)
    
    #we inclue the first T and add it to the general DF
    Ti = np.repeat(T, dic_Lattice["num_Plants"])
    initialDF["Time"] =Ti
    generalDF= pd.concat([generalDF, initialDF])
    
    #now w add a empty dataframe or infected plants during harvest just to fill it later
    #RH = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust"]) #dataFRramevacio
    
    
    #whole loop simlation
    while T< dic_Simulation["Tmax"]:
        tau = 0
        while tau<1:
        
            if dic_Simulation["har_vest"] == True:
                pass #por lo pront 
                # se regresa aqui u R_Hno vacio debe regresar un temporal con la asignacion 0.25 para lo contagiado durante cosecha
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


def contactModel(old_DF, dic_Simulation): #r_h is ruested trees durign harvest
    tempDF = old_DF 
    #print("tempDF",tempDF)
    
    tempDF.loc[tempDF["Rust"] ==0.5, "Rust"] = 0.75 #this will save these data separately before infection 
    
     #now for the local infection
    IT = tempDF.loc[tempDF["Rust"] == 1]#this is a view
    ST = tempDF.loc[tempDF["Rust"] == 0] #this is a view
    maxDistance = dic_Simulation["contactDistance"]**2 #lo pongo así para no usar raices cuadrasd
    LC_total = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust", "T"]) #dataFRramevacio
    
    for row in range(1, len(IT)):
        
        LC = ST.copy()  #this takes the susceptibles at the moment and makes copy (rusted by contact)
        LC["Distance"] = (LC["X"] - IT.iloc[row]["X"])**2  + (LC["Y"] - IT.iloc[row]["Y"])**2 #this calculat distance between the each infected plants and all suceptibles
        LC = LC.loc[LC["Distance"]<maxDistance] #tis filters only the neighburs
        #print(RC)
        if len(LC) >0:
            LC.drop(columns= ["Distance"]) #quitarla para no tener una de más
            #print(RC)
            LC_total = pd.concat([LC_total, LC])
        else:
            pass

        LC_total.loc[LC_total["Rust"] ==0, "Rust"] = 0.5        
        
    #now the general actualization (this sets should no overlpa)
    
    #print("tempDF", tempDF)
    #print("ID_new_rusted", RC_total["ID"])
    tempDF.loc[tempDF.ID.isin(LC_total.ID), ["Rust"]] = 0.5 #esta se puede traslapar con la de arriba, pero no hay pedp
    
    
    tempDF.loc[tempDF["Rust"] ==0.25, "Rust"] = 0.5 #rusted during harvest
    
    tempDF.loc[tempDF["Rust"] ==0.75, "Rust"] = 1 # already infecred at the beignning

    
    return(tempDF)

    
# def HM_closeness(old_DF, dic_Harvest):
#     tempDF = old_DF
    

# def HM_maxPro(old_DF, haresDI*):
#  #   tempDF = old_DF 
    
        
        
#################################


    

#if __name__ == "__main__": 
