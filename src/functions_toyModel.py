#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 11:55:53 2023

@author: emilio
"""

import numpy as np  ##numpy paquete que acelera todo el manejo de matrices y demás
import random as rd
import pandas as pd
import math as m


def setScenario(dic_Lattice, dic_Simulation):
    if dic_Lattice["mode_Arr"] == "random":
        
        Jump = np.repeat(dic_Simulation["jumps_"], dic_Lattice["num_Plants"])
        Rep = np.repeat(dic_Simulation["rep_"], dic_Lattice["num_Plants"])
        
        IDplants = np.arange(0, dic_Lattice["num_Plants"], 1)
        X = np.random.randint(dic_Lattice["dim_Ini"][0], size= dic_Lattice["num_Plants"])
        Y = np.random.randint(dic_Lattice["dim_Ini"][1], size= dic_Lattice["num_Plants"])
        Harvest = np.repeat(0, dic_Lattice["num_Plants"])
        
        iniInfected = [i*dic_Lattice["num_Plants"] for i in dic_Lattice["ini_Inf"]]
        S = np.repeat(0, iniInfected[0])
        L = np.repeat(0.5, iniInfected[1])
        I = np.repeat(1, iniInfected[2])
        Rust = np.concatenate((S, L, I), axis= None)
       # Tcero= np.repeat(0, dic_Lattice["num_Plants"])  #esto es solo para crear la columna, se va a reemplrazar
        
        
        initialLattice = pd.DataFrame({"Jump": Jump, "Rep": Rep, "ID": IDplants, "X": X, "Y": Y, "Harvest":Harvest, "Rust": Rust})
    else:
        pass
    
    return(initialLattice)


def generalDynamic(dic_Lattice, dic_Simulation, dic_Workers):
    
    generalDF = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust", "T"])
    T = 0
    
    initialDF= setScenario(dic_Lattice, dic_Simulation)
    temporalDF= initialDF.copy()
    
    Ti = np.repeat(T, dicLattice["num_Plants"])
    initialDF["T"] =Ti
    generalDF= pd.concat([generalDF, initialDF])
    
    RH = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust"]) #dataFRramevacio
    
    
    while T< dic_Simulation["Tmax"]:
        print("time1", T)
        tau = 0
        while tau<1:
        
            if dic_Simulation["har_vest"] == True:
                pass #por lo pront 
                # se regresa aqui u R_Hno vacio
            else:
                pass
        
        #aplica el modulo contact
            tau = tau + 0.5
            T = T+ 0.5
            
            #oldDF = temporalDF.tail(dicLattice["num_Plants"]).copy()
            
            newDF = contactModel(temporalDF, dic_Simulation, RH)
            
            Tli = np.repeat(T, dicLattice["num_Plants"])
            newDF["T"] = Tli
            generalDF= pd.concat([generalDF, newDF])
            

            temporalDF = newDF.drop("T", axis=1)
            
        
    return(generalDF)


def contactModel(old_DF, dic_Simulation,  R_H): #r_h is ruested trees durign harvest
    tempDF = old_DF 
    print("tempDF",tempDF)
    LT_temp = tempDF.loc[tempDF["Rust"] == 0.5].copy()  #https://towardsdatascience.com/explaining-the-settingwithcopywarning-in-pandas-ebc19d799d25
    LT_temp.loc[LT_temp["Rust"] ==0.5, "Rust"] = 1  #this creates a data frame where L --> I
    
    #we actualize the harvested to be infected

    R_H.loc[R_H["Rust"] ==0, "Rust"] = 0.5  #this takes the values of RH, that will be among S, and --->L
    
    #now for the local infection
    RT = tempDF.loc[tempDF["Rust"] == 1]
    ST = tempDF.loc[tempDF["Rust"] == 0]
    maxDistance = dic_Simulation["contactDistance"]**2 #lo pongo así para no usar raices cuadrasd
    RC_total = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust", "T"]) #dataFRramevacio
    
    for row in range(1, len(RT)):
        
        RC = ST.copy()  #this takes the susceptibles at the moment and makes copy (rusted by contact)
        RC["Distance"] = (RC["X"] - RT.iloc[row]["X"])**2  + (RC["Y"] - RT.iloc[row]["Y"])**2 #this calculat distance between the each infected plants and all suceptibles
        RC = RC.loc[RC["Distance"]<maxDistance] #tis filters only the neighburs
        #print(RC)
        if len(RC) >0:
            RC.drop(columns= ["Distance"]) #quitarla para no tener una de más
            #print(RC)
            RC_total = pd.concat([RC_total, RC])
        else:
            pass

        RC_total.loc[RC_total["Rust"] ==0, "Rust"] = 0.5        
        
    #now the general actualization (this sets should no overlpa)
    
    tempDF.loc[tempDF.ID.isin(LT_temp.ID), ["Rust"]] = LT_temp[["Rust"]] #esta debe ser un conjunto distinto a los anterioes.
    tempDF.loc[tempDF.ID.isin(R_H.ID), ["Rust"]] = R_H[["Rust"]]
    tempDF.loc[tempDF.ID.isin(RC_total.ID), ["Rust"]] = RC_total[["Rust"]] #esta se puede traslapar con la de arriba, pero no hay pedp
    
    return(tempDF)

    
    
        
        
#################################







# for row in range(1, len(L)):
#     ST_row = intento
#     ST_row["Distance"] = (ST_row["X"] - L.iloc[row]["X"])**2  + (ST_row["Y"] - L.iloc[row]["Y"])**2
#     ST_row = ST_row[ST_row["Distance"]<25] #lo puse como 25 porque la srt no me esta funcioando
         
    
    
    

#if __name__ == "__main__": 
Tmax = 2
dimIni = [10, 10]
iniInf = [0.8, 0.1, 0.1]
numPlants = 10  #para el intento
modeArr = "random"
numWorkers = 3
modeWorkers = "random"
contactoDis = 5 #meters
saltos = 5 #estas se van a poner externas en el patung
rep = 7 # esta se van a controlar externas en el patung 
harvest = False
dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicWorkers = {"num_Workers": numWorkers, "mode_Workers": modeWorkers}
dicSimulation = {"jumps_": saltos, "rep_":rep, "har_vest":harvest, "Tmax": Tmax, "contactDistance":contactoDis}

intento = generalDynamic(dicLattice, dicSimulation, dicWorkers)   