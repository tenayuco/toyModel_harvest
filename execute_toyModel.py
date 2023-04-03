#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

from functions_toyModel import *  #this works from terminal. I am gonna pause it. 

#initial conditions

import pandas as pd
import numpy as np

Tmax = 5
dimIni = [100, 100]
iniInf = [0.8, 0.1, 0.1]
numPlants = 2000  #add multiple of 10 or 100 to have exact percentages, but ir does not change much. 
#modeArr = "random"
#listHarvest = 3
#harvest = True

contactoDis = 1.5 #meters
#rep = 7 # esta se van a controlar externas en el patung 



#hlPlants = round(numPlants/2)
#harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
 #see full documentation, in each step, 25 trees per worker

#dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
#dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "list_Harvest":listHarvest}

super_GeneralDF = pd.DataFrame(columns= ["ID", "X", "Y", "Rust", "Time", "WorkerID", "HarvestStep" ,"FruitLoad", "TotalHarvest", "HarvestModel", "HarvestTime", "numWorkers", "Rep"]) #we create a general data frame thatwill store the whole dynamic



repetition = range(3)
tiemposHarvest= [1,2] 
workers = [1, 5]  #articulo ESteli, pero mejor buscar los extremos.
modelSwitch = [True, False]

for harvest in modelSwitch:
    for numWorkers in workers:
        for timeHarvest in tiemposHarvest:
            for rep in repetition:
                
                
                print("HAR", harvest, "Nworkers", numWorkers,"H", timeHarvest,"REP", rep)
    
                modeArr = "random"
    
                hlPlants = round(numPlants/2)
                harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
     #see full documentation, in each step, 25 trees per worker
                dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
                dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "time_Harvest":timeHarvest}
                dicSimulation = {"rep_":rep, "har_vest":harvest, "Tmax": Tmax, "contactDistance":contactoDis}
                
                intento = generalDynamic(dicLattice, dicSimulation, dicHarvest)
                
                largoDF = len(intento)
                
                #esto se agrega al final
                intento["HarvestModel"] =np.repeat(harvest, largoDF)
                intento["numWorkers"] =np.repeat(numWorkers, largoDF)
                intento["HarvestTime"] =np.repeat(timeHarvest, largoDF)
                intento["Rep"] =np.repeat(rep, largoDF)
                
                
                super_GeneralDF= pd.concat([super_GeneralDF, intento])

super_GeneralDF.to_csv("../data/intentoDF.csv")
    
    
    
