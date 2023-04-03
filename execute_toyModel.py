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
numPlants = 200  #add multiple of 10 or 100 to have exact percentages, but ir does not change much. 
modeArr = "random"
listHarvest = 3
#harvest = True
hlPlants = round(numPlants/2)


contactoDis = 1.5 #meters


#harvesgin conditions (deinfed in the loop)

#numWorkers = 3
#timeHarvest = 1
#harvestSteps = int(hlPlants/numWorkers)


#rep = 3


 ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
 #see full documentation, in each step, 25 trees per worker

#dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
#dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "list_Harvest":listHarvest}

super_GeneralDF = pd.DataFrame(columns= ["ID", "X", "Y", "Rust", "Time", "WorkerID", "HarvestStep" ,"FruitLoad", "TotalHarvest", "HarvestModel", "HarvestTime", "numWorkers", "Rep"]) #we create a general data frame thatwill store the whole dynamic



repetition = range(10)
tiemposHarvest= [1,2,3,4] 
workers = [1,3,5,7]  #articulo ESteli, pero mejor buscar los extremos.
modelSwitch = [True, False]

for rep in repetition:
    for harvest in modelSwitch:
        if harvest == False:
            print("CONTROL", "rep", rep)

            dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants} #todo esto prefeindo
            dicHarvest = {"num_Workers": "NA", "harvest_Steps": "NA", "hl_Plants":hlPlants, "time_Harvest":"NA"}
            dicSimulation = {"rep_":999, "har_vest":harvest, "T_max": Tmax, "contactDistance":contactoDis}
            intento = generalDynamic(dicLattice, dicSimulation, dicHarvest)
        
            largoDF = len(intento)
    
    #esto se agrega al final
            intento["HarvestModel"] =np.repeat("Control", largoDF)
            intento["numWorkers"] =np.repeat("NA", largoDF)
            intento["HarvestTime"] =np.repeat("NA", largoDF)
            intento["Rep"] =np.repeat(rep, largoDF)
            super_GeneralDF= pd.concat([super_GeneralDF, intento])
        
        

        else:        
    
            for numWorkers in workers:
                for timeHarvest in tiemposHarvest:
                    print("HAR", harvest, "Nworkers", numWorkers,"H", timeHarvest,"REP", rep)
    

                    hlPlants = round(numPlants/2)
                    harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
                    #see full documentation, in each step, 25 trees per worker
                    dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
                    dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "time_Harvest":timeHarvest}
                    dicSimulation = {"rep_":rep, "har_vest":harvest, "T_max": Tmax, "contactDistance":contactoDis}
                
                    intento = generalDynamic(dicLattice, dicSimulation, dicHarvest)
                
                    largoDF = len(intento)
                
                    #esto se agrega al final
                    intento["HarvestModel"] =np.repeat(harvest, largoDF)
                    intento["numWorkers"] =np.repeat(numWorkers, largoDF)
                    intento["HarvestTime"] =np.repeat(timeHarvest, largoDF)
                    intento["Rep"] =np.repeat(rep, largoDF)
                    super_GeneralDF= pd.concat([super_GeneralDF, intento])

super_GeneralDF.to_csv("../data/intentoDF.csv")
    
    
    
