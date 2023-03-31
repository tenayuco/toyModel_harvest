#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

from functions_toyModel import *  #this works from terminal. I am gonna pause it. 

#initial conditions

import pandas as pd

Tmax = 5
dimIni = [100, 100]
iniInf = [0.8, 0.1, 0.1]
numPlants = 2000  #add multiple of 10 or 100 to have exact percentages, but ir does not change much. 
modeArr = "random"

contactoDis = 1.5 #meters
#rep = 7 # esta se van a controlar externas en el patung 

harvest = True
numWorkers = 3
hlPlants = round(numPlants/2)
harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
print("harvestStep", harvestSteps)
 #see full documentation, in each step, 25 trees per worker

dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants}

super_GeneralDF = pd.DataFrame(columns= ["HarvestModel", "Rep", "ID", "X", "Y", "Rust", "Time", "WorkerID", "HarvestStep", "HarvestEvent","FruitLoad", "TotalHarvest"]) #we create a general data frame thatwill store the whole dynamic

for rep in range(10):
    print("REP", rep)
    dicSimulation = {"rep_":rep, "har_vest":harvest, "Tmax": Tmax, "contactDistance":contactoDis}
    intento = generalDynamic(dicLattice, dicSimulation, dicHarvest)  
    
    super_GeneralDF= pd.concat([super_GeneralDF, intento])

super_GeneralDF.to_csv("../data/intentoDF.csv")
    
    
    
