#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

from functions_toyModel import *  #this works from terminal. I am gonna pause it. 

#initial conditions

Tmax = 5
dimIni = [10, 10]
iniInf = [0.8, 0.1, 0.1]
numPlants = 50  #add multiple of 10 or 100 to have exact percentages, but ir does not change much. 
modeArr = "random"

contactoDis = 1.5 #meters
rep = 7 # esta se van a controlar externas en el patung 

harvest = True
numWorkers = 3
hlPlants = round(numPlants/2)
harvestSteps = int(hlPlants/numWorkers) ##*E
print("harvestStep", harvestSteps)
 #see full documentation, in each step, 25 trees per worker

dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicHarvest = {"num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants}
dicSimulation = {"rep_":rep, "har_vest":harvest, "Tmax": Tmax, "contactDistance":contactoDis}


intento = generalDynamic(dicLattice, dicSimulation, dicHarvest)  

#intento.to_csv("../data/intentoDF.csv")   
    
