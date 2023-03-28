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
numPlants = 35  #add multiple of 10 or 100 to have exact percentages, but ir does not change much. 
modeArr = "random"
numWorkers = 3
modeWorkers = "random"
contactoDis = 1.5 #meters
saltos = 5 #estas se van a poner externas en el patung
rep = 7 # esta se van a controlar externas en el patung 
harvest = False
dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicWorkers = {"num_Workers": numWorkers, "mode_Workers": modeWorkers}
dicSimulation = {"jumps_": saltos, "rep_":rep, "har_vest":harvest, "Tmax": Tmax, "contactDistance":contactoDis}


intento = generalDynamic(dicLattice, dicSimulation, dicWorkers)  

intento.to_csv("../data/intentoDF.csv")   
    
