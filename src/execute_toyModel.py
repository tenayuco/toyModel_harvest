#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

from functions_toyModel import *  #this works from terminal. I am gonna pause it. 

#initial conditions

dimIni = [100, 100]
iniInf = [0.8, 0.1, 0.1]
numPlants = 10  #para el intento
modeArr = "random"
numWorkers = 3
modeWorkers = "random"

dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicWorkers = {"num_Workers": numWorkers, "mode_Workers": modeWorkers}

## run funcion

print(ftm.setScenario(dicLattice))

    
