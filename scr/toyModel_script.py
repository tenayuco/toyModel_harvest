#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

import numpy as np  ##numpy paquete que acelera todo el manejo de matrices y demás
import random as rd
import pandas as pd


dimIni = [100, 100]
iniInf = [0.8, 0.1, 0.1]
numPlants = 10  #para el intento
modeArr = "random"
numWorkers = 3
modeWorkers = "random"

dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicWorkers = {"num_Workers": numWorkers, "mode_Workers": modeWorkers}


def setScenario(dic_Lattice):
    if dic_Lattice["mode_Arr"] == "random":
        
        IDplants = np.arange(0, dic_Lattice["num_Plants"], 1)
        X = np.random.randint(dic_Lattice["dim_Ini"][0], size= dic_Lattice["num_Plants"])
        Y = np.random.randint(dic_Lattice["dim_Ini"][1], size= dic_Lattice["num_Plants"])
        Harvest = np.repeat(0, dic_Lattice["num_Plants"])
        
        iniInfected = [i*dic_Lattice["num_Plants"] for i in dic_Lattice["ini_Inf"]]
        S = np.repeat(0, iniInfected[0])
        L = np.repeat(0.5, iniInfected[1])
        I = np.repeat(1, iniInfected[2])
        Rust = np.concatenate((S, L, I), axis= None)
        
        initialLattice = pd.DataFrame({"ID": IDplants, "X": X, "Y": Y, "Harvest":Harvest, "Rust": Rust})
    else:
        pass
    
    return(initialLattice)
    
