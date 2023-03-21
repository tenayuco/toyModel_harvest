#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Mar 21 11:55:53 2023

@author: emilio
"""

import numpy as np  ##numpy paquete que acelera todo el manejo de matrices y demás
import random as rd
import pandas as pd


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
        Tcero= np.repeat(0, dic_Lattice["num_Plants"])  #esto es solo para crear la columna, se va a reemplrazar
        
        
        initialLattice = pd.DataFrame({"Jump": Jump, "Rep": Rep, "ID": IDplants, "X": X, "Y": Y, "Harvest":Harvest, "Rust": Rust, "T": Tcero})
    else:
        pass
    
    return(initialLattice)


def generalDynamic(dic_Lattice, dic_Simulation, dic_Workers):
    
    generalDF = pd.DataFrame(columns= ["Jump", "Rep", "ID", "X", "Y", "Harvest", "Rust", "T"])
    T = 0
    
    initialDF= setScenario(dic_Lattice, dic_Simulation)
    while T< dic_Simulation["Tmax"]:
        tau = 0
        while tau<1:
        
            if dic_Simulation["har_vest"] == True:
                pass #por lo pront
            else:
                pass
        
        #aplica el modulo contact
            temporalDF = conctact***### ahora lo cambio cuando defina el modeilo 
            tau = tau + 0.5
            T = T+tau
            generalDF= pd.concat([generalDF, temporalDF])
        
    return(generalDF)


#def contactModel(dic_Simulation):
    


if __name__ == "__main__": 
    Tmax = 2
    dimIni = [100, 100]
    iniInf = [0.8, 0.1, 0.1]
    numPlants = 10  #para el intento
    modeArr = "random"
    numWorkers = 3
    modeWorkers = "random"
    saltos = 5 #estas se van a poner externas en el patung
    rep = 7 # esta se van a controlar externas en el patung 
    harvest = False
    dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
    dicWorkers = {"num_Workers": numWorkers, "mode_Workers": modeWorkers}
    dicSimulation = {"jumps_": saltos, "rep_":rep, "har_vest":harvest, "Tmax": Tmax}
    
    intento = generalDynamic(dicLattice, dicSimulation, dicWorkers)