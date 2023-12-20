#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This code creates a file (initialConditions) with all the textfiles that correspond to each of the initial conditions. In Condor language, this file will
be used to create the submit_file


@author: emilio
"""

import os
from os import path
#from sh import mkdir
import pickle
import numpy as np

#!/usr/bin/env python3
# -*- coding: utf-8 -*-


""" 
1. Initial conditions
Here we define the different values of the parameters

1.1. For the general figures we will fix  r,b1, b2, a and mu values. 
Aqui por si hay algo mas que variar

"""


"""
Then we define the different values for m, g, io and the patterns
"""


repetition = range(30)
#repetition = range(10)
numberPlants = [500, 1000, 2000, 3000, 5000]
#numberPlants = [500, 1000]
tiemposHarvest= [5] 
#tiemposHarvest= [0,1,2] 
#workers = [1,5,8]
workers = [1]  #articulo ESteli, pero mejor buscar los extremos.  #toca ir hasta el 8!!!!
modelSwitch = ["control", "closeness"]
#porcionCosecha = [0.5,1]
#porcionCosecha = [1]
porcionCosecha = ["A", "S_I", "S_F"] #asincronico, sincronico inicia y sincronico final 


"""
1.2 If we want to do the sensitivy analysis we ran our initial procedure, but changing the value
of the other parameters at once.
"""

"""
1.3 If we want to do the euler analysis we  we ran our initial procedure, but changing the value
of the step size.
"""

"""
Here we run the general loop
"""

linea = []

patungDirectory = '/srv/home/emilio/toyModelHarvest'  #this changes depending the computer



##################AQUI VOY

controlNumW = 999 #ESTO valores no impor
controlTimeHar = "NoH"
controlPorcion = 1 ##este si improt, hay que dejarlo en 1



for rep in repetition:
    simID = 0
    for numPlants in numberPlants:
        for harvest in modelSwitch:
            if harvest == "control":
                #print("CONTROL", "rep", rep)
                
                linea.append(rep)
                linea.append(numPlants)
                linea.append(harvest)
                linea.append(controlNumW)
                linea.append(controlTimeHar)
                linea.append(controlPorcion)
                
                linea.append(simID)
                
                liga = patungDirectory+ "/initialConditions"
                os.makedirs(liga, exist_ok= True)

                with open(path.join(liga, "CI_%s_%s_%s_%s_%s_%s_%s" %(rep, numPlants, harvest, controlNumW, controlTimeHar, controlPorcion, simID)), "wb") as output:
                    pickle.dump(linea, output)
     #output.write(lista)
                simID = simID+1
                linea= []

    
            else:
                for numWorkers in workers:
                    for timeHarvest in tiemposHarvest:
                        for porcion in porcionCosecha:
                            #print("HAR", harvest, "Nworkers", numWorkers,"H", timeHarvest,"REP", rep)
                            linea.append(rep)
                            linea.append(numPlants)
                            linea.append(harvest)
                            linea.append(numWorkers)
                            linea.append(timeHarvest)
                            linea.append(porcion)
                            linea.append(simID)
                        
                            liga = patungDirectory+ "/initialConditions"
                            os.makedirs(liga, exist_ok= True)
    
                            with open(path.join(liga, "CI_%s_%s_%s_%s_%s_%s_%s" %(rep, numPlants, harvest, numWorkers, timeHarvest, porcion, simID)), "wb") as output:
                                pickle.dump(linea, output)
             #output.write(lista)
                            simID = simID+1  #en principio este debe guardar cada combinacion para cada grupo de repeticiones. en el MISMO orden
                            linea= []
        
    




