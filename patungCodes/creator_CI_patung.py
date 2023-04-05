#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This code creates a file (initialConditions) with all the textfiles that correspond to each of the initial conditions. In Condor language, this file will
be used to create the submit_file


@author: emilio
"""

import argparse
import os
from os import path
#from sh import mkdir
import pickle
import pandas as pd
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


repetition = range(5)
numberPlants = [200, 400]
tiemposHarvest= [1,2] 
workers = [1,3]  #articulo ESteli, pero mejor buscar los extremos.
modelSwitch = ["control", "closeness", "productivity"]


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

controlNumW = 0
controlTimeHar = "NoH"

for rep in repetition:
    for numPlants in numberPlants:
        for harvest in modelSwitch:
            if harvest == "control":
                #print("CONTROL", "rep", rep)
                
                linea.append(rep)
                linea.append(numPlants)
                linea.append(harvest)
                linea.append(controlNumW)
                linea.append(controlTimeHar)
                
                liga = patungDirectory+ "/initialConditions"
                os.makedirs(liga, exist_ok= True)

                with open(path.join(liga, "CI_%s_%s_%s_%s_%s" %(rep, numPlants, harvest, controlNumW, controlTimeHar)), "wb") as output:
                    pickle.dump(linea, output)
     #output.write(lista)
                linea= []

    
            else:        
    
                for numWorkers in workers:
                    for timeHarvest in tiemposHarvest:
                        #print("HAR", harvest, "Nworkers", numWorkers,"H", timeHarvest,"REP", rep)
                        linea.append(rep)
                        linea.append(numPlants)
                        linea.append(harvest)
                        linea.append(numWorkers)
                        linea.append(timeHarvest)
                    
                        liga = patungDirectory+ "/initialConditions"
                        os.makedirs(liga, exist_ok= True)
    
                        with open(path.join(liga, "CI_%s_%s_%s_%s_%s" %(rep, numPlants, harvest, numWorkers, timeHarvest)), "wb") as output:
                            pickle.dump(linea, output)
             #output.write(lista)
                        linea= []
        
    




