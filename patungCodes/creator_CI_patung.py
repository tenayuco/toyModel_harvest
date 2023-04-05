#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
This code creates a file (initialConditions) with all the textfiles that correspond to each of the initial conditions. In Condor language, this file will
be used to create the submit_file


@author: emilio
"""

import argparse
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


repetition = range(10)
numPlants = [1000, 2000, 3000]
tiemposHarvest= [1,2,3,4,5] 
workers = [1,3,5,7]  #articulo ESteli, pero mejor buscar los extremos.
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

patungDirectory = '/srv/home/emilio/toyModel_harvest'  #this changes depending the computer



##################AQUI VOY






sim = 0

for g in valoresG:
    for m in valoresM:
        for ini in valoresIni:
            for pat in valoresPatrones:
                
                if pat == "random":
                    sim = sim +1
                    
                else:
                    sim = 1
                
                for r in valoresR:
                    for b1 in valoresB1:
                        for b2 in valoresB2:
                            for a in valoresA:
                                for mu in valoresMU:
                                    for size in sizeEuler:
                                        linea.append(g)
                                        linea.append(m)
                                        linea.append(ini)
                                        linea.append(pat)
                                        linea.append(sim)
                                        linea.append(r)
                                        linea.append(b1)
                                        linea.append(b2)
                                        linea.append(a)
                                        linea.append(mu)
                                        linea.append(size)
                
                        
                                        liga = patungDirectory+ "/initialConditions"
                                        os.makedirs(liga, exist_ok= True)
                        
                                        with open(path.join(liga, "CI_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s_%s.txt" %(g,m, ini, pat, sim, r, b1, b2, a, mu, size)), "wb") as output:
                                            pickle.dump(linea, output)
                                 #output.write(lista)
                                        linea= []

