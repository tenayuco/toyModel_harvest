#!/usr/bin/env python3
# coding: utf-8
"""
This codes instantiates the functions defined in __init_py (kept in the .nwg file) using the different values defined in the submitfile, 
Created on Tue Mar 21 09:27:57 2023

@author: emilio
"""

import argparse
from os import path
#from sh import mkdir
import pickle
import pandas as pd
import numpy as np


from nwg import *  # here we import the __init__.py script

""" 
We first import the values of the initial conditions in the submitfile
"""


parser = argparse.ArgumentParser(description = "arguments that have to be read by the code")
parser.add_argument('--code', help = 'initial matrix', required= True)
parser.add_argument('--subdir', help= 'subdirectory', required= True)
args = parser.parse_args()


with open(path.join(args.subdir, args.code), "rb") as out:
	condiciones = pickle.load(out)

print (condiciones)

        
"""
Fixed parameters
"""



Tmax = 12  #ESTE ESEL CA;MIBO NUEBO por szatch
dimIni = [100, 100]
iniInf = [0.8, 0, 0.2]
modeArr = "random"
#harvest = "closeness"
contactoDis = 1.5 #meters


"""
Here we used the changing parameters, according to the submit file

It is very important to keep track on the placement of the args
in condiciones 
"""

rep = condiciones[0]
numPlants = condiciones[1]
harvest = condiciones[2] 

timeHarvest = condiciones[4]


###

#harvestSteps = int(round(numPlants/2)/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
#see full documentation, in each step, 25 trees per worker


if(condiciones[5] == "A"):
    hlPlants = round(numPlants*0.5)
    numWorkers = round(hlPlants/250)
else:
    hlPlants = round(numPlants*1)
    numWorkers = round(hlPlants/500)  # asi van a quedar el mismo numbero por esecnario! 


#condiciones[3]  ##queda como 1 este ya no se usa


harvestSteps = int(round(hlPlants)/numWorkers) ##*Entero a ver si esto funciona, espara que los harvest steps no sean los mismos.


simID = condiciones[6]



dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicHarvest = {"har_vest": harvest, "num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "time_Harvest":timeHarvest, "model_harvest": condiciones[5]}
dicSimulation = {"rep_":rep, "T_max": Tmax, "contactDistance":contactoDis}


"""
General dynamics. This chunk runs the general dynamics and keep the full matrix in the matricesGenerales file
"""   

DF = generalDynamic(dic_Lattice= dicLattice, dic_Simulation=dicSimulation, dic_Harvest= dicHarvest)
largoDF = len(DF)

#esto se agrega al final
DF["Rep"] =np.repeat(rep, largoDF)
DF["numPlants"] =np.repeat(numPlants, largoDF)
DF["HarvestModel"] =np.repeat(harvest, largoDF)
DF["numWorkers"] =np.repeat(numWorkers, largoDF)
DF["HarvestTime"] =np.repeat(timeHarvest, largoDF)
DF["porcionCosecha"]= np.repeat(condiciones[5], largoDF)
DF["SimID"] =np.repeat(simID, largoDF)



"""
General matrices. voy a hacer un cambio, para no tener toda la base de datos

"""
"""
General matrices
"""

"""
Filtramos solo una rep, un ejemplo, pero completo
"""



patungDirectory = '/srv/home/emilio/toyModelHarvest'  #this is the general directory of the superComputer. 


########

DF_spatialAverage = DF.groupby(["Rep", "numPlants", "numWorkers", "HarvestModel", "HarvestTime", "porcionCosecha", "Time", "SimID"])[['Rust']].mean()

liga0 = patungDirectory + "/salida/DF_spatialAverage/"  #mkdir salida/matricesGenerales before running inside the directory
DF_spatialAverage.to_csv(liga0+ "DF_spatialAverage_%s.csv" %(args.code)) #average matrix



liga1 = patungDirectory + "/salida/DF_muestrasPath/"  #mkdir salida/matricesGenerales before running inside the directory


if harvest != "control":
    if timeHarvest == 5:
        DF_sample = DF.loc[(DF["Time"] == 5.5)].copy()  #agarro 5.5 porque es justo despues de la cosecha
        DF_sample.to_csv(liga1+ "DF_muestrasPath_%s.csv" %(args.code)) #these are for the path examples
                
            
liga3 = patungDirectory + "/salida/DF_total/"



DF_harvest = DF.loc[(DF["Time"] == Tmax)].copy()
DF_harvest.to_csv(liga3+ "DF_distanceW_%s.csv" %(args.code)) #average matrix




#DF.to_csv(liga+ "DF_total_%s.csv" %(args.code)) #average matrix
###NO BAJAR Todosolo un ejemplo!!!






    
    
