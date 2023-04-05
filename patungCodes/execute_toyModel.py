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



Tmax = 5
dimIni = [100, 100]
iniInf = [0.8, 0.1, 0.1]
modeArr = "random"
#harvest = True
contactoDis = 1.5 #meters


"""
Here we used the changing parameters, according to the submit file

It is very important to keep track on the placement of the args
in condiciones 
"""

rep = condiciones[0]
numPlants = condiciones[1]
harvest = condiciones[2]
numWorkers = condiciones[3]
timeHarvest = condiciones[4]


###
hlPlants = round(numPlants/2)
harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
#see full documentation, in each step, 25 trees per worker

dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
dicHarvest = {"har_vest": harvest, "num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "time_Harvest":timeHarvest}
dicSimulation = {"rep_":rep, "T_max": Tmax, "contactDistance":contactoDis}


"""
General dynamics. This chunk runs the general dynamics and keep the full matrix in the matricesGenerales file
"""   

DF = generalDynamic(dic_Lattice= dicLattice, dic_Simulation=dicSimulation, dic_Harvest= dicHarvest)
largoDF = len(DF)

#esto se agrega al final
DF["HarvestModel"] =np.repeat("Control", largoDF)
DF["numWorkers"] =np.repeat("NA", largoDF)
DF["HarvestTime"] =np.repeat("NA", largoDF)
DF["Rep"] =np.repeat(rep, largoDF)


"""
General matrices
"""

patungDirectory = '/srv/home/emilio/toyModel_harvest'  #this is the general directory of the superComputer. 

"""
General matrices
"""
liga = patungDirectory + "/salida/DF_full"  #mkdir salida/matricesGenerales before running inside the directory
DF.to_csv(liga+ "DF_full_%s.csv" %(args.code)) #average matrix

    
"""
poner aqui el filtro de DF FALTA!*!
"""

#dfPromedio= creadorDF_MaPromedio(matrizGeneral, dicODE, dicLattice, dicMigration, sim, marcadoresHechos, Dc)

liga2 = patungDirectory + "/salida/df_finalTime/"    #mkdir salida/dfPromedios before running inside the directory
DF_finalTime.to_csv(liga2+"df_finalTime_%s.csv"%(args.code)) #average matrix

















for rep in repetition:
    for harvest in modelSwitch:
        if harvest == "control":
            print("CONTROL", "rep", rep)

            dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants} #todo esto prefeindo
            dicHarvest = {"har_vest":harvest, "num_Workers": "NA", "harvest_Steps": "NA", "hl_Plants":hlPlants, "time_Harvest":"NA"}
            dicSimulation = {"rep_":999, "T_max": Tmax, "contactDistance":contactoDis}
            intento = generalDynamic(dic_Lattice= dicLattice, dic_Simulation=dicSimulation, dic_Harvest= dicHarvest)
        
            largoDF = len(intento)
    
    #esto se agrega al final
            intento["HarvestModel"] =np.repeat("Control", largoDF)
            intento["numWorkers"] =np.repeat("NA", largoDF)
            intento["HarvestTime"] =np.repeat("NA", largoDF)
            intento["Rep"] =np.repeat(rep, largoDF)
            super_GeneralDF= pd.concat([super_GeneralDF, intento])
        
        

        else:        
    
            for numWorkers in workers:
                for timeHarvest in tiemposHarvest:
                    print("HAR", harvest, "Nworkers", numWorkers,"H", timeHarvest,"REP", rep)
    

                    hlPlants = round(numPlants/2)
                    harvestSteps = int(hlPlants/numWorkers) ##*Entero hace que nunca hagas mas de la mitad. Tendria que sobrar 1 planta
                    #see full documentation, in each step, 25 trees per worker
                    dicLattice = {"dim_Ini": dimIni, "ini_Inf": iniInf, "mode_Arr": modeArr, "num_Plants": numPlants}
                    dicHarvest = {"har_vest": harvest, "num_Workers": numWorkers, "harvest_Steps": harvestSteps, "hl_Plants":hlPlants, "time_Harvest":timeHarvest}
                    dicSimulation = {"rep_":rep, "T_max": Tmax, "contactDistance":contactoDis}
                
                    intento = generalDynamic(dic_Lattice= dicLattice, dic_Simulation=dicSimulation, dic_Harvest= dicHarvest)
                
                    largoDF = len(intento)
                
                    #esto se agrega al final
                    intento["HarvestModel"] =np.repeat(harvest, largoDF)
                    intento["numWorkers"] =np.repeat(numWorkers, largoDF)
                    intento["HarvestTime"] =np.repeat(timeHarvest, largoDF)
                    intento["Rep"] =np.repeat(rep, largoDF)
                    super_GeneralDF= pd.concat([super_GeneralDF, intento])

super_GeneralDF.to_csv("../data/intentoDF.csv")
    
    
    
