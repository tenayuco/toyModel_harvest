#!/usr/bin/env python3
# coding: utf-8
"""
This codes instantiates the functions defined in __init_py (kept in the .nwg file) using the different values defined in the submitfile, 
"""

import numpy as np
import pandas as pd
import argparse
from os import path
#from sh import mkdir
import pickle



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


from nwg import *  # here we import the __init__.py script
        
"""
Fixed parameters
"""
    

x0 = [0,0, 0] #inital condition
tiempoReal = 301
sizeStep = condiciones[10] #euler integration step
steps = int(tiempoReal/sizeStep)# remember, this is not time, time would be sizeStep*steps
mode= "mono"  #lthe other is log"
    

"""
parameters used during the sensitivity analysis
"""

r= condiciones[5]  #growth rate of leaves
b1= condiciones[6] #primary infection rate
b2 = condiciones[7] #secondary infection rate
a=  condiciones[8]  #new incorporation to the spore from infected leaf rate
mu= condiciones[9]  #spore death rate



"""
Here we used the changing parameters, according to the submit file

It is very important to keep track on the placement of the args
in condiciones 
"""


g = condiciones[0]
parameters =[r, b1,b2, g, a, mu]
dicODE = {"condIni": x0, "para":parameters, "steps": steps ,"sizeStep": sizeStep, "mode": mode}
IresPro = 0.02  
IresMax = condiciones[2] #proporcion de infeccion por planta max"
latticeSize= [12, 12] #x and y lattice size Here we define 12, since we have 2 rows and 2 columns that stand for the borders
Ires = [IresMax, IresPro]
patron= condiciones[3]  
den = 0.5 #this creates 50 plants
dicLattice = {"lattice_size": latticeSize, "Ires": Ires, "patron":patron, "density":den}
perX= 0
numPert= 0
dicPerturbation = {"PertuX": perX, "numPert":numPert}
actMigra = 1
m = condiciones[1]
dicMigration = {"actualizacion": actMigra, "tasa_Mig":m}
sim= condiciones[4]

 
"""
General dynamics. This chunk runs the general dynamics and keep the full matrix in the matricesGenerales file
"""   

matrizGeneral_marc= dinaMig(dicODE, dicLattice, dicMigration, dicPerturbation)
matrizGeneral = matrizGeneral_marc[0]
marcadoresHechos = matrizGeneral_marc[1]

patungDirectory = '/srv/home/emilio/sim_fullLattice'  #this is the general directory of the superComputer. 


"""
General matrices
"""
liga = patungDirectory + "/salida/matricesGenerales"  #mkdir salida/matricesGenerales before running inside the directory

with open(path.join(liga, "matrizGeneral_%s" %(args.code)), "wb") as output:
	pickle.dump(matrizGeneral, output)

    
"""
This chunk calculates, the D value, the maximum values of infection per plant and the average matrix
"""

#lo primero data indices
dataIndices = calculadorIndice(marcadoresHechos, dicLattice, sim)  #here we obtain the H (see formula of D)

Dc = 1/dataIndices.mean(axis='index')[0] #saca el resumen de 1/DC por eso lo invierto


dfPromedio= creadorDF_MaPromedio(matrizGeneral, dicODE, dicLattice, dicMigration, sim, marcadoresHechos, Dc)
liga2 = patungDirectory + "/salida/dfPromedios/"    #mkdir salida/dfPromedios before running inside the directory
dfPromedio.to_csv(liga2+"dfPromedio_%s.csv"%(args.code)) #average matrix



df_picosPlantas= df_calculadorPicosPlanta(matrizGeneral, dicODE, dicLattice, dicMigration, sim, marcadoresHechos, Dc)
liga3 =  patungDirectory + "/salida/dfPicos/"  #mkdir salida/dfPicos before running inside the director
df_picosPlantas.to_csv(liga3+"df_PicosPlantas%s.csv" %(args.code))  #maxium peaks 


