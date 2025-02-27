# Interplay between harvesting, planting density and ripening time affects coffee leaf rust dispersal and infection


This repository provides the code to reproduce the results and figures of the paper. The dynamics were wrote in python and the figures in R. Here I only uploaded the src folder. But, the code will ask for the data folder and output folder in the following structure:

## structure

toyModel_harvest

	  - src
		 - patungCodes (run the simulation)
			-creator_CI_patung.py
			-creator_SubmitFile.py
			-execute_toyModer_pat.py
			- __init__.py 
			- README_patungCodes.md
		-/dataBaseCodes
			- dataBaseModifications.rmd
		-/grapherCodes
			-  figuresLattice.Rmd
		-/analisCodes
			- fittingDistribution.R
		
	-data
		- DF_total_TF.csv 
		- DF_total_TF_short.csv 
		- DF_muestrasPath_complete.csv
		- DF_spatialAverage_complete.csv
		- datosParaFiguras.csv (from another paper)
		- histHar.csv
	-output
		-graficas
			-DIF_RUST (for figures 4)
			-NETWORK (for figure 5)
			-PATH (figure 2 and 3)
			-SUP_FIG (supplementary figures)
		-figurasLattice

For the purposes of the reproducibilty, the full data folder is available in the following link

https://drive.google.com/drive/folders/1aOuhljg_9z-g45MkrTvafkeaSxyl2Vj9?usp=sharing


In the following lines, I describe the role of /src folder

## patungCodes

This folder runs the simulation in a condor language machine. Take a look at the README.md to follow the logic of each file. And the shortCommand to run everything (step by step). It uses the following files:

 * creator_CI_patung.py 
 (creates text files with all the initial conditions you want to test. These text files names will be condensed in one big list for the creator submit file. The list.txt). 
 * creator_SubmitFile.py
 (creates the submitfile as required by condor, using the intial conditions created by the previous code)
 * execute_toyModer_pat.py 
 Run all the simulation calling the __init__.py file. And creates the output for the salida folder. 
 
 *  __init__.py 
 (This file has all the core functions of the model). 
 

After runing everything and downloading the data (see README_patungCodes) you will have a dataPatung_descargado/salida folder. 

## dataBaseCodes 

This folder takes the the dataPatung_descargado/salida and modificates the data base to create more suitable and useful data structure to make the figures. For the purposes of the reproducibilty, the full data folder is available in the following link (drive*)

* dataBaseModifications.rmd 

So this code will write the 

a) data/spatialAverage_complete.csv, 
b) the data/DF_muestrasPath_complete.csv, 
c) data/DF_total_TF.csv, and 
d) data/DF_total_TF_short.csv


## analisisCodes 
This folder analyses the results at a network level and send a csv to the grapher codes to reproduce the figures. 

* harvestNetworkDis.rmd 

This codes uses the data/DF_total_TF.csv and generates the data/histHar.csv (to plot the networks). 

* fittingDistribution.R

This codes fits the step length distribution to different functions (Nicholas Medina function). And then generates Fig. S.1.3

* Spatial_networks_old.R

Old code that generates an old version figure of the netwrok plots. 

## grapherCodes 

* figuresLattice.Rmd

This code creates all the figures (manuscript and supplementary) and store them in the output subfolders



## Authors

Emilio Mora Van Cauwelaert

emiliomora92@gmail.com 


## Version History

* 27 feb 2025. This version is already functional. I will clean it no make it easier to follow. 
