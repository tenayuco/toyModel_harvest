# Interplay between harvesting, planting density and ripening time affects coffee leaf rust dispersal and infection


This repository provides the code to reproduce the results and figures of the paper. The dynamics were wrote in python and the figures in R. Here I only uploaded the src folder. But, the code will ask for the data folder and output folder in the following structure:

## structure

toyModel_harvest
	--src
		--/patungCodes (run the simulation)
			--creator_CI_patung.py
			--creator_SubmitFile.py
			--execute_toyModer_pat.py
			-- __init__.py 
			-- README_patungCodes.md
		--/grapherCodes
		--/analisCodes
	--data
		--dataPatungDescargado/
		--DF_total_TF.csv (large data with all scenarios at final time)
		--DF_total_TF_short.csv (created)
		-- DF_muestrasPath_complete.csv
		-- DF_spatialAverage_complete.csv
		-- datosParaFiguras.csv
		--baseDatosREDES_N.csv
		
	--output
		--


For the purposes of the reproducibilty, the full data folder is available in the following link (drive*)
The ouput folder will be created through the code.  

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

##dataBaseCodes 

This folder takes the the dataPatung_descargado/salida and modificates the data base to create more suitable and useful data structure to make the figures. For the purposes of the reproducibilty, the full data folder is available in the following link (drive*)

* dataBaseModifications.rmd 

So this code will write the 

a) data/spatialAverage_complete.csv, 
b) the data/DF_muestrasPath_complete.csv, 
c) data/DF_total_TF.csv, and 
d) data/DF_total_TF_short.csv


##networkCodes 
This folder analyses the results at a network level and send a csv to the grapher codes to reproduce the figures. 

* harvestNetworkDis.rmd 

This codes uses the data/DF_total_TF.csv and generates the data/histHar.csv (to plot the networks). 

* Spatial_networks_old.R

Old code that generates an old version figure of the netwrok plots. 

##grapherCodes 



##otherCodes 

This codes make some analisis on the distributions

* distributionSteps
* anaSteps






analisisCodes
basicCodes



## Getting Started

### Dependencies

* Describe any prerequisites, libraries, OS version, etc., needed before installing program.
* ex. Windows 10

### Installing

* How/where to download your program
* Any modifications needed to be made to files/folders

### Executing program

* How to run the program
* Step-by-step bullets
```
code blocks for commands
```

## Help

Any advise for common problems or issues.
```
command to run if program contains helper info
```

## Authors

Contributors names and contact info

ex. Dominique Pizzie  
ex. [@DomPizzie](https://twitter.com/dompizzie)

## Version History

* 0.2
    * Various bug fixes and optimizations
    * See [commit change]() or See [release history]()
* 0.1
    * Initial Release

## License

This project is licensed under the [NAME HERE] License - see the LICENSE.md file for details

## Acknowledgments

Inspiration, code snippets, etc.
* [awesome-readme](https://github.com/matiassingers/awesome-readme)
* [PurpleBooth](https://gist.github.com/PurpleBooth/109311bb0361f32d87a2)
* [dbader](https://github.com/dbader/readme-template)
* [zenorocha](https://gist.github.com/zenorocha/4526327)
* [fvcproductions](https://gist.github.com/fvcproductions/1bfc2d4aecb01a834b46)
