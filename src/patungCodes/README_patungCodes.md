
### Instructions to run the model

## 1. Prepare the folder in the supercomputer (condor based)

Use two terminals, one of your computer and one in the supercomputer (condor based) 

## Terminal 1 
- Enter super computer (see short command for details, but this depends on your server). 
- Erase old files (with rm -r)
- activate virtual environment (where you install all the dependencies)

##Terminal 2
- Upload all the files from your computer to the supercomputer. 

`(scp -r r /home/toyModelHarvest/src/patungCodes/* serverDirection:/srv/home/toyModelHarvest/`

## Terminal 1
- Create the files and necessary folders and make some codes executable. 

This first creates some folder, move some files..

`chmod +x execute_toyModel_pat.py; mkdir bitacora salida initialConditions nwg; cd salida/ ; mkdir DF_total DF_spatialAverage DF_muestrasPath ; cd ..;  mv __init__.py ./nwg/`

This creates a list of initial condistion to create the submitfile
`python3 creator_CI_patung.py; cd initialConditions/ ; ls -R > ../listCI.txt ; cd .. ; sed -i '1d' listCI.txt; python3 creator_SubmitFile.py `
 
 - Your directory should have the following structure:
 
-------------------------------------------------------------

codeMatrizSim.py (activated)
creator_CI_patung.py
creator_SubmitFile.py
__init__.py 
bitacora
salida
salida/dfPromedios
salida/dfPicos
salida/matricesGenerales
initialConditions
listCI.txt
submit_file_fullLattice.sub
.nwg
.nwg/__init__.py
--------------------------
 
## 2. Run the simulations in paralel. (jobs in patung)

`condor_submit submit_file_toyModel.sub`

To see the progress
`condor_q`
`condor_status` 

To check the status, errors, or anything, you can look the bitacora folder. 

This will fill the dfPromedios, dfPicos and matricesGenerales folders


* notes: 
 The bicatora directory keeps track of the different problems or messages during the simulations (.out, .log and .err) 

## 3. Download everything to your folder. 

scp -r server:/srv/home/emilio/toyModelHarvest/salida /home/toyModelHarvest/dataPatung_descargado



