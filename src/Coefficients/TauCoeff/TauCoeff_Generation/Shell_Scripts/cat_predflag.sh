#!/bin/sh

#-----------------------------------------------------------
#  Script name: cat_predflag.sh
#
#  Purpose: concatenate predictor flag files, each containing
#           flags for a subset of channels on a sensor.
#
#  Usage: cat_predflag.sh param_file
#
#    Example: cat_predflag.sh parameters_ALLCOM
#
#  Inputs:
#     param_file - name of the file containing a list of
#                  parameters.   
#
#  Output: a file containing predictor flags for the sensor
#
#  Developer: Yong Han, 2003/06/10
#------------------------------------------------------------    

#--- load functions read_parameters() and get_senInfo()

. optran_coeff.func

#----------------------------------------------------
# The argument $1 contains a filename.
# The following script segment is to read in and parse 
# the parameters from the file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET GAS_NAME_LIST"

PARAM_FILE=$1

#--- read in parameters from $PARAM_FILE and assign their 
#--- values to the variables whose names are listed in
#--- $PARAM_NAME_LIST 

read_parameters
  
#--- current directory

SCRIPT_DIR=${PWD}

#--- get the sensor names from the sensor_list file

SENSOR_LIST_FILENAME=${SCRIPT_DIR}/sensor_list
SENSOR_LIST=`awk '/BEGIN_LIST/ {
  while(getline){ 
    if(match($0,"END_LIST"))exit
    print $1} 
}' ${SENSOR_LIST_FILENAME}`

ProfSet=$PROF_SET

#--- parse gas name list and put the gas names into another string 
#--- delimitated by spaces

GasName_List=`echo $GAS_NAME_LIST | awk -F, '{for(i=1;i<=NF;i++)print $i}'`

#--- Loop over sensors

for SatSen in ${SENSOR_LIST}; do

  #--- work directory for current sensor

  Sensor_DIR=${WORK_DIR}/${ProfSet}/${SatSen}

  if [ ! -d $Sensor_DIR ];then
  
     echo "directory $Sensor_DIR does not exist"
     exit 1
  fi
  
  cd $Sensor_DIR
  
  #----------------------
  # Loop over gases
  #----------------------

  for GasName in $GasName_List; do

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_${GasName}_all"
    
   #--- wait if the jobs sent by gensub.sh are not completed
  
    while [ ! -f $ExecutionDirFile ]; do
       
      sleep 10
      
    done
 
    DirList=`cat $ExecutionDirFile`
    
    for signalFileDIR in $DirList ;do
  
      while [ ! -f ${signalFileDIR}/Completion_signal.txt ]; do
       
        sleep 10
	 
      done
  
    done

   #------------------------------------------------
   # combine fredFlag files from different jobs into one
   #------------------------------------------------

    PredFlagOutFile=${Sensor_DIR}/predflag.${SatSen}.${GasName}.stat.${ProfSet}.txt
    rm -f $PredFlagOutFile 2>/dev/null

    for signalFileDIR in $DirList ;do
      
      # --- fort.40 contains statistics
  
       cat ${signalFileDIR}/fort.40 >> ${PredFlagOutFile}
	
    done

  done

done

exit
