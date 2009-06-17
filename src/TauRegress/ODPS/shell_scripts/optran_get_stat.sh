#!/bin/sh

#--- load functions read_parameters() and get_senInfo()

. optran_coeff.func

#----------------------------------------------------
#  Input and parse parameters from external file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET"

PARAM_FILE=$1

#--- read in parameters from $PARAM_FILE and assign their 
#--- values to the variables whose names are listed in
#--- $PARAM_NAME_LIST 

read_parameters

STAT_DIR=$WORK_DIR/statistics
if [ ! -d $STAT_DIR ];then
  mkdir -p $STAT_DIR
fi

SCRIPT_DIR=${PWD}

#--- get the sensor names from the sensor_list file

SENSOR_LIST_FILENAME=${SCRIPT_DIR}/sensor_list
SENSOR_LIST=`awk '/BEGIN_LIST/ {
  while(getline){ 
    if(match($0,"END_LIST"))exit
    print $1} 
}' ${SENSOR_LIST_FILENAME}`


ProfSet=$PROF_SET

#--- Loop over sensors

for SatSen in ${SENSOR_LIST}; do

  #--- work directory for current sensor

  Sensor_DIR=${WORK_DIR}/${ProfSet}/${SatSen}
  
  echo $Sensor_DIR

  if [ ! -d ${Sensor_DIR} ]; then
   
    continue
     
  fi
     
  cd $Sensor_DIR

  # get component names
  GasName_List=`cat ${Sensor_DIR}/optran_components.txt`
  
  #----------------------
  # Loop over gases
  #----------------------

  for GasName in $GasName_List; do

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_optran_${GasName_List}"
    
    if [ ! -f $ExecutionDirFile ]; then
    
      continue
      
    fi
    

   #------------------------------------------------
   # combine stat files from different jobs into one
   #------------------------------------------------
    
    StatOutDir=${STAT_DIR}/stat_${ProfSet}
    mkdir -p $StatOutDir
    ProcSuffix=${SatSen}.${GasName}.${ProfSet}.optran
    StatOutFile=${StatOutDir}/stat.${ProcSuffix}.txt
    rm -f $StatOutFile 2>/dev/null
    
    DirList=`cat $ExecutionDirFile`
    
    for signalFileDIR in $DirList ;do
      
      # --- fort.40 contains statistics for the selected predictors
  
       cat ${signalFileDIR}/fort.40 >> ${StatOutFile}


    done
        
  done


done

exit
