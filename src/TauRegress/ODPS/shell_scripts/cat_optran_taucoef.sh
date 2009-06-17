#!/bin/sh

#--- load functions read_parameters() and get_senInfo()

. optran_coeff.func

#----------------------------------------------------
#  Input and parse parameters from external file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET"

PARAM_FILE=$1
EXE_FILE=$2  # exe file to concatenate Tau files

#--- read in parameters from $PARAM_FILE and assign their 
#--- values to the variables whose names are listed in
#--- $PARAM_NAME_LIST 

read_parameters
  
#--- directory containing final results

RESULT_DIR=${WORK_DIR}/results
if [ ! -d $RESULT_DIR ];then
  mkdir -p $RESULT_DIR
fi

#--- current directory

SCRIPT_DIR=${PWD}

#--- get the sensor names from the sensor_list file

SENSOR_LIST_FILENAME=${SCRIPT_DIR}/sensor_list
SENSOR_LIST=`awk '/BEGIN_LIST/ {
  while(getline){ 
    if(match($0,"END_LIST"))exit
    print $1} 
}' ${SENSOR_LIST_FILENAME}`

#--- parse gas name list and put the gas names into another string 
#--- delimitated by spaces

GasName_List=`echo $GAS_NAME_LIST | awk -F, '{for(i=1;i<=NF;i++)print $i}'`

#--- Loop over sensors

for SatSen in ${SENSOR_LIST}; do

  #--- work directory for current sensor

  Sensor_DIR=${WORK_DIR}/${PROF_SET}/${SatSen}

  if [ ! -d $Sensor_DIR ];then
  
     echo "directory $Sensor_DIR does not exist"
     exit 1
  fi
  
  cd $Sensor_DIR

  # get component names
  GasName_List=`cat ${Sensor_DIR}/optran_components.txt`
  
  #----------------------
  # Loop over gases
  #----------------------

  TAU_COEFF_FILE_SENSOR=${RESULT_DIR}/taucoef.${SatSen}.${PROF_SET}.nc
  TAU_COEFF_FILE1_SENSOR=""

  for GasName in $GasName_List; do

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_optran_${GasName}"
   
   #--- wait if the jobs are not completed
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
   # Concatenate Tau coefficient files from different jobs into one
   # along channel direction first, then along absorbers
   #------------------------------------------------

    TAU_COEFF_FILE=${RESULT_DIR}/taucoef.${SatSen}.${GasName}.${PROF_SET}.optran.nc
    TAU_COEFF_FILE1=""

    for signalFileDIR in $DirList ;do
      
       if [ "$TAU_COEFF_FILE1" == "" ];then

         TAU_COEFF_FILE1=${signalFileDIR}/TauCoeff.nc
# add by ychen 04/11/08 for one channel sensor case
         cp ${TAU_COEFF_FILE1} ${TAU_COEFF_FILE}
       else
       
         TAU_COEFF_FILE2=${signalFileDIR}/TauCoeff.nc	 

#         echo "${SCRIPT_DIR}/$EXE_FILE << EOF" > Cat_tmp_script
         echo "$EXE_FILE << EOF" > Cat_tmp_script
         echo "$TAU_COEFF_FILE1" >> Cat_tmp_script
         echo "$TAU_COEFF_FILE2" >> Cat_tmp_script
         echo "$TAU_COEFF_FILE" >> Cat_tmp_script
         echo "2" >> Cat_tmp_script
         echo "EOF" >> Cat_tmp_script  
         chmod 700 Cat_tmp_script
         Cat_tmp_script
         rm -f Cat_tmp_script
	 
	 TAU_COEFF_FILE1=$TAU_COEFF_FILE

      fi
  	 	  	
    done
    
    if [ "$TAU_COEFF_FILE1_SENSOR" == "" ];then

      TAU_COEFF_FILE1_SENSOR=$TAU_COEFF_FILE
      
    else
    
      TAU_COEFF_FILE2_SENSOR=$TAU_COEFF_FILE
#      echo "${SCRIPT_DIR}/$EXE_FILE << EOF" > Cat_tmp_script
      echo "$EXE_FILE << EOF" > Cat_tmp_script
      echo "$TAU_COEFF_FILE1_SENSOR" >> Cat_tmp_script
      echo "$TAU_COEFF_FILE2_SENSOR" >> Cat_tmp_script
      echo "$TAU_COEFF_FILE_SENSOR" >> Cat_tmp_script
      echo "1" >> Cat_tmp_script
      echo "EOF" >> Cat_tmp_script  
      chmod 700 Cat_tmp_script
      Cat_tmp_script
      rm -f Cat_tmp_script
	 
      TAU_COEFF_FILE1_SENSOR=$TAU_COEFF_FILE_SENSOR
      
    fi
  
  done

  # delete intermediary files

#  for GasName in $GasName_List; do

#    rm -f ${RESULT_DIR}/taucoef.${SatSen}.${GasName}.${PROF_SET}.nc 2>/dev/null

#  done

done

exit

