#!/bin/sh

#####################################################################
# The script concatenate tau component into a single file
#    Written by Yong Han, July 8, 2008
#####################################################################

FILE_TYPE='nc'
#FILE_TYPE='bin'

#--- load functions read_parameters() and get_senInfo()

. tau_coeff.func

#----------------------------------------------------
#  Input and parse parameters from external file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET"

PARAM_FILE=$1  # parameter file
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
  GasName_List=`cat ${Sensor_DIR}/components.txt`
  
  #------------------------------
  # Loop over gases (components)
  #------------------------------

  TAU_COEFF_FILE_SENSOR=${RESULT_DIR}/taucoef.${SatSen}.${PROF_SET}.${FILE_TYPE}
  TAU_COEFF_FILE1_SENSOR=""

  for GasName in $GasName_List; do

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_${GasName}.txt"
    
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

    #-------------------------------------------------------------------------
    # 1. Check file existence (file will not be created if there is no significant
    # absorption from this tau component.                  
    # 2. Concatenate Tau coefficient files from different jobs into one   
    # along channel direction first, then along tau components             
    #----------------------------------------------------------------   

    TAU_COEFF_FILE=${RESULT_DIR}/taucoef.${SatSen}.${GasName}.${PROF_SET}.${FILE_TYPE}   
    TAU_COEFF_FILE1=""                                                                   

    for signalFileDIR in $DirList ;do                                                    

      TAU_FILE_IN=${signalFileDIR}/TauCoeff.${FILE_TYPE} 
                                                                                         
      if [ "$TAU_COEFF_FILE1" == "" ];then                                               

        TAU_COEFF_FILE1=${TAU_FILE_IN}                                                   
# add by ychen 03/02/09 for one channel sensor case
         cp ${TAU_COEFF_FILE1} ${TAU_COEFF_FILE}

      else                                                                               
                                                                                         
        TAU_COEFF_FILE2=${TAU_FILE_IN}                                                   

#        echo "${SCRIPT_DIR}/$EXE_FILE << EOF" > Cat_tmp_script                                         
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

    TAU_FILE_IN=$TAU_COEFF_FILE   
    
    if [ "$TAU_COEFF_FILE1_SENSOR" == "" ];then                                         

      TAU_COEFF_FILE1_SENSOR=$TAU_FILE_IN                                            
                                                                                        
    else                                                                                
                                                           
        TAU_COEFF_FILE2_SENSOR=$TAU_FILE_IN                
#        echo "${SCRIPT_DIR}/$EXE_FILE << EOF" > Cat_tmp_script           
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

  for GasName in $GasName_List; do

    rm -f ${RESULT_DIR}/taucoef.${SatSen}.${GasName}.${PROF_SET}.${FILE_TYPE} 2>/dev/null

  done

done

exit

