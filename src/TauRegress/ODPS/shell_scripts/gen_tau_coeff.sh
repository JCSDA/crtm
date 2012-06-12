#!/bin/sh -x

#-----------------------------------------------------------
#  Script name: gen_tau_coeff.sh
#
#  Purpose: compute transmittance coeffcients
#
#  Usage: gen_tau_coeff.sh CASE PARAM_FILENAME
#
#    
#
#  Inputs:
#
# 
#     PARAM_FILENAME - name of the file containing a list of
#                      parameters.   
#
#  Output: transmittance coefficients
#
#  Required files (read in):
#
#    Files with fixed names and must be stored in the direcotry
#    with this script.
#
#      sensor_list: a list of sensor names
#                   that are being processed.
#      SensorInof: sensor information
#
#  Developer: Yong Han, 2003/06/10
#  Modified : Yong Han, 2008/9/1 --- adapted to ODPS algorithm
#
#------------------------------------------------------------    

#--- load functions read_parameters() and get_senInfo()

. tau_coeff.func

#----------------------------------------------------
# Argument $1 is a filename. The file contains
# parameters used in this script. 
#----------------------------------------------------

PARAM_FILE=$1

#------------------------------------------------------
# Read in and parse parameters from the file $PARAM_FILE.
# The parameter name list PARAM_NAME_LIST includes
# some of the parameters that are being extracted from
# the file.  These parameters must be defined and
# assigned values in $PARAM_FILE.
#------------------------------------------------------

PARAM_NAME_LIST="MAX_CPUs CH_INT EXE_FILE WORK_DIR PROF_SET  "\
"SPC_COEFF_DIR TAU_PROFILE_DIR ATM_PROFLE_FILE "\
"GET_SEN_INFO COMPONENTS IR_TYPE COMPONENT_GROUP1 COMPONENT_GROUP2 "\
"COMPONENT_GROUP3" 

#--- read in and parse parameters 

read_parameters

#-----------------------  
# current directory
#-----------------------

SCRIPT_DIR=${PWD}

#------------------------------------------------
# check the existence of some of the input files
#------------------------------------------------

#--- File containing a list of sensor names.
#--- This script will loop over the sensor names.

SENSOR_LIST_FILENAME=${SCRIPT_DIR}/sensor_list
if [ ! -f ${SENSOR_LIST_FILENAME} ];then
  echo "The file ${SENSOR_LIST_FILENAME} does not exist in current directory"
  exit 1
fi

#--- get the sensor names from the sensor_list file

SENSOR_LIST=`awk '/BEGIN_LIST/ {
  while(getline){ 
    if(match($0,"END_LIST"))exit
    print $1} 
}' ${SENSOR_LIST_FILENAME}`


#--- the exe file

if [ ! -f ${EXE_FILE} ];then
  echo "The exe file ${EXE_FILE} does not exist"
  exit 1
fi

#--- the atmospheric profiles

if [ ! -f ${ATM_PROFLE_FILE} ];then
  echo "The atmospheric profile file ${ATM_PROFLE_FILE} does not exist"
  exit 1
fi

if [ ! -f ${GET_SEN_INFO} ]; then
  echo "The Fortran program ${GET_SEN_INFO} does not exist"
  exit 1
fi


#--- channel increment

ChInt=${CH_INT:-1}
 
#--- set profile tag

ProfSet=$PROF_SET
ConvSet=$PROF_SET

#----------------------
# Loop over sensors
#----------------------

BATCH_DIR=${WORK_DIR}/${ProfSet}
rm -f ${BATCH_DIR}/CoeffJob_*.sh 2>/dev/null
TotalJob_count=0

for SatSen in ${SENSOR_LIST}; do

  #--- work directory for current sensor

  Sensor_DIR=${WORK_DIR}/${ProfSet}/${SatSen}
  echo "Processing $Sensor_DIR"
  mkdir -p $Sensor_DIR
  
  #--- SpcCoeff.nc, TauProfile.nc files and TauCoef file that
  #    cotains tau coeff. for non-correction components

  SpcCoeffFile=${SPC_COEFF_DIR}/${SatSen}.SpcCoeff.nc
  TauProfileFile=${TAU_PROFILE_DIR}/upwelling.${SatSen}.TauProfile.nc
#  TauCoeffFile=${Sensor_DIR}/TauCoef.${SatSen}.${ProfSet}.bin
  TauCoeffFile=${Sensor_DIR}/TauCoef.${SatSen}.${ProfSet}.nc

  if [ ! -f ${TauProfileFile} ];then
    echo "The file ${TauProfileFile} does not exist"
    exit 1
  fi

  if [ ! -f ${SpcCoeffFile} ];then
    echo "The file ${SpcCoeffFile} does not exist"
    exit 1
  fi

  # --- Call the fortran program to return parameters from the SpcCoeff file
  SEN_INFO_LIST=`${GET_SEN_INFO}<<EOF
${SpcCoeffFile}
EOF`
  Sensor_Type=`echo $SEN_INFO_LIST | cut -d" " -f1`
  Nchan=`echo $SEN_INFO_LIST | cut -d" " -f2`

  # --- Get the tau components to be processed
  if [ "${COMPONENTS}" == "DEFAULT" ]; then
    if [ ${Sensor_Type} == 1 ]; then  # MW sensors
      GasName_List=$COMPONENT_GROUP3
    else  # IR sensors
      if [ ${IR_TYPE} -eq 1 ]; then
         GasName_List=$COMPONENT_GROUP1
      else
         GasName_List=$COMPONENT_GROUP2
      fi
    fi
    GasName_List=`echo $GasName_List | awk -F, '{for(i=1;i<=NF;i++)print $i}'`
  else
    #--- parse gas name list
    GasName_List=`echo $COMPONENTS | awk -F, '{for(i=1;i<=NF;i++)print $i}'`
  fi

  echo $GasName_List > ${Sensor_DIR}/components.txt

  # --- Get the full algorithm-associated components and assign group ID
  if [ ${Sensor_Type} == 1 ]; then
    COMPONENT_LIST=$COMPONENT_GROUP3
    GROUP_ID=3
  else
    if [ ${IR_TYPE} -eq 1 ]; then
      COMPONENT_LIST=$COMPONENT_GROUP1
      GROUP_ID=1
    else
      COMPONENT_LIST=$COMPONENT_GROUP2
      GROUP_ID=2
    fi
  fi
  COMPONENT_LIST=`echo $COMPONENT_LIST | awk -F, '{for(i=1;i<=NF;i++)print $i}'`

  #--- when computing components such as dry, wet and ozo, the
  #    file $TauCoeffFile does not exist
  
  if [ ! -f ${TauCoeffFile} ];then
    TauCoeffFile="NotExist"
  fi

  #--- number of angles

  case $SenName in
   
    IMAGER|SOUNDER) Nincang=7;;
     
     *) Nincang=7;;
     
  esac

  #-----------------------------------------------------------
  #  calculate the number of jobs per sensor 
  #-----------------------------------------------------------

  Job_count=0   # count number of jobs
#  Ngas=`echo $GasName_List | awk '{print NF}'`
#  jobs_sensor=`echo $Nchan $ChInt | awk '{n=$1/$2;if(n-int(n)!=0)n=int(n)+1;print n}'`
#  jobs_sensor=`echo $jobs_sensor $Ngas | awk '{print ($1 * $2)}'`

  #----------------------
  # Loop over gases
  #----------------------

  for GasName in $GasName_List; do

    # figure out the component index by matching the component name $GasName
    # with one of the item in the component list $COMPONENT_LIST
    idx=0
    for item in $COMPONENT_LIST;do
      idx=`expr $idx + 1`
      if [ "$item" == "$GasName" ]; then
        COMP_IDX=$idx
      fi
    done

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_${GasName}.txt"
    rm -f $ExecutionDirFile 2>/dev/null
 
    #----------------------
    # Loop over channels
    #----------------------

    TopSeqCh=1   # starting channel number 
    EndChan=`expr $TopSeqCh + $Nchan - 1`  # ending channel number
  
    while [ $TopSeqCh -le $EndChan ]; do

      #--- set starting and ending channel numbers for a job (ChInt channels)
      LastSeqCh=`expr $TopSeqCh + $ChInt - 1`
      if [ $LastSeqCh -gt $EndChan ]; then
        LastSeqCh=$EndChan
      fi
    
      #--- patch "0" if the number has less than three digits

      TopSeqCh=`echo ${TopSeqCh} 000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`
      LastSeqCh=`echo ${LastSeqCh} 000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`

      #--- set a tag for current process

      ProcSuffix=${SatSen}.${GasName}.ch${TopSeqCh}to${LastSeqCh}.${ProfSet} 
    
      #--- create and go to execution directory

      RunDir=${Sensor_DIR}/${GasName}/${ProcSuffix}
      mkdir -p $RunDir
      cd ${RunDir}

             
      #--- delete the completion signal file
      rm -f *signal.txt 2>/dev/null

      #--- create namelist file

cat << EOF > Namelist.txt

    &SATSEN
      File_Prefix  = "$SatSen",
      Ichan_start    =  $TopSeqCh,
      Ichan_end   =  $LastSeqCh 
/
    &GENCOEF
      icom = $COMP_IDX,
      Nangle_regression    = $Nincang      
/
    &FILENAMES
      inFilename_spcCoef = "$SpcCoeffFile",
      inFilename_atmProfile = "$ATM_PROFLE_FILE",
      inFilename_tauProfile = "$TauProfileFile"
      inFilename_tauCoeff = "$TauCoeffFile"
/      
EOF

      #-------------------------------------------------------
      #  Execute jobs
      #-------------------------------------------------------

     #--- move back to where the batch files are

      cd ${BATCH_DIR}
  
      Job_count=`expr $Job_count + 1`                                       
      TotalJob_count=`expr $TotalJob_count + 1`                             
                                                                            
      runningJobs=`ls CoeffJob_*.sh 2>/dev/null | wc -l`                    

      total_seconds=0                                                       

      while [ $runningJobs -ge $MAX_CPUs ]; do                              

        sleep 1                                                             
                                                                            
        total_seconds=`expr $total_seconds + 1`                             
       #--- terminate process if the job has waited for                     
       #--- a day (20864 seconds) for execution.                            
        if [ $total_seconds -gt 20864 ]; then                               
                                                                            
           echo "No job has been sent out since a day ago" >> CoeffJob.log  
           echo "current waiting job: ${Job_count}"                         
           echo "this script is terminated!" >> CoeffJob.log                
           exit 1                                                           
                                                                            
        fi                                                                  
                                                                            
        runningJobs=`ls CoeffJob_*.sh 2>/dev/null | wc -l`                  
                                                                            
      done                                                                  

      jobScript=CoeffJob_${TotalJob_count}.sh                               

      Error_file_id=${SatSen}.${GasName}.ch${TopSeqCh}
      
      echo "#!/bin/sh --login" > ${jobScript}                                                   
      echo "#PBS -N TauCoeff_run" >> ${jobScript}                                 
      echo "#PBS -o \$(job_name).\$(jobid).out" >> ${jobScript}                    
      echo "#PBS -e \$(job_name).${Error_file_id}.err" >> ${jobScript}                     
      echo "#PBS -M ${USER}@noaa.gov" >> ${jobScript} 
      echo "#PBS -m a" >> ${jobScript}                                    
#      echo "#PBS -q batch" >> ${jobScript}                                       
      echo "#PBS -l walltime =01:00:00" >> ${jobScript}                             
      echo "#PBS -A ada" >> ${jobScript}                                          
      echo "#PBS -l mem=500M" >> ${jobScript}                                          
      echo "# " >> ${jobScript}                                                         
      echo "# Load required modules" >> ${jobScript}
      echo "module load intel/12-12.0.4.191" >> ${jobScript}
      echo "module load netcdf/4.1.3-intel" >> ${jobScript}
      echo "# " >> ${jobScript}                                                         
      echo "# go to the execution directory" >> ${jobScript}                            
      echo "cd ${RunDir}" >> ${jobScript}                                               
      echo "# execute ${EXE_FILE}" >> ${jobScript}                                      
      echo "${EXE_FILE}<<EOF > $RunDir/stdout.${ProcSuffix}.txt" >> ${jobScript}        
      echo "${GROUP_ID}" >> ${jobScript}                                                
      echo "EOF" >> ${jobScript}                                                        
      echo "# kill the stdout file if the excution is successful" >> ${jobScript}       
      echo "if [ -f Completion_signal.txt ];then" >> ${jobScript}                       
      echo "  rm -f stdout.*.txt" >> ${jobScript}                                       
      echo "fi" >> ${jobScript}                                                         
      echo "# kill the jobscript" >> ${jobScript}                                       
      echo "cd ${BATCH_DIR}" >> ${jobScript}                                            
      echo "rm -f ${jobScript}" >> ${jobScript}                                         
      echo "echo End of the job: $Job_count >> CoeffJob.log" >> ${jobScript}            

      # -- execute the job script                                                       

      chmod 700 $jobScript  
#      qsub $jobScript   

      #--- put the execution directory in a file

      echo $RunDir >> $ExecutionDirFile
	  		  	
      TopSeqCh=`expr $TopSeqCh + $ChInt`

    done  # channel loop

  done  # gas loop

done  # sensor loop

exit
