#!/bin/sh 

#-----------------------------------------------------------
#  Script name: gen_optran_coeff.sh
#
#  Purpose: compute optimal OPTRAN coeffcients
#
#  Usage: gen_optran_coeff.sh CASE PARAM_FILENAME
#
#    
#
#  Inputs:
#
#     CASE == ALLCOM      indicate exhaustive search
#          == P1SEL       indicate stepwise search
# 
#     PARAM_FILENAME - name of the file containing a list of
#                      parameters.   
#
#  Output: OPTRAN transmittance coefficients
#
#  Required files (read in):
#
#    Files with fixed names and must be stored in the direcotry
#    with this script.
#
#      sensor_list: a list of sensor names
#                   that are being processed.
#      Alpha_list: data of alpha, c0 & c1 
#                  for converting vertical coordinates.
#      SensorInof: sensor information
#      predflag.all.txt: initial predictor flags
#
#    Files whose names and locations are variables
#  
#      spectral coefficient file
#      file containing atmospheric profile
#      file containting transmittance profiles
#    
#  Developer: Yong Han, 2003/06/10
#------------------------------------------------------------    

#--- load functions read_parameters() and get_senInfo()

. optran_coeff.func

#----------------------------------------------------
#  Argument $1 gives the case. CASE == ALLCOM or P1SEL
#----------------------------------------------------

CASE=$1

if [ "$CASE" != "ALLCOM" -a "$CASE" != "P1SEL" ];then

  echo "argument 1 must be either ALLCOM or P1SEL"
  exit 1
  
fi

#----------------------------------------------------
# Argument $2 is a filename. The file contains
# parameters used in this script. 
#----------------------------------------------------

PARAM_FILE=$2

#------------------------------------------------------
# Read in and parse parameters from the file $PARAM_FILE.
# The parameter name list PARAM_NAME_LIST includes
# some of the parameters that are being extracted from
# the file.  These parameters must be defined and
# assigned values in $PARAM_FILE.
#------------------------------------------------------

PARAM_NAME_LIST="MAX_CPUs CH_INT EXE_FILE WORK_DIR PROF_SET GAS_NAME_LIST  "\
"SPC_COEFF_DIR TAU_PROFILE_DIR ATM_PROFLE_FILE SENSOR_INFO_FILE" 


#--- read in and parse parameters 

read_parameters

#-----------------------  
# current directory
#-----------------------

SCRIPT_DIR=${PWD}

#------------------------------------------------
# check the existence of some of the input files
#------------------------------------------------

#--- alpha file that contains alpha, C0, C1 for converting vertical coordinates

ALPHA_FILE=${SCRIPT_DIR}/Alpha_list

if [ ! -f ${ALPHA_FILE} ]; then
  echo "The alpha file ${ALPHA_FILE} does not exist in current directory"
  exit 1
fi

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

#--- The sensorInfo file

#SENSOR_INFO=${SCRIPT_DIR}/SensorInfo
SENSOR_INFO=$SENSOR_INFO_FILE
if [ ! -f ${SENSOR_INFO} ];then
  echo "The file ${SENSOR_INFO} does not exist in current directory"
  exit 1
fi

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

#--- channel increment

ChInt=${CH_INT:-1}
 
#--- parse gas name list and put the gas names into another string 
#--- delimitated by spaces

GasName_List=`echo $GAS_NAME_LIST | awk -F, '{for(i=1;i<=NF;i++)print $i}'`

#--- set profile tag

ProfSet=$PROF_SET
ConvSet=$PROF_SET

#-----------------------------------------------------------
#  calculate the number of jobs per batch file. 
#-----------------------------------------------------------

TotalJobs=0
TotalChan=0

for SatSen in ${SENSOR_LIST}; do
  #--- call function to get the number of channels on the sensor, Nchan

  get_senInfo

  Ngas=`echo $GasName_List | awk '{print NF}'`
  jobs_sensor=`echo $Nchan $ChInt | awk '{n=$1/$2;if(n-int(n)!=0)n=int(n)+1;print n}'`
  jobs_sensor=`echo $jobs_sensor $Ngas | awk '{print ($1 * $2)}'`
  TotalJobs=`expr $TotalJobs + $jobs_sensor`
  Chan_sensor=`echo $Nchan $Ngas | awk '{print ($1 * $2)}'`
  TotalChan=`expr $TotalChan + $Chan_sensor`
done

JobsPerBatch=`echo $TotalJobs $MAX_CPUs | awk '{n=$1/$2;if(n-int(n)!=0)n=int(n)+1;print n}'`

#----------------------
# Loop over sensors
#----------------------

ChanCount=0        # count total number of channels
BatchJob_count=0   # count number jobs in a batch file
TotalJob_count=0   # count total number of jobs
NumCPUsUsed=0      # count number of CPUs used
BatchNum=1         # tag of batch files

BATCH_DIR=${WORK_DIR}/${ProfSet}

for SatSen in ${SENSOR_LIST}; do

  #--- call function to get SatName, SenName, and Nchan

  get_senInfo

  #--- work directory for current sensor

  Sensor_DIR=${WORK_DIR}/${ProfSet}/${SatSen}
  echo "Processing $Sensor_DIR"

  #--- SpcCoeff.nc and TauProfile.nc files

  SpcCoeffFile=${SPC_COEFF_DIR}/${SatSen}.SpcCoeff.nc
#  TauProfileFile=${TAU_PROFILE_DIR}/upwelling.${SatSen}.Effective.TauProfile.nc
  TauProfileFile=${TAU_PROFILE_DIR}/upwelling.${SatSen}.TauProfile.nc

  if [ ! -f ${SpcCoeffFile} ];then
    echo "The file ${SpcCoeffFile} does not exist"
    exit 1
  fi

  if [ ! -f ${TauProfileFile} ];then
    echo "The file ${TauProfileFile} does not exist"
    exit 1
  fi

  #--- number of angles

  case $SenName in
   
    IMAGER|SOUNDER) Nincang=7;;
     
     *) Nincang=6;;
     
  esac


  #----------------------
  # Loop over gases
  #----------------------

  for GasName in $GasName_List; do

   #--- Set PredFlag filename

    case $CASE in

      ALLCOM) Ncomb=6
              PredFlag=all
              PredFlagInFile=${SCRIPT_DIR}/predflag.all.txt;;

      P1SEL)  Ncomb=1
              PredFlag=selected
              PredFlagInFile=${Sensor_DIR}/predflag.${SatSen}.${GasName}.selected.${ProfSet}.txt;;

      *) echo "Error: CASE is neither ALLCOM nor P1SEL"
         exit 1;;

    esac

    if [ ! -f ${PredFlagInFile} ];then
      echo "The file ${PredFlagInFile} does not exist"
      exit 1
    fi

    #--- file hoding a list of execution directories

    ExecutionDirFile="${Sensor_DIR}/executionDir_${GasName}_${PredFlag}"
    rm -f $ExecutionDirFile 2>/dev/null

 
    #----------------------
    # Loop over channels
    #----------------------

    TopSeqCh=1   # starting channel number 
    EndChan=`expr $TopSeqCh + $Nchan - 1`  # ending channel number

#    EndChan=`expr $TopSeqCh + 20 - 1`  # ending channel number

    while [ $TopSeqCh -le $EndChan ]; do

      #--- set starting and ending channel numbers for a job (ChInt channels)
      LastSeqCh=`expr $TopSeqCh + $ChInt - 1`
      if [ $LastSeqCh -gt $EndChan ]; then
        LastSeqCh=$EndChan
      fi
    
      #--- count the number of channels processed so far

      NumChan=`expr $LastSeqCh - $TopSeqCh + 1`
      ChanCount=`expr $ChanCount + $NumChan`

      #--- read alpha, c0, and c1 for converting vertical coordinates.

      Alpha_data=`grep ${ConvSet} ${ALPHA_FILE} | grep ${GasName}`

      GAS_ID=`echo $Alpha_data | cut -d',' -f 3`
      ALPHA=`echo $Alpha_data | cut -d',' -f 4`
      C0=`echo $Alpha_data | cut -d',' -f 5`
      C1=`echo $Alpha_data | cut -d',' -f 6`

      #--- patch "0" if the number has less than three digits

      TopSeqCh=`echo ${TopSeqCh} 0000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`
      LastSeqCh=`echo ${LastSeqCh} 0000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`
#      TopSeqCh=`echo ${TopSeqCh} 000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`
#      LastSeqCh=`echo ${LastSeqCh} 000 | awk '{print substr($2,1,length($2)-length($1)) $1}'`

      #--- set a tag for current process

      ProcSuffix=${SatSen}.${GasName}.ch${TopSeqCh}to${LastSeqCh}.pcomb${Ncomb}${PredFlag}.${ProfSet} 
    
      #--- create and go to execution directory

      RunDir=${Sensor_DIR}/${ProcSuffix}
      mkdir -p $RunDir
      cd ${RunDir}
 
      #--- delete the completion signal file
      rm -f *signal.txt 2>/dev/null
      
      #--- create namelist file

      if [ ! -f ${TauProfileFile} ];then
        echo "The file ${TauProfileFile} does not exist"
        exit 1
      fi

      Flag_netCDF=.true.
      Flag_Lev2Lay=.true.

cat << EOF > Namelist.txt
    &SATSEN
      SatName      = "$SatName",
      SenName      = "$SenName",
      Ichan_top    =  $TopSeqCh,
      Ichan_last   =  $LastSeqCh 
/
    &GENCOEF
      Iabsorber = $GAS_ID,
      Alpha     = $ALPHA,
      A0        = $C0,
      A1        = $C1,
      Nangle    = $Nincang,
      Natmpred_allcombsearch  = $Ncomb,
      Flag_netCDF_file        = $Flag_netCDF,
      Flag_AtmProfile_Lev2Lay = $Flag_Lev2Lay 
/
EOF

      #--- Link files to fixed names
    
      ln -sf $SENSOR_INFO			SenInfo.txt
      ln -sf $SpcCoeffFile		        SpcCoef.nc
      ln -sf $ATM_PROFLE_FILE			AtmProfile.nc
      ln -sf $PredFlagInFile   			AtmPredFlag.txt
      ln -sf $TauProfileFile			TauProfile.nc

      #--- (out file) trans coef table

      ln -sf taucoef.${ProcSuffix}.nc		TauCoeff.nc

      #--- (out file) selected predictor sets

      ln -sf predflag.${ProcSuffix}.txt	fort.31

      #--- (out file) statistical results

      ln -sf stat.${ProcSuffix}.txt	fort.40		# Stat for ch
#      ln -sf tb_stat.${ProcSuffix}.txt	fort.41		# Tb stat for prof

      #--- (out file) layer and level values for monitoring

#      ln -sf monit_layer.${ProcSuffix}.txt		fort.43
#      ln -sf monit_level.${ProcSuffix}.txt		fort.44
   
      #-------------------------------------------------------
      #  Execute or batch-execute jobs
      #-------------------------------------------------------

     #--- move back to where the batch files are

      cd ${BATCH_DIR}
  
      if [ $TotalJob_count -eq 0 ]; then	
	  rm -f optranCoeffJob_*.sh 2>/dev/null	  
	  echo "total # of jobs = " $TotalJobs > optranCoeffJob.log
      fi
	
      TotalJob_count=`expr $TotalJob_count + 1`
		
      runningJobs=`ls optranCoeffJob_*.sh 2>/dev/null | wc -l`
	
      total_seconds=0
	
      while [ $runningJobs -ge $MAX_CPUs ]; do
	
	sleep 1
	  
	total_seconds=`expr $total_seconds + 1`
	#--- terminate process if the job has waited for
	#--- a day (20864 seconds) for execution.
	if [ $total_seconds -gt 20864 ]; then
	  
	   echo "No job has been sent out since a day ago" >> optranCoeffJob.log
	   echo "current waiting job: ${TotalJob_count}"
	   echo "this script is terminated!" >> optranCoeffJob.log
	   exit 1
	     
	fi
	  
	runningJobs=`ls optranCoeffJob_*.sh 2>/dev/null | wc -l`
	  
      done
	
      jobScript=optranCoeffJob_$TotalJob_count.sh

      echo "#!/bin/sh" > ${jobScript}
      echo "#@ job_name = optranC_run" >> ${jobScript}
#      echo "#@ output = \$(job_name).\$(jobid).out" >> ${jobScript}
      echo "#@ error  = \$(job_name).\$(jobid).err" >> ${jobScript}
      echo "#@ notification = error" >> ${jobScript}
      echo "#@ job_type = serial" >> ${jobScript}
      echo "#@ wall_clock_limit = 01:00:00" >> ${jobScript}
      echo "#@ class = jcsda" >> ${jobScript}
#      echo "#@ class = 1" >> ${jobScript}
      echo "#@ group = jcsda" >> ${jobScript}
      echo "#@ account_no = JCSDA001-RES" >> ${jobScript}
      echo "#@ resources = ConsumableCpus(1) ConsumableMemory(100 MB)" >> ${jobScript}
      echo "#@ queue" >> ${jobScript}
      echo "# " >> ${jobScript}
      echo "# go to the execution directory" >> ${jobScript}
      echo "cd ${RunDir}" >> ${jobScript}
      echo "# execute ${EXE_FILE}" >> ${jobScript}
      echo "${EXE_FILE} > $RunDir/stdout.${ProcSuffix}.txt" >> ${jobScript}
      echo "# kill the stdout file if the excution is successful" >> ${jobScript}
      echo "if [ -f Completion_signal.txt ];then" >> ${jobScript}
      echo "  rm -f stdout.*.txt" >> ${jobScript} 
      echo "fi" >> ${jobScript}    
      echo "# kill the jobscript" >> ${jobScript}
      echo "cd ${BATCH_DIR}" >> ${jobScript}
      echo "rm -f ${jobScript}" >> ${jobScript}
      echo "echo End of the job: $TotalJob_count >> optranCoeffJob.log" >> ${jobScript}

      # -- execute the job script

      chmod 700 $jobScript
      llsubmit $jobScript

      #--- put the execution directory in a file

      echo $RunDir >> $ExecutionDirFile
	  		  	
      TopSeqCh=`expr $TopSeqCh + $ChInt`

    done  # channel loop

  done  # gas loop

done  # sensor loop

exit
