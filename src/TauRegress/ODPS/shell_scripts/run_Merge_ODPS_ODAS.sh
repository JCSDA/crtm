#!/bin/sh

#--- load functions read_parameters() and get_senInfo()

. optran_coeff.func

#----------------------------------------------------
#  Input and parse parameters from external file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET SPC_COEFF_DIR GET_SEN_INFO"

PARAM_FILE=tau_coeff.parameters

# Merge program
EXE_FILE=/home/Paul.Vandelst/CRTM/trunk/src/TauRegress/ODPS/Merge_ODPS_ODAS/Merge_ODPS_ODAS

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


for SatSen in ${SENSOR_LIST}; do

  echo 'Processing ' $SatSen

  SpcCoeffFile=${SPC_COEFF_DIR}/${SatSen}.SpcCoeff.nc
  if [ ! -f ${SpcCoeffFile} ];then
    echo "The file ${SpcCoeffFile} does not exist"
    exit 1
  fi

  # --- Call the fortran program to return parameters from the SpcCoeff file
  SEN_INFO_LIST=`${GET_SEN_INFO}<<EOF
${SpcCoeffFile}
EOF`
  Sensor_Type=`echo $SEN_INFO_LIST | cut -d" " -f1`

  if [ ${Sensor_Type} == 1 ]; then  # MW sensors
    CompName=wet
  else
    CompName=wlo
  fi


#  ODPS_file=${WORK_DIR}/results/taucoef.${SatSen}.${PROF_SET}.bin
  ODPS_file=${WORK_DIR}/results/taucoef.${SatSen}.${PROF_SET}.nc
  ODPS_errfile=${WORK_DIR}/statistics/stat_${PROF_SET}/stat.${SatSen}.${CompName}.${PROF_SET}.txt
  OPTRAN_file=${WORK_DIR}/results/taucoef.${SatSen}.${CompName}.${PROF_SET}.optran.nc
  OPTRAN_errfile=${WORK_DIR}/statistics/stat_${PROF_SET}/stat.${SatSen}.${CompName}.${PROF_SET}.optran.txt
#  file_out=${WORK_DIR}/results/taucoef.${SatSen}.ODPS_OPTRAN.${PROF_SET}.bin
  file_out=${WORK_DIR}/results/taucoef.${SatSen}.ODPS_OPTRAN.${PROF_SET}.nc

${EXE_FILE}<<EOF
$ODPS_file
$ODPS_errfile
$OPTRAN_file
$OPTRAN_errfile
$file_out
EOF

done

exit
