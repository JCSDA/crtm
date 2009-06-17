#!/bin/sh

#####################################################################
# The script first convert the netCDF format to binary format, then
# compare the LBL results with the the CRTM results with training 
# transmittance coefficients
#    Written by Yong Han, July 8, 2008
#    Modified by Yong Chen, March 2, 2009
#####################################################################

#FILE_TYPE='nc'
FILE_TYPE='bin'

#--- load functions read_parameters() and get_senInfo()

. tau_coeff.func

#----------------------------------------------------
#  Input and parse parameters from external file
#----------------------------------------------------

PARAM_NAME_LIST="WORK_DIR PROF_SET SPC_COEFF_DIR TAU_PROFILE_DIR ATM_PROFLE_FILE IR_TYPE"

PARAM_FILE=$1 # parameter file
EXE_FILE=$2  # exe file to compute fitting error
OPTRAN=$3

#--- read in parameters from $PARAM_FILE and assign their 
#--- values to the variables whose names are listed in
#--- $PARAM_NAME_LIST 

read_parameters

#-----------------------  
# current directory
#-----------------------

SCRIPT_DIR=${PWD}

NC2BIN_dir=/jcsda/save/wx23yc/CRTM_svn/branches/src/EXP-Multiple_Algorithm/Coefficients/TauCoeff/ODPS/ODPS_NC2BIN  

# directory containing the tau coefficient files
TAU_COEFFE_DIR=${WORK_DIR}/results
#TAU_COEFFE_DIR=/jcsda/noscrub/wx23yh/work_umbc_mix3/results
# directory containign the final results
Fit_ERROR_DIR=${WORK_DIR}/statistics/stat_${PROF_SET}

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


for sensor in $SENSOR_LIST;do

  echo '<<< processing ' ${sensor} ' >>>'
  SpcCoeff_file=${SPC_COEFF_DIR}/${sensor}.SpcCoeff.nc
  TauProfile_file=${TAU_PROFILE_DIR}/upwelling.${sensor}.TauProfile.nc
#  TauProfile_file=/jcsda/noscrub/wx23yc/TauProfile_Angle/ECMWF83_CFC_wco_correction/upwelling.${sensor}.TauProfile.nc
#  FitError_file=${Fit_ERROR_DIR}/FitErr.${sensor}.txt

  TauCoeff_file=${sensor}.TauCoeff.bin
  if [ ${OPTRAN} == 1 ]; then
# first convert TauCoeff file from netCDF to binary format
${NC2BIN_dir}/ODPS_NC2BIN<<EOF1
${TAU_COEFFE_DIR}/taucoef.${sensor}.ODPS_OPTRAN.${PROF_SET}.nc
${TAU_COEFFE_DIR}/taucoef.${sensor}.ODPS_OPTRAN.${PROF_SET}.bin
EOF1
    ln -s ${TAU_COEFFE_DIR}/taucoef.${sensor}.ODPS_OPTRAN.${PROF_SET}.bin $TauCoeff_file
    FitError_file=${Fit_ERROR_DIR}/FitErr.${sensor}.txt-ODPS-OPTRAN_g${IR_TYPE}
  else
# first convert TauCoeff file from netCDF to binary format
${NC2BIN_dir}/ODPS_NC2BIN<<EOF2
${TAU_COEFFE_DIR}/taucoef.${sensor}.${PROF_SET}.nc
${TAU_COEFFE_DIR}/taucoef.${sensor}.${PROF_SET}.bin
EOF2
    ln -s ${TAU_COEFFE_DIR}/taucoef.${sensor}.${PROF_SET}.bin $TauCoeff_file  
    FitError_file=${Fit_ERROR_DIR}/FitErr.${sensor}.txt-ODPS_g${IR_TYPE}
  fi 

${EXE_FILE}<<EOF
${SpcCoeff_file}
${ATM_PROFLE_FILE}  
${TauProfile_file}
${sensor}
${FitError_file}
EOF

rm -f $TauCoeff_file
done

exit
