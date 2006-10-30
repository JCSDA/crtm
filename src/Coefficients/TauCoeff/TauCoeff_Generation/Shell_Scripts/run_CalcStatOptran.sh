WORK_DIR=/disk5/pub/yhan/TransCoeff_work
EXE_FILE_BIN=${HOME}/gen_tau_coeff/bin
SPC_COEFF_DIR=${HOME}/Transmittances/seninfo
TAU_PROFILE_DIR=${HOME}/Transmittances/LBLRTM_Trans/Paulv/UMBC48101
ATM_PROFLE_FILE=${HOME}/Transmittances/Atm_Profiles/UMBC48101/AtmProfile.nc
TAU_COEFF_DIR=${WORK_DIR}/results
PROF_SET=umbc48101
DIR_NAME_PATTERN='airsM*_aqua'
STAT_DIR=${WORK_DIR}/statistics/stat_recomputed

ScrptDir=${PWD}

cd ${WORK_DIR}/${PROF_SET}
SENSOR_LIST=`ls -d ${DIR_NAME_PATTERN}`
cd ${ScrptDir}

for senSat in $SENSOR_LIST;do

  echo $senSat
  
  mkdir -p ${WORK_DIR}/tmp_dir
  cd ${WORK_DIR}/tmp_dir

  mkdir -p $STAT_DIR
      
  ${EXE_FILE_BIN}/CalcStatOptran << EOF > log.dat
${SPC_COEFF_DIR}/${senSat}.SpcCoeff.nc
${ATM_PROFLE_FILE}
${TAU_PROFILE_DIR}/upwelling.${senSat}.TauProfile.nc
${TAU_COEFF_DIR}/taucoef.${senSat}.umbc48101.nc
6
EOF
  
  mv fort.40 ${STAT_DIR}/stat.${senSat}.tot.${PROF_SET}.txt  
  mv fort.90 ${STAT_DIR}/stat.${senSat}.dry.${PROF_SET}.txt  
  mv fort.91 ${STAT_DIR}/stat.${senSat}.wet.${PROF_SET}.txt  
  mv fort.92 ${STAT_DIR}/stat.${senSat}.ozo.${PROF_SET}.txt  
  
done

exit

