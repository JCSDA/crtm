#!/bin/sh -x

#parameters
#--- the exe file
EXE_FILE=ODAS_NC2BIN.big_endian
# Input directory
dir_in=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/PW/netCDF
#dir_in=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/ORD-PW/netCDF
#dir_in=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/ORD/netCDF
#dir_in=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Microwave/netCDF
# out directory
dir_out=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/PW/Big_Endian
#dir_out=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/ORD-PW/Little_Endian
#dir_out=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Infrared/ORD/Little_Endian
#dir_out=/state/partition1/home/ychen/trunk/fix/TauCoeff/ODAS/Microwave/Big_Endian
SENSOR_LIST_FILENAME=sensor_list

mkdir -p ${dir_out}

#--- get the sensor names from the sensor_list file
SENSOR_LIST=`awk '/BEGIN_LIST/ {
  while(getline){ 
    if(match($0,"END_LIST"))exit
    print $1} 
}' ${SENSOR_LIST_FILENAME}`

echo ${SENSOR_LIST}
if [ ! -f ${EXE_FILE} ];then
  echo "The exe file ${EXE_FILE} does not exist"
  exit 1
fi

for SatSen in ${SENSOR_LIST}; do

  #--- work directory for current sensor
  Sensor_DIR=${dir_in}
  echo "Processing $Sensor_DIR"
#  mkdir -p $Sensor_DIR
  TauCoeffFile=${Sensor_DIR}/${SatSen}.TauCoeff.nc
  if [ ! -f ${TauCoeffFile} ];then
    echo "The file ${TauCoeffFile} does not exist"
    exit 1
  fi
#  ln -sf ${TauCoeffFile} ${SatSen}.TauCoeff.nc
  TauCoeffBIN=${dir_out}/${SatSen}.TauCoeff.bin
  
  # run convert ODAS_NC2BIN
${EXE_FILE}<<EOF
${TauCoeffFile}
${TauCoeffBIN}
EOF
 
done

exit
  
