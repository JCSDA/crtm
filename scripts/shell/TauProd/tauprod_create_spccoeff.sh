#!/bin/bash

#
# Bash script to run the Create_SpcCoeff application for all the required
# sensors for which there are oSRF datafiles in this directory hierarchy.
#
# Format of the "create_spccoeff.config" include file is an array of descriptors
# including the sensor id and the final SpcCoeff data version number using ":" as
# the delimiter, e.g.:
#
# CONFIG=( [1]=sensor1_platform1:1
#          [2]=sensor1_platform2:4
#          ...
#          [I]=sensorN_platformK:2 )
#

# Check for existence of SensorInfo
if [ ! -f SensorInfo ]; then
  echo "SensorInfo file not found"
  exit 1
fi


# Link in the solar files
link_solar.sh
link_solar.sh dF_0.0025.Solar.nc


# Define the sensors to be processed and their **NEW** data version number
source ./tauprod_create_spccoeff.config


# Loop over the sensors to convert
for (( element = 1 ; element <= ${#CONFIG[@]} ; element++ )) do

  # Extract out sensor id and data version
  sensor_id=`echo ${CONFIG[$element]} | cut -d: -f1`
  version=`echo ${CONFIG[$element]} | cut -d: -f2`

  # Create data filename
  osrf_filename="${sensor_id}.osrf.nc"

  # Run the conversion program
  Create_SpcCoeff <<-EOF
	${sensor_id}/${osrf_filename}
	${version}
	EOF

done


# Clean up
rm -f *.Solar.nc *.signal
