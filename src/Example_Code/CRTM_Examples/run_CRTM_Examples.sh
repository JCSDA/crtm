#!/bin/sh

# Check that the example code has been compiled
# and is executable by the user.
EXE_FILE="CRTM_Examples"
if [ ! -f ${EXE_FILE} ]; then
  echo; echo "  ${EXE_FILE} executable not found. Have you compiled the program?"; echo
  exit 1
else
  if [ ! -x ${EXE_FILE} ]; then
    echo; echo "  ${EXE_FILE} found, but it does not have execute permission. That's weird."; echo
    exit 1
  fi
fi

# Specify test sensor ids
SENSOR_ID="hirs4_n18, amsua_n18, mhs_n18"

# Specify the directory that holds coefficient data
COEFF_DIR="../Coefficient_Data/"

# Specify model type
# CRTM model type: 1 - Forward, 2 - Tangent-linear, 3 - Adjoint, 4 - K-Matrix
MODEL_ID="1 2 3 4"

# Specify test case 
#                        Ocean_ClearSky          = 1,   &
#                        Mixed_Surfaces          = 2,   &
#                        Land_1Cloud             = 3,   &
#                        Land_1Aerosol           = 4,   &
#                        Land_2Cloud_2Aerosol    = 5,   &
#                        ClearSky_2Prifles       = 6,   &
#                        Optional_Inputs_Outputs = 7
CASE_ID="1 2 3 4 5 6 7"

# Loop over test model types and test cases
#  echo "Running CRTM_Examples for the ${SID} instrument..."  
for MID in ${MODEL_ID}; do
  for CID in ${CASE_ID}; do

    ${EXE_FILE} <<NoMoreInput
${SENSOR_ID}
${COEFF_DIR}
${MID}
${CID}
NoMoreInput

  done
done
