#!/bin/sh

# Check that the example code has been compiled
# and is executable by the user.
EXE_FILE="Example1_Simple"
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
SENSOR_ID="amsua_metop-a mhs_n18 hirs4_n18 ssmis_f16 v.seviri_m09 hirs3_n17 amsre_aqua"

# Initialise counters
N_PASS=0
N_FAIL=0

# Loop over test sensors
echo
echo "Running Example_K_Matrix for all test instruments..."
echo "----------------------------------------------------"
for SID in ${SENSOR_ID}; do
  # Delete the sensor signal file
  SIGNALFILE="Results/${SID}.signal"
  if [ -f ${SIGNALFILE} ]; then
    rm -f ${SIGNALFILE}
  fi
  echo
  # Run the example code
  OUTFILE="${SID}.K_Matrix.output"
  ${EXE_FILE} <<-NoMoreInput > ${OUTFILE}
	${SID}
	NoMoreInput
  if [ $? -ne 0 ]; then
    echo "  Run failed for the ${SID} instrument!"
    ((N_FAIL = N_FAIL + 1))
    continue
  fi
  # Check the output results
  echo "Checking results for the ${SID} instrument..."
  if [ -f ${SIGNALFILE} ]; then
    echo "  The results are the same!"
    ((N_PASS = N_PASS + 1))
    rm -f ${SIGNALFILE} > /dev/null
  else
    echo "  The results are different! Check the output in ${OUTFILE}"
    ((N_FAIL = N_FAIL + 1))
  fi
  ((N_TOTAL = N_PASS + N_FAIL))
done

# Output comnparison results
if [ ${N_FAIL} -gt 0 ]; then
  WARNING="  <----<<<  **WARNING**"
else
  WARNING=""
fi
echo
echo "  ======================="
echo "  SUMMARY of test results"
echo "  ======================="
echo
echo "  Passed ${N_PASS} of ${N_TOTAL} tests."
echo "  Failed ${N_FAIL} of ${N_TOTAL} tests.${WARNING}"
echo
