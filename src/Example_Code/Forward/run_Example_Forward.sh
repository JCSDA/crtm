#!/bin/sh

# Check that the example code has been compiled
# and is executable by the user.
EXE_FILE="Example_Forward"
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
SENSOR_ID="ssu_n14 zssmis_f16 hirs4_n18 amsua_n18 mhs_n18"

# Loop over test sensors
for SID in ${SENSOR_ID}; do
  OUTFILE="${SID}.Forward.output"
  echo
  # Run the example code
  echo "Running Example_Forward for the ${SID} instrument..."
  ${EXE_FILE} -Wl,-T <<-NoMoreInput > ${OUTFILE}
	${SID}
	NoMoreInput
  # Diff the outputs
  echo "Diff'ing the result files for the ${SID} instrument..."
  diff Results/${OUTFILE} ${OUTFILE} > ${OUTFILE}.diff
  # Check the file size...
  OUTFILE_SIZE=`ls -l ${OUTFILE}.diff | awk '{print $5}'`
  if [ ${OUTFILE_SIZE} -eq 0 ]; then
    echo "  The results are the same!"
    rm -f ${OUTFILE}.diff
  else
    echo "  The results are different! Check the diff output in ${OUTFILE}.diff"
  fi
done
