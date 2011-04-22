# Script to run Compute_NLTE_Coeff for generating NLTE
# correction coefficients

SENSOR=crisB3_npp
#SENSOR=iasiB3_metop-a
#SENSOR=airs_aqua

EXE_FILE='../Compute_NLTE_Coeff/Compute_NLTE_Coeff'
TrainSet_DIR=/u/wx23yh/noscrub_jcsda/work_nlte
PATH_SPCCOEFF=/u/wx23yh/CRTM/EXP-NLTE/fix/SpcCoeff/Big_Endian/


if [ "${SENSOR}" == "airs_aqua" ];then

../Compute_NLTE_Coeff/Compute_NLTE_Coeff<<EOF
2
${TrainSet_DIR}/airsM2b_aqua_radSpectrum.nc
${TrainSet_DIR}/airsM1b_aqua_radSpectrum.nc
${SENSOR}
${PATH_SPCCOEFF}
EOF

else

${EXE_FILE}<<EOF
1
${TrainSet_DIR}/${SENSOR}_radSpectrum.nc
${SENSOR}
${PATH_SPCCOEFF}
EOF

fi

exit
