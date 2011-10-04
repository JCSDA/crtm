# concatenation of tau files along channel dimension
SSMIS=ssmis_f16
sensor=z${SSMIS}

ROOT=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src

EXE_FILE=${ROOT}/TauRegress/ODPS/Assemble_ODPS/Cat_ODPS


$EXE_FILE<<EOF
TauCoeff.19.${sensor}.nc
TauCoeff.20.${sensor}.nc
TauCoeff.${sensor}.tmp.nc
2
EOF

$EXE_FILE << EOF
TauCoeff.${sensor}.tmp.nc
TauCoeff.21.${sensor}.nc
taucoef.${sensor}.nc
2
EOF
mv taucoef.${sensor}.nc TauCoeff.${sensor}.tmp.nc

$EXE_FILE << EOF
TauCoeff.${sensor}.tmp.nc
TauCoeff.22.${sensor}.nc
z${SSMIS}.TauCoeff.nc
2
EOF

# already in R2, don't need convert form R1 to R2
# convert ZTauCoeff R1 to R2
#EXE_FILE=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/Coefficients/TauCoeff/ODPS/ODPS_R1_to_R2/Convert_ODPS_R1toR2
#AtmProfile=/jcsda/save/wx23yc/CRTM_svn/trunk/src/TauProd/AtmProfile/netCDF/ECMWF_101LVL_83ATM.AtmProfile.nc
#${EXE_FILE}<<EOF
#${AtmProfile} 
#z${SSMIS}.TauCoeff.nc
#./TauCoeff_R2/z${SSMIS}.TauCoeff.nc
#EOF


#/u/wx23yh/CRTM/branches/src/EXP-Multiple_Algorithm/Coefficients/TauCoeff/ODPS/ODPS_NC2BIN/ODPS_NC2BIN<<EOF
#/jcsda/save/wx23yc/RB-2.0/src/Coefficients/TauCoeff/ODPS/ODPS_NC2BIN/ODPS_NC2BIN<<EOF
#z${SSMIS}.TauCoeff.nc
#z${sensor}.TauCoeff.bin
#EOF

rm TauCoeff.${sensor}.tmp.nc
exit
