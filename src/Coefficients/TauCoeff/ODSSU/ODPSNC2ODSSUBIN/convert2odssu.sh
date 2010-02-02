#!/bin/sh -x

for sensor in ssu_tirosn ssu_n06 ssu_n07 ssu_n08 ssu_n09 ssu_n11 ssu_n14 ssu_pseudo; do
#for sensor in ssu_pseudo; do

ln -sf /jcsda/save/wx23yc/RB-2.0/fix/TauCoeff/ODPS/netCDF/SSU/${sensor}.cellpressure.ASC .

ODPSNC2ODSSUBIN < ${sensor}.input.txt  

done  
