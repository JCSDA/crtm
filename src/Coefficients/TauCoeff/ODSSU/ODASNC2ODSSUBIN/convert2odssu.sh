#!/bin/sh -x

for sensor in ssu_n05 ssu_n06 ssu_n07 ssu_n08 ssu_n09 ssu_n11 ssu_n14; do
#for sensor in ssu_pseudo; do

ln -sf /jcsda/save/wx23yc/RB-2.0/fix/TauCoeff/ODAS/netCDF/SSU/${sensor}.cellpressure.ASC .

ODASNC2ODSSUBIN < ${sensor}.input.txt  

done  
