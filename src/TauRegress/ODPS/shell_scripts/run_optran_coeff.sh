#!/bin/sh

# First step
EXE_FILE=/home/Paul.Vandelst/CRTM/trunk/src/TauRegress/ODPS/ODAS_WLO_Regress/gencoef

gen_optran_coeff.sh ALLCOM tau_coeff.parameters $EXE_FILE

# Second step
#optran_get_stat.sh tau_coeff.parameters

#EXE_file=/home/Paul.Vandelst/CRTM/trunk/src/TauRegress/ODAS/Assemble_ODAS/Cat_ODAS
#cat_optran_taucoef.sh tau_coeff.parameters $EXE_file

exit
