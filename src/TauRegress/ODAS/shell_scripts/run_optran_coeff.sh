#!/bin/sh

#gen_optran_coeff.sh ALLCOM optran_coeff.parameters

get_stat.sh optran_coeff.parameters

EXE_file=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/TauRegress/ODAS/Assemble_ODAS/Cat_ODAS
cat_taucoef.sh optran_coeff.parameters $EXE_file
exit
