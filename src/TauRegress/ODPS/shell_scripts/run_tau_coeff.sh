#!/bin/sh

#------------------------------------------------------------------------
# Generate transmittance coefficients for each channel and tau component
#------------------------------------------------------------------------

#-----------------------------------
# 1): Generate tau coefficients
#-----------------------------------

gen_tau_coeff.sh tau_coeff.parameters

#------------------------------------------------------------------
# 2) Get fitting errors for each component - run after 1)
#------------------------------------------------------------------

#get_stat.sh tau_coeff.parameters

#----------------------------------------------------------------------
# 3) Merge (concatenate) tau coefficient files into one - run after 1)
#----------------------------------------------------------------------

#EXE_file=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/TauRegress/ODPS/Assemble_ODPS/Cat_ODPS
#cat_taucoef.sh tau_coeff.parameters $EXE_file

#-----------------------------------------------------------------------------
# 4) Compute fitting error for each channel (all component) - run after 1) & 3)
#-----------------------------------------------------------------------------

#EXE_file=../src_FitErr/Compute_FitErr
#EXE_file=/jcsda/save/wx23yc/CRTM_ODPStraining/training/src_FitErr/Compute_FitErr
#set it to 1 to include OPTRAN wlo and set it to 0 to exclude it
#OPTRAN=1
#run_Compute_FitErr.sh tau_coeff.parameters $EXE_file $OPTRAN

exit
