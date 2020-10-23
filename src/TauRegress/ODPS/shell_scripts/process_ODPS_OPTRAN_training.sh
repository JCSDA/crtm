#!/bin/sh
#------------------------------------------------------------------------------
#
# NAME:
#       process_ODPS_OPTRAN_training
#
# PURPOSE:
#       Shell script to process the ODPS_OPTRAN combination Coefficients 
#       files using the defined parameters.
#
# CATEGORY:
#       TauCoeff Production
#
# LANGUAGE:
#       Bourne shell script
#
# CALLING SEQUENCE:
#       process_ODPS_OPTRAN_training -h
#
# CREATION HISTORY:
#       Written by:     Yong Chen, CIRA/CSU/JCSDA 20-Feb-2009
#                       Yong.Chen@noaa.gov
#
#  Copyright (C) 2009 Yong Chen
#
#
#------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#                        -- USAGE DESCRIPTION FUNCTION --
#-------------------------------------------------------------------------------
usage()
{
  echo
  echo " Usage: process_ODPS_OPTRAN_training [-h] [-s step]  "
  echo
  echo "   -h           Print this message and exit"
  echo
  echo "   -s           Use this option to specify the step to generate the TauCoeff"
  echo "                files."
  echo "                Valid  step indices are:"
  echo "                   1 == Generate ODPS tau coefficients"
  echo "                   2 == Get fitting errors for each component - run after step 1 "
  echo "                   3 == Merge (concatenate) tau coefficient files into one - run after step 1 "
  echo "                   4 == Generate OPTRAN tau coefficients for water vapor (MW) or water vapor line (IR)"
  echo "                   5 == Get fitting errors for water vapor component - run after step 4 "
  echo "                   6 == Merge (concatenate) tau coefficient files into one - run after step 4 "
  echo "                   7 == Merge ODPS and OPTRAN coefficient files into one - run after step 1-6 "
  echo "                   8 == Compute fitting error for each channel (all component) - "
  echo "                     run after 1) & 3) (without OPTRAN) or run after 1) & 7) (with OPTRAN)  "
   
