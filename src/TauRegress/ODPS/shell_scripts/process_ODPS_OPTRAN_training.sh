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
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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
   
