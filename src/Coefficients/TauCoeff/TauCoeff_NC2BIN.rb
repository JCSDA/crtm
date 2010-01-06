#!/usr/bin/env ruby
# == Synopsis
#
# TauCoeff_NC2BIN.rb::  Wrapper script for converting the various CRTM transmittance
#                       coefficient files from netCDF to binary format.
#
# == Usage
#
# TauCoeff_NC2BIN.rb [OPTIONS] [format1] [format2] ... [formatN]
#
#
# == Arguments
#
#   format1 ... formatN:
#    The name of the format to convert, e.g. ODAS, ODPS, etc.
#    Note that this script assumes that:
#      1) the format itself exists, as does the required Fortran95/2003 executable.
#      2) the executable was built using gfortran. The gfortran environment variable
#         GFORTRAN_CONVERT_UNIT is used to create little and big endian files.
#
# == Options
#
# --help  (-h):
#    you're looking at it.
#
# --debug (-g):
#    Set this switch to output debug information. Setting this displays the commands
#    to be executed without invoking them.
#
#
# Written by:: Paul van Delst, 14-Oct-2009 (paul.vandelst@noaa.gov)
#

require 'getoptlong'
require 'rdoc/usage'

# Accepted command line options
options = GetoptLong.new(
  [ "--help"  , "-h", GetoptLong::NO_ARGUMENT],
  [ "--debug" , "-g", GetoptLong::NO_ARGUMENT] )

