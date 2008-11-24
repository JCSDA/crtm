#!/usr/bin/env ruby
#
# == Synopsis
#
# run_crtm_lib.rb::  Script to compile the CRTM using different compiler targets and
#                    install the resultant library and module files in target- and
#                    compiler-specific locations within the ${HOME}/local/CRTM hierarchy.
#
# == Usage
#
# run_crtm_lib.rb [OPTIONS] [target1 target2 ... targetN]
#
#
# --help  (-h):
#    you're looking at it.
#
# --debug (-g):
#    Set this switch to output debug information.
#
# --install-dir <dir> (-i <dir>)
#    Use this option to specify a different directory in which to install the
#    generated CRTM libraries and include files. If not specified, the default
#    install directory is ${HOME}/local/CRTM.
#
# --crtm-dir <dir> (-c <dir>)
#    Use this option to specify a different CRTM source code directory. If not
#    specified, the default CRTM directory is that defined by the environment
#    variable CRTM_SOURCE_ROOT.
#
# --no-build  (-b):
#    Skip the library build phase.
#
# --no-link  (-l):
#    Skip the library generic link phase.
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 21-Nov-2008 (paul.vandelst@noaa.gov)
#

require 'crtm_lib'


# Configuration setup
# -------------------
config = CRTM_Lib::Config.new()
config.process_arguments
config.display


# Begin the processing
# --------------------
CRTM_Lib::Processor.new.process(config)

