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
# --build  (-b):
#    Build the library(ies) for the specified target(s). The default is to NOT
#    build anything.
#
# --link  (-l):
#    Create a generic link for the specified target. If multiple targets are
#    specified, the last one is linked. The default is to NOT link anything.
#    
# --install-dir <dir> (-i <dir>)
#    Use this option to specify a different directory in which to install the
#    generated CRTM libraries and include files. If not specified, the default
#    install directory is ${HOME}/local/CRTM.
#
# --crtm-dir <dir> (-c <dir>)
#    Use this option to specify a different CRTM source code repository working
#    copy directory. If not specified, the default CRTM repository working copy
#    is that defined by the environment variable CRTM_SOURCE_ROOT.
#
# --debug (-g):
#    Set this switch to output debug information. Setting this displays the commands
#    to be executed without invoking them.
#
#
# Written by:: Paul van Delst, 21-Nov-2008 (paul.vandelst@noaa.gov)
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

