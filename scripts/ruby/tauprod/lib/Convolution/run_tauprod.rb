#!/usr/bin/env ruby
#
# == Synopsis
#
# run_tauprod.rb::  Run the TauProd processing scripts.
#
# == Usage
#
# run_tauprod.rb [OPTIONS]
#
# If no options are specified, default values are determined by the
# TauProd.config defaults file.
#
#
# --help  (-h):
#    you're looking at it.
#
# --angles a1[-a2][,a3,a4,..]  (-a):
#    This option overrides the settings in the config file.
#    Specify the angles to process. The angles can be specified individually,
#    a1,a2,a3,... or as ranges, a1-a2,a3-a4, or combinations thereof, a1-a2,a3,a4.
#    Valid values are:
#      1 == SEC(z)=1.00, z= 0.000
#      2 == SEC(z)=1.25, z=36.870
#      3 == SEC(z)=1.50, z=48.190
#      4 == SEC(z)=1.75, z=55.150
#      5 == SEC(z)=2.00, z=60.000
#      6 == SEC(z)=2.25, z=63.612
#      7 == SEC(z)=3.00, z=70.529
#
# --bands b1[-b2][,b3,b4,..]  (-b):
#    This option overrides the settings in the config file.
#    Specify the LBL bands to process. The bands can be specfied individually
#    b1,b2,b3,... or as ranges, b1-b2,b3-b4, or combinations thereof, b1-b2,b3,b4.
#    Note that the band limits differ based on the specified bandwidth in the
#    defaults file.
#
# --dir dirname  (-d):
#    This option overrides the settings in the config file.
#    Specify the location of the LBLRTM TAPE5 input files.
#
# --molid m1[-m2][,m3,m4,..]  (-m):
#    This option overrides the settings in the config file.
#    Specify the molecule sets to check. The molecule set ids can be specified
#    individually, m1,m2,m3,... or as ranges, m1-m2,m3-m4, or combinations
#    thereof, m1-m2,m3,m4.
#    Valid values are:
#       1-7 == individual molecule numbers, no continua
#       8   == all first seven molecules, no continua
#       9   == continua only
#      10   == all first seven molecules and their continua
#      11   == water vapor + ozone only and their continua
#      12   == water vapor only and it's continua
#      13   == dry gases. Everything but h2o and o3 and their continua
#      14   == ozone only and it's continua
#      15   == water vapor continua only
#
# --profiles p1[-p2][,p3,p4,..]  (-p):
#    This option overrides the settings in the config file.
#    Specify the profiles to process. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
# --queue name  (-q):
#    This option overrides the settings in the config file.
#    Specify the batch queue to which the generated script file will be submitted.
#
# --tape3id name  (-t):
#    This option overrides the settings in the config file.
#    Specify the TAPE3 spectroscopic database to use in the LBLRTM calculations.
#    Valid id names are:
#      2000_AER == the HITRAN 2000 database with AER updates
#      1996_JPL == the HITRAN 1996 database with JPL/Toth updates
#
# CONFIG FILE:
#    The name of the TauProd configuration file to read. If not specified the
#    default file read is TauProd.config
#
# Written by:: Paul van Delst, CIMSS/SSEC 31-Oct-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'getoptlong'
require 'rdoc/usage'
require 'tauprod'

# Define the script name for messages
script_name=File.basename($0)

# Specify accepted options
options=GetoptLong.new(
  [ "--help",     "-h", GetoptLong::NO_ARGUMENT       ],
  [ "--debug",    "-g", GetoptLong::NO_ARGUMENT       ],
  [ "--angles",   "-a", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--bands",    "-b", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--dir",      "-d", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--molid",    "-m", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--profiles", "-p", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--queue",    "-q", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--tape3id",  "-t", GetoptLong::REQUIRED_ARGUMENT ] )

# Parse the command line options
args = {}
debug = false
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--debug"
        debug = true
      when "--angles"
        args[:angles] = arg
      when "--bands"
        args[:bands] = arg
      when "--molid"
        args[:molids] = arg
      when "--profiles"
        args[:profiles] = arg
      when "--dir"
        args[:tape5_dir] = arg
      when "--queue"
        args[:queue] = arg
      when "--tape3id"
        args[:tape3_id] = arg
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end

# Check if a config file was specified
config_file = ARGV.length == 0 ? "TauProd.config" : ARGV[0]
begin
  raise "TauProd config file not found" unless File.file? config_file
rescue RuntimeError=>error_message
  puts "ERROR: #{error_message}"
  exit 1
end

# Start the TauProd processing
TauProd::Processor.new.process(config_file,debug,args)
