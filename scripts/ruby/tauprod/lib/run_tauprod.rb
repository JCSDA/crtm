#!/usr/bin/env ruby
#
# == Synopsis
#
# run_tauprod.rb::  Generate and submit the IASI TauProd processing scripts.
#
# == Usage
#
# run_tauprod.rb [OPTIONS]
#
#
# --help  (-h):
#    you're looking at it.
#
# --debug (-g):
#    Set this switch to output debug information.
#
# --noop  (-n):
#    Create all the directories and script files, but do not submit any jobs
#    to the batch processor.
#
# --keeplbl  (-k):
#    Keep all the intermediate line-by-line (LBL) datafiles. The default action
#    is to delete them with they are no longer needed.
#
# --angles a1[-a2][,a3,a4,..]  (-a):
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
#    Specify the IASI bands to process. The bands can be specfied individually
#    b1,b2,b3,... or as a range, b1-b3.
#
# --molid m1[-m2][,m3,m4,..]  (-m):
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
#    Specify the profiles to process. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
# --co2_mr ppmv  (-c):
#    The CO2 mixing ratio, in ppmv, to use in the LBLRTM calculations. If not
#    specified the default value is 380.0ppmv.
#
# --start_delay min  (-i):
#    The initial delay, in minutes, for which jobs are put on hold in the
#    the batch processor. If not specified, the default is 5 minutes.
#
# --queue name  (-q):
#    Specify the batch queue to which the generated script file will be submitted.
#
# --profile_set name  (-s):
#    Specify the profile set being used.
#    Valid profile set names are:
#      UMBC  (DEFAULT)
#      ECMWF
#      CIMSS
#
# --t5_dir dirname  (-d):
#    Specify the location of the LBLRTM TAPE5 input files.
#
# --t3_id name  (-t):
#    Specify the TAPE3 spectroscopic database to use in the LBLRTM calculations.
#    Valid id names are:
#      hitran2004     == the HITRAN 2004 database
#      hitran2000_aer == the HITRAN 2000 database with AER updates (DEFAULT)
#      hitran2000     == the HITRAN 2000 database
#      hitran1996_jpl == the HITRAN 1996 database with JPL/Toth updates
#
#
# Written by:: Paul van Delst, CIMSS/SSEC 27-Sep-2007 (paul.vandelst@noaa.gov)
#

require 'tauprod'


# Configuration setup
# -------------------
config = TauProd::Config.new()
config.process_arguments
config.display


# Begin the processing
# --------------------
TauProd::Processor.new.process(config)
