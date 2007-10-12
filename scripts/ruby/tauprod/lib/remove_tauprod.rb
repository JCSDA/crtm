#!/usr/bin/env ruby
#
# == Synopsis
#
# remove_tauprod.rb::  Remove the TauProd directories.
#
# == Usage
#
# remove_tauprod.rb [OPTIONS]
#
#
# --help  (-h):
#    you're looking at it.
#
# --debug (-g):
#    Set this switch to output debug information.
#
# --angles a1[-a2][,a3,a4,..]  (-a):
#    Specify the angle directories to remove. The angles can be specified individually,
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
#    Specify the IASI band directories to remove. The bands can be specfied individually
#    b1,b2,b3,... or as a range, b1-b3.
#
# --molid m1[-m2][,m3,m4,..]  (-m):
#    Specify the molecule set directories to remove. The molecule set ids can be specified
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
#    Specify the profile directories to remove. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
#
# Written by:: Paul van Delst, CIMSS/SSEC 11-Oct-2007 (paul.vandelst@noaa.gov)
#

require 'fileutils'
require 'tauprod'

# Configuration setup
# -------------------
cfg = TauProd::Config.new
cfg.process_arguments
cfg.display

# The file extensions that may need 
# removing from the profile directory
# -----------------------------------
ext = ["jcf","err","out","sh","rdk"]

# Begin the error handling block
# ------------------------------
begin

  # Define the current directory
  root_dir = Dir.pwd

  # Begin the band loop
  # -------------------
  cfg.bands.each do |b|
    bdir = "band%.1d" % b
    next unless File.exists?(bdir)
    
    # Remove the molecule directories
    # -------------------------------
    FileUtils.cd(bdir, :verbose=>cfg.debug) do
      cfg.molids.each do |m|
        mdir = TauProd::Config::MOL_INFO[m][:name]
        next unless File.exists?(mdir)
        
        # Remove the profile directories
        # ------------------------------
        FileUtils.cd(mdir, :verbose=>cfg.debug) do
          cfg.profiles.each do |p|
            pdir = "profile%.2d"%p
            next unless File.exists?(pdir)
            
            # Remove the files from the profile directory
            ext.each {|e| system("rm -f #{pdir}/*.#{e}") unless cfg.noop}
            
            # Remove the angle directories
            # ----------------------------
            FileUtils.cd(pdir, :verbose=>cfg.debug) do
              cfg.angles.each do |a|
                adir = "angle%.1d"%a
                next unless File.exists?(adir)
                FileUtils.rm_rf(adir, :verbose=>cfg.debug, :noop=>cfg.noop)
              end # angle loop
              
            end # exit profile dir
            FileUtils.rmdir(pdir, :verbose=>cfg.debug, :noop=>cfg.noop) if Dir["#{pdir}/*"].empty?
          end # profile loop
          
        end # exit molecule dir
        FileUtils.rmdir(mdir, :verbose=>cfg.debug, :noop=>cfg.noop) if Dir["#{mdir}/*"].empty?
      end # molecule loop
      
    end # exit band dir
    FileUtils.rmdir(bdir, :verbose=>cfg.debug, :noop=>cfg.noop) if Dir["#{bdir}/*"].empty?
  end # band loop

rescue StandardError => error_message
  puts("\nERROR: #{error_message}\n")
  exit 1
end  # error handling block
