#!/usr/bin/env ruby
#
# == Synopsis
#
# check_tauprod.rb::  Check the completion status of the TauProd processing.
#
# == Usage
#
# check_tauprod.rb [OPTIONS]
#
#
# --help  (-h):
#    you're looking at it.
#
# --angles a1[-a2][,a3,a4,..]  (-a):
#    Specify the angles to check. The angles can be specified individually,
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
#      16   == dry gases + ozone only and their continua
#      17   == water vapor + dry gases only and their continua
#
# --profiles p1[-p2][,p3,p4,..]  (-p):
#    Specify the profiles to process. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
#
# Written by:: Paul van Delst, CIMSS/SSEC 01-Oct-2007 (paul.vandelst@noaa.gov)
#

require 'tauprod'


# Configuration setup
# -------------------
cfg = TauProd::Config.new
cfg.process_arguments
cfg.display


# Begin the error handling block
# ------------------------------
begin

  # Define the current directory
  root_dir = Dir.pwd
  
  # Define the filename(s) to search for
  signal_file = ["upwelling.#{TauProd::Config::SENSOR_ID}.REAL.TauProfile.nc.signal",
                 "downwelling.#{TauProd::Config::SENSOR_ID}.REAL.TauProfile.nc.signal"]
  
  # Set the expected number of signal files
  n_expected = cfg.molids.length * cfg.profiles.length * cfg.angles.length * signal_file.length
  n_angles_expected = cfg.angles.length * signal_file.length


  # Begin the band loop
  # -------------------
  cfg.bands.each do |b|
    binfo = TauProd::Config::BAND_INFO[b]
    bname = "band%.1d" % b
    bdir = bname
    puts("Checking #{bname} run completion...")
 
    # Completion counter initialisation
    n_completed = 0
    
    # Begin the molecule loop
    # -----------------------
    cfg.molids.each do |m|
      minfo = TauProd::Config::MOL_INFO[m]
      mname = minfo[:name]
      mdir = File.join(bdir,mname)
      next unless File.exists?(mdir)
      
      # Begin the profile loop
      # ----------------------
      cfg.profiles.each do |p|
        pname = "profile%.2d"%p
        pdir = File.join(mdir,pname)
        next unless File.exists?(pdir)
        
        # initialise angle counter
        n_angles = 0; alist=[]

        # Begin the angle loop
        # --------------------
        cfg.angles.each do |a|
          ainfo = TauProd::Config::ANGLE_INFO[a]
          aname = "angle%.1d"%a
          adir = File.join(pdir,aname)
          next unless File.exists?(adir)

          signal_file_exist = signal_file.collect {|s| File.exists?("#{adir}/#{s}")}
          alist << a if signal_file_exist.any? {|s| not s}

          # Count the angle signal files that exist
          n_angles += ((signal_file.select {|s| File.exists?("#{adir}/#{s}")}) - [false]).length
          
        end # angle loop
        
        n_completed += n_angles

        # Check the angle file count
        puts("    #{pdir}: #{alist.length} [#{alist.join(",")}] angles incomplete.") unless n_angles == n_angles_expected 
        
      end # profile loop
    end # molecule loop
    
    # Check the total file count
    puts("  #{bdir}: #{n_completed} of #{n_expected} runs completed")
        
  end # band loop                
   
rescue StandardError => error_message
  puts("\nERROR: #{error_message}\n")
  exit 1
end  # error handling block
