#!/usr/bin/env ruby

# == Synopsis
#
# rmdataTauProd.rb:: Remove data directories after a TauProd run. Upper level
#                    directories (i.e. angle and profile directories) are 
#                    removed only if they are empty.
#
# == Usage
#
# rmdataTauProd.rb [OPTION]
#
# If no options are specified, default values are determined
# by the procdef defaults file.
#
# -h, --help:
#    you're looking at it
#
# -n, --noop:
#    loop through the directory heirarchy but do nothing.
#
# -v, --verbose
#    output information about the removal process.
#
# -a a1[-a2][,a3,a4,..], --angles a1[-a2][,a3,a4,..]:
#    Specify the angle runs to clean up. The angles can be specified individually,
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
# -m m1[-m2][,m3,m4,..], --molid m1[-m2][,m3,m4,..]:
#    Specify the molecule set runs to clean up. The molecule set ids can be
#    specified individually, m1,m2,m3,... or as ranges, m1-m2,m3-m4, or
#    combinations thereof, m1-m2,m3,m4.
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
#
# -p p1[-p2][,p3,p4,..], --profiles p1[-p2][,p3,p4,..]:
#    Specify the profile run to clean up. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
# Written by:: Paul van Delst, CIMSS/SSEC 06-Aug-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'procdef'
require 'getoptlong'
require 'rdoc/usage'
require 'fileutils'

# Read the defaults file
procdef_file="TauProd.procdef"
pd=ProcDef.new
begin
  pd.read(procdef_file)
rescue
  puts("Defaults file #{procdef_file} not found.")
  exit 1
end

# Set other defaults
noop=false
verbose=false

# Specify accepted options
options=GetoptLong.new(
  [ "--help",     "-h", GetoptLong::NO_ARGUMENT       ],
  [ "--noop",     "-n", GetoptLong::NO_ARGUMENT       ],
  [ "--verbose",  "-v", GetoptLong::NO_ARGUMENT       ],
  [ "--angles",   "-a", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--molid",    "-m", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--profiles", "-p", GetoptLong::REQUIRED_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--noop"
        noop=true
      when "--verbose"
        verbose=true
      when "--angles"
        pd.angles=arg.parse_range
      when "--molid"
        pd.molids=arg.parse_range
      when "--profiles"
        pd.profiles=arg.parse_range
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end


# ---------------------------
# Remove data run directories
# ---------------------------
# Loop over profiles
pd.profiles.each do |p|

  # Enter the profile directory
  pdir='profile%.2d' % p
  next unless File.exists?(pdir)
  puts("Removing data directories for #{pdir}...")
  FileUtils.cd(pdir,:verbose=>verbose)
  
  # Loop over angles
  pd.angles.each do |a|
  
    # Enter the angle directory
    adir='angle%.1d' % a
    next unless File.exists?(adir)
    puts("  #{adir}...")
    FileUtils.cd(adir,:verbose=>verbose)
    
    # Loop over molecule sets
    pd.molids.each do |m|
    
      # Remove the molecule directory
      mdir=ProcDef::MOLID[m]["name"]
      next unless File.exists?(mdir)
      FileUtils.rmtree(mdir,:verbose=>verbose,:noop=>noop)
   
    end # molid loop
    
    # Remove the angle directory
    FileUtils.cd("..",:verbose=>verbose)
    FileUtils.rmdir(adir,:verbose=>verbose,:noop=>noop) if Dir["#{adir}/*"].length==0
    
  end # angle loop

  # Remove the profile directory
  FileUtils.cd("..",:verbose=>verbose)
  FileUtils.rmdir(pdir,:verbose=>verbose,:noop=>noop) if Dir["#{pdir}/*"].length==0

end # profile loop
