#!/usr/bin/env ruby

# == Synopsis
#
# checkTauProd.rb:: Check the status of LBL and TauSpc processing runs and outputs
#                   information about incomplete runs.
#
# == Usage
#
# checkTauProd.rb [OPTION]
#
# If no options are specified, default values are determined
# by the procdef defaults file.
#
# -h, --help:
#    you're looking at it
#
# -a a1[-a2][,a3,a4,..], --angles a1[-a2][,a3,a4,..]:
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
# -m m1[-m2][,m3,m4,..], --molid m1[-m2][,m3,m4,..]:
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
#
# -p p1[-p2][,p3,p4,..], --profiles p1[-p2][,p3,p4,..]:
#    Specify the profiles to check. The profiles can be specified individually,
#    p1,p2,p3,... or as ranges, p1-p2,p3-p4, or combinations thereof, p1-p2,p3,p4.
#    
# Written by:: Paul van Delst, CIMSS/SSEC 06-Aug-2006 (paul.vandelst@ssec.wisc.edu)
#

require 'procdef'
require 'getoptlong'
require 'rdoc/usage'

# Read the defaults file
procdef_file="TauProd.procdef"
pd=ProcDef.new
begin
  pd.read(procdef_file)
rescue
  puts("Defaults file #{procdef_file} not found.")
  exit 1
end

# Specify accepted options
options=GetoptLong.new(
  [ "--help",     "-h", GetoptLong::NO_ARGUMENT       ],
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

# Begin directory search
pd.profiles.each do |p|
  ptag='profile%.2d' % p
  ncomplete=0
  nexpected=pd.angles.length * pd.molids.length

  pd.angles.each do |a|
    atag='angle%.1d' % a

    pd.molids.each do |m|
      mtag=ProcDef::MOLID[m]["name"]
      rootdir=File.join(ptag,atag,mtag)
      next if not File.exists?(rootdir)
      
      # Count the LBL-related directories and files
      nbands=Dir["#{rootdir}/band*"].length
      upbands,downbands=["up","down"].collect {
        |d| x=Dir["#{rootdir}/band*/#{d}welling_tau.*.signal"]
            b=[]
            x.each {|f| f=~/band0?0?(\d*)/; b<<$1}  # Note, this regex is not a good one.
            b
        }

      # Count the number of completed TauSpc runs
      nuptauspc,ndowntauspc=["up","down"].collect {
        |d| Dir["#{rootdir}/ProcessControl.#{atag}_#{mtag}.#{d}welling.completed.signal"].length
        }
      
      # Gather info about runs....
      if nuptauspc != 1 || ndowntauspc != 1
        # Construct string about incompleteness
        print <<-EOFTAUSPC
  TAUSPC #{ptag}, #{atag}, #{mtag}(#{m}) Incomplete.
    UP completed:   #{nuptauspc}
    DOWN completed: #{ndowntauspc}
    LBL Bands present:  #{nbands}
      UP completed:   #{upbands.length}; bands #{upbands.join(" ")}
      DOWN completed: #{downbands.length}; bands #{downbands.join(" ")}
  EOFTAUSPC
      else
        # Otherwise, increment completed counter
        ncomplete+=1
      end
      
    end # molid loop
  end # angle loop

  puts "TAUSPC #{ptag} Completed." if ncomplete == nexpected
  
end # profile loop
