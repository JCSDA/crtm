#!/usr/bin/env ruby

# == Synopsis
#
# lblTauProd.rb: Begins LBLRTM TAPE5 file processing
#
# == Usage
#
# lblTauProd.rb [OPTION]
#
# If no options are specified, default values are determined
# by the procdef defaults file.
#
# --help  (-h):
#    you're looking at it.
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
#    Specify the LBL bands to process. The bands can be specfied individually
#    b1,b2,b3,... or as ranges, b1-b2,b3-b4, or combinations thereof, b1-b2,b3,b4.
#    Note that the band limits differ based on the specified bandwidth in the
#    defaults file.
#
# --dir dirname  (-d):
#    Specify the location of the LBLRTM TAPE5 input files.
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
# --queue name  (-q):
#    Specify the batch queue to which the generated script file will be submitted.
#
# --tape3id name  (-t):
#    Specify the TAPE3 spectroscopic database to use in the LBLRTM calculations.
#    Valid id names are:
#      2000_AER == the HITRAN 2000 database with AER updates
#      1996_JPL == the HITRAN 1996 database with JPL/Toth updates
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

# Other defaults
verbose=false

# Specify accepted options
options=GetoptLong.new(
  [ "--help",     "-h", GetoptLong::NO_ARGUMENT       ],
  [ "--verbose",  "-v", GetoptLong::NO_ARGUMENT       ],
  [ "--angles",   "-a", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--bands",    "-b", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--dir",      "-d", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--molid",    "-m", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--profiles", "-p", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--queue",    "-q", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--tape3id",  "-t", GetoptLong::REQUIRED_ARGUMENT ] )

# Parse the command line options
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--verbose"
        verbose=true
      when "--angles"
        pd.angles=arg.parse_range
      when "--bands"
        pd.bands=arg.parse_range
      when "--molid"
        pd.molids=arg.parse_range
      when "--profiles"
        pd.profiles=arg.parse_range
      when "--dir"
        pd.tape5_dir=arg
      when "--queue"
        pd.queue=arg
      when "--tape3id"
        pd.tape3_id=arg
    end
  end
rescue StandardError=>error_message
  puts "ERROR: #{error_message}"
  RDoc::usage
  exit 1
end
pd.display


# ----------------------------
# Assign processing parameters
# ----------------------------

zero=0.0
one=1.0

# Default continua, i.e. none.
h2o_sc=zero
h2o_fc=zero
co2_c=zero
o3_c=zero
o2_c=zero
n2_c=zero
rayleigh_ext=zero

# This directory
rootDir=Dir.pwd

# LBLRTM/HITRAN version string
lblrtm_hitran_version="LBLRTM v9.4; HITRAN 2000 + AER updates"
        

# ------------------
# Begin profile loop
# ------------------
pd.profiles.each do |p|
  ptag='profile%.2d' % p
  FileUtils.mkdir(ptag,:verbose=>verbose) unless File.exists?(ptag) && File.directory?(ptag) 
  
  # Generic TAPE5 filename
  gt5File="#{pd.tape5_dir}/TAPE5.#{pd.profile_set_id}_#{ptag}"

  # ----------------
  # Begin angle loop
  # ----------------
  pd.angles.each do |a|
    atag='angle%.1d' % a

    # Initialise molid counter
    nmol=0
    
    # ----------------
    # Loop over molids
    # ----------------
    pd.molid.each do |m|

      # Assign the molid tagname and continua settings
      mtag=ProcDef::MOLID[m]["name"]
      continua='%3.1f '*ProcDef::MOLID[m]["cont"].size % ProcDef::MOLID[m]["cont"]
      
      # Construct the TAPE3 filename
      t3File="tape3#{ProcDef::MOLID[m]["t3tag"]}.#{pd.tape3_id}"

      # Create the molid directory
      mDir=File.join(ptag,atag,mtag)
      FileUtils.mkdir_p(mDir,:verbose=>verbose)

      # Create jcf file for current band set
      # I use a random number between 0 and 100000
      # to tag the file since the bands are no longer 
      # necessarily contiguous
      jcFilename=""
      loop do
        jcFilename=mDir.gsub(/\//,"_") << "_" << "%.6d" % rand(100000) << ".jcf"
        break unless File.exists?(jcFilename)
      end
      jcFile=File.open(jcFilename)
      # Define the script interpreter
      jcFile.puts("#!/bin/sh")
      
      # ---------------
      # Begin band loop
      # ---------------
      pd.bands.each do |b|
        btag='band%.3d' % b

        # Create the band directory
        bDir=File.join(mDir,btag)
        FileUtils.mkdir_p(bDir,:verbose=>verbose)
        
        # Assign frequency parameters for this band
        # Actual frequencies used in LBLRTM calculation has
        # one unit of slop either side.
        f1=pd.f1_band1+((b-1)*pd.df_band)
        f2=f1+pd.df_band
        f1_lbl=f1-1.0  # Begin frequency for LBL calcs
        f2_lbl=f2+1.0  # End frequency for LBL calcs
        f1_scnmrg=f1            # Begin frequency for SCNMRG
        f2_scnmrg=f2-pd.df_avg  # End frequency for SCNMRG
        
        # Define an id tag for filenames, and an attribute for netCDF files
        idtag=bDir.gsub(/\//,"_")
        idatt="("<<bDir.gsub(/\//,":")<<")"

        # TAPE5 input filename for this run
        t5File="tape5.#{idtag}.rdk"

        # ----------------------------------------------
        # Create band/angle/continua specific TAPE5 file
        # ----------------------------------------------
        t5=File.readlines(gt5File)
        t5[0].sub!(/;\s*$/,"; #{idtag}")
        t5.insert(2,continua)
        
        # The continua flag...only replace first occurrance
        re=[/CN=0/,"CN=6"]
        t5.find_all {|s| s=~re.first}.first {|n| n.gsub!(re.first,re.last)}
        
        # Every thing else, replace all occurrances
        replace=[[/UUUU.UUU/ ,'%8.3f'%f1_lbl],                      # LBL begin frequency
                 [/VVVV.VVV/ ,'%8.3f'%f2_lbl],                      # LBL end frequency
                 [/CCC.CCC/  ,'%7.3f'%pd.co2mr],                    # CO2 mixing ratio
                 [/AAA.AAA/  ,'%7.3f'%ProcDef::ANGLE[a]["zenith"],  # Zenith angle
                 [/D.DDDD/   ,'%6.4f'%pd.df_avg],                   # Averaging kernel width
                 [/SSSS.SSSS/,'%9.4f'%f1_scnmrg],                   # Averaging begin frequency
                 [/TTTT.TTTT/,'%9.4f'%f2_scnmrg]]                   # Averaging end frequency
        replace.each do |re|
          t5.find_all {|s| s=~re.first}.each {|n| n.gsub!(re.first,re.last)}
        end
        
        # Write the new tape5 file
        File.open(t5File,"w") {|f| f.puts(t5)}

        # ---------------------------------
        # Create the executable script file
        # ---------------------------------
        scriptFile="#{idtag}.sh"
        
        # Output data filenames
        uptapeFile, downtapeFile="TAPE20","TAPE21"
        
        uplblFile,downlblFile=["up","down"].each {|d| "#{d}welling_tau.#{idtag}" }
        upncFile, downncFile =["up","down"].each {|d| "#{d}welling_tau.#{idtag}.nc" }

        # Start the script build
        script=["#!/bin/sh"]
        script << "\n"
        script << "# LBLRTM transmittance production run script"
        script << "# file for the input TAPE5 file:"
        script << "#   #{t5File}"
        script << "# and using the TAPE3 spectroscopic file:"
        script << "#   #{t5File}"

        # Change to the directory containing the current TAPE5 file
        script << "\n"
        script << "# Change to required directory"
        script << "cd #{rootDir}"


      end # band loop

      jcFile.close     
      
    rescue StandardError=>error_message
      puts "ERROR: #{error_message}"
      exit 1
    end # molid loop
  end # angle loop
end # profile loop
