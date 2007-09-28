#!/usr/bin/env ruby
#
# == Synopsis
#
# iasi_tauprod.rb::  Generate and submit the IASI TauProd processing scripts.
#
# == Usage
#
# iasi_tauprod.rb [OPTIONS]
#
#
# --help  (-h):
#    you're looking at it.
#
# --noop  (-n):
#    Create all the directories and script files, but do not submit any jobs
#    to the batch processor.
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

require 'getoptlong'
require 'rdoc/usage'


# Define methods
# --------------
# Method to parse input range definitions
# e.g. 1-3,6,10-12  produces [1,2,3,6,10,11,12]
def parse_range(input)
  input.split(",").collect do |element|
    if element =~ /(\d+)-(\d+)/
      ($1.to_i..$2.to_i).to_a
    else
      element.to_i
    end
  end.flatten.uniq.sort
end

# Method to replace only the first
# occurance of the leading spaces
# in each line of input text.
def strip_output(text)
  text =~ /^\s+/
  leading_spaces = $&
  text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,"")}.to_s
end


# Define the constants
# --------------------
LL_SUBMIT    = "llsubmit"
LL_ACCOUNT   = { :ccs=>"GDAS-T2O", :haze=>"JCSDA001-RES"}
LL_TIMELIMIT = "03:00:00"
LL_RESOURCES = "ConsumableCpus(1) ConsumableMemory(500)"

SUBMIT_INCREMENT = 60*60*3  # seconds

LBLRTM_HITRAN_VERSION = "LBLRTM v9.4; HITRAN 2000 + AER updates"
DFAVG   = 0.001  # Averaging kernel width in cm^-1
NPANELS = 1      # No. of LBL panels
UPDIRN  = 1      # Direction flag for upwelling
DNDIRN  = 2      # Direction flag for upwelling
SENSOR_ID = "iasi_metopa"

# Literal constants
ZERO=0.0; ONE=1.0


# -------------------
# Define the defaults
# -------------------
# Default processing queue
queue = "dev"

# Start delay
start_delay = 60*5  # seconds

# Default location of the LBLRTM input files
t5_dir = "./TAPE5_files"

# Default spectroscopic database
t3_id = "hitran2000_aer"

# Default CO2 mixing ratio
co2_mr = 380.0

# Profile set info
PROFILE_SET_INFO = { "UMBC"  => {:id => 1, :n_profiles => 48},
                     "ECMWF" => {:id => 2, :n_profiles => 52},
                     "CIMSS" => {:id => 3, :n_profiles => 32}}
profile_set = "UMBC"
profiles = (1..PROFILE_SET_INFO[profile_set][:n_profiles]).to_a
                   
# Band information. The SCNMRG frequencies are those values that,
# for a point spacing of 0.001cm^-1, produce a number of points
# that has only prime factors of 2, 3, and 5.
BAND_INFO = { 1 => {:f1_scnmrg =>  605.0, :f2_scnmrg => 1253.00},
              2 => {:f1_scnmrg => 1170.0, :f2_scnmrg => 2107.50},
              3 => {:f1_scnmrg => 1960.0, :f2_scnmrg => 2803.75}}
bands = BAND_INFO.keys.sort

# Molecule information
MOL_INFO=Hash.new
(1..7).to_a.collect {|m| MOL_INFO[m] = {:name  => "mol#{m}",
                                        :t3tag => "mol#{m}",
                                        :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}}
MOL_INFO[8]  = {:name  => "all_nocontinua",
                :t3tag => "7mol",
                :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}
MOL_INFO[9]  = {:name  => "continua_only",
                :t3tag => "nomol",
                :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
MOL_INFO[10] = {:name  => "all_withcontinua",
                :t3tag => "7mol",
                :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
MOL_INFO[11] = {:name  => "wvo",
                :t3tag => "wvo",
                :cont  => [ONE,ONE,ZERO,ONE,ZERO,ZERO,ZERO]}
MOL_INFO[12] = {:name  => "wet",
                :t3tag => "mol1",
                :cont  => [ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}
MOL_INFO[13] = {:name  => "dry",
                :t3tag => "dry",
                :cont  => [ZERO,ZERO,ONE,ZERO,ONE,ONE,ONE]}
MOL_INFO[14] = {:name  => "ozo",
                :t3tag => "mol3",
                :cont  => [ZERO,ZERO,ZERO,ONE,ZERO,ZERO,ZERO]}
MOL_INFO[15] = {:name  =>"wco",
                :t3tag =>"nomol",
                :cont  =>[ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}
molids = [1, 10, 11, 12, 13, 14, 15]
                
# Angle information
ANGLE_INFO = { 1 => {:secant => 1.00, :angle => 0.0   , :zenith => 180.000},
               2 => {:secant => 1.25, :angle => 36.870, :zenith => 143.130},
               3 => {:secant => 1.50, :angle => 48.190, :zenith => 131.810},
               4 => {:secant => 1.75, :angle => 55.150, :zenith => 124.850},
               5 => {:secant => 2.00, :angle => 60.000, :zenith => 120.000},
               6 => {:secant => 2.25, :angle => 63.612, :zenith => 116.388},
               7 => {:secant => 3.00, :angle => 70.529, :zenith => 109.471} }
angles = ANGLE_INFO.keys.sort

# No debug output by default
debug = false

# Submit jobs by default
noop = false


# Parse the command line options
# ------------------------------
# Specify accepted options
options=GetoptLong.new(
  [ "--help"       , "-h", GetoptLong::NO_ARGUMENT       ],
  [ "--debug"      , "-g", GetoptLong::NO_ARGUMENT       ],
  [ "--noop"       , "-n", GetoptLong::NO_ARGUMENT       ],
  [ "--angles"     , "-a", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--bands"      , "-b", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--co2_mr"     , "-c", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--start_delay", "-i", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--molid"      , "-m", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--profiles"   , "-p", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--queue"      , "-q", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--profile_set", "-s", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--t5_dir"     , "-d", GetoptLong::REQUIRED_ARGUMENT ],
  [ "--t3_id"      , "-t", GetoptLong::REQUIRED_ARGUMENT ] )

# Process the arguments
begin
  options.each do |opt, arg|
    case opt
      when "--help"
        RDoc::usage
        exit 0
      when "--debug"
        debug = true
      when "--noop"
        noop = true
      when "--angles"
        angles = parse_range(arg)
      when "--bands"
        bands = parse_range(arg)
      when "--molid"
        molids = parse_range(arg)
      when "--profiles"
        profiles = parse_range(arg)
      when "--co2_mr"
        co2_mr = arg.to_f
      when "--start_delay"
        start_delay = (arg.to_i)*60  # Convert to seconds
      when "--queue"
        queue = arg
      when "--profile_set"
        profile_set = arg if PROFILE_SET_INFO.has_key?(arg)
      when "--t5_dir"
        t5_dir = arg
      when "--t3_id"
        t3_id = arg
    end
  end
rescue StandardError => error_message
  puts("\nERROR: #{error_message}\n")
  RDoc::usage
  exit 1
end


# Begin the error handling block
# ------------------------------
begin

  # Define the current directory
  root_dir = Dir.pwd

  # Get the initial submit time
  time  = Time.now + start_delay

  
  # Begin the band loop
  # -------------------
  bands.each do |b|
    binfo = BAND_INFO[b]
    bname = "band%.1d" % b
    bdir = bname
    cmd = "mkdir #{bdir}"
    system(cmd) unless File.directory?(bdir)
    
    # Assign the frequency parameters for this band
    f1_scnmrg = binfo[:f1_scnmrg]      # Begin frequency for SCNMRG
    f2_scnmrg = binfo[:f2_scnmrg]      # End frequency for SCNMRG
    f1_lbl = f1_scnmrg - 1.0           # Begin frequency for LBL calcs
    f2_lbl = f2_scnmrg + 1.0           # End frequency for LBL calcs
    

    # Begin the molecule loop
    # -----------------------
    molids.each do |m|
      minfo = MOL_INFO[m]
      mname = minfo[:name]
      mdir = File.join(bdir,mname)
      cmd = "mkdir #{mdir}"
      system(cmd) unless File.directory?(mdir)
      
      # Specify the continua
      continua = "%3.1f "*minfo[:cont].size % minfo[:cont]
    
      # Construct the tape3 filename
      t3file = "tape3.#{minfo[:t3tag]}.#{t3_id}"
      
      
      # Begin the profile loop
      # ----------------------
      profiles.each do |p|
        pname = "profile%.2d"%p
        pdir = File.join(mdir,pname)
        cmd = "mkdir #{pdir}"
        system(cmd) unless File.directory?(pdir)
      
        # Construct the generic TAPE5 filename 
        gt5file="#{t5_dir}/TAPE5.#{profile_set}_#{pname}"
        
        
        # Create a Job Control File for this band/molid combination
        n_jobs = 0
        jcf_file = "#{pdir}/#{bname}_#{mname}_#{pname}.jcf"
        jcf = File.open(jcf_file,"w")
        jcf.puts(strip_output(<<-EOF))
          #!/bin/sh
          #
          # Job step for: #{bname}_#{mname}_#{pname}
          # @ job_name = #{bname}_#{mname}_#{pname}
          # @ output = #{bname}_#{mname}_#{pname}.out
          # @ error = #{bname}_#{mname}_#{pname}.err
          # @ class = #{queue}
          # @ group = #{queue}
          # @ wall_clock_limit = #{LL_TIMELIMIT}
          # @ resources = #{LL_RESOURCES}
          # @ account_no = #{LL_ACCOUNT[:ccs]}
          # @ startdate = #{time.strftime("%m/%d/%Y %H:%M")}
          # @ queue
          cd #{root_dir}/#{pdir}
          EOF


        # Begin the angle loop
        # --------------------
        angles.each do |a|
          ainfo = ANGLE_INFO[a]
          aname = "angle%.1d"%a
          adir = File.join(pdir,aname)
          cmd = "mkdir #{adir}"
          system(cmd) unless File.directory?(adir)
          
          # Define an id tag for filenames, and an attribute for netCDF files
          id_tag = adir.gsub(/\//,"_")
          id_att = "(#{adir.gsub(/\//,":")})"
          
          
          # Create the TAPE5 input file for this run
          # ----------------------------------------
          # Read the generic TAPE5 file
          t5 = File.readlines(gt5file)
          # Update the id tag
          t5[0].sub!(/;\s*$/,"; #{id_tag}")
          # Insert the continua scale factors
          t5.insert(2,continua)
          # Modify the first occurrance of the continua flag
          re=[/CN=0/,"CN=6"]
          t5.find_all {|s| s=~re.first}.first.gsub!(re.first,re.last)
          # Every thing else, replace all occurrances
          replace=[[/UUUU.UUU/ ,"%8.3f"%f1_lbl],               # LBL begin frequency
                   [/VVVV.VVV/ ,"%8.3f"%f2_lbl],               # LBL end frequency
                   [/CCC.CCC/  ,"%7.3f"%co2_mr],               # CO2 mixing ratio
                   [/AAA.AAA/  ,"%7.3f"%ainfo[:zenith]],       # Zenith angle
                   [/C.CCCC/   ,"%6.4f"%(DFAVG/2.0)],          # Averaging kernel halfwidth
                   [/D.DDDD/   ,"%6.4f"%DFAVG],                # Output spectral spacing
                   [/SSSS.SSSS/,"%9.4f"%f1_scnmrg],            # Averaging begin frequency
                   [/TTTT.TTTT/,"%9.4f"%f2_scnmrg]]            # Averaging end frequency
          replace.each do |re|
            t5.find_all {|s| s=~re.first}.each {|n| n.gsub!(re.first,re.last)}
          end
          # Write the new tape5 file
          t5file = "tape5.#{id_tag}.rdk"
          File.open("#{pdir}/#{t5file}","w") {|f| f.puts(t5)}

          
          # Write the schell script file
          # ----------------------------
          script_file = "#{pdir}/#{id_tag}.sh"
          open(script_file,"w") {|sf| sf.puts(strip_output(<<-EOF))
            #!/bin/sh
            # LBLRTM transmittance production run script
            # file for the input TAPE5 file:
            #   #{t5file}
            # and using the TAPE3 spectroscopic file:
            #   #{t3file}
            
            # File and directory names
            SCRIPT_NAME=$(basename $0)
            
            RESULTS_DIR="#{aname}"
            LOG_FILE="Error.Log"
            
            TAPE3_FILE="#{t3file}"
            TAPE5_FILE="#{t5file}"
            
            ID_TAG="#{id_tag}"
            ID_ATTRIBUTE="#{id_att}"
            PROFILE_SET="#{profile_set}"
            LBLRTM_HITRAN_VERSION="#{LBLRTM_HITRAN_VERSION}"
            SENSOR_ID="#{SENSOR_ID}"
            
            UP_TAPE_FILE="TAPE20"
            DOWN_TAPE_FILE="TAPE21"
            
            UP_LBL_FILE="upwelling_tau"
            DOWN_LBL_FILE="downwelling_tau"

            UP_NC_FILE="${UP_LBL_FILE}.nc"
            DOWN_NC_FILE="${DOWN_LBL_FILE}.nc"
            
            COMPLEX_TYPE="REAL IMAG"
            UP_INSTRUMENT_REAL_FILE="${UP_LBL_FILE}.${SENSOR_ID}.REAL.TauProfile.nc"
            UP_INSTRUMENT_IMAG_FILE="${UP_LBL_FILE}.${SENSOR_ID}.IMAG.TauProfile.nc"
            DOWN_INSTRUMENT_REAL_FILE="${DOWN_LBL_FILE}.${SENSOR_ID}.REAL.TauProfile.nc"
            DOWN_INSTRUMENT_IMAG_FILE="${DOWN_LBL_FILE}.${SENSOR_ID}.IMAG.TauProfile.nc"
                    
            # Add begin time stamp to error log file
            echo >> ${RESULTS_DIR}/${LOG_FILE}
            echo "--------------------------" >> ${RESULTS_DIR}/${LOG_FILE}
            echo "Processing run started at:  \`date\`" >> ${RESULTS_DIR}/${LOG_FILE}

            # Start LBLRTM run
            lblrun -d -n -r ${RESULTS_DIR} -s ${TAPE3_FILE} ${TAPE5_FILE}
            if [ $? -ne 0 ]; then
              echo "${SCRIPT_NAME}:lblrun fail" >> ${RESULTS_DIR}/${LOG_FILE}
              exit 1
            fi

            # Wait for all file copying to complete
            sleep 10
            
            # Remove the generated TAPE5 file
            mv ${TAPE5_FILE} ${RESULTS_DIR} 2>>${RESULTS_DIR}/${LOG_FILE}

            # Enter the results directory
            cd ${RESULTS_DIR}

            # Rename or remove the data files
            if [ -f ${UP_TAPE_FILE} ]; then
              mv ${UP_TAPE_FILE} ${UP_LBL_FILE} 2>>${LOG_FILE}
            else
              echo "${SCRIPT_NAME}:${UP_TAPE_FILE} not found!" >> ${LOG_FILE}
              ls >> ${LOG_FILE}
              exit 1
            fi
            if [ -f ${DOWN_TAPE_FILE} ]; then
              mv ${DOWN_TAPE_FILE} ${DOWN_LBL_FILE} 2>>${LOG_FILE}
            else
              echo "${SCRIPT_NAME}:${DOWN_TAPE_FILE} not found!" >> ${LOG_FILE}
              ls >> ${LOG_FILE}
              exit 1
            fi
            mv TAPE6 tape6 2>>${LOG_FILE}
            mv TAPE7 tape7 2>>${LOG_FILE}
            rm -f TAPE* 2>>${LOG_FILE}
            rm -f OD* 2>>${LOG_FILE}

            # Convert the LBLRTM files to netCDF
            #
            # Upwelling
            LBL2NC << NoMoreInput
            ${UP_LBL_FILE}
            ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET} profile set.
            Upwelling (Layer->TOA) transmittance. ${LBLRTM_HITRAN_VERSION}
            ${PROFILE_SET}
            #{UPDIRN}
            #{NPANELS}
            NoMoreInput
            sleep 10
            if [ -f ${UP_NC_FILE}.signal ]; then
              rm -f ${UP_LBL_FILE} 2>>${LOG_FILE}
            else
              echo "${SCRIPT_NAME}:${UP_NC_FILE} creation failed" >> ${LOG_FILE}
              exit 1
            fi
            # Downwelling
            LBL2NC << NoMoreInput
            ${DOWN_LBL_FILE}
            ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET} profile set.
            Downwelling (Layer->SFC) transmittance. ${LBLRTM_HITRAN_VERSION}
            ${PROFILE_SET}
            #{DNDIRN}
            #{NPANELS}
            NoMoreInput
            sleep 10
            if [ -f ${DOWN_NC_FILE}.signal ]; then
              rm -f ${DOWN_LBL_FILE} 2>>${LOG_FILE}
            else
              echo "${SCRIPT_NAME}:${DOWN_NC_FILE} creation failed" >> ${LOG_FILE}
              exit 1
            fi


            # Apply the instrument apodisation
            #
            # Upwelling
            Apodize_TauSpc_with_IRF << NoMoreInput
            ${UP_NC_FILE}
            #{PROFILE_SET_INFO[profile_set][:id]}
            #{m}
            #{p}
            #{a}
            #{UPDIRN}
            #{b}
            NoMoreInput
            sleep 10
            for TYPE in ${COMPLEX_TYPE}; do
              FILE="${UP_LBL_FILE}.${SENSOR_ID}.${TYPE}.TauProfile.nc"
              if [ -f ${FILE}.signal ]; then
                rm -f ${UP_NC_FILE} 2>>${LOG_FILE}
              else
                echo "${SCRIPT_NAME}:${FILE} creation failed" >> ${LOG_FILE}
                exit 1
              fi
            done
            # Downwelling
            Apodize_TauSpc_with_IRF << NoMoreInput
            ${DOWN_NC_FILE}
            #{PROFILE_SET_INFO[profile_set][:id]}
            #{m}
            #{p}
            #{a}
            #{DNDIRN}
            #{b}
            NoMoreInput
            sleep 10
            for TYPE in ${COMPLEX_TYPE}; do
              FILE="${DOWN_LBL_FILE}.${SENSOR_ID}.${TYPE}.TauProfile.nc"
              if [ -f ${FILE}.signal ]; then
                rm -f ${DOWN_NC_FILE} 2>>${LOG_FILE}
              else
                echo "${SCRIPT_NAME}:${FILE} creation failed" >> ${LOG_FILE}
                exit 1
              fi
            done
            
            # Add end time stamp to error log file
            echo "Processing run finished at: \`date\`" >> ${LOG_FILE}

            # Remove the script
            cd ..
            rm -f ${SCRIPT_NAME} 2>>${RESULTS_DIR}/${LOG_FILE}
           
            EOF
          }

          # Make the script file executable
          File.chmod(0755, script_file)

          # Add the current script to the Job Control File
          jcf.puts(File.basename(script_file))

          # Increment job counter
          n_jobs += 1
          
        end # angle loop
      
        # Submit the job command file
        jcf.close
        cmd = "#{LL_SUBMIT} #{jcf_file}"
        system(cmd) unless noop

      end # profile loop

      # Increment the submit time
      time = time + SUBMIT_INCREMENT

      puts("  Completed submission of #{bname},#{mname} jobs.")

    end # molecule loop
  end # band loop                
   
rescue StandardError => error_message
  puts("\nERROR: #{error_message}\n")
  exit 1
end  # error handling block
