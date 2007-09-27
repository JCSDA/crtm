#!/usr/bin/env ruby

# ------------
# Set up stuff
# ------------
LL_SUBMIT    = "llsubmit"
LL_QUEUE     = "dev"
LL_ACCOUNT   = { :ccs=>"GDAS-T2O", :haze=>"JCSDA"}
LL_TIMELIMIT = "02:00:00"
LL_RESOURCES = "ConsumableCpus(1) ConsumableMemory(500)"

#TAPE5_DIR = "./TAPE5"
TAPE5_DIR = "/usr1/wd20pd/src/CRTM/trunk/TauProd/Infrared/Create_LBLRTM_Input_Files/TAPE5_files/UMBC_fixed"
PROFILE_SET_ID = "UMBC"
PROFILE_SET = 1
TAPE3_ID = "hitran2000_aer"
LBLRTM_HITRAN_VERSION = "LBLRTM v9.4; HITRAN 2000 + AER updates"
CO2MR   = 380.0  # Default CO2 mixing ratio in ppmv
DFAVG   = 0.001  # Averaging kernel width in cm^-1
NPANELS = 1      # No. of LBL panels
UPDIRN  = 1      # Direction flag for upwelling
DNDIRN  = 2      # Direction flag for upwelling
SENSOR_ID = "iasi_metopa"

# Literal constants
ZERO=0.0; ONE=1.0

# Band information
BAND_INFO = { 1 => {:f1 =>  645.0, :f2 => 1209.75, :npts => 2260},
              2 => {:f1 => 1210.0, :f2 => 1999.75, :npts => 3160},
              3 => {:f1 => 2000.0, :f2 => 2760.00, :npts => 3041}}
bands = BAND_INFO.keys.sort
    
# Molecule information
MOL_INFO=Hash.new
(1..7).to_a.collect {|m| MOL_INFO[m] = {:name  => "mol#{m}",
                                        :t3tag => ".mol#{m}",
                                        :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}}
MOL_INFO[8]  = {:name  => "all_nocontinua",
                :t3tag => "",
                :cont  => [ZERO,ZERO,ZERO,ZERO,ZERO,ZERO,ZERO]}
MOL_INFO[9]  = {:name  => "continua_only",
                :t3tag => ".nomol",
                :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
MOL_INFO[10] = {:name  => "all_withcontinua",
                :t3tag => "",
                :cont  => [ONE,ONE,ONE,ONE,ONE,ONE,ONE]}
MOL_INFO[11] = {:name  => "wvo",
                :t3tag => ".wvo",
                :cont  => [ONE,ONE,ZERO,ONE,ZERO,ZERO,ZERO]}
MOL_INFO[12] = {:name  => "wet",
                :t3tag => ".mol1",
                :cont  => [ONE,ONE,ZERO,ZERO,ZERO,ZERO,ZERO]}
MOL_INFO[13] = {:name  => "dry",
                :t3tag => ".dry",
                :cont  => [ZERO,ZERO,ONE,ZERO,ONE,ONE,ONE]}
MOL_INFO[14] = {:name  => "ozo",
                :t3tag => ".mol3",
                :cont  => [ZERO,ZERO,ZERO,ONE,ZERO,ZERO,ZERO]}
MOL_INFO[15] = {:name  =>"wco",
                :t3tag =>".nomol",
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

PROFILE1 = 1
PROFILE2 = 48
profiles = (PROFILE1..PROFILE2).collect {|x| x}

# Method to replace only the first
# occurance of the leading spaces
# in each line of input text.
def strip_output(text)
  text =~ /^\s+/
  leading_spaces = $&
  text = text.to_a.collect {|l| l.sub(/^#{leading_spaces}/,"")}.to_s
end


# ----------------------------------
# The start of the script processing
# ----------------------------------
if $0 == __FILE__

# Define the current directory
root_dir = Dir.pwd

# Begin the error handling block
begin
  
  # Begin the band loop
  # -------------------
  bands.each do |b|
    binfo = BAND_INFO[b]
    bname = "band%.1d" % b
    bdir = bname
    cmd = "mkdir #{bdir}"
#    system(cmd) unless File.directory?(bdir)
    
    # Assign the frequency parameters for this band
    f1_lbl = binfo[:f1] - 1.0   # Begin frequency for LBL calcs
    f2_lbl = binfo[:f2] + 1.0   # End frequency for LBL calcs
    f1_scnmrg = binfo[:f1]      # Begin frequency for SCNMRG
    f2_scnmrg = binfo[:f2]      # End frequency for SCNMRG
    

    # Begin the molecule loop
    # -----------------------
    molids.each do |m|
      minfo = MOL_INFO[m]
      mname = minfo[:name]
      mdir = File.join(bdir,mname)
      cmd = "mkdir #{mdir}"
#      system(cmd) unless File.directory?(mdir)
      
      # Specify the continua
      continua = "%3.1f "*minfo[:cont].size % minfo[:cont]
    
      # Construct the tape3 filename
      t3file = "tape3#{minfo[:t3tag]}.#{TAPE3_ID}"
      
      # Create a Job Control File for this band/molid combination
      n_jobs = 0
      jcf_file = "#{bname}_#{mname}.jcf"
      jcf = File.open(jcf_file,"w")
      jcf.puts(strip_output(<<-EOF))
        #!/bin/sh
        #
        # Job step for: #{bname}_#{mname}
        # @ job_name = #{bname}_#{mname}
        # @ output = #{bname}_#{mname}.out
        # @ error = #{bname}_#{mname}.err
        # @ class = #{LL_QUEUE}
        # @ group = #{LL_QUEUE}
        # @ wall_clock_limit = #{LL_TIMELIMIT}
        # @ resources = #{LL_RESOURCES}
        # @ account_no = #{LL_ACCOUNT[:ccs]}
        # @ queue
        EOF

      
      # Begin the profile loop
      # ----------------------
      profiles.each do |p|
        pname = "profile%.2d"%p
        pdir = File.join(mdir,pname)
        cmd = "mkdir #{pdir}"
#        system(cmd) unless File.directory?(pdir)
      
        # Construct the generic TAPE5 filename 
        gt5file="#{TAPE5_DIR}/TAPE5.#{PROFILE_SET_ID}_#{pname}"
        
        
        # Begin the angle loop
        # --------------------
        angles.each do |a|
          ainfo = ANGLE_INFO[a]
          aname = "angle%.1d"%a
          adir = File.join(pdir,aname)
          cmd = "mkdir #{adir}"
#          system(cmd) unless File.directory?(adir)
          
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
                   [/CCC.CCC/  ,"%7.3f"%CO2MR],                # CO2 mixing ratio
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
          File.open(t5file,"w") {|f| f.puts(t5)}

          
          # Write the schell script file
          # ----------------------------
          script_file = "#{id_tag}.sh"
          open(script_file,"w") {|sf| sf.puts(strip_output(<<-EOF))
            #!/bin/sh
            # LBLRTM transmittance production run script
            # file for the input TAPE5 file:
            #   #{t5file}
            # and using the TAPE3 spectroscopic file:
            #   #{t3file}
            
            # File and directory names
            SCRIPT_NAME=$(basename $0)
            
            RESULTS_DIR="#{adir}"
            LOG_FILE="Error.Log"
            
            TAPE3_FILE="#{t3file}"
            TAPE5_FILE="#{t5file}"
            
            ID_TAG="#{id_tag}"
            ID_ATTRIBUTE="#{id_att}"
            PROFILE_SET_ID="#{PROFILE_SET_ID}"
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
                    
            # Change to required directory
            cd #{root_dir}
            
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

            # Rename or remove the data files
            cd ${RESULTS_DIR}
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
            hpmcount LBL2NC << NoMoreInput
            ${UP_LBL_FILE}
            ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set. Upwelling (Layer->TOA) transmittance. ${LBLRTM_HITRAN_VERSION}
            ${PROFILE_SET_ID}
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
            hpmcount LBLRTM_to_netCDF << NoMoreInput
            ${DOWN_LBL_FILE}
            ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set. Downwelling (Layer->SFC) transmittance. ${LBLRTM_HITRAN_VERSION}
            ${PROFILE_SET_ID}
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
            hpmcount Apodize_TauSpc_with_IRF << NoMoreInput
            ${UP_NC_FILE}
            #{PROFILE_SET}
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
            fi
            # Downwelling
            hpmcount Apodize_TauSpc_with_IRF << NoMoreInput
            ${DOWN_LBL_FILE}
            #{PROFILE_SET}
            #{m}
            #{p}
            #{a}
            #{DNDIRN}
            #{b}
            NoMoreInput
            sleep 10
            for TYPE in ${COMPLEX_TYPE}; do
              FILE="${DOWN_LBL_FILE}.${SENSOR_ID}.${TYPE}.TauProfile.nc"
              if [ ! -f ${FILE}.signal ]; then
                rm -f ${DOWN_NC_FILE} 2>>${LOG_FILE}
              else
                echo "${SCRIPT_NAME}:${FILE} creation failed" >> ${LOG_FILE}
                exit 1
              fi
            fi
            
            # Add end time stamp to error log file
            echo "Processing run finished at: \`date\`" >> ${LOG_FILE}

            # Remove the script
            cd #{root_dir}
            rm -f ${SCRIPT_NAME} 2>>${RESULTS_DIR}/${LOG_FILE}
           
            EOF
          }

          # Make the script file executable
          File.chmod(0755, script_file)

          # Add the current script to the Job Control File
          jcf.puts("#{script_file}")

          # Increment job counter
          n_jobs += 1
          
        end # angle loop
      end # profile loop
      
      # Submit the job command file
      jcf.close
      puts("  Number of jobs in #{jcf_file}: #{n_jobs}")
      cmd = "#{LL_SUBMIT} #{jcf_file}"
#      system(cmd)

    end # molecule loop
  end # band loop                
   
rescue StandardError=>error_message
  puts("\nERROR--#{error_message}\n")
  exit 1
end  # error handling block

end
