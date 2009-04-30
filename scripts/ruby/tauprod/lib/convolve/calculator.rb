require 'convolve/base'
require 'convolve/runinfo'
module TauProd
  module Convolve
    class Processor < TauProd::Convolve::Base
    
      def initialize(config)
        @config  = config
        @runinfo = RunInfo.new(config)
      end
      
      def process
        puts self.class

        # Begin the error handling block
        # ------------------------------
        begin

          # Begin iteration over cases
          # --------------------------
          @config.molids.each do |m|
            @config.profiles.each do |p|
              @config.angles.each do |a|

                @runinfo.setup(m,p,a)
                @config.bands.each {|b| @runinfo.band_setup(b)}
                @runinfo.submit
                
              end # angle loop
              
              @runinfo.increment_time

            end # profile loop
          end # molecule loop
           
        rescue StandardError => error_message
          puts("\nERROR: #{error_message}\n")
          exit 1
        end  # error handling block

      end  # process method
    end  # Processor class
  end  # Convolve module
end  # TauProd module
              
#        root_dir = Dir.pwd
#
#        # Begin the error handling block
#        # ------------------------------
#        begin
#
#          time  = Time.now + config.start_delay
#          config.profiles.each do |p|
#            ptag = "profile%.2d"%p
# 
#            # Create profile directories
#            cmd = "mkdir #{ptag}"
#            puts("\n#{self.class}: #{cmd}") if config.debug
#            system(cmd) unless File.directory?(ptag)
#
#            # Generic tape5 filename
#            gt5file="#{config.tape5_dir}/TAPE5.#{config.profile_set_id}_#{ptag}"
#
#
#            # Begin angle loop
#            # ----------------
#            config.angles.each do |a|
#              angle_info = TauProd::Base::ANGLE_INFO[a]
#              atag = "angle%.1d"%a
#              
#              # Create the molid directory
#              adir = File.join(ptag,atag)
#              cmd = "mkdir #{adir}"
#              puts("#{self.class}: #{cmd}") if config.debug
#              system(cmd) unless File.directory?(adir)
#                
#              
#              # Begin molid loop
#              # ----------------
#              config.molids.each do |m|
#                mol_info = TauProd::Base::MOL_INFO[m]
#                mtag = mol_info[:name]
#
#                # Specify continua
#                continua = "%3.1f "*mol_info[:cont].size % mol_info[:cont]
#
#                # Construct the tape3 filename
#                t3file = "tape3#{mol_info[:t3tag]}.#{config.tape3_id}"
#                
#                # Create the molid directory
#                mdir = File.join(adir,mtag)
#                cmd = "mkdir #{mdir}"
#                puts("#{self.class}: #{cmd}") if config.debug
#                system(cmd) unless File.directory?(mdir)
#                
#                # Create Job Control File (jcf) file for current band set
#                # I use a random number between 0 and 100000
#                # to tag the file since the bands are no longer 
#                # necessarily contiguous
##                jcf_file = ""
##                loop do
##                  jcf_file = mdir.gsub(/\//,"_") << "_" << "%.6d" % rand(100000) << ".jcf"
##                  break unless File.exists?(jcf_file)
##                end
##                jcf_file = ""
#                jcf_file = mdir.gsub(/\//,"_") << ".jcf"
#                jcf = File.open(jcf_file,"w")
#                
#                # Define the script interpreter
#                jcf.puts("#!/bin/sh")
#
#                
#                # Begin band loop
#                # ---------------
#                config.bands.each do |b|
#                  btag = "band%.3d" % b
#                  
#                  # Create the band directory
#                  bdir = File.join(mdir,btag)
#                  cmd = "mkdir #{bdir}"
#                  puts("#{self.class}: #{cmd}") if config.debug
#                  system(cmd) unless File.directory?(bdir)
#                
#                  # Assign frequency parameters for this band
#                  # Actual frequencies used in LBLRTM calculation has
#                  # one unit of slop either side.
#                  f1 = config.f1_band1 + ((b-1)*config.df_band)
#                  f2 = f1 + config.df_band
#                  f1_lbl = f1 - 1.0               # Begin frequency for LBL calcs
#                  f2_lbl = f2 + 1.0               # End frequency for LBL calcs
#                  f1_scnmrg = f1                  # Begin frequency for SCNMRG
#                  f2_scnmrg = f2 - config.df_avg  # End frequency for SCNMRG
#                  
#                  # Define an id tag for filenames, and an attribute for netCDF files
#                  id_tag = bdir.gsub(/\//,"_")
#                  id_att = "("<<bdir.gsub(/\//,":")<<")"
#
#                  # TAPE5 input filename for this run
#                  t5file = "tape5.#{id_tag}.rdk"
#
#                  # Create band/angle/continua specific TAPE5 file
#                  t5 = File.readlines(gt5file)
#                  t5[0].sub!(/;\s*$/,"; #{id_tag}")
#                  t5.insert(2,continua)
#                  # The continua flag...only replace first occurrance
#                  re=[/CN=0/,"CN=6"]
#                  t5.find_all {|s| s=~re.first}.first {|n| n.gsub!(re.first,re.last)}
#                  # Every thing else, replace all occurrances
#                  replace=[[/UUUU.UUU/ ,"%8.3f"%f1_lbl],               # LBL begin frequency
#                           [/VVVV.VVV/ ,"%8.3f"%f2_lbl],               # LBL end frequency
#                           [/CCC.CCC/  ,"%7.3f"%config.co2mr],         # CO2 mixing ratio
#                           [/AAA.AAA/  ,"%7.3f"%angle_info[:zenith]],  # Zenith angle
#                           [/C.CCCC/   ,"%6.4f"%(config.df_avg/2.0)],  # Averaging kernel halfwidth
#                           [/D.DDDD/   ,"%6.4f"%config.df_avg],        # Output spectral spacing
#                           [/SSSS.SSSS/,"%9.4f"%f1_scnmrg],            # Averaging begin frequency
#                           [/TTTT.TTTT/,"%9.4f"%f2_scnmrg]]            # Averaging end frequency
#                  replace.each do |re|
#                    t5.find_all {|s| s=~re.first}.each {|n| n.gsub!(re.first,re.last)}
#                  end
#                  # Write the new tape5 file
#                  File.open(t5file,"w") {|f| f.puts(t5)}
#                  
#
#                  # Write the script file
#                  # ---------------------
#                  script_file = "#{id_tag}.sh"
#                  open(script_file,"w") {|sf| sf.puts(strip_output(<<-EOT))
#                    #!/bin/sh
#                    # LBLRTM transmittance production run script
#                    # file for the input TAPE5 file:
#                    #   #{t5file}
#                    # and using the TAPE3 spectroscopic file:
#                    #   #{t3file}
#                    
#                    # File and directory names
#                    SCRIPT_NAME=$(basename $0)
#                    
#                    RESULTS_DIR="#{bdir}"
#                    LOG_FILE="Error.Log"
#                    
#                    TAPE3_FILE="#{t3file}"
#                    TAPE5_FILE="#{t5file}"
#                    
#                    ID_TAG="#{id_tag}"
#                    ID_ATTRIBUTE="#{id_att}"
#                    PROFILE_SET_ID="#{config.profile_set_id}"
#                    LBLRTM_HITRAN_VERSION="#{TauProd::Base::LBLRTM_HITRAN_VERSION}"
#                    
#                    UPWELLING_TAPE_FILE="TAPE20"
#                    DOWNWELLING_TAPE_FILE="TAPE21"
#                    
#                    UPWELLING_LBL_FILE="upwelling_tau.${ID_TAG}"
#                    DOWNWELLING_LBL_FILE="downwelling_tau.${ID_TAG}"
#
#                    UPWELLING_NC_FILE="${UPWELLING_LBL_FILE}.nc"
#                    DOWNWELLING_NC_FILE="${DOWNWELLING_LBL_FILE}.nc"
#                    
#
#                    # Change to required directory
#                    cd #{root_dir}
#                    
#                    # Add begin time stamp to error log file
#                    echo >> ${RESULTS_DIR}/${LOG_FILE}
#                    echo "--------------------------" >> ${RESULTS_DIR}/${LOG_FILE}
#                    echo "Processing run started at:  \`date\`" >> ${RESULTS_DIR}/${LOG_FILE}
#
#                    # Start LBLRTM run
#                    lblrun -d -n -r #{bdir} -s ${TAPE3_FILE} ${TAPE5_FILE}
#                    if [ $? -ne 0 ]; then
#                      echo "${SCRIPT_NAME}:lblrun fail" >> ${RESULTS_DIR}/${LOG_FILE}
#                      exit 2
#                    fi
#
#                    # Wait for all file copying to complete
#                    sleep 10
#                    
#                    # Remove the generated TAPE5 file
#                    mv ${TAPE5_FILE} ${RESULTS_DIR} 2>>${RESULTS_DIR}/${LOG_FILE}
#
#                    # Rename or remove the data files
#                    cd ${RESULTS_DIR}
#                    if [ -f ${UPWELLING_TAPE_FILE} ]; then
#                      mv ${UPWELLING_TAPE_FILE} ${UPWELLING_LBL_FILE}
#                    else
#                      echo "${SCRIPT_NAME}:${UPWELLING_TAPE_FILE} not found!" >> ${LOG_FILE}
#                      ls >> ${LOG_FILE}
#                      exit 2
#                    fi
#                    if [ -f ${DOWNWELLING_TAPE_FILE} ]; then
#                      mv ${DOWNWELLING_TAPE_FILE} ${DOWNWELLING_LBL_FILE}
#                    else
#                      echo "${SCRIPT_FILE}:${DOWNWELLING_TAPE_FILE} not found!" >> ${LOG_FILE}
#                      ls >> ${LOG_FILE}
#                      exit 2
#                    fi
#                    mv TAPE6 tape6.${ID_TAG} 2>>${LOG_FILE}
#                    mv TAPE7 tape7.${ID_TAG} 2>>${LOG_FILE}
#                    rm -f TAPE* 2>>${LOG_FILE}
#                    rm -f OD* 2>>${LOG_FILE}
#
#                    # Convert the LBLRTM files to netCDF
#                    #
#                    # Upwelling
#                    hpmcount LBLRTM_to_netCDF << NoMoreInput
#                    ${UPWELLING_LBL_FILE}
#                    ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set. Upwelling (Layer->TOA) transmittance.
#                    ${LBLRTM_HITRAN_VERSION}
#                    ${PROFILE_SET_ID}
#                    NoMoreInput
#                    sleep 10
#                    if [ ! -f ${UPWELLING_LBL_FILE}.nc.signal ]; then
#                      echo "${SCRIPT_FILE}:${UPWELLING_LBL_FILE}.nc creation failed" >> ${LOG_FILE}
#                      exit 2
#                    fi
#                    # Downwelling
#                    hpmcount LBLRTM_to_netCDF << NoMoreInput
#                    ${DOWNWELLING_LBL_FILE}
#                    ${ID_ATTRIBUTE} Transmittance production run for ${PROFILE_SET_ID} profile set. Downwelling (Layer->SFC) transmittance.
#                    ${LBLRTM_HITRAN_VERSION}
#                    ${PROFILE_SET_ID}
#                    NoMoreInput
#                    sleep 10
#                    if [ ! -f ${DOWNWELLING_LBL_FILE}.nc.signal ]; then
#                      echo "${SCRIPT_FILE}:${DOWNWELLING_LBL_FILE}.nc creation failed" >> ${LOG_FILE}
#                      exit 2
#                    fi
#
#                    # Remove LBLRTM format files
#                    rm -f ${UPWELLING_LBL_FILE} 2>>${LOG_FILE}
#                    rm -f ${DOWNWELLING_LBL_FILE} 2>>${LOG_FILE}
#
#                    # Add end time stamp to error log file
#                    echo "Processing run finished at: \`date\`" >> ${LOG_FILE}
#
#                    # Remove the script
#                    cd ${ROOT_DIR}
#                    rm -f ${SCRIPT_NAME} 2>>${RESULTS_DIR}/${LOG_FILE}
#
#                    EOT
#                  }
#                  
#                  # Make the script file executable
#                  File.chmod(0755, script_file)
#
#                  
#                  # Add the current script as a job step in the job command file
#                  # ------------------------------------------------------------
#                  jcf.puts(strip_output(<<-EOT))
#                    #
#                    # Job step for: #{id_tag}
#                    # @ job_name = #{ptag}_#{atag}_#{mtag}
#                    # @ executable = #{script_file}
#                    # @ step_name = #{script_file}
#                    # @ output = #{script_file}.out
#                    # @ error = #{script_file}.out
#                    # @ class = #{config.queue}
#                    # @ wall_clock_limit = 00:15:00
#                    # @ account_no = GDAS-T2O
#                    # @ queue
#                    EOT
#
#                  # Increment job counter
#                  njobs+=1
#
#                end  # band loop
#                
#                # Close the jcf file
#                jcf.close
#              
#                # Submit the job command file
#                puts("  Number of spectral band jobs steps in #{jcf_file}: #{config.bands.length}")
#                cmd = "llsubmit #{jcf_file}"
#                puts("#{self.class}: #{cmd}") if config.debug
#                system(cmd)
#
#              end  # molid loop
#              puts("  Number of molecule set jobs submitted: #{config.molids.length}")
#            end  # angle loop
#            puts("  Number of angle jobs submitted: #{config.angles.length}")
#          end  # profile loop
#          puts("  Number of profile jobs submitted: #{config.profiles.length}")
#          puts("  Number of total jobs submitted: #{njobs}")
#          
#        rescue StandardError=>error_message
#          puts("\nERROR--#{self.class}: #{error_message}\n")
#          exit 1
#        end  # error handling block


