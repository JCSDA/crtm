require 'fileutils'
require 'base/base'
module TauProd
  module Apodize
    class RunInfo < TauProd::Apodize::Base
      attr_accessor :root_dir,
                    :band    ,:band_info    ,:band_name    ,
                    :molecule,:molecule_info,:molecule_name,
                    :profile                ,:profile_name ,:profile_dir,
                    :angle   ,:angle_info   ,:angle_name   ,:angle_dir  ,
                    :f1_scnmrg,:f2_scnmrg,
                    :f1_lbl   ,:f2_lbl   ,
                    :continua,
                    :id_tag,:id_attribute,
                    :gt5_file,
                    :t5_file,
                    :t3_file,
                    :job_name,:jcf_file,:jcf_id,:n_jobs,
                    :script_file,
                    :time

      def initialize(config)
        @config   = config
        @root_dir = Dir.pwd
        @time     = Time.now + config.start_delay
      end


      def setup(band,molecule,profile)
        
        @band     = band
        @molecule = molecule
        @profile  = profile
        
        @band_info     = TauProd::Config::BAND_INFO[band]
        @molecule_info = TauProd::Config::MOL_INFO[molecule]
        
        @band_name     = "band%.1d" % band
        @molecule_name = @molecule_info[:name]
        @profile_name  = "profile%.2d" % profile
        
        @f1_scnmrg = @band_info[:f1_scnmrg]      # Begin frequency for SCNMRG
        @f2_scnmrg = @band_info[:f2_scnmrg]      # End frequency for SCNMRG
        @f1_lbl = @f1_scnmrg - 1.0               # Begin frequency for LBL calcs
        @f2_lbl = @f2_scnmrg + 1.0               # End frequency for LBL calcs

        @continua = "%3.1f "*@molecule_info[:cont].size % @molecule_info[:cont]
 
        @profile_dir = File.join(@band_name,@molecule_name,@profile_name)
        
        @gt5_file = "#{config.t5_dir}/TAPE5.#{config.profile_set}_#{@profile_name}"
        @t3_file  = "tape3.#{@molecule_info[:t3tag]}.#{config.t3_id}"
        
        create_dir(@profile_dir)
        
        @job_name = @profile_dir.gsub(/\//,"_")
        @jcf_file = "#{@profile_dir}/#{@job_name}.jcf"
        @n_jobs   = 0
        create_jcf()
        
      end

      def angle_setup(angle)
        @angle      = angle
        @angle_info = TauProd::Config::ANGLE_INFO[angle]
        @angle_name = "angle%.1d" % angle
        @angle_dir  = File.join(@profile_dir,@angle_name)
        
        @id_tag       = @angle_dir.gsub(/\//,"_")
        @id_attribute = "(" + @angle_dir.gsub(/\//,":") + ")"
        
        @t5_file  = "tape5.#{@id_tag}.rdk"
        
        @script_file = "#{@profile_dir}/#{@id_tag}.sh"
        @jcf_id.puts(File.basename(@script_file))
        
        create_dir(@angle_dir)
        create_tape5()
        create_script()
      end

      def submit
        @jcf_id.close
        cmd = "#{TauProd::Config::LL_SUBMIT} #{@jcf_file}"
        puts(cmd) if config.debug
        system(cmd) unless config.noop
      end        
        
      def increment_time    
        @time += TauProd::Config::SUBMIT_INCREMENT
      end


      private


      def create_script
        open(@script_file,"w") {|sf| sf.puts(strip_output(<<-EOF))
          #!/bin/sh
          # LBLRTM transmittance production run script
          # file for the input TAPE5 file:
          #   #{@t5_file}
          # and using the TAPE3 spectroscopic file:
          #   #{@t3_file}
          
          #{@config.debug ? "set -x" : ""}
          
          # File and directory names
          SCRIPT_NAME=$(basename $0)

          LOG_FILE="Process.Log"

          KEEP_LBL="#{@config.keeplbl}"            
          RESULTS_DIR="#{@angle_name}"
          TAPE3_FILE="#{@t3_file}"
          TAPE5_FILE="#{@t5_file}"
          ID_TAG="#{@id_tag}"
          ID_ATTRIBUTE="#{@id_attribute}"
          PROFILE_SET="#{@config.profile_set}"
          LBLRTM_HITRAN_VERSION="#{TauProd::Config::LBLRTM_HITRAN_VERSION}"
          SENSOR_ID="#{TauProd::Config::SENSOR_ID}"
          
          UP_TAPE_FILE="TAPE20"
          DOWN_TAPE_FILE="TAPE21"
          
          UP_LBL_FILE="upwelling"
          DOWN_LBL_FILE="downwelling"

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

          # Remove any old, lingering signal files
          rm -f *.signal 2>>${LOG_FILE}

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
          #{TauProd::Config::UPDIRN}
          #{TauProd::Config::NPANELS}
          NoMoreInput
          sleep 10
          if [ -f ${UP_NC_FILE}.signal ]; then
            if [ "${KEEP_LBL}" = "false" ]; then
              rm -f ${UP_LBL_FILE} 2>>${LOG_FILE}
            fi
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
          #{TauProd::Config::DNDIRN}
          #{TauProd::Config::NPANELS}
          NoMoreInput
          sleep 10
          if [ -f ${DOWN_NC_FILE}.signal ]; then
            if [ "${KEEP_LBL}" = "false" ]; then
              rm -f ${DOWN_LBL_FILE} 2>>${LOG_FILE}
            fi
          else
            echo "${SCRIPT_NAME}:${DOWN_NC_FILE} creation failed" >> ${LOG_FILE}
            exit 1
          fi


          # Apply the instrument apodisation
          #
          # Upwelling
          Apodize_TauSpc_with_IRF << NoMoreInput
          ${UP_NC_FILE}
          #{TauProd::Config::PROFILE_SET_INFO[@config.profile_set][:id]}
          #{@molecule}
          #{@profile}
          #{@angle}
          #{TauProd::Config::UPDIRN}
          #{@band}
          NoMoreInput
          sleep 10
          for TYPE in ${COMPLEX_TYPE}; do
            FILE="${UP_LBL_FILE}.${SENSOR_ID}.${TYPE}.TauProfile.nc"
            if [ -f ${FILE}.signal ]; then
              if [ "${KEEP_LBL}" = "false" ]; then
                rm -f ${UP_NC_FILE} 2>>${LOG_FILE}
              fi
            else
              echo "${SCRIPT_NAME}:${FILE} creation failed" >> ${LOG_FILE}
              exit 1
            fi
          done
          # Downwelling
          Apodize_TauSpc_with_IRF << NoMoreInput
          ${DOWN_NC_FILE}
          #{TauProd::Config::PROFILE_SET_INFO[@config.profile_set][:id]}
          #{@molecule}
          #{@profile}
          #{@angle}
          #{TauProd::Config::DNDIRN}
          #{@band}
          NoMoreInput
          sleep 10
          for TYPE in ${COMPLEX_TYPE}; do
            FILE="${DOWN_LBL_FILE}.${SENSOR_ID}.${TYPE}.TauProfile.nc"
            if [ -f ${FILE}.signal ]; then
              if [ "${KEEP_LBL}" = "false" ]; then
                rm -f ${DOWN_NC_FILE} 2>>${LOG_FILE}
              fi
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
        File.chmod(0755, @script_file)
        
        # Increment job counter
        @n_jobs += 1
      end

      def create_tape5
        # Read the generic TAPE5 file
        t5 = File.readlines(@gt5_file)

        # Update the id tag
        t5[0].sub!(/;\s*$/,"; #{@id_tag}")

        # Insert the continua scale factors
        t5.insert(2,@continua)

        # Modify only the first occurrance of the continua flag
        re=[/CN=0/,"CN=6"]
        t5.find_all {|s| s=~re.first}.first.gsub!(re.first,re.last)

        # Every thing else, replace all occurrances
        replace=[[/UUUU.UUU/ ,"%8.3f"%@f1_lbl],                      # LBL begin frequency
                 [/VVVV.VVV/ ,"%8.3f"%@f2_lbl],                      # LBL end frequency
                 [/CCC.CCC/  ,"%7.3f"%config.co2_mr],                # CO2 mixing ratio
                 [/AAA.AAA/  ,"%7.3f"%@angle_info[:zenith]],         # Zenith angle
                 [/C.CCCC/   ,"%6.4f"%(TauProd::Config::DFAVG/2.0)], # Averaging kernel halfwidth
                 [/D.DDDD/   ,"%6.4f"%TauProd::Config::DFAVG],       # Output spectral spacing
                 [/SSSS.SSSS/,"%9.4f"%@f1_scnmrg],                   # Averaging begin frequency
                 [/TTTT.TTTT/,"%9.4f"%@f2_scnmrg]]                   # Averaging end frequency
        replace.each do |re|
          t5.find_all {|s| s=~re.first}.each {|n| n.gsub!(re.first,re.last)}
        end
        
        # Write the new tape5 file
        File.open("#{@profile_dir}/#{@t5_file}","w") {|f| f.puts(t5)}
      end

      def create_jcf
        # Create a Job Control File for this band/molid combination
        @jcf_id = File.open(@jcf_file,"w")
        @jcf_id.puts(strip_output(<<-EOF))
          #!/bin/sh
          #
          # Job step for: #{@job_name}
          # @ job_name = #{@job_name}
          # @ output = #{@root_dir}/#{@profile_dir}/#{@job_name}.out
          # @ error = #{@root_dir}/#{@profile_dir}/#{@job_name}.err
          # @ class = #{config.queue}
          # @ group = #{config.queue}
          # @ wall_clock_limit = #{TauProd::Config::LL_TIMELIMIT}
          # @ resources = #{TauProd::Config::LL_RESOURCES}
          # @ account_no = #{TauProd::Config::LL_ACCOUNT[:ccs]}
          # @ startdate = #{@time.strftime("%m/%d/%Y %H:%M")}
          # @ queue
          cd #{@root_dir}/#{@profile_dir}
          EOF
      end 

      def create_dir(dir)
        FileUtils.mkdir_p(dir) unless File.directory?(dir)
      end
    end
  end
end
