#!/bin/sh 

RUN_SPEC=0       # 1 - run lblrtm and SRF convolusion; 0 - not to run
RUN_MERGE=0      # 1 - run merge; 0 - not run merge
RUN_CONCAT=1     # 1 - run concatenation; 0 - not run concatenation
RUN_CRTM=0       # 1 - run crtm; 0 - not run crtm

SENSOR_INDEX=11  # 1 == IASI; 2 == CRIS; 10 == airsM1b_aqua; 11 == airsM2b_aqua 
BAND=3
if [ $SENSOR_INDEX == 1 ]; then
  SENSOR=iasiB${BAND}_metop-a
fi
if [ $SENSOR_INDEX == 2 ]; then
  SENSOR=crisB${BAND}_npp
fi
if [ $SENSOR_INDEX == 10 ]; then
  SENSOR=airsM1b_aqua
fi
if [ $SENSOR_INDEX == 11 ]; then
  SENSOR=airsM2b_aqua
fi

CASE_LIST='1 2'  # 1 == nlte; 2 == lte

SOLOR_ANGLE_LIST='0 40 60 80 85 90'
SENSOR_ANGLE_LIST='1.00 1.25 1.50 1.75 2.00 2.25 2.50 2.75 3.00 3.25 3.50 3.75 4.00'

CRTM_DIR=/u/wx23yh/CRTM/EXP-NLTE
WORK_ROOT=/u/wx23yh/noscrub_jcsda/work_nlte

LBLRTM_EXE=/u/wx23yh/bin/lblrtm_v11.7_NLTE_dbl
TAPE3=${CRTM_DIR}/externals//AER/LBLRTM/examples/TAPE3_files/TAPE3_aer_v_2.5_big_endian
TAPE5_DIR=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Set_tape4tape5/tape5_files
TAPE4_DIR=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Set_tape4tape5/tape4_files
LBLRTM_INPUTFILE_DIR=${CRTM_DIR}/externals/AER/NLTE_data/lblrtm_inputFiles

LBL2NCDF_EXE=${CRTM_DIR}/src/TauProd/Infrared/LBLRTM_to_netCDF/LBLRTM_to_netCDF
MERGE_FILE_EXE=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Merge_files/Merge_files
CONCAT_FILE_EXE=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Concat/Cat_TrainSet

if [ $SENSOR_INDEX -lt 10 ];then  # interferometers
  CONVOL_SRF_EXE=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Apodize_RadSpc_with_IRF/Apodize_RadSpc_with_IRF
else # ordinary sensors
  CONVOL_SRF_EXE=${CRTM_DIR}/src/NLTE/Compute_TrainSet/Convolve_RadSpc_with_SRF/Convolve_RadSpc_with_SRF
  SRF_FILE=${CRTM_DIR}/fix/TauProd/SRF_Data/netCDF/Interpolated/${SENSOR}.srf.nc
fi

CRTM_DIR=../crtm

SCRIPT_DIR=$PWD

NUM_CPU=24

NUM_PROFILES=48

LBL_OUTPUT_FILE=lbl_radSpectrum
LBL_SIGNAL_FILE=${LBL_OUTPUT_FILE}.nc.signal

CONVOL_OUTPUT_FILE=${SENSOR}_radSpectrum
CONVOL_SIGNAL_FILE=${SENSOR}_Convol_Completion.signal

SCRIPT_SIGNAL_FILE=${SENSOR}_script_complete.signal

#----------------------------------------------------------------------------------
#   *** Step 1 ***
# Run jobs to Compute nlte and lte channel radiance spectra and SRF convolusions
#----------------------------------------------------------------------------------

if [ $RUN_SPEC == 1 ];then

  # --- Create scripts
  for lbl_case in ${CASE_LIST}; do

    job_check=1

    # this conditional loop is arranged to re-run those jobs not completed.
    while [ $job_check -eq 1 ]; do 

      job_check=0

      for SOLOR_ANGLE in ${SOLOR_ANGLE_LIST};do                                                                 

        PROFILE=1                                                                                               
        while [ $PROFILE -le $NUM_PROFILES ]; do                                                                
                                                                                                                
          for SENSOR_ANGLE in ${SENSOR_ANGLE_LIST};do                                                           

            # link various files
            WORK_DIR_HEAD=${WORK_ROOT}/prof${PROFILE}/amass${SENSOR_ANGLE}                                      
            if [ $lbl_case == 1 ]; then  # nlte case                                                            
              WORK_DIR=${WORK_DIR_HEAD}/results_nlte/sunang${SOLOR_ANGLE}                                       
              mkdir -p $WORK_DIR                                                                                
              cd $WORK_DIR                                                                                      
              TAG=nlte
              jobname=${SENSOR}_${PROFILE}.${SOLOR_ANGLE}.${SENSOR_ANGLE}.${TAG}                                
              ln -sf ${TAPE5_DIR}/TAPE5_prf${PROFILE}_nlte.${SENSOR_ANGLE} ./TAPE5       
              ln -sf ${TAPE4_DIR}/TAPE4_vt${PROFILE}_s${SOLOR_ANGLE}.prf ./TAPE4            

            else      # lte case, independent from sun angle                                                    
              WORK_DIR=${WORK_DIR_HEAD}/results_lte                                                             
              mkdir -p $WORK_DIR                                                                                
              cd $WORK_DIR                                                                                      
              TAG=lte                                                                                           
              jobname=${SENSOR}_${PROFILE}.${SENSOR_ANGLE}.${TAG}                                               
              ln -sf ${TAPE5_DIR}/TAPE5_prf${PROFILE}_lte.${SENSOR_ANGLE} ./TAPE5 
            fi                                                                                                  

            ln -sf ${TAPE3} ./TAPE3                                                   
            ln -sf ${LBLRTM_INPUTFILE_DIR}/EMISS_sse_WuSm_vza_0_ws_0 ./EMISSIVITY        
#            ln -sf ${LBLRTM_INPUTFILE_DIR}/FSCDXS .                                      
            ln -sf ${LBLRTM_INPUTFILE_DIR}/REFL_ssr_WuSm_vza_0_ws_0 ./REFLECTIVITY       
            ln -sf ${LBLRTM_INPUTFILE_DIR}/xs .                                          

            ln -sf ${LBLRTM_EXE} ./lblrtm                                             
            ln -sf ${LBL2NCDF_EXE} ./lbl2ncdf                                         
            ln -sf ${CONVOL_SRF_EXE} ./srf_convol                                     
             
            # create scripts                                                                                                    
            jobScript=${jobname}.sh                                                                             
                                                                                                                
            SCRIPT_COMPLETE_FILE=$SCRIPT_SIGNAL_FILE                                                            

            rm -f ${jobname}.err ${jobname}.out starting.signal                                                 

            echo "#!/bin/sh" > ${jobScript}                                                                      
            echo "#@ job_name = ${jobname}_run" >> ${jobScript}                                                  
            echo "#@ output = ${jobname}.out" >> ${jobScript}                                                    
            echo "#@ error  = ${jobname}.err" >> ${jobScript}                                                    
            echo "#@ notification = error" >> ${jobScript}                                                       
            echo "#@ job_type = serial" >> ${jobScript}                                                          
            echo "#@ wall_clock_limit = 00:15:00" >> ${jobScript}                                                
            echo "#@ class = jcsda" >> ${jobScript}                                                              
            echo "#@ group = jcsda" >> ${jobScript}                                                              
            echo "#@ account_no = JCSDA001-RES" >> ${jobScript}                                                  
            echo "#@ resources = ConsumableCpus(1) ConsumableMemory(5000 MB)" >> ${jobScript}                    
            echo "#@ queue" >> ${jobScript}                                                                      
            echo "# " >> ${jobScript}                                                                            

                                                                                                                 
            echo "rm -f TAPE6 TAPE7 TAPE9 TAPE10 TAPE11 TAPE12 TAPE13" >> ${jobScript}                                            
            echo "# " >> ${jobScript}                                                                            


            echo "if [ ! -f lbl_radSpectrum.nc.signal ];then" >> ${jobScript}                                    
            echo "# " >> ${jobScript}                                                                            
            echo "# run lblrtm" >> ${jobScript}                                                                  
            echo "lblrtm" >> ${jobScript}                                                                    
            echo "# convert LBLRTM file to NCDF file" >> ${jobScript}                                            
            echo "mv TAPE13 ${LBL_OUTPUT_FILE}" >> ${jobScript}                                                  
            echo "lbl2ncdf<<EOF" >> ${jobScript}                                                           
            echo "${LBL_OUTPUT_FILE}" >> ${jobScript}                                                            
            echo "0" >> ${jobScript}                                                                             
            echo "'${SENSOR} LBLRTM radiance spectrum'" >> ${jobScript}                                          
            echo "'Computed at airmass angle ${SENSOR_ANGLE} at satellite hight'" >> ${jobScript}                
            echo "'UMBC profile ${PROFILE}'" >> ${jobScript}                                                     
            echo "EOF" >> ${jobScript}                                                                           
            echo "fi" >> ${jobScript}                                                                            
            echo "# " >> ${jobScript}                                                                            
            echo "# Compute channel radiance" >> ${jobScript}                                                    
            echo "if [ -f ${LBL_SIGNAL_FILE} ]; then" >> ${jobScript}                                            
          if [ $SENSOR_INDEX -lt 10 ];then  # interferometers
            echo "srf_convol<<EOF" >> ${jobScript}                                                         
            echo "${LBL_OUTPUT_FILE}.nc" >> ${jobScript}                                                         
            echo "${CONVOL_OUTPUT_FILE}" >> ${jobScript}                                                         
            echo "${SENSOR_INDEX}" >> ${jobScript}                                                               
            echo "${BAND}" >> ${jobScript}                                                                       
            echo "EOF" >> ${jobScript}
          else
            echo "srf_convol<<EOF" >> ${jobScript}                                                         
            echo "${LBL_OUTPUT_FILE}.nc" >> ${jobScript} 
            echo "${SRF_FILE}" >> ${jobScript}                                                       
            echo "${CONVOL_OUTPUT_FILE}" >> ${jobScript}                                                         
            echo "${SENSOR}" >> ${jobScript}                                                         
            echo "EOF" >> ${jobScript}
          fi                                                                                     
            echo "  if [ -f Convol_Completion.signal ];then" >> ${jobScript}                                     
            echo "     mv Convol_Completion.signal $CONVOL_SIGNAL_FILE " >> ${jobScript}                         
            echo "  fi " >> ${jobScript}                                                                         
            echo "fi" >> ${jobScript}                                                                            
            echo "# delete TAPE10 TAPE11 TAPE12" >> ${jobScript}                                                 
            echo "rm -f TAPE10 TAPE11 TAPE12 SCNINTF" >> ${jobScript}                                                   
            echo "echo 'script completed' > ${SCRIPT_COMPLETE_FILE}" >> ${jobScript}                             

            # -- execute the job script                                                                         

            # the job is skipped if the case is already completed.           
            if [ ! -f ${CONVOL_SIGNAL_FILE} ]; then                          

              job_check=1                                                    
              SCRIPT_COMPLETE_FILE=$SCRIPT_SIGNAL_FILE                       

              rm -f ${jobname}.err ${jobname}.out           


              # -- execute the job script                                    
              chmod 700 $jobScript                                                                                
              llsubmit $jobScript                                            
              sleep 1                                                      
            fi                                                               

            # if the assigned CPUs are used up, wait for a job to complet    
            job_running=`llq -u wx23yh | grep -i wx23yh | wc -l`             
            while [ $job_running -ge $NUM_CPU ];do                           
              sleep 1                                                        
              job_running=`llq -u wx23yh | grep -i wx23yh | wc -l`           
            done                                                             


            cd $SCRIPT_DIR                                                                                       


          done  # sensor angle                                                                                    


          PROFILE=`expr $PROFILE + 1`                                                                             
                                                                                                                
        done # profile                                                                                            
      done # soloar angle                                                                                         

      # wait until all jobs run through     
      job_running=`llq -u wx23yh | grep -i wx23yh | wc -l`             
      while [ $job_running -gt 0 ];do                           
        sleep 1                                                        
        job_running=`llq -u wx23yh | grep -i wx23yh | wc -l`           
      done                                                             
     
    done # job check while
     
  done # case loop

  echo "----- Step one completed"
  
fi


#----------------------------------------------------------------------------------
#   *** Step 2 ***
# Merge nlte and lte channel radiance spectrum and profile files into a netcdf file
#----------------------------------------------------------------------------------
if [ $RUN_MERGE == 1 ]; then  # run merging files

    for SOLOR_ANGLE in ${SOLOR_ANGLE_LIST};do

      PROFILE=1
      while [ $PROFILE -le $NUM_PROFILES ]; do
      
        for SENSOR_ANGLE in ${SENSOR_ANGLE_LIST};do

          WORK_DIR_HEAD=${WORK_ROOT}/prof${PROFILE}/amass${SENSOR_ANGLE}
          WORK_DIR_NLTE=${WORK_DIR_HEAD}/results_nlte/sunang${SOLOR_ANGLE}
          WORK_DIR_LTE=${WORK_DIR_HEAD}/results_lte
          WORK_DIR_MERGED=${WORK_DIR_HEAD}/results_merged/sunang${SOLOR_ANGLE}
          
          mkdir -p $WORK_DIR_MERGED
          cd $WORK_DIR_MERGED           

#          if [ ! -f ${WORK_DIR_MERGED}/${CONVOL_OUTPUT_FILE}.nc ];then            
${MERGE_FILE_EXE}<<EOF
${WORK_DIR_NLTE}/TAPE5
${WORK_DIR_NLTE}/${CONVOL_OUTPUT_FILE}
${WORK_DIR_LTE}/${CONVOL_OUTPUT_FILE}
${SENSOR_ANGLE}
${SOLOR_ANGLE}.0
${WORK_DIR_MERGED}/${CONVOL_OUTPUT_FILE}.nc
EOF
#          fi
          cd $SCRIPT_DIR

        done  # sensor angle


        PROFILE=`expr $PROFILE + 1`  
      
      done # profile
    done # solar angle

  echo "----- Step two completed"

fi

#----------------------------------------------------------------------------------
#   *** Step 3 ***
# Concatenate files along file, sensor angle and sun angle dimentions
#----------------------------------------------------------------------------------
if [ $RUN_CONCAT == 1 ]; then  # run merging files
  # along sun angle direction                                                     
  PROFILE=1                                                                       
  while [ $PROFILE -le $NUM_PROFILES ]; do                                        

    for SENSOR_ANGLE in ${SENSOR_ANGLE_LIST};do                                   

      f1=" "                                                                        
      for SOLOR_ANGLE in ${SOLOR_ANGLE_LIST};do                                   

        WORK_DIR_HEAD=${WORK_ROOT}/prof${PROFILE}/amass${SENSOR_ANGLE}  
        SOURCE_DIR=${WORK_DIR_HEAD}/results_merged/sunang${SOLOR_ANGLE}           
        WORK_DIR_RESULT=${WORK_DIR_HEAD}/results_merged                           
                                                                                  
        cd $WORK_DIR_RESULT                                                       
                                                                                  
        if [ "${f1}" == " " ]; then                                               
          f1=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                                    
        else                                                                      
          f2=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                                    
${CONCAT_FILE_EXE}<<EOF
${f1}
${f2}
${CONVOL_OUTPUT_FILE}.tmp.nc
3
EOF
          mv -f ${CONVOL_OUTPUT_FILE}.tmp.nc ${CONVOL_OUTPUT_FILE}.nc  
                                                           
          f1=${WORK_DIR_RESULT}/${CONVOL_OUTPUT_FILE}.nc                             

        fi                                                                        

                                                           
        cd $SCRIPT_DIR                                     

      done # solar angle
        
    done  # sensor angle

    PROFILE=`expr $PROFILE + 1`  
      
  done # profile

  # along sensor angle direction                                                              
  PROFILE=1                                                                                   
  while [ $PROFILE -le $NUM_PROFILES ]; do                                                    

    f1=" "                                                                                    
    for SENSOR_ANGLE in ${SENSOR_ANGLE_LIST};do                                               

      SOURCE_DIR=${WORK_ROOT}/prof${PROFILE}/amass${SENSOR_ANGLE}/results_merged    
      WORK_DIR_RESULT=${WORK_ROOT}/prof${PROFILE}                                   
                                                                                              
      cd $WORK_DIR_RESULT                                                                     
                                                                                              
      if [ "${f1}" == " " ]; then                                                             
        f1=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                                                  
      else                                                                                    
        f2=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                                                  
${CONCAT_FILE_EXE}<<EOF
${f1}
${f2}
${CONVOL_OUTPUT_FILE}.tmp.nc
2
EOF
        mv -f ${CONVOL_OUTPUT_FILE}.tmp.nc ${CONVOL_OUTPUT_FILE}.nc    
                                                           
        f1=${WORK_DIR_RESULT}/${CONVOL_OUTPUT_FILE}.nc                             
      fi                                                                                      
                                                           
      cd $SCRIPT_DIR                                       

    done  # sensor angle

    PROFILE=`expr $PROFILE + 1`  
      
  done # profile

  # along profile direction                               
  PROFILE=1                                               
  f1=" "                                                
  while [ $PROFILE -le $NUM_PROFILES ]; do                

    SOURCE_DIR=${WORK_ROOT}/prof${PROFILE}      
    WORK_DIR_RESULT=${WORK_ROOT}                
                                                          
    cd $WORK_DIR_RESULT                                   
                                                          
    if [ "${f1}" == " " ]; then                           
      f1=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                
    else                                                  
      f2=${SOURCE_DIR}/${CONVOL_OUTPUT_FILE}.nc                
${CONCAT_FILE_EXE}<<EOF
${f1}
${f2}
${CONVOL_OUTPUT_FILE}.tmp.nc
1
EOF
      mv -f ${CONVOL_OUTPUT_FILE}.tmp.nc ${CONVOL_OUTPUT_FILE}.nc      
                                                           
      f1=${WORK_DIR_RESULT}/${CONVOL_OUTPUT_FILE}.nc                             
    fi                                                    
                                                           
    cd $SCRIPT_DIR                                         

    PROFILE=`expr $PROFILE + 1`  
      
  done # profile

  echo "----- Step three completed"
fi

#----------------------------------------------------------------------------------
# run CRTM for TAPE5 profile
#----------------------------------------------------------------------------------

if [ $RUN_CRTM == 1 ]; then  # run CRTM

    CRTM_RESULT=${WORK_ROOT}/${SENSOR}_crtm_odas.txt
    rm -f $CRTM_RESULT
     
    cd $CRTM_DIR
    
    PROFILE=1                                                           
    while [ $PROFILE -le $NUM_PROFILES ]; do                            

      for SENSOR_ANGLE in ${SENSOR_ANGLE_LIST};do                       

        WORK_DIR_HEAD=${WORK_ROOT}/prof${PROFILE}/amass${SENSOR_ANGLE}  
        WORK_DIR_LTE=${WORK_DIR_HEAD}/results_lte                       

forward_model<<EOF
${SENSOR}
${WORK_DIR_LTE}/TAPE5
${SENSOR_ANGLE}
100.0       # solar zenith angle: not be used here
${WORK_DIR_LTE}/${SENSOR}_crtm.txt
1
EOF

      echo "profile${PROFILE} amass${SENSOR_ANGLE}" >> ${CRTM_RESULT}
      cat ${WORK_DIR_LTE}/${SENSOR}_crtm.txt >> ${CRTM_RESULT}
      
      done  # sensor angle          

      PROFILE=`expr $PROFILE + 1`    
                                    
    done # profile                  

    cd $SCRIPT_DIR                                         

fi


exit


