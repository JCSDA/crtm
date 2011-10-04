sensor=$1
WORK_DIR=$2
POLARIZATION=$4

prof_set=UMBC_101LVL_48
POLARIZATION=2   # 1 - LCP; 2 - RCP

DELTA_F_LIST='-80.0 -60.0 -40.0 -20.0 0.0 20.0 40.0 60.0 80.0'
ANGLE_LIST='51.0 52.5 54.0'
CHAN_LIST='19 20 21 22'
mkdir -p $WORK_DIR

MAX_CPUs=96

filename_in=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/Zeeman/training/ssmis/fix_zeeman/${prof_set}ATM_extended.AtmProfile.nc
#EXE_FILE=../src_ssmis/compute_channel_trans
#EXE_FILE=/u/wx23yh/CRTM_ODPStraining/Zeeman/Compute_TauProfile/ssmis/src_ssmis/compute_channel_trans
EXE_FILE=/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/Zeeman/Compute_TauProfile/src_ssmis/compute_channel_trans
n_freqs=`echo $DELTA_F_LIST | wc -w`
n_chan=`echo $CHAN_LIST | wc -w`
n_ang=`echo $ANGLE_LIST | wc -w`


Job_count=0
rm -f Job_*.sh


for ichan in ${CHAN_LIST}; do

  FILE_LIST=file_list.${ichan}.txt
  n_files=`echo $n_freqs $n_ang | awk '{print $1 * $2}'`
  echo $ichan > $FILE_LIST
  echo "$n_files $n_ang $n_freqs" >> $FILE_LIST
  echo $ANGLE_LIST >> $FILE_LIST
  echo $DELTA_F_LIST >> $FILE_LIST
  
  for delta_f in ${DELTA_F_LIST}; do

    for ang in $ANGLE_LIST;do


      Job_count=`expr $Job_count + 1`                                       
      runningJobs=`ls Job_*.sh 2>/dev/null | wc -l`                    

      total_seconds=0                                                       

      while [ $runningJobs -ge $MAX_CPUs ]; do                              

        sleep 1                                                             
                                                                           
       total_seconds=`expr $total_seconds + 1`                             
       #--- terminate process if the job has waited for                     
       #--- a day (20864 seconds) for execution.                            
        if [ $total_seconds -gt 20864 ]; then                               
                                                                           
           echo "No job has been sent out since a day ago" >> CoeffJob.log  
           echo "current waiting job: ${Job_count}"                         
           echo "this script is terminated!" >> CoeffJob.log                
           exit 1                                                           
                                                                           
        fi                                                                  
                                                                           
       runningJobs=`ls Job_*.sh 2>/dev/null | wc -l`                  
                                                                           
      done                                                                  


      ProcSuffix=${ichan}.${ang}.${delta_f}
      filename_out=zssmis_tau_${prof_set}.${ProcSuffix}.txt
      echo $filename_out
      echo $filename_out >> $FILE_LIST 
      
      COMPLETION_SIGNAL=${filename_out}.CompleteSignal
      rm -f $COMPLETION_SIGNAL


        # Create scripts
        jobScript=Job_${ProcSuffix}.sh   
        STDOUT=stdout.${ProcSuffix}.txt                           

        echo "#!/bin/sh" > ${jobScript}                                                   
        echo "#@ job_name = Tauprof_run" >> ${jobScript}                                 
   #    echo "#@ output = \$(job_name).\$(jobid).out" >> ${jobScript}                    
        echo "#@ error  = \$(job_name).\$(jobid).err" >> ${jobScript}                     
        echo "#@ notification = error" >> ${jobScript}                                    
        echo "#@ job_type = serial" >> ${jobScript}                                       
        echo "#@ wall_clock_limit = 03:00:00" >> ${jobScript}                             
        echo "#@ class = jcsda" >> ${jobScript}                                          
        echo "#@ group = jcsda" >> ${jobScript}                                           
        echo "#@ account_no = JCSDA010-RES" >> ${jobScript}                               
        echo "#@ resources = ConsumableCpus(1) ConsumableMemory(500 MB)" >> ${jobScript}  
        echo "#@ queue" >> ${jobScript}                                                   
        echo "# " >> ${jobScript}      
  #                                                   
        echo "# go to the execution directory" >> ${jobScript}                            
        echo "# execute ${EXE_FILE}" >> ${jobScript}                                      
        echo "${EXE_FILE}<<EOF > ${STDOUT}" >> ${jobScript} 
        echo "${sensor}" >> ${jobScript}       
        echo "${ichan}" >> ${jobScript}                                                
        echo "${ang}" >> ${jobScript}                                                
        echo "${delta_f}" >> ${jobScript}  
        echo "${POLARIZATION}" >> ${jobScript}                                             
        echo "${filename_in}" >> ${jobScript}                                                
        echo "${WORK_DIR}/${filename_out}" >> ${jobScript}                                                
        echo "EOF" >> ${jobScript}                                                        
        echo "# kill the stdout file if the excution is successful" >> ${jobScript}       
        echo "if [ -f ${WORK_DIR}/${COMPLETION_SIGNAL} ];then" >> ${jobScript}                       
        echo "  rm -f ${STDOUT}" >> ${jobScript}                                       
        echo "fi" >> ${jobScript}                                                         
        echo "# kill the jobscript" >> ${jobScript}                                       
        echo "rm -f ${jobScript}" >> ${jobScript}                                         

        # -- execute the job script                                                       

        chmod 700 $jobScript  
        llsubmit $jobScript 

  done
  done
done

exit
