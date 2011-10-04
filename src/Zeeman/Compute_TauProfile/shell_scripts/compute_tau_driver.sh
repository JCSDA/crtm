sensor=ssmis_f16
zsensor=z${sensor}
#WORK_DIR=${HOME}/noscrub_jcsda/work_zeeman2/${zsensor}
WORK_DIR=/jcsda/noscrub/wx23yc/CRTM_ODPStraining/work_zeeman2/${zsensor}

# Step 1
#run_compute_channel_trans_umbc.sh $sensor $WORK_DIR

# Step 2
run_merge.sh $WORK_DIR

exit
