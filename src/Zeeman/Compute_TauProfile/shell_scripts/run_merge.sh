script_dir=$PWD

WORK_DIR=$1

cd $WORK_DIR

CHAN_LIST='19 20 21 22'

for chan in ${CHAN_LIST};do
echo $chan
${script_dir}/../src_merge/merge<<EOF
${script_dir}/file_list.${chan}.txt
zssmis_tau_UMBC_101LVL_48.${chan}.txt
EOF
done

exit
