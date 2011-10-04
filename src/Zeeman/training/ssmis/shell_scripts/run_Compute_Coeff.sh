SSMIS=ssmis_f16
sensor=z${SSMIS}

#data_dir=/u/wx23yh/noscrub_jcsda/work_zeeman2/z${SSMIS}
data_dir=/jcsda/noscrub/wx23yc/CRTM_ODPStraining/work_zeeman2/z${SSMIS}

for chan in 19 20 21 22;do
/jcsda/save/wx23yc/CRTM_clean_copy/src_0824/src/Zeeman/training/ssmis/src_ssmis/Compute_Coeff<<EOF
${data_dir}/zssmis_tau_UMBC_101LVL_48.${chan}.txt
${data_dir}/AtmProfile.txt
/jcsda/noscrub/wx23yc/CRTM_ODPStraining/Zeeman/SpcCoeff/exec/${sensor}.SpcCoeff.nc
EOF
mv TauCoeff.nc TauCoeff.${chan}.${sensor}.nc
mv fort.40 fitting_err.${chan}.${sensor}.txt
done
