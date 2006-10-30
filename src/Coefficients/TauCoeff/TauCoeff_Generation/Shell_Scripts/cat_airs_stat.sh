DIR_STAT=/disk5/pub/yhan/TransCoeff_work_test3/statistics/stat
tag_list="M12 M11 M10 M9 M8 M7 M6 M5 M4d M4c M3 M4b M4a M2b M1b M2a M1a"
prof_set=umbc48101

for gas in wet dry ozo;do

  outFile=$DIR_STAT/stat.airs_aqua.${gas}.varyingOrder.${prof_set}.txt
  rm -f $outFile
  
  for tag in $tag_list;do
  
    file=${DIR_STAT}/stat.airs${tag}_aqua.${gas}.varyingOrder.${prof_set}.txt
    
    cat $file >> $outFile

  done
done  
exit
