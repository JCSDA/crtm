find ./ -name "*.f90" -exec sed -i "s/  WRITE( *,'\(/5x,\"Enter sensor id: \"\)',ADVANCE='NO' \)//'" {} \;
echo  READ( *,'(a)' ) Sensor_Id
