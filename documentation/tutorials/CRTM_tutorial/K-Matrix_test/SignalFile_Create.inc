  SUBROUTINE SignalFile_Create()
    CHARACTER(256) :: Filename
    INTEGER :: fid
    Filename = RESULTS_PATH//TRIM(Sensor_Id)//'.signal'
    fid = Get_Lun()
    OPEN( fid, FILE = Filename )
    WRITE( fid,* ) TRIM(Filename)
    CLOSE( fid )
  END SUBROUTINE SignalFile_Create
