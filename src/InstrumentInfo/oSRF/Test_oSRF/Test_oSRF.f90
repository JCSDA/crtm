! Test_SRF
!
! Program to test the oSRF modules
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_oSRF

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Unit_Test
  USE oSRF_Define, ONLY: oSRF_type, &
                         Allocated_oSRF, &
                         Create_oSRF, &
                         Destroy_oSRF, &
                         Assign_oSRF, &
                         Equal_oSRF, &
                         Set_Property_oSRF, &
                         Get_Property_oSRF, &
                         Inspect_oSRF, &
                         Info_oSRF, &
                         N_SENSOR_TYPES, & 
                         MICROWAVE_SENSOR, &
                         INFRARED_SENSOR, &
                         VISIBLE_SENSOR, &
                         ULTRAVIOLET_SENSOR, &
                         SENSOR_TYPE_NAME
  USE oSRF_File_Define, ONLY: oSRF_File_type, &
                              Allocated_oSRF_File, &
                              Create_oSRF_File, &
                              Destroy_oSRF_File, &
                              Set_Property_oSRF_File, &
                              Get_Property_oSRF_File, &
                              Inspect_oSRF_File, &
                              AddTo_oSRF_File, &
                              GetFrom_oSRF_File, &
                              Read_oSRF_File
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_oSRF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Single band number of points
  INTEGER, PARAMETER :: N1_POINTS(1) = (/ 20 /)
  ! Multi-band number of points
  INTEGER, PARAMETER :: N2_POINTS(4) = (/10, 12, 15, 11/)
  ! Number of channels
  INTEGER, PARAMETER :: N_CHANNELS = 3
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_status
  INTEGER :: i, n
  CHARACTER(256) :: Sensor_Id, Info
  INTEGER :: Sensor_Type
  INTEGER :: n_Points
  REAL(fp), ALLOCATABLE :: frequency(:)
  TYPE(oSRF_type) :: osrf1, osrf2, osrf2_copy
  TYPE(oSRF_File_type) :: osrf_file

  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the oSRF modules.', &
                        '$Revision$' )


  ! oSRF_Define MODULE TESTS
  ! ========================
  WRITE( *,'(//2x,"oSRF_Define MODULE TESTS")' )
  WRITE( *,'(  2x,"========================",/)' )
  
  ! Test initial status
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test inspect and Info routines
!  WRITE( *,'(/5x,"Inspecting unallocated oSRF...")' )
!  CALL Inspect_oSRF( osrf2 )
  CALL Info_oSRF( osrf2, Info )
  PRINT *, TRIM(Info)
  


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  WRITE( *,'( 7x,"Single band creation...")' )
  err_status = Create_oSRF(osrf1, N1_POINTS)
  CALL Display_Message('Create_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 7x,"Multiple band creation...")' )
  err_status = Create_oSRF(osrf2, N2_POINTS)
  CALL Display_Message('Create_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test set routines
  WRITE( *,'(/5x,"Testing set routines...")' )
  err_status = Set_Property_oSRF( osrf2, &
    sensor_id   = 'sensor_platform', &
    sensor_type = MICROWAVE_SENSOR )
  CALL Display_Message('Set_Property_oSRF','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  DO n = 1, osrf2%n_Bands
    err_status = Set_Property_oSRF( osrf2, &
      Band = n, &
      Frequency = (/(real(i,fp),i=1,N2_POINTS(n))/) )
    CALL Display_Message('Set_Property_oSRF','band',err_status)
    IF ( err_status /= SUCCESS ) STOP
  END DO
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))


  ! Test inspect routines
!  WRITE( *,'(/5x,"Inspecting original oSRF...")' )
!  CALL Inspect_oSRF( osrf2 )
  
  
  ! Test get routines
  WRITE( *,'(/5x,"Testing get routines...")' )
  err_status = Get_Property_oSRF( osrf2, &
    sensor_id   = sensor_id, &
    sensor_type = sensor_type )
  CALL Display_Message('Get_Property_oSRF','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  PRINT *, 'Sensor Id  : ', TRIM(sensor_id)
  PRINT *, 'Sensor Type: ', SENSOR_TYPE_NAME(sensor_type)
  DO n = 1, osrf2%n_Bands
    err_status = Get_Property_oSRF( osrf2, &
      Band = n, &
      n_Points = n_Points )
    CALL Display_Message('Get_Property_oSRF','band, n_Points',err_status)
    IF ( err_status /= SUCCESS ) STOP
    ALLOCATE(Frequency(n_Points))
    err_status = Get_Property_oSRF( osrf2, &
      Band = n, &
      Frequency = Frequency )
    CALL Display_Message('Get_Property_oSRF','band, Frequency',err_status)
    IF ( err_status /= SUCCESS ) STOP
    PRINT *, 'n_Points  : ', n_Points
    PRINT *, 'Frequency : ', Frequency
    DEALLOCATE(Frequency)
  END DO
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))


  ! Test assignment
  WRITE( *,'(/5x,"Testing assignment routines...")' )
  DO n = 1, 5
    err_status = Assign_oSRF(osrf2, osrf2_copy)
    CALL Display_Message('Assign_oSRF','',err_status)
  END DO
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))


  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting copied oSRF...")' )
!  CALL Inspect_oSRF( osrf2_copy )
  CALL Info_oSRF( osrf2_copy, Info )
  PRINT *, TRIM(Info)
  
  
  ! Test equality
  WRITE( *,'(/5x,"Testing equality routines...")' )
  err_status = Equal_oSRF(osrf2, osrf2_copy)
  CALL Display_Message('Equal_oSRF','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF structures are equal")' )
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))
  

  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_oSRF(osrf1)
  CALL Display_Message('Destroy_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_oSRF(osrf2)
  CALL Display_Message('Destroy_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Destroy_oSRF(osrf2_copy)
  CALL Display_Message('Destroy_oSRF','Multiple band copy',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",3(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2,osrf2_copy/))



  ! oSRF_File_Define MODULE TESTS
  ! =============================
  WRITE( *,'(//2x,"oSRF_File_Define MODULE TESTS")' )
  WRITE( *,'(  2x,"=============================",/)' )
  
  ! Test initial status
  WRITE( *,'( 5x,"oSRF_File allocation status: ",2(1x,l5))' ) Allocated_oSRF_File(osrf_File)


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  err_status = Create_oSRF_File(osrf_file, N_CHANNELS)
  CALL Display_Message('Create_oSRF_File','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF_File allocation status: ",2(1x,l5))' ) Allocated_oSRF_File(osrf_File)


  ! Test set routines
  WRITE( *,'(/5x,"Testing set routines...")' )
  err_status = Set_Property_oSRF_File( osrf_File, &
    sensor_id   = 'sensor_platform', &
    sensor_type = MICROWAVE_SENSOR )
  CALL Display_Message('Set_Property_oSRF_File','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF_File allocation status:",2(1x,l5))' ) Allocated_oSRF_File(osrf_file)


  ! Test adding to container
  WRITE( *,'(/5x,"Testing AddTo routines...")' )
  err_status = Create_oSRF(osrf1, N1_POINTS)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Create_oSRF(osrf2, N2_POINTS)
  IF ( err_status /= SUCCESS ) STOP

  err_status = AddTo_oSRF_File(osrf_file, osrf1)  
  CALL Display_Message('AddTo_oSRF_File','1',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = AddTo_oSRF_File(osrf_file, osrf2, pos=2)  
  CALL Display_Message('AddTo_oSRF_File','2',err_status)
  IF ( err_status /= SUCCESS ) STOP

  err_status = AddTo_oSRF_File(osrf_file, osrf1, pos=N_CHANNELS+5)
  CALL Display_Message('AddTo_oSRF_File','This should always fail',err_status)
  IF ( err_status /= FAILURE ) STOP


  ! Test getting from container
  WRITE( *,'(/5x,"Testing GetFrom routines...")' )
  err_status = GetFrom_oSRF_File(osrf_file, osrf2_copy, pos=2)  
  CALL Display_Message('GetFrom_oSRF_File','2',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Equal_oSRF(osrf2, osrf2_copy)
  CALL Display_Message('Equal_oSRF','2',err_status)
  IF ( err_status /= SUCCESS ) STOP

  err_status = GetFrom_oSRF_File(osrf_file, osrf2)  
  CALL Display_Message('GetFrom_oSRF_File','1',err_status)
  IF ( err_status /= SUCCESS ) STOP
  err_status = Equal_oSRF(osrf1, osrf2)
  CALL Display_Message('Equal_oSRF','1',err_status)
  IF ( err_status /= SUCCESS ) STOP
  
  err_status = GetFrom_oSRF_File(osrf_file, osrf1, pos=N_CHANNELS+5)
  CALL Display_Message('GetFrom_oSRF_File','This should always fail',err_status)
  IF ( err_status /= FAILURE ) STOP


  ! Test destruction
  WRITE( *,'(/5x,"Testing destruction routines...")' )
  err_status = Destroy_oSRF_File(osrf_File)
  CALL Display_Message('Destroy_oSRF_File','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF_File allocation status: ",2(1x,l5))' ) Allocated_oSRF_File(osrf_File)


  ! Test read from file
  WRITE( *,'(/5x,"Testing read routines...")' )
  err_status = Read_oSRF_File(osrf_File, 'test.oSRF.nc')
  CALL Display_Message('Read_oSRF_File','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF_File allocation status: ",2(1x,l5))' ) Allocated_oSRF_File(osrf_File)

  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting oSRF_File...")' )
  CALL Inspect_oSRF_File( osrf_file, verbose = .true. )
  
  


  WRITE(*,*)
  CALL Display_Message(PROGRAM_NAME,'All tests passed',SUCCESS)
  
END PROGRAM Test_oSRF
