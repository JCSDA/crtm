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
                         Create_oSRF   , &
                         Destroy_oSRF  , &
                         Assign_oSRF   , &
                         Equal_oSRF    , &
                         Set_oSRF      , &
                         Get_oSRF      , &
                         Inspect_oSRF  , &
                         N_SENSOR_TYPES    , & 
                         MICROWAVE_SENSOR  , &
                         INFRARED_SENSOR   , &
                         VISIBLE_SENSOR    , &
                         ULTRAVIOLET_SENSOR, &
                         SENSOR_TYPE_NAME
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
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_status
  INTEGER :: i, n
  TYPE(oSRF_type) :: osrf1, osrf2, osrf2_copy
  CHARACTER(256) :: Sensor_Id
  INTEGER :: Sensor_Type
  INTEGER :: n_Points
  REAL(fp), ALLOCATABLE :: frequency(:)

  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the oSRF modules.', &
                        '$Revision$' )

    
  ! Test initial status
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting unallocated oSRF...")' )
  CALL Inspect_oSRF( osrf2 )


  ! Test creation
  WRITE( *,'(/5x,"Testing creation routines...")' )
  WRITE( *,'( 7x,"Single band creation...")' )
  err_status = Create_oSRF(N1_POINTS, osrf1)
  CALL Display_Message('Create_oSRF','Single band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 7x,"Multiple band creation...")' )
  err_status = Create_oSRF(N2_POINTS, osrf2)
  CALL Display_Message('Create_oSRF','Multiple band',err_status)
  IF ( err_status /= SUCCESS ) STOP
  WRITE( *,'( 5x,"oSRF allocation status: ",2(1x,l5))' ) Allocated_oSRF((/osrf1,osrf2/))


  ! Test set routines
  WRITE( *,'(/5x,"Testing set routines...")' )
  err_status = Set_oSRF( osrf2, &
    sensor_id   = 'sensor_platform', &
    sensor_type = MICROWAVE_SENSOR )
  CALL Display_Message('Set_oSRF','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  DO n = 1, osrf2%n_Bands
    err_status = Set_oSRF( osrf2, &
      Band = n, &
      Frequency = (/(real(i,fp),i=1,N2_POINTS(n))/) )
    CALL Display_Message('Set_oSRF','band',err_status)
    IF ( err_status /= SUCCESS ) STOP
  END DO
  WRITE( *,'( 5x,"oSRF allocation status:",2(1x,l5))' ) Allocated_oSRF((/osrf2,osrf2_copy/))


  ! Test inspect routines
  WRITE( *,'(/5x,"Inspecting original oSRF...")' )
  CALL Inspect_oSRF( osrf2 )
  
  
  ! Test get routines
  WRITE( *,'(/5x,"Testing get routines...")' )
  err_status = Get_oSRF( osrf2, &
    sensor_id   = sensor_id, &
    sensor_type = sensor_type )
  CALL Display_Message('Get_oSRF','',err_status)
  IF ( err_status /= SUCCESS ) STOP
  PRINT *, 'Sensor Id  : ', TRIM(sensor_id)
  PRINT *, 'Sensor Type: ', SENSOR_TYPE_NAME(sensor_type)
  DO n = 1, osrf2%n_Bands
    err_status = Get_oSRF( osrf2, &
      Band = n, &
      n_Points = n_Points )
    CALL Display_Message('Get_oSRF','band, n_Points',err_status)
    IF ( err_status /= SUCCESS ) STOP
    ALLOCATE(Frequency(n_Points))
    err_status = Get_oSRF( osrf2, &
      Band = n, &
      Frequency = Frequency )
    CALL Display_Message('Get_oSRF','band, Frequency',err_status)
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
  CALL Inspect_oSRF( osrf2_copy )
  
  
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

END PROGRAM Test_oSRF
