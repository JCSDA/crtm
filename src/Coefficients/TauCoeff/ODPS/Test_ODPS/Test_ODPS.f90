!
! Test_ODPS
!
! Program to test the ODPS definition and I/O routines.
!
!
! FILES ACCESSED:
!       - netCDF format ODPS data file, both input and output
!       - Binary format ODPS data file, both input and output
!
! SIDE EFFECTS:
!       The test output files are overwritten if they already exist.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, 27-Feb-2009
!                       Yong.Chen@noaa.gov
!

PROGRAM Test_ODPS

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp, Single, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, &
                             Warning, Display_Message, Program_Message
  USE ODPS_Define
  USE ODPS_Binary_IO
  USE ODPS_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_ODPS'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER ::  NC_FILENAME = 'Test.ODPS.nc'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.ODPS.bin'
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 5000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Layers,n_Components,n_Coeffs, n_OPIndex, n_OCoeffs
  INTEGER :: i
  INTEGER :: n_Absorbers,  j
  INTEGER :: n_Channels,   l
  INTEGER :: n
  TYPE(ODPS_type) :: ODPS1
  TYPE(ODPS_type) :: ODPS2
  TYPE(ODPS_type) :: ODPS3

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test ODPS definition and I/O routines', &
                        '$Revision$' )

  ! Fill a structure with dummy values
  ! ----------------------------------
  ! Define some typical dimensions
  n_Layers     = 100
  n_Components = 5
  n_Absorbers  = 3
  n_Channels   = 100
!  n_Coeffs     = 2000
  n_Coeffs     = 0
  n_OPIndex    = 6
!  n_OCoeffs    = 200
  n_OCoeffs    = 0

  ! Allocate a ODPS structure
  Error_Status = Allocate_ODPS( n_Layers    , &  ! Input
                                n_Components, &  ! Input
                                n_Absorbers , &  ! Input
                                n_Channels  , &  ! Input
                                n_Coeffs    , &  ! Input 
                                ODPS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating ODPS1 structre.', &
                          Error_Status )
    STOP
  END IF
  IF( n_OCoeffs > 0 ) THEN
    ! Allocate a ODPS structure
    Error_Status =Allocate_ODPS_OPTRAN( n_OCoeffs   , &  ! Input
                                        ODPS1 )          ! Output
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating ODPS1 OPTRAN structre.', &
                            Error_Status )
      STOP
    END IF
    ODPS1%Alpha    = 4.0_Double
    ODPS1%Alpha_C1 = 5.0_Double
    ODPS1%Alpha_C2 = 6.0_Double
    ODPS1%OSignificance = 1
    
    ODPS1%Order = 5
    DO l = 1, n_Channels
      ODPS1%OP_Index(:,l) = (/(j,j=0,n_OPIndex)/)
    END DO
    ODPS1%OC = -99.0_Double

  ENDIF

  ! Fill up the structure with stuff
  ODPS1%Sensor_Id          = 'sensor_platform'
  ODPS1%WMO_Satellite_ID   = 2
  ODPS1%WMO_Sensor_ID      = 3
  ODPS1%Group_Index        = 1
  ODPS1%Sensor_Type        = INFRARED_SENSOR
  ODPS1%Ref_Level_Pressure = ZERO
  ODPS1%Ref_Pressure       = ZERO
  ODPS1%Ref_Absorber       = ZERO
  
  ODPS1%Sensor_Channel     = (/(l,l=1,n_Channels)/)
  ODPS1%Component_ID       = (/(j,j=1,n_Components)/)
  ODPS1%Absorber_ID        = (/(j,j=1,n_Absorbers)/)
 
  DO l = 1, n_Channels
    ODPS1%n_Predictors(:,l) = (/(j,j=1,n_Components)/)
    ODPS1%Pos_Index(:,l) = (/(j,j=1,n_Components)/)
  END DO
!  ODPS1%C = -1.0_Single
    


  ! Copy the structure and test for equality
  ! ----------------------------------------
  Error_Status = Assign_ODPS( ODPS1, ODPS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying ODPS1 structure.', &
                          Error_Status )
    STOP
  END IF
  
  Error_Status = Equal_ODPS( ODPS1, ODPS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'ODPS1 and ODPS2 Structures are not equal.', &
                          Error_Status )
    STOP
  END IF
  
  
  ! Write and read a netCDF ODPS file
  ! -------------------------------------
  WRITE( *,'(/5x,"Testing ODPS netCDF I/O functions ...")' )

  ! Write the netCDF data file
  Error_Status = Write_ODPS_netCDF( NC_FILENAME, &
                                    ODPS1, &
                                    Title         ='This is the title attribute', &
                                    History       ='This is the history attribute', &
                                    Comment       ='This is the comment attribute',&
                                    Profile_Set_ID='This is the id_tag attribute' )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF ODPS file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the netCDF data file
  Error_Status = Read_ODPS_netCDF( NC_FILENAME, ODPS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF ODPS file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_ODPS( ODPS2, ODPS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O structures are equal'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  WRITE( *,'(//5x,"Press <ENTER> to test Binary I/O functions...")' )
  READ( *,* )


  ! Write and read a Binary ODPS file
  ! -------------------------------------
  WRITE( *, '( /5x, "Testing ODPS Binary Write/Read functions ..." )' )

  ! Write the Binary data file
  Error_Status = Write_ODPS_Binary( BIN_FILENAME, &
                                        ODPS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary ODPS file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the Binary data file
  Error_Status = Read_ODPS_Binary( BIN_FILENAME, &
                                       ODPS3 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary ODPS file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_ODPS( ODPS3, ODPS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Binary I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'Binary I/O structures are equal'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  WRITE( *,'(//5x,"Press <ENTER> to test for netCDF reader memory leaks...")' )
  READ( *,* )


  ! Loop for memory leak test
  ! -------------------------
  ! Test the netCDF reader for memory leaks
  WRITE( *,'(/5x,"Looping for netCDF read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_ODPS_netCDF( NC_FILENAME, &
                                     ODPS1, &
                                     Quiet=SET )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #",i0," of ", i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO
  WRITE( *,'(//5x,"Press <ENTER> to test for Binary reader memory leaks...")' )
  READ( *,* )

  ! Test the Binary reader for memory leaks
  WRITE( *,'(/5x,"Looping for Binary read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_ODPS_Binary( BIN_FILENAME, &
                                    ODPS2, &
                                    Quiet=SET )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #",i0," of ", i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO
  WRITE( *,'(//5x,"Press <ENTER> to test for structure assign function memory leaks...")' )
  READ( *,* )

  ! Test the Assign function for memory leaks
  WRITE( *,'(/5x,"Looping for structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_ODPS( ODPS3, ODPS2 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #",i0," of ", i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structures
  ! ----------------------
  Error_Status = Destroy_ODPS( ODPS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS1 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODPS( ODPS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODPS( ODPS3 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODPS3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_ODPS
