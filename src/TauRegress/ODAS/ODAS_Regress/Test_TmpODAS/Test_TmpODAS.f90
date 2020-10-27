!
! Test_TauCoeff
!
! Program to test the TauCoeff definition and I/O routines.
!
!
! FILES ACCESSED:
!       - netCDF format TauCoeff data file, both input and output
!       - Binary format TauCoeff data file, both input and output
!
! SIDE EFFECTS:
!       The test output files are overwritten if they already exist.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_TauCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE TauCoeff_Define
  USE TauCoeff_Binary_IO
  USE TauCoeff_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_TauCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER ::  NC_FILENAME = 'Test.TauCoeff.nc'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.TauCoeff.bin'
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 5000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Orders
  INTEGER :: n_Predictors, i
  INTEGER :: n_Absorbers,  j
  INTEGER :: n_Channels,   l
  INTEGER :: n
  TYPE(TauCoeff_type) :: TauCoeff1
  TYPE(TauCoeff_type) :: TauCoeff2
  TYPE(TauCoeff_type) :: TauCoeff3

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test TauCoeff definition and I/O routines', &
                        '$Revision$' )

  ! Fill a structure with dummy values
  ! ----------------------------------
  ! Define some typical dimensions
  n_Orders     = 10
  n_Predictors = 5
  n_Absorbers  = 3
  n_Channels   = 100

  ! Allocate a TauCoeff structure
  Error_Status = Allocate_TauCoeff( n_Orders, &
                                    n_Predictors, &
                                    n_Absorbers, &
                                    n_Channels, &
                                    TauCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating TauCoeff1 structre.', &
                          Error_Status )
    STOP
  END IF

  ! Fill up the structure with stuff
  TauCoeff1%n_Sensors = 1
  TauCoeff1%Sensor_Descriptor = 'sensor_platform'
  TauCoeff1%NCEP_Sensor_ID   = 1 
  TauCoeff1%WMO_Satellite_ID = 2
  TauCoeff1%WMO_Sensor_ID    = 3
  TauCoeff1%Sensor_Channel   = (/(l,l=1,n_Channels)/)
  TauCoeff1%Absorber_ID = (/(j,j=1,n_Absorbers)/)    
  TauCoeff1%Alpha    = 4.0_Double
  TauCoeff1%Alpha_C1 = 5.0_Double
  TauCoeff1%Alpha_C2 = 6.0_Double
  TauCoeff1%Order_Index = 5
  DO l = 1, n_Channels
    DO j = 1, n_Absorbers
      TauCoeff1%Predictor_Index(:,j,l) = (/(i,i=0,n_Predictors)/)
    END DO
  END DO
  TauCoeff1%C = -99.0_Double


  ! Write and read a netCDF TauCoeff file
  ! -------------------------------------
  WRITE( *, '( /5x, "Testing TauCoeff netCDF I/O functions ..." )' )

  ! Write the netCDF data file
  Error_Status = Write_TauCoeff_netCDF( NC_FILENAME, &
                                        TauCoeff1, &
                                        Title        ='This is the title attribute', &
                                        History      ='This is the history attribute', &
                                        Sensor_Name  ='This is the sensor_name attribute',   &
                                        Platform_Name='This is the platform_name attribute', &
                                        Comment      ='This is the comment attribute',&
                                        ID_Tag       ='This is the id_tag attribute' )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF TauCoeff file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the netCDF data file
  Error_Status = Read_TauCoeff_netCDF( NC_FILENAME, &
                                       TauCoeff2 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF TauCoeff file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_TauCoeff( TauCoeff2, TauCoeff1 )
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


  ! Write and read a Binary TauCoeff file
  ! -------------------------------------
  WRITE( *, '( /5x, "Testing TauCoeff Binary Write/Read functions ..." )' )

  ! Write the Binary data file
  Error_Status = Write_TauCoeff_Binary( BIN_FILENAME, &
                                        TauCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary TauCoeff file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the Binary data file
  Error_Status = Read_TauCoeff_Binary( BIN_FILENAME, &
                                       TauCoeff3 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary TauCoeff file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_TauCoeff( TauCoeff3, TauCoeff1 )
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
    Error_Status = Read_TauCoeff_netCDF( NC_FILENAME, &
                                         TauCoeff1, &
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
    Error_Status = Read_TauCoeff_Binary( BIN_FILENAME, &
                                         TauCoeff2, &
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
    Error_Status = Assign_TauCoeff( TauCoeff3, TauCoeff2 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #",i0," of ", i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structures
  ! ----------------------
  Error_Status = Destroy_TauCoeff( TauCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff1 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_TauCoeff( TauCoeff2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_TauCoeff( TauCoeff3 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_TauCoeff
