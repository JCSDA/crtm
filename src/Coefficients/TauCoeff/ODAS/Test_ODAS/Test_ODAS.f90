!
! Test_ODAS
!
! Program to test the ODAS definition and I/O routines.
!
!
! FILES ACCESSED:
!       - netCDF format ODAS data file, both input and output
!       - Binary format ODAS data file, both input and output
!
! SIDE EFFECTS:
!       The test output files are overwritten if they already exist.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Jun-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_ODAS

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE ODAS_Define
  USE ODAS_Binary_IO
  USE ODAS_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_ODAS'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER ::  NC_FILENAME = 'Test.ODAS.nc'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.ODAS.bin'
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
  TYPE(ODAS_type) :: ODAS1
  TYPE(ODAS_type) :: ODAS2
  TYPE(ODAS_type) :: ODAS3

  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test ODAS definition and I/O routines', &
                        '$Revision$' )

  ! Fill a structure with dummy values
  ! ----------------------------------
  ! Define some typical dimensions
  n_Orders     = 10
  n_Predictors = 5
  n_Absorbers  = 3
  n_Channels   = 100

  ! Allocate a ODAS structure
  Error_Status = Allocate_ODAS( n_Orders, &
                                n_Predictors, &
                                n_Absorbers, &
                                n_Channels, &
                                ODAS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating ODAS1 structre.', &
                          Error_Status )
    STOP
  END IF

  ! Fill up the structure with stuff
  ODAS1%Sensor_Id        = 'sensor_platform'
  ODAS1%WMO_Satellite_ID = 2
  ODAS1%WMO_Sensor_ID    = 3
  ODAS1%Sensor_Channel   = (/(l,l=1,n_Channels)/)
  ODAS1%Absorber_ID = (/(j,j=1,n_Absorbers)/)    
  ODAS1%Alpha    = 4.0_Double
  ODAS1%Alpha_C1 = 5.0_Double
  ODAS1%Alpha_C2 = 6.0_Double
  ODAS1%Order_Index = 5
  DO l = 1, n_Channels
    DO j = 1, n_Absorbers
      ODAS1%Predictor_Index(:,j,l) = (/(i,i=0,n_Predictors)/)
    END DO
  END DO
  ODAS1%C = -99.0_Double


  ! Copy the structure and test for equality
  ! ----------------------------------------
  Error_Status = Assign_ODAS( ODAS1, ODAS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error copying ODAS1 structure.', &
                          Error_Status )
    STOP
  END IF
  
  Error_Status = Equal_ODAS( ODAS1, ODAS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'ODAS1 and ODAS2 Structures are not equal.', &
                          Error_Status )
    STOP
  END IF
  
  
  ! Write and read a netCDF ODAS file
  ! -------------------------------------
  WRITE( *,'(/5x,"Testing ODAS netCDF I/O functions ...")' )

  ! Write the netCDF data file
  Error_Status = Write_ODAS_netCDF( NC_FILENAME, &
                                    ODAS1, &
                                    Title         ='This is the title attribute', &
                                    History       ='This is the history attribute', &
                                    Comment       ='This is the comment attribute',&
                                    Profile_Set_ID='This is the id_tag attribute' )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF ODAS file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the netCDF data file
  Error_Status = Read_ODAS_netCDF( NC_FILENAME, ODAS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF ODAS file '//TRIM(NC_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(NC_FILENAME)//' netCDF file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_ODAS( ODAS2, ODAS1 )
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


  ! Write and read a Binary ODAS file
  ! -------------------------------------
  WRITE( *, '( /5x, "Testing ODAS Binary Write/Read functions ..." )' )

  ! Write the Binary data file
  Error_Status = Write_ODAS_Binary( BIN_FILENAME, &
                                        ODAS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary ODAS file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Read the Binary data file
  Error_Status = Read_ODAS_Binary( BIN_FILENAME, &
                                       ODAS3 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary ODAS file '//TRIM(BIN_FILENAME)
  ELSE
    Error_Status = INFORMATION
    Message = TRIM(BIN_FILENAME)//' Binary file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_ODAS( ODAS3, ODAS1 )
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
    Error_Status = Read_ODAS_netCDF( NC_FILENAME, &
                                     ODAS1, &
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
    Error_Status = Read_ODAS_Binary( BIN_FILENAME, &
                                    ODAS2, &
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
    Error_Status = Assign_ODAS( ODAS3, ODAS2 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #",i0," of ", i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structures
  ! ----------------------
  Error_Status = Destroy_ODAS( ODAS1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS1 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODAS( ODAS2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_ODAS( ODAS3 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ODAS3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_ODAS
