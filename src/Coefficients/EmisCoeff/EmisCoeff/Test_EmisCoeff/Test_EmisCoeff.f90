!
! Test_EmisCoeff
!
! Program to test the EmisCoeff definition and I/O routines.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-Jun-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_EmisCoeff

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds,          ONLY: fp=>fp_kind
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message, Program_Message
  USE EmisCoeff_Define,    ONLY: EmisCoeff_type, &
                                 Allocate_EmisCoeff, &
                                 Destroy_EmisCoeff, &
                                 Assign_EmisCoeff, &
                                 Equal_EmisCoeff
  USE EmisCoeff_Binary_IO, ONLY: Read_EmisCoeff_Binary, &
                                 Write_EmisCoeff_Binary
  USE EmisCoeff_netCDF_IO, ONLY: Read_EmisCoeff_netCDF, &
                                 Write_EmisCoeff_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_EmisCoeff'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER ::  NC_FILENAME = 'Test.EmisCoeff.nc'
  CHARACTER( * ), PARAMETER :: BIN_FILENAME = 'Test.EmisCoeff.bin'

  INTEGER, PARAMETER ::  MAX_N_LOOPS = 5000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 500

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Angles
  INTEGER :: n_Frequencies
  INTEGER :: n_Wind_Speeds
  INTEGER :: n
  TYPE(EmisCoeff_type) :: EmisCoeff1
  TYPE(EmisCoeff_type) :: EmisCoeff2
  TYPE(EmisCoeff_type) :: EmisCoeff3

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the EmisCoeff definition and IO functions.', &
                        '$Revision$' )

  ! Define some typical dimensions
  n_Angles      = 20
  n_Frequencies = 200
  n_Wind_Speeds = 50

  ! Allocate a EmisCoeff structure
  Error_Status = Allocate_EmisCoeff( n_Angles, &
                                     n_Frequencies, &
                                     n_Wind_Speeds, &
                                     EmisCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating EmisCoeff1 structre.', &
                          Error_Status )
    STOP
  END IF

  ! Fill up the structure with stuff
  EmisCoeff1%Angle      = 1.0_fp
  EmisCoeff1%Frequency  = 2.0_fp
  EmisCoeff1%Wind_Speed = 3.0_fp
  EmisCoeff1%Emissivity = 4.0_fp

  ! Write a netCDF file
  WRITE( *, '( /5x, "Testing EmisCoeff netCDF I/O functions ..." )' )
  Error_Status = Write_EmisCoeff_netCDF( NC_FILENAME, &
                                        EmisCoeff1, &
                                        Title = 'This is the title attribute', &
                                        History = 'This is the history attribute', &
                                        Comment = 'This is the comment attribute')
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the netCDF EmisCoeff file '//TRIM( NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( NC_FILENAME )//' netCDF file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  ! Read the netCDF data file
  Error_Status = Read_EmisCoeff_netCDF( NC_FILENAME, &
                                        EmisCoeff2 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the netCDF EmisCoeff file '//TRIM( NC_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( NC_FILENAME )//' netCDF file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_EmisCoeff( EmisCoeff2, EmisCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'NetCDF I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'NetCDF I/O structures are equal'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test Binary I/O functions..." )' )
  READ( *, * )


  ! Write a Binary file
  WRITE( *, '( /5x, "Testing EmisCoeff Binary Write/Read functions ..." )' )
  Error_Status = Write_EmisCoeff_Binary( BIN_FILENAME, &
                                         EmisCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error writing the Binary EmisCoeff file '//TRIM( BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( BIN_FILENAME )//' Binary file write was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  ! Read the Binary data file
  Error_Status = Read_EmisCoeff_Binary( BIN_FILENAME, &
                                        EmisCoeff3 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Error reading the Binary EmisCoeff file '//TRIM( BIN_FILENAME )
  ELSE
    Error_Status = INFORMATION
    Message = TRIM( BIN_FILENAME )//' Binary file read was successful'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  ! Compare the structures
  Error_Status = Equal_EmisCoeff( EmisCoeff3, EmisCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    Message = 'Binary I/O structures are different'
  ELSE
    Error_Status = INFORMATION
    Message = 'Binary I/O structures are equal'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM( Message ), &
                        Error_Status )

  WRITE( *, '( //5x, "Press <ENTER> to test for netCDF reader memory leaks..." )' )
  READ( *, * )


  ! Test the netCDF reader for memory leaks
  WRITE( *, '( /5x, "Looping for netCDF read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_EmisCoeff_netCDF( NC_FILENAME, &
                                          EmisCoeff1, &
                                          Quiet = 1 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for Binary reader memory leaks..." )' )
  READ( *, * )


  ! Test the Binary reader for memory leaks
  WRITE( *, '( /5x, "Looping for Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_EmisCoeff_Binary( BIN_FILENAME, &
                                          EmisCoeff2, &
                                          Quiet = 1 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO

  WRITE( *, '( //5x, "Press <ENTER> to test for structure assign function memory leaks..." )' )
  READ( *, * )


  ! Test the Assign function for memory leaks
  WRITE( *, '( /5x, "Looping for structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_EmisCoeff( EmisCoeff3, EmisCoeff2 )
    IF ( MOD( n, INFO_N_LOOPS ) == 0 .AND. n /= MAX_N_LOOPS ) THEN
      WRITE( Message, '( "Completed loop #", i5, " of ", i5 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structures
  Error_Status = Destroy_EmisCoeff( EmisCoeff1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff1 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_EmisCoeff( EmisCoeff2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff2 structure.', &
                          WARNING )
  END IF

  Error_Status = Destroy_EmisCoeff( EmisCoeff3 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying EmisCoeff3 structure.', &
                          WARNING )
  END IF

END PROGRAM Test_EmisCoeff
