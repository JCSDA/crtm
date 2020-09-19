!
! Test_AntCorr
!
! Program to test the AntCorr structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jun-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_AntCorr

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE AntCorr_Define
  USE AntCorr_Binary_IO
  USE AntCorr_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_AntCorr'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.AntCorr.bin'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.AntCorr.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 200
  INTEGER, PARAMETER :: INFO_N_LOOPS = 50
  ! Structure dimensions
  INTEGER, PARAMETER :: N_FOVS     = 90
  INTEGER, PARAMETER :: N_CHANNELS = 200


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: n
  TYPE(AntCorr_type) :: AntCorr, AntCorr_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the AntCorr structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the AntCorr structure
  ! ---------------------------------
  Error_Status = Allocate_AntCorr( N_FOVS    , &  ! Input
                                   N_CHANNELS, &  ! Input
                                   AntCorr     )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating AntCorr structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  AntCorr%Sensor_Id = 'sensor_platform'
  AntCorr%Sensor_Channel = (/(n, n=1,AntCorr%n_Channels)/)
  AntCorr%A_earth    = 0.97_fp
  AntCorr%A_space    = 0.02_fp
  AntCorr%A_platform = 0.01_fp


  ! Test the AntCorr Binary I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing AntCorr Binary I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test Binary AntCorr datafile ..." )' )
  Error_Status = Write_AntCorr_Binary( BIN_FILENAME, AntCorr )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test AntCorr Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_AntCorr_Binary( BIN_FILENAME, AntCorr_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test AntCorr Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Equal_AntCorr( AntCorr, AntCorr_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test AntCorr data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for AntCorr Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_AntCorr_Binary( BIN_FILENAME, AntCorr, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading AntCorr datafile on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Test the AntCorr netCDF I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing AntCorr netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test netCDF AntCorr datafile ..." )' )
  Error_Status = Write_AntCorr_netCDF( NC_FILENAME, AntCorr, &
                                       Title   = 'This is a title'  , &
                                       History = PROGRAM_RCS_ID     , &
                                       Comment = 'This is a comment'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test AntCorr netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_AntCorr_netCDF( NC_FILENAME, AntCorr_Copy, &
                                      Title   = Title  , &
                                      History = History, &
                                      Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test AntCorr netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *, '(10x, "Title  : ",a)') TRIM(Title  )
  WRITE( *, '(10x, "History: ",a)') TRIM(History)
  WRITE( *, '(10x, "Comment: ",a)') TRIM(Comment)
  Error_Status = Equal_AntCorr( AntCorr, AntCorr_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test AntCorr data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for AntCorr netCDF read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_AntCorr_netCDF( NC_FILENAME, AntCorr, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading AntCorr datafile on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE(*,'( /5x, "Looping for AntCorr structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_AntCorr( AntCorr, AntCorr_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying AntCorr structure array on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = Destroy_AntCorr( AntCorr )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AntCorr structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_AntCorr( AntCorr_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AntCorr_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_AntCorr
