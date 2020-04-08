!
! Test_ComponentTest
!
! Program to test the ComponentTest structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Mar-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_ComponentTest

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE ComponentTest_Define
  USE ComponentTest_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_ComponentTest'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER ::  NC_FILENAME = 'Test.ComponentTest.nc'
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: N_DATASETS   = 10
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: ID_Tag
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: nK, nL, nP, nIV, nOV
  INTEGER :: n
  TYPE(ComponentTest_type) :: cTest, cTest_Copy


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the ComponentTest structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the ComponentTest structure
  ! ---------------------------------
  ! Define some typical dimensions
  nK  = 100
  nL  = 15
  nP  = 21
  nIV = 4
  nOV = 5
  ! Allocate
  Error_Status = Allocate_ComponentTest( nK, nL, nP, nIV, nOV, cTest )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating ComponentTest structure.', &
                          FAILURE )
    STOP
  END IF


  ! Fill up the structures with stuff
  ! ---------------------------------
  cTest%Sensor_Id = 'sensor_platform'
  cTest%TestType = COMPONENTTEST_TLAD_TESTTYPE
  cTest%DataType = COMPONENTTEST_POLY_DATATYPE
  cTest%Pressure     = 1.0_fp
  cTest%Spectral     = (/(REAL(n,fp), n=1,cTest%nL)/)
  cTest%Perturbation = 3.0_fp
  DO n = 1, nIV
    WRITE( cTest%Input_Variable_Name(n), '("Input Variable #", i2.2 )' ) n
    WRITE( cTest%Input_Variable_Units(n), '("Units for input variable #", i2.2 )' ) n
  END DO
  DO n = 1, nOV
    WRITE( cTest%Output_Variable_Name(n), '("Output Variable #", i2.2 )' ) n
    WRITE( cTest%Output_Variable_Units(n), '("Units for output variable #", i2.2 )' ) n
  END DO
  cTest%d1 = 4.0_fp
  cTest%d2 = 5.0_fp


  ! Test the ComponentTest netCDF I/O functions
  ! ----------------------------------------
  WRITE( *,'(/5x,"Testing ComponentTest netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE( *,'(10x,"Writing/reading/checking test netCDF ComponentTest datafile ...")' )
  DO n = 1, N_DATASETS
    cTest%nM = n
    WRITE( cTest%nM_Name,'("dataset #",i0)' ) n
    Error_Status = Write_ComponentTest_netCDF( NC_FILENAME, cTest, &
                                               ID_Tag  = 'This is the profile set ID tag', &
                                               Title   = 'This is a title', &
                                               History = PROGRAM_RCS_ID, &
                                               Comment = 'This is a comment' )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing '//TRIM(cTest%nM_Name)//' to '//NC_FILENAME, &
                            Error_Status )
      STOP
    END IF
  END DO
  DO n = 1, N_DATASETS
    WRITE( Message,'("dataset #",i0)' ) n
    Error_Status = Read_ComponentTest_netCDF( NC_FILENAME, n, cTest_Copy, &
                                              ID_Tag  = ID_Tag , &
                                              Title   = Title  , &
                                              History = History, &
                                              Comment = Comment  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading '//TRIM(Message)//' from '//NC_FILENAME, &
                            Error_Status )
      STOP
    END IF
    IF ( n == 1 ) THEN
      WRITE( *,'(10x,"ID_Tag : ",a)') TRIM(ID_Tag )
      WRITE( *,'(10x,"Title  : ",a)') TRIM(Title  )
      WRITE( *,'(10x,"History: ",a)') TRIM(History)
      WRITE( *,'(10x,"Comment: ",a)') TRIM(Comment)
    END IF
    IF ( n == N_DATASETS ) THEN
      Error_Status = Equal_ComponentTest( cTest, cTest_Copy, Check_All=SET )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error comparing '//TRIM(Message)//' ComponentTest data', &
                              FAILURE )
        STOP
      END IF
    END IF
  END DO

  ! Test read the datafile
  WRITE( *,'(10x,"Looping for ComponentTest netCDF read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_ComponentTest_netCDF( NC_FILENAME, 1, cTest, Quiet=.TRUE. )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading ComponentTest datafile on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE( *,'(/5x,"Looping for ComponentTest structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_ComponentTest( cTest, cTest_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying ComponentTest structure array on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = Destroy_ComponentTest( cTest )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ComponentTest structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_ComponentTest( cTest_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ComponentTest_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_ComponentTest
