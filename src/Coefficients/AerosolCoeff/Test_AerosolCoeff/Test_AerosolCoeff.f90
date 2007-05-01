!
! Test_AerosolCoeff
!
! Program to test the AerosolCoeff structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_AerosolCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                       Display_Message, PRogram_MEssage
  USE File_Utility
  USE AerosolCoeff_Define
  USE AerosolCoeff_Binary_IO
  USE AerosolCoeff_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_AerosolCoeff'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.AerosolCoeff.bin'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.AerosolCoeff.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 50000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 5000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_WAVELENGTHS    = 3
  INTEGER, PARAMETER :: N_RADII          = 4
  INTEGER, PARAMETER :: N_TYPES          = 5
  INTEGER, PARAMETER :: N_R_HUMIDITY     = 6
  INTEGER, PARAMETER :: N_LEGENDRE_TERMS = 2
  INTEGER, PARAMETER :: N_PHASE_ELEMENTS = 6


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: i, n
  TYPE(AerosolCoeff_type) :: AerosolC, AerosolC_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the AerosolCoeff structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the AerosolCoeff structure
  ! ---------------------------------
  Error_Status = Allocate_AerosolCoeff( N_WAVELENGTHS   , &  ! Input
                                        N_RADII         , &  ! Input
                                        N_TYPES         , &  ! Input
                                        N_R_HUMIDITY    , &  ! Input
                                        N_LEGENDRE_TERMS, &  ! Input
                                        N_PHASE_ELEMENTS, &  ! Input
                                        AerosolC          )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating AerosolCoeff structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  DO n = 1, N_TYPES
    WRITE(AerosolC%Type_Name(n),'("Type ",i0)') n
    AerosolC%Type(n) = n
  END DO

  AerosolC%Wavelength = (/(REAL(n,fp), n=1,AerosolC%n_Wavelengths)/)
  AerosolC%Frequency  = 10000.0_fp/AerosolC%Wavelength
  AerosolC%Reff       = RESHAPE((/(REAL(n,fp), n=1,SIZE(AerosolC%Reff))/), SHAPE(AerosolC%Reff))
  AerosolC%RH         = (/(REAL(n,fp), n=1,AerosolC%n_RH)/)

  AerosolC%ke     = RESHAPE((/(REAL(n,fp), n=1,SIZE(AerosolC%ke    ))/), SHAPE(AerosolC%ke    ))
  AerosolC%w      = RESHAPE((/(REAL(n,fp), n=1,SIZE(AerosolC%w     ))/), SHAPE(AerosolC%w     ))
  AerosolC%g      = RESHAPE((/(REAL(n,fp), n=1,SIZE(AerosolC%g     ))/), SHAPE(AerosolC%g     ))
  AerosolC%pcoeff = RESHAPE((/(REAL(n,fp), n=1,SIZE(AerosolC%pcoeff))/), SHAPE(AerosolC%pcoeff))
  

  ! Test the AerosolCoeff Binary I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing AerosolCoeff Binary I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test Binary AerosolCoeff datafile ..." )' )
  Error_Status = Write_AerosolCoeff_Binary( BIN_FILENAME, AerosolC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test AerosolCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_AerosolCoeff_Binary( BIN_FILENAME, AerosolC_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test AerosolCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Equal_AerosolCoeff( AerosolC, AerosolC_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test AerosolCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for AerosolCoeff Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_AerosolCoeff_Binary( BIN_FILENAME, AerosolC, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading AerosolCoeff datafile on attempt # ", i0 )' ) n
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


  ! Test the AerosolCoeff netCDF I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing AerosolCoeff netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test netCDF AerosolCoeff datafile ..." )' )
  Error_Status = Write_AerosolCoeff_netCDF( NC_FILENAME, AerosolC, &
                                            Title   = 'This is a title'  , &
                                            History = PROGRAM_RCS_ID     , &
                                            Comment = 'This is a comment'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test AerosolCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_AerosolCoeff_netCDF( NC_FILENAME, AerosolC_Copy, &
                                           Title   = Title  , &
                                           History = History, &
                                           Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test AerosolCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *, '(10x, "Title  : ",a)') TRIM(Title  )
  WRITE( *, '(10x, "History: ",a)') TRIM(History)
  WRITE( *, '(10x, "Comment: ",a)') TRIM(Comment)
  Error_Status = Equal_AerosolCoeff( AerosolC, AerosolC_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test AerosolCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for AerosolCoeff netCDF read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_AerosolCoeff_netCDF( NC_FILENAME, AerosolC, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading AerosolCoeff datafile on attempt # ", i0 )' ) n
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
  WRITE(*,'( /5x, "Looping for AerosolCoeff structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_AerosolCoeff( AerosolC, AerosolC_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying AerosolCoeff structure array on attempt # ", i0 )' ) n
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
  Error_Status = Destroy_AerosolCoeff( AerosolC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolC structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_AerosolCoeff( AerosolC_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying AerosolC_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_AerosolCoeff
