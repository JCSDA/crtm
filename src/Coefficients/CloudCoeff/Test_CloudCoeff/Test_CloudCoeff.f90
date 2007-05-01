!
! Test_CloudCoeff
!
! Program to test the CloudCoeff structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_CloudCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                       Display_Message, PRogram_MEssage
  USE File_Utility
  USE CloudCoeff_Define
  USE CloudCoeff_Binary_IO
  USE CloudCoeff_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_CloudCoeff'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.CloudCoeff.bin'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.CloudCoeff.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 50000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 5000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_MW_FREQUENCIES = 3
  INTEGER, PARAMETER :: N_MW_RADII       = 4
  INTEGER, PARAMETER :: N_IR_FREQUENCIES = 5
  INTEGER, PARAMETER :: N_IR_RADII       = 6
  INTEGER, PARAMETER :: N_TEMPERATURES   = 7
  INTEGER, PARAMETER :: N_DENSITIES      = 8
  INTEGER, PARAMETER :: N_LEGENDRE_TERMS = 2
  INTEGER, PARAMETER :: N_PHASE_ELEMENTS = 2


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: n
  TYPE(CloudCoeff_type) :: CloudC, CloudC_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the CloudCoeff structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the CloudCoeff structure
  ! ---------------------------------
  Error_Status = Allocate_CloudCoeff( N_MW_FREQUENCIES, &  ! Input
                                      N_MW_RADII      , &  ! Input
                                      N_IR_FREQUENCIES, &  ! Input
                                      N_IR_RADII      , &  ! Input
                                      N_TEMPERATURES  , &  ! Input
                                      N_DENSITIES     , &  ! Input
                                      N_LEGENDRE_TERMS, &  ! Input
                                      N_PHASE_ELEMENTS, &  ! Input
                                      CloudC            )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating CloudCoeff structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  CloudC%Frequency_MW = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Frequency_MW))/), SHAPE(CloudC%Frequency_MW))
  CloudC%Frequency_IR = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Frequency_IR))/), SHAPE(CloudC%Frequency_IR))
  CloudC%Reff_MW      = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Reff_MW     ))/), SHAPE(CloudC%Reff_MW     ))
  CloudC%Reff_IR      = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Reff_IR     ))/), SHAPE(CloudC%Reff_IR     ))
  CloudC%Temperature  = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Temperature ))/), SHAPE(CloudC%Temperature ))
  CloudC%Density      = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%Density     ))/), SHAPE(CloudC%Density     ))

  CloudC%ke_L_MW      = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%ke_L_MW     ))/), SHAPE(CloudC%ke_L_MW     ))
  CloudC%w_L_MW       = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%w_L_MW      ))/), SHAPE(CloudC%w_L_MW      ))
  CloudC%g_L_MW       = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%g_L_MW      ))/), SHAPE(CloudC%g_L_MW      ))
  CloudC%pcoeff_L_MW  = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%pcoeff_L_MW ))/), SHAPE(CloudC%pcoeff_L_MW ))

  CloudC%ke_S_MW      = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%ke_S_MW     ))/), SHAPE(CloudC%ke_S_MW     ))
  CloudC%w_S_MW       = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%w_S_MW      ))/), SHAPE(CloudC%w_S_MW      ))
  CloudC%g_S_MW       = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%g_S_MW      ))/), SHAPE(CloudC%g_S_MW      ))
  CloudC%pcoeff_S_MW  = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%pcoeff_S_MW ))/), SHAPE(CloudC%pcoeff_S_MW ))

  CloudC%ke_IR        = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%ke_IR       ))/), SHAPE(CloudC%ke_IR       ))
  CloudC%w_IR         = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%w_IR        ))/), SHAPE(CloudC%w_IR        ))
  CloudC%g_IR         = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%g_IR        ))/), SHAPE(CloudC%g_IR        ))
  CloudC%pcoeff_IR    = RESHAPE((/(REAL(n,fp), n=1,SIZE(CloudC%pcoeff_IR   ))/), SHAPE(CloudC%pcoeff_IR   ))
  

  ! Test the CloudCoeff Binary I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing CloudCoeff Binary I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test Binary CloudCoeff datafile ..." )' )
  Error_Status = Write_CloudCoeff_Binary( BIN_FILENAME, CloudC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test CloudCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_CloudCoeff_Binary( BIN_FILENAME, CloudC_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test CloudCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Equal_CloudCoeff( CloudC, CloudC_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test CloudCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for CloudCoeff Binary read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_CloudCoeff_Binary( BIN_FILENAME, CloudC, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading CloudCoeff datafile on attempt # ", i0 )' ) n
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


  ! Test the CloudCoeff netCDF I/O functions
  ! ----------------------------------------
  WRITE(*,'( /5x, "Testing CloudCoeff netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE(*,'( 10x, "Writing/reading/checking test netCDF CloudCoeff datafile ..." )' )
  Error_Status = Write_CloudCoeff_netCDF( NC_FILENAME, CloudC, &
                                          Title   = 'This is a title'  , &
                                          History = PROGRAM_RCS_ID     , &
                                          Comment = 'This is a comment'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test CloudCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_CloudCoeff_netCDF( NC_FILENAME, CloudC_Copy, &
                                         Title   = Title  , &
                                         History = History, &
                                         Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test CloudCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *, '(10x, "Title  : ",a)') TRIM(Title  )
  WRITE( *, '(10x, "History: ",a)') TRIM(History)
  WRITE( *, '(10x, "Comment: ",a)') TRIM(Comment)
  Error_Status = Equal_CloudCoeff( CloudC, CloudC_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test CloudCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *, '( 10x, "Looping for CloudCoeff netCDF read memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_CloudCoeff_netCDF( NC_FILENAME, CloudC, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading CloudCoeff datafile on attempt # ", i0 )' ) n
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
  WRITE(*,'( /5x, "Looping for CloudCoeff structure copy memory leak test ..." )' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_CloudCoeff( CloudC, CloudC_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error copying CloudCoeff structure array on attempt # ", i0 )' ) n
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
  Error_Status = Destroy_CloudCoeff( CloudC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CloudC structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_CloudCoeff( CloudC_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CloudC_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_CloudCoeff
