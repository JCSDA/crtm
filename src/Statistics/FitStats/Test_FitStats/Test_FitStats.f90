!
! Test_FitStats
!
! Program to test the FitStats structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Jan-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_FitStats

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE FitStats_Define
  USE FitStats_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_FitStats'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.FitStats.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 500
  INTEGER, PARAMETER :: INFO_N_LOOPS = 100
  ! Structure dimensions
  INTEGER, PARAMETER :: N_CHANNELS   = 1000


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: ID_Tag
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: n
  TYPE(FitStats_type) :: FitStats, FitStats_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the FitStats structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the FitStats structure
  ! ---------------------------------
  Error_Status = Allocate_FitStats( N_CHANNELS, &  ! Input
                                    FitStats    )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating FitStats structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  FitStats%Sensor_Id = 'sensor_platform'
  FitStats%Sensor_Channel = (/(n, n=1,FitStats%n_Channels)/)
  FitStats%Order         = 1
  FitStats%n_Predictors  = 2
  FitStats%Predictor_Idx = 3
  FitStats%Frequency     =  0.5_fp
  FitStats%Fit_Residual  =  1.0_fp
  FitStats%Tb_BIAS       =  2.0_fp
  FitStats%Tb_SDEV       =  3.0_fp
  FitStats%Tb_RMS        =  4.0_fp
  FitStats%Tb_MAX        =  5.0_fp
  FitStats%Tau_BIAS      =  6.0_fp
  FitStats%Tau_SDEV      =  7.0_fp
  FitStats%Tau_RMS       =  8.0_fp
  FitStats%Tau_MAX       =  9.0_fp
  FitStats%Tau_Max_BIAS  = 10.0_fp
  FitStats%Tau_Max_SDEV  = 11.0_fp
  FitStats%Tau_Max_RMS   = 12.0_fp
  FitStats%Max_Pred_Term = 13.0_fp


  ! Test the FitStats netCDF I/O functions
  ! ----------------------------------------
  WRITE( *,'(/5x,"Testing FitStats netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE( *,'(10x,"Writing/reading/checking test netCDF FitStats datafile ...")' )
  Error_Status = Write_FitStats_netCDF( NC_FILENAME, FitStats, &
                                        ID_Tag  = 'This is the profile set ID tag', &
                                        Title   = 'This is a title'  , &
                                        History = PROGRAM_RCS_ID     , &
                                        Comment = 'This is a comment'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test FitStats netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_FitStats_netCDF( NC_FILENAME, FitStats_Copy, &
                                       ID_Tag  = ID_Tag , &
                                       Title   = Title  , &
                                       History = History, &
                                       Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test FitStats netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *,'(10x,"ID_Tag : ",a)') TRIM(ID_Tag )
  WRITE( *,'(10x,"Title  : ",a)') TRIM(Title  )
  WRITE( *,'(10x,"History: ",a)') TRIM(History)
  WRITE( *,'(10x,"Comment: ",a)') TRIM(Comment)
  Error_Status = Equal_FitStats( FitStats, FitStats_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test FitStats data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *,'(10x,"Looping for FitStats netCDF read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_FitStats_netCDF( NC_FILENAME, FitStats, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading FitStats datafile on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Loop for assign leak test
  ! -------------------------
  WRITE( *,'(/5x,"Looping for FitStats structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_FitStats( FitStats, FitStats_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying FitStats structure array on attempt # ",i0)' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message,'("Completed loop #",i0," of ",i0)' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
  END DO


  ! Destroy the structure arrays
  ! ----------------------------
  Error_Status = Destroy_FitStats( FitStats )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying FitStats structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_FitStats( FitStats_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying FitStats_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_FitStats
