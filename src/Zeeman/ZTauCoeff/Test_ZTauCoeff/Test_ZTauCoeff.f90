!
! Test_ZTauCoeff
!
! Program to test the ZTauCoeff structure manipulation and
! I/O functions
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Jul-2007
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_ZTauCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE ZTauCoeff_Define
  USE ZTauCoeff_Binary_IO
  USE ZTauCoeff_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_ZTauCoeff'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.ZTauCoeff.bin'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.ZTauCoeff.nc'
  
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  
  INTEGER, PARAMETER :: SET = 1
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 50000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_PREDICTORS = 10
  INTEGER, PARAMETER :: N_LAYERS     = 100
  INTEGER, PARAMETER :: N_CHANNELS   = 6
  INTEGER, PARAMETER :: N_SETS       = 3


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: n
  TYPE(ZTauCoeff_type) :: ZTauCoeff, ZTauCoeff_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the ZTauCoeff structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Allocate the ZTauCoeff structure
  ! ---------------------------------
  Error_Status = Allocate_ZTauCoeff( N_PREDICTORS, &  ! Input
                                     N_LAYERS    , &  ! Input
                                     N_CHANNELS  , &  ! Input
                                     N_SETS      , &  ! Input
                                     ZTauCoeff     )  ! Output
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error allocating ZTauCoeff structure array.', &
                          FAILURE )
    STOP
  END IF


  ! Fill structure with pretend data
  ! --------------------------------
  ZTauCoeff%Sensor_Id = 'sensor_platform'
  ZTauCoeff%Sensor_Channel = (/(n, n=1,ZTauCoeff%n_Channels)/)
  ZTauCoeff%Level_Pressure = 1.0_fp
  ZTauCoeff%Pressure       = 2.0_fp
  ZTauCoeff%Temperature    = 3.0_fp
  ZTauCoeff%ChannelIndex   = 4
  ZTauCoeff%PredictorIndex = 5
  ZTauCoeff%Secant_Zenith  = 6.0_fp
  ZTauCoeff%C              = 7.0_fp


  ! Test the ZTauCoeff Binary I/O functions
  ! ----------------------------------------
  WRITE( *,'(/5x,"Testing ZTauCoeff Binary I/O functions ...")' )

  ! Write, read and test for equality
  WRITE( *,'(10x,"Writing/reading/checking test Binary ZTauCoeff datafile ...")' )
  Error_Status = Write_ZTauCoeff_Binary( BIN_FILENAME, ZTauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test ZTauCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_ZTauCoeff_Binary( BIN_FILENAME, ZTauCoeff_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test ZTauCoeff Binary data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Equal_ZTauCoeff( ZTauCoeff, ZTauCoeff_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test ZTauCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *,'(10x,"Looping for ZTauCoeff Binary read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_ZTauCoeff_Binary( BIN_FILENAME, ZTauCoeff, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading ZTauCoeff datafile on attempt # ",i0)' ) n
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


  ! Test the ZTauCoeff netCDF I/O functions
  ! ----------------------------------------
  WRITE( *,'(/5x,"Testing ZTauCoeff netCDF I/O functions ..." )' )

  ! Write, read and test for equality
  WRITE( *,'(10x,"Writing/reading/checking test netCDF ZTauCoeff datafile ...")' )
  Error_Status = Write_ZTauCoeff_netCDF( NC_FILENAME, ZTauCoeff, &
                                         Title   = 'This is a title'  , &
                                         History = PROGRAM_RCS_ID     , &
                                         Comment = 'This is a comment'  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing test ZTauCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  Error_Status = Read_ZTauCoeff_netCDF( NC_FILENAME, ZTauCoeff_Copy, &
                                        Title   = Title  , &
                                        History = History, &
                                        Comment = Comment  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading test ZTauCoeff netCDF data file.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *,'(10x,"Title  : ",a)') TRIM(Title  )
  WRITE( *,'(10x,"History: ",a)') TRIM(History)
  WRITE( *,'(10x,"Comment: ",a)') TRIM(Comment)
  Error_Status = Equal_ZTauCoeff( ZTauCoeff, ZTauCoeff_Copy, Check_All=SET )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error comparing test ZTauCoeff data.', &
                          FAILURE )
    STOP
  END IF

  ! Test read the datafile
  WRITE( *,'(10x,"Looping for ZTauCoeff netCDF read memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Read_ZTauCoeff_netCDF( NC_FILENAME, ZTauCoeff, Quiet=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error reading ZTauCoeff datafile on attempt # ",i0)' ) n
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
  WRITE( *,'(/5x,"Looping for ZTauCoeff structure copy memory leak test ...")' )
  DO n = 1, MAX_N_LOOPS
    Error_Status = Assign_ZTauCoeff( ZTauCoeff, ZTauCoeff_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Error copying ZTauCoeff structure array on attempt # ",i0)' ) n
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
  Error_Status = Destroy_ZTauCoeff( ZTauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ZTauCoeff structure.', &
                          Error_Status )
    STOP
  END IF
  Error_Status = Destroy_ZTauCoeff( ZTauCoeff_Copy )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ZTauCoeff_Copy structure.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_ZTauCoeff
