!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_SpcCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE File_Utility
  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*),  PARAMETER :: PROGRAM_NAME   = 'Test_SpcCoeff'
  CHARACTER(*),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Test output filenames
  CHARACTER(*), PARAMETER :: BIN_FILENAME = 'Test.SpcCoeff.bin'
  CHARACTER(*), PARAMETER :: NC_FILENAME  = 'Test.SpcCoeff.nc'
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Memory leak looping values
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 5000
  INTEGER, PARAMETER :: INFO_N_LOOPS = 1000
  ! Structure dimensions
  INTEGER, PARAMETER :: N_CHANNELS = 200
  INTEGER, PARAMETER :: N_N_FOVS = 2
  INTEGER, PARAMETER :: N_FOVS(N_N_FOVS) = (/0,90/)

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: History
  CHARACTER(256) :: Comment
  INTEGER :: Error_Status
  INTEGER :: i, n
  TYPE(SpcCoeff_type) :: SpcCoeff, SpcCoeff_Copy

                                                           
  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the SpcCoeff structure '//&
                        'manipulation and I/O functions.', &
                        '$Revision$' )


  ! Loop over N_FOVS values
  ! -----------------------
  FOV_Loop: DO i = 1, N_N_FOVS
  
    WRITE( *,'(////10x,"NUMBER OF FIELDS OF VIEW FOR THIS TEST: ",i0,//)' ) N_FOVS(i)
    
    
    ! Allocate the SpcCoeff structure
    ! ---------------------------------
    Error_Status = Allocate_SpcCoeff( N_CHANNELS      , &  ! Input
                                      SpcCoeff        , &  ! Output
                                      n_FOVs=N_FOVS(i)  ) ! Optional Input
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error allocating SpcCoeff structure array.', &
                            FAILURE )
      STOP
    END IF


    ! Fill structure with pretend data
    ! --------------------------------
    SpcCoeff%Sensor_Id                     = 'sensor_platform'
    SpcCoeff%WMO_Satellite_ID              = 1
    SpcCoeff%WMO_Sensor_ID                 = 2
    SpcCoeff%Sensor_Type                   = INFRARED_SENSOR
    SpcCoeff%Sensor_Channel                = (/(n, n=1,SpcCoeff%n_Channels)/)
    SpcCoeff%Polarization                  = UNPOLARIZED 
    SpcCoeff%Channel_Flag                  = 3
    SpcCoeff%Frequency                     = ZERO
    SpcCoeff%Wavenumber                    = ZERO
    SpcCoeff%Planck_C1                     = ZERO
    SpcCoeff%Planck_C2                     = ZERO
    SpcCoeff%Band_C1                       = ZERO
    SpcCoeff%Band_C2                       = ZERO
    SpcCoeff%Cosmic_Background_Radiance    = ZERO
    SpcCoeff%Solar_Irradiance              = ZERO 

    IF ( SpcCoeff%AC_Present ) THEN
      SpcCoeff%AC%Sensor_Id        = SpcCoeff%Sensor_Id       
      SpcCoeff%AC%WMO_Satellite_ID = SpcCoeff%WMO_Satellite_ID
      SpcCoeff%AC%WMO_Sensor_ID    = SpcCoeff%WMO_Sensor_ID   
      SpcCoeff%AC%Sensor_Channel   = SpcCoeff%Sensor_Channel  
      SpcCoeff%AC%A_earth    = 0.97_fp
      SpcCoeff%AC%A_space    = 0.02_fp
      SpcCoeff%AC%A_platform = 0.01_fp
    END IF


    ! Test the SpcCoeff Binary I/O functions
    ! ----------------------------------------
    WRITE(*,'( /5x, "Testing SpcCoeff Binary I/O functions ..." )' )

    ! Write, read and test for equality
    WRITE(*,'( 10x, "Writing/reading/checking test Binary SpcCoeff datafile ..." )' )
    Error_Status = Write_SpcCoeff_Binary( BIN_FILENAME, SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing test SpcCoeff Binary data file.', &
                            FAILURE )
      STOP
    END IF
    Error_Status = Read_SpcCoeff_Binary( BIN_FILENAME, SpcCoeff_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading test SpcCoeff Binary data file.', &
                            FAILURE )
      STOP
    END IF
    Error_Status = Equal_SpcCoeff( SpcCoeff, SpcCoeff_Copy, Check_All=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error comparing test SpcCoeff data.', &
                            FAILURE )
      STOP
    END IF

    ! Test read the datafile
    WRITE( *, '( 10x, "Looping for SpcCoeff Binary read memory leak test ..." )' )
    DO n = 1, MAX_N_LOOPS
      Error_Status = Read_SpcCoeff_Binary( BIN_FILENAME, SpcCoeff, Quiet=SET )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading SpcCoeff datafile on attempt # ", i0 )' ) n
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


    ! Test the SpcCoeff netCDF I/O functions
    ! ----------------------------------------
    WRITE(*,'( /5x, "Testing SpcCoeff netCDF I/O functions ..." )' )

    ! Write, read and test for equality
    WRITE(*,'( 10x, "Writing/reading/checking test netCDF SpcCoeff datafile ..." )' )
    Error_Status = Write_SpcCoeff_netCDF( NC_FILENAME, SpcCoeff, &
                                         Title   = 'This is a title'  , &
                                         History = PROGRAM_RCS_ID     , &
                                         Comment = 'This is a comment'  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing test SpcCoeff netCDF data file.', &
                            FAILURE )
      STOP
    END IF
    Error_Status = Read_SpcCoeff_netCDF( NC_FILENAME, SpcCoeff_Copy, &
                                        Title   = Title  , &
                                        History = History, &
                                        Comment = Comment  )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading test SpcCoeff netCDF data file.', &
                            FAILURE )
      STOP
    END IF
    WRITE( *, '(10x, "Title  : ",a)') TRIM(Title  )
    WRITE( *, '(10x, "History: ",a)') TRIM(History)
    WRITE( *, '(10x, "Comment: ",a)') TRIM(Comment)
    Error_Status = Equal_SpcCoeff( SpcCoeff, SpcCoeff_Copy, Check_All=SET )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error comparing test SpcCoeff data.', &
                            FAILURE )
      STOP
    END IF

    ! Test read the datafile
    WRITE( *, '( 10x, "Looping for SpcCoeff netCDF read memory leak test ..." )' )
    DO n = 1, MAX_N_LOOPS
      Error_Status = Read_SpcCoeff_netCDF( NC_FILENAME, SpcCoeff, Quiet=SET )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading SpcCoeff datafile on attempt # ", i0 )' ) n
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
    WRITE(*,'( /5x, "Looping for SpcCoeff structure copy memory leak test ..." )' )
    DO n = 1, MAX_N_LOOPS
      Error_Status = Assign_SpcCoeff( SpcCoeff, SpcCoeff_Copy )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying SpcCoeff structure array on attempt # ", i0 )' ) n
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
    Error_Status = Destroy_SpcCoeff( SpcCoeff )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff structure.', &
                            Error_Status )
      STOP
    END IF
    Error_Status = Destroy_SpcCoeff( SpcCoeff_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff_Copy structure.', &
                            Error_Status )
      STOP
    END IF
    
  END DO FOV_Loop
  
END PROGRAM Test_SpcCoeff
