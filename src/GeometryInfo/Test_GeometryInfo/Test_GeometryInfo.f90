!
! Test_GeometryInfo
!
! Program to test the GeometryInfo definition and application functions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_GeometryInfo

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds, ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, &
                             Display_Message, Program_Message
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_Assign_GeometryInfo, &
                                      CRTM_Equal_GeometryInfo
  USE CRTM_GeometryInfo_IO, ONLY: CRTM_Write_GeometryInfo, &
                                  CRTM_Inquire_GeometryInfo, &
                                  CRTM_Read_GeometryInfo
  USE CRTM_GeometryInfo, ONLY: CRTM_Compute_GeometryInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_GeometryInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  CHARACTER(*), PARAMETER :: TEST_FILENAME = 'Test.gInfo'
  INTEGER,      PARAMETER :: N_PROFILES = 6

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: Error_Status
  INTEGER :: m
  TYPE(CRTM_GeometryInfo_type), DIMENSION(N_PROFILES) :: gInfo, gInfo_read, gInfo_assign


  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the CRTM GeometryInfo structure '//&
                        'manipulation functions.', &
                        '$Revision$' )


  ! Load some input data
  gInfo%Longitude            = 123.0_fp
  gInfo%Latitude             = 34.0_fp
  gInfo%Surface_Altitude     = 25.0_fp
  gInfo%Sensor_Scan_Angle    = 55.0_fp
  gInfo%Sensor_Zenith_Angle  = 60.0_fp
  gInfo%Sensor_Azimuth_Angle = 0.5_fp
  gInfo%Source_Zenith_Angle  = 71.0_fp
  gInfo%Source_Azimuth_Angle = 20.0_fp
  gInfo%Flux_Zenith_Angle    = 56.3_fp


  ! Fill the rest of GeometryInfo structure
  DO m = 1, N_PROFILES
    WRITE( msg, '("gInfo(",i0,") compute")' ) m
    Error_Status = CRTM_Compute_GeometryInfo( gInfo(m) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
      STOP
    ELSE
      CALL Display_Message( PROGRAM_NAME, TRIM(msg), SUCCESS )
    END IF
  END DO

  
  ! Assign and compare the data
  Error_Status = CRTM_Assign_GeometryInfo( gInfo, gInfo_assign )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'gInfo assignment', FAILURE )
    STOP
  END IF
  Error_Status = CRTM_Equal_GeometryInfo( gInfo, gInfo_assign )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Assign: gInfo structures different', FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, 'Assign: gInfo structures equal!', SUCCESS )
  END IF

  ! Output results
  CALL Print_gInfo(gInfo)


  ! Write the datafile
  Error_Status = CRTM_Write_GeometryInfo( TEST_FILENAME, gInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'gInfo write', FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, 'gInfo write', SUCCESS )
  END IF


  ! Inquire the datafile
  msg = TEST_FILENAME//' inquire'
  Error_Status = CRTM_Inquire_GeometryInfo( TEST_FILENAME, n_Profiles=m )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, TRIM(msg), FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, TRIM(msg), SUCCESS )
  END IF
  ! ...Test inquire result
  IF ( m /= N_PROFILES ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Inquire: n_profiles different', FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, 'Inquire: n_profiles equal', SUCCESS )
  END IF


  ! Read the data
  Error_Status = CRTM_Read_GeometryInfo( TEST_FILENAME, gInfo_read )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'gInfo read', FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, 'gInfo read', SUCCESS )
  END IF


  ! Compare the data
  Error_Status = CRTM_Equal_GeometryInfo( gInfo, gInfo_read )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, 'Read: gInfo structures different', FAILURE )
    STOP
  ELSE
    CALL Display_Message( PROGRAM_NAME, 'Read: gInfo structures equal!', SUCCESS )
  END IF

CONTAINS

  SUBROUTINE Print_gInfo( gInfo )
    TYPE(CRTM_GeometryInfo_type) :: gInfo(:)
    CHARACTER(80) :: fmt
    WRITE( fmt, '(i0,"(1x,es13.6)")' ) SIZE(gInfo)
    
    WRITE( *, '( /2x, "USER INPUT" )' )
    WRITE( *, '( 4x, "Longitude            :", '//TRIM(fmt)//')' ) gInfo%Longitude           
    WRITE( *, '( 4x, "Latitude             :", '//TRIM(fmt)//')' ) gInfo%Latitude            
    WRITE( *, '( 4x, "Surface_Altitude     :", '//TRIM(fmt)//')' ) gInfo%Surface_Altitude    
    WRITE( *, '( 4x, "Sensor_Scan_Angle    :", '//TRIM(fmt)//')' ) gInfo%Sensor_Scan_Angle   
    WRITE( *, '( 4x, "Sensor_Zenith_Angle  :", '//TRIM(fmt)//')' ) gInfo%Sensor_Zenith_Angle 
    WRITE( *, '( 4x, "Sensor_Azimuth_Angle :", '//TRIM(fmt)//')' ) gInfo%Sensor_Azimuth_Angle
    WRITE( *, '( 4x, "Source_Zenith_Angle  :", '//TRIM(fmt)//')' ) gInfo%Source_Zenith_Angle 
    WRITE( *, '( 4x, "Source_Azimuth_Angle :", '//TRIM(fmt)//')' ) gInfo%Source_Azimuth_Angle
    WRITE( *, '( 4x, "Flux_Zenith_Angle    :", '//TRIM(fmt)//')' ) gInfo%Flux_Zenith_Angle

    WRITE( *, '( /2x, "DERIVED INPUT" )' )
    WRITE( *, '( 4x, "Distance_Ratio       :", '//TRIM(fmt)//')' ) gInfo%Distance_Ratio
    WRITE( *, '( 4x, "Sensor_Scan_Radian   :", '//TRIM(fmt)//')' ) gInfo%Sensor_Scan_Radian
    WRITE( *, '( 4x, "Sensor_Zenith_Radian :", '//TRIM(fmt)//')' ) gInfo%Sensor_Zenith_Radian
    WRITE( *, '( 4x, "Sensor_Azimuth_Radian:", '//TRIM(fmt)//')' ) gInfo%Sensor_Azimuth_Radian
    WRITE( *, '( 4x, "Secant_Sensor_Zenith :", '//TRIM(fmt)//')' ) gInfo%Secant_Sensor_Zenith
    WRITE( *, '( 4x, "Source_Zenith_Radian :", '//TRIM(fmt)//')' ) gInfo%Source_Zenith_Radian
    WRITE( *, '( 4x, "Source_Azimuth_Radian:", '//TRIM(fmt)//')' ) gInfo%Source_Azimuth_Radian
    WRITE( *, '( 4x, "Secant_Source_Zenith :", '//TRIM(fmt)//')' ) gInfo%Secant_Source_Zenith
    WRITE( *, '( 4x, "Flux_Zenith_Radian   :", '//TRIM(fmt)//')' ) gInfo%Flux_Zenith_Radian
    WRITE( *, '( 4x, "Secant_Flux_Zenith   :", '//TRIM(fmt)//',/)' ) gInfo%Secant_Flux_Zenith
  END SUBROUTINE Print_gInfo

END PROGRAM Test_GeometryInfo
