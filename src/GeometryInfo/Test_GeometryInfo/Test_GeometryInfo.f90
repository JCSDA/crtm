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
  USE Type_Kinds              , ONLY: fp
  USE Message_Handler         , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                      Display_Message, Program_Message
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_Compute_GeometryInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_GeometryInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  INTEGER, PARAMETER :: INVALID = -1


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  TYPE(CRTM_GeometryInfo_type) :: gInfo


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test all the CRTM GeometryInfo structure '//&
                        'manipulation functions.', &
                        '$Revision$' )


  ! Load some input data
  ! --------------------
  gInfo%Longitude            = 0.0_fp
  gInfo%Latitude             = 0.0_fp
  gInfo%Surface_Altitude     = 0.0_fp
  gInfo%Sensor_Scan_Angle    = 55.0_fp
  gInfo%Sensor_Zenith_Angle  = 60.0_fp
  gInfo%Sensor_Azimuth_Angle = 0.0_fp
  gInfo%Source_Zenith_Angle  = 71.0_fp
  gInfo%Source_Azimuth_Angle = 0.0_fp
  gInfo%Flux_Zenith_Angle    = 56.3_fp


  ! Fill the rest of GeometryInfo structure
  ! ---------------------------------------
  Error_Status = CRTM_Compute_GeometryInfo( gInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error computing GeometryInfo.', &
                          FAILURE )
    STOP
  END IF

  
  ! Output results
  ! --------------
  WRITE( *, '( /5x, "USER INPUT" )' )
  WRITE( *, '( 10x, "Longitude           :", es13.6 )' ) gInfo%Longitude           
  WRITE( *, '( 10x, "Latitude            :", es13.6 )' ) gInfo%Latitude            
  WRITE( *, '( 10x, "Surface_Altitude    :", es13.6 )' ) gInfo%Surface_Altitude    
  WRITE( *, '( 10x, "Sensor_Scan_Angle   :", es13.6 )' ) gInfo%Sensor_Scan_Angle   
  WRITE( *, '( 10x, "Sensor_Zenith_Angle :", es13.6 )' ) gInfo%Sensor_Zenith_Angle 
  WRITE( *, '( 10x, "Sensor_Azimuth_Angle:", es13.6 )' ) gInfo%Sensor_Azimuth_Angle
  WRITE( *, '( 10x, "Source_Zenith_Angle :", es13.6 )' ) gInfo%Source_Zenith_Angle 
  WRITE( *, '( 10x, "Source_Azimuth_Angle:", es13.6 )' ) gInfo%Source_Azimuth_Angle
  WRITE( *, '( 10x, "Flux_Zenith_Angle   :", es13.6 )' ) gInfo%Flux_Zenith_Angle

  WRITE( *, '( /5x, "DERIVED INPUT" )' )
  WRITE( *, '( 10x, "Distance_Ratio       :", es13.6 )' ) gInfo%Distance_Ratio
  WRITE( *, '( 10x, "Sensor_Scan_Radian   :", es13.6 )' ) gInfo%Sensor_Scan_Radian
  WRITE( *, '( 10x, "Sensor_Zenith_Radian :", es13.6 )' ) gInfo%Sensor_Zenith_Radian
  WRITE( *, '( 10x, "Sensor_Azimuth_Radian:", es13.6 )' ) gInfo%Sensor_Azimuth_Radian
  WRITE( *, '( 10x, "Secant_Sensor_Zenith :", es13.6 )' ) gInfo%Secant_Sensor_Zenith
  WRITE( *, '( 10x, "Source_Zenith_Radian :", es13.6 )' ) gInfo%Source_Zenith_Radian
  WRITE( *, '( 10x, "Source_Azimuth_Radian:", es13.6 )' ) gInfo%Source_Azimuth_Radian
  WRITE( *, '( 10x, "Secant_Source_Zenith :", es13.6 )' ) gInfo%Secant_Source_Zenith
  WRITE( *, '( 10x, "Flux_Zenith_Radian   :", es13.6 )' ) gInfo%Flux_Zenith_Radian
  WRITE( *, '( 10x, "Secant_Flux_Zenith   :", es13.6 )' ) gInfo%Secant_Flux_Zenith

END PROGRAM Test_GeometryInfo
