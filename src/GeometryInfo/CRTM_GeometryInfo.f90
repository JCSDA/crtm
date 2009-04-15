!
! CRTM_GeometryInfo
!
! Application module for the GeometryInfo structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_GeometryInfo

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, WARNING, FAILURE, Display_Message
  USE CRTM_Parameters, ONLY: ZERO, ONE               , &
                             EARTH_RADIUS            , &
                             SATELLITE_HEIGHT        , &
                             DEGREES_TO_RADIANS      , &
                             MAX_SENSOR_ZENITH_ANGLE , &
                             MAX_SENSOR_AZIMUTH_ANGLE, &
                             MAX_SOURCE_AZIMUTH_ANGLE, &
                             MAX_FLUX_ZENITH_ANGLE   , &
                             DIFFUSIVITY_ANGLE       , &
                             DIFFUSIVITY_RADIAN      , &
                             SECANT_DIFFUSIVITY
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type  , &
                                      CRTM_Assign_GeometryInfo, &
                                      CRTM_Equal_GeometryInfo
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! CRTM_GeometryInfo structure data type
  ! in the CRTM_GeometryInfo_Define module
  PUBLIC :: CRTM_GeometryInfo_type
  ! CRTM_GeometryInfo structure routines inherited
  ! from the CRTM_GeometryInfo_Define module
  PUBLIC :: CRTM_Assign_GeometryInfo
  PUBLIC :: CRTM_Equal_GeometryInfo
  ! Public procedures
  PUBLIC :: CRTM_Compute_GeometryInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Compute_GeometryInfo
! 
! PURPOSE:
!       Function to compute the derived geometry from the user specified
!       components of the CRTM GeometryInfo structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo           , &
!                                                 Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       GeometryInfo:  The GeometryInfo structure containing the user
!                      defined inputs, in particular the angles.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!       GeometryInfo:  The GeometryInfo structure with the derived
!                      angle components filled..
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:   Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to the screen.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the computation was sucessful
!                          == WARNING invalid data was found, but altered to default.
!                          == FAILURE invalid data was found
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       This function changes the values of the derived components of the
!       GeometryInfo structure argument.
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_GeometryInfo( gInfo      , &  ! In/Output
                                      Message_Log) &  ! Optional input
                                    RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo
    CHARACTER(*),       OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_GeometryInfo'

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check sensor angles
    IF ( ABS(gInfo%Sensor_Zenith_Angle) > MAX_SENSOR_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor zenith angle', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    IF ( gInfo%Sensor_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Sensor_Azimuth_Angle > MAX_SENSOR_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log=Message_Log )
      gInfo%Sensor_Azimuth_Angle = ZERO
    END IF

    ! Check source angles
    IF ( gInfo%Source_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Source_Azimuth_Angle > MAX_SOURCE_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid source azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log=Message_Log )
      gInfo%Source_Azimuth_Angle = ZERO
    END IF

    ! Check flux angles
    IF ( ABS(gInfo%Flux_Zenith_Angle) > MAX_FLUX_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid flux zenith angle', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Compute the derived components
    ! ------------------------------    
    ! Sensor angles
    gInfo%Sensor_Scan_Radian    = DEGREES_TO_RADIANS * gInfo%Sensor_Scan_Angle
    gInfo%Sensor_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Sensor_Zenith_Angle
    gInfo%Sensor_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Sensor_Azimuth_Angle
    gInfo%Secant_Sensor_Zenith  = ONE / COS(gInfo%Sensor_Zenith_Radian)
    
    ! Distance ratio. Only modify if zenith angle large enough.
    IF ( ABS(gInfo%Sensor_Zenith_Angle) > ONE ) THEN
      gInfo%Distance_Ratio = ABS(SIN(gInfo%Sensor_Scan_Radian)/SIN(gInfo%Sensor_Zenith_Radian))
    END IF

    ! Source angles
    gInfo%Source_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Source_Zenith_Angle
    gInfo%Source_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Source_Azimuth_Angle
    gInfo%Secant_Source_Zenith  = ONE / COS(gInfo%Source_Zenith_Radian)

    ! Flux angles
    gInfo%Flux_Zenith_Radian = DEGREES_TO_RADIANS * gInfo%Flux_Zenith_Angle
    gInfo%Secant_Flux_Zenith = ONE / COS(gInfo%Flux_Zenith_Radian)

  END FUNCTION CRTM_Compute_GeometryInfo

END MODULE CRTM_GeometryInfo
