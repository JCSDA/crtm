!
! CRTM_GeometryInfo_Define
!
! Module defining the CRTM GeometryInfo data structure.
!
!
! DERIVED TYPES:
!       CRTM_GeometryInfo_type
!       ----------------------
!       Definition of the public CRTM_GeometryInfo data structure.
!       Fields are:
!
!         USER INPUTS: It is expected the user will fill these components
!         -----------  of the GeometryInfo structure
!
!
!         Earth_Radius:          Radius of the Earth at the current location.
!                                UNITS:      kilometres (km)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Satellite_Height:      Height of the satellite above the Earth's 
!                                surface at the current location.
!                                UNITS:      kilometres (km)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Longitude:             Earth longitude.
!                                UNITS:      degrees East (0->360)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Latitude:              Earth latitude.
!                                UNITS:      degrees North (-90->+90)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Surface_Altitude:      Altitude of the Earth's surface at the specified
!                                lon/lat location.
!                                UNITS:      metres (m)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Sensor_Zenith_Angle:   The sensor zenith angle, Z. If a flat Earth
!                                is assumed, then the Sensor_Scan_Angle and
!                                Sensor_Zenith_Angle are the same.
!                                              | 
!                                   Zenith   -0A0-  <--Satellite
!                                      |      /
!                                      |     /
!                                      |    /
!                                      |   /
!                                      |  /
!                                      |Z/
!                                      |/
!                                   -------------
!                                      ^
!                                      FOV
!                                UNITS:      Degrees.
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Sensor_Azimuth_Angle:  The sensor azimuth angle, A, is the angle
!                                subtended by the horizontal projection of
!                                a direct line from the satellite to the FOV
!                                and the North-South axis measured clockwise
!                                from North.
!                                         North     O  <-- Sub-satellite point
!                                           |      /
!                                           |     /
!                                           |    /
!                                           |   /
!                                           |  /
!                                           |A/
!                                           |/
!                                  West-----O-----East
!                                           ^
!                                          FOV
!                                UNITS:      Degrees form North (0->360)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Source_Zenith_Angle:   The source zenith angle, ZS. The source is
!                                typically the Sun (IR/VIS) or Moon (MW/VIS).
!                                   Zenith     X  <--Source
!                                      |      /
!                                      |     /
!                                      |    /
!                                      |   /
!                                      |ZS/
!                                      | /
!                                      |/
!                                   -------------
!                                      ^
!                                      FOV
!                                UNITS:      Degrees (-180 -> +180)
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Source_Azimuth_Angle:  The source azimuth angle, AS, is the angle
!                                subtended by the horizontal projection of
!                                a direct line from the source to the FOV
!                                and the North-South axis measured clockwise
!                                from North.
!                                         North     
!                                           |      / <-- horizontal projection
!                                           |     /      of direct line from 
!                                           |    /       source 
!                                           |   /
!                                           |AS/
!                                           | /
!                                           |/
!                                  West-----O-----East
!                                           ^
!                                          FOV
!                                UNITS:      Degrees from North (0->360).
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Angle:     The zenith angle, F, used to approximate downwelling
!                                flux transmissivity. If not set, the default value
!                                is that of the diffusivity approximation, such that
!                                  sec(F) = 5/3
!                                Maximum allowed value of F is determined from
!                                  sec(F) = 9/4
!                                UNITS:      Degrees
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!
!         DERIVED COMPONENTS: These components will be derived from values from the
!         ------------------  user input components via CRTM_Compute_GeometryInfo.
!
!         Sensor_Scan_Angle:     The sensor scan angle, S, from nadir.
!                                      |         
!                                    -0A0-  <--Satellite     
!                                      |\        
!                                      |S\       
!                                      |  \      
!                                      |   \     
!                                      |    \    
!                                      |     \   
!                                      |      \  
!                                    ------------
!                                      ^       ^ 
!                                     Nadir   FOV
!                                UNITS:      Degrees
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Sensor_Scan_Radian:    The sensor scan angle, S, in radians. This
!                                value is derived from the Sensor_Scan_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Sensor_Zenith_Radian:  The sensor zenith angle, Z, in radians. This
!                                value is derived from the Sensor_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Sensor_Azimuth_Radian: The sensor azimuth angle, A, in radians. This
!                                value is derived from the Sensor_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Secant_Sensor_Zenith:  The secant of the sensor zenith
!                                angle, sec(Z). The value is derived
!                                from the Sensor_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Source_Zenith_Radian:  The Source zenith angle, ZS, in radians. This
!                                value is derived from the Source_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Source_Azimuth_Radian: The Source azimuth angle, AS, in radians. This
!                                value is derived from the Source_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Secant_Source_Zenith:  The secant of the source zenith
!                                angle, sec(ZS). The value is derived
!                                from the Source_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Radian:    The flux zenith angle, F, in radians. This
!                                value is derived from the Flux_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!         Secant_Flux_Zenith:    The secant of the flux zenith angle, sec(F).
!                                This value is derived from the Flux_Zenith_Angle
!                                component and is used to approximate the
!                                downwelling flux transmissivity (diffusivity
!                                approximation).
!                                UNITS:      N/A
!                                TYPE:       REAL(fp_kind)
!                                DIMENSION:  Scalar
!
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 19-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_GeometryInfo_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds
  USE Message_Handler
  USE CRTM_Parameters
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_GeometryInfo_type
  PUBLIC :: CRTM_Compute_GeometryInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_GeometryInfo_Define.f90,v 1.12 2006/05/02 14:58:34 dgroff Exp $'


  ! ---------------------------------
  ! GeometryInfo data type definition
  ! ---------------------------------

  TYPE :: CRTM_GeometryInfo_type

    ! User Input
    ! ----------
    ! Earth radius and satellite height
    REAL(fp_kind) :: Earth_Radius     = EARTH_RADIUS
    REAL(fp_kind) :: Satellite_Height = SATELLITE_HEIGHT
    REAL(fp_kind) :: Distance_Ratio   = EARTH_RADIUS / (EARTH_RADIUS + SATELLITE_HEIGHT)
    ! Earth location
    REAL(fp_kind) :: Longitude        = ZERO
    REAL(fp_kind) :: Latitude         = ZERO
    REAL(fp_kind) :: Surface_Altitude = ZERO
    ! Sensor angle information
    REAL(fp_kind) :: Sensor_Zenith_Angle  = ZERO
    REAL(fp_kind) :: Sensor_Azimuth_Angle = ZERO 
    ! Source angle information
    REAL(fp_kind) :: Source_Zenith_Angle  = ZERO
    REAL(fp_kind) :: Source_Azimuth_Angle = ZERO
    ! Flux angle information
    REAL(fp_kind) :: Flux_Zenith_Angle = DIFFUSIVITY_ANGLE

    ! Derived from User Input
    ! -----------------------
    ! Sensor angle information
    REAL(fp_kind) :: Sensor_Scan_Angle     = ZERO
    REAL(fp_kind) :: Sensor_Scan_Radian    = ZERO
    REAL(fp_kind) :: Sensor_Zenith_Radian  = ZERO
    REAL(fp_kind) :: Sensor_Azimuth_Radian = ZERO
    REAL(fp_kind) :: Secant_Sensor_Zenith  = ZERO
    ! Source angle information
    REAL(fp_kind) :: Source_Zenith_Radian  = ZERO
    REAL(fp_kind) :: Source_Azimuth_Radian = ZERO
    REAL(fp_kind) :: Secant_Source_Zenith  = ZERO
    ! Flux angle information
    REAL(fp_kind) :: Flux_Zenith_Radian = DIFFUSIVITY_RADIAN
    REAL(fp_kind) :: Secant_Flux_Zenith = SECANT_DIFFUSIVITY

  END TYPE CRTM_GeometryInfo_type


CONTAINS


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_GeometryInfo
! 
! PURPOSE:
!       Function to compute the derived geometry from the user specified
!       components of the CRTM_GeometryInfo structure.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_GeometryInfo( GeometryInfo,             &  ! In/Output
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       GeometryInfo:  The CRTM_GeometryInfo structure containing the user
!                      defined inputs, in particular the angles.
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
!
! OUTPUT ARGUMENTS:
!       GeometryInfo:  The CRTM_GeometryInfo structure with the derived
!                      angle components filled..
!                      UNITS:      N/A
!                      TYPE:       CRTM_GeometryInfo_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN OUT )
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
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_GeometryInfo( gInfo      , &  ! In/Output
                                      Message_Log) &  ! Optional input
                                    RESULT ( Error_Status )
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

    ! Check the sensor angles
    IF ( ABS( gInfo%Sensor_Zenith_Angle ) > MAX_SENSOR_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor zenith angle', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( gInfo%Sensor_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Sensor_Azimuth_Angle > MAX_SENSOR_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid sensor azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      gInfo%Sensor_Azimuth_Angle = ZERO
    END IF

    ! Check the Source angles
    IF ( gInfo%Source_Azimuth_Angle < ZERO                     .OR. &
         gInfo%Source_Azimuth_Angle > MAX_SOURCE_AZIMUTH_ANGLE      ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid source azimuth angle. Setting to 0.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      gInfo%Source_Azimuth_Angle = ZERO
    END IF

    ! Check the Flux angles
    IF ( ABS( gInfo%Flux_Zenith_Angle ) > MAX_FLUX_ZENITH_ANGLE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid flux zenith angle', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Perform the angle conversions
    ! -----------------------------
    ! Sensor angles
    gInfo%Sensor_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Sensor_Zenith_Angle
    gInfo%Sensor_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Sensor_Azimuth_Angle
    gInfo%Secant_Sensor_Zenith  = ONE / COS( gInfo%Sensor_Zenith_Radian )

    ! Source angles
    gInfo%Source_Zenith_Radian  = DEGREES_TO_RADIANS * gInfo%Source_Zenith_Angle
    gInfo%Source_Azimuth_Radian = DEGREES_TO_RADIANS * gInfo%Source_Azimuth_Angle
    gInfo%Secant_Source_Zenith  = ONE / COS( gInfo%Source_Zenith_Radian )

    ! Flux angles
    gInfo%Flux_Zenith_Radian = DEGREES_TO_RADIANS * gInfo%Flux_Zenith_Angle
    gInfo%Secant_Flux_Zenith = ONE / COS( gInfo%Flux_Zenith_Radian )

  END FUNCTION CRTM_Compute_GeometryInfo

END MODULE CRTM_GeometryInfo_Define
