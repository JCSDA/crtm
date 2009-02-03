!
! CRTM_GeometryInfo_Define
!
! Module defining the CRTM GeometryInfo data structure.
!
!
! DERIVED TYPES:
!       CRTM_GeometryInfo_type
!       ----------------------
!       Definition of the public CRTM GeometryInfo data structure.
!       Fields are:
!
!         USER INPUTS: It is expected the user will fill these components
!         -----------  of the GeometryInfo structure
!
!
!         Longitude:             Earth longitude.
!                                UNITS:      degrees East (0->360)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Latitude:              Earth latitude.
!                                UNITS:      degrees North (-90->+90)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Surface_Altitude:      Altitude of the Earth's surface at the specified
!                                lon/lat location.
!                                UNITS:      metres (m)
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
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
!                                TYPE:       REAL(fp)
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
!                                TYPE:       REAL(fp)
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
!                                TYPE:       REAL(fp)
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
!                                TYPE:       REAL(fp)
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
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Angle:     The zenith angle, F, used to approximate downwelling
!                                flux transmissivity. If not set, the default value
!                                is that of the diffusivity approximation, such that
!                                  sec(F) = 5/3
!                                Maximum allowed value of F is determined from
!                                  sec(F) = 9/4
!                                UNITS:      Degrees
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!
!         DERIVED COMPONENTS: These components will be derived from values from the
!         ------------------  user input components via CRTM_Compute_GeometryInfo.
!
!         Distance_Ratio:        The ratio of the radius of the earth at the
!                                FOV location, Re(FOV), to the sum of the radius
!                                of the earth at nadir, Re(nadir), and the satellite
!                                altitude, h:
!                                                       Re(FOV)
!                                  Distance_Ratio = ---------------
!                                                    Re(nadir) + h
!
!                                Note that this quantity is actually computed
!                                using the user input sensor scan and zenith
!                                angles:
!                                                     SIN(scan angle)
!                                  Distance_Ratio = -------------------
!                                                    SIN(zenith angle)
!
!                                for |SIN(zenith angle)| > 1.0.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Sensor_Scan_Radian:    The sensor scan angle, S, in radians. This
!                                value is derived from the Sensor_Scan_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Sensor_Zenith_Radian:  The sensor zenith angle, Z, in radians. This
!                                value is derived from the Sensor_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Sensor_Azimuth_Radian: The sensor azimuth angle, A, in radians. This
!                                value is derived from the Sensor_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Secant_Sensor_Zenith:  The secant of the sensor zenith
!                                angle, sec(Z). The value is derived
!                                from the Sensor_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Source_Zenith_Radian:  The Source zenith angle, ZS, in radians. This
!                                value is derived from the Source_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Source_Azimuth_Radian: The Source azimuth angle, AS, in radians. This
!                                value is derived from the Source_Azimuth_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Secant_Source_Zenith:  The secant of the source zenith
!                                angle, sec(ZS). The value is derived
!                                from the Source_Zenith_Angle component.
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Flux_Zenith_Radian:    The flux zenith angle, F, in radians. This
!                                value is derived from the Flux_Zenith_Angle
!                                component.
!                                UNITS:      Radians
!                                TYPE:       REAL(fp)
!                                DIMENSION:  Scalar
!
!         Secant_Flux_Zenith:    The secant of the flux zenith angle, sec(F).
!                                This value is derived from the Flux_Zenith_Angle
!                                component and is used to approximate the
!                                downwelling flux transmissivity (diffusivity
!                                approximation).
!                                UNITS:      N/A
!                                TYPE:       REAL(fp)
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
  USE Type_Kinds     , ONLY: fp
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
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_GeometryInfo_type
  PUBLIC :: CRTM_Compute_GeometryInfo
  PUBLIC :: CRTM_RCS_Id_GeometryInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! ---------------------------------
  ! GeometryInfo data type definition
  ! ---------------------------------
  !:tdoc+:
  TYPE :: CRTM_GeometryInfo_type
    ! User Input
    ! ----------
    ! Earth location
    REAL(fp) :: Longitude        = ZERO
    REAL(fp) :: Latitude         = ZERO
    REAL(fp) :: Surface_Altitude = ZERO
    ! Field of view index (1-nFOV)
    INTEGER  :: iFOV = 0
    ! Sensor angle information
    REAL(fp) :: Sensor_Scan_Angle    = ZERO
    REAL(fp) :: Sensor_Zenith_Angle  = ZERO
    REAL(fp) :: Sensor_Azimuth_Angle = ZERO 
    ! Source angle information
    REAL(fp) :: Source_Zenith_Angle  = 100.0_fp  ! Below horizon
    REAL(fp) :: Source_Azimuth_Angle = ZERO
    ! Flux angle information
    REAL(fp) :: Flux_Zenith_Angle = DIFFUSIVITY_ANGLE
    ! Derived from User Input
    ! -----------------------
    ! Default distance ratio
    REAL(fp) :: Distance_Ratio = EARTH_RADIUS/(EARTH_RADIUS + SATELLITE_HEIGHT)
    ! Sensor angle information
    REAL(fp) :: Sensor_Scan_Radian    = ZERO
    REAL(fp) :: Sensor_Zenith_Radian  = ZERO
    REAL(fp) :: Sensor_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Sensor_Zenith  = ZERO
    ! Source angle information
    REAL(fp) :: Source_Zenith_Radian  = ZERO
    REAL(fp) :: Source_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Source_Zenith  = ZERO
    ! Flux angle information
    REAL(fp) :: Flux_Zenith_Radian = DIFFUSIVITY_RADIAN
    REAL(fp) :: Secant_Flux_Zenith = SECANT_DIFFUSIVITY
  END TYPE CRTM_GeometryInfo_type
  !:tdoc-:


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


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_RCS_ID_GeometryInfo
!
! PURPOSE:
!       Subroutine to return the module RCS Id information.
!
! CALLING SEQUENCE:
!       CALL CRTM_RCS_Id_GeometryInfo( RCS_Id )
!
! OUTPUT ARGUMENTS:
!       RCS_Id:        Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_RCS_ID_GeometryInfo( RCS_Id )
    CHARACTER(*), INTENT(OUT) :: RCS_Id
    RCS_Id = MODULE_RCS_ID
  END SUBROUTINE CRTM_RCS_ID_GeometryInfo


END MODULE CRTM_GeometryInfo_Define
