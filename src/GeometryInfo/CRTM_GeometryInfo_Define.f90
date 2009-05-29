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
  USE Type_Kinds,            ONLY: fp
  USE Message_Handler,       ONLY: SUCCESS, WARNING, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters,       ONLY: ZERO, ONE, SET          , &
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
  ! Structure data type
  PUBLIC :: CRTM_GeometryInfo_type
  ! Public procedures
  PUBLIC :: CRTM_Assign_GeometryInfo
  PUBLIC :: CRTM_Equal_GeometryInfo
  PUBLIC :: CRTM_RCS_ID_GeometryInfo


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Assign_GeometryInfo
    MODULE PROCEDURE Assign_Scalar
    MODULE PROCEDURE Assign_Rank1
  END INTERFACE CRTM_Assign_GeometryInfo

  INTERFACE CRTM_Equal_GeometryInfo
    MODULE PROCEDURE Equal_Scalar
    MODULE PROCEDURE Equal_Rank1
  END INTERFACE CRTM_Equal_GeometryInfo
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Maximum message length
  INTEGER, PARAMETER :: ML=256

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
    !-------------------------------------------------------------
    ! Parameters used for ZSSMIS (SSMIS Zeeman channels 19 - 22)
    !-------------------------------------------------------------
    ! Earth magnetic field strength in Gauss
    REAL(fp) :: Be = 0.3_fp  ! initialized to the default value - low end value 
    ! Cosine of the angle between the Earth magnetic field and wave 
    ! propagation direction                     
    REAL(fp) :: CosBK = ZERO
    ! Doppler frequency shift caused by Earth-rotation in KHz (possivity towards sensor)
    ! A zero value means no frequency shift.
    REAL(fp) :: Doppler_Shift = ZERO 

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


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Assign_GeometryInfo
!
! PURPOSE:
!       Function to copy valid CRTM GeometryInfo structures.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Assign_GeometryInfo( gInfo_in          , &
!                                                gInfo_out         , &
!                                              Message_Log=Message_Log  )
!
! INPUT ARGUMENTS:
!       gInfo_in:   GeometryInfo structure which is to be copied.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Scalar, Rank-1, or Rank-2
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       gInfo_out:  Copy of the input structure, gInfo_in.
!                        UNITS:      N/A
!                        TYPE:       CRTM_GeometryInfo_type
!                        DIMENSION:  Same as input gInfo_in
!                        ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the structure assignment was successful
!                           == FAILURE an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Assign_Scalar( &
    gInfo_in   , &  ! Input
    gInfo_out  , &  ! Output
    Message_Log) &  ! Error messaging
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: gInfo_in
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo_out
    CHARACTER(*),       OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_GeometryInfo(Scalar)'
    ! Local variables
    LOGICAL :: dummy

    ! Set up
    Error_Status = SUCCESS
    dummy = PRESENT(Message_Log)


    ! Assign scalar data
    ! ...User input data
    gInfo_out%Longitude            = gInfo_in%Longitude           
    gInfo_out%Latitude             = gInfo_in%Latitude            
    gInfo_out%Surface_Altitude     = gInfo_in%Surface_Altitude    
    gInfo_out%iFOV                 = gInfo_in%iFOV                
    gInfo_out%Sensor_Scan_Angle    = gInfo_in%Sensor_Scan_Angle   
    gInfo_out%Sensor_Zenith_Angle  = gInfo_in%Sensor_Zenith_Angle 
    gInfo_out%Sensor_Azimuth_Angle = gInfo_in%Sensor_Azimuth_Angle
    gInfo_out%Source_Zenith_Angle  = gInfo_in%Source_Zenith_Angle 
    gInfo_out%Source_Azimuth_Angle = gInfo_in%Source_Azimuth_Angle
    gInfo_out%Flux_Zenith_Angle    = gInfo_in%Flux_Zenith_Angle   

    ! ...Derived data
    gInfo_out%Distance_Ratio        = gInfo_in%Distance_Ratio       
    gInfo_out%Sensor_Scan_Radian    = gInfo_in%Sensor_Scan_Radian   
    gInfo_out%Sensor_Zenith_Radian  = gInfo_in%Sensor_Zenith_Radian 
    gInfo_out%Sensor_Azimuth_Radian = gInfo_in%Sensor_Azimuth_Radian
    gInfo_out%Secant_Sensor_Zenith  = gInfo_in%Secant_Sensor_Zenith 
    gInfo_out%Source_Zenith_Radian  = gInfo_in%Source_Zenith_Radian 
    gInfo_out%Source_Azimuth_Radian = gInfo_in%Source_Azimuth_Radian
    gInfo_out%Secant_Source_Zenith  = gInfo_in%Secant_Source_Zenith 
    gInfo_out%Flux_Zenith_Radian    = gInfo_in%Flux_Zenith_Radian   
    gInfo_out%Secant_Flux_Zenith    = gInfo_in%Secant_Flux_Zenith   
    
  END FUNCTION Assign_Scalar


  FUNCTION Assign_Rank1( &
    gInfo_in   , &  ! Input
    gInfo_out  , &  ! Output
    Message_Log) &  ! Error messaging
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: gInfo_in(:)
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo_out(:)
    CHARACTER(*),       OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Assign_GeometryInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: Scalar_Status
    INTEGER :: i, n


    ! Set up
    Error_Status = SUCCESS

    ! ...Check dimensions
    n = SIZE( gInfo_in )
    IF ( SIZE( gInfo_out ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input gInfo_in and gInfo_out arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Perform the asignment
    DO i = 1, n
      Scalar_Status = Assign_Scalar( gInfo_in(i), &
                                     gInfo_out(i), &
                                     Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( msg, '("Error copying element #",i0, &
                     &" of rank-1 GeometryInfo structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(msg), &
                              Error_Status, &
                              Message_Log=Message_Log )
      END IF
    END DO
  END FUNCTION Assign_Rank1


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Equal_GeometryInfo
!
! PURPOSE:
!       Function to test if two GeometryInfo structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Equal_GeometryInfo( gInfo_LHS, &
!                                               gInfo_RHS, &
!                                               ULP_Scale = ULP_Scale, &
!                                               Check_All = Check_All, &
!                                               Message_Log = Message_Log )         
!
!
! INPUT ARGUMENTS:
!       gInfo_LHS:     GeometryInfo structure to be compared; equivalent to the
!                           left-hand side of a lexical comparison, e.g.
!                             IF ( gInfo_LHS == gInfo_RHS ).
!                           UNITS:      N/A
!                           TYPE:       CRTM_GeometryInfo_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       gInfo_RHS:     GeometryInfo structure to be compared to; equivalent to
!                           right-hand side of a lexical comparison, e.g.
!                             IF ( gInfo_LHS == gInfo_RHS ).
!                           UNITS:      N/A
!                           TYPE:       CRTM_GeometryInfo_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:          Unit of data precision used to scale the floating
!                           point comparison. ULP stands for "Unit in the Last Place,"
!                           the smallest possible increment or decrement that can be
!                           made using a machine's floating point arithmetic.
!                           Value must be positive - if a negative value is supplied,
!                           the absolute value is used. If not specified, the default
!                           value is 1.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:          Set this argument to check ALL the data of the
!                           GeometryInfo structures. The default action is to
!                           return with a FAILURE status as soon as any difference
!                           is found. This optional argument can be used to get a
!                           listing of ALL the differences between data in
!                           GeometryInfo structures.
!                           If == 0, Return with FAILURE status as soon as
!                                    ANY difference is found  *DEFAULT*
!                              == 1, Set FAILURE status if ANY difference is
!                                    found, but continue to check ALL data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the structures were equal
!                              == FAILURE - an error occurred, or
!                                         - the structures were different.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_Scalar( &
    gInfo_LHS  , &  ! Input
    gInfo_RHS  , &  ! Input
    ULP_Scale  , &  ! Optional input
    Check_All  , &  ! Optional input
    Message_Log) &  ! Error messaging
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: gInfo_LHS
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: gInfo_RHS
    INTEGER,            OPTIONAL, INTENT(IN)  :: ULP_Scale
    INTEGER,            OPTIONAL, INTENT(IN)  :: Check_All
    CHARACTER(*),       OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_GeometryInfo(scalar)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Check_Once

    ! Set up
    Error_Status = SUCCESS

    ! ...Default action is to return on ANY difference
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF


    ! Perform comparisons
    ! ...User input data
    IF ( .NOT. Compare_Float( gInfo_LHS%Longitude, &
                              gInfo_RHS%Longitude, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Longitude values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Longitude, &
             gInfo_RHS%Longitude, &
             gInfo_LHS%Longitude-gInfo_RHS%Longitude
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Latitude, &
                              gInfo_RHS%Latitude, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Latitude values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Latitude, &
             gInfo_RHS%Latitude, &
             gInfo_LHS%Latitude-gInfo_RHS%Latitude
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Surface_Altitude, &
                              gInfo_RHS%Surface_Altitude, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Surface_Altitude values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Surface_Altitude, &
             gInfo_RHS%Surface_Altitude, &
             gInfo_LHS%Surface_Altitude-gInfo_RHS%Surface_Altitude
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( gInfo_LHS%iFOV /= gInfo_RHS%iFOV ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("iFOV values are different:",3(1x,i0))' ) &
             gInfo_LHS%iFOV, &
             gInfo_RHS%iFOV, &
             gInfo_LHS%iFOV-gInfo_RHS%iFOV
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Scan_Angle, &
                              gInfo_RHS%Sensor_Scan_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Scan_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Scan_Angle, &
             gInfo_RHS%Sensor_Scan_Angle, &
             gInfo_LHS%Sensor_Scan_Angle-gInfo_RHS%Sensor_Scan_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Zenith_Angle, &
                              gInfo_RHS%Sensor_Zenith_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Zenith_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Zenith_Angle, &
             gInfo_RHS%Sensor_Zenith_Angle, &
             gInfo_LHS%Sensor_Zenith_Angle-gInfo_RHS%Sensor_Zenith_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Azimuth_Angle, &
                              gInfo_RHS%Sensor_Azimuth_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Azimuth_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Azimuth_Angle, &
             gInfo_RHS%Sensor_Azimuth_Angle, &
             gInfo_LHS%Sensor_Azimuth_Angle-gInfo_RHS%Sensor_Azimuth_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Source_Zenith_Angle, &
                              gInfo_RHS%Source_Zenith_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Source_Zenith_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Source_Zenith_Angle, &
             gInfo_RHS%Source_Zenith_Angle, &
             gInfo_LHS%Source_Zenith_Angle-gInfo_RHS%Source_Zenith_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Source_Azimuth_Angle, &
                              gInfo_RHS%Source_Azimuth_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Source_Azimuth_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Source_Azimuth_Angle, &
             gInfo_RHS%Source_Azimuth_Angle, &
             gInfo_LHS%Source_Azimuth_Angle-gInfo_RHS%Source_Azimuth_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Flux_Zenith_Angle, &
                              gInfo_RHS%Flux_Zenith_Angle, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Flux_Zenith_Angle values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Flux_Zenith_Angle, &
             gInfo_RHS%Flux_Zenith_Angle, &
             gInfo_LHS%Flux_Zenith_Angle-gInfo_RHS%Flux_Zenith_Angle
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

    ! ...Derived data
    IF ( .NOT. Compare_Float( gInfo_LHS%Distance_Ratio, &
                              gInfo_RHS%Distance_Ratio, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Distance_Ratio values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Distance_Ratio, &
             gInfo_RHS%Distance_Ratio, &
             gInfo_LHS%Distance_Ratio-gInfo_RHS%Distance_Ratio
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Scan_Radian, &
                              gInfo_RHS%Sensor_Scan_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Scan_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Scan_Radian, &
             gInfo_RHS%Sensor_Scan_Radian, &
             gInfo_LHS%Sensor_Scan_Radian-gInfo_RHS%Sensor_Scan_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Zenith_Radian, &
                              gInfo_RHS%Sensor_Zenith_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Zenith_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Zenith_Radian, &
             gInfo_RHS%Sensor_Zenith_Radian, &
             gInfo_LHS%Sensor_Zenith_Radian-gInfo_RHS%Sensor_Zenith_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Sensor_Azimuth_Radian, &
                              gInfo_RHS%Sensor_Azimuth_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Sensor_Azimuth_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Sensor_Azimuth_Radian, &
             gInfo_RHS%Sensor_Azimuth_Radian, &
             gInfo_LHS%Sensor_Azimuth_Radian-gInfo_RHS%Sensor_Azimuth_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Secant_Sensor_Zenith, &
                              gInfo_RHS%Secant_Sensor_Zenith, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Secant_Sensor_Zenith values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Secant_Sensor_Zenith, &
             gInfo_RHS%Secant_Sensor_Zenith, &
             gInfo_LHS%Secant_Sensor_Zenith-gInfo_RHS%Secant_Sensor_Zenith
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Source_Zenith_Radian, &
                              gInfo_RHS%Source_Zenith_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Source_Zenith_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Source_Zenith_Radian, &
             gInfo_RHS%Source_Zenith_Radian, &
             gInfo_LHS%Source_Zenith_Radian-gInfo_RHS%Source_Zenith_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Source_Azimuth_Radian, &
                              gInfo_RHS%Source_Azimuth_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Source_Azimuth_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Source_Azimuth_Radian, &
             gInfo_RHS%Source_Azimuth_Radian, &
             gInfo_LHS%Source_Azimuth_Radian-gInfo_RHS%Source_Azimuth_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Secant_Source_Zenith, &
                              gInfo_RHS%Secant_Source_Zenith, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Secant_Source_Zenith values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Secant_Source_Zenith, &
             gInfo_RHS%Secant_Source_Zenith, &
             gInfo_LHS%Secant_Source_Zenith-gInfo_RHS%Secant_Source_Zenith
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Flux_Zenith_Radian, &
                              gInfo_RHS%Flux_Zenith_Radian, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Flux_Zenith_Radian values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Flux_Zenith_Radian, &
             gInfo_RHS%Flux_Zenith_Radian, &
             gInfo_LHS%Flux_Zenith_Radian-gInfo_RHS%Flux_Zenith_Radian
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF
    IF ( .NOT. Compare_Float( gInfo_LHS%Secant_Flux_Zenith, &
                              gInfo_RHS%Secant_Flux_Zenith, &
                              ULP = ULP_Scale ) ) THEN
      Error_Status = FAILURE
      WRITE( msg,'("Secant_Flux_Zenith values are different:",3(1x,es13.6))' ) &
             gInfo_LHS%Secant_Flux_Zenith, &
             gInfo_RHS%Secant_Flux_Zenith, &
             gInfo_LHS%Secant_Flux_Zenith-gInfo_RHS%Secant_Flux_Zenith
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
      IF ( Check_Once ) RETURN
    END IF

  END FUNCTION Equal_Scalar


  FUNCTION Equal_Rank1( &
    gInfo_LHS    , &  ! Input
    gInfo_RHS    , &  ! Input
    ULP_Scale         , &  ! Optional input
    Check_All         , &  ! Optional input
    Message_Log       ) &  ! Error messaging
  RESULT( Error_Status )
    ! Arguments
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gInfo_LHS(:)
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: gInfo_RHS(:)
    INTEGER,            OPTIONAL, INTENT(IN) :: ULP_Scale
    INTEGER,            OPTIONAL, INTENT(IN) :: Check_All
    CHARACTER(*),       OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Equal_GeometryInfo(Rank-1)'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Check_Once
    INTEGER :: Scalar_Status
    INTEGER :: i, n

    ! Set up
    Error_Status = SUCCESS

    ! ...Default action is to return on ANY difference...
    Check_Once = .TRUE.
    ! ...unless the Check_All argument is set
    IF ( PRESENT( Check_All ) ) THEN
      IF ( Check_All == SET ) Check_Once = .FALSE.
    END IF

    ! ...Check dimensions
    n = SIZE( gInfo_LHS )
    IF ( SIZE( gInfo_RHS ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input GeometryInfo_LHS and GeometryInfo_RHS arrays'//&
                            ' have different sizes', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Test for equality
    DO i = 1, n
      Scalar_Status = Equal_Scalar( gInfo_LHS(i), &
                                    gInfo_RHS(i), &
                                    ULP_Scale  =ULP_Scale, &
                                    Check_All  =Check_All, &
                                    Message_Log=Message_Log )
      IF ( Scalar_Status /= SUCCESS ) THEN
        Error_Status = Scalar_Status
        WRITE( msg,'("Error comparing element (",i0,")", &
                    &" of rank-1 GeometryInfo structure array." )' ) i
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(msg), &
                              Error_Status, &
                              Message_Log=Message_Log )
        IF ( Check_Once ) RETURN
      END IF
    END DO
  END FUNCTION Equal_Rank1


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


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Clear_GeometryInfo
!
! PURPOSE:
!       Subroutine to reset the scalar members of a CRTM GeometryInfo
!       structure to their default values.
!
! CALLING SEQUENCE:
!       CALL CRTM_Clear_GeometryInfo( gInfo )
!
! OUTPUT ARGUMENTS:
!       gInfo:       GeometryInfo structure for which the scalar members have
!                    have been reinitialised.
!                    UNITS:      N/A
!                    TYPE:       CRTM_GeometryInfo_type
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN OUT)
!
!----------------------------------------------------------------------------------

  SUBROUTINE CRTM_Clear_GeometryInfo( gInfo )
    TYPE(CRTM_GeometryInfo_type), INTENT(IN OUT) :: gInfo
    TYPE(CRTM_GeometryInfo_type) :: dummy

    ! User input data
    gInfo%Longitude            = dummy%Longitude           
    gInfo%Latitude             = dummy%Latitude            
    gInfo%Surface_Altitude     = dummy%Surface_Altitude    
    gInfo%iFOV                 = dummy%iFOV                
    gInfo%Sensor_Scan_Angle    = dummy%Sensor_Scan_Angle   
    gInfo%Sensor_Zenith_Angle  = dummy%Sensor_Zenith_Angle 
    gInfo%Sensor_Azimuth_Angle = dummy%Sensor_Azimuth_Angle
    gInfo%Source_Zenith_Angle  = dummy%Source_Zenith_Angle 
    gInfo%Source_Azimuth_Angle = dummy%Source_Azimuth_Angle
    gInfo%Flux_Zenith_Angle    = dummy%Flux_Zenith_Angle   

    ! Derived data
    gInfo%Distance_Ratio        = dummy%Distance_Ratio       
    gInfo%Sensor_Scan_Radian    = dummy%Sensor_Scan_Radian   
    gInfo%Sensor_Zenith_Radian  = dummy%Sensor_Zenith_Radian 
    gInfo%Sensor_Azimuth_Radian = dummy%Sensor_Azimuth_Radian
    gInfo%Secant_Sensor_Zenith  = dummy%Secant_Sensor_Zenith 
    gInfo%Source_Zenith_Radian  = dummy%Source_Zenith_Radian 
    gInfo%Source_Azimuth_Radian = dummy%Source_Azimuth_Radian
    gInfo%Secant_Source_Zenith  = dummy%Secant_Source_Zenith 
    gInfo%Flux_Zenith_Radian    = dummy%Flux_Zenith_Radian   
    gInfo%Secant_Flux_Zenith    = dummy%Secant_Flux_Zenith   

  END SUBROUTINE CRTM_Clear_GeometryInfo

END MODULE CRTM_GeometryInfo_Define
