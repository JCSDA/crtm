!
! CRTM_IR_Water_SfcOptics
!
! Module to compute the surface optical properties for WATER surfaces at
! infrared frequencies required for determining the WATER surface
! contribution to the radiative transfer.
!
! This module is provided to allow developers to "wrap" their existing
! codes inside the provided functions to simplify integration into
! the main CRTM_SfcOptics module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_IR_Water_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,               ONLY: fp=>fp_kind
  USE Message_Handler,          ONLY: SUCCESS, Display_Message
  USE CRTM_Parameters,          ONLY: ZERO, ONE, MAX_N_ANGLES
  USE CRTM_SpcCoeff,            ONLY: SC
  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,    ONLY: CRTM_SfcOptics_type
  USE CRTM_IRSSEM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  PUBLIC :: IRWSOVariables_type
  ! Science routines
  PUBLIC :: Compute_IR_Water_SfcOptics
  PUBLIC :: Compute_IR_Water_SfcOptics_TL
  PUBLIC :: Compute_IR_Water_SfcOptics_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_IR_Water_SfcOptics.f90,v 1.6.2.1 2006/09/07 09:54:32 frpv Exp $'


  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  TYPE :: IRWSOVariables_type
    PRIVATE
    INTEGER :: Dummy = 0
  END TYPE IRWSOVariables_type


CONTAINS



!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Water_SfcOptics
!
! PURPOSE:
!       Function to compute the surface emissivity and reflectivity at infrared
!       frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics( Surface               , &  ! Input
!                                                  GeometryInfo          , &  ! Input
!                                                  SensorIndex           , &  ! Input
!                                                  ChannelIndex          , &  ! Output     
!                                                  SfcOptics             , &  ! Output     
!                                                  IRWSOVariables        , &  ! Internal variable output
!                                                  Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation. On input the Angle component
!                        is assumed to contain data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       IRWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRWSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics argument is IN OUT rather
!       than just OUT as it is assumed to contain some data upon input.
!
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics( Surface     , &  ! Input
                                       GeometryInfo, &  ! Input
                                       SensorIndex , &  ! Input
                                       ChannelIndex, &  ! Input
                                       SfcOptics   , &  ! Output
                                       IRWSOV      , &  ! Internal variable output
                                       Message_Log ) &  ! Error messaging
                                     RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    TYPE(IRWSOVariables_type),    INTENT(IN OUT) :: IRWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics'
    ! Local variables
    INTEGER :: j, nZ, iZ


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang


    ! ---------------------------------
    ! Compute IR sea surface emissivity
    ! ---------------------------------
    Error_Status = CRTM_Compute_IRSSEM( Surface%Wind_Speed, &
                                        SC(SensorIndex)%Wavenumber(ChannelIndex), &
                                        SfcOptics%Angle(1:nZ), &
                                        SfcOptics%Emissivity(1:nZ,1), &
                                        Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing IR sea surface emissivity', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! ----------------------------------------------------
    ! Compute Lambertian surface reflectance
    ! Note that only the satellite zenith angle emissivity
    ! (index iZ) is used to compute the reflectivity for
    ! all angles (1:nZ).
    ! ----------------------------------------------------
    ! Solar direct
    IF ( SC(SensorIndex)%Is_Solar_Channel(ChannelIndex) == 1 ) THEN
      SfcOptics%Direct_Reflectivity(1:nZ,1) = ONE-SfcOptics%Emissivity(iZ,1)
    END IF

    ! Surface
    DO j = 1, nZ
      SfcOptics%Reflectivity(1:nZ,1,j,1) = (ONE-SfcOptics%Emissivity(iZ,1))*SfcOptics%Weight(j)
    END DO

  END FUNCTION Compute_IR_Water_SfcOptics


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Water_SfcOptics_TL
!
! PURPOSE:
!       Function to compute the tangent-linear surface emissivity and
!       reflectivity at infrared frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics_TL( Surface               , &  ! Input
!                                                     SfcOptics             , &  ! Input     
!                                                     Surface_TL            , &  ! Input
!                                                     GeometryInfo          , &  ! Input
!                                                     SensorIndex           , &  ! Input
!                                                     ChannelIndex          , &  ! Output     
!                                                     SfcOptics_TL          , &  ! Output     
!                                                     IRWSOVariables        , &  ! Internal variable input
!                                                     Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       Surface_TL:      CRTM_Surface structure containing the tangent-linear 
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       IRWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRWSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       SfcOptics_TL:    CRTM_SfcOptics structure containing the tangent-linear
!                        surface optical properties required for the tangent-
!                        linear radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output SfcOptics_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics_TL( Surface     , &  ! Input
                                          SfcOptics   , &  ! Input     
                                          Surface_TL  , &  ! Input
                                          GeometryInfo, &  ! Input
                                          SensorIndex , &  ! Input
                                          ChannelIndex, &  ! Input
                                          SfcOptics_TL, &  ! Output     
                                          IRWSOV      , &  ! Internal variable input
                                          Message_Log ) &  ! Error messaging 
                                        RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface_TL
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_TL
    TYPE(IRWSOVariables_type),    INTENT(IN)     :: IRWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics_TL'
    ! Local variables
    INTEGER :: j, nZ, iZ


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang


    ! ------------------------------------------------
    ! Compute tangent-linear IR sea surface emissivity
    ! ------------------------------------------------
    Error_Status = CRTM_Compute_IRSSEM_TL( Surface%Wind_Speed, &
                                           SC(SensorIndex)%Wavenumber(ChannelIndex), &
                                           SfcOptics%Angle(1:nZ), &
                                           Surface_TL%Wind_Speed, &
                                           SfcOptics_TL%Emissivity(1:nZ,1), &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing Tangent_linear IR sea surface emissivity', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! -----------------------------------------------------
    ! Compute tangent-linear Lambertian surface reflectance
    ! Note that only the satellite zenith angle emissivity
    ! (index iZ) is used to compute the reflectivity for
    ! all angles (1:nZ).
    ! -----------------------------------------------------
    ! Solar direct
    IF ( SC(SensorIndex)%Is_Solar_Channel(ChannelIndex) == 1 ) THEN
      SfcOptics_TL%Direct_Reflectivity(1:nZ,1) = -SfcOptics_TL%Emissivity(iZ,1)
    END IF

    DO j = 1, nZ
      SfcOptics_TL%Reflectivity(1:nZ,1,j,1) = (-SfcOptics_TL%Emissivity(iZ,1))*SfcOptics%Weight(j)
    END DO

  END FUNCTION Compute_IR_Water_SfcOptics_TL


!----------------------------------------------------------------------------------
!
! NAME:
!       Compute_IR_Water_SfcOptics_AD
!
! PURPOSE:
!       Function to compute the adjoint surface emissivity and
!       reflectivity at infrared frequencies over a water surface.
!
!       This function is a wrapper for third party code.
!
! CALLING SEQUENCE:
!       Error_Status = Compute_IR_Water_SfcOptics_AD( Surface               , &  ! Input
!                                                     SfcOptics             , &  ! Input     
!                                                     SfcOptics_AD          , &  ! Input     
!                                                     GeometryInfo          , &  ! Input
!                                                     SensorIndex           , &  ! Input
!                                                     ChannelIndex          , &  ! Output     
!                                                     Surface_AD            , &  ! Output
!                                                     IRWSOVariables        , &  ! Internal variable input
!                                                     Message_Log=Message_Log )  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Surface:         CRTM_Surface structure containing the surface state
!                        data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics:       CRTM_SfcOptics structure containing the surface
!                        optical properties required for the radiative
!                        transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SfcOptics_AD:    CRTM_SfcOptics structure containing the adjoint
!                        surface optical properties required for the adjoint
!                        radiative transfer calculation.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_SfcOptics_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       GeometryInfo:    CRTM_GeometryInfo structure containing the 
!                        view geometry information.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_GeometryInfo_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       SensorIndex:     Sensor index id. This is a unique index associated
!                        with a (supported) sensor used to access the
!                        shared coefficient data for a particular sensor.
!                        See the ChannelIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:    Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data for a particular sensor's
!                        channel.
!                        See the SensorIndex argument.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       IRWSOVariables:  Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_IR_Water_SfcOptics module.
!                        UNITS:      N/A
!                        TYPE:       IRWSOVariables_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
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
! OUTPUT ARGUMENTS:
!       Surface_AD:      CRTM_Surface structure containing the adjoint
!                        surface state data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Surface_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the computation was sucessful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the input SfcOptics_AD argument is IN OUT rather
!       than just OUT. This is necessary because components of this argument
!       may need to be zeroed out upon output.
!
!       Note the INTENT on the output Surface_AD argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!----------------------------------------------------------------------------------

  FUNCTION Compute_IR_Water_SfcOptics_AD( Surface     , &  ! Input
                                          SfcOptics   , &  ! Input     
                                          SfcOptics_AD, &  ! Input
                                          GeometryInfo, &  ! Input
                                          SensorIndex , &  ! Input
                                          ChannelIndex, &  ! Input
                                          Surface_AD  , &  ! Output     
                                          IRWSOV      , &  ! Internal variable input
                                          Message_Log ) &  ! Error messaging 
                                        RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_SfcOptics_type),    INTENT(IN)     :: SfcOptics
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics_AD
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: SensorIndex
    INTEGER,                      INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Surface_type),      INTENT(IN OUT) :: Surface_AD
    TYPE(IRWSOVariables_type),    INTENT(IN)     :: IRWSOV
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_IR_Water_SfcOptics_AD'
    ! Local variables
    INTEGER :: j, nZ, iZ


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Short name for angle dimensions
    nZ = SfcOptics%n_Angles
    iZ = SfcOptics%Index_Sat_Ang


    ! ----------------------------------------------
    ! Compute adjoint Lambertian surface reflectance
    ! ----------------------------------------------
    ! Surface
    DO j = nZ, 1, -1
      SfcOptics_AD%Emissivity(iZ,1) = SfcOptics_AD%Emissivity(iZ,1) - &
        (SUM(SfcOptics_AD%Reflectivity(1:nZ,1,j,1))*SfcOptics%Weight(j))
      SfcOptics_AD%Reflectivity(1:nZ,1,j,1) = ZERO
    END DO

    ! Solar direct
    IF ( SC(SensorIndex)%Is_Solar_Channel(ChannelIndex) == 1 ) THEN
      SfcOptics_AD%Emissivity(iZ,1) = SfcOptics_AD%Emissivity(iZ,1) - &
        SUM(SfcOptics_AD%Direct_Reflectivity(1:nZ,1))
      SfcOptics_AD%Direct_Reflectivity(1:nZ,1) = ZERO
    END IF


    ! ---------------------------------------------
    ! Compute sdjoint IRSSEM sea surface emissivity
    ! ---------------------------------------------
    Error_Status = CRTM_Compute_IRSSEM_AD( Surface%Wind_Speed, &
                                           SC(SensorIndex)%Wavenumber(ChannelIndex), &
                                           SfcOptics%Angle(1:nZ), &
                                           SfcOptics_AD%Emissivity(1:nZ,1), &
                                           Surface_AD%Wind_Speed, &
                                           Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error computing Adjoint IR sea surface emissivity', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF

  END FUNCTION Compute_IR_Water_SfcOptics_AD

END MODULE CRTM_IR_Water_SfcOptics
