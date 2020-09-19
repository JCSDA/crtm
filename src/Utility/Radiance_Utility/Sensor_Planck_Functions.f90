!
! Sensor_Planck_Functions
!
! Module containing Planck function radiance, temperature, dB/dT, and
! dT/dB routines for use in computing sensor channel values using the
! sensor's SpcCoeff data structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 07-May-2001
!                       paul.vandelst@noaa.gov


MODULE Sensor_Planck_Functions

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds               , ONLY: fp
  USE Message_Handler          , ONLY: SUCCESS, FAILURE, Display_Message
  USE Spectral_Units_Conversion, ONLY: Wavelength => inverse_cm_to_micron
  USE SpcCoeff_Define          , ONLY: SpcCoeff_type
  USE Planck_Functions         , ONLY: Planck_Radiance   , &
                                       Planck_Temperature, &
                                       Planck_dBdT       , &
                                       Planck_dTdB       , &
                                       Planck_Version
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Sensor_Planck_Radiance
  PUBLIC :: Sensor_Planck_Temperature
  PUBLIC :: Sensor_Planck_dBdT
  PUBLIC :: Sensor_Planck_dTdB
  PUBLIC :: Sensor_Planck_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Numeric literals
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Planck_Radiance
!
! PURPOSE:
!       Subroutine to calculate the Planck radiance for a sensor channel.
!
! CALLING SEQUENCE:
!       CALL Sensor_Planck_Radiance( &
!              SpcCoeff                         , &  ! Input
!              Sensor_Channel                   , &  ! Input
!              Temperature                      , &  ! Input
!              Radiance                         , &  ! Output
!              Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUTS:
!       SpcCoeff:         The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the radiance is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Temperature:      Temperature for which the sensor Planck radiance is
!                         required.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Radiance:         The Planck radiance for the requested sensor channel
!                         at the specified temperature.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units: Set this logical argument to specify the spectral
!                         ordinate and radiance units in terms of wavelength
!                         rather than frequency.
!                         If == .FALSE., Input spectral ordinate units are cm^-1,  [DEFAULT]
!                                        Ouptut Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                            == .TRUE.,  Input spectral ordinate units are microns
!                                        Ouptut Radiance units are W/(m2.sr.micron)
!                         If not specified, default is .FALSE.
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Conformable with x input argument.
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Sensor_Planck_Radiance( &
    SpcCoeff        , &  ! Input
    Sensor_Channel  , &  ! Input
    Temperature     , &  ! Input
    Radiance        , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    INTEGER ,            INTENT(IN)  :: Sensor_Channel
    REAL(fp),            INTENT(IN)  :: Temperature
    REAL(fp),            INTENT(OUT) :: Radiance
    LOGICAL ,  OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Planck_Radiance'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    Radiance = ZERO
    ! ...Check sensor channel input
    IF ( .NOT. ( ANY(SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            FAILURE )
      RETURN
    END IF
    ! ...Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units


    ! Get the required channel index
    l = Get_Channel_Index( SpcCoeff, Sensor_Channel )


    ! Convert the frequency if required
    x = Get_Spectral_Variable( SpcCoeff, Frequency_Units, l )


    ! Calculate the temperature corrected
    ! for polychromaticity
    Effective_Temperature = Compute_Effective_Temperature(SpcCoeff, l, Temperature)


    ! Calculate the monochromatic Planck radiance
    ! with the corrected temperature
    CALL Planck_Radiance( &
      x                    , &
      Effective_Temperature, &
      Radiance             , &
      Wavelength_Units=Wavelength_Units )

  END SUBROUTINE Sensor_Planck_Radiance


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Planck_Temperature
!
! PURPOSE:
!       Subroutine to calculate the Planck temperature for a sensor channel.
!
! CALLING SEQUENCE:
!       CALL Sensor_Planck_Temperature( &
!              SpcCoeff                         , &  ! Input
!              Sensor_Channel                   , &  ! Input
!              Radiance                         , &  ! Input
!              Temperature                      , &  ! Output
!              Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUTS:
!       SpcCoeff:         The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the temperature is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Radiance:         The Planck radiance of the sensor channel for
!                         which the brightness temperature is required.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature:      Brightness temperature of the required sensor channel
!                         for the given radiance.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Wavelength_Units: Set this logical argument to specify the spectral
!                         ordinate and radiance units in terms of wavelength
!                         rather than frequency.
!                         If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                        Input Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                            == .TRUE.,  Input spectral ordinate units are microns
!                                        Input Radiance units are W/(m2.sr.micron)
!                         If not specified, default is .FALSE.
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Conformable with x input argument.
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Sensor_Planck_Temperature( &
    SpcCoeff        , &  ! Input
    Sensor_Channel  , &  ! Input
    Radiance        , &  ! Input
    Temperature     , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    INTEGER ,            INTENT(IN)  :: Sensor_Channel
    REAL(fp),            INTENT(IN)  :: Radiance
    REAL(fp),            INTENT(OUT) :: Temperature
    LOGICAL ,  OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Planck_Temperature'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    Temperature = ZERO
    ! ...Check sensor channel input
    IF ( .NOT. ( ANY(SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            FAILURE )
      RETURN
    END IF
    ! ...Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units


    ! Get the required channel index
    l = Get_Channel_Index( SpcCoeff, Sensor_Channel )


    ! Convert the frequency if required
    x = Get_Spectral_Variable( SpcCoeff, Frequency_Units, l )


    ! Calculate the brightness temperature with
    ! the monochromatic Planck function
    CALL Planck_Temperature( &
      x                    , &
      Radiance             , &
      Effective_Temperature, &
      Wavelength_Units=Wavelength_Units )


    ! Correct the brightness temperature
    ! for polychromaticity
    Temperature = Compute_Temperature(SpcCoeff, l, Effective_Temperature)

  END SUBROUTINE Sensor_Planck_Temperature


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Planck_dBdT
!
! PURPOSE:
!       Subroutine to calculate the derivative of the Planck radiance with
!       respect to temperature for a sensor channel.
!
! CALLING SEQUENCE:
!       CALL Sensor_Planck_dBdT( &
!              SpcCoeff                         , &  ! Input
!              Sensor_Channel                   , &  ! Input
!              Temperature                      , &  ! Input
!              dBdT                             , &  ! Output
!              Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUTS:
!       SpcCoeff:         The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the dB/dT derivative is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Temperature:      Temperature for which the sensor dB/dT is
!                         required.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       dBdT:             The derivative of the Planck radiance with respect
!                         to temperature at the supplied temperature for the
!                         requested sensor channel.
!                         UNITS:      mW/(m2.sr.cm-1.K)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.um.K)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units: Set this logical argument to specify the spectral
!                         ordinate and radiance units in terms of wavelength
!                         rather than frequency.
!                         If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                        Output dB/dT units are mW/(m2.sr.cm-1.K) [DEFAULT]
!                            == .TRUE.,  Input spectral ordinate units are microns
!                                        Output dB/dT units are W/(m2.sr.um.K)
!                         If not specified, default is .FALSE.
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Conformable with x input argument.
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Sensor_Planck_dBdT( &
    SpcCoeff        , &  ! Input
    Sensor_Channel  , &  ! Input
    Temperature     , &  ! Input
    dBdT            , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    INTEGER ,            INTENT(IN)  :: Sensor_Channel
    REAL(fp),            INTENT(IN)  :: Temperature
    REAL(fp),            INTENT(OUT) :: dBdT
    LOGICAL ,  OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Planck_dBdT'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    dBdT = ZERO
    ! ...Check sensor channel input
    IF ( .NOT. ( ANY(SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            FAILURE )
      RETURN
    END IF
    ! ...Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units


    ! Get the required channel index
    l = Get_Channel_Index( SpcCoeff, Sensor_Channel )


    ! Convert the frequency if required
    x = Get_Spectral_Variable( SpcCoeff, Frequency_Units, l )


    ! Calculate the temperature corrected
    ! for polychromaticity
    Effective_Temperature = Compute_Effective_Temperature( SpcCoeff, l, Temperature )


    ! Calculate the monochromatic Planck dB/dT
    ! with the corrected temperature
    CALL Planck_dBdT( &
      x                    , &
      Effective_Temperature, &
      dBdT                 , &
      Wavelength_Units=Wavelength_Units )

  END SUBROUTINE Sensor_Planck_dBdT


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Planck_dTdB
!
! PURPOSE:
!       Subroutine to calculate the Planck temperature derivative with respect
!       to radiance for a sensor channel.
!
! CALLING SEQUENCE:
!       CALL  Sensor_Planck_dTdB( &
!               SpcCoeff                         , &  ! Input
!               Sensor_Channel                   , &  ! Input
!               Radiance                         , &  ! Input
!               dTdB                             , &  ! Output
!               Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUTS:
!       SpcCoeff:         The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the temperature is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       Radiance:         The Planck radiance of the sensor channel for
!                         which the dT/dB derivate is required.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       dTdB:             The derivative of Planck temperature with respect
!                         to radiance of the required sensor channel for the
!                         given radiance.
!                         UNITS:      (K.m2.sr.cm-1)/mW  *DEFAULT*
!                                         or
!                                     (K.m2.sr.um)/W
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Wavelength_Units: Set this logical argument to specify the spectral
!                         ordinate and radiance units in terms of wavelength
!                         rather than frequency.
!                         If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                        Input Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                                        Output dT/dB units are (K.m2.sr.cm-1)/mW [DEFAULT]
!                            == .TRUE.,  Input spectral ordinate units are microns
!                                        Input Radiance units are W/(m2.sr.um)
!                                        Output dT/dB units are (K.m2.sr.um)/W
!                         If not specified, default is .FALSE.
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Conformable with x input argument.
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE Sensor_Planck_dTdB( &
    SpcCoeff        , &  ! Input
    Sensor_Channel  , &  ! Input
    Radiance        , &  ! Input
    dTdB            , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    TYPE(SpcCoeff_type), INTENT(IN)  :: SpcCoeff
    INTEGER ,            INTENT(IN)  :: Sensor_Channel
    REAL(fp),            INTENT(IN)  :: Radiance
    REAL(fp),            INTENT(OUT) :: dTdB
    LOGICAL ,  OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Planck_dTdB'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: l
    REAL(fp) :: x
    REAL(fp) :: Effective_dTdB

    ! Set up
    dTdB = ZERO
    ! ...Check sensor channel input
    IF ( .NOT. ( ANY(SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            FAILURE )
      RETURN
    END IF
    ! ...Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units


    ! Get the required channel index
    l = Get_Channel_Index( SpcCoeff, Sensor_Channel )


    ! Convert the frequency if required
    x = Get_Spectral_Variable( SpcCoeff, Frequency_Units, l )


    ! Calculate the dT/dB derivative with
    ! the monochromatic Planck function
    CALL Planck_dTdB( &
      x             , &
      Radiance      , &
      Effective_dTdB, &
      Wavelength_Units=Wavelength_Units )


    ! Correct the derivative for polychromaticity
    dTdB = Effective_dTdB / SpcCoeff%Band_C2(l)

  END SUBROUTINE Sensor_Planck_dTdB


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Planck_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Sensor_Planck_Version( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Sensor_Planck_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    CHARACTER(256) :: planck_version_id
    CALL Planck_Version(planck_version_id)
    Id = MODULE_VERSION_ID//'; '//TRIM(planck_version_id)
  END SUBROUTINE Sensor_Planck_Version


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Function to search the SpcCoeff
  ! structure for a particular channel
  FUNCTION Get_Channel_Index( SC, Ch ) RESULT( ChIdx )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    INTEGER            , INTENT(IN) :: Ch
    INTEGER :: ChIdx
    INTEGER :: Idx(1), l
    Idx = PACK( (/(l,l=1,SC%n_Channels)/), SC%Sensor_Channel == Ch )
    ChIdx = Idx(1)
  END FUNCTION Get_Channel_Index

  ! Function to retrieive the required
  ! spectral variable
  FUNCTION Get_Spectral_Variable( SC, FUnits, ChIdx ) RESULT( x )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    LOGICAL            , INTENT(IN) :: FUnits
    INTEGER            , INTENT(IN) :: ChIdx
    REAL(fp) :: x
    IF ( FUnits ) THEN
      ! Frequency in cm^-1
      x = SC%Wavenumber(ChIdx)
    ELSE
      ! Wavelength in microns
      x = Wavelength(SC%Wavenumber(ChIdx))
    END IF
  END FUNCTION Get_Spectral_Variable

  ! Function to compute an effective temperature
  ! due to polychromaticity.
  FUNCTION Compute_Effective_Temperature(SC, ChIdx, T ) RESULT( Teff )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    INTEGER            , INTENT(IN) :: ChIdx
    REAL(fp)           , INTENT(IN) :: T
    REAL(fp) :: Teff
    Teff = SC%Band_C1(ChIdx) + (SC%Band_C2(ChIdx)*T)
  END FUNCTION Compute_Effective_Temperature

  ! Function to correct temperatures for polychromaticity
  FUNCTION Compute_Temperature(SC, ChIdx, Teff ) RESULT( T )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    INTEGER            , INTENT(IN) :: ChIdx
    REAL(fp)           , INTENT(IN) :: Teff
    REAL(fp) :: T
    T = (Teff - SC%Band_C1(ChIdx))/SC%Band_C2(ChIdx)
  END FUNCTION Compute_Temperature

END MODULE Sensor_Planck_Functions
