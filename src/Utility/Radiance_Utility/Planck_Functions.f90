!
! Planck_Functions
!
! Module containing Planck function radiance, temperature, dB/dT, and
! dT/dB routines.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 14-Oct-1999
!                     paul.vandelst@noaa.gov
!

MODULE Planck_Functions

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds           , ONLY: fp
  USE Fundamental_Constants, ONLY: C_1, C_2, &
                                   Fundamental_Constants_Version
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Planck_Radiance
  PUBLIC :: Planck_Temperature
  PUBLIC :: Planck_dBdT
  PUBLIC :: Planck_dTdB
  PUBLIC :: Planck_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Numeric literals
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  ! Unit types
  INTEGER, PARAMETER :: N_UNIT_TYPES = 2
  INTEGER, PARAMETER :: FREQUENCY_INDEX  = 1
  INTEGER, PARAMETER :: WAVELENGTH_INDEX = 2
  ! Scale factors. One for each unit type.
  REAL(fp), PARAMETER :: RADIANCE_SCALE_FACTOR(N_UNIT_TYPES) = (/  1000.0_fp, 1.0_fp /)
  REAL(fp), PARAMETER :: C_1_SCALE_FACTOR(N_UNIT_TYPES)      = (/ 1.0e+08_fp, 1.0e+24_fp /)
  REAL(fp), PARAMETER :: C_2_SCALE_FACTOR(N_UNIT_TYPES)      = (/   100.0_fp, 1.0e+06_fp /)


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
!       Planck_Radiance
!
! PURPOSE:
!       Elemental subroutine to calculate the Planck Radiance given the
!       spectral ordinate (frequency or wavelength) and temperature.
!
! CALLING SEQUENCE:
!       CALL Planck_Radiance( x                                , & ! Input
!                             Temperature                      , & ! Input
!                             Radiance                         , & ! Output
!                             Wavelength_Units=Wavelength_Units  ) ! Optional input
!
! INPUTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or any rank.
!                           ATTRIBUTES: INTENT(IN)
!
!       Temperature:        Temperature(s) for which the Planck Radiance(s)
!                           is(are) required. Can be a SCALAR or VECTOR.
!                           See Radiance output description for allowed
!                           dimensionality.
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Radiance:           The Planck radiance for the supplied temperature.
!                           UNITS:      mW/(m2.sr.cm-1)
!                                         or
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units:   Set this logical argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency.
!                           If == .FALSE., Input spectral ordinate units are cm^-1,  [DEFAULT]
!                                          Ouptut Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                              == .TRUE.,  Input spectral ordinate units are microns
!                                          Ouptut Radiance units are W/(m2.sr.micron)
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! PROCEDURE:
!       For frequency input, the Planck Radiance is calculated using:
!
!                   c1 * frequency^3
!         B =  ---------------------------
!                  ( c2 * frequency )
!               EXP( -------------- ) - 1
!                  (        T       )
!
!       For wavelength input:
!
!                                    c1
!         B = --------------------------------------------------
!                             [    (        c2      )     ]
!              wavelength^5 * [ EXP( -------------- ) - 1 ]
!                             [    ( wavelength * T )     ]
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m ->
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Planck_Radiance( &
    x               , &  ! Input
    Temperature     , &  ! Input
    Radiance        , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: x
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(OUT) :: Radiance
    LOGICAL , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Exponential

    ! Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units

    ! Calculate spectral parameters
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Unit_Index = FREQUENCY_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 * ( x**3 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 * x
    ELSE
      ! Spectral Units in microns
      Unit_Index = WAVELENGTH_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 / ( x**5 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 / x
    END IF

    ! Calculate radiance
    Exponential = EXP(x_c_2/Temperature)
    Radiance    = RADIANCE_SCALE_FACTOR(Unit_Index) * x_c_1 / ( Exponential - ONE )

  END SUBROUTINE Planck_Radiance


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_Temperature
!
! PURPOSE:
!       Elemental subroutine to calculate the Planck temperature given the
!       spectral ordinate (frequency or wavelength), and radiance.
!
! CALLING SEQUENCE:
!       CALL Planck_Temperature( x                                , & ! Input
!                                Radiance                         , & ! Input
!                                Temperature                      , & ! Output
!                                Wavelength_Units=Wavelength_Units  ) ! Optional input
!
! INPUTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1) [DEFAULT]
!                                         OR
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or any rank.
!                           ATTRIBUTES: INTENT(IN)
!
!       Radiance:           Planck radiance for which the temperature is
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1) [DEFAULT]
!                                         OR
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Temperature:        The brightness temperature corresponding to the
!                           supplied radiance.
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units:   Set this logical argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency.
!                           If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                          Input Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                              == .TRUE.,  Input spectral ordinate units are microns
!                                          Input Radiance units are W/(m2.sr.micron)
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! PROCEDURE:
!       For frequency input, the Planck temperature is calculated using:
!
!                    c2 * frequency
!         T = -----------------------------
!                ( c1 * frequency^3      )
!              LN( ---------------- +  1 )
!                (          B            )
!
!       For wavelength input :
!
!                                c2
!          T = -----------------------------------------
!                              (        c1            )
!               wavelength * LN( ---------------- + 1 )
!                              ( wavelength^5 * B     )
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m ->
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Planck_Temperature( &
    x               , &  ! Input
    Radiance        , &  ! Input
    Temperature     , &  ! output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: x
    REAL(fp),           INTENT(IN)  :: Radiance
    REAL(fp),           INTENT(OUT) :: Temperature
    LOGICAL , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Logarithm

    ! Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units

    ! Calculate spectral parameters
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Unit_Index = FREQUENCY_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 * ( x**3 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 * x
    ELSE
      ! Spectral Units in microns
      Unit_Index = WAVELENGTH_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 / ( x**5 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 / x
    END IF

    ! Calculate temperature
    Logarithm   = LOG( ( RADIANCE_SCALE_FACTOR(Unit_Index) * x_c_1 / Radiance ) + ONE )
    Temperature = x_c_2 / Logarithm

  END SUBROUTINE Planck_Temperature


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_dBdT
!
! PURPOSE:
!       Elemental subroutine to calculate the derivative of the Planck
!       radiance with respect to temperature given the spectral ordinate
!       (frequency or wavelength) and temperature.
!
! CALLING SEQUENCE:
!       CALL Planck_dBdT( x                                , &  ! Input
!                         Temperature                      , &  ! Input
!                         dBdT                             , &  ! Output
!                         Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         OR
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or any rank.
!                           ATTRIBUTES: INTENT(IN)
!
!       Temperature:        Temperature for which the radiance derivative
!                           is required.
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       dBdT:               The derivative of the Planck radiance with respect to
!                           temperature at the supplied temperature.
!                           UNITS:      mW/(m2.sr.cm-1.K)
!                                         OR
!                                       W/(m2.sr.um.K)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units:   Set this logical argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency.
!                           If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                          Output dB/dT units are mW/(m2.sr.cm-1.K) [DEFAULT]
!                              == .TRUE.,  Input spectral ordinate units are microns
!                                          Output dB/dT units are W/(m2.sr.um.K)
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! PROCEDURE:
!       For frequency input, the Planck radiance derivative with respect
!       to temperature is calculated using :
!
!                                              ( c2 * frequency )
!                   c1 * c2 * frequency^4 * EXP( -------------- )
!                                              (        T       )
!          dB/dT = -----------------------------------------------
!                      {     [    ( c2 * frequency )     ] }^2
!                      { T * [ EXP( -------------- ) - 1 ] }
!                      {     [    (        T       )     ] }
!
!
!       For wavelength input :
!                                            (       c2       )
!                               c1 * c2 * EXP( -------------- )
!                                            ( wavelength * T )
!          dB/dT = --------------------------------------------------------
!                                  {     [    (       c2       )     ] }^2
!                   wavelength^6 * { T * [ EXP( -------------- ) - 1 ] }
!                                  {     [    ( wavelength * T )     ] }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m ->
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Planck_dBdT( &
    x               , &  ! Input
    Temperature     , &  ! Input
    dBdT            , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: x
    REAL(fp),           INTENT(IN)  :: Temperature
    REAL(fp),           INTENT(OUT) :: dBdT
    LOGICAL , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Exponential

    ! Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units

    ! Calculate spectral parameters
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Unit_Index = FREQUENCY_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 * &
              C_2_SCALE_FACTOR(Unit_Index) * C_2 * ( x**4 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 * x
    ELSE
      ! Spectral Units in microns
      Unit_Index = WAVELENGTH_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 * &
              C_2_SCALE_FACTOR(Unit_Index) * C_2 / ( x**6 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 / x
    END IF

    ! Calculate dBdT
    Exponential = EXP( x_c_2 / Temperature )
    dBdT        = RADIANCE_SCALE_FACTOR(Unit_Index) * x_c_1 * Exponential / &
                  ( Temperature * ( Exponential - ONE ) )**2

  END SUBROUTINE Planck_dBdT


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_dTdB
!
! PURPOSE:
!       Elemental subroutine to calculate the Planck temperature derivative
!       with respect to radiance given the spectral ordinate (frequency or
!       wavelength) and radiance.
!
! CALLING SEQUENCE:
!       CALL Planck_dTdB( x                                , &  ! Input
!                         Radiance                         , &  ! Input
!                         dTdB                             , &  ! Output
!                         Wavelength_Units=Wavelength_Units  )  ! Optional input
!
!
! INPUTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         OR
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or any rank.
!                           ATTRIBUTES: INTENT(IN)
!
!       Radiance:           Planck radiance for which the dT/dB derivative is
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1)
!                                         OR
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with the x input argument
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       dTdB:               The derivative of Planck temperature with respect
!                           to radiance.
!                           UNITS:      (K.m2.sr.cm-1)/mW
!                                         OR
!                                       (K.m2.sr.um)/W
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Conformable with the x input argument.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Wavelength_Units:   Set this logical argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency.
!                           If == .FALSE., Input spectral ordinate units are cm^-1, [DEFAULT]
!                                          Input Radiance units are mW/(m2.sr.cm-1) [DEFAULT]
!                                          Output dT/dB units are (K.m2.sr.cm-1)/mW [DEFAULT]
!                              == .TRUE.,  Input spectral ordinate units are microns
!                                          Input Radiance units are W/(m2.sr.um)
!                                          Output dT/dB units are (K.m2.sr.um)/W
!                           If not specified, default is .FALSE.
!                           UNITS:      N/A
!                           TYPE:       LOGICAL
!                           DIMENSION:  Conformable with x input argument.
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! PROCEDURE:
!       For frequency input, the Planck temperature differential with respect
!       to radiance is calculated using:
!
!                               c1 * c2 * frequency^4
!  dT/dB = ------------------------------------------------------------------
!           { c1 * frequency^3     }   {       ( c1 * frequency^3      ) }^2
!           { ---------------- + 1 } * { B * LN( ---------------- +  1 ) }
!           {         B            }   {       (          B            ) }
!
!
!       For wavelength input:
!
!                                           c1 * c2
!  dT/dB = --------------------------------------------------------------------------------
!                          {        c1            }   {       (        c1            ) }^2
!           wavelength^6 * { ---------------- + 1 } * { B * LN( ---------------- + 1 ) }
!                          { wavelength^5 * B     }   {       ( wavelength^5 * B     ) }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiances such that radiances
!       are in the units of mW/(m2.sr.cm-1) for frequency input or
!       W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m ->
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Planck_dTdB( &
    x               , &  ! Input
    Radiance        , &  ! Input
    dTdB            , &  ! Output
    Wavelength_Units  )  ! Optional input
    ! Arguments
    REAL(fp),           INTENT(IN)  :: x
    REAL(fp),           INTENT(IN)  :: Radiance
    REAL(fp),           INTENT(OUT) :: dTdB
    LOGICAL , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Scaled_Radiance
    REAL(fp) :: Argument

    ! Set spectral units
    Frequency_Units = .TRUE.
    IF ( PRESENT(Wavelength_Units) ) Frequency_Units = .NOT. Wavelength_Units

    ! Calculate spectral parameters
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Unit_Index = FREQUENCY_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 * ( x**3 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 * x
    ELSE
      ! Spectral Units in microns
      Unit_Index = WAVELENGTH_INDEX
      x_c_1 = C_1_SCALE_FACTOR(Unit_Index) * C_1 / ( x**5 )
      x_c_2 = C_2_SCALE_FACTOR(Unit_Index) * C_2 / x
    END IF

    ! Calculate dTdB
    ! ...Radiance in terms of W
    Scaled_Radiance = Radiance / RADIANCE_SCALE_FACTOR(Unit_Index)
    ! ...Common term in dT/dB formulation
    Argument = ( x_c_1 / Scaled_Radiance ) + ONE
    ! ...Calculate dT/dB in (K.....)/W
    dTdB = x_c_1 * x_c_2 / ( Argument * ( Scaled_Radiance * LOG(Argument) )**2 )
    ! ...Convert dT/dB Units to (K...cm-1)/mW if required.
    dTdB = dTdB / RADIANCE_SCALE_FACTOR(Unit_Index)

  END SUBROUTINE Planck_dTdB


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL Planck_Version( Id )
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

  SUBROUTINE Planck_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    CHARACTER(256) :: fundamental_constants_version_id
    CALL Fundamental_Constants_Version(fundamental_constants_version_id)
    Id = MODULE_VERSION_ID//'; '//TRIM(fundamental_constants_version_id)
  END SUBROUTINE Planck_Version

END MODULE Planck_Functions
