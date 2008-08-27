!
! Sensor_Planck_Functions
!
! Module containing Planck function radiance, temperature, dB/dT, and 
! dT/dB routines for use in computing sensor channel values using the
! sensor's SpcCoeff data structure.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-May-2001
!                       paul.vandelst@ssec.wisc.edu


MODULE Sensor_Planck_Functions

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds      , ONLY: fp
  USE Message_Handler , ONLY: SUCCESS, FAILURE, Display_Message
  USE SpcCoeff_Define , ONLY: SpcCoeff_type
  USE Planck_Functions, ONLY: Planck_Radiance   , &
                              Planck_Temperature, &
                              Planck_dBdT       , &
                              Planck_dTdB
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Sensor_Radiance
  PUBLIC :: Sensor_Temperature
  PUBLIC :: Sensor_dBdT
  PUBLIC :: Sensor_dTdB


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Numeric literals
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  ! Floating point precision
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ZERO)


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
!       Sensor_Radiance
!
! PURPOSE:
!       Function to calculate the Planck radiance for a sensor channel.
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_Radiance( Sensor_SpcCoeff                  , &  ! Input
!                                       Sensor_Channel                   , &  ! Input
!                                       Temperature                      , &  ! Input
!                                       Radiance                         , &  ! Output
!                                       Wavelength_Units=Wavelength_Units, &  ! Optional input
!                                       Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
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
! OUTPUT ARGUMENTS:
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
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the Planck calculation was successful
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Sensor_Radiance( Sensor_SpcCoeff , &  ! Input
                            Sensor_Channel  , &  ! Input
                            Temperature     , &  ! Input
                            Radiance        , &  ! Output
                            Wavelength_Units, &  ! Optional input
                            Message_Log     ) &  ! Error messaging
                          RESULT( Error_Status )
    ! Arguments
    TYPE(SpcCoeff_type)   , INTENT(IN)  :: Sensor_SpcCoeff
    INTEGER               , INTENT(IN)  :: Sensor_Channel
    REAL(fp)              , INTENT(IN)  :: Temperature
    REAL(fp)              , INTENT(OUT) :: Radiance
    INTEGER     , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Radiance'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Frequency_Units
    INTEGER :: Idx(1), l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Sensor channel input
    IF ( .NOT. ( ANY(Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT(Wavelength_Units) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF


    ! Get the required channel index
    ! ------------------------------
    l = Get_ChannelIndex( Sensor_SpcCoeff, Sensor_Channel )
    

    ! Convert the frequency if required
    ! ---------------------------------
    x = Get_SpectralVariable( Sensor_SpcCoeff, Frequency_Units, l )


    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------
    Effective_Temperature = Compute_Effective_Temperature(Sensor_SpcCoeff, l, Temperature)


    ! Calculate the monochromatic Planck radiance
    ! with the corrected temperature
    ! -------------------------------------------
    Error_Status = Planck_Radiance( x                    , &
                                    Effective_Temperature, &
                                    Radiance             , &
                                    Wavelength_Units=Wavelength_Units, &
                                    Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Call to Planck_Radiance failed for channel ",i0)' ) Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

  END FUNCTION Sensor_Radiance 


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_Temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature for a sensor channel.
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_Temperature( Sensor_SpcCoeff                  , &  ! Input
!                                          Sensor_Channel                   , &  ! Input
!                                          Radiance                         , &  ! Input
!                                          Temperature                      , &  ! Output
!                                          Wavelength_Units=Wavelength_Units, &  ! Optional input
!                                          Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
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
! OUTPUT ARGUMENTS:
!       Temperature:      Brightness temperature of the required sensor channel
!                         for the given radiance.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL(fp)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Sensor_Temperature( Sensor_SpcCoeff , &  ! Input
                               Sensor_Channel  , &  ! Input
                               Radiance        , &  ! Input
                               Temperature     , &  ! Output
                               Wavelength_Units, &  ! Optional input
                               Message_Log     ) &  ! Error messaging
                             RESULT( Error_Status )
    ! Arguments
    TYPE( SpcCoeff_type ) , INTENT(IN)  :: Sensor_SpcCoeff
    INTEGER               , INTENT(IN)  :: Sensor_Channel
    REAL(fp)              , INTENT(IN)  :: Radiance
    REAL(fp)              , INTENT(OUT) :: Temperature
    INTEGER     , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_Temperature'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Frequency_Units
    INTEGER :: Idx(1), l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Sensor channel input
    IF ( .NOT. ( ANY(Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF


    ! Get the required channel index
    ! ------------------------------
    l = Get_ChannelIndex( Sensor_SpcCoeff, Sensor_Channel )
    

    ! Convert the frequency if required
    ! ---------------------------------
    x = Get_SpectralVariable( Sensor_SpcCoeff, Frequency_Units, l )


    ! Calculate the brightness temperature with
    ! the monochromatic Planck function
    ! -----------------------------------------
    Error_Status = Planck_Temperature( x                    , &
                                       Radiance             , &
                                       Effective_Temperature, &
                                       Wavelength_Units=Wavelength_Units, &
                                       Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Call to Planck_Temperature failed for channel ",i0)' ) &
                     Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF


    ! Correct the brightness temperature
    ! for polychromaticity
    ! ----------------------------------
    Temperature = Compute_Temperature(Sensor_SpcCoeff, l, Effective_Temperature)

  END FUNCTION Sensor_Temperature


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_dBdT
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature for a sensor channel.
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_dBdT( Sensor_SpcCoeff                  , &  ! Input
!                                   Sensor_Channel                   , &  ! Input
!                                   Temperature                      , &  ! Input
!                                   dBdT                             , &  ! Output
!                                   Wavelength_Units=Wavelength_Units, &  ! Optional input
!                                   Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
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
! OUTPUT ARGUMENTS:
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
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, dBdT units are mW/(m2.sr.cm-1.K)  *DEFAULT*
!                            == 1, dBdT units are W/(m2.sr.um.K)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Sensor_dBdT( Sensor_SpcCoeff , &  ! Input
                        Sensor_Channel  , &  ! Input
                        Temperature     , &  ! Input
                        dBdT            , &  ! Output
                        Wavelength_Units, &  ! Optional input
                        Message_Log     ) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE( SpcCoeff_type ) , INTENT(IN)  :: Sensor_SpcCoeff
    INTEGER               , INTENT(IN)  :: Sensor_Channel
    REAL(fp)              , INTENT(IN)  :: Temperature
    REAL(fp)              , INTENT(OUT) :: dBdT
    INTEGER     , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_dBdT'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Frequency_Units
    INTEGER :: Idx(1), l
    REAL(fp) :: x
    REAL(fp) :: Effective_Temperature

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Sensor channel input
    IF ( .NOT. ( ANY(Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF
 
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT(Wavelength_Units) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF


    ! Get the required channel index
    ! ------------------------------
    l = Get_ChannelIndex( Sensor_SpcCoeff, Sensor_Channel )
    

    ! Convert the frequency if required
    ! ---------------------------------
    x = Get_SpectralVariable( Sensor_SpcCoeff, Frequency_Units, l )


    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------
    Effective_Temperature = Compute_Effective_Temperature( Sensor_SpcCoeff, l, Temperature )


    ! Calculate the monochromatic Planck dB/dT
    ! with the corrected temperature
    ! ----------------------------------------
    Error_Status = Planck_dBdT( x                    , &
                                Effective_Temperature, &
                                dBdT                 , &
                                Wavelength_Units=Wavelength_Units, &
                                Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Call to Planck_dBdT failed for channel ",i0)' ) &
                     Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF

  END FUNCTION Sensor_dBdT


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Sensor_dTdB
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance for a sensor channel.
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_dTdB( Sensor_SpcCoeff                  , &  ! Input
!                                   Sensor_Channel                   , &  ! Input
!                                   Radiance                         , &  ! Input
!                                   dTdB                             , &  ! Output
!                                   Wavelength_Units=Wavelength_Units, &  ! Optional input
!                                   Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
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
! OUTPUT ARGUMENTS:
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
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                                  dT/dB units are (K.m2.sr.cm-1)/mW   *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                                  dT/dB units are (K.m2.sr.um)/W
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the Planck calculation was successful
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Sensor_dTdB( Sensor_SpcCoeff , &  ! Input
                        Sensor_Channel  , &  ! Input
                        Radiance        , &  ! Input
                        dTdB            , &  ! Output
                        Wavelength_Units, &  ! Optional input
                        Message_Log     ) &  ! Error messaging
                      RESULT( Error_Status )
    ! Arguments
    TYPE( SpcCoeff_type ) , INTENT(IN)  :: Sensor_SpcCoeff
    INTEGER               , INTENT(IN)  :: Sensor_Channel
    REAL(fp)              , INTENT(IN)  :: Radiance
    REAL(fp)              , INTENT(OUT) :: dTdB
    INTEGER     , OPTIONAL, INTENT(IN)  :: Wavelength_Units
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Sensor_dTdB'
    ! Local variables
    CHARACTER(256) :: Message
    LOGICAL :: Frequency_Units
    INTEGER :: Idx(1), l
    REAL(fp) :: x
    REAL(fp) :: Effective_dTdB

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Sensor channel input
    IF ( .NOT. ( ANY(Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF
 
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF


    ! Get the required channel index
    ! ------------------------------
    l = Get_ChannelIndex( Sensor_SpcCoeff, Sensor_Channel )
    

    ! Convert the frequency if required
    ! ---------------------------------
    x = Get_SpectralVariable( Sensor_SpcCoeff, Frequency_Units, l )


    ! Calculate the dT/dB derivative with
    ! the monochromatic Planck function
    ! -----------------------------------
    Error_Status = Planck_dTdB( x             , &
                                Radiance      , &
                                Effective_dTdB, &
                                Wavelength_Units=Wavelength_Units, &
                                Message_Log     =Message_Log       )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message,'("Call to Planck_dTdB failed for channel ",i0)' ) &
                     Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    ENDIF


    ! Correct the derivative
    ! for polychromaticity
    ! ----------------------
    dTdB = Effective_dTdB / Sensor_SpcCoeff%Band_C2(l)

  END FUNCTION Sensor_dTdB



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Function to search the SpcCoeff
  ! structure for a particular channel
  FUNCTION Get_ChannelIndex( SC, Ch ) RESULT( ChIdx )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    INTEGER            , INTENT(IN) :: Ch
    INTEGER :: ChIdx
    INTEGER :: Idx(1), l
    Idx = PACK( (/(l,l=1,SC%n_Channels)/), &
                SC%Sensor_Channel == Ch )
    ChIdx = Idx(1)
  END FUNCTION Get_ChannelIndex

  ! Function to retrieive the required
  ! spectral variable
  FUNCTION Get_SpectralVariable( SC, FUnits, ChIdx ) RESULT( x )
    TYPE(SpcCoeff_type), INTENT(IN) :: SC
    LOGICAL            , INTENT(IN) :: FUnits
    INTEGER            , INTENT(IN) :: ChIdx
    REAL(fp), PARAMETER :: TEN_THOUSAND = 10000.0_fp
    REAL(fp) :: x
    IF ( FUnits ) THEN
      ! Frequency in cm^-1
      x = SC%Wavenumber(ChIdx)
    ELSE
      ! Wavelength in microns
      x = TEN_THOUSAND/SC%Wavenumber(ChIdx)
    END IF
  END FUNCTION Get_SpectralVariable
  
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
