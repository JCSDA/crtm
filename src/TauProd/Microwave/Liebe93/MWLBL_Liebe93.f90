!
! NAME:
!       MWLBL_Liebe93
!
! PURPOSE:
!       Module containing routines to calculate microwave atmospheric
!       attenuation according to the Liebe 93 MPM model.
!
!
! CREATION HISTORY:
!       This module has been translated from FORTRAN-77 source code
!       and include files written by:
!         L. Phalippou ECMWF 13-Dec-1993
!         Peter Rayer UKMO 19-Dec-2000
!         Roger Saunders UKMO
!
!       Written by:     Paul van Delst, 21-Apr-2002
!                       paul.vandelst@noaa.gov
!

MODULE MWLBL_Liebe93

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Liebe92_Coefficients
!  USE Liebe93_Coefficients
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Liebe93


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Liebe93
    MODULE PROCEDURE Liebe93_By_Layer
    MODULE PROCEDURE Liebe93_By_Frequency
  END INTERFACE Liebe93


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO           = 0.0_fp
  REAL(fp), PARAMETER :: ZEROpointEIGHT = 0.8_fp
  REAL(fp), PARAMETER :: ONE            = 1.0_fp
  REAL(fp), PARAMETER :: ONEpointONE    = 1.1_fp
  REAL(fp), PARAMETER :: ONEpointFIVE   = 1.5_fp
  REAL(fp), PARAMETER :: THREE          = 3.0_fp
  REAL(fp), PARAMETER :: THREEpointFIVE = 3.5_fp
  REAL(fp), PARAMETER :: FOURpointFIVE  = 4.5_fp
  REAL(fp), PARAMETER :: SEVENpointFIVE = 7.5_fp
  REAL(fp), PARAMETER :: TOLERANCE      = EPSILON(ZERO)
  ! Conversion factor for attenuation computation.
  REAL(fp), PARAMETER :: ATT_K = 0.1820_fp
  ! Min/max temperatures in Kelvin
  REAL(fp), PARAMETER :: MIN_TEMPERATURE = 150.0_fp
  REAL(fp), PARAMETER :: MAX_TEMPERATURE = 350.0_fp
  ! Min/max frequencies in GHz
  REAL(fp), PARAMETER :: MIN_FREQUENCY = ONE
  REAL(fp), PARAMETER :: MAX_FREQUENCY = 1000.0_fp
  ! Default message string length
  INTEGER,  PARAMETER :: ML = 256

CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Liebe93
!
! PURPOSE:
!       Function to calculate the atmospheric attentuation in the microwave
!       according to Liebe's MPM93 model.
!
! CALLING SEQUENCE:
!       Error_Status = Liebe93( &
!         Frequency       , &  ! Input
!         Dry_Air_Pressure, &  ! Input
!         H2O_Pressure    , &  ! Input
!         Temperature     , &  ! Input
!         Quiet                    = Quiet                   , &  ! Optional input
!         WetLine_Attenuation      = WetLine_Attenuation     , &  ! Optional output
!         WetContinuum_Attenuation = WetContinuum_Attenuation, &  ! Optional output
!         DryLine_Attenuation      = DryLine_Attenuation     , &  ! Optional output
!         DryContinuum_Attenuation = DryContinuum_Attenuation  )  ! Optional output
!
!
!       Two forms of argument specification are available - by FREQUENCY
!       or by LAYER.
!
!       If by FREQUENCY, then multiple frequencies are processed for
!       a single layer:
!         Frequency        - Rank-1, size n_Frequencies
!         Dry_Air_Pressure - Scalar
!         H2O_Pressure     - Scalar
!         Temperature      - Scalar
!         Attenuation      - Rank-1, size n_Frequencies
!
!       If by LAYER, then a single frequency is processed for multiple
!       layers:
!         Frequency        - Scalar
!         Dry_Air_Pressure - Rank-1, size n_Layers
!         H2O_Pressure     - Rank-1, size n_Layers
!         Temperature      - Rank-1, size n_Layers
!         Attenuation      - Rank-1, size n_Layers
!
!
! INPUTS:
!       Frequency:                  Frequency for which the atmospheric attenuation
!                                   is required
!                                   UNITS:      GHz
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1  (by FREQUENCY)
!                                                 OR
!                                               Scalar  (by LAYER)
!                                   ATTRIBUTES: INTENT(IN)
!
!       Dry_Air_Pressure:           Dry air partial pressure profile.
!                                   UNITS:      hectoPascals, hPa
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Scalar  (by FREQUENCY)
!                                                 OR
!                                               Rank-1  (by LAYER)
!                                   ATTRIBUTES: INTENT(IN)
!
!       H2O_Pressure:               Water vapor partial pressure profile.
!                                   UNITS:      hectoPascals, hPa
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Same as DRY_AIR_PRESSURE argument
!                                   ATTRIBUTES: INTENT(IN)
!
!       Temperature:                Temperature profile.
!                                   UNITS:      Kelvin
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Same as DRY_AIR_PRESSURE argument
!                                   ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:                      Set this logical argument to suppress WARNING and
!                                   INFORMATION messages being printed to stdout
!                                   If == .FALSE., messages are OUTPUT [DEFAULT].
!                                      == .TRUE.,  messages are SUPPRESSED.
!                                   If not specified, default is .FALSE.
!                                   UNITS:      N/A
!                                   TYPE:       LOGICAL
!                                   DIMENSION:  Scalar
!                                   ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       WetLine_Attenuation:        Power attenuation due to absorption by water
!                                   vapour absorption lines.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1  (n_Frequencies, by FREQUENCY)
!                                                 OR
!                                               Rank-1  (n_Layers, by LAYER)
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WetContinuum_Attenuation:   Power attenuation due to water vapour continuum
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Same as WETLINE_ATTENUATION argument.
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DryLine_Attenuation:        Power attenuation due to absorption by oxygen
!                                   absorption lines.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Same as WETLINE_ATTENUATION argument.
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DryContinuum_Attenuation:   Power attenuation due to non-resonant dry air
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Same as WETLINE_ATTENUATION argument.
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:               The return value is an integer defining the error status.
!                                   status. The error codes are defined in the Message_Handler
!                                   module.
!                                   If == SUCCESS the calculation was successful
!                                      == FAILURE an unrecoverable error occurred.
!                                   UNITS:      N/A
!                                   TYPE:       INTEGER
!                                   DIMENSION:  Scalar
!
! PROCEDURE:
!       Liebe, H.J., G.A. Hufford, and M.G. Cotton, 1993: "Propagation
!         modelling of moist air and suspended water/ice particles at
!         frequencies below 1000 GHz". In AGARD Conference Proceedings
!         542, Atmospheric propagation effects through natural and man-made
!         obscurants for visible through MM-wave radiation, pp 3.1-3.10.
!         Presented at the Electromagnetic Wave Propagation Panel Symposium,
!         Palma de Mallorca, Spain, 17-20 May 1993.
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou ECMWF 13-Dec-1993
!       F77 version modified by: Peter Rayer UKMO 19-Dec-2000
!                                Roger Saunders UKMO
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  ! --------------------------------------
  ! Multi-Frequency, Single-Layer function
  ! --------------------------------------
  FUNCTION Liebe93_By_Frequency( &
    Frequency               , &  ! Input
    Dry_Air_Pressure        , &  ! Input
    H2O_Pressure            , &  ! Input
    Temperature             , &  ! Input
    Quiet,                    &  ! Optional input
    WetLine_Attenuation     , &  ! Optional output
    WetContinuum_Attenuation, &  ! Optional output
    DryLine_Attenuation     , &  ! Optional output
    DryContinuum_Attenuation) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Frequency(:)
    REAL(fp),           INTENT(IN)  :: Dry_Air_Pressure
    REAL(fp),           INTENT(IN)  :: H2O_Pressure
    REAL(fp),           INTENT(IN)  :: Temperature
    LOGICAL , OPTIONAL, INTENT(IN)  :: Quiet
    REAL(fp), OPTIONAL, INTENT(OUT) :: WetLine_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: WetContinuum_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: DryLine_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: DryContinuum_Attenuation(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Liebe93::By_Frequency'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    INTEGER :: n_Frequencies, l
    REAL(fp) :: Theta
    REAL(fp) :: O2_s1, Common_s1, O2_g1, O2_d1, H2O_s1
    REAL(fp) :: H2O_Line_Absorption
    REAL(fp) :: WetContinuum_Absorption
    REAL(fp) :: O2_Line_Absorption
    REAL(fp) :: DryContinuum_Absorption

    ! Set up
    err_stat = SUCCESS
    ! ...Process keywords
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT.Quiet

    ! Check output argument validity
    n_Frequencies = SIZE(Frequency)
    ! ...The water vapour line attenuation
    Compute_WetLine = .FALSE.
    IF ( PRESENT(WetLine_Attenuation) ) THEN
      Compute_WetLine = .TRUE.
      IF ( SIZE(WetLine_Attenuation) /= n_Frequencies ) THEN
        err_stat = FAILURE
        msg = 'WetLine_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour continuum attenuation
    Compute_WetContinuum = .FALSE.
    IF ( PRESENT(WetContinuum_Attenuation) ) THEN
      Compute_WetContinuum = .TRUE.
      IF ( SIZE(WetContinuum_Attenuation) /= n_Frequencies ) THEN
        err_stat = FAILURE
        msg = 'WetContinuum_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour line attenuation
    Compute_DryLine = .FALSE.
    IF ( PRESENT(DryLine_Attenuation) ) THEN
      Compute_DryLine = .TRUE.
      IF ( SIZE(DryLine_Attenuation) /= n_Frequencies ) THEN
        err_stat = FAILURE
        msg = 'DryLine_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour continuum attenuation
    Compute_DryContinuum = .FALSE.
    IF ( PRESENT( DryContinuum_Attenuation ) ) THEN
      Compute_DryContinuum = .TRUE.
      IF ( SIZE( DryContinuum_Attenuation ) /= n_Frequencies ) THEN
        err_stat = FAILURE
        msg = 'DryContinuum_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...If no output arguments at all, return
    IF ( ( .NOT. Compute_WetLine      ) .AND. &
         ( .NOT. Compute_WetContinuum ) .AND. &
         ( .NOT. Compute_DryLine      ) .AND. &
         ( .NOT. Compute_DryContinuum )       ) RETURN
    ! ...Check for invalid frequencies
    IF ( ANY( Frequency < ZERO ) ) THEN
      err_stat = FAILURE
        msg = 'Input frequencies must be > 0.0.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Check for invalid profile data
    IF ( Dry_Air_Pressure < TOLERANCE .OR. &
         H2O_Pressure     < TOLERANCE .OR. &
         Temperature      < TOLERANCE      ) THEN
      err_stat = FAILURE
      msg = 'Input tressure/temperature arguments must be > 0.0.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Issue warnings for data outside range
    IF ( noisy )  THEN
    
      ! Frequencies < min value or > max_value
      IF ( ANY(Frequency < MIN_FREQUENCY) ) THEN
        WRITE( msg,'("Input frequencies < ",f6.1," GHz found.")' ) MIN_FREQUENCY
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF
      IF ( ANY(Frequency > MAX_FREQUENCY) ) THEN
        WRITE( msg,'("Input frequencies > ",f6.1," GHz found.")' ) MAX_FREQUENCY
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF

      ! Temperatures < min value or > max_value
      IF ( Temperature < MIN_TEMPERATURE ) THEN
        WRITE( msg,'("Input temperature < ",f6.1," K.")' ) MIN_TEMPERATURE
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF
      IF ( Temperature > MAX_TEMPERATURE ) THEN
        WRITE( msg,'("Input temperature > ",f6.1," K.")' ) MAX_TEMPERATURE
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF

    END IF


    ! Calculate the relative reciprocal temperature, Theta,
    ! and common line strength factor, Common_s1
    Theta     = Compute_Theta( Temperature )
    Common_s1 = Compute_Common_s1( Theta )


    ! Calculate the wet line attentuation
    WetLine_Compute: IF ( Compute_WetLine ) THEN
      ! ...Calculate the H2O line strength factor
      H2O_s1 = Compute_H2O_s1( H2O_Pressure, Theta )
      ! ...Loop over the required frequencies
      WetLine_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the water vapour line absoprtion
        H2O_Line_Absorption = Compute_H2O_Line_Absorption( &
                                Frequency(l)    , &
                                Dry_Air_Pressure, &
                                H2O_Pressure    , &
                                Theta           , &
                                Common_s1       , &
                                H2O_s1            )
        IF ( H2O_Line_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative sum for H2O line absorption, ",es13.6,1x,&
                        &"at Frequency ",f8.3,1x,&
                        &"GHz. Setting to 0.0.")' ) H2O_Line_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          H2O_Line_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        WetLine_Attenuation(l) = ATT_K * Frequency(l) * H2O_Line_Absorption
      END DO WetLine_Frequency_Loop
    END IF WetLine_Compute


    ! Calculate the wet continuum attentuation for each frequency
    WetContinuum_Compute: IF ( Compute_WetContinuum ) THEN
      ! ...Loop over the required frequencies
      WetContinuum_Frequency_Loop: DO l = 1, n_Frequencies
!***TURNING OFF WET CONTINUUM***
!        ! ...Compute the water vapour continuum absoprtion
!        WetContinuum_Absorption = Compute_Wet_Continuum( &
!                                    Frequency(l)    , &
!                                    Dry_Air_Pressure, &
!                                    H2O_Pressure    , &
!                                    Theta             )
!        IF ( WetContinuum_Absorption < ZERO ) THEN
!          IF ( noisy ) THEN
!            WRITE( msg,'("Negative value for wet continuum absorption, ",es13.6,1x,&
!                        &"at Frequency ",f8.3,1x,&
!                        &"GHz. Setting to 0.0.")' ) WetContinuum_Absorption, Frequency(l)
!            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
!          END IF
!          WetContinuum_Absorption = ZERO
!        END IF
WetContinuum_Absorption = ZERO
!***TURNING OFF WET CONTINUUM***
        ! ...Compute the attenuation in dB/km
        WetContinuum_Attenuation(l) = ATT_K * Frequency(l) * WetContinuum_Absorption
      END DO WetContinuum_Frequency_Loop
    END IF WetContinuum_Compute


    ! Calculate the dry line attentuation for each frequency
    DryLine_Compute: IF ( Compute_DryLine ) THEN
      ! ...Calculate the O2 line strength, width, and overlap factors
      O2_s1 = Compute_O2_s1( Dry_Air_Pressure, Theta )
      O2_g1 = Compute_O2_g1( H2O_Pressure, Theta )
      O2_d1 = Compute_O2_d1( Dry_Air_Pressure + H2O_Pressure, Theta )
      ! ...Loop over the required frequencies
      DryLine_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the oxygen line absoprtion
        O2_Line_Absorption = Compute_O2_Line_Absorption( &
                               Frequency(l)    , &
                               Dry_Air_Pressure, &
                               Theta           , &
                               Common_s1       , &
                               O2_s1           , &
                               O2_g1           , &
                               O2_d1             )
        IF ( O2_Line_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative sum for O2 line absorption, ",es13.6,1x,&
                        &" at Frequency ",f8.3,1x,&
                        &"GHz. Setting to 0.0.")' ) O2_Line_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          O2_Line_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        DryLine_Attenuation(l) = ATT_K * Frequency(l) * O2_Line_Absorption
      END DO DryLine_Frequency_Loop
    END IF DryLine_Compute


    ! Calculate the dry continuum attentuation for each frequency
    DryContinuum_Compute: IF ( Compute_DryContinuum ) THEN
      ! ...Loop over the required frequencies
      DryContinuum_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the non-resonant dry air absorption
        DryContinuum_Absorption = Compute_Dry_Continuum( &
                                    Frequency(l)    , &
                                    Dry_Air_Pressure, &
                                    H2O_Pressure    , &
                                    Theta             )
        IF ( DryContinuum_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative sum for dry continuum absorption, ",es13.6,1x,&
                        &" at Frequency ",f8.3,1x,&
                        &"GHz. Setting to 0.0.")' ) DryContinuum_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          DryContinuum_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        DryContinuum_Attenuation(l) = ATT_K * Frequency(l) * DryContinuum_Absorption
      END DO DryContinuum_Frequency_Loop
    END IF DryContinuum_Compute

  END FUNCTION Liebe93_By_Frequency


  ! --------------------------------------
  ! Single-Frequency, Multi-Layer function
  ! --------------------------------------
  FUNCTION Liebe93_By_Layer( &
    Frequency               , &  ! Input
    Dry_Air_Pressure        , &  ! Input
    H2O_Pressure            , &  ! Input
    Temperature             , &  ! Input
    Quiet,                    &  ! Optional input
    WetLine_Attenuation     , &  ! Optional output
    WetContinuum_Attenuation, &  ! Optional output
    DryLine_Attenuation     , &  ! Optional output
    DryContinuum_Attenuation) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Frequency
    REAL(fp),           INTENT(IN)  :: Dry_Air_Pressure(:)
    REAL(fp),           INTENT(IN)  :: H2O_Pressure(:)
    REAL(fp),           INTENT(IN)  :: Temperature(:)
    LOGICAL , OPTIONAL, INTENT(IN)  :: Quiet
    REAL(fp), OPTIONAL, INTENT(OUT) :: WetLine_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: WetContinuum_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: DryLine_Attenuation(:)
    REAL(fp), OPTIONAL, INTENT(OUT) :: DryContinuum_Attenuation(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Liebe93::By_Layer'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    INTEGER :: n_Layers, k
    REAL(fp) :: Theta
    REAL(fp) :: O2_s1, Common_s1, O2_g1, O2_d1, H2O_s1
    REAL(fp) :: H2O_Line_Absorption
    REAL(fp) :: WetContinuum_Absorption
    REAL(fp) :: O2_Line_Absorption
    REAL(fp) :: DryContinuum_Absorption

    ! Setup
    err_stat = SUCCESS
    ! ...Check for consistent input array sizes
    n_Layers = SIZE(Dry_Air_Pressure)
    IF ( SIZE(H2O_Pressure) /= n_Layers .OR. &
         SIZE(Temperature ) /= n_Layers      ) THEN
      err_stat = FAILURE
      msg = 'Input argument arrays have inconsistent sizes.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Process keywords
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT.Quiet


    ! Check output argument validity
    ! ...The water vapour line attenuation
    Compute_WetLine = .FALSE.
    IF ( PRESENT(WetLine_Attenuation) ) THEN
      Compute_WetLine = .TRUE.
      IF ( SIZE(WetLine_Attenuation) /= n_layers ) THEN
        err_stat = FAILURE
        msg = 'WetLine_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour continuum attenuation
    Compute_WetContinuum = .FALSE.
    IF ( PRESENT(WetContinuum_Attenuation) ) THEN
      Compute_WetContinuum = .TRUE.
      IF ( SIZE(WetContinuum_Attenuation) /= n_layers ) THEN
        err_stat = FAILURE
        msg = 'WetContinuum_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour line attenuation
    Compute_DryLine = .FALSE.
    IF ( PRESENT(DryLine_Attenuation) ) THEN
      Compute_DryLine = .TRUE.
      IF ( SIZE(DryLine_Attenuation) /= n_layers ) THEN
        err_stat = FAILURE
        msg = 'DryLine_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...The water vapour continuum attenuation
    Compute_DryContinuum = .FALSE.
    IF ( PRESENT( DryContinuum_Attenuation ) ) THEN
      Compute_DryContinuum = .TRUE.
      IF ( SIZE( DryContinuum_Attenuation ) /= n_layers ) THEN
        err_stat = FAILURE
        msg = 'DryContinuum_Attenuation argument array has inconsistent size.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
      END IF
    END IF
    ! ...If no output arguments at all, return
    IF ( ( .NOT. Compute_WetLine      ) .AND. &
         ( .NOT. Compute_WetContinuum ) .AND. &
         ( .NOT. Compute_DryLine      ) .AND. &
         ( .NOT. Compute_DryContinuum )       ) RETURN
    ! ...Check for invalid frequency
    IF ( Frequency < ZERO ) THEN
      err_stat = FAILURE
        msg = 'Input frequency must be > 0.0.'
        CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Check for invalid profile data
    IF ( ANY(Dry_Air_Pressure < TOLERANCE) .OR. &
         ANY(H2O_Pressure     < TOLERANCE) .OR. &
         ANY(Temperature      < TOLERANCE)      ) THEN
      err_stat = FAILURE
      msg = 'Input tressure/temperature arguments must be > 0.0.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
         

    ! Issue warnings for data outside range
    IF ( noisy )  THEN
    
      ! Frequency < min value or > max_value
      IF ( Frequency < MIN_FREQUENCY ) THEN
        WRITE( msg,'("Input frequency < ",f6.1," GHz.")' ) MIN_FREQUENCY
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF
      IF ( Frequency > MAX_FREQUENCY ) THEN
        WRITE( msg,'("Input frequency > ",f6.1," GHz.")' ) MAX_FREQUENCY
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF

      ! Temperatures < min value or > max_value
      IF ( ANY(Temperature < MIN_TEMPERATURE) ) THEN
        WRITE( msg,'("Input temperatures < ",f6.1," K found.")' ) MIN_TEMPERATURE
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF
      IF ( ANY(Temperature > MAX_TEMPERATURE) ) THEN
        WRITE( msg,'("Input temperatures > ",f6.1," K found.")' ) MAX_TEMPERATURE
        CALL Display_Message( ROUTINE_NAME, msg, WARNING )
      END IF

    END IF


    ! Begin the loop over atmospheric layers
    Layer_Loop: DO k = 1, n_Layers


      ! Calculate the reciprocal temperature, theta,
      ! and common line strength factor, Common_s1
      Theta     = Compute_Theta( Temperature(k) )
      Common_s1 = Compute_Common_s1( Theta )


      ! Calculate the wet line attentuation
      WetLine_Compute: IF ( Compute_WetLine ) THEN
        ! ...Calculate the H2O absorption equation coefficients
        H2O_s1 = Compute_H2O_s1( H2O_Pressure(k), Theta )
        ! ...Water vapour line absorption
        H2O_Line_Absorption = Compute_H2O_line_Absorption( &
                                Frequency          , &
                                Dry_Air_Pressure(k), &
                                H2O_Pressure(k)    , &
                                Theta              , &
                                Common_s1          , &
                                H2O_s1               )
        IF ( H2O_Line_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative sum for H2O line absorption, ",es13.6,1x,&
                        &"in layer ",i0," for frequency ",f8.3,1x, &
                        &"GHz. Setting to 0.0.")' ) H2O_Line_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          H2O_Line_Absorption = ZERO
        END IF
        ! ...Calculate the attenuation in dB/km
        WetLine_Attenuation(k) = ATT_K * Frequency * H2O_Line_Absorption
      END IF WetLine_Compute
 

      ! Calculate the wet continuum attentuation
      WetContinuum_Compute: IF ( Compute_WetContinuum ) THEN
!***TURNING OFF WET CONTINUUM***
!        ! ...Compute the water vapour continuum
!        WetContinuum_Absorption = Compute_Wet_Continuum( &
!                                    Frequency          , &
!                                    Dry_Air_Pressure(k), &
!                                    H2O_Pressure(k)    , &
!                                    Theta                )
!        IF ( WetContinuum_Absorption < ZERO ) THEN
!          IF ( noisy ) THEN
!            WRITE( msg,'("Negative value for wet continuum absorption, ",es13.6,1x,&
!                        &"in layer ",i0," for frequency ",f8.3,1x, &
!                        &"GHz. Setting to 0.0.")' ) WetContinuum_Absorption, k, Frequency
!            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
!          END IF
!          WetContinuum_Absorption = ZERO
!        END IF
WetContinuum_Absorption = ZERO
!***TURNING OFF WET CONTINUUM***
        ! ...Compute the attenuation in dB/km
        WetContinuum_Attenuation(k) = ATT_K * Frequency * WetContinuum_Absorption
      END IF WetContinuum_Compute


      ! Calculate the dry line attentuation
      DryLine_Compute: IF ( Compute_DryLine ) THEN
        ! ...Calculate the O2 line strength, width, and overlap factors
        O2_s1 = Compute_O2_s1( Dry_Air_Pressure(k), Theta )
        O2_g1 = Compute_O2_g1( H2O_Pressure(k), Theta )
        O2_d1 = Compute_O2_d1( Dry_Air_Pressure(k) + H2O_Pressure(k), Theta )
        ! ...Summation of the absorption lines of oxygen
        O2_Line_Absorption = Compute_O2_Line_Absorption( &
                               Frequency          , &
                               Dry_Air_Pressure(k), &
                               Theta              , &
                               Common_s1          , &
                               O2_s1              , &
                               O2_g1              , &
                               O2_d1                )
        IF ( O2_Line_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative value for O2 line absorption, ",es13.6,1x,&
                        &"in layer ",i0," for frequency ",f8.3,1x, &
                        &"GHz. Setting to 0.0.")' ) O2_Line_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          O2_Line_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        DryLine_Attenuation(k) = ATT_K * Frequency * O2_Line_Absorption
      END IF DryLine_Compute


      ! Calculate the dry continuum attentuation
      DryContinuum_Compute: IF ( Compute_DryContinuum ) THEN
        ! ...Compute the non-resonant dry air absorption
        !    Formula from Liebe 89 paper + correction of Liebe himself
        DryContinuum_Absorption = Compute_Dry_Continuum( Frequency, &
                                                         Dry_Air_Pressure(k), &
                                                         H2O_Pressure(k), &
                                                         Theta )
        IF ( DryContinuum_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative value for dry continuum absorption, ",es13.6,1x,&
                        &"in layer ",i0," for frequency ",f8.3,1x, &
                        &"GHz. Setting to 0.0.")' ) DryContinuum_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          DryContinuum_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        DryContinuum_Attenuation(k) = ATT_K * Frequency * DryContinuum_Absorption
      END IF DryContinuum_Compute

    END DO Layer_Loop

  END FUNCTION Liebe93_By_Layer



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! --------------------------------
  ! Reference reciprocal temperature
  ! --------------------------------
  FUNCTION Compute_Theta( Temperature ) RESULT( Theta )
    REAL(fp), INTENT(IN) :: Temperature
    REAL(fp)             :: Theta
    REAL(fp), PARAMETER :: REFERENCE_TEMPERATURE = 300.0_fp
    Theta = REFERENCE_TEMPERATURE / Temperature
  END FUNCTION Compute_Theta


  ! --------------------------------------
  ! Common O2 and H2O line strength factor
  ! --------------------------------------
  FUNCTION Compute_Common_s1( Theta ) RESULT( Common_s1 )
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp)             :: Common_s1
    Common_s1 = ONE - Theta
  END FUNCTION Compute_Common_s1


  ! -----------------------
  ! O2 line strength factor
  ! -----------------------
  FUNCTION Compute_O2_s1( Dry_Air_P, Theta ) RESULT( O2_s1 )
    REAL(fp), INTENT(IN) :: Dry_Air_P
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp)             :: O2_s1
    O2_s1 = Dry_Air_P * ( Theta**THREE )
  END FUNCTION Compute_O2_s1


  ! --------------------
  ! O2 line width factor
  ! --------------------
  FUNCTION Compute_O2_g1( H2O_P, Theta ) RESULT( O2_g1 )
    REAL(fp), INTENT(IN) :: H2O_P
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp)             :: O2_g1
    O2_g1 = ONEpointONE * H2O_P * Theta
  END FUNCTION Compute_O2_g1


  ! ----------------------
  ! O2 line overlap factor
  ! ----------------------
  FUNCTION Compute_O2_d1( Total_P, Theta ) RESULT( O2_d1 )
    REAL(fp), INTENT(IN) :: Total_P
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp)             :: O2_d1
    O2_d1 = Total_P * ( Theta**ZEROpointEIGHT )
  END FUNCTION Compute_O2_d1


  ! ------------------------
  ! H2O line strength factor
  ! ------------------------
  FUNCTION Compute_H2O_s1( H2O_P, Theta ) RESULT( H2O_s1 )
    REAL(fp), INTENT(IN) :: H2O_P
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp)             :: H2O_s1
    H2O_s1 = H2O_P * ( Theta**THREEpointFIVE )
  END FUNCTION Compute_H2O_s1


!------------------------------------------------------------------------------
!
! NAME:
!       Compute_O2_Line_Absorption
!
! PURPOSE:
!       Function to calculate the oxygen line terms of the imaginary part of
!       the atmospheric refractivity.
!
! CALLING SEQUENCE:
!       O2_Line_Absorption = Compute_O2_Line_Absorption( f,         &  ! Input
!                                                        pd,        &  ! Input
!                                                        Theta,     &  ! Input
!                                                        Common_s1, &  ! Input
!                                                        O2_s1,     &  ! Input
!                                                        O2_g1,     &  ! Input
!                                                        O2_d1      )  ! Input
!
! INPUT ARGUMENTS:
!       f:                   Frequency
!                            UNITS:      GHz
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       pd:                  Dry air partial pressure.
!                            UNITS:      hectoPascals, hPa
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       Theta:               Reciprocal temperature ratio, 300/T
!                            UNITS:      None.
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       Common_s1:           Line strength factor common to both O2 and H2O
!                            forumlations, (1 - Theta)
!                            UNITS:      None.
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       O2_s1:               O2 line strength factor, pd * Theta^3
!                            UNITS:      kPa
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       O2_g1:               O2 line width factor, 1.1 * pw * Theta
!                            UNITS:      kPa
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
!       O2_d1:               O2 line overlap factor, pd * Theta^0.8
!                            UNITS:      kPa
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       O2_Line_Absorption:  Dry air refractivity due to O2 line
!                            absorption            
!                            UNITS:      ppm
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!
! PROCEDURE:
!       Liebe, H.J., G.A. Hufford, and M.G. Cotton, 1993: "Propagation
!         modelling of moist air and suspended water/ice particles at
!         frequencies below 1000 GHz". In AGARD Conference Proceedings
!         542, Atmospheric propagation effects through natural and man-made
!         obscurants for visible through MM-wave radiation, pp 3.1-3.10.
!         Presented at the Electromagnetic Wave Propagation Panel Symposium,
!         Palma de Mallorca, Spain, 17-20 May 1993.
!
!       Synopsis:
!
!       The imaginary part of the refractivity due to O2 line absorption in    
!       moist air can be expressed by,                                         
!               __                                                             
!              \                                                               
!         ND =  > S(f).F(f)    ppm  .....(2)                                   
!              /__                                                             
!                i                                                             
!                                                                              
!       with F being the line shape function derived from the complex function 
!       shown in eqn.(3) of the MPM93 paper,                                   
!                                                                              
!                f   [    g - d.(fo - f)         g - d.(fo + f)    ]           
!        F(f) = ---- [ -------------------- + -------------------- ]           
!                fo  [  ( fo - f )^2 + g^2     ( fo + f )^2 + g^2  ]           
!                                                                              
!       where i = O2 line index                                                
!             f = frequency                                                    
!             S = O2 line strength                                             
!             g = O2 line width (gamma)                                        
!             d = O2 line overlap (delta)                                      
!                                                                              
!       The line strength, width, and overlap are modeled using,               
!                                                                              
!         S = a1 . pd . theta^3 . EXP[ a2 . ( 1 - theta ) ]                    
!                                                                              
!         g = a3.10^-3 ( pd.theta^a4 + 1.1.pw.theta )                          
!                                                                              
!         d = ( a5 + a6.theta ) . pd . theta^0.8                               
!                                                                              
!       with a1,a2 = line strength coefficients                                
!            a3,a4 = line width coefficients                                   
!            a5,a6 = line overlap coefficients                                 
!            pd    = dry gas partial pressure                                  
!            pw    = water vapor partial pressure                              
!            theta = reciprocal temperature ratio, 300/T                       
!                                                                              
!       A rough estimate of line behaviour in the mesosphere is obtained by    
!       g with gh where                                                        
!                 _______________                                              
!                /                                                             
!         gh = \/ g^2 + 625.B^2                                                
!                                                                              
!       where B = the magnetic field strength (22-65uT) depending on the       
!                 geographic location and altitude                             
!                                                                              
!       The Zeeman parameter approximates this solution.                       
!                                                                              
!       All equation numbers refer to the referenced Liebe93 paper.            
!
!              
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 13-Dec-1993
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION Compute_O2_Line_Absorption( f,         &  ! Input
                                       pd,        &  ! Input
                                       Theta,     &  ! Input
                                       Common_s1, &  ! Input
                                       O2_s1,     &  ! Input
                                       O2_g1,     &  ! Input
                                       O2_d1 )    &  ! Input
                                     RESULT( O2_Line_Absorption )
    ! Arguments
    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp), INTENT(IN) :: Common_s1
    REAL(fp), INTENT(IN) :: O2_s1
    REAL(fp), INTENT(IN) :: O2_g1
    REAL(fp), INTENT(IN) :: O2_d1
    ! Function result
    REAL(fp) :: O2_Line_Absorption
    ! Local parameters
    REAL(fp), PARAMETER :: ZEEMAN_K = ( 25.0_fp * 0.6e-04_fp )**2
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Si
    REAL(fp) :: Gamma, Gamma2
    REAL(fp) :: Delta
    REAL(fp) :: f_diff, f_sum
    REAL(fp) :: Fi_n1, Fi_n2
    REAL(fp) :: Fi_d1, Fi_d2
    REAL(fp) :: Fi


    ! Initialise the return value
    ! ---------------------------
    O2_Line_Absorption = ZERO      

    ! Loop over the oxygen lines
    ! --------------------------
    DO i = 1, N_O2_LINES

      ! Calculate the line strength
      ! ---------------------------
      Si = O2_A1(i) * O2_s1 * EXP( O2_A2(i) * Common_s1 )

      ! Calculate the line width and overlap parameters
      ! -----------------------------------------------
      ! Calculate the pressure-broadened line width
      Gamma  = O2_A3(i) * ( ( pd * ( Theta**(ZEROpointEIGHT - O2_A4(i)) ) ) + O2_g1 )
      ! Zeeman effect in the mesosphere due to magnetic field 
!!      Gamma  = SQRT( Gamma**2 + ZEEMAN_K )
      ! Compute the square of the line width
      Gamma2 = Gamma * Gamma
      ! Calculate the line overlap (interference)
      Delta = ( O2_A5(i) + ( O2_A6(i) * Theta ) ) * O2_d1

      ! Calculate the line shape, F
      ! ---------------------------
      ! Frequency difference and sum from the line frequency
      f_diff = O2_LINE_FREQUENCY(i) - f
      f_sum  = O2_LINE_FREQUENCY(i) + f
      ! The numerator of the line shape function components
      Fi_n1 = Gamma - ( Delta * f_diff )
      Fi_n2 = Gamma - ( Delta * f_sum  )
      ! The denominator of the line shape components
      Fi_d1 = ( f_diff * f_diff ) + Gamma2
      Fi_d2 = ( f_sum  * f_sum  ) + Gamma2
      ! Calculate the absorption line shape
      Fi = ( f / O2_LINE_FREQUENCY(i) ) * ( ( Fi_n1/Fi_d1 ) + ( Fi_n2/Fi_d2 ) )

      ! Calculate the imaginary component of the
      ! dry air refractivity - the absorption
      ! ----------------------------------------
      O2_Line_Absorption = O2_Line_Absorption + ( Si * Fi )

    END DO

  END FUNCTION Compute_O2_Line_Absorption




!------------------------------------------------------------------------------
!
! NAME:
!       Compute_Dry_Continuum
!
! PURPOSE:
!       Function to calculate the nonresonant imaginary term of the dry air
!       refractivity.
!
! CALLING SEQUENCE:
!       Dry_Continuum = Compute_Dry_Continuum( f,         &  ! Input
!                                              pd,        &  ! Input
!                                              pw,        &  ! Input
!                                              Theta )    &  ! Input
!
! INPUT ARGUMENTS:
!       f:                 Frequency
!                          UNITS:      GHz
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       pd:                Dry air partial pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       pw:                Water vapor partial pressure.
!                          UNITS:      hectoPascals, hPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Theta:             Reciprocal temperature ratio, 300/T
!                          UNITS:      None.
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Dry_Continuum:     Nonresonant refractivity term for dry air
!                          UNITS:      ppm
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!
! PROCEDURE:
!       Liebe, H.J., G.A. Hufford, and M.G. Cotton, 1993: "Propagation
!         modelling of moist air and suspended water/ice particles at
!         frequencies below 1000 GHz". In AGARD Conference Proceedings
!         542, Atmospheric propagation effects through natural and man-made
!         obscurants for visible through MM-wave radiation, pp 3.1-3.10.
!         Presented at the Electromagnetic Wave Propagation Panel Symposium,
!         Palma de Mallorca, Spain, 17-20 May 1993.
!
!       Synopsis:
!
!       Nonresonant refractivity of dry air makes a small contribution at    
!       surface pressures due to the Debye spectrum of oxygen below 10GHz and
!       the pressure-induced nitrogen absorption that becomes effective above
!       100GHz. The functional form is,                                      
!                                                                            
!         Nn = So.Fo(f) + i.Sn.Fn"(f)     ppm                                
!                                                                            
!       where f     = frequency,                                             
!             So,Fo = nonresonant O2 spectrum contribution,                  
!             Sn,Fn = pressure-induced N2 absorption contribution            
!                                                                            
!       and the imaginary part of Nn provides the attenuated power due to the
!       dry continuum term.                                                  
!                                                                            
!                                                                            
!       The non-resonant O2 strength and lineshape are given by,             
!                                                                            
!         So = 6.14x10^-5 . pd . theta^2                                     
!                                                                            
!       and                                                                  
!                                                                            
!                  -f                                                        
!         Fo = ----------                                                    
!               f + i.go                                                     
!                                                                            
!       where go = relaxation frequency                                      
!                = 0.56x10^-3.p.theta^0.8                                    
!             p  = Total pressure                                            
!                                                                            
!       The imaginary part of the non-resonant lineshape is therefore,       
!                                                                            
!                  f.go                                                      
!         Fo = ------------                                                  
!               f^2 + go^2                                                   
!                                                                            
!                                                                            
!       The strength and line shape of the N2 absorption term is given by,   
!                                                                            
!         Sn = 1.40x10^-12 . pd^2 . theta^3.5                                
!                                                                            
!       and                                                                  
!                                                                            
!                       f                                                    
!         Fn ~ ---------------------------                                   
!               1 + ( 1.9x10^-5 . f^1.5 )
!
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 13-Dec-1993
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!
!------------------------------------------------------------------------------

  FUNCTION Compute_Dry_Continuum( f,       &  ! Input
                                  pd,      &  ! Input
                                  pw,      &  ! Input
                                  Theta )  &  ! Input
                                RESULT( Dry_Continuum )
    ! Arguments
    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: Theta
    ! Function result
    REAL(fp) :: Dry_Continuum
    ! Parameters
    REAL(fp), PARAMETER :: So_K    = 6.14e-05_fp
    REAL(fp), PARAMETER :: GAMMA_K = 5.6e-04_fp
    REAL(fp), PARAMETER :: Sn_K    = 1.4e-12_fp
    REAL(fp), PARAMETER :: Fn_K    = 1.93e-05_fp
    ! Local variables
    REAL(fp) :: Gamma
    REAL(fp) :: So, Fo
    REAL(fp) :: Sn, Fn

    ! Calculate the effect of the non-resonant oxygen spectrum
    ! ...Calculate the strength
    So = So_K * pd * ( Theta**2 )
    ! ...Calculate the width
    Gamma = GAMMA_K * ( pd + pw ) * ( Theta**ZEROpointEIGHT )
    ! ...Calculate the lineshape
    Fo = ( f * Gamma ) / ( f**2 + Gamma**2 )

    ! Calculate the effect of pressure induced N2 absorption
    ! ...Calculate the N2 absorption strength
    Sn = Sn_K * ( pd**2 ) * ( theta**THREEpointFIVE )
    ! ...Calculate the shape
    Fn = f / ( ONE + ( Fn_K * ( f**ONEpointFIVE ) ) )

    ! Sum the components
    Dry_Continuum = ( So * Fo ) + ( Sn * Fn )

  END FUNCTION Compute_Dry_Continuum



!------------------------------------------------------------------------------
!
! NAME:
!       Compute_H2O_Line_Absorption
!
! PURPOSE:
!       Function to calculate the water vapor line terms of the imaginary part
!       of the atmospheric refractivity.
!
!       A continuum term is included by considering a pseudo-line centred out
!       of band at 1780GHz. The parameters for this pseudo-line are in the
!       coefficient data modules.
!
! CALLING SEQUENCE:
!       H2O_Line_Absorption = Compute_H2O_Line_Absorption( f,         &  ! Input
!                                                          pd,        &  ! Input
!                                                          pw,        &  ! Input
!                                                          Theta,     &  ! Input
!                                                          Common_s1, &  ! Input
!                                                          H2O_s1 )   &  ! Input
!
! INPUT ARGUMENTS:
!       f:                 Frequency
!                          UNITS:      GHz
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       pd:                Dry air partial pressure.
!                          UNITS:      kiloPascals, kPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       pw:                Water vapor partial pressure.
!                          UNITS:      kiloPascals, kPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Theta:             Reciprocal temperature ratio, 300/T
!                          UNITS:      None.
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Common_s1:         Line strength factor common to both H2O and H2O
!                          forumlations, (1 - Theta)
!                          UNITS:      None.
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       H2O_s1:            H2O line strength factor, pw * Theta^3.5
!                          UNITS:      kPa
!                          TYPE:       REAL(fp)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       H2O_Line_Absorption: Atmospheric refractivity due to H2O line
!                            absorption
!                            UNITS:      ppm
!                            TYPE:       REAL(fp)
!                            DIMENSION:  Scalar
!
! PROCEDURE:
!       Liebe, H.J., G.A. Hufford, and M.G. Cotton, 1993: "Propagation
!         modelling of moist air and suspended water/ice particles at
!         frequencies below 1000 GHz". In AGARD Conference Proceedings
!         542, Atmospheric propagation effects through natural and man-made
!         obscurants for visible through MM-wave radiation, pp 3.1-3.10.
!         Presented at the Electromagnetic Wave Propagation Panel Symposium,
!         Palma de Mallorca, Spain, 17-20 May 1993.
!
!       Synopsis:
!
!       The imaginary part of the refractivity due to H2O line absorption in    
!       moist air can be expressed by,                                          
!               __                                                              
!              \                                                                
!         NW =  > S(f).F(f)    ppm  .....(5)                                    
!              /__                                                              
!                i                                                              
!                                                                               
!       with F being the line shape function derived from the complex function  
!       shown in eqn.(3) of the MPM93 paper,                                    
!                                                                               
!                f   [    g - d.(fo - f)         g - d.(fo + f)    ]            
!        F(f) = ---- [ -------------------- + -------------------- ]            
!                fo  [  ( fo - f )^2 + g^2     ( fo + f )^2 + g^2  ]            
!                                                                               
!       where i = H2O line index                                                
!             f = frequency                                                     
!             S = H2O line strength                                             
!             g = H2O line width (gamma)                                        
!             d = H2O line overlap (delta)                                      
!                                                                               
!       The line strength, width, and overlap are modeled using,                
!                                                                               
!         S = a1 . pw . theta^3.5 . EXP[ a2 . ( 1 - theta ) ]                   
!                                                                               
!         g = a3.10^-3 ( a4.pw.theta^a6 + pd.theta^a5 )                         
!                                                                               
!         d = 0                                                                 
!                                                                               
!       with a1,a2       = line strength coefficients                           
!            a3,a4,a5,a6 = line width coefficients                              
!            pd          = dry gas partial pressure                             
!            pw          = water vapor partial pressure                         
!            theta       = reciprocal temperature ratio, 300/T                  
!                                                                               
!       Line overlap (delta) is neglected (d(i) = 0) and Doppler-broadening     
!       is approximated for pressures below 0.7mb by                            
!                                  _______________________                      
!                                 /                                             
!         g*(i) = 0.535.g(i) +   /  0.217.g(i)^2 + g(D)^2                       
!                              \/                                               
!                                                                               
!       where g(D) = Doppler width                                              
!                                                                               
!                     1.46x10^-6 . fo(i)                                        
!                  = --------------------                                       
!                          ______                                               
!                         /                                                     
!                       \/ theta                                                
!                                                                               
!       Note that the water vapor continuum is modeled by considering a pseudo- 
!       line centred out of band above 1 THz. Thus there is an extra line in the
!       coefficient data for a line centred at 1780GHz.                                                    
!
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 13-Dec-1993
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!
!------------------------------------------------------------------------------

  FUNCTION Compute_H2O_Line_Absorption( f,         &  ! Input
                                        pd,        &  ! Input
                                        pw,        &  ! Input
                                        Theta,     &  ! Input
                                        Common_s1, &  ! Input
                                        H2O_s1 )   &  ! Input
                                      RESULT( H2O_Line_Absorption )
    ! Arguments
    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: Theta
    REAL(fp), INTENT(IN) :: Common_s1
    REAL(fp), INTENT(IN) :: H2O_s1
    ! Function result
    REAL(fp) :: H2O_Line_Absorption
    ! Local parameters
    REAL(fp), PARAMETER :: GAMMA_D_K1 = 1.46e-06_fp
    REAL(fp), PARAMETER :: GAMMA_D_K2 = 0.535_fp
    REAL(fp), PARAMETER :: GAMMA_D_K3 = 0.217_fp
    ! Local variables
    INTEGER :: i
    REAL(fp) :: Si
    REAL(fp) :: Gamma, Gamma2, GammaD2
    REAL(fp) :: f_diff, f_sum
    REAL(fp) :: Fi_d1, Fi_d2
    REAL(fp) :: Fi


    ! Initialise the return value
    H2O_Line_Absorption = ZERO


    ! Loop over water vapour lines
    DO i = 1, N_H2O_LINES


      ! Calculate the line strength
      Si = H2O_A1(i) * H2O_s1 * EXP( H2O_A2(i) * Common_s1 )


      ! Calculate the line width
      ! ...Calculate the pressure-broadened width
      Gamma = H2O_A3(i) * ( (             pd * ( Theta**H2O_A5(i) ) ) + &
                            ( H2O_A4(i) * pw * ( Theta**H2O_A6(i) ) )   )
      ! ...Calculate the square of the Doppler width
      GammaD2 = ( ( GAMMA_D_K1 * H2O_LINE_FREQUENCY(i) )**2 ) / Theta
      ! ...Approximate the Doppler broadening
      Gamma   = ( GAMMA_D_K2 * Gamma ) + SQRT( ( GAMMA_D_K3 * Gamma**2 ) + GammaD2 )
      Gamma2  = Gamma * Gamma


      ! Calculate the line shape, F
      ! ...Frequency difference and sum from the line frequency
      f_diff = H2O_LINE_FREQUENCY(i) - f
      f_sum  = H2O_LINE_FREQUENCY(i) + f
      ! ...The denominator of the line shape components
      Fi_d1 = ( f_diff * f_diff ) + Gamma2
      Fi_d2 = ( f_sum  * f_sum  ) + Gamma2
      ! ...Calculate the absorption line shape
      Fi = ( f / H2O_LINE_FREQUENCY(i) ) * ( ( Gamma/Fi_d1 ) + ( Gamma/Fi_d2 )   )


      ! Calculate the imaginary component of the
      ! water vapor refractivity - the absorption
      H2O_Line_Absorption = H2O_Line_Absorption + ( Si * Fi )

    END DO

  END FUNCTION Compute_H2O_Line_Absorption

END MODULE MWLBL_Liebe93
