!
! MWLBL_Rosenkranz03
!
! Module containing data and routines to calculate microwave atmospheric
! attenuation according to the Rosenkranz 2003 LBL code.
!
!
! CREATION HISTORY:
!       Some subprograms in this module have been translated from the
!       FORTRAN-77 source code written by P.W.Rosenkranz, 1988-2003
!
!       Written by:     Paul van Delst, 08-Nov-2004
!                       paul.vandelst@noaa.gov
!

MODULE MWLBL_Rosenkranz03

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Fundamental_Constants, ONLY: E_LOG10, PI_RECIPROCAL
  USE Units_Conversion     , ONLY: PP_to_ND
  USE Rosenkranz03_Coefficients
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Rosenkranz03


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE Rosenkranz03
    MODULE PROCEDURE Rosenkranz03_By_Layer
    MODULE PROCEDURE Rosenkranz03_By_Frequency
  END INTERFACE Rosenkranz03


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constants
  REAL(fp), PRIVATE, PARAMETER :: ZERO           =  0.0_fp
  REAL(fp), PRIVATE, PARAMETER :: ZEROpointFIVE  =  0.5_fp
  REAL(fp), PRIVATE, PARAMETER :: ONE            =  1.0_fp
  REAL(fp), PRIVATE, PARAMETER :: ONEpointONE    =  1.1_fp
  REAL(fp), PRIVATE, PARAMETER :: TWOpointFIVE   =  2.5_fp
  REAL(fp), PRIVATE, PARAMETER :: THREE          =  3.0_fp
  REAL(fp), PRIVATE, PARAMETER :: SEVENpointFIVE =  7.5_fp
  REAL(fp), PRIVATE, PARAMETER :: TEN            = 10.0_fp
  REAL(fp), PRIVATE, PARAMETER :: TOLERANCE      = EPSILON(ZERO)
  ! Conversion factor for Np->dB, 10 * LOG10(e)
  REAL(fp), PRIVATE, PARAMETER :: NP_TO_DB = TEN * E_LOG10
  ! Min/max temperatures in Kelvin
  REAL(fp), PRIVATE, PARAMETER :: MIN_TEMPERATURE = 150.0_fp
  REAL(fp), PRIVATE, PARAMETER :: MAX_TEMPERATURE = 350.0_fp
  ! Min/max frequencies in GHz
  REAL(fp), PRIVATE, PARAMETER :: MIN_FREQUENCY = ONE
  REAL(fp), PRIVATE, PARAMETER :: MAX_FREQUENCY = 800.0_fp
  ! Default message string length
  INTEGER,  PARAMETER :: ML = 256


CONTAINS


!###############################################################################
!###############################################################################
!##                                                                           ##
!##                        ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                           ##
!###############################################################################
!###############################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Rosenkranz03
!
! PURPOSE:
!       Function to calculate the atmospheric attentuation in the microwave
!       according to Rosenkranz's 2003 model.
!
! CALLING SEQUENCE:
!       Error_Status = Rosenkranz03( &
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
!                                   DIMENSION:  Scalar
!                                                 OR
!                                               Rank-1, n_Frequencies
!                                   ATTRIBUTES: INTENT(IN)
!
!       Dry_Air_Pressure:           Dry air partial pressure profile.
!                                   UNITS:      hectoPascals, hPa
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1, n_Layers
!                                                 OR
!                                               Scalar
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
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WetContinuum_Attenuation:   Power attenuation due to water vapour continuum
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DryLine_Attenuation:        Power attenuation due to absorption by oxygen
!                                   absorption lines.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       DryContinuum_Attenuation:   Power attenuation due to non-resonant dry air
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL(fp)
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:               The return value is an integer defining the error status.
!                                   status. The error codes are defined in the Message_Handler
!                                   module.
!                                   If == SUCCESS the data write was successful
!                                      == FAILURE an unrecoverable error occurred.
!                                   UNITS:      N/A
!                                   TYPE:       INTEGER
!                                   DIMENSION:  Scalar
!                           
! PROCEDURE:
!       H2O absorption model:
!       ---------------------
!       P.W. Rosenkranz, CHAP. 2 and appendix, in "Atmospheric Remote Sensing
!         by Microwave Radiometry" (M.A. Janssen, ed., 1993).
!       P.W. Rosenkranz, Radio Science, v33, pp919-928 (1998); v34, p1025 (1999).
!       A.Bauer et al. ASA Workshop (Sept. 1989) (380GHz).
!       M. Tretyakov et al., J. Mol. Spect. (2003)
!
!       O2 absorption model:
!       --------------------
!       P.W. Rosenkranz, CHAP. 2 and appendix, in "Atmospheric Remote Sensing
!         by Microwave Radiometry" (M.A. Janssen, ed., 1993).
!       H.J. Liebe et al, JQSRT v48, pp629-643 (1992).
!       M.J. Schwartz, Ph.D. thesis, M.I.T. (1998).
!       A.F. Krupnov et al, J. Mol. Spect. v215, pp309-311 (2002).
!       M.Yu. Tretyakov et al, J. Mol. Spect. (2003 preprint).
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Rosenkranz03_By_Frequency( &
    Frequency,                &  ! Input
    Dry_Air_Pressure,         &  ! Input
    H2O_Pressure,             &  ! Input
    Temperature ,             &  ! Input
    Quiet,                    &  ! Optional input
    WetLine_Attenuation,      &  ! Optional output
    WetContinuum_Attenuation, &  ! Optional output
    DryLine_Attenuation,      &  ! Optional output
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
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Rosenkranz03::By_Frequency'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    INTEGER :: n_Frequencies, l
    REAL(fp) :: Theta
    REAL(fp) :: H2O_Line_Absorption
    REAL(fp) :: WetContinuum_Absorption
    REAL(fp) :: O2_Line_Absorption
    REAL(fp) :: DryContinuum_Absorption


    ! Setup
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


    ! Compute the reciprocal temperature
    Theta = Compute_Theta( Temperature )


    ! Calculate the wet-line attenuation
    IF ( Compute_WetLine ) THEN
      WetLine_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the water vapour line absoprtion
        H2O_Line_Absorption = Compute_H2O_Line_Absorption( &
                                Frequency(l)    , &
                                Dry_Air_Pressure, &
                                H2O_Pressure    , &
                                Temperature     , &
                                Theta             )
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
        WetLine_Attenuation(l) = NP_TO_DB * H2O_Line_Absorption
      END DO WetLine_Frequency_Loop
    END IF


    ! Calculate the wet continuum attentuation
    IF ( Compute_WetContinuum ) THEN
      WetContinuum_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the water vapour continuum absoprtion
        WetContinuum_Absorption = Compute_Wet_Continuum( &
                                    Frequency(l)    , &
                                    Dry_Air_Pressure, &
                                    H2O_Pressure    , &
                                    Theta             )
        IF ( WetContinuum_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative value for wet continuum absorption, ",es13.6,1x,&
                        &"at Frequency ",f8.3,1x,&
                        &"GHz. Setting to 0.0.")' ) WetContinuum_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          WetContinuum_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        WetContinuum_Attenuation(l) = NP_TO_DB * WetContinuum_Absorption
      END DO WetContinuum_Frequency_Loop

    END IF


    ! Calculate the dry-line attenuation
    IF ( Compute_DryLine ) THEN
      DryLine_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the oxygen line absoprtion
        O2_Line_Absorption = Compute_O2_Line_Absorption( &
                               Frequency(l)    , &
                               Dry_Air_Pressure, &
                               H2O_Pressure    , &
                               Theta             ) 
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
        DryLine_Attenuation(l) = NP_TO_DB * O2_Line_Absorption
      END DO DryLine_Frequency_Loop
    END IF


    ! Calculate the dry continuum attenuation
    IF ( Compute_DryContinuum ) THEN
      DryContinuum_Frequency_Loop: DO l = 1, n_Frequencies
        ! ...Compute the non-resonant dry air absorption
        DryContinuum_Absorption = Compute_Dry_Continuum( &
                                    Frequency(l), &
                                    Dry_Air_Pressure, &
                                    H2O_Pressure, &
                                    Theta )
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
        DryContinuum_Attenuation( l ) = NP_TO_DB * DryContinuum_Absorption
      END DO DryContinuum_Frequency_Loop
    END IF

  END FUNCTION Rosenkranz03_By_Frequency




  FUNCTION Rosenkranz03_By_Layer( &
    Frequency,                &  ! Input
    Dry_Air_Pressure,         &  ! Input
    H2O_Pressure,             &  ! Input
    Temperature ,             &  ! Input
    Quiet,                    &  ! Optional input
    WetLine_Attenuation,      &  ! Optional output
    WetContinuum_Attenuation, &  ! Optional output
    DryLine_Attenuation,      &  ! Optional output
    DryContinuum_Attenuation) &  ! Optional output
  RESULT ( err_stat )
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
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Rosenkranz03::By_Layer'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    INTEGER :: n_Layers, k
    REAL(fp) :: Theta
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


      ! Calculate the reciprocal temperature, Theta
      Theta  = Compute_Theta( Temperature(k) )


      ! Calculate the wet-line attenuation
      IF ( Compute_WetLine ) THEN
        ! ...Compute the water vapour line absoprtion
        H2O_Line_Absorption = Compute_H2O_Line_Absorption( &
                                Frequency          , &
                                Dry_Air_Pressure(k), &
                                H2O_Pressure(k)    , &
                                Temperature(k)     , &
                                Theta                )
        IF ( H2O_Line_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative sum for H2O line absorption, ",es13.6,1x,&
                        &"in layer ",i0," for frequency ",f8.3,1x, &
                        &"GHz. Setting to 0.0.")' ) H2O_Line_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          H2O_Line_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        WetLine_Attenuation(k) = NP_TO_DB * H2O_Line_Absorption
      END IF


      ! Calculate the wet continuum attentuation
      IF ( Compute_WetContinuum ) THEN
        ! ...Compute the water vapour continuum
        WetContinuum_Absorption = Compute_Wet_Continuum( &
                                    Frequency          , &
                                    Dry_Air_Pressure(k), &
                                    H2O_Pressure(k)    , &
                                    Theta                )
        IF ( WetContinuum_Absorption < ZERO ) THEN
          IF ( noisy ) THEN
            WRITE( msg,'("Negative value for wet continuum absorption, ",es13.6,1x,&
                        &"in layer ",i0," for frequency ",f8.3,1x, &
                        &"GHz. Setting to 0.0.")' ) WetContinuum_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, msg, WARNING )
          END IF
          WetContinuum_Absorption = ZERO
        END IF
        ! ...Compute the attenuation in dB/km
        WetContinuum_Attenuation(k) = NP_TO_DB * WetContinuum_Absorption
      END IF


      ! Calculate the dry-line attenuation
      IF ( Compute_DryLine ) THEN
        ! ...Compute the oxygen line absoprtion
        O2_Line_Absorption = Compute_O2_Line_Absorption( &
                               Frequency          , &
                               Dry_Air_Pressure(k), &
                               H2O_Pressure(k)    , &
                               Theta                )
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
        DryLine_Attenuation(k) = NP_TO_DB * O2_Line_Absorption
      END IF


      ! Calculate the dry continuum attenuation
      IF ( Compute_DryContinuum ) THEN
        ! ...Compute the non-resonant dry air absorption
        DryContinuum_Absorption = Compute_Dry_Continuum( &
                                    Frequency          , &
                                    Dry_Air_Pressure(k), &
                                    H2O_Pressure(k)    , &
                                    Theta                )
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
        DryContinuum_Attenuation(k) = NP_TO_DB * DryContinuum_Absorption
      END IF

    END DO Layer_Loop

  END FUNCTION Rosenkranz03_By_Layer


!###############################################################################
!###############################################################################
!##                                                                           ##
!##                       ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                           ##
!###############################################################################
!###############################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_O2_Line_Absorption
!
! PURPOSE:
!       Function to calculate the oxygen line terms of the absorption
!       coefficient of oxygen using the Rosenkranz03 parameterisation.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Absorption = Compute_O2_Line_Absorption( f,    &  ! Input
!                                                pd,   &  ! Input
!                                                pw,   &  ! Input
!                                                Theta )  ! Input
!
! INPUT ARGUMENTS:
!       f:           Frequency
!                    UNITS:      GHz
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Absorption:  The absorption coefficient of oxygen due to
!                    line contributions.
!                    UNITS:      Nepers/km
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       Rosenkranz03 parameterisation. Adapted from o2abs.for Fortran77
!
! CREATION HISTORY:
!       F77 version written by : P.Rosenkranz 01-May-1995 to 20-Mar-2003
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 16-Nov-2004
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_O2_Line_Absorption( f,      &  ! Input
                                       pd,     &  ! Input
                                       pw,     &  ! Input
                                       Theta ) &  ! Input
                                     RESULT ( Absorption )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL(fp) :: Absorption


    ! ---------------
    ! Local prameters
    ! ---------------

    REAL(fp), PARAMETER :: MAGNITUDE_SCALE_FACTOR = 0.001_fp


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i
    REAL(fp) :: Pressure 
    REAL(fp) :: Theta08, Theta09, ONEminusTheta
    REAL(fp) :: H2O_Gamma_Component, Gamma08, Gamma09
    REAL(fp) :: Si
    REAL(fp) :: Gamma, Gamma2
    REAL(fp) :: Delta
    REAL(fp) :: f_diff, f_sum
    REAL(fp) :: Fi_n1, Fi_n2
    REAL(fp) :: Fi_d1, Fi_d2
    REAL(fp) :: Fi



    !#--------------------------------------------------------------------------#
    !#                      -- INITIALISE SOME VARIABLES --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the absorption sum
    ! -----------------------------

    Absorption = ZERO


    ! ------------------
    ! The total pressure
    ! ------------------

    Pressure = pd + pw


    ! ----------------------------------------
    ! Inverse reference temperature variations
    ! ----------------------------------------

    Theta08       = Theta**O2_X08
    Theta09       = Theta**O2_X09
    ONEminusTheta = ONE - Theta


    !#--------------------------------------------------------------------------#
    !#                    -- COMPUTE GENERIC O2 LINE WIDTH --                   #
    !#--------------------------------------------------------------------------#

    H2O_Gamma_Component = ONEpointONE * pw * Theta
    Gamma08 = MAGNITUDE_SCALE_FACTOR * ( ( pd * Theta08 ) + H2O_Gamma_Component )
    Gamma09 = MAGNITUDE_SCALE_FACTOR * ( ( pd * Theta09 ) + H2O_Gamma_Component )



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER THE O2 LINES --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, N_O2_LINES


      ! ---------------------------
      ! Calculate the line strength
! partial EQN 13 in Liebe92
      ! ---------------------------

      Si = O2_LINE_INTENSITY(i) * EXP( O2_B(i) * ONEminusTheta )


      ! -----------------------------------------------
      ! Calculate the line width and overlap parameters
      ! -----------------------------------------------

      ! -- Select temperature dependent line width
      IF ( i > 1 ) THEN
        Gamma = Gamma08  ! For all O2 lines except the 1- line
      ELSE
        Gamma = Gamma09  ! For the 1- O2 line
      END IF

      ! -- Calculate the pressure-broadened line width
      Gamma  = O2_W(i) * Gamma
      Gamma2 = Gamma * Gamma

      ! -- Calculate the line overlap (interference)
! EQN 2A.6 in Janssen
! Slightly modified to use ONEminusTheta rather than ThetaminusONE
      Delta = MAGNITUDE_SCALE_FACTOR * Pressure * Theta08 * ( O2_Y(i) - ( O2_V(i)*ONEminusTheta ) )


      ! ---------------------------
      ! Calculate the line shape, F
      ! ---------------------------

      ! -- Frequency difference and sum from the line frequency
      f_diff = f - O2_LINE_FREQUENCY(i)
      f_sum  = f + O2_LINE_FREQUENCY(i)

      ! -- The numerator of the line shape function components
      Fi_n1 = Gamma + ( Delta * f_diff )
      Fi_n2 = Gamma - ( Delta * f_sum  )

      ! -- The denominator of the line shape components
      Fi_d1 = ( f_diff * f_diff ) + Gamma2
      Fi_d2 = ( f_sum  * f_sum  ) + Gamma2

      ! -- Calculate the absorption line shape
      Fi = ( ( f / O2_LINE_FREQUENCY(i) )**2 ) * ( ( Fi_n1/Fi_d1 ) + ( Fi_n2/Fi_d2 ) )

      ! ----------------------------
      ! Calculate the absorption sum
      ! ----------------------------

      Absorption = Absorption + ( Si * Fi )

    END DO



    !#--------------------------------------------------------------------------#
    !#                     -- COMPUTE THE FINAL ABSORPTION --                   #
    !#--------------------------------------------------------------------------#

    Absorption = PI_RECIPROCAL * 0.5034e12_fp * Absorption * pd * Theta**3

  END FUNCTION Compute_O2_Line_Absorption




!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Dry_Continuum
!
! PURPOSE:
!       Function to calculate the dry gas continuum absorption
!       coefficient using the Rosenkranz03 parameterisation.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Absorption = Compute_Dry_Continuum( f,    &  ! Input
!                                           pd,   &  ! Input
!                                           pw,   &  ! Input
!                                           Theta )  ! Input
!
! INPUT ARGUMENTS:
!       f:           Frequency
!                    UNITS:      GHz
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Absorption:  The absorption coefficient of oxygen due to
!                    line contributions.
!                    UNITS:      Nepers/km
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       Rosenkranz03 parameterisation. Adapted from o2abs.for Fortran77
!
! CREATION HISTORY:
!       F77 version written by : P.Rosenkranz 01-May-1995 to 20-Mar-2003
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 16-Nov-2004
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_Dry_Continuum( f,      &  ! Input
                                  pd,     &  ! Input
                                  pw,     &  ! Input
                                  Theta ) &  ! Input
                                RESULT ( Absorption )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL(fp) :: Absorption


    ! ---------------
    ! Local prameters
    ! ---------------

    REAL(fp), PARAMETER :: MAGNITUDE_SCALE_FACTOR = 0.001_fp

    ! -- Strength parameter. Constant = 1.6e-17 * 0.5034e12 from o2abs
    REAL(fp), PARAMETER :: S_K = PI_RECIPROCAL * 8.0544e-06_fp


    ! ---------------
    ! Local variables
    ! ---------------

    REAL(fp) :: Gamma
    REAL(fp) :: f2, Gamma2
    REAL(fp) :: So, Fo

    REAL(fp) Denominator, Frequency_Dependence, bF



    !#--------------------------------------------------------------------------#
    !#             -- COMPUTE THE NON-RESONANT OXYGEN ABSORPTION --             #
    !#--------------------------------------------------------------------------#

    ! ------------
    ! The strength
    ! ------------

    So = S_K * pd * ( Theta**2 )


    ! ------------------------
    ! The line width parameter
    ! ------------------------

    Gamma = O2_WB * MAGNITUDE_SCALE_FACTOR * ( (               pd * Theta**O2_X08 ) + &
                                               ( ONEpointONE * pw * Theta         )   )

    ! --------------
    ! The line shape
    ! --------------

    f2     = f * f
    Gamma2 = Gamma * Gamma

    Fo = f2 * Gamma / ( f2 + Gamma2 )


    ! --------------
    ! The absorption
    ! --------------

    Absorption = So * Fo


    !#--------------------------------------------------------------------------#
    !#               -- COMPUTE THE COLLISION INDUCED ABSORPTION --             #
    !#--------------------------------------------------------------------------#

    Denominator = ONE + ( f / 450.0_fp )**2
    Frequency_Dependence = ZEROpointFIVE + ( ZEROpointFIVE / Denominator )

    bF = 6.5e-14_fp * Frequency_Dependence * ( ( pd + pw )**2 ) * ( f**2 ) * ( Theta**3.6_fp )

    Absorption = Absorption + ( 1.29_fp * bF )


  END FUNCTION Compute_Dry_Continuum




!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_H2O_Line_Absorption
!
! PURPOSE:
!       Function to calculate the water vapor line terms of the absorption
!       coefficient of water vapour using the Rosenkranz03 parameterisation.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Absorption = Compute_H2O_Line_Absorption( f,    &  ! Input
!                                                 pd,   &  ! Input
!                                                 pw,   &  ! Input
!                                                 T,    &  ! Input
!                                                 Theta )  ! Input
!
! INPUT ARGUMENTS:
!       f:           Frequency
!                    UNITS:      GHz
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       T:           Temperature.
!                    UNITS:      Kelvin, K
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Absorption:  The absorption coefficient of water vapour due to
!                    line contributions.
!                    UNITS:      Nepers/km
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!
! CALLS:
!       PP_to_ND:    Function to convert gas concentrations in pressure
!                    units to number density.
!                    SOURCE: UNITS_CONVERSION module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       Rosenkranz03 parameterisation. Adapted from abh2o.for Fortran77
!
! CREATION HISTORY:
!       F77 version written by : P.Rosenkranz 06-Oct-1999 to 02-Mar-2003
!
!       F95 version translation: Paul van Delst, CIMSS/SSEC 15-Nov-2004
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_H2O_Line_Absorption( f,      &  ! Input
                                        pd,     &  ! Input
                                        pw,     &  ! Input
                                        T,      &  ! Input
                                        Theta ) &  ! Input
                                      RESULT ( Absorption )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: T
    REAL(fp), INTENT(IN) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL(fp) :: Absorption


    ! ----------------
    ! Local parameters
    ! ----------------

    REAL(fp), PARAMETER :: BASE_CONSTANT = 562500.0_fp
    REAL(fp), PARAMETER :: DF_CUTOFF     = 750.0_fp
    REAL(fp), PARAMETER :: SCALE_FACTOR  = PI_RECIPROCAL * 1.0e-10_fp


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j

    REAL(fp) :: Density
    REAL(fp) :: Theta25, ONEminusTheta
    REAL(fp) :: Gamma, Gamma2
    REAL(fp) :: F_Shift
    REAL(fp) :: S
    REAL(fp), DIMENSION(2) :: df
    REAL(fp) :: Base
    REAL(fp) :: Resonance



    !#--------------------------------------------------------------------------#
    !#                      -- INITIALISE SOME VARIABLES --                     #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Initialise the absorption sum
    ! -----------------------------

    Absorption = ZERO


    ! ---------------------------------------
    ! Compute the water vapour number density
    ! ---------------------------------------

    CALL PP_to_ND( T, pw, Density)


    ! ----------------------------------------
    ! Inverse reference temperature variations
    ! ----------------------------------------

    Theta25       = Theta**TWOpointFive
    ONEminusTheta = ONE - Theta



    !#--------------------------------------------------------------------------#
    !#                      -- LOOP OVER THE H2O LINES --                       #
    !#--------------------------------------------------------------------------#

    DO i = 1, N_H2O_LINES


      ! ------------------------
      ! Calculate the line width
      ! ------------------------

      Gamma = ( H2O_WF(i) * pd * ( Theta**H2O_XF(i) ) ) + & ! Foreign-broadening
              ( H2O_WS(i) * pw * ( Theta**H2O_XS(i) ) )     ! Self-broadening

      Gamma2 = Gamma*Gamma


      ! ------------------------
      ! Calculate the line shift
      ! ------------------------

      F_Shift = H2O_SR(i) * Gamma   ! Unknown temperature dependence


      ! ---------------------------
      ! Calculate the line strength
      ! ---------------------------

      S = H2O_LINE_INTENSITY(i) * Theta25 * EXP( H2O_B(i) * ONEminusTheta )


      ! -----------------------
      ! Calculate the resonance
      ! -----------------------

      ! -- Frequency difference and sum from the line frequency
      df(1) = f - H2O_LINE_FREQUENCY(i) - F_Shift
      df(2) = f + H2O_LINE_FREQUENCY(i) + F_Shift

      ! -- Clough's definition of local line contribution
      Base = Gamma / ( BASE_CONSTANT + Gamma2 )

      ! -- Loop over +/- resonances
      Resonance = ZERO
      DO j = 1, 2
        IF ( ABS( df(j) ) < DF_CUTOFF ) &
          Resonance = Resonance + ( Gamma/(df(j)**2 + Gamma2 ) ) - Base
      END DO


      ! ---------------------
      ! Sum the contributions
      ! ---------------------

      Absorption = Absorption + S * Resonance * ( f/H2O_LINE_FREQUENCY(i) )**2

    END DO


    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE ABSORPTION COEFFICIENT --                #
    !#                                                                          #
    !# Ignoring the line shift, the absorption equation, eqn.(2.56) in Janssen, #
    !# can be written as,                                                       #
    !#                                                                          #
    !#            __         2                                                  #
    !#        n  \     [ f  ] [          g                       g          ]   #
    !#   a = ---- > Si.[----] [ -------------------- + -------------------- ]   #
    !#        PI /__   [ fi ] [  ( f - fi )^2 + g^2     ( f + fi )^2 + g^2  ]   #
    !#             i                                                            #
    !#                                                                          #
    !# where i  = line index                                                    #
    !#       Si = line strength in units of cm^2.Hz                             #
    !#       fi = line frequency in units of GHz                                #
    !#       f  = frequency in units of GHz                                     #
    !#       g  = line width (gamma) in units of GHz                            #
    !#       n  = water vapour number density in units of m^-3                  #
    !#                                                                          #
    !# If the above equation is written in terms of the units alone, we have    #
    !#                                                                          #
    !#          1                   GHz                                         #
    !#   a == ----- . cm^2 . Hz . -------                                       #
    !#         m^3                 GHz^2                                        #
    !#                                                                          #
    !# Getting everything into the same units,                                  #
    !#                                                                          #
    !#          1                                   GHz                         #
    !#   a == ----- . 10^-4 . m^2 . 10^-9 . GHz . -------                       #
    !#         m^3                                 GHz^2                        #
    !#                                                                          #
    !#                  1                                                       #
    !#     == 10^-13 . ---                                                      #
    !#                  m                                                       #
    !#                                                                          #
    !# Since the result is typically reported in Nepers/km, an addition factor  #
    !# of 1000 is required for the m->km scaling, giving,                       #
    !#                                                                          #
    !#                 1                                                        #
    !#   a == 10^-10. ----                                                      #
    !#                 km                                                       #
    !#                                                                          #
    !# So the SCALE_FACTOR parameters is a units conversion factor with the     #
    !# reciprocal of PI thrown in.                                              #
    !#--------------------------------------------------------------------------#

    Absorption = SCALE_FACTOR * Density * Absorption

  END FUNCTION Compute_H2O_Line_Absorption





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Wet_Continuum
!
! PURPOSE:
!       Function to calculate the water vapor continuum absorption
!       coefficient using the Rosenkranz03 parameterisation.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Absorption = Compute_Wet_Continuum( f,    &  ! Input
!                                           pd,   &  ! Input
!                                           pw,   &  ! Input
!                                           Theta )  ! Input
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
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Absorption:  The absorption coefficient of water vapour due to
!                    continuum contributions.
!                    UNITS:      Nepers/km
!                    TYPE:       REAL(fp)
!                    DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       Rosenkranz03 parameterisation. Adapted from abh2o.for Fortran77
!
! CREATION HISTORY:
!       F77 version written by : P.Rosenkranz 06-Oct-1999 to 02-Mar-2003
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 15-Nov-2004
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_Wet_Continuum( f,       &  ! Input
                                  pd,      &  ! Input
                                  pw,      &  ! Input
                                  Theta )  &  ! Input
                                RESULT ( Absorption )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL(fp), INTENT(IN) :: f
    REAL(fp), INTENT(IN) :: pd
    REAL(fp), INTENT(IN) :: pw
    REAL(fp), INTENT(IN) :: Theta

    ! ---------------
    ! Function result
    ! ---------------

    REAL(fp) :: Absorption


    ! ----------------
    ! Local Parameters
    ! ----------------

    REAL(fp), PARAMETER :: CF = 5.43e-10_fp
    REAL(fp), PARAMETER :: CS = 1.8e-8_fp



    !#--------------------------------------------------------------------------#
    !#                       -- CALCULATE THE CONTINUUM --                      #
    !#--------------------------------------------------------------------------#

    Absorption = pw * f * f * ( ( CF * pd * (Theta**THREE)          ) + &
                                ( CS * pw * (Theta**SEVENpointFIVE) )   )

  END FUNCTION Compute_Wet_Continuum


  !
  ! Reference reciprocal temperature
  !
  FUNCTION Compute_Theta( Temperature ) RESULT( Theta )
    REAL(fp), INTENT(IN) :: Temperature
    REAL(fp)               :: Theta

    REAL(fp), PARAMETER :: REFERENCE_TEMPERATURE = 300.0_fp

    Theta = REFERENCE_TEMPERATURE / Temperature
  END FUNCTION Compute_Theta



END MODULE MWLBL_Rosenkranz03
