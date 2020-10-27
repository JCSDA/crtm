!
! NAME:
!       MWLBL_Transmittance
!
! PURPOSE:
!       Module containing routines for microwave transmittance profile
!       calculations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 04-Sep-2002
!                       paul.vandelst@noaa.gov
!

MODULE MWLBL_Transmittance

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE MWLBL_Liebe89
  USE MWLBL_Liebe93
  USE MWLBL_Rosenkranz03
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MWLBL_Compute_Tau
  PUBLIC :: MWLBL_Transmittance_Version


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Literal constant
  REAL(fp), PARAMETER :: ZERO      =  0.0_fp
  REAL(fp), PARAMETER :: ONE       =  1.0_fp
  REAL(fp), PARAMETER :: TEN       = 10.0_fp
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ZERO)
  ! Min/max frequencies in GHz
  REAL(fp), PARAMETER :: MIN_FREQUENCY = ONE
  REAL(fp), PARAMETER :: MAX_FREQUENCY = 1000.0_fp
  ! Default message string length
  INTEGER,  PARAMETER :: ML = 256


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWLBL_Compute_Tau
!
! PURPOSE:
!       Function to calculate LBL microwave transmittances for an input
!       profile and frequency grid.
!
! CALLING SEQUENCE:
!       Error_Status = MWLBL_Compute_Tau( &
!         Pressure                 , &  ! Input
!         Temperature              , &  ! Input
!         Water_Vapor_Pressure     , &  ! Input
!         Layer_Thickness          , &  ! Input
!         Angle_Secant             , &  ! Input
!         Frequency                , &  ! Input
!         TauALL                   , &  ! Output
!         TauWLO                   , &  ! Output
!         TauWCO                   , &  ! Output
!         TauWET                   , &  ! Output
!         TauDRY                   , &  ! Output
!         Downwelling = Downwelling, &  ! Optional input
!         Rosenkranz  = Rosenkranz , &  ! Optional input
!         Quiet       = Quiet        )  ! Optional input
!
! INPUTS:
!       Pressure:              Layer pressure profile. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order.
!                              UNITS:      hectoPascals (hPa)
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT(IN)
!
!       Temperature:           Layer temperature profile. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order but must be in the same order as
!                              the pressure profile.
!                              UNITS:      Kelvin (K)
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT(IN)
!
!       Water_Vapor_Pressure:  Layer water vapor partial pressure
!                              profile. Values can be in either TOA->SFC
!                              or SFC->TOA order but must be in the same
!                              order as the pressure profile.
!                              UNITS:      hectoPascals (hPa)
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT(IN)
!
!       Layer_Thickness:       Atmospheric layer thickness. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order but must be in the same order as
!                              the pressure profile.
!                              UNITS:      metres (m)
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT(IN)
!
!       Angle_Secant:          Zenith angle secant used to modify
!                              path length.
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, I
!                              ATTRIBUTES: INTENT(IN)
!
!       Frequency:             Frequency grid at which to calculate
!                              the microwave transmittances.
!                              UNITS:      Gigahertz (GHz)
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-1, L
!                              ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       TauALL:                Layer transmittance profiles due to all absorbers
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT(OUT)
!
!       TauWLO:                Layer wet transmittance profiles due to H2O line
!                              absorption only.
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT(OUT)
!
!       TauWCO:                Layer wet transmittance profiles due to H2O
!                              continuum absorption only.
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT(OUT)
!
!       TauWET:                Layer wet transmittance profiles due to H2O line
!                              *AND* continuum absorption.
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT(OUT)
!
!       TauDRY:                Layer dry transmittance profiles due to O2 line 
!                              absorption *AND* dry gas non-resonant absorption.
!                              UNITS:      None.
!                              TYPE:       REAL(fp)
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT(OUT) 
!
! OPTIONAL INPUTS:
!       Downwelling:           Set this logical argument to perform a
!                              downwelling (LYR->SFC) transmittance
!                              calculations. If not specified, the default
!                              is to perform an upwelling (LYR->TOA)
!                              calculation.
!                              If == .FALSE., Upwelling calculation [DEFAULT].
!                                 == .TRUE.,  Downwelling calculation.
!                              UNITS:      N/A 
!                              TYPE:       LOGICAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Rosenkranz:            Set this logical optional argument to perform the
!                              calculations using the Rosenkranz03 model.
!                              If not specified, the default is to use the
!                              Liebe89/93 model.
!                              If == .FALSE., Use Liebe89/93 model [DEFAULT].
!                                 == .TRUE.,  Use Rosenkranz03 model.
!                              UNITS:      N/A 
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:                 Set this logical argument to suppress WARNING and
!                              INFORMATION messages being printed to stdout
!                              If == .FALSE., messages are OUTPUT [DEFAULT].
!                                 == .TRUE.,  messages are SUPPRESSED.
!                              If not specified, default is .FALSE.
!                              UNITS:      N/A
!                              TYPE:       LOGICAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error status.
!                              status. The error codes are defined in the Message_Handler
!                              module.
!                              If == SUCCESS the calculation was successful
!                                 == FAILURE an unrecoverable error occurred.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 21-Apr-2002
!                       paul.vandelst@noaa.gov
!
!       Adapted from certain parts of F77 code written by L. Phalippou
!       28-Jul-1992 and modified by Peter Rayer and Roger Saunders, UKMO.
!S-
!------------------------------------------------------------------------------

  FUNCTION MWLBL_Compute_Tau( &
    Pressure            , &  ! Input
    Temperature         , &  ! Input
    Water_Vapor_Pressure, &  ! Input
    Layer_Thickness     , &  ! Input
    Angle_Secant        , &  ! Input
    Frequency           , &  ! Input
    TauALL              , &  ! Output
    TauWLO              , &  ! Output
    TauWCO              , &  ! Output
    TauWET              , &  ! Output
    TauDRY              , &  ! Output
    Downwelling         , &  ! Optional input
    Rosenkranz          , &  ! Optional input
    Quiet               ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    REAL(fp),           INTENT(IN)  :: Pressure(:)              ! K
    REAL(fp),           INTENT(IN)  :: Temperature(:)           ! K
    REAL(fp),           INTENT(IN)  :: Water_Vapor_Pressure(:)  ! K
    REAL(fp),           INTENT(IN)  :: Layer_Thickness(:)       ! K
    REAL(fp),           INTENT(IN)  :: Angle_Secant(:)          ! I
    REAL(fp),           INTENT(IN)  :: Frequency(:)             ! L
    REAL(fp),           INTENT(OUT) :: TauALL(:,:,:)  ! L x K x I
    REAL(fp),           INTENT(OUT) :: TauWLO(:,:,:)  ! L x K x I
    REAL(fp),           INTENT(OUT) :: TauWCO(:,:,:)  ! L x K x I
    REAL(fp),           INTENT(OUT) :: TauWET(:,:,:)  ! L x K x I
    REAL(fp),           INTENT(OUT) :: TauDRY(:,:,:)  ! L x K x I
    LOGICAL , OPTIONAL, INTENT(IN)  :: Downwelling
    LOGICAL , OPTIONAL, INTENT(IN)  :: Rosenkranz
    LOGICAL , OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'MWLBL_Compute_Tau'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_Frequencies
    INTEGER :: n_Layers, k
    INTEGER :: n_Angles, i
    LOGICAL :: upwelling
    LOGICAL :: liebe
    LOGICAL :: noisy
    LOGICAL :: TOA_to_SFC
    INTEGER :: k_Reverse
    REAL(fp), DIMENSION(SIZE(Pressure            )) :: P
    REAL(fp), DIMENSION(SIZE(Temperature         )) :: T
    REAL(fp), DIMENSION(SIZE(Water_Vapor_Pressure)) :: Wp
    REAL(fp), DIMENSION(SIZE(Layer_Thickness     )) :: dZ
    REAL(fp), DIMENSION(SIZE(Water_Vapor_Pressure)) :: Dp
    REAL(fp), DIMENSION(SIZE(Frequency           )) :: WetLine_Attenuation
    REAL(fp), DIMENSION(SIZE(Frequency           )) :: WetContinuum_Attenuation
    REAL(fp), DIMENSION(SIZE(Frequency           )) :: DryLine_Attenuation
    REAL(fp), DIMENSION(SIZE(Frequency           )) :: DryContinuum_Attenuation


    ! Set up
    err_stat = SUCCESS
    ! ...Process keywords
    upwelling = .TRUE.
    IF ( PRESENT(Downwelling) ) upwelling = .NOT. Downwelling
    liebe = .TRUE.
    IF ( PRESENT(Rosenkranz) ) liebe = .NOT. Rosenkranz
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT.Quiet


    ! Check argument array sizes
    ! ...Get dimensions
    n_Frequencies = SIZE(Frequency)
    n_Layers      = SIZE(Pressure)
    n_Angles      = SIZE(Angle_Secant)
    ! ...Check for consistent INPUT array sizes
    IF ( SIZE(Temperature         ) /= n_Layers .OR. &
         SIZE(Water_Vapor_Pressure) /= n_Layers .OR. &
         SIZE(Layer_Thickness     ) /= n_Layers      ) THEN
      err_stat = FAILURE
      msg = 'Input profile arrays have inconsistent sizes.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Check for consistent OUTPUT array sizes
    IF ( SIZE(TauALL, DIM=1) /= n_Frequencies .OR. &
         SIZE(TauALL, DIM=2) /= n_Layers      .OR. &
         SIZE(TauALL, DIM=3) /= n_Angles      .OR. &

         SIZE(TauWLO, DIM=1) /= n_Frequencies .OR. &
         SIZE(TauWLO, DIM=2) /= n_Layers      .OR. &
         SIZE(TauWLO, DIM=3) /= n_Angles      .OR. &

         SIZE(TauWCO, DIM=1) /= n_Frequencies .OR. &
         SIZE(TauWCO, DIM=2) /= n_Layers      .OR. &
         SIZE(TauWCO, DIM=3) /= n_Angles      .OR. &

         SIZE(TauWET, DIM=1) /= n_Frequencies .OR. &
         SIZE(TauWET, DIM=2) /= n_Layers      .OR. &
         SIZE(TauWET, DIM=3) /= n_Angles      .OR. &

         SIZE(TauDRY, DIM=1) /= n_Frequencies .OR. &
         SIZE(TauDRY, DIM=2) /= n_Layers      .OR. &
         SIZE(TauDRY, DIM=3) /= n_Angles           ) THEN
      err_stat = FAILURE
      msg = 'Output transmittance arrays have inconsistent sizes.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
         

    ! Check input for invalid values
    ! ...Profile data
    IF ( ANY(Pressure             < TOLERANCE) .OR. &
         ANY(Temperature          < TOLERANCE) .OR. &
         ANY(Water_Vapor_Pressure < TOLERANCE) .OR. &
         ANY(Layer_Thickness      < TOLERANCE)      ) THEN
      err_stat = FAILURE
      msg = 'Input profile data must be > 0.0.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Angle data
    IF ( ANY(Angle_Secant < ONE) ) THEN
      err_stat = FAILURE
      msg = 'Input angle secants must be > or = 1.0.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF
    ! ...Frequency data
    IF ( ANY(Frequency < MIN_FREQUENCY) .OR. ANY(Frequency > MAX_FREQUENCY) ) THEN
      err_stat = FAILURE
      WRITE( msg,'("Input frequencies must be ",f6.1," < f < ",f6.1)' ) &
                 MIN_FREQUENCY, MAX_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
    END IF


    ! Reverse the arrays if required
    ! ...Determine the input profile ordering
    TOA_to_SFC = .TRUE.
    IF ( (Pressure(2)-Pressure(1)) < ZERO ) TOA_to_SFC = .FALSE.
    ! ...Check the array ordering with the up/downwelling requirement
    IF ( TOA_to_SFC .EQV. upwelling ) THEN
      ! ...Order is correct, so simply copy
      P(:)  = Pressure(:)
      T(:)  = Temperature(:)
      Wp(:) = Water_Vapor_Pressure(:)
      dZ(:) = Layer_Thickness(:)
    ELSE
      ! ...Order must be reversed
      DO k_Reverse = 1, n_Layers
        k = n_Layers - k_Reverse + 1
        P(k)  = Pressure(k_Reverse)
        T(k)  = Temperature(k_Reverse)
        Wp(k) = Water_Vapor_Pressure(k_Reverse)
        dZ(k) = Layer_Thickness(k_Reverse)
      END DO
      IF ( noisy ) THEN
        msg = 'Input profile arrays reversed for required calculation.'
        CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
      END IF
    END IF


    ! Convert layer thickness to unit of kilometres
    dZ = dZ / 1000.0_fp


    ! Compute the dry gas partial pressure
    Dp = P - Wp


    ! Loop over atmospheric layers
    k_Layer_loop: DO k = 1, n_Layers


      ! Compute the nadir attenuation
      Liebe_or_Rosenkranz: IF ( liebe ) THEN
        ! ...Water vapor attenuation using the Liebe89 model
        err_stat = Liebe89( &
          Frequency, &
          Dp(k), &
          Wp(k), &
          T(k), &
          WetLine_Attenuation      = WetLine_Attenuation, &
          WetContinuum_Attenuation = WetContinuum_Attenuation, &
          Quiet                    = Quiet )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error calculating Liebe89 water vapor attentuation at level ",i0 )' ) k
          CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
        END IF
        ! ...Dry gas attenuation using the Liebe93 model
        err_stat = Liebe93( &
          Frequency, &
          Dp(k), &
          Wp(k), &
          T(k), &
          DryLine_Attenuation      = DryLine_Attenuation, &
          DryContinuum_Attenuation = DryContinuum_Attenuation, &
          Quiet                    = Quiet )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error calculating Liebe93 dry gas attentuation at level ",i0 )' ) k
          CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
        END IF
      ELSE Liebe_or_Rosenkranz
        ! ...Nadir layer attenuation using Rosenkranz model
        err_stat = Rosenkranz03( &
          Frequency, &
          Dp(k), &
          Wp(k), &
          T(k), &
          WetLine_Attenuation      = WetLine_Attenuation, &
          WetContinuum_Attenuation = WetContinuum_Attenuation, &
          DryLine_Attenuation      = DryLine_Attenuation, &
          DryContinuum_Attenuation = DryContinuum_Attenuation, &
          Quiet                    = Quiet )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error calculating Rosenkranz03 attentuation at level ",i0 )' ) k
          CALL Display_Message( ROUTINE_NAME, msg, err_stat ); RETURN
        END IF
      END IF Liebe_or_Rosenkranz


      ! Save the nadir ATTENUATIONs in the transmittance arrays
      IF ( k == 1 ) THEN
        ! ...First layer
        TauALL(:,k,1 ) = ( WetLine_Attenuation      + &
                           WetContinuum_Attenuation + &
                           DryLine_Attenuation      + &
                           DryContinuum_Attenuation   ) * dZ(k)
        TauWLO(:,k,1 ) = WetLine_Attenuation * dZ(k)
        TauWCO(:,k,1 ) = WetContinuum_Attenuation * dZ(k)
        TauWET(:,k,1 ) = ( WetLine_Attenuation      + &
                           WetContinuum_Attenuation   ) * dZ(k)
        TauDRY(:,k,1 ) = ( DryLine_Attenuation      + &
                           DryContinuum_Attenuation   ) * dZ(k)
      ELSE
        ! ...All the rest
        TauALL(:,k,1 ) = TauALL(:,k-1,1 ) + ( ( WetLine_Attenuation      + &
                                                WetContinuum_Attenuation + &
                                                DryLine_Attenuation      + &
                                                DryContinuum_Attenuation   ) * dZ(k) )
        TauWLO(:,k,1 ) = TauWLO(:,k-1,1 ) + ( WetLine_Attenuation * dZ(k) )
        TauWCO(:,k,1 ) = TauWCO(:,k-1,1 ) + ( WetContinuum_Attenuation * dZ(k) )
        TauWET(:,k,1 ) = TauWET(:,k-1,1 ) + ( ( WetLine_Attenuation      + &
                                                WetContinuum_Attenuation   ) * dZ(k) )
        TauDRY(:,k,1 ) = TauDRY(:,k-1,1 ) + ( ( DryLine_Attenuation      + &
                                                 DryContinuum_Attenuation   ) * dZ(k) )
      END IF

    END DO k_Layer_loop


    ! Compute the transmittances for all angles
    !
    ! Loop over angles BACKWARDS. Thus the nadir attentuations
    ! are not overwritten as they are used.
    i_Angle_Loop: DO i = n_Angles, 1, -1
      ! ...Modify the path lengths
      TauALL(:,:,i) = TauALL(:,:,1) * Angle_Secant(i)
      TauWLO(:,:,i) = TauWLO(:,:,1) * Angle_Secant(i)
      TauWCO(:,:,i) = TauWCO(:,:,1) * Angle_Secant(i)
      TauWET(:,:,i) = TauWET(:,:,1) * Angle_Secant(i)
      TauDRY(:,:,i) = TauDRY(:,:,1) * Angle_Secant(i)
      ! ...Compute the transmittances
      TauALL(:,:,i) = 10**(-(TauALL(:,:,i)/TEN))
      TauWLO(:,:,i) = 10**(-(TauWLO(:,:,i)/TEN))
      TauWCO(:,:,i) = 10**(-(TauWCO(:,:,i)/TEN))
      TauWET(:,:,i) = 10**(-(TauWET(:,:,i)/TEN))
      TauDRY(:,:,i) = 10**(-(TauDRY(:,:,i)/TEN))
    END DO i_Angle_Loop

  END FUNCTION MWLBL_Compute_Tau


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       MWLBL_Transmittance_Version
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL MWLBL_Transmittance_Version( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE MWLBL_Transmittance_Version( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE MWLBL_Transmittance_Version



END MODULE MWLBL_Transmittance
