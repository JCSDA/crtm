!
! CRTM_IRSSEM
!
! Module containing function to invoke the CRTM Spectral Infrared
! Sea Surface Emissivity Model (IRSSEM).
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jun-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_IRSSEM

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,            ONLY: fp=>fp_kind
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE CRTM_Parameters,       ONLY: ZERO, ONE, SET
  USE CRTM_EmisCoeff,        ONLY: EmisC
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Science routines in this modules
  PUBLIC :: CRTM_Compute_IRSSEM
  PUBLIC :: CRTM_Compute_IRSSEM_TL
  PUBLIC :: CRTM_Compute_IRSSEM_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_IRSSEM.f90,v 1.6 2006/05/25 19:40:30 wd20pd Exp $'


CONTAINS



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Bisection_Search
!
! PURPOSE:
!       Function to search an array using the bisection method. This function
!       is an adaptation from Numerical Recipes and is most efficient across
!       multiple calls when the value to be searched for in the array occurs
!       randomly.
!
! CALLING SEQUENCE:
!       Index = Bisection_Search( x, u,            &  ! Input
!                                 xLower = xLower, &  ! Optional input
!                                 xUpper = xUpper  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x:         The array to be searched.
!                  UNITS:      N/A
!                  TYPE:       REAL(fp)
!                  DIMENSION:  Rank-1
!                  ATTRIBUTES: INTENT(IN)
!
!       u:         The value to be searched for in the array.
!                  UNITS:      N/A
!                  TYPE:       Same as input array, x
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       xLower:    Set this optional argument to the INDEX of the input
!                  array corresponding to the LOWER search boundary.
!                  If not specified, the default value is 1.
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       xUpper:    Set this optional argument to the INDEX of the input
!                  array corresponding to the UPPER search boundary.
!                  If not specified, the default value is SIZE(x).
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!                  ATTRIBUTES: OPTIONAL, INTENT(IN)
!
! FUNCTION RESULT:
!       Index:     The lower index of the two values in the input array, x,
!                  that bracket the input value, u, i.e.
!                    x(Index) < u < x(Index+1)
!                  UNITS:      N/A
!                  TYPE:       INTEGER
!                  DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Bisection_Search( x, u,     &  ! Input
                             xLower,   &  ! Optional input
                             xUpper  ) &  ! Optional input
                           RESULT( j )
    ! Arguments
    REAL(fp), DIMENSION(:), INTENT(IN) :: x
    REAL(fp),               INTENT(IN) :: u
    INTEGER,  OPTIONAL,     INTENT(IN) :: xLower
    INTEGER,  OPTIONAL,     INTENT(IN) :: xUpper
    ! Function result
    INTEGER :: j
    ! Local variables
    INTEGER :: n
    INTEGER :: jLower
    INTEGER :: jMiddle
    INTEGER :: jUpper


    ! ------------------------------------
    ! Initialise upper and lower limits to
    ! the valid maximums, 1 and n
    ! ------------------------------------
    n = SIZE( x )
    IF ( PRESENT( xLower ) ) THEN
      jLower = xLower
    ELSE
      jLower = 1
    END IF
    IF ( PRESENT( xUpper ) ) THEN
      jUpper = xUpper
    ELSE
      jUpper = n
    END IF


    ! ----------------------
    ! Begin bisection search
    ! ----------------------
    Search_Loop: DO

      ! If the index ranges have converged, we're done
      IF ( ( jUpper - jLower ) <= 1 ) EXIT Search_Loop

      ! Define a middle point
      jMiddle = ( jLower + jUpper ) / 2


      ! ----------------------------------------------
      ! Which half is the required value in?
      !
      ! The following produces the result 
      !   x(i) <= x < x(i+1)
      ! when x == x(i).
      ! 
      ! To get the equivalent of
      !   x(i) < x <= x(i+1)
      ! for x == x(i+1), change the logical expression
      !   u < x( jMiddle )
      ! to
      !   u > x( jMiddle )
      ! ----------------------------------------------
      IF ( ( x(n) > x(1) ) .EQV. ( u < x( jMiddle ) ) ) THEN
        ! The "lower" half
        jUpper = jMiddle
      ELSE
        ! The "upper" half
        jLower = jMiddle
      END IF

    END DO Search_Loop


    ! -----------------------
    ! Define the return value
    ! -----------------------
    j = jLower

  END FUNCTION Bisection_Search





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IRSSEM
!
! PURPOSE:
!       Function to compute the CRTM infrared sea surface emissivity (IRSSE)
!       for input wind speed, frequency, and angles.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM( Wind_Speed,                 &  ! Input
!                                           Frequency,                  &  ! Input 
!                                           Angle,                      &  ! Input 
!                                           Emissivity,                 &  ! Output
!                                           Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Frequency:      Frequency, F, at which the emissivity is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Angle:          Angles, Z, at which the emissivity is to be calculated.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Emissivity:     sea surface emissivities for the 
!                       requested wind speed, frequency, and angles.
!                       UNITS:      None
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input ANGLE argument.
!                       ATTRIBUTES: INTENT(OUT)
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM( Wind_Speed,   &  ! Input
                                Frequency,    &  ! Input
                                Angle,        &  ! Input
                                Emissivity,   &  ! Output
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    REAL(fp),                 INTENT(IN)  :: Wind_Speed
    REAL(fp),                 INTENT(IN)  :: Frequency
    REAL(fp), DIMENSION(:), INTENT(IN)  :: Angle
    REAL(fp), DIMENSION(:), INTENT(OUT) :: Emissivity
    CHARACTER(*), OPTIONAL,        INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM'
    ! Local variables
    INTEGER :: n_Angles, i
    REAL(fp), SAVE :: Old_Wind_Speed = -99.0_fp ! Old wind speed value
    INTEGER,  SAVE :: iv = 1     ! Wind speed index into EmisC%Wind_Speed
    REAL(fp), SAVE :: v  = ZERO  ! Wind speed interpolation factor
    INTEGER        :: iu         ! Frequency index into EmisC%Frequency
    REAL(fp)       :: u          ! Frequency interpolation factor
    INTEGER        :: it         ! Angle index into EmisC%Angle
    REAL(fp)       :: t          ! Angle interpolation factor


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Dimensions
    n_Angles = SIZE( Angle )
    IF ( SIZE( Emissivity ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Angle and output Emissivity array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    ! ------------------------------------------------------------------
    ! Compute the wind speed index and interpolation factor if necessary
    ! ------------------------------------------------------------------
    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN

      ! Update the old wind speed
      Old_Wind_Speed = Wind_Speed

      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      iv = Bisection_Search( EmisC%Wind_Speed, Wind_Speed )

      ! Compute the linear interpolation
      ! factor for the input wind speed
      v = (      Wind_Speed        - EmisC%Wind_Speed(iv) ) / &
      !   -------------------------------------------------
          ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) )

    END IF Update_Wind_Speed



    ! ----------------------------------------------------
    ! Compute the frequency index and interpolation factor
    ! ----------------------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    iu = Bisection_Search( EmisC%Frequency, Frequency )

    ! Compute the linear interpolation
    ! factor for the input frequency
    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )


    ! ----------------------
    ! Begin loop over angles
    ! ----------------------
    DO i = 1, n_Angles

      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )

      ! Compute the linear interpolation
      ! factor for the current input angle
      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )

      ! Perform the tri-linear interpolation
      Emissivity(i) = ( (ONE-t)*(ONE-u)*(ONE-v)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                      (    t   *(ONE-u)*(ONE-v)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                      (    t   *   u   *(ONE-v)*EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                      ( (ONE-t)*   u   *(ONE-v)*EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                      ( (ONE-t)*(ONE-u)*   v   *EmisC%Emissivity(it,  iu,  iv+1) ) + &
                      (    t   *(ONE-u)*   v   *EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                      (    t   *   u   *   v   *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                      ( (ONE-t)*   u   *   v   *EmisC%Emissivity(it,  iu+1,iv+1) )
    END DO

  END FUNCTION CRTM_Compute_IRSSEM


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IRSSEM_TL
!
! PURPOSE:
!       Function to compute the tangent-linear CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_TL( Wind_Speed,                 &  ! Input
!                                              Frequency,                  &  ! Input 
!                                              Angle,                      &  ! Input 
!                                              Wind_Speed_TL,              &  ! Input
!                                              Emissivity_TL,              &  ! Output
!                                              Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Frequency:      Frequency, F, at which the emissivity is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Angle:          Angles, Z, at which the emissivity is to be calculated.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Wind_Speed_TL:  The tangent-linear wind speed, dV, at sea surface.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Emissivity_TL:  Tangent-linear sea surface emissivities for the 
!                       requested wind speed, frequency, and angles, due 
!                       to the input tangent-linear wind speed.
!                       UNITS:      None
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input ANGLE argument
!                       ATTRIBUTES: INTENT(OUT)
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_TL( Wind_Speed,    &  ! Input
                                   Frequency,     &  ! Input
                                   Angle,         &  ! Input
                                   Wind_Speed_TL, &  ! Input
                                   Emissivity_TL, &  ! Output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: Wind_Speed
    REAL(fp),               INTENT(IN)  :: Frequency
    REAL(fp), DIMENSION(:), INTENT(IN)  :: Angle
    REAL(fp),               INTENT(IN)  :: Wind_Speed_TL
    REAL(fp), DIMENSION(:), INTENT(OUT) :: Emissivity_TL
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_TL'
    ! Local variables
    INTEGER :: n_Angles, i
    REAL(fp), SAVE :: Old_Wind_Speed = -99.0_fp ! Old wind speed value
    INTEGER,  SAVE :: iv = 1    ! Wind speed index into EmisC%Wind_Speed
    REAL(fp)       :: v_TL      ! Tangent-linear wind speed interpolation factor
    INTEGER        :: iu        ! Frequency index into EmisC%Frequency
    REAL(fp)       :: u         ! Frequency interpolation factor
    INTEGER        :: it        ! Angle index into EmisC%Angle
    REAL(fp)       :: t         ! Angle interpolation factor


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Dimensions
    n_Angles = SIZE( Angle )
    IF ( SIZE( Emissivity_TL ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Angle and output Emissivity_TL array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Compute the wind speed index
    ! ----------------------------
    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN

      ! Update the old wind speed
      Old_Wind_Speed = Wind_Speed

      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      iv = Bisection_Search( EmisC%Wind_Speed, ABS( Wind_Speed ) )

    END IF Update_Wind_Speed



    ! ----------------------------------------------------------
    ! Compute the wind speed tangent-linear interpolation factor
    ! ----------------------------------------------------------
    v_TL =                  Wind_Speed_TL                    / &
    !      -------------------------------------------------
           ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) )



    ! ----------------------------------------------------
    ! Compute the frequency index and interpolation factor
    ! ----------------------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    iu = Bisection_Search( EmisC%Frequency, Frequency )

    ! Compute the linear interpolation
    ! factor for the input frequency
    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )


    ! ----------------------
    ! Begin loop over angles
    ! ----------------------
    DO i = 1, n_Angles

      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )

      ! Compute the linear interpolation
      ! factor for the current input angle
      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )

      ! Perform the tri-linear interpolation
      Emissivity_TL(i) = ( (ONE-t)*(ONE-u)*(-v_TL)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                         (    t   *(ONE-u)*(-v_TL)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                         (    t   *   u   *(-v_TL)*EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                         ( (ONE-t)*   u   *(-v_TL)*EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                         ( (ONE-t)*(ONE-u)*  v_TL *EmisC%Emissivity(it,  iu,  iv+1) ) + &
                         (    t   *(ONE-u)*  v_TL *EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                         (    t   *   u   *  v_TL *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                         ( (ONE-t)*   u   *  v_TL *EmisC%Emissivity(it,  iu+1,iv+1) )
    END DO

  END FUNCTION CRTM_Compute_IRSSEM_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_IRSSEM_AD
!
! PURPOSE:
!       Function to compute the adjoint of the CRTM infrared sea surface
!       emissivity (IRSSE) for input wind speed, frequency, and angles.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Compute_IRSSEM_AD( Wind_Speed,                 &  ! Input
!                                              Frequency,                  &  ! Input 
!                                              Angle,                      &  ! Input 
!                                              Emissivity_AD,              &  ! Input
!                                              Wind_Speed_AD,              &  ! Output
!                                              Message_Log = Message_Log ) &  ! Error messaging 
!
! INPUT ARGUMENTS:
!       Wind_Speed:     Wind speed, V, at sea surface. Valid range
!                       is 0 to 15 m/s.
!                       UNITS:      metres per second (m.s^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Frequency:      Frequency, F, at which the emissivity adjoint
!                       is required.
!                       Valid range is 600 to 3000cm^-1
!                       UNITS:      inverse centimetres (cm^-1)
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Angle:          Angles, Z, over which the emissivity adjoint is
!                       calculated and summed. See the output Wind_Speed_AD
!                       argument description for more information about the
!                       angle summation.
!                       Valid range is -65 to 65 degrees, although the value
!                       used in the calculations is the absolute value, |Z|.
!                       UNITS:      Degrees
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Rank-1 (n_Angles)
!                       ATTRIBUTES: INTENT(IN)
!
!       Emissivity_AD:  Adjoint sea surface emissivities for the 
!                       requested wind speed, frequency, and angles.
!                       UNITS:      None
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Same as input ANGLE argument
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to the screen.
!                       UNITS:      None
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Wind_Speed_AD:  Adjoint wind speed, de/dV, at sea surface, SUMMED
!                       OVER THE INPUT ANGLES. The sum over angle is done because
!                       this model was designed for use with a multi-stream radiative
!                       transfer scheme. As such, the sum over angle produces
!                       a result, e.g. dRadiance/dV for a single frequency.
!                       For emissivity-only calculations for a range of angles,
!                       the adjoint function must be called separately for each
!                       angle. Alternatively, you can use the tangent-linear
!                       function to compute derivatives for multiple angles.
!                       UNITS:      per metres per second, (m.s^-1)^-1
!                       TYPE:       REAL(fp)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the emissivity computation was successful.
!                        == WARNING the inputs were outside their valid ranges.
!                                   In these cases the computation continues
!                                   but using the relevant wind speed, frequency,
!                                   or angle boundary values.
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION CRTM_Compute_IRSSEM_AD( Wind_Speed,    &  ! Input
                                   Frequency,     &  ! Input
                                   Angle,         &  ! Input
                                   Emissivity_AD, &  ! Input
                                   Wind_Speed_AD, &  ! Output
                                   Message_Log )  &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)     :: Wind_Speed
    REAL(fp),               INTENT(IN)     :: Frequency
    REAL(fp), DIMENSION(:), INTENT(IN)     :: Angle
    REAL(fp), DIMENSION(:), INTENT(IN OUT) :: Emissivity_AD
    REAL(fp),               INTENT(IN OUT) :: Wind_Speed_AD
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_IRSSEM_AD'
    ! Local variables
    CHARACTER( 256 ) :: Message
    INTEGER :: n_Angles, i
    REAL(fp), SAVE :: Old_Wind_Speed = -99.0_fp ! Old wind speed value
    INTEGER,  SAVE :: iv = 1  ! Wind speed index into EmisC%Wind_Speed
    REAL(fp)       :: v_AD    ! Adjoint wind speed interpolation factor
    INTEGER        :: iu      ! Frequency index into EmisC%Frequency
    REAL(fp)       :: u       ! Frequency interpolation factor
    INTEGER        :: it      ! Angle index into EmisC%Angle
    REAL(fp)       :: t       ! Angle interpolation factor


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Dimensions
    n_Angles = SIZE( Angle )
    IF ( SIZE( Emissivity_AD ) /= n_Angles ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Angle and Emissivity_AD array dimensions inconsistent.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------
    ! Compute the wind speed index
    ! ----------------------------
    Update_Wind_Speed: IF ( .NOT. Compare_Float( Wind_Speed, Old_Wind_Speed ) ) THEN

      ! Update the old wind speed
      Old_Wind_Speed = Wind_Speed

      ! Compute the index of the input wind speed
      ! into the EmisC%Wind_Speed array. Result
      ! must be from 1 to n_Wind_Speeds-1
      iv = Bisection_Search( EmisC%Wind_Speed, ABS( Wind_Speed ) )

    END IF Update_Wind_Speed


    ! ----------------------------------------------------
    ! Compute the frequency index and interpolation factor
    ! ----------------------------------------------------
    ! Compute the index of the input frequency
    ! into the EmisC%Frequency array. Result
    ! must be from 1 to n_Frequencies-1
    iu = Bisection_Search( EmisC%Frequency, Frequency )

    ! Compute the linear interpolation
    ! factor for the input frequency
    u = (      Frequency        - EmisC%Frequency(iu) ) / &
    !   -----------------------------------------------
        ( EmisC%Frequency(iu+1) - EmisC%Frequency(iu) )


    ! ---------------------------------------------------
    ! Compute the wind speed interpolation factor adjoint
    ! ---------------------------------------------------
    ! Initialise the local adjoint variable
    v_AD = ZERO

    ! Sum local adjoint over angles
    DO i = 1, n_Angles

      ! Compute the index of the current input angle
      ! into the EmisC%Angle array. Result must be
      ! from 1 to n_Angles-1
      it = Bisection_Search( EmisC%Angle, ABS(Angle(i)) )

      ! Compute the linear interpolation
      ! factor for the current input angle
      t = (   ABS(Angle(i))   - EmisC%Angle(it) ) / &
      !   ---------------------------------------
          ( EmisC%Angle(it+1) - EmisC%Angle(it) )

      ! Compute the local wind speed adjoint
      v_AD = v_AD + ( Emissivity_AD(i) * &
                      ( ( (-ONE)*(ONE-t)*(ONE-u)*EmisC%Emissivity(it,  iu,  iv  ) ) + &
                        ( (-ONE)*   t   *(ONE-u)*EmisC%Emissivity(it+1,iu,  iv  ) ) + &
                        ( (-ONE)*   t   *   u   *EmisC%Emissivity(it+1,iu+1,iv  ) ) + &
                        ( (-ONE)*(ONE-t)*   u   *EmisC%Emissivity(it,  iu+1,iv  ) ) + &
                        (        (ONE-t)*(ONE-u)*EmisC%Emissivity(it,  iu,  iv+1) ) + &
                        (           t   *(ONE-u)*EmisC%Emissivity(it+1,iu,  iv+1) ) + &
                        (           t   *   u   *EmisC%Emissivity(it+1,iu+1,iv+1) ) + &
                        (        (ONE-t)*   u   *EmisC%Emissivity(it,  iu+1,iv+1) )   ) )
      Emissivity_AD(i) = ZERO
    END DO


    ! ------------------------------
    ! Compute the wind speed adjoint
    ! ------------------------------
    Wind_Speed_AD = Wind_Speed_AD + (                        v_AD                       / &
    !                                 -------------------------------------------------
                                      ( EmisC%Wind_Speed(iv+1) - EmisC%Wind_Speed(iv) ) )

  END FUNCTION CRTM_Compute_IRSSEM_AD

END MODULE CRTM_IRSSEM
