!
! CRTM_AtmAbsorption
!
! Module containing routines to compute the optical depth profile
! due to gaseous absorption.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 13-May-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_AtmAbsorption

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                ONLY: fp
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, POINT_5        , &
                                       LIMIT_EXP, LIMIT_LOG , &
                                       MAX_N_LAYERS         , &
                                       MAX_N_ABSORBERS      , &
                                       MAX_N_PREDICTORS_USED, &
                                       MAX_N_ORDERS
  USE CRTM_TauCoeff,             ONLY: TC
  USE CRTM_Atmosphere_Define,    ONLY: CRTM_Atmosphere_type, H2O_ID, O3_ID
  USE CRTM_GeometryInfo_Define,  ONLY: CRTM_GeometryInfo_type
  USE CRTM_Predictor_Define,     ONLY: CRTM_Predictor_type
  USE CRTM_AtmAbsorption_Define, ONLY: CRTM_AtmAbsorption_type, &
                                       CRTM_Associated_AtmAbsorption, &
                                       CRTM_Destroy_AtmAbsorption, &
                                       CRTM_Allocate_AtmAbsorption, &
                                       CRTM_Assign_AtmAbsorption
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! CRTM_AtmAbsorption structure data type
  ! in the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_AtmAbsorption_type
  ! CRTM_AtmAbsorption structure routines inherited
  ! from the CRTM_AtmAbsorption_Define module
  PUBLIC :: CRTM_Associated_AtmAbsorption
  PUBLIC :: CRTM_Destroy_AtmAbsorption
  PUBLIC :: CRTM_Allocate_AtmAbsorption
  PUBLIC :: CRTM_Assign_AtmAbsorption
  ! Science routines in this modules
  PUBLIC :: CRTM_Compute_AtmAbsorption
  PUBLIC :: CRTM_Compute_AtmAbsorption_TL
  PUBLIC :: CRTM_Compute_AtmAbsorption_AD
  ! Internal variable structure
  PUBLIC :: CRTM_AAVariables_type


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


  ! ------------------------------------------
  ! Structure definition to hold forward model
  ! variables across FWD, TL, and AD calls
  ! ------------------------------------------
  TYPE :: CRTM_AAVariables_type
    PRIVATE
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: A_Level = ZERO
    REAL(fp), DIMENSION(0:MAX_N_ORDERS,0:MAX_N_PREDICTORS_USED,&
                        MAX_N_LAYERS,MAX_N_ABSORBERS) :: b = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: LN_Chi = ZERO
    REAL(fp), DIMENSION(MAX_N_LAYERS,MAX_N_ABSORBERS) :: Chi    = ZERO
  END TYPE CRTM_AAVariables_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption
!
! PURPOSE:
!       Subroutine to calculate the layer optical depths due to gaseous
!       absorption for a given sensor and channel and atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption( SensorIndex  , &  ! Input
!                                        ChannelIndex , &  ! Input
!                                        Predictor    , &  ! Input
!                                        AtmAbsorption, &  ! Output
!                                        AAVariables    )  ! Internal variable output
!
! INPUT ARGUMENTS:
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
!       Predictor:       Structure containing the integrated absorber and
!                        predictor profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_Predictor_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption:  Structure containing computed optical depth
!                        profile data.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:     Structure containing internal variables required for
!                        subsequent tangent-linear or adjoint model calls.
!                        The contents of this structure are NOT accessible
!                        outside of the CRTM_AtmAbsorption module.
!                        UNITS:      N/A
!                        TYPE:       TYPE(CRTM_AAVariables_type)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption( SensorIndex  , &  ! Input
                                         ChannelIndex , &  ! Input
                                         Predictor    , &  ! Input
                                         AtmAbsorption, &  ! Output
                                         AAV            )  ! Internal variable output
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)    , INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    TYPE(CRTM_AAVariables_type)  , INTENT(OUT)    :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption'
    ! Local variables
    INTEGER :: n       ! Sensor index
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: np      ! Polynomial index

    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n = SensorIndex
    l = ChannelIndex
    ! Initilise the optical depth
    AtmAbsorption%Optical_Depth = ZERO


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the absorber loop.
      ! -----------------------------------------
      IF ( TC(n)%Predictor_Index(0,j,l) <= 0 ) CYCLE Absorber_Loop

 
      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        ! ----------------------------------------------------------
        AAV%A_Level(k,j) = LOG((Predictor%aveA(k,j) - TC(n)%Alpha_C2(j)) / TC(n)%Alpha_C1(j)) / &
        !                  ------------------------------------------------------------------
                                                  TC(n)%Alpha(j)


        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          np
        !   b(i) =  > c(np,i).k
        !          /__
        !             np=0
        !
        ! NOTE:
        ! 1) The coefficient array, c(np,i), corresponds to the array
        !    TC(n)%C(np,i,j,l) for the given absorber, j, channel, l,
        !    and sensor, n.
        !
        ! 2) The summation maximum, N, corresponds to the value in
        !    TC(n)%Order_Index(i,j,l) for the given absorber, j, 
        !    channel, l, and sensor, n. If
        !      TC(n)%Order_Index(i,j,l) = 0
        !    then
        !      b(i) = c(0,i)
        !
        ! 3) TC(n)%Order_Index( i, j, l ) contains the polynomial
        !    order to be used in reconstructing the b(i) coefficients
        !    for the current predictor, i, for absorber j, channel l,
        !    and sensor, n. This value is used to access the coefficient
        !    array, TC(n)%C.
        !
        ! 4) Note that if
        !      TC(n)%Order_Index( 0, j, l ) == 0
        !    then the inner loop below is not entered. If your compiler
        !    has a "zero trip" loop option where DO loops are *always*
        !    executed at least once, regardless of the loop indices,
        !    make sure it's not on by default!
        ! ----------------------------------------------------------------
        DO i = 0, TC(n)%Predictor_Index(0,j,l)
          AAV%b(TC(n)%Order_Index(i,j,l),i,k,j) = TC(n)%C( TC(n)%Order_Index(i,j,l),i,j,l )
          DO np = TC(n)%Order_Index(i,j,l) - 1, 0, -1
            AAV%b(np,i,k,j) = ( AAV%b(np+1,i,k,j) * AAV%A_Level(k,j) ) + TC(n)%C(np,i,j,l)
          END DO
        END DO


        ! ---------------------------------------------------------
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).X(i)
        !                    /__
        !                       i=1
        !
        ! ---------------------------------------------------------
        AAV%LN_Chi(k,j) = AAV%b(0,0,k,j)
        DO i = 1, TC(n)%Predictor_Index(0,j,l)
          ip = TC(n)%Predictor_Index(i,j,l)
          AAV%LN_Chi(k,j) = AAV%LN_Chi(k,j) + ( AAV%b(0,i,k,j) * Predictor%X(ip,k) )
        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------
        IF( AAV%LN_Chi(k,j) > LIMIT_EXP ) THEN
          AAV%Chi(k,j) = LIMIT_LOG
        ELSE IF( AAV%LN_Chi(k,j) < -LIMIT_EXP ) THEN
          AAV%Chi(k,j) = ZERO
        ELSE
          AAV%Chi(k,j) = EXP(AAV%LN_Chi(k,j))
        ENDIF


        ! -----------------------
        ! Calculate optical_depth
        ! -----------------------
        AtmAbsorption%Optical_Depth(k) = AtmAbsorption%Optical_Depth(k) + &
                                         ( AAV%Chi(k,j) * Predictor%dA(k,j) )

      END DO Layer_Loop

    END DO Absorber_Loop


    ! --------------------------------
    ! Scale the optical depth to nadir
    ! --------------------------------
    AtmAbsorption%Optical_Depth = AtmAbsorption%Optical_Depth / &
                                  Predictor%Secant_Sensor_Zenith

  END SUBROUTINE CRTM_Compute_AtmAbsorption


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer optical depths due
!       to gaseous absorption for a given sensor and channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
!                                           ChannelIndex    , &  ! Input
!                                           Predictor       , &  ! FWD Input
!                                           Predictor_TL    , &  ! TL Input
!                                           AtmAbsorption_TL, &  ! TL Output
!                                           AAVariables       )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor_TL:       Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!        AtmAbsorption_TL:  Structure containing the computed tangent-linear
!                           optical depth profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! COMMENTS:
!       Note the INTENT on the AtmAbsorption_TL argument is IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_TL( SensorIndex     , &  ! Input
                                            ChannelIndex    , &  ! Input
                                            Predictor       , &  ! Input
                                            Predictor_TL    , &  ! Input
                                            AtmAbsorption_TL, &  ! Output
                                            AAV               )  ! Internal variable input
    ! Arguments
    INTEGER                      , INTENT(IN)     :: SensorIndex
    INTEGER                      , INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type)    , INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type)    , INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    TYPE(CRTM_AAVariables_type)  , INTENT(IN)     :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_TL'
    ! Local variables
    INTEGER :: n       ! Sensor index
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: np      ! Polynomial index
    REAL(fp) :: A_Level_TL
    REAL(fp) :: LN_Chi_TL
    REAL(fp) :: Chi_TL
    ! Polynomial derived coefficients
    REAL(fp), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b_TL


    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n = SensorIndex
    l = ChannelIndex
    ! Initilise the tangent-linear optical depth
    AtmAbsorption_TL%Optical_Depth = ZERO


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_Loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the absorber loop.
      ! -----------------------------------------
      IF ( TC(n)%Predictor_Index(0,j,l) <= 0 ) CYCLE Absorber_Loop


      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = 1, AtmAbsorption_TL%n_Layers


        ! ----------------------------------------------------------
        ! Calculate absorber space level associated with the average
        ! absorber amount
        ! 
        ! Absorber level, k, to amount 
        ! 
        !     A(k) = C1.exp(Alpha * k) + C2
        ! 
        ! Absorber amount to level 
        ! 
        !           1      A - C2
        !     k = ----- LN ------
        !         Alpha      C1
        ! 
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        !
        ! The tangent-linear equation is
        !
        !                   dA
        !     dk(A) = ----------------
        !             Alpha.( A - C2 )
        !
        ! ----------------------------------------------------------
        A_Level_TL =                  Predictor_TL%aveA(k,j) / &
        !             ---------------------------------------------------------------
                      ( TC(n)%Alpha(j) * (Predictor%aveA(k,j) - TC(n)%Alpha_C2(j) ) )



        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          np
        !   b(i) =  > c(np,i).k
        !          /__
        !             np=0
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \              np-1
        !   db(i) =  > c(np,i).np.k    dk
        !           /__
        !              np=0
        !
        ! NOTE:
        ! 1) Note the actual computation of the b(i) and db(i) use a 
        !    recurrance relation, Horner's method, starting at the
        !    maximum polynomial order, N, to minimise round off error.
        !    So for a given predictor index i, we accumulate the value
        !    of b for successive orders of the N'th degree polynomial:
        !
        !      N:   b[N]   = c[N]
        !      N-1: b[N-1] = b[N].k + c[N-1]
        !                  = c[N].k + c[N-1]
        !      N-2: b[N-2] = b[N-1].k + c[N-2]
        !                  = (c[N].k + c[N-1]).k + c[N-1]
        !      N-3: b[N-3] = b[N-2].k + c[N-3]
        !                  = ((c[N].k + c[N-1]).k + c[N-1]).k + c[N-3]
        !    etc.
        !
        !    So for any polynomial order, np,
        !
        !      b[np] = b[np-1].k + c(np)
        !
        !    Thus the tangent linear form for db[np] is,
        !
        !     db[np] = b[np-1].dk  +  db[np-1].k
        !
        !    This means the tangent linear form, db[np] must be computed
        !    BEFORE the b[np-1] is updated to the b[np] value. This is
        !    noted in the code below also.
        !
        ! 2) The coefficient array, c(np,i), corresponds to the array
        !    TC(n)%C(np,i,j,l) for the given absorber, j, channel, l,
        !    and sensor, n
        !
        ! 3) The summation maximum, N, corresponds to the value in
        !    TC(n)%Order_Index(i,j,l) for the given absorber, j, 
        !    channel, l, and sensor, n. If
        !      TC(n)%Order_Index(i,j,l) = 0
        !    then
        !      b(i)    = c(0,i)
        !    and
        !      b_TL(i) = 0.0
        !
        ! 4) TC(n)%Order_Index( i, j, l ) contains the polynomial
        !    order to be used in reconstructing the b(i) coefficients
        !    for the current predictor, i, for absorber, j, channel, l,
        !    and sensor, n. This value is used to access the coefficient
        !    array, TC(n)%C.
        !
        ! 5) Note that if
        !      TC(n)%Order_Index( 0, j, l ) == 0
        !    then the inner loop below is not entered. If your compiler
        !    has a "zero trip" loop option where DO loops are *always*
        !    executed at least once, regardless of the loop indices,
        !    make sure it's not on by default!
        ! ----------------------------------------------------------------
        DO i = 0, TC(n)%Predictor_Index(0,j,l)
          b_TL(i) = ZERO
          DO np = TC(n)%Order_Index(i,j,l) - 1, 0, -1
            b_TL(i) = ( AAV%b(np+1,i,k,j) * A_Level_TL ) + ( b_TL(i) * AAV%A_Level(k,j) )
          END DO
        END DO

        ! ---------------------------------------------------------
        ! Compute the logarithm of the absorption coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).X(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dX(i)) + (db(i).X(i)) 
        !              /__
        !                 i=1
        !
        ! ---------------------------------------------------------
        LN_Chi_TL = b_TL(0)
        DO i = 1, TC(n)%Predictor_Index(0,j,l)
          ip = TC(n)%Predictor_Index(i,j,l)
          LN_Chi_TL = LN_Chi_TL + ( AAV%b(0,i,k,j) * Predictor_TL%X(ip,k) ) + &
                                  ( b_TL(i)        * Predictor%X(   ip,k) )
        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------
        IF( AAV%LN_Chi(k,j) > LIMIT_EXP ) THEN
          Chi_TL = ZERO
        ELSE IF( AAV%LN_Chi(k,j) < -LIMIT_EXP ) THEN
          Chi_TL = ZERO
        ELSE
          Chi_TL = AAV%Chi(k,j) * LN_Chi_TL
        ENDIF


        ! ------------------------------------------
        ! Calculate the tangent-linear optical depth
        ! ------------------------------------------
        AtmAbsorption_TL%Optical_Depth(k) = AtmAbsorption_TL%Optical_Depth(k) + &
                                            ( Chi_TL       * Predictor%dA(k,j)    ) + &
                                            ( AAV%Chi(k,j) * Predictor_TL%dA(k,j) )

      END DO Layer_Loop

    END DO Absorber_Loop


    ! -----------------------------------------------
    ! Scale the tangent-linear optical depth to nadir
    ! -----------------------------------------------
    AtmAbsorption_TL%Optical_Depth = AtmAbsorption_TL%Optical_Depth / &
                                     Predictor%Secant_Sensor_Zenith

  END SUBROUTINE CRTM_Compute_AtmAbsorption_TL


!--------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Compute_AtmAbsorption_AD
!
! PURPOSE:
!       Subroutine to calculate the layer optical depth adjoints due to
!       gaseous absorption for a given sensor and channel and atmospheric
!       profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
!                                           ChannelIndex    , &  ! Input
!                                           Predictor       , &  ! FWD Input
!                                           AtmAbsorption_AD, &  ! TL  Input
!                                           Predictor_AD    , &  ! TL  Output
!                                           AAVariables       )  ! Internal variable input
!
! INPUT ARGUMENTS:
!       SensorIndex:        Sensor index id. This is a unique index associated
!                           with a (supported) sensor used to access the
!                           shared coefficient data for a particular sensor.
!                           See the ChannelIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ChannelIndex:       Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data for a particular sensor's
!                           channel.
!                           See the SensorIndex argument.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Predictor:          Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       AtmAbsorption_AD:   Structure containing the computed adjoint
!                           optical depth profile data.
!                           Set to zero upon output.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!       AAVariables:        Structure containing internal variables required for
!                           subsequent tangent-linear or adjoint model calls.
!                           The contents of this structure are NOT accessible
!                           outside of the CRTM_AtmAbsorption module.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AAVariables_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OUTPUT ARGUMENTS:
!       Predictor_AD:       Structure containing the adjoint integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_AD( SensorIndex     , &  ! Input
                                            ChannelIndex    , &  ! Input
                                            Predictor       , &  ! FWD Input
                                            AtmAbsorption_AD, &  ! AD  Input
                                            Predictor_AD    , &  ! AD  Output
                                            AAV               )  ! Internal variable input
    ! Arguments
    INTEGER,                       INTENT(IN)     :: SensorIndex
    INTEGER,                       INTENT(IN)     :: ChannelIndex
    TYPE(CRTM_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(CRTM_Predictor_type),     INTENT(IN OUT) :: Predictor_AD
    TYPE(CRTM_AAVariables_type)  , INTENT(IN)     :: AAV
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_AD'
    ! Local variables
    INTEGER :: n       ! Sensor index
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: np      ! Polynomial index
    REAL(fp) :: A_Level_AD
    REAL(fp) :: LN_Chi_AD
    REAL(fp) :: Chi_AD
    ! Polynomial derived coefficients
    REAL(fp), DIMENSION(0:MAX_N_PREDICTORS_USED) :: b_AD


    ! ------
    ! Set up
    ! ------
    ! Assign the indices to a short name
    n = SensorIndex
    l = ChannelIndex
    ! Initilise the local adjoint variables
    A_Level_AD = ZERO


    ! -------------------------------------------
    ! Compute adjoint nadir optical depth profile
    ! -------------------------------------------
    AtmAbsorption_AD%Optical_Depth = AtmAbsorption_AD%Optical_Depth / &
                                     Predictor%Secant_Sensor_Zenith


    ! -----------------------------------------------------
    ! Loop over each absorber for optical depth calculation
    ! -----------------------------------------------------
    Absorber_loop: DO j = 1, Predictor%n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the Absorber loop.
      ! -----------------------------------------
      IF ( TC(n)%Predictor_index(0,j,l) == 0 ) CYCLE Absorber_loop


      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = AtmAbsorption_AD%n_Layers, 1, -1


        ! -----------------------------
        ! Adjoints of the optical depth
        ! -----------------------------
        Predictor_AD%dA(k,j) = Predictor_AD%dA(k,j) + &
                               (AAV%Chi(k,j) * AtmAbsorption_AD%Optical_Depth(k))
        Chi_AD = Predictor%dA(k,j) * AtmAbsorption_AD%Optical_Depth(k)


        ! ----------------------------------------
        ! Initialise the LOCAL adjoint variable,
        !   LN_Chi_AD.
        ! Note that the reinitialisaiton of the
        ! LOCAL adjoint variable
        !   Absorption_Coefficient_AD
        ! is implied since for each layer it is
        ! reassigned in the preceding line of code
        ! ----------------------------------------
        IF( ABS( AAV%LN_Chi(k,j) ) > LIMIT_EXP ) THEN
          LN_Chi_AD = ZERO
        ELSE
          LN_Chi_AD = AAV%Chi(k,j) * Chi_AD
        ENDIF


        ! ---------------------------------------------------------
        ! Compute the adjoint of the logarithm of the absorption
        ! coefficient
        !
        ! The logarithm of the absorption coefficient, LN(chi), is
        ! determined from the regression equation,
        !
        !                     __Iuse
        !                    \
        !   LN(chi) = b(0) +  > b(i).X(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dX(i)) + (db(i).X(i)) 
        !              /__
        !                 i=1
        !
        ! So the adjoint forms are for each predictor index i,
        !               
        !    *        *             *
        !   d X(i) = d X(i) + b(i).d LN(chi)
        !
        !
        ! and,
        !
        !
        !    *             *
        !   d b(i) = X(i).d LN(chi)
        !
        !            *
        ! where the d  indicates an adjoint variable. Note two
        ! things:
        ! 1) the order of the loop is not important.
        ! 2) the b coefficient adjoints are local adjoint variables
        !    and are thus initialised to their value on each
        !    iteration. I.e. no b_AD = ZERO before the loop.
        !
        ! ---------------------------------------------------------
        DO i = 1, TC(n)%Predictor_Index(0,j,l)
          ip = TC(n)%Predictor_Index(i,j,l)
          Predictor_AD%X(ip,k) = Predictor_AD%X(ip,k) + ( AAV%b(0,i,k,j)*LN_Chi_AD )
          b_AD(i) = Predictor%X(ip,k) * LN_Chi_AD
        END DO 
        b_AD(0) = LN_Chi_AD
        LN_Chi_AD = ZERO


        ! ----------------------------------------------------------------
        ! Compute the adjoints of the coefficients use with the
        ! atmospheric predictors.
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !                    __ N
        !                   \         np
        !   b(i) = c(0,i) +  > c(np,i)  . k
        !                   /__
        !                      np=1
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \              np-1
        !   db(i) =  > c(np,i).np.k    dk
        !           /__
        !              np=1
        !
        ! and the adjoint forms are,
        !
        !
        !             __ 1
        !    *       \           *
        !   d k(i) =  > b(np,i).d b
        !            /__
        !               np=N
        !
        ! and
        !
        !    *           *
        !   d b(i) = k.d b(i)
        !
        ! ----------------------------------------------------------------
        DO i = 0, TC(n)%Predictor_Index(0,j,l)
          ! Note that the order of the A_Level_AD and b_AD
          ! calculation are important
          DO np = 0, TC(n)%Order_Index(i,j,l) - 1
            A_Level_AD = A_Level_AD + ( AAV%b(np+1,i,k,j) * b_AD(i) )
            b_AD(i) = AAV%A_Level(k,j) * b_AD(i)
          END DO
          b_AD(i) = ZERO
        END DO


        ! ----------------------------------------------------------
        ! Calculate the adjoint of the absorber space level
        ! associated with average absorber amount
        !
        ! Absorber level to amount
        !
        !     A(k) = C1 exp(Alpha * k) + C2
        !
        ! Absorber amount to level
        !
        !              1      A - C2
        !     k(A) = ----- ln ------
        !            Alpha      C1
        !
        !   Alpha : absorber amount-level coordinate constant
        !   C1,C2 : scaling factors for level in the range of 0 to 1
        !
        ! The tangent-linear equation is
        !
        !                   dA
        !     dk(A) = ----------------
        !             Alpha.( A - C2 )
        !
        ! and the adjoint form is
        !
        !                *
        !    *          d k
        !   d A = ----------------
        !         Alpha.( A - C2 )
        !
        ! ----------------------------------------------------------
        Predictor_AD%aveA(k,j) = Predictor_AD%aveA(k,j) + &
                                       A_Level_AD / &
        !          --------------------------------------------------------------
                   ( TC(n)%Alpha(j) * (Predictor%aveA(k,j) - TC(n)%Alpha_C2(j) ))

        A_Level_AD = ZERO

      END DO Layer_Loop

    END DO Absorber_Loop

  END SUBROUTINE CRTM_Compute_AtmAbsorption_AD

END MODULE CRTM_AtmAbsorption
