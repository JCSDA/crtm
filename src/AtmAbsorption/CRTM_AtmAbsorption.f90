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
  USE Type_Kinds,                ONLY: fp=>fp_kind
  USE Message_Handler,           ONLY: SUCCESS, FAILURE, Display_Message
  USE CRTM_Parameters,           ONLY: ZERO, POINT_5, &
                                       LIMIT_EXP, LIMIT_LOG, &
                                       MAX_N_ABSORBERS, &
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


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: CRTM_AtmAbsorption.f90,v 2.8 2006/08/31 17:02:26 frpv Exp $'


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
!       absorption for a given input atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption( Channel_Index, &  ! Input
!                                        Predictor,     &  ! Input
!                                        AtmAbsorption  )  ! Output
!
! INPUT ARGUMENTS:
!       Channel_Index:   Channel index id. This is a unique index associated
!                        with a (supported) sensor channel used to access the
!                        shared coefficient data.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!        Predictor:      Structure containing the integrated absorber and
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
! COMMENTS:
!       Note the INTENT on the structure arguments are IN OUT rather
!       than just OUT. This is necessary because the argument is defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption( Channel_Index, &  ! Input, scalar
                                         Predictor,     &  ! Input
                                         AtmAbsorption  )  ! Output
    ! Arguments
    INTEGER,                       INTENT(IN)     :: Channel_Index
    TYPE(CRTM_Predictor_type),     INTENT(IN OUT) :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption'
    ! Local variables
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n       ! Polynomial index
    REAL(fp) :: ave_A
    REAL(fp) :: d_A
    REAL(fp) :: A_Level
    REAL(fp) :: LN_Chi
    REAL(fp) :: Absorption_Coefficient
    ! Polynomial derived coefficients
    REAL(fp), DIMENSION(0:MAX_N_PREDICTORS_USED) :: b

    ! ------
    ! Set up
    ! ------
    ! Assign the channel index to a short name
    l = Channel_Index
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
      IF ( TC%Predictor_Index( 0, j, l ) <= 0 ) CYCLE Absorber_Loop

 
      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = 1, AtmAbsorption%n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! integrated absorber amount and difference
        ! -----------------------------------
        ave_A = POINT_5 * ( Predictor%A(k,j) + Predictor%A(k-1,j) )
        d_A = Predictor%A(k,j) - Predictor%A(k-1,j)


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
        A_Level = LOG( ( ave_A - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !         --------------------------------------------------
                                       TC%Alpha(j)


        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! NOTE:
        ! 1) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 2) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i) = c(0,i)
        !
        ! 3) TC%Order_Index( i, j, l ) contains the polynomial
        !    order to be used in reconstructing the b(i) coefficients
        !    for the current predictor, i, for absorber j and channel l.
        !    This value is used to access the coefficient array, TC%C.
        !
        ! 4) Note that if
        !      TC%Order_Index( 0, j, l ) == 0
        !    then the inner loop below is not entered. If your compiler
        !    has a "zero trip" loop option where DO loops are *always*
        !    executed at least once, regardless of the loop indices,
        !    make sure it's not on by default!
        ! ----------------------------------------------------------------
        DO i = 0, TC%Predictor_Index(0,j,l)
          b(i) = TC%C( TC%Order_Index(i,j,l),i,j,l )
          DO n = TC%Order_Index(i,j,l ) - 1, 0, -1
            b(i) = ( b(i) * A_Level ) + TC%C(n,i,j,l)
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
        LN_Chi = b(0)
        DO i = 1, TC%Predictor_Index(0,j,l)
          ip = TC%Predictor_Index(i,j,l)
          LN_Chi = LN_Chi + ( b(i) * Predictor%X(ip,k) )
        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------
        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient = LIMIT_LOG
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient = ZERO
        ELSE
          Absorption_Coefficient = EXP( LN_Chi )
        ENDIF


        ! -----------------------
        ! Calculate optical_depth
        ! -----------------------
        AtmAbsorption%Optical_Depth( k ) = AtmAbsorption%Optical_Depth( k ) + &
                                           ( Absorption_Coefficient * d_A )

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
!       Subroutine to calculate the tangent-linear layer optical depths
!       due to gaseous absorption for a given input atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_TL( Channel_Index,    &  ! Input
!                                           Predictor,        &  ! FWD Input
!                                           Predictor_TL,     &  ! TL Input
!                                           AtmAbsorption_TL  )  ! TL Output
!
! INPUT ARGUMENTS:
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!        Predictor:         Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!        Predictor_TL:      Structure containing the tangent-linear integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
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

  SUBROUTINE CRTM_Compute_AtmAbsorption_TL( Channel_Index,   &  ! Input
                                            Predictor,       &  ! Input
                                            Predictor_TL,    &  ! Input
                                            AtmAbsorption_TL )  ! Output
    ! Arguments
    INTEGER,                       INTENT(IN)     :: Channel_Index
    TYPE(CRTM_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_Predictor_type),     INTENT(IN)     :: Predictor_TL
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_TL
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_TL'
    ! Local variables
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n       ! Polynomial index
    REAL(fp) :: ave_A,   ave_A_TL
    REAL(fp) :: d_A,     d_A_TL
    REAL(fp) :: A_Level, A_Level_TL
    REAL(fp) :: LN_Chi,  LN_Chi_TL
    REAL(fp) :: Absorption_Coefficient, Absorption_Coefficient_TL
    ! Polynomial derived coefficients
    REAL(fp), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b, b_TL


    ! ------
    ! Set up
    ! ------
    ! Assign the channel index to a short name
    l = Channel_Index
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
      IF ( TC%Predictor_Index( 0, j, l ) <= 0 ) CYCLE Absorber_Loop


      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = 1, AtmAbsorption_TL%n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and difference
        ! -----------------------------------
        ave_A    = POINT_5*(Predictor%A(k,j)    + Predictor%A(k-1,j))
        ave_A_TL = POINT_5*(Predictor_TL%A(k,j) + Predictor_TL%A(k-1,j))

        d_A    = Predictor%A(k,j)    - Predictor%A(k-1,j)
        d_A_TL = Predictor_TL%A(k,j) - Predictor_TL%A(k-1,j)


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
        A_Level = LOG( ( ave_A - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !         --------------------------------------------------
                                      TC%Alpha(j)

        A_Level_TL =                ave_A_TL / &
        !             --------------------------------------------
                      ( TC%Alpha(j) * ( ave_A - TC%Alpha_C2(j) ) )



        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \            n-1
        !   db(i) =  > c(n,i).n.k    dk
        !           /__
        !              n=0
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
        !    So for any polynomial order, n,
        !
        !      b[n] = b[n-1].k + c(n)
        !
        !    Thus the tangent linear form for db[n] is,
        !
        !     db[n] = b[n-1].dk  +  db[n-1].k
        !
        !    This means the tangent linear form, db[n] must be computed
        !    BEFORE the b[n-1] is updated to the b[n] value. This is
        !    noted in the code below also.
        !
        ! 2) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 3) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i)    = c(0,i)
        !    and
        !      b_TL(i) = 0.0
        !
        ! 4) TC%Order_Index( i, j, l ) contains the polynomial
        !    order to be used in reconstructing the b(i) coefficients
        !    for the current predictor, i, for absorber j and channel l.
        !    This value is used to access the coefficient array, TC%C.
        !
        ! 5) Note that if
        !      TC%Order_Index( 0, j, l ) == 0
        !    then the inner loop below is not entered. If your compiler
        !    has a "zero trip" loop option where DO loops are *always*
        !    executed at least once, regardless of the loop indices,
        !    make sure it's not on by default!
        ! ----------------------------------------------------------------
        DO i = 0, TC%Predictor_Index(0,j,l)
          b(i)    = TC%C( TC%Order_Index(i,j,l),i,j,l )
          b_TL(i) = ZERO
          ! NOTE: The tangent-linear term is calculated FIRST
          !       See explanation note 1) above.
          DO n = TC%Order_Index(i,j,l ) - 1, 0, -1
            b_TL(i) = ( b(i) * A_Level_TL ) + ( b_TL(i) * A_Level )
            b(i)    = ( b(i) * A_Level ) + TC%C(n,i,j,l)
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
        LN_Chi = b(0)
        LN_Chi_TL = b_TL(0)
        DO i = 1, TC%Predictor_Index(0,j,l)
          ip = TC%Predictor_Index(i,j,l)
          LN_Chi    = LN_Chi    + ( b(i) * Predictor%X(ip,k) )
          LN_Chi_TL = LN_Chi_TL + ( b(i)    * Predictor_TL%X(ip,k) ) + &
                                  ( b_TL(i) * Predictor%X(   ip,k) )
        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------
        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient    = LIMIT_LOG
          Absorption_Coefficient_TL = ZERO
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient    = ZERO
          Absorption_Coefficient_TL = ZERO
        ELSE
          Absorption_Coefficient    = EXP( LN_Chi )
          Absorption_Coefficient_TL = Absorption_Coefficient * LN_Chi_TL
        ENDIF


        ! ------------------------------------------
        ! Calculate the tangent-linear optical depth
        ! ------------------------------------------
        AtmAbsorption_TL%Optical_Depth( k ) = AtmAbsorption_TL%Optical_Depth( k ) + &
                                              ( Absorption_Coefficient_TL * d_A    ) + &
                                              ( Absorption_Coefficient    * d_A_TL )

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
!       Function to calculate the layer optical depths adjoint due
!       to gaseous absorption for a given input atmospheric profile.
!
! CALLING SEQUENCE:
!       CALL CRTM_Compute_AtmAbsorption_AD( Channel_Index,    &  ! Input
!                                           Predictor,        &  ! FWD Input
!                                           AtmAbsorption_AD, &  ! TL Input
!                                           Predictor_AD      )  ! TL Output
!
! INPUT ARGUMENTS:
!       Channel_Index:      Channel index id. This is a unique index associated
!                           with a (supported) sensor channel used to access the
!                           shared coefficient data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!        Predictor:         Structure containing the integrated absorber and
!                           predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!        AtmAbsorption_AD:  Structure containing the computed adjoint
!                           optical depth profile data.
!                           Set to zero upon output.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_AtmAbsorption_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OUTPUT ARGUMENTS:
!        Predictor_AD:      Structure containing the adjoint integrated
!                           absorber and predictor profile data.
!                           UNITS:      N/A
!                           TYPE:       TYPE(CRTM_Predictor_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
!
! SIDE EFFECTS:
!       Components of the AtmAbsorption_AD structure argument are modified
!       in this function.
!
!------------------------------------------------------------------------------

  SUBROUTINE CRTM_Compute_AtmAbsorption_AD( Channel_Index,    &  ! Input
                                            Predictor,        &  ! FWD Input
                                            AtmAbsorption_AD, &  ! TL Input
                                            Predictor_AD      )  ! TL Output
    ! Arguments
    INTEGER,                       INTENT(IN)     :: Channel_Index
    TYPE(CRTM_Predictor_type),     INTENT(IN)     :: Predictor
    TYPE(CRTM_AtmAbsorption_type), INTENT(IN OUT) :: AtmAbsorption_AD
    TYPE(CRTM_Predictor_type),     INTENT(IN OUT) :: Predictor_AD
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Compute_AtmAbsorption_AD'
    ! Local variables
    INTEGER :: l       ! Channel index
    INTEGER :: k       ! Layer index
    INTEGER :: j       ! Absorber index
    INTEGER :: i, ip   ! Predictor index
    INTEGER :: n       ! Polynomial index
    REAL(fp) :: ave_A,   ave_A_AD
    REAL(fp) :: d_A,     d_A_AD
    REAL(fp) :: A_Level, A_Level_AD
    REAL(fp) :: LN_Chi,  LN_Chi_AD
    REAL(fp) :: Absorption_Coefficient, Absorption_Coefficient_AD
    ! Polynomial derived coefficients
    REAL(fp), DIMENSION(0:MAX_N_ORDERS, &
                        0:MAX_N_PREDICTORS_USED) :: b
    REAL(fp), DIMENSION(0:MAX_N_PREDICTORS_USED) :: b_AD


    ! ------
    ! Set up
    ! ------
    ! Assign the channel index to a short name
    l = Channel_Index
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
      IF ( TC%Predictor_index( 0, j, l ) == 0 ) CYCLE Absorber_loop


      ! ----------------
      ! Loop over layers
      ! ----------------
      Layer_Loop: DO k = AtmAbsorption_AD%n_Layers, 1, -1


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and differences
        ! -----------------------------------
        ave_A = POINT_5 * ( Predictor%A(k,j) + Predictor%A(k-1,j) )
        d_A   = Predictor%A(k,j) - Predictor%A(k-1,j)



        !#----------------------------------------------------------------------#
        !#           -- HERE REPEAT THE FORWARD CALCULATION OF THE   --         #
        !#           -- ABSORPTION COEFFICIENT FOR THE CURRENT LAYER --         #
        !#----------------------------------------------------------------------#

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
        A_Level = LOG( ( ave_A - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !         --------------------------------------------------
                                     TC%Alpha(j)


        ! ----------------------------------------------------------------
        ! Compute the coefficients for use with the atmospheric predictors
        !
        ! For every atmospheric predictor, Pred(i), the coefficient
        ! associated with it, b(i), at a particular absorber amount
        ! level, k, is given by an N'th order polynomial,
        !
        !           __ N
        !          \          n
        !   b(i) =  > c(n,i).k
        !          /__
        !             n=0
        !
        ! NOTE:
        ! 1) The actual b coefficient array is dimensioned as b(n,i) as
        !    we will need the accumulated b(i) values at each stage of the
        !    polynomial summation for computing the adjoint value.
        !
        ! 2) The coefficient array, c(n,i), corresponds to the array
        !    TC%C(n,i,j,l) for the given absorber, j, and channel, l.
        !
        ! 3) The summation maximum, N, corresponds to the value in
        !    TC%Order_Index(i,j,l) for the given absorber, j, and
        !    channel, l. If
        !      TC%Order_Index(i,j,l) = 0
        !    then
        !      b(i) = c(0,i)
        !
        ! 4) TC%Order_Index( i, j, l ) contains the polynomial
        !    order to be used in reconstructing the b(i) coefficients
        !    for the current predictor, i, for absorber j and channel l.
        !    This value is used to access the coefficient array, TC%C.
        !
        ! 5) Note that if
        !      TC%Order_Index( 0, j, l ) == 0
        !    then the inner loop below is not entered. If your compiler
        !    has a "zero trip" loop option where DO loops are *always*
        !    executed at least once, regardless of the loop indices,
        !    make sure it's not on by default!
        ! ----------------------------------------------------------------
        DO i = 0, TC%Predictor_Index( 0, j, l )
          b( TC%Order_Index(i,j,l),i ) = TC%C( TC%Order_Index(i,j,l),i,j,l )
          DO n = TC%Order_Index(i,j,l) - 1, 0, -1
            b(n,i) = ( b(n+1,i) * A_Level ) + TC%C(n,i,j,l)
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
        ! Note that only the final, accumulated results for the
        ! b coefficients, the b(0,i) are used. The b(1:N,i) are
        ! used in the adjoint form of the calculation that produced
        ! the b coefficient values.
        ! ---------------------------------------------------------
        LN_Chi = b(0,0)
        DO i = 1, TC%Predictor_Index(0,j,l)
          ip = TC%Predictor_Index(i,j,l)
          LN_Chi = LN_Chi + ( b(0,i) * Predictor%X(ip,k) )
        END DO 


        ! --------------------------------
        ! Check the value of the logarithm
        ! of the absorption coefficient 
        ! --------------------------------
        IF( LN_Chi > LIMIT_EXP ) THEN
          Absorption_Coefficient = LIMIT_LOG
        ELSE IF( LN_Chi < -LIMIT_EXP ) THEN
          Absorption_Coefficient = ZERO
        ELSE
          Absorption_Coefficient = EXP( LN_Chi )
        ENDIF



        !#----------------------------------------------------------------------#
        !#                  -- BEGIN ADJOINT CALCULATIONS --                    #
        !#----------------------------------------------------------------------#

        ! ---------------------------------------------------------------
        ! Adjoints of the optical depth.
        !
        ! These quantities are local to the Layer_Loop and are equal to
        ! zero at this point so a straight initialisation is used, i.e.
        ! there is no
        !   d_IntAbsorber_AD          = d_IntAbsorber_AD + (...)
        !   Absorption_Coefficient_AD = Absorption_Coefficient_AD + (...)
        ! This also eliminates the need to zero out the two
        ! quanitities later in the loop once they no longer
        ! have an impact on the gradient vector result.
        !
        ! Also not that there is no
        !   AtmAbsorption_AD%Optical_Depth( k ) = ZERO
        ! because
        !   AtmAbsorption_TL%Optical_Depth( k ) = AtmAbsorption_TL%Optical_Depth( k ) + (....)
        ! ---------------------------------------------------------------
        d_A_AD = Absorption_Coefficient * AtmAbsorption_AD%Optical_Depth( k )   ! .... (1)
        Absorption_Coefficient_AD = d_A * AtmAbsorption_AD%Optical_Depth( k )



        ! ----------------------------------------
        ! Initialise the LOCAL adjoint variable,
        !   LN_Chi_AD.
        ! Note that the reinitialisaiton of the
        ! LOCAL adjoint variable
        !   Absorption_Coefficient_AD
        ! is implied since for each layer it is
        ! reassigned in the preceding line of code
        ! ----------------------------------------
        IF( ABS( LN_Chi ) > LIMIT_EXP ) THEN
          LN_Chi_AD = ZERO
        ELSE
          LN_Chi_AD = Absorption_Coefficient * Absorption_Coefficient_AD
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
        DO i = 1, TC%Predictor_Index(0,j,l)
          ip = TC%Predictor_Index(i,j,l)
          Predictor_AD%X(ip,k) = Predictor_AD%X(ip,k) + ( b(0,i)*LN_Chi_AD )
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
        !                   \          n
        !   b(i) = c(0,i) +  > c(n,i).k
        !                   /__
        !                      n=1
        !
        ! The tangent-linear form is thus
        !
        !            __ N
        !           \            n-1
        !   db(i) =  > c(n,i).n.k    dk
        !           /__
        !              n=1
        !
        ! and the adjoint forms are,
        !
        !
        !             __ 1
        !    *       \          *
        !   d k(i) =  > b(n,i).d b
        !            /__
        !               n=N
        !
        ! and
        !
        !    *           *
        !   d b(i) = k.d b(i)
        !
        ! ----------------------------------------------------------------
        DO i = 0, TC%Predictor_Index(0,j,l)
          ! Note that the order of the A_Level_AD and b_AD
          ! calculation are important
          DO n = 0, TC%Order_Index(i,j,l) - 1
            A_Level_AD = A_Level_AD + ( b(n+1,i) * b_AD(i) )
            b_AD(i) = A_Level * b_AD(i)
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
        ave_A_AD =               A_Level_AD / &
        !          --------------------------------------------  ....(2)
                   ( TC%Alpha(j) * ( ave_A - TC%Alpha_C2(j) ) )

        A_Level_AD = ZERO



        ! ---------------------------------------------------
        ! Adjoints of the current layer average absorber
        ! amount and difference.
        !
        ! Neither d_A_AD nor ave_A_AD need to be set to zero
        ! after this as they are explicitly reassigned each
        ! layer iteration at (1) and (2) above respectively.
        ! ---------------------------------------------------
        Predictor_AD%A(k-1,j) = Predictor_AD%A(k-1,j) - d_A_AD + (POINT_5*ave_A_AD)
        Predictor_AD%A( k, j) = Predictor_AD%A( k, j) + d_A_AD + (POINT_5*ave_A_AD)

      END DO Layer_Loop

    END DO Absorber_Loop

  END SUBROUTINE CRTM_Compute_AtmAbsorption_AD

END MODULE CRTM_AtmAbsorption
