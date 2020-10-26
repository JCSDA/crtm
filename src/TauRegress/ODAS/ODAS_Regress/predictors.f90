
MODULE Predictors


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind
  USE Parameters
  USE Message_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Compute_Predictors
  PUBLIC :: Compute_Predictors_TL
  PUBLIC :: Compute_Predictors_AD


CONTAINS




  SUBROUTINE Compute_Predictors ( Pressure,    &  ! Input,  K
                                  Temperature, &  ! Input,  K
                                  Water_Vapor, &  ! Input,  K
                                  Absorber,    &  ! Input,  0:K x J

                                  Predictor,   &  ! Output, I x K

                                  no_standard  )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Pressure      ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Temperature   ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )            :: Water_Vapor   ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )            :: Absorber      ! 0:K x J

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT )           :: Predictor     ! I x K

    INTEGER,                             INTENT( IN ),  OPTIONAL :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE STANDARD PREDICTORS IF NEEDED --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      CALL Compute_Std_Predictors( Pressure,      &
                                   Temperature,   &
                                   Water_Vapor,   &
                                   Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) )

    END IF


                                                   
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber, DIM = 2 )

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictors for the current absorber
      CALL Compute_Int_Predictors( Pressure, &
                                   Temperature, &
                                   Absorber( 0:, j ), &
                                   Predictor( i1:i2, : ) )

    END DO j_Absorber_loop

  END SUBROUTINE Compute_Predictors




  SUBROUTINE Compute_Std_Predictors( p,        &  ! Input,  K
                                     t,        &  ! Input,  K
                                     w,        &  ! Input,  K
                                     Predictor )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p          ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t          ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w          ! K

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: Predictor  ! Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: p2
    REAL( fp_kind ) :: t2



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE STANDARD PREDICTOR SET --                #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )

      ! -- Calculate and assign the Absorber independent Predictors
      Predictor(  1, k ) = t( k )
      Predictor(  2, k ) = p( k )
      Predictor(  3, k ) = t2
      Predictor(  4, k ) = p2
      Predictor(  5, k ) = t( k ) * p( k )
      Predictor(  6, k ) = t2     * p( k )
      Predictor(  7, k ) = t( k ) * p2
      Predictor(  8, k ) = t2     * p2
      Predictor(  9, k ) = p( k )**POINT_25
      Predictor( 10, k ) = w( k )
      Predictor( 11, k ) = w( k ) / t2

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors







  SUBROUTINE Compute_Int_Predictors( Pressure,    &  ! Input,  K
                                     Temperature, &  ! Input,  K
                                     Absorber,    &  ! Input,  0:K
                                     Predictor    )  ! Output, Iint x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber     ! 0:K

    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Layers
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_Absorber
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Absorber_3

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2

    ! -- Intermediate summation array. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ) ) :: s

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor, DIM=1 ), 0:SIZE( Pressure ) ) :: x



    !#--------------------------------------------------------------------------#
    !#                  -- DETERMINE THE NUMBER PREDICTORS --                   #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    Absorber_2( 0 ) = Absorber(0) * Absorber(0)

    s( : )    = ZERO
    x( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( Pressure )


      ! -----------------------------------------
      ! Calculate Absorber multiplicative Factors
      ! -----------------------------------------

      Absorber_2( k ) = Absorber( k ) * Absorber( k )

      d_Absorber = Absorber( k ) - Absorber( k-1 )                      ! For * terms
      Factor_1   = ( Absorber( k )   + Absorber( k-1 )   ) * d_Absorber ! For ** terms
      Factor_2   = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber ! For *** terms


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      s( 1 ) = s( 1 ) + ( Temperature( k ) * d_Absorber )  ! T*
      s( 2 ) = s( 2 ) + ( Pressure( k )    * d_Absorber )  ! P*

      s( 3 ) = s( 3 ) + ( Temperature( k ) * Factor_1 )    ! T**
      s( 4 ) = s( 4 ) + ( Pressure( k )    * Factor_1 )    ! P**

      s( 5 ) = s( 5 ) + ( Temperature( k ) * Factor_2 )    ! T***
      s( 6 ) = s( 6 ) + ( Pressure( k )    * Factor_2 )    ! P***


      ! -------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! predictors. Note that the checks below, the IF tests to
      ! determine if the absorber products are represenatble
      ! are to minimise the number of calcs. I.e if Inverse_1
      ! is toast because Absorber(k) is too small there's no
      ! need to check any further.
      ! -------------------------------------------------------

      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2  = Inverse_1 * Inverse_1
          Absorber_3 = Absorber( k ) * Absorber_2( k )

          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3 = Inverse_2 * Inverse_1

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO

      END IF Inverse_1_check


      ! ---------------------------------------------
      ! Scale and normalise the integrated Predictors
      ! ---------------------------------------------

      x( 1, k ) = POINT_5  * s( 1 ) * Inverse_1  ! T*
      x( 2, k ) = POINT_5  * s( 2 ) * Inverse_1  ! P*

      x( 3, k ) = POINT_5  * s( 3 ) * Inverse_2  ! T**
      x( 4, k ) = POINT_5  * s( 4 ) * Inverse_2  ! P**

      x( 5, k ) = POINT_75 * s( 5 ) * Inverse_3  ! T***
      x( 6, k ) = POINT_75 * s( 6 ) * Inverse_3  ! P***


      ! ----------------------------
      ! Sum Predictors across Layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor( i, k ) = x( i, k ) + x( i, k-1 )
      END DO

    END DO k_Layer_loop

  END SUBROUTINE Compute_Int_Predictors







  SUBROUTINE Compute_Predictors_TL ( Pressure,       &  ! Input,  K
                                     Temperature,    &  ! Input,  K
                                     Water_Vapor,    &  ! Input,  K
                                     Absorber,       &  ! Input,  0:K x J

                                     Pressure_TL,    &  ! Input,  K
                                     Temperature_TL, &  ! Input,  K
                                     Water_Vapor_TL, &  ! Input,  K
                                     Absorber_TL,    &  ! Input,  0:K x J

                                     Predictor_TL,   &  ! Output, I x K

                                     no_standard     )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Water_Vapor     ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )           :: Absorber        ! 0:K x J

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Pressure_TL     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Temperature_TL  ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )           :: Water_Vapor_TL  ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )           :: Absorber_TL     ! 0:K x J

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT )          :: Predictor_TL    ! I x K

    INTEGER,                             INTENT( IN ), OPTIONAL :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTORS IF NEEDED --     #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      CALL Compute_Std_Predictors_TL( Pressure,         &
                                      Temperature,      &
                                      Water_Vapor,      &

                                      Pressure_TL,      &
                                      Temperature_TL,   &
                                      Water_Vapor_TL,   &

                                      Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) )

    END IF


                                                   
    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE INTEGRATED PREDICTORS --                 #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber_TL, DIM = 2 )

      ! -- Determine indices of current absorber predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Calculate tangent-linear predictors for current absorber
      CALL Compute_Int_Predictors_TL( Pressure,             &
                                      Temperature,          &
                                      Absorber( 0:, j ),    &

                                      Pressure_TL,          &
                                      Temperature_TL,       &
                                      Absorber_TL( 0:, j ), &

                                      Predictor_TL( i1:i2, : ) )

    END DO j_Absorber_loop

  END SUBROUTINE Compute_Predictors_TL






  SUBROUTINE Compute_Std_Predictors_TL( p,           &  ! Input,  K
                                        t,           &  ! Input,  K
                                        w,           &  ! Input,  K

                                        p_TL,        &  ! Input,  K
                                        t_TL,        &  ! Input,  K
                                        w_TL,        &  ! Input,  K

                                        Predictor_TL )  ! Output, Istd x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p              ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w             ! Input,  K

    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: p_TL          ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: t_TL          ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: w_TL          ! Input,  K

    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( OUT ) :: Predictor_TL  ! Output, Istd x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k

    REAL( fp_kind ) :: p2, p2_TL
    REAL( fp_kind ) :: t2, t2_TL



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE TANGENT-LINEAR STANDARD PREDICTOR SET --         #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )

      ! -- Tangent-linear of squared terms
      p2_TL = TWO * p( k ) * p_TL( k )
      t2_TL = TWO * t( k ) * t_TL( k )
      
      ! -- Calculate and assign the Absorber independent Predictors
      Predictor_TL(  1, k ) = t_TL( k )
      Predictor_TL(  2, k ) = p_TL( k )
      Predictor_TL(  3, k ) = t2_TL
      Predictor_TL(  4, k ) = p2_TL
      Predictor_TL(  5, k ) = ( t( k ) * p_TL( k ) ) + ( p( k ) * t_TL( k ) )
      Predictor_TL(  6, k ) = ( p( k ) * t2_TL     ) + ( t2     * p_TL( k ) )
      Predictor_TL(  7, k ) = ( t( k ) * p2_TL     ) + ( p2     * t_TL( k ) )
      Predictor_TL(  8, k ) = ( t2     * p2_TL     ) + ( p2     * t2_TL     )
      Predictor_TL(  9, k ) = POINT_25 * (p(k)**(-POINT_75)) * p_TL(k)
      Predictor_TL( 10, k ) = w_TL( k )
      Predictor_TL( 11, k ) = ( w_TL(k) - ( w(k) * t2_TL / t2 ) ) / t2

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors_TL






  SUBROUTINE Compute_Int_Predictors_TL( Pressure,       &  ! Input,  K
                                        Temperature,    &  ! Input,  K
                                        Absorber,       &  ! Input,  0:K

                                        Pressure_TL,    &  ! Input,  K
                                        Temperature_TL, &  ! Input,  K
                                        Absorber_TL,    &  ! Input,  0:K

                                        Predictor_TL    )  ! Output, Iint x K
 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber        ! 0:K

    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Pressure_TL     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: Temperature_TL  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )  :: Absorber_TL     ! 0:K

    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Predictor_TL    ! Iint x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k
    INTEGER :: n_Predictors

    REAL( fp_kind ) :: d_Absorber
    REAL( fp_kind ) :: d_Absorber_TL
    REAL( fp_kind ) :: Factor_1
    REAL( fp_kind ) :: Factor_1_TL
    REAL( fp_kind ) :: Factor_2
    REAL( fp_kind ) :: Factor_2_TL
    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Absorber_3
    REAL( fp_kind ) :: Absorber_4
    REAL( fp_kind ) :: Inverse_1_TL
    REAL( fp_kind ) :: Inverse_2_TL
    REAL( fp_kind ) :: Inverse_3_TL

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2_TL

    ! -- Intermediate summation arrays. Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ) ) :: s_TL

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_TL, DIM=1 ), 0:SIZE( Pressure ) ) :: x_TL



    !#--------------------------------------------------------------------------#
    !#          -- DETERMINE THE NUMBER OF LAYERS AND PREDICTORS --             #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_TL, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#                         -- INITIALISE VALUES --                          #
    !#--------------------------------------------------------------------------#

    Absorber_2( 0 )    = Absorber(0) * Absorber(0)
    Absorber_2_TL( 0 ) = TWO*Absorber(0) * Absorber_TL(0)

    s( : )       = ZERO
    s_TL( : )    = ZERO
    x_TL( :, 0 ) = ZERO



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE INTEGRATED PREDICTOR SET --               #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( Pressure )


      ! --------------------------------
      ! Calculate multiplicative Factors
      ! --------------------------------

      Absorber_2( k )    = Absorber( k ) * Absorber( k )
      Absorber_2_TL( k ) = TWO * Absorber(k) * Absorber_TL(k)

      ! -- For the * terms
      d_Absorber    = Absorber( k )    - Absorber( k-1 )
      d_Absorber_TL = Absorber_TL( k ) - Absorber_TL( k-1 )

      ! -- For the ** terms
      Factor_1    = ( Absorber( k ) + Absorber( k-1 ) ) * d_Absorber
      Factor_1_TL = ( ( Absorber( k )    + Absorber( k-1 )    ) * d_Absorber_TL ) + &
                    ( ( Absorber_TL( k ) + Absorber_TL( k-1 ) ) * d_Absorber    )

      ! -- For the *** terms       
      Factor_2    = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber
      Factor_2_TL = ( ( Absorber_2( k )    + Absorber_2( k-1 )    ) * d_Absorber_TL ) + &
                    ( ( Absorber_2_TL( k ) + Absorber_2_TL( k-1 ) ) * d_Absorber )


      ! -------------------------------
      ! Calculate the intermediate sums
      ! -------------------------------

      ! -- T*
      s( 1 )    = s( 1 )    + ( Temperature( k )    * d_Absorber    )     ! Forward Predictor
      s_TL( 1 ) = s_TL( 1 ) + ( Temperature_TL( k ) * d_Absorber    ) + &
                              ( Temperature( k )    * d_Absorber_TL )

      ! -- P*
      s( 2 )    = s( 2 )    + ( Pressure( k )       * d_Absorber    )     ! Forward Predictor
      s_TL( 2 ) = s_TL( 2 ) + ( Pressure_TL( k )    * d_Absorber    ) + &
                              ( Pressure( k )       * d_Absorber_TL )

      ! -- T**
      s( 3 )    = s( 3 )    + ( Temperature( k )    * Factor_1    )       ! Forward Predictor
      s_TL( 3 ) = s_TL( 3 ) + ( Temperature_TL( k ) * Factor_1    ) + &
                              ( Temperature( k )    * Factor_1_TL )

      ! -- P**
      s( 4 )    = s( 4 )    + ( Pressure( k )       * Factor_1    )       ! Forward Predictor
      s_TL( 4 ) = s_TL( 4 ) + ( Pressure_TL( k )    * Factor_1    ) + &
                              ( Pressure( k )       * Factor_1_TL )

      ! -- T***
      s( 5 )    = s( 5 )    + ( Temperature( k )    * Factor_2    )       ! Forward Predictor
      s_TL( 5 ) = s_TL( 5 ) + ( Temperature_TL( k ) * Factor_2    ) + &
                              ( Temperature( k )    * Factor_2_TL )

      ! -- P***
      s( 6 )    = s( 6 )    + ( Pressure( k )       * Factor_2    )       ! Forward Predictor
      s_TL( 6 ) = s_TL( 6 ) + ( Pressure_TL( k )    * Factor_2    ) + &
                              ( Pressure( k )       * Factor_2_TL )


      ! ------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! tangent-linear predictors. Note that the checks below,
      ! the IF tests to determine if the absorber products are
      ! represenatble are to minimise the number of calcs. I.e
      ! if Inverse_1 is toast because Absorber(k) is too small
      ! there's no need to check any further.
      ! ------------------------------------------------------

      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2    =  Inverse_1 * Inverse_1
          Inverse_1_TL = -Inverse_2 * Absorber_TL( k )
          Absorber_3   =  Absorber( k ) * Absorber_2( k )

          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3    =  Inverse_2 * Inverse_1
            Inverse_2_TL = -Inverse_3 * Absorber_TL( k ) * TWO
            Absorber_4   =  Absorber( k ) * Absorber_3

            ! -- Is Inverse_4 represenatble?
            Inverse_4_check: IF ( Absorber_4 > TOLERANCE ) THEN

              Inverse_4    =  Inverse_3 * Inverse_1
              Inverse_3_TL = -Inverse_4 * Absorber_TL( k ) * THREE

            ELSE ! Inverse_4_check

              Inverse_3_TL = ZERO

            END IF Inverse_4_check

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO

            Inverse_2_TL = ZERO
            Inverse_3_TL = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO

          Inverse_1_TL = ZERO
          Inverse_2_TL = ZERO
          Inverse_3_TL = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO

        Inverse_1_TL = ZERO
        Inverse_2_TL = ZERO
        Inverse_3_TL = ZERO

      END IF Inverse_1_check


      ! ------------------------------------------------------------
      ! Scale and normalise the tangent-linear integrated Predictors
      ! ------------------------------------------------------------

      ! -- T*
      x_TL( 1, k ) = POINT_5  * ( ( s_TL( 1 ) * Inverse_1    ) + &
                                  ( s( 1 )    * Inverse_1_TL ) )

      ! -- P*
      x_TL( 2, k ) = POINT_5  * ( ( s_TL( 2 ) * Inverse_1    ) + &
                                  ( s( 2 )    * Inverse_1_TL ) )

      ! -- T**
      x_TL( 3, k ) = POINT_5  * ( ( s_TL( 3 ) * Inverse_2    ) + &
                                  ( s( 3 )    * Inverse_2_TL ) )

      ! -- P**
      x_TL( 4, k ) = POINT_5  * ( ( s_TL( 4 ) * Inverse_2    ) + &
                                  ( s( 4 )    * Inverse_2_TL ) )

      ! -- T***
      x_TL( 5, k ) = POINT_75 * ( ( s_TL( 5 ) * Inverse_3    ) + &
                                  ( s( 5 )    * Inverse_3_TL ) )

      ! -- P***
      x_TL( 6, k ) = POINT_75 * ( ( s_TL( 6 ) * Inverse_3    ) + &
                                  ( s( 6 )    * Inverse_3_TL ) )


      ! ----------------------------
      ! Sum Predictors across Layers
      ! ----------------------------

      DO i = 1, n_Predictors
        Predictor_TL( i, k ) = x_TL( i, k ) + x_TL( i, k - 1 )
      END DO

    END DO k_Layer_loop

  END SUBROUTINE Compute_Int_Predictors_TL





  SUBROUTINE Compute_Predictors_AD ( &
                                     ! -- Forward input
                                     Pressure,       &  ! Input, K
                                     Temperature,    &  ! Input, K
                                     Water_Vapor,    &  ! Input, K
                                     Absorber,       &  ! Input, 0:K x J

                                     ! -- Adjoint input
                                     Predictor_AD,   &  ! Input, I x K

                                     ! -- Adjoint output
                                     Pressure_AD,    &  ! In/Output, K
                                     Temperature_AD, &  ! In/Output, K
                                     Water_Vapor_AD, &  ! In/Output, K
                                     Absorber_AD,    &  ! In/Output, 0:K x J

                                     ! -- Optional input
                                     no_standard     )  ! Optional input



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Water_Vapor     ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )     :: Absorber        ! 0:K x J

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )     :: Predictor_AD    ! I x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Pressure_AD     ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Temperature_AD  ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Water_Vapor_AD  ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: Absorber_AD     ! 0:K x J

    ! -- Optional input
    INTEGER,         OPTIONAL,           INTENT( IN )     :: no_standard


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i1, i2, j



    !#--------------------------------------------------------------------------#
    !#         -- CALCULATE THE ADJOINT OF THE INTEGRATED PREDICTORS --         #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, SIZE( Absorber, DIM = 2 )

      ! -- Determine indices of current Absorber Predictors
      i1 = MAX_N_STANDARD_PREDICTORS + ( ( j - 1 ) * MAX_N_INTEGRATED_PREDICTORS ) + 1
      i2 = i1 + MAX_N_INTEGRATED_PREDICTORS - 1

      ! -- Compute the predictor adjoints for the current absorber
      CALL Compute_Int_Predictors_AD( &
                                      ! -- Forward input
                                      Pressure,                 &  ! Input,  K
                                      Temperature,              &  ! Input,  K
                                      Absorber( 0:, j ),        &  ! Input,  0:K

                                      ! -- Adjoint input
                                      Predictor_AD( i1:i2, : ), &  ! Input,  Iint x K
                                                              
                                      ! -- Adjoint output
                                      Pressure_AD,              &  ! In/Output,  K
                                      Temperature_AD,           &  ! In/Output,  K
                                      Absorber_AD( 0:, j )      )  ! In/Output,  0:K
                                                              
    END DO j_Absorber_loop                                    




    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE ADJOINT OF THE STANDARD PREDICTORS IF NEEDED --     #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. PRESENT( no_standard ) ) THEN

      ! -- Compute the predictor adjoints
      CALL Compute_Std_Predictors_AD( &
                                      ! -- Forward input
                                      Pressure,          &  ! Input,  K
                                      Temperature,       &  ! Input,  K
                                      Water_Vapor,       &  ! Input,  K

                                      ! -- Adjoint input
                                      Predictor_AD( 1:MAX_N_STANDARD_PREDICTORS, : ), &  ! Input, Istd x K

                                      ! -- Adjoint output
                                      Pressure_AD,       &  ! In/Output,  K
                                      Temperature_AD,    &  ! In/Output,  K
                                      Water_Vapor_AD     )  ! In/Output,  K

    END IF

  END SUBROUTINE Compute_Predictors_AD






  SUBROUTINE Compute_Std_Predictors_AD( &
                                        ! -- Forward input
                                        p,            &  ! Input,  K
                                        t,            &  ! Input,  K
                                        w,            &  ! Input,  K

                                        ! -- Adjoint input
                                        Predictor_AD, &  ! Input,  Istd x K

                                        ! -- Adjoint output
                                        p_AD,         &  ! In/Output,  K
                                        t_AD,         &  ! In/Output,  K
                                        w_AD          )  ! In/Output,  K




    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: p             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: t             ! Input,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: w             ! Input,  K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )     :: Predictor_AD  ! Input,  Istd x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: p_AD          ! In/Output,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: t_AD          ! In/Output,  K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: w_AD          ! In/Output,  K



    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Std_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n_Predictors
    INTEGER :: k

    REAL( fp_kind ) :: p2, p2_AD
    REAL( fp_kind ) :: t2, t2_AD
    REAL( fp_kind ) :: t4



    !#--------------------------------------------------------------------------#
    !#              -- Determine the number of Predictors --                    #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_AD, DIM = 1 )



    !#--------------------------------------------------------------------------#
    !#        -- Calculate the adjoints of the standard Predictor set --        #
    !#                                                                          #
    !# Don't have to loop backwards here as this is a parallel loop.            #
    !#                                                                          #
    !# Pressure and Temperature squared adjoint terms are not zeroed out every  #
    !# loop iteration as they are local to each iteration and can be simply     #
    !# re-assigned.                                                             #
    !#--------------------------------------------------------------------------#

    k_Layer_loop: DO k = 1, SIZE( p )


      ! -- Precalculate the squared terms
      p2 = p( k ) * p( k )
      t2 = t( k ) * t( k )
      t4 = t2 * t2

      ! -- Pressure squared adjoint
      p2_AD =          Predictor_AD( 4, k )   + &   ! Predictor #4, P^2
              ( t(k) * Predictor_AD( 7, k ) ) + &   ! Predictor #7, T.P^2
              ( t2   * Predictor_AD( 8, k ) )       ! Predictor #8, T^2.P^2

      ! -- Temperature squared adjoint
      t2_AD =          Predictor_AD( 3, k )         + &  ! Predictor #3, T^2
              ( p(k) * Predictor_AD( 6, k ) )       + &  ! Predictor #6, T^2.P
              ( p2   * Predictor_AD( 8, k ) )       + &  ! Predictor #8, T^2.P^2
              (-w(k) * Predictor_AD( 11, k ) / t4 )      ! Predictor #11, W/T^2

      ! -- Water vapor adjoint
      w_AD( k ) = w_AD( k ) +   Predictor_AD( 10, k ) + &     ! Predictor #10, W
                              ( Predictor_AD( 11, k ) / t2 )  ! Predictor #11, W/T^2

      ! -- Temperature adjoint
      t_AD( k ) = t_AD( k ) + &
                  ( TWO * t(k) * t2_AD )  + &          ! T^2 term
                           Predictor_AD( 1, k )   + &  ! Predictor #1, T
                  ( p(k) * Predictor_AD( 5, k ) ) + &  ! Predictor #5, T.P
                  ( p2   * Predictor_AD( 7, k ) )      ! Predictor #7, T.P^2

      ! -- Pressure adjoint
      p_AD( k ) = p_AD( k ) + &
                  ( TWO * p(k) * p2_AD ) + &                                 ! P^2 term
                           Predictor_AD( 2, k )   + &                        ! Predictor #2, P
                  ( t(k) * Predictor_AD( 5, k ) ) + &                        ! Predictor #5, T.P
                  ( t2   * Predictor_AD( 6, k ) ) + &                        ! Predictor #6, T^2.P
                  ( POINT_25 * (p(k)**(-POINT_75)) * Predictor_AD( 9, k ) )  ! Predictor #9, P^1/4

    END DO k_Layer_loop

  END SUBROUTINE Compute_Std_Predictors_AD








  SUBROUTINE Compute_Int_Predictors_AD( &
                                        ! -- Forward input
                                        Pressure,       &  ! Input,  K
                                        Temperature,    &  ! Input,  K
                                        Absorber,       &  ! Input,  0:K

                                        ! -- Adjoint input
                                        Predictor_AD,   &  ! Input,  Iint x K

                                        ! -- Adjoint output
                                        Pressure_AD,    &  ! In/Output,  K
                                        Temperature_AD, &  ! In/Output,  K
                                        Absorber_AD     )  ! In/Output,  0:K

 


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: Pressure        ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )     :: Temperature     ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN )     :: Absorber        ! 0:K

    ! -- Adjoint input
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )     :: Predictor_AD    ! Iint x K

    ! -- Adjoint output
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: Pressure_AD     ! K
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN OUT ) :: Temperature_AD  ! K
    REAL( fp_kind ), DIMENSION( 0: ),   INTENT( IN OUT ) :: Absorber_AD     ! 0:K



    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Int_Predictors_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Square of the Absorber amount. 0:K
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2
    REAL( fp_kind ), DIMENSION( 0:SIZE( Pressure ) ) :: Absorber_2_AD

    ! -- Multiplicative Factors, K
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: d_Absorber
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: Factor_1
    REAL( fp_kind ), DIMENSION( SIZE( Pressure ) ) :: Factor_2

    ! -- Intermediate summation arrays, Iint x 0:K and Iint
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), 0:SIZE( Pressure ) ) :: s
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ) ) :: s_AD

    ! -- LEVEL Predictor, Iint x 0:K
    REAL( fp_kind ), DIMENSION( SIZE( Predictor_AD, DIM=1 ), 0:SIZE( Pressure ) ) :: x_AD

    ! -- Scalars
    INTEGER :: i, n_Predictors
    INTEGER :: k, n_Layers

    REAL( fp_kind ) :: d_Absorber_AD
    REAL( fp_kind ) :: Factor_1_AD
    REAL( fp_kind ) :: Factor_2_AD

    REAL( fp_kind ) :: Inverse_1
    REAL( fp_kind ) :: Inverse_2
    REAL( fp_kind ) :: Inverse_3
    REAL( fp_kind ) :: Inverse_4
    REAL( fp_kind ) :: Absorber_3
    REAL( fp_kind ) :: Absorber_4
    REAL( fp_kind ) :: Inverse_1_AD
    REAL( fp_kind ) :: Inverse_2_AD
    REAL( fp_kind ) :: Inverse_3_AD
    REAL( fp_kind ) :: Multiplier
    REAL( fp_kind ) :: Add_Factor



    !#--------------------------------------------------------------------------#
    !#                       -- ASSIGN THE DIMENSIONS --                        #
    !#--------------------------------------------------------------------------#

    n_Predictors = SIZE( Predictor_AD, DIM=1 )
    n_Layers     = SIZE( Pressure )



    !#--------------------------------------------------------------------------#
    !#          -- RECALCULATE THE INTERMEDIATE FORWARD MODEL SUMS --           #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Initialise top level of arrays
    ! ------------------------------

    Absorber_2( 0 ) = Absorber(0) * Absorber(0)
    s( :, 0 ) = ZERO


    ! ----------------
    ! Loop over Layers
    ! ----------------

    k_Layer_loop_forward: DO k = 1, n_Layers


      ! -----------------------------------------
      ! Calculate Absorber multiplicative Factors
      ! and save for adjoint calculation.
      ! -----------------------------------------

      Absorber_2( k ) = Absorber( k ) * Absorber( k )

      d_Absorber( k ) = Absorber( k ) - Absorber( k-1 )                           ! For * terms
      Factor_1( k )   = ( Absorber( k )   + Absorber( k-1 )   ) * d_Absorber( k ) ! For ** terms
      Factor_2( k )   = ( Absorber_2( k ) + Absorber_2( k-1 ) ) * d_Absorber( k ) ! For *** terms


      ! ----------------------------------------
      ! Calculate and save the intermediate sums
      ! ----------------------------------------

      s( 1, k ) = s( 1, k-1 ) + ( Temperature( k ) * d_Absorber( k ) )  ! T*
      s( 2, k ) = s( 2, k-1 ) + ( Pressure( k )    * d_Absorber( k ) )  ! P*

      s( 3, k ) = s( 3, k-1 ) + ( Temperature( k ) * Factor_1( k ) )    ! T**
      s( 4, k ) = s( 4, k-1 ) + ( Pressure( k )    * Factor_1( k ) )    ! P**

      s( 5, k ) = s( 5, k-1 ) + ( Temperature( k ) * Factor_2( k ) )    ! T***
      s( 6, k ) = s( 6, k-1 ) + ( Pressure( k )    * Factor_2( k ) )    ! P***

    END DO k_Layer_loop_forward



    !#--------------------------------------------------------------------------#
    !#                -- INITIALISE LOCAL ADJOINT VARIABLES --                  #
    !#--------------------------------------------------------------------------#

    x_AD( :, n_Layers )       = ZERO
    s_AD( : )                 = ZERO
    Absorber_2_AD( n_Layers ) = ZERO



    !#--------------------------------------------------------------------------#
    !#            -- CALCULATE THE INTEGRATED PREDICTOR ADJOINTS --             #
    !#--------------------------------------------------------------------------#


    ! -----------------------------------
    ! Here loop order does matter as this
    ! is a sequential loop
    ! -----------------------------------

    k_Layer_loop_adjoint: DO k = n_Layers, 1, -1



      ! -------------------------------------------------------
      ! Calculate the normalising factors for the integrated
      ! predictors. Note that the checks below, the IF tests to
      ! determine if the absorber products are represenatble
      ! are to minimise the number of calcs. I.e if Inverse_1
      ! is toast because Absorber(k) is too small there's no
      ! need to check any further.
      ! -------------------------------------------------------

inverse_1=one/absorber(k)
inverse_2=inverse_1*inverse_1
inverse_3=inverse_2*inverse_1
inverse_4=inverse_3*inverse_1
goto 100
      ! -- Is Inverse_1 representable?
      Inverse_1_check: IF ( Absorber( k ) > TOLERANCE ) THEN

        Inverse_1 = ONE / Absorber( k )

        ! -- Is Inverse_2 representable
        Inverse_2_check: IF ( Absorber_2( k ) > TOLERANCE ) THEN

          Inverse_2  = Inverse_1 * Inverse_1
          Absorber_3 = Absorber( k ) * Absorber_2( k )
         
          ! -- Is Inverse_3 representable
          Inverse_3_check: IF ( Absorber_3 > TOLERANCE ) THEN

            Inverse_3  = Inverse_2 * Inverse_1
            Absorber_4 = Absorber( k ) * Absorber_3

            ! -- Is Inverse_4 represenatble?
            Inverse_4_check: IF ( Absorber_4 > TOLERANCE ) THEN

              Inverse_4 = Inverse_3 * Inverse_1

            ELSE ! Inverse_4_check

              Inverse_4 = ZERO

            END IF Inverse_4_check

          ELSE ! Inverse_3_check

            Inverse_3 = ZERO
            Inverse_4 = ZERO

          END IF Inverse_3_check

        ELSE ! Inverse_2_check

          Inverse_2 = ZERO
          Inverse_3 = ZERO
          Inverse_4 = ZERO

        END IF Inverse_2_check

      ELSE ! Inverse_1_check

        Inverse_1 = ZERO
        Inverse_2 = ZERO
        Inverse_3 = ZERO
        Inverse_4 = ZERO

      END IF Inverse_1_check
100 continue


      ! --------------------------------------------
      ! Adjoint of Predictor summation across Layers
      ! --------------------------------------------


      DO i = 1, n_Predictors

        x_AD( i, k )   = x_AD( i, k )   + Predictor_AD( i, k )
        x_AD( i, k-1 ) = Predictor_AD( i, k )

      END DO


      ! --------------------------------------------------------------
      ! Adjoint of the LEVEL integrated predictors intermediate sums
      !
      ! Note that the adjoint variables Inverse_X_AD are local to this
      ! loop iteration so they are simply assigned when they are first
      ! used.
      ! --------------------------------------------------------------

      ! -- P* and T*, Predictor indices #2 and 1
      ! -- Simply assign a value for Inverse_1_AD
      Multiplier   = POINT_5 * Inverse_1
      s_AD( 1 )    = s_AD( 1 ) + ( Multiplier * x_AD( 1, k ) )
      s_AD( 2 )    = s_AD( 2 ) + ( Multiplier * x_AD( 2, k ) )
      Inverse_1_AD = POINT_5 * ( ( s( 1, k ) * x_AD( 1, k ) ) + &
                                 ( s( 2, k ) * x_AD( 2, k ) ) )

      ! -- P** and T**, Predictor indices #4 and 3
      Multiplier   = POINT_5 * Inverse_2
      s_AD( 3 )    = s_AD( 3 ) + ( Multiplier * x_AD( 3, k ) )
      s_AD( 4 )    = s_AD( 4 ) + ( Multiplier * x_AD( 4, k ) )
      Inverse_2_AD = POINT_5 * ( ( s( 3, k ) * x_AD( 3, k ) ) + &
                                 ( s( 4, k ) * x_AD( 4, k ) ) )

      ! -- P*** and T***, Predictor indices #6 and 5
      Multiplier   = POINT_75 * Inverse_3
      s_AD( 5 )    = s_AD( 5 ) + ( Multiplier * x_AD( 5, k ) )
      s_AD( 6 )    = s_AD( 6 ) + ( Multiplier * x_AD( 6, k ) )
      Inverse_3_AD = POINT_75 * ( ( s( 5, k ) * x_AD( 5, k ) ) + &
                                  ( s( 6, k ) * x_AD( 6, k ) ) )

      ! -- Adjoint of Inverse terms. Note that the Inverse_X_AD
      ! -- terms are *not* zeroed out as they are re-assigned values
      ! -- each loop iteration above.
      Absorber_AD( k ) = Absorber_AD( k ) - (         Inverse_2 * Inverse_1_AD ) - &
                                            ( TWO *   Inverse_3 * Inverse_2_AD ) - &
                                            ( THREE * Inverse_4 * Inverse_3_AD )


      ! ---------------------------------
      ! Pressure and temperature adjoints
      ! ---------------------------------

      ! -- Pressure
      Pressure_AD( k ) = Pressure_AD( k ) + ( d_Absorber( k ) * s_AD( 2 ) ) + &  ! P*
                                            ( Factor_1( k )   * s_AD( 4 ) ) + &  ! P**
                                            ( Factor_2( k )   * s_AD( 6 ) )      ! P***


      ! -- Temperature
      Temperature_AD( k ) = Temperature_AD( k ) + ( d_Absorber( k ) * s_AD( 1 ) ) + &  ! T*
                                                  ( Factor_1( k )   * s_AD( 3 ) ) + &  ! T**
                                                  ( Factor_2( k )   * s_AD( 5 ) )      ! T***


      ! --------------------------------------------------
      ! Adjoint of the absorber amount
      !
      ! Note that the adjoint variables Factor_X_AD and
      ! d_Absorber_AD are local to this loop iteration
      ! so they are simply assigned when they are first
      ! used.
      !
      ! Note there are no
      !   s_AD() = 0
      ! because all the tangent-linear forms are
      !   s_TL() = s_TL() + (...)
      ! summing from the previous Layer.
      !
      ! Note that the Factor_X_AD and d_Absorber_AD
      ! terms are *not* zeroed out as they are re-assigned
      ! values each loop iteration.
      ! --------------------------------------------------

      ! -- Multiplicative factors
      Factor_1_AD = ( Temperature( k ) * s_AD( 3 ) ) + &
                    ( Pressure( k )    * s_AD( 4 ) )

      Factor_2_AD = ( Temperature( k ) * s_AD( 5 ) ) + &
                    ( Pressure( k )    * s_AD( 6 ) )

      ! -- Adjoint of Absorber_2(). Note that Absorber_2_AD() is a LOCAL adjoint
      ! -- variable, so the initialisation of Absorber_2_AD( k-1 ) here for
      ! -- each "k-1" is o.k. rather than
      ! --   Absorber_2_AD( k-1 ) = Absorber_2_AD( k-1 ) + ( d_Absorber( k ) * Factor_2_AD )
      ! --   Absorber_2_AD(  k  ) = Absorber_2_AD(  k  ) + ( d_Absorber( k ) * Factor_2_AD )
      ! -- since only Absorber_2_AD( n_Layers ) is initialised outside the
      ! -- current layer loop.
      Absorber_2_AD( k-1 ) = d_Absorber( k ) * Factor_2_AD
      Absorber_2_AD(  k  ) = Absorber_2_AD(  k  ) + Absorber_2_AD( k-1 )

      ! -- Adjoint of Absorber(). Here, since Absorber_AD() is NOT a local adjoint
      ! -- variable, we can't use the same form as for Absorber_2_AD() above.
      d_Absorber_AD        = ( Temperature( k ) * s_AD( 1 ) ) + &
                             ( Pressure( k )    * s_AD( 2 ) ) + &
                             ( ( Absorber( k )   + Absorber( k-1 )   ) * Factor_1_AD ) + &
                             ( ( Absorber_2( k ) + Absorber_2( k-1 ) ) * Factor_2_AD )

      Add_Factor = d_Absorber( k ) * Factor_1_AD
      Absorber_AD( k-1 ) = Absorber_AD( k-1 ) + Add_Factor - d_Absorber_AD
      Absorber_AD(  k  ) = Absorber_AD(  k  ) + Add_Factor + d_Absorber_AD + &
                                                ( TWO * Absorber(k) * Absorber_2_AD(k) )
      Absorber_2_AD( k ) = ZERO

    END DO k_Layer_loop_adjoint

    ! -- Adjoint of level 0 Absorber
    Absorber_AD( 0 )   = Absorber_AD( 0 ) + ( TWO * Absorber(0) * Absorber_2_AD(0) )
    Absorber_2_AD( 0 ) = ZERO

  END SUBROUTINE Compute_Int_Predictors_AD

END MODULE predictors


