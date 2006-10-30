!------------------------------------------------------------------------------
!M+
! NAME:
!       Transmittance
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) transmittance routines.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Transmittance
!
! MODULES:
!       Type_Kinds:                 Module containing data type kind definitions.
!
!       Parameters:                 Module containing parameter definitions for
!                                   the pCRTM.
!                                   USEs: TYPE_KINDS module
!
!       Transmitance_Coefficients:  Module containing the pCRTM gas absorption
!                                   model coefficients and their load/destruction
!                                   routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         PARAMETERS module
!                                         TAUCOEFF_DEFINE module
!                                         TAUCOEFF_BINARY_IO module
!
! CONTAINS:
!       Compute_Transmittance:      Subroutine to calculate the layer
!                                   transmittances for an input atmospheric
!                                   profile.
!
!       Compute_Transmittance_TL:   Subroutine to calculate the layer
!                                   tangent-linear transmittances for an
!                                   input atmospheric profile.
!
!       Compute_Transmittance_AD:   Subroutine to calculate the layer
!                                   transmittance adjoints for an input
!                                   atmospheric profile.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I Predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2000
!                       paul.vandelst@ssec.wisc.edu
!
!       Adapted from code written by: Thomas J.Kleespies
!                                     NOAA/NESDIS/ORA
!                                     Thomas.J.Kleespies@noaa.gov
!
!                                     and
!
!                                     John Derber
!                                     NOAA/NCEP/EMC
!                                     John.Derber@noaa.gov
!
!       New algorithm produced by:    Yoshihiko Tahara
!                                     JMA / UCAR / NOAA/NCEP/EMC
!
!
!  Copyright (C) 2000, 2003 Thomas Kleespies, John Derber,
!                           Yoshihiko Tahara, Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE Transmittance

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds, ONLY : fp_kind
  USE Parameters
  USE Transmittance_Coefficients


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Compute_Transmittance
  PUBLIC :: Compute_Transmittance_TL
  PUBLIC :: Compute_Transmittance_AD


CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Transmittance
!
! PURPOSE:
!       Subroutine to calculate the layer transmittances given an
!       input atmospheric profile.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compute_Transmittance( Absorber,      &   ! Input, 0:K x J
!                                   Predictor,     &   ! Input, I x K
!                                   Channel_Index, &   ! Input, scalar
!                                   Direction,     &   ! Input, scalar
!                                   Tau            )   ! Output, K
!
! INPUT ARGUMENTS:
!       Absorber:         Profile LEVEL integrated Absorber amount array.
!                         UNITS:      Varies with Absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (0:K x J)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor:        Profile LAYER Predictors array.
!                         UNITS:      Varies with Predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (I x K)
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:    Channel index id. This is a unique index associated
!                         with a (supported) sensor channel.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Direction:        Direction identifier.
!                         If = 0, calculate layer->surface transmittances (i.e. down)
!                            = 1, calculate layer->space   transmittances (i.e. up)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!        None.
!
! OUTPUT ARGUMENTS:
!        Tau:             Layer to boundary transmittances for the input atmosphere
!                         and channel.
!                         UNITS:      N/A
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K)
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Transmittance( Absorber,      &   ! Input, 0:K x J
                                    Predictor,     &   ! Input, I x K
                                    Channel_Index, &   ! Input, scalar
                                    Direction,     &   ! Input, scalar, 0==down, 1==up
                                    Tau            )   ! Output, K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )  :: Absorber         ! Input, 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )  :: Predictor        ! Input, I x K
    INTEGER,                             INTENT( IN )  :: Channel_Index    ! Input, scalar
    INTEGER,                             INTENT( IN )  :: Direction        ! Input, scalar, 0==down, 1==up

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: Tau              ! Output, K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Transmittance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l                         ! Channel index
    INTEGER :: k, k1, k2, dk, n_Layers   ! Layer index
    INTEGER :: j, n_Absorbers            ! Absorber index
    INTEGER :: i, ip                     ! Predictor index
    INTEGER :: n                         ! Polynomial index

    REAL( fp_kind ) :: ave_Absorber
    REAL( fp_kind ) :: d_Absorber
    REAL( fp_kind ) :: Absorber_Level
    REAL( fp_kind ) :: LN_Chi
    REAL( fp_kind ) :: Absorption_Coefficient
    REAL( fp_kind ) :: Total_OD

    REAL( fp_kind ), DIMENSION( SIZE( Tau ) ) :: Optical_Depth

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ! -- Number of atmospheric layers. The "-1"
    ! -- for the layer assign is because Absorber
    ! -- is based on LEVELS.
    n_Layers    = SIZE( Absorber, DIM = 1 ) - 1

    ! -- Number of atmospheric absorbers
    n_Absorbers = SIZE( Absorber, DIM = 2 )



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! ---------------------------
    ! Initilise the optical depth
    ! ---------------------------

    Optical_Depth( : ) = ZERO


    ! -----------------------------------------------------
    ! Loop over each Absorber for optical depth calculation
    ! -----------------------------------------------------

    j_Absorber_loop: DO j = 1, n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_Index( 0, j, l ) <= 0 ) CYCLE j_Absorber_loop



      !#------------------------------------------------------------------------#
      !#                    -- BEGIN LOOP OVER LAYERS --                        #
      !#------------------------------------------------------------------------#

      k_Layer_OD_loop: DO k = 1, n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! absorber amount and difference
        ! -----------------------------------

        ave_Absorber = POINT_5 * ( Absorber( k, j ) + Absorber( k-1, j ) )
        d_Absorber   = Absorber( k, j ) - Absorber( k-1, j )


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

        Absorber_Level = LOG( ( ave_Absorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                ---------------------------------------------------------
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
        ! 3) The computation of the b(i) below uses Horner's method,
        !    that's why the polynomial calculation *begins* at the
        !    maximum polynomial order, TC%Order_Index( i, j, l ).
        !
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum.
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- for the current predictor, i, for absorber j and channel l.
          ! -- This value is used to access the coefficient array, TC%C.
          b(i) = TC%C( TC%Order_Index( i, j, l ), i, j, l )

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b(i) = ( b(i) * Absorber_Level ) + TC%C( n, i, j, l )
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
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi = b(0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi = LN_Chi + ( b(i) * Predictor( ip, k ) )

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

        Optical_Depth( k ) = Optical_Depth( k ) + &
                             ( Absorption_Coefficient * d_Absorber )

      END DO k_Layer_OD_loop

    END DO j_Absorber_loop



    !#--------------------------------------------------------------------------#
    !#           -- CALCULATE THE LAYER->BOUNDARY TRANSMITTANCES --             #
    !#                                                                          #
    !# This step involves another loop over layers. One *could* reverse the     #
    !# order of the j absorber loop and k layer od loop and calculate the Tau   #
    !# values outside the absorber loop. However, this would involve an IF test #
    !# for every layer even if PREDICTOR_INDEX( 0, l ) == 0. I figured it is    #
    !# better to skip the layer loop altogether if there is no absorption for   #
    !# a particular absorber, despite the need for an extra layer loop here to  #
    !# calculate the transmittance.                                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Determine loop indices
    ! ----------------------

    IF ( Direction == UP ) THEN
      ! -- Transmittance going up, e.g. UPWELLING. Layer 1 => TOA
      k1 = 1
      k2 = n_Layers
      dk = 1
    ELSE
      ! -- Transmittance going down, e.g. DOWNWELLING FLUX, SOLAR. Layer n_Layers => SFC
      k1 =  n_Layers
      k2 =  1
      dk = -1
    END IF


    ! ----------------------------------
    ! Initialise the total optical depth
    ! ----------------------------------

    Total_OD = ZERO


    ! ----------------------------------------------
    ! Loop over layers for transmittance calculation
    ! ----------------------------------------------

    k_Layer_Tau_loop: DO k = k1, k2, dk

      ! -- Update total optical depth
      Total_OD = Total_OD + Optical_Depth( k )

      ! -- If optical depth is < than tolerance, calculate transmittance
      IF ( Total_OD < LIMIT_EXP ) THEN

        Tau( k ) = EXP( -Total_OD )

      ELSE

        Tau( k:k2:dk ) = ZERO
        EXIT k_Layer_Tau_loop

        ! -- The following is an alternative if the above is
        ! -- slow. However, on some compilers, the following is
        ! -- even slower. It depends on how well the compiler
        ! -- implements array access using triplets with a
        ! -- negative stride, i.e. if dk is -1.
        !Tau( k ) = ZERO

      END IF

    END DO k_Layer_Tau_loop

  END SUBROUTINE Compute_Transmittance




!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Transmittance_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear layer transmittances
!       of an input atmospheric profile.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!        CALL Compute_Transmittance_TL( Absorber,      &   ! Input, 0:K x J
!                                       Predictor,     &   ! Input, I x K
!                                       Tau,           &   ! Input, K
!                                       Absorber_TL,   &   ! Input, 0:K x J
!                                       Predictor_TL,  &   ! Input, I x K
!                                       Channel_Index, &   ! Input, scalar
!                                       Direction,     &   ! Input, scalar, 0==down, 1==up
!                                       Tau_TL         )   ! Output, K
!
! INPUT ARGUMENTS:
!       Absorber:         Profile LEVEL integrated Absorber amount array.
!                         UNITS:      Varies with Absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (0:K x J)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor:        Profile LAYER Predictors array.
!                         UNITS:      Varies with Predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (I x K)
!                         ATTRIBUTES: INTENT( IN )
!
!       Absorber_TL:      Profile LEVEL tangent-linear integrated Absorber
!                         amount array.
!                         UNITS:      Varies with Absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (0:K x J)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor_TL:     Profile LAYER tangent-linear Predictors array.
!                         UNITS:      Varies with Predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (I x K)
!                         ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:    Channel index id. This is a unique index associated
!                         with a (supported) sensor channel.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Direction:        Direction identifier.
!                         If = 0, calculate layer->surface Transmittances (i.e. down)
!                            = 1, calculate layer->space   Transmittances (i.e. up)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!        None.
!
! OUTPUT ARGUMENTS:
!        Tau_TL:          Layer to boundary tangent-linear Transmittances for the
!                         input atmosphere and channel.
!                         UNITS:      N/A
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K)
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Transmittance_TL( Absorber,      &   ! Input, 0:K x J
                                       Predictor,     &   ! Input, I x K
                                       Tau,           &   ! Input, K
                                       Absorber_TL,   &   ! Input, 0:K x J
                                       Predictor_TL,  &   ! Input, I x K
                                       Channel_Index, &   ! Input, scalar
                                       Direction,     &   ! Input, scalar, 0==down, 1==up
                                       Tau_TL         )   ! Output, K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )  :: Absorber         ! 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )  :: Predictor        ! I x K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )  :: Tau              ! K
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )  :: Absorber_TL      ! 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )  :: Predictor_TL     ! I x K
    INTEGER,                             INTENT( IN )  :: Channel_Index    ! scalar
    INTEGER,                             INTENT( IN )  :: Direction        ! scalar, 0==down, 1==up

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ),     INTENT( OUT ) :: Tau_TL           ! K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Transmittance_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l                         ! Channel index
    INTEGER :: k, k1, k2, dk, n_Layers   ! Layer index
    INTEGER :: j, n_Absorbers            ! Absorber index
    INTEGER :: i, ip                     ! Predictor index
    INTEGER :: n                         ! Polynomial index

    REAL( fp_kind ) :: ave_Absorber,           ave_Absorber_TL
    REAL( fp_kind ) :: d_Absorber,             d_Absorber_TL
    REAL( fp_kind ) :: Absorber_Level,         Absorber_Level_TL
    REAL( fp_kind ) :: LN_Chi,                 LN_Chi_TL
    REAL( fp_kind ) :: Absorption_Coefficient, Absorption_Coefficient_TL
    REAL( fp_kind ) :: Total_OD_TL

    REAL( fp_kind ), DIMENSION( SIZE( Tau ) ) :: Optical_Depth_TL

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED ) :: b, b_TL



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ! -- Number of atmospheric layers. The "-1"
    ! -- for the layer assign is because Absorber
    ! -- is based on LEVELS.
    n_Layers    = SIZE( Absorber, DIM = 1 ) - 1

    ! -- Number of atmospheric absorbers
    n_Absorbers = SIZE( Absorber, DIM = 2 )



    !#--------------------------------------------------------------------------#
    !#                -- CALCULATE THE LAYER OPTICAL DEPTHS --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! ------------------------------------------
    ! Initilise the tangent-linear optical depth
    ! ------------------------------------------

    Optical_Depth_TL( : ) = ZERO


    ! -----------------------------------------------------
    ! Loop over each Absorber for optical depth calculation
    ! -----------------------------------------------------

    j_Absorber_loop: DO j = 1, n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! Absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the Absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_Index( 0, j, l ) == 0 ) CYCLE j_Absorber_loop



      !#------------------------------------------------------------------------#
      !#                    -- BEGIN LOOP OVER LAYERS --                        #
      !#------------------------------------------------------------------------#

      k_Layer_OD_loop: DO k = 1, n_Layers


        ! -----------------------------------
        ! Calculate the current layer average
        ! Absorber amounts and differences
        ! -----------------------------------

        ave_Absorber    = POINT_5 * ( Absorber(    k, j ) + Absorber(    k-1, j ) )
        ave_Absorber_TL = POINT_5 * ( Absorber_TL( k, j ) + Absorber_TL( k-1, j ) )

        d_Absorber    = Absorber(    k, j ) - Absorber(    k-1, j )
        d_Absorber_TL = Absorber_TL( k, j ) - Absorber_TL( k-1, j )


        ! ----------------------------------------------------------
        ! Calculate absorber space level
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
        ! ----------------------------------------------------------

        Absorber_Level = LOG( ( ave_Absorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                ---------------------------------------------------------
                                                TC%Alpha(j)

        Absorber_Level_TL =                 ave_Absorber_TL / &
        !                   ---------------------------------------------------
                            ( TC%Alpha(j) * ( ave_Absorber - TC%Alpha_C2(j) ) )



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
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- and their tangent-linear form for the current predictor,
          ! -- i, for absorber j and channel l. This value is used to
          ! -- access the coefficient array, TC%C.
          b(i)    = TC%C( TC%Order_Index( i, j, l ), i, j, l )
          b_TL(i) = ZERO

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          ! --
          ! -- NOTE: The tangent-linear term is calculated FIRST
          ! --       See explanation note 1) above.
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b_TL(i) = ( b(i) * Absorber_Level_TL ) + ( b_TL(i) * Absorber_Level )
            b(i)    = ( b(i) * Absorber_Level ) + TC%C( n, i, j, l )
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
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dPred(i)) + (db(i).Pred(i)) 
        !              /__
        !                 i=1
        !
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi    = b(0)
        LN_Chi_TL = b_TL(0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi    = LN_Chi    + ( b(i)    * Predictor(    ip, k ) )
          LN_Chi_TL = LN_Chi_TL + ( b(i)    * Predictor_TL( ip, k ) ) + &
                                  ( b_TL(i) * Predictor(    ip, k ) )

        END DO 


        ! ---------------------------------
        ! Check the value of the absorption
        ! coefficient logarithms
        ! ---------------------------------

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



        ! --------------------------------------
        ! Calculate tangent-linear optical depth
        ! --------------------------------------

        Optical_Depth_TL( k ) = Optical_Depth_TL( k ) + &
                                ( Absorption_Coefficient_TL * d_Absorber    ) + &
                                ( Absorption_Coefficient    * d_Absorber_TL )

      END DO k_layer_od_loop

    END DO j_Absorber_loop



    !#--------------------------------------------------------------------------#
    !#    -- CALCULATE THE LAYER->BOUNDARY TANGENT-LINEAR TRANSMITTANCES --     #
    !#                                                                          #
    !# This step involves another loop over layers. One *could* reverse the     #
    !# order of the j absorber loop and k layer od loop and calculate the Tau   #
    !# values outside the absorber loop. However, this would involve an IF test #
    !# for every layer even if PREDICTOR_INDEX( 0, l ) == 0. I figured it is    #
    !# better to skip the layer loop altogether if there is no absorption for   #
    !# a particular absorber, despite the need for an extra layer loop here to  #
    !# calculate the transmittance.                                             #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Determine loop indices
    ! ----------------------

    IF ( Direction == UP ) THEN
      ! -- Transmittance going up, e.g. UPWELLING. Layer 1 => TOA
      k1 = 1
      k2 = n_Layers
      dk = 1
    ELSE
      ! -- Transmittance going down, e.g. DOWNWELLING FLUX, SOLAR. Layer n_Layers => SFC
      k1 =  n_Layers
      k2 =  1
      dk = -1
    END IF


    ! ----------------------------------------------
    ! Loop over layers for Transmittance calculation
    ! ----------------------------------------------

    Total_OD_TL = ZERO

    k_Layer_Tau_loop: DO k = k1, k2, dk

      Total_OD_TL = Total_OD_TL + Optical_Depth_TL( k )
      Tau_TL( k ) = -Tau( k ) * Total_OD_TL

    END DO k_Layer_Tau_loop

  END SUBROUTINE Compute_Transmittance_TL





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Transmittance_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the layer transmittances
!       of an input atmospheric profile.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compute_Transmittance_AD( Absorber,      &   ! Input, 0:K x J
!                                      Predictor,     &   ! Input, I x K
!                                      Tau,           &   ! Input, K
!                                      Tau_AD,        &   ! Input, K
!                                      Channel_Index, &   ! Input, scalar
!                                      Direction,     &   ! Input, scalar, 0==down, 1==up
!                                      Absorber_AD,   &   ! Output, 0:K x J
!                                      Predictor_AD   )   ! Output, I x K
!
! INPUT ARGUMENTS:
!       Absorber:         Profile LEVEL integrated Absorber amount array.
!                         UNITS:      Varies with Absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (0:K x J)
!                         ATTRIBUTES: INTENT( IN )
!
!       Predictor:        Profile LAYER Predictors array.
!                         UNITS:      Varies with Predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (I x K)
!                         ATTRIBUTES: INTENT( IN )
!
!       Tau:              Layer to boundary Transmittances for the input
!                         atmosphere and channel.
!                         UNITS:      N/A
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K)
!                         ATTRIBUTES: INTENT( OUT )
!
!       Tau_AD:           Adjoint of the layer to boundary Transmittances
!                         for the input atmosphere and channel.
!                         **THIS ARGUMENT IS SET TO ZERO ON OUTPUT.**
!                         UNITS:      N/A
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-1 (K)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Channel_Index:    Channel index id. This is a unique index associated
!                         with a (supported) sensor channel.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Direction:        Direction identifier.
!                         If = 0, calculate layer->surface Transmittances (i.e. down)
!                            = 1, calculate layer->space   Transmittances (i.e. up)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Absorber_AD:      Adjoint of the profile LEVEL integrated Absorber
!                         amount array.
!                         UNITS:      Varies with Absorber.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (0:K x J)
!                         ATTRIBUTES: INTENT( IN OUT )
!
!       Predictor_AD:     Adjoint of the profile LAYER Predictors.
!                         UNITS:      Varies with Predictor type.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Rank-2 (I x K)
!                         ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       The input argument TAU_AD is set to zero upon output.
!
! RESTRICTIONS:
!       None.
!
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Compute_Transmittance_AD( Absorber,      &   ! Input, 0:K x J
                                       Predictor,     &   ! Input, I x K
                                       Tau,           &   ! Input, K
                                       Tau_AD,        &   ! Input, K
                                       Channel_Index, &   ! Input, scalar
                                       Direction,     &   ! Input, scalar, 0==down, 1==up
                                       Absorber_AD,   &   ! Output, 0:K x J
                                       Predictor_AD   )   ! Output, I x K



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN )     :: Absorber       ! 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN )     :: Predictor      ! I x K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN )     :: Tau            ! K
    REAL( fp_kind ), DIMENSION( : ),     INTENT( IN OUT ) :: Tau_AD         ! K
    INTEGER,                             INTENT( IN )     :: Channel_Index  ! scalar
    INTEGER,                             INTENT( IN )     :: Direction      ! scalar, 0==down, 1==up

    ! -- Output
    REAL( fp_kind ), DIMENSION( 0:, : ), INTENT( IN OUT ) :: Absorber_AD    ! 0:K x J
    REAL( fp_kind ), DIMENSION( :, : ),  INTENT( IN OUT ) :: Predictor_AD   ! I x K


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Transmittance_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: l                         ! Channel index
    INTEGER :: k, k1, k2, dk, n_Layers   ! Layer index
    INTEGER :: j, n_Absorbers            ! Absorber index
    INTEGER :: i, ip                     ! Predictor index
    INTEGER :: n                         ! Polynomial index

    REAL( fp_kind ) :: ave_Absorber,           ave_Absorber_AD
    REAL( fp_kind ) :: d_Absorber,             d_Absorber_AD
    REAL( fp_kind ) :: Absorber_Level,         Absorber_Level_AD
    REAL( fp_kind ) :: LN_Chi,                 LN_Chi_AD
    REAL( fp_kind ) :: Absorption_Coefficient, Absorption_Coefficient_AD

    REAL( fp_kind ) :: Total_OD_AD

    REAL( fp_kind ), DIMENSION( SIZE( Tau ) ) :: Optical_Depth_AD

    ! -- Polynomial derived coefficients
    REAL( fp_kind ), DIMENSION( 0:MAX_N_ORDERS, &
                                0:MAX_N_PREDICTORS_USED  ) :: b
    REAL( fp_kind ), DIMENSION( 0:MAX_N_PREDICTORS_USED  ) :: b_AD



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ! -- Number of atmospheric layers. The "-1"
    ! -- for the layer assign is because Absorber
    ! -- is based on LEVELS.
    n_Layers    = SIZE( Absorber, DIM = 1 ) - 1

    ! -- Number of atmospheric absorbers
    n_Absorbers = SIZE( Absorber, DIM = 2 )



    !#--------------------------------------------------------------------------#
    !#                       -- SOME INITIALISATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Assign the channel index to a short name
    ! ----------------------------------------

    l = Channel_Index


    ! -------------------------------------
    ! Initilise the local adjoint variables
    ! -------------------------------------

    Total_OD_AD           = ZERO
    Optical_Depth_AD( : ) = ZERO

    Absorber_Level_AD = ZERO



    !#--------------------------------------------------------------------------#
    !#       -- CALCULATE THE LAYER->BOUNDARY TRANSMITTANCE ADJOINT --          #
    !#                                                                          #
    !# This step involves another loop over layers. One *could* reverse the     #
    !# order of the j absorber loop and k layer od loop and calculate the Tau   #
    !# values outside the absorber loop. However, this would involve an IF test #
    !# for every layer even if PREDICTOR_INDEX( 0, l ) == 0. I figured it is    #
    !# better to skip the layer loop altogether if there is no absorption for   #
    !# a particular absorber, despite the need for an extra layer loop here to  #
    !# calculate the transmittance.                                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Determine loop indices. Same Direction
    ! as FORWARD and TANGENT-LINEAR models
    ! --------------------------------------

    IF ( Direction == UP ) THEN
      ! -- Transmittance going up, e.g. UPWELLING. Layer 1 => TOA
      k1 = 1
      k2 = n_Layers
      dk = 1
    ELSE
      ! -- Transmittance going down, e.g. DOWNWELLING FLUX, SOLAR. Layer n_Layers => SFC
      k1 =  n_Layers
      k2 =  1
      dk = -1
    END IF


    ! -----------------------------------------------
    ! Loop over layers for transmittance calculation.
    ! Note the loop index reversal.
    ! -----------------------------------------------

    k_Layer_Tau_loop: DO k = k2, k1, -dk

      Total_OD_AD = Total_OD_AD - ( Tau( k ) * Tau_AD( k ) )
      Tau_AD( k ) = ZERO

      Optical_Depth_AD( k ) = Optical_Depth_AD( k ) + Total_OD_AD
      ! Note: No total_od_AD = ZERO here because
      !       total_od_TL = total_od_TL + (....)

    END DO k_Layer_Tau_loop



    !#--------------------------------------------------------------------------#
    !#                        -- LOOP OVER ABSORBERS --                         #
    !#--------------------------------------------------------------------------#

    j_Absorber_loop: DO j = 1, n_Absorbers


      ! -----------------------------------------
      ! Check if there is any absorption for this
      ! absorber/channel combination.
      !
      ! This check is the reason why all channels
      ! cannot be processed at once and why the
      ! layer loop is within the Absorber loop.
      ! -----------------------------------------

      IF ( TC%Predictor_index( 0, j, l ) == 0 ) CYCLE j_Absorber_loop



      !#------------------------------------------------------------------------#
      !#                        -- LOOP OVER LAYERS --                          #
      !#------------------------------------------------------------------------#

      k_Layer_OD_loop: DO k = n_Layers, 1, -1


        ! -----------------------------------
        ! Calculate the current layer average
        ! Absorber amounts and differences
        ! -----------------------------------

        ave_Absorber = POINT_5 * ( Absorber( k, j ) + Absorber( k-1, j ) )
        d_Absorber   = Absorber( k, j ) - Absorber( k-1, j )



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

        Absorber_Level = LOG( ( ave_Absorber - TC%Alpha_C2(j) ) / TC%Alpha_C1(j) ) / &
        !                ---------------------------------------------------------
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
        ! 1) The actual computation of the b(i) uses Horner's method,
        !    starting at the maximum polynomial order, N, to
        !    minimise round off error. So for a given predictor index i,
        !    we accumulate the value of b for successive orders of the
        !    N'th degree polynomial:
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
        !      b[n] = b[n-1].k + cn
        !
        !    Note that the b coefficient array is dimensioned as b(n,i) as
        !    we will need the accumulated b(i) values at each stage of the
        !    polynomial summation for computing the adjoint value.

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
        ! ----------------------------------------------------------------

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Initialise the polynomial sum.
          ! -- TC%Order_Index( i, j, l ) contains the polynomial
          ! -- order to be used in reconstructing the b(i) coefficients
          ! -- for the current predictor, i, for absorber j and channel l.
          ! -- This value is used to access the coefficient array, TC%C.
          b( TC%Order_Index( i, j, l ), i) = TC%C( TC%Order_Index( i, j, l ), i, j, l )

          ! -- Loop over the polynomial orders and accumulate the
          ! -- coefficient value. Note that if
          ! --   TC%Order_Index( 0, j, l ) == 0
          ! -- then this loop is not entered. If your compiler has
          ! -- a "zero trip" loop option where DO loops are *always*
          ! -- executed at least once, regardless of the loop indices,
          ! -- make sure it's not on by default!
          DO n = TC%Order_Index( i, j, l ) - 1, 0, -1
            b(n,i) = ( b(n+1,i) * Absorber_Level ) + TC%C( n, i, j, l )
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
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! Note that only the final, accumulated results for the
        ! b coefficients, the b(0,i) are used. The b(1:N,i) are
        ! used in the adjoint form of the calculation that produced
        ! the b coefficient values.
        ! ---------------------------------------------------------

        ! -- Initialise the log of the absorption coefficient
        LN_Chi = b(0,0)

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          LN_Chi = LN_Chi + ( b(0,i) * Predictor( ip, k ) )

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
        ! These quantities are local to the k_Layer_OD_loop
        ! and are equal to zero at this point so a straight
        ! initialisation is used, i.e. there is no
        !   d_adbsorber_AD            = d_Absorber_AD + (...)
        !   absorption_Coefficient_AD = absorption_Coefficient_AD + (...)
        ! This also eliminates the need to zero out the two
        ! quanitities later in the loop once they no longer
        ! have an impact on the gradient vector result.
        !
        ! Also not that there is no
        !   Optical_Depth_AD( k ) = ZERO
        ! because
        !   Optical_Depth_TL( k ) = Optical_Depth_TL( k ) + (....)
        ! ---------------------------------------------------------------

        d_Absorber_AD = Absorption_Coefficient * Optical_Depth_AD( k )   ! .... (1)
        Absorption_Coefficient_AD = d_Absorber * Optical_Depth_AD( k )



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
        !   LN(chi) = b(0) +  > b(i).Pred(i)
        !                    /__
        !                       i=1
        !
        ! The tangent-linear form is
        !
        !               __Iuse
        !              \
        !   dLN(chi) =  >  (b(i).dPred(i)) + (db(i).Pred(i)) 
        !              /__
        !                 i=1
        !
        ! So the adjoint forms are for each predictor index i,
        !               
        !    *           *                *
        !   d Pred(i) = d Pred(i) + b(i).d LN(chi)
        !
        !
        ! and,
        !
        !
        !    *                *
        !   d b(i) = Pred(i).d LN(chi)
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

        ! -- Loop over the number of predictors
        DO i = 1, TC%Predictor_Index( 0, j, l )

          ip = TC%Predictor_Index( i, j, l )
          Predictor_AD(ip,k) = Predictor_AD( ip,k ) + ( b(0,i) * LN_Chi_AD )
          b_AD(i) = Predictor(ip,k) * LN_Chi_AD

        END DO 

        ! -- Initialise the b(0) adjoint and zero the LN(Chi) adjoint
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

        ! -- Loop over predictors
        DO i = 0, TC%Predictor_Index( 0, j, l )

          ! -- Loop over the polynomial orders in the reverse order
          ! -- Note that the order of the Absorber_Level_AD and b_AD
          ! -- calculation are important
          DO n = 0, TC%Order_Index( i, j, l ) - 1
            Absorber_Level_AD = Absorber_Level_AD + ( b(n+1,i) * b_AD(i) )
            b_AD(i) = Absorber_Level * b_AD(i)
          END DO

          ! -- Reset the b coefficient adjoint
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

        ave_Absorber_AD =               Absorber_Level_AD / &
        !                 ---------------------------------------------------  ....(2)
                          ( TC%Alpha(j) * ( ave_Absorber - TC%Alpha_C2(j) ) )

        Absorber_Level_AD = ZERO



        ! ---------------------------------------------------
        ! Adjoints of the current layer average
        ! Absorber amount and difference.
        !
        ! Neither d_Absorber_AD nor ave_Absorber_AD need
        ! to be set to zero after this as they are explicitly
        ! reassigned each layer iteration at (1) and (2) above
        ! respectively.
        ! ---------------------------------------------------

        Absorber_AD( k-1, j ) = Absorber_AD( k-1, j ) - d_Absorber_AD + ( POINT_5 * ave_Absorber_AD )
        Absorber_AD( k,   j ) = Absorber_AD( k,   j ) + d_Absorber_AD + ( POINT_5 * ave_Absorber_AD )

      END DO k_Layer_OD_loop

    END DO j_Absorber_loop

  END SUBROUTINE Compute_Transmittance_AD

END MODULE Transmittance


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: transmittance.f90,v 2.5 2004/12/22 17:43:33 paulv Exp $
!
! $Date: 2004/12/22 17:43:33 $
!
! $Revision: 2.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: transmittance.f90,v $
! Revision 2.5  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.4  2004/10/04 21:26:48  paulv
! - Removed intrinsics declaration.
!
! Revision 2.3  2004/06/10 19:12:25  paulv
! - Using the parameter LIMIT_EXP rather than local OD_Tolerance variable
!   to test for too large optical depths.
!
! Revision 2.2  2004/02/25 22:44:23  paulv
! - Now using the Order_Index() component of the TauCoeff data to loop over
!   the required polynomial orders to reconstruct the transmittance model
!   coefficients.
! - Reordered the absorber/channel indices from l, j to j, l. The former
!   order was out of stride and a throwback to much earlier versions of the
!   code.
!
! Revision 2.1  2003/11/25 21:38:30  paulv
! - Removed all references to Quad precision kind type for variables. Switched
!   to the generic fp_kind.
!
! Revision 2.0  2003/05/02 13:28:12  paulv
! - New transmittance algorithm implemented.
!
! Revision 1.10  2002/07/24 14:56:34  paulv
! - Updated documentation.
!
! Revision 1.9  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.8  2001/08/16 17:19:11  paulv
! - Updated documentation
!
! Revision 1.7  2001/08/01 17:04:00  paulv
! - The absorber space levels are no longer calculated during model
!   initialisation, but are precalculated and stored in the transmittance
!   coefficient data file. This means that,
!     USE absorber_space, ONLY : absorber_space_levels
!   was deleted as the absorber space level array is now available from
!   the TRANSMITTANCE_COEFFICIENTS module.
!
! Revision 1.6  2001/07/12 18:38:28  paulv
! - Use of ABSORBER_SPACE module now includes an ONLY clause so that the
!   absorber_space_levels is all that is available.
! - Direction specification changed from
!     IF ( direction == 0 ) THEN
!       ...do DOWNWELLING STUFF...
!     ELSE
!       ...do UPWELLING STUFF...
!     END IF
!   to
!     IF ( direction == UP ) THEN
!       ...do UPWELLING STUFF...
!     ELSE
!       ...do DOWNWELLING STUFF...
!     END IF
!   since the upwelling case is required for every call, but the downwelling
!   may not be. Also, the parameter UP is now used in the IF rather than
!   an actual number (0 in this case).
! - Changed
!      od_tolerance = ABS( ALOG( TOLERANCE ) )
!   to
!      od_tolerance = ABS( LOG( TOLERANCE ) )
! - Corrected bug in the forward calculation of the absorption coefficient
!   in TRANSMITTANCE_AD. The offset coefficients are defined as
!     b1o = tau_coefficients( 0, k1, l, j )
!     b2o = tau_coefficients( 0, k2, l, j )
!   and the offset term was initialised as
!     absorption_coefficient = b2 + ( gradient * ( b1o - b2o ) )
!   instead of
!     absorption_coefficient = b2o + ( gradient * ( b1o - b2o ) )
!   where in the former, B2 was specified rather than B2O
!
! Revision 1.5  2001/05/29 18:00:08  paulv
! - Added adjoint form of the transmittance calculation.
! - Removed the FIND_ABSORBER_SPACE_LAYER  routine. Now resides in the
!   ABSORBER_PROFILE module. The absorber space bracket layer indices are
!   now passed as arguments from the calling routine.
! - The predictor indices and transmittance coefficients are no longer passed
!   as arguments but read from the transmittance_coefficients module.
!
! Revision 1.4  2000/11/14 18:42:32  paulv
! - Merged branch incorporating tangent-linear code into main truck.
!   Optical depth debug code still present.
!
! Revision 1.3.1.2  2000/11/14 18:34:56  paulv
! - Finished adding tangent-linear code. Optical depth debug code still
!   present - output sent to unit numbers 51 and 61.
!
! Revision 1.3.1.1  2000/11/09 20:49:35  paulv
! - Adding tangent linear forms of the optical depth and transmittance
!   computation. IN PROGRESS AND INCOMPLETE.
! - Removed code that finds the absorber space bracket layers into its own
!   subroutine. Both the forward and tangent linear routines use the same
!   search method.
!
! Revision 1.3  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 16:55:33  paulv
! - Added optional NO_STANDARD input argument to the COMPUTE_TRANSMITTANCE
!   subprogram to prevent the (angle independent) standard predictors from being
!   recalculated when only the path angle has changed in the calling procedure.
! - The  profile data integration has been removed from the COMPUTE_TRANSMITTANCE
!   subprogram and is now performed outside of this module in the ABSORBER_PROFILE
!   module. This has a number of consequences:
!   o The VIEW_ANGLE input argument was removed and replaced with the path-angle
!     scaled ABSORBER_AMOUNTS argument.
!   o The interface pressure and ozone profile data are no longer required
!     and have been removed from the input argument list.
! - All profile integration and predictor calculation code has been removed
!   from the COMPUTE_TRANSMITTANCE subprogram.
! - Updated module and subprogram documentation.
!
! Revision 1.1  2000/08/22 15:57:26  paulv
! Initial checkin.
!
!
!
!
