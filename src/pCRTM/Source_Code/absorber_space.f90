!------------------------------------------------------------------------------
!M+
! NAME:
!       absorber_space
!
! PURPOSE:
!       RT model module to define the absorber space levels
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE absorber_space
!
! MODULES:
!       type_kinds:             Module containing data type kind definitions.
!
!       error_handler:          Module to define error codes and handle error
!                               conditions
!
!       parameters:             Module containing parameter definitions for the
!                               RT model.
!
!
! CONTAINS:
!       compute_absorber_space: PUBLIC function that computes the absorber space
!                               levels and fills the public data array.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None known.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP 11-Jul-2000
!                       pvandelst@ncep.noaa.gov
!
!       Adapted from code written by: Thomas J.Kleespies
!                                     NOAA/NESDIS/ORA
!                                     tkleespies@nesdis.noaa.gov
!
!  Copyright (C) 2000 Thomas Kleespies, Paul van Delst
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

MODULE absorber_space


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE type_kinds
  USE error_handler
  USE parameters


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  PRIVATE
  PUBLIC  :: compute_absorber_space


  ! ---------------------------
  ! Definitions of private data
  ! ---------------------------

  ! -- These are the minimum and maximum absorber amounts
  ! -- at absorber LEVELS 1 and MAX_N_ABSORBER_LAYERS respectively.

  ! -- THE DEFAULT VALUES USED ARE THE 32 ATMOSPHERE SET.

  INTEGER, PRIVATE, PARAMETER :: N_ABSORBER_SETS = 2

  REAL( Double ), PRIVATE, PARAMETER, &
                  DIMENSION( MAX_N_ABSORBERS, N_ABSORBER_SETS ) :: &
    MIN_ABSORBER = RESHAPE( (/ 1.0e-07_Double, &      ! Wet  \
                               0.01_Double,    &      ! Dry   > 32 atmosphere minimums
                               2.0e-05_Double, &      ! Ozo  /

                               1.0e-07_Double, &      ! Wet  \
                               0.005_Double,   &      ! Dry   > UMBC AIRS set minimums
                               2.0e-06_Double  /), &  ! Ozo  /
                            (/ MAX_N_ABSORBERS, N_ABSORBER_SETS /) ), &

    MAX_ABSORBER = RESHAPE( (/ 13.87981_Double,  &     ! Wet \
                               2100.0_Double,    &     ! Dry  > 32 atmosphere maximums
                               1.1403912_Double, &     ! Ozo /

                               21.08_Double,     &     ! Wet  \
                               2200.0_Double,    &     ! Dry   > UMBC AIRS set maximums
                               2.36_Double       /), & ! Ozo  /
                            (/ MAX_N_ABSORBERS, N_ABSORBER_SETS /) )


  ! ----------------------------------
  ! Explicit visibility of subprograms
  ! ----------------------------------



CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       compute_absorber_space
!
! PURPOSE:
!       PUBLIC function to compute the absorber space levels.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       result = compute_absorber_space( alpha,                    &  ! Output
!                                        absorber_space_levels,    &  ! Output
!                                        absorber_set_index,       &  ! Optional input
!                                        message_log = message_log )  ! Optional input
!
! INPUT ARGUMENTS:
!       None.
!
! OPTIONAL INPUT ARGUMENTS:
!       absorber_set_index:    Integer value representing the absorber set limits
!                              to use. Currently there are two sets:
!                                1: 32 atmosphere set
!                                2: UMBC atmosphere set (used for AIRS)
!                              If not specified, the 32 atmosphere set (#1) is used.
!                              UNITS:      None
!                              TYPE:       Integer
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       message_log:           Character string specifying a filename in which any
!                              messages will be logged. If not specified, or if an
!                              error occurs opening the log file, the default action
!                              is to output messages to the screen.
!                              UNITS:      None
!                              TYPE:       Character
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       alpha:                 Vector containing the exponential parameter
!                              for each absorber used to construct the 
!                              absorber space.
!                              UNITS:      None
!                              TYPE:       Double
!                              DIMENSION:  J
!                              ATTRIBUTES: INTENT( OUT )
!
!       absorber_space_levels: Array containing the absorber space levels
!                              used in the transmittance model.
!                              UNITS:      Varies with absorber
!                              TYPE:       Double
!                              DIMENSION:  0:Ka x J
!                              ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None
!
! FUNCTION RESULT:
!       Result = SUCCESS => absorber space calculation successful
!              = FAILURE => No convergence obtained in numberical solution
!                           to determine alpha (see below).
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       If the convergence criterion is not met within a specified number
!       of iterations, an error message is printed and the function returns
!       with a FAILURE code.
!
!       In this case, the absorber_space output array is undefined.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       McMillin, L.M., L.J. Crone, M.D. Goldberg, and T.J. Kleespies,
!         "Atmospheric transmittance of an absorbing gas. 4. OPTRAN: a
!          computationally fast and accurate transmittance model for absorbing
!          with fixed and with variable mixing ratios at variable viewing
!          angles.", Applied Optics, 1995, v34, pp6269-6274.
!
!       Standard absorber amounts at which transmittances are computed need
!       to be determined. The standard amounts need to be dense enough to 
!       adequately represent all atmospheres. To do this a set of Ka+1 amounts,
!       A(0), A(1), A(2), ... , A(Ka), are used where A(0) = 0 and A(Ka)
!       is large enough such that the transmittance through that much absorber
!       is effectively zero (for a highly absorbing channel) or no atmosphere
!       with more absorber can be expected to occur (for a relatively transparent
!       channel).
!
!       The increments, dA(k) = A(k) - A(k-1), are chosen to form an increasing 
!       geometric sequence:
!
!         dA(k+1) = dA(k).EXP(alpha)
!
!       where alpha is a positive number. Then,
!
!                        EXP( k.alpha ) - 1
!         A(k) = A(1) . --------------------  ...........(1)
!                         EXP( alpha ) - 1
!
!       If A(1) and A(Ka) are known, then alpha can be determined by a numerical
!       solution to the equation:
!
!          EXP( k.alpha ) - 1     A(Ka)
!         -------------------- = -------      ...........(2)
!           EXP( alpha ) - 1      A(1)
!       
!       This is done using Newton's method to find the root of eqn (2) recast as:
!
!                                              A(Ka)
!         f(alpha) = ( EXP( k.alpha ) - 1 ) - ------- . ( EXP( alpha ) - 1 ) = 0
!                                              A(1)
!
!       The derivative of this function is obtained:
!
!                                         A(Ka)
!         f'(alpha) = k.EXP( k.alpha ) - ------- . EXP( alpha )
!                                         A(1)
!
!       Successive approximations are made to alpha using,
!
!                                  f(alpha)
!         alpha(i+1) = alpha(i) - -----------
!                                  f'(alpha)
!
!       When
!
!         | alpha(i+1) - alpha(i) | < tolerance
!
!       the value of alpha(i+1) is used in equation (1) to determine the 
!       absorber space.
!
!       If the convergence criterion is not met within a specified number
!       of iterations, an error message is printed and the function returns
!       with a FAILURE code.
!S-
!------------------------------------------------------------------------------

  FUNCTION compute_absorber_space( alpha,                 &  ! Output
                                   absorber_space_levels, &  ! Output
                                   absorber_set_index,    &  ! Optional input
                                   message_log )          &  ! Optional input
                                 RESULT ( error_status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Outputs
    REAL( Double ), DIMENSION(     : ), INTENT( OUT ) :: alpha
    REAL( Double ), DIMENSION( 0:, : ), INTENT( OUT ) :: absorber_space_levels

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN ) :: absorber_set_index
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: message_log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: error_status


    ! ----------------
    ! Local parameters
    ! ----------------

    INTEGER,        PARAMETER :: MAX_N_ITERATIONS      = 500

    REAL( Double ), PARAMETER :: CONVERGENCE_CRITERION = 1.0e-10_Double
    REAL( Double ), PARAMETER :: D_ONE                 = 1.0_Double

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME          = 'COMPUTE_ABSORBER_SPACE'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: n_absorbers
    INTEGER :: n_layers

    INTEGER :: i, j, k
    INTEGER :: iteration

    REAL( Double ) :: ratio    ! Ratio of absorber amount extrema
    REAL( Double ) :: n        ! Total number of layers
    REAL( Double ) :: x1, x2   ! Previous and new approximations
    REAL( Double ) :: fx, fp   ! Function and its derivative
    REAL( Double ) :: kr       ! Level multiplier

    CHARACTER( 80 ) :: message


    ! ----------
    ! Intrinsics
    ! ----------

    INTRINSIC ABS,     &
              EXP,     &
              MAX,     &
              MIN,     &
              PRESENT, &
              REAL,    &
              SIZE,    &
              TRIM



    !#--------------------------------------------------------------------------#
    !#                          -- Check input --                               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( absorber_set_index ) ) THEN
      i = MIN( N_ABSORBER_SETS, MAX( 1, absorber_set_index ) )
    ELSE
      i = 1
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- Check input --                              #
    !#--------------------------------------------------------------------------#

    ! -- The number of input absorbers
    n_absorbers = SIZE( alpha )
    IF ( SIZE( absorber_space_levels, DIM = 2 ) /= n_absorbers .OR. &
         n_absorbers /= MAX_N_ABSORBERS                             ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Absorber dimensions for output arguments inconsistent.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF


    ! -- The number of absorber space layers
    n_layers = SIZE( absorber_space_levels, DIM = 1 ) - 1
    IF ( n_layers /= MAX_N_ABSORBER_LAYERS ) THEN
      error_status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Invalid level dimension for ABSORBER_SPACE_LEVELS output.', &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- Initialisation --                             #
    !#--------------------------------------------------------------------------#

    ! -- Make the number of layers double precision type
    n = REAL( n_layers, Double )


    ! -- Initialise the top-of-atmosphere absorber amounts
    ! -- They are always zero.
    absorber_space_levels( 0, : ) = ZERO



    !#--------------------------------------------------------------------------#
    !#                -- Solve for alpha by Newton's method --                  #
    !#--------------------------------------------------------------------------#

    j_absorber_loop: DO j = 1, n_absorbers


      ! -------------------------------------------
      ! Initialise first level (1 - just below TOA)
      ! amount for this absorber
      ! -------------------------------------------

      absorber_space_levels( 1, j ) = MIN_ABSORBER( j, i )


      ! ---------------------------------------------------
      ! Set the initial approximation value.
      ! Somewhat arbitrary but tailored for wet atmospheres
      ! ---------------------------------------------------

      x2 = 0.3_Double


      ! -----------------------------
      ! Initialise iteration counter
      ! -----------------------------

      iteration = 0


      ! -------------------------------------------
      ! Calculate the absorber amount extrema ratio
      ! -------------------------------------------

      ratio = MAX_ABSORBER( j, i ) / MIN_ABSORBER( j, i )


      ! ---------------------------------------
      ! Begin iteration loop to calculate alpha
      ! ---------------------------------------

      alpha_loop: DO

        ! -- Increment iteration counter
        iteration = iteration + 1

        ! -- No convergence?
        IF ( iteration > MAX_N_ITERATIONS ) THEN
          error_status = FAILURE
          WRITE( message, '( "Absorber #", i1, &
                            &": ALPHA calculation failed to converge." )' ) &
                          j
          CALL display_message( ROUTINE_NAME, &
                                message, &
                                error_status, &
                                message_log = message_log )
          RETURN
        END IF

        ! -- Update the initial approximation
        x1 = x2

        ! -- calculate the function
        fx = ( EXP( n * x1 ) - D_ONE ) - ( ratio * ( EXP( x1 ) - D_ONE ) )

        ! -- Calculate its derivative
        fp = n * EXP( n * x1 ) - ( ratio * EXP( x1 ) )

        ! -- Calculate a new approximation 
        x2 = x1 - ( fx / fp )

        ! -- Have we converged to a solution?
        IF ( ABS( x2 - x1 ) < CONVERGENCE_CRITERION ) EXIT alpha_loop

      END DO alpha_loop


      ! ----------------------
      ! We have an alpha value
      ! ----------------------

      alpha( j ) = x2


      ! ---------------------------------------------------------
      ! Now calculate the rest of the absorber space level values
      ! ---------------------------------------------------------

      k_level_loop: DO k = 2, n_layers

        kr = REAL( k, Double )

        absorber_space_levels( k, j ) = absorber_space_levels( 1, j ) * &
                                          ( EXP( kr * alpha(j) ) - ONE ) / &
                                          ( EXP( alpha(j) )      - ONE )
      END DO k_level_loop

    END DO j_absorber_loop



    !#--------------------------------------------------------------------------#
    !#                     -- Successful completion --                          #
    !#--------------------------------------------------------------------------#

    error_status = SUCCESS

  END FUNCTION compute_absorber_space

END MODULE absorber_space


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: absorber_space.f90,v 1.10 2001/09/28 21:16:33 paulv Exp $
!
! $Date: 2001/09/28 21:16:33 $
!
! $Revision: 1.10 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: absorber_space.f90,v $
! Revision 1.10  2001/09/28 21:16:33  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 1.9  2001/08/31 21:46:32  paulv
! - Altered the default minimum and maximum absorber amount definitions to
!   array values so that more than one set can be specified. Currently the
!   32 atmosphere and the UMBC AIRS dependent atmosphere set min and max
!   absorber amounts are included.
! - Changed the COMPUTE_ABSORBER_SPACE call to include an optional argument
!   allowing the user to select the min/max values by set. The default is to
!   use the 32 atmosphere values.
! - Removed PUBLIC shared data ABSORBER_SPACE_LEVELS and made it an output
!   argument of the COMPUTE_ABSORBER_SPACE function.
! - Added ALPHA as an output argument to COMPUTE_ABSORBER_SPACE.
!
! Revision 1.8  2001/08/01 16:27:05  paulv
! Removal from main NCEP_RTM directory.... for good this time :o)
!
! Revision 1.7  2001/08/01 16:24:42  paulv
! Restore for move to absorber_space_test directory
!
! Revision 1.5  2001/05/29 17:38:26  paulv
! - Now use TYPE_KINDS module parameter FP_KIND to set the default floating
!   point data type (i.e. real variables that are not Double).
! - Changed all assignations to reflect use of explicit types:
!
!   From:   absorber_space_levels( 1, j ) = MIN_ABSORBER( j )
!   To:     absorber_space_levels( 1, j ) = REAL( MIN_ABSORBER( j ), fp_kind )
!
!   From:   x2 = 0.3d0
!   To:     x2 = 0.3_Double
!
!   From:   alpha = REAL( x2, Single )
!   To:     alpha = REAL( x2, fp_kind )
!
!   From:   kr = REAL( k, Single )
!   To:     kr = REAL( k, fp_kind )
!
! Revision 1.4  2000/08/31 19:36:31  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.3  2000/08/24 15:12:13  paulv
! - Updated in-line documentation.
!
! Revision 1.2  2000/08/18 20:35:00  paulv
! - Updated header and function documentation.
! - All parameters default uppercase.
! - Output array name changed from absorber_levels to absorber_space_levels.
!
! Revision 1.1  2000/08/08 16:59:08  paulv
! Initial checkin
!
!
!
