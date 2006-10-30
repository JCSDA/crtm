!------------------------------------------------------------------------------
!M+
! NAME:
!       MWLBL_Transmittance
!
! PURPOSE:
!       Module containing routines for microwave transmittance profile
!       calculations.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE MWLBL_Utility
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       Error_Handler:      Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       MWLBL_Liebe89:      Module containing data and routines to calculate
!                           microwave line-by-line transmittances according 
!                           to Liebe 89.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 LIEBE89_COEFFICIENTS module
!
!       MWLBL_Liebe93:      Module containing data and routines to calculate
!                           microwave line-by-line transmittances according 
!                           to Liebe 93.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 LIEBE92_COEFFICIENTS module
!
!       MWLBL_Rosenkranz03: Module containing data and routines to calculate
!                           microwave line-by-line transmittances according 
!                           to Rosenkranz03
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 ROSENKRANZ03_COEFFICIENTS module
!
! CONTAINS:
!       MWLBL_Compute_Tau:  Function to calculate microwave LBL transmittances
!                           for an input profile and frequency grid.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Sep-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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


MODULE MWLBL_Transmittance


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Error_Handler

  USE MWLBL_Liebe89
  USE MWLBL_Liebe93
  USE MWLBL_Rosenkranz03


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: MWLBL_Compute_Tau


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: MWLBL_Transmittance.f90,v 3.2 2005/01/25 21:34:18 paulv Exp $'

  ! -- Numerical constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO      =  0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE       =  1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TEN       = 10.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( ZERO )

  ! -- Keyword set value
  INTEGER,         PRIVATE, PARAMETER :: SET = 1

  ! -- Min/max frequencies in GHz
  REAL( fp_kind ), PARAMETER :: MIN_FREQUENCY = ONE
  REAL( fp_kind ), PARAMETER :: MAX_FREQUENCY = 1000.0_fp_kind



CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       MWLBL_Compute_Tau
!
! PURPOSE:
!       Function to calculate LBL microwave transmittances for an input
!       profile and frequency grid.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = MWLBL_Compute_Tau( Pressure,                  &  ! Input
!                                         Temperature,               &  ! Input
!                                         Water_Vapor_Pressure,      &  ! Input
!                                         Layer_Thickness,           &  ! Input
!                                         Angle_Secant,              &  ! Input
!                                         Frequency,                 &  ! Input
!
!                                         TauALL,                    &  ! Output
!                                         TauWLO,                    &  ! Output
!                                         TauWCO,                    &  ! Output
!                                         TauWET,                    &  ! Output
!                                         TauDRY,                    &  ! Output
!
!                                         Downwelling = Downwelling, &  ! Optional input
!                                         Rosenkranz  = Rosenkranz,  &  ! Optional input
!
!                                         RCS_Id      = RCS_Id,      &  ! Revision control
!                                         Quiet       = Quiet,       &  ! Output message control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Pressure:              Layer pressure profile. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order.
!                              UNITS:      hectoPascals (hPa)
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT( IN )
!
!       Temperature:           Layer temperature profile. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order but must be in the same order as
!                              the pressure profile.
!                              UNITS:      Kelvin (K)
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT( IN )
!
!       Water_Vapor_Pressure:  Layer water vapor partial pressure
!                              profile. Values can be in either TOA->SFC
!                              or SFC->TOA order but must be in the same
!                              order as the pressure profile.
!                              UNITS:      hectoPascals (hPa)
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT( IN )
!
!       Layer_Thickness:       Atmospheric layer thickness. Values can
!                              be in either TOA->SFC or SFC->TOA
!                              order but must be in the same order as
!                              the pressure profile.
!                              UNITS:      metres (m)
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, K
!                              ATTRIBUTES: INTENT( IN )
!
!       Angle_Secant:          Zenith angle secant used to modify
!                              path length.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, I
!                              ATTRIBUTES: INTENT( IN )
!
!       Frequency:             Frequency grid at which to calculate
!                              the microwave transmittances.
!                              UNITS:      Gigahertz (GHz)
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-1, L
!                              ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Downwelling:           Set this optional argument to perform a
!                              downwelling (LYR->SFC) transmittance
!                              calculations. If not specified, the default
!                              is to perform an upwelling (LYR->TOA)
!                              calculation.
!                              If = 0; Upwelling calculation (default)
!                                 = 1; Downwelling calculation.
!                              UNITS:      N/A 
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Rosenkranz:            Set this optional argument to perform the
!                              calculations using the Rosenkranz03 model.
!                              If not specified, the default is to use the
!                              Liebe89/93 model.
!                              If = 0; Use Liebe89/93 model (default)
!                                 = 1; Use Rosenkranz03 model
!                              UNITS:      N/A 
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Quiet:                 Set this optional argument to disable
!                              output of warning and information messages.
!                              If not specified the default action is to
!                              output all messages.
!                              If = 0; All messages are output (default)
!                                 = 1; Warning and information messages
!                                      are suppressed.
!                              UNITS:      N/A 
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:           Character string specifying a filename in which any
!                              Messages will be logged. If not specified, or if an
!                              error occurs opening the log file, the default action
!                              is to output Messages to standard output.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauALL:                Layer transmittance profiles due to all absorbers
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT( OUT )
!
!       TauWLO:                Layer wet transmittance profiles due to H2O line
!                              absorption only.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT( OUT )
!
!       TauWCO:                Layer wet transmittance profiles due to H2O
!                              continuum absorption only.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT( OUT )
!
!       TauWET:                Layer wet transmittance profiles due to H2O line
!                              *AND* continuum absorption.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT( OUT )
!
!       TauDRY:                Layer dry transmittance profiles due to O2 line 
!                              absorption *AND* dry gas non-resonant absorption.
!                              UNITS:      None.
!                              TYPE:       REAL( fp_kind )
!                              DIMENSION:  Rank-3, L x K x I
!                              ATTRIBUTES: INTENT( OUT ) 
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:                Character string containing the Revision Control
!                              System Id field for the module.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER( * )
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error
!                              status. The error codes are defined in the
!                              ERROR_HANDLER module.
!                              If == SUCCESS the calculation was successful
!                                 == FAILURE an unrecoverable error occurred.
!                                 == WARNING if any of the line and/or continuum
!                                            absorption calculations produced a
!                                            negative result and was set to zero.
!
! CALLS:
!      Liebe89:                Function to compute LBL microwave attenuation
!                              using the Liebe89 model.
!                              SOURCE: MWLBL_LIEBE89 module
!
!      Liebe93:                Function to compute LBL microwave attenuation
!                              using the Liebe93 model.
!                              SOURCE: MWLBL_LIEBE93 module
!
!      Rosenkranz03:           Function to compute LBL microwave attenuation
!                              using the Rosenkranz03 model.
!                              SOURCE: MWLBL_ROSENKRANZ03 module
!
!      Display_Message:        Subroutine to output Messages
!                              SOURCE: ERROR_HANDLER module
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!
!       Adapted from certain parts of F77 code written by L. Phalippou
!       28-Jul-1992 and modified by Peter Rayer and Roger Saunders, UKMO.
!S-
!------------------------------------------------------------------------------

  FUNCTION MWLBL_Compute_Tau( Pressure,              &  ! Input
                              Temperature,           &  ! Input
                              Water_Vapor_Pressure,  &  ! Input
                              Layer_Thickness,       &  ! Input
                              Angle_Secant,          &  ! Input
                              Frequency,             &  ! Input

                              TauALL,                &  ! Output
                              TauWLO,                &  ! Output
                              TauWCO,                &  ! Output
                              TauWET,                &  ! Output
                              TauDRY,                &  ! Output

                              Downwelling,           &  ! Optional input
                              Rosenkranz,            &  ! Optional input
                              Quiet,                 &  ! Optional input

                              RCS_Id,                &  ! Revision control

                              Message_Log )          &  ! Error messaging

                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Pressure              ! K
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Temperature           ! K
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Water_Vapor_Pressure  ! K
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Layer_Thickness       ! K
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Angle_Secant          ! I
    REAL( fp_kind ), DIMENSION( : ),       INTENT( IN )  :: Frequency             ! L

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: TauALL  ! L x K x I
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: TauWLO  ! L x K x I
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: TauWCO  ! L x K x I
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: TauWET  ! L x K x I
    REAL( fp_kind ), DIMENSION( :, :, : ), INTENT( OUT ) :: TauDRY  ! L x K x I

    ! -- Optional input
    INTEGER,         OPTIONAL,             INTENT( IN )  :: Downwelling
    INTEGER,         OPTIONAL,             INTENT( IN )  :: Rosenkranz

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL,             INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    INTEGER,         OPTIONAL,             INTENT( IN )  :: Quiet
    CHARACTER( * ),  OPTIONAL,             INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'MWLBL_Compute_Tau'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n_Frequencies
    INTEGER :: n_Layers, k
    INTEGER :: n_Angles, i

    LOGICAL :: Upwelling
    LOGICAL :: Liebe
    LOGICAL :: Noisy

    LOGICAL :: TOA_to_SFC
    INTEGER :: k_Reverse
    REAL( fp_kind ) :: deltaP

    REAL( fp_kind ), DIMENSION( SIZE( Pressure             ) ) :: P
    REAL( fp_kind ), DIMENSION( SIZE( Temperature          ) ) :: T
    REAL( fp_kind ), DIMENSION( SIZE( Water_Vapor_Pressure ) ) :: Wp
    REAL( fp_kind ), DIMENSION( SIZE( Layer_Thickness      ) ) :: dZ

    REAL( fp_kind ), DIMENSION( SIZE( Water_Vapor_Pressure ) ) :: Dp

    REAL( fp_kind ), DIMENSION( SIZE( Frequency ) ) :: WetLine_Attenuation
    REAL( fp_kind ), DIMENSION( SIZE( Frequency ) ) :: WetContinuum_Attenuation
    REAL( fp_kind ), DIMENSION( SIZE( Frequency ) ) :: DryLine_Attenuation
    REAL( fp_kind ), DIMENSION( SIZE( Frequency ) ) :: DryContinuum_Attenuation



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------
    ! Get dimensions
    ! --------------

    n_Frequencies = SIZE( Frequency )
    n_Layers      = SIZE( Pressure )
    n_Angles      = SIZE( Angle_Secant )


    ! --------------------------------------
    ! Check for consistent INPUT array sizes
    ! --------------------------------------

    IF ( SIZE( Temperature          ) /= n_Layers .OR. &
         SIZE( Water_Vapor_Pressure ) /= n_Layers .OR. &
         SIZE( Layer_Thickness      ) /= n_Layers      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input profile arrays have inconsistent sizes.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check for consistent OUTPUT array sizes
    ! ---------------------------------------

    IF ( SIZE( TauALL, DIM=1 ) /= n_Frequencies .OR. &
         SIZE( TauALL, DIM=2 ) /= n_Layers      .OR. &
         SIZE( TauALL, DIM=3 ) /= n_Angles      .OR. &

         SIZE( TauWLO, DIM=1 ) /= n_Frequencies .OR. &
         SIZE( TauWLO, DIM=2 ) /= n_Layers      .OR. &
         SIZE( TauWLO, DIM=3 ) /= n_Angles      .OR. &

         SIZE( TauWCO, DIM=1 ) /= n_Frequencies .OR. &
         SIZE( TauWCO, DIM=2 ) /= n_Layers      .OR. &
         SIZE( TauWCO, DIM=3 ) /= n_Angles      .OR. &

         SIZE( TauWET, DIM=1 ) /= n_Frequencies .OR. &
         SIZE( TauWET, DIM=2 ) /= n_Layers      .OR. &
         SIZE( TauWET, DIM=3 ) /= n_Angles      .OR. &

         SIZE( TauDRY, DIM=1 ) /= n_Frequencies .OR. &
         SIZE( TauDRY, DIM=2 ) /= n_Layers      .OR. &
         SIZE( TauDRY, DIM=3 ) /= n_Angles           ) THEN

      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Output transmittance arrays have inconsistent sizes.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
         

    ! ------------------------
    ! Check for invalid values
    ! ------------------------

    ! -- Profile data
    IF ( ANY( Pressure             < TOLERANCE ) .OR. &
         ANY( Temperature          < TOLERANCE ) .OR. &
         ANY( Water_Vapor_Pressure < TOLERANCE ) .OR. &
         ANY( Layer_Thickness      < TOLERANCE )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input profile data must be > 0.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Angle data
    IF ( ANY( Angle_Secant < ONE ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input angle secants must be > or = 1.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Frequency data
    IF ( ANY( Frequency < MIN_FREQUENCY ) .OR. ANY( Frequency > MAX_FREQUENCY ) ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input frequencies must be ", f3.2, " < f < ", f6.1 )' ) &
                      MIN_FREQUENCY, MAX_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the Downwelling optional argument
    ! ---------------------------------------
    
    ! -- Default is to calculate upwelling (i.e. layer->space) transmittances
    Upwelling = .TRUE.

    ! -- ....unless the DOWNWELLING optional argument is set
    IF ( PRESENT( Downwelling ) ) THEN
      IF ( Downwelling == SET ) Upwelling = .FALSE.
    END IF


    ! ---------------------------------------
    ! Check the Rosenkranz optional argument
    ! ---------------------------------------
    
    ! -- Default is use the Liebe model
    Liebe = .TRUE.

    ! -- ....unless the ROSENKRANZ optional argument is set
    IF ( PRESENT( Rosenkranz ) ) THEN
      IF ( Rosenkranz == SET ) Liebe = .FALSE.
    END IF


    ! ---------------------------------
    ! Check the Quiet optional argument
    ! ---------------------------------

    ! -- Default is to output information messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET optional argument is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                   -- REVERSE THE ARRAYS IF REQUIRED --                   #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Determine the input profile ordering
    ! ------------------------------------

    ! -- Assume TOA->SFC profile inputs by default...
    TOA_to_SFC = .TRUE.

    ! -- ....unless the pressure difference is negative
    deltaP = Pressure( 2 ) - Pressure( 1 )
    IF ( deltaP < ZERO ) TOA_to_SFC = .FALSE.


    ! ------------------------------------------------------------
    ! Check the array ordering with the up/downwelling requirement
    ! ------------------------------------------------------------

    IF ( TOA_to_SFC .EQV. Upwelling ) THEN

      ! -- Order is correct for required
      ! -- calculation, so simply copy
      P(:)  = Pressure(:)
      T(:)  = Temperature(:)
      Wp(:) = Water_Vapor_Pressure(:)
      dZ(:) = Layer_Thickness(:)

    ELSE

      ! -- Order must be reversed for
      ! -- required calculation. Using a
      ! -- loop rather than a -1 stride
      ! -- triplet is faster.
      DO k_Reverse = 1, n_Layers
        k = n_Layers - k_Reverse + 1

        P(k)  = Pressure(k_Reverse)
        T(k)  = Temperature(k_Reverse)
        Wp(k) = Water_Vapor_Pressure(k_Reverse)
        dZ(k) = Layer_Thickness(k_Reverse)
      END DO

      ! -- Output info message
      IF ( Noisy ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Input profile arrays reversed for required calculation.', &
                              INFORMATION, &
                              Message_Log = Message_Log )
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#           -- CONVERT LAYER THICKNESS TO UNITS OF KILOMETRES --           #
    !#--------------------------------------------------------------------------#

    dZ = dZ / 1000.0_fp_kind



    !#--------------------------------------------------------------------------#
    !#                -- COMPUTE THE DRY GAS PARTIAL PRESSURE --                #
    !#--------------------------------------------------------------------------#

    Dp = P - Wp



    !#--------------------------------------------------------------------------#
    !#                -- COMPUTE THE NADIR ATTENUATION PROFILE --               #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Loop over atmospheric levels
    ! ----------------------------

    k_Layer_loop: DO k = 1, n_Layers


      Liebe_or_Rosenkranz: IF ( Liebe ) THEN

        ! -----------------------------------------------------
        ! Compute the nadir layer attenuation using Liebe model
        ! -----------------------------------------------------

        ! -- Water vapor attenuation using the Liebe89 model
        Error_Status = Liebe89( Frequency, &
                                Dp(k), &
                                Wp(k), &
                                T(k), &
                                WetLine_Attenuation      = WetLine_Attenuation, &
                                WetContinuum_Attenuation = WetContinuum_Attenuation, &
                                Quiet       = Quiet, &
                                Message_Log = Message_Log )

        IF ( Error_Status == FAILURE ) THEN
          WRITE( Message, '( "Error calculating Liebe89 water vapor attentuation at level ", i3, "." )' ) &
                          k
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

        ! -- Dry gas attenuation using the Liebe93 model
        Error_Status = Liebe93( Frequency, &
                                Dp(k), &
                                Wp(k), &
                                T(k), &
                                DryLine_Attenuation      = DryLine_Attenuation, &
                                DryContinuum_Attenuation = DryContinuum_Attenuation, &
                                Quiet = Quiet, &
                                Message_Log = Message_Log )

        IF ( Error_Status == FAILURE ) THEN
          WRITE( Message, '( "Error calculating Liebe93 dry gas attentuation at level ", i3, "." )' ) &
                          k
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF


      ELSE Liebe_or_Rosenkranz


        ! ----------------------------------------------------------
        ! Compute the nadir layer attenuation using Rosenkranz model
        ! ----------------------------------------------------------

        Error_Status = Rosenkranz03( Frequency, &
                                     Dp(k), &
                                     Wp(k), &
                                     T(k), &
                                     WetLine_Attenuation      = WetLine_Attenuation, &
                                     WetContinuum_Attenuation = WetContinuum_Attenuation, &
                                     DryLine_Attenuation      = DryLine_Attenuation, &
                                     DryContinuum_Attenuation = DryContinuum_Attenuation, &
                                     Quiet       = Quiet, &
                                     Message_Log = Message_Log )

        IF ( Error_Status == FAILURE ) THEN
          WRITE( Message, '( "Error calculating Rosenkranz03 water vapor attentuation at level ", i3, "." )' ) &
                          k
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF Liebe_or_Rosenkranz


      ! -------------------------------------------------------
      ! Save the nadir ATTENUATIONs in the transmittance arrays
      ! -------------------------------------------------------

      IF ( k == 1 ) THEN

        ! -- First layer
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

        ! -- All the rest
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



    !#--------------------------------------------------------------------------#
    !#              -- COMPUTE THE TRANSMITTANCES FOR ALL ANGLES --             #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------------
    ! Loop over angles BACKWARDS. Thus the nadir attentuations
    ! are not overwritten as they are used.
    ! --------------------------------------------------------

    i_Angle_Loop: DO i = n_Angles, 1, -1

      ! -- Modify the path lengths
      TauALL(:,:,i) = TauALL(:,:,1) * Angle_Secant(i)
      TauWLO(:,:,i) = TauWLO(:,:,1) * Angle_Secant(i)
      TauWCO(:,:,i) = TauWCO(:,:,1) * Angle_Secant(i)
      TauWET(:,:,i) = TauWET(:,:,1) * Angle_Secant(i)
      TauDRY(:,:,i) = TauDRY(:,:,1) * Angle_Secant(i)

      ! -- Compute the transmittances
      TauALL(:,:,i) = 10**(-(TauALL(:,:,i)/TEN))
      TauWLO(:,:,i) = 10**(-(TauWLO(:,:,i)/TEN))
      TauWCO(:,:,i) = 10**(-(TauWCO(:,:,i)/TEN))
      TauWET(:,:,i) = 10**(-(TauWET(:,:,i)/TEN))
      TauDRY(:,:,i) = 10**(-(TauDRY(:,:,i)/TEN))

    END DO i_Angle_Loop

  END FUNCTION MWLBL_Compute_Tau

END MODULE MWLBL_Transmittance


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: MWLBL_Transmittance.f90,v 3.2 2005/01/25 21:34:18 paulv Exp $
!
! $Date: 2005/01/25 21:34:18 $
!
! $Revision: 3.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MWLBL_Transmittance.f90,v $
! Revision 3.2  2005/01/25 21:34:18  paulv
! - Removed type declarations of unused variables.
!
! Revision 3.1  2004/11/17 19:54:26  paulv
! - Added call to Rosenkranz absorption model based on new optional argument.
! - Updated header documentation.
!
! Revision 3.0  2004/05/27 22:46:02  paulv
! - New version with different interface. Now each computed transmittance
!   component is returned separately.
!
! Revision 2.0  2003/03/08 01:47:16  paulv
! - New version without instrument specific parameters.
! - Documentation added.
!
! Revision 1.3  2002/12/16 14:19:12  paulv
! - Completed method of computing LBL frequencies. Untested. Checked in for
!   synchronisation of repository for testing.
!
! Revision 1.2  2002/09/20 16:18:28  paulv
! - In process of adding calculation code. Incomplete.
!
! Revision 1.1  2002/09/12 20:35:32  paulv
! Initial checkin. Incomplete.
!
!
!
!
