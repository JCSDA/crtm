!------------------------------------------------------------------------------
!M+
! NAME:
!       SRF_Utility
!
! PURPOSE:
!       Module containing routines for application of SRF data.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SRF_Utility
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       Compare_Float_Numbers:  Module containing routines to perform equality
!                               and relational comparisons on floating point
!                               numbers.
!                               USEs: TYPE_KINDS module
!
!       Interpolate:            Module containing interpolation routines.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
!       Integrate:              Module containing integration routines.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     INTERPOLATE module
!
!       SRF_Define:             Module defining the SRF data structure and
!                               containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!
! CONTAINS:
!       Interpolate_SRF:        Function to interpolate input SRFs to another 
!                               frequency grid.
!
!       Convolve_with_SRF:      Function to convolve an spectrum with an SRF.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

MODULE SRF_Utility


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE Compare_Float_Numbers

  USE Interpolate
  USE Integrate

  USE SRF_Define


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Interpolate_SRF
  PUBLIC :: Convolve_with_SRF


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: SRF_Utility.f90,v 1.6 2006/05/02 16:58:02 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Literal constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE  = 1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONEpointFIVE  = 1.5_fp_kind


CONTAINS





!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------------
!
! NAME:
!       Compute_Frequency_Interval
!
! PURPOSE:
!       Function to compute the interval of the input frequency grid.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       dF = Compute_Frequency_Interval( F )
!
! INPUT ARGUMENTS:
!       F:   Array of frequency values. Should be regularly spaced and in
!            ascending order for the result to have any meaning.
!            UNITS:      Inverse centimetres (cm^-1)
!            TYPE:       REAL( fp_kind )
!            DIMENSION:  Rank-1
!            ATTRIBUTES: INTENT( IN )
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
!       dF:  Interval of the input frequency grid. 
!            UNITS:      Inverse centimetres (cm^-1)
!            TYPE:       REAL( fp_kind )
!            DIMENSION:  Scalar
!
! CALLS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       The input frequency grid should be regularly spaced and in ascending
!       order for the result to be meaningful
!
! PROCEDURE:
!       The average frequency interval for a frequency grid of nF points
!       is computed,
!
!                 __ nF-1
!                \
!                 >  F(i+1) - F(i)
!                /__
!                    i=1
!         dF = -----------------------
!                     nF - 1
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------

  FUNCTION Compute_Frequency_Interval( F ) RESULT( dF )

    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: F
    REAL( fp_kind ) :: dF
    INTEGER :: nF

    nF = SIZE( F )
    dF = SUM( F( 2:nF ) - F( 1:nF-1 ) ) / REAL( nF-1, fp_kind )

  END FUNCTION Compute_Frequency_Interval





!------------------------------------------------------------------------------------
!
! NAME:
!       Bracket_Points
!
! PURPOSE:
!       Subroutine to determine the index values of a frequency grid that
!       correspond to the begin and end frequencies of an SRF.
!
! CATEGORY:
!       Instrument Information: SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Bracket_Points( SRF_F1, &  ! Input
!                            SRF_F2, &  ! Input
!                            F1,     &  ! Input
!                            dF,     &  ! Input
!                            n1,     &  ! Output
!                            n2,     &  ! Output
!                            n       )  ! Output
!
! INPUT ARGUMENTS:
!       SRF_F1:   Frequency of the first point of an SRF.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL( fp_kind )
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN )
!
!       SRF_F2:   Frequency of the last point of an SRF. Should be > SRF_F1
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL( fp_kind )
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN )
!
!       F1:       Frequency of the first point of a frequency grid. Should
!                 be < SRF_F1.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL( fp_kind )
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN )
!
!       dF:       Interval of the frequency grid.
!                 UNITS:      Inverse centimetres (cm^-1)
!                 TYPE:       REAL( fp_kind )
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       n1:       Index of the frequency grid that corresponds to the first
!                 SRF point frequency.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( OUT )
!
!       n2:       Index of the frequency grid that corresponds to the last
!                 SRF point frequency.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( OUT )
!
!       n:        Total number of points between n1 and n2, inclusive.
!                 UNITS:      N/A
!                 TYPE:       INTEGER
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT( OUT )
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
!       The input frequency values should have magnitudes such that
!
!         F1 <= SRF_F1 < SRF_F2
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------------

  SUBROUTINE Bracket_Points( SRF_F1, SRF_F2, F1, dF, &  ! Inputs
                             n1, n2, n )                ! Outputs

    REAL( fp_kind ), INTENT( IN ) :: SRF_F1, SRF_F2  ! SRF begin and end frequencies
    REAL( fp_kind ), INTENT( IN ) :: F1, dF          ! Spectrum start frequency and interval

    INTEGER, INTENT( OUT ) :: n1, n2, n

    ! -- The begin point and frequency
    n1 = INT( ONEpointFIVE + ( ( SRF_F1 - F1 ) / dF ) )

    ! -- The end point and frequency
    n2 = INT( ONEpointFIVE + ( ( SRF_F2 - F1 ) / dF ) )

    ! -- Total number of points
    n = n2 - n1 + 1

  END SUBROUTINE Bracket_Points





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Interpolate_SRF
!
! PURPOSE:
!       Function to interpolate and input SRF to another frequency grid.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_STatus = Interpolate_SRF( Frequency,                &  ! Input
!                                       SRF,                      &  ! Input
!                                       iSRF,                     &  ! Output
!                                       Order       = Order,      &  ! Optional input
!                                       RCS_Id      = RCS_Id,     &  ! Revision control
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Frequency:    Frequency grid to which the SRF will be interpolated.
!                     UNITS:      Inverse centimetres (cm^-1)
!                     TYPE:       REAL( fp_kind )
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT( IN )
!
!       SRF:          SRF structure containing the instrument channel
!                     response to be interpolated.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Order:        The order of the interpolating polynomial. This 
!                     argument, if supplied, is passed to the interpolation
!                     function where, if not specified, linear interpolation
!                     (order = 1) is the default. If specified, must be an
!                     odd number > 0.
!                     UNITS:      None
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       Character
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       iSRF:         SRF structure containing the instrument channel
!                     response interpolated to the input frequency grid.
!                     UNITS:      N/A
!                     TYPE:       SRF_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the SRF integration was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! OPERATORS:
!       .GreaterThan.               Relational operator to test if one floating
!                                   point operand is greater than another.
!                                   SOURCE: COMPARE_FLOAT_NUMBERS module
!
!       .LessThan.                  Relational operator to test if one floating
!                                   point operand is less than another.
!                                   SOURCE: COMPARE_FLOAT_NUMBERS module
!
! CALLS:
!      Allocate_SRF:                Function to allocate the pointer members
!                                   of an SRF structure.
!                                   SOURCE: SRF_DEFINE module
!
!      Frequency_SRF:               Function to compute the frequency grid
!                                   for an SRF data structure
!                                   SOURCE SRF_DEFINE module
!
!      Polynomial_Interpolate:      Function to perform polynomial interpolation.
!                                   SOURCE: INTERPOLATE module
!                                   
!      Simpsons_Integral:           Function to integratea tabulated function
!                                   using Simpson's rule.
!                                   SOURCE: INTEGRATE module
!
!      Display_Message:             Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!      Compute_Frequency_Interval:  PRIVATE subroutine to compute the frequency
!                                   spacing of the input frequency grid.
!
!      Bracket_Points:              PRIVATE subroutine to compute the points
!                                   in the frequency grid that bracket the
!                                   SRF.
!
! CONTAINS:
!       None.
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SRF argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Interpolate_SRF( Frequency,    &  ! Input
                            SRF,          &  ! Input
                            iSRF,         &  ! Output
                            Order,        &  ! Optional input
                            RCS_Id,       &  ! Revision control
                            Message_Log ) &  ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Frequency
    TYPE( SRF_type ),                INTENT( IN )     :: SRF

    ! -- Output
    TYPE( SRF_type ),                INTENT( IN OUT ) :: iSRF

    ! -- Optional input
    INTEGER,               OPTIONAL, INTENT( IN )     :: Order

    ! -- Revision control
    CHARACTER( * ),        OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),        OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Interpolate_SRF'

    REAL( fp_kind ), PARAMETER :: EPS = EPSILON( 1.0_fp_kind )


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: n_Frequencies
    INTEGER :: n1, n2, n
    REAL( fp_kind ) :: Frequency_Interval



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_SRF( SRF ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT SRF pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Check frequency for -ve values
    IF ( ANY( Frequency < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Negative input frequencies found', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Check frequency ordering
    IF ( Frequency(2) <  Frequency(1) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input frequencies must be in ascending order', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Check frequency limits
    n_Frequencies = SIZE( Frequency )

    IF ( ( Frequency(1)             .GreaterThan. SRF%Begin_Frequency ) .OR. &
         ( Frequency(n_Frequencies) .LessThan.    SRF%End_Frequency   )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Interpolating frequencies do not span entire SRF.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      WRITE( Message, '( "Frequency(1),             ", f22.16, " >  SRF%Begin_Frequency, ", f22.16 )' ) &
                      Frequency(1), SRF%Begin_Frequency
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      WRITE( Message, '( "Frequency(n_Frequencies), ", f22.16, " <  SRF%End_Frequency,   ", f22.16 )' ) &
                      Frequency(n_Frequencies), SRF%End_Frequency
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE BEGIN AND END INTERPOLATED FREQUENCIES --        #
    !#        -- AND THE NUMBER OF INTERPOLATED POINTS                --        #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Calculate the input frequency interval
    ! --------------------------------------

    Frequency_Interval = Compute_Frequency_Interval( Frequency )


    ! ------------------------------------------------
    ! Get the SRF bracket points in the input spectrum
    ! ------------------------------------------------

    CALL Bracket_Points( SRF%Begin_Frequency, SRF%End_Frequency, &
                         Frequency(1), Frequency_Interval, &
                         n1, n2, n )


    
    !#--------------------------------------------------------------------------#
    !#                 -- ALLOCATE THE INTERPOLATED SRF STRUCTURE --            #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_SRF( n_Frequencies, iSRF )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating iSRF structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#           -- COPY THE SCALAR COMPONENTS OF THE INPUT SRF --              #
    !#--------------------------------------------------------------------------#

    ! -- All the scalar stuff
    Error_Status = Assign_SRF( SRF, iSRF, &
                               Scalar_Only = SET, &
                               Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error copying scalar components of input SRF to output iSRF.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- INTERPOLATE THE INPUT SRF --                      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Assign the interpolation frequency
    ! grid to the output SRF structure
    ! ----------------------------------

    iSRF%Begin_Frequency = Frequency(1)
    iSRF%End_Frequency   = Frequency(n_Frequencies)
    iSRF%Frequency       = Frequency


    ! --------------------
    ! Do the interpolation
    ! --------------------

    Error_Status = Polynomial_Interpolate( SRF%Frequency,  &  ! Input,  X
                                           SRF%Response,   &  ! Input,  Y
                                           iSRF%Frequency, &  ! Input,  Xint
                                           iSRF%Response,  &  ! Output, Yint
                                           Order = Order,  &  ! Optional input
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error interpolating input SRF response', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------
    ! Ensure there is no extrapolation
    ! --------------------------------

    ! -- Low frequency edge
    IF ( n1 > 1 ) iSRF%Response(1:n1) = ZERO

    ! -- High frequency edge
    IF ( n2 < n_Frequencies ) iSRF%Response(n2:) = ZERO


    ! ------------------------------
    ! Integrate the interpolated SRF
    ! ------------------------------

    Error_Status = Integrate_SRF( iSRF, &
                                  Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occurred integrating interpolated SRF', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Interpolate_SRF


!------------------------------------------------------------------------------
!S+
! NAME:
!       Convolve_with_SRF
!
! PURPOSE:
!       Procedure to convolve a spectrum with an instrument channel SRF.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Convolve_with_SRF( Frequency,                 &  ! Input
!                                         Spectrum,                  &  ! Input
!                                         SRF,                       &  ! Input
!                                         Convolved_Spectrum,        &  ! Output
!                                         Interpolate = Interpolate, &  ! Optional input
!                                         Integrate   = Integrate,   &  ! Optional input
!                                         Order       = Order,       &  ! Optional input
!                                         RCS_Id      = RCS_Id,      &  ! Revision control
!                                         Message_Log = Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Frequency:           Frequency grid of the input spectrum.
!                            UNITS:      Inverse centimetres (cm^-1)
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: INTENT( IN )
!
!       Spectrum:            Spectrum to be convolved with the SRF response.
!                            UNITS:      Variable.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Rank-1
!                            ATTRIBUTES: INTENT( IN )
!
!       SRF:                 SRF structure containing the instrument channel
!                            response to be used in the convolution.
!                            UNITS:      N/A
!                            TYPE:       SRF_type
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Interpolate:         Set this argument to force interpolation of the SRF
!                            response to the input frequency grid. If not set,
!                            the default is to assume that the SRF and spectrum
!                            frequency grids match up for the required span.
!                            If = 0; do not interpolate the SRF response (DEFAULT)
!                               = 1; interpolate input SRF response to the input
!                                    frequency grid.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Integrate:           Set this argument to perform numerical integration
!                            when doing the convolutions. If not set, the default
!                            action is to simply sum the results.
!                            If = 0; use summation to compute convolved value. (DEFAULT)
!                               = 1; perform numerical integration to compute the
!                                    convolved value.
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Order:               The order of the interpolating polynomial. This
!                            argument, if supplied, is passed to the interpolation
!                            function where, if not specified, linear interpolation
!                            (order = 1) is the default. If specified, must be an
!                            odd number > 0.
!                            UNITS:      None
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:         Character string specifying a filename in which any
!                            messages will be logged. If not specified, or if an
!                            error occurs opening the log file, the default action
!                            is to output messages to standard output.
!                            UNITS:      None
!                            TYPE:       Character
!                            DIMENSION:  Scalar, LEN = *
!                            ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Convolved_Spectrum:  The input spectrum convolved with the input SRF
!                            response.
!                            UNITS:      Variable.
!                            TYPE:       REAL( fp_kind )
!                            DIMENSION:  Scalar
!                            ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:              Character string containing the Revision Control
!                            System Id field for the module.
!                            UNITS:      None
!                            TYPE:       CHARACTER
!                            DIMENSION:  Scalar, LEN = *
!                            ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:        The return value is an integer defining the error status.
!                            The error codes are defined in the ERROR_HANDLER module.
!                            If == SUCCESS the spectrum convolution was successful
!                               == FAILURE an error occurred
!                            UNITS:      N/A
!                            TYPE:       INTEGER
!                            DIMENSION:  Scalar
!
! CALLS:
!      Interpolate_SRF:             Function to interpolate and input SRF to
!                                   another frequency grid.
!
!      Assign_SRF:                  Function to copy an SRF structure.
!                                   SOURCE: SRF_DEFINE module
!
!      Destroy_SRF:                 Function to re-initialize an SRF structure.
!                                   SOURCE: SRF_DEFINE module
!
!      Simpsons_Integral:           Function to integratea tabulated function
!                                   using Simpson's rule.
!                                   SOURCE: INTEGRATE module
!
!      Display_Message:             Subroutine to output messages
!                                   SOURCE: ERROR_HANDLER module
!
!      Compute_Frequency_Interval:  PRIVATE subroutine to compute the frequency
!                                   spacing of the input frequency grid.
!
!      Bracket_Points:              PRIVATE subroutine to compute the points
!                                   in the frequency grid that bracket the
!                                   SRF.
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2003
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Convolve_with_SRF( Frequency,          &  ! Input
                              Spectrum,           &  ! Input
                              SRF,                &  ! Input
                              Convolved_Spectrum, &  ! Output
                              Interpolate,        &  ! Optional input
                              Integrate,          &  ! Optional input
                              Order,              &  ! Optional input
                              RCS_Id,             &  ! Revision control
                              Message_Log )       &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Frequency
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Spectrum
    TYPE( SRF_type ),                INTENT( IN )  :: SRF

    ! -- Output
    REAL( fp_kind ),                 INTENT( OUT ) :: Convolved_Spectrum

    ! -- Optional input
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Interpolate
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Integrate
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Order

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,        INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Convolve_with_SRF'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Frequency_Match
    LOGICAL :: Summation

    TYPE( SRF_type ) :: SRF_To_Use

    INTEGER :: n_Frequencies
    INTEGER :: n1, n2, n

    REAL( fp_kind ) :: Frequency_Interval



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
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
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Check for consistent array sizes
    ! --------------------------------

    n_Frequencies = SIZE( Frequency )
    IF ( SIZE( Spectrum ) /= n_Frequencies ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Size of input frequency and spectrum arrays are different', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Check optional arguments
    ! ------------------------

    ! -- Default is no SRF interpolation as SRF and spectrum frequency match is assumed...
    Frequency_Match = .TRUE.
    ! -- ...unless the Interpolate argument is set
    IF ( PRESENT( Interpolate ) ) THEN
      IF ( Interpolate == SET ) Frequency_Match = .FALSE.
    END IF

    ! -- Default is to sum the convolved SRF and spectrum...
    Summation = .TRUE.
    ! -- ...unless the Integrate argument is set
    IF ( PRESENT( Integrate ) ) THEN
      IF ( Integrate == SET ) Summation = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- COPY OR INTERPOLATE SRF AS REQUIRED --                #
    !#--------------------------------------------------------------------------#

    IF ( Frequency_Match ) THEN


      ! ------------
      ! Copy the SRF
      ! ------------

      Error_Status = Assign_SRF( SRF, SRF_To_Use, &
                                 Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying SRF', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    ELSE


      ! -------------------
      ! Interpolate the SRF
      ! -------------------

      Error_Status = Interpolate_SRF( Frequency, &
                                      SRF, &
                                      SRF_To_Use, &
                                      Order = Order, &
                                      Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error interpolating SRF', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#             -- CALCULATE THE BEGIN AND END SRF FREQUENCIES --            #
    !#             -- AND THE NUMBER OF SRF POINTS                --            #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Calculate the input frequency interval
    ! --------------------------------------

    Frequency_Interval = Compute_Frequency_Interval( Frequency )


    ! ------------------------------------------------
    ! Get the SRF bracket points in the input spectrum
    ! ------------------------------------------------

    CALL Bracket_Points( SRF_To_Use%Begin_Frequency, SRF_To_Use%End_Frequency, &
                         Frequency(1), Frequency_Interval, &
                         n1, n2, n )



    !#--------------------------------------------------------------------------#
    !#                      -- PERFORM THE CONVOLUTION --                       #
    !#--------------------------------------------------------------------------#

    IF ( Summation ) THEN


      ! ----------------
      ! Simple summation
      ! ----------------

      Convolved_Spectrum = SUM( SRF_To_Use%Response * Spectrum(n1:n2) ) * Frequency_Interval / &
                           SRF_to_Use%Summation_SRF

    ELSE


      ! -----------
      ! Integration
      ! -----------

      ! -- Integrate the function
      Error_Status = Simpsons_Integral( Frequency(n1:n2), &
                                        SRF_To_Use%Response * Spectrum(n1:n2), &
                                        Convolved_Spectrum, &
                                        Order = Order )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occurred integrating SRF*Spectrum', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Normalize it
      Convolved_Spectrum = Convolved_Spectrum / SRF_to_Use%Integrated_SRF

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- DESTROY THE LOCAL SRF STRUCTURE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SRF( SRF_To_Use, &
                                Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying local SRF_To_Use structure', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Convolve_with_SRF

END MODULE SRF_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SRF_Utility.f90,v 1.6 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 1.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_Utility.f90,v $
! Revision 1.6  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.5  2004/08/31 20:57:15  paulv
! - Upgraded to Fortran95.
! - Now using Compare_Float_Numbers module for floating point comparisons.
! - Changed INTENT of iSRF structure in Interpolate_SRF() function from OUT to
!   IN OUT. Necessary to prevent memory leaks.
! - Added structure association test to the Interpolate_SRF() function.
!
! Revision 1.4  2003/11/19 15:26:27  paulv
! - Updated header documentation.
!
! Revision 1.3  2003/09/15 15:33:49  paulv
! - Changes made to use updated SRF definition module.
!
! Revision 1.2  2003/03/25 22:31:19  paulv
! - Corrected convolution to normalise the result with the integrated SRF.
! - Added documentation.
!
! Revision 1.1  2003/03/21 22:40:36  paulv
! Initial checkin. Incomplete.
!
!
!
