!------------------------------------------------------------------------------
!M+
! NAME:
!       MWLBL_Rosenkranz03
!
! PURPOSE:
!       Module containing data and routines to calculate microwave atmospheric
!       attenuation according to the Rosenkranz 2003 LBL code.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE MWLBL_Rosenkranz03
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Fundamental_Constants:      Module containing various fundamental physical
!                                   constants.
!                                   USEs: TYPE_KINDS module
!
!       Units_Conversion:           Module containing routines to convert atmospheric
!                                   profile concentration units.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         PROFILE_UTILITY_PARAMETERS module
!                                         ATMOSPHERIC_PROPERTIES module
!
!       Rosenkranz03_Coefficients:  Module containing line and coefficient data for
!                                   microwave attenuation calculations according to
!                                   the Rosenkranz 2003 LBL code.
!                                   USEs: TYPE_KINDS module
!
! CONTAINS:
!       Rosenkranz03:    Function to calculate the atmospheric attentuation
!                        in the microwave according to Rosenkranz's 2003 model.
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
! CREATION HISTORY:
!       Some subprograms in this module have been translated from the
!       FORTRAN-77 source code written by P.W.Rosenkranz, 1988-2003
!
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Nov-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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


MODULE MWLBL_Rosenkranz03


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Error_Handler
  USE Fundamental_Constants
  USE Units_Conversion

  USE Rosenkranz03_Coefficients


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

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

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: MWLBL_Rosenkranz03.f90,v 1.4 2005/01/25 21:34:18 paulv Exp $'

  ! -- Numerical constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO           =  0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZEROpointFIVE  =  0.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE            =  1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONEpointONE    =  1.1_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TWOpointFIVE   =  2.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: THREE          =  3.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SEVENpointFIVE =  7.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TEN            = 10.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE      = EPSILON( ZERO )

  ! -- Conversion factor for Np->dB, 10 * LOG10(e)
  REAL( fp_kind ), PRIVATE, PARAMETER :: NP_TO_DB = TEN * E_LOG10

  ! -- Min/max temperatures in Kelvin
  REAL( fp_kind ), PRIVATE, PARAMETER :: MIN_TEMPERATURE = 150.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_TEMPERATURE = 350.0_fp_kind

  ! -- Min/max frequencies in GHz
  REAL( fp_kind ), PRIVATE, PARAMETER :: MIN_FREQUENCY = ONE
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_FREQUENCY = 800.0_fp_kind

  ! -- Definition of keyword set flag
  INTEGER,         PRIVATE, PARAMETER :: SET = 1


CONTAINS



  !#----------------------------------------------------------------------------#
  !#                            -- UTILITY ROUTINES --                          #
  !#----------------------------------------------------------------------------#

  ! --------------------------------
  ! Reference reciprocal temperature
  ! --------------------------------

  FUNCTION Compute_Theta( Temperature ) RESULT( Theta )
    REAL( fp_kind ), INTENT( IN ) :: Temperature
    REAL( fp_kind )               :: Theta

    REAL( fp_kind ), PARAMETER :: REFERENCE_TEMPERATURE = 300.0_fp_kind

    Theta = REFERENCE_TEMPERATURE / Temperature
  END FUNCTION Compute_Theta




!------------------------------------------------------------------------------
!S+
! NAME:
!       Rosenkranz03
!
! PURPOSE:
!       Function to calculate the atmospheric attentuation in the microwave
!       according to Liebe's MPM89 model.
!
! CATEGORY:
!       LBL : microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = Rosenkranz03( Frequency,                &  ! Input
!                                    Dry_Air_Pressure,         &  ! Input
!                                    H2O_Pressure,             &  ! Input
!                                    Temperature ,             &  ! Input
!
!                                    WetLine_Attenuation,      &  ! Optional output
!                                    WetContinuum_Attenuation, &  ! Optional output
!                                    DryLine_Attenuation,      &  ! Optional output
!                                    DryContinuum_Attenuation, &  ! Optional output
!
!                                    RCS_Id = RCS_Id,          &  ! Revision control
!                                    Quiet = Quiet,            &  ! Output message control
!                                    Message_Log = Message_Log )  ! Error messaging
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
! INPUT ARGUMENTS:
!       Frequency:                  Frequency for which the atmospheric attenuation
!                                   is required
!                                   UNITS:      GHz
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Scalar
!                                                 OR
!                                               Rank-1, n_Frequencies
!                                   ATTRIBUTES: INTENT( IN )
!
!       Dry_Air_Pressure:           Dry air partial pressure profile.
!                                   UNITS:      hectoPascals, hPa
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Rank-1, n_Layers
!                                                 OR
!                                               Scalar
!                                   ATTRIBUTES: INTENT( IN )
!
!       H2O_Pressure:               Water vapor partial pressure profile.
!                                   UNITS:      hectoPascals, hPa
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Same as DRY_AIR_PRESSURE argument
!                                   ATTRIBUTES: INTENT( IN )
!
!       Temperature:                Temperature profile.
!                                   UNITS:      Kelvin
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Same as DRY_AIR_PRESSURE argument
!                                   ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:                      Set this argument to suppress the output of
!                                   WARNING level messages. If not specified the
!                                   default is to output the messages.
!                                   If = 0, Output WARNING messages (default)
!                                      = 1, Suppress WARNING message output.
!                                   UNITS:      N/A
!                                   TYPE:       INTEGER
!                                   DIMENSION:  Scalar
!                                   ATTRIBUTES: INTENT( IN ), OPTIONAL
!                          
!       Message_Log:                Character string specifying a filename in which any
!                                   messages will be logged. If not specified, or if an
!                                   error occurs opening the log file, the default action
!                                   is to output messages to standard output.
!                                   UNITS:      N/A
!                                   TYPE:       CHARACTER( * )
!                                   DIMENSION:  Scalar
!                                   ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       WetLine_Attenuation:        Power attenuation due to absorption by water
!                                   vapour absorption lines.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       WetContinuum_Attenuation:   Power attenuation due to water vapour continuum
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       DryLine_Attenuation:        Power attenuation due to absorption by oxygen
!                                   absorption lines.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       DryContinuum_Attenuation:   Power attenuation due to non-resonant dry air
!                                   absorption.
!                                   UNITS:      dB/km
!                                   TYPE:       REAL( fp_kind )
!                                   DIMENSION:  Rank-1
!                                               If called BY LAYER: n_Layers
!                                                 OR
!                                               If called BY FREQUENCY: n_Frequencies
!                                   ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:                     Character string containing the Revision Control
!                                   System Id field for the module.
!                                   UNITS:      N/A
!                                   TYPE:       CHARACTER( * )
!                                   DIMENSION:  Scalar
!                                   ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:               The return value is an integer defining the error
!                                   status. The error codes are defined in the
!                                   ERROR_HANDLER module.
!                                   If == SUCCESS the calculation was successful
!                                      == FAILURE an error was found with the input.
!                                      == WARNING if any of the line and/or continuum
!                                                 absorption calculations produced a
!                                                 negative result and was set to zero.
!                                   UNITS:      N/A
!                                   TYPE:       INTEGER
!                                   DIMENSION:  Scalar
!                           
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
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
! PROCEDURE:
!       H2O absorption model:
!       ---------------------
!     P.W. Rosenkranz, CHAP. 2 and appendix, in ATMOSPHERIC REMOTE SENSING
!      BY MICROWAVE RADIOMETRY (M.A. Janssen, ed., 1993).
!   P.W. ROSENKRANZ, RADIO SCIENCE V.33, PP.919-928 (1998); V.34, P.1025 (1999).
!     A.BAUER ET AL.ASA WORKSHOP (SEPT. 1989) (380GHz).
!     M. TRETYAKOV et al., J. MOLEC. SPEC. (2003)
!
!       O2 absorption model:
!       --------------------
!     P.W. Rosenkranz, CHAP. 2 and appendix, in ATMOSPHERIC REMOTE SENSING
!      BY MICROWAVE RADIOMETRY (M.A. Janssen, ed., 1993).
!     H.J. Liebe et al, JQSRT V.48, pp.629-643 (1992).
!     M.J. Schwartz, Ph.D. thesis, M.I.T. (1998).
!     A.F. Krupnov et al, J. Mol. Spect. v.215, pp.309-311 (2002).
!     M.Yu. Tretyakov et al, J. Mol. Spect. (2003 preprint).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 17-Nov-2004
!                       paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Rosenkranz03_By_Frequency( Frequency,                &  ! Input
                                      Dry_Air_Pressure,         &  ! Input
                                      H2O_Pressure,             &  ! Input
                                      Temperature ,             &  ! Input

                                      WetLine_Attenuation,      &  ! Optional output
                                      WetContinuum_Attenuation, &  ! Optional output
                                      DryLine_Attenuation,      &  ! Optional output
                                      DryContinuum_Attenuation, &  ! Optional output

                                      RCS_Id,                   &  ! Revision control
                                      Quiet,                    &  ! Output message control
                                      Message_Log )             &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Frequency
    REAL( fp_kind ),                           INTENT( IN )  :: Dry_Air_Pressure
    REAL( fp_kind ),                           INTENT( IN )  :: H2O_Pressure
    REAL( fp_kind ),                           INTENT( IN )  :: Temperature

    ! -- Optional output
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: WetLine_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: WetContinuum_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: DryLine_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: DryContinuum_Attenuation

    ! -- Revision control
    CHARACTER( * ),                  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    INTEGER,                         OPTIONAL, INTENT( IN )  :: Quiet
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Rosenkranz03'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    LOGICAL :: Noisy

    INTEGER :: n_Frequencies, l

    REAL( fp_kind ) :: Theta

    REAL( fp_kind ) :: H2O_Line_Absorption
    REAL( fp_kind ) :: WetContinuum_Absorption

    REAL( fp_kind ) :: O2_Line_Absorption
    REAL( fp_kind ) :: DryContinuum_Absorption



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! -----------------------------------
    ! Determine what output arguments are
    ! present and check their sizes
    ! -----------------------------------

    n_Frequencies = SIZE( Frequency )


    ! -- The water vapour line attenuation
    Compute_WetLine = .FALSE.
    IF ( PRESENT( WetLine_Attenuation ) ) THEN

      Compute_WetLine = .TRUE.

      IF ( SIZE( WetLine_Attenuation ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency/WetLine_Attenuation argument arrays have inconsistent sizes.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour continuum attenuation
    Compute_WetContinuum = .FALSE.
    IF ( PRESENT( WetContinuum_Attenuation ) ) THEN

      Compute_WetContinuum = .TRUE.

      IF ( SIZE( WetContinuum_Attenuation ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency/WetContinuum_Attenuation argument arrays have inconsistent sizes.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour line attenuation
    Compute_DryLine = .FALSE.
    IF ( PRESENT( DryLine_Attenuation ) ) THEN

      Compute_DryLine = .TRUE.

      IF ( SIZE( DryLine_Attenuation ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency/DryLine_Attenuation argument arrays have inconsistent sizes.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour continuum attenuation
    Compute_DryContinuum = .FALSE.
    IF ( PRESENT( DryContinuum_Attenuation ) ) THEN

      Compute_DryContinuum = .TRUE.

      IF ( SIZE( DryContinuum_Attenuation ) /= n_Frequencies ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Frequency/DryContinuum_Attenuation argument arrays have inconsistent sizes.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! --------------------------------------
    ! If no output arguments present, return
    ! --------------------------------------

    IF ( ( .NOT. Compute_WetLine      ) .AND. &
         ( .NOT. Compute_WetContinuum ) .AND. &
         ( .NOT. Compute_DryLine      ) .AND. &
         ( .NOT. Compute_DryContinuum )       ) THEN
      RETURN
    END IF
         

    ! -----------------------------
    ! Check for invalid frequencies
    ! -----------------------------

    IF ( ANY( Frequency < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input frequencies must be > 0.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Issue warning if any frequencies < min value...
    IF ( ANY( Frequency < MIN_FREQUENCY ) ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input frequencies < ", f6.1, " GHz found." )' ) &
                      MIN_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    ! -- ....or > max_value
    IF ( ANY( Frequency > MAX_FREQUENCY ) ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input frequencies > ", f6.1, " GHz found." )' ) &
                      MAX_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------
    ! Profile data
    ! ------------

    IF ( Dry_Air_Pressure < TOLERANCE .OR. &
         H2O_Pressure     < TOLERANCE .OR. &
         Temperature      < TOLERANCE      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input tressure/temperature arguments must be > 0.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Issue warning if temperature < min value....
    IF ( Temperature < MIN_TEMPERATURE ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "TEMPERATURE is < ", f6.2, " K." )' ) MIN_TEMPERATURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    ! -- ....or > max_value
    IF ( Temperature > MAX_TEMPERATURE ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "TEMPERATURE is > ", f6.2, " K." )' ) MAX_TEMPERATURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------
    ! Output warning messages?
    ! ------------------------

    ! -- Default is to output warning messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET argument is set
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE RECIPROCAL TEMPERATURE --               #
    !#--------------------------------------------------------------------------#

    Theta = Compute_Theta( Temperature )



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE WET LINE ATTENUATION FOR EACH FREQUENCY --       #
    !#--------------------------------------------------------------------------#

    IF ( Compute_WetLine ) THEN


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      WetLine_Frequency_Loop: DO l = 1, n_Frequencies


        ! ----------------------------------------
        ! Compute the water vapour line absoprtion
        ! ----------------------------------------

        H2O_Line_Absorption = Compute_H2O_Line_Absorption( Frequency(l), &
                                                           Dry_Air_Pressure, &
                                                           H2O_Pressure, &
                                                           Temperature, &
                                                           Theta )
 
        ! -- Check result
!        IF ( H2O_Line_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative sum for H2O line absorption, ", es13.6, 1x, &
!                              &"at Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            H2O_Line_Absorption, Frequency(l)
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          H2O_Line_Absorption = ZERO
!
!        END IF



        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetLine_Attenuation( l ) = NP_TO_DB * H2O_Line_Absorption

      END DO WetLine_Frequency_Loop

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE WET CONTINUUM ATTENUATION FOR EACH FREQUENCY --    #
    !#--------------------------------------------------------------------------#

    IF ( Compute_WetContinuum ) THEN


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      WetContinuum_Frequency_Loop: DO l = 1, n_Frequencies


        ! ---------------------------------------------
        ! Compute the water vapour continuum absoprtion
        ! ---------------------------------------------

        WetContinuum_Absorption = Compute_Wet_Continuum( Frequency(l), &
                                                         Dry_Air_Pressure, &
                                                         H2O_Pressure, &
                                                         Theta )

        ! -- Check result
!        IF ( WetContinuum_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative value for wet continuum absorption, ", es13.6, 1x, &
!                              &"at Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            WetContinuum_Absorption, Frequency(l)
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          WetContinuum_Absorption = ZERO
!
!        END IF



        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetContinuum_Attenuation( l ) = NP_TO_DB * WetContinuum_Absorption

      END DO WetContinuum_Frequency_Loop

    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE DRY LINE ATTENUATION FOR EACH FREQUENCY --       #
    !#--------------------------------------------------------------------------#

    IF ( Compute_DryLine ) THEN


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      DryLine_Frequency_Loop: DO l = 1, n_Frequencies


        ! ----------------------------------
        ! Compute the oxygen line absoprtion
        ! ----------------------------------

        O2_Line_Absorption = Compute_O2_Line_Absorption( Frequency(l), &
                                                         Dry_Air_Pressure, &
                                                         H2O_Pressure, &
                                                         Temperature, &
                                                         Theta )

        ! -- Check result
!        IF ( O2_Line_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative sum for O2 line absorption, ", es13.6, 1x, &
!                              &" at Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            O2_Line_Absorption, Frequency(l)
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          O2_Line_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryLine_Attenuation( l ) = NP_TO_DB * O2_Line_Absorption

      END DO DryLine_Frequency_Loop

    END IF



    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE DRY CONTINUUM ATTENUATION FOR EACH FREQUENCY --     #
    !#--------------------------------------------------------------------------#

    IF ( Compute_DryContinuum ) THEN


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      DryContinuum_Frequency_Loop: DO l = 1, n_Frequencies


        ! -------------------------------------------
        ! Compute the non-resonant dry air absorption
        ! -------------------------------------------

        DryContinuum_Absorption = Compute_Dry_Continuum( Frequency(l), &
                                                         Dry_Air_Pressure, &
                                                         H2O_Pressure, &
                                                         Theta )

        ! -- Check result
!        IF ( DryContinuum_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative value for dry continuum absorption, ", es13.6, 1x, &
!                              &" at Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            DryContinuum_Absorption, Frequency(l)
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          DryContinuum_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryContinuum_Attenuation( l ) = NP_TO_DB * DryContinuum_Absorption

      END DO DryContinuum_Frequency_Loop

    END IF

  END FUNCTION Rosenkranz03_By_Frequency




  FUNCTION Rosenkranz03_By_Layer( Frequency,                &  ! Input
                                  Dry_Air_Pressure,         &  ! Input
                                  H2O_Pressure,             &  ! Input
                                  Temperature ,             &  ! Input

                                  WetLine_Attenuation,      &  ! Optional output
                                  WetContinuum_Attenuation, &  ! Optional output
                                  DryLine_Attenuation,      &  ! Optional output
                                  DryContinuum_Attenuation, &  ! Optional output

                                  RCS_Id,                   &  ! Revision control
                                  Quiet,                    &  ! Output message control
                                  Message_Log )             &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),                           INTENT( IN )  :: Frequency
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Dry_Air_Pressure
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: H2O_Pressure
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Temperature

    ! -- Optional output
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: WetLine_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: WetContinuum_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: DryLine_Attenuation
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( OUT ) :: DryContinuum_Attenuation

    ! -- Revision control
    CHARACTER( * ),                  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    INTEGER,                         OPTIONAL, INTENT( IN )  :: Quiet
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Rosenkranz03'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Compute_WetLine
    LOGICAL :: Compute_WetContinuum
    LOGICAL :: Compute_DryLine
    LOGICAL :: Compute_DryContinuum
    LOGICAL :: Noisy

    INTEGER :: n_Layers, k

    REAL( fp_kind ) :: Theta

    REAL( fp_kind ) :: H2O_Line_Absorption
    REAL( fp_kind ) :: WetContinuum_Absorption

    REAL( fp_kind ) :: O2_Line_Absorption
    REAL( fp_kind ) :: DryContinuum_Absorption



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    ! --------------------------------------
    ! Check for consistent input array sizes
    ! --------------------------------------

    n_Layers = SIZE( Dry_Air_Pressure )
    IF ( SIZE( H2O_Pressure ) /= n_Layers .OR. &
         SIZE( Temperature  ) /= n_Layers      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input argument arrays have inconsistent sizes.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------------
    ! Determine what output arguments are
    ! present and check their sizes
    ! -----------------------------------

    ! -- The water vapour line attenuation
    Compute_WetLine = .FALSE.
    IF ( PRESENT( WetLine_Attenuation ) ) THEN

      Compute_WetLine = .TRUE.

      IF ( SIZE( WetLine_Attenuation ) /= n_Layers ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'WetLine_Attenuation argument array has inconsistent size.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour continuum attenuation
    Compute_WetContinuum = .FALSE.
    IF ( PRESENT( WetContinuum_Attenuation ) ) THEN

      Compute_WetContinuum = .TRUE.

      IF ( SIZE( WetContinuum_Attenuation ) /= n_Layers ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'WetContinuum_Attenuation argument array has inconsistent size.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour line attenuation
    Compute_DryLine = .FALSE.
    IF ( PRESENT( DryLine_Attenuation ) ) THEN

      Compute_DryLine = .TRUE.

      IF ( SIZE( DryLine_Attenuation ) /= n_Layers ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'DryLine_Attenuation argument array has inconsistent size.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- The water vapour continuum attenuation
    Compute_DryContinuum = .FALSE.
    IF ( PRESENT( DryContinuum_Attenuation ) ) THEN

      Compute_DryContinuum = .TRUE.

      IF ( SIZE( DryContinuum_Attenuation ) /= n_Layers ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'DryContinuum_Attenuation argument array has inconsistent size.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! --------------------------------------
    ! If no output arguments present, return
    ! --------------------------------------

    IF ( ( .NOT. Compute_WetLine      ) .AND. &
         ( .NOT. Compute_WetContinuum ) .AND. &
         ( .NOT. Compute_DryLine      ) .AND. &
         ( .NOT. Compute_DryContinuum )       ) THEN
      RETURN
    END IF
         

    ! -----------------------------
    ! Check for invalid frequencies
    ! -----------------------------

    IF ( Frequency < ZERO ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Frequency must be > 0.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Issue warning if frequency < min value...
    IF ( Frequency < MIN_FREQUENCY ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input frequency < ", f6.1, " GHz." )' ) &
                      MIN_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    ! -- ....or > max_value
    IF ( Frequency > MAX_FREQUENCY ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input frequency > ", f6.1, " GHz." )' ) &
                      MAX_FREQUENCY
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------
    ! Profile data
    ! ------------

    IF ( ANY( Dry_Air_Pressure < TOLERANCE ) .OR. &
         ANY( H2O_Pressure     < TOLERANCE ) .OR. &
         ANY( Temperature      < TOLERANCE )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input pressure/temperature must be > 0.0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Issue warning if any Temperature < min value....
    IF ( ANY( Temperature < MIN_TEMPERATURE ) ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input temperature < ", f6.2, " K found." )' ) MIN_TEMPERATURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

    ! -- ....or > max_value
    IF ( ANY( Temperature > MAX_TEMPERATURE ) ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Input temperature > ", f6.2, " K found." )' ) MAX_TEMPERATURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------
    ! Output warning messages?
    ! ------------------------

    ! -- Default is to output warning messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET argument is set
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CALCULATE THE ATTENUATION FOR EACH LAYER --               #
    !#--------------------------------------------------------------------------#

    Layer_Loop: DO k = 1, n_Layers


      ! -------------------------------------------
      ! Calculate the reciprocal temperature, Theta
      ! -------------------------------------------

      Theta  = Compute_Theta( Temperature(k) )



      !#------------------------------------------------------------------------#
      !#                -- CALCULATE THE WET LINE ATTENTUATION --               #
      !#------------------------------------------------------------------------#

      IF ( Compute_WetLine ) THEN


        ! ----------------------------
        ! Water vapour line absorption
        ! ----------------------------

        H2O_Line_Absorption = Compute_H2O_line_Absorption( Frequency, &
                                                           Dry_Air_Pressure(k), &
                                                           H2O_Pressure(k), &
                                                           Temperature(k), &
                                                           Theta )
 
        ! -- Check result
!        IF ( H2O_Line_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative sum for H2O line absorption, ", es13.6, 1x, &
!                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            H2O_Line_Absorption, k, Frequency
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          H2O_Line_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetLine_Attenuation( k ) = NP_TO_DB * H2O_Line_Absorption

      END IF



      !#------------------------------------------------------------------------#
      !#             -- CALCULATE THE WET CONTINUUM ATTENTUATION --             #
      !#------------------------------------------------------------------------#

      IF ( Compute_WetContinuum ) THEN


        ! ----------------------------------
        ! Compute the water vapour continuum
        ! ----------------------------------

        WetContinuum_Absorption = Compute_Wet_Continuum( Frequency, &
                                                         Dry_Air_Pressure(k), &
                                                         H2O_Pressure(k), &
                                                         Theta )

        ! -- Check result
!        IF ( WetContinuum_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative value for wet continuum absorption, ", es13.6, 1x, &
!                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            WetContinuum_Absorption, k, Frequency
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          WetContinuum_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetContinuum_Attenuation( k ) = NP_TO_DB * WetContinuum_Absorption

      END IF



      !#------------------------------------------------------------------------#
      !#                -- CALCULATE THE DRY LINE ATTENTUATION --               #
      !#------------------------------------------------------------------------#

      IF ( Compute_DryLine ) THEN


        ! ----------------------------------
        ! Compute the oxygen line absoprtion
        ! ----------------------------------

        O2_Line_Absorption = Compute_O2_Line_Absorption( Frequency, &
                                                         Dry_Air_Pressure(k), &
                                                         H2O_Pressure(k), &
                                                         Temperature(k), &
                                                         Theta )

        ! -- Check result
!        IF ( O2_Line_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative sum for O2 line absorption, ", es13.6, 1x, &
!                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            O2_Line_Absorption, k, Frequency
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          O2_Line_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryLine_Attenuation( k ) = NP_TO_DB * O2_Line_Absorption

      END IF



      !#------------------------------------------------------------------------#
      !#            -- CALCULATE THE DRY CONTINUUM ATTENTUATION --              #
      !#------------------------------------------------------------------------#

      IF ( Compute_DryContinuum ) THEN


        ! -------------------------------------------
        ! Compute the dry gas non-resonant absorption
        ! -------------------------------------------

        DryContinuum_Absorption = Compute_Dry_Continuum( Frequency, &
                                                         Dry_Air_Pressure(k), &
                                                         H2O_Pressure(k), &
                                                         Theta )

        ! -- Check result
!        IF ( DryContinuum_Absorption < ZERO ) THEN
!
!          Error_Status = WARNING
!          IF ( Noisy ) THEN
!            WRITE( Message, '( "Negative value for dry continuum absorption, ", es13.6, 1x, &
!                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
!                              &"GHz. Setting to 0.0." )' ) &
!                            DryContinuum_Absorption, k, Frequency
!            CALL Display_Message( ROUTINE_NAME, &
!                                  TRIM( Message ), &
!                                  Error_Status, &
!                                  Message_Log = Message_Log )
!          END IF
!
!          DryContinuum_Absorption = ZERO
!
!        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryContinuum_Attenuation( k ) = NP_TO_DB * DryContinuum_Absorption

      END IF

    END DO Layer_Loop

  END FUNCTION Rosenkranz03_By_Layer






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
!                                                T,    &  ! Input
!                                                Theta )  ! Input
!
! INPUT ARGUMENTS:
!       f:           Frequency
!                    UNITS:      GHz
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       T:           Temperature.
!                    UNITS:      Kelvin, K
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
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
!                    TYPE:       REAL( fp_kind )
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
                                       T,      &  ! Input
                                       Theta ) &  ! Input
                                     RESULT ( Absorption )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: f
    REAL( fp_kind ), INTENT( IN ) :: pd
    REAL( fp_kind ), INTENT( IN ) :: pw
    REAL( fp_kind ), INTENT( IN ) :: T
    REAL( fp_kind ), INTENT( IN ) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: Absorption


    ! ---------------
    ! Local prameters
    ! ---------------

    REAL( fp_kind ), PARAMETER :: MAGNITUDE_SCALE_FACTOR = 0.001_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i
    REAL( fp_kind ) :: Pressure 
    REAL( fp_kind ) :: Theta08, Theta09, ONEminusTheta
    REAL( fp_kind ) :: H2O_Gamma_Component, Gamma08, Gamma09
    REAL( fp_kind ) :: Si
    REAL( fp_kind ) :: Gamma, Gamma2
    REAL( fp_kind ) :: Delta
    REAL( fp_kind ) :: f_diff, f_sum
    REAL( fp_kind ) :: Fi_n1, Fi_n2
    REAL( fp_kind ) :: Fi_d1, Fi_d2
    REAL( fp_kind ) :: Fi



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

    Absorption = PI_RECIPROCAL * 0.5034e12_fp_kind * Absorption * pd * Theta**3

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
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
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
!                    TYPE:       REAL( fp_kind )
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

    REAL( fp_kind ), INTENT( IN ) :: f
    REAL( fp_kind ), INTENT( IN ) :: pd
    REAL( fp_kind ), INTENT( IN ) :: pw
    REAL( fp_kind ), INTENT( IN ) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: Absorption


    ! ---------------
    ! Local prameters
    ! ---------------

    REAL( fp_kind ), PARAMETER :: MAGNITUDE_SCALE_FACTOR = 0.001_fp_kind

    ! -- Strength parameter. Constant = 1.6e-17 * 0.5034e12 from o2abs
    REAL( fp_kind ), PARAMETER :: S_K = PI_RECIPROCAL * 8.0544e-06_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: Gamma
    REAL( fp_kind ) :: f2, Gamma2
    REAL( fp_kind ) :: So, Fo

    REAL( fp_kind ) Denominator, Frequency_Dependence, bF



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

    Denominator = ONE + ( f / 450.0_fp_kind )**2
    Frequency_Dependence = ZEROpointFIVE + ( ZEROpointFIVE / Denominator )

    bF = 6.5e-14_fp_kind * Frequency_Dependence * ( ( pd + pw )**2 ) * ( f**2 ) * ( Theta**3.6_fp_kind )

    Absorption = Absorption + ( 1.29_fp_kind * bF )


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
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pd:          Dry air partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       pw:          Water vapor partial pressure.
!                    UNITS:      hectoPascals, hPa
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       T:           Temperature.
!                    UNITS:      Kelvin, K
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
!
!       Theta:       Reciprocal temperature ratio, 300/T
!                    UNITS:      None.
!                    TYPE:       REAL( fp_kind )
!                    DIMENSION:  Scalar
!                    ATTRIBUTES: INTENT( IN )
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
!                    TYPE:       REAL( fp_kind )
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

    REAL( fp_kind ), INTENT( IN ) :: f
    REAL( fp_kind ), INTENT( IN ) :: pd
    REAL( fp_kind ), INTENT( IN ) :: pw
    REAL( fp_kind ), INTENT( IN ) :: T
    REAL( fp_kind ), INTENT( IN ) :: Theta


    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: Absorption


    ! ----------------
    ! Local parameters
    ! ----------------

    REAL( fp_kind ), PARAMETER :: BASE_CONSTANT = 562500.0_fp_kind
    REAL( fp_kind ), PARAMETER :: DF_CUTOFF     = 750.0_fp_kind
    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR  = PI_RECIPROCAL * 1.0e-10_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j

    REAL( fp_kind ) :: Density
    REAL( fp_kind ) :: Theta25, ONEminusTheta
    REAL( fp_kind ) :: Gamma, Gamma2
    REAL( fp_kind ) :: F_Shift
    REAL( fp_kind ) :: S
    REAL( fp_kind ), DIMENSION(2) :: df
    REAL( fp_kind ) :: Base
    REAL( fp_kind ) :: Resonance



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

    Density = PP_to_ND( pw, T )


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
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       pd:                Dry air partial pressure.
!                          UNITS:      kiloPascals, kPa
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       pw:                Water vapor partial pressure.
!                          UNITS:      kiloPascals, kPa
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Theta:             Reciprocal temperature ratio, 300/T
!                          UNITS:      None.
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
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
!                    TYPE:       REAL( fp_kind )
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

    REAL( fp_kind ), INTENT( IN ) :: f
    REAL( fp_kind ), INTENT( IN ) :: pd
    REAL( fp_kind ), INTENT( IN ) :: pw
    REAL( fp_kind ), INTENT( IN ) :: Theta

    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: Absorption


    ! ----------------
    ! Local Parameters
    ! ----------------

    REAL( fp_kind ), PARAMETER :: CF = 5.43e-10_fp_kind
    REAL( fp_kind ), PARAMETER :: CS = 1.8e-8_fp_kind



    !#--------------------------------------------------------------------------#
    !#                       -- CALCULATE THE CONTINUUM --                      #
    !#--------------------------------------------------------------------------#

    Absorption = pw * f * f * ( ( CF * pd * (Theta**THREE)          ) + &
                                ( CS * pw * (Theta**SEVENpointFIVE) )   )

  END FUNCTION Compute_Wet_Continuum

END MODULE MWLBL_Rosenkranz03


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: MWLBL_Rosenkranz03.f90,v 1.4 2005/01/25 21:34:18 paulv Exp $
!
! $Date: 2005/01/25 21:34:18 $
!
! $Revision: 1.4 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MWLBL_Rosenkranz03.f90,v $
! Revision 1.4  2005/01/25 21:34:18  paulv
! - Removed type declarations of unused variables.
!
! Revision 1.3  2004/11/18 14:40:33  paulv
! - Added collision induced absorption calculation (equivalent of absn2 from
!   P.Rosenkranz's original f77 code) to the dry continuum subprogram.
!
! Revision 1.2  2004/11/17 19:19:08  paulv
! - Added oxygen absorption routines.
! - Added public wrapper routines.
! - Updated header documentation.
!
! Revision 1.1  2004/11/15 21:18:47  paulv
! Initial checkin. H2O functions only.
!
!
!
