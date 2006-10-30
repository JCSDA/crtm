!------------------------------------------------------------------------------
!M+
! NAME:
!       MWLBL_Liebe89
!
! PURPOSE:
!       Module containing data and routines to calculate microwave line-by-line
!       transmittances according to the Liebe 89 formulation.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE MWLBL_Liebe89
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       Error_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Liebe89_Coefficients:  Module containing line and coefficient data for
!                              microwave attenuation calculations according to
!                              the Liebe 89 reference.
!                              USEs: TYPE_KINDS module
! CONTAINS:
!       Liebe89:   Function to calculate the atmospheric attentuation
!                  in the microwave according to Liebe's MPM89 model.
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
!       This module has been translated from FORTRAN-77 source code
!       and Include files written by:
!         L. Phalippou 28-Jul-1992
!         Peter Rayer UKMO
!         Roger Saunders UKMO
!
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Apr-2002
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


MODULE MWLBL_Liebe89


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Error_Handler

  USE Liebe89_Coefficients


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Liebe89


  ! -------------------
  ! Procedure overloads
  ! -------------------

  INTERFACE Liebe89
    MODULE PROCEDURE Liebe89_By_Layer
    MODULE PROCEDURE Liebe89_By_Frequency
  END INTERFACE ! Liebe89


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: MWLBL_Liebe89.f90,v 2.2 2005/01/25 21:34:18 paulv Exp $'

  ! -- Numerical constants
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZERO           = 0.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ZEROpointEIGHT = 0.8_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE            = 1.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONEpointONE    = 1.1_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONEpointFIVE   = 1.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: THREE          = 3.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: THREEpointFIVE = 3.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: SEVENpointFIVE = 7.5_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE      = EPSILON( ZERO )

  ! -- Conversion factor for attenuation computation.
  REAL( fp_kind ), PRIVATE, PARAMETER :: ATT_K = 0.1820_fp_kind

  ! -- Min/max temperatures in Kelvin
  REAL( fp_kind ), PRIVATE, PARAMETER :: MIN_TEMPERATURE = 150.0_fp_kind
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_TEMPERATURE = 350.0_fp_kind

  ! -- Min/max frequencies in GHz
  REAL( fp_kind ), PRIVATE, PARAMETER :: MIN_FREQUENCY = ONE
  REAL( fp_kind ), PRIVATE, PARAMETER :: MAX_FREQUENCY = 1000.0_fp_kind

  ! -- Definition of keyword set flag
  INTEGER,         PRIVATE, PARAMETER :: SET = 1



CONTAINS



  !#----------------------------------------------------------------------------#
  !#                            -- UTILITY ROUTINES --                          #
  !#----------------------------------------------------------------------------#

  ! -------------------------
  ! Pressure scaling function
  ! -------------------------

  FUNCTION Scale_Pressure_to_kPa( Pressure_in_hPa ) RESULT( Pressure_in_kPa )
    REAL( fp_kind ), INTENT( IN ) :: Pressure_in_hPa
    REAL( fp_kind )               :: Pressure_in_kPa

    REAL( fp_kind ), PARAMETER :: HPA_TO_KPA = 0.1_fp_kind

    Pressure_in_kPa = Pressure_in_hPa * HPA_TO_KPA
  END FUNCTION Scale_Pressure_to_kPa


  ! --------------------------------
  ! Reference reciprocal temperature
  ! --------------------------------

  FUNCTION Compute_Theta( Temperature ) RESULT( Theta )
    REAL( fp_kind ), INTENT( IN ) :: Temperature
    REAL( fp_kind )               :: Theta

    REAL( fp_kind ), PARAMETER :: REFERENCE_TEMPERATURE = 300.0_fp_kind

    Theta = REFERENCE_TEMPERATURE / Temperature
  END FUNCTION Compute_Theta


  ! --------------------------------------
  ! Common O2 and H2O line strength factor
  ! --------------------------------------

  FUNCTION Compute_Common_s1( Theta ) RESULT( Common_s1 )
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind )               :: Common_s1

    Common_s1 = ONE - Theta
  END FUNCTION Compute_Common_s1


  ! -----------------------
  ! O2 line strength factor
  ! -----------------------

  FUNCTION Compute_O2_s1( Dry_Air_P, Theta ) RESULT( O2_s1 )
    REAL( fp_kind ), INTENT( IN ) :: Dry_Air_P
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind )               :: O2_s1

    O2_s1 = Dry_Air_P * ( Theta**THREE )
  END FUNCTION Compute_O2_s1


  ! --------------------
  ! O2 line width factor
  ! --------------------

  FUNCTION Compute_O2_g1( H2O_P, Theta ) RESULT( O2_g1 )
    REAL( fp_kind ), INTENT( IN ) :: H2O_P
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind )               :: O2_g1

    O2_g1 = ONEpointONE * H2O_P * Theta
  END FUNCTION Compute_O2_g1


  ! ----------------------
  ! O2 line overlap factor
  ! ----------------------

  FUNCTION Compute_O2_d1( Dry_Air_P, Theta ) RESULT( O2_d1 )
    REAL( fp_kind ), INTENT( IN ) :: Dry_Air_P
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind )               :: O2_d1

    O2_d1 = Dry_Air_P * ( Theta**ZEROpointEIGHT )
  END FUNCTION Compute_O2_d1


  ! ------------------------
  ! H2O line strength factor
  ! ------------------------

  FUNCTION Compute_H2O_s1( H2O_P, Theta ) RESULT( H2O_s1 )
    REAL( fp_kind ), INTENT( IN ) :: H2O_P
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind )               :: H2O_s1

    H2O_s1 = H2O_P * ( Theta**THREEpointFIVE )
  END FUNCTION Compute_H2O_s1



!------------------------------------------------------------------------------
!S+
! NAME:
!       Liebe89
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
!       Error_Status = Liebe89( Frequency,                &  ! Input
!                               Dry_Air_Pressure,         &  ! Input
!                               H2O_Pressure,             &  ! Input
!                               Temperature ,             &  ! Input
!
!                               WetLine_Attenuation,      &  ! Optional output
!                               WetContinuum_Attenuation, &  ! Optional output
!                               DryLine_Attenuation,      &  ! Optional output
!                               DryContinuum_Attenuation, &  ! Optional output
!
!                               RCS_Id = RCS_Id,          &  ! Revision control
!                               Quiet = Quiet,            &  ! Output message control
!                               Message_Log = Message_Log )  ! Error messaging
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
!       Liebe, H.J., 1989, MPM - An atmospheric millimeter-wave
!         propagation model, International Journal of Infrared and
!         Millimeter Waves, Vol 10(6), pp631-650
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 28-Jul-1992
!       F77 version modified by: Peter Rayer UKMO
!                                Roger Saunders UKMO
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Liebe89_By_Frequency( Frequency,                &  ! Input
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

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Liebe89'


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

    REAL( fp_kind ) :: Dry_Air_P
    REAL( fp_kind ) :: H2O_P
    REAL( fp_kind ) :: Theta

    REAL( fp_kind ) :: O2_s1, Common_s1, O2_g1, O2_d1, H2O_s1

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
    !#                    -- COMPUTE SOME COMMON QUANTITIES --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Scale the pressures to kPa
    ! --------------------------

    Dry_Air_P = Scale_Pressure_to_kPa( Dry_Air_Pressure )
    H2O_P     = Scale_Pressure_to_kPa( H2O_Pressure )


    ! --------------------------------------------
    ! Calculate the reciprocal temperature, theta,
    ! and common line strength factor, Common_s1
    ! --------------------------------------------

    Theta     = Compute_Theta( Temperature )
    Common_s1 = Compute_Common_s1( Theta )



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE WET LINE ATTENUATION FOR EACH FREQUENCY --       #
    !#--------------------------------------------------------------------------#

    IF ( Compute_WetLine ) THEN


      ! --------------------------------------
      ! Calculate the H2O line strength factor
      ! --------------------------------------

      H2O_s1 = Compute_H2O_s1( H2O_P, Theta )


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      WetLine_Frequency_Loop: DO l = 1, n_Frequencies


        ! ----------------------------------------
        ! Compute the water vapour line absoprtion
        ! ----------------------------------------

        H2O_Line_Absorption = Compute_H2O_Line_Absorption( Frequency(l), &
                                                           Dry_Air_P, &
                                                           H2O_P, &
                                                           Theta, &
                                                           Common_s1, &
                                                           H2O_s1 )
 
        ! -- Check result
        IF ( H2O_Line_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for H2O line absorption, ", es13.6, 1x, &
                              &"at Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            H2O_Line_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          H2O_Line_Absorption = ZERO

        END IF



        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetLine_Attenuation( l ) = ATT_K * Frequency( l ) * H2O_Line_Absorption

!       *** THIS CONVERSION WAS COMMENTED OUT IN UKMO CODE ***
!       *** A STRIAGHT SUM WAS USED FOR ALL FREQUENCIES    ***
!       ! -- Scale water vapour component for low frequencies
!       IF ( Frequency < 50.0_fp_kind ) THEN
!         att = att * ONEpointONE
!       END IF

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
                                                         Dry_Air_P, &
                                                         H2O_P, &
                                                         Theta )

        ! -- Check result
        IF ( WetContinuum_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for wet continuum absorption, ", es13.6, 1x, &
                              &"at Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            WetContinuum_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          WetContinuum_Absorption = ZERO

        END IF



        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetContinuum_Attenuation( l ) = ATT_K * Frequency( l ) * WetContinuum_Absorption

!       *** THIS CONVERSION WAS COMMENTED OUT IN UKMO CODE ***
!       *** A STRIAGHT SUM WAS USED FOR ALL FREQUENCIES    ***
!       ! -- Scale water vapour component for low frequencies
!       IF ( Frequency < 50.0_fp_kind ) THEN
!         att = att * ONEpointONE
!       END IF

      END DO WetContinuum_Frequency_Loop

    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE DRY LINE ATTENUATION FOR EACH FREQUENCY --       #
    !#--------------------------------------------------------------------------#

    IF ( Compute_DryLine ) THEN


      ! --------------------------------------
      ! Calculate the O2 line strength, width,
      ! and overlap factors
      ! --------------------------------------

      O2_s1 = Compute_O2_s1( Dry_Air_P, Theta )
      O2_g1 = Compute_O2_g1( H2O_P, Theta )
      O2_d1 = Compute_O2_d1( Dry_Air_P, Theta )


      ! ----------------------------------
      ! Loop over the required frequencies
      ! ----------------------------------

      DryLine_Frequency_Loop: DO l = 1, n_Frequencies


        ! ----------------------------------
        ! Compute the oxygen line absoprtion
        ! ----------------------------------

        O2_Line_Absorption = Compute_O2_Line_Absorption( Frequency(l), &
                                                         Dry_Air_P, &
                                                         Theta, &
                                                         Common_s1, &
                                                         O2_s1, &
                                                         O2_g1, &
                                                         O2_d1 )

        ! -- Check result
        IF ( O2_Line_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for O2 line absorption, ", es13.6, 1x, &
                              &" at Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            O2_Line_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          O2_Line_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryLine_Attenuation( l ) = ATT_K * Frequency( l ) * O2_Line_Absorption

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
                                                         Dry_Air_P, &
                                                         H2O_P,     &
                                                         Theta      )

        ! -- Check result
        IF ( DryContinuum_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for dry continuum absorption, ", es13.6, 1x, &
                              &" at Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            DryContinuum_Absorption, Frequency(l)
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          DryContinuum_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryContinuum_Attenuation( l ) = ATT_K * Frequency( l ) * DryContinuum_Absorption

      END DO DryContinuum_Frequency_Loop

    END IF

  END FUNCTION Liebe89_By_Frequency




  FUNCTION Liebe89_By_Layer( Frequency,                &  ! Input
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

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Liebe89'


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

    REAL( fp_kind ) :: Dry_Air_P
    REAL( fp_kind ) :: H2O_P
    REAL( fp_kind ) :: Theta

    REAL( fp_kind ) :: O2_s1, Common_s1, O2_g1, O2_d1, H2O_s1

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


      ! --------------------------
      ! Scale the pressures to kPa
      ! --------------------------

      Dry_Air_P = Scale_Pressure_to_kPa( Dry_Air_Pressure(k) )
      H2O_P     = Scale_Pressure_to_kPa( H2O_Pressure(k) )


      ! --------------------------------------------
      ! Calculate the reciprocal temperature, Theta,
      ! and common line strength factor, Common_s1
      ! --------------------------------------------

      Theta     = Compute_Theta( Temperature(k) )
      Common_s1 = Compute_Common_s1( Theta )



      !#------------------------------------------------------------------------#
      !#                -- CALCULATE THE WET LINE ATTENTUATION --               #
      !#------------------------------------------------------------------------#

      IF ( Compute_WetLine ) THEN


        ! ----------------------------
        ! Calculate the H2O absorption
        ! equation coefficients
        ! ----------------------------

        H2O_s1 = Compute_H2O_s1( H2O_P, Theta )



        ! ----------------------------
        ! Water vapour line absorption
        ! ----------------------------

        H2O_Line_Absorption = Compute_H2O_line_Absorption( Frequency, &
                                                           Dry_Air_P, &
                                                           H2O_P, &
                                                           Theta, &
                                                           Common_s1, &
                                                           H2O_s1 )
 
        ! -- Check result
        IF ( H2O_Line_Absorption < ZERO ) THEN

          Error_Status = WARNING

          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for H2O line absorption, ", es13.6, 1x, &
                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            H2O_Line_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          H2O_Line_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetLine_Attenuation( k ) = ATT_K * Frequency * H2O_Line_Absorption

!       *** THIS CONVERSION WAS COMMENTED OUT IN UKMO CODE ***
!       *** A STRIAGHT SUM WAS USED FOR ALL FREQUENCIES    ***
!       ! -- Scale water vapour component for low frequencies
!       IF ( Frequency < 50.0_fp_kind ) THEN
!         att = att * ONEpointONE
!       END IF

      END IF



      !#------------------------------------------------------------------------#
      !#             -- CALCULATE THE WET CONTINUUM ATTENTUATION --             #
      !#------------------------------------------------------------------------#

      IF ( Compute_WetContinuum ) THEN


        ! ----------------------------------
        ! Compute the water vapour continuum
        ! ----------------------------------

        WetContinuum_Absorption = Compute_Wet_Continuum( Frequency, &
                                                         Dry_Air_P, &
                                                         H2O_P, &
                                                         Theta )

        ! -- Check result
        IF ( WetContinuum_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for wet continuum absorption, ", es13.6, 1x, &
                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            WetContinuum_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          WetContinuum_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        WetContinuum_Attenuation( k ) = ATT_K * Frequency * WetContinuum_Absorption

!       *** THIS CONVERSION WAS COMMENTED OUT IN UKMO CODE ***
!       *** A STRIAGHT SUM WAS USED FOR ALL FREQUENCIES    ***
!       ! -- Scale water vapour component for low frequencies
!       IF ( Frequency < 50.0_fp_kind ) THEN
!         att = att * ONEpointONE
!       END IF

      END IF



      !#------------------------------------------------------------------------#
      !#                -- CALCULATE THE DRY LINE ATTENTUATION --               #
      !#------------------------------------------------------------------------#

      IF ( Compute_DryLine ) THEN


        ! --------------------------------------
        ! Calculate the O2 line strength, width,
        ! and overlap factors
        ! --------------------------------------

        O2_s1 = Compute_O2_s1( Dry_Air_P, Theta )
        O2_g1 = Compute_O2_g1( H2O_P, Theta )
        O2_d1 = Compute_O2_d1( Dry_Air_P, Theta )


        ! ----------------------------------
        ! Compute the oxygen line absoprtion
        ! ----------------------------------

        O2_Line_Absorption = Compute_O2_Line_Absorption( Frequency, &
                                                         Dry_Air_P, &
                                                         Theta, &
                                                         Common_s1, &
                                                         O2_s1, &
                                                         O2_g1, &
                                                         O2_d1 )

        ! -- Check result
        IF ( O2_Line_Absorption < ZERO ) THEN

          Error_Status = WARNING

          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for O2 line absorption, ", es13.6, 1x, &
                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            O2_Line_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          O2_Line_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryLine_Attenuation( k ) = ATT_K * Frequency * O2_Line_Absorption

      END IF



      !#------------------------------------------------------------------------#
      !#            -- CALCULATE THE DRY CONTINUUM ATTENTUATION --              #
      !#------------------------------------------------------------------------#

      IF ( Compute_DryContinuum ) THEN


        ! -------------------------------------------
        ! Compute the dry gas non-resonant absorption
        ! -------------------------------------------

        DryContinuum_Absorption = Compute_Dry_Continuum( Frequency, &
                                                         Dry_Air_P, &
                                                         H2O_P, &
                                                         Theta )

        ! -- Check result
        IF ( DryContinuum_Absorption < ZERO ) THEN

          Error_Status = WARNING
          IF ( Noisy ) THEN
            WRITE( Message, '( "Negative sum for dry continuum absorption, ", es13.6, 1x, &
                              &"in layer ", i5, " for Frequency ", f8.3, 1x, &
                              &"GHz. Setting to 0.0." )' ) &
                            DryContinuum_Absorption, k, Frequency
            CALL Display_Message( ROUTINE_NAME, &
                                  TRIM( Message ), &
                                  Error_Status, &
                                  Message_Log = Message_Log )
          END IF

          DryContinuum_Absorption = ZERO

        END IF


        ! --------------------------------
        ! Compute the attenuation in dB/km
        ! --------------------------------

        DryContinuum_Attenuation( k ) = ATT_K * Frequency * DryContinuum_Absorption

      END IF

    END DO Layer_Loop

  END FUNCTION Liebe89_By_Layer





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_O2_Line_Absorption
!
! PURPOSE:
!       Function to calculate the oxygen line terms of the imaginary part of
!       the atmospheric refractivity.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Compute_O2_Line_Absorption( f,         &  ! Input
!                                            pd,        &  ! Input
!                                            Theta,     &  ! Input
!                                            Common_s1, &  ! Input
!                                            O2_s1,     &  ! Input
!                                            O2_g1,     &  ! Input
!                                            O2_d1 )    &  ! Input
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
!       Theta:             Reciprocal temperature ratio, 300/T
!                          UNITS:      None.
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Common_s1:         Line strength factor common to both O2 and H2O
!                          forumlations, (1 - Theta)
!                          UNITS:      None.
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       O2_s1:             O2 line strength factor, pd * Theta^3
!                          UNITS:      kPa
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       O2_g1:             O2 line width factor, 1.1 * pw * Theta
!                          UNITS:      kPa
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       O2_d1:             O2 line overlap factor, pd * Theta^0.8
!                          UNITS:      kPa
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
!       The return value is the dry air refractivity due to O2 line
!       absorption in ppm.
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
!       Liebe, H.J., 1989, MPM - An atmospheric millimeter-wave
!         propagation model, International Journal of Infrared and
!         Millimeter Waves, Vol 10(6), pp631-650
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 28-Jul-1992
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_O2_Line_Absorption( f,         &  ! Input
                                       pd,        &  ! Input
                                       Theta,     &  ! Input
                                       Common_s1, &  ! Input
                                       O2_s1,     &  ! Input
                                       O2_g1,     &  ! Input
                                       O2_d1 )    &  ! Input
                                     RESULT ( O2_Line_Absorption )


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    REAL( fp_kind ), INTENT( IN ) :: f
    REAL( fp_kind ), INTENT( IN ) :: pd
    REAL( fp_kind ), INTENT( IN ) :: Theta
    REAL( fp_kind ), INTENT( IN ) :: Common_s1
    REAL( fp_kind ), INTENT( IN ) :: O2_s1
    REAL( fp_kind ), INTENT( IN ) :: O2_g1
    REAL( fp_kind ), INTENT( IN ) :: O2_d1


    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: O2_Line_Absorption


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i
    REAL( fp_kind ) :: Si
    REAL( fp_kind ) :: Gamma, Gamma2
    REAL( fp_kind ) :: Delta
    REAL( fp_kind ) :: f_diff, f_sum
    REAL( fp_kind ) :: Fi_n1, Fi_n2
    REAL( fp_kind ) :: Fi_d1, Fi_d2
    REAL( fp_kind ) :: Fi



    !#--------------------------------------------------------------------------#
    !#         -- INITIALISE THE LINE ABSORPTION SUMMATION VARIABLE --          #
    !#--------------------------------------------------------------------------#

    O2_Line_Absorption = ZERO      



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER THE O2 LINES --                       #
    !#                                                                          #
    !# The imaginary part of the refractivity due to O2 line absorption in      #
    !# moist air can be expressed by,                                           #
    !#         __                                                               #
    !#        \                                                                 #
    !#   ND =  > S(f).F(f)    ppm  .....(8)                                     #
    !#        /__                                                               #
    !#          i                                                               #
    !#                                                                          #
    !# with F being the line shape function,                                    #
    !#                                                                          #
    !#          f   [    g - d.(fo - f)         g - d.(fo + f)    ]             #
    !#  F(f) = ---- [ -------------------- + -------------------- ]  .....(9)   #
    !#          fo  [  ( fo - f )^2 + g^2     ( fo + f )^2 + g^2  ]             #
    !#                                                                          #
    !# where i = O2 line index                                                  #
    !#       f = frequency                                                      #
    !#       S = O2 line strength                                               #
    !#       g = O2 line width (gamma)                                          #
    !#       d = O2 line overlap (delta)                                        #
    !#                                                                          #
    !# The line strength, width, and overlap are modeled using,                 #
    !#                                                                          #
    !#   S = a1.10^-6 . pd . theta^3 . EXP[ a2 . ( 1 - theta ) ]  .....(10)     #
    !#                                                                          #
    !#   g = a3.10^-3 ( pd.theta^[0.8-a4] + 1.1.pw.theta )        .....(11)     #
    !#                                                                          #
    !#   d = ( a5 + a6.theta ).10^-3 . pd . theta^0.8             .....(12)     #
    !#                                                                          #
    !# with a1,a2 = line strength coefficients                                  #
    !#      a3,a4 = line width coefficients                                     #
    !#      a5,a6 = line overlap coefficients                                   #
    !#      pd    = dry gas partial pressure                                    #
    !#      pw    = water vapor partial pressure                                #
    !#      theta = reciprocal temperature ratio, 300/T                         #
    !#                                                                          #
    !# All equation numbers refer to the referenced Liebe89 paper.              #
    !#--------------------------------------------------------------------------#

    DO i = 1, N_O2_LINES


      ! ---------------------------
      ! Calculate the line strength
      ! ---------------------------

      Si = O2_A1(i) * O2_s1 * EXP( O2_A2(i) * Common_s1 )


      ! -----------------------------------------------
      ! Calculate the line width and overlap parameters
      ! -----------------------------------------------

      ! -- Calculate the pressure-broadened line width
      Gamma  = O2_A3(i) * ( ( pd * ( Theta**(ZEROpointEIGHT - O2_A4(i)) ) ) + O2_g1 )
      Gamma2 = Gamma * Gamma

      ! -- Calculate the line overlap (interference)
      Delta = ( O2_A5(i) + ( O2_A6(i) * Theta ) ) * O2_d1


      ! ---------------------------
      ! Calculate the line shape, F
      ! ---------------------------

      ! -- Frequency difference and sum from the line frequency
      f_diff = O2_LINE_FREQUENCY(i) - f
      f_sum  = O2_LINE_FREQUENCY(i) + f

      ! -- The numerator of the line shape function components
      Fi_n1 = Gamma - ( Delta * f_diff )
      Fi_n2 = Gamma - ( Delta * f_sum  )

      ! -- The denominator of the line shape components
      Fi_d1 = ( f_diff * f_diff ) + Gamma2
      Fi_d2 = ( f_sum  * f_sum  ) + Gamma2

      ! -- Calculate the absorption line shape
      Fi = ( f / O2_LINE_FREQUENCY(i) ) * ( ( Fi_n1/Fi_d1 ) + ( Fi_n2/Fi_d2 ) )


      ! ----------------------------------------
      ! Calculate the imaginary component of the
      ! dry air refractivity - the absorption
      ! ----------------------------------------

      O2_Line_Absorption = O2_Line_Absorption + ( Si * Fi )

    END DO

  END FUNCTION Compute_O2_Line_Absorption




!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Dry_Continuum
!
! PURPOSE:
!       Function to calculate the nonresonant imaginary term of the dry air
!       refractivity.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Compute_Dry_Continuum( f,         &  ! Input
!                                       pd,        &  ! Input
!                                       pw,        &  ! Input
!                                       Theta )    &  ! Input
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
!       The return value is the nonresonant refractivity term for dry air
!       in ppm.
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
!       Liebe, H.J., 1989, MPM - An atmospheric millimeter-wave
!         propagation model, International Journal of Infrared and
!         Millimeter Waves, Vol 10(6), pp631-650
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 28-Jul-1992
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_Dry_Continuum( f,       &  ! Input
                                  pd,      &  ! Input
                                  pw,      &  ! Input
                                  Theta )  &  ! Input
                                RESULT ( Dry_Continuum )


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
    
    REAL( fp_kind ) :: Dry_Continuum


    ! ----------
    ! Parameters
    ! ----------

    REAL( fp_kind ), PARAMETER :: So_K    = 6.14e-4_fp_kind
    REAL( fp_kind ), PARAMETER :: GAMMA_K = 5.6e-3_fp_kind
    REAL( fp_kind ), PARAMETER :: Sn_K    = 1.4e-10_fp_kind
    REAL( fp_kind ), PARAMETER :: Fn_K    = 1.2e-5_fp_kind



    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: Gamma
    REAL( fp_kind ) :: So, Fo
    REAL( fp_kind ) :: Sn, Fn



    !#--------------------------------------------------------------------------#
    !#                     -- START THE CALCULATION --                          #
    !#                                                                          #
    !# Nonresonant refractivity of dry air makes a small contribution at        #
    !# surface pressures due to the Debye spectrum of oxygen below 10GHz and    #
    !# the pressure-induced nitrogen absorption that becomes effective above    #
    !# 100GHz. The functional form is,                                          #
    !#                                                                          #
    !#   Nc = So.Fo + Sn.Fn  .....(13)                                          #
    !#                                                                          #
    !# where So,Fo = nonresonant O2 spectrum contribution                       #
    !#       Sn,Fn = pressure-induced N2 absorption contribution                #
    !#                                                                          #
    !#                                                                          #
    !# The non-resonant O2 strength and lineshape are given by,                 #
    !#                                                                          #
    !#   So = So_K . pd . theta^2                                               #
    !#                                                                          #
    !# and                                                                      #
    !#                                                                          #
    !#                 f                                                        #
    !#   Fo = ----------------------                                            #
    !#              [     ( f )^2 ]                                             #
    !#          g . [ 1 + (---)   ]                                             #
    !#              [     ( g )   ]                                             #
    !#                                                                          #
    !# where So_K = strength coefficient                                        #
    !#       f    = frequency,                                                  #
    !#       g    = Debye width                                                 #
    !#            = go . ( pd + 1.1pw ) . theta                                 #
    !#                                                                          #
    !# and go    = width coefficient,                                           #
    !#     pd    = dry air partial pressure                                     #
    !#     pw    = water vapor partial pressure                                 #
    !#     theta = reciprocal temperature ration, 300/T                         #
    !#                                                                          #
    !#                                                                          #
    !# The strength and line shape of the N2 absorption term is given by,       #
    !#                                                                          #
    !#   Sn = Sn_K . pd^2 . theta^3.5                                           #
    !#                                                                          #
    !# and                                                                      #
    !#                                                                          #
    !#                 f                                                        #
    !#   Fn = ----------------------                                            #
    !#         1 + ( Fn_K . f^1.5 )                                             #
    !#                                                                          #
    !# where Sn_K = strength coefficient                                        #
    !#       Fn_K = line shape ecoefficient                                     #
    !#                                                                          #
    !# Note that the form of the N2 term given in equation (13) in the Liebe89  #
    !# paper is different from that above. This correction was noted in the     #
    !# original ECMWF code.                                                     #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------------
    ! Calculate the effect of the Debye oxygen spectrum
    ! -------------------------------------------------

    ! -- Calculate the strength
    So = So_K * pd * ( Theta**2 )

    ! -- Calculate the width
    Gamma = GAMMA_K * ( pd + ( ONEpointONE * pw ) ) * Theta

    ! -- Calculate the shape
    Fo = f / ( Gamma * ( ONE + (f/Gamma)**2 ) )


    ! ------------------------------------------------------
    ! Calculate the effect of pressure induced N2 absorption
    ! ------------------------------------------------------

    ! -- Calculate the N2 absorption strength
    Sn = Sn_K * ( pd**2 ) * ( Theta**THREEpointFIVE )

    ! -- Calculate the shape
    Fn = f / ( ONE + ( Fn_K * ( f**ONEpointFIVE ) ) )


    ! ------------------
    ! Sum the components
    ! ------------------

    Dry_Continuum = ( So * Fo ) + ( Sn * Fn )

  END FUNCTION Compute_Dry_Continuum





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_H2O_Line_Absorption
!
! PURPOSE:
!       Function to calculate the water vapor line terms of the imaginary part
!       of the atmospheric refractivity.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Compute_H2O_Line_Absorption( f,         &  ! Input
!                                             pd,        &  ! Input
!                                             pd,        &  ! Input
!                                             Theta,     &  ! Input
!                                             Common_s1, &  ! Input
!                                             H2O_s1 )   &  ! Input
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
!       Common_s1:         Line strength factor common to both H2O and H2O
!                          forumlations, (1 - Theta)
!                          UNITS:      None.
!                          TYPE:       REAL( fp_kind )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       H2O_s1:            H2O line strength factor, pw * Theta^3.5
!                          UNITS:      kPa
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
!       The return value is the atmospheric refractivity due to H2O line
!       absorption in ppm.
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
!       Liebe, H.J., 1989, MPM - An atmospheric millimeter-wave
!         propagation model, International Journal of Infrared and
!         Millimeter Waves, Vol 10(6), pp631-650
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 28-Jul-1992
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_H2O_Line_Absorption( f,         &  ! Input
                                        pd,        &  ! Input
                                        pw,        &  ! Input
                                        Theta,     &  ! Input
                                        Common_s1, &  ! Input
                                        H2O_s1 )   &  ! Input
                                      RESULT ( H2O_Line_Absorption )


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
    REAL( fp_kind ), INTENT( IN ) :: Common_s1
    REAL( fp_kind ), INTENT( IN ) :: H2O_s1


    ! ---------------
    ! Function result
    ! ---------------

    REAL( fp_kind ) :: H2O_Line_Absorption


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i
    REAL( fp_kind ) :: Si
    REAL( fp_kind ) :: Gamma, Gamma2
    REAL( fp_kind ) :: f_diff, f_sum
    REAL( fp_kind ) :: Fi_d1, Fi_d2
    REAL( fp_kind ) :: Fi



    !#--------------------------------------------------------------------------#
    !#         -- INITIALISE THE LINE ABSORPTION SUMMATION VARIABLE --          #
    !#--------------------------------------------------------------------------#

    H2O_Line_Absorption = ZERO



    !#--------------------------------------------------------------------------#
    !#                      -- LOOP OVER THE H2O LINES --                       #
    !#                                                                          #
    !# The imaginary part of the refractivity due to H2O line absorption in     #
    !# moist air can be expressed by,                                           #
    !#         __                                                               #
    !#        \                                                                 #
    !#   NW =  > S(f).F(f)    ppm  .....(8)                                     #
    !#        /__                                                               #
    !#          i                                                               #
    !#                                                                          #
    !# with F being the line shape function,                                    #
    !#                                                                          #
    !#          f   [    g - d.(fo - f)         g - d.(fo + f)    ]             #
    !#  F(f) = ---- [ -------------------- + -------------------- ]  .....(9)   #
    !#          fo  [  ( fo - f )^2 + g^2     ( fo + f )^2 + g^2  ]             #
    !#                                                                          #
    !# where i = H2O line index                                                 #
    !#       f = frequency                                                      #
    !#       S = H2O line strength                                              #
    !#       g = H2O line width (gamma)                                         #
    !#       d = H2O line overlap (delta)                                       #
    !#                                                                          #
    !# The line strength, width, and overlap are modeled using,                 #
    !#                                                                          #
    !#   S = a1 . pw . theta^3.5 . EXP[ a2 . ( 1 - theta ) ]  .....(10)         #
    !#                                                                          #
    !#   g = a3.10^-3 ( pd.theta^a4 + a5.pw.theta^a6 )        .....(11)         #
    !#                                                                          #
    !#   d = 0                                                .....(12)         #
    !#                                                                          #
    !# with a1,a2       = line strength coefficients                            #
    !#      a3,a4,a5,a6 = line width coefficients                               #
    !#      pd          = dry gas partial pressure                              #
    !#      pw          = water vapor partial pressure                          #
    !#      theta       = reciprocal temperature ratio, 300/T                   #
    !#                                                                          #
    !# All equation numbers refer to the referenced Liebe89 paper. The Liebe    #
    !# paper refers to the H2O coefficients as b1-b6.                           #
    !#--------------------------------------------------------------------------#

    DO i = 1, N_H2O_LINES


      ! ---------------------------
      ! Calculate the line strength
      ! ---------------------------

      Si = H2O_A1(i) * H2O_s1 * EXP( H2O_A2(i) * Common_s1 )


      ! -------------------------------------------
      ! Calculate the pressure-broadened line width
      ! -------------------------------------------

      Gamma = H2O_A3(i) * ( (             pd * ( Theta**H2O_A4(i) ) ) + &
                            ( H2O_A5(i) * pw * ( Theta**H2O_A6(i) ) )   )
      Gamma2 = Gamma * Gamma


      ! ---------------------------
      ! Calculate the line shape, F
      ! ---------------------------

      ! -- Frequency difference and sum from the line frequency
      f_diff = H2O_LINE_FREQUENCY(i) - f
      f_sum  = H2O_LINE_FREQUENCY(i) + f

      ! -- The denominator of the line shape components
      Fi_d1 = ( f_diff * f_diff ) + Gamma2
      Fi_d2 = ( f_sum  * f_sum  ) + Gamma2

      ! -- Calculate the absorption line shape
      Fi = ( f / H2O_LINE_FREQUENCY(i) ) * ( ( Gamma/Fi_d1 ) + ( Gamma/Fi_d2 )   )


      ! -----------------------------------------
      ! Calculate the imaginary component of the
      ! water vapor refractivity - the absorption
      ! -----------------------------------------

      H2O_Line_Absorption = H2O_Line_Absorption + ( Si * Fi )

    END DO

  END FUNCTION Compute_H2O_Line_Absorption





!------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Wet_Continuum
!
! PURPOSE:
!       Function to calculate the atmospheric refractivity due to the
!       water vapor continuum.
!
! CATEGORY:
!       LBL : Microwave
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       result = Compute_Wet_Continuum( f,         &  ! Input
!                                       pd,        &  ! Input
!                                       pw,        &  ! Input
!                                       Theta )    &  ! Input
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
!       The return value is the atmospheric refractivity term for the
!       water vapor continuum in ppm.
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
!       Liebe, H.J., 1989, MPM - An atmospheric millimeter-wave
!         propagation model, International Journal of Infrared and
!         Millimeter Waves, Vol 10(6), pp631-650
!
! CREATION HISTORY:
!       F77 version written by : L. Phalippou 28-Jul-1992
!
!       F90 version translation: Paul van Delst, CIMSS/SSEC 21-Apr-2002
!                                paul.vandelst@ssec.wisc.edu
!
!S-
!------------------------------------------------------------------------------

  FUNCTION Compute_Wet_Continuum( f,       &  ! Frequency, GHz
                                  pd,      &  ! Dry air pressure, kPa
                                  pw,      &  ! Water vapour pressure, kPa
                                  Theta )  &  ! Temperature ratio, 300/T
                                RESULT ( Wet_Continuum )



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

    REAL( fp_kind ) :: Wet_Continuum


    ! ----------------
    ! Local Parameters
    ! ----------------

    REAL( fp_kind ), PARAMETER :: BS_K = 3.57e-5_fp_kind
    REAL( fp_kind ), PARAMETER :: BF_K = 1.13e-6_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( fp_kind ) :: c
    REAL( fp_kind ) :: Bs
    REAL( fp_kind ) :: Bf



    !#--------------------------------------------------------------------------#
    !#                       -- CALCULATE THE CONTINUUM --                      #
    !#                                                                          #
    !# The absorption term of the water vapour continuum is given as,           #
    !#                                                                          #
    !#   Nc = f.( bs.e + bf.p ).10^-5 . e . theta^3  .....(14)                  #
    !#                                                                          #
    !# where f     = frequency in gigahertz (GHz)                               #
    !#       e     = water vapour partial pressure in kiloPascals (kPa)         #
    !#       p     = dry air partial pressure in kiloPascals (kPa)              #
    !#       theta = relative inverse temperature                               #
    !#             = 300/T                                                      #
    !#       T     = temperature in Kelvin                                      #
    !#       bs    = self-broadening coefficient,                               #
    !#             = 3.57.theta^7.5                                             #
    !#       bf    = foreign-broadening coefficient,                            #
    !#             = 0.113                                                      #
    !#                                                                          #
    !#--------------------------------------------------------------------------#

    c = f * pw * ( Theta**THREE )

    Bs = BS_K * (Theta**SEVENpointFIVE) * pw
    Bf = BF_K * pd

    Wet_Continuum = c * ( Bs + Bf ) 

  END FUNCTION Compute_Wet_Continuum


END MODULE MWLBL_Liebe89


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: MWLBL_Liebe89.f90,v 2.2 2005/01/25 21:34:18 paulv Exp $
!
! $Date: 2005/01/25 21:34:18 $
!
! $Revision: 2.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MWLBL_Liebe89.f90,v $
! Revision 2.2  2005/01/25 21:34:18  paulv
! - Removed type declarations of unused variables.
!
! Revision 2.1  2004/12/16 18:56:58  paulv
! - Removed intrinsic list.
!
! Revision 2.0  2004/05/27 22:44:27  paulv
! - New version with different interface. Now each computed transmittance
!   component is returned separately as required depending on what arguments
!   are present.
!
! Revision 1.6  2002/09/18 16:41:23  paulv
! - Updated documentation. Explanations from the Liebe89 paper have been
!   added.
! - Added QUIET and RCS_ID optional arguments to the Liebe89 function.
! - Completed update of H2O absorption code.
!
! Revision 1.5  2002/09/13 20:28:14  paulv
! - Overloaded Liebe89 function to pass arrays by layer or by frequency.
!   This code is untested.
! - Rearranged ecode for dry contiuuum calculation. Tested new code and it
!   agrees with the old code.
! - Documented code.
!
! Revision 1.4  2002/08/29 17:50:54  paulv
! - Removed the INCLUDE_DRY_GAS optional argument and replaced it with the
!   EXCLUDE_WET and EXCLUDE_DRY optional arguments in the Liebe93
!   function. Using the EXCLUDE_WET and EXCLUDE_DRY arguments, the user
!   can select what absorption components are retained in the final
!   calculation.
!
! Revision 1.3  2002/05/14 23:30:18  paulv
! - Moved coefficients into their own module.
! - Working version.
!
! Revision 1.1  2002/04/14 14:05:58  paulv
! Initial checkin.
!
!
!
