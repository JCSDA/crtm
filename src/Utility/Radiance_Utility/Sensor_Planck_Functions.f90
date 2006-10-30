!------------------------------------------------------------------------------
!M+
! NAME:
!       Sensor_Planck_Functions
!
! PURPOSE:
!       Module containing Planck function radiance, temperature, dB/dT, and 
!       dT/dB routines for use in computing sensor channel values.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Sensor_Planck_Functions
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds
!                            of variable types.
!
!       Error_Handler:       Module to define simple error codes and
!                            handle error conditions
!                            USEs: FILE_UTILITY module
!
!       SpcCoeff_Define:     Module defining the SpcCoeff data structure
!                            and containing routines to manipulate it.
!                            USEs: TYPE_KINDS module
!                                  ERROR_HANDLER module
!                                  COMPARE_FLOAT_NUMBERS module
!
!       Planck_Functions:    Module containing monochromatic Planck
!                            function radiance, temperature, dB/dT, and
!                            dT/dB routines.
!                            USEs: TYPE_KINDS module
!                                  FUNDAMENTAL_CONSTANTS module
!                                  ERROR_HANDLER module
!
! CONTAINS:
!       Sensor_Radiance:     Function to calculate the Planck radiance
!                            for a sensor channel.
!
!       Sensor_Temperature:  Function to calculate the Planck temperature
!                            for a sensor channel.
!
!       Sensor_dBdT:         Function to calculate the derivative of
!                            the Planck radiance with respect to temperature
!                            for a sensor channel.
!
!       Sensor_dTdB:         Function to calculate the Planck temperature
!                            derivative with respect to radiance for a
!                            sensor channel.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 07-May-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001, 2003 Paul van Delst
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


MODULE Sensor_Planck_Functions


  ! ------------
  ! Modules used
  ! ------------

  USE Type_Kinds
  USE Error_Handler

  USE SpcCoeff_Define

  USE Planck_Functions


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------------------
  ! Expose/hide members explicitly
  ! ------------------------------

  PRIVATE
  PUBLIC :: Sensor_Radiance
  PUBLIC :: Sensor_Temperature
  PUBLIC :: Sensor_dBdT
  PUBLIC :: Sensor_dTdB


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id field
  CHARACTER( * ),  PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Sensor_Planck_Functions.f90,v 2.2 2004/09/08 23:32:54 paulv Exp $'

  ! -- Keyword set value
  INTEGER,         PRIVATE, PARAMETER :: SET = 1

  ! -- Numeric literals
  REAL( fp_kind ), PRIVATE, PARAMETER :: TEN_THOUSAND = 10000.0_fp_kind

  ! -- Floating point precision
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( TEN_THOUSAND )


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Sensor_Radiance
!
! PURPOSE:
!       Function to calculate the Planck radiance for a sensor channel.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_Radiance( Sensor_SpcCoeff,                     &  ! Input
!                                       Sensor_Channel,                      &  ! Input
!                                       Temperature,                         &  ! Input
!                                       Radiance,                            &  ! Output
!                                       Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                       Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the radiance is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Temperature for which the sensor Planck radiance is
!                         required.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Radiance:         The Planck radiance for the requested sensor channel
!                         at the specified temperature.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error was found with the input.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
!      Planck_Radiance:    Function to calculate monochromatic Planck
!                          radiances.
!                          SOURCE: PLANCK_FUNCTIONS module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       A polychromatic correction is applied to the input temperature, T,
!
!         Tc = b0 + ( b1 * T )
!
!       where b0 and b1 are sensor channel dependent constants.
!
!       The corrected temperature, Tc, is then used in the monochromatic
!       Planck radiance calculation.
!S-
!------------------------------------------------------------------------------

  FUNCTION Sensor_Radiance( Sensor_SpcCoeff,  &  ! Input
                            Sensor_Channel,   &  ! Input
                            Temperature,      &  ! Input
                            Radiance,         &  ! Output
                            Wavelength_Units, &  ! Optional input
                            Message_Log )     &  ! Error messaging
                          RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_type ),    INTENT( IN )  :: Sensor_SpcCoeff
    INTEGER,                  INTENT( IN )  :: Sensor_Channel
    REAL( fp_kind ),          INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: Radiance

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Sensor_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Frequency_Units

    INTEGER, DIMENSION( 1 ) :: Idx
    INTEGER :: l

    REAL( fp_kind ) :: x
    REAL( fp_kind ) :: Effective_Temperature



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Only one sensor in SpcCoeff structure allowed
    IF ( Sensor_SpcCoeff%n_Sensors /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not have '//&
                            'data for 1 (and only 1) sensor.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    IF ( .NOT. ( ANY( Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF
 
    ! -- Temperature input
    IF ( Temperature < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Temperature argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Default units are in terms of frequency....
    Frequency_Units = .TRUE.

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CALCULATE RADIANCE --                         #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    Idx = MINLOC( ABS( Sensor_SpcCoeff%Sensor_Channel - Sensor_Channel ) )
    l   = Idx(1)


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( Frequency_Units ) THEN

      ! -- Frequency in cm^-1
      x = Sensor_SpcCoeff%Wavenumber(l)

    ELSE

      ! -- Wavelength in microns
      x = TEN_THOUSAND / Sensor_SpcCoeff%Wavenumber(l)

    END IF


    ! -----------------------------------
    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------

    Effective_Temperature = Sensor_SpcCoeff%Band_C1(l) + ( Sensor_SpcCoeff%Band_C2(l) * Temperature )


    ! -------------------------------------------
    ! Calculate the monochromatic Planck radiance
    ! with the corrected temperature
    ! -------------------------------------------

    Error_Status = Planck_Radiance( x,                     &
                                    Effective_Temperature, &
                                    Radiance,              &
                                    Wavelength_Units = Wavelength_Units, &
                                    Message_Log      = Message_Log       )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Call to Planck_Radiance failed for channel ", i4 )' ) &
                      Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

  END FUNCTION Sensor_Radiance 





!------------------------------------------------------------------------------
!S+
! NAME:
!       Sensor_Temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature for a sensor channel.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_Temperature( Sensor_SpcCoeff,                     &  ! Input  
!                                          Sensor_Channel,                      &  ! Input  
!                                          Radiance,                            &  ! Input  
!                                          Temperature,                         &  ! Output 
!                                          Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                          Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the temperature is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Radiance:         The Planck radiance of the sensor channel for
!                         which the brightness temperature is required.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Temperature:      Brightness temperature of the required sensor channel
!                         for the given radiance.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error was found with the input.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
!      Planck_Temperature: Function to calculate monochromatic Planck
!                          temperature.
!                          SOURCE: PLANCK_FUNCTIONS module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The Planck temperature is calculated using the input polychromatic
!       radiance assuming monochromaticity. This result yields a "polychromatic
!       temperature", Tc, which is then corrected for polychromaticity to yield
!       the returned temperature T,
!
!         T = ( Tc - b0 ) / b1
!
!       where b0 and b1 are sensor channel dependent constants.
!S-
!------------------------------------------------------------------------------

  FUNCTION Sensor_Temperature( Sensor_SpcCoeff,  &  ! Input
                               Sensor_Channel,   &  ! Input
                               Radiance,         &  ! Input
                               Temperature,      &  ! Output
                               Wavelength_Units, &  ! Optional input
                               Message_Log )     &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_type ),    INTENT( IN )  :: Sensor_SpcCoeff
    INTEGER,                  INTENT( IN )  :: Sensor_Channel
    REAL( fp_kind ),          INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: Temperature

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Funciton result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Sensor_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Frequency_Units

    INTEGER, DIMENSION( 1 ) :: Idx
    INTEGER :: l

    REAL( fp_kind ) :: x
    REAL( fp_kind ) :: Effective_Temperature



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Only one sensor in SpcCoeff structure allowed
    IF ( Sensor_SpcCoeff%n_Sensors /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not have '//&
                            'data for 1 (and only 1) sensor.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    IF ( .NOT. ( ANY( Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF
 
    ! -- Radiance input
    IF ( Radiance < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Radiance argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Default units are in terms of frequency....
    Frequency_Units = .TRUE.

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE TEMPERATURE --                       #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    Idx = MINLOC( ABS( Sensor_SpcCoeff%Sensor_Channel - Sensor_Channel ) )
    l   = Idx(1)


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( Frequency_Units ) THEN

      ! -- Frequency in cm^-1
      x = Sensor_SpcCoeff%Wavenumber(l)

    ELSE

      ! -- Wavelength in microns
      x = TEN_THOUSAND / Sensor_SpcCoeff%Wavenumber(l)

    END IF


    ! -----------------------------------------
    ! Calculate the brightness temperature with
    ! the monochromatic Planck function
    ! -----------------------------------------

    Error_Status = Planck_Temperature( x,                     &
                                       Radiance,              &
                                       Effective_Temperature, &
                                       Wavelength_Units = Wavelength_Units, &
                                       Message_Log      = Message_Log       )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Call to Planck_Temperature failed for channel ", i4 )' ) &
                      Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF


    ! ----------------------------------
    ! Correct the brightness temperature
    ! for polychromaticity
    ! ----------------------------------

    Temperature = ( Effective_Temperature - Sensor_SpcCoeff%Band_C1(l) ) / &
    !             ------------------------------------------------------
                                 Sensor_SpcCoeff%Band_C2(l)


  END FUNCTION Sensor_Temperature





!------------------------------------------------------------------------------
!S+
! NAME:
!       Sensor_dBdT
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature for a sensor channel.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_dBdT( Sensor_SpcCoeff,                     &  ! Input
!                                   Sensor_Channel,                      &  ! Input
!                                   Temperature,                         &  ! Input
!                                   dBdT,                                &  ! Output
!                                   Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                   Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the dB/dT derivative is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Temperature:      Temperature for which the sensor dB/dT is
!                         required.
!                         UNITS:      Kelvin (K)
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, dBdT units are mW/(m2.sr.cm-1.K)  *DEFAULT*
!                            == 1, dBdT units are W/(m2.sr.um.K)
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dBdT:             The derivative of the Planck radiance with respect
!                         to temperature at the supplied temperature for the
!                         requested sensor channel.
!                         UNITS:      mW/(m2.sr.cm-1.K)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.um.K)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error was found with the input.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
!      Planck_dBdT:        Function to calculate monochromatic Planck
!                          radiance derivative.
!                          SOURCE: PLANCK_FUNCTIONS module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       A polychromatic correction is applied to the input temperature, T,
!
!         Tc = b0 + ( b1 * T )
!
!       where b0 and b1 are sensor channel dependent constants.
!
!       The corrected temperature, Tc, is then used in the monochromatic
!       Planck radiance derivative calculation.
!S-
!------------------------------------------------------------------------------

  FUNCTION Sensor_dBdT( Sensor_SpcCoeff,  &  ! Input
                        Sensor_Channel,   &  ! Input
                        Temperature,      &  ! Input
                        dBdT,             &  ! Output
                        Wavelength_Units, &  ! Optional input
                        Message_Log )     &  ! Error messaging
                      RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_type ),    INTENT( IN )  :: Sensor_SpcCoeff
    INTEGER,                  INTENT( IN )  :: Sensor_Channel
    REAL( fp_kind ),          INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: dBdT

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Sensor_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Frequency_Units

    INTEGER, DIMENSION( 1 ) :: Idx
    INTEGER :: l

    REAL( fp_kind ) :: x
    REAL( fp_kind ) :: Effective_Temperature



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Only one sensor in SpcCoeff structure allowed
    IF ( Sensor_SpcCoeff%n_Sensors /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not have '//&
                            'data for 1 (and only 1) sensor.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    IF ( .NOT. ( ANY( Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF
 
    ! -- Temperature input
    IF ( Temperature < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Temperature argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Default units are in terms of frequency....
    Frequency_Units = .TRUE.

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CALCULATE RADIANCE --                         #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    Idx = MINLOC( ABS( Sensor_SpcCoeff%Sensor_Channel - Sensor_Channel ) )
    l   = Idx(1)


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( Frequency_Units ) THEN

      ! -- Frequency in cm^-1
      x = Sensor_SpcCoeff%Wavenumber(l)

    ELSE

      ! -- Wavelength in microns
      x = TEN_THOUSAND / Sensor_SpcCoeff%Wavenumber(l)

    END IF


    ! -----------------------------------
    ! Calculate the temperature corrected
    ! for polychromaticity
    ! -----------------------------------

    Effective_Temperature = Sensor_SpcCoeff%Band_C1(l) + ( Sensor_SpcCoeff%Band_C2(l) * Temperature )


    ! ----------------------------------------
    ! Calculate the monochromatic Planck dB/dT
    ! with the corrected temperature
    ! ----------------------------------------

    Error_Status = Planck_dBdT( x,                     &
                                Effective_Temperature, &
                                dBdT,                  &
                                Wavelength_Units = Wavelength_Units, &
                                Message_Log      = Message_Log       )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Call to Planck_dBdT failed for channel ", i4 )' ) &
                      Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

  END FUNCTION Sensor_dBdT





!------------------------------------------------------------------------------
!S+
! NAME:
!       Sensor_dTdB
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance for a sensor channel.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Sensor_dTdB( Sensor_SpcCoeff,                     &  ! Input
!                                   Sensor_Channel,                      &  ! Input
!                                   Radiance,                            &  ! Input
!                                   dTdB,                                &  ! Output
!                                   Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                   Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Sensor_SpcCoeff:  The SpcCoeff data structure for the required sensor.
!                         UNITS:      N/A
!                         TYPE:       SpcCoeff_type
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Sensor_Channel:   Channel number of the specified sensor for which
!                         the temperature is required.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!       Radiance:         The Planck radiance of the sensor channel for
!                         which the dT/dB derivate is required.
!                         UNITS:      mW/(m2.sr.cm-1)  *DEFAULT*
!                                       or
!                                     W/(m2.sr.micron)
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units: Set this optional argument to specify the radiance
!                         units in terms of wavelength rather than frequency
!                         (the default).
!                         If == 0, Radiance units are mW/(m2.sr.cm-1)  *DEFAULT*
!                                  dT/dB units are (K.m2.sr.cm-1)/mW   *DEFAULT*
!                            == 1, Radiance units are W/(m2.sr.micron)
!                                  dT/dB units are (K.m2.sr.um)/W
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which any
!                         messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output messages to standard output.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dTdB:             The derivative of Planck temperature with respect
!                         to radiance of the required sensor channel for the
!                         given radiance.
!                         UNITS:      (K.m2.sr.cm-1)/mW  *DEFAULT*
!                                         or
!                                     (K.m2.sr.um)/W
!                                     See WAVELENGTH_UNITS optional argument.
!                         TYPE:       REAL( fp_kind )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the ERROR_HANDLER module.
!                          If == SUCCESS the Planck calculation was successful
!                             == FAILURE an error was found with the input.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
!      Planck_dTdB:        Function to calculate monochromatic Planck
!                          temperature derivative.
!                          SOURCE: PLANCK_FUNCTIONS module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The Planck temperature derivative is calculated using the input
!       polychromatic radiance assuming monochromaticity. This result yields a
!       polychromatic derivative, (dTc/dB), which is then corrected for
!       polychromaticity to yield the returned derivative, dT/dB,
!
!         dT/dB = ( dTc/dB ) / b1
!
!       where b1 is a sensor channel dependent constant.
!S-
!------------------------------------------------------------------------------

  FUNCTION Sensor_dTdB( Sensor_SpcCoeff,  &  ! Input
                        Sensor_Channel,   &  ! Input
                        Radiance,         &  ! Input
                        dTdB,             &  ! Output
                        Wavelength_Units, &  ! Optional input
                        Message_Log )     &  ! Error messaging
                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE( SpcCoeff_type ),    INTENT( IN )  :: Sensor_SpcCoeff
    INTEGER,                  INTENT( IN )  :: Sensor_Channel
    REAL( fp_kind ),          INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: dTdB

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error handler message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Funciton result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Sensor_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Frequency_Units

    INTEGER, DIMENSION( 1 ) :: Idx
    INTEGER :: l

    REAL( fp_kind ) :: x
    REAL( fp_kind ) :: Effective_dTdB



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Only one sensor in SpcCoeff structure allowed
    IF ( Sensor_SpcCoeff%n_Sensors /= 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not have '//&
                            'data for 1 (and only 1) sensor.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Sensor channel input
    IF ( .NOT. ( ANY( Sensor_SpcCoeff%Sensor_Channel == Sensor_Channel ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Sensor_SpcCoeff structure does not contain '//&
                            'the requested Sensor_Channel.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF
 
    ! -- Radiance input
    IF ( Radiance < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid Radiance argument', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Default units are in terms of frequency....
    Frequency_Units = .TRUE.

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) Frequency_Units = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE TEMPERATURE --                       #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Get the required channel index
    ! ------------------------------

    Idx = MINLOC( ABS( Sensor_SpcCoeff%Sensor_Channel - Sensor_Channel ) )
    l   = Idx(1)


    ! ---------------------------------
    ! Convert the frequency if required
    ! ---------------------------------

    IF ( Frequency_Units ) THEN

      ! -- Frequency in cm^-1
      x = Sensor_SpcCoeff%Wavenumber(l)

    ELSE

      ! -- Wavelength in microns
      x = TEN_THOUSAND / Sensor_SpcCoeff%Wavenumber(l)

    END IF


    ! -----------------------------------
    ! Calculate the dT/dB derivative with
    ! the monochromatic Planck function
    ! -----------------------------------

    Error_Status = Planck_dTdB( x,              &
                                Radiance,       &
                                Effective_dTdB, &
                                Wavelength_Units = Wavelength_Units, &
                                Message_Log      = Message_Log       )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Call to Planck_dTdB failed for channel ", i4 )' ) &
                      Sensor_Channel
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF


    ! ----------------------
    ! Correct the derivative
    ! for polychromaticity
    ! ----------------------

    dTdB = Effective_dTdB / Sensor_SpcCoeff%Band_C2(l)

  END FUNCTION Sensor_dTdB

END MODULE Sensor_Planck_Functions


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Sensor_Planck_Functions.f90,v 2.2 2004/09/08 23:32:54 paulv Exp $
!
! $Date: 2004/09/08 23:32:54 $
!
! $Revision: 2.2 $
!
! $State: Exp $
!
! $Log: Sensor_Planck_Functions.f90,v $
! Revision 2.2  2004/09/08 23:32:54  paulv
! - Update for new Utility modules.
!
! Revision 2.1  2003/11/13 14:57:34  paulv
! - Cosmetic changes to documentation only.
!
! Revision 2.0  2003/10/24 18:50:17  paulv
! - New version of Sensor Planck Function module that uses SpcCoeff data structures
!   for sensor information rather than numbers from DATA statements.
!
! Revision 1.3  2001/05/09 18:19:58  paulv
! - Updated header documentation.
!
! Revision 1.2  2001/05/09 17:37:50  paulv
! - Added SENSOR_CONVERT_RADIANCE() function.
! - Removed definition of WAVENUMBER_UNIT and WAVELENGTH_UNIT. Values are
!   inherited from the PLANCK_FUNCTIONS module and made PUBLIC.
! - Corrected bug with channel list assignment in SENSOR_CHANNELS() function.
!   List assignment was by:
!     channels = channel( l1:l2 )
!   where CHANNELS is an assumed shape dummy argument array that may not
!   correspond to the same length as l2-l1+1. This was changed to:
!     channels( 1:n_channels ) = channel( l1:l2 )
!   where N_CHANNELS is previously assigned as the maximum number of channels
!   that can be returned.
! - All functions except SENSOR_CHANNELS now return a WARNING status if any
!   input radiances or temperatures are less than numberical precision
!   defined using the EPSILON() instrinsic. If this occurs, output variables
!   are set to 0.0.
!   Output messages corresponding to this scenario were made more descriptive.
! - All references to CHANNEL( l ) in output error messages were not needed
!   and replaced with SENSOR_CHANNEL.
!
! Revision 1.1  2001/05/08 21:38:08  paulv
! Initial checkin
!
!
!
