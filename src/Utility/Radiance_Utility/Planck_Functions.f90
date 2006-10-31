!------------------------------------------------------------------------------
!M+
! NAME:
!       Planck_Functions
!
! PURPOSE:
!       Module containing Planck function radiance, temperature, dB/dT, and 
!       dT/dB routines.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Planck_Functions
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       Fundamental_Constants:  Module containing definitions of various
!                               fundamental physical constants.
!                               USEs: TYPE_KINDS module
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
! CONTAINS:
!       Planck_Radiance:        Function to calculate the Planck radiance
!                               given the spectral ordinate (frequency or
!                               wavelength) and temperature.
!
!       Planck_Temperature:     Function to calculate the Planck temperature
!                               given the spectral ordinate (frequency or
!                               wavelength) and radiance.
!
!       Planck_dBdT:            Function to calculate the derivative of
!                               the Planck radiance with respect to temperature
!                               given the spectral ordinate (frequency or
!                               wavelength) and temperature.
!
!       Planck_dTdB:            Function to calculate the Planck temperature
!                               derivative with respect to radiance given the
!                               spectral ordinate (frequency or wavelength)
!                               and radiance.
!
! INCLUDE FILES:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 14-Oct-1999
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 1999, 2001, 2003, 2004 Paul van Delst
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

MODULE Planck_Functions


  ! ------------
  ! Modules used
  ! ------------
 
  USE Type_Kinds, ONLY: fp_kind
  USE Fundamental_Constants, ONLY: C_1, C_2
  USE Message_Handler


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------------------
  ! Expose/hide members explicitly
  ! ------------------------------

  PRIVATE
  PUBLIC :: Planck_Radiance
  PUBLIC :: Planck_Temperature
  PUBLIC :: Planck_dBdT
  PUBLIC :: Planck_dTdB


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE Planck_Radiance
    MODULE PROCEDURE scalar_Planck_Radiance       ! Main function
    MODULE PROCEDURE R1_x_S_t_Planck_Radiance     ! I/P Rank1  x,  Scalar Temperature
    MODULE PROCEDURE S_x_R1_t_Planck_Radiance     ! I/P Scalar x,  Rank1  Temperature
    MODULE PROCEDURE R1_xt_Planck_Radiance        ! I/P Rank1  x,  Rank1  Temperature
    MODULE PROCEDURE R1_x_R2_t_Planck_Radiance    ! I/P Rank1  x,  Rank2  Temperature
  END INTERFACE Planck_Radiance

  INTERFACE Planck_Temperature
    MODULE PROCEDURE scalar_Planck_Temperature    ! Main function
    MODULE PROCEDURE R1_x_S_r_Planck_Temperature  ! I/P Rank1  x,  Scalar Radiance
    MODULE PROCEDURE S_x_R1_r_Planck_Temperature  ! I/P Scalar x,  Rank1  Radiance
    MODULE PROCEDURE R1_xr_Planck_Temperature     ! I/P Rank1  x,  Rank1  Radiance
    MODULE PROCEDURE R1_x_R2_r_Planck_Temperature ! I/P Rank1  x,  Rank2  Radiance
  END INTERFACE Planck_Temperature

  INTERFACE Planck_dBdT
    MODULE PROCEDURE scalar_Planck_dBdT           ! Main function
    MODULE PROCEDURE R1_x_S_t_Planck_dBdT         ! I/P Rank1  x,  Scalar Temperature
    MODULE PROCEDURE S_x_R1_t_Planck_dBdT         ! I/P Scalar x,  Rank1  Temperature
    MODULE PROCEDURE R1_xt_Planck_dBdT            ! I/P Rank1  x,  Rank1  Temperature
    MODULE PROCEDURE R1_x_R2_t_Planck_dBdT        ! I/P Rank1  x,  Rank2  Temperature
  END INTERFACE Planck_dBdT

  INTERFACE Planck_dTdB
    MODULE PROCEDURE scalar_Planck_dTdB           ! Main function
    MODULE PROCEDURE R1_x_S_r_Planck_dTdB         ! I/P Rank1  x,  Scalar Radiance
    MODULE PROCEDURE S_x_R1_r_Planck_dTdB         ! I/P Scalar x,  Rank1  Radiance
    MODULE PROCEDURE R1_xr_Planck_dTdB            ! I/P Rank1  x,  Rank1  Radiance
    MODULE PROCEDURE R1_x_R2_r_Planck_dTdB        ! I/P Rank1  x,  Rank2  Radiance
  END INTERFACE Planck_dTdB


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id field
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: Planck_Functions.f90,v 3.2 2006/09/21 17:57:31 wd20pd Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- Numeric literals
  REAL( fp_kind ), PRIVATE, PARAMETER :: ONE = 1.0_fp_kind

  ! -- Floating point precision
  REAL( fp_kind ), PRIVATE, PARAMETER :: TOLERANCE = EPSILON( ONE )

  ! -- Unit types
  INTEGER, PRIVATE, PARAMETER :: N_UNIT_TYPES = 2
  INTEGER, PRIVATE, PARAMETER ::  FREQUENCY_INDEX = 1
  INTEGER, PRIVATE, PARAMETER :: WAVELENGTH_INDEX = 2

  ! -- Scale factors. One for each unit type.
  REAL( fp_kind ), PRIVATE, PARAMETER, &
                   DIMENSION( N_UNIT_TYPES ) :: RADIANCE_SCALE_FACTOR = &
                                                  (/ 1000.0_fp_kind, &
                                                        1.0_fp_kind /) 
  REAL( fp_kind ), PRIVATE, PARAMETER, &
                   DIMENSION( N_UNIT_TYPES ) :: C_1_SCALE_FACTOR = &
                                                  (/ 1.0e+08_fp_kind, &
                                                     1.0e+24_fp_kind /)
  REAL( fp_kind ), PRIVATE, PARAMETER, &
                   DIMENSION( N_UNIT_TYPES ) :: C_2_SCALE_FACTOR = &
                                                  (/   100.0_fp_kind, &
                                                     1.0e+06_fp_kind /)


CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Planck_Radiance
!
! PURPOSE:
!       Function to calculate the Planck Radiance given the spectral ordinate
!       (frequency or wavelength) and temperature.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Planck_Radiance( x,                                   & ! Input
!                                       Temperature,                         & ! Input
!                                       Radiance,                            & ! Output
!                                       Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                       Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar or Rank-1
!                                       See output radiance dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
!       Temperature:        Temperature(s) for which the Planck Radiance(s)
!                           is(are) required. Can be a SCALAR or VECTOR.
!                           See Radiance output description for allowed
!                           dimensionality.
!                           UNITS:      Kelvin
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output radiance dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                  } DEFAULT
!                                    Ouptut Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                              == 1, Input x units are microns
!                                    Ouptut Radiance units are W/(m2.sr.micron)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Radiance:           The Planck Radiance(s) for the supplied Temperature(s).
!                           The output dimensions are determined by the input.
!                           In the following chart N == frequencies, K == temperatures or
!                           spectra (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Temperature    Output Radiance
!                              dimension       dimension            dimension
!                           ------------------------------------------------------
!                               scalar          scalar                scalar
!                                 N             scalar                  N
!                               scalar            K                     K
!                                 N               N                     N
!                                 N               K                not allowed
!                                 N             N x K                 N x K
!
!                           UNITS:      mW/(m2.sr.cm-1) 
!                                         or
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:     Subroutine to output messages
!                           SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For frequency input, the Planck Radiance is calculated using:
!
!                   c1 * frequency^3
!         B =  ---------------------------
!                  ( c2 * frequency )
!               EXP( -------------- ) - 1
!                  (        T       )
!
!       For wavelength input:
!
!                                    c1
!         B = --------------------------------------------------
!                             [    (        c2      )     ]
!              wavelength^5 * [ EXP( -------------- ) - 1 ]
!                             [    ( wavelength * T )     ]
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar                scalar                #
  !############################################################################

  FUNCTION scalar_Planck_Radiance( x,                &  ! Input
                                   Temperature,      &  ! Input
                                   Radiance,         &  ! Output
                                   Wavelength_Units, &  ! Optional input
                                   Message_Log     ) &  ! Error messaging
                                 RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------
 
    ! -- Input
    REAL( fp_kind ),          INTENT( IN )  :: x
    REAL( fp_kind ),          INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: Radiance
 
    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index

    REAL( fp_kind ) :: Frequency, Wavelength
    REAL( fp_kind ) :: x_c_1, x_c_2
    REAL( fp_kind ) :: Exponential



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid X (Frequency/wavelength) argument', &
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
    Unit_Index      = FREQUENCY_INDEX

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF


 
    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE SPECTRAL PARAMETERS --                  #
    !#--------------------------------------------------------------------------#

    IF ( Frequency_Units ) THEN


      ! -----------------------
      ! Spectral Units in cm^-1
      ! -----------------------

      Frequency = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency


    ELSE


      ! -------------------------
      ! Spectral Units in microns
      ! -------------------------

      Wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength

    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CALCULATE RADIANCE --                         #
    !#--------------------------------------------------------------------------#

    Exponential = EXP( x_c_2 / Temperature )
    Radiance    = RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 / ( Exponential - ONE )

  END FUNCTION scalar_Planck_Radiance





  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                 N            scalar                  N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_t_Planck_Radiance( x,                &  ! Input
                                     Temperature,      &  ! Input
                                     Radiance,         &  ! Output
                                     Wavelength_Units, &  ! Optional input
                                     Message_Log     ) &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------
 
    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ),                 INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Radiance
 
    ! -- Optional input
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( x )
    IF ( SIZE( Radiance ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X input and Radiance output.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_Radiance( x(i),        &  ! Input
                                             Temperature, &  ! Input
                                             Radiance(i), &  ! Output
                                             Wavelength_Units = Wavelength_Units, &  ! Optional input
                                             Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_x_S_t_Planck_Radiance





  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar            K                     K                   #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_t_Planck_Radiance( x,                &  ! Input
                                     Temperature,      &  ! Input
                                     Radiance,         &  ! Output
                                     Wavelength_Units, &  ! Optional input
                                     Message_Log     ) &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------
 
    ! -- Input
    REAL( fp_kind ),                 INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Radiance
 
    ! -- Optional input
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    k = SIZE( Temperature )
    IF ( SIZE( Radiance ) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature input and Radiance output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, k

      Error_Status = scalar_Planck_Radiance( x,              &  ! Input
                                             Temperature(i), &  ! Input
                                             Radiance(i),    &  ! Output
                                             Wavelength_Units = Wavelength_Units, &  ! Optional input
                                             Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION S_x_R1_t_Planck_Radiance





  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N               N                     N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xt_Planck_Radiance( x,                &  ! Input
                                  Temperature,      &  ! Input
                                  Radiance,         &  ! Output
                                  Wavelength_Units, &  ! Optional input
                                  Message_Log     ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------
 
    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Radiance
 
    ! -- Optional input
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log



    ! ------
    ! Result
    ! ------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( Temperature ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and Temperature inputs', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( Radiance ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Radiance output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_Radiance( x(i),           &  ! Input
                                             Temperature(i), &  ! Input
                                             Radiance(i),    &  ! Output
                                             Wavelength_Units = Wavelength_Units, &  ! Optional input
                                             Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_xt_Planck_Radiance





  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N             N x K                 N x K                 #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_t_Planck_Radiance( x,                &  ! Input
                                      Temperature,      &  ! Input
                                      Radiance,         &  ! Output
                                      Wavelength_Units, &  ! Optional input
                                      Message_Log     ) &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Radiance
 
    ! -- Optional input
    INTEGER,        OPTIONAL,           INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,           INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same size
    n = SIZE( Temperature, DIM = 1 )
    k = SIZE( Temperature, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and Temperature[NxK] inputs.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect shape
    IF ( SIZE( Radiance, DIM = 1 ) /= n .AND. &
         SIZE( Radiance, DIM = 2 ) /= k       ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of input Temperature and output Radiance.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    Spectrum_loop: DO j = 1, k

      Frequency_loop: DO i = 1, n

        Error_Status = scalar_Planck_Radiance( x(i),             &  ! Input
                                               Temperature(i,j), &  ! Input
                                               Radiance(i,j),    &  ! Output
                                               Wavelength_Units = Wavelength_Units, &  ! Optional input
                                               Message_Log      = Message_Log       )  ! Error messaging
        IF ( Error_Status /= SUCCESS ) EXIT Spectrum_loop

      END DO Frequency_loop

    END DO Spectrum_loop

  END FUNCTION R1_x_R2_t_Planck_Radiance




!------------------------------------------------------------------------------
!S+
! NAME:
!       Planck_Temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature given the spectral
!       ordinate (frequency or wavelength), and radiance.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Planck_Temperature( x,                                   & ! Input
!                                          Radiance,                            & ! Input
!                                          Temperature,                         & ! Output
!                                          Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                          Message_Log      = Message_Log       )  ! Error messaging
!  
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar or Rank-1
!                           ATTRIBUTES: INTENT( IN )
!
!       Radiance:           Planck radiance(s) for which temperature(s) are
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output Temperature dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                 } DEFAULT
!                                    Input Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                              == 1, Input x units are microns
!                                    Input Radiance units are W/(m2.sr.micron)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Temperature:        The brightness temperature(s) corresponding to the
!                           supplied radiance(s). The output dimensions are
!                           determined by the input. In the following chart
!                           N == frequencies, K == temperatures or spectra
!                           (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Radiance     Output Temperature
!                              dimension       dimension           dimension
!                           ------------------------------------------------------
!                               scalar          scalar               scalar
!                                 N             scalar                 N
!                               scalar            K                    K
!                                 N               N                    N
!                                 N               K               not allowed
!                                 N             N x K                N x K
!
!                           UNITS:      Kelvin
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For frequency input, the Planck temperature is calculated using:
!
!                    c2 * frequency
!         T = -----------------------------
!                ( c1 * frequency^3      )
!              LN( ---------------- +  1 )
!                (          B            )
!
!       For wavelength input :
!
!                                c2
!          T = -----------------------------------------
!                              (        c1            )
!               wavelength * LN( ---------------- + 1 )
!                              ( wavelength^5 * B     )
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar               scalar                 #
  !############################################################################

  FUNCTION scalar_Planck_Temperature( x,                &  ! Input
                                      Radiance,         &  ! Input
                                      Temperature,      &  ! output
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
    REAL( fp_kind ),           INTENT( IN )  :: x
    REAL( fp_kind ),           INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ),           INTENT( OUT ) :: Temperature
 
    ! -- Optional input
    INTEGER,        OPTIONAL,  INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index

    REAL( fp_kind ) :: Frequency, Wavelength
    REAL( fp_kind ) :: x_c_1, x_c_2
    REAL( fp_kind ) :: Logarithm




    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid X (Frequency/wavelength) argument', &
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
    Unit_Index      = FREQUENCY_INDEX

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

 
 
    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE SPECTRAL PARAMETERS --                  #
    !#--------------------------------------------------------------------------#

    IF ( Frequency_Units ) THEN


      ! -----------------------
      ! Spectral Units in cm^-1
      ! -----------------------

      Frequency = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency

    ELSE


      ! -------------------------
      ! Spectral Units in microns
      ! -------------------------

      Wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength

    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- CALCULATE TEMPERATURE --                       #
    !#--------------------------------------------------------------------------#

    Logarithm   = LOG( ( RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 / Radiance ) + ONE )
    Temperature = x_c_2 / Logarithm

  END FUNCTION scalar_Planck_Temperature





  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             scalar                 N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_r_Planck_Temperature( x,                &  ! Input
                                        Radiance,         &  ! Input
                                        Temperature,      &  ! output
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
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ),                 INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Temperature
 
    ! -- Optional input
    INTEGER,        OPTIONAL,        INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,        INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Output array has incorrect length
    n = SIZE( x )
    IF ( SIZE( Temperature ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

 

    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_Temperature( x(i),           &  ! Input
                                                Radiance,       &  ! Input
                                                Temperature(i), &  ! Output
                                                Wavelength_Units = Wavelength_Units, & ! Optional input
                                                Message_Log = Message_Log            )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_x_S_r_Planck_Temperature





  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar            K                    K                    #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_r_Planck_Temperature( x,                &  ! Input
                                        Radiance,         &  ! Input
                                        Temperature,      &  ! output
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
    REAL( fp_kind ),                 INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Temperature
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( Radiance )
    IF ( SIZE( Temperature ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

 

    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_Temperature( x,              &  ! Input
                                                Radiance(i),    &  ! Input
                                                Temperature(i), &  ! Output
                                                Wavelength_Units = Wavelength_Units, & ! Optional input
                                                Message_Log = Message_Log            )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION S_x_R1_r_Planck_Temperature





  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N               N                    N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xr_Planck_Temperature( x,                &  ! Input
                                     Radiance,         &  ! Input
                                     Temperature,      &  ! output
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
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Temperature
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( Radiance ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and Radiance inputs', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( Temperature ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

 

    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_Temperature( x(i),           &  ! Input
                                                Radiance(i),    &  ! Input
                                                Temperature(i), &  ! Output
                                                Wavelength_Units = Wavelength_Units, & ! Optional input
                                                Message_Log = Message_Log            )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_xr_Planck_Temperature





  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             N x K                N x K                  #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_r_Planck_Temperature( x,                &  ! Input
                                         Radiance,         &  ! Input
                                         Temperature,      &  ! output
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
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: Temperature
 
    ! -- Optional input
    INTEGER,         OPTIONAL,          INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,          INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( Radiance, DIM = 1 )
    k = SIZE( Radiance, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and Radiance[NxK] inputs.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( Temperature, DIM = 1 ) /= n .AND. &
         SIZE( Temperature, DIM = 2 ) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of input Radiance and output Temperature.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    Spectrum_loop: DO j = 1, k

      Frequency_loop: DO i = 1, n

        Error_Status = scalar_Planck_Temperature( x(i),             &  ! Input
                                                  Radiance(i,j),    &  ! Input
                                                  Temperature(i,j), &  ! Output
                                                  Wavelength_Units = Wavelength_Units, & ! Optional input
                                                  Message_Log = Message_Log            )  ! Error messaging
        IF ( Error_Status /= SUCCESS ) EXIT Spectrum_loop

      END DO Frequency_loop

    END DO Spectrum_loop

  END FUNCTION R1_x_R2_r_Planck_Temperature





!------------------------------------------------------------------------------
!S+
! NAME:
!       Planck_dBdT
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature given the spectral ordinate (frequency or
!       wavelength) and temperature.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Planck_dBdT( x,                                   &  ! Input
!                                   Temperature,                         &  ! Input
!                                   dBdT,                                &  ! Output
!                                   Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                   Message_Log = Message_Log            )  ! Error messaging
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar or Rank-1
!                                       See output dBdT dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
!       Temperature:        Temperature(s) for which the dBdT(s) is(are)
!                           required.
!                           UNITS:      Kelvin
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output dBdT dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                } DEFAULT
!                                    Ouptut dBdT units are mW/(m2.sr.cm-1.K) } DEFAULT
!                              == 1, Input x units are microns
!                                    Ouptut dBdT units are W/(m2.sr.um.K)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dBdT:               The derivative(s) of Planck radiance with respect to
!                           temperature for the supplied temperature(s). The
!                           output dimensions are determined by the input. In the
!                           following chart N == frequencies, K == Temperatures or
!                           spectra (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Temperature      Output dBdT
!                              dimension       dimension            dimension
!                           ------------------------------------------------------
!                               scalar          scalar                scalar
!                                 N             scalar                  N
!                               scalar            K                     K
!                                 N               N                     N
!                                 N               K                not allowed
!                                 N             N x K                 N x K
!
!                           UNITS:      mW/(m2.sr.cm-1.K) 
!                                         or
!                                       W/(m2.sr.um.K)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For frequency input, the Planck radiance derivative with respect
!       to temperature is calculated using :
!
!                                              ( c2 * frequency )
!                   c1 * c2 * frequency^4 * EXP( -------------- )
!                                              (        T       )             
!          dB/dT = -----------------------------------------------
!                      {     [    ( c2 * frequency )     ] }^2
!                      { T * [ EXP( -------------- ) - 1 ] }
!                      {     [    (        T       )     ] }
!
!
!       For wavelength input :
!                                            (       c2       )
!                               c1 * c2 * EXP( -------------- )
!                                            ( wavelength * T )
!          dB/dT = --------------------------------------------------------
!                                  {     [    (       c2       )     ] }^2
!                   wavelength^6 * { T * [ EXP( -------------- ) - 1 ] }
!                                  {     [    ( wavelength * T )     ] }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Jul-1996
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar                scalar                #
  !############################################################################

  FUNCTION scalar_Planck_dBdT( x,                &  ! Input
                               Temperature,      &  ! Input
                               dBdT,             &  ! Output
                               Wavelength_Units, &  ! Optional input
                               Message_Log     ) &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),          INTENT( IN )  :: x
    REAL( fp_kind ),          INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ),          INTENT( OUT ) :: dBdT
 
    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index

    REAL( fp_kind ) :: Frequency, Wavelength
    REAL( fp_kind ) :: x_c_1, x_c_2
    REAL( fp_kind ) :: Exponential



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid X (Frequency/wavelength) argument', &
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
    Unit_Index      = FREQUENCY_INDEX

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF


 
    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE SPECTRAL PARAMETERS --                  #
    !#--------------------------------------------------------------------------#

    IF ( Frequency_Units ) THEN


      ! -----------------------
      ! Spectral Units in cm^-1
      ! -----------------------

      Frequency = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * &
              C_2_SCALE_FACTOR( Unit_Index ) * C_2 * ( Frequency**4 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency

    ELSE


      ! -------------------------
      ! Spectral Units in microns
      ! -------------------------

      Wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * &
              C_2_SCALE_FACTOR( Unit_Index ) * C_2 / ( Wavelength**6 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength

    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- CALCULATE dBdT --                           #
    !#--------------------------------------------------------------------------#

    Exponential = EXP( x_c_2 / Temperature )
    dBdT        = RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 * Exponential / &
                       ( Temperature * ( Exponential - ONE ) )**2

  END FUNCTION scalar_Planck_dBdT





  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                 N            scalar                  N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_t_Planck_dBdT( x,                &  ! Input
                                 Temperature,      &  ! Input
                                 dBdT,             &  ! Output
                                 Wavelength_Units, &  ! Optional input
                                 Message_Log     ) &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ),                 INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dBdT
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( x )
    IF ( SIZE( dBdT ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of dBdT output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_dBdT( x(i),        &  ! Input
                                         Temperature, &  ! Input
                                         dBdT(i),     &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_x_S_t_Planck_dBdT





  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar            K                     K                   #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_t_Planck_dBdT( x,                &  ! Input
                                 Temperature,      &  ! Input
                                 dBdT,             &  ! Output
                                 Wavelength_Units, &  ! Optional input
                                 Message_Log     ) &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ),                 INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dBdT
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    k = SIZE( Temperature )
    IF ( SIZE( dBdT ) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature input and dBdT output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF




    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#                                                                          #
    !# Believe it or not, doing it this way rather than using F90's ability to  #
    !# perform arithmetic on arrays is *faster*! At least on the systems this   #
    !# code was tested on (IBM Thinkpad 600, Sun dual-processor, and SGI        #
    !# Origin). How 'bout that?                                                 #
    !#--------------------------------------------------------------------------#

    DO i = 1, k

      Error_Status = scalar_Planck_dBdT( x,              &  ! Input
                                         Temperature(i), &  ! Input
                                         dBdT(i),        &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION S_x_R1_t_Planck_dBdT





  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N               N                     N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xt_Planck_dBdT( x,                &  ! Input
                              Temperature,      &  ! Input
                              dBdT,             &  ! Output
                              Wavelength_Units, &  ! Optional input
                              Message_Log     ) &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dBdT
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( Temperature ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and Temperature inputs', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dBdT ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of dBdT output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_dBdT( x(i),           &  ! Input
                                         Temperature(i), &  ! Input
                                         dBdT(i),        &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Error messaging
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_xt_Planck_dBdT





  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N             N x K                 N x K                 #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_t_Planck_dBdT( x,                &  ! Input
                                  Temperature,      &  ! Input
                                  dBdT,             &  ! Output
                                  Wavelength_Units, &  ! Optional input
                                  Message_Log     ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )  :: Temperature

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: dBdT
 
    ! -- Optional input
    INTEGER,         OPTIONAL,          INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,          INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( Temperature, DIM = 1 )
    k = SIZE( Temperature, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and Temperature[NxK] inputs.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dBdT, DIM = 1 ) /= n .AND. &
         SIZE( dBdT, DIM = 2 ) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of input Temperature and output dBdT.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    Spectrum_loop: DO j = 1, k

      Frequency_loop: DO i = 1, n

        Error_Status = scalar_Planck_dBdT( x(i),             &  ! Input
                                           Temperature(i,j), &  ! Input
                                           dBdT(i,j),        &  ! Output
                                           Wavelength_Units = Wavelength_Units, &  ! Optional input
                                           Message_Log      = Message_Log       )  ! Error messaging
        IF ( Error_Status /= SUCCESS ) EXIT Spectrum_loop

      END DO Frequency_loop

    END DO Spectrum_loop

  END FUNCTION R1_x_R2_t_Planck_dBdT



!------------------------------------------------------------------------------
!S+
! NAME:
!       Planck_dTdB
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance given the spectral ordinate (frequency or wavelength)
!       and radiance.
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Planck_dTdB( x,                                   & ! Input
!                                   Radiance,                            & ! Input
!                                   dTdB,                                & ! Output
!                                   Wavelength_Units = Wavelength_Units, &  ! Optional input
!                                   Message_Log      = Message_Log       )  ! Error messaging
!
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar or Rank-1
!                           ATTRIBUTES: INTENT( IN )
!
!       Radiance:           Planck radiance(s) for which dTdB(s) is(are)
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1)
!                                         or
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output Temperature dimensionality chart
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                 } DEFAULT
!                                    Input Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                                    Output dTdB units are (K.m2.sr.cm-1)/mW  } DEFAULT
!                              == 1, Input x units are microns
!                                    Input Radiance units are W/(m2.sr.micron)
!                                    Output dTdB units are (K.m2.sr.um)/W
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      None
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       dTdB:               The derivative(s) of Planck temperature with respect
!                           to radiance. The output dimensions are determined by
!                           the input. In the following chart N == frequencies,
!                           K == radiances or spectra (i.e. separate radiances
!                           or radiance spectra):
!
!                               Input X     Input Radiance        Output dTdB
!                              dimension       dimension           dimension
!                           ------------------------------------------------------
!                               scalar          scalar               scalar
!                                 N             scalar                 N
!                               scalar            K                    K
!                                 N               N                    N
!                                 N               K               not allowed
!                                 N             N x K                N x K
!
!                           UNITS:      (K.m2.sr.cm-1)/mW
!                                         or
!                                       (K.m2.sr.um)/W
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:    Subroutine to output messages
!                          SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! PROCEDURE:
!       For frequency input, the Planck temperature differential with respect
!       to radiance is calculated using:
!
!                               c1 * c2 * frequency^4
!  dT/dB = ------------------------------------------------------------------
!           { c1 * frequency^3     }   {       ( c1 * frequency^3      ) }^2
!           { ---------------- + 1 } * { B * LN( ---------------- +  1 ) }
!           {         B            }   {       (          B            ) }
!
!
!       For wavelength input:
!
!                                           c1 * c2
!  dT/dB = --------------------------------------------------------------------------------
!                          {        c1            }   {       (        c1            ) }^2
!           wavelength^6 * { ---------------- + 1 } * { B * LN( ---------------- + 1 ) }
!                          { wavelength^5 * B     }   {       ( wavelength^5 * B     ) }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiances such that radiances
!       are in the units of mW/(m2.sr.cm-1) for frequency input or
!       W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Nov-2000
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar               scalar                 #
  !############################################################################

  FUNCTION scalar_Planck_dTdB( x,                &  ! Input
                               Radiance,         &  ! Input
                               dTdB,             &  ! output
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
    REAL( fp_kind ),            INTENT( IN )  :: x
    REAL( fp_kind ),            INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ),            INTENT( OUT ) :: dTdB
 
    ! -- Optional input
    INTEGER,         OPTIONAL,  INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index

    REAL( fp_kind ) :: Frequency, Wavelength
    REAL( fp_kind ) :: x_c_1, x_c_2
    REAL( fp_kind ) :: Scaled_Radiance
    REAL( fp_kind ) :: Argument



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Frequency/wavelength input
    IF ( x < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Invalid X (Frequency/wavelength) argument', &
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
    Unit_Index      = FREQUENCY_INDEX

    ! -- ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

 
 
    !#--------------------------------------------------------------------------#
    !#                     -- CALCULATE SPECTRAL PARAMETERS --                  #
    !#--------------------------------------------------------------------------#

    IF ( Frequency_Units ) THEN


      ! -----------------------
      ! Spectral units in cm^-1
      ! -----------------------

      Frequency = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency


    ELSE


      ! -------------------------
      ! Spectral units in microns
      ! -------------------------

      Wavelength = x

      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength

    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CALCULATE dT/dB --                           #
    !#--------------------------------------------------------------------------#

    ! -- Radiance in terms of W
    Scaled_Radiance = Radiance / RADIANCE_SCALE_FACTOR( Unit_Index )

    ! -- Common term in dT/dB formulation
    Argument = ( x_c_1 / Scaled_Radiance ) + ONE

    ! -- Calculate dT/dB in (K.....)/W
    dTdB =                    x_c_1 * x_c_2 / &
    !      ---------------------------------------------------------
            ( Argument * ( Scaled_Radiance * LOG( Argument ) )**2 )

    ! -- Convert dT/dB Units to (K...cm-1)/mW if required.
    dTdB = dTdB / RADIANCE_SCALE_FACTOR( Unit_Index )

  END FUNCTION scalar_Planck_dTdB





  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             scalar                 N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_r_Planck_dTdB( x,                &  ! Input
                                 Radiance,         &  ! Input
                                 dTdB,             &  ! output
                                 Wavelength_Units, &  ! Optional input
                                 Message_Log )     &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#
 
    ! ---------
    ! Arguments
    ! ---------

    ! Input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ),                 INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dTdB
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( x )
    IF ( SIZE( dTdB ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of dTdB output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_dTdB( x(i),     &  ! Input
                                         Radiance, &  ! Input
                                         dTdB(i),  &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Optional input
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_x_S_r_Planck_dTdB





  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar            K                    K                    #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_r_Planck_dTdB( x,                &  ! Input
                                 Radiance,         &  ! Input
                                 dTdB,             &  ! output
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
    REAL( fp_kind ),                 INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dTdB
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    n = SIZE( Radiance )
    IF ( SIZE( dTdB ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of dTdB output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_dTdB( x,                        &  ! Input
                                         Radiance(i),              &  ! Input
                                         dTdB(i),                  &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Optional input
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION S_x_R1_r_Planck_dTdB





  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N               N                    N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xr_Planck_dTdB( x,                &  ! Input
                              Radiance,         &  ! Input
                              dTdB,             &  ! output
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
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: dTdB
 
    ! -- Optional input
    INTEGER,         OPTIONAL,       INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, n



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

    ! -- Arrays have same lengths
    n = SIZE( x )
    IF ( SIZE( Radiance ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and Radiance inputs', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dTdB ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of dTdB output', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    DO i = 1, n

      Error_Status = scalar_Planck_dTdB( x(i),        &  ! Input
                                         Radiance(i), &  ! Input
                                         dTdB(i),     &  ! Output
                                         Wavelength_Units = Wavelength_Units, &  ! Optional input
                                         Message_Log      = Message_Log       )  ! Optional input
      IF ( Error_Status /= SUCCESS ) EXIT

    END DO

  END FUNCTION R1_xr_Planck_dTdB





  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             N x K                N x K                  #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_r_Planck_dTdB( x,                &  ! Input
                                  Radiance,         &  ! Input
                                  dTdB,             &  ! output
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
    REAL( fp_kind ), DIMENSION( : ),    INTENT( IN )  :: x
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( IN )  :: Radiance

    ! -- Output
    REAL( fp_kind ), DIMENSION( :, : ), INTENT( OUT ) :: dTdB
 
    ! -- Optional input
    INTEGER,         OPTIONAL,          INTENT( IN )  :: Wavelength_Units

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,          INTENT( IN )  :: Message_Log


    ! -------------
    ! Result status
    ! -------------
 
    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: i, j, n, k



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! -- Arrays have same lengths
    n = SIZE( Radiance, DIM = 1 )
    k = SIZE( Radiance, DIM = 2 )

    IF ( SIZE( x ) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X[Nx1] and Radiance[NxK] inputs.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF

    ! -- Output array has incorrect length
    IF ( SIZE( dTdB, DIM = 1 ) /= n .AND. &
         SIZE( dTdB, DIM = 2 ) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of input Radiance and output dTdB.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    ENDIF



    !#--------------------------------------------------------------------------#
    !#                       -- LOOP OVER SPECTRAL POINTS --                    #
    !#--------------------------------------------------------------------------#

    Spectrum_loop: DO j = 1, k

      Frequency_loop: DO i = 1, n

        Error_Status = scalar_Planck_dTdB( x(i),                     &  ! Input
                                           Radiance(i,j),            &  ! Input
                                           dTdB(i,j),                &  ! Output
                                           Wavelength_Units = Wavelength_Units, &  ! Optional input
                                           Message_Log      = Message_Log       )  ! Optional input
        IF ( Error_Status /= SUCCESS ) EXIT Spectrum_loop

      END DO Frequency_loop

    END DO Spectrum_loop

  END FUNCTION R1_x_R2_r_Planck_dTdB

END MODULE Planck_Functions


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Planck_Functions.f90,v 3.2 2006/09/21 17:57:31 wd20pd Exp $
!
! $Date: 2006/09/21 17:57:31 $
!
! $Revision: 3.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Planck_Functions.f90,v $
! Revision 3.2  2006/09/21 17:57:31  wd20pd
! Replaced all references to Error_Handler with Message_Handler.
!
! Revision 3.1  2004/09/08 23:32:54  paulv
! - Update for new Utility modules.
!
! Revision 3.0  2003/10/23 21:43:52  paulv
! - New version that uses an optional argument to specify the spectral ordinate
!   in terms of wavelength. Default is to assume frequency.
!   NOTE: INTERFACE CHANGED FROM v2 CODE.
!
! Revision 2.10  2003/03/18 19:52:57  paulv
! - Updated module documentation.
!
! Revision 2.9  2001/10/24 17:40:12  paulv
! - Changed all floating point kind types from DOUBLE to FP_KIND. The latter
!   is the only kind type "inherited" from the type_kinds module via the
!   ONLY clause.
! - Redefined all literal constant definitions to use the FP_KIND type, e.g.
!     1.0d0
!   was changed to
!     1.0_fp_kind
!
! Revision 2.8  2001/05/09 17:30:36  paulv
! - Added PUBLIC parameter definitions for Units.
! - Replaced all references to explicit number units key to the defined
!   parameter, WAVENUMBER_UNIT or WAVELENGTH_UNIT.
!
! Revision 2.7  2001/05/08 21:37:33  paulv
! - Changed all references to derived constants D_C_1 and D_C_2 to C_1
!   and C_2 to reflect changes in the FUNDAMENTAL_CONSTANTS module.
!
! Revision 2.6  2000/12/13 16:08:19  paulv
! - More doc. corrections! Dammit.
!
! Revision 2.5  2000/12/13 16:04:48  paulv
! - Updated header documentation.
!
! Revision 2.3  2000/12/11 20:43:00  paulv
! - Updated and corrected header documentation.
!
! Revision 2.2  2000/12/11 20:32:34  paulv
! - Modified to reflect changes in FUNDAMENTAL_CONSTANTS module.
! - All explicit typing of integers as ( Long ) removed.
! - Cosmetic changes.
!
! Revision 2.1  2000/12/11 18:35:33  paulv
! - Added multiple dimension input options.
!
! Revision 1.8  2000/12/01 22:40:27  paulv
! - Adding more specific functions to allow more freedom in specifying
!   input argument dimensions.
! - Adding header for each generic function.
! - INCOMPLETE....IN PROGRESS
!
! Revision 1.7  2000/11/16 21:11:33  paulv
! - Added Planck_dTdB() functions.
! - Updated header documentation.
! - Converted all declared parameter names to uppercase.
!
! Revision 1.6  2000/11/03 20:09:05  paulv
! - Added ONLY clause to the USE statement for the fundamental_constants
!   module. Only the derived constants c_1 and c_2 are required for the
!   Planck function calculations.
!
! Revision 1.5  2000/05/03 18:32:18  paulv
! - Fundamental constants are now supplied via a module rather than defined
!   in line. This is true for the derived radiation constants c_1 and c_2
! - Return error codes are defined as parameters:
!     SUCCESS =  1
!     FAILURE = -1
! - Extraneous array definitions removed.
! - Updated header documentation.
!
! Revision 1.4  1999/10/18 18:54:22  paulv
! Corrected error in header documentation
!
! Revision 1.3  1999/10/18 17:10:21  paulv
! Added RCS Id keyword
!
! Revision 1.2  1999/10/18 15:23:26  paulv
! - Replaced rank-1 functions with calls-in-a-loop to scalar functions.
!   This eliminated the replicated code and actually turned out to be
!   faster than using vector arithmetic.
! - Updated header documentation.
!
