!------------------------------------------------------------------------------
!
! NAME:
!       Planck_Functions_Test
!
! PURPOSE:
!       Program to test the Planck function module
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:            Module to hold specification kinds for variable
!                              declaration.
!
!       Error_Handler:         Module to define simple error codes and handle
!                              error conditions
!                              USEs: FILE_UTILITY module
!
!       Planck_Functions:      Module containing Planck function Radiance,
!                              Temperature, dB/dT, and dT/dB routines.
!                              USEs: TYPE_KINDS module
!                                    FUNDAMENTAL_CONSTANTS module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       None.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 1999, 2001 Paul van Delst
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
!
!------------------------------------------------------------------------------

PROGRAM Planck_Functions_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Error_Handler

  USE Planck_Functions


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Planck_Functions_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Planck_Functions_Test.f90,v 1.5 2004/09/08 16:52:13 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Unit name strings
  INTEGER,        PARAMETER :: N_UNITS = 2
  CHARACTER( * ), PARAMETER, DIMENSION( N_UNITS ) :: V_UNIT_STRING  = (/ 'frequency (cm-1)', &
                                                                         'wavelength (um) ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_UNITS ) :: R_UNIT_STRING  = (/ 'mW/(m2.sr.cm-1)', &
                                                                         'W/(m2.sr.um)   ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_UNITS ) :: D1_UNIT_STRING = (/ 'mW/(m2.sr.cm-1.K)', &
                                                                         'W/(m2.sr.um.K)   ' /)
  CHARACTER( * ), PARAMETER, DIMENSION( N_UNITS ) :: D2_UNIT_STRING = (/ '(K.m2.sr.cm-1)/mW', &
                                                                         '(K.m2.sr.um)/W   ' /)
  ! -- Array sizes
  INTEGER,        PARAMETER :: N_FREQUENCIES  = 5
  INTEGER,        PARAMETER :: N_TEMPERATURES = 3


  ! -- Number of perturbations and the size
  INTEGER,         PARAMETER :: N_PERTURBATIONS = 11
  REAL( fp_kind ), PARAMETER :: D_PERTURBATION  = 1.0_fp_kind

  ! -- Default temeprature
  REAL( fp_kind ), PARAMETER :: T0 = 300.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: i, j, k, l, m, n

  CHARACTER( 80 ) :: Output_Fmt

  REAL( fp_kind ), DIMENSION( N_FREQUENCIES ) :: Frequency
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES ) :: x

  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_TEMPERATURES ) :: Radiance
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_TEMPERATURES ) :: Temperature
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_TEMPERATURES ) :: dBdT
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_TEMPERATURES ) :: dTdB

  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: Temperature_NL
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: Radiance_NL
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: dTemperature_NL
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: dRadiance_NL
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: dRadiance_dBdT
  REAL( fp_kind ), DIMENSION( N_FREQUENCIES, N_PERTURBATIONS ) :: dTemperature_dTdB

  INTEGER :: Wavelength_Units



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to test the Planck functions module routines for ")' )
  WRITE( *, '( 5x, "   scalar, rank-1, and rank-2 input.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                             -- SET UP STUFF --                             #
  !#----------------------------------------------------------------------------#

  ! -------------------------------------
  ! Fill frequency and temperature arrays
  ! -------------------------------------

  DO i = 1, N_FREQUENCIES
    Frequency( i ) = 800.0_fp_kind + ( REAL( i-1, fp_kind ) * 400.0_fp_kind )
  END DO

  DO i = 1, N_TEMPERATURES
    Temperature( :, i ) = T0 + ( REAL( i-1, fp_kind ) * 5.0_fp_kind )
  END DO


  ! -------------------------------
  ! Create the output format string
  ! -------------------------------

  WRITE( Output_Fmt, '( "(",i2,"(1x,f12.6))" )' ) N_FREQUENCIES



  !#----------------------------------------------------------------------------#
  !#                    -- BEGIN CHECK OF GENERIC INTERFACE --                  #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //15x, "GENERIC INTERFACE CHECK", //)' )


  ! ---------------------------------
  ! Loop over spectral ordinate units
  ! ---------------------------------

  Unit_Loop: DO i = 1, n_Units


    ! -----------------------------------
    ! Determine input units and unit flag
    ! -----------------------------------

    IF ( i == 1 ) THEN
      x = Frequency
      Wavelength_Units = 0
    ELSE
      x   = 10000.0_fp_kind / Frequency
      Wavelength_Units = 1
    END IF


    ! -------------------
    ! Calculate Radiances
    ! -------------------

    WRITE( *, '( /5x, "TEMPERATURE (K) -> RADIANCE (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Scalar Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(1), &
                                    Temperature(1,1), &
                                    Radiance(1,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Nx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(1,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)
    WRITE( *, '( 5x, "Output: Kx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(1), &
                                    Temperature(1,:), &
                                    Radiance(1,:), &
                                    Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)
    WRITE( *, '( 5x, "Output: Nx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(:,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)
    WRITE( *, '( 5x, "Output: NxK Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(:,:), &
                                    Radiance(:,:), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! ----------------------
    ! Calculate temperatures
    ! ----------------------

    WRITE( *, '( //5x, "RADIANCE (", a, ") -> TEMPERATURE (K)..." )' ) TRIM( R_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Scalar Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(1), &
                                       Radiance(1,1), &
                                       Temperature(1,1), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Nx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(1,1), &
                                       Temperature(:,1), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)
    WRITE( *, '( 5x, "Output: Kx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(1), &
                                       Radiance(1,:), &
                                       Temperature(1,:), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)
    WRITE( *, '( 5x, "Output: Nx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(:,1), &
                                       Temperature(:,1), &
                                       Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)
    WRITE( *, '( 5x, "Output: NxK Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(:,:), &
                                       Temperature(:,:), &
                                       Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! ---------------
    ! Calculate dB/dT
    ! ---------------

    WRITE( *, '( //5x, "TEMPERATURE (K) -> dB/dT (", a, ")..." )' ) TRIM( D1_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Scalar dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(1), &
                                Temperature(1,1), &
                                dBdT(1,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Nx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(1,1), &
                                dBdT(:,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)
    WRITE( *, '( 5x, "Output: Kx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(1), &
                                Temperature(1,:), &
                                dBdT(1,:), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)
    WRITE( *, '( 5x, "Output: Nx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(:,1), &
                                dBdT(:,1), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)
    WRITE( *, '( 5x, "Output: NxK dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(:,:), &
                                dBdT(:,:), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! ---------------
    ! Calculate dT/dB
    ! ---------------

    WRITE( *, '( //5x, "RADIANCE (", a, ") -> dT/dB (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) ), &
                                                                       TRIM( D2_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Scalar dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(1), &
                                Radiance(1,1), &
                                dTdB(1,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Nx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(1,1), &
                                dTdB(:,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)
    WRITE( *, '( 5x, "Output: Kx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(1), &
                                Radiance(1,:), &
                                dTdB(1,:), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)
    WRITE( *, '( 5x, "Output: Nx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(:,1), &
                                dTdB(:,1), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)
    WRITE( *, '( 5x, "Output: NxK dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(:,:), &
                                dTdB(:,:), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )
              
  END DO Unit_Loop



  !#----------------------------------------------------------------------------#
  !#                  -- BEGIN CHECK OF DERIVATIVE ROUTINE --                   #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //15x, "DERIVATIVE ROUTINE CHECKS")' )



  ! ---------------------------------
  ! Loop over spectral ordinate units
  ! ---------------------------------

  Unit_Loop_Derivative_Check: DO i = 1, n_Units


    ! -----------------------------------
    ! Determine input units and unit flag
    ! -----------------------------------

    IF ( i == 1 ) THEN
      x = Frequency
      Wavelength_Units = 0
    ELSE
      x   = 10000.0_fp_kind / Frequency
      Wavelength_Units = 1
    END IF


    ! ----------------------------------------
    ! Construct temperature perturbation array
    ! ----------------------------------------

    j = 0
    DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2
      j = j+1
      Temperature_NL(  :, j ) = T0 + ( REAL( n, fp_kind ) * D_PERTURBATION )
      dTemperature_NL( :, j ) = Temperature_NL(  :, j ) - T0
    END DO


    ! ----------------------------
    ! Compute the dB/dT derivative
    ! ----------------------------

    WRITE( *, '( //5x, "TEMPERATURE (K) -> dB/dT (", a, ")..." )' ) TRIM( D1_UNIT_STRING( i ) )

    ! -- The finite difference
    Error_Status = Planck_Radiance( x, &
                                    Temperature_NL, &
                                    Radiance_NL, &
                                    Wavelength_Units = Wavelength_Units )

    DO j = 1, N_PERTURBATIONS
      dRadiance_NL(:,j) = Radiance_NL(:,j) - Radiance_NL(:,N_PERTURBATIONS/2+1)
    END DO

    ! -- The derivative
    Error_Status = Planck_dBdT( x, &
                                Temperature_NL, &
                                dRadiance_dBdT, &
                                Wavelength_Units = Wavelength_Units )

    dRadiance_dBdT = dRadiance_dBdT * dTemperature_NL


    ! -- Output the two datasets
    WRITE( *, '( /10x, "Finite difference dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dB/dT routine dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_dBdT(:,j)
    END DO

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! ----------------------------
    ! Compute the dT/dB derivative
    ! ----------------------------

    WRITE( *, '( //5x, "RADIANCE (", a, ") -> dT/dB (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) ), &
                                                                       TRIM( D2_UNIT_STRING( i ) )

    ! -- The finite difference
    Error_Status = Planck_Temperature( x, &
                                       Radiance_NL, &
                                       Temperature_NL, &
                                       Wavelength_Units = Wavelength_Units )

    DO j = 1, N_PERTURBATIONS
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO

    ! -- The derivative
    Error_Status = Planck_dTdB( x, &
                                Radiance_NL, &
                                dTemperature_dTdB, &
                                Wavelength_Units = Wavelength_Units )

    dTemperature_dTdB = dTemperature_dTdB * dRadiance_NL


    ! -- Output the two datasets
    WRITE( *, '( /10x, "Finite difference result (K):" )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dT/dB routine result (K):" )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_dTdB(:,j)
    END DO

    IF ( i < N_UNITS ) THEN
      WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF
              
  END DO Unit_Loop_Derivative_Check

END PROGRAM Planck_Functions_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Planck_Functions_Test.f90,v 1.5 2004/09/08 16:52:13 paulv Exp $
!
! $Date: 2004/09/08 16:52:13 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Planck_Functions_Test.f90,v $
! Revision 1.5  2004/09/08 16:52:13  paulv
! - Using new Utility modules.
!
! Revision 1.4  2003/10/27 20:23:33  paulv
! - Added code to compare the finite difference results for dB/dT and dT/dB
!   with the actual dBdT and dTdB routines.
!
! Revision 1.3  2003/10/23 21:44:51  paulv
! - Updated to use new Planck_Functions module.
!
! Revision 1.2  2001/10/24 17:59:12  paulv
! - Cleaned up (!) output of test results.
! - Altered array dimensioning and assignment to allow easy changes.
! - Added documentation.
!
!
!
!
