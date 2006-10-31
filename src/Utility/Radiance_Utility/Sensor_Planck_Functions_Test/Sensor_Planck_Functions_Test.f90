!------------------------------------------------------------------------------
!
! NAME:
!       Sensor_Planck_Functions_Test
!
! PURPOSE:
!       Program to test the sensor Planck function module
!
! CATEGORY:
!       Radiance
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                Module to hold specification kinds for
!                                  variable declaration.
!
!       Message_Handler:           Module to define simple error codes and
!                                  handle error conditions
!                                  USEs: FILE_UTILITY module
!
!       SpcCoeff_Define:           Module defining the SpcCoeff data structure
!                                  and containing routines to manipulate it.
!                                  USEs: TYPE_KINDS module
!                                        Message_Handler module
!                                        COMPARE_FLOAT_NUMBERS module
!                                        
!       SpcCoeff_netCDF_IO:        Module containing routines to read and
!                                  write netCDF format SpcCoeff files.
!                                  USEs: TYPE_KINDS module
!                                        Message_Handler module
!                                        SPCCOEFF_DEFINE module
!                                        NETCDF module
!                                        NETCDF_UTILITY module
!
!       Sensor_Planck_Functions:   Module containing Planck function radiance,
!                                  temperature, dB/dT, and dT/dB routines for
!                                  application to sensor channels.
!                                  USEs: TYPE_KINDS module
!                                        FUNDAMENTAL_CONSTANTS module
!                                        Message_Handler module
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
!       netCDF SpcCoeff files are read in.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-May-2001
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
!
!------------------------------------------------------------------------------

PROGRAM Sensor_Planck_Functions_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO

  USE Sensor_Planck_Functions


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ---------- 

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME = 'Sensor_Planck_Functions_Test'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Sensor_Planck_Functions_Test.f90,v 2.3 2006/09/21 17:58:25 wd20pd Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Unit name strings
  INTEGER,         PARAMETER :: N_UNITS = 2
  CHARACTER( * ),  PARAMETER, DIMENSION( N_UNITS ) :: V_UNIT_STRING  = (/ 'Frequency (cm^-1)', &
                                                                          'Wavelength (um)  ' /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N_UNITS ) :: R_UNIT_STRING  = (/ 'mW/(m2.sr.cm-1)', &
                                                                          'W/(m2.sr.um)   ' /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N_UNITS ) :: DBDT_UNIT_STRING = (/ 'mW/(m2.sr.cm-1.K)', &
                                                                            'W/(m2.sr.um.K)   ' /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N_UNITS ) :: DTDB_UNIT_STRING = (/ '(K.m2.sr.cm-1)/mW', &
                                                                           '(K.m2.sr.um)/W   ' /)

  ! -- SpcCoeff datafile name
  CHARACTER( * ),  PARAMETER :: SPCCOEFF_FILENAME = 'hirs3_n17.SpcCoeff.nc'

  ! -- Array sizes
  INTEGER,         PARAMETER :: N_CHANNELS = 19
  INTEGER,         PARAMETER :: N_TEMPERATURES = 5
  CHARACTER(*),    PARAMETER :: C_TEMPERATURES = '5'

  ! -- Number of perturbations and the size
  INTEGER,         PARAMETER :: N_PERTURBATIONS = 11
  REAL( fp_kind ), PARAMETER :: D_PERTURBATION  = 1.0_fp_kind

  ! -- Default temeprature
  REAL( fp_kind ), PARAMETER :: T0 = 273.0_fp_kind


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  TYPE( SpcCoeff_type ) :: SpcCoeff

  CHARACTER( 12 )  ::    R_Fmt
  CHARACTER( 12 )  ::    T_Fmt
  CHARACTER( 12 )  :: dBdT_Fmt
  CHARACTER( 12 )  :: dTdB_Fmt
  CHARACTER( 200 ) :: Channel_Fmt
  CHARACTER( 200 ) :: Output_Fmt

  INTEGER :: Wavelength_Units

  INTEGER :: i, j, l, n
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: Temperature
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: Radiance
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: dBdT
  REAL( fp_kind ), DIMENSION( N_TEMPERATURES ) :: dTdB

  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: Temperature_NL
  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: Radiance_NL
  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dTemperature_NL
  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dRadiance_NL
  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dRadiance_dBdT
  REAL( fp_kind ), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dTemperature_dTdB



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the sensor Planck function routines.")' )
  WRITE( *, '(/5x, " $Revision: 2.3 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                    -- READ THE TEST SpcCoeff DATAFILE --                   #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_SpcCoeff_netCDF( SPCCOEFF_FILENAME, &
                                       SpcCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading SpcCoeff file '//SPCCOEFF_FILENAME, &
                          Error_Status )
    STOP
  END IF

  IF ( N_CHANNELS /= SpcCoeff%n_Channels ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Number of test channels different from value in '//&
                          SPCCOEFF_FILENAME, &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                             -- SET UP STUFF --                             #
  !#----------------------------------------------------------------------------#

  ! -------------------
  ! Default temperature
  ! -------------------

  DO j = 1, N_TEMPERATURES
    Temperature( j ) = T0 + ( REAL( j-1, fp_kind ) * 5.0_fp_kind )
  END DO



  !#----------------------------------------------------------------------------#
  !#                          -- BEGIN GENERIC CHECK --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //15x, "GENERIC INTERFACE CHECK", //)' )


  ! ---------------------------------
  ! Loop over spectral ordinate units
  ! ---------------------------------

  Unit_Loop_1: DO i = 1, N_UNITS


    ! --------------
    ! Set units flag
    ! --------------

    IF ( i == 1 ) THEN
      Wavelength_Units = 0
    ELSE
      Wavelength_Units = 1
    END IF


    ! ------------------
    ! Loop over channels
    ! ------------------

    WRITE( *, '( /5x, "Spectral ordinate: ", a, / )' ) V_UNIT_STRING( i )


    Channel_Loop_1: DO l = 1, SpcCoeff%n_Channels


      ! ----------------------
      ! Loop over temperatures
      ! ----------------------

      Temperature_Loop_1: DO j = 1, N_TEMPERATURES


        ! ------------------------
        ! Calculate channel values
        ! ------------------------

        Error_Status = Sensor_Radiance( SpcCoeff, &
                                        SpcCoeff%Sensor_Channel(l), &
                                        Temperature(j), &
                                        Radiance(j), &
                                        Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_Temperature( SpcCoeff, &
                                           SpcCoeff%Sensor_Channel(l), &
                                           Radiance(j), &
                                           Temperature(j), &
                                           Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_dBdT( SpcCoeff, &
                                    SpcCoeff%Sensor_Channel(l), &
                                    Temperature(j), &
                                    dBdT(j), &
                                    Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_dTdB( SpcCoeff, &
                                    SpcCoeff%Sensor_Channel(l), &
                                    Radiance(j), &
                                    dTdB(j), &
                                    Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

      END DO Temperature_Loop_1


      ! --------------
      ! Output results
      ! --------------

      ! -- Futz around with the output formats
      IF ( ANY( Radiance < 1.0e-06_fp_kind ) ) THEN
        R_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        R_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      T_Fmt = C_TEMPERATURES//'(1x,f10.6) '

      IF ( ANY( dBdT < 1.0e-06_fp_kind ) ) THEN
        dBdT_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        dBdT_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      IF ( ANY( ABS( dTdB ) > 999.0_fp_kind ) ) THEN
        dTdB_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        dTdB_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      Output_Fmt = '( /2x, "Ch. ", i2, '// &
                          '     " R     = ", '//R_Fmt//', 1x,a, '// &
                          '/8x, " T     = ", '//T_Fmt//', 1x,"K   ", '// &
                          '/8x, " dB/dT = ", '//dBdT_Fmt//', 1x,a, '// &
                          '/8x, " dT/dB = ", '//dTdB_Fmt//', 1x,a )'

      ! -- Write results to screen
      WRITE( *, FMT = TRIM( output_fmt ) ) SpcCoeff%Sensor_Channel(l), &
                                           Radiance, R_UNIT_STRING(i), &
                                           Temperature, &
                                           dBdT, DBDT_UNIT_STRING(i), &
                                           dTdB, DTDB_UNIT_STRING(i)


    END DO Channel_Loop_1

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )
 
  END DO Unit_Loop_1



  !#----------------------------------------------------------------------------#
  !#                 -- BEGIN DERIVATIVE ROUTINE CHECK --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( //15x, "DERIVATIVE ROUTINE CHECK", //)' )


  ! ---------------------------------------------------------
  ! Create the output format strings for the derivative check
  ! ---------------------------------------------------------

  Channel_Fmt = ' '
  Output_Fmt  = ' '

  WRITE( Channel_Fmt, '( "(",i2,"(4x,i2,3x))" )' ) N_CHANNELS
  WRITE( Output_Fmt,  '( "(",i2,"(1x,f8.4))" )' ) N_CHANNELS


  ! ---------------------------------
  ! Loop over spectral ordinate units
  ! ---------------------------------

  Unit_Loop_2: DO i = 1, N_UNITS


    ! --------------
    ! Set units flag
    ! --------------

    IF ( i == 1 ) THEN
      Wavelength_Units = 0
    ELSE
      Wavelength_Units = 1
    END IF


    ! ----------------------------------------
    ! Construct temperature perturbation array
    ! ----------------------------------------

    j = 0
    DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2
      j = j+1
      Temperature_NL(  :, j ) = T0 + ( REAL( n, fp_kind ) * D_PERTURBATION )
      dTemperature_NL( :, j ) = Temperature_NL( :, j ) - T0
    END DO


    ! ----------------------------
    ! Compute the dB/dT derivative
    ! ----------------------------

    WRITE( *, '( //5x, "TEMPERATURE (K) -> dB/dT (", a, ")..." )' ) TRIM( DBDT_UNIT_STRING( i ) )

    Channel_Loop_dBdT: DO l = 1, SpcCoeff%n_Channels


      ! ---------------------------
      ! Loop over the perturbations
      ! ---------------------------

      DO j = 1, N_PERTURBATIONS


        ! ----------------------------
        ! Compute the dB/dT derivative
        ! ----------------------------

        ! -- The finite difference
        Error_Status = Sensor_Radiance( SpcCoeff, &
                                        SpcCoeff%Sensor_Channel(l), &
                                        Temperature_NL(l,j), &
                                        Radiance_NL(l,j), &
                                        Wavelength_Units = Wavelength_Units )


        ! -- The derivative
        Error_Status = Sensor_dBdT( SpcCoeff, &
                                    SpcCoeff%Sensor_Channel(l), &
                                    Temperature_NL(l,j), &
                                    dRadiance_dBdT(l,j), &
                                    Wavelength_Units = Wavelength_Units )

      END DO

    END DO Channel_Loop_dBdT

    DO j = 1, N_PERTURBATIONS
      dRadiance_NL(:,j) = Radiance_NL(:,j) - Radiance_NL(:,N_PERTURBATIONS/2+1)
    END DO

    dRadiance_dBdT = dRadiance_dBdT * dTemperature_NL


    ! -- Output the two datasets
    WRITE( *, '( /10x, "Finite difference dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SpcCoeff%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dB/dT routine dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SpcCoeff%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_dBdT(:,j)
    END DO

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! ----------------------------
    ! Compute the dT/dB derivative
    ! ----------------------------

    WRITE( *, '( //5x, "RADIANCE (", a, ") -> dT/dB (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) ), &
                                                                       TRIM( DTDB_UNIT_STRING( i ) )

    Channel_Loop_dTdB: DO l = 1, SpcCoeff%n_Channels


      ! ---------------------------
      ! Loop over the perturbations
      ! ---------------------------

      DO j = 1, N_PERTURBATIONS


        ! ----------------------------
        ! Compute the dT/dB derivative
        ! ----------------------------

        ! -- The finite difference
        Error_Status = Sensor_Temperature( SpcCoeff, &
                                           SpcCoeff%Sensor_Channel(l), &
                                           Radiance_NL(l,j), &
                                           Temperature_NL(l,j), &
                                           Wavelength_Units = Wavelength_Units )


        ! -- The derivative
        Error_Status = Sensor_dTdB( SpcCoeff, &
                                    SpcCoeff%Sensor_Channel(l), &
                                    Radiance_NL(l,j), &
                                    dTemperature_dTdB(l,j), &
                                    Wavelength_Units = Wavelength_Units )

      END DO

    END DO Channel_Loop_dTdB

    DO j = 1, N_PERTURBATIONS
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO

    dTemperature_dTdB = dTemperature_dTdB * dRadiance_NL


    ! -- Output the two datasets
    WRITE( *, '( /10x, "Finite difference dT result (K):" )' )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SpcCoeff%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dT/dB routine dT result (K):" )' )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SpcCoeff%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_dTdB(:,j)
    END DO

    IF ( i < N_UNITS ) THEN
      WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF

  END DO Unit_Loop_2



  !#----------------------------------------------------------------------------#
  !#                    -- DESTROY THE SpcCoeff STRUCTURE --                    #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SpcCoeff( SpcCoeff )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff structure', &
                          WARNING )
  END IF

END PROGRAM Sensor_Planck_Functions_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Sensor_Planck_Functions_Test.f90,v 2.3 2006/09/21 17:58:25 wd20pd Exp $
!
! $Date: 2006/09/21 17:58:25 $
!
! $Revision: 2.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Sensor_Planck_Functions_Test.f90,v $
! Revision 2.3  2006/09/21 17:58:25  wd20pd
! Replaced all references to Error_Handler with Message_Handler.
!
! Revision 2.2  2004/09/08 23:32:54  paulv
! - Update for new Utility modules.
!
! Revision 2.1  2003/11/13 14:41:19  paulv
! - Completed changes to test code to include derivative routine checks.
! - Changed output of generic interface checks to include more temperatures.
!
! Revision 2.0  2003/10/24 18:48:18  paulv
! - Test program for new versions of Sensor Planck Function module.
!
!
!
!


