!------------------------------------------------------------------------------
!M+
! NAME:
!       Create_AtmProfile
!
! PURPOSE:
!       Program to create netCDF AtmProfile data files. This format requires
!       all the profiles to have the same number of levels/layers so profile
!       sets with different layering between profiles are interpolated to the
!       AIRS set of levels.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       File_Utility:               Module containing generic file utility
!                                   routines
!
!       Error_Handler:              Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       Interpolate:                Module containing interpolation routines.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       AtmProfile_Define:          Module defining the AtmProfile data
!                                   structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       AtmProfile_netCDF_IO:       Module containing routines to read and
!                                   write AtmProfile netCDF format files.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         ATMPROFILE_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
!
!       Profile_Utility_Parameters: Module containing parameters used in the
!                                   profile utility modules.
!                                   USEs: TYPE_KINDS module
!                                         FUNDAMENTAL_CONSTANTS module
!
!       Units_Conversion:           Module containing routines to convert
!                                   atmospheric profile concentration units.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         PROFILE_UTILITY_PARAMETERS module
!                                         ATMOSPHERIC_PROPERTIES module
!
!       Geopotential:               Module containing routines for calculating
!                                   geopotential heights.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         FUNDAMENTAL_CONSTANTS module
!                                         ATMOSPHERIC_PROPERTIES module
!
!       Tau_Production_Parameters:  Module defining parameters used in the
!                                   LBL transmittance production runs
!                                   USEs: TYPE_KINDS module
!                                         LBLRTM_PARAMETERS module
!
!       UMBC_Profile_Set:           Module containing the UMBC atmospheric
!                                   profile dependent set data definitions
!                                   and access routines
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         PROFILE_UTILITY_PARAMETERS module
!                                         UNITS_CONVERSION module
!
!       CIMSS_Profile_Set:          Module containing the CIMSS atmospheric
!                                   profile dependent set data definitions
!                                   and access routines
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       ECMWF_Profile_Set:          Module containing the ECMWF atmospheric
!                                   profile dependent set data definitions
!                                   and access routines
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!
!       Model_Profile_Set:          Module containing the climatology model
!                                   atmospheric profile set data definitions
!                                   and access routines
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
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
!       - Input profile data file(s)
!       - Output netCDF format AtmProfile file.
!
! SIDE EFFECTS:
!       All output files are overwritten if they already exist.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jul-2002
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

PROGRAM Create_AtmProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Error_Handler

  USE Interpolate

  USE AtmProfile_Parameters
  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE UMBC_Profile_Set
  USE CIMSS_Profile_Set
  USE ECMWF_Profile_Set
  USE Model_Profile_Set

  USE Profile_Utility_Parameters
  USE Units_Conversion
  USE Geopotential


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Create_AtmProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Create_AtmProfile.f90,v 1.11 2005/01/04 22:48:28 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Numbers
  REAL( fp_kind ), PARAMETER :: ZEROpointFIVE = 0.5_fp_kind


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  CHARACTER( 256 ) :: AtmProfile_Filename

  ! -- Input data
  REAL( fp_kind ), DIMENSION( : ),    POINTER :: Pressure
  REAL( fp_kind ), DIMENSION( : ),    POINTER :: Temperature
  REAL( fp_kind ), DIMENSION( :, : ), POINTER :: Absorber

  ! -- Array for correcting interpolated absorber amounts
  REAL( fp_kind ), DIMENSION( N_ATMPROFILE_LEVELS+1 ) :: acorr

  ! -- Array for water vapor partial pressure
  REAL( fp_kind ), DIMENSION( N_ATMPROFILE_LEVELS ) :: H2O_Pressure

  LOGICAL :: Profile_Interpolate

  INTEGER :: n_Layers, k, ik
  INTEGER :: n_Levels
  INTEGER :: n_Absorbers, j
  INTEGER :: n_Profiles, m
  INTEGER, DIMENSION( 1 ) :: j_idx

  INTEGER :: Profile_Set

  CHARACTER( 3 ) :: cLevel
  CHARACTER( 3 ) :: cProfile

  CHARACTER( 512 ) :: History
  CHARACTER( 256 ) :: Comment
  CHARACTER( 256 ) :: Interpolation_RCS_Id
  CHARACTER( 256 ) :: Profile_Set_RCS_Id

  TYPE( AtmProfile_type ) :: AtmProfile



  !#----------------------------------------------------------------------------#
  !#                     -- INITIALIZE POINTER VARIABLES --                     #
  !#----------------------------------------------------------------------------#

  ! -----------------
  ! Input data arrays
  ! -----------------

  NULLIFY( Pressure, &
           Temperature, &
           Absorber )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to create netCDF AtmProfile data files. This    ")' )
  WRITE( *, '( 5x, "   format requires all the profiles to have the same     ")' )
  WRITE( *, '( 5x, "   number of levels/layers so profile sets with different")' )
  WRITE( *, '( 5x, "   layering between profiles are interpolated to the AIRS")' )
  WRITE( *, '( 5x, "   set of levels.")' )
  WRITE( *, '(/5x, " $Revision: 1.11 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                -- SELECT THE PROFILE SET TO INTERPOLATE --                 #
  !#----------------------------------------------------------------------------#

  Profile_Set_select: DO

    WRITE( *, '( /5x, "Select the profile set to process:" )' )
    DO m = 1, N_ATMPROFILE_SETS
      WRITE( *, '( 10x, i1, ") ", a, " set" )' ) m, ATMPROFILE_SET_ID_TAG( m )
    END DO
    WRITE( *, FMT = '( 5x, "Enter choice: " )', &
              ADVANCE = 'NO' )
    READ( *, * ) Profile_Set

    IF ( Profile_Set > 0 .AND. Profile_Set <= N_ATMPROFILE_SETS ) EXIT

    WRITE( *, '( 10x, "** Invalid selection **" )' )

  END DO Profile_Set_select


  ! --------------------------------------------
  ! Assign the number of available absorbers and
  ! profiles and set the interpolation flag
  ! --------------------------------------------

  SELECT CASE ( TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ) )

    CASE ( 'UMBC' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_UMBC_ABSORBERS
      n_Profiles  = N_UMBC_PROFILES
      Profile_Interpolate = .TRUE.

    CASE ( 'CIMSS' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_CIMSS_ABSORBERS
      n_Profiles  = N_CIMSS_PROFILES
      Profile_Interpolate = .TRUE.

    CASE ( 'ECMWF' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_ECMWF_ABSORBERS
      n_Profiles  = N_ECMWF_PROFILES
      Profile_Interpolate = .FALSE.  ! Already at 101 levels

    CASE ( 'Model' )
      n_Layers    = N_ATMPROFILE_LAYERS
      n_Levels    = n_Layers + 1
      n_Absorbers = N_MODEL_ABSORBERS
      n_Profiles  = N_MODEL_PROFILES
      Profile_Interpolate = .TRUE.

    CASE DEFAULT
      WRITE( *, '( /5x, "This option not yet implemented." )' )
      STOP

  END SELECT


  ! ----------------------------------
  ! Doublecheck the number of profiles
  ! ----------------------------------

  IF ( n_Profiles /= N_ATMPROFILES( Profile_Set ) ) THEN
    Error_Status = FAILURE
    WRITE( Message, '( "Number of ", a, " profiles, ", i3, &
                      &", is different from definition, ", i3, "." )' ) &
                    TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ), &
                    n_Profiles, &
                    N_ATMPROFILES( Profile_Set )
    CALL display_message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                   -- ALLOCATE THE AtmProfile STRUCTURE --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Allocate_AtmProfile( n_Layers, &
                                      n_Absorbers, &
                                      n_Profiles, &
                                      AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error occurred allocating AtmProfile structure.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                             -- PROFILE LOOP --                             #
  !#----------------------------------------------------------------------------#

  WRITE( *, * )

  Profile_Loop: DO m = 1, n_Profiles


    WRITE( *, '( 5x, "Processing ", a, " profile #", i2 )' ) &
              TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ), m



    !#--------------------------------------------------------------------------#
    !#                       -- LOAD THE PROFILE DATA --                        #
    !#--------------------------------------------------------------------------#

    SELECT CASE ( TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ) )

      CASE ( 'UMBC' )
        Error_Status = Load_UMBC_Profile( m, &
                                          Pressure, &
                                          Temperature, &
                                          Absorber, &
                                          Absorber_ID       = AtmProfile%Absorber_ID, &
                                          Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                          Description       = AtmProfile%Description(m), &
                                          Climatology_Model = AtmProfile%Climatology_Model(m), &
                                          Year              = AtmProfile%DateTime(m)%Year, &
                                          Month             = AtmProfile%DateTime(m)%Month, &
                                          Day               = AtmProfile%DateTime(m)%Day, &
                                          Hour              = AtmProfile%DateTime(m)%Hour, &
                                          Latitude          = AtmProfile%Location(m)%Latitude, &
                                          Longitude         = AtmProfile%Location(m)%Longitude, &
                                          Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                          RCS_Id = Profile_Set_RCS_Id )

        AtmProfile%Level_Pressure(:,m) = ATMPROFILE_LEVEL_PRESSURE
        AtmProfile%Layer_Pressure(:,m) = ( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) - &
                                           ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     ) / &
       !                                 -------------------------------------------------
                                         LOG( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) / &
                                              ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     )


      CASE ( 'CIMSS' )
        Error_Status = Load_CIMSS_Profile( m, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                           Description       = AtmProfile%Description(m), &
                                           Climatology_Model = AtmProfile%Climatology_Model(m), &
                                           Year              = AtmProfile%DateTime(m)%Year, &
                                           Month             = AtmProfile%DateTime(m)%Month, &
                                           Day               = AtmProfile%DateTime(m)%Day, &
                                           Hour              = AtmProfile%DateTime(m)%Hour, &
                                           Latitude          = AtmProfile%Location(m)%Latitude, &
                                           Longitude         = AtmProfile%Location(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_RCS_Id )

        AtmProfile%Level_Pressure(:,m) = ATMPROFILE_LEVEL_PRESSURE
        AtmProfile%Layer_Pressure(:,m) = ( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) - &
                                           ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     ) / &
       !                                 -------------------------------------------------
                                         LOG( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) / &
                                              ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     )

      CASE ( 'ECMWF' )
        Error_Status = Load_ECMWF_Profile( m, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                           Description       = AtmProfile%Description(m), &
                                           Climatology_Model = AtmProfile%Climatology_Model(m), &
                                           Year              = AtmProfile%DateTime(m)%Year, &
                                           Month             = AtmProfile%DateTime(m)%Month, &
                                           Day               = AtmProfile%DateTime(m)%Day, &
                                           Hour              = AtmProfile%DateTime(m)%Hour, &
                                           Latitude          = AtmProfile%Location(m)%Latitude, &
                                           Longitude         = AtmProfile%Location(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_RCS_Id )

        AtmProfile%Level_Pressure(:,m) = ATMPROFILE_LEVEL_PRESSURE
        AtmProfile%Layer_Pressure(:,m) = ( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) - &
                                           ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     ) / &
       !                                 -------------------------------------------------
                                         LOG( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) / &
                                              ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     )

      CASE ( 'Model' )
        Error_Status = Load_Model_Profile( m, &
                                           Pressure, &
                                           Temperature, &
                                           Absorber, &
                                           Absorber_ID       = AtmProfile%Absorber_ID, &
                                           Absorber_Units_ID = AtmProfile%Absorber_Units_ID, &
                                           Description       = AtmProfile%Description(m), &
                                           Climatology_Model = AtmProfile%Climatology_Model(m), &
                                           Year              = AtmProfile%DateTime(m)%Year, &
                                           Month             = AtmProfile%DateTime(m)%Month, &
                                           Day               = AtmProfile%DateTime(m)%Day, &
                                           Hour              = AtmProfile%DateTime(m)%Hour, &
                                           Latitude          = AtmProfile%Location(m)%Latitude, &
                                           Longitude         = AtmProfile%Location(m)%Longitude, &
                                           Surface_Altitude  = AtmProfile%Location(m)%Surface_Altitude, &
                                           RCS_Id = Profile_Set_RCS_Id )

        AtmProfile%Level_Pressure(:,m) = ATMPROFILE_LEVEL_PRESSURE
        AtmProfile%Layer_Pressure(:,m) = ( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) - &
                                           ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     ) / &
       !                                 -------------------------------------------------
                                         LOG( ATMPROFILE_LEVEL_PRESSURE(1:n_Levels-1) / &
                                              ATMPROFILE_LEVEL_PRESSURE(2:n_Levels)     )

      CASE DEFAULT
        WRITE( *, '( /5x, "This option not yet implemented." )' )
        STOP

    END SELECT


    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error loading ", a, " profile # ", i3, "." )' ) &
                      TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ), m
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- INTERPOLATE THE PROFILE_DATA IF REQUIRED --              #
    !#--------------------------------------------------------------------------#

    Perform_Interpolation: IF ( Profile_Interpolate ) THEN



      !#------------------------------------------------------------------------#
      !#  -- PERFORM THE INTERPOLATION LINEAR WITH RESPECT TO LOG(PRESSURE) --  #
      !#------------------------------------------------------------------------#

      WRITE( *, '( 10x, "Interpolating..." )' )


      ! -----------------------------------------
      ! Set the AtmProfile file COMMENT attribute
      ! -----------------------------------------

      Comment = 'Original profiles interpolated to the 101 AIRS pressure levels.'


      ! ---------------------------
      ! Interpolate the temperature
      ! ---------------------------

      Error_Status = Polynomial_Interpolate( LOG( Pressure ),                       &  ! X
                                             Temperature,                           &  ! Y
                                             LOG( AtmProfile%Level_Pressure(:,m) ), &  ! XINT
                                             AtmProfile%Level_Temperature(:,m),     &  ! YINT
                                             order = 1,                             &  ! Linear
                                             RCS_Id = Interpolation_RCS_Id )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error occurred interpolating the temperature ", &
                          &"for profile #", i2, "." )' ) m
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF


      ! --------------------------------
      ! Interpolate the absorber amounts
      ! --------------------------------

      Absorber_Interpolation_Loop: DO j = 1, n_Absorbers


        ! ------------------------------
        ! Call the interpolation routine
        ! ------------------------------

        Error_Status = Polynomial_Interpolate( LOG( Pressure ),                       &  ! X
                                               Absorber( :, j ),                      &  ! Y
                                               LOG( AtmProfile%Level_Pressure(:,m) ), &  ! XINT
                                               AtmProfile%Level_Absorber( :, j, m ),  &  ! YINT
                                               order = 1                              )  ! Linear

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred interpolating the amount for absorber #", i2, &
                            &", profile #", i2, "." )' ) j, m
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF


        ! -------------------------------------------------------
        ! Check the interpolation result and correct if necessary
        ! -------------------------------------------------------

        Negative_Absorber: IF ( ANY( AtmProfile%Level_Absorber( :, j, m ) < ZERO ) ) THEN

          ! -- Output warning message
          WRITE( Message, '( "Correcting interpolated amount for absorber #", i2, &
                            &", profile #", i2, " is < 0. Inspect result." )' ) j, m
          CALL display_message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                WARNING )

          ! -- Load data into correction array
          acorr(1:n_Levels) = AtmProfile%Level_Absorber( :, j, m )
          acorr(n_Levels+1) = acorr(n_Levels)

          ! -- Loop over all levels
          Correction_Check_Loop: DO k = 1, n_Levels

            ! -- Check for non-physical absorber amount
            Correct_Absorber: IF ( acorr( k ) > ZERO ) THEN

              ! -- No correction, cycle loop
              CYCLE Correction_Check_Loop

            ELSE Correct_Absorber

              ! -- Output some info
              WRITE( Message, '( "Absorber #", i2, " amount for level ", i3, &
                                &" (",es9.3,"hPa) : ", es13.6 )' ) &
                              j, k, ATMPROFILE_LEVEL_PRESSURE(k), acorr( k )
              CALL display_message( PROGRAM_NAME, &
                                    TRIM( Message ), &
                                    WARNING )

              ! -- Set invalid value equal to next non-negative value
              Correction_Loop: DO ik = k+1, n_Levels+1
                IF ( acorr( ik ) > ZERO ) THEN
                  acorr(k) = acorr(ik)
                  EXIT Correction_Loop
                END IF
              END DO Correction_Loop

            END IF Correct_Absorber

          END DO Correction_Check_Loop

          ! -- Restore the corrected profile
          AtmProfile%Level_Absorber( :, j, m ) = acorr(1:n_Levels)

        END IF Negative_Absorber

      END DO Absorber_Interpolation_Loop



    !#--------------------------------------------------------------------------#
    !#                -- NO INTERPOLATION, JUST COPY THE DATA --                #
    !#--------------------------------------------------------------------------#

    ELSE Perform_Interpolation


      ! -----------------------------------------
      ! Set the AtmProfile file COMMENT attribute
      ! -----------------------------------------

      Comment = 'No interpolation performed.'


      ! --------------------------------------
      ! Copy the temperature and absorber data
      ! --------------------------------------

      AtmProfile%Level_Temperature(:,m)    = Temperature
      AtmProfile%Level_Absorber( :, :, m ) = Absorber

    END IF Perform_Interpolation



    !#--------------------------------------------------------------------------#
    !#            -- COMPUTE THE HEIGHTS AND THICKNESS PROFILES --              #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( 10x, "Computing geopotential heights..." )' )


    ! ---------------------------------------------------------
    ! Convert the water vapour mixing ratio to partial pressure
    ! ---------------------------------------------------------

    ! -- Is water vapor present?
    IF ( COUNT( AtmProfile%Absorber_ID == ID_H2O ) /= 1 ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'No water vapor data in profile.', &
                            FAILURE )
      STOP
    END IF

    ! -- Find the water vapor index
    j_idx = PACK( (/ ( j, j = 1, AtmProfile%n_Absorbers ) /), &
                  AtmProfile%Absorber_ID == ID_H2O )

    ! -- Convert from mixing ratio
    H2O_Pressure = MR_to_PP( ATMPROFILE_LEVEL_PRESSURE, &
                             AtmProfile%Level_Absorber( :, j_idx(1), m ) )

    ! -- Check the result
    IF ( ANY( H2O_Pressure < ZERO ) ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'Water vapor unit conversion failed.', &
                            FAILURE )
     STOP
    END IF


    ! ---------------------------------------
    ! Compute the geopotential height profile
    ! ---------------------------------------

    Error_Status = Geopotential_Height( AtmProfile%Level_Pressure(:,m),    &  ! Input
                                        AtmProfile%Level_Temperature(:,m), &  ! Input
                                        H2O_Pressure,                      &  ! Input
                                        AtmProfile%Level_Altitude(:,m),    &  ! Output
                                        Gravity_Correction = 1,                    &  ! Optional Input
                                        Latitude = AtmProfile%Location(m)%Latitude )  ! Optional Input

    IF ( Error_Status /= SUCCESS ) THEN
      CALL display_message( PROGRAM_NAME, &
                            'Error calculating geopotential height profile.', &
                            Error_Status )
      STOP
    END IF


    ! -------------------------
    ! Calculate the thicknesses
    ! -------------------------

    AtmProfile%Layer_Delta_Z(:,m) = ABS( AtmProfile%Level_Altitude(1:n_Levels-1,m) - &
                                         AtmProfile%Level_Altitude(2:n_Levels,  m)   )



    !#--------------------------------------------------------------------------#
    !#            -- CREATE SIMPLE LAYER AVERAGE AtmProfile DATA --             #
    !#--------------------------------------------------------------------------#

    WRITE( *, '( 10x, "Computing layer average values..." )' )


    ! -----------------------
    ! Average the temperature
    ! -----------------------

    Temperature_Average_loop: DO k = 1, n_Layers

      AtmProfile%Layer_Temperature(k,m) = ZEROpointFIVE * &
                                          ( AtmProfile%Level_Temperature( k,  m ) + &
                                            AtmProfile%Level_Temperature( k+1,m )   )

    END DO Temperature_Average_loop


    ! ---------------------
    ! Average the absorbers
    ! ---------------------

    j_Absorber_Average_loop: DO j = 1, n_Absorbers

      k_Absorber_Average_loop: DO k = 1, n_Layers

        AtmProfile%Layer_Absorber(k,j,m) = ZEROpointFIVE * &
                                           ( AtmProfile%Level_Absorber( k,  j,m ) + &
                                             AtmProfile%Level_Absorber( k+1,j,m )   )

      END DO k_Absorber_Average_loop

    END DO j_Absorber_Average_loop



    !#----------------------------------------------------------------------------#
    !#             -- DEALLOCATE DATA POINTER ARRAYS FOR NEXT READ --             #
    !#----------------------------------------------------------------------------#

    ! --------
    ! Pressure
    ! --------

    IF ( ASSOCIATED( Pressure ) ) THEN
      DEALLOCATE( Pressure, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "Error deallocating PRESSURE input array for profile #", i3, ". STAT = ", i5 )' ) &
                        m, Allocate_Status
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
      END IF
    END IF


    ! -----------
    ! Temperature
    ! -----------

    IF ( ASSOCIATED( Temperature ) ) THEN
      DEALLOCATE( Temperature, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "Error deallocating TEMPERATURE input array for profile #", i3, ". STAT = ", i5 )' ) &
                        m, Allocate_Status
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
      END IF
    END IF


    ! --------
    ! Absorber
    ! --------

    IF ( ASSOCIATED( Absorber ) ) THEN
      DEALLOCATE( Absorber, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = WARNING
        WRITE( Message, '( "Error deallocating ABSORBER input array for profile #", i3, ". STAT = ", i5 )' ) &
                        m, Allocate_Status
        CALL display_message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
      END IF
    END IF

  END DO Profile_Loop



  !#----------------------------------------------------------------------------#
  !#                   -- CHECK FOR DUMMY LAT/LON BEFORE WRITE --               #
  !#----------------------------------------------------------------------------#

  WHERE( ABS( AtmProfile%Location%Longitude ) > 360.0_fp_kind )
    AtmProfile%Location%Longitude = -999.0_fp_kind
  END WHERE

  WHERE( ABS( AtmProfile%Location%Latitude ) > 90.0_fp_kind )
    AtmProfile%Location%Latitude = -999.0_fp_kind
  END WHERE



  !#----------------------------------------------------------------------------#
  !#                        -- WRITE THE AtmProfile DATA --                     #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Create the netCDF file name
  ! ---------------------------

  WRITE( cLevel,   '( i3 )' ) N_ATMPROFILE_LAYERS + 1
  WRITE( cProfile, '( i3 )' ) N_ATMPROFILES( Profile_Set )

  AtmProfile_Filename = TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) )//&
                       '_'//TRIM( ADJUSTL( cLevel   ) )//'LVL'//&
                       '_'//TRIM( ADJUSTL( cProfile ) )//'ATM'//&
                       '.AtmProfile.nc'

  WRITE( *, '(/5x, "Writing the output file ", a, "..." )' ) TRIM( AtmProfile_FileNAME )


  ! -----------------------------------
  ! Create the History global attribute
  ! -----------------------------------

  IF ( Profile_Interpolate ) THEN
    History = TRIM( Profile_Set_RCS_Id ) //'; ' //&
              TRIM( Interpolation_RCS_Id ) // '; ' //&
              PROGRAM_RCS_ID
  ELSE
    History = TRIM( Profile_Set_RCS_Id ) //'; ' //&
              PROGRAM_RCS_ID
  END IF


  ! -----------------------------------
  ! Write the interpolated data to file
  ! -----------------------------------


  Error_Status = Write_AtmProfile_netCDF( TRIM( AtmProfile_Filename ), &
                                          AtmProfile, &
                                          Title   = TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) )//&
                                                    ' atmospheric profile set.', &
                                          History = TRIM( History ), &
                                          Comment = TRIM( Comment ), &
                                          ID_Tag  = TRIM( ATMPROFILE_SET_ID_TAG( Profile_Set ) ) )

  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error writing AtmProfile data to profile # ", i2, &
                      &" AtmProfile record ", a )' ) &
                    m, TRIM( AtmProfile_Filename )
    CALL display_message( PROGRAM_NAME, &
                          'Error writing AtmProfile data to '//&
                          TRIM( AtmProfile_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                 -- DO A TEST READ OF THE AtmProfile FILE --                #
  !#----------------------------------------------------------------------------#

  m = MIN( 23, AtmProfile%n_Profiles )
  WRITE( *, '( /5x, "Test reading ", a, "..." )' ) TRIM( AtmProfile_Filename )


  ! ----------------------------------------
  ! Destroy the current AtmProfile structure
  ! ----------------------------------------
 
  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying AtmProfile for test read', &
                          FAILURE )
    STOP
  END IF


  ! --------------
  ! Read a profile
  ! --------------

  Error_Status = Read_AtmProfile_netCDF( AtmProfile_Filename, &
                                         AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error test reading AtmProfile netCDF file '//&
                          TRIM( AtmProfile_Filename ), &
                          FAILURE )
    STOP
  END IF


  ! -------------------
  ! Print out some data
  ! -------------------

  WRITE( *, '( 5x, "Description for profile#", i2, " read:" )' ) m
  WRITE( *, '( a )' ) TRIM( AtmProfile%Description(m) )
  WRITE( *, '( 5x, "Date/Time for profile#", i2, " read:" )' ) m
  WRITE( *, '( i2.2, "/", i2.2, "/", i4.4, " at ", i2.2, "00UTC" )' ) &
            AtmProfile%DateTime(m)%Day, &
            AtmProfile%DateTime(m)%Month, &
            AtmProfile%DateTime(m)%Year, &
            AtmProfile%DateTime(m)%Hour
  WRITE( *, '( 5x, "Location for profile#", i2, " read:" )' ) m
  WRITE( *, '( "Lat: ", f8.3, ", Lon: ", f8.3 )' ) &
            AtmProfile%Location(m)%Latitude, &
            AtmProfile%Location(m)%Longitude



  !#----------------------------------------------------------------------------#
  !#                     -- DESTROY THE AtmProfile STRUCTURE --                 #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_AtmProfile( AtmProfile )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying AtmProfile', &
                          WARNING )
    STOP
  END IF

END PROGRAM Create_AtmProfile


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Create_AtmProfile.f90,v 1.11 2005/01/04 22:48:28 paulv Exp $
!
! $Date: 2005/01/04 22:48:28 $
!
! $Revision: 1.11 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Create_AtmProfile.f90,v $
! Revision 1.11  2005/01/04 22:48:28  paulv
! - Added model climatology profile set option.
!
! Revision 1.10  2004/11/02 20:11:14  paulv
! - Modified to use new AtmProfile modules.
!
! Revision 1.9  2003/09/09 15:28:29  paulv
! - Added some more output in the absorber correction IF block.
!
! Revision 1.8  2003/08/13 21:20:00  paulv
! - Removed conversion of H2O data. All Load() functions now supply the
!   H2O in g/kg and other quantities in ppmv.
! - Changed name of output file to XXX.AtmProfile.nc.
!
! Revision 1.7  2003/07/23 21:28:24  paulv
! - Added a profile interpolation flag that is set depending on the profile
!   set selected. For the ECMWF set, no interpolation is done - the data is
!   simply copied as it is already at 101L. The reslting datafile's comment
!   and history attributes reflect this change.
!
! Revision 1.6  2003/07/21 20:25:36  paulv
! - Added check for dummy lat/lon values before teh AtmProfile file write.
!
! Revision 1.5  2003/07/21 20:04:50  paulv
! - Updated to use new AtmProfile definition and I/O modules.
! - Added ECMWF profile set selection.
!
! Revision 1.4  2003/02/25 17:55:55  paulv
! - Added geopotential height and layer thickness calculations.
!
! Revision 1.3  2002/12/19 19:33:55  paulv
! - Added profile averaging code to output the LAYER quantities. Only a simple
!   layer average is performed.
!
! Revision 1.2  2002/07/22 19:19:36  paulv
! - Added an exceedingly brain-dead but simple method of correcting interpolated
!   absorber amounts if they are extrapolated and become negative.
!
! Revision 1.1  2002/07/22 17:10:11  paulv
! Initial checkin.
!
!
!
