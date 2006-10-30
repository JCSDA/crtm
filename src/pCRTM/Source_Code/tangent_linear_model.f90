!------------------------------------------------------------------------------
!M+
! NAME:
!       Tangent_Linear_Model
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) tangent-linear function.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Tangent_Linear_Model
!
! MODULES:
!       Type_Kinds:              Module to define kind types for variable
!                                declaration.
!
!       Message_Handler:         Module to define error codes and handle
!                                error conditions
!                                USEs: FILE_UTILITY module
!
!       Parameters:              Module containing parameter definitions for
!                                the pCRTM.
!                                USEs: TYPE_KINDS module
!
!       Spectral_Coefficients:   Module containing the spectral coefficients.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PARAMETERS module
!                                      SPCCOEFF_DEFINE module
!                                      SPCCOEFF_BINARY_IO module
!
!       Absorber_Profile:        Module containing routines for generating the
!                                absorber profiles.
!                                USEs: TYPE_KINDS module
!                                      PARAMETERS module
!
!       Predictors:              Module containing routines for generating the
!                                predictor profiles.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PARAMETERS module
!
!       Transmittance:           Module containing transmittance calculation
!                                routines.
!                                USEs: TYPE_KINDS module
!                                      PARAMETERS module
!                                      TRANSMITTANCE_COEFFICIENTS module
!
!       Radiance:                Module containing radiance calculation
!                                routines.
!                                USEs: TYPE_KINDS module
!                                      PARAMETERS module
!                                      SPECTRAL_COEFFICIENTS module
!                                      SENSOR_PLANCK_FUNCTIONS module
!
!       Forward_Model:           Module containing the pCRTM forward function.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PARAMETERS module
!                                      SPECTRAL_COEFFICIENTS module
!                                      ABSORBER_PROFILE module
!                                      PREDICTORS module
!                                      TRANSMITTANCE module
!                                      RADIANCE module
!
! CONTAINS:
!       Compute_RTM_TL:          Function that calculates the top-of-atmosphere
!                                (TOA) tangent-linear radiances and brightness 
!                                temperatures for an input atmospheric profile
!                                set and user specified satellites/channels.
!
!                                This function is simply a wrapper around both
!                                the FORWARD model and the TANGENT-LINEAR model
!                                so that the user doesn't have to declare the
!                                absorber/predictor/etc arrays in the calling
!                                routine.
!
!       Tangent_Linear_RTM:      Function that calculates top-of-atmosphere
!                                (TOA) tangent-linear radiances and brightness
!                                temperatures for user specified profiles and
!                                satellites/channels. 
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! COMMENTS:
!       All of the array documentation lists the dimensions by a single letter.
!       Throughout the RTM code these are:
!         I: Array dimension is of I predictors (Istd and Iint are variants).
!         J: Array dimension is of J absorbing species.
!         K: Array dimension is of K atmospheric layers.
!         L: Array dimension is of L spectral channels.
!         M: Array dimension is of M profiles.
!       Not all of these dimensions will appear in every module.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-July-2000
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2004 Paul van Delst
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

MODULE Tangent_Linear_Model


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds, ONLY : fp_kind
  USE Message_Handler
  USE Parameters
  USE Spectral_Coefficients

  USE Absorber_Profile, ONLY : Compute_Absorber_Amount_TL
  USE Predictors,       ONLY : Compute_Predictors_TL
  USE Transmittance,    ONLY : Compute_Transmittance_TL
  USE Radiance,         ONLY : Compute_Radiance_TL

  USE Forward_Model, ONLY: Forward_RTM 


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------

  PRIVATE
  PUBLIC :: Compute_RTM_TL
  PUBLIC :: Tangent_Linear_RTM


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE Compute_RTM_TL
    MODULE PROCEDURE Compute_RTM_TL_rank1
    MODULE PROCEDURE Compute_RTM_TL_rank2
  END INTERFACE Compute_RTM_TL

  INTERFACE Tangent_Linear_RTM
    MODULE PROCEDURE Tangent_Linear_RTM_rank1
    MODULE PROCEDURE Tangent_Linear_RTM_rank2
  END INTERFACE Tangent_linear_rtm


  ! -------------------------
  ! PRIVATE module parameters
  ! -------------------------

  CHARACTER( * ), PARAMETER :: MODULE_RCS_ID = &
    '$Id: tangent_linear_model.f90,v 2.6 2006/05/02 14:58:35 dgroff Exp $'

  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_RTM_TL
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) tangent-linear
!       radiances and brightness temperatures for an input atmospheric profile
!       set and user specified satellites/channels.
!
!       This function is simply a wrapper around both the FORWARD model and the
!       TANGENT-LINEAR model so that the user doesn't have to declare the
!       absorber/predictor/etc arrays in the calling routine.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_RTM_TL( &
!                        ! Forward inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
!                        Surface_Temperature,                                        &  ! Input, M
!                        Surface_Emissivity,                                         &  ! Input, L*M
!                        Surface_Reflectivity,                                       &  ! Input, L*M
!
!                        ! -- Tangent-linear inputs
!                        Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K x M
!                        Surface_Temperature_TL,                                     &  ! Input, M
!                        Surface_Emissivity_TL,                                      &  ! Input, L*M
!                        Surface_Reflectivity_TL,                                    &  ! Input, L*M
!
!                        ! -- Other inputs
!                        Secant_View_Angle,                                          &  ! Input, M
!                        Secant_Solar_Angle,                                         &  ! Input, M
!                        n_Channels_Per_Profile,                                     &  ! Input, M
!                        Channel_Index,                                              &  ! Input, L*M
!
!                        ! -- Forward outputs
!                        Tau,                                                        &  ! Output, K x L*M
!                        Flux_Tau,                                                   &  ! Output, K x L*M
!                        Solar_Tau,                                                  &  ! Output, K x L*M
!                        Upwelling_Radiance,                                         &  ! Output, L*M
!                        Brightness_Temperature,                                     &  ! Output, L*M
!
!                        ! -- Tangent-linear outputs
!                        Tau_TL,                                                     &  ! Output, K x L*M
!                        Flux_Tau_TL,                                                &  ! Output, K x L*M
!                        Solar_Tau_TL,                                               &  ! Output, K x L*M
!                        Upwelling_Radiance_TL,                                      &  ! Output, L*M
!                        Brightness_Temperature_TL,                                  &  ! Output, L*M
!
!                        ! -- Optional inputs
!                        Solar_Reflectivity    = Solar_Reflectivity,                 &  ! Optional input,  L*M
!                        Solar_Reflectivity_TL = Solar_Reflectivity_TL,              &  ! Optional input,  L*M
!                        Secant_Flux_Angle     = Secant_Flux_Angle,                  &  ! Optional input, M
!                        n_Input_Profiles      = n_Input_Profiles,                   &  ! Optional input, Scalar
!
!                        ! -- Error messaging
!                        Message_Log = MEssage_Log                                   ) 
!
! INPUT ARGUMENTS:
!
!       Level_P:                   Profile set layer interface pressure array. The TOA
!                                  pressure is not included. TOA pressure is parameterised
!                                  in the PARAMETERS module.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_P:                   Profile set layer average pressure array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_T:                   Profile set layer average temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_W:                   Profile set layer average water vapor mixing ratio array
!                                  UNITS:      g/kg
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_O:                   Profile set layer average ozone mixing ratio array.
!                                  UNITS:      ppmv
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:       Profile set surface temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:        Profile set surface emissivity array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:      Profile set surface reflectivity array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Level_P_TL:                Profile set layer interface pressure tangent-linear
!                                  array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_P_TL:                Profile set layer average pressure tangent-linear
!                                  array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_T_TL:                Profile set layer average temperature tangent-linear
!                                  array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_W_TL:                Profile set layer average water vapor mixing ratio
!                                  tangent-linear array.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_O_TL:                Profile set layer average ozone mixing ratio
!                                  tangent-linear array.
!                                  UNITS:      ppmv
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature_TL:    Profile set surface temperature tangent-linear array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity_TL:     Profile set surface emissivity tangent-linear array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity_TL:   Profile set surface reflectivity tangent-linear array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Secant_View_Angle:         Secant of the satellite view angle measured
!                                  from nadir for each profile in the set.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:        Secant of the solar zenith angle for each
!                                  profile in the set.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       n_Channels_Per_Profile:    The number of channels for each profile in the
!                                  set for which radiances are required.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:             Channel index id array. Each element is a unique
!                                  index to a (supported) sensor channel.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:        Profile set surface reflectivity array for the
!                                  solar term only. If not specified, the
!                                  Surface_Reflectivity argument is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Solar_Reflectivity_TL:     Profile set surface reflectivity tangent-
!                                  linear array for the solar term only. If not
!                                  specified, the Surface_Reflectivity_TL argument
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:         Secant of the angle to be used to approximate
!                                  the downwelling flux transmittance for the INFRARED
!                                  only. If not specified a default value of 5/3 (1.6666..)
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       n_Input_Profiles:          The number of profiles in the passed arrays to process.
!                                  If not specified, the default value is the SECOND dimension
!                                  of the pressure array determined using the SIZE intrinsic,
!                                  N_PROFILES. If N_INPUT_PROFILES is specified and is < 1 or
!                                  greater than N_PROFILES, the default value is set to N_PROFILES.
!                                  This argument is ignored if the input profile arrays are
!                                  vectors, i.e. a single profile.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:               Character string specifying a filename in which any
!                                  messages will be logged. If not specified, or if an
!                                  error occurs opening the log file, the default action
!                                  is to output messages to the screen.
!                                  UNITS:      N/A
!                                  TYPE:       CHARACTER( * )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!
!       Tau:                       Layer->TOA transmittance for the satellite
!                                  view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau:                  Layer->SFC transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau:                 Layer->SFC transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance:        TOA radiances for each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature:    TOA brightness temperatures corresponding
!                                  to the TOA radiances.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Tau_TL:                    Layer->TOA tangent-linear transmittance for the satellite
!                                  view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau_TL:               Layer->SFC tangent-linear transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau_TL:              Layer->SFC tangent-linear transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance_TL:     TOA tangent-linear radiances for each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature_TL: Tangent-linear temperatures corresponding to the
!                                  TOA tangent-linear radiances.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:              The return value is an integer defining the
!                                  error status. The error codes are defined in
!                                  the ERROR_HANDLER module.
!                                  If == SUCCESS the calculation was successful.
!                                     == FAILURE an unrecoverable error occurred.
!                                     == WARNING a recoverable error occurred.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:            Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
!      Forward_RTM:                Function that calculates top-of-atmosphere
!                                  (TOA) radiances and brightness temperatures
!                                  for an input atmospheric profile or profile
!                                  set and user specified satellites/channels.
!                                  SOURCE: FORWARD_MODEL module
!
!      Tangent_Linear_RTM:         Function that calculates top-of-atmosphere
!                                  (TOA) tangent-linear radiances and brightness
!                                  temperatures for an input atmospheric profile
!                                  set and user specified satellites/channels.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       See individual module function documentation.
!S-
!--------------------------------------------------------------------------------

  FUNCTION Compute_RTM_TL_rank2( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
             Surface_Temperature,                                        &  ! Input, M
             Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L*M
             ! -- Tangent-linear inputs
             Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K x M
             Surface_Temperature_TL,                                     &  ! Input, M
             Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L*M
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, M
             n_Channels_Per_Profile,                                     &  ! Input, M
             Channel_Index,                                              &  ! Input, L*M
             ! -- Forward outputs
             Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L*M
             Upwelling_Radiance,                                         &  ! Input, L*M
             Brightness_Temperature,                                     &  ! Input, L*M
             ! -- Tangent-linear outputs
             Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L*M
             Upwelling_Radiance_TL,                                      &  ! Output, L*M
             Brightness_Temperature_TL,                                  &  ! Output, L*M
             ! -- Optional inputs
             Solar_Reflectivity,  Solar_Reflectivity_TL,                 &  ! Optional input,  L*M
             Secant_Flux_Angle,                                          &  ! Optional input,  M
             n_Input_Profiles,                                           &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                               &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O                    ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature        ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity         ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity       ! L*M

    ! -- Tangent-linear inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O_TL                 ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature_TL     ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity_TL      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity_TL    ! L*M

    ! -- Other inputs
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_View_Angle          ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_Solar_Angle         ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: n_Channels_Per_Profile     ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: Channel_Index              ! L*M

    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau                        ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau                   ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau                  ! K x L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance         ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature     ! L*M

    ! -- Tangent-linear outputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau_TL                     ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau_TL                ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau_TL               ! K x L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance_TL      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature_TL  ! L*M

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity         ! L*M
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity_TL      ! L*M
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Secant_Flux_Angle          ! M
    INTEGER,                         OPTIONAL, INTENT( IN )  :: n_Input_Profiles           ! Scalar

    ! -- Error messaging
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM_TL(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Error_Status_TL

    ! -- Array for integrated Absorber amounts, 0:K x J x M
    REAL( fp_kind ), DIMENSION( 0:SIZE( Layer_P, DIM = 1 ), &
                                  MAX_N_ABSORBERS,          &
                                  SIZE( Layer_P, DIM = 2 )  ) :: Absorber

    ! -- Arrays for predictors, Imax x K x M
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( Layer_P, DIM = 1 ), &
                                SIZE( Layer_P, DIM = 2 )  ) :: Tau_Predictor,      &
                                                               Flux_Tau_Predictor, &
                                                               Solar_Tau_Predictor

    ! -- Array for layer Planck radiance term, K x L*M
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P, DIM = 1 ),  &
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance, &
                                                                Layer_Radiance_TL
      

    ! -- Array for downwelling radiance (flux + solar), L*M
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance, &
                                                                Downwelling_Radiance_TL



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE FORWARD RADIANCES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Forward_RTM( &
                     ! -- Forward inputs
                     Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input,  K x M
                     Surface_Temperature,                                    &  ! Input,  M
                     Surface_Emissivity, Surface_Reflectivity,               &  ! Input,  L*M
                     ! -- Other inputs
                     Secant_View_Angle,                                      &  ! Input,  M
                     Secant_Solar_Angle,                                     &  ! Input,  M
                     n_Channels_Per_Profile,                                 &  ! Input,  M
                     Channel_Index,                                          &  ! Input,  L*M
                     ! -- Outputs
                     Absorber,                                               &  ! Output, 0:K x J x M
                     Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor, &  ! Output, Imax x K x M
                     Tau, Flux_Tau, Solar_Tau,                               &  ! Output, K x L*M
                     Layer_Radiance,                                         &  ! Output, K x L*M
                     Downwelling_Radiance, Upwelling_Radiance,               &  ! Output, L*M
                     Brightness_Temperature,                                 &  ! Output, L*M
                     ! -- Optional inputs
                     Solar_Reflectivity = Solar_Reflectivity,                &  ! Optional input,  L*M
                     Secant_Flux_Angle  = Secant_Flux_Angle,                 &  ! Optional input,  M
                     n_Input_Profiles   = n_Input_Profiles,                  &  ! Optional input,  Scalar
                     Message_Log        = Message_Log                        )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    IF ( Error_Status == FAILURE ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occured in Forward_RTM', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- COMPUTE THE TANGENT-LINEAR RADIANCES AND TEMPERATURES --        #
    !#--------------------------------------------------------------------------#

    Error_Status_TL = Tangent_Linear_RTM_rank2( &
                        ! -- Forward inputs
                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
                        Surface_Temperature,                                        &  ! Input, M
                        Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L*M
                        Absorber,                                                   &  ! Input, 0:K x J x M
                        Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor,     &  ! Input, Imax x K x M
                        Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L*M
                        Layer_Radiance,                                             &  ! Input, K x L*M
                        Downwelling_Radiance, Upwelling_Radiance,                   &  ! Input, L*M
                        ! -- Tangent-linear inputs
                        Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K x M
                        Surface_Temperature_TL,                                     &  ! Input, M
                        Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L*M
                        ! -- Other inputs
                        Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, M
                        n_Channels_Per_Profile,                                     &  ! Input, M
                        Channel_Index,                                              &  ! Input, L*M
                        ! -- Tangent-linear outputs
                        Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L*M
                        Layer_Radiance_TL,                                          &  ! Output, K x L*M
                        Downwelling_Radiance_TL, Upwelling_Radiance_TL,             &  ! Output, L*M
                        Brightness_Temperature_TL,                                  &  ! Output, L*M
                        ! -- Optional inputs
                        Solar_Reflectivity    = Solar_Reflectivity,                 &  ! Optional input,  L*M
                        Solar_Reflectivity_TL = Solar_Reflectivity_TL,              &  ! Optional input,  L*M
                        Secant_Flux_Angle     = Secant_Flux_Angle,                  &  ! Optional input, M
                        n_Input_Profiles      = n_Input_Profiles,                   &  ! Optional input, Scalar
                        Message_Log           = Message_Log                         )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    SELECT CASE ( Error_Status_TL )
      CASE ( FAILURE )
        Error_Status= Error_Status_TL
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occured in Tangent_Linear_RTM(Rank-1)', &
                              Error_Status, &
                              Message_Log = Message_Log )

      CASE ( WARNING )
        Error_Status= Error_Status_TL

      CASE DEFAULT
        ! -- Result is SUCCESS, so do nothing to
        ! -- pass on result of Forward_RTM

    END SELECT

  END FUNCTION Compute_RTM_TL_rank2





  FUNCTION Compute_RTM_TL_rank1( &
             ! Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K
             Surface_Temperature,                                        &  ! Input, Scalar
             Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L
             ! -- Tangent-linear inputs
             Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K
             Surface_Temperature_TL,                                     &  ! Input, Scalar
             Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, Scalar
             n_Channels,                                                 &  ! Input, Scalar
             Channel_Index,                                              &  ! Input, L
             ! -- Forward output
             Tau, Flux_Tau, Solar_Tau,                                   &  ! Output, K x L
             Upwelling_Radiance,                                         &  ! Output, L
             Brightness_Temperature,                                     &  ! Output, L
             ! -- Tangent-linear outputs
             Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L
             Upwelling_Radiance_TL,                                      &  ! Output, L
             Brightness_Temperature_TL,                                  &  ! Output, L
             ! -- Optional inputs
             Solar_Reflectivity, Solar_Reflectivity_TL,                  &  ! Optional input,  L
             Secant_Flux_Angle,                                          &  ! Optional input,  Scalar
             n_Input_Profiles,                                           &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                               &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Level_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_T                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_W                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_O                    ! K

    REAL( fp_kind ),                               INTENT( IN )  :: Surface_Temperature        ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Emissivity         ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Reflectivity       ! L

    ! -- Tangent-linear inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Level_P_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_P_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_T_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_W_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_O_TL                 ! K

    REAL( fp_kind ),                               INTENT( IN )  :: Surface_Temperature_TL     ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Emissivity_TL      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Reflectivity_TL    ! L

    ! -- Other inputs
    REAL( fp_kind ),                               INTENT( IN )  :: Secant_View_Angle          ! Scalar
    REAL( fp_kind ),                               INTENT( IN )  :: Secant_Solar_Angle         ! Scalar
    INTEGER,                                       INTENT( IN )  :: n_Channels                 ! Scalar
    INTEGER,                   DIMENSION( : ),     INTENT( IN )  :: Channel_Index              ! L

    ! -- Forward outputs
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Tau                        ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Flux_Tau                   ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Solar_Tau                  ! K x L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Upwelling_Radiance         ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Brightness_Temperature     ! L

    ! -- Tangent-linear outputs
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Tau_TL                     ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Flux_Tau_TL                ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Solar_Tau_TL               ! K x L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Upwelling_Radiance_TL      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Brightness_Temperature_TL  ! L

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )  :: Solar_Reflectivity         ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )  :: Solar_Reflectivity_TL      ! L
    REAL( fp_kind ), OPTIONAL,                     INTENT( IN )  :: Secant_Flux_Angle          ! Scalar
    INTEGER,         OPTIONAL,                     INTENT( IN )  :: n_Input_Profiles           ! Scalar

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                     INTENT( IN )  :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM_TL(Rank-1)'



    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Error_Status_TL

    ! -- Array for integrated Absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( Layer_P, DIM = 1 ), &
                                  MAX_N_ABSORBERS           ) :: Absorber

    ! -- Arrays for predictors, Imax x K
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( Layer_P, DIM = 1 )  ) :: Tau_Predictor,      &
                                                               Flux_Tau_Predictor, &
                                                               Solar_Tau_Predictor

    ! -- Array for layer Planck radiance term, K x L
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P, DIM = 1 ),  &
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance, &
                                                                Layer_Radiance_TL
      

    ! -- Array for downwelling radiance (flux + solar), L
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance, &
                                                                Downwelling_Radiance_TL



    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FORWARD RADIANCES AND TEMPERATURES --             #
    !#--------------------------------------------------------------------------#

    Error_Status = Forward_RTM( &
                     ! -- Forward inputs
                     Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input,  K
                     Surface_Temperature,                                    &  ! Input,  Scalar
                     Surface_Emissivity, Surface_Reflectivity,               &  ! Input,  L
                     ! -- Other inputs
                     Secant_View_Angle,                                      &  ! Input,  Scalar
                     Secant_Solar_Angle,                                     &  ! Input,  Scalar
                     n_Channels,                                             &  ! Input,  Scalar
                     Channel_Index,                                          &  ! Input,  L
                     ! -- Outputs
                     Absorber,                                               &  ! Output, 0:K x J
                     Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor, &  ! Output, Imax x K
                     Tau, Flux_Tau, Solar_Tau,                               &  ! Output, K x L
                     Layer_Radiance,                                         &  ! Output, K x L
                     Downwelling_Radiance, Upwelling_Radiance,               &  ! Output, L
                     Brightness_Temperature,                                 &  ! Output, L
                     ! -- Optional inputs
                     Solar_Reflectivity = Solar_Reflectivity,                &  ! Optional input,  L
                     Secant_Flux_Angle  = Secant_Flux_Angle,                 &  ! Optional input,  Scalar
                     Message_Log = Message_Log                               )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    IF ( Error_Status == FAILURE ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error occured in Forward_RTM', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- COMPUTE THE TANGENT-LINEAR RADIANCES AND TEMPERATURES --        #
    !#--------------------------------------------------------------------------#

    Error_Status_TL = Tangent_Linear_RTM_rank1( &
                        ! -- Forward inputs
                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K
                        Surface_Temperature,                                        &  ! Input, Scalar
                        Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L
                        Absorber,                                                   &  ! Input, 0:K x J
                        Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor,     &  ! Input, Imax x K
                        Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L
                        Layer_Radiance,                                             &  ! Input, K x L
                        Downwelling_Radiance, Upwelling_Radiance,                   &  ! Input, L
                        ! -- Tangent-linear inputs
                        Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K
                        Surface_Temperature_TL,                                     &  ! Input, Scalar
                        Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L
                        ! -- Other inputs
                        Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, Scalar
                        n_Channels,                                                 &  ! Input, Scalar
                        Channel_Index,                                              &  ! Input, L
                        ! -- Tangent-linear outputs
                        Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L
                        Layer_Radiance_TL,                                          &  ! Output, K x L
                        Downwelling_Radiance_TL, Upwelling_Radiance_TL,             &  ! Output, L
                        Brightness_Temperature_TL,                                  &  ! Output, L
                        ! -- Optional inputs
                        Solar_Reflectivity    = Solar_Reflectivity,                 &  ! Optional input,  L
                        Solar_Reflectivity_TL = Solar_Reflectivity_TL,              &  ! Optional input,  L
                        Secant_Flux_Angle     = Secant_Flux_Angle,                  &  ! Optional input, Scalar
                        n_Input_Profiles      = n_Input_Profiles,                   &  ! Optional input, Scalar
                        Message_Log           = Message_Log                         )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    SELECT CASE ( Error_Status_TL )
      CASE ( FAILURE )
        Error_Status= Error_Status_TL
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occured in Tangent_Linear_RTM(Rank-1)', &
                              Error_Status, &
                              Message_Log = Message_Log )

      CASE ( WARNING )
        Error_Status= Error_Status_TL

      CASE DEFAULT
        ! -- Result is SUCCESS, so do nothing to
        ! -- pass on result of Forward_RTM

    END SELECT

  END FUNCTION Compute_RTM_TL_rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Tangent_Linear_RTM
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) tangent-linear
!       radiances and brightness temperatures for an input atmospheric profile
!       set and user specified satellites/channels.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
!       Error_Status = Tangent_Linear_RTM( &
!                        ! Forward inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
!                        Surface_Temperature,                                        &  ! Input, M
!                        Surface_Emissivity,                                         &  ! Input, L*M
!                        Surface_Reflectivity,                                       &  ! Input, L*M
!                        Absorber,                                                   &  ! Input, 0:K x J x M
!                        Tau_Predictor,                                              &  ! Input, Imax x K x M
!                        Flux_Tau_Predictor,                                         &  ! Input, Imax x K x M
!                        Solar_Tau_Predictor,                                        &  ! Input, Imax x K x M
!                        Tau,                                                        &  ! Input, K x L*M
!                        Flux_Tau,                                                   &  ! Input, K x L*M
!                        Solar_Tau,                                                  &  ! Input, K x L*M
!                        Layer_Radiance,                                             &  ! Input, K x L*M
!                        Downwelling_Radiance,                                       &  ! Input, L*M
!                        Upwelling_Radiance,                                         &  ! Input, L*M
!
!                        ! -- Tangent-linear inputs
!                        Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K x M
!                        Surface_Temperature_TL,                                     &  ! Input, M
!                        Surface_Emissivity_TL,                                      &  ! Input, L*M
!                        Surface_Reflectivity_TL,                                    &  ! Input, L*M
!
!                        ! -- Other inputs
!                        Secant_View_Angle,                                          &  ! Input, M
!                        Secant_Solar_Angle,                                         &  ! Input, M
!                        n_Channels_Per_Profile,                                     &  ! Input, M
!                        Channel_Index,                                              &  ! Input, L*M
!
!                        ! -- Tangent-linear outputs
!                        Tau_TL,                                                     &  ! Output, K x L*M
!                        Flux_Tau_TL,                                                &  ! Output, K x L*M
!                        Solar_Tau_TL,                                               &  ! Output, K x L*M
!                        Layer_Radiance_TL,                                          &  ! Output, K x L*M
!                        Downwelling_Radiance_TL,                                    &  ! Output, L*M
!                        Upwelling_Radiance_TL,                                      &  ! Output, L*M
!                        Brightness_Temperature_TL,                                  &  ! Output, L*M
!
!                        ! -- Optional inputs
!                        Solar_Reflectivity    = Solar_Reflectivity,                 &  ! Optional input,  L*M
!                        Solar_Reflectivity_TL = Solar_Reflectivity_TL,              &  ! Optional input,  L*M
!                        Secant_Flux_Angle     = Secant_Flux_Angle,                  &  ! Optional input, M
!                        n_Input_Profiles      = n_Input_Profiles,                   &  ! Optional input, Scalar
!
!                        ! -- Error messaging
!                        Message_Log = Message_Log                                   ) 
!                             
! INPUT ARGUMENTS:
!
!       Level_P:                   Profile set layer interface pressure array. The TOA
!                                  pressure is not included. TOA pressure is parameterised
!                                  in the PARAMETERS module.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_P:                   Profile set layer average pressure array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_T:                   Profile set layer average temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_W:                   Profile set layer average water vapor mixing ratio array
!                                  UNITS:      g/kg
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_O:                   Profile set layer average ozone mixing ratio array.
!                                  UNITS:      ppmv
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:       Profile set surface temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:        Profile set surface emissivity array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:      Profile set surface reflectivity array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Absorber:                  Array of absorber amount for nadir view.
!                                  UNITS:      Absorber dependent.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (0:K x J) or Rank-3 (0:K x J x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau_Predictor:             Predictor profiles for the layer->TOA transmittance.
!                                  UNITS:      Predictor dependent
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau_Predictor:        Predictor profiles for the thermal flux transmittance.
!                                  UNITS:      Predictor dependent
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau_Predictor:       Predictor profiles for the solar transmittance.
!                                  UNITS:      Predictor dependent
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau:                       Layer->TOA transmittance for the satellite
!                                  view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau:                  Layer->SFC transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau:                 Layer->SFC transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_Radiance:            Layer Planck radiances at every layer for
!                                  each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Downwelling_Radiance:      TOA->SFC radiances for each channel/profile due
!                                  to thermal flux and solar components.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Upwelling_Radiance:        TOA radiances for each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Level_P_TL:                Profile set layer interface pressure tangent-linear
!                                  array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_P_TL:                Profile set layer average pressure tangent-linear
!                                  array.
!                                  UNITS:      hPa
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_T_TL:                Profile set layer average temperature tangent-linear
!                                  array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_W_TL:                Profile set layer average water vapor mixing ratio
!                                  tangent-linear array.
!                                  UNITS:      g/kg
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_O_TL:                Profile set layer average ozone mixing ratio
!                                  tangent-linear array.
!                                  UNITS:      ppmv
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature_TL:    Profile set surface temperature tangent-linear array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity_TL:     Profile set surface emissivity tangent-linear array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity_TL:   Profile set surface reflectivity tangent-linear array
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Secant_View_Angle:         Secant of the satellite view angle measured
!                                  from nadir for each profile in the set.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:        Secant of the solar zenith angle for each
!                                  profile in the set.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       n_Channels_Per_Profile:    The number of channels for each profile in the
!                                  set for which radiances are required.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:             Channel index id array. Each element is a unique
!                                  index to a (supported) sensor channel.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:        Profile set surface reflectivity array for the
!                                  solar term only. If not specified, the
!                                  Surface_Reflectivity argument is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Solar_Reflectivity_TL:     Profile set surface reflectivity tangent-
!                                  linear array for the solar term only. If not
!                                  specified, the Surface_Reflectivity_TL argument
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:         Secant of the angle to be used to approximate
!                                  the downwelling flux transmittance for the INFRARED
!                                  only. If not specified a default value of 5/3 (1.6666..)
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )( fp_kind )
!                                  DIMENSION:  Scalar or Rank-1 (M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       n_Input_Profiles:          The number of profiles in the passed arrays to process.
!                                  If not specified, the default value is the SECOND dimension
!                                  of the pressure array determined using the SIZE intrinsic,
!                                  N_PROFILES. If N_INPUT_PROFILES is specified and is < 1 or
!                                  greater than N_PROFILES, the default value is set to N_PROFILES.
!                                  This argument is ignored if the input profile arrays are
!                                  vectors, i.e. a single profile.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:               Character string specifying a filename in which any
!                                  messages will be logged. If not specified, or if an
!                                  error occurs opening the log file, the default action
!                                  is to output messages to the screen.
!                                  UNITS:      N/A
!                                  TYPE:       CHARACTER( * )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!
!       Tau_TL:                    Layer->TOA tangent-linear transmittance for the satellite
!                                  view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau_TL:               Layer->SFC tangent-linear transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau_TL:              Layer->SFC tangent-linear transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Layer_Radiance_TL:         Layer Planck tangent-linear radiances at every layer for
!                                  each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Downwelling_Radiance_TL:   TOA->SFC tangent-linear radiances for each channel/profile due
!                                  to thermal flux and solar components.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance_TL:     TOA tangent-linear radiances for each channel/profile.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature_TL: Tangent-linear temperatures corresponding to the
!                                  TOA tangent-linear radiances.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:              The return value is an integer defining the
!                                  error status. The error codes are defined in
!                                  the ERROR_HANDLER module.
!                                  If == SUCCESS the calculation was successful.
!                                     == FAILURE an unrecoverable error occurred.
!                                     == WARNING a recoverable error occurred.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:            Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
!      Get_Max_n_Channels:         Routine to retrieve the value of the
!                                  MAX_N_CHANNELS "pseudo-parameter".
!                                  SOURCE: PARAMETERS module
!
!      Compute_Absorber_Amount_TL: Subroutine to compute the tangent-linear
!                                  absorber profiles
!                                  SOURCE: ABSORBER_PROFILE module
!
!      Compute_Predictors_TL:      Subroutine to compute the tangent-linear 
!                                  transmittance predictor profiles.
!                                  SOURCE: PREDICTOR module
!
!      Compute_Transmittance_TL:   Subroutine to compute the tangent-linear
!                                  transmittance profiles.
!                                  SOURCE: TRANSMITTANCE module
!
!      Compute_Radiance_TL:        Subroutine to compute the TOA tangent-linear 
!                                  radiances and brightness temperatures.
!                                  SOURCE: RADIANCE module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION Tangent_Linear_RTM_rank2( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
             Surface_Temperature,                                        &  ! Input, M
             Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L*M
             Absorber,                                                   &  ! Input, 0:K x J x M
             Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor,     &  ! Input, Imax x K x M
             Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L*M
             Layer_Radiance,                                             &  ! Input, K x L*M
             Downwelling_Radiance, Upwelling_Radiance,                   &  ! Input, L*M
             ! -- Tangent-linear inputs
             Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K x M
             Surface_Temperature_TL,                                     &  ! Input, M
             Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L*M
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, M
             n_Channels_Per_Profile,                                     &  ! Input, M
             Channel_Index,                                              &  ! Input, L*M
             ! -- Tangent-linear outputs
             Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L*M
             Layer_Radiance_TL,                                          &  ! Output, K x L*M
             Downwelling_Radiance_TL, Upwelling_Radiance_TL,             &  ! Output, L*M
             Brightness_Temperature_TL,                                  &  ! Output, L*M
             ! -- Optional inputs
             Solar_Reflectivity,  Solar_Reflectivity_TL,                 &  ! Optional input,  L*M
             Secant_Flux_Angle,                                          &  ! Optional input,  M
             n_Input_Profiles,                                           &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                               &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W                    ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O                    ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature        ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity         ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity       ! L*M

    REAL( fp_kind ), DIMENSION( 0:, :, : ),    INTENT( IN )  :: Absorber                   ! 0:K x J x M

    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( IN )  :: Tau_Predictor              ! Imax x K x M
    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( IN )  :: Flux_Tau_Predictor         ! Imax x K x M
    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( IN )  :: Solar_Tau_Predictor        ! Imax x K x M

    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Tau                        ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Flux_Tau                   ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Solar_Tau                  ! K x L*M

    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_Radiance             ! K x L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Downwelling_Radiance       ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Upwelling_Radiance         ! L*M

    ! -- Tangent-linear inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W_TL                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O_TL                 ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature_TL     ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity_TL      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity_TL    ! L*M

    ! -- Other inputs
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_View_Angle          ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_Solar_Angle         ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: n_Channels_Per_Profile     ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: Channel_Index              ! L*M

    ! -- Tangent-linear outputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau_TL                     ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau_TL                ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau_TL               ! K x L*M

    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Layer_Radiance_TL          ! K x L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Downwelling_Radiance_TL    ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance_TL      ! L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature_TL  ! L*M

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity      ! L*M
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity_TL   ! L*M
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Secant_Flux_Angle       ! M
    INTEGER,                         OPTIONAL, INTENT( IN )  :: n_Input_Profiles        ! Scalar

    ! -- Error messaging
    CHARACTER( * ),                  OPTIONAL, INTENT( IN )  :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Tangent_Linear_RTM(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_In, Value_Allowed

    INTEGER :: m, n_Profiles          ! Profile loop/index variables
    INTEGER :: l, l1, l2, n_Channels  ! Channel loop/index variables

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: Is_Set

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity    ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity_TL ) ) :: Solar_Reflectivity_Used_TL
    REAL( fp_kind ), DIMENSION( SIZE( Secant_View_Angle       ) ) :: Secant_Flux_Angle_Used



    !#--------------------------------------------------------------------------#
    !#          -- DETERMINE ARRAY DIMENSIONS AND CHECK THE VALUES --           #
    !#--------------------------------------------------------------------------#

    ! ----------------------------
    ! Check the number of profiles
    ! ----------------------------

    ! -- Number of atmospheric profiles. Default size is the SECOND
    ! -- dimension of the LAYER_P argument.

    n_Profiles = SIZE( Layer_P, DIM = 2 )
    IF ( PRESENT( n_Input_Profiles ) ) THEN
      IF ( n_Input_Profiles > 0 .AND. n_Input_Profiles <= n_Profiles ) THEN
        n_Profiles = n_Input_Profiles
      ELSE
        Error_Status = WARNING
        WRITE( Message, '( "Invalid N_INPUT_PROFILES value: ", i5, &
                          &". Using pressure array dimension value of ", i5, "." )' ) &
                        n_Input_Profiles, n_Profiles
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
      END IF
    END IF


    ! -- Check that the number of profiles is not greater than
    ! -- MAX_N_PROFILES. This is simply a limit to restrict the
    ! -- size of the input arrays so they're not TOO big.

    IF ( n_Profiles > MAX_N_PROFILES ) THEN

      Error_Status = FAILURE
      WRITE( Value_In,      '( i5 )' ) n_Profiles
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of passed profiles ('// &
                            TRIM( ADJUSTL( Value_In ) )// &
                            ') > maximum number of profiles allowed ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN

    END IF


    ! ----------------------------
    ! Check the number of channels
    ! ----------------------------

    ! -- Check for a negative number of channels

    IF ( ANY( n_Channels_Per_Profile < 0 ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Elements of N_CHANNELS_PER_PROFILE are negative.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Retrieve the maximum number of channels
    ! -- set during the model initialisation

    CALL Get_Max_n_Channels( MAX_N_CHANNELS, Is_Set )

    IF ( .NOT. Is_Set ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'MAX_N_CHANNELS value not set. Check that RTM is initialised.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -- Check that the maximum number of channels for any profile
    ! -- is not greater than than the number of channels with which
    ! -- the model was initialised

    n_Channels = MAXVAL( n_Channels_Per_Profile )

    IF ( n_Channels > MAX_N_CHANNELS ) THEN
      Error_Status = FAILURE
      WRITE( Value_In,      '( i5 )' ) n_Channels
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_CHANNELS
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of requested channels ('// &
                            TRIM( ADJUSTL( Value_In ) )// &
                            ') > number of initialisation channels ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- SET THE OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Solar reflectivities
    ! --------------------

    IF ( PRESENT( Solar_Reflectivity ) ) THEN
      Solar_Reflectivity_Used = Solar_Reflectivity
    ELSE
      Solar_Reflectivity_Used = Surface_Reflectivity
    END IF


    IF ( PRESENT( Solar_Reflectivity_TL ) ) THEN
      Solar_Reflectivity_Used_TL = Solar_Reflectivity_TL
    ELSE
      Solar_Reflectivity_Used_TL = Surface_Reflectivity_TL
    END IF


    ! ----------
    ! Flux angle
    ! ----------

    IF ( PRESENT( Secant_Flux_Angle ) ) THEN
      Secant_Flux_Angle_Used = Secant_Flux_Angle
    ELSE
      Secant_Flux_Angle_Used = SECANT_DIFFUSIVITY_ANGLE
    END IF



    !#--------------------------------------------------------------------------#
    !#                           -- PROFILE LOOP --                             #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! Initialise channel index finder
    ! -------------------------------

    l1 = 1


    ! ------------------
    ! Begin profile loop
    ! ------------------

    Profile_Loop: DO m = 1, n_Profiles


      ! -----------------------------------------------
      ! Check for the "no channel" case. If no channels
      ! required for this profile, go to the next one.
      ! -----------------------------------------------

      IF ( n_Channels_Per_Profile( m ) == 0 ) CYCLE Profile_Loop


      ! -------------------------------------------
      ! Determine the end channel index index range
      ! -------------------------------------------

      l2 = l1 + n_Channels_Per_Profile( m ) - 1


      ! ------------------------
      ! Call the rank-1 function
      ! ------------------------

      Error_Status = Tangent_Linear_RTM_rank1( &
                       ! -- Forward inputs
                       Level_P( :, m ), Layer_P( :, m ), Layer_T( :, m ), Layer_W( :, m ), Layer_O( :, m ), &  ! Input, K
                       Surface_Temperature( m ),           &  ! Input,  Scalar
                       Surface_Emissivity( l1:l2 ),        &  ! Input,  L
                       Surface_Reflectivity( l1:l2 ),      &  ! Input,  L
                       Absorber( 0:, :, m ),               &  ! Input, 0:K x J
                       Tau_Predictor( :, :, m ),           &  ! Input, Imax x K
                       Flux_Tau_Predictor( :, :, m ),      &  ! Input, Imax x K
                       Solar_Tau_Predictor( :, :, m ),     &  ! Input, Imax x K
                       Tau( :, l1:l2 ),                    &  ! Input, K x L
                       Flux_Tau( :, l1:l2 ),               &  ! Input, K x L
                       Solar_Tau( :, l1:l2 ),              &  ! Input, K x L
                       Layer_Radiance( :, l1:l2 ),         &  ! Input, K x L
                       Downwelling_Radiance( l1:l2 ),      &  ! Input, L
                       Upwelling_Radiance( l1:l2 ),        &  ! Input, L
                       ! -- Tangent-linear inputs
                       Level_P_TL( :, m ), Layer_P_TL( :, m ), Layer_T_TL( :, m ), Layer_W_TL( :, m ), Layer_O_TL( :, m ), &  ! Input, K
                       Surface_Temperature_TL( m ),        &  ! Input, Scalar
                       Surface_Emissivity_TL( l1:l2 ),     &  ! Input, L
                       Surface_Reflectivity_TL( l1:l2 ),   &  ! Input, L
                       ! -- Other inputs
                       Secant_View_Angle( m ),             &  ! Input,  Scalar
                       Secant_Solar_Angle( m ),            &  ! Input,  Scalar
                       n_Channels_Per_Profile( m ),        &  ! Input,  Scalar
                       Channel_Index( l1:l2 ),             &  ! Input,  L
                       ! -- Tangent-linear outputs
                       Tau_TL( :, l1:l2 ),                 &  ! Output, K x L*M
                       Flux_Tau_TL( :, l1:l2 ),            &  ! Output, K x L*M
                       Solar_Tau_TL( :, l1:l2 ),           &  ! Output, K x L*M
                       Layer_Radiance_TL( :, l1:l2 ),      &  ! Output, K x L*M
                       Downwelling_Radiance_TL( l1:l2 ),   &  ! Output, L*M
                       Upwelling_Radiance_TL( l1:l2 ),     &  ! Output, L*M
                       Brightness_Temperature_TL( l1:l2 ), &  ! Output, L*M
                       ! -- Optional inputs
                       Solar_Reflectivity    = Solar_Reflectivity_Used( l1:l2 ),    &  ! Input,  L
                       Solar_Reflectivity_TL = Solar_Reflectivity_Used_TL( l1:l2 ), &  ! Input,  L
                       Secant_Flux_Angle     = Secant_Flux_Angle_Used( m ),         &  ! Input,  Scalar

                       ! -- Error messaging
                       Message_Log = Message_Log )


      ! -------------------------------
      ! Check for successful completion
      ! -------------------------------

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error occured in Tangent_Linear_RTM(Rank-1) for profile #", i5 )' ) m
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF


      ! ------------------------------
      ! Update the channel begin index
      ! ------------------------------

      l1 = l2 + 1

    END DO Profile_Loop

  END FUNCTION Tangent_Linear_RTM_rank2


  FUNCTION Tangent_Linear_RTM_rank1( &
             ! Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K
             Surface_Temperature,                                        &  ! Input, Scalar
             Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L
             Absorber,                                                   &  ! Input, 0:K x J
             Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor,     &  ! Input, Imax x K
             Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L
             Layer_Radiance,                                             &  ! Input, K x L
             Downwelling_Radiance, Upwelling_Radiance,                   &  ! Input, L
             ! -- Tangent-linear inputs
             Level_P_TL, Layer_P_TL, Layer_T_TL, Layer_W_TL, Layer_O_TL, &  ! Input, K
             Surface_Temperature_TL,                                     &  ! Input, Scalar
             Surface_Emissivity_TL, Surface_Reflectivity_TL,             &  ! Input, L
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                      &  ! Input, Scalar
             n_Channels,                                                 &  ! Input, Scalar
             Channel_Index,                                              &  ! Input, L
             ! -- Tangent-linear outputs
             Tau_TL, Flux_Tau_TL, Solar_Tau_TL,                          &  ! Output, K x L
             Layer_Radiance_TL,                                          &  ! Output, K x L
             Downwelling_Radiance_TL, Upwelling_Radiance_TL,             &  ! Output, L
             Brightness_Temperature_TL,                                  &  ! Output, L
             ! -- Optional inputs
             Solar_Reflectivity, Solar_Reflectivity_TL,                  &  ! Optional input,  L
             Secant_Flux_Angle,                                          &  ! Optional input,  Scalar
             n_Input_Profiles,                                           &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                               &

           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Level_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_T                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_W                    ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_O                    ! K

    REAL( fp_kind ),                               INTENT( IN )  :: Surface_Temperature        ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Emissivity         ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Reflectivity       ! L

    REAL( fp_kind ),           DIMENSION( 0:, : ), INTENT( IN )  :: Absorber                   ! 0:K x J

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Tau_Predictor              ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Flux_Tau_Predictor         ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Solar_Tau_Predictor        ! Imax x K

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Tau                        ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Flux_Tau                   ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Solar_Tau                  ! K x L

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )  :: Layer_Radiance             ! K x L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Downwelling_Radiance       ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Upwelling_Radiance         ! L

    ! -- Tangent-linear inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Level_P_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_P_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_T_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_W_TL                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_O_TL                 ! K

    REAL( fp_kind ),                               INTENT( IN )  :: Surface_Temperature_TL     ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Emissivity_TL      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Reflectivity_TL    ! L

    ! -- Other inputs
    REAL( fp_kind ),                               INTENT( IN )  :: Secant_View_Angle          ! Scalar
    REAL( fp_kind ),                               INTENT( IN )  :: Secant_Solar_Angle         ! Scalar
    INTEGER,                                       INTENT( IN )  :: n_Channels                 ! Scalar
    INTEGER,                   DIMENSION( : ),     INTENT( IN )  :: Channel_Index              ! L

    ! -- Tangent-linear outputs
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Tau_TL                     ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Flux_Tau_TL                ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Solar_Tau_TL               ! K x L

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Layer_Radiance_TL          ! K x L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Downwelling_Radiance_TL    ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Upwelling_Radiance_TL      ! L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Brightness_Temperature_TL  ! L

    ! -- Optional input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )  :: Solar_Reflectivity         ! L
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )  :: Solar_Reflectivity_TL      ! L
    REAL( fp_kind ), OPTIONAL,                     INTENT( IN )  :: Secant_Flux_Angle          ! Scalar
    INTEGER,         OPTIONAL,                     INTENT( IN )  :: n_Input_Profiles           ! Scalar

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                     INTENT( IN )  :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Tangent_Linear_RTM(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 256 ) :: Message

    INTEGER :: k, n_Layers  ! Layer loop/index variables
    INTEGER :: l            ! Channel loop/index variable

    INTEGER :: Valid_Solar

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: Is_Set

    ! -- Arrays for integrated absorber amounts
    REAL( fp_kind ), DIMENSION( 0:SIZE( Absorber, DIM = 1 )-1, &
                                  SIZE( Absorber, DIM = 2 )    ) :: Tau_Absorber,      &
                                                                    Flux_Tau_Absorber, &
                                                                    Solar_Tau_Absorber

    REAL( fp_kind ), DIMENSION( 0:SIZE( Absorber, DIM = 1 )-1, &
                                  SIZE( Absorber, DIM = 2 )    ) :: Absorber_TL, &
                                                                    Tau_Absorber_TL,      &
                                                                    Flux_Tau_Absorber_TL, &
                                                                    Solar_Tau_Absorber_TL

    ! -- Arrays for tangent-linear predictors, Imax x K
    REAL( fp_kind ), DIMENSION( SIZE( Tau_Predictor, DIM = 1 ), &
                                SIZE( Tau_Predictor, DIM = 2 )  ) :: Tau_Predictor_TL,      &
                                                                     Flux_Tau_Predictor_TL, &
                                                                     Solar_Tau_Predictor_TL

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity    ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity_TL ) ) :: Solar_Reflectivity_Used_TL
    REAL( fp_kind ) :: Secant_Flux_Angle_Used

    ! -- Array for checking input pressure profiles
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P ) - 1 ) :: dP



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL EXIT STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#             -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --             #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Check the number of channels - if zero
    ! then simply RETURN.
    ! --------------------------------------

    IF ( n_Channels == 0 ) RETURN


    ! ------------------
    ! Get the dimensions
    ! ------------------

    n_Layers = SIZE( Layer_P )


    ! -----------------------------------
    ! Perform a simple check on the input
    ! profile data for negative values
    ! -----------------------------------

    ! -- Profile data
    IF ( ANY( Level_P < ZERO ) .OR. &
         ANY( Layer_P < ZERO ) .OR. &
         ANY( Layer_T < ZERO ) .OR. &
         ANY( Layer_W < ZERO ) .OR. &
         ANY( Layer_O < ZERO )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Negative values found in input profile data.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
      
    ! -- Surface properties
    IF (      Surface_Temperature  < ZERO   .OR. &
         ANY( Surface_Emissivity   < ZERO ) .OR. &
         ANY( Surface_Reflectivity < ZERO )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Negative values found in surface properties (Tsfc,esfc,rsfc).', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Angles
    IF ( Secant_View_Angle < ONE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Secant view angle must be > or = 1.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    IF ( Secant_Solar_Angle < ONE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Secant solar angle must be > or = 1.0', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- Number of channels
    IF ( n_Channels < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Negative number of channels passed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------------------------
    ! Check for physically meaningful pressure profiles
    ! -------------------------------------------------

    ! -- The level pressure profile
    dP = Level_P(2:n_Layers) - Level_P(1:n_Layers-1)

    IF ( ANY( dP < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Level pressure profile is not monotonic.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -- The layer pressure profile
    dP = Layer_P(2:n_Layers) - Layer_P(1:n_Layers-1)

    IF ( ANY( dP < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Layer pressure profile is not monotonic.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------------------------
    ! Check for physically meaningful surface emis/refl
    ! -------------------------------------------------

    IF ( ANY( Surface_Emissivity   > ONE ) .OR. &
         ANY( Surface_Reflectivity > ONE )      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Values > 1.0 found in surface properties (esfc,rsfc).', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------
    ! Optional arguments
    ! ------------------

    ! -- Solar reflectivity
    IF ( PRESENT( Solar_Reflectivity ) ) THEN
      Solar_Reflectivity_Used = Solar_Reflectivity
    ELSE
      Solar_Reflectivity_Used = Surface_Reflectivity
    END IF

    ! -- Tangent-linear solar reflectivity
    IF ( PRESENT( Solar_Reflectivity_TL ) ) THEN
      Solar_Reflectivity_Used_TL = Solar_Reflectivity_TL
    ELSE
      Solar_Reflectivity_Used_TL = Surface_Reflectivity_TL
    END IF

    ! -- Flux angle
    Secant_Flux_Angle_Used = SECANT_DIFFUSIVITY_ANGLE
    IF ( PRESENT( Secant_Flux_Angle ) ) THEN
      IF ( ABS( Secant_Flux_Angle ) <= MAX_SECANT_FLUX_ANGLE ) THEN
        Secant_Flux_Angle_Used = ABS( Secant_Flux_Angle )
      ELSE
        Error_Status = WARNING
        WRITE( Message, '( "Invalid value, ", f7.3, ", for secant flux angle. ", &
                          &"Using default SECANT_DIFFUSIVITY_ANGLE" )' ) Secant_Flux_Angle
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#   -- CALCULATE THE PROFILE GENERIC TANGENT-LINEAR ABSORBER AMOUNTS --    #
    !#--------------------------------------------------------------------------#

    CALL Compute_Absorber_Amount_TL( &
                                     ! -- Forward input
                                     Level_P,    &  ! Input,  K
                                     Layer_W,    &  ! Input,  K
                                     Layer_O,    &  ! Input,  K
                                     ! -- Tangent-linear input
                                     Level_P_TL, &  ! Input,  K
                                     Layer_W_TL, &  ! Input,  K
                                     Layer_O_TL, &  ! Input,  K
                                     ! -- Tangent-linear output
                                     Absorber_TL )  ! Output, 0:K x J



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ----------------------------------------------------

    Tau_Absorber    = Secant_View_Angle * Absorber
    Tau_Absorber_TL = Secant_View_Angle * Absorber_TL


    ! ---------------------------------------
    ! Calculate the tangent-linear predictors
    ! for the satellite view angle
    ! ---------------------------------------

    CALL Compute_Predictors_TL( &
                                ! -- Forward input
                                Layer_P,         &  ! Input,  K
                                Layer_T,         &  ! Input,  K
                                Layer_W,         &  ! Input,  K
                                Tau_Absorber,    &  ! Input,  0:K x J
                                ! -- Tangent-linear input
                                Layer_P_TL,      &  ! Input,  K
                                Layer_T_TL,      &  ! Input,  K
                                Layer_W_TL,      &  ! Input,  K
                                Tau_Absorber_TL, &  ! Input,  0:K x J
                                ! -- Tangent-linear output
                                Tau_Predictor_TL )  ! Output, I x K
 


    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE PREDICTORS FOR THE FLUX TRANSMITTANCE --         #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Have any INFRARED channels been specified for
    ! the current profile? (Microwave channels are
    ! flagged as == 1, so IR == 0).
    !
    ! For microwave channels the downwelling flux
    ! transmission is assumed == upwelling view
    ! angle transmission.
    ! ---------------------------------------------

    IF ( ANY( SC%Is_Microwave_Channel( Channel_Index( 1:n_Channels ) ) == 0 ) ) THEN


      ! ---------------------------------
      ! Modify the nadir absorber amounts
      ! ---------------------------------

      Flux_Tau_Absorber    = Secant_Flux_Angle_Used * Absorber
      Flux_Tau_Absorber_TL = Secant_Flux_Angle_Used * Absorber_TL
      

      ! ---------------------------------------
      ! Calculate the tangent-linear predictors
      ! for the diffusivity angle
      ! ---------------------------------------

      ! -- Calculate the integrated predictors only
      CALL Compute_Predictors_TL( &
                                  ! -- Forward input
                                  Layer_P,               &  ! Input,  K
                                  Layer_T,               &  ! Input,  K
                                  Layer_W,               &  ! Input,  K
                                  Flux_Tau_Absorber,     &  ! Input,  0:K x J

                                  ! -- Tangent-linear input
                                  Layer_P_TL,            &  ! Input,  K
                                  Layer_T_TL,            &  ! Input,  K
                                  Layer_W_TL,            &  ! Input,  K
                                  Flux_Tau_Absorber_TL,  &  ! Input,  0:K x J

                                  ! -- Tangent-linear output
                                  Flux_Tau_Predictor_TL, &  ! Output, I x K

                                  No_Standard = SET      )  ! Optional input

      ! -- Copy the angle independent (standard) predictors
      Flux_Tau_Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
           Tau_Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) 

    END IF



    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE PREDICTORS FOR THE SOLAR TRANSMITTANCE --        #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------
    ! Have *any* SOLAR sensitive channels been specified
    ! for the current profile (Flagged as == 1)?
    !
    ! AND
    !
    ! Is the specified solar zenith angle valid (<85)?
    ! --------------------------------------------------

    IF ( ( ANY( SC%Is_Solar_Channel( Channel_Index( 1:n_Channels ) ) == 1 ) ) .AND. &
         Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE ) THEN


      ! --------------------------------
      ! Modify the nadir absorber amount
      ! --------------------------------

      Solar_Tau_Absorber    = Secant_Solar_Angle * Absorber
      Solar_Tau_Absorber_TL = Secant_Solar_Angle * Absorber_TL


      ! ---------------------------------------
      ! Calculate the tangent-linear predictors
      ! for the solar zenith angle
      ! ---------------------------------------

      ! -- Calculate the integrated predictors only
      CALL Compute_Predictors_TL( &
                                  ! -- Forward input
                                  Layer_P,                &  ! Input,  K
                                  Layer_T,                &  ! Input,  K
                                  Layer_W,                &  ! Input,  K
                                  Solar_Tau_Absorber,     &  ! Input,  0:K x J
                                  ! -- Tangent-linear input
                                  Layer_P_TL,             &  ! Input,  K
                                  Layer_T_TL,             &  ! Input,  K
                                  Layer_W_TL,             &  ! Input,  K
                                  Solar_Tau_Absorber_TL,  &  ! Input,  0:K x J
                                  ! -- Tangent-linear output
                                  Solar_Tau_Predictor_TL, &  ! Output, I x K

                                  No_Standard = SET       )  ! Optional input

      ! -- Copy the angle independent predictors
      Solar_Tau_Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
            Tau_Predictor_TL( 1:MAX_N_STANDARD_PREDICTORS, : ) 

    END IF
     


    !#--------------------------------------------------------------------------#
    !#                            -- CHANNEL LOOP --                            #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, n_Channels


      ! --------------------------------------------
      ! Calculate the current channel tangent-linear
      ! transmittances for the satellite view angle
      ! --------------------------------------------

      CALL Compute_Transmittance_TL( Tau_Absorber,       &   ! Input, 0:K x J
                                     Tau_Predictor,      &   ! Input, I x K
                                     Tau( :, l ),        &   ! Input, K
                                     Tau_Absorber_TL,    &   ! Input, 0:K x J
                                     Tau_Predictor_TL,   &   ! Input, I x K
                                     Channel_Index( l ), &   ! Input, scalar
                                     UP,                 &   ! Input, scalar

                                     Tau_TL( :, l )      )   ! Output, K



      ! -----------------------------------------------------
      ! If the current channel is an INFRARED channel,
      ! then calculate the downwelling flux transmittance.
      !
      ! If the current channel is a MICROWAVE channel,
      ! then use the predictors for the upwelling
      ! transmittance calculations.
      !
      ! Two things:
      ! - Currently, the predictors and coefficients are
      !   the same for the up- and downwelling cases,
      !   hence the simple "upending" of the transmittances
      !   for the microwave (assumed specular) case.
      ! - The "upending", or assumption that the downwelling
      !   transmittances can be derived directly from the
      !   upwelling transmittances, is only valid for
      !   monochromatic transmittances. For broadband sensors
      !   this is an approximation - and depending on the
      !   spectral structure of the transmittances within
      !   a channel's response - not usually a good one.
      ! -----------------------------------------------------

      IF ( SC%Is_Microwave_Channel( Channel_Index( l ) ) == 0 ) THEN

        ! -- IR channel
        CALL Compute_Transmittance_TL( Flux_Tau_Absorber,     &   ! Input, 0:K x J
                                       Flux_Tau_Predictor,    &   ! Input, I x K
                                       Flux_Tau( :, l ),      &   ! Input, K
                                       Flux_Tau_Absorber_TL,  &   ! Input, 0:K x J
                                       Flux_Tau_Predictor_TL, &   ! Input, I x K
                                       Channel_Index( l ),    &   ! Input, scalar
                                       DOWN,                  &   ! Input, scalar

                                       Flux_Tau_TL( :, l )    )   ! Output, K

      ELSE


        ! -- uW channel

!  This can be considered a "hook" for future versions where
!  downwelling will not be derived from the upwelling.
!
!        CALL Compute_Transmittance_TL( Tau_Absorber,       &   ! Input, 0:K x J
!                                       Tau_Predictor,      &   ! Input, I x K
!                                       Flux_Tau( :, l ),   &   ! Input, K
!                                       Tau_Absorber_TL,    &   ! Input, 0:K x J
!                                       Tau_Predictor_TL,   &   ! Input, I x K
!                                       Channel_Index( l ), &   ! Input, scalar
!                                       DOWN,               &   ! Input, scalar
!
!                                       Flux_Tau_TL( :, l ) )   ! Output, K

        Flux_Tau_TL( :, l ) = ZERO
        IF ( Tau( n_Layers, l ) > TOLERANCE ) THEN
          Flux_Tau_TL( 1, l ) = Tau_TL( n_Layers, l )
          DO k = 2, n_Layers

            Flux_Tau_TL( k, l ) = ( Flux_Tau_TL( 1, l ) / &
            !                       -------------------   
                                      Tau( k-1, l )     ) - &

                                  ( Flux_Tau( 1, l ) * Tau_TL( k-1, l ) / &
            !                       -----------------------------------
                                              Tau( k-1, l )**2          )
          END DO
        END IF

      END IF



      ! ----------------------------------------------------
      ! If the current channel is a SOLAR SENSITIVE channel,
      !   AND
      ! the solar angle is valid, then calculate the
      ! transmittance for direct solar.
      ! ----------------------------------------------------

      IF ( SC%Is_Solar_Channel( Channel_Index( l ) ) == 1 .AND. &
           Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE       ) THEN

        Valid_Solar = 1

        CALL Compute_Transmittance_TL( Solar_Tau_Absorber,     &   ! Input, 0:K x J
                                       Solar_Tau_Predictor,    &   ! Input, I x K
                                       Solar_Tau( :, l ),      &   ! Input, K
                                       Solar_Tau_Absorber_TL,  &   ! Input, 0:K x J
                                       Solar_Tau_Predictor_TL, &   ! Input, I x K
                                       Channel_Index( l ),     &   ! Input, scalar
                                       DOWN,                   &   ! Input, scalar

                                       Solar_Tau_TL( :, l )    )   ! Output, K

      ELSE

        Valid_Solar = 0
        Solar_Tau_TL( :, l ) = ZERO

      END IF


      ! ------------------------------------------------------
      ! Calculate the profile/channel tangent-linear radiances
      ! ------------------------------------------------------

      CALL Compute_Radiance_TL( &
                                ! -- Forward input
                                Layer_T( : ),                     &  ! Input, K
                                Surface_Temperature,              &  ! Input, scalar
                                Surface_Emissivity( l ),          &  ! Input, scalar
                                Surface_Reflectivity( l ),        &  ! Input, scalar
                                Solar_Reflectivity_Used( l ),     &  ! Input, scalar
                                Tau(      :, l ),                 &  ! Input, K
                                Flux_Tau( :, l ),                 &  ! Input, K
                                Solar_Tau( n_Layers, l ),         &  ! Input, scalar
                                Layer_Radiance( :, l ),           &  ! Input, K
                                Downwelling_Radiance( l ),        &  ! Input, scalar
                                Upwelling_Radiance( l ),          &  ! Input, scalar
                                ! -- Tangent-linear input
                                Layer_T_TL( : ),                  &  ! Input, K
                                Surface_Temperature_TL,           &  ! Input, scalar
                                Surface_Emissivity_TL( l ),       &  ! Input, scalar
                                Surface_Reflectivity_TL( l ),     &  ! Input, scalar
                                Solar_Reflectivity_Used_TL( l ),  &  ! Input, scalar
                                Tau_TL(      :, l ),              &  ! Input, K
                                Flux_Tau_TL( :, l ),              &  ! Input, K
                                Solar_Tau_TL( n_Layers, l ),      &  ! Input, scalar
                                ! -- Other input
                                Secant_Solar_Angle,               &  ! Input, scalar
                                Valid_Solar,                      &  ! Input, scalar
                                Channel_Index( l ),               &  ! Input, scalar
                                ! -- Tangent-linear output
                                Layer_Radiance_TL( :, l ),        &  ! Output, K
                                Downwelling_Radiance_TL( l ),     &  ! Output, scalar
                                Upwelling_Radiance_TL( l ),       &  ! Output, scalar
                                Brightness_Temperature_TL( l )    )  ! Output, scalar


    END DO Channel_Loop

  END FUNCTION Tangent_Linear_RTM_rank1

END MODULE Tangent_Linear_Model


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: tangent_linear_model.f90,v 2.6 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: tangent_linear_model.f90,v $
! Revision 2.6  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.5  2004/12/28 20:27:29  paulv
! - Added check for monotonicity of input pressure profiles.
! - Added check for input surface emissivity/reflectivity > 1.0.
!
! Revision 2.4  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.3  2004/10/04 21:30:56  paulv
! - Removed intrinsics declaration.
!
! Revision 2.2  2004/06/12 19:15:29  paulv
! - Updated header documentation
! - Fixed error handling in wrapper functions
!
! Revision 2.1  2004/06/11 22:58:06  paulv
! - Corrected some minor bugs.
! - Updated some documentation.
!
! Revision 2.0  2004/06/10 19:12:58  paulv
! - New version with overloaded wrapper functions.
!
! Revision 1.9  2001/11/07 15:08:07  paulv
! - Changed the logical IF test for the number of input profiles in the
!   [<TANGENT_LINEAR><ADJOINT><K_MATRIX>]_RTM_RANK2() functions from
!     IF ( n_input_profiles >= 1 .AND. n_input_profiles <= n_profiles ) THEN
!   to
!     IF ( n_input_profiles > 0 .AND. n_input_profiles <= n_profiles ) THEN
!   The use of both the ">=" and "<=" relational operators with the .AND. I
!   found confusing.
!
! Revision 1.8  2001/11/07 14:57:46  paulv
! - Added check for negative number of channels to TANGENT_LINEAR_RTM_RANK2() function.
! - Added profile loop CYCLE statement to TANGENT_LINEAR_RTM_RANK2() function.
! - Added check for negative number of channels to TANGENT_LINEAR_RTM_RANK1() function.
!
! Revision 1.7  2001/10/01 20:26:20  paulv
! - Overloaded the TANGENT_LINEAR_RTM function to accept both a
!   single or group of profiles. Contained PRIVATE functions are now
!     o TANGENT_LINEAR_RTM_RANK1 for single profile input
!     o TANGENT_LINEAR_RTM_RANK2 for multiple profile input
! - Put N_INPUT_PROFILES optional argument back in TANGENT_LINEAR_RTM
!   argument lists. This allows more control over how many profiles are to be
!   processed rather than simply relying on the dimension of the input arrays.
!   Now, as before,
!     n_profiles = SIZE( layer_p, DIM = 2 )
!   but also,
!     IF ( PRESENT( n_input_profiles ) ) THEN
!       IF ( n_input_profiles >= 1 .AND. n_input_profiles <= n_profiles ) THEN
!         n_profiles = n_input_profiles
!     ....
! - Changed SURFACE_TEMPERATURE argument check from
!     IF ( ANY( surface_temperature < ZERO ) )
!   to
!     IF (      surface_temperature < ZERO )
!   as the check is now done in the rank-1 TANGENT_LINEAR_RTM function. This eliminates
!   the need to fully populate the input arrays with data when only certain
!   "chunks" may be processed. Previously, the use of ANY() could generate
!   and error if the full surface_temperature array was not initialised.
! - Added "Name" to RCS keyword list.
!
! Revision 1.6  2001/09/04 21:29:11  paulv
! - Updated documentation.
!
! Revision 1.5  2001/08/31 21:33:04  paulv
! - Added check for negative profile and surface data in TANGENT_LINEAR_RTM.
! - Maximum solar angle secant is no longer calculated in TANGENT_LINEAR_RTM but
!   is declared as a parameter in the PARAMETERS module.
!
! Revision 1.4  2001/08/16 16:37:52  paulv
! - Updated documentation.
! - The channel dimension is now obtained by:
!     n_channels = MAXVAL( n_channels_per_profile )
!   rather than
!     n_channels = SIZE( channel_index ) / n_profiles
!   since the latter assumes the same number of channels will be processed
!   for each profile - which may not be the case. The new method determines
!   the largest number of channels to be processed for any particular
!   profile.
! - The comparison of n_channels and MAX_N_CHANNELS is now done via the
!   MAX_N_CHANNELS methods in the PARAMETERS module. And extra check to
!   see if MAX_N_CHANNELS has been set was added.
!
! Revision 1.3  2001/08/01 17:00:16  paulv
! - Altered function declaration to avoid more than the standard 39
!   continuation lines allowed in Fortran 90.
! - Updated input argument checking. Now consistent with other model
!   components.
!
! Revision 1.2  2001/07/12 18:46:06  paulv
! - Commented out informational message output at start of function.
! - Corrected bug in the calculation of the thermal flux transmittance for
!   the infrared channels. The call to COMPUTE_TRANSMITTANCE_TL used the
!   absorber bracketing index array TAU_LAYER_INDEX (for the upwelling
!   transmittance) rather than FLUX_TAU_LAYER_INDEX.
!
! Revision 1.1  2001/05/29 16:36:02  paulv
! Initial checkin
!
!
!
