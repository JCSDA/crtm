!------------------------------------------------------------------------------
!M+
! NAME:
!       K_Matrix_Model
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) K-matrix function.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE K_Matrix_Model
!
! MODULES:
!       Type_Kinds:            Module to define kind types for variable
!                              declaration.
!
!       Message_Handler:       Module to define error codes and handle
!                              error conditions
!                              USEs: FILE_UTILITY module
!
!       Parameters:            Module containing parameter definitions for the
!                              pCRTM.
!                              USEs: TYPE_KINDS module
!
!       Spectral_Coefficients: Module containing spectral coefficients.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    PARAMETERS module
!                                    SPCCOEFF_DEFINE module
!                                    SPCCOEFF_BINARY_IO module
!
!       Absorber_Profile:      Module containing routines for generating the
!                              absorber profiles.
!                              USEs: TYPE_KINDS module
!                                    PARAMETERS module
!
!       Predictors:            Module containing routines for generating the
!                              predictor profiles.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    PARAMETERS module
!
!       Transmittance:         Module containing transmittance calculation
!                              routines.
!                              USEs: TYPE_KINDS module
!                                    PARAMETERS module
!                                    TRANSMITTANCE_COEFFICIENTS module
!
!       Radiance:              Module containing radiance calculation
!                              routines.
!                              USEs: TYPE_KINDS module
!                                    PARAMETERS module
!                                    SPECTRAL_COEFFICIENTS module
!                                    SENSOR_PLANCK_FUNCTIONS module
!
!       Forward_Model:         Module containing the pCRTM forward function.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    PARAMETERS module
!                                    SPECTRAL_COEFFICIENTS module
!                                    ABSORBER_PROFILE module
!                                    PREDICTORS module
!                                    TRANSMITTANCE module
!                                    RADIANCE module
!
! CONTAINS:
!       Compute_RTM_K:         Function that calculates the K-matrix of the 
!                              top-of-atmosphere (TOA) radiances and brightness 
!                              temperatures for an input atmospheric profile set and
!                              user specified satellites/channels.
!
!                              This function is simply a wrapper around both the
!                              FORWARD model and the K-MATRIX model so that the user
!                              doesn't have to declare the absorber/predictor/etc.
!                              arrays in the calling routine.
!
!       K_Matrix_RTM:          Function that calculates the K-matrix of the
!                              top-of-atmosphere (TOA) radiances and brightness
!                              temperatures for user specified profiles and
!                              satellite/channel transmittance profiles, radiances
!                              and brightness temperatures.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
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
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP/EMC 18-Jul-2001
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2001, 2004 Paul van Delst
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

MODULE k_matrix_model


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds, ONLY : fp_kind
  USE Message_Handler
  USE Parameters
  USE Spectral_Coefficients

  USE Absorber_Profile, ONLY : Compute_Absorber_Amount_AD
  USE Predictors,       ONLY : Compute_Predictors_AD
  USE Transmittance,    ONLY : Compute_Transmittance_AD
  USE Radiance,         ONLY : Compute_Radiance_AD

  USE Forward_Model, ONLY : Forward_RTM


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! --------------------
  ! Default visibilities
  ! --------------------

  PRIVATE
  PUBLIC :: Compute_RTM_K
  PUBLIC :: K_Matrix_RTM


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE Compute_RTM_K
    MODULE PROCEDURE Compute_RTM_K_rank1
    MODULE PROCEDURE Compute_RTM_K_rank2
  END INTERFACE Compute_RTM_K

  INTERFACE K_Matrix_RTM
    MODULE PROCEDURE K_Matrix_RTM_rank1
    MODULE PROCEDURE K_Matrix_RTM_rank2
  END INTERFACE K_Matrix_RTM


  ! -------------------------
  ! PRIVATE module parameters
  ! -------------------------

  CHARACTER( * ), PARAMETER :: MODULE_RCS_ID = &
    '$Id: k_matrix_model.f90,v 2.5 2006/05/02 14:58:35 dgroff Exp $'

  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_RTM_K
!
! PURPOSE:
!       Function that calculates the K-matrix of the top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric profile
!       set and user specified satellites/channels.
!
!       This function is simply a wrapper around both the FORWARD model and the
!       K-MATRIX model so that the user doesn't have to declare the absorber/
!       predictor/etc arrays in the calling routine.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       Error_Status = Compute_RTM_K( &
!                        ! -- Forward inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,           &  ! Input, K x M
!
!                        Surface_Temperature,                                   &  ! Input, M
!                        Surface_Emissivity,                                    &  ! Input, L*M
!                        Surface_Reflectivity,                                  &  ! Input, L*M
!
!                        ! -- K-matrix inputs
!                        Tau_K,                                                 &  ! In/Output, K x L*M
!                        Flux_Tau_K,                                            &  ! In/Output, K x L*M
!                        Solar_Tau_K,                                           &  ! In/Output, K x L*M
!
!                        Upwelling_Radiance_K,                                  &  ! In/Output, L*M
!                        Brightness_Temperature_K,                              &  ! In/Output, L*M
!
!                        ! -- Other inputs
!                        Secant_View_Angle,                                     &  ! Input, M
!                        Secant_Solar_Angle,                                    &  ! Input, M
!                        n_Channels_per_Profile,                                &  ! Input, M
!                        Channel_Index,                                         &  ! Input, L*M
!
!                        ! -- Forward output
!                        Tau,                                                   &  ! Input, K x L*M
!                        Flux_Tau,                                              &  ! Input, K x L*M
!                        Solar_Tau,                                             &  ! Input, K x L*M
!
!                        Upwelling_Radiance,                                    &  ! Input, L*M
!                        Brightness_Temperature,                                &  ! Input, L*M
!
!                        ! -- K-matrix outputs
!                        Level_p_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K, &  ! In/Output, K x L*M
!
!                        Surface_Temperature_K,                                 &  ! In/Output, L*M
!                        Surface_Emissivity_K,                                  &  ! In/Output, L*M
!                        Surface_Reflectivity_K,                                &  ! In/Output, L*M
!
!                        ! -- Optional forward inputs
!                        Solar_Reflectivity = Solar_Reflectivity,               &  ! Optional Input, L*M
!
!                        ! -- Other optional inputs
!                        Secant_Flux_Angle = Secant_Flux_Angle,                 &  ! Optional Input, M
!                        n_Input_Profiles = n_Input_Profiles,                   &  ! Optional Input, Scalar
!
!                        ! -- Optional K-matrix outputs
!                        Solar_Reflectivity_K = Solar_Reflectivity_K,           &  ! Optional In/Output, L*M
!
!                        ! -- Error messaging
!                        Message_Log = Message_Log )
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
!       Tau_K:                     Layer->TOA adjoint transmittance for the satellite
!                                  view angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Flux_Tau_K:                Layer->SFC adjoint transmittance for the default
!                                  diffusivity angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Solar_Tau_K:               Layer->SFC adjoint transmittance for the solar
!                                  zenith angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Upwelling_Radiance_K:      TOA adjoint radiances for each channel/profile.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Brightness_Temperature_K:  Adjoint temperatures corresponding to the
!                                  TOA adjoint radiances.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
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
!       n_Channels_per_Profile:    The number of channels for each profile in the
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
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:        Profile set surface reflectivity array for the
!                                  solar term only. If not specified, the
!                                  Surface_Reflectivity argument is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:         Secant of the angle to be used to approximate
!                                  the downwelling flux transmittance for the INFRARED
!                                  only. If not specified a default value of 5/3 (1.6666..)
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
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
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau:                  Layer->SFC transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau:                 Layer->SFC transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A.
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
!                                  N.B.: Set to ZERO upon output.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Level_P_K:                 Profile set layer interface pressure K-matrix
!                                  adjoint array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_P_K:                 Profile set layer average pressure K-matrix
!                                  adjoint array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_T_K:                 Profile set layer average temperature K-matrix
!                                  adjoint array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_W_K:      .          Profile set layer average water vapor mixing ratio
!                                  K-matrix adjoint array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_O_K:                 Profile set layer average ozone mixing ratio
!                                  K-matrix adjoint array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Temperature_K:     Profile set surface temperature K-matrix adjoint
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Emissivity_K:      Profile set surface emissivity K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Reflectivity_K:    Profile set surface reflectivity K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Solar_Reflectivity_K:      Profile set surface reflectivity K-matrix
!                                  array for the solar term only. If not
!                                  specified, the Surface_Reflectivity_K
!                                  argument contains this contribution.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT ), OPTIONAL
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
!      K_Matrix_RTM:               Function that calculates the K-matrix of the
!                                  top-of-atmosphere (TOA) radiances or brightness
!                                  temperatures for an input atmospheric profile
!                                  set and user specified satellites/channels.
!
! SIDE EFFECTS:
!       All INPUT K-matrix arguments are set to ZERO on OUTPUT.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION Compute_RTM_K_rank2( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,           &  ! Input, K x M
             Surface_Temperature,                                   &  ! Input, M
             Surface_Emissivity, Surface_Reflectivity,              &  ! Input, L*M
             ! -- K-matrix inputs
             Tau_K, Flux_Tau_K, Solar_Tau_K,                        &  ! In/Output, K x L*M
             Upwelling_Radiance_K,                                  &  ! In/Output, L*M
             Brightness_Temperature_K,                              &  ! In/Output, L*M
             ! -- Other inputs
             Secant_View_Angle,                                     &  ! Input, M
             Secant_Solar_Angle,                                    &  ! Input, M
             n_Channels_Per_Profile,                                &  ! Input, M
             Channel_Index,                                         &  ! Input, L*M
             ! -- Forward output
             Tau, Flux_Tau, Solar_Tau,                              &  ! Input, K x L*M
             Upwelling_Radiance,                                    &  ! Input, L*M
             Brightness_Temperature,                                &  ! Input, L*M
             ! -- K-matrix outputs
             Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K, &  ! In/Output, K x L*M
             Surface_Temperature_K,                                 &  ! In/Output, M
             Surface_Emissivity_K, Surface_Reflectivity_K,          &  ! In/Output, L*M
             ! -- Optional forward inputs
             Solar_Reflectivity,                                    &  ! Optional input,  L*M
             ! -- Other optional inputs
             Secant_Flux_Angle,                                     &  ! Optional input,  M
             n_Input_Profiles,                                      &  ! Optional input,  Scalar
             ! -- Optional K-matrix outputs
             Solar_Reflectivity_K,                                  &  ! Optional input,  L*M
             ! -- Error messaging
             Message_Log )                                          &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN )     :: Level_P                    ! K x M  
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN )     :: Layer_P                    ! K x M  
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN )     :: Layer_T                    ! K x M  
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN )     :: Layer_W                    ! K x M  
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN )     :: Layer_O                    ! K x M  

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Surface_Temperature        ! M      
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Surface_Emissivity         ! L*M    
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Surface_Reflectivity       ! L*M    

    ! -- K-matrix inputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Tau_K                      ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Flux_Tau_K                 ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Solar_Tau_K                ! K x L*M

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Upwelling_Radiance_K       ! L*M
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Brightness_Temperature_K   ! L*M

    ! -- Other inputs
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Secant_View_Angle          ! M
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Secant_Solar_Angle         ! M
    INTEGER,                   DIMENSION( : ),    INTENT( IN )     :: n_Channels_per_Profile     ! M
    INTEGER,                   DIMENSION( : ),    INTENT( IN )     :: Channel_Index              ! L*M

    ! -- Forward outputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Tau                        ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Flux_Tau                   ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Solar_Tau                  ! K x L*M

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( OUT )    :: Upwelling_Radiance         ! L*M
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( OUT )    :: Brightness_Temperature     ! L*M

    ! -- K-matrix outputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Level_P_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_P_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_T_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_W_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_O_K                  ! K x L*M

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Temperature_K      ! L*M
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Emissivity_K       ! L*M
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Reflectivity_K     ! L*M

    ! -- Optional Forward input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN )     :: Solar_Reflectivity         ! L*M

    ! -- Other optional inputs
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN )     :: Secant_Flux_Angle          ! M
    INTEGER,         OPTIONAL,                    INTENT( IN )     :: n_Input_Profiles           ! Scalar

    ! -- Optional Adjoint output
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN OUT ) :: Solar_Reflectivity_K       ! L*M

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM_K(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Error_Status_K

    ! -- Array for integrated absorber amounts, 0:K x J x M
    REAL( fp_kind ), DIMENSION( 0:SIZE( Layer_P, DIM = 1 ), &
                                  MAX_N_ABSORBERS,          &
                                  SIZE( Layer_P, DIM = 2 )  ) :: Absorber

    ! -- Arrays for predictors, Imax x K x M
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( Layer_P, DIM = 1 ), &
                                SIZE( Layer_P, DIM = 2 )  ) :: Tau_Predictor,      &
                                                               Flux_Tau_Predictor, &
                                                               Solar_Tau_Predictor

    ! -- Array for forward and K-matrix layer Planck radiance term, K x L*M
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P, DIM = 1 ),  &
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance,  &
                                                                Layer_Radiance_K
      

    ! -- Array for forward and K-matrix downwelling radiance (flux + solar), L*M
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance,  &
                                                                Downwelling_Radiance_K



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
    !#                   -- COMPUTE THE K-MATRIX PROFILES --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! Initialise all local K-matrix variables
    ! ---------------------------------------

    Layer_Radiance_K       = ZERO
    Downwelling_Radiance_K = ZERO


    ! -----------------------
    ! Call the K-matrix model
    ! -----------------------

    Error_Status_K = K_Matrix_RTM_rank2( &
                       ! -- Forward inputs
                       Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input,  K x M
                       Surface_Temperature,                                    &  ! Input, M
                       Surface_Emissivity, Surface_Reflectivity,               &  ! Input, L*M
                       Absorber,                                               &  ! Input, 0:K x J x M
                       Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor, &  ! Input, Imax x K x M
                       Tau, Flux_Tau, Solar_Tau,                               &  ! Input, K x L*M
                       Layer_Radiance,                                         &  ! Input, K x L*M
                       Downwelling_Radiance, Upwelling_Radiance,               &  ! Input, L*M
                       ! -- K-matrix inputs
                       Tau_K, Flux_Tau_K, Solar_Tau_K,                         &  ! In/Output, K x L*M
                       Layer_Radiance_K,                                       &  ! In/Output, K x L*M
                       Downwelling_Radiance_K, Upwelling_Radiance_K,           &  ! In/Output, L*M
                       Brightness_Temperature_K,                               &  ! In/Output, L*M
                       ! -- Other inputs
                       Secant_View_Angle, Secant_Solar_Angle,                  &  ! Input, M
                       n_Channels_Per_Profile,                                 &  ! Input, M
                       Channel_Index,                                          &  ! Input, L*M
                       ! -- K-matrix outputs
                       Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K,  &  ! In/Output, K x L*M
                       Surface_Temperature_K,                                  &  ! In/Output, L*M
                       Surface_Emissivity_K, Surface_Reflectivity_K,           &  ! In/Output, L*M
                       ! -- Optional forward inputs
                       Solar_Reflectivity = Solar_Reflectivity,                &  ! Optional input, L*M
                       ! -- Other optional inputs
                       Secant_Flux_Angle = Secant_Flux_Angle,                  &  ! Optional input, M
                       n_Input_Profiles  = n_Input_Profiles,                   &  ! Optional input, Scalar
                       ! -- Optional K-matrix outputs
                       Solar_Reflectivity_K = Solar_Reflectivity_K,            &  ! Optional In/Output,  L*M
                       ! -- Error messaging
                       Message_Log = Message_Log                               )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    SELECT CASE ( Error_Status_K )
      CASE ( FAILURE )
        Error_Status= Error_Status_K
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occured in K_Matrix_RTM(Rank-2)', &
                              Error_Status, &
                              Message_Log = Message_Log )

      CASE ( WARNING )
        Error_Status= Error_Status_K

      CASE DEFAULT
        ! -- Result is SUCCESS, so do nothing to
        ! -- pass on result of Forward_RTM

    END SELECT

  END FUNCTION Compute_RTM_K_rank2





  FUNCTION Compute_RTM_K_rank1( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,           &  ! Input, K
             Surface_Temperature,                                   &  ! Input, Scalar
             Surface_Emissivity, Surface_Reflectivity,              &  ! Input, L
             ! -- K-matrix inputs
             Tau_K, Flux_Tau_K, Solar_Tau_K,                        &  ! In/Output, K x L
             Upwelling_Radiance_K,                                  &  ! In/Output, L
             Brightness_Temperature_K,                              &  ! In/Output, L
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                 &  ! Input, Scalar
             n_Channels_Per_Profile,                                &  ! Input, Scalar
             Channel_Index,                                         &  ! Input, L
             ! -- Forward output
             Tau, Flux_Tau, Solar_Tau,                              &  ! Input, K x L
             Upwelling_Radiance,                                    &  ! Input, L
             Brightness_Temperature,                                &  ! Input, L
             ! -- K-matrix outputs
             Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K, &  ! In/Output, K x L
             Surface_Temperature_K,                                 &  ! In/Output, Scalar
             Surface_Emissivity_K, Surface_Reflectivity_K,          &  ! In/Output, L
             ! -- Optional forward inputs
             Solar_Reflectivity,                                    &  ! Optional input,  L
             ! -- Other optional inputs
             Secant_Flux_Angle,                                     &  ! Optional input,  Scalar
             n_Input_Profiles,                                      &  ! Optional input,  Scalar
             ! -- Optional K-matrix outputs
             Solar_Reflectivity_K,                                  &  ! Optional input,  L
             ! -- Error messaging
             Message_Log )                                          &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Level_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Layer_P                    ! K
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Layer_T                    ! K
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Layer_W                    ! K
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Layer_O                    ! K

    REAL( fp_kind ),                              INTENT( IN )     :: Surface_Temperature        ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Surface_Emissivity         ! L
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN )     :: Surface_Reflectivity       ! L

    ! -- K-matrix inputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Tau_K                      ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Flux_Tau_K                 ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Solar_Tau_K                ! K x L

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Upwelling_Radiance_K       ! L
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Brightness_Temperature_K   ! L

    ! -- Other inputs
    REAL( fp_kind ),                              INTENT( IN )     :: Secant_View_Angle          ! Scalar
    REAL( fp_kind ),                              INTENT( IN )     :: Secant_Solar_Angle         ! Scalar
    INTEGER,                                      INTENT( IN )     :: n_Channels_Per_Profile     ! Scalar
    INTEGER,                   DIMENSION( : ),    INTENT( IN )     :: Channel_Index              ! L

    ! -- Forward outputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Tau                        ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Flux_Tau                   ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( OUT )    :: Solar_Tau                  ! K x L

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( OUT )    :: Upwelling_Radiance         ! L
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( OUT )    :: Brightness_Temperature     ! L

    ! -- K-matrix outputs
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Level_P_K                  ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_P_K                  ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_T_K                  ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_W_K                  ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ), INTENT( IN OUT ) :: Layer_O_K                  ! K x L

    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Temperature_K      ! L
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Emissivity_K       ! L
    REAL( fp_kind ),           DIMENSION( : ),    INTENT( IN OUT ) :: Surface_Reflectivity_K     ! L

    ! -- Optional Forward input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN )     :: Solar_Reflectivity         ! L

    ! -- Other optional inputs
    REAL( fp_kind ), OPTIONAL,                    INTENT( IN )     :: Secant_Flux_Angle          ! Scalar
    INTEGER,         OPTIONAL,                    INTENT( IN )     :: n_Input_Profiles           ! Scalar

    ! -- Optional K-matrix output
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),    INTENT( IN OUT ) :: Solar_Reflectivity_K       ! L

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM_K(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Error_Status_K

    ! -- Array for integrated absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( Layer_P, DIM = 1 ), &
                                  MAX_N_ABSORBERS           ) :: Absorber

    ! -- Arrays for predictors, Imax x K
    REAL( fp_kind ), DIMENSION( MAX_N_PREDICTORS,         &
                                SIZE( Layer_P, DIM = 1 )  ) :: Tau_Predictor,      &
                                                               Flux_Tau_Predictor, &
                                                               Solar_Tau_Predictor

    ! -- Array for forward and K-matrix layer Planck radiance term, K x L
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P, DIM = 1 ),  &
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance,  &
                                                                Layer_Radiance_K
      

    ! -- Array for forward and K-matrix downwelling radiance (flux + solar), L
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance,  &
                                                                Downwelling_Radiance_K 



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE FORWARD RADIANCES --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Forward_RTM( &
                     ! -- Forward inputs
                     Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input,  K
                     Surface_Temperature,                                    &  ! Input,  Scalar
                     Surface_Emissivity, Surface_Reflectivity,               &  ! Input,  L
                     ! -- Other inputs
                     Secant_View_Angle,                                      &  ! Input,  Scalar
                     Secant_Solar_Angle,                                     &  ! Input,  Scalar
                     n_Channels_Per_Profile,                                 &  ! Input,  Scalar
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
    !#                   -- COMPUTE THE K-MATRIX PROFILES --                    #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------
    ! Initialise all local K-matrix variables
    ! ---------------------------------------

    Layer_Radiance_K       = ZERO
    Downwelling_Radiance_K = ZERO


    ! -----------------------
    ! Call the K-matrix model
    ! -----------------------

    Error_Status_K = K_Matrix_RTM_rank1( &
                       ! -- Forward inputs
                       Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input, K
                       Surface_Temperature,                                    &  ! Input, Scalar
                       Surface_Emissivity,                                     &  ! Input, L
                       Surface_Reflectivity,                                   &  ! Input, L
                       Absorber,                                               &  ! Input, 0:K x J
                       Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor, &  ! Input, Imax x K  
                       Tau, Flux_Tau, Solar_Tau,                               &  ! Input, K x L     
                       Layer_Radiance,                                         &  ! Input, K x L     
                       Downwelling_Radiance, Upwelling_Radiance,               &  ! Input, L         
                       ! -- K-matrix inputs
                       Tau_K, Flux_Tau_K, Solar_Tau_K,                         &  ! In/Output, K x L 
                       Layer_Radiance_K,                                       &  ! In/Output, K x L 
                       Downwelling_Radiance_K, Upwelling_Radiance_K,           &  ! In/Output, L     
                       Brightness_Temperature_K,                               &  ! In/Output, L     
                       ! -- Other inputs
                       Secant_View_Angle, Secant_Solar_Angle,                  &  ! Input, Scalar    
                       n_Channels_Per_Profile,                                 &  ! Input, Scalar    
                       Channel_Index,                                          &  ! Input, L         
                       ! -- K-matrix outputs
                       Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K,  &  ! In/Output, K x L
                       Surface_Temperature_K,                                  &  ! In/Output, L
                       Surface_Emissivity_K, Surface_Reflectivity_K,           &  ! In/Output, L
                       ! -- Optional forward inputs
                       Solar_Reflectivity = Solar_Reflectivity,                &  ! Optional input,  L
                       ! -- Other optional inputs
                       Secant_Flux_Angle = Secant_Flux_Angle,                  &  ! Optional input, Scalar
                       n_Input_Profiles  = n_Input_Profiles,                   &  ! Optional input, Scalar
                       ! -- Optional K-matrix outputs
                       Solar_Reflectivity_K = Solar_Reflectivity_K,            &  ! Optional In/Output,  L
                       ! -- Error messaging
                       Message_Log = Message_Log                               )  ! Error messaging


    ! -------------------------------
    ! Check for successful completion
    ! -------------------------------

    SELECT CASE ( Error_Status_K )
      CASE ( FAILURE )
        Error_Status= Error_Status_K
        CALL Display_Message( ROUTINE_NAME, &
                              'Error occured in K_Matrix_RTM(Rank-1)', &
                              Error_Status, &
                              Message_Log = Message_Log )

      CASE ( WARNING )
        Error_Status= Error_Status_K

      CASE DEFAULT
        ! -- Result is SUCCESS, so do nothing to
        ! -- pass on result of Forward_RTM

    END SELECT

  END FUNCTION Compute_RTM_K_rank1





!--------------------------------------------------------------------------------
!S+
! NAME:
!       K_Matrix_RTM
!
! PURPOSE:
!       PUBLIC function that calculates the K-matrix of the top-of-atmosphere
!       (TOA) radiances and brightness temperatures for an input atmospheric
!       profile set and user specified satellites/channels.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = K_Matrix_RTM( &
!                        ! -- Forward inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O,           &  ! Input, K x M
!                        Surface_Temperature,                                   &  ! Input, M
!                        Surface_Emissivity,                                    &  ! Input, L*M
!                        Surface_Reflectivity,                                  &  ! Input, L*M
!
!                        Absorber,                                              &  ! Input, 0:K x J x M
!
!                        Tau_Predictor,                                         &  ! Input, Imax x K x M
!                        Flux_Tau_Predictor,                                    &  ! Input, Imax x K x M
!                        Solar_Tau_Predictor,                                   &  ! Input, Imax x K x M
!
!                        Tau,                                                   &  ! Input, K x L*M
!                        Flux_Tau,                                              &  ! Input, K x L*M
!                        Solar_Tau,                                             &  ! Input, K x L*M
!
!                        Layer_Radiance,                                        &  ! Input, K x L*M
!                        Downwelling_Radiance,                                  &  ! Input, L*M
!                        Upwelling_Radiance,                                    &  ! Input, L*M
!
!                        ! -- K-matrix inputs
!                        Tau_K,                                                 &  ! In/Output, K x L*M
!                        Flux_Tau_K,                                            &  ! In/Output, K x L*M
!                        Solar_Tau_K,                                           &  ! In/Output, K x L*M
!
!                        Layer_Radiance_K,                                      &  ! In/Output, K x L*M
!                        Downwelling_Radiance_K,                                &  ! In/Output, L*M
!                        Upwelling_Radiance_K,                                  &  ! In/Output, L*M
!
!                        Brightness_Temperature_K,                              &  ! In/Output, L*M
!
!                        ! -- Other inputs
!                        Secant_View_Angle,                                     &  ! Input, M
!                        Secant_Solar_Angle,                                    &  ! Input, M
!                        n_Channels_per_Profile,                                &  ! Input, M
!                        Channel_Index,                                         &  ! Input, L*M
!
!                        ! -- K-matrix outputs
!                        Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K, &  ! In/Output, K x L*M
!
!                        Surface_Temperature_K,                                 &  ! In/Output, L*M
!                        Surface_Emissivity_K,                                  &  ! In/Output, L*M
!                        Surface_Reflectivity_K,                                &  ! In/Output, L*M
!
!                        ! -- Optional forward inputs
!                        Solar_Reflectivity = Solar_Reflectivity,               &  ! Optional Input, L*M
!
!                        ! -- Other optional inputs
!                        Secant_Flux_Angle = Secant_Flux_Angle,                 &  ! Optional Input, M
!                        n_Input_Profiles = n_Input_Profiles,                   &  ! Optional Input, Scalar
!
!                        ! -- Optional K-matrix outputs
!                        Solar_Reflectivity_K = Solar_Reflectivity_K,           &  ! Optional In/Output, L*M
!
!                        ! -- Error messaging
!                        Message_Log = Message_Log )
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
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau_Predictor:        Predictor profiles for the thermal flux transmittance.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau_Predictor:       Predictor profiles for the solar transmittance.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (I x K) or Rank-3 (I x K x M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau:                       Layer->TOA transmittance for the satellite
!                                  view angle.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau:                  Layer->SFC transmittance for the default
!                                  diffusivity angle.
!                                  UNITS:      N/A.
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau:                 Layer->SFC transmittance for the solar
!                                  zenith angle.
!                                  UNITS:      N/A.
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
!       Tau_K:                     Layer->TOA K-matrix transmittance for the satellite
!                                  view angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Flux_Tau_K:                Layer->SFC K-matrix transmittance for the default
!                                  diffusivity angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Solar_Tau_K:               Layer->SFC K-matrix transmittance for the solar
!                                  zenith angle.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_Radiance_K:          Layer Planck K-matrix radiances at every layer for
!                                  each channel/profile.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Downwelling_Radiance_K:    TOA->SFC K-matrix radiances for each channel/profile due
!                                  to thermal flux and solar components.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Upwelling_Radiance_K:      TOA K-matrix radiances for each channel/profile.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Brightness_Temperature_K:  K-matrix temperatures corresponding to the
!                                  TOA K-matrix radiances.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
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
!       n_Channels_per_Profile:    The number of channels for each profile in the
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
!                                              NB: This is a 1-D array.
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:        Profile set surface reflectivity array for the
!                                  solar term only. If not specified, the
!                                  Surface_Reflectivity argument is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:         Secant of the angle to be used to approximate
!                                  the downwelling flux transmittance for the INFRARED
!                                  only. If not specified a default value of 5/3 (1.6666..)
!                                  is used.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
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
!       Level_P_K:                 Profile set layer interface pressure K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_P_K:                 Profile set layer average pressure K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_T_K:                 Profile set layer average temperature K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_W_K:                 Profile set layer average water vapor mixing ratio
!                                  K-matrix array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Layer_O_K:                 Profile set layer average ozone mixing ratio
!                                  K-matrix array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-2 (K x L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Temperature_K:     Profile set surface temperature K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Emissivity_K:      Profile set surface emissivity K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Reflectivity_K:    Profile set surface reflectivity K-matrix
!                                  array.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!
!       Solar_Reflectivity_K:      Profile set surface reflectivity K-matrix
!                                  array for the solar term only. If not
!                                  specified, the Surface_Reflectivity_K
!                                  argument contains this contribution.
!                                  UNITS:      Variable depending on what input
!                                              K-matrix argument is not zero. 
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1 (L*M)
!                                  ATTRIBUTES: INTENT( IN OUT ), OPTIONAL
!
! FUNCTION RESULT:
!
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
!      Compute_Absorber_Amount_AD: Subroutine to compute the adjoint
!                                  absorber profiles
!                                  SOURCE: ABSORBER_PROFILE module
!
!      Compute_Predictors_AD:      Subroutine to compute the adjoint 
!                                  transmittance predictor profiles.
!                                  SOURCE: PREDICTOR module
!
!      Compute_Transmittance_AD:   Subroutine to compute the adjoint
!                                  transmittance profiles.
!                                  SOURCE: TRANSMITTANCE module
!
!      Compute_Radiance_AD:        Subroutine to compute the TOA adjoint 
!                                  radiances and brightness temperatures.
!                                  SOURCE: RADIANCE module
!
! SIDE EFFECTS:
!       All INPUT K-matrix arguments are set to ZERO on OUTPUT.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION K_Matrix_RTM_rank2( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,                &  ! Input, K x M
             Surface_Temperature,                                        &  ! Input, M
             Surface_Emissivity, Surface_Reflectivity,                   &  ! Input, L*M
             Absorber,                                                   &  ! Input, 0:K x J x M
             Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor,     &  ! Input, Imax x K x M
             Tau, Flux_Tau, Solar_Tau,                                   &  ! Input, K x L*M
             Layer_Radiance,                                             &  ! Input, K x L*M
             Downwelling_Radiance, Upwelling_Radiance,                   &  ! Input, L*M
             ! -- K-matrix inputs
             Tau_K, Flux_Tau_K, Solar_Tau_K,                             &  ! In/Output, K x L*M
             Layer_Radiance_K,                                           &  ! In/Output, K x L*M
             Downwelling_Radiance_K, Upwelling_Radiance_K,               &  ! In/Output, L*M
             Brightness_Temperature_K,                                   &  ! In/Output, L*M
             ! -- Other inputs
             Secant_View_Angle,                                          &  ! Input, M
             Secant_Solar_Angle,                                         &  ! Input, M
             n_Channels_per_Profile,                                     &  ! Input, M
             Channel_Index,                                              &  ! Input, L*M
             ! -- K-matrix outputs
             Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K,      &  ! In/Output, K x L*M
             Surface_Temperature_K,                                      &  ! In/Output, L*M
             Surface_Emissivity_K, Surface_Reflectivity_K,               &  ! In/Output, L*M
             ! -- Optional forward inputs
             Solar_Reflectivity,                                         &  ! Optional Input, L*M
             ! -- Other optional inputs
             Secant_Flux_Angle,                                          &  ! Optional Input, M
             n_Input_Profiles,                                           &  ! Optional Input, scalar
             ! -- Optional K-matrix outputs
             Solar_Reflectivity_K,                                       &  ! Optional In/Output, L*M
             ! -- Error messaging
             Message_Log )                                               &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Level_P                    ! K x M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Layer_P                    ! K x M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Layer_T                    ! K x M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Layer_W                    ! K x M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Layer_O                    ! K x M

    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Surface_Temperature        ! M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Surface_Emissivity         ! L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Surface_Reflectivity       ! L*M

    REAL( fp_kind ),           DIMENSION( 0:, :, : ), INTENT( IN )     :: Absorber                   ! 0:K x J x M

    REAL( fp_kind ),           DIMENSION( :, :, : ),  INTENT( IN )     :: Tau_Predictor              ! Imax x K x M
    REAL( fp_kind ),           DIMENSION( :, :, : ),  INTENT( IN )     :: Flux_Tau_Predictor         ! Imax x K x M
    REAL( fp_kind ),           DIMENSION( :, :, : ),  INTENT( IN )     :: Solar_Tau_Predictor        ! Imax x K x M

    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Tau                        ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Flux_Tau                   ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Solar_Tau                  ! K x L*M

    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN )     :: Layer_Radiance             ! K x L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Downwelling_Radiance       ! L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Upwelling_Radiance         ! L*M

    ! -- K-matrix inputs
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Tau_K                      ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Flux_Tau_K                 ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Solar_Tau_K                ! K x L*M

    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Layer_Radiance_K           ! K x L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Downwelling_Radiance_K     ! L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Upwelling_Radiance_K       ! L*M

    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Brightness_Temperature_K   ! L*M

    ! -- Other inputs
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Secant_View_Angle          ! M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN )     :: Secant_Solar_Angle         ! M
    INTEGER,                   DIMENSION( : ),        INTENT( IN )     :: n_Channels_per_Profile     ! M
    INTEGER,                   DIMENSION( : ),        INTENT( IN )     :: Channel_Index              ! L*M

    ! -- K-matrix outputs
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Level_P_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Layer_P_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Layer_T_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Layer_W_K                  ! K x L*M
    REAL( fp_kind ),           DIMENSION( :, : ),     INTENT( IN OUT ) :: Layer_O_K                  ! K x L*M

    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Surface_Temperature_K      ! L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Surface_Emissivity_K       ! L*M
    REAL( fp_kind ),           DIMENSION( : ),        INTENT( IN OUT ) :: Surface_Reflectivity_K     ! L*M

    ! -- Optional Forward input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),        INTENT( IN )     :: Solar_Reflectivity         ! L*M

    ! -- Other optional inputs
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),        INTENT( IN )     :: Secant_Flux_Angle          ! Scalar
    INTEGER,         OPTIONAL,                        INTENT( IN )     :: n_Input_Profiles           ! Scalar

    ! -- Optional K-Matrix output
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),        INTENT( IN OUT)  :: Solar_Reflectivity_K       ! L*M

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                        INTENT( IN )     :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'K_Matrix_RTM(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 256 ) :: Message
    CHARACTER( 5 )   :: Value_In, Value_Allowed

    INTEGER :: m, n_Profiles          ! Profile loop variables
    INTEGER :: l, l1, l2, n_Channels  ! Channel loop/index variables

    INTEGER :: Valid_Solar

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: Is_Set

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity    ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity_K  ) ) :: Solar_Reflectivity_Used_K
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
        WRITE( Message, '( "Invalid N_INPUT_PROFILES value: ", i5, &
                          &". Using pressure array dimension value of ", i5, "." )' ) &
                        n_Input_Profiles, n_Profiles
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              WARNING,         &
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

    IF ( ANY( n_Channels_per_Profile < 0 ) ) THEN
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


    IF ( PRESENT( Solar_Reflectivity_K ) ) THEN
      Solar_Reflectivity_Used_K = Solar_Reflectivity_K
    ELSE
      Solar_Reflectivity_Used_K = Surface_Reflectivity_K
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

      IF ( n_Channels_per_Profile( m ) == 0 ) CYCLE Profile_Loop


      ! -------------------------------------------
      ! Determine the end channel index index range
      ! -------------------------------------------

      l2 = l1 + n_Channels_per_Profile( m ) - 1


      ! -----------------------
      ! Call the K-matrix model
      ! -----------------------

      Error_Status = K_Matrix_RTM_rank1( &
        ! -- Forward inputs
        Level_P( :, m ), Layer_P( :, m ), Layer_T( :, m ),                                       &  ! Input,  K
        Layer_W( :, m ), Layer_O( :, m ),                                                        &  ! Input,  K
        Surface_Temperature( m ),                                                                &  ! Input,  Scalar
        Surface_Emissivity( l1:l2 ), Surface_Reflectivity( l1:l2 ),                              &  ! Input,  L
        Absorber( 0:, :, m ),                                                                    &  ! Input, 0:K x J
        Tau_Predictor( :, :, m ), Flux_Tau_Predictor( :, :, m ), Solar_Tau_Predictor( :, :, m ), &  ! Input, Imax x K
        Tau( :, l1:l2 ), Flux_Tau( :, l1:l2 ), Solar_Tau( :, l1:l2 ),                            &  ! Input, K x L
        Layer_Radiance( :, l1:l2 ),                                                              &  ! Input, K x L
        Downwelling_Radiance( l1:l2 ), Upwelling_Radiance( l1:l2 ),                              &  ! Input, L
        ! -- K-matrix inputs
        Tau_K( :, l1:l2 ), Flux_Tau_K( :, l1:l2 ), Solar_Tau_K( :, l1:l2 ),                      &  ! In/Output, K x L
        Layer_Radiance_K( :, l1:l2 ),                                                            &  ! In/Output, K x L
        Downwelling_Radiance_K( l1:l2 ), Upwelling_Radiance_K( l1:l2 ),                          &  ! In/Output, L
        Brightness_Temperature_K( l1:l2 ),                                                       &  ! In/Output, L
        ! -- Other inputs
        Secant_View_Angle( m ), Secant_Solar_Angle( m ),                                         &  ! Input, Scalar
        n_Channels_per_Profile( m ),                                                             &  ! Input, Scalar   
        Channel_Index( l1:l2 ),                                                                  &  ! Input, L        
        ! -- K-matrix outputs
        Level_P_K( :, l1:l2 ), Layer_P_K( :, l1:l2 ), Layer_T_K( :, l1:l2 ),                     &  ! In/Output,  K x L
        Layer_W_K( :, l1:l2 ), Layer_O_K( :, l1:l2 ),                                            &  ! In/Output,  K x L
        Surface_Temperature_K( l1:l2 ),                                                          &  ! In/Output, L
        Surface_Emissivity_K( l1:l2 ), Surface_Reflectivity_K( l1:l2 ),                          &  ! In/Output, L
        ! -- Optional forward inputs
        Solar_Reflectivity = Solar_Reflectivity_Used( l1:l2 ),                                   &  ! Optional Input, L
        ! -- Other optional inputs
        Secant_Flux_Angle = Secant_Flux_Angle_Used( m ),                                         &  ! Optional Input, Scalar
        ! -- Optional K-matrix outputs
        Solar_Reflectivity_K = Solar_Reflectivity_Used_K( l1:l2 ),                               &  ! Optional In/Output, L
        ! -- Error Messaging
        Message_Log = Message_Log )


      ! -------------------------------
      ! Check for successful completion
      ! -------------------------------

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error occured in K_Matrix_RTM(Rank-1) for profile #", i5 )' ) m
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

  END FUNCTION K_Matrix_RTM_rank2




  FUNCTION K_Matrix_RTM_rank1( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input, K
             Surface_Temperature,                                    &  ! Input, Scalar
             Surface_Emissivity, Surface_Reflectivity,               &  ! Input, L
             Absorber,                                               &  ! Input, 0:K x J
             Tau_Predictor, Flux_Tau_Predictor, Solar_Tau_Predictor, &  ! Input, Imax x K
             Tau, Flux_Tau, Solar_Tau,                               &  ! Input, K x L
             Layer_Radiance,                                         &  ! Input, K x L
             Downwelling_Radiance, Upwelling_Radiance,               &  ! Input, L
             ! -- K-matrix inputs
             Tau_K, Flux_Tau_K, Solar_Tau_K,                         &  ! In/Output, K x L
             Layer_Radiance_K,                                       &  ! In/Output, K x L
             Downwelling_Radiance_K, Upwelling_Radiance_K,           &  ! In/Output, L
             Brightness_Temperature_K,                               &  ! In/Output, L
             ! -- Other inputs
             Secant_View_Angle, Secant_Solar_Angle,                  &  ! Input, Scalar
             n_Channels_Per_Profile,                                 &  ! Input, Scalar
             Channel_Index,                                          &  ! Input, L
             ! -- K-matrix outputs
             Level_P_K, Layer_P_K, Layer_T_K, Layer_W_K, Layer_O_K,  &  ! In/Output, K x L
             Surface_Temperature_K,                                  &  ! In/Output, L
             Surface_Emissivity_K, Surface_Reflectivity_K,           &  ! In/Output, L
             ! -- Optional forward inputs
             Solar_Reflectivity,                                     &  ! Optional Input, L
             ! -- Other optional inputs
             Secant_Flux_Angle,                                      &  ! Optional Input, Scalar
             n_Input_Profiles,                                       &  ! Optional Input, Scalar
             ! -- Optional K-matrix outputs
             Solar_Reflectivity_K,                                   &  ! Optional In/Output, L
             ! -- Error messaging
             Message_Log )                                           &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Level_P                   ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Layer_P                   ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Layer_T                   ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Layer_W                   ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Layer_O                   ! K

    REAL( fp_kind ),                               INTENT( IN )     :: Surface_Temperature       ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Surface_Emissivity        ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Surface_Reflectivity      ! L

    REAL( fp_kind ),           DIMENSION( 0:, : ), INTENT( IN )     :: Absorber                  ! 0:K x J

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Tau_Predictor             ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Flux_Tau_Predictor        ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Solar_Tau_Predictor       ! Imax x K

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Tau                       ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Flux_Tau                  ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Solar_Tau                 ! K x L

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN )     :: Layer_Radiance            ! K x L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Downwelling_Radiance      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )     :: Upwelling_Radiance        ! L

    ! -- K-matrix inputs
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Tau_K                     ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Flux_Tau_K                ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Solar_Tau_K               ! K x L

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Layer_Radiance_K          ! K x L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Downwelling_Radiance_K    ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Upwelling_Radiance_K      ! L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Brightness_Temperature_K  ! L

    ! -- Other inputs
    REAL( fp_kind ),                               INTENT( IN )     :: Secant_View_Angle         ! Scalar
    REAL( fp_kind ),                               INTENT( IN )     :: Secant_Solar_Angle        ! Scalar
    INTEGER,                                       INTENT( IN )     :: n_Channels_Per_Profile    ! Scalar
    INTEGER,                   DIMENSION( : ),     INTENT( IN )     :: Channel_Index             ! L

    ! -- K-matrix outputs
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Level_P_K                 ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Layer_P_K                 ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Layer_T_K                 ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Layer_W_K                 ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( IN OUT ) :: Layer_O_K                 ! K x L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Surface_Temperature_K     ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Surface_Emissivity_K      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN OUT ) :: Surface_Reflectivity_K    ! L

    ! -- Optional Forward input
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )     :: Solar_Reflectivity        ! L

    ! -- Other optional inputs
    REAL( fp_kind ), OPTIONAL,                     INTENT( IN )     :: Secant_Flux_Angle         ! Scalar
    INTEGER,         OPTIONAL,                     INTENT( IN )     :: n_Input_Profiles          ! Scalar

    ! -- Optional K-matrix output
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN OUT)  :: Solar_Reflectivity_K      ! L

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                     INTENT( IN )     :: Message_Log
    
    
    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'K_Matrix_RTM(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 256 ) :: Message

    INTEGER :: k, n_layers   ! Layer loop index and dimension
    INTEGER :: l             ! Channel loop index variable

    INTEGER :: Valid_Solar

    ! -- Arrays for integrated absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( Absorber, DIM = 1 )-1, &
                                  SIZE( Absorber, DIM = 2 )    ) :: Tau_Absorber,      &
                                                                    Flux_Tau_Absorber, &
                                                                    Solar_Tau_Absorber

    ! -- Arrays for K-matrix of integrated absorber amounts, 0:K x J
    REAL( fp_kind ), DIMENSION( 0:SIZE( Absorber, DIM = 1 )-1, &
                                  SIZE( Absorber, DIM = 2 )    ) :: Absorber_K, &
                                                                    Tau_Absorber_K,      &
                                                                    Flux_Tau_Absorber_K, &
                                                                    Solar_Tau_Absorber_K

    ! -- Arrays for K-matrix predictors, Imax x K
    REAL( fp_kind ), DIMENSION( SIZE( Tau_Predictor, DIM = 1 ), &
                                SIZE( Tau_Predictor, DIM = 2 )  ) :: Tau_Predictor_K,      &
                                                                     Flux_Tau_Predictor_K, &
                                                                     Solar_Tau_Predictor_K

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity   ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity_K ) ) :: Solar_Reflectivity_Used_K
    REAL( fp_kind ) :: Secant_Flux_Angle_Used

    ! -- Array for checking input pressure profiles
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P ) - 1 ) :: dP



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL EXIT STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#           -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --               #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! Check the number of channels - if zero
    ! then simply RETURN.
    ! --------------------------------------

    IF ( n_Channels_Per_Profile == 0 ) RETURN


    ! ------------------
    ! Get the dimensions
    ! ------------------

    n_layers = SIZE( Layer_P, DIM = 1 )


    ! -----------------------------------
    ! Perform a simple check on the input
    ! data for negative values
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
    IF ( n_Channels_Per_Profile < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Negative number of channels passed..', &
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

    ! -- K-matrix solar reflectivity
    IF ( PRESENT( Solar_Reflectivity_K ) ) THEN
      Solar_Reflectivity_Used_K = Solar_Reflectivity_K
    ELSE
      Solar_Reflectivity_Used_K = Surface_Reflectivity_K
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
    !#            -- MODIFY ABSORBER QUANTITIES BY THE ANGLE SECANT --          #
    !#                                                                          #
    !# Could put a loop here but here's hoping the compiler recognises this as  #
    !# a group of loops over layer and optimises it as such.                    #
    !#--------------------------------------------------------------------------#

    ! -- For upwelling transmittance
    Tau_Absorber = Secant_View_Angle * Absorber

    ! -- Flux transmittance
    IF ( ANY( SC%Is_Microwave_Channel( Channel_Index( 1:n_Channels_Per_Profile ) ) == 0 ) ) THEN
      Flux_Tau_Absorber = Secant_Flux_Angle_Used * Absorber
    END IF

    ! -- Solar transmittance
    IF ( ( ANY( SC%Is_Solar_Channel( Channel_Index( 1:n_Channels_Per_Profile ) ) == 1 ) ) .AND. &
         Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE ) THEN
      Solar_Tau_Absorber = Secant_Solar_Angle * Absorber
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHANNEL LOOP --                            #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, n_Channels_Per_Profile


      ! ---------------------------------------------------------
      ! Initialise local, channel dependent, K-matrix variables
      !
      ! This initialisation may slow things down a bit if 
      ! a) the compiler isn't too clever about optimising and
      !    generates code that loops over each array separately
      ! b) there are a large number of channels.
      !
      ! The solution to (a) above is put in explicit loops, which
      ! may be necessary.
      !
      ! The alternative to (b) above is to define the arrays
      ! with a channel dimension which could be problematical
      ! memorywise for lots of channels.
      !
      ! These will eventually be moved outside both the channel
      ! and profile loop as once they are initialised, they are
      ! set to zero in the relevant adjoint routine on output.
      !----------------------------------------------------------

      ! -- Absorber arrays, 0:K x J
      Absorber_K           = ZERO
      Tau_Absorber_K       = ZERO
      Flux_Tau_Absorber_K  = ZERO
      Solar_Tau_Absorber_K = ZERO

      ! -- Predictor arrays, Imax x K
      Tau_Predictor_K       = ZERO
      Flux_Tau_Predictor_K  = ZERO
      Solar_Tau_Predictor_K = ZERO 


      ! ----------------------------------------------------
      ! Set the "this is a channel influenced by solar" flag
      ! ----------------------------------------------------

      Valid_Solar = 0

      IF ( SC%Is_Solar_Channel( Channel_Index( l ) ) == 1 .AND. &
           Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE       ) THEN
        Valid_Solar = 1
      END IF



      ! ------------------------------------------------
      ! Calculate the adjoint of the current channel TOA
      ! radiance or brightness temperature
      ! ------------------------------------------------

      CALL Compute_Radiance_AD( &
                                ! -- Forward input
                                Layer_T,                        &  ! Input, K
                                Surface_Temperature,            &  ! Input, scalar
                                Surface_Emissivity( l ),        &  ! Input, scalar
                                Surface_Reflectivity( l ),      &  ! Input, scalar
                                Solar_Reflectivity( l ),        &  ! Input, scalar
                                Tau(      :, l ),               &  ! Input, K
                                Flux_Tau( :, l ),               &  ! Input, K
                                Solar_Tau( n_Layers, l ),       &  ! Input, scalar
                                Layer_Radiance( :, l ),         &  ! Input, K
                                Downwelling_Radiance( l ),      &  ! Input, scalar
                                Upwelling_Radiance( l ),        &  ! Input, scalar
                                ! -- K-matrix input
                                Layer_Radiance_K( :, l ),       &  ! In/Output, K
                                Downwelling_Radiance_K( l ),    &  ! In/Output, scalar
                                Upwelling_Radiance_K( l ),      &  ! In/Output, scalar
                                Brightness_Temperature_K( l ),  &  ! In/Output, scalar
                                ! -- Other input
                                Secant_Solar_Angle,             &  ! Input, scalar
                                Valid_Solar,                    &  ! Input, scalar
                                Channel_Index( l ),             &  ! Input, scalar
                                ! -- K-matrix output
                                Layer_T_K( :, l ),              &  ! In/Output, K
                                Surface_Temperature_K( l ),     &  ! In/Output, scalar
                                Surface_Emissivity_K( l ),      &  ! In/Output, scalar
                                Surface_Reflectivity_K( l ),    &  ! In/Output, scalar
                                Solar_Reflectivity_Used_K( l ), &  ! In/Output, scalar
                                Tau_K( :, l ),                  &  ! In/Output, K
                                Flux_Tau_K( :, l ),             &  ! In/Output, K
                                Solar_Tau_K( n_layers, l )      )  ! In/Output, scalar



      !#------------------------------------------------------------------------#
      !#         -- CALCULATE THE K-MATRIX RESULT FOR THE SOLAR TERM --         #
      !#------------------------------------------------------------------------#

      ! ----------------------------------------------------
      ! If the current channel is a SOLAR SENSITIVE channel,
      !   AND
      ! the solar angle is valid, then calculate the adjoint
      ! of the transmittance for direct solar.
      ! ----------------------------------------------------

      Solar_K_Matrix: IF ( Valid_Solar == 1 ) THEN


        ! -----------------------------------------
        ! Adjoint of the direct solar transmittance
        ! -----------------------------------------

        CALL Compute_Transmittance_AD( &
                                       ! -- Forward input
                                       Solar_Tau_Absorber,   &   ! Input, 0:K x J
                                       Solar_Tau_Predictor,  &   ! Input, I x K
                                       Solar_Tau( :, l ),    &   ! Input, K
                                       ! -- K-matrix input
                                       Solar_Tau_K( :, l ),  &   ! In/Output, K
                                       ! -- Other input
                                       Channel_Index( l ),   &   ! Input, scalar
                                       DOWN,                 &   ! Input, scalar
                                       ! -- K-matrix output
                                       Solar_Tau_Absorber_K, &   ! In/Output, 0:K x J
                                       Solar_Tau_Predictor_K )   ! In/Output, I x K


        ! ---------------------------------------
        ! K-matrix of the standard predictor copy
        ! ---------------------------------------

        Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
                Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) + &
          Solar_Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : )

        Solar_Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) = ZERO


        ! --------------------------------------
        ! K-matrix of the integrateed predictors
        ! --------------------------------------

        CALL Compute_Predictors_AD( &
                                    ! -- Forward input
                                    Layer_P,               &  ! Input,  K
                                    Layer_T,               &  ! Input,  K
                                    Layer_W,               &  ! Input,  K
                                    Solar_Tau_Absorber,    &  ! Input,  0:K x J
                                    ! -- K-matrix input
                                    Solar_Tau_Predictor_K, &  ! In/Output, I x K
                                    ! -- K-matrix output
                                    Layer_P_K( :, l ),     &  ! In/Output,  K
                                    Layer_T_K( :, l ),     &  ! In/Output,  K
                                    Layer_W_K( :, l ),     &  ! In/Output,  K
                                    Solar_Tau_Absorber_K,  &  ! In/Output,  0:K x J

                                    No_Standard = SET      )  ! Optional input


        ! --------------------------------------------------
        ! K-matrix of the nadir absorber amount modification
        ! --------------------------------------------------

        Absorber_K = Absorber_K + ( Secant_Solar_Angle * Solar_Tau_Absorber_K )

      END IF Solar_K_Matrix



      !#------------------------------------------------------------------------#
      !#         -- CALCULATE THE K-MATRIX RESULT FOR THE FLUX TERM --          #
      !#------------------------------------------------------------------------#

      ! ----------------------------------------------------
      ! If the current channel is an INFRARED channel,
      ! then calculate the adjoint of the downwelling flux
      ! transmittance using the flux absorber amounts.
      !
      ! If the current channel is a MICROWAVE channel,
      ! then calculate the adjoint of the flux transmittance
      ! using the upwelling absorber amounts.
      ! ----------------------------------------------------

      Flux_K_Matrix: IF ( SC%Is_Microwave_Channel( Channel_Index( l ) ) == 0 ) THEN


        ! ------------------------------------
        ! Adjoint of the IR flux transmittance
        ! ------------------------------------

        CALL Compute_Transmittance_AD( &
                                       ! -- Forward input
                                       Flux_Tau_Absorber,   &   ! Input, 0:K x J
                                       Flux_Tau_Predictor,  &   ! Input, I x K
                                       Flux_Tau( :, l ),    &   ! Input, K
                                       ! -- K-matrix input
                                       Flux_Tau_K( :, l ),  &   ! In/Output, K
                                       ! -- Other input
                                       Channel_Index( l ),  &   ! Input, scalar
                                       DOWN,                &   ! Input, scalar
                                       ! -- K-matrix output
                                       Flux_Tau_Absorber_K, &   ! In/Output, 0:K x J
                                       Flux_Tau_Predictor_K )   ! In/Output, I x K


        ! ---------------------------------------
        ! K-matrix of the standard predictor copy
        ! ---------------------------------------

        Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
               Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) + &
          Flux_Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : )

        Flux_Tau_Predictor_K( 1:MAX_N_STANDARD_PREDICTORS, : ) = ZERO


        ! --------------------------------------
        ! K-matrix of the integrateed predictors
        ! --------------------------------------

        CALL Compute_Predictors_AD( &
                                    ! -- Forward input
                                    Layer_P,              &  ! Input,  K
                                    Layer_T,              &  ! Input,  K
                                    Layer_W,              &  ! Input,  K
                                    Flux_Tau_Absorber,    &  ! Input,  0:K x J
                                    ! -- K-matrix input
                                    Flux_Tau_Predictor_K, &  ! In/Output, I x K
                                    ! -- K-matrix output
                                    Layer_P_K( :, l ),    &  ! In/Output,  K
                                    Layer_T_K( :, l ),    &  ! In/Output,  K
                                    Layer_W_K( :, l ),    &  ! In/Output,  K
                                    Flux_Tau_Absorber_K,  &  ! In/Output,  0:K x J

                                    No_Standard = SET     )  ! Optional input


        ! --------------------------------------------------
        ! K-matrix of the nadir absorber amount modification
        ! --------------------------------------------------

        Absorber_K = Absorber_K + ( Secant_Flux_Angle_Used * Flux_Tau_Absorber_K )


      ELSE  ! We have a microwave channel....


        ! ----------------------------------------------
        ! If total transmittance /= 0, calculate adjoint
        ! flux transmittance for the microwave
        ! ----------------------------------------------
      
        IF ( Tau( n_layers, l ) > TOLERANCE ) THEN
          DO k = n_layers, 2, -1
            Flux_Tau_K( 1, l ) = Flux_Tau_K( 1, l ) + ( Flux_Tau_K( k, l ) / &
            !                                           ------------------
                                                           Tau( k-1, l )   )

            Tau_K( k-1, l ) = Tau_K( k-1, l ) - ( Flux_Tau_K( k, l ) * Flux_Tau( 1, l ) / &
            !                                     -------------------------------------
                                                              Tau( k-1, l )**2          )
            Flux_Tau_K( k, l ) = ZERO
          END DO
          Tau_K( n_layers, l ) = Tau_K( n_layers, l ) + Flux_Tau_K( 1, l )
          Flux_Tau_K( 1, l ) = ZERO
        ELSE
          Flux_Tau_K( :, l ) = ZERO
        END IF

      END IF Flux_K_Matrix



      !#------------------------------------------------------------------------#
      !#       -- CALCULATE THE K-MATRIX RESULT FOR THE TRANSMITTANCE --        #
      !#------------------------------------------------------------------------#

      ! -----------------------------------------------------
      ! Calculate the adjoint of the upwelling transmittances
      ! for the satellite view angle
      ! -----------------------------------------------------

      CALL Compute_Transmittance_AD( &
                                     ! -- Forward input
                                     Tau_Absorber,       &   ! Input, 0:K x J
                                     Tau_Predictor,      &   ! Input, I x K
                                     Tau( :, l ),        &   ! Input, K
                                     ! -- K-matrix input
                                     Tau_K( :, l ),      &   ! In/Output, K
                                     ! -- Other input
                                     Channel_Index( l ), &   ! Input, scalar
                                     UP,                 &   ! Input, scalar
                                     ! -- K-matrix output
                                     Tau_Absorber_K,     &   ! In/Output, 0:K x J
                                     Tau_Predictor_K     )   ! In/Output, I x K


      ! ------------------------------
      ! K-matrix of all the predictors
      ! ------------------------------

      CALL Compute_Predictors_AD( &
                                  ! -- Forward input
                                  Layer_P,           &  ! Input,  K
                                  Layer_T,           &  ! Input,  K
                                  Layer_W,           &  ! Input,  K
                                  Tau_Absorber,      &  ! Input,  0:K x J
                                  ! -- K-matrix input
                                  Tau_Predictor_K,   &  ! In/Output, I x K
                                  ! -- K-matrix output
                                  Layer_P_K( :, l ), &  ! In/Output,  K
                                  Layer_T_K( :, l ), &  ! In/Output,  K
                                  Layer_W_K( :, l ), &  ! In/Output,  K
                                  Tau_Absorber_K     )  ! In/Output,  0:K x J


      ! --------------------------------------------------
      ! K-matrix of the nadir absorber amount modification
      ! --------------------------------------------------

      Absorber_K = Absorber_K + ( Secant_View_Angle * Tau_Absorber_K )



      !#------------------------------------------------------------------------#
      !#                  -- CALCULATE THE ABSORBER K-MATRIX --                 #
      !#------------------------------------------------------------------------#

      CALL Compute_Absorber_Amount_AD( &
                                       ! -- Forward input
                                       Level_P,           &  ! Input,  K
                                       Layer_W,           &  ! Input,  K
                                       Layer_O,           &  ! Input,  K
                                       ! -- K-matrix input
                                       Absorber_K,        &  ! In/Output, 0:K x J
                                       ! -- K-matrix output
                                       Level_P_K( :, l ), &  ! In/Ouput,  K
                                       Layer_W_K( :, l ), &  ! In/Ouput,  K
                                       Layer_O_K( :, l )  )  ! In/Ouput,  K


    END DO Channel_Loop



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE OPTIONAL K-MATRIX ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -- K-matrix solar reflectivity
    IF ( PRESENT( Solar_Reflectivity_K ) ) THEN
      Solar_Reflectivity_K = Solar_Reflectivity_Used_K
    ELSE
      Surface_Reflectivity_K = Surface_Reflectivity_K + Solar_Reflectivity_Used_K
    END IF

  END FUNCTION K_Matrix_RTM_rank1

END MODULE K_Matrix_Model


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: k_matrix_model.f90,v 2.5 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: k_matrix_model.f90,v $
! Revision 2.5  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.4  2004/12/28 20:27:29  paulv
! - Added check for monotonicity of input pressure profiles.
! - Added check for input surface emissivity/reflectivity > 1.0.
!
! Revision 2.3  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.2  2004/06/21 20:23:21  paulv
! - Fixed a bug in the dimensionality of the optional secant flux angle
!   argument definition in k_matrix_rtm_rank2.
!
! Revision 2.1  2004/06/13 15:12:29  paulv
! - Corrected bug in error checking code.
!
! Revision 2.0  2004/06/12 19:25:00  paulv
! - New version with optional arguments for solar reflectivity and flux
!   angle. Initial tests outputting microwave dTb/dTsfc indicate a bug....
!   somewhere.
!
! Revision 1.10  2004/06/10 19:25:30  paulv
! - Cosmetic changes only.
!
! Revision 1.9  2003/05/02 18:48:24  paulv
! - Updated for use with new transmittance algorithm and spectral coefficients
!   module. Layer index arrays are no longer required and the spectral
!   coefficients are stored in a structure rather than as separate arrays.
!
! Revision 1.8  2001/11/07 15:08:08  paulv
! - Changed the logical IF test for the number of input profiles in the
!   [<TANGENT_LINEAR><ADJOINT><K_MATRIX>]_RTM_RANK2() functions from
!     IF ( n_input_profiles >= 1 .AND. n_input_profiles <= n_profiles ) THEN
!   to
!     IF ( n_input_profiles > 0 .AND. n_input_profiles <= n_profiles ) THEN
!   The use of both the ">=" and "<=" relational operators with the .AND. I
!   found confusing.
!
! Revision 1.7  2001/11/07 14:49:24  paulv
! - Corrected adjoint variable units documentation.
! - Added check for negative number of channels to K_MATRIX_RTM_RANK2() function.
! - Added profile loop CYCLE statement to K_MATRIX_RTM_RANK2() function.
! - Added check for negative number of channels to K_MATRIX_RTM_RANK1() function.
! - Fixed an apparent copy/paste bug in the K_MATRIX_RTM_RANK1() function. The
!   absorber amount modification by the diffusivity angle for the flux transmittance
!   was followed by
!     flux_tau_absorber( 0:, : ) = secant_view_angle * absorber( 0:, : )
!   making the angular dependence the same as for the downlooking transmittance.
!   This line was removed.
!
! Revision 1.6  2001/10/01 20:06:16  paulv
! - Minor cosmetic changes
!
! Revision 1.5  2001/09/28 22:53:14  paulv
! - Overloaded the COMPUTE_RTM_K and K_MATRIX_RTM functions to accept both a
!   single or group of profiles. Contained PRIVATE functions are now
!     o COMPUTE_RTM_K_RANK1 and K_MATRIX_RTM_RANK1 for single profile input
!     o COMPUTE_RTM_K_RANK2 and K_MATRIX_RTM_RANK2 for multiple profile input
! - Put N_INPUT_PROFILES optional argument back in the COMPUTE_RTM and FORWARD_RTM
!   argument lists. This allows more control over how many profiles are to be
!   processed rather than simply relying on the dimension of the input arrays.
!   Now, as before,
!     n_profiles = SIZE( layer_p, DIM = 2 )
!   but also,
!     IF ( PRESENT( n_input_profiles ) ) THEN
!       IF ( n_input_profiles >= 1 .AND. n_input_profiles <= n_profiles ) THEN
!         n_profiles = n_input_profiles
!     ....
!   The check for N_INPUT_PROFILES is only performed in the K_MATRIX_RTM_RANK2
!   function.
! - Changed SURFACE_TEMPERATURE argument check from
!     IF ( ANY( surface_temperature < ZERO ) )
!   to
!     IF (      surface_temperature < ZERO )
!   as the check is now done in the rank-1 K_MATRIX_RTM function. This eliminates
!   the need to fully populate the input arrays with data when only certain
!   "chunks" may be processed. Previously, the use of ANY() could generate
!   and error if the full surface_temperature array was not initialised.
! - Added "Name" to RCS keyword list.
!
! Revision 1.4  2001/09/04 21:29:11  paulv
! - Updated documentation.
!
! Revision 1.3  2001/08/31 21:28:52  paulv
! - Removed input data checks from COMPUTE_RTM_K. The same checks are performed
!   in the main routine, K_MATRIX_RTM, so there was no need to replicate them.
! - Added check for negative profile and surface data in K_MATRIX_RTM.
! - Maximum solar angle secant is no longer calculated in K_MATRIX_RTM but
!   is declared as a parameter in the PARAMETERS module.
!
! Revision 1.2  2001/08/16 16:38:57  paulv
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
! Revision 1.1  2001/08/01 16:34:02  paulv
! Initial checkin.
!
!
!
!
