!--------------------------------------------------------------------------------
!M+
! NAME:
!       Forward_Model
!
! PURPOSE:
!       Module containing the prototype CRTM (pCRTM) forward function.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Forward_Model
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
!       Spectral_Coefficients: Module containing the spectral coefficients.
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
! CONTAINS:
!       Compute_RTM:           Function that calculates the forward model top-
!                              of-atmosphere (TOA) radiances and brightness 
!                              temperatures for an input atmospheric profile
!                              set and user specified satellites/channels.
!
!                              This function is simply a wrapper around the
!                              FORWARD_RTM function so that the user doesn't
!                              have to declare the absorber/predictor/etc
!                              arrays in the calling routine.
!
!       Forward_RTM:           Function that calculates top-of-atmosphere
!                              (TOA) radiances and brightness temperatures
!                              for user specified profiles and satellites/
!                              channels.
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
!  Copyright (C) 2000 Paul van Delst
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

MODULE Forward_Model


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds,            ONLY : fp_kind
  USE Message_Handler
  USE Parameters
  USE Spectral_Coefficients

  USE Absorber_Profile,      ONLY : Compute_Absorber_Amount
  USE Predictors,            ONLY : Compute_Predictors
  USE Transmittance,         ONLY : Compute_Transmittance
  USE Radiance,              ONLY : Compute_Radiance


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Compute_RTM
  PUBLIC :: Forward_RTM


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE Compute_RTM
    MODULE PROCEDURE Compute_RTM_rank1
    MODULE PROCEDURE Compute_RTM_rank2
  END INTERFACE Compute_RTM

  INTERFACE Forward_RTM
    MODULE PROCEDURE Forward_RTM_rank1
    MODULE PROCEDURE Forward_RTM_rank2
  END INTERFACE Forward_RTM


  ! -------------------------
  ! PRIVATE module parameters
  ! -------------------------

  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: forward_model.f90,v 2.21 2006/05/02 14:58:35 dgroff Exp $'

  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_RTM
!
! PURPOSE:
!       Function that calculates the forward model top-of-atmosphere (TOA)
!       radiances and brightness temperatures for an input atmospheric profile
!       set and user specified satellites/channels.
!
!       This function is simply a wrapper around the FORWARD model so that the
!       user doesn't have to declare the Absorber/predictor/etc arrays in the
!       calling routine.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Compute_RTM( &
!                        ! -- Forward inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O, &  ! Input, K x M
!                        Surface_Temperature,                         &  ! Input, M
!                        Surface_Emissivity, Surface_Reflectivity,    &  ! Input, L*M
!
!                        ! -- Other inputs
!                        Secant_View_Angle,                           &  ! Input, M
!                        Secant_Solar_Angle,                          &  ! Input, M
!                        n_Channels_Per_Profile,                      &  ! Input, M
!                        Channel_Index,                               &  ! Input, L*M
!
!                        ! -- Forward output
!                        Tau, Flux_Tau, Solar_Tau,                    &  ! Output, K x L*M
!                        Upwelling_Radiance,                          &  ! Output, L*M
!                        Brightness_Temperature,                      &  ! Output, L*M
!
!                        ! -- Optional inputs
!                        Solar_Reflectivity = Solar_Reflectivity,     &  ! Optional input, L*M
!                        Secant_Flux_Angle  = Secant_Flux_Angle,      &  ! Optional input, M
!                        n_Input_Profiles   = n_Input_Profiles,       &  ! Optional input, Scalar
!
!                        ! -- Error messaging
!                        Message_Log = Message_Log                    )
!
! INPUT ARGUMENTS:
!
!       Level_P:                Profile set layer interface pressure array. The TOA
!                               pressure is not included. TOA pressure is parameterised
!                               in the PARAMETERS module.
!                               UNITS:      hPa
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Layer_P:                Profile set layer average pressure array.
!                               UNITS:      hPa
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Layer_T:                Profile set layer average temperature array.
!                               UNITS:      Kelvin
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Layer_W:                Profile set layer average water vapor mixing ratio array
!                               UNITS:      g/kg
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Layer_O:                Profile set layer average ozone mixing ratio array.
!                               UNITS:      ppmv
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:    Profile set surface temperature array.
!                               UNITS:      Kelvin
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Scalar or Rank-1 (M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:     Profile set surface emissivity array
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:   Profile set surface reflectivity array
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( IN )
!
!       Secant_View_Angle:      Secant of the satellite view angle measured
!                               from nadir for each profile in the set.
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Scalar or Rank-1 (M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:     Secant of the solar zenith angle for each
!                               profile in the set.
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Scalar or Rank-1 (M)
!                               ATTRIBUTES: INTENT( IN )
!
!       n_Channels_Per_Profile: The number of channels for each profile in the
!                               set for which radiances are required.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar or Rank-1 (M)
!                               ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:          Channel index id array. Each element is a unique
!                               index to a (supported) sensor channel.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:     Profile set surface reflectivity array for the
!                               solar term only. If not specified, the
!                               Surface_Reflectivity argument is used.
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:      Secant of the angle to be used to approximate
!                               the downwelling flux transmittance for the INFRARED
!                               only. If not specified a default value of 5/3 (1.6666..)
!                               is used.
!                               UNITS:      N/A
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Scalar or Rank-1 (M)
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       n_Input_Profiles:       The number of profiles in the passed arrays to process.
!                               If not specified, the default value is the SECOND dimension
!                               of the pressure array determined using the SIZE intrinsic,
!                               N_PROFILES. If N_INPUT_PROFILES is specified and is < 1 or
!                               greater than N_PROFILES, the default value is set to N_PROFILES.
!                               This argument is ignored if the input profile arrays are
!                               vectors, i.e. a single profile.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:            Character string specifying a filename in which any
!                               messages will be logged. If not specified, or if an
!                               error occurs opening the log file, the default action
!                               is to output messages to the screen.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER( * )
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!
!       Tau:                    Layer->TOA transmittance for the satellite
!                               view angle.
!                               UNITS:      N/A.
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-2 (K x L*M)
!                               ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau:               Layer->SFC transmittance for the default
!                               diffusivity angle.
!                               UNITS:      N/A.
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-2 (K x L*M)
!                               ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau:              Layer->SFC transmittance for the solar
!                               zenith angle.
!                               UNITS:      N/A.
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-2 (K x L*M)
!                               ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance:     TOA radiances for each channel/profile.
!                               UNITS:      mW/(m^2.sr.cm^-1)
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature: TOA brightness temperatures corresponding
!                               to the TOA radiances.
!                               UNITS:      Kelvin
!                               TYPE:       REAL( fp_kind )
!                               DIMENSION:  Rank-1 (L*M)
!                                           NB: This is a 1-D array.
!                               ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:           The return value is an integer defining the
!                               error status. The error codes are defined in
!                               the ERROR_HANDLER module.
!                               If == SUCCESS the calculation was successful.
!                                  == FAILURE an unrecoverable error occurred.
!                                  == WARNING a recoverable error occurred.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!
! CALLS:
!       Display_Message:        Subroutine to output messages
!                               SOURCE: ERROR_HANDLER module
!
!       Forward_RTM:            Function to construct the forward model and
!                               calculate the transmittance profiles and TOA
!                               radiance/temperatures.
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

  FUNCTION Compute_RTM_rank2( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O, &  ! Input, K x M
             Surface_Temperature,                         &  ! Input, M
             Surface_Emissivity, Surface_Reflectivity,    &  ! Input, L*M
             ! -- Other inputs
             Secant_View_Angle,                           &  ! Input, M
             Secant_Solar_Angle,                          &  ! Input, M
             n_Channels_Per_Profile,                      &  ! Input, M
             Channel_Index,                               &  ! Input, L*M
             ! -- Forward output
             Tau, Flux_Tau, Solar_Tau,                    &  ! Output, K x L*M
             Upwelling_Radiance,                          &  ! Output, L*M
             Brightness_Temperature,                      &  ! Output, L*M
             ! -- Optional inputs
             Solar_Reflectivity,                          &  ! Optional input, L*M  
             Secant_Flux_Angle,                           &  ! Optional input, M  
             n_Input_Profiles,                            &  ! Optional input, Scalar
             ! -- Error messaging
             Message_Log )                                &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O                 ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature     ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity    ! L*M

    ! -- Other inputs
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_View_Angle       ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_Solar_Angle      ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: n_Channels_Per_Profile  ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: Channel_Index           ! L*M

    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau                     ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau                ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau               ! K x L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature  ! L*M

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity      ! L*M
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

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
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance
      

    ! -- Array for downwelling radiance (flux + solar), L*M
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance




    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FORWARD RADIANCES AND TEMPERATURES --             #
    !#--------------------------------------------------------------------------#

    Error_Status = Forward_RTM_rank2( &
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
                            'Error occured in Forward_RTM(Rank-2)', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Compute_RTM_rank2



  FUNCTION Compute_RTM_rank1( &
             ! -- Forward inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O, &  ! Input, K
             Surface_Temperature,                         &  ! Input, Scalar
             Surface_Emissivity, Surface_Reflectivity,    &  ! Input, L
             ! -- Other inputs
             Secant_View_Angle,                           &  ! Input, Scalar
             Secant_Solar_Angle,                          &  ! Input, Scalar
             n_Channels,                                  &  ! Input, Scalar
             Channel_Index,                               &  ! Input, L
             ! -- Forward output
             Tau, Flux_Tau, Solar_Tau,                    &  ! Output, K x L
             Upwelling_Radiance,                          &  ! Output, L
             Brightness_Temperature,                      &  ! Output, L
             ! -- Optional inputs
             Solar_Reflectivity,                          &  ! Optional input, L
             Secant_Flux_Angle,                           &  ! Optional input, L
             n_Input_Profiles,                            &  ! Optional input, Scalar
             Message_Log )                                &  ! Error messaging

           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward inputs
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Level_P                 ! K
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Layer_P                 ! K
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Layer_T                 ! K
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Layer_W                 ! K
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Layer_O                 ! K

    REAL( fp_kind ),                           INTENT( IN )  :: Surface_Temperature     ! Scalar
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity      ! L
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity    ! L

    ! -- Other inputs
    REAL( fp_kind ),                           INTENT( IN )  :: Secant_View_Angle       ! Scalar
    REAL( fp_kind ),                           INTENT( IN )  :: Secant_Solar_Angle      ! Scalar
    INTEGER,                                   INTENT( IN )  :: n_Channels              ! Scalar
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: Channel_Index           ! L

    ! -- Forward outputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau                     ! K x L
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau                ! K x L
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau               ! K x L

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance      ! L
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature  ! L

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity      ! L
    REAL( fp_kind ),                 OPTIONAL, INTENT( IN )  :: Secant_Flux_Angle       ! Scalar
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_RTM(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

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
                                SIZE( Upwelling_Radiance ) ) :: Layer_Radiance
      

    ! -- Array for downwelling radiance (flux + solar), L
    REAL( fp_kind ), DIMENSION( SIZE( Upwelling_Radiance ) ) :: Downwelling_Radiance




    !#--------------------------------------------------------------------------#
    !#         -- COMPUTE THE FORWARD RADIANCES AND TEMPERATURES --             #
    !#--------------------------------------------------------------------------#

    Error_Status = Forward_RTM_rank1( &
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
                            'Error occured in Forward_RTM(Rank-1)', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Compute_RTM_rank1







!--------------------------------------------------------------------------------
!S+
! NAME:
!       Forward_RTM
!
! PURPOSE:
!       Function that calculates top-of-atmosphere (TOA) radiances
!       and brightness temperatures for an input atmospheric profile or
!       profile set and user specified satellites/channels.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!
!       Error_Status = Forward_RTM( &
!                        ! -- Inputs
!                        Level_P, Layer_P, Layer_T, Layer_W, Layer_O, &  ! Input, K x M
!
!                        Surface_Temperature,                         &  ! Input,  M
!                        Surface_Emissivity,                          &  ! Input,  L*M
!                        Surface_Reflectivity,                        &  ! Input,  L*M
!
!                        Secant_View_Angle,                           &  ! Input,  M
!                        Secant_Solar_Angle,                          &  ! Input,  M
!                        n_Channels_Per_Profile,                      &  ! Input,  M
!                        Channel_Index,                               &  ! Input,  L*M
!
!                        ! -- Outputs
!                        Absorber,                                    &  ! Output, 0:K x J x M
!
!                        Tau_Predictor,                               &  ! Output, Imax x K x M
!                        Flux_Tau_Predictor,                          &  ! Output, Imax x K x M
!                        Solar_Tau_Predictor,                         &  ! Output, Imax x K x M
!
!                        Tau,                                         &  ! Output, K x L*M
!                        Flux_Tau,                                    &  ! Output, K x L*M
!                        Solar_Tau,                                   &  ! Output, K x L*M
!
!                        Layer_Radiance,                              &  ! Output, K x L*M
!                        Downwelling_Radiance,                        &  ! Output, L*M
!                        Upwelling_Radiance,                          &  ! Output, L*M
!
!                        Brightness_Temperature,                      &  ! Output, L*M
!
!                        ! -- Optional inputs
!                        Solar_Reflectivity = Solar_Reflectivity,     &  ! Optional input, L*M
!                        Secant_Flux_Angle  = Secant_Flux_Angle,      &  ! Optional input, M
!                        n_Input_Profiles   = n_Input_Profiles,       &  ! Optional input, Scalar
!
!                        ! -- Error messaging
!                        Message_Log = Message_Log                    )
!                             
! INPUT ARGUMENTS:
!
!       Level_P:                 Profile set layer interface pressure array. The TOA
!                                pressure is not included. TOA pressure is parameterised
!                                in the PARAMETERS module.
!                                UNITS:      hPa
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Layer_P:                 Profile set layer average pressure array.
!                                UNITS:      hPa
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Layer_T:                 Profile set layer average temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Layer_W:                 Profile set layer average water vapor mixing ratio array
!                                UNITS:      g/kg
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Layer_O:                 Profile set layer average ozone mixing ratio array.
!                                UNITS:      ppmv
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (K) or Rank-2 (K x M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:     Profile set surface temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar or Rank-1 (M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:      Profile set surface emissivity array
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:    Profile set surface reflectivity array
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Secant_View_Angle:       Secant of the satellite view angle measured
!                                from nadir for each profile in the set.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar or Rank-1 (M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:      Secant of the solar zenith angle for each
!                                profile in the set.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar or Rank-1 (M)
!                                ATTRIBUTES: INTENT( IN )
!
!       n_Channels_Per_Profile:  The number of channels for each profile in the
!                                set for which radiances are required.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar or Rank-1 (M)
!                                ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:           Channel index id array. Each element is a unique
!                                index to a (supported) sensor channel.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!
!       Solar_Reflectivity:      Profile set surface reflectivity array for the
!                                solar term only. If not specified, the
!                                Surface_Reflectivity argument is used.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Secant_Flux_Angle:       Secant of the angle to be used to approximate
!                                the downwelling flux transmittance for the INFRARED
!                                only. If not specified a default value of 5/3 (1.6666..)
!                                is used.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar or Rank-1 (M)
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       n_Input_Profiles:        The number of profiles in the passed arrays to process.
!                                If not specified, the default value is the SECOND dimension
!                                of the pressure array determined using the SIZE intrinsic,
!                                N_PROFILES. If N_INPUT_PROFILES is specified and is < 1 or
!                                greater than N_PROFILES, the default value is set to N_PROFILES.
!                                This argument is ignored if the input profile arrays are
!                                vectors, i.e. a single profile.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:             Character string specifying a filename in which any
!                                messages will be logged. If not specified, or if an
!                                error occurs opening the log file, the default action
!                                is to output messages to the screen.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER( * )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!
!       Absorber:                Array of Absorber amount for nadir view.
!                                UNITS:      Absorber dependent.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3 (0:K x J x M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Tau_Predictor:           Predictor profiles for the layer->TOA transmittance.
!                                UNITS:      Predictor dependent.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3 (I x K x M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau_Predictor:      Predictor profiles for the thermal flux transmittance.
!                                UNITS:      Predictor dependent.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3 (I x K x M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau_Predictor:     Predictor profiles for the solar transmittance.
!                                UNITS:      Predictor dependent.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-3 (I x K x M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Tau:                     Layer->TOA transmittance for the satellite
!                                view angle.
!                                UNITS:      N/A.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2 (K x L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Flux_Tau:                Layer->SFC transmittance for the default
!                                diffusivity angle.
!                                UNITS:      N/A.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2 (K x L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Solar_Tau:               Layer->SFC transmittance for the solar
!                                zenith angle.
!                                UNITS:      N/A.
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2 (K x L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Layer_Radiance:          Layer Planck radiances at every layer for
!                                each channel/profile.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-2 (K x L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Downwelling_Radiance:    TOA->SFC radiances for each channel/profile due
!                                to thermal flux and solar components.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance:      TOA radiances for each channel/profile.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature:  Temperatures corresponding to the TOA radiances
!                                for each channel/profile.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Rank-1 (L*M)
!                                ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:            The return value is an integer defining the
!                                error status. The error codes are defined in
!                                the ERROR_HANDLER module.
!                                If == SUCCESS the calculation was successful.
!                                   == FAILURE an unrecoverable error occurred.
!                                   == WARNING a recoverable error occurred.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!
! CALLS:
!      Display_Message:          Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
!      Get_Max_n_Channels:       Routine to retrieve the value of the
!                                MAX_N_CHANNELS "pseudo-parameter".
!                                SOURCE: PARAMETERS module
!
!      Compute_Absorber_Amount:  Subroutine to integrate the absorber profiles
!                                SOURCE: ABSORBER_PROFILE module
!
!      Compute_Predictors:       Subroutine to compute the transmittance predictor
!                                profiles.
!                                SOURCE: PREDICTOR module
!
!      Compute_Transmittance:    Subroutine to compute the transmittance profiles.
!                                SOURCE: TRANSMITTANCE module
!
!      Compute_Radiance:         Subroutine to compute the TOA radiances and
!                                brightness temperatures.
!                                SOURCE: RADIANCE module
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

  FUNCTION Forward_RTM_rank2( &
             ! -- Inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input, K x M
             Surface_Temperature,                                    &  ! Input,  M
             Surface_Emissivity, Surface_Reflectivity,               &  ! Input,  L*M
             Secant_View_Angle, Secant_Solar_Angle,                  &  ! Input,  M
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
             Solar_Reflectivity,                                     &  ! Optional input,  L*M
             Secant_Flux_Angle,                                      &  ! Optional input,  M
             n_Input_Profiles,                                       &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                           &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Level_P                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_P                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_T                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_W                 ! K x M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( IN )  :: Layer_O                 ! K x M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Temperature     ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Emissivity      ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Surface_Reflectivity    ! L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_View_Angle       ! M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( IN )  :: Secant_Solar_Angle      ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: n_Channels_Per_Profile  ! M
    INTEGER,         DIMENSION( : ),           INTENT( IN )  :: Channel_Index           ! L*M

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( 0:, :, : ),    INTENT( OUT ) :: Absorber                ! 0:K x J x M

    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( OUT ) :: Tau_Predictor           ! Imax x K x M
    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( OUT ) :: Flux_Tau_Predictor      ! Imax x K x M
    REAL( fp_kind ), DIMENSION( :, :, : ),     INTENT( OUT ) :: Solar_Tau_Predictor     ! Imax x K x M

    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Tau                     ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Flux_Tau                ! K x L*M
    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Solar_Tau               ! K x L*M

    REAL( fp_kind ), DIMENSION( :, : ),        INTENT( OUT ) :: Layer_Radiance          ! K x L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Downwelling_Radiance    ! L*M
    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Upwelling_Radiance      ! L*M

    REAL( fp_kind ), DIMENSION( : ),           INTENT( OUT ) :: Brightness_Temperature  ! L*M

    ! -- Optional input
    REAL( fp_kind ), DIMENSION( : ), OPTIONAL, INTENT( IN )  :: Solar_Reflectivity      ! L*M
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

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Forward_RTM(Rank-2)'


    ! ---------------
    ! Local variables
    ! ---------------

    ! -- Scalars
    CHARACTER( 256 ) :: Message
    CHARACTER( 10 )  :: Value_Input, Value_Allowed

    INTEGER :: m, n_Profiles          ! Profile loop variables
    INTEGER :: l, l1, l2, n_Channels  ! Channel loop/index variables

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: is_set

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ), DIMENSION( SIZE( Secant_View_Angle    ) ) :: Secant_Flux_Angle_Used



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



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

      WRITE( Value_Input,   '( i5 )' ) n_Profiles
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_PROFILES
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of passed profiles ('// &
                            TRIM( ADJUSTL( Value_Input ) )// &
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

      WRITE( Value_Input,   '( i5 )' ) n_Channels
      WRITE( Value_Allowed, '( i5 )' ) MAX_N_CHANNELS
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of requested channels ('// &
                            TRIM( ADJUSTL( Value_Input ) )// &
                            ') > number of initialisation channels ('// &
                            TRIM( ADJUSTL( Value_Allowed ) )//').', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- SET THE OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! ------------------
    ! Solar reflectivity
    ! ------------------

    IF ( PRESENT( Solar_Reflectivity ) ) THEN
      Solar_Reflectivity_Used = Solar_Reflectivity
    ELSE
      Solar_Reflectivity_Used = Surface_Reflectivity
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

      Error_Status = Forward_RTM_rank1( &
                       ! -- Forward inputs
                       Level_P( :, m ),                   &  ! Input,  K
                       Layer_P( :, m ),                   &  ! Input,  K
                       Layer_T( :, m ),                   &  ! Input,  K
                       Layer_W( :, m ),                   &  ! Input,  K
                       Layer_O( :, m ),                   &  ! Input,  K
                       Surface_Temperature( m ),          &  ! Input,  Scalar
                       Surface_Emissivity( l1:l2 ),       &  ! Input,  L
                       Surface_Reflectivity( l1:l2 ),     &  ! Input,  L
                       ! -- Other inputs
                       Secant_View_Angle( m ),            &  ! Input,  Scalar
                       Secant_Solar_Angle( m ),           &  ! Input,  Scalar
                       n_Channels_Per_Profile( m ),       &  ! Input,  Scalar
                       Channel_Index( l1:l2 ),            &  ! Input,  L
                       ! -- Outputs
                       Absorber( 0:, :, m ),              &  ! Output, 0:K x J
                       Tau_Predictor( :, :, m ),          &  ! Output, Imax x K
                       Flux_Tau_Predictor( :, :, m ),     &  ! Output, Imax x K
                       Solar_Tau_Predictor( :, :, m ),    &  ! Output, Imax x K
                       Tau( :, l1:l2 ),                   &  ! Output, K x L
                       Flux_Tau( :, l1:l2 ),              &  ! Output, K x L
                       Solar_Tau( :, l1:l2 ),             &  ! Output, K x L
                       Layer_Radiance( :, l1:l2 ),        &  ! Output, K x L
                       Downwelling_Radiance( l1:l2 ),     &  ! Output, L
                       Upwelling_Radiance( l1:l2 ),       &  ! Output, L
                       Brightness_Temperature( l1:l2 ),   &  ! Output, L
                       ! -- Optional inputs
                       Solar_Reflectivity = Solar_Reflectivity_Used( l1:l2 ), &  ! Input,  L
                       Secant_Flux_Angle  = Secant_Flux_Angle_Used( m ),      &  ! Input,  Scalar
                       ! -- Error messaging
                       Message_Log = Message_Log )


      ! -------------------------------
      ! Check for successful completion
      ! -------------------------------

      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error occured in Forward_RTM(Rank-1) for profile #", i5 )' ) m
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

  END FUNCTION Forward_RTM_rank2


  FUNCTION Forward_RTM_rank1( &
             ! -- Inputs
             Level_P, Layer_P, Layer_T, Layer_W, Layer_O,            &  ! Input,  K
             Surface_Temperature,                                    &  ! Input,  Scalar
             Surface_Emissivity, Surface_Reflectivity,               &  ! Input,  L
             Secant_View_Angle, Secant_Solar_Angle,                  &  ! Input,  Scalar
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
             Solar_Reflectivity,                                     &  ! Optional input,  L
             Secant_Flux_Angle,                                      &  ! Optional input,  Scalar
             n_Input_Profiles,                                       &  ! Optional input,  Scalar
             ! -- Error messaging
             Message_Log )                                           &  ! Error messaging
           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Level_P                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_P                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_T                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_W                 ! K
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Layer_O                 ! K

    REAL( fp_kind ),                               INTENT( IN )  :: Surface_Temperature     ! Scalar
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Emissivity      ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( IN )  :: Surface_Reflectivity    ! L

    REAL( fp_kind ),                               INTENT( IN )  :: Secant_View_Angle       ! Scalar
    REAL( fp_kind ),                               INTENT( IN )  :: Secant_Solar_Angle      ! Scalar
    INTEGER,                                       INTENT( IN )  :: n_Channels              ! Scalar
    INTEGER,                   DIMENSION( : ),     INTENT( IN )  :: Channel_Index           ! L

    ! -- Outputs
    REAL( fp_kind ),           DIMENSION( 0:, : ), INTENT( OUT ) :: Absorber                ! 0:K x J

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Tau_Predictor           ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Flux_Tau_Predictor      ! Imax x K
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Solar_Tau_Predictor     ! Imax x K

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Tau                     ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Flux_Tau                ! K x L
    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Solar_Tau               ! K x L

    REAL( fp_kind ),           DIMENSION( :, : ),  INTENT( OUT ) :: Layer_Radiance          ! K x L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Downwelling_Radiance    ! L
    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Upwelling_Radiance      ! L

    REAL( fp_kind ),           DIMENSION( : ),     INTENT( OUT ) :: Brightness_Temperature  ! L

    ! -- Optional input.
    REAL( fp_kind ), OPTIONAL, DIMENSION( : ),     INTENT( IN )  :: Solar_Reflectivity      ! L
    REAL( fp_kind ), OPTIONAL,                     INTENT( IN )  :: Secant_Flux_Angle       ! Scalar
    INTEGER,         OPTIONAL,                     INTENT( IN )  :: n_Input_Profiles        ! Scalar

    ! -- Error messaging
    CHARACTER( * ),  OPTIONAL,                     INTENT( IN )  :: Message_Log
    

    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Forward_RTM(Rank-1)'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n_Layers   ! Layer dimension
    INTEGER :: l  ,k      ! Channel loop/index variables

    INTEGER :: Valid_Solar

    ! -- Maximum channels pseudo parameter
    INTEGER :: MAX_N_CHANNELS
    LOGICAL :: is_set

    ! -- Arrays for integrated absorber amounts.
    REAL( fp_kind ), DIMENSION( 0:SIZE( Absorber, DIM = 1 )-1, &
                                  SIZE( Absorber, DIM = 2 )    ) :: Tau_Absorber,      &
                                                                    Flux_Tau_Absorber, &
                                                                    Solar_Tau_Absorber

    ! -- Values for optional arguments
    REAL( fp_kind ), DIMENSION( SIZE( Surface_Reflectivity ) ) :: Solar_Reflectivity_Used
    REAL( fp_kind ) :: Secant_Flux_Angle_Used

    ! -- Array for checking input pressure profiles
    REAL( fp_kind ), DIMENSION( SIZE( Layer_P ) - 1 ) :: dP



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#           -- DETERMINE ARRAY DIMENSIONS AND CHECK INPUT --               #
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
    !#           -- CALCULATE THE PROFILE GENERIC ABSORBER AMOUNTS --           #
    !#--------------------------------------------------------------------------#

    CALL Compute_Absorber_Amount( Level_P, &  ! Input,  K
                                  Layer_W, &  ! Input,  K
                                  Layer_O, &  ! Input,  K

                                  Absorber )  ! Output, 0:K x J



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE PREDICTORS FOR THE UPWELLING TRANSMITTANCE --      #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------------
    ! Modify absorber quantities by the angle secant
    ! Could put a loop here but here's hoping the compiler
    ! recognises this as a group of loops over layer.
    ! ------------------------------------------------------

    Tau_Absorber = Secant_View_Angle * Absorber


    ! -----------------------------------------------------
    ! Calculate the predictors for the satellite view angle
    ! -----------------------------------------------------

    CALL Compute_Predictors( Layer_P,      &  ! Input,  K
                             Layer_T,      &  ! Input,  K
                             Layer_W,      &  ! Input,  K
                             Tau_Absorber, &  ! Input,  0:K x J

                             Tau_Predictor )  ! Output, I x K



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

      Flux_Tau_Absorber = Secant_Flux_Angle_Used * Absorber


      ! --------------------------------
      ! Calculate the predictors for the
      ! diffusivity angle
      ! --------------------------------

      ! -- Calculate the integrated predictors only
      CALL Compute_Predictors( Layer_P,            &  ! Input,  K
                               Layer_T,            &  ! Input,  K
                               Layer_W,            &  ! Input,  K
                               Flux_Tau_Absorber,  &  ! Input,  0:K x J

                               Flux_Tau_Predictor, &  ! Output, I x K

                               No_Standard = 1     )  ! Optional input

      ! -- Copy the angle independent (standard) predictors
      Flux_Tau_Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
           Tau_Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) 

    END IF
     


    !#--------------------------------------------------------------------------#
    !#       -- CALCULATE THE PREDICTORS FOR THE SOLAR TRANSMITTANCE --         #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------
    ! Have *any* SOLAR sensitive channels been specified
    ! for the current profile (Flagged as == 1)?
    !
    ! AND
    !
    ! Is the specified solar zenith angle valid?
    ! --------------------------------------------------

    IF ( ( ANY( SC%Is_Solar_Channel( Channel_Index( 1:n_Channels ) ) == 1 ) ) .AND. &
         Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE ) THEN


      ! --------------------------------
      ! Modify the nadir absorber amount
      ! --------------------------------

      Solar_Tau_Absorber = Secant_Solar_Angle * Absorber


      ! --------------------------------
      ! Calculate the predictors for the
      ! solar zenith angle
      ! --------------------------------

      ! -- Calculate the integrated predictors only
      CALL Compute_Predictors( Layer_P,             &  ! Input,  K
                               Layer_T,             &  ! Input,  K
                               Layer_W,             &  ! Input,  K
                               Solar_Tau_Absorber,  &  ! Input,  0:K x J

                               Solar_Tau_Predictor, &  ! Output, I x K

                               No_Standard = 1      )  ! Optional input

      ! -- Copy the angle independent predictors
      Solar_Tau_Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) = &
            Tau_Predictor( 1:MAX_N_STANDARD_PREDICTORS, : ) 

    END IF
     


    !#--------------------------------------------------------------------------#
    !#                           -- CHANNEL LOOP --                             #
    !#--------------------------------------------------------------------------#

    Channel_Loop: DO l = 1, n_Channels


      ! --------------------------------------------------
      ! Calculate the current channel layer transmittances
      ! for the satellite view angle
      ! --------------------------------------------------

      CALL Compute_Transmittance( Tau_Absorber,       &   ! Input, 0:K x J
                                  Tau_Predictor,      &   ! Input, I x K
                                  Channel_Index( l ), &   ! Input, scalar
                                  UP,                 &   ! Input, scalar

                                  Tau( :, l )         )   ! Output, K


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
        CALL Compute_Transmittance( Flux_Tau_Absorber,  &   ! Input, 0:K x J
                                    Flux_Tau_Predictor, &   ! Input, I x K
                                    Channel_Index( l ), &   ! Input, scalar
                                    DOWN,               &   ! Input, scalar

                                    Flux_Tau( :, l )    )   ! Output, K

      ELSE


        ! -- uW channel

!  This can be considered a "hook" for future versions where
!  downwelling will not be derived from the upwelling.
!
!        CALL Compute_Transmittance( Tau_Absorber,       &   ! Input, 0:K x J
!                                    Tau_Predictor,      &   ! Input, I x K
!                                    Channel_Index( l ), &   ! Input, scalar
!                                    DOWN,               &   ! Input, scalar
!
!                                    Flux_Tau( :, l )    )   ! Output, K

        ! -- This gives the identical result as a separate call
        ! -- but without the extra computational burden.
        Flux_Tau( :, l ) = ZERO
        IF ( Tau( n_Layers, l ) > TOLERANCE ) THEN
          Flux_Tau( 1, l ) = Tau( n_Layers, l )
          Flux_Tau( 2:n_Layers, l ) =     Flux_Tau( 1, l ) / &
          !                           ------------------------
                                       Tau( 1:n_Layers-1, l )
        END IF

      END IF



      ! ----------------------------------------------------
      ! If the current channel is a SOLAR SENSITIVE channel,
      !   AND
      ! the solar angle is valid, then calculate the
      ! transmittance for direct solar.
      ! ----------------------------------------------------

      IF ( SC%Is_Solar_Channel( Channel_Index( l ) ) == 1 .AND. &
           Secant_Solar_Angle < MAX_SECANT_SOLAR_ANGLE ) THEN

        Valid_Solar = 1

        CALL Compute_Transmittance( Solar_Tau_Absorber,  &   ! Input, 0:K x J
                                    Solar_Tau_Predictor, &   ! Input, I x K
                                    Channel_Index( l ),  &   ! Input, scalar
                                    DOWN,                &   ! Input, scalar

                                    Solar_Tau( :, l )    )   ! Output, K

      ELSE

        Valid_Solar = 0

        Solar_Tau( :, l ) = ZERO

      END IF


      ! ---------------------------------------
      ! Calculate the profile/channel radiances
      ! ---------------------------------------

      CALL Compute_Radiance( Layer_T,                      &  ! Input, K

                             Surface_Temperature,          &  ! Input, scalar
                             Surface_Emissivity( l ),      &  ! Input, scalar
                             Surface_Reflectivity( l ),    &  ! Input, scalar
                             Solar_Reflectivity_Used( l ), &  ! Input, scalar

                             Tau(      :, l ),             &  ! Input, K
                             Flux_Tau( :, l ),             &  ! Input, K
                             Solar_Tau( n_Layers, l ),     &  ! Input, scalar

                             Secant_Solar_Angle,           &  ! Input, scalar
                             Valid_Solar,                  &  ! Input, scalar
                             Channel_Index( l ),           &  ! Input, scalar

                             Layer_Radiance( :, l ),       &  ! Output, K
                             Downwelling_Radiance( l ),    &  ! Output, scalar
                             Upwelling_Radiance( l ),      &  ! Output, scalar

                             Brightness_Temperature( l )   )  ! Output, scalar

    END DO Channel_Loop

  END FUNCTION Forward_RTM_rank1

END MODULE Forward_Model


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: forward_model.f90,v 2.21 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 2.21 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: forward_model.f90,v $
! Revision 2.21  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 2.20  2004/12/28 20:27:29  paulv
! - Added check for monotonicity of input pressure profiles.
! - Added check for input surface emissivity/reflectivity > 1.0.
!
! Revision 2.19  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.18  2004/10/04 21:30:56  paulv
! - Removed intrinsics declaration.
!
! Revision 2.17  2004/06/11 23:02:16  paulv
! - Corrected minor bug in checking invalid input angle secants.
! - Updated some header documentation.
!
! Revision 2.16  2004/06/10 19:15:13  paulv
!  Updated documentation.
!
! Revision 2.15  2004/03/02 21:24:50  paulv
! - Added solar reflectivity and secant flux angle as optional arguments
!   to all routines. This required temporary arrays in some case.
! - Updated documentation to reflect changes.
! - Added checks for negative values for secant view and solar angle arguments.
! - Added solar reflectivity argument to the Compute_Radiance() call.
!
! Revision 2.14  2003/07/10 16:27:32  paulv
! - Corrected some header documentation.
!
! Revision 2.13  2003/05/02 18:48:23  paulv
! - Updated for use with new transmittance algorithm and spectral coefficients
!   module. Layer index arrays are no longer required and the spectral
!   coefficients are stored in a structure rather than as separate arrays.
!
! Revision 2.12  2001/11/07 15:03:15  paulv
! - Added check for negative number of channels to FORWARD_RTM_RANK2() function.
! - Added profile loop CYCLE statement to FORWARD_RTM_RANK2() function.
! - Added check for negative number of channels to FORWARD_RTM_RANK1() function.
! - Changed the logical IF test for the number of input profiles in the
!   FORWARD_RTM_RANK2() function from
!     IF ( n_input_profiles >= 1 .AND. n_input_profiles <= n_profiles ) THEN
!   to
!     IF ( n_input_profiles > 0 .AND. n_input_profiles <= n_profiles ) THEN
!   The use of both the ">=" and "<=" realtional operators with the .AND. I
!   found confusing.
!
! Revision 2.11  2001/09/28 22:44:24  paulv
! - Overloaded the COMPUTE_RTM and FORWARD_RTM functions to accept both a
!   single or group of profiles. Contained PRIVATE functions are now
!     o COMPUTE_RTM_RANK1 and FORWARD_RTM_RANK1 for single profile input
!     o COMPUTE_RTM_RANK2 and FORWARD_RTM_RANK2 for multiple profile input
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
! - Changed SURFACE_TEMPERATURE argument check from
!     IF ( ANY( surface_temperature < ZERO ) )
!   to
!     IF (      surface_temperature < ZERO )
!   as the check is now done in the rank-1 FORWARD_RTM function. This eliminates
!   the need to fully populate the input arrays with data when only certain
!   "chunks" may be processed. Previously, the use of ANY() could generate
!   and error if the full surface_temperature array was not initialised.
! - Added "Name" to RCS keyword list.
!
! Revision 2.10  2001/09/04 21:29:11  paulv
! - Updated documentation.
!
! Revision 2.9  2001/08/31 21:22:23  paulv
! - Removed input data checks from COMPUTE_RTM. The same checks are performed
!   in the main routine, FORWARD_RTM, so there was no need to replicate them.
! - Added check for negative profile and surface data in FORWARD_RTM.
! - Maximum solar angle secant is no longer calculated in FORWARD_RTM but
!   is declared as a parameter in the PARAMETERS module.
!
! Revision 2.8  2001/08/16 16:36:29  paulv
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
! Revision 2.7  2001/08/01 16:47:59  paulv
! - Removed USE of ABSORBER_SPACE module.
! - ONLY clauses added to other USE statements so only those module members
!   required in this module are visible.
! - Added COMPUTE_RTM function. This is a wrapper for the FORWARD_RTM function.
! - Updated input argument checking. Now consistent with other model
!   components.
!
! Revision 2.6  2001/07/12 18:41:37  paulv
! - Commented out informational message output at start of function.
!
! Revision 2.5  2001/05/29 18:21:02  paulv
! - Now use TYPE_KINDS module parameter FP_KIND to set the floating point
!   data type.
! - Added ABSORBER_SPACE and PREDICTORS module use to provide access to
!   the absorber space definitions and the predictor calculation routines.
! - All angle parameters placed in the PARAMETERS module.
! - Argument list increased to provide data for other component RTM calls
!   (e.g. TL or AD).
! - For full listing of differences, do a cvs diff between revisions 2.4
!   and 2.5 (this one).
!
! Revision 2.4  2001/01/24 20:14:21  paulv
! - Latest test versions.
!
! Revision 2.3  2000/11/09 20:58:28  paulv
! - Made radiance function call consistent with changes to that module.
!   (specifically regarding the solar, flux, and reflectivity terms).
!
! Revision 2.2  2000/08/31 19:36:32  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 2.1  2000/08/24 18:08:28  paulv
! - Many changes from initial version. Too many to list here.
! - Updated module and subprogram documentation.
!
!
!
!
!
