!------------------------------------------------------------------------------
!M+
! NAME:
!       Radiance
!
! PURPOSE:
!       Module containing routines to perform rudimentary radiative transfer
!       in the prototype CRTM (pCRTM)
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE Radiance
!
! MODULES:
!       Type_Kinds:              Module containing data type kind definitions.
!
!       Parameters:              Module containing parameter definitions for the
!                                pCRTM.
!                                USEs: TYPE_KINDS module
!
!       Spectral_Coefficients:   Module containing the spectral coefficients.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!                                      PARAMETERS module
!                                      SPCCOEFF_DEFINE module
!                                      SPCCOEFF_BINARY_IO module
!
!       Sensor_Planck_Routines:  Module containing the Planck radiance and
!                                temperature subroutines. 
!                                USEs: TYPE_KINDS module
!                                      PARAMETERS module
!                                      SPECTRAL_COEFFICIENTS module
!
! CONTAINS:
!       Compute_Radiance:        Subroutine to calculate the channel TOA
!                                radiance and brightness temperature.
!
!       Compute_Radiance_TL:     Subroutine to calculate the tangent-
!                                linear TOA radiance and brightness temperature.
!
!       Compute_Radiance_AD:     Subroutine to calculate the adjoint of
!                                the TOA radiance and brightness temperature.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2000
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

MODULE Radiance


  ! ---------------------
  ! Module use statements
  ! ---------------------

  USE Type_Kinds, ONLY : fp_kind
  USE Parameters
  USE Spectral_Coefficients
  USE Sensor_Planck_Routines


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC  :: Compute_Radiance
  PUBLIC  :: Compute_Radiance_TL
  PUBLIC  :: Compute_Radiance_AD


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Radiance
!
! PURPOSE:
!       Subroutine to calculate the TOA radiance and brightness temperature.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compute_Radiance( Temperature,           &  ! Input, K
!                              Surface_Temperature,   &  ! Input, scalar
!                              Surface_Emissivity,    &  ! Input, scalar
!                              Surface_Reflectivity,  &  ! Input, scalar
!                              Solar_Reflectivity,    &  ! Input, scalar
!                              Tau,                   &  ! Input, K
!                              Flux_Tau,              &  ! Input, K
!                              Solar_Tau,             &  ! Input, scalar
!                              Secant_Solar_Angle,    &  ! Input, scalar
!                              Valid_Solar,           &  ! Input, scalar
!                              Channel_Index,         &  ! Input, scalar
!                              Layer_Radiance,        &  ! Output, K
!                              Downwelling_Radiance,  &  ! Output, scalar
!                              Upwelling_Radiance,    &  ! Output, scalar
!                              Brightness_Temperature )  ! Output, scalar
!
!
! INPUT ARGUMENTS:
!       Temperature:             Profile LAYER average temperature array.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:     Surface boundary temperature.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:      Surface boundary emissivity
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:    Surface boundary reflectivity
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Solar_Reflectivity:      Surface boundary reflectivity for solar
!                                term only.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Tau:                     Layer-to-space transmittance profile for
!                                a particular satellite view angle.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau:                Layer-to-surface transmittance profile for
!                                either the diffuse approximation angle (IR)
!                                or the satellite view angle (MW). The latter
!                                assumes specular reflectivity.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K
!                                ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau:               Total space-to-surface transmittance at the
!                                solar zenith angle.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:      Secant of the solar zenith angle corresponding
!                                to that used in calculating the total solar
!                                transmittance.
!                                UNITS:      N/A
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Valid_Solar:             Flag indicating if the solar component should
!                                be included.
!                                If = 0, no solar (if sensor channel frequency
!                                        is less than a preset cutoff or if solar
!                                        zenith angle is greater than its preset
!                                         cutoff.)
!                                   = 1, include solar
!                                UNITS:      N/A.
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:           Channel index id. This is a unique index
!                                to a (supported) sensor channel.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Layer_Radiance:          Channel Planck radiance for every input layer.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  K.
!                                ATTRIBUTES: INTENT( OUT )
!
!       Downwelling_Radiance:    Channel radiance at surface due to downwelling
!                                flux.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance:      Channel TOA radiance simulating the satellite
!                                sensor measurement. This is composed of the
!                                reflected downwelling and solar propagated
!                                through the atmosphere as well as the upwelling
!                                only component.
!                                UNITS:      mW/(m^2.sr.cm^-1)
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature:  Channel TOA brightness temperature.
!                                UNITS:      Kelvin
!                                TYPE:       REAL( fp_kind )
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Sensor_Planck_Radiance:    Function to compute the Planck radiance
!                                  for a specified channel given the temperature.
!                                  SOURCE: SENSOR_PLANCK_ROUTINES module
!
!       Sensor_Planck_Temperature: Function to compute the Planck temperature
!                                  for a specified channel given the radiance.
!                                  SOURCE: SENSOR_PLANCK_ROUTINES module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The downwelling radiance is first initialised to the space emissisio
!       boundary term using precalculated cosmic background radiances,
!
!         R_down = CBR * Flux_Tau(1)
!
!       where the emissivity of space is implicitly assumed to be 1.0 and
!       Flux_Tau(1) is the space-to-ground transmittance.
!
!       The contributions of all the layers EXCEPT THE SURFACE LAYER to the
!       downwelling flux is accumulated,
!
!                            __K-1
!                           \
!         R_down = R_down +  >  B(k) * dFlux_Tau(k)
!                           /__
!                              k=1
!
!       The surface layer contribution is then added explicitly,
!
!         R_down = R_down + ( B(K) * ( 1 - Flux_Tau(K) ) )
!
!       to avoid exceeding the arrays bounds of Flux_Tau 
!       (i.e. Flux_Tau(K+1) == 1.0 ) or requiring Flux_Tau to be
!       dimensioned 0:K.
!
!       The solar term is then computed if required,
!
!         R_solar = r_solar * I_solar * Solar_Tau * COS(theta_solar)
!
!       or set to zero if not.
!
!       The downwelling radiance is then reflected off the surface, added
!       to the solar and surface emission term, propagated upwards through the atmosphere
!       and used to initialise the upwelling radiance term,
!
!         R_up = ( ( e_sfc * B_sfc ) + ( r_sfc * R_down ) + R_solar ) * Tau(K)
!
!       The contributions of all the layers EXCEPT THE TOP LAYER to the
!       upwelling radiance is accumulated,
!
!                        __ 2
!                       \
!         R_up = R_up +  >  B(k) * dTau(k)
!                       /__
!                          k=K
!
!       The top layer contribution is then added explicitly,
!
!         R_up = R_up + ( B(1) * ( 1 - Tau(1) ) )
!
!       to avoid exceeding the arrays bounds of Tau (i.e. Tau(0) == 1.0 )
!       or requiring Tau to be dimensioned 0:K.
!
!       The final upwelling radiance is then converted to a brightness
!       temperature.
!
!S-      
!--------------------------------------------------------------------------------

  SUBROUTINE Compute_Radiance( Temperature,           &  ! Input, K      
                               Surface_Temperature,   &  ! Input, scalar 
                               Surface_Emissivity,    &  ! Input, scalar 
                               Surface_Reflectivity,  &  ! Input, scalar 
                               Solar_Reflectivity,    &  ! Input, scalar 
                               Tau,                   &  ! Input, K      
                               Flux_Tau,              &  ! Input, K      
                               Solar_Tau,             &  ! Input, scalar 
                               Secant_Solar_Angle,    &  ! Input, scalar 
                               Valid_Solar,           &  ! Input, scalar 
                               Channel_Index,         &  ! Input, scalar 
                               Layer_Radiance,        &  ! Output, K     
                               Downwelling_Radiance,  &  ! Output, scalar
                               Upwelling_Radiance,    &  ! Output, scalar
                               Brightness_Temperature )  ! Output, scalar



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Inputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Temperature
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Emissivity
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Reflectivity
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Reflectivity
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Tau
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Flux_Tau
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Tau
    REAL( fp_kind ),                 INTENT( IN )  :: Secant_Solar_Angle
    INTEGER,                         INTENT( IN )  :: Valid_Solar
    INTEGER,                         INTENT( IN )  :: Channel_Index

    ! -- Outputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Layer_Radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: Downwelling_Radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: Upwelling_Radiance
    REAL( fp_kind ),                 INTENT( OUT ) :: Brightness_Temperature

 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n_Layers
    INTEGER :: l

    REAL( fp_kind ) :: Solar_Radiance
    REAL( fp_kind ) :: Surface_B



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    n_Layers = SIZE( Temperature )



    !#--------------------------------------------------------------------------#
    !#              -- CALCULATE THE DOWNWELLING THERMAL FLUX --                #
    !#--------------------------------------------------------------------------#

    ! -- Assign the channel index to a short name
    l = Channel_Index


    ! --------------------------------------------
    ! Initialise the downwelling radiance to the
    ! space emission boundary term reaching the
    ! surface. Thhe cosmic background radiance is
    ! zero for infrared channels and precalculated
    ! for microwave channels. The emissivity of
    ! space is assumed to be 1.0.
    !
    ! Cosmic background data from the
    ! SPECTRAL_COEFFICIENTS module
    ! --------------------------------------------

    Downwelling_Radiance = SC%Cosmic_Background_Radiance( l ) * Flux_Tau( 1 )


    ! --------------------------------
    ! Loop over layers from TOA->SFC-1
    ! --------------------------------

    Down_Layer_Loop: DO k = 1, n_Layers - 1

      ! -- Calculate the Planck layer radiance
      CALL Sensor_Planck_Radiance( l,                  &  ! Input
                                   Temperature( k ),   &  ! Input
                                   Layer_Radiance( k ) )  ! Output

      ! -- Accumulate absorption and emission for current layer.
      ! -- LTE assumed.
      Downwelling_Radiance = Downwelling_Radiance + &
                             ( Layer_Radiance( k ) * ( Flux_Tau( k+1 ) - Flux_Tau( k ) ) )

    END DO Down_Layer_Loop


    ! ----------------------------------
    ! Flux bottom layer (closest to SFC)
    ! ----------------------------------

    ! -- Lowest layer Planck radiance
    CALL Sensor_Planck_Radiance( l,                         &  ! Input
                                 Temperature( n_Layers ),   &  ! Input
                                 Layer_Radiance( n_Layers ) )  ! Output

    ! -- Contribution of lowest layer. Note that at the
    ! -- surface, a transmittance of 1.0 is used.
    Downwelling_Radiance = Downwelling_Radiance + &
                           ( Layer_Radiance( n_Layers ) * ( ONE - Flux_Tau( n_Layers ) ) )



    !#--------------------------------------------------------------------------#
    !#             -- CALCULATE THE DOWNWELLING DIRECT SOLAR TERM --            #
    !#--------------------------------------------------------------------------#

    Solar_Term: IF ( Valid_Solar == 1 ) THEN

      Solar_Radiance = SC%Solar_Irradiance( l ) * Solar_Tau / &
      !                ------------------------------------
                                Secant_Solar_Angle

    ELSE

      Solar_Radiance  = ZERO

    END IF Solar_Term



    !#--------------------------------------------------------------------------#
    !#   -- REFLECT THE DOWNWELLING RADIANCE AND ADD THE SURFACE EMISSION --    #
    !#   -- AND USE IT TO INITIALISE THE UPWELLING RADIANCE               --    #
    !#--------------------------------------------------------------------------#

    ! -- Calculate the surface term
    CALL Sensor_Planck_Radiance( l,                   &  ! Input
                                 Surface_Temperature, &  ! Input
                                 Surface_B            )  ! Output

    ! -- Initialise upwelling radiance
    Upwelling_Radiance = ( ( Surface_Emissivity   * Surface_B            ) + &
                           ( Surface_Reflectivity * Downwelling_Radiance ) + &
                           ( Solar_Reflectivity   * Solar_Radiance       )   ) * Tau( n_Layers )



    !#--------------------------------------------------------------------------#
    !#                  -- CALCULATE THE UPWELLING RADIANCE --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Loop over layers from SFC->TOA-1
    ! --------------------------------

    Up_Layer_Loop: DO k = n_Layers, 2, -1

      Upwelling_Radiance = Upwelling_Radiance + &
                           ( Layer_Radiance( k ) * ( Tau( k-1 ) - Tau( k ) ) )

    END DO Up_Layer_Loop


    ! --------------------------
    ! Top layer (closest to TOA)
    ! --------------------------

    Upwelling_Radiance = Upwelling_Radiance + &
                         ( Layer_Radiance( 1 ) * ( ONE - Tau( 1 ) ) )



    !#--------------------------------------------------------------------------#
    !#           -- CONVERT THE RADIANCES TO BRIGHTNESS TEMPERATURES --         #
    !#--------------------------------------------------------------------------#

    CALL Sensor_Planck_Temperature( l,                     &  ! Input
                                    Upwelling_Radiance,    &  ! Input
                                    Brightness_Temperature )  ! Output

  END SUBROUTINE Compute_Radiance



!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Radiance_TL
!
! PURPOSE:
!       Subroutine to calculate the tangent-linear TOA radiance and
!       brightness temperature.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
! CALLING SEQUENCE:
!       CALL Compute_Radiance_TL( &
!                                 ! -- Forward inputs
!                                 Temperature,              &  ! Input, K
!                                 Surface_Temperature,      &  ! Input, scalar
!                                 Surface_Emissivity,       &  ! Input, scalar
!                                 Surface_Reflectivity,     &  ! Input, scalar
!                                 Solar_Reflectivity,       &  ! Input, scalar
!                                 Tau,                      &  ! Input, K
!                                 Flux_Tau,                 &  ! Input, K
!                                 Solar_Tau,                &  ! Input, scalar
!                                 Layer_Radiance,           &  ! Input, K
!                                 Downwelling_Radiance,     &  ! Input, scalar
!                                 Upwelling_Radiance,       &  ! Input, scalar
!
!                                 ! -- Tangent-linear inputs
!                                 Temperature_TL,           &  ! Input, K
!                                 Surface_Temperature_TL,   &  ! Input, scalar
!                                 Surface_Emissivity_TL,    &  ! Input, scalar
!                                 Surface_Reflectivity_TL,  &  ! Input, scalar
!                                 Solar_Reflectivity_TL,    &  ! Input, scalar
!                                 Tau_TL,                   &  ! Input, K
!                                 Flux_Tau_TL,              &  ! Input, K
!                                 Solar_Tau_TL,             &  ! Input, scalar
!
!                                 ! -- Other inputs
!                                 Secant_Solar_Angle,       &  ! Input, scalar
!                                 Valid_Solar,              &  ! Input, scalar
!                                 Channel_Index,            &  ! Input, scalar
!
!                                 ! -- Tangent-linear outputs
!                                 Layer_Radiance_TL,        &  ! Output, K
!                                 Downwelling_Radiance_TL,  &  ! Output, scalar
!                                 Upwelling_Radiance_TL,    &  ! Output, scalar
!                                 Brightness_Temperature_TL )  ! Output, scalar
!
!
! INPUT ARGUMENTS:
!       Temperature:               Profile LAYER average temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:       Surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:        Surface boundary emissivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:      Surface boundary reflectivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Reflectivity:        Surface boundary reflectivity for solar
!                                  term only.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau:                       Layer-to-space transmittance profile for
!                                  a particular satellite view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau:                  Layer-to-surface transmittance profile for
!                                  either the diffuse approximation angle (IR)
!                                  or the satellite view angle (MW). The latter
!                                  assumes specular reflectivity.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau:                 Total space-to-surface transmittance at the
!                                  solar zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_Radiance:            Channel Planck radiance for every layer.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Downwelling_Radiance:      Channel radiance at surface due to downwelling
!                                  flux.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Upwelling_Radiance:        Channel TOA radiance simulating the satellite
!                                  sensor measurement. This is composed of the
!                                  reflected downwelling propagated through the
!                                  atmosphere as well as the upwelling only component.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Temperature_TL:            Tangent-linear temperature profile.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature_TL:    Tangent-linear surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity_TL:     Tangent-linear surface boundary emissivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity_TL:   Tangent-linear surface boundary reflectivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Reflectivity_TL:     Tangent-linear surface boundary reflectivity
!                                  for solar term only.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau_TL:                    Tangent-linear layer-to-space transmittance
!                                  profile.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau_TL:               Tangent-linear layer-to-surface flux transmittance
!                                  profile.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau_TL:              Tangent-linear total space-to-surface solar
!                                  transmittance.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Secant_Solar_Angle:        Secant of the solar zenith angle corresponding
!                                  to that used in calculating the total solar
!                                  transmittance.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Valid_Solar:               Flag indicating if the solar component should
!                                  be included.
!                                  If = 0, no solar (if sensor channel frequency
!                                          is less than a preset cutoff or if solar
!                                          zenith angle is greater than its preset
!                                           cutoff.)
!                                     = 1, include solar
!                                  UNITS:      N/A.
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:             Channel index id. This is a unique index
!                                  to a (supported) sensor channel.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Layer_Radiance_TL:         Tangent-linear channel Planck radiance for
!                                  every layer.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K.
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Downwelling_Radiance_TL:   Tangent-linear channel radiance at surface
!                                  due to downwelling flux.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Upwelling_Radiance_TL:     Tangent-linear channel TOA radiance.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
!       Brightness_Temperature_TL: Tangent-linear channel TOA brightness
!                                  temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Sensor_Planck_Radiance_TL:    Function to compute the tangent-linear
!                                     Planck radiance.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
!
!       Sensor_Planck_Temperature_TL: Function to compute the tangent-linear
!                                     Planck temperature.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       The downwelling radiance is first initialised to the space emission
!       boundary term using precalculated cosmic background radiances,
!
!         R_down_TL = CBR * Flux_Tau_TL(1)
!
!       and the emissivity of space is implicitly assumed to be 1.0 and
!       Flux_Tau_TL(1) is the tangent-linear form of the space-to-ground
!       transmittance.
!
!       The contributions of all the layers EXCEPT THE SURFACE LAYER to the
!       downwelling flux is accumulated,
!
!                                  __K-1
!                                 \
!         R_down_TL = R_down_TL +  >  ( B(k) * dFlux_Tau_TL(k) ) + ( B_TL(k) * dFlux_Tau(k) )
!                                 /__
!                                    k=1
!
!       The surface layer contribution is then added explicitly,
!
!         R_down_TL = R_down_TL + ( B(K) * ( -Flux_Tau_TL(K) ) ) + ( B_TL(K) * ( 1 - Flux_Tau(K) ) )
!
!       to avoid exceeding the arrays bounds of Flux_Tau and Flux_Tau_TL
!       (i.e. Flux_Tau(K+1) == 1.0 ) or requiring them to be
!       dimensioned 0:K.
!
!       The solar term is then computed if required,
!
!         R_solar_TL = Solar_Tau_TL * I_solar
!                      ----------------------
!                         SEC(theta_solar)
!
!       or set to zero if not.
!
!       The downwelling radiance is then reflected off the surface, added
!       to the solar and surface emission term, propagated upwards through
!       the atmosphere and used to initialise the upwelling radiance term,
!
!         R_up_TL = ( ( e_sfc      * B_sfc_TL   ) + &
!                     ( e_sfc_TL   * B_sfc      ) + &
!                     ( r_sfc      * R_down_TL  ) + &
!                     ( r_sfc_TL   * R_down     ) + &
!                     ( r_solar    * R_solar_TL ) + &
!                     ( r_solar_TL * R_solar    ) ) * Tau(K) ) + &
!                   ( ( ( e_sfc * B_sfc ) + ( r_sfc * R_down ) + ( r_solar * R_solar ) ) * Tau_TL(K) )
!
!       The contributions of all the layers EXCEPT THE TOP LAYER to the
!       upwelling radiance is accumulated,
!
!                              __ 2
!                             \
!         R_up_TL = R_up_TL +  >  ( B(k) * dTau_TL(k) ) + ( B_TL(k) * dTau(k) )
!                             /__
!                                k=K
!
!       The top layer contribution is then added explicitly,
!
!         R_up_TL = R_up_TL + ( B(1) * ( -Tau_TL(1) ) ) + ( B_TL(1) * ( 1 - Tau(1) ) )
!
!       to avoid exceeding the arrays bounds of Tau (i.e. Tau(0) == 1.0 )
!       or Tau_TL or requiring them to be dimensioned 0:K.
!
!       The final tangent-linear upwelling radiance is then converted to a
!       tangent-linear  brightness temperature.
!
!S-      
!--------------------------------------------------------------------------------


  SUBROUTINE Compute_Radiance_TL( &
                                  ! -- Forward inputs
                                  Temperature,              &  ! Input, K
                                  Surface_Temperature,      &  ! Input, scalar
                                  Surface_Emissivity,       &  ! Input, scalar
                                  Surface_Reflectivity,     &  ! Input, scalar
                                  Solar_Reflectivity,       &  ! Input, scalar
                                  Tau,                      &  ! Input, K
                                  Flux_Tau,                 &  ! Input, K
                                  Solar_Tau,                &  ! Input, scalar
                                  Layer_Radiance,           &  ! Input, K
                                  Downwelling_Radiance,     &  ! Input, scalar
                                  Upwelling_Radiance,       &  ! Input, scalar

                                  ! -- Tangent-linear inputs
                                  Temperature_TL,           &  ! Input, K
                                  Surface_Temperature_TL,   &  ! Input, scalar
                                  Surface_Emissivity_TL,    &  ! Input, scalar
                                  Surface_Reflectivity_TL,  &  ! Input, scalar
                                  Solar_Reflectivity_TL,    &  ! Input, scalar
                                  Tau_TL,                   &  ! Input, K
                                  Flux_Tau_TL,              &  ! Input, K
                                  Solar_Tau_TL,             &  ! Input, scalar

                                  ! -- Other inputs
                                  Secant_Solar_Angle,       &  ! Input, scalar
                                  Valid_Solar,              &  ! Input, scalar
                                  Channel_Index,            &  ! Input, scalar

                                  ! -- Tangent-linear outputs
                                  Layer_Radiance_TL,        &  ! Output, K
                                  Downwelling_Radiance_TL,  &  ! Output, scalar
                                  Upwelling_Radiance_TL,    &  ! Output, scalar
                                  Brightness_Temperature_TL )  ! Output, scalar



    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Temperature
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Emissivity
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Reflectivity
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Reflectivity
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Tau
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Flux_Tau
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Tau
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Layer_Radiance
    REAL( fp_kind ),                 INTENT( IN )  :: Downwelling_Radiance
    REAL( fp_kind ),                 INTENT( IN )  :: Upwelling_Radiance

    ! -- Tangent-linear input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Temperature_TL
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Temperature_TL
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Emissivity_TL
    REAL( fp_kind ),                 INTENT( IN )  :: Surface_Reflectivity_TL
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Reflectivity_TL
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Tau_TL
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )  :: Flux_Tau_TL
    REAL( fp_kind ),                 INTENT( IN )  :: Solar_Tau_TL

    ! -- Other input
    REAL( fp_kind ),                 INTENT( IN )  :: Secant_Solar_Angle
    INTEGER,                         INTENT( IN )  :: Valid_Solar
    INTEGER,                         INTENT( IN )  :: Channel_Index

    ! -- Tangent-linear output
    REAL( fp_kind ), DIMENSION( : ), INTENT( OUT ) :: Layer_Radiance_TL
    REAL( fp_kind ),                 INTENT( OUT ) :: Downwelling_Radiance_TL
    REAL( fp_kind ),                 INTENT( OUT ) :: Upwelling_Radiance_TL
    REAL( fp_kind ),                 INTENT( OUT ) :: Brightness_Temperature_TL

 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance_TL'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n_Layers
    INTEGER :: l

    REAL( fp_kind ) :: Solar_Radiance
    REAL( fp_kind ) :: Solar_Radiance_TL

    REAL( fp_kind ) :: Surface_B
    REAL( fp_kind ) :: Surface_B_TL



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    n_Layers = SIZE( Temperature )



    !#--------------------------------------------------------------------------#
    !#       -- CALCULATE THE TANGENT-LINEAR DOWNWELLING THERMAL FLUX --        #
    !#--------------------------------------------------------------------------#

    ! -- Assign the channel index to a short name
    l = Channel_Index


    ! ---------------------------------------------
    ! Initialise the tangent-linear downwelling
    ! radiance to the space emission boundary term
    ! reaching the surface. The cosmic background
    ! radiance is zero for infrared channels and
    ! precalculated for microwave channels. The
    ! emissivity of space is assumed to be 1.0.
    !
    ! Cosmic background data from the
    ! SPECTRAL_COEFFICIENTS module
    ! ---------------------------------------------

    Downwelling_Radiance_TL = SC%Cosmic_Background_Radiance( l ) * Flux_Tau_TL( 1 )


    ! --------------------------------
    ! Loop over layers from TOA->SFC-1
    ! --------------------------------

    Down_Layer_Loop: DO k = 1, n_Layers - 1

      ! -- Calculate the tangent-linear layer Planck radiance
      CALL Sensor_Planck_Radiance_TL( l,                     &  ! Input
                                      Temperature( k ),      &  ! Input
                                      Temperature_TL( k ),   &  ! Input
                                      Layer_Radiance_TL( k ) )  ! Output

      ! -- Accumulate tangent-linear absorption and emission for current layer.
      ! -- LTE assumed.
      Downwelling_Radiance_TL = Downwelling_Radiance_TL + &
                                ( Layer_Radiance(k)    * ( Flux_Tau_TL(k+1) - Flux_Tau_TL(k) ) ) + &
                                ( Layer_Radiance_TL(k) * ( Flux_Tau(k+1)    - Flux_Tau(k)    ) )

    END DO Down_Layer_Loop


    ! ----------------------------------
    ! Flux bottom layer (closest to SFC)
    ! ----------------------------------

    ! -- Lowest layer tangent-linear Planck radiance
    CALL Sensor_Planck_Radiance_TL( l,                            &  ! Input
                                    Temperature( n_Layers ),      &  ! Input
                                    Temperature_TL( n_Layers ),   &  ! Input
                                    Layer_Radiance_TL( n_Layers ) )  ! Output

    ! -- Contribution of lowest layer. Note that at the
    ! -- surface, a transmittance of 1.0 is used.
    Downwelling_Radiance_TL = Downwelling_Radiance_TL + &
                              ( Layer_Radiance(n_Layers)    * (      -Flux_Tau_TL(n_Layers) ) ) + &
                              ( Layer_Radiance_TL(n_Layers) * ( ONE - Flux_Tau(n_Layers)    ) )



    !#--------------------------------------------------------------------------#
    !#               -- CALCULATE THE TANGENT-LINEAR SOLAR TERM --              #
    !#--------------------------------------------------------------------------#

    Solar_Term: IF ( Valid_Solar == 1 ) THEN

      Solar_Radiance = Solar_Tau * SC%Solar_Irradiance(l) / &
      !                ----------------------------------
                               Secant_Solar_Angle

      Solar_Radiance_TL = Solar_Tau_TL * SC%Solar_Irradiance(l) / &
      !                   -------------------------------------
                                    Secant_Solar_Angle
    ELSE

      ! -- Not a solar affected channel. Everything's zero.
      Solar_Radiance    = ZERO
      Solar_Radiance_TL = ZERO

    END IF Solar_Term



    !#--------------------------------------------------------------------------#
    !#      -- REFLECT THE TANGENT-LINEAR DOWNWELLING RADIANCE, ADD THE  --     #
    !#      -- SURFACE EMISSION AND USE IT TO INITIALISE THE TANGENT-    --     #
    !#      -- LINEAR UPWELLING RADIANCE                                 --     #
    !#--------------------------------------------------------------------------#

    ! -- Calculate the surface terms
    CALL Sensor_Planck_Radiance( l,                   &  ! Input
                                 Surface_Temperature, &  ! Input
                                 Surface_B            )  ! Output

    CALL Sensor_Planck_Radiance_TL( l,                      &  ! Input
                                    Surface_Temperature,    &  ! Input
                                    Surface_Temperature_TL, &  ! Input
                                    Surface_B_TL            )  ! Output

    ! -- Initialise the tangent-linear upwelling radiance
    Upwelling_Radiance_TL = ( ( ( Surface_B * Surface_Emissivity_TL ) + &
                                ( Surface_Emissivity * Surface_B_TL ) + &
                                ( Downwelling_Radiance * Surface_Reflectivity_TL ) + &
                                ( Surface_Reflectivity * Downwelling_Radiance_TL ) + &
                                ( Solar_Radiance     * Solar_Reflectivity_TL ) + &
                                ( Solar_Reflectivity * Solar_Radiance_TL     )   ) * Tau(n_Layers) ) + &

                            ( ( ( Surface_Emissivity * Surface_B              ) + &
                                ( Surface_Reflectivity * Downwelling_Radiance ) + &
                                ( Solar_Reflectivity * Solar_Radiance         )   ) * Tau_TL(n_Layers) )



    !#--------------------------------------------------------------------------#
    !#           -- CALCULATE THE TANGENT-LINEAR UPWELLING RADIANCE --          #
    !#--------------------------------------------------------------------------#

    ! --------------------------------
    ! Loop over layers from SFC->TOA-1
    ! --------------------------------

    Up_Layer_Loop: DO k = n_Layers, 2, -1

      Upwelling_Radiance_TL = Upwelling_Radiance_TL + &
                              ( Layer_Radiance(k)    * ( Tau_TL(k-1) - Tau_TL(k) ) ) + &
                              ( Layer_Radiance_TL(k) * ( Tau(k-1)    - Tau(k)    ) )

    END DO Up_Layer_Loop


    ! --------------------------
    ! Top layer (closest to TOA)
    ! --------------------------

    Upwelling_Radiance_TL = Upwelling_Radiance_TL + &
                            ( Layer_Radiance(1)    * (      -Tau_TL(1) ) ) + &
                            ( Layer_Radiance_TL(1) * ( ONE - Tau(1)    ) )



    !#--------------------------------------------------------------------------#
    !#    -- CONVERT THE TANGENT-LINEAR RADIANCE TO BRIGHTNESS TEMPERATURE --   #
    !#--------------------------------------------------------------------------#

    CALL Sensor_Planck_Temperature_TL( l,                        &  ! Input
                                       Upwelling_Radiance,       &  ! Input
                                       Upwelling_Radiance_TL,    &  ! Input
                                       Brightness_Temperature_TL )  ! Output

  END SUBROUTINE Compute_Radiance_TL






!--------------------------------------------------------------------------------
!S+
! NAME:
!       Compute_Radiance_AD
!
! PURPOSE:
!       Subroutine to calculate the adjoint of the TOA radiance and
!       brightness temperature.
!
! CATEGORY:
!       pCRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Compute_Radiance_AD( &
!                                 ! -- Forward inputs
!                                 Temperature,               &  ! Input, K
!                                 Surface_Temperature,       &  ! Input, scalar
!                                 Surface_Emissivity,        &  ! Input, scalar
!                                 Surface_Reflectivity,      &  ! Input, scalar
!                                 Solar_Reflectivity,        &  ! Input, scalar
!                                 Tau,                       &  ! Input, K
!                                 Flux_Tau,                  &  ! Input, K
!                                 Solar_Tau,                 &  ! Input, scalar
!                                 Layer_Radiance,            &  ! Input, K
!                                 Downwelling_Radiance,      &  ! Input, scalar
!                                 Upwelling_Radiance,        &  ! Input, scalar
!
!                                 ! -- Adjoint inputs
!                                 Layer_Radiance_AD,         &  ! In/Output, K
!                                 Downwelling_Radiance_AD,   &  ! In/Output, scalar
!                                 Upwelling_Radiance_AD,     &  ! In/Output, scalar
!                                 Brightness_Temperature_AD, &  ! In/Output, scalar
!
!                                 ! -- Other inputs
!                                 Secant_Solar_Angle,        &  ! Input, scalar
!                                 Valid_Solar,               &  ! Input, scalar
!                                 Channel_Index,             &  ! Input, scalar
!
!                                 ! -- Adjoint outputs
!                                 Temperature_AD,            &  ! In/Output, K
!                                 Surface_Temperature_AD,    &  ! In/Output, scalar
!                                 Surface_Emissivity_AD,     &  ! In/Output, scalar
!                                 Surface_Reflectivity_AD,   &  ! In/Output, scalar
!                                 Solar_Reflectivity_AD,     &  ! In/Output, scalar
!                                 Tau_AD,                    &  ! In/Output, K
!                                 Flux_Tau_AD,               &  ! In/Output, K
!                                 Solar_Tau_AD               )  ! In/Output, scalar
!
!
! INPUT ARGUMENTS:
!       Temperature:               Profile LAYER average temperature array.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Temperature:       Surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Emissivity:        Surface boundary emissivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Surface_Reflectivity:      Surface boundary reflectivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Reflectivity:        Surface boundary reflectivity for solar
!                                  term only.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Tau:                       Layer-to-space transmittance profile for
!                                  a particular satellite view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       Flux_Tau:                  Layer-to-surface transmittance profile for
!                                  either the diffuse approximation angle (IR)
!                                  or the satellite view angle (MW). The latter
!                                  assumes specular reflectivity.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       Solar_Tau:                 Total space-to-surface transmittance at the
!                                  solar zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_Radiance:            Channel Planck radiance for every layer.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN )
!
!       Downwelling_Radiance:      Channel radiance at surface due to downwelling
!                                  flux.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Upwelling_Radiance:        Channel TOA radiance simulating the satellite
!                                  sensor measurement. This is composed of the
!                                  reflected downwelling propagated through the
!                                  atmosphere as well as the upwelling only component.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Layer_Radiance_AD:         Adjoint of the channel Planck radiance for every
!                                  layer.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Downwelling_Radiance_AD:   Adjoint of the channel radiance at surface due
!                                  to downwelling flux.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Upwelling_Radiance_AD:     Adjoint of the channel TOA radiance simulating
!                                  the satellite sensor measurement.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      mW/(m^2.sr.cm^-1)
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Brightness_Temperature_AD: Adjoint of the channel TOA brightness temperature.
!                                  ** THIS ARGUMENT IS SET TO ZERO ON OUTPUT **.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Secant_Solar_Angle:        Secant of the solar zenith angle corresponding
!                                  to that used in calculating the total solar
!                                  transmittance.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Valid_Solar:               Flag indicating if the solar component should
!                                  be included.
!                                  If = 0, no solar (if sensor channel frequency
!                                          is less than a preset cutoff or if solar
!                                          zenith angle is greater than its preset
!                                           cutoff.)
!                                     = 1, include solar
!                                  UNITS:      N/A.
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!       Channel_Index:             Channel index id. This is a unique index
!                                  to a (supported) sensor channel.
!                                  UNITS:      N/A
!                                  TYPE:       INTEGER
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN )
!
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       Temperature_AD:            Adjoint of the profile LAYER average temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Temperature_AD:    Adjoint of the surface boundary temperature.
!                                  UNITS:      Kelvin
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Emissivity_AD:     Adjoint of the surface boundary emissivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Surface_Reflectivity_AD:   Adjoint of the surface boundary reflectivity
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Solar_Reflectivity_AD:     Adjoint of the surface boundary reflectivity
!                                  for solar term only.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Tau_AD:                    Adjoint of the layer-to-space transmittance
!                                  profile for a particular satellite view angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Flux_Tau_AD:               Adjoint of the layer-to-surface transmittance
!                                  profile for either the diffuse approximation
!                                  angle (IR) or the satellite view angle (MW).
!                                  The latter assumes specular reflectivity.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  K
!                                  ATTRIBUTES: INTENT( IN OUT )
!
!       Solar_Tau_AD:              Adjoint of the total space-to-surface 
!                                  transmittance at the solar zenith angle.
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!                                  ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Sensor_Planck_Radiance_AD:    Function to compute the adjoint
!                                     Planck radiance.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
!
!       Sensor_Planck_Temperature_AD: Function to compute the adjoint
!                                     Planck temperature.
!                                     SOURCE: SENSOR_PLANCK_ROUTINES module
!
! SIDE EFFECTS:
!       All *input* adjoint arguments are set to zero on output.
!
! RESTRICTIONS:
!       None.
!
! PROCEDURE:
!       This adjoint code is derived directly from the transpose of the
!       tangent-linear model. Starting with the final statement of the
!       tangent-linear model,
!
!         R_up_TL = R_up_TL + ( B(1) * ( -Tau_TL(1) ) ) + ( B_TL(1) * ( 1 - Tau(1) ) )
!
!       the adjoint of the components is given by,
!
!         Tau_AD(1) = -B(1) * R_up_AD
!         B_AD(1)   = ( 1 - Tau(1) ) * R_up_AD
!
!       R_up_AD is not set to zero at this point as it is being summed. The 
!       surface to space tangent-linear summation is given by,
!
!                              __ 2
!                             \
!         R_up_TL = R_up_TL +  >  ( B(k) * dTau_TL(k) ) + ( B_TL(k) * dTau(k) )
!                             /__
!                                k=K
!
!       The adjoint of the components are,
!
!         B_AD(k)     = dTau(k) * R_up_AD
!         Tau_AD(k)   = Tau_AD(k)   - ( B(k) * R_up_AD )
!         Tau_AD(k-1) = Tau_AD(k-1) + ( B(k) * R_up_AD )
!         T_AD(k)     = planck_AD(T(k),B_AD(k))
!         B_AD(k)     = 0.0
!
!       Next comes the tangent-linea surface term,
!
!         R_up_TL = ( e_sfc      * B_sfc_TL  * Tau(K) ) + &
!                   ( e_sfc_TL   * B_sfc     * Tau(K) ) + &
!                   ( r_sfc      * R_down_TL * Tau(K) ) + &
!                   ( r_sfc_TL   * R_down    * Tau(K) ) + &
!                   ( r_solar    * R_solar_TL * Tau(K) ) + &
!                   ( r_solar_TL * R_solar    * Tau(K) ) + &
!                   ( ( ( e_sfc * B_sfc ) + ( r_sfc * R_down ) + ( _solar * R_solar ) ) * Tau_TL(K) )
!
!       with the adjoints,
!
!         Tau_AD(K)  = Tau_AD(K) + (( e_sfc * B_sfc ) + ( r_sfc * R_down ) + ( _solar * R_solar ))*R_up_AD
!         R_solar_AD = r_solar * Tau(K) * R_up_AD
!         r_solar_AD = r_solar_AD + ( R_solar * Tau(K) * R_up_AD )
!         R_down_AD  = r_sfc * Tau(K) * R_up_AD
!         r_sfc_AD   = r_sfc_AD + ( R_down * Tau(K) * R_up_AD )
!         B_AD(sfc)  = e_sfc * Tau(K) * R_up_AD
!         e_sfc_AD   = e_sfc_AD + ( B(sfc) * Tau(K) * R_up_AD )
!         R_up_AD    = 0.0
!
!       The tangent-linear solar term, if used, is
!
!         R_solar_TL = R_solar_TL + ( solar_irradiance * Solar_Tau_TL * COS(solar_theta) )
!
!       with its adjoint being,
!
!         Solar_Tau_AD = Solar_Tau_AD + ( solar_irradiance * COS(solar_theta) * R_solar_AD )
!
!       The tangent-linear surface layer contribution to the downwelling flux
!       is calculated separately,
!
!         R_down_TL = R_down_TL + ( B(K) * ( -Flux_Tau_TL(K) ) ) + ( B_TL(K) * ( 1 - Flux_Tau(K) ) )
!         
!       and its component adjoints are,
!
!         B_AD(K)        = B_AD(K) + ( 1 - Flux_Tau(K) ) * R_down_AD
!         Flux_Tau_AD(K) = Flux_Tau_AD(K) - ( B(K) * R_down_AD )
!         T_AD(K)        = planck_AD(T(K),B_AD(K))
!         B_AD(K)        = 0.0
!
!       As with the upwelling tangent-linear summation, the downwelling is
!       given by,
!
!                                  __K-1
!                                 \
!         R_down_TL = R_down_TL +  >  ( B(k) * dFlux_Tau_TL(k) ) + ( B_TL(k) * dFlux_Tau(k) )
!                                 /__
!                                    k=1
!
!       The adjoint of the components are,
!
!         B_AD(k)          = B_AD(k ) + ( dFlux_Tau(k) * R_down_AD )
!         Flux_Tau_AD(k)   = Flux_Tau_AD(k)   - ( B(k) * R_down_AD )
!         Flux_Tau_AD(k-1) = Flux_Tau_AD(k-1) + ( B(k) * R_down_AD )
!         T_AD(k)          = planck_AD(T(k),B_AD(k))
!         B_AD(k)          = 0.0
!
!       The final step is to determine the adjoints of the cosmic background
!       tangent-linear term,
!
!         R_down_TL = CBR * Flux_Tau_TL(1)
!
!       which is
!
!         Flux_Tau_AD(1) = Flux_Tau_AD(1) + ( CBR * R_down_AD )
!         R_down_AD      = 0.0
!
!S-      
!--------------------------------------------------------------------------------


  SUBROUTINE Compute_Radiance_AD( &
                                  ! -- Forward inputs
                                  Temperature,               &  ! Input, K
                                  Surface_Temperature,       &  ! Input, scalar
                                  Surface_Emissivity,        &  ! Input, scalar
                                  Surface_Reflectivity,      &  ! Input, scalar
                                  Solar_Reflectivity,        &  ! Input, scalar
                                  Tau,                       &  ! Input, K
                                  Flux_Tau,                  &  ! Input, K
                                  Solar_Tau,                 &  ! Input, scalar
                                  Layer_Radiance,            &  ! Input, K
                                  Downwelling_Radiance,      &  ! Input, scalar
                                  Upwelling_Radiance,        &  ! Input, scalar

                                  ! -- Adjoint inputs
                                  Layer_Radiance_AD,         &  ! In/Output, K
                                  Downwelling_Radiance_AD,   &  ! In/Output, scalar
                                  Upwelling_Radiance_AD,     &  ! In/Output, scalar
                                  Brightness_Temperature_AD, &  ! In/Output, scalar

                                  ! -- Other inputs
                                  Secant_Solar_Angle,        &  ! Input, scalar
                                  Valid_Solar,               &  ! Input, scalar
                                  Channel_Index,             &  ! Input, scalar

                                  ! -- Adjoint outputs
                                  Temperature_AD,            &  ! In/Output, K
                                  Surface_Temperature_AD,    &  ! In/Output, scalar
                                  Surface_Emissivity_AD,     &  ! In/Output, scalar
                                  Surface_Reflectivity_AD,   &  ! In/Output, scalar
                                  Solar_Reflectivity_AD,     &  ! In/Output, scalar
                                  Tau_AD,                    &  ! In/Output, K
                                  Flux_Tau_AD,               &  ! In/Output, K
                                  Solar_Tau_AD               )  ! In/Output, scalar




    !#--------------------------------------------------------------------------#
    !#                         -- Type declarations --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Forward input
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Temperature
    REAL( fp_kind ),                 INTENT( IN )     :: Surface_Temperature
    REAL( fp_kind ),                 INTENT( IN )     :: Surface_Emissivity
    REAL( fp_kind ),                 INTENT( IN )     :: Surface_Reflectivity
    REAL( fp_kind ),                 INTENT( IN )     :: Solar_Reflectivity
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Tau
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Flux_Tau
    REAL( fp_kind ),                 INTENT( IN )     :: Solar_Tau
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN )     :: Layer_Radiance
    REAL( fp_kind ),                 INTENT( IN )     :: Downwelling_Radiance
    REAL( fp_kind ),                 INTENT( IN )     :: Upwelling_Radiance

    ! -- Adjoint inputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Layer_Radiance_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Downwelling_Radiance_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Upwelling_Radiance_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Brightness_Temperature_AD

    ! -- Other input
    REAL( fp_kind ),                 INTENT( IN )     :: Secant_Solar_Angle
    INTEGER,                         INTENT( IN )     :: Valid_Solar
    INTEGER,                         INTENT( IN )     :: Channel_Index

    ! -- Adjoint outputs
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Temperature_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Surface_Temperature_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Surface_Emissivity_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Surface_Reflectivity_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Solar_Reflectivity_AD
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Tau_AD
    REAL( fp_kind ), DIMENSION( : ), INTENT( IN OUT ) :: Flux_Tau_AD
    REAL( fp_kind ),                 INTENT( IN OUT ) :: Solar_Tau_AD


 

    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Compute_Radiance_AD'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: k, n_Layers
    INTEGER :: l

    REAL( fp_kind ) :: Solar_Radiance
    REAL( fp_kind ) :: Solar_Radiance_AD

    REAL( fp_kind ) :: Surface_B
    REAL( fp_kind ) :: Surface_B_AD

    REAL( fp_kind ) :: B_ur_AD
    REAL( fp_kind ) :: B_dr_AD



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE ARRAY DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    n_Layers = SIZE( Temperature )



    !#--------------------------------------------------------------------------#
    !#         -- CALCULATE THE ADJOINT OF THE BRIGHTNESS TEMPERATURE --        #
    !#--------------------------------------------------------------------------#

    ! -- Assign the channel index to a short name
    l = Channel_Index

    ! -- upwelling radiance adjoint
    CALL Sensor_Planck_Temperature_AD( l,                         &  ! Input
                                       Upwelling_Radiance,        &  ! Input
                                       Brightness_Temperature_AD, &  ! Input
                                       Upwelling_Radiance_AD      )  ! In/Output
    Brightness_Temperature_AD = ZERO



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE ADJOINTS OF THE UPWELLING RADIANCE TERM --         #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Top layer (closest to TOA)
    ! --------------------------

    ! -- Adjoint of top layer radiance
    Layer_Radiance_AD( 1 ) = Layer_Radiance_AD( 1 ) + ( ( ONE - Tau( 1 ) ) * Upwelling_Radiance_AD )

    ! -- Adjoint of top layer dTau
    Tau_AD( 1 ) = Tau_AD( 1 ) + ( -Layer_Radiance( 1 ) * Upwelling_Radiance_AD )
    ! NOTE: No Upwelling_Radiance_AD = 0 here since
    !       Upwelling_Radiance_tl = Upwelling_Radiance_tl + (...)

    ! -- Adjoint of top layer temperature
    CALL Sensor_Planck_Radiance_AD( l,                    &  ! Input
                                    Temperature(1),       &  ! Input
                                    Layer_Radiance_AD(1), &  ! Input
                                    Temperature_AD(1)     )  ! In/Output
    Layer_Radiance_AD( 1 ) = ZERO


    ! --------------------------------
    ! Loop over layers from TOA-1->SFC
    ! --------------------------------

    Down_Layer_Loop: DO k = 2, n_Layers

      ! -- Adjoint of layer radiance
      Layer_Radiance_AD( k ) = Layer_Radiance_AD( k ) + &
                               ( ( Tau( k-1 ) - Tau( k ) ) * Upwelling_Radiance_AD )

      ! -- Adjoint of dTau
      B_ur_AD = Layer_Radiance( k ) * Upwelling_Radiance_AD
      Tau_AD( k )   = Tau_AD(k)     - B_ur_AD
      Tau_AD( k-1 ) = Tau_AD( k-1 ) + B_ur_AD
      ! NOTE: No Upwelling_Radiance_AD = 0 here since
      !       Upwelling_Radiance_tl = Upwelling_Radiance_tl + (...)

      ! -- Adjoint of layer temperature
      CALL Sensor_Planck_Radiance_AD( l,                      &  ! Input
                                      Temperature( k ),       &  ! Input
                                      Layer_Radiance_AD( k ), &  ! Input
                                      Temperature_AD( k )     )  ! In/Output
      Layer_Radiance_AD( k ) = ZERO
 
    END DO Down_Layer_Loop



    !#--------------------------------------------------------------------------#
    !#      -- CALCULATE THE ADJOINTS OF THE SOLAR TRANSMITTANCE TERM --        #
    !#                                                                          #
    !# These are all grouped together here. If one strictly "reversed" the      #
    !# tangent-linear code, only the adjoint of the solar transmittance would   #
    !# be within this IF block. However, because the solar adjoint terms are    #
    !# independent of the other adjoint terms, they can be all grouped here to  #
    !# minimise computation of terms with no impact.                            #
    !#--------------------------------------------------------------------------#

    Solar_Term: IF ( Valid_Solar == 1 ) THEN

      ! -- Compute the solar radiance forward term
      Solar_Radiance = Solar_Tau * SC%Solar_Irradiance(l) / &
      !                ----------------------------------
                               Secant_Solar_Angle

      ! -- Solar reflectivity adjoint
      Solar_Reflectivity_AD   = Solar_Reflectivity_AD + &
                                ( Tau( n_Layers ) * Solar_Radiance * Upwelling_Radiance_AD )

      ! -- Solar radiance adjoint
      Solar_Radiance_AD = ( Tau( n_Layers ) * Solar_Reflectivity * Upwelling_Radiance_AD )

      ! -- Solar transmittance adjoint
      Solar_Tau_AD = Solar_Tau_AD + &
                     ( ( SC%Solar_Irradiance( l ) / Secant_Solar_Angle ) * &
                       Solar_Radiance_AD )

    ELSE

      Solar_Radiance = ZERO

    END IF Solar_Term



    !#--------------------------------------------------------------------------#
    !#     -- CALCULATE THE ADJOINTS OF THE SURFACE TANGENT-LINEAR TERMS --     #
    !#--------------------------------------------------------------------------#

    ! -- Recalculate surface Planck radiance
    CALL Sensor_Planck_Radiance( l,                   &  ! Input
                                 Surface_Temperature, &  ! Input
                                 Surface_B            )  ! Output


    ! -- Surface property adjoints
    Surface_Reflectivity_AD = Surface_Reflectivity_AD + &
                              ( Tau( n_Layers ) * Downwelling_Radiance * Upwelling_Radiance_AD )
 
    Surface_Emissivity_AD   = Surface_Emissivity_AD + &
                              ( Tau( n_Layers ) * Surface_B * Upwelling_Radiance_AD )
 
    ! -- Total transmittance adjoint
    Tau_AD( n_Layers ) = Tau_AD( n_Layers ) + &
                         ( ( ( Surface_Emissivity   * Surface_B            ) + &
                             ( Surface_Reflectivity * Downwelling_Radiance ) + &
                             ( Solar_Reflectivity   * Solar_Radiance       )   ) * Upwelling_Radiance_AD )
 
    ! -- Downwelling radiance adjoint
    Downwelling_Radiance_AD = Downwelling_Radiance_AD + &
                              ( Tau( n_Layers ) * Surface_Reflectivity * Upwelling_Radiance_AD )

    ! -- Surface emission adjoint. This quantity
    ! -- is initialised each call.
    Surface_B_AD = ( Tau( n_Layers ) * Surface_Emissivity * Upwelling_Radiance_AD )

    ! -- Set upwelling adjoint to zero.
    ! --  No more impact on gradient vector
    Upwelling_Radiance_AD = ZERO

    ! -- Surface temperature adjoint
    CALL Sensor_Planck_Radiance_AD( l,                     & ! Input
                                    Surface_Temperature,   & ! Input
                                    Surface_B_AD,          & ! Input
                                    Surface_Temperature_AD ) ! In/Output
    ! -- No need to zero Surface_B_AD as it is initialised each call.
  

    !#--------------------------------------------------------------------------#
    !#        -- CALCULATE THE ADJOINTS OF THE DOWNWELLING FLUX TERM --         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Bottom layer (closest to SFC)
    ! -----------------------------

    ! -- Adjoint of bottom layer radiance
    Layer_Radiance_AD( n_Layers ) = Layer_Radiance_AD( n_Layers ) + &
                                    ( ( ONE - Flux_Tau( n_Layers ) ) * Downwelling_Radiance_AD )

    ! -- Adjoint of flux transmittance
    Flux_Tau_AD( n_Layers ) = Flux_Tau_AD( n_Layers ) - &
                              ( Layer_Radiance( n_Layers ) * Downwelling_Radiance_AD )
    ! NOTE: No Downwelling_Radiance_AD = 0 here since
    !       Downwelling_Radiance_tl = Downwelling_Radiance_tl + (TL flux term)
 
    ! -- Adjoint of layer temperature
    CALL Sensor_Planck_Radiance_AD( l,                             &  ! Input
                                    Temperature( n_Layers ),       &  ! Input
                                    Layer_Radiance_AD( n_Layers ), &  ! Input
                                    Temperature_AD( n_Layers )     )  ! In/Output
    Layer_Radiance_AD( n_Layers ) = ZERO


    ! --------------------------------
    ! Loop over layers from SFC-1->TOA
    ! --------------------------------

    Up_Layer_Loop: DO k = n_Layers - 1, 1, -1

      ! -- Adjoint of layer radiance
      Layer_Radiance_AD( k ) = Layer_Radiance_AD( k ) + &
                               ( ( Flux_Tau( k+1 ) - Flux_Tau( k ) ) * Downwelling_Radiance_AD )

      ! -- Adjoint of dTau
      B_dr_AD = Layer_Radiance( k ) * Downwelling_Radiance_AD
      Flux_Tau_AD( k )   = Flux_Tau_AD( k )   - B_dr_AD
      Flux_Tau_AD( k+1 ) = Flux_Tau_AD( k+1 ) + B_dr_AD
      ! NOTE: No Downwelling_Radiance_AD = 0 here since
      !       Downwelling_Radiance_tl = Downwelling_Radiance_tl + (...)

      ! -- Adjoint of layer temperature
      CALL Sensor_Planck_Radiance_AD( l,                 &  ! Input
                                 Temperature( k ),       &  ! Input
                                 Layer_Radiance_AD( k ), &  ! Input
                                 Temperature_AD( k )     )  ! In/Output
      Layer_Radiance_AD( k ) = ZERO

    END DO Up_Layer_Loop


    ! --------------------------------------------
    ! Background term. Note that the emissivity of
    ! space is implicitly assumed to 1.0.
    ! --------------------------------------------

    Flux_Tau_AD( 1 ) = Flux_Tau_AD( 1 ) + &
                       ( SC%Cosmic_Background_Radiance( l ) * Downwelling_Radiance_AD )

    Downwelling_Radiance_AD = ZERO

  END SUBROUTINE Compute_Radiance_AD

END MODULE Radiance


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: radiance.f90,v 2.8 2004/12/22 17:43:33 paulv Exp $
!
! $Date: 2004/12/22 17:43:33 $
!
! $Revision: 2.8 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: radiance.f90,v $
! Revision 2.8  2004/12/22 17:43:33  paulv
! - Updated header documentation.
!
! Revision 2.7  2004/02/25 22:48:12  paulv
! - Added the solar reflectivity as an argument. This was done for the FWD,
!   TL, and AD forms (including all the required TL and AD arguments also.)
! - Modified the solar term computation to use the input solar reflectivity.
!   Packaged up the adjoint form so that it does the least amount of solar
!   calculations for non-solar affected channels.
!
! Revision 2.6  2003/05/02 18:14:03  paulv
! - Spectral coefficient data for the cosmic background radiance and solar
!   irradiance is now via the SpcCoeff structure.
!
! Revision 2.5  2001/10/01 20:28:47  paulv
! - Added "Name" to RCS keyword list.
!
! Revision 2.4  2001/08/16 17:11:57  paulv
! - Updated documentation
!
! Revision 2.3  2001/08/08 20:02:12  paulv
! - Removed sensor Planck function routines and placed them in their own
!   module, SENSOR_PLANCK_ROUTINES. Some routines were required for other
!   uses so their PRIVATE subprogram status wasn't amenable to code-sharing.
! - Updated header documentation.
!
! Revision 2.2  2001/08/01 16:58:32  paulv
! - Corrected bug in COMPUTE_RADIANCE() function. The initialisation
!   statement of the downwelling radiance was,
!      downwelling_radiance = cosmic_background_radiance( l )
!   and has been changed to,
!      downwelling_radiance = cosmic_background_radiance( l ) * flux_tau( 1 )
!   i.e. the transmission of the space emission term to the surface. This
!   was a holdover from earlier versions of the functions when the transmittances
!   were calculated and passed as *layer* rather than layer-to-surface
!   transmittances.
! - Removed initialisation and zeroing of the adjoint of the surface emissioni
!   term SURFACE_B_AD. This is used in only one place so there is no need to
!   do,
!     surface_B_AD = ZERO
!     surface_B_AD = surface_B_AD + &
!                    ( tau( n_layers ) * surface_emissivity * upwelling_radiance_AD )
!     ....use surface_B_AD...
!     surface_B_AD = ZERO
!   when,
!     surface_B_AD = ( tau( n_layers ) * surface_emissivity * upwelling_radiance_AD )
!   will do.
! - Updated documentation.
!
! Revision 2.1  2001/05/29 18:05:29  paulv
! - All tangent-linear and adjoint routines included.
! - No more optional arguments of downwelling flux and surface reflectivity -
!   they are expected.
! - Altered the method of calculating the layer contributions. Changed code
!   from:
!     layer_radiance(k) = (1-tau)*B(T) + tau*layer_radiance(k-1)
!   to:
!     layer_radiance(k) = B(T) * dtau
!
! Revision 1.5  2001/01/24 20:14:21  paulv
! - Latest test versions.
!
! Revision 1.4  2000/11/09 20:46:07  paulv
! - Added solar term.
! - Downwelling flux transmittance term is now an optional argument.
!   If not specified, the layer radiances are calculated during the
!   upwelling radiance integration.
! - Surface reflectivity is an optional argument. If not specified the
!   surface emissivity is used to generate an isotropic reflectivity.
!
! Revision 1.3  2000/08/31 19:36:33  paulv
! - Added documentation delimiters.
! - Updated documentation headers.
!
! Revision 1.2  2000/08/24 15:48:34  paulv
! - Replaced "regular" reflectivity for reflected downwelling thermal with
!   the isotropic reflectivity in the COMPUTE_RADIANCE subprogram.
! - Updated module and subprogram documentation.
!
! Revision 1.1  2000/08/21 20:59:34  paulv
! Initial checkin.
!
!
!
!
