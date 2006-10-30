!--------------------------------------------------------------------------------
!M+
! NAME:
!       NESDIS_LandEM_Module
!
! PURPOSE:
!       Module containing the microwave land emissivity model
!
! CATEGORY:
!       Surface : MW Surface Land Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LandEM_Module
!
! MODULES:
!       Type_Kinds:          Module containing definitions for kinds of variable types.
!
! CONTAINS:
!       NESDIS_LandEM             : Subroutine to calculate the microwave land emissivity
!
!       Canopy_Diel        : Subroutine to calculate the dielectric constants of vegetation canopy
!
!       Soil_Diel          : Subroutine to calculate the dielectric properties of soil
!
!       Snow_Diel          : Subroutine to calculate the dielectric properties of snow
!
!       Reflectance        : Subroutine to compute the surface reflectivety using fresnel equations
!
!       Transmittance      : Subroutine to compute the surface transmitance using fresnel equations
!
!       Roughness_Reflectance : Subroutine to compute the surface reflectivety using fresnel equations
!                                      for a rough surface having a standard deviation of height of sigma
!       Canopy_Optic       : Subroutine to compute optic parameters for canopy
!
!       Snow_Optic         : Subroutine to compute optic parameters for snow
!
!       Two_Stream_Solution: Subroutine to simulate microwave emissivity over land conditions using
!                                   two stream approximation RTEs
!
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
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (06-01-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!     Fixed bugs and added quality control: Banghua Yan, Quanhua Liu, Hong Yan (09-10-2005)
!
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!--------------------------------------------------------------------------------

MODULE NESDIS_LandEM_Module





  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------



  PRIVATE

  PUBLIC  :: NESDIS_LandEM

  real(fp_kind),public, parameter:: zero = 0.0_fp_kind
  real(fp_kind),public, parameter:: one_tenth = 0.1_fp_kind
  real(fp_kind),public, parameter:: half = 0.5_fp_kind
  real(fp_kind),public, parameter:: one = 1.0_fp_kind
  real(fp_kind),public, parameter:: two = 2.0_fp_kind
  real(fp_kind),public, parameter:: three = 3.0_fp_kind
  real(fp_kind),public, parameter:: four = 4.0_fp_kind
  real(fp_kind),public, parameter:: pi = 3.14159_fp_kind
  real(fp_kind),public, parameter:: emissh_default = 0.25_fp_kind
  real(fp_kind),public, parameter:: emissv_default = 0.30_fp_kind

CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!-------------------------------------------------------------------------------------------------------------
!
! NAME:
!       NESDIS_LandEM
!
! PURPOSE:
!       Subroutine to simulate microwave emissivity over land conditions.
!
! REFERENCES:
!       Weng, F., B. Yan, and N. Grody, 2001: "A microwave land emissivity model", J. Geophys. Res., 106,
!                                             20, 115-20, 123
!
! CATEGORY:
!       CRTM : Surface : MW LandEM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL NESDIS_LandEM
!
! INPUT ARGUMENTS:
!
!         Frequency                Frequency User defines
!                                  This is the "I" dimension
!                                  UNITS:      GHz
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!
!         Angle:                   The angle values in degree.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      Degrees
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Rank-1, (I)
!         t_skin = Land_Temperature:        The land surface temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         mv = Soil_Moisture_Content:   The volumetric water content of the soil(0~1).
!                                  UNITS:      demensionless
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         mg = Canopy_Water_Content:The gravimetric water content of the canopy (0~1)
!                                  UNITS:      demensionless
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         veg_frac = Vegetation_Fraction:     The vegetation fraction of the surface(0:1).
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         t_soil = Soil_Temperature:        The soil temperature.
!                                  UNITS:      Kelvin, K
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Snow_Depth:              The snow depth.
!                                  UNITS:      mm
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!
! OUTPUT ARGUMENTS:
!
!         Emissivity_H:            The surface emissivity at a horizontal polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!         Emissivity_V:            The surface emissivity at a vertical polarization.
!                                  ** NOTE: THIS IS A MANDATORY MEMBER **
!                                  **       OF THIS STRUCTURE          **
!                                  UNITS:      N/A
!                                  TYPE:       REAL( fp_kind )
!                                  DIMENSION:  Scalar
!
!
! INTERNAL ARGUMENTS:
!       theta       -  local zenith angle in radian
!       rhob        -  bulk volume density of the soil (1.18-1.12)
!       rhos        -  density of the solids (2.65 g.cm^3 for solid soil material)
!       sand        -  sand fraction (sand + clay = 1.0)
!       clay        -  clay fraction
!       lai         -  leaf area index (eg. lai = 4.0 for corn leaves)
!       sigma       -  surface roughness formed between medium 1 and 2,
!                      expressed as the standard deviation of roughtness height (mm)
!       leaf_thick  --  leaf thickness (mm)
!       rad         -  radius of dense medium scatterers (mm)
!       va          -  fraction volume of dense medium scatterers(0.0 - 1.0)
!       ep          -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       Canopy_Diel        : Subroutine to calculate the dielectric constants of vegetation canopy
!
!       Soil_Diel          : Subroutine to calculate the dielectric properties of soil
!
!       Snow_Diel          : Subroutine to calculate the dielectric properties of snow
!
!       Reflectance        : Subroutine to compute the surface reflectivety using fresnel equations
!
!       Transmittance      : Subroutine to compute the surface transmitance using fresnel equations
!
!       Roughness_Reflectance : Subroutine to compute the surface reflectivety using fresnel equations
!                                      for a rough surface having a standard deviation of height of sigma
!       Canopy_Optic       : Subroutine to compute optic parameters for canopy
!
!       Snow_Optic         : Subroutine to compute optic parameters for snow
!
!       Two_Stream_Solution: Subroutine to simulate microwave emissivity over land conditions using
!                                   two stream approximation RTEs
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorData argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Banghua Yan, QSS Group Inc., Banghua.Yan@noaa.gov (16-May-2005)
!
!
!       and             Fuzhong Weng, NOAA/NESDIS/ORA, Fuzhong.Weng@noaa.gov
!
!      Fixed bugs and added quality controls:
!
!                        Banghua Yan, Quanhua Liu and Hong Han  (10-September-2005)
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!
!------------------------------------------------------------------------------------------------------------


  SUBROUTINE NESDIS_LandEM(Angle,                 &   ! Input
                           Frequency,             &   ! Input
                           Soil_Moisture_Content, &   ! Input
                           Vegetation_Fraction,   &   ! Input
                           Soil_Temperature,      &   ! Input
                           t_skin,                &   ! Input
                           Snow_Depth,            &   ! Input
                           Emissivity_H,          &   ! Output
                           Emissivity_V)              ! Output

  USE NESDIS_SnowEM_Parameters
  use type_kinds, only: fp_kind, ip_kind
  implicit none

! Declare passed variables
  real(fp_kind),intent(in) :: Angle
  real(fp_kind),intent(in) :: Frequency
  real(fp_kind),intent(in) :: Soil_Moisture_Content
  real(fp_kind),intent(in) :: Vegetation_Fraction
  real(fp_kind),intent(in) :: Soil_Temperature
  real(fp_kind),intent(in) :: t_skin
  real(fp_kind),intent(in) :: Snow_Depth
  real(fp_kind),intent(out):: Emissivity_V,Emissivity_H

! Declare local parameters

  integer(ip_kind),parameter :: PHYSICAL_MODEL = 1, EMPRIRICAL_METHOD = 2
  real(fp_kind),parameter :: snow_depth_c = 10.0
  real(fp_kind),parameter:: rhob = 1.18_fp_kind
  real(fp_kind),parameter:: rhos = 2.65_fp_kind
  real(fp_kind),parameter:: sand = 0.8_fp_kind
  real(fp_kind),parameter:: clay = 0.2_fp_kind


! Declare local variables

  integer(ip_kind) SNOWEM_APPROACH
  real(fp_kind) mv,veg_frac,theta,theta_i,theta_t,mu,r21_h,r21_v,r23_h,r23_v,  &
                t21_v,t21_h,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v,mge, &
                lai,leaf_thick,rad,sigma,va,ep_real,ep_imag
  real(fp_kind) :: t_soil
  real(fp_kind) :: local_snow_depth

  complex esoil, eveg, esnow, eair

  eair = cmplx(one,-zero)

  theta = Angle*pi/180.0_fp_kind

! Assign local variable
  mv               = Soil_Moisture_Content
  veg_frac         = Vegetation_Fraction
  t_soil           = Soil_Temperature
  local_snow_depth = Snow_Depth

!Quality Control

  if ( (t_soil .le. 100.0_fp_kind .or. t_soil .ge. 350.0_fp_kind) .AND.    &

       (t_skin .ge. 100.0_fp_kind .and. t_skin .le. 350.0_fp_kind) ) t_soil = t_skin


  if (mv > one)  mv = one         !domensional (zero ~ one)

  if (mv < zero) mv = zero        !domensional (zero ~ one)

  if (local_snow_depth .gt.one_tenth) then

     SNOWEM_APPROACH = PHYSICAL_MODEL

     if (local_snow_depth .gt. snow_depth_c)  SNOWEM_APPROACH = EMPRIRICAL_METHOD

     GET_SNOWEM: SELECT CASE (SNOWEM_APPROACH)

     CASE (PHYSICAL_MODEL)

      ep_real = 3.2_fp_kind

      ep_imag = -0.0005_fp_kind

      sigma = one

!     For deep snow, the performance of the model is poor
      if (local_snow_depth > 1000.0_fp_kind) local_snow_depth = 1000.0_fp_kind

      va = 0.4_fp_kind + 0.0004_fp_kind*local_snow_depth

!     the fraction volume of dense medium scatterers must be less than one.
      if( va > one ) va = one
!     the fraction volume of dense medium scatterers must be greater than/equal to zero.
      if( va < zero ) va = zero

      rad = half + 0.005_fp_kind*local_snow_depth

      if( rad > one ) rad = one  ! Limit for snow grain size

      if (t_soil .ge. 280.5_fp_kind) t_soil = 285.0_fp_kind

      call Snow_Diel(Frequency, ep_real, ep_imag, rad, va, esnow)

      call Soil_Diel(Frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)

      theta_i = asin(real(sin(theta)*csqrt(eair)/csqrt(esnow)))

      call Reflectance(esnow, eair, theta_i,  theta, r21_v, r21_h)

      call Transmittance(esnow, eair, theta_i, theta, t21_v, t21_h)

      mu  = cos(theta_i)

      theta_t = asin(real(sin(theta_i)*csqrt(esnow)/csqrt(esoil)))

      call Reflectance(esnow, esoil, theta_i, theta_t, r23_v, r23_h)

      call Roughness_Reflectance(Frequency, sigma, r23_v, r23_h)

      call Snow_Optic(Frequency,rad,local_snow_depth,va,ep_real, ep_imag,gv,gh,&

                      ssalb_v,ssalb_h,tau_v,tau_h)


      call Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &

                               r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,Emissivity_V,Emissivity_H)


    CASE (EMPRIRICAL_METHOD)


      CALL SnowEM_Default(Frequency,t_skin, local_snow_depth,Emissivity_V,Emissivity_H)


    END SELECT GET_SNOWEM


  else

     sigma = half

     if (veg_frac > one) veg_frac = one

     if (veg_frac < zero) veg_frac = zero

     lai = three*veg_frac + half

     mge = half*veg_frac

     leaf_thick = 0.07_fp_kind

     mu  = cos(theta)

     r21_h    = zero

     r21_v    = zero

     t21_h    = one

     t21_v    = one

     call Soil_Diel(Frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)

     theta_t = asin(real(sin(theta)*csqrt(eair)/csqrt(esoil)))

     call Reflectance(eair, esoil, theta, theta_t, r23_v, r23_h)

     call Roughness_Reflectance(Frequency, sigma, r23_v, r23_h)

     call Canopy_Diel(Frequency, mge, eveg)

     call Canopy_Optic(lai,Frequency,theta,eveg,leaf_thick,gv,gh,ssalb_v,ssalb_h,tau_v,tau_h)

     call Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &

                                      r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,Emissivity_V,Emissivity_H)
  endif

  return


END SUBROUTINE NESDIS_LandEM



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################



subroutine SnowEM_Default(frequency,ts, depth,Emissivity_V,Emissivity_H)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .
!   prgmmr:  Banghua Yan and Fuzhong Weng               org: nesdis              date: 2005-12-01
!
! abstract: preliminary estimate of snow emissivity using  surface temperature and snow depth
!
! input argument list:
!
!      ts         -  surface temperature
!      frequency   -  frequency (ghz)
!
! output argument list:
!
!      Emissivity_V         -  snow emissivty at V-POL
!      Emissivity_H         -  snow emissivty at H-POL
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  use type_kinds, only: fp_kind, ip_kind

  USE NESDIS_SnowEM_Parameters

  implicit none

  integer(ip_kind), parameter :: new = 7

  integer(ip_kind), parameter :: NFRESH_SHALLOW_SNOW = 1, NPOWDER_SNOW = 2, NWET_SNOW = 3, NDEEP_SNOW = 4

  real(fp_kind), parameter :: twet = 270.0, tcrust = 235.0, depth_s = 50.0, depth_c = 100.0

  integer(ip_kind) :: ich,basic_snow_type

  real(fp_kind)    :: frequency,ts, depth,Emissivity_V,Emissivity_H

  real(fp_kind), dimension(new) :: ev, eh, freq


  freq(1:new) = FREQUENCY_AMSRE(1:new)

  basic_snow_type = NFRESH_SHALLOW_SNOW

  if (ts .ge. twet .and. depth .le. depth_s) then

     basic_snow_type = NWET_SNOW

  else

     if (depth .le. depth_s) then

         basic_snow_type = NFRESH_SHALLOW_SNOW

     else

        basic_snow_type = NPOWDER_SNOW

     endif

  endif

  if (ts .le. tcrust .and. depth .ge. depth_c) basic_snow_type = NDEEP_SNOW

! INITIALIZATION

  ev(1:new) = GRASS_AFTER_SNOW_EV_AMSRE(1:new)

  eh(1:new) = GRASS_AFTER_SNOW_EH_AMSRE(1:new)

  GET_SNOWTYPE: SELECT CASE (basic_snow_type)

     CASE (NFRESH_SHALLOW_SNOW)

          ev(1:new) = GRASS_AFTER_SNOW_EV_AMSRE(1:new)

          eh(1:new) = GRASS_AFTER_SNOW_EH_AMSRE(1:new)

     CASE (NPOWDER_SNOW)

          ev(1:new) = POWDER_SNOW_EV_AMSRE(1:new)

          eh(1:new) = POWDER_SNOW_EH_AMSRE(1:new)

     CASE (NWET_SNOW)

         ev(1:new) = WET_SNOW_EV_AMSRE(1:new)

         eh(1:new) = WET_SNOW_EH_AMSRE(1:new)

     CASE (NDEEP_SNOW)

         ev(1:new) = DEEP_SNOW_EV_AMSRE(1:new)

         eh(1:new) = DEEP_SNOW_EH_AMSRE(1:new)

   END SELECT GET_SNOWTYPE




! Interpolate emissivity at a certain frequency

  do ich=1,new

     if (frequency .le. freq(1)) then

         Emissivity_H = eh(1)

         Emissivity_V = ev(1)

         exit

      endif

     if (frequency .ge. freq(new)) then

         Emissivity_H = eh(new)

         Emissivity_V = ev(new)

         exit

      endif


      if (frequency .le. freq(ich)) then

           Emissivity_H = eh(ich-1) + &

                          (frequency-freq(ich-1))*(eh(ich) - eh(ich-1))/(freq(ich)-freq(ich-1))

           Emissivity_V = ev(ich-1) + &

                          (frequency-freq(ich-1))*(ev(ich) - ev(ich-1))/(freq(ich)-freq(ich-1))

          exit

      endif

  enddo

end subroutine SnowEM_Default


subroutine Canopy_Optic(lai,frequency,theta,esv,d,gv,gh,&
     ssalb_v,ssalb_h,tau_v, tau_h)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    canopy_optic compute optic parameters for canopy
!
!   prgmmr:  Fuzhong Weng and Banghua Yan                org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for canopy
!
! program history log:
!
! input argument list:
!
!      lai         -  leaf area index
!      frequency   -  frequency (ghz)
!      theta       -  incident angle
!      esv         -  leaf dielectric constant
!      d           -  leaf thickness (mm)
!
! output argument list:
!
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization
!      tau_h        -  optical depth at h. polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!------------------------------------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) threshold

  real(fp_kind) frequency,theta,d,lai,ssalb_v,ssalb_h,tau_v,tau_h,gv, gh, mu

  complex  ix,k0,kz0,kz1,rhc,rvc,esv,expval1,factt,factrvc,factrhc

  real(fp_kind) rh,rvert,th,tv


  threshold=0.999_fp_kind

  mu = cos(theta)

  ix  = cmplx(zero,one)

  k0  = cmplx(two*pi*frequency/300.0_fp_kind, zero)   ! 1/mm

  kz0 = k0*mu

  kz1 = k0*sqrt(esv - sin(theta)**2)

  rhc = (kz0 - kz1)/(kz0 + kz1)

  rvc = (esv*kz0 - kz1)/(esv*kz0 + kz1)

  expval1=exp(-two*ix*kz1*d)

  factrvc=one-rvc**2*expval1

  factrhc=one-rhc**2*expval1

  factt=four*kz0*kz1*exp(ix*(kz0-kz1)*d)

  rvert = abs(rvc*(one - expval1)/factrvc)**2

  rh = abs(rhc*(one - expval1)/factrhc)**2

  th = abs(factt/((kz1+kz0)**2*factrhc))**2

  tv = abs(esv*factt/((kz1+esv*kz0)**2*factrvc))**2

  gv = half

  gh = half

  tau_v = half*lai*(two-tv-th)

  tau_h = tau_v

  ssalb_v = min((rvert+rh)/ (two-tv-th),threshold)

  ssalb_h = ssalb_v

  return

end subroutine Canopy_Optic


subroutine Snow_Optic(frequency,a,h,f,ep_real,ep_imag,gv,gh, ssalb_v,ssalb_h,tau_v,tau_h)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    landem      comput optic parameters for snow
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute optic parameters for snow
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      ep_real      -  real part of dielectric constant of particles
!      ep_imag      -  imaginary part of dielectric constant of particles
!      a            -  particle radiu (mm)
!      h            -  snow depth(mm)
!      f            -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ssalb       -  single scattering albedo
!       tau         -  optical depth
!       g           -  asymmetry factor
!
!   important internal variables:
!
!       ks          -  scattering coeffcient (/mm)
!       ka          -  absorption coeffient (/mm)
!       kp          -  eigenvalue of two-stream approximation
!       y           -  = yr+iyi
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) yr,yi,ep_real,ep_imag

  real(fp_kind) frequency,a,h,f,ssalb_v,ssalb_h,tau_v,tau_h,gv,gh,k

  real(fp_kind) ks1,ks2,ks3,ks,kr1,kr2,kr3,kr,ki1,ki2,ki3,ki

  real(fp_kind) fact1,fact2,fact3,fact4,fact5


  k = two*pi/(300._fp_kind/frequency)

  yr  = (ep_real - one)/(ep_real + two)

  yi =  - ep_imag/(ep_real + two)

  fact1 = (one+two*f)**2

  fact2 = one-f*yr

  fact3 = (one-f)**4

  fact4 = f*(k*a)**3

  fact5 = one+two*f*yr

  ks1 = k*sqrt(fact2/fact5)

  ks2 = fact4*fact3/fact1

  ks3 = (yr/fact2)**2

  ks = ks1*ks2*ks3

  kr1 = fact5/fact2

  kr2 = two*ks2

  kr3 = two*yi*yr/(fact2**3)

  kr = k*sqrt(kr1+kr2*kr3)

  ki1 = three*f*yi/fact2**2

  ki2 = kr2

  ki3 = ks3

  ki  = k**2/(two*kr)*(ki1+ki2*ki3)

  gv = half

  gh = half

  ssalb_v = ks / ki

  if(ssalb_v .gt. 0.999_fp_kind) ssalb_v = 0.999_fp_kind

  ssalb_h = ssalb_v

  tau_v = two*ki*h

  tau_h = tau_v

  return

end subroutine Snow_Optic


subroutine Soil_Diel(freq,t_soil,vmc,rhob,rhos,sand,clay,esm)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Soil_Diel   calculate the dielectric properties of soil
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute the dilectric constant of the bare soil
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      frequency    -  frequency (ghz)
!      t_soil       -  soil temperature
!      vmc          -  volumetric moisture content (demensionless)
!      rhob         -  bulk volume density of the soil (1.18-1.12)
!      rhos         -  density of the solids (2.65 g.cm^3 for
!                       solid soil material)
!      sand         -  sand fraction (sand + clay = 1.0)
!      clay         -  clay fraction
!
! output argument list:
!
!      esm          -  dielectric constant for bare soil
!
! important internal variables:
!
!      esof         -  the permittivity of free space
!      eswo         -  static dieletric constant
!      tauw         -  relaxation time of water
!      s            -  salinity
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) esof

  real(fp_kind)    f,tauw,freq,t_soil,vmc,rhob,rhos,sand,clay

  real(fp_kind)    alpha,beta,ess,rhoef,t,eswi,eswo

  complex esm,esw,es1,es2


  alpha = 0.65_fp_kind

  beta  = 1.09_fp_kind - 0.11_fp_kind*sand + 0.18_fp_kind*clay

  ess = (1.01_fp_kind + 0.44_fp_kind*rhos)**2 - 0.062_fp_kind

  rhoef = -1.645_fp_kind + 1.939_fp_kind*rhob - 0.020213_fp_kind*sand + 0.01594_fp_kind*clay

  t = t_soil - 273.0_fp_kind

  f = freq*1.0e9_fp_kind

! the permittivity at the high frequency limit

  eswi = 5.5_fp_kind

! the permittivity of free space (esof)

  esof = 8.854e-12_fp_kind

! static dieletric constant (eswo)

  eswo = 87.134_fp_kind+(-1.949e-1_fp_kind+(-1.276e-2_fp_kind+2.491e-4_fp_kind*t)*t)*t

  tauw = 1.1109e-10_fp_kind+(-3.824e-12_fp_kind+(6.938e-14_fp_kind-5.096e-16_fp_kind*t)*t)*t

  if (vmc .gt. zero) then

     es1 = cmplx(eswi, - rhoef *(rhos - rhob)/(two*pi*f*esof*rhos*vmc))

  else

     es1 = cmplx(eswi, zero)

  endif

  es2 = cmplx(eswo - eswi,zero)/cmplx(one, f*tauw)

  esw = es1 + es2

  esm = one + (ess**alpha - one)*rhob/rhos + vmc**beta*esw**alpha - vmc

  esm = esm**(one/alpha)

  if(aimag(esm) .ge. zero) esm = cmplx(real(esm), -0.0001_fp_kind)

  return

end subroutine Soil_Diel


subroutine Snow_Diel(frequency,ep_real,ep_imag,rad,frac,ep_eff)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Snow_Diel   compute dielectric constant of snow
!
!   prgmmr: Fuzhong Weng and Banghua Yan                 org: nesdis              date: 2000-11-28
!
! abstract: compute dielectric constant of snow
!
!
! program history log:
!
! input argument list:
!
!       frequency   -  frequency (ghz)
!       ep_real     -  real part of dielectric constant of particle
!       ep_imag     -  imaginary part of dielectric constant of particle
!       rad         -  particle radiu (mm)
!       frac        -  fraction volume of snow (0.0 - 1.0)
!
! output argument list:
!
!       ep_eff      -  dielectric constant of the dense medium
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!  Copyright (C) 2005 Fuzhong Weng and Banghua Yan
!
!  This program is free software; you can redistribute it and/or modify it under the terms of the GNU
!  General Public License as published by the Free Software Foundation; either version 2 of the License,
!  or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
!  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
!  License for more details.
!
!  You should have received a copy of the GNU General Public License along with this program; if not, write
!  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) ep_imag,ep_real

  real(fp_kind) frequency,rad,frac,k0,yr,yi

  complex  y,ep_r,ep_i,ep_eff,fracy

  k0 = two*pi/(300.0_fp_kind/frequency)

  yr  = (ep_real - one)/(ep_real + two)

  yi =   ep_imag/(ep_real + two)

  y = cmplx(yr, yi)

  fracy=frac*y

  ep_r = (one + two*fracy)/(one - fracy)

  ep_i = two*fracy*y*(k0*rad)**3*(one-frac)**4/((one-fracy)**2*(one+two*frac)**2)

  ep_eff = ep_r - cmplx(zero,one)*ep_i

  if (aimag(ep_eff).ge.zero) ep_eff = cmplx(real(ep_eff), -0.0001_fp_kind)

  return

end subroutine Snow_Diel


subroutine Canopy_Diel(frequency,mg,esv)

!----------------------------------------------------------------------------------
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:   canopy_diel compute the dielectric constant of the vegetation canopy
!
!   prgmmr:  Fuzhong Weng and Banghua Yan                org: nesdis              date: 2000-11-28
!
! abstract: compute the dielectric constant of the vegetation canopy geomatrical optics approximation
!
!           for vegetation canopy work for horizontal leaves
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!      mg           -  gravimetric water content
!
! output argument list:
!
!      esv          -  dielectric constant of leaves
!
! remarks:
!
! references:
!
!     ulaby and el-rayer, 1987: microwave dielectric spectrum of vegetation part ii,
!           dual-dispersion model, ieee trans geosci. remote sensing, 25, 550-557
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind)  frequency,  mg, en, vf, vb

  complex  esv, xx

  en = 1.7_fp_kind - (0.74_fp_kind - 6.16_fp_kind*mg)*mg

  vf = mg*(0.55_fp_kind*mg - 0.076_fp_kind)

  vb = 4.64_fp_kind*mg*mg/( one + 7.36_fp_kind*mg*mg)

  xx = cmplx(zero,one)

  esv = en + vf*(4.9_fp_kind + 75.0_fp_kind/(one + xx*frequency/18.0_fp_kind)-xx*(18.0_fp_kind/frequency)) + &

       vb*(2.9_fp_kind + 55.0_fp_kind/(one + sqrt(xx*frequency/0.18_fp_kind)))

  if (aimag(esv).ge.zero) esv = cmplx(real(esv), -0.0001_fp_kind)

  return

end subroutine Canopy_Diel


subroutine Reflectance(em1, em2, theta_i, theta_t, rvert, rh)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Reflectance compute the surface reflectivity
!
!   prgmmr:                  org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety using fresnel equations
!    for a rough surface having a standard deviation of height of sigma
!
! program history log:
!
! input argument list:
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      rvert        -  reflectivity at vertical polarization
!      rh           -  reflectivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) theta_i, theta_t

  real(fp_kind) rh, rvert,cos_i,cos_t

  complex em1, em2, m1, m2, angle_i, angle_t

! compute the refractive index ratio between medium 2 and 1 using dielectric constant (n = sqrt(e))

  cos_i=cos(theta_i)

  cos_t=cos(theta_t)

  angle_i = cmplx(cos_i, zero)

  angle_t = cmplx(cos_t, zero)

  m1 = csqrt(em1)

  m2 = csqrt(em2)

  rvert = (cabs((m1*angle_t-m2*angle_i)/(m1*angle_t+m2*angle_i)))**2

  rh = (cabs((m1*angle_i-m2*angle_t)/(m1*angle_i+m2*angle_t)))**2

  return

end subroutine Reflectance

subroutine Transmittance(em1,em2,theta_i,theta_t,tv,th)

!----------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    Transmittance    calculate Transmittance
!
!   prgmmr:  Banghua Yan and Fuzhong Weng               org: nesdis              date: 2000-11-28
!
! abstract: compute Transmittance
!
! program history log:
!
! input argument list:
!
!      theta        -  local zenith angle (degree)
!      theta_i      -  incident angle (degree)
!      theta_t      -  transmitted angle (degree)
!      em1          -  dielectric constant of the medium 1
!      em2          -  dielectric constant of the medium 2
!
! output argument list:
!
!      tv           -  transmisivity at vertical polarization
!      th           -  transmisivity at horizontal polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!----------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) theta_i, theta_t

  real(fp_kind) th, tv, rr, cos_i,cos_t

  complex em1, em2, m1, m2, angle_i, angle_t

! compute the refractive index ratio between medium 2 and 1 using dielectric constant (n = sqrt(e))

  cos_i=cos(theta_i)

  cos_t=cos(theta_t)

  angle_i = cmplx(cos_i, zero)

  angle_t = cmplx(cos_t, zero)

  m1 = csqrt(em1)

  m2 = csqrt(em2)

  rr = cabs(m2/m1)*cos_t/cos_i

  tv =  rr*(abs(two*m1*angle_i/(m1*angle_t + m2*angle_i)))**2

  th =  rr*(abs(two*m1*angle_i/(m1*angle_i + m2*angle_t)))**2

  return

end subroutine Transmittance


subroutine Roughness_Reflectance(frequency,sigma,rvert,rh)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    rought_reflectance    calculate surface relectivity
!
!   prgmmr: Banghua Yan and Fuzhong Weng                 org: nesdis              date: 2000-11-28
!
! abstract: compute the surface reflectivety for a rough surface having a standard devoation of height of sigma
!
!
! program history log:
!
! input argument list:
!
!      frequency    -  frequency (ghz)
!
!      theta        -  local zenith angle (degree) (currently, not used here)
!
!      sigma        -  standard deviation of rough surface height
!
!                      smooth surface:0.38, medium: 1.10, rough:2.15 cm
!
!    internal variables
!
!
! output argument list:
!
!      rvert         -  reflectivity at vertical polarization
!
!      rh            -  reflectivity at horizontal polarization
!
!
!   important internal variables:
!
!      k0           -  a propagation constant or wavenumber in a free space
!
! remarks:
!
! references:
!
!   wang, j. and b. j. choudhury, 1992: passive microwave radiation from soil: examples...
!    passive microwave remote sensing of .. ed. b. j. choudhury, etal vsp.
!    also wang and choudhury (1982)
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) frequency

  real(fp_kind) q, rh, rvert, rh_s, rv_s, sigma

  rh_s = 0.3_fp_kind*rh

  rv_s = 0.3_fp_kind*rvert

  q = 0.35_fp_kind*(one - exp(-0.60_fp_kind*frequency*sigma**two))

  rh = rh_s + q*(rv_s-rh_s)

  rvert = rv_s + q*(rh_s-rv_s)

  return

end subroutine Roughness_Reflectance


subroutine Two_Stream_Solution(mu,gv,gh,ssalb_h,ssalb_v,tau_h,tau_v, &

                                      r21_h,r21_v,r23_h,r23_v,t21_v,t21_h,esv,esh)

!-------------------------------------------------------------------------------------------------------------
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:    two_stream_solution
!
!   prgmmr: Banghua Yan and Fuzhong Weng                 org: nesdis              date: 2000-11-28
!
! abstract: two stream solution
!
! REFERENCES:
!       Weng, F., B. Yan, and N. Grody, 2001: "A microwave land emissivity model", J. Geophys. Res., 106,
!                                             20, 115-20, 123
!   version: beta
!
! program history log:
!
! input argument list:
!
!      b            -  scattering layer temperature (k)         (gdas)   (not used here)
!      mu           -  cos(theta)
!      gv           -  asymmetry factor for v pol
!      gh           -  asymmetry factor for h pol
!      ssalb_v      -  single scattering albedo at v. polarization
!      ssalb_h      -  single scattering albedo at h. polarization
!      tau_v        -  optical depth at v. polarization
!      tau_h        -  optical depth at h. polarization
!      r12_v        -  reflectivity at vertical polarization   (not used here)
!      r12_h        -  reflectivity at horizontal polarization (not used here)
!      r21_v        -  reflectivity at vertical polarization
!      r21_h        -  reflectivity at horizontal polarization
!      r23_v        -  reflectivity at vertical polarization
!      r23_h        -  reflectivity at horizontal polarization
!      t21_v        -  transmisivity at vertical polarization
!      t21_h        -  transmisivity at horizontal polarization
!      t12_v        -  transmisivity at vertical polarization   (not used here)
!      t12_h        -  transmisivity at horizontal polarization (not used here)
!
! output argument list:
!
!       esh         -  emissivity for horizontal polarization
!       esv         -  emissivity for vertical polarization
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!-------------------------------------------------------------------------------------------------------------

  use type_kinds, only: fp_kind

  implicit none

  real(fp_kind) mu, gv, gh, ssalb_h, ssalb_v, tau_h,tau_v,                 &

                r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, esv, esh

  real(fp_kind) alfa_v, alfa_h, kk_h, kk_v, gamma_h, gamma_v, beta_v, beta_h

  real(fp_kind) fact1,fact2

  alfa_h = sqrt((one - ssalb_h)/(one - gh*ssalb_h))

  kk_h = sqrt ((one - ssalb_h)*(one -  gh*ssalb_h))/mu

  beta_h = (one - alfa_h)/(one + alfa_h)

  gamma_h = (beta_h -r23_h)/(one-beta_h*r23_h)

  alfa_v = sqrt((one-ssalb_v)/(one - gv*ssalb_v))

  kk_v = sqrt ((one-ssalb_v)*(one - gv*ssalb_v))/mu

  beta_v = (one - alfa_v)/(one + alfa_v)

  gamma_v = (beta_v -r23_v)/(one-beta_v*r23_v)

  fact1=gamma_h*exp(-two*kk_h*tau_h)

  fact2=gamma_v*exp(-two*kk_v*tau_v)

  esh  = t21_h*(one - beta_h)*(one + fact1) /(one-beta_h*r21_h-(beta_h-r21_h)*fact1)

  esv  = t21_v*(one - beta_v)*(one + fact2) /(one-beta_v*r21_v-(beta_v-r21_v)*fact2)

  if (esh .lt. emissh_default) esh = emissh_default

  if (esv .lt. emissv_default) esv = emissv_default

  if (esh .gt. one) esh = one

  if (esv .gt. one) esv = one

  return

end subroutine Two_Stream_Solution



END MODULE NESDIS_LandEM_Module
