!
! Ocean_Permittivity
!
! Container for modules to compute complex permittivities for
! sea water.
!
! Three models are included in this module; that of
!
!   Guillou, C. et al. (1998) Impact of new permittivity measurements
!      on sea surface emissivity modeling in microwaves.
!      Radio Science, Volume 33, Number 3, Pages 649-667
!
! and of
!
!   Ellison, W.J. et al. (2003) A comparison of ocean emissivity models
!     using the Advanced Microwave Sounding Unit, the Special Sensor
!     Microwave Imager, the TRMM Microwave Imager, and airborne radiometer
!     observations. Journal of Geophysical Research, v108, D21, Pages ACL 1,1-14
!     doi:10.1029/2002JD0032132
!
! and of
!
!   Liu, Q. et al. (2010) An improved fast microwave water emissivity model.
!      IEEE Trans. Geosci. Remote Sensing, accepted June 25, 2010
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Apr-2007
!                       paul.vandelst@noaa.gov
!

MODULE Ocean_Permittivity

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Guillou, ONLY: GuillouVar_type => iVar_type , &
                     Guillou_Ocean_Permittivity   , &
                     Guillou_Ocean_Permittivity_TL, &
                     Guillou_Ocean_Permittivity_AD
  USE Ellison, ONLY: EllisonVar_type => iVar_type , &
                     Ellison_Ocean_Permittivity   , &
                     Ellison_Ocean_Permittivity_TL, &
                     Ellison_Ocean_Permittivity_AD
  USE Liu    , ONLY: LiuVar_type => iVar_type , &
                     Liu_Ocean_Permittivity   , &
                     Liu_Ocean_Permittivity_TL, &
                     Liu_Ocean_Permittivity_AD

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! For Guillou model
  PUBLIC :: GuillouVar_type
  PUBLIC :: Guillou_Ocean_Permittivity
  PUBLIC :: Guillou_Ocean_Permittivity_TL
  PUBLIC :: Guillou_Ocean_Permittivity_AD
  ! For Ellison model
  PUBLIC :: EllisonVar_type
  PUBLIC :: Ellison_Ocean_Permittivity
  PUBLIC :: Ellison_Ocean_Permittivity_TL
  PUBLIC :: Ellison_Ocean_Permittivity_AD
  ! For Liu model
  PUBLIC :: LiuVar_type
  PUBLIC :: Liu_Ocean_Permittivity
  PUBLIC :: Liu_Ocean_Permittivity_TL
  PUBLIC :: Liu_Ocean_Permittivity_AD

  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &

END MODULE Ocean_Permittivity
