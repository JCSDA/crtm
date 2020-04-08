!
! Units_Conversion
!
! Container for the various modules that perform conversion of atmospheric
! absorber concentration units.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 01-May-2000
!                       paul.vandelst@noaa.gov
!

MODULE Units_Conversion

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Indivudual unit conversion modules
  USE MR_PPMV
  USE SA_MR
  USE RH_MR
  USE PPMV_PP
  USE MR_PP
  USE PP_MD
  USE PP_ND
  USE PPMV_ND
  USE PPMV_CD
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: MR_to_PPMV, MR_to_PPMV_TL, MR_to_PPMV_AD
  PUBLIC :: PPMV_to_MR, PPMV_to_MR_TL, PPMV_to_MR_AD

  PUBLIC :: SA_to_MR, SA_to_MR_TL, SA_to_MR_AD
  PUBLIC :: MR_to_SA, MR_to_SA_TL, MR_to_SA_AD

  PUBLIC :: RH_to_MR, RH_to_MR_TL, RH_to_MR_AD
  PUBLIC :: MR_to_RH, MR_to_RH_TL, MR_to_RH_AD

  PUBLIC :: PPMV_to_PP, PPMV_to_PP_TL, PPMV_to_PP_AD
  PUBLIC :: PP_to_PPMV, PP_to_PPMV_TL, PP_to_PPMV_AD

  PUBLIC :: MR_to_PP, MR_to_PP_TL, MR_to_PP_AD
  PUBLIC :: PP_to_MR, PP_to_MR_TL, PP_to_MR_AD

  PUBLIC :: PP_to_MD, PP_to_MD_TL, PP_to_MD_AD
  PUBLIC :: MD_to_PP, MD_to_PP_TL, MD_to_PP_AD

  PUBLIC :: PP_to_ND, PP_to_ND_TL, PP_to_ND_AD
  PUBLIC :: ND_to_PP, ND_to_PP_TL, ND_to_PP_AD

  PUBLIC :: PPMV_to_ND, PPMV_to_ND_TL, PPMV_to_ND_AD
  PUBLIC :: ND_to_PPMV, ND_to_PPMV_TL, ND_to_PPMV_AD

  PUBLIC :: PPMV_to_CD, PPMV_to_CD_TL, PPMV_to_CD_AD
  PUBLIC :: CD_to_PPMV, CD_to_PPMV_TL, CD_to_PPMV_AD


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &

END MODULE Units_Conversion
