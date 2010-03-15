MODULE CRTM_SharedData

! Provides CRTM-wide access to shared data without the
! need to USE specific shared data modules.

  USE CRTM_SpcCoeff    , ONLY: SC
  USE CRTM_TauCoeff    , ONLY: TC
  USE CRTM_CloudCoeff  , ONLY: CloudC
  USE CRTM_AerosolCoeff, ONLY: AeroC
  USE CRTM_EmisCoeff   , ONLY: EmisC
  USE CRTM_BeCoeff     , ONLY: BeC

END MODULE CRTM_SharedData
