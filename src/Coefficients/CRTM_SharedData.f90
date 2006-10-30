MODULE CRTM_SharedData

! Provides CRTM-wide access to shared data without the
! need to USE specific shared data modules.

  USE CRTM_SpcCoeff,     ONLY: SC
  USE CRTM_TauCoeff,     ONLY: TC
  USE CRTM_ScatterCoeff, ONLY: ScatC

END MODULE CRTM_SharedData
