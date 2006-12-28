MODULE CRTM_Module

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_RTSolution_Define
  USE CRTM_Options_Define

  USE CRTM_Parameters

  USE CRTM_LifeCycle
!  USE CRTM_ChannelInfo

  USE CRTM_Forward_Module
  USE CRTM_Tangent_Linear_Module
  USE CRTM_Adjoint_Module
  USE CRTM_K_Matrix_Module

  PUBLIC

END MODULE CRTM_Module
