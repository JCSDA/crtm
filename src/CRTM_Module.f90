MODULE CRTM_Module

  ! Support modules
  USE Type_Kinds
  USE Message_Handler

  ! Structure definition modules
  USE CRTM_Atmosphere_Define
  USE CRTM_Surface_Define
  USE CRTM_GeometryInfo_Define
  USE CRTM_ChannelInfo_Define
  USE CRTM_RTSolution_Define
  USE CRTM_Options_Define
  USE CRTM_SensorInput_Define

  ! Parameter definition module
  USE CRTM_Parameters
  
  ! Module for manipulating ChannelInfo
  ! structure to control CRTM execution
!  USE CRTM_ChannelInfo

  ! The main function modules
  USE CRTM_LifeCycle
  USE CRTM_Forward_Module
  USE CRTM_Tangent_Linear_Module
  USE CRTM_Adjoint_Module
  USE CRTM_K_Matrix_Module

  ! Structure I/O modules
  USE CRTM_Atmosphere_Binary_IO  ! Need to remove the "Binary" in the name
  USE CRTM_Surface_Binary_IO     ! Need to remove the "Binary" in the name
  USE CRTM_GeometryInfo_IO
  USE CRTM_RTSolution_Binary_IO  ! Need to remove the "Binary" in the name
  
  PUBLIC

END MODULE CRTM_Module
