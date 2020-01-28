!
! LBLRTMIO_Module
!
! Container module for the LBLRTM definition and I/O modules used
! in the LBLRTM I/O library.
!

MODULE LBLRTMIO_Module

  ! Module information
  ! ------------------
  ! Generic utility modules
  USE Message_Handler

  ! LBLRTM parameter and utility modules
  USE LBLRTM_Parameters
  USE LBLRTM_Utility

  ! LBLRTM Object definition modules
  USE LBLRTM_Phdr_Define
  USE LBLRTM_Panel_Define
  USE LBLRTM_Fhdr_Define
  USE LBLRTM_Layer_Define
  USE LBLRTM_File_Define

  ! LBLRTM I/O definition modules
  USE LBLRTM_Phdr_IO
  USE LBLRTM_Panel_IO
  USE LBLRTM_Fhdr_IO
  USE LBLRTM_Layer_IO
  USE LBLRTM_File_IO

  ! LBLRTM netCDF I/O definition modules
  USE LBLRTM_netCDF_IO


  ! Visibility
  ! ----------
  PUBLIC


  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTMIO_Module.f90 42407 2014-07-01 18:42:53Z paul.vandelst@noaa.gov $'
  CHARACTER(*), PRIVATE, PARAMETER :: LBLRTMIO_VERSION_ID = &
  'v1.0.0'


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTMIO_Version
!
! PURPOSE:
!       Subroutine to the LBLRTM I/O version information.
!
! CALLING SEQUENCE:
!       CALL LBLRTMIO_Version( version )
!
! OUTPUTS:
!       version:       Character string identifying the LBLRTM I/O library
!                      release version.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTMIO_Version( version )
    CHARACTER(*), INTENT(OUT) :: version
    version = LBLRTMIO_VERSION_ID
  END SUBROUTINE LBLRTMIO_Version

END MODULE LBLRTMIO_Module
