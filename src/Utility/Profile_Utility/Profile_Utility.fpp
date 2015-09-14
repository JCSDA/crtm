!
! Profile_Utility module
!
! Container module for all the Profile_Utility modules and dependencies.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-May-2006
!                       paul.vandelst@noaa.gov
!

MODULE Profile_Utility

  ! Module information
  ! ------------------
  ! Support modules
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Fundamental_Constants
  USE Compare_Float_Numbers

  ! The library modules
  USE Atmospheric_Properties
  USE Geopotential
  USE Level_Layer_Conversion
  USE Profile_Utility_Parameters
  USE Units_Conversion


  ! Visibility
  ! ----------
  PUBLIC


  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  CHARACTER(*), PRIVATE, PARAMETER :: LIBRARY_VERSION_ID = &
#include "Profile_Utility_Version.inc"


CONTAINS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Profile_Utility_Version
!
! PURPOSE:
!       Subroutine to provide profile utility library version information.
!
! CALLING SEQUENCE:
!       CALL Profile_Utility_Version( version )
!
! OUTPUTS:
!       version:  Character string identifying the profile utility
!                 library version.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER(*)
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Profile_Utility_Version( version )
    CHARACTER(*), INTENT(OUT) :: version
    version = LIBRARY_VERSION_ID
  END SUBROUTINE Profile_Utility_Version

END MODULE Profile_Utility
