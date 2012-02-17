!
! Sensor_Planck_Utility module
!
! Container module for all the Planck and Sensor_Planck modules and dependencies.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 16-Feb-2012
!                       paul.vandelst@noaa.gov
!

MODULE Sensor_Planck_Utility
  ! The Utility modules
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Fundamental_Constants
  USE Compare_Float_Numbers
  ! The SpcCoeff modules
  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  ! The Planck modules
  USE Planck_Functions
  USE Sensor_Planck_Functions
  PUBLIC
END MODULE Sensor_Planck_Utility
