!
! AIRS_Define
!
! Module containing AIRS instrument definitions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!

MODULE AIRS_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: N_AIRS_CHANNELS
  PUBLIC :: N_AIRS_MODULES
  PUBLIC :: AIRS_MODULE
  PUBLIC :: AIRS_MODULE_BEGIN_CHANNEL
  PUBLIC :: AIRS_MODULE_END_CHANNEL
  PUBLIC :: N_AIRS_CHANNELS_PER_MODULE
  PUBLIC :: MAX_N_MODULE_CHANNELS

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! The number of AIRS channels
  INTEGER, PARAMETER :: N_AIRS_CHANNELS = 2378

  ! The AIRS module names IN FREQUENCY/CHANNEL ORDER
  INTEGER, PARAMETER :: N_AIRS_MODULES = 17
  CHARACTER(*), PARAMETER :: AIRS_MODULE(N_AIRS_MODULES) = &
    (/ 'M12','M11','M10','M9 ','M8 ','M7 ','M6 ','M5 ', &
       'M4d','M4c','M3 ','M4b','M4a','M2b','M1b','M2a','M1a' /)
 
  ! The begin channel number for each module
  INTEGER, PARAMETER :: AIRS_MODULE_BEGIN_CHANNEL(N_AIRS_MODULES) = &
    (/     1,  131,  275,  442,  609,  770,  937, 1104, &
        1263, 1369, 1463, 1655, 1761, 1865, 2015, 2145, 2261 /)

  ! The end channel number for each module
  INTEGER, PARAMETER :: AIRS_MODULE_END_CHANNEL(N_AIRS_MODULES) = &
    (/   130,  274,  441,  608,  769,  936, 1103, 1262, &
        1368, 1462, 1654, 1760, 1864, 2014, 2144, 2260, 2378 /)

  ! The number of channels per module
  INTEGER, PARAMETER :: N_AIRS_CHANNELS_PER_MODULE(N_AIRS_MODULES) = &
    (/   130,  144,  167,  167,  161,  167,  167,  159, &
         106,   94,  192,  106,  104,  150,  130,  116,  118 /)
  INTEGER, PARAMETER :: MAX_N_MODULE_CHANNELS = 192

END MODULE AIRS_Define
