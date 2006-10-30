!------------------------------------------------------------------------------
!M+
! NAME:
!       AtmProfile_Parameters
!
! PURPOSE:
!       Module defining parameters used in creating AtmProfile netCDF files.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE AtmProfile_Parameters
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Oct-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE AtmProfile_Parameters


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- The current number of profile sets and their ID tag
  INTEGER,        PUBLIC, PARAMETER :: N_ATMPROFILE_SETS = 3
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_ATMPROFILE_SETS ) :: &
    ATMPROFILE_SET_ID_TAG = (/ 'UMBC ', &
                               'CIMSS', &
                               'ECMWF' /)

  ! -- The number of profiles in each set
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_ATMPROFILE_SETS ) :: &
    N_ATMPROFILES = (/ 48, &
                    32, &
                    52 /)
  INTEGER, PUBLIC, PARAMETER :: ATMPROFILE_BEGIN = 1

  ! -- The number of interpolation levels (AIRS standard levels)
  INTEGER, PUBLIC, PARAMETER :: N_ATMPROFILE_LEVELS = 101
  INTEGER, PUBLIC, PARAMETER :: N_ATMPROFILE_LAYERS = 100


  ! -----------------------------------------
  ! The pressure levels. Standard AIRS levels
  ! -----------------------------------------

  REAL( fp_kind ), PUBLIC, PARAMETER, DIMENSION( N_ATMPROFILE_LEVELS ) :: ATMPROFILE_LEVEL_PRESSURE = &
  (/ 1100.000000_fp_kind, 1070.916940_fp_kind, 1042.231940_fp_kind, 1013.947655_fp_kind, &
      986.066601_fp_kind,  958.591154_fp_kind,  931.523549_fp_kind,  904.865880_fp_kind, &
      878.620096_fp_kind,  852.788003_fp_kind,  827.371259_fp_kind,  802.371376_fp_kind, &
      777.789716_fp_kind,  753.627494_fp_kind,  729.885772_fp_kind,  706.565460_fp_kind, &
      683.667316_fp_kind,  661.191946_fp_kind,  639.139797_fp_kind,  617.511163_fp_kind, &
      596.306182_fp_kind,  575.524832_fp_kind,  555.166935_fp_kind,  535.232153_fp_kind, &
      515.719989_fp_kind,  496.629785_fp_kind,  477.960722_fp_kind,  459.711821_fp_kind, &
      441.881941_fp_kind,  424.469776_fp_kind,  407.473861_fp_kind,  390.892566_fp_kind, &
      374.724098_fp_kind,  358.966503_fp_kind,  343.617659_fp_kind,  328.675286_fp_kind, &
      314.136936_fp_kind,  300.000000_fp_kind,  286.261706_fp_kind,  272.919120_fp_kind, &
      259.969142_fp_kind,  247.408514_fp_kind,  235.233814_fp_kind,  223.441461_fp_kind, &
      212.027712_fp_kind,  200.988665_fp_kind,  190.320260_fp_kind,  180.018279_fp_kind, &
      170.078348_fp_kind,  160.495939_fp_kind,  151.266366_fp_kind,  142.384796_fp_kind, &
      133.846240_fp_kind,  125.645562_fp_kind,  117.777481_fp_kind,  110.236565_fp_kind, &
      103.017244_fp_kind,   96.113803_fp_kind,   89.520390_fp_kind,   83.231016_fp_kind, &
       77.239560_fp_kind,   71.539768_fp_kind,   66.125259_fp_kind,   60.989530_fp_kind, &
       56.125953_fp_kind,   51.527786_fp_kind,   47.188171_fp_kind,   43.100144_fp_kind, &
       39.256633_fp_kind,   35.650467_fp_kind,   32.274378_fp_kind,   29.121009_fp_kind, &
       26.182918_fp_kind,   23.452583_fp_kind,   20.922408_fp_kind,   18.584732_fp_kind, &
       16.431833_fp_kind,   14.455936_fp_kind,   12.649223_fp_kind,   11.003835_fp_kind, &
        9.511889_fp_kind,    8.165480_fp_kind,    6.956695_fp_kind,    5.877623_fp_kind, &
        4.920362_fp_kind,    4.077038_fp_kind,    3.339812_fp_kind,    2.700897_fp_kind, &
        2.152573_fp_kind,    1.687200_fp_kind,    1.297240_fp_kind,    0.975274_fp_kind, &
        0.714023_fp_kind,    0.506374_fp_kind,    0.345404_fp_kind,    0.224412_fp_kind, &
        0.136954_fp_kind,    0.076879_fp_kind,    0.038383_fp_kind,    0.016065_fp_kind, &
        0.005000_fp_kind /)

END MODULE AtmProfile_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AtmProfile_Parameters.f90,v 4.0 2004/11/02 20:13:02 paulv Exp $
!
! $Date: 2004/11/02 20:13:02 $
!
! $Revision: 4.0 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AtmProfile_Parameters.f90,v $
! Revision 4.0  2004/11/02 20:13:02  paulv
! - New versions for modified AtmProfile structure.
!
!
!
