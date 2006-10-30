!------------------------------------------------------------------------------
!M+
! NAME:
!       AIRS_Define
!
! PURPOSE:
!       Module containing AIRS instrument definitions.
!
! CATEGORY:
!       AIRS
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
! CONTAINS:
!       None.
!
! DERIVED TYPES:
!       None.
!
! PARAMETERS:
!       N_AIRS_CHANNELS:             Total number of AIRS channels
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!       N_AIRS_MODULES:              Number of AIRS detector array modules.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
!
!       AIRS_MODULE:                 Character string of AIRS detector array
!                                    modules in FREQUENCY/CHANNEL order.
!                                    UNITS:      N/A
!                                    TYPE:       CHARACTER(*)
!                                    DIMENSION:  Rank-1 (N_AIRS_MODULES)
!
!       AIRS_MODULE_BEGIN_CHANNEL:   The beginning channel number of each
!                                    detector array module. Same module order
!                                    as AIRS_MODULE array.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Rank-1 (N_AIRS_MODULES)
!
!       AIRS_MODULE_END_CHANNEL:     The ending channel number of each
!                                    detector array module Same module order
!                                    as AIRS_MODULE array.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Rank-1 (N_AIRS_MODULES)
!
!       N_AIRS_CHANNELS_PER_MODULE:  The number of channels in each detector
!                                    array module. Same module order
!                                    as AIRS_MODULE array.
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Rank-1 (N_AIRS_MODULES)
!
!       MAX_N_MODULE_CHANNELS:       The maximum number of channels in any
!                                    detector array module
!                                    UNITS:      N/A
!                                    TYPE:       INTEGER
!                                    DIMENSION:  Scalar
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
!       Written by:     Paul van Delst, CIMSS/SSEC 25-Nov-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
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

MODULE AIRS_Define

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  ! -- The module version control ID string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: AIRS_Define.f90,v 1.3 2004/08/10 16:53:55 paulv Exp $'

  ! -- The number of AIRS channels
  INTEGER, PUBLIC, PARAMETER :: N_AIRS_CHANNELS = 2378

  ! -- The AIRS module names IN FREQUENCY/CHANNEL ORDER
  INTEGER, PUBLIC, PARAMETER :: N_AIRS_MODULES = 17
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( N_AIRS_MODULES ) :: AIRS_MODULE = &
    (/ 'M12','M11','M10','M9 ','M8 ','M7 ','M6 ','M5 ', &
       'M4d','M4c','M3 ','M4b','M4a','M2b','M1b','M2a','M1a' /)
 
  ! -- The begin channel number for each module
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_AIRS_MODULES ) :: AIRS_MODULE_BEGIN_CHANNEL = &
    (/     1,  131,  275,  442,  609,  770,  937, 1104, &
        1263, 1369, 1463, 1655, 1761, 1865, 2015, 2145, 2261 /)

  ! -- The end channel number for each module
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_AIRS_MODULES ) :: AIRS_MODULE_END_CHANNEL = &
    (/   130,  274,  441,  608,  769,  936, 1103, 1262, &
        1368, 1462, 1654, 1760, 1864, 2014, 2144, 2260, 2378 /)

  ! -- The number of channels per module
  INTEGER, PUBLIC, PARAMETER, DIMENSION( N_AIRS_MODULES ) :: N_AIRS_CHANNELS_PER_MODULE = &
    (/   130,  144,  167,  167,  161,  167,  167,  159, &
         106,   94,  192,  106,  104,  150,  130,  116,  118 /)
  INTEGER, PUBLIC, PARAMETER :: MAX_N_MODULE_CHANNELS = 192

END MODULE AIRS_Define


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: AIRS_Define.f90,v 1.3 2004/08/10 16:53:55 paulv Exp $
!
! $Date: 2004/08/10 16:53:55 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: AIRS_Define.f90,v $
! Revision 1.3  2004/08/10 16:53:55  paulv
! - Modified for f90->f95 conversion.
!
! Revision 1.2  2003/11/21 14:59:48  paulv
! - Replaced old, crappy AIRS_Channel_Index routines and derived types with
!   the AIRS_Subset derived type and it's associated initialisation and
!   destruction functions.
! - Added Index_AIRS_Subset() function to return the required AIRS channel
!   indices for the subset channels iun a given module.
!
! Revision 1.1  2002/11/27 15:01:24  paulv
! Initial checkin.
!
!
!
!
