!--------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_Parameters
!
! PURPOSE:
!       Module containing shared parameters required for LBLRTM
!       format file IO
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LBLRTM_Parameters
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
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
!--------------------------------------------------------------------------------

MODULE LBLRTM_Parameters

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  !#----------------------------------------------------------------------------#
  !#                           -- MODULE PARAMETERS --                          #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------------------------
  ! Data types. The data types are respecified here so that they are
  ! easy to change for the case where the "double precision" version
  ! of LBLRTM has been used. In this case, all the single precision
  ! variable become double precision, and the 4-byte integers become
  ! 8-byte integers. The latter is not available on all platforms.
  ! ----------------------------------------------------------------

  ! -- Single precision types
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_KIND    = Long
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_N_BYTES = n_Bytes_Long
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_KIND    = Single
! INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_N_BYTES = n_Bytes_Single

  ! -- Double precision types
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_KIND    = LLong
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_IP_N_BYTES = n_Bytes_LLong
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_KIND    = Double
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FP_N_BYTES = n_Bytes_Double


  ! ---------------------------
  ! Maximum number of molecules
  ! ---------------------------

  INTEGER, PUBLIC, PARAMETER :: LBLRTM_MAX_N_MOLECULES = 64


  ! -------------------------------------------
  ! Maximum number of points in a panel of data
  ! -------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: LBLRTM_MAX_PANEL_POINTS = 2400


  ! -----------------
  ! File status flags
  ! -----------------

  ! -- File status is o.k.
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_OK    =   1

  ! -- End-Of-File
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_EOF   =   0

  ! -- End-Of-Layer
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_EOL   = -99

  ! -- File status is undefined
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_FILE_PTR_UNDEF =  -1


  ! ----------------
  ! Panel inforation
  ! ----------------

  ! -- The number of recognised panel types
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_N_PANEL_TYPES = 4

  ! -- Panel type codes
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_UNKNOWN_PANEL_TYPE   = 1
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_UNDEFINED_PANEL_TYPE = 2
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_SINGLE_PANEL_TYPE    = 3 ! This must be 3
  INTEGER, PUBLIC, PARAMETER :: LBLRTM_DOUBLE_PANEL_TYPE    = 4 ! This must be 4
  INTEGER, PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_PANEL_TYPE = (/ LBLRTM_UNKNOWN_PANEL_TYPE,   &   
                           LBLRTM_UNDEFINED_PANEL_TYPE, & 
                           LBLRTM_SINGLE_PANEL_TYPE,    &    
                           LBLRTM_DOUBLE_PANEL_TYPE    /)

  ! -- The number of panels for each type    
  INTEGER, PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_N_PANELS = (/ -1, &  ! LBLRTM_UNKNOWN_PANEL_TYPE  
                         -1, &  ! LBLRTM_UNDEFINED_PANEL_TYPE
                          1, &  ! LBLRTM_SINGLE_PANEL_TYPE   
                          2 /)  ! LBLRTM_DOUBLE_PANEL_TYPE   

  ! -- The panel type names. Used for error/message output.
  CHARACTER( * ), PUBLIC, PARAMETER, DIMENSION( LBLRTM_N_PANEL_TYPES ) :: &
    LBLRTM_PANEL_TYPE_NAME = (/ 'UNKNOWN  ', &  ! LBLRTM_UNKNOWN_PANEL_TYPE  
                                'UNDEFINED', &  ! LBLRTM_UNDEFINED_PANEL_TYPE
                                'SINGLE   ', &  ! LBLRTM_SINGLE_PANEL_TYPE   
                                'DOUBLE   ' /)  ! LBLRTM_DOUBLE_PANEL_TYPE   

END MODULE LBLRTM_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: LBLRTM_Parameters.f90,v 1.7 2005/05/08 14:58:59 paulv Exp $
!
! $Date: 2005/05/08 14:58:59 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_Parameters.f90,v $
! Revision 1.7  2005/05/08 14:58:59  paulv
! - Upgraded to Fortran-95
! - Rearranged the panel type definition arrays.
!
! Revision 1.6  2003/07/16 16:35:26  paulv
! - Correct mistake in header docs.
!
! Revision 1.5  2002/05/15 20:21:04  paulv
! - Changed default LBLRTM floating point and integer kind types from single
!   precision to double precision.
!
! Revision 1.4  2002/04/16 18:38:19  paulv
! - Added the number of bytes for the various LBLRTM_XX_KINDs as parameters.
!   This allows header sizes to be calculated.
! - Updated documentation.
!
! Revision 1.3  2002/04/09 15:35:24  paulv
! - Updated documentation.
!
! Revision 1.2  2002/04/09 03:20:55  paulv
! - Bringing repository up to date. Minor changes but still incomplete.
!
! Revision 1.1  2002/03/29 19:53:56  paulv
! Initial checkin of new LBLRTM IO routines.
!
!
!
