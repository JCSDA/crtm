!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Type_Kinds
!
! PURPOSE:
!       Program to display the kind types and sizes defined in the
!       Type_Kinds module.
!
! CATEGORY:
!       Utility
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:     Module containing definitions for kinds
!                       of variable types.
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
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 23-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003, 2004 Paul van Delst
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
!P-
!------------------------------------------------------------------------------

PROGRAM Test_Type_Kinds


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Type_Kinds'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Type_Kinds.f90,v 1.3 2004/11/30 19:19:34 paulv Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x,a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to display kind type values and sizes defined" )' )
  WRITE( *, '( 5x, "   in the Type_Kinds module." )' )
  WRITE( *, '(/5x, " $Revision: 1.3 $")' )
  WRITE( *, '( 5x, a )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#             -- DISPLAY THE INTEGER KIND TYPES AND BYTE-SIZES --            #
  !#----------------------------------------------------------------------------#

  ! ----------
  ! Kind types
  ! ----------

  WRITE( *, '(  /5x, "Integer Kind types: ", &
              &/10x, "Byte    integer kind type: ", i5, &
              &/10x, "Short   integer kind type: ", i5, &
              &/10x, "Long    integer kind type: ", i5, &
              &/10x, "LLong   integer kind type: ", i5, &
              &/10x, "ip_kind integer kind type: ", i5 )' ) &
             Byte, Short, Long, LLong, ip_kind

  ! ----------
  ! Byte sizes
  ! ----------

  WRITE( *, '(  /5x, "Expected Integer 8-bit byte sizes: ", &
              &/10x, "Byte    integer kind type size: ", i5, " byte", &
              &/10x, "Short   integer kind type size: ", i5, " bytes", &
              &/10x, "Long    integer kind type size: ", i5, " bytes", &
              &/10x, "LLong   integer kind type size: ", i5, " bytes", &
              &/10x, "ip_kind integer kind type size: ", i5, " bytes" )' ) &
             n_Bytes_Byte, n_Bytes_Short, n_Bytes_Long, n_Bytes_LLong, n_Bytes_IP_Kind


  !#----------------------------------------------------------------------------#
  !#               -- DISPLAY THE REAL KIND TYPES AND BYTE-SIZES --             #
  !#----------------------------------------------------------------------------#

  ! ----------
  ! Kind types
  ! ----------

  WRITE( *, '( //5x, "Real Kind types: ", &
              &/10x, "Single  float kind type: ", i5, &
              &/10x, "Double  float kind type: ", i5, &
              &/10x, "Quad    float kind type: ", i5, &
              &/10x, "fp_kind float kind type: ", i5 )' ) &
             Single, Double, Quad, fp_kind


  ! ----------
  ! Byte sizes
  ! ----------

  WRITE( *, '(  /5x, "Expected Real 8-bit byte sizes: ", &
              &/10x, "Single  float kind type size: ", i5, " bytes", &
              &/10x, "Double  float kind type size: ", i5, " bytes", &
              &/10x, "Quad    float kind type size: ", i5, " bytes", &
              &/10x, "fp_kind float kind type size: ", i5, " bytes" )' ) &
             n_Bytes_Single, n_Bytes_Double, n_Bytes_Quad, n_Bytes_FP_Kind

END PROGRAM Test_Type_Kinds


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Type_Kinds.f90,v 1.3 2004/11/30 19:19:34 paulv Exp $
!
! $Date: 2004/11/30 19:19:34 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Type_Kinds.f90,v $
! Revision 1.3  2004/11/30 19:19:34  paulv
! - Added program header
! - Added output of expected byte sizes.
!
!
!

