!------------------------------------------------------------------------------
!M+
! NAME:
!       Test_GeometryInfo
!
! PURPOSE:
!       Program to test the GeometryInfo definition and application functions.
!
! CATEGORY:
!       CRTM : GeometryInfo
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       Message_Handler:    Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       CRTM_GeometryInfo:  Module containing routines to populate the CRTM
!                           GeometryInfo structure.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 CRTM_GEOMETRYINFO_DEFINE module
!
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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


PROGRAM Test_GeometryInfo


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_GeometryInfo_Define


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_GeometryInfo'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_GeometryInfo.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: INVALID = -1


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message

  INTEGER :: Error_Status
  TYPE( CRTM_GeometryInfo_type ) :: gInfo



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the CRTM GeometryInfo definition and")' )
  WRITE( *, '( 5x, " application routines. ")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                           -- LOAD SOME INPUT DATA --                       #
  !#----------------------------------------------------------------------------#

  gInfo%Earth_Radius         = 6400.0_fp_kind
  gInfo%Satellite_Height     = 750.0_fp_kind
  gInfo%Longitude            = 0.0_fp_kind
  gInfo%Latitude             = 0.0_fp_kind
  gInfo%Surface_Altitude     = 0.0_fp_kind
  gInfo%Sensor_Scan_Angle    = 55.0_fp_kind
  gInfo%Sensor_Zenith_Angle  = 60.0_fp_kind
  gInfo%Sensor_Azimuth_Angle = 0.0_fp_kind
  gInfo%Source_Zenith_Angle  = 71.0_fp_kind
  gInfo%Source_Azimuth_Angle = 0.0_fp_kind



  !#----------------------------------------------------------------------------#
  !#              -- FILL THE REST OF THE GeometryInfo STRUCTURE --             #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Compute_GeometryInfo( gInfo )

  


  !#----------------------------------------------------------------------------#
  !#                            -- OUTPUT RESULTS --                            #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "USER INPUT" )' )
  WRITE( *, '( 10x, "Earth_Radius        :", es13.6 )' ) gInfo%Earth_Radius        
  WRITE( *, '( 10x, "Satellite_Height    :", es13.6 )' ) gInfo%Satellite_Height    
  WRITE( *, '( 10x, "Longitude           :", es13.6 )' ) gInfo%Longitude           
  WRITE( *, '( 10x, "Latitude            :", es13.6 )' ) gInfo%Latitude            
  WRITE( *, '( 10x, "Surface_Altitude    :", es13.6 )' ) gInfo%Surface_Altitude    
  WRITE( *, '( 10x, "Sensor_Scan_Angle   :", es13.6 )' ) gInfo%Sensor_Scan_Angle   
  WRITE( *, '( 10x, "Sensor_Zenith_Angle :", es13.6 )' ) gInfo%Sensor_Zenith_Angle 
  WRITE( *, '( 10x, "Sensor_Azimuth_Angle:", es13.6 )' ) gInfo%Sensor_Azimuth_Angle
  WRITE( *, '( 10x, "Source_Zenith_Angle :", es13.6 )' ) gInfo%Source_Zenith_Angle 
  WRITE( *, '( 10x, "Source_Azimuth_Angle:", es13.6 )' ) gInfo%Source_Azimuth_Angle
  WRITE( *, '( 10x, "Flux_Zenith_Angle   :", es13.6 )' ) gInfo%Flux_Zenith_Angle

  WRITE( *, '( /5x, "DERIVED INPUT" )' )
  WRITE( *, '( 10x, "Sensor_Scan_Radian   :", es13.6 )' ) gInfo%Sensor_Scan_Radian
  WRITE( *, '( 10x, "Sensor_Zenith_Radian :", es13.6 )' ) gInfo%Sensor_Zenith_Radian
  WRITE( *, '( 10x, "Sensor_Azimuth_Radian:", es13.6 )' ) gInfo%Sensor_Azimuth_Radian
  WRITE( *, '( 10x, "Secant_Sensor_Zenith :", es13.6 )' ) gInfo%Secant_Sensor_Zenith
  WRITE( *, '( 10x, "Source_Zenith_Radian :", es13.6 )' ) gInfo%Source_Zenith_Radian
  WRITE( *, '( 10x, "Source_Azimuth_Radian:", es13.6 )' ) gInfo%Source_Azimuth_Radian
  WRITE( *, '( 10x, "Secant_Source_Zenith :", es13.6 )' ) gInfo%Secant_Source_Zenith
  WRITE( *, '( 10x, "Flux_Zenith_Radian   :", es13.6 )' ) gInfo%Flux_Zenith_Radian
  WRITE( *, '( 10x, "Secant_Flux_Zenith   :", es13.6 )' ) gInfo%Secant_Flux_Zenith

END PROGRAM Test_GeometryInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_GeometryInfo.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_GeometryInfo.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/07/20 15:24:49  paulv
! Initial checkin.
!
!
!
