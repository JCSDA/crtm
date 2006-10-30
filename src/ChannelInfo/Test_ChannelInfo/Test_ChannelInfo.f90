!------------------------------------------------------------------------------
!M+
! NAME:
!       Test_ChannelInfo
!
! PURPOSE:
!       Program to test the ChannelInfo definition and application functions.
!
! CATEGORY:
!       CRTM : ChannelInfo
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
!       CRTM_SpcCoeff:      Module containing the shared CRTM spectral
!                           coefficients (SpcCoeff) and their load/destruction
!                           routines.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 SPCCOEFF_DEFINE module
!                                 SPCCOEFF_BINARY_IO module
!                                 CRTM_PARAMETERS module
!
!       CRTM_ChannelInfo:   Module containing routines to populate the CRTM
!                           ChannelInfo structure.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 CRTM_SPCCOEFF module
!                                 CRTM_CHANNELINFO_DEFINE module
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
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2005
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


PROGRAM Test_ChannelInfo


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE CRTM_SpcCoeff
  USE CRTM_ChannelInfo


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_ChannelInfo'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_ChannelInfo.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: INVALID = -1


  ! ---------
  ! Variables
  ! ---------

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: Message
  CHARACTER( 256 ) :: Filename

  INTEGER :: Error_Status
  INTEGER :: IO_Status

  INTEGER :: n, lS, lT

  INTEGER,          DIMENSION( : ), ALLOCATABLE :: Sensor_Channel
  CHARACTER( 256 ), DIMENSION( : ), ALLOCATABLE :: Sensor_Descriptor
  INTEGER,          DIMENSION( : ), ALLOCATABLE :: NCEP_Sensor_ID

  TYPE( CRTM_ChannelInfo_type ) :: ChannelInfo



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the CRTM ChannelInfo definition and")' )
  WRITE( *, '( 5x, " indexing routines. ")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                -- GET A SPECTRAL SpcCoeff DATA FILE NAME --                #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter the Binary Spectral SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename
  Filename = ADJUSTL( FileNAME )
 


  !#----------------------------------------------------------------------------#
  !#                 -- LOAD THE SPECTRAL SpcCoeff SHARED DATA --               #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Load_SpcCoeff( Filename )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error loading shared data from the Binary Spectral '//&
                          'SpcCoeff file '//TRIM( Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#      -- LIST THE AVAILABLE CHANNELS AND REQUEST USER FOR SELECTION --      #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Available channels for ", a )' ) SC%Sensor_Descriptor(1)
  WRITE( *, '( 10i5 )' ) SC%Sensor_Channel

  WRITE( *, FMT     = '( /5x, "Enter number of requested channels: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '(i5)', &
           IOSTAT = IO_Status ) n

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading number of requested channels', &
                          FAILURE )
    STOP
  END IF


  ALLOCATE( Sensor_Channel( n ), &
            Sensor_Descriptor( n ), &
            NCEP_Sensor_ID( n ) )

  WRITE( *, '( /5x, "Enter the requested ", i5, " channels:" )' ) n
  READ( *, *, IOSTAT = IO_Status ) Sensor_Channel

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading requested channel list', &
                          FAILURE )
    STOP
  END IF


  Sensor_Descriptor = SC%Sensor_Descriptor(1)
  NCEP_Sensor_ID    = SC%NCEP_Sensor_ID(1)



  !#----------------------------------------------------------------------------#
  !#                      -- INDEX THE REQUESTED CHANNELS --                    #
  !#----------------------------------------------------------------------------#

  ! -------------------------
  ! Index by Sensor_Desriptor
  ! -------------------------

  WRITE( *, '(//5x, "Indexing ChannelInfo by Sensor_Descriptor:", / )' )

  Error_Status = CRTM_Index_ChannelInfo( Sensor_Descriptor, &
                                         Sensor_Channel, &
                                         ChannelInfo )


  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error indexing by Sensor_Descriptor', &
                          Error_Status )
    STOP
  END IF


  WRITE( *, '( /10x, "Selected_Channel_Index:", /, 9(10i5,/) )' ) ChannelInfo%Selected_Channel_Index
  WRITE( *, '( /10x, "Sensor_Descriptor  :", /, 9(10(1x,a),/) )' ) ChannelInfo%Sensor_Descriptor
  WRITE( *, '( /10x, "NCEP_Sensor_ID     :", /, 9(10i5,/) )' ) ChannelInfo%NCEP_Sensor_ID  
  WRITE( *, '( /10x, "WMO_Satellite_ID   :", /, 9(10i5,/) )' ) ChannelInfo%WMO_Satellite_ID
  WRITE( *, '( /10x, "WMO_Sensor_ID      :", /, 9(10i5,/) )' ) ChannelInfo%WMO_Sensor_ID   
  WRITE( *, '( /10x, "Sensor_Channel     :", /, 9(10i5,/) )' ) ChannelInfo%Sensor_Channel
  WRITE( *, '( /10x, "Channel_Index      :", /, 9(10i5,/) )' ) ChannelInfo%Channel_Index      
  WRITE( *, '( /10x, "Node_Index         :", /, 9(10i5,/) )' ) ChannelInfo%Node_Index         
  WRITE( *, '( /10x, "n_Channels_per_Node:", /, 9(10i5,/) )' ) ChannelInfo%n_Channels_per_Node
  WRITE( *, '( /10x, "Channel_Node_Map   :", /, 9(10i5,/) )' ) ChannelInfo%Channel_Node_Map


  ! -----------------------
  ! Index by NCEP_Sensor_ID
  ! -----------------------

  WRITE( *, '(//5x, "Indexing ChannelInfo by NCEP_Sensor_ID:", / )' )

  Error_Status = CRTM_Index_ChannelInfo( NCEP_Sensor_ID, &
                                         Sensor_Channel, &
                                         ChannelInfo )


  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error indexing by NCEP_Sensor_ID', &
                          Error_Status )
    STOP
  END IF


  WRITE( *, '( /10x, "Selected_Channel_Index:", /, 9(10i5,/) )' ) ChannelInfo%Selected_Channel_Index
  WRITE( *, '( /10x, "Sensor_Descriptor  :", /, 9(10(1x,a),/) )' ) ChannelInfo%Sensor_Descriptor
  WRITE( *, '( /10x, "NCEP_Sensor_ID     :", /, 9(10i5,/) )' ) ChannelInfo%NCEP_Sensor_ID  
  WRITE( *, '( /10x, "WMO_Satellite_ID   :", /, 9(10i5,/) )' ) ChannelInfo%WMO_Satellite_ID
  WRITE( *, '( /10x, "WMO_Sensor_ID      :", /, 9(10i5,/) )' ) ChannelInfo%WMO_Sensor_ID   
  WRITE( *, '( /10x, "Sensor_Channel     :", /, 9(10i5,/) )' ) ChannelInfo%Sensor_Channel
  WRITE( *, '( /10x, "Channel_Index      :", /, 9(10i5,/) )' ) ChannelInfo%Channel_Index      
  WRITE( *, '( /10x, "Node_Index         :", /, 9(10i5,/) )' ) ChannelInfo%Node_Index         
  WRITE( *, '( /10x, "n_Channels_per_Node:", /, 9(10i5,/) )' ) ChannelInfo%n_Channels_per_Node
  WRITE( *, '( /10x, "Channel_Node_Map   :", /, 9(10i5,/) )' ) ChannelInfo%Channel_Node_Map



  !#----------------------------------------------------------------------------#
  !#               -- DESTROY THE SPECTRAL SpcCoeff SHARED DATA --              #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_SpcCoeff()

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying the Spectral SpcCoeff shared data structure.', &
                          WARNING )
  END IF

END PROGRAM Test_ChannelInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_ChannelInfo.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_ChannelInfo.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/07/14 17:28:36  paulv
! Initial checkin.
!
!
!
