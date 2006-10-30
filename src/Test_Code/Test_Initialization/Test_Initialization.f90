!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_Initialization
!
! PURPOSE:
!       Program to test the CRTM initialization code.
!
! CATEGORY:
!       CRTM : Test : Initialization
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:     Module containing definitions for kinds
!                       of variable types.
!
!       Message_Handler:Module to define simple error codes and
!                       handle error conditions
!                       USEs: FILE_UTILITY module
!
!       CRTM_Module:    The main CRTM module.
!
! CONTAINS:
!       Print_ChannelInfo:  Subroutine to print the contents of the
!                           ChannelInfo structure returned from the
!                           initialisation.
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
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
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
!P-
!------------------------------------------------------------------------------

PROGRAM Test_Initialization


  ! ------------
  ! Module usage
  ! ------------

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler


  ! -- CRTM modules
  USE CRTM_ChannelInfo_Define
  USE CRTM_ChannelInfo
  USE CRTM_LifeCycle


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_Initialization'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_Initialization.f90,v 1.5 2006/05/02 14:58:35 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: N_AIRS_CHANNELS  = 2378
  INTEGER, PARAMETER :: N_SSMIS_CHANNELS = 24
  INTEGER, PARAMETER :: N_HIRS_CHANNELS  = 19
  INTEGER, PARAMETER :: N_AMSUA_CHANNELS = 15


  ! ---------
  ! Variables
  ! ---------

  INTEGER :: Error_Status
  INTEGER :: l

  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: CloudCoeff_File  
  CHARACTER( 256 ) :: AerosolCoeff_File
  CHARACTER( 256 ) :: EmisCoeff_File   

  TYPE( CRTM_ChannelInfo_type ) :: ChannelInfo



  !#----------------------------------------------------------------------------#
  !#                         -- INITIALISE THE CRTM --                          #
  !#----------------------------------------------------------------------------#

  ! -----------------------------
  ! Get the coefficient filenames
  ! -----------------------------

  WRITE( *, FMT     = '( /5x, "Enter the SpcCoeff filename    : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) SpcCoeff_File
  SpcCoeff_File = ADJUSTL( SpcCoeff_File )

  WRITE( *, FMT     = '(  5x, "Enter the TauCoeff filename    : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) TauCoeff_File
  TauCoeff_File = ADJUSTL( TauCoeff_File )

  WRITE( *, FMT     = '(  5x, "Enter the CloudCoeff filename  : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) CloudCoeff_File
  CloudCoeff_File = ADJUSTL( CloudCoeff_File )

  WRITE( *, FMT     = '(  5x, "Enter the AerosolCoeff filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) AerosolCoeff_File
  AerosolCoeff_File = ADJUSTL( AerosolCoeff_File )

  WRITE( *, FMT     = '(  5x, "Enter the EmisCoeff filename   : " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) EmisCoeff_File
  EmisCoeff_File = ADJUSTL( EmisCoeff_File )


  ! --------------------------
  ! Perform the initialisation
  ! --------------------------

  WRITE( *, '( /5x, "Initializing the CRTM..." )' )

  Error_Status = CRTM_Init( ChannelInfo, &
                            SpcCoeff_File     = SpcCoeff_File,     &
                            TauCoeff_File     = TauCoeff_File,     &
                            CloudCoeff_File   = CloudCoeff_File,   &
                            AerosolCoeff_File = AerosolCoeff_File, &
                            EmisCoeff_File    = EmisCoeff_File,    &
                            Sensor_Descriptor = (/ ( 'airs281SUBSET_aqua', l =  1, N_AIRS_CHANNELS, 40 ), &
                                                   ( 'ssmis_f16         ', l =  1, N_SSMIS_CHANNELS, 2 ), &
                                                   ( 'airs281SUBSET_aqua', l = 10, N_AIRS_CHANNELS, 40 ), &
                                                   ( 'hirs3_n17         ', l = N_HIRS_CHANNELS, 1, -1 ),  &
                                                   ( 'amsua_n17         ', l = 1, N_AMSUA_CHANNELS ) /), &
                            Sensor_Channel = (/ ( l, l =  1, N_AIRS_CHANNELS, 40 ), &
                                                ( l, l =  1, N_SSMIS_CHANNELS, 2 ), &
                                                ( l, l = 10, N_AIRS_CHANNELS, 40 ), &
                                                ( l, l = N_HIRS_CHANNELS, 1, -1 ),  &
                                                ( l, l = 1, N_AMSUA_CHANNELS ) /)   )

  IF ( Error_Status /= SUCCESS ) THEN 
     CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
   STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                          -- PRINT OUT SOME INFO --                         #
  !#----------------------------------------------------------------------------#

  CALL Print_ChannelInfo( ChannelInfo )



  !#----------------------------------------------------------------------------#
  !#                           -- DESTROY THE CRTM --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Destroying the CRTM..." )' )

  Error_Status = CRTM_Destroy( ChannelInfo )

  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
    STOP
  END IF


CONTAINS


  SUBROUTINE Print_ChannelInfo( ChannelInfo )
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN ) :: ChannelInfo
    INTEGER :: l

    WRITE( *, '( /5x, "Number of channels indexed: ", i5 )' ) ChannelInfo%n_Channels
    WRITE( *, '(  /2x, "Channel         Sensor             NCEP          WMO           WMO     Channel", &
                 &/2x, " Index        Descriptor         Sensor ID   Satellite ID   Sensor ID   Number", &
                 &/2x, "------------------------------------------------------------------------------" )' )
    DO l = 1, ChannelInfo%n_Channels
      WRITE( *, '( 2x, 2x, i4, 2x, ">", a, "<", 5x, i3, 11x, i3, 11x, i3, 7x, i4 )' ) &
                ChannelInfo%Channel_Index( l ), &
                ChannelInfo%Sensor_Descriptor( l ), &
                ChannelInfo%NCEP_Sensor_ID( l ), &
                ChannelInfo%WMO_Satellite_ID( l ), &
                ChannelInfo%WMO_Sensor_ID( l ), &
                ChannelInfo%Sensor_Channel( l )
    END DO
 
  END SUBROUTINE Print_ChannelInfo

END PROGRAM Test_Initialization


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_Initialization.f90,v 1.5 2006/05/02 14:58:35 dgroff Exp $
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_Initialization.f90,v $
! Revision 1.5  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.4  2006/03/01 21:25:36  paulv
! - Updated test for latest code.
!
! Revision 1.3  2004/06/24 18:46:42  paulv
! - Updated to include ScatterCoeff data in the initialization.
!
! Revision 1.2  2004/06/23 14:36:44  paulv
! - Testing the Sensor_Descriptor and Sensor_Channel optional arguments to
!   the initialisation function.
!
! Revision 1.1  2004/06/15 20:25:00  paulv
! Initial checkin.
!
!
!
