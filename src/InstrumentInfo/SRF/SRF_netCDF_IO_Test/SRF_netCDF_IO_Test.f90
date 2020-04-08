!------------------------------------------------------------------------------
!P+
! NAME:
!       SRF_netCDF_IO_Test
!
! PURPOSE:
!       Program to test the SRF netCDF format I/O functions.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       SRF_Define:                 Module defining the SRF data structure
!                                   and containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         Message_Handler module
!
!       SRF_netCDF_IO:              Module containing routines to read and
!                                   write SRF netCDF format files.
!                                   USEs: TYPE_KINDS module
!                                         Message_Handler module
!                                         SRF_DEFINE module
!                                         NETCDF module
!                                         NETCDF_UTILITY module
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
!       netCDF SRF data file
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Apr-2002
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
!P-
!------------------------------------------------------------------------------

PROGRAM SRF_netCDF_IO_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SRF_Define
  USE SRF_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SRF_netCDF_IO_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  CHARACTER( * ), PARAMETER :: OUTPUT_SRF_FILENAME = 'Test.srf.nc'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SRF_Filename

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status

  ! -- SRF dimensions
  INTEGER :: n_Channels, l

  ! -- SRF global attributes
  CHARACTER( 256 ) :: Title
  CHARACTER( 256 ) :: History
  CHARACTER( 256 ) :: Sensor_Name
  CHARACTER( 256 ) :: Platform_Name
  CHARACTER( 256 ) :: Comment

  ! -- SRF sensor IDs
  INTEGER :: NCEP_Sensor_ID  
  INTEGER :: WMO_Satellite_ID
  INTEGER :: WMO_Sensor_ID

  ! -- SRF channel list data array
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Channel_List

  ! -- SRF data structure
  TYPE( SRF_type ) :: SRF



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, * )
  WRITE( *, '( 5x, " Program to test read netCDF SRF data files.")' )
  WRITE( *, * )
  WRITE( *, '( 5x, " $Revision: 1.7 $")' )
  WRITE( *, '( 5x, a)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                     -- ENTER A NETCDF SRF FILENAME --                      #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a netCDF SRF data file : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SRF_Filename
  SRF_Filename = ADJUSTL( SRF_Filename )



  !#----------------------------------------------------------------------------#
  !#                        -- INQUIRE THE SRF FILE --                          #
  !#----------------------------------------------------------------------------#

  ! ---------------------------------------------
  ! Get the file dimensions and global attributes
  ! ---------------------------------------------

  Error_Status = Inquire_SRF_netCDF( TRIM( SRF_Filename ), &
                                     n_Channels       = n_Channels, &
                                     NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                     WMO_Satellite_ID = WMO_Satellite_ID, &
                                     WMO_Sensor_ID    = WMO_Sensor_ID, &
                                     Title            = Title, &
                                     History          = History, &
                                     Sensor_Name      = Sensor_Name, &
                                     Platform_Name    = Platform_Name, &
                                     Comment          = Comment )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error inquiring the netCDF SRF file '//&
                          TRIM( SRF_Filename ), &
                          Error_Status )
    STOP
  END IF


  ! ----------------
  ! Output some info
  ! ----------------

  WRITE( *, '( /5x, "SRF data set attributes:" )' )
  WRITE( *, '(  5x, "TITLE         : ", a )' ) TRIM( Title         )
  WRITE( *, '(  5x, "HISTORY       : ", a )' ) TRIM( History       )
  WRITE( *, '(  5x, "SENSOR_NAME   : ", a )' ) TRIM( Sensor_Name   )
  WRITE( *, '(  5x, "PLATFORM_NAME : ", a )' ) TRIM( Platform_Name )
  WRITE( *, '(  5x, "COMMENT       : ", a )' ) TRIM( Comment       )


  ! -------------------------------
  ! Allocate the channel list array
  ! -------------------------------

  ALLOCATE( Channel_List( n_Channels ), &
            STAT = Allocate_Status )


  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error allocating CHANNEL_LIST or SRF arrays.', &
                          Error_Status )
    STOP
  END IF


  ! ---------------------
  ! Read the channel list
  ! ---------------------

  Error_Status = Inquire_SRF_netCDF( TRIM( SRF_Filename ), &
                                     Channel_List = Channel_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading CHANNEL_LIST from netCDF SRF file '//&
                          TRIM( SRF_Filename ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- CREATE A FILE FOR OUTPUT --                        #
  !#----------------------------------------------------------------------------#

  Error_Status = Create_SRF_netCDF( OUTPUT_SRF_FILENAME, &
                                    Channel_List, &
                                    NCEP_Sensor_ID   = NCEP_Sensor_ID, &
                                    WMO_Satellite_ID = WMO_Satellite_ID, &
                                    WMO_Sensor_ID    = WMO_Sensor_ID, &
                                    Title            = TRIM( Title ), &
                                    History          = PROGRAM_RCS_ID//';'//TRIM( History ), &
                                    Sensor_Name      = TRIM( Sensor_Name ), &
                                    Platform_Name    = TRIM( Platform_Name ), &
                                    Comment          = TRIM( Comment ) )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM( OUTPUT_SRF_FILENAME ), &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                         -- LOOP OVER CHANNELS --                           #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Channel",4x,"n_Points",6x,"Frequency limits" )' )

  Channel_Read_Loop: DO l = 1, n_Channels


    ! ---------------------------------
    ! Read the current SRF channel data
    ! ---------------------------------

    Error_Status = Read_SRF_netCDF( TRIM( SRF_Filename ), &  ! Input
                                    Channel_List( l ),    &  ! Input
                                    SRF                   )  ! Output

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading channel #", i5, " SRF from ", a )' ) &
                      Channel_List( l ), TRIM( SRF_Filename )
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! ----------------------------------------------
    ! Ouput some information about the SRF data read
    ! ----------------------------------------------

    WRITE( *, '( 5x, i5, 5x, i8, 5x,  "[",f9.4,",",f9.4,"]" )' ) &
              SRF%Channel, SRF%n_Points, &
              SRF%Begin_Frequency, SRF%End_Frequency

    IF ( MOD( l, 30 ) == 0 ) THEN
      WRITE( *, '( 5x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF


    ! ---------------------------------------
    ! Write the SRF to the output netCDF file
    ! ---------------------------------------

    Error_Status = Write_SRF_netCDF( OUTPUT_SRF_FILENAME, &
                                     SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing channel #", i5, " SRF to ", a )' ) &
                      Channel_List( l ), OUTPUT_SRF_FILENAME
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! ------------------------------------------------
    ! Destroy the SRF data structure for the next read
    ! ------------------------------------------------

    Error_Status = Destroy_SRF( SRF )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error occurred destroying SRF structure ", &
                        &"after channel #", i4, " read." )' ) &
                      Channel_List( l )
      CALL display_message( PROGRAM_NAME,    &
                            TRIM( Message ), &
                            Error_Status     )
      STOP
    END IF

  END DO Channel_Read_Loop



  !#----------------------------------------------------------------------------#
  !#                    -- DEALLOCATE THE CHANNEL LIST --                       #
  !#----------------------------------------------------------------------------#

  DEALLOCATE( Channel_List, &
              STAT = Allocate_Status )


  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error deallocating CHANNEL_LIST array after netCDF test read.', &
                          WARNING )
  END IF

END PROGRAM SRF_netCDF_IO_Test


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/08/15 20:51:04 $
!
! $Revision: 1.7 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_netCDF_IO_Test.f90,v $
! Revision 1.7  2006/08/15 20:51:04  wd20pd
! Additional replacement of Error_Handler with Message_Handler.
!
! Revision 1.6  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.5  2004/08/23 22:18:42  paulv
! - Upgraded to Fortran-95
! - Removed structure initialisation calls.
! - Added Create() and Write() test.
!
! Revision 1.4  2003/11/19 13:36:20  paulv
! - Updated program header documentation delimiters.
!
! Revision 1.2  2003/02/13 21:47:21  paulv
! - Increased format size of output points.
!
! Revision 1.1  2002/11/06 12:09:07  paulv
! Initial checkin.
!
!
!
!
!
