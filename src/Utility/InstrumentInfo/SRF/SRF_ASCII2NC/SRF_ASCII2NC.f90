!------------------------------------------------------------------------------
!P+
! NAME:
!       SRF_ASCII2NC
!
! PURPOSE:
!       Program to read the ASCII format SRF data files and write netCDF format
!       SRF data files.
!
! CATEGORY:
!       Instrument Information : SRF
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:             Module containing definitions for kinds
!                               of variable types.
!
!       File_Utility:           Module containing generic file utility routines
!
!       Message_Handler:        Module to define simple error codes and
!                               handle error conditions
!                               USEs: FILE_UTILITY module
!
!       SensorInfo_Define:      Module defining the SensorInfo data structure
!                               and containing routines to manipulate it.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SensorInfo_LinkedList:  Module defining the SensorInfo Linked List
!                               data structure and containing routines to
!                               manipulate it.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SENSORINFO_DEFINE module
!
!       SensorInfo_IO:          Module continaing routines to read and write
!                               ASCII SensorInfo format files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     SensorInfo_DEFINE module
!
!       SRF_Define:             Module defining the generic SRF data structure
!                               and its manipulation routines.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!
!       SRF_ASCII_IO:           Module containing routines to read and write
!                               ASCII format SRF files.
!                               USEs: TYPE_KINDS module
!                                     FILE_UTILITY module
!                                     ERROR_HANDLER module
!                                     STRING_PROCESSING module
!                                     SRF_DEFINE module
!
!       SRF_netCDF_IO:          Module containing routines to read and write
!                               netCDF format SRF files.
!                               USEs: TYPE_KINDS module
!                                     ERROR_HANDLER module
!                                     SRF_DEFINE module
!                                     NETCDF module
!                                     NETCDF_UTILITY module
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
!       Input: 
!         - User specified SensorInfo data file.
!         - ASCII format SRF data file(s) specified from SensorInfo entries.
!
!       Output:
!         - netCDF format SRF data file(s)
!
! SIDE EFFECTS:
!       If the netCDF format SRF file already exists, it is overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jan-2002
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

PROGRAM SRF_ASCII2NC


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO

  USE SRF_Define
  USE SRF_ASCII_IO
  USE SRF_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SRF_ASCII2NC'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: SRF_ASCII2NC.f90,v 1.5 2006/05/02 16:58:02 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! -- Optional boolean argument set value
  INTEGER, PARAMETER :: SET = 1


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  CHARACTER( 256 ) :: SensorInfo_Filename
  CHARACTER( 256 ) :: ASCII_SRF_Filename
  CHARACTER( 256 ) ::    NC_SRF_Filename
  INTEGER          :: ASCII_SRF_FileID

  INTEGER :: Error_Status

  INTEGER                                  :: n_Channels, l
  INTEGER, DIMENSION( MAX_N_SRF_CHANNELS ) :: Channel_List
  CHARACTER( 256 )                         :: Title
  CHARACTER( 256 )                         :: History
  CHARACTER( 256 )                         :: Sensor_Name
  CHARACTER( 256 )                         :: Platform_Name
  CHARACTER( 256 )                         :: Comment

  TYPE( SRF_type ) :: SRF

  INTEGER :: n_Sensors, n
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List


  !#----------------------------------------------------------------------------#
  !#                      -- INITIALIZE THE LINKED LIST --                      #
  !#----------------------------------------------------------------------------#

  SensorInfo_List = New_SensorInfo_List()



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read ASCII format SRF data files and write    ")' )
  WRITE( *, '( 5x, "   netCDF format SRF data files.                          ")' )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                         -- GET USER INPUTS --                              #
  !#----------------------------------------------------------------------------#

  ! ---------------------------
  ! Get the SensorInfo filename
  ! ---------------------------

  WRITE( *, FMT     = '( /5x, "Enter a SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename

  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = SET )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//&
                          TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF

  ! -- Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                      -- BEGIN LOOP OVER SENSORS --                         #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, n_Sensors



    !#--------------------------------------------------------------------------#
    !#          -- GET THE CURRENT SensorInfo DATA FROM THE LIST --             #
    !#--------------------------------------------------------------------------#

    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! ---------------------------
    ! Construct the SRF filenames
    ! ---------------------------

    ASCII_SRF_Filename = TRIM( SensorInfo%File_Prefix )//'.srf'
    NC_SRF_Filename    = TRIM( ASCII_SRF_Filename )//'.nc'



    !#--------------------------------------------------------------------------#
    !#                   -- OPERATE ONLY ON FILES THAT EXIST --                 #
    !#--------------------------------------------------------------------------#

    Available_Sensors: IF ( File_Exists( TRIM( ASCII_SRF_Filename ) ) ) THEN


      WRITE( *, '( 5x, "Processing SRF file ", a, "..." )' ) TRIM( ASCII_SRF_Filename )


      ! ------------------------------
      ! Read the ASCII SRF file header
      ! ------------------------------

      Error_Status = Read_SRF_ASCII_Header( TRIM( ASCII_SRF_Filename ), &
                                            ASCII_SRF_FileID, &
                                            n_Channels,    &
                                            Channel_List,  &
                                            Title,         &
                                            History,       &
                                            Sensor_Name,   &
                                            Platform_Name, &
                                            Comment        )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading header from '//TRIM( ASCII_SRF_Filename ), &
                              FAILURE )
        STOP
      END IF


      ! ------------------------------------------
      ! Double check the SENSOR and PLATFORM names
      ! ------------------------------------------

      IF ( TRIM( Sensor_Name ) /= TRIM( SensorInfo%Sensor_Name ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'ASCII SRF sensor name, '//&
                              TRIM( Sensor_Name )//&
                              ', different from SensorInfo entry, '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              WARNING )
      END IF
           
      IF ( TRIM( Platform_Name ) /= TRIM( SensorInfo%Satellite_Name ) ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'ASCII SRF platform name, '//&
                              TRIM( Platform_Name )//&
                              ', different from SensorInfo entry, '//&
                              TRIM( SensorInfo%Satellite_Name ), &
                              WARNING )
      END IF


      ! -----------------------------
      ! Create the output netCDF file
      ! -----------------------------

      Error_Status = Create_SRF_netCDF( TRIM( NC_SRF_Filename ), &
                                        Channel_List(1:n_Channels), &
                                        NCEP_Sensor_ID   = SensorInfo%NCEP_Sensor_ID, &
                                        WMO_Satellite_ID = SensorInfo%WMO_Satellite_ID, &
                                        WMO_Sensor_ID    = SensorInfo%WMO_Sensor_ID, &
                                        Title            = TRIM( Title ), &
                                        History          = PROGRAM_RCS_ID//';'//TRIM( History ), &
                                        Sensor_Name      = TRIM( Sensor_Name ), &
                                        Platform_Name    = TRIM( Platform_Name ), &
                                        Comment          = TRIM( Comment ) )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating '//TRIM( NC_SRF_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! -----------------------------------
      ! Begin read/write loop over channels
      ! -----------------------------------

      Channel_Loop: DO l = 1, n_Channels


        ! ------------------------
        ! Read SRF from ASCII file
        ! ------------------------

        Error_Status = Read_SRF_ASCII( TRIM( ASCII_SRF_Filename ), &
                                       ASCII_SRF_FileID, &
                                       Channel_List(l), &
                                       SRF, &
                                       Quiet = SET )
 
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading SRF channel ", i5, " from ", a )' ) &
                          Channel_List(l), TRIM( ASCII_SRF_Filename )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                FAILURE )
          STOP
        END IF


        ! -----------------------------------------
        ! Load sensor IDs. These don't change for
        ! different channels in a file, but the SRF
        ! allocation clears the structure and the
        ! netCDF write double checks these values
        ! against the file contents.
        ! -----------------------------------------

        SRF%NCEP_Sensor_Id   = SensorInfo%NCEP_Sensor_ID
        SRF%WMO_Satellite_Id = SensorInfo%WMO_Satellite_ID
        SRF%WMO_Sensor_Id    = SensorInfo%WMO_Sensor_ID
        

        ! --------------------------------
        ! Write the SRF to the netCDF file
        ! --------------------------------

        Error_Status = Write_SRF_netCDF( NC_SRF_Filename, &
                                         SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred writing channel #", i4, " SRF to ", a, "." )' ) &
                          Channel_List(l), TRIM( NC_SRF_Filename )
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                Error_Status     )
          STOP
        END IF


        ! --------------------------------------
        ! Destroy the SRF structure for next use
        ! --------------------------------------

        Error_Status = Destroy_SRF( SRF )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error occurred destroying SRF structure ", &
                            &"for channel #", i4, " read." )' ) &
                          Channel_List(l)
          CALL Display_Message( PROGRAM_NAME,    &
                                TRIM( Message ), &
                                Error_Status     )
          STOP
        END IF

      END DO Channel_Loop


      ! ------------------------------
      ! Close the input ASCII SRF file
      ! ------------------------------

      CLOSE( ASCII_SRF_FileID )

    END IF Available_Sensors



    !#--------------------------------------------------------------------------#
    !#              -- DESTROY THE CURRENT SensorInfo STRUCTURE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying SensorInfo linked list.', &
                          WARNING )
  END IF

END PROGRAM SRF_ASCII2NC


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SRF_ASCII2NC.f90,v 1.5 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_ASCII2NC.f90,v $
! Revision 1.5  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.4  2004/08/23 20:27:39  paulv
! - Converted to Fortran-95
! - Removed derived type initialisation calls.
! - New SensorInfo and SRF modules used.
!
! Revision 1.3  2003/11/19 14:00:25  paulv
! - Updated program header documentation delimiters.
! - Updated program header documentation.
!
! Revision 1.2  2003/11/18 15:50:15  paulv
! - Updated Category listing from SRF to Instrument Information : SRF.
!
! Revision 1.1  2003/08/29 18:31:34  paulv
! Initial checkin.
!
!
!
!
