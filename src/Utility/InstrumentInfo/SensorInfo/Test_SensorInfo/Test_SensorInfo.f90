!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_SensorInfo
!
! PURPOSE:
!       Program to test the SensorInfo linked list and I/O routines and
!       demonstrate how to use them to read a SensorInfo file, extract
!       data from the resulting linked list, and write a SensorInfo file.
!
! CATEGORY:
!       Instrument_Information : SensorInfo
!
! LANGUAGE:
!       Fortran-90
!
! MODULES:
!       Type_Kinds:                 Module containing definitions for kinds
!                                   of variable types.
!
!       Message_Handler:            Module to define simple error codes and
!                                   handle error conditions
!                                   USEs: FILE_UTILITY module
!
!       SensorInfo_Define:          Module defining the SensorInfo data structure and
!                                   containing routines to manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!
!       SensorInfo_LinkedList:      Module defining the SensorInfo Linked List
!                                   data structure and containing routines to
!                                   manipulate it.
!                                   USEs: TYPE_KINDS module
!                                         ERROR_HANDLER module
!                                         SENSORINFO_DEFINE module
!
!       SensorInfo_IO:              Module continaing routines to read and write ASCII
!                                   SensorInfo format files.
!                                   USEs: TYPE_KINDS module
!                                         FILE_UTILITY module
!                                         ERROR_HANDLER module
!                                         SENSORINFO_DEFINE module
!
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
!       Input:  ASCII SensorInfo file
!
!       Output: ASCII SensorInfo file (different from input)
!
! SIDE EFFECTS:
!       If the output file exists it is overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 28-Feb-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
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

PROGRAM Test_SensorInfo


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------
 
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_SensorInfo'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Test_SensorInfo.f90,v 2.6 2006/05/02 16:58:02 dgroff Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: MAX_N_READS = 10000


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  INTEGER :: n_Sensors, n

  CHARACTER( 256 )             :: SensorInfo_FileNAME
  TYPE( SensorInfo_type )      :: SensorInfo
  TYPE( SensorInfo_List_type ) :: SensorInfo_List



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the SensorInfo linked list and I/O ")' )
  WRITE( *, '( 5x, "   routines ")' )
  WRITE( *, '(/5x, " $Revision: 2.6 $")' )
  WRITE( *, '(/5x, a, / )' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                       -- READ THE SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test reading the input SensorInfo file..." )' )

  SensorInfo_FileNAME = 'SensorInfo.input_test'

  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
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

  WRITE( *, '( 5x, "Number of sensors in list: ", i5 )' ) n_Sensors



  !#----------------------------------------------------------------------------#
  !#                    -- LOOP OVER ALL SENSORS IN THE LIST --                 #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Looping over SensorInfo list entries to extract data....", / )' )

  WRITE( *, '("                                                 +--------------------------- Microwave flag" )' )
  WRITE( *, '("    +------------------- Sensor name             |                            (0==IR, 1==MW)" )' )
  WRITE( *, '("    |            +------ Satellite/platform name |     +--------------------- NCEP sensor ID" )' )
  WRITE( *, '("    |            |  +--- File prefix             |     |    +---------------- WMO sensor ID" )' )
  WRITE( *, '("    |            |  |                            |     |    |     +---------- WMO satellite ID" )' )
  WRITE( *, '("    |            |  +-------------+              |     |    |     |      +--- No. of channels" )' )
  WRITE( *, '("    |            |                |              |     |    |     |      |" )' )


  n_Sensor_loop: DO n = 1, n_Sensors


    ! ---------------------------------------------
    ! Get the current SensorInfo node from the list
    ! ---------------------------------------------

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


    ! ---------------------------------------
    ! Output some data for the current sensor
    ! ---------------------------------------

    WRITE( *, '( 1x, 2( 1x, a12 ), 1x, a20, 1x, i1, 4( 1x, i5 ) )' ) &
              SensorInfo%Sensor_Name, &
              SensorInfo%Satellite_Name, &
              SensorInfo%File_Prefix, &
              SensorInfo%Microwave_Flag, &
              SensorInfo%NCEP_Sensor_ID, &
              SensorInfo%WMO_Satellite_ID, &
              SensorInfo%WMO_Sensor_ID, &
              SensorInfo%n_Channels


    ! ---------------------------------------
    ! Destroy the current SensorInfo node in
    ! preparation for the next node retrieval
    ! ---------------------------------------

    Error_Status = Destroy_SensorInfo( SensorInfo )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF


    ! --------------------------
    ! Pause to have a lookee....
    ! --------------------------

    IF ( MOD( n, 25 ) == 0 ) THEN
      WRITE( *, '( 10x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF

  END DO n_Sensor_loop



  !#----------------------------------------------------------------------------#
  !#                        -- WRITE A SensorInfo FILE --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Test writing an output SensorInfo file..." )' )

  SensorInfo_FileNAME = 'SensorInfo.output_test'

  Error_Status = Write_SensorInfo( TRIM( SensorInfo_Filename ), &
                                   SensorInfo_List, &
                                   Quiet = 1 )
                               
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                  -- DESTROY THE SensorInfo LINKED LIST --                  #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          Error_Status )
  END IF



  !#----------------------------------------------------------------------------#
  !#          -- LOOP OVER READ FUNCTION TO ACCUMULATE MEMORY LEAKS --          #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Looping over read function for memory leak test (run top)..." )' )

  SensorInfo_FileNAME = 'SensorInfo.input_test'


  DO n = 1, MAX_N_READS

    Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                    SensorInfo_List, &
                                    Quiet = 1 )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                            FAILURE )
      STOP
    END IF

    IF ( MOD( n, 1000 ) == 0 ) THEN
      WRITE( Message, '( "Number of reads performed: ", i5, " of ", i5 )' ) n, MAX_N_READS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF

  END DO

END PROGRAM Test_SensorInfo


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_SensorInfo.f90,v 2.6 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 2.6 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_SensorInfo.f90,v $
! Revision 2.6  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 2.5  2004/08/18 15:29:47  paulv
! - Added a read loop to test for memory leaks.
!
! Revision 2.4  2004/08/18 14:28:40  paulv
! - Upgrade to Fortran95.
! - Using updated SensorInfo modules. Initialisation routines removed.
!
! Revision 2.3  2003/11/20 15:51:58  paulv
! - Updated program header documentation delimiters.
!
! Revision 2.2  2003/11/20 15:50:43  paulv
! - Added list access test code.
! - Added QUIET optional argument to read and write function calls.
!
! Revision 2.1  2003/06/04 16:30:31  paulv
! - Removed un-needed module definitions.
!
! Revision 2.0  2003/05/23 15:47:40  paulv
! - New version for linked list test.
!
!
!
