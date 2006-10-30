!------------------------------------------------------------------------------
!M+
! NAME:
!       SensorInfo_IO
!
! PURPOSE:
!       Module containing routines to read and write ASCII format SensorInfo
!       data files.
!       
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE SensorInfo_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       SensorInfo_Define:     Module defining the SensorInfo data structure
!                              and containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!
!       SensorInfo_LinkedList: Module containing definitions and manipulation
!                              routines for a SensorInfo linked list.
!                              USEs: TYPE_KINDS module
!                                    ERROR_HANDLER module
!                                    SENSORINFO_DEFINE module
!
! CONTAINS:
!       Read_SensorInfo:        Function to read ASCII format
!                               SensorInfo file data into a SensorInfo
!                               linked list.
!
!       Write_SensorInfo:       Function to write the data within a
!                               SensorInfo linked list to an ASCII
!                               format SensorInfo file.
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
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
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

MODULE SensorInfo_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE SensorInfo_Define
  USE SensorInfo_LinkedList


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Read_SensorInfo
  PUBLIC :: Write_SensorInfo


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: SensorInfo_IO.f90,v 3.9 2006/05/02 16:58:02 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Input data formats
  CHARACTER( * ), PUBLIC, PARAMETER ::  SENSORINFO_FORMAT = '( 1x, 2( 1x, a12 ), 1x, a20, 1x, i1, 4( 1x, i5 ) )'
  CHARACTER( * ), PUBLIC, PARAMETER :: CHANNELINFO_FORMAT = '( i5, 3x, i2, 5x, es13.6 )'


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ADJUSTL, &
            ASSOCIATED, &
            PRESENT,    &
            TRIM


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_SensorInfo
!
! PURPOSE:
!       Function to read ASCII format SensorInfo file data into a
!       SensorInfo linked list.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_SensorInfo( Filename,                 &  ! Input
!                                       SensorInfo_List,          &  ! Output
!                                       Quiet       = Quiet,      &  ! Optional input
!                                       Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of an ASCII
!                        format SensorInfo data file.
!                        UNITS:      None
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information Messages being
!                        printed to standard output (or the Message log file if
!                        the Message_Log optional argument is used.) By default,
!                        information Messages are printed.
!                        If QUIET = 0, information Messages are OUTPUT.
!                           QUIET = 1, information Messages are SUPPRESSED.
!                        UNITS:      None
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a Filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      None
!                        TYPE:       CHARACTER
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       SensorInfo_List: Linked list containing the SensorInfo data. Each list
!                        node corresponds to a SensorInfo file entry.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the SensorInfo data read was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       New_SensorInfo_List:     Function to create a new, initialised
!                                SensorInfo linked list.
!                                SOURCE: SENSORINFO_LINKEDLIST module
!
!       AddTo_SensorInfo_List:   Function to add a node to a SensorInfo
!                                linked list.
!                                SOURCE: SENSORINFO_LINKEDLIST module
!
!       Destroy_SensorInfo:      Function to destroy a SensorInfo
!                                structure.
!                                SOURCE: SENSORINFO_DEFINE module
!
!       File_Exists:             Function to determine if a data file
!                                exists.
!                                SOURCE: FILE_UTILITY module
!
!       Get_Lun:                 Function to obtain a free logical
!                                unit number for file access.
!                                SOURCE: FILE_UTILITY module
!
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output SensorInfo_List argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_SensorInfo( Filename,        &  ! Input
                            SensorInfo_List, &  ! Output
                            Quiet,           &  ! Optional input
                            Message_Log )    &  ! Error messaging
                          RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN )     :: Filename

    ! -- Output
    TYPE( SensorInfo_List_type ), INTENT( IN OUT ) :: SensorInfo_List

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN )     :: Quiet

    ! -- Error Message log file
    CHARACTER( * ),     OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_SensorInfo'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status

    INTEGER :: FileID

    CHARACTER( 256 ) :: Line_Buffer

    INTEGER :: n_Channels, l
    INTEGER :: n_Sensors

    TYPE( SensorInfo_type ) :: SensorInfo



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------
    ! Check keywords
    ! --------------

    ! -- Output informational messages....
    Noisy = .TRUE.
    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- CREATE A NEW SensorInfo LINKED LIST --               #
    !#--------------------------------------------------------------------------#

    Error_Status = Destroy_SensorInfo_List( SensorInfo_List, &
                                            Quiet = Quiet, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying SensorInfo_List.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    SensorInfo_List = New_SensorInfo_List( Message_Log = Message_Log )



    !#--------------------------------------------------------------------------#
    !#                    -- OPEN THE SensorInfo DATA FILE --                   #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------
    ! Open the SensorInfo file
    ! ------------------------

    OPEN( FileID, FILE   = TRIM( ADJUSTL( Filename ) ), &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- LOOP OVER COMMENT LINES IN SensorInfo FILE --             #
    !#--------------------------------------------------------------------------#

    Comment_Read_loop: DO

      ! -- Read a line of the file
      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Line_Buffer

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading SensorInfo file in comment skip. IOSTAT = ", i5 )' ) &
                        IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Exit loop if this is NOT a comment or blank line
      IF ( Line_Buffer(1:1) /= '!' .AND. &
           LEN_TRIM( Line_Buffer ) /= 0 ) THEN
        BACKSPACE( FileID )
        EXIT Comment_Read_loop
      END IF

    END DO Comment_Read_loop



    !#--------------------------------------------------------------------------#
    !#                    -- READ THE SensorInfo FILE DATA --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Initialise sensor counter
    ! -------------------------

    n_Sensors = 0


    ! ----------------------------
    ! Begin open loop over sensors
    ! ----------------------------

    SensorInfo_Read_loop: DO


      ! -----------------------------------------------
      ! Read a line of the file into a character buffer
      ! -----------------------------------------------

      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Line_Buffer

      ! -- End of file?
      IF ( IO_Status < 0 ) EXIT SensorInfo_Read_Loop

      ! -- Read error
      IF ( IO_Status > 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading SensorInfo file in sensor header read. ",&
                          &"Sensors already read = ", i4, ". IOSTAT = ", i5 )' ) &
                        n_Sensors, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Cycle loop if this is a blank line
      IF ( LEN_TRIM( Line_Buffer ) == 0 ) CYCLE SensorInfo_Read_Loop


      ! ------------------------
      ! Increment sensor counter
      ! ------------------------

      n_Sensors = n_Sensors + 1


      ! --------------------------------------------
      ! Read the SensorInfo data line into variables
      ! --------------------------------------------

      READ( Line_Buffer, FMT    = SENSORINFO_FORMAT, &
                         IOSTAT = IO_Status ) SensorInfo%Sensor_Name, &
                                              SensorInfo%Satellite_Name, &
                                              SensorInfo%File_Prefix, &
                                              SensorInfo%Microwave_Flag, &
                                              SensorInfo%NCEP_Sensor_ID, &
                                              SensorInfo%WMO_Sensor_ID, &
                                              SensorInfo%WMO_Satellite_ID, &
                                              n_Channels 


      ! ----------------------------------------------------
      ! Allocate the SensorInfo structure pointer components
      ! ----------------------------------------------------

      Error_Status = Allocate_SensorInfo( n_Channels, &
                                          SensorInfo, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error allocating SensorInfo structure for '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! ----------------------
      ! Output an info message
      ! ----------------------

      IF ( Noisy ) THEN
        WRITE( Message, '( "SENSOR/PLATFORM: ", a, 1x, a, ", N_CHANNELS=",i4 )' ) &
                        TRIM( SensorInfo%Sensor_Name ), &
                        TRIM( SensorInfo%Satellite_Name ), &
                        SensorInfo%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              INFORMATION, &
                              Message_Log = Message_Log )
      END IF


      ! ----------------------------
      ! Read the channel information
      ! ----------------------------

      ChannelInfo_Read_loop: DO l = 1, n_Channels

        READ( FileID, FMT    = CHANNELINFO_FORMAT, &
                      IOSTAT = IO_Status ) SensorInfo%Sensor_Channel(l), &
                                           SensorInfo%Use_Flag(l), &
                                           SensorInfo%Noise(l)

        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error reading ChannelInfo data for ", a, 1x, a, &
                            &", channel # ", i4, ". IOSTAT = ", i5 )' ) &
                          TRIM( SensorInfo%Sensor_Name ), &
                          TRIM( SensorInfo%Satellite_Name ), &
                          l, IO_Status
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN
        END IF

      END DO ChannelInfo_Read_loop


      ! ------------------------------------------------
      ! Add the current SensorInfo structure to the list
      ! ------------------------------------------------

      Error_Status = AddTo_SensorInfo_List( SensorInfo, &
                                            SensorInfo_List, &
                                            Node_Number = n_Sensors, &
                                            Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error adding '//&
                              TRIM( SensorInfo%Satellite_Name )//' '//&
                              TRIM( SensorInfo%Sensor_Name )//' to SensorInfo list.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! --------------------------------------------------
      ! Destroy the SensorInfo structure for the next read
      ! --------------------------------------------------

      Error_Status = Destroy_SensorInfo( SensorInfo, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error destroying SensorInfo structures at sensor # ", i5 )' ) &
                        n_Sensors
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO SensorInfo_Read_loop



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO MESSAGE --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( "FILE: ", a, ", N_SENSORS=",i4 )' ) &
                      TRIM( Filename ), &
                      n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_SensorInfo




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_SensorInfo
!
! PURPOSE:
!       Function to write the data within a SensorInfo linked list to an
!       ASCII format SensorInfo file.
!
! CATEGORY:
!       Instrument Information : SensorInfo
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_SensorInfo( Filename,                 &  ! Input
!                                        SensorInfo_List,          &  ! Input
!                                        Quiet       = Quiet,      &  ! Optional input
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:        Character string specifying the name of an output
!                        SensorInfo data file.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN )
!
!       SensorInfo_List: Linked list containing the SensorInfo data to write.
!                        Each list node corresponds to a SensorInfo file entry.
!                        UNITS:      N/A
!                        TYPE:       SensorInfo_List_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information Messages being
!                        printed to standard output (or the Message log file if
!                        the Message_Log optional argument is used.) By default,
!                        information Messages are printed.
!                        If QUIET = 0, information Messages are OUTPUT.
!                           QUIET = 1, information Messages are SUPPRESSED.
!                        UNITS:      None
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:     Character string specifying a Filename in which any
!                        Messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output Messages to standard output.
!                        UNITS:      None
!                        TYPE:       Character
!                        DIMENSION:  Scalar, LEN = *
!                        ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the ERROR_HANDLER module.
!                        If == SUCCESS the SensorInfo data write was successful
!                           == FAILURE an unrecoverable error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       Destroy_SensorInfo:      Function to destroy a SensorInfo
!                                structure.
!                                SOURCE: SENSORINFO_DEFINE module
!
!       GetFrom_SensorInfo_List: Function to retrieve a node from a
!                                SensorInfo linked list
!                                SOURCE: SENSORINFO_LINKEDLIST module
!
!       Count_SensorInfo_Nodes:  Function to count the number of nodes
!                                in a SensorInfo linked list.
!                                SOURCE: SENSORINFO_LINKEDLIST module
!
!       File_Exists:             Function to determine if a data file
!                                exists.
!                                SOURCE: FILE_UTILITY module
!
!       Get_Lun:                 Function to obtain a free logical
!                                unit number for file access.
!                                SOURCE: FILE_UTILITY module
!
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs in this routine, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       This function checks the association status of the SensorInfo linked
!       list nodes. Therefore, this function should *only* be called
!       *after* the SensorInfo linked list has been filled with data.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_SensorInfo( Filename,        &  ! Input
                             SensorInfo_List, &  ! Input
                             Quiet,           &  ! Optional input
                             Message_Log )    &  ! Error messaging
                           RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),               INTENT( IN ) :: Filename
    TYPE( SensorInfo_List_type ), INTENT( IN ) :: SensorInfo_List

    ! -- Optional input
    INTEGER,            OPTIONAL, INTENT( IN ) :: Quiet

    ! -- Error Message log file
    CHARACTER( * ),     OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_SensorInfo'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status

    INTEGER :: FileID
    INTEGER :: l
    INTEGER :: n_Sensors, n

    TYPE( SensorInfo_type ) :: SensorInfo



    !#--------------------------------------------------------------------------#
    !#                             -- CHECK INPUT --                            #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( File_Exists( TRIM( Filename ) ) ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' will be overwritten.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------------
    ! Check keywords
    ! --------------

    ! -- Output informational Messages....
    Noisy = .TRUE.
    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE SensorInfo DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------
    ! Open it
    ! -------

    OPEN( FileID, FILE   = Filename, &
                  STATUS = 'REPLACE', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'WRITE', &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- DETERMINE THE NUMBER OF SENSORS --                  #
    !#--------------------------------------------------------------------------#

    n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )

    IF ( n_Sensors < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'SensorInfo list is empty', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = 'DELETE' )
      RETURN
    END IF


    
    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE SensorInfo DATA --                       #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! Loop over the number of sensors
    ! -------------------------------

    SensorInfo_Write_loop: DO n = 1, n_Sensors


      ! -----------------------------------------
      ! Get the current sensor data from the list
      ! -----------------------------------------

      Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                              n, &
                                              SensorInfo, &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = 'DELETE' )
        RETURN
      END IF


      ! -------------------------
      ! Write the SensorInfo data
      ! -------------------------

      WRITE( FileID, FMT    = SENSORINFO_FORMAT, &
                     IOSTAT = IO_Status ) SensorInfo%Sensor_Name, &
                                          SensorInfo%Satellite_Name, &
                                          SensorInfo%File_Prefix, &
                                          SensorInfo%Microwave_Flag, &
                                          SensorInfo%NCEP_Sensor_ID, &
                                          SensorInfo%WMO_Sensor_ID, &
                                          SensorInfo%WMO_Satellite_ID, &
                                          SensorInfo%n_Channels

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing SensorInfo data for sensor # ", i5, &
                          &". IOSTAT = ", i5 )' ) n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = 'DELETE' )
        RETURN
      END IF


      ! ----------------------
      ! Output an info message
      ! ----------------------

      IF ( Noisy ) THEN
        WRITE( Message, '( "SENSOR/PLATFORM: ", a, 1x, a, ", N_CHANNELS=",i4 )' ) &
                        TRIM( SensorInfo%Sensor_Name ), &
                        TRIM( SensorInfo%Satellite_Name ), &
                        SensorInfo%n_Channels
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              INFORMATION, &
                              Message_Log = Message_Log )
      END IF


      ! --------------------------
      ! Write the ChannelInfo data
      ! --------------------------

      ChannelInfo_Write_loop: DO l = 1, SensorInfo%n_Channels

        WRITE( FileID, FMT    = CHANNELINFO_FORMAT, &
                       IOSTAT = IO_Status ) SensorInfo%Sensor_Channel(l), &
                                            SensorInfo%Use_Flag(l), &
                                            SensorInfo%Noise(l)

        IF ( IO_Status /= 0 ) THEN
          Error_Status = FAILURE
          WRITE( Message, '( "Error writing ChannelInfo data for ", a, 1x, a, &
                            &", channel # ", i4, ". IOSTAT = ", i5 )' ) &
                          TRIM( SensorInfo%Sensor_Name ), &
                          TRIM( SensorInfo%Satellite_Name ), &
                          l, IO_Status
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( Message ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          CLOSE( FileID )
          RETURN
        END IF

      END DO ChannelInfo_Write_loop


      ! --------------------------------------------------
      ! Destroy the SensorInfo structure for the next node
      ! --------------------------------------------------

      Error_Status = Destroy_SensorInfo( SensorInfo, &
                                         Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error destroying SensorInfo structure at sensor # ", i5 )' ) &
                        n_Sensors
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO SensorInfo_Write_loop



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      WRITE( Message, '( "FILE: ", a, ", N_SENSORS=",i4 )' ) &
                      TRIM( Filename ), n_Sensors
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- CLOSE THE FILE --                             #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                    IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- SUCCESSFUL COMPLETION --                         #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS

  END FUNCTION Write_SensorInfo

END MODULE SensorInfo_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: SensorInfo_IO.f90,v 3.9 2006/05/02 16:58:02 dgroff Exp $
!
! $Date: 2006/05/02 16:58:02 $
!
! $Revision: 3.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SensorInfo_IO.f90,v $
! Revision 3.9  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 3.8  2004/09/03 16:28:42  paulv
! - Minor header documentation update.
!
! Revision 3.7  2004/08/18 15:48:15  paulv
! - Upgraded to Fortran95.
! - Using new SensorInfo_Define and SensorInfo_LinkedList modules.
! - Replaced Init_SensorInfo_List() call in Read_SensorInfo() function with
!   New_SensorInfo_List() call, preceded by a Destroy_SensorInfo_List() call.
! - Modified Read_SensorInfo() function to also ignore blank lines when
!   reading form the SensorInfo file.
! - Removed SensorInfo initialisation calls.
!
! Revision 3.6  2004/07/19 19:32:11  paulv
! - Updated the document headers.
!
! Revision 3.5  2003/07/24 18:58:50  paulv
! - Updated some header documentation.
! - Altered ChannelInfo format to allow for -ve values of the Use_Flag to
!   be read/written.
!
! Revision 3.4  2003/06/16 19:17:51  paulv
! - Altered SensorInfo line format to read in the new microwave flag.
! - Added SensorInfo%Microwave_Flag to the READ/WRITE lists in the respective
!   functions.
!
! Revision 3.3  2003/06/04 15:23:44  paulv
! - Removed all No_ChannelInfo optional arguments from calls to routines from
!   SensorInfo_Define() module. Channel information is now read in by default.
! - Removed all references to the ChannelInfo structure and replaced them with
!   the requisite reference within the SensorInfo structure.
! - Added Allocate_SensorInfo() call to the Read_SensorInfo() function. This
!   allocates the local SensorInfo structure prior to reading the channel
!   information.
!
! Revision 3.2  2003/05/23 15:43:38  paulv
! - Corrected bug in n_Sensor increment in READ() function.
!
! Revision 3.1  2003/05/21 18:59:45  paulv
! - Updated documentation.
!
! Revision 3.0  2003/04/16 21:12:01  paulv
! - New version for use with the SensorInfo_LinkedList module.
!
! Revision 2.3  2003/04/16 16:43:51  paulv
! - Removed the need to have the N_SENSORS field immediately after the
!   comment haeder.
!
! Revision 2.2  2003/04/11 16:32:41  paulv
! - Updated INTRINSIC list.
!
! Revision 2.1  2003/02/28 19:38:20  paulv
! - Renamed loop in read function.
! - Added No_ChannelInfo optional argument to Allocate_SensorInfo() call in
!   the Read_SensorInfo() function.
!
! Revision 2.0  2003/02/07 21:34:15  paulv
! - New version to read new format SensorInfo data files with or without
!   ChannelInfo data.
!
! Revision 1.1  2002/08/09 18:30:17  paulv
! Initial checkin.
!
!
!
!
