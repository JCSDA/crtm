
!  SatInfo_IO module
!
!  Module containing routines to read and write ASCII SatInfo files.
!       
!  Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                  paul.vandelst@ssec.wisc.edu
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

MODULE SatInfo_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE SatInfo_Define
  ! Disable implicit typing
  IMPLICIT NONE


  ! -------------------
  ! Module visibilities
  ! -------------------
  PRIVATE
  ! Derived type defintion from SatInfo_Define
  PUBLIC :: SatInfo_type
  ! Module procedures from SatInfo_Define
  PUBLIC :: SatInfo_CountSensors
  PUBLIC :: SatInfo_IndexChannels
  PUBLIC :: SatInfo_UniqueSensors
  ! Module procedure
  PUBLIC :: Read_SatInfo
  PUBLIC :: Write_SatInfo


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Write_SatInfo
    MODULE PROCEDURE Write_SatInfo_by_FileID
    MODULE PROCEDURE Write_SatInfo_by_Filename
  END INTERFACE Write_SatInfo


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Read and write format
  CHARACTER(*), PARAMETER :: SATINFO_FMTSTRING = &
    '( 1x, a18, 2i5, 2f7.3, f7.2, f8.3 )'
  ! Header for output
  CHARACTER(*), PARAMETER :: SATINFO_HEADER = &
    '!sensor/instr/sat   chan iuse  error  ermax  var_b  var_pg'

CONTAINS


! NAME:
!   Read_SatInfo
!
! PURPOSE:
!   Function to read ASCII SatInfo files.
!
! CALLING SEQUENCE:
!   Error_Status = Read_SatInfo( Filename,                 &  ! Input
!                                SatInfo,                  &  ! Output
!                                RCS_Id      = RCS_Id,     &  ! Revision control
!                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:         Character string specifying the name of a
!                     SatInfo data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:      Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!   SatInfo:          Data structure array to hold the contents of the
!                     SatInfo data file.
!                     UNITS:      N/A
!                     TYPE:       SatInfo_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:           Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:     The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the SatInfo file read was successful.
!                        == FAILURE an unrecoverable error occurred.
!
! SIDE EFFECTS:
!   None.
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                   paul.vandelst@ssec.wisc.edu

  FUNCTION Read_SatInfo( Filename,     &  ! Input
                         SatInfo,      &  ! Output
                         RCS_Id,       &  ! Revision control
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                     INTENT(IN)  :: Filename
    TYPE(SatInfo_type), DIMENSION(:), INTENT(OUT) :: SatInfo
    CHARACTER(*), OPTIONAL,           INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,           INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SatInfo'
    ! Function variables
    CHARACTER(256) :: Message
    CHARACTER(200) :: Buffer
    INTEGER :: IO_Status
    INTEGER :: nLines, nEntries
    INTEGER :: maxEntries
    INTEGER :: FileID


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Does the input file exist?
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'SatInfo file '//TRIM( Filename )//' not found.'
      GOTO 2000
    END IF

    ! Get the maximum number of channel
    ! entries that can be read
    maxEntries = SIZE( SatInfo )
    IF ( maxEntries < 1 ) THEN
      Message = 'Output SatInfo structure array has zero size'
      GOTO 2000
    END IF

    ! Get an available file unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Message = 'Error obtaining unit number for SatInfo file '//TRIM( Filename )
      GOTO 2000
    END IF

    ! Open the file
    OPEN( FileID, FILE   = TRIM( Filename ), &
                  STATUS = 'OLD',           &
                  ACCESS = 'SEQUENTIAL',    &
                  FORM   = 'FORMATTED',     &
                  ACTION = 'READ',          &
                  IOSTAT = IO_Status        )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening SatInfo file ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 2000
    END IF

    ! Initialise counters
    nLines   = 0
    nEntries = 0

    ! Begin open read loop
    Read_Loop: DO

      ! Read a line into a buffer
      nLines = nLines+1
      READ( FileID, FMT='(a)', IOSTAT=IO_Status ) Buffer

      ! Check for EOF
      IF ( IO_Status < 0 ) EXIT Read_Loop

      ! Check for error
      IF ( IO_Status > 0 ) THEN
        WRITE( Message, '( "Error reading SatInfo file at line #", i5, ". IOSTAT = ", i5 )' ) &
                        nLines, IO_Status
        GOTO 1000
      END IF

      ! Cycle loop if buffer is a comment or blank
      IF ( Buffer(1:1) == '!' .OR. LEN_TRIM(Buffer) == 0 ) CYCLE Read_Loop

      ! Increment entry counter and check
      nEntries = nEntries+1
      IF ( nEntries > maxEntries ) THEN
        WRITE( Message, '( "Size of output SatInfo array, ", i5, &
                          &", is too small to hold all file entries" )' ) &
                        maxEntries
        GOTO 1000
      END IF

      ! Read data from buffer line
      READ( Buffer, FMT    = SATINFO_FMTSTRING, &
                    IOSTAT = IO_Status          ) &
        SatInfo(nEntries)%Sensor_Id, &
        SatInfo(nEntries)%Channel, &
        SatInfo(nEntries)%Use_Flag, &
        SatInfo(nEntries)%Error, &
        SatInfo(nEntries)%Error_Max, &
        SatInfo(nEntries)%Var_b, &
        SatInfo(nEntries)%Var_pg        

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SatInfo file ", a, " entry # ", i5, &
                          &". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), nEntries, IO_Status
        GOTO 1000
      END IF

    END DO Read_Loop

    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Read_SatInfo



! NAME:
!   Write_SatInfo
!
! PURPOSE:
!   Function to write SatInfo data to file
!
! CALLING SEQUENCE:
!   Error_Status = Write_SatInfo( Filename | FileID,        &  ! Input
!                                 SatInfo,                  &  ! Input
!                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!   Filename:          Character string specifying the name of the 
!                      ASCII SatInfo file to write. If the file is
!                      already open, it is written to. If the file
!                      is not open, it is opened. The file is closed
!                      upon successful exit.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!     OR
!   FileID:            Unit number of file to write to. The file must
!                      already be open. The file is NOT closed upon exit.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!   SatInfo:           Data structure array containing the SatInfo
!                      data to write to file.
!                      UNITS:      N/A
!                      TYPE:       SatInfo_type
!                      DIMENSION:  Rank-1
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!   Message_Log:       Character string specifying a filename in which any
!                      messages will be logged. If not specified, or if an
!                      error occurs opening the log file, the default action
!                      is to output messages to standard output.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!   RCS_Id:            Character string containing the Revision Control
!                      System Id field for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!   Error_Status:      The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the SatInfo file write was successful
!                         == FAILURE an unrecoverable error occurred.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                   paul.vandelst@ssec.wisc.edu
!S-

  FUNCTION Write_SatInfo_by_FileID( FileID,       &  ! Input
                                    SatInfo,      &  ! Input
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    INTEGER,                          INTENT(IN)  :: FileID
    TYPE(SatInfo_type), DIMENSION(:), INTENT(IN)  :: SatInfo
    CHARACTER(*), OPTIONAL,           INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,           INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SatInfo(FileID)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: i, j, nEntries


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Get the number of channel entries to write
    nEntries = SIZE(SatInfo)
    IF ( nEntries < 1 ) THEN
      Message = 'Input SatInfo structure array has zero size'
      GOTO 1000
    END IF

    ! Check that output file is open
    IF ( .NOT. File_Open(FileID) ) THEN
      Message = 'SatInfo file is not open'
      GOTO 1000
    END IF

    ! Write the SatInfo header
    WRITE( FileID, FMT = '(a)', IOSTAT = IO_Status ) SATINFO_HEADER
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing SatInfo file header. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000
    END IF

    ! Write the SatInfo entries
    DO i = 1, nEntries

      WRITE( FileID, FMT = SATINFO_FMTSTRING, IOSTAT = IO_Status ) &
        SatInfo(i)%Sensor_Id, &
        SatInfo(i)%Channel, &
        SatInfo(i)%Use_Flag, &
        SatInfo(i)%Error, &
        SatInfo(i)%Error_Max, &
        SatInfo(i)%Var_b, &
        SatInfo(i)%Var_pg        
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SatInfo file entry # ", i5, &
                          &". IOSTAT = ", i5 )' ) i, IO_Status
        GOTO 1000
      END IF

    END DO

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_SatInfo_by_FileID

  FUNCTION Write_SatInfo_by_Filename( Filename,     &  ! Input
                                      SatInfo,      &  ! Input
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                     INTENT(IN)  :: Filename
    TYPE(SatInfo_type), DIMENSION(:), INTENT(IN)  :: SatInfo
    CHARACTER(*), OPTIONAL,           INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,           INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SatInfo(Filename)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID


    ! Set up
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check if output file is open
    IF ( File_Open(Filename) ) THEN

      ! Get the unit number of the open file 
      INQUIRE(FILE=Filename, NUMBER=FileID)

    ELSE

      ! Open the file
      FileID = Get_Lun()
      IF ( FileID < 0 ) THEN
        Message = 'Error obtaining file unit number.'
        GOTO 1000
      END IF

      OPEN( FileID, FILE   = TRIM( Filename ), &
                    STATUS = 'UNKNOWN', &
                    ACCESS = 'SEQUENTIAL', &
                    FORM   = 'FORMATTED', &
                    ACTION = 'WRITE', &
                    IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error opening GSI SatInfo file ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        GOTO 1000
      END IF

    END IF


    ! Write the SatInfo structure array
    Error_Status = Write_SatInfo( FileID, SatInfo, &
                                  Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) RETURN
    
    CLOSE( FileID )

    RETURN


    ! --------------
    ! Process errors
    ! --------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_SatInfo_by_Filename

END MODULE SatInfo_IO
