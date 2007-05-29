!--------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_Panel_IO
!
! PURPOSE:
!       Module containing routines to read panel data from and write panel data
!       to an LBLRTM format file
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LBLRTM_Panel_IO
!
! MODULES:
!       Type_Kinds:         Module with data type kind definitions.
!
!       File_Utility:       Module containing generic file utility routines
!
!       Message_Handler:    Module containing error handling definitions and
!                           routines.
!                           USEs: FILE_UTILITY module
!
!       LBLRTM_Parameters:  Module containing shared parameters required
!                           for LBLRTM format file IO
!                           USEs: TYPE_KINDS module
!
! CONTAINS:
!       Read_LBLRTM_Panel:  Function to read panel data from an LBLRTM
!                           format file.
!
!       Write_LBLRTM_Panel: Function to write panel data to an LBLRTM
!                           format file.
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

MODULE LBLRTM_Panel_IO

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE LBLRTM_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Read_LBLRTM_Panel
  PUBLIC :: Write_LBLRTM_Panel


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Read_LBLRTM_Panel
!
! PURPOSE:
!       Function to read panel data from an LBLRTM format file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_status = Read_LBLRTM_Panel( FileID,                   &  ! Input
!                                         LBLRTM_Panel,             &  ! Output
!                                         EOF,                      &  ! Output
!                                         n_Points    = n_Points,   &  ! Optional input
!                                         RCS_Id      = RCS_Id,     &  ! Revision control
!                                         Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       n_Points:     Number of points to read. If this number is not specified
!                     the number of points read is determined by the size of
!                     the input array LBLRTM_Panel.
!                     If specified, then n_Points > 0
!                                   and           < or = MAX_LBLRTM_PANEL_POINTS
!                                   and           < or = SIZE( LBLRTM_Panel )
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       LBLRTM_Panel: LBLRTM panel data array. If a zero-sized array is passed,
!                     the routine returns without doing anything. I.e. the file
!                     pointer is in the same position as before the call.
!                     The data type kind is determined by the values set in the
!                     LBLRTM_Parameters module.
!                     UNITS:      N/A
!                     TYPE:       REAL( LBLRTM_FP_KIND )
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT( OUT )
!
!       EOF:          Flag indicating end-of-file status for the LBLRTM
!                     format file after the read. Valid return values are
!                     defined in the LBLRTM_Parameters module.
!                       = LBLRTM_FILE_PTR_EOF:   End-of-file has been reached.
!                                                The file is then closed.
!                       = LBLRTM_FILE_PTR_OK:    No EOF or EOL condition. File
!                                                is positioned for further 
!                                                reading.
!                       = LBLRTM_FILE_PTR_UNDEF: An error occurred. The file is
!                                                closed.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM panel data read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs or the end-of-file is encountered, the input file is 
!       closed.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Read_LBLRTM_Panel( FileID,       &  ! Input
                              LBLRTM_Panel, &  ! Output
                              EOF,          &  ! Output
                              n_Points,     &  ! Optional input
                              RCS_Id,       &  ! Revision control
                              Message_Log ) &  ! Error messaging
                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                                INTENT( IN )  :: FileID

    ! -- Output
    REAL( LBLRTM_FP_KIND ), DIMENSION( : ), INTENT( OUT ) :: LBLRTM_Panel
    INTEGER,                                INTENT( OUT ) :: EOF

    ! -- Optional input
    INTEGER,                     OPTIONAL,  INTENT( IN )  :: n_Points

    ! -- Revision control
    CHARACTER( * ),              OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),              OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_Panel'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK IF FILE IS OPEN --                           #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      EOF          = LBLRTM_FILE_PTR_UNDEF
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN NUMBER OF POINTS TO READ --                    #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Default is to read as many points as
    ! will fit in the output data array
    ! ------------------------------------

    n = SIZE( LBLRTM_Panel )

    ! -- If the input array is zero sized, do nothing
    IF ( n == 0 ) THEN
      Error_Status = SUCCESS
      EOF          = LBLRTM_FILE_PTR_OK
      RETURN
    END IF

    ! -- If it's too big, complain.
    IF ( n > LBLRTM_MAX_PANEL_POINTS ) THEN
      Error_Status = FAILURE
      EOF          = LBLRTM_FILE_PTR_UNDEF
      WRITE( Message, '( "Max. panel array size is ", i4, &
                        &" points. Passed array size is ", i10, "." )' ) &
                      LBLRTM_MAX_PANEL_POINTS, n
      CALL Display_Message( ROUTINE_NAME,    &
                            TRIM( Message ), &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! If the N_POINTS argument is passed...
    ! -------------------------------------

    IF ( PRESENT( n_Points ) ) THEN

      ! -- Is it too small?
      IF ( n_Points < 1 ) THEN
        Error_Status = FAILURE
        EOF          = LBLRTM_FILE_PTR_UNDEF
        CALL Display_Message( ROUTINE_NAME, &
                              'Input N_POINTS argument is < or = to 0.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Is it too large?
      IF ( n_Points > n ) THEN
        Error_Status = FAILURE
        EOF          = LBLRTM_FILE_PTR_UNDEF
        WRITE( Message, '( "Input panel array size is ", i4, &
                          &" points. Requested N_POINTS is ", i4, "." )' ) &
                        n, n_Points
        CALL Display_Message( ROUTINE_NAME,    &
                              TRIM( Message ), &
                              Error_Status,    &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Or is it just right?  :o)
      n = n_Points

    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE DATA --                             #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) LBLRTM_Panel( 1:n )


    ! -----------------
    ! Check read status
    ! -----------------

    IO_Status_check: SELECT CASE ( IO_Status )

      ! -- End of file has been reached
      CASE ( :-1 )
        Error_Status = SUCCESS
        EOF          = LBLRTM_FILE_PTR_EOF
        CLOSE( FileID )
        RETURN

      ! -- Read was successful, no errors
      CASE ( 0 )
        Error_Status = SUCCESS
        EOF          = LBLRTM_FILE_PTR_OK

      ! -- Error occurred in read
      CASE ( 1: )
        Error_Status = FAILURE
        EOF          = LBLRTM_FILE_PTR_UNDEF
        WRITE( Message, '( "Error reading LBLRTM file panel. ", &
                         &"IOSTAT = ", i5 )' ) IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

    END SELECT IO_Status_check
    
  END FUNCTION Read_LBLRTM_Panel





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_LBLRTM_Panel
!
! PURPOSE:
!       Function to write panel data to an LBLRTM format file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_Panel ( FileID,                   &  ! Input
!                                           LBLRTM_Panel,             &  ! Input
!                                           n_Points    = n_Points,   &  ! Optional input
!                                           RCS_Id      = RCS_Id,     &  ! Revision control
!                                           Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       LBLRTM_Panel: LBLRTM panel data array. If a zero-sized array is passed,
!                     the routine returns without doing anything. I.e. the file
!                     pointer is in the same position as before the call.
!                     The data type kind is determined by the values set in the
!                     LBLRTM_Parameters module.
!                     UNITS:      N/A
!                     TYPE:       REAL( LBLRTM_FP_KIND )
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       n_Points:     Number of points to write. If this number is not specified
!                     the number of points written is determined by the size of
!                     the input array LBLRTM_Panel.
!                     If specified, then n_Points > 0
!                                   and           < or = MAX_LBLRTM_PANEL_POINTS
!                                   and           < or = SIZE( LBLRTM_Panel )
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUTS:
!       None
!
! OPTIONAL OUTUTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the LBLRTM panel data write was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs the output file is closed.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  FUNCTION Write_LBLRTM_Panel( FileID,       &  ! Input
                               LBLRTM_Panel, &  ! Input
                               n_Points,     &  ! Optional input
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                                INTENT( IN )  :: FileID
    REAL( LBLRTM_FP_KIND ), DIMENSION( : ), INTENT( IN )  :: LBLRTM_Panel

    ! -- Optional input
    INTEGER,                     OPTIONAL,  INTENT( IN )  :: n_Points

    ! -- Revision control
    CHARACTER( * ),              OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ),              OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_Panel'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: n
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                    -- SET SUCCESSFUL RETURN STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- CHECK IF FILE IS OPEN --                           #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. File_Open( FileID ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'LBLRTM file is not open.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN NUMBER OF POINTS TO WRITE --                   #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Default is to write as many points as
    ! will fit in the output data array
    ! -------------------------------------

    n = SIZE( LBLRTM_Panel )

    ! -- If the input array is zero sized, do nothing
    IF ( n == 0 ) THEN
      Error_Status = SUCCESS
      RETURN
    END IF

    ! -- If it's too big, complain.
    IF ( n > LBLRTM_MAX_PANEL_POINTS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Max. panel array size is ", i4, &
                        &" points. Passed array size is ", i10, "." )' ) &
                      LBLRTM_MAX_PANEL_POINTS, n
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------------------------
    ! If the N_POINTS argument is passed...
    ! -------------------------------------

    IF ( PRESENT( n_Points ) ) THEN

      ! -- Is it too small?
      IF ( n_Points < 1 ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Input N_POINTS argument is < or = to 0.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Is it too large?
      IF ( n_Points > n ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Input panel array size is ", i4, &
                          &" points. Specified N_POINTS is ", i4, "." )' ) &
                        n, n_Points
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Or is it just right?  :o)
      n = n_Points

    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- WRITE THE DATA --                            #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) LBLRTM_Panel( 1:n )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing LBLRTM panel data. ", &
                        &"IOSTAT = ", i5 )' ) IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    
  END FUNCTION Write_LBLRTM_Panel

END MODULE LBLRTM_Panel_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id$
!
! $Date: 2006/07/26 21:43:58 $
!
! $Revision$
!
! $Name:  $
!
! $State: Exp $
!
! $Log: LBLRTM_Panel_IO.f90,v $
! Revision 1.10  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 1.9  2005/05/08 14:51:48  paulv
! - Upgraded to Fortran-95
! - Added optional RCS_Id argument to public procedures.
!
! Revision 1.8  2002/06/05 19:03:05  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.7  2002/04/16 18:36:44  paulv
! - Updated documentation
!
! Revision 1.6  2002/04/16 18:30:32  paulv
! - Added write function to public procedure list.
! - Updated documentation.
!
! Revision 1.5  2002/04/12 20:53:13  paulv
! - Add the checks for n > LBLRTM_MAX_PANEL_POINTS back into the READ_LBLRTM_PANEL()
!   and WRITE_LBLRTM_PANEL() functions. Not doing this would require the use
!   of an allocatable array in the READ_LBLRTM_LAYER() function in the
!   LBLRTM_LAYER_IO module. I didn't want to do that.
!
! Revision 1.4  2002/04/10 02:43:02  paulv
! - Added write function.
! - Removed constraint that panel data I/O could only be in chunks of maximum
!   size MAX_LBLRTM_PANEL_POINTS. This is only true for LBLRTM generated
!   data files. One written by a user could consist of only one big panel.
! - Updated documentation.
!
! Revision 1.3  2002/04/09 03:20:55  paulv
! - Bringing repository up to date. Minor changes but still incomplete.
!
! Revision 1.2  2002/03/30 04:51:50  paulv
! - Corrected some naming errors.
! - Moved return status definitions to begin of function rather than after
!   the file open check.
! - Code untested.
!
! Revision 1.1  2002/03/29 19:53:56  paulv
! Initial checkin of new LBLRTM IO routines.
!
!
!
