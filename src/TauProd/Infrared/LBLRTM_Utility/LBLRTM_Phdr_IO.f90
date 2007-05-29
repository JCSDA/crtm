!--------------------------------------------------------------------------------
!M+
! NAME:
!       LBLRTM_Phdr_IO
!
! PURPOSE:
!       Module containing routines to read, write, and manipulate an LBLRTM
!       format panel header
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE LBLRTM_Phdr_IO
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
!       Clear_LBLRTM_Phdr:       Subroutine to clear all panel header
!                                structure member values to an "INVALID" value.
!
!       Print_LBLRTM_Phdr:       Function to print the contents of the panel header
!                                to standard output or to a log file.
!
!       Read_LBLRTM_Phdr:        Function to read a panel header from an LBLRTM
!                                format file.
!
!       Write_LBLRTM_Phdr:       Function to write a panel header to an LBLRTM
!                                format file.
!
! DERIVED TYPES:
!       LBLRTM_Phdr_type:   Definition of the public LBLRTM_Phdr data structure.
!                           Fields are:
!
!         Begin_Frequency:     Beginning frequency for the spectral data in the
!                              panel following the panel header.
!                              UNITS:      cm^-1
!                              TYPE:       REAL( Double )
!                              DIMENSION:  Scalar
!
!         End_Frequency:       Ending frequency for the spectral data in the
!                              panel following the panel header.
!                              UNITS:      cm^-1
!                              TYPE:       REAL( Double )
!                              DIMENSION:  Scalar
!
!         Frequency_Interval:  Frequency interval for the spectral data in the
!                              panel following the panel header. The data type
!                              kind is determined by the values set in the
!                              LBLRTM_Parameters module.
!                              UNITS:      cm^-1
!                              TYPE:       REAL( LBLRTM_FP_KIND )
!                              DIMENSION:  Scalar
!
!         n_Points:            Number of spectral points in the panel following
!                              the panel header. The data type kind is determined
!                              by the values set in the LBLRTM_Parameters module.
!                              UNITS:      N/A
!                              TYPE:       INTEGER( LBLRTM_IP_KIND )
!                              DIMENSION:  Scalar
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

MODULE LBLRTM_Phdr_IO

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
  PUBLIC :: Print_LBLRTM_Phdr
  PUBLIC :: Read_LBLRTM_Phdr
  PUBLIC :: Write_LBLRTM_Phdr


  ! -------------------------
  ! PRIVATE Module parameters
  ! -------------------------

  ! -- RCS Id for the module
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER ::   SET = 1

  ! -- Invalid values
  INTEGER( LBLRTM_IP_KIND ), PRIVATE, PARAMETER :: IP_INVALID = 0_LBLRTM_IP_KIND
  REAL( LBLRTM_FP_KIND ),    PRIVATE, PARAMETER :: FP_INVALID = -1.0_LBLRTM_FP_KIND
  REAL( Double ),            PRIVATE, PARAMETER :: DP_INVALID = -1.0_Double


  ! -------------------------------------
  ! Panel header derived type definitions
  ! -------------------------------------

  TYPE, PUBLIC :: LBLRTM_Phdr_type
    REAL( Double )            :: Begin_Frequency     = DP_INVALID
    REAL( Double )            :: End_Frequency       = DP_INVALID
    REAL( LBLRTM_FP_KIND )    :: Frequency_Interval  = FP_INVALID
    INTEGER( LBLRTM_IP_KIND ) :: n_Points            = IP_INVALID
  END TYPE LBLRTM_Phdr_type


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Reset value
  INTEGER, PRIVATE, PARAMETER :: INVALID = -1



CONTAINS





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Clear_LBLRTM_Phdr
!
! PURPOSE:
!       Subroutine to clear the LBLRTM panel header structure member values
!       to an "INVALID" value.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Clear_LBLRTM_Phdr( LBLRTM_Phdr )
!
! INPUTS:
!       None
!
! OPTIONAL INPUTS:
!       None
!
! OUTPUTS:
!       LBLRTM_Phdr:  LBLRTM panel header structure with all member
!                     values reset to an "INVALID" value.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Phdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTUTS:
!       None
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Clear_LBLRTM_Phdr ( LBLRTM_Phdr )

    TYPE ( LBLRTM_Phdr_type ), INTENT( OUT ) :: LBLRTM_Phdr

    LBLRTM_Phdr = LBLRTM_Phdr_type( REAL( INVALID, Double ),         &  ! Begin_Frequency
                                    REAL( INVALID, Double ),         &  ! End_Frequency
                                    REAL( INVALID, LBLRTM_FP_KIND ), &  ! Frequency_Interval
                                    INVALID                          )  ! n_Points

  END SUBROUTINE Clear_LBLRTM_Phdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Print_LBLRTM_Phdr
!
! PURPOSE:
!       Subroutine to print the panel header structure member values.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       CALL Print_LBLRTM_Phdr( LBLRTM_Phdr,              &  ! Input
!                               Panel_Number,             &  ! Input
!                               RCS_Id = RCS_Id,          &  ! Revision control
!                               Message_Log = Message_Log )  ! Message logging
!
! INPUTS:
!       LBLRTM_Phdr:  LBLRTM panel header structure to print.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Phdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Panel_Number: The number of the panel in the LBLRTM file/layer
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
!       Message_Log:  Character string specifying a filename to which the
!                     output will be sent. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to write to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
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
! SIDE EFFECTS:
!       Data is writen to a log file if the optional MESSAGE_LOG argument
!       is used.
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 29-Mar-2003
!                     paul.vandelst@ssec.wisc.edu
!S-
!--------------------------------------------------------------------------------

  SUBROUTINE Print_LBLRTM_Phdr ( LBLRTM_Phdr,  &  ! Input
                                 Panel_Number, &  ! Input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log   )  ! Error messaging



    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    TYPE ( LBLRTM_Phdr_type ), INTENT( IN )  :: LBLRTM_Phdr
    INTEGER,                   INTENT( IN )  :: Panel_Number

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Message logging
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Print_LBLRTM_Phdr'

    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 5000 ) :: String1, String2
    CHARACTER( 2 ) :: CRLF



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = ROUTINE_NAME//' from '//MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CREATE THE OUTPUT STRINGS --                     #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------
    ! The carriage return - line feed string
    ! --------------------------------------

    CRLF = ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)


    ! ----------------
    ! The panel number
    ! ----------------

    WRITE( String1, '( a, 2x, "Panel # ",i5, " header" )' ) CRLF, Panel_Number


    ! -----------------------------------
    ! The frequency and point information
    ! -----------------------------------

    WRITE( String2, '( a, 5x, "Begin frequency    = ", es13.6, " cm-1", &
                      &a, 5x, "End frequency      = ", es13.6, " cm-1", &
                      &a, 5x, "Frequency interval = ", es13.6, " cm-1", &
                      &a, 5x, "Number of points   = ", i5 )' ) &
                    CRLF, LBLRTM_Phdr%Begin_Frequency, &
                    CRLF, LBLRTM_Phdr%End_Frequency, &
                    CRLF, LBLRTM_Phdr%Frequency_Interval, &
                    CRLF, LBLRTM_Phdr%n_Points



    !#--------------------------------------------------------------------------#
    !#                 -- OUTPUT THE PANEL HEADER INFORMATION --                #
    !#--------------------------------------------------------------------------#

    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( String1 )//&
                          TRIM( String2 ), &
                          INFORMATION, &
                          Message_Log = Message_Log )
     
  END SUBROUTINE Print_LBLRTM_Phdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Read_LBLRTM_Phdr
!
! PURPOSE:
!       Function to read an LBLRTM panel header structure from an LBLRTM format
!       file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_LBLRTM_Phdr( FileID,                   &  ! Input
!                                        LBLRTM_Phdr,              &  ! Output
!                                        EOF,                      &  ! Output
!                                        RCS_Id = RCS_Id,          &  ! Revision control
!                                        Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUTS:
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
!       LBLRTM_Phdr:  LBLRTM panel header structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Phdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
!       EOF:          Flag indicating end-of-file status for the LBLRTM
!                     format file after the read. Valid return values are
!                     defined in the LBLRTM_Parameters module.
!                       = LBLRTM_FILE_PTR_EOF:   End-of-file has been reached.
!                                                The file is then closed.
!                       = LBLRTM_FILE_PTR_EOL:   End-of-layer has been reached.
!                                                In this case, the next read
!                                                should be of the file header
!                                                for the (possible) next layer.
!                       = LBLRTM_FILE_PTR_OK:    No EOF or EOL condition. The
!                                                next read should be the panel
!                                                data.
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
!                     If == SUCCESS the LBLRTM panel header read was successful
!                        == FAILURE an error occurred
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

  FUNCTION Read_LBLRTM_Phdr ( FileID,       &  ! Input
                              LBLRTM_Phdr,  &  ! Output
                              EOF,          &  ! Output
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
    INTEGER,                   INTENT( IN )  :: FileID

    ! -- Output
    TYPE ( LBLRTM_Phdr_type ), INTENT( OUT ) :: LBLRTM_Phdr
    INTEGER,                   INTENT( OUT ) :: EOF

     ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

   ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_LBLRTM_Phdr'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status



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
    !#                          -- READ THE DATA --                             #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) LBLRTM_Phdr


    ! -----------------
    ! Check read status
    ! -----------------

    SELECT CASE ( IO_Status )

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
        WRITE( Message, '( "Error reading LBLRTM file panel header. ", &
                          &"IOSTAT = ", i5 )' ) IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN

    END SELECT



    !#--------------------------------------------------------------------------#
    !#                       -- CHECK FOR END OF LAYER --                       #
    !#--------------------------------------------------------------------------#

    IF ( LBLRTM_Phdr%n_Points < 0 ) EOF = LBLRTM_FILE_PTR_EOL
  
  END FUNCTION Read_LBLRTM_Phdr





!--------------------------------------------------------------------------------
!S+
! NAME:
!       Write_LBLRTM_Phdr
!
! PURPOSE:
!       Function to write an LBLRTM panel header structure to an LBLRTM format
!       file.
!
! CATEGORY:
!       LBLRTM
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_LBLRTM_Phdr ( FileID,                   &  ! Input
!                                          LBLRTM_Phdr,              &  ! Input
!                                          RCS_Id = RCS_Id,          &  ! Revision control
!                                          Message_Log = Message_Log )  ! Error messaging
!
! INPUTS:
!       FileID:       Logical unit number associated with LBLRTM file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       LBLRTM_Phdr:  LBLRTM panel header structure to write.
!                     UNITS:      N/A
!                     TYPE:       TYPE( LBLRTM_Phdr_type )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL INPUTS:
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
!                     If == SUCCESS the LBLRTM panel header write was successful
!                        == FAILURE an error occurred
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

  FUNCTION Write_LBLRTM_Phdr ( FileID,       &  ! Input
                               LBLRTM_Phdr,  &  ! Input
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
    INTEGER,                   INTENT( IN )  :: FileID
    TYPE ( LBLRTM_Phdr_type ), INTENT( IN )  :: LBLRTM_Phdr

    ! -- Revision control
    CHARACTER( * ), OPTIONAL,  INTENT( OUT ) :: RCS_Id

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_LBLRTM_Phdr'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
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
    !#                      -- CHECK IF FILE IS OPEN --                         #
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
    !#                          -- WRITE THE DATA --                            #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) LBLRTM_Phdr

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing LBLRTM file panel header. ", &
                        &"IOSTAT = ", i5 )' ) IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

  END FUNCTION Write_LBLRTM_Phdr

END MODULE LBLRTM_Phdr_IO


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
! $Log: LBLRTM_Phdr_IO.f90,v $
! Revision 1.9  2006/07/26 21:43:58  wd20pd
! Replacement of "Error_Handler" with "Message_Handler" in USE statements and
! in documentaiton blocks.
!
! Revision 1.8  2005/05/08 15:11:03  paulv
! - Upgraded to Fortran-95
! - Removed Initialization() subroutine. Structure initialisation is now done
!   in the type definition.
! - Added Clear() subroutine.
! - Removed message log file open from the Print() subroutine.
! - Added optional RCS_Id argument to public procedures.
!
! Revision 1.7  2002/06/05 19:03:56  paulv
! - Removed MESSAGE as a module variable and placed definitions in each
!   module subprogram.
!
! Revision 1.6  2002/04/16 18:36:43  paulv
! - Updated documentation
!
! Revision 1.5  2002/04/12 20:55:56  paulv
! - Added PRINT_LBLRTM_PHDR() subroutine.
! - Corrected bug in referencing the LBLRTM_Phdr%n_Points component in the
!   READ_LBLRTM_PHDR() function.
!
! Revision 1.4  2002/04/09 15:34:59  paulv
! - Added write function.
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
