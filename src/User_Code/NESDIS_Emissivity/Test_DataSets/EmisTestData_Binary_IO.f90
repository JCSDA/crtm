!------------------------------------------------------------------------------
!M+
! NAME:
!       EmisTestData_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write the Emissivity Test
!       Data files.
!       
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE EmisTestData_Binary_IO
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
!       EmisTestData_Define:   Module defining the EmisTestData data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Read_EmisTestData_Binary:    Function to read the Emissivity Test
!                                    Data files.
!
!       Write_EmisTestData_Binary:   Function to write the Emissivity Test
!                                    Data files.
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
!       User specified EmisTestData data files for both input and output.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
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
!M-
!------------------------------------------------------------------------------

MODULE EmisTestData_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE EmisTestData_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Read_EmisTestData_Binary
  PUBLIC :: Write_EmisTestData_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

  ! -- The base record length in bytes
  INTEGER, PRIVATE, PARAMETER :: BASE_RECORD_LENGTH = 170

CONTAINS





!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_EmisTestData_Binary
!
! PURPOSE:
!       Function to read the Emissivity Test Data files.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_EmisTestData_Binary( Filename,                 &  ! Input
!                                                n_Channels,               &  ! Input
!                                                Record_Number,            &  ! Input
!                                                EmisTestData,             &  ! Output
!                                                RCS_Id      = RCS_Id,     &  ! Revision control
!                                                Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           input EmisTestData data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       n_Channels:         Number of defined channels. Used to allocate the 
!                           output structure and define the correct record
!                           length.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
!       Record_Number:      The record number to read from the file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       EmisTestData:       Structure to contain the emissivity test data
!                           record read from the file.
!                           UNITS:      N/A
!                           TYPE:       EmisTestData_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the file read was successful
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Allocate_EmisTestData:   Function to allocate the pointer members
!                                of the EmisTestData structure.
!                                SOURCE: EmisTestData_DEFINE module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output EmisTestData argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 03-Oct-2001
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_EmisTestData_Binary( Filename,      &  ! Input
                                     n_Channels,    &  ! Input
                                     Record_Number, &  ! Input
                                     EmisTestData,  &  ! Output
                                     RCS_Id,        &  ! Revision control
                                     Message_Log )  &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )     :: Filename
    INTEGER,                   INTENT( IN )     :: n_Channels
    INTEGER,                   INTENT( IN )     :: Record_Number

    ! -- Output
    TYPE( EmisTestData_type ), INTENT( IN OUT ) :: EmisTestData

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_EmisTestData_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Record_Length
 



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OPEN THE EmisTestData DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( .NOT. File_Exists( Filename ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------
    ! Compute the record length in BYTES
    ! ----------------------------------

    Record_Length = BASE_RECORD_LENGTH + &
                    ( n_Bytes_Double * n_Channels )


    ! -------------------------
    ! Get a logical unit number
    ! -------------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining a free unit number', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = Filename, &
                  STATUS = 'OLD', &
                  ACTION = 'READ', &
                  ACCESS = 'DIRECT', &
                  FORM   = 'UNFORMATTED', &
                  RECL   = Record_Length, &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- ALLOCATE THE OUTPUT STRUCTURE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Allocate_EmisTestData( n_Channels, &
                                          EmisTestData, &
                                          Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,    &
                            'Error occurred allocating EmisTestData structure.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


     
    !#--------------------------------------------------------------------------#
    !#                       -- READ THE REQUESTED RECORD --                    #
    !#--------------------------------------------------------------------------#

    READ( FileID, REC    = Record_Number, &
                  IOSTAT = IO_Status      ) EmisTestData%ObsType, &               
                                            EmisTestData%Channel, &               
                                            EmisTestData%Frequency, &             
                                            EmisTestData%Channel_Polarization, &  
                                            EmisTestData%Latitude, &              
                                            EmisTestData%Longitude, &             
                                            EmisTestData%Satellite_Zenith_Angle, &
                                            EmisTestData%Satellite_View_Angle, &  
                                            EmisTestData%LandSea_Flag, &          
                                            EmisTestData%IceSnow_Flag, &          
                                            EmisTestData%Surface_Type, &          
                                            EmisTestData%Wind_Speed_10m, &        
                                            EmisTestData%Skin_Temperature, &      
                                            EmisTestData%Snow_Depth, &            
                                            EmisTestData%Vegetation_Fraction, &   
                                            EmisTestData%Vegetation_Type, &       
                                            EmisTestData%Soil_Type, &             
                                            EmisTestData%Soil_Moisture, &         
                                            EmisTestData%Soil_Temperature, &      
                                            EmisTestData%SimulatedTb, &           
                                            EmisTestData%Emissivity, &            
                                            EmisTestData%Emissivity_Vertical, &   
                                            EmisTestData%Emissivity_Horizontal, &
                                            EmisTestData%ObsTb

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading EmisTestData file record # ", i5, " from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      Record_Number, TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_EmisTestData_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_EmisTestData_Binary
!
! PURPOSE:
!       Function to write the Emissivity Test Data files.
!
! CATEGORY:
!       CRTM : User Code : NESDIS Emissivity
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_EmisTestData_Binary( Filename,                 &  ! Input
!                                                 n_Channels,               &  ! Input
!                                                 Record_Number,            &  ! Input
!                                                 EmisTestData,             &  ! Input
!                                                 New         = New,        &  ! Optional input
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of an output
!                       EmisTestData data file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       n_Channels:     Number of defined channels. Used to allocate the 
!                       output structure and define the correct record
!                       length.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       Record_Number:  The record number to read from the file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
!       EmisTestData:   Structure containing the spectral coefficient data.
!                       UNITS:      N/A
!                       TYPE:       EmisTestData_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       New:            Set this argument to create a new file when it
!                       is opened for writing.
!                       If == 0, file STATUS is "OLD"      [Default]
!                          == 1, file STATUS is "REPLACE"
!                       If not specified the default action is for the file
!                       status to be "OLD". If the file status is "OLD" and
!                       the file does not exist, the file status is changed
!                       to "NEW".
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER( * )
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file write was successful
!                          == FAILURE - the input EmisTestData structure contains
!                                       unassociated pointer members, or
!                                     - a unrecoverable write error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CALLS:
!       Associated_EmisTestData: Function to test the association status
!                                of the pointer members of a EmisTestData
!                                structure.
!                                SOURCE: EmisTestData_DEFINE module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the same record number is specified in different calls, the record
!         is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Dec-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_EmisTestData_Binary( Filename,      &  ! Input
                                      n_Channels,    &  ! Input
                                      Record_Number, &  ! Input
                                      EmisTestData,  &  ! Input
                                      New,           &  ! Optional input
                                      RCS_Id,        &  ! Revision control
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )  :: Filename
    INTEGER,                   INTENT( IN )  :: n_Channels
    INTEGER,                   INTENT( IN )  :: Record_Number
    TYPE( EmisTestData_type ), INTENT( IN )  :: EmisTestData

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )  :: New

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_EmisTestData_Binary'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    CHARACTER( 7 ) :: File_Status
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Record_Length
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_EmisTestData( EmisTestData ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT EmisTestData pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK OPTIONAL ARGUMENTS --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default action is to write to an existing file...
    File_Status = 'OLD'

    ! -- ... unless the NEW argument is set
    IF ( PRESENT( New ) ) THEN
      IF ( New == SET ) File_Status = 'REPLACE'
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- OPEN THE EmisTestData DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    ! --------------------
    ! Does the file exist?
    ! --------------------

    IF ( .NOT. File_Exists( Filename ) ) File_Status = 'NEW'


    ! ----------------------------------
    ! Compute the record length in BYTES
    ! ----------------------------------

    Record_Length = BASE_RECORD_LENGTH + &
                    ( n_Bytes_Double * n_Channels )


    ! -------------------------
    ! Get a logical unit number
    ! -------------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining a free unit number', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = Filename, &
                  STATUS = File_Status, &
                  ACTION = 'WRITE', &
                  ACCESS = 'DIRECT', &
                  FORM   = 'UNFORMATTED', &
                  RECL   = Record_Length, &
                  IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- READ THE REQUESTED RECORD --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, REC    = Record_Number, &
                   IOSTAT = IO_Status      ) EmisTestData%ObsType, &               
                                             EmisTestData%Channel, &               
                                             EmisTestData%Frequency, &             
                                             EmisTestData%Channel_Polarization, &  
                                             EmisTestData%Latitude, &              
                                             EmisTestData%Longitude, &             
                                             EmisTestData%Satellite_Zenith_Angle, &
                                             EmisTestData%Satellite_View_Angle, &  
                                             EmisTestData%LandSea_Flag, &          
                                             EmisTestData%IceSnow_Flag, &          
                                             EmisTestData%Surface_Type, &          
                                             EmisTestData%Wind_Speed_10m, &        
                                             EmisTestData%Skin_Temperature, &      
                                             EmisTestData%Snow_Depth, &            
                                             EmisTestData%Vegetation_Fraction, &   
                                             EmisTestData%Vegetation_Type, &       
                                             EmisTestData%Soil_Type, &             
                                             EmisTestData%Soil_Moisture, &         
                                             EmisTestData%Soil_Temperature, &      
                                             EmisTestData%SimulatedTb, &           
                                             EmisTestData%Emissivity, &            
                                             EmisTestData%Emissivity_Vertical, &   
                                             EmisTestData%Emissivity_Horizontal, &
                                             EmisTestData%ObsTb

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing EmisTestData file record # ", i5, " to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      Record_Number, TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_EmisTestData_Binary

END MODULE EmisTestData_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/02 14:58:35 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: EmisTestData_Binary_IO.f90,v $
! Revision 1.2  2006/05/02 14:58:35  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2004/12/08 16:47:35  paulv
! Initial checkin.
!
!
!
!
