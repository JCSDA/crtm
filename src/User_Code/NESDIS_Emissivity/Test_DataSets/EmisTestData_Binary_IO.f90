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
