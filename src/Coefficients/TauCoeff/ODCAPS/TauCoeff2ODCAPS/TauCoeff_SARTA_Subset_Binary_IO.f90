!------------------------------------------------------------------------------
!M+
! NAME:
!       TauCoeff_SARTA_Subset_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write Binary format
!       TauCoeff_SARTA_Subset files.
!       
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE TauCoeff_SARTA_Subset_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:         Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Binary_File_Utility:   Module containing utility routines for "Binary" 
!                              format datafiles.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    Message_Handler module
!
!       TauCoeff_SARTA_Subset_Define:Module defining the TauCoeff_SARTA_Subset data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    Message_Handler module
!
! CONTAINS:
!       Inquire_TauCoeff_Subset_Binary: Function to inquire a Binary format
!                                       TauCoeff_SARTA_Subset file.
!
!       Read_TauCoeff_Subset_Binary:    Function to read a Binary format
!                                       TauCoeff_SARTA_Subset file.
!
!       Write_TauCoeff_Subset_Binary:   Function to write a Binary format
!                                       TauCoeff_SARTA_Subset file.
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
!       User specified Binary format TauCoeff_SARTA_Subset data files for both
!       input and output.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!
!  Copyright (C) 2006 Yong Chen
!
!M-
!------------------------------------------------------------------------------

MODULE TauCoeff_SARTA_Subset_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE TauCoeff_SARTA_Subset_Define



  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_TauCoeff_Subset_Binary
  PUBLIC :: Read_TauCoeff_Subset_Binary
  PUBLIC :: Write_TauCoeff_Subset_Binary

  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Read_TauCoeff_Subset_Binary 
    MODULE PROCEDURE Read_TauCoeff_Subset_Scalar
    MODULE PROCEDURE Read_TauCoeff_Subset_Rank1
  END INTERFACE Read_TauCoeff_Subset_Binary 

  INTERFACE Write_TauCoeff_Subset_Binary 
    MODULE PROCEDURE Write_TauCoeff_Subset_Scalar
    MODULE PROCEDURE Write_TauCoeff_Subset_Rank1
  END INTERFACE Write_TauCoeff_Subset_Binary
  
  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: UNSET = 0
  INTEGER, PRIVATE, PARAMETER :: SET   = 1

  INTEGER, PRIVATE, PARAMETER :: NO    = UNSET
  INTEGER, PRIVATE, PARAMETER :: YES   = SET


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  FUNCTION Read_TauCoeff_Subset_Record( FileID,           &  ! Input
                                     	TauCoeff_Subset,  &  ! Output
                                     	No_Allocate,      &  ! Optional input
                                     	Message_Log )     &  ! Error messaging
                                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )     :: FileID

    ! -- Outut
    TYPE( TauCoeff_SARTA_Subset_type ),  INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_SARTA_Subset_Binary(Record)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_Allocate
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER( Long ) :: n_Layers
    INTEGER( Long ) :: n_Total_Predictors  
    INTEGER( Long ) :: n_Absorbers        
    INTEGER( Long ) :: n_Channels         
  

    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------------
    ! Process the optional No_Allocate argument
    ! -------------------------------------------

    ! -- Default action is to allocate the structure....
    Yes_Allocate = .TRUE.

    ! -- ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Release, &
                                       TauCoeff_Subset%Version
				   
    IF ( IO_Status /= 0 ) THEN
      WRITE(Message, '("Error reading TauCoeff_SARTA_Subset file Release/Version. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_TauCoeff_Subset_Release( TauCoeff_Subset, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message='TauCoeff_SARTA_Subset Release check failed.' 
      GOTO 1000  ! Clean up
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- READ THE TauCoeff_SARTA_Subset DIMENSIONS --                      #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Layers, &                    
                                       n_Total_Predictors, &
                                       n_Absorbers, &
                                       n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- ALLOCATE THE TauCoeff_SARTA_Subset STRUCTURE IF REQUIRED --              #
    !#--------------------------------------------------------------------------#

    IF ( Yes_Allocate ) THEN


      ! ----------------------
      ! Perform the allocation
      ! ----------------------

      Error_Status = Allocate_TauCoeff_Subset( n_Layers, &
                                               n_Total_Predictors, &
                                               n_Absorbers, &
                                               n_Channels, &
                                               TauCoeff_Subset, &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error allocating TauCoeff_SARTA_Subset structure.'
        GOTO 1000
      END IF


    ELSE


      ! ---------------------------------------------------------
      ! Structure already allocated. Check the association status
      ! ---------------------------------------------------------

      IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset ) ) THEN
        Message = 'TauCoeff_SARTA_Subset structure components are not associated.'
        GOTO 1000  ! Clean up
      END IF


      ! --------------------------
      ! Check the dimension values
      ! --------------------------

      IF ( n_Layers /= TauCoeff_Subset%n_Layers ) THEN
        WRITE( Message, '( "TauCoeff_SARTA_Subset data dimensions, ", i5, &
                          &" are inconsistent with structure definition, ", i5, "." )' ) &
                        n_Layers, TauCoeff_Subset%n_Layers
        GOTO 1000  ! Clean up
      END IF

    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE TauCoeff_SARTA_Subset DATA --             #
    !#--------------------------------------------------------------------------#


    !#--------------------------------------------------------------------------#
    !#                       -- READ THE CHANNEL INDEX DATA --                  #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Channel_Index

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset Channel_Index data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                      -- READ THE ABSORBER ID DATA --                     #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset absorber ID data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- READ THE ABSORBER PREDICTOR INDICES --          #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Absorber_Predictor

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset Absorber_Predictor data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                -- READ THE GAS ABSORPTION COEFFICIENTS --                #
    !#--------------------------------------------------------------------------#
 
    READ( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%C

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading TauCoeff_SARTA_Subset absorption coefficients data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    RETURN
        
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

    IF ( Associated_TauCoeff_Subset( TauCoeff_Subset ) ) THEN
      Destroy_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset, &
                                           Message_Log = Message_Log )
    END IF

    CLOSE( FileID, IOSTAT = IO_Status )

  END FUNCTION Read_TauCoeff_Subset_Record


  FUNCTION Write_TauCoeff_Subset_Record( FileID,           &  ! Input
                                     	 TauCoeff_Subset,  &  ! Input
                                     	 Message_Log )     &  ! Error messaging
                                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )  :: FileID
    TYPE( TauCoeff_SARTA_Subset_type ),  INTENT( IN )  :: TauCoeff_Subset

    ! -- Error handler Message log
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_SARTA_Subset_Binary(Record)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset ) ) THEN
      Message = 'Some or all INPUT TauCoeff_SARTA_Subset pointer members are NOT associated.'
      GOTO 1000
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! WRITE the Release/Version information
    ! ------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Release, &
                                        TauCoeff_Subset%Version

    IF ( IO_Status /= 0 ) THEN
      WRITE(Message, '("Error writing TauCoeff_SARTA_Subset file Release/Version. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                    -- WRITE THE TauCoeff_SARTA_Subset DIMENSIONS --            #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%n_Layers, &
                                        TauCoeff_Subset%n_Total_Predictors, &
                                        TauCoeff_Subset%n_Absorbers, &
                                        TauCoeff_Subset%n_Channels

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE TauCoeff_SARTA_Subset DATA --             #
    !#--------------------------------------------------------------------------#

    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE CHANNEL INDEX DATA --                 #
    !#--------------------------------------------------------------------------#


    WRITE( FileID, IOSTAT = IO_Status )  TauCoeff_Subset%Channel_Index 
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset Channel_Index data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE ABSORBER ID DATA --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Absorber_ID

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset absorber ID data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- WRITE THE ABSORBER PREDICTOR INDICES --         #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%Absorber_Predictor

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset Absorber_Predictor data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                -- WRITE THE GAS ABSORPTION COEFFICIENTS --               #
    !#--------------------------------------------------------------------------#
 
    WRITE( FileID, IOSTAT = IO_Status ) TauCoeff_Subset%C

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing TauCoeff_SARTA_Subset absorption coefficients data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, IOSTAT = IO_Status )

  END FUNCTION Write_TauCoeff_Subset_Record




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
!       Inquire_TauCoeff_Subset_Binary
!
! PURPOSE:
!       Function to inquire a Binary format TauCoeff_SARTA_Subset structure file.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_TauCoeff_Subset_Binary( Filename,                    &  ! Input
!                                               n_Subsets     = n_Subsets,          &  ! Optional output
!                                               RCS_Id       = RCS_Id,       &  ! Revision control
!                                               Message_Log  = Message_Log   )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           TauCoeff_SARTA_Subset data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Subsets:           The number of the subset aborber coefficients.
!                            UNITS:	 N/A		  
!                            TYPE:	 INTEGER( Long )  
!                            DIMENSION:  Scalar 	  
!
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:  Function to open Binary format	   
!                          data files.  			   
!                          SOURCE: BINARY_FILE_UTILITY module	   
!
!       Display_Message:   Subroutine to output Messages	   
!                          SOURCE: Message_Handler module 	   
!
!       File_Exists:       Function to test for the existance
!                          of files.
!                          SOURCE: FILE_UTILITY module
!
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_TauCoeff_Subset_Binary( Filename,     &  ! Input
                                           n_Subsets,	 &  ! Optional output
                                           RCS_Id,	 &  ! Revision control
                                           Message_Log ) &  ! Error messaging
                                         RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_Subsets
 
    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_TauCoeff_SARTA_Subset_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER( Long ) :: File_n_Subsets
 



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF

    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- OPEN THE BINARY FORMAT TauCoeff_SARTA_Subset DATA FILE --              #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening TauCoeff_SARTA_Subset file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE n_Subsets of TauCoeff --                #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_Subsets 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading File_n_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#                    -- SAVE THE NUMBER OF SUBSET  --                      #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = File_n_Subsets

    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


 
  END FUNCTION Inquire_TauCoeff_Subset_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_TauCoeff_Subset_Binary
!
! PURPOSE:
!       Function to read Binary format TauCoeff_SARTA_Subset files.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_TauCoeff_Subset_Binary( Filename,               &  ! Input
!                                            TauCoeff_Subset,               &  ! Output
!                                            No_File_Close = No_File_Close, &  ! Optional input
!                                            No_Allocate   = No_Allocate,   &  ! Optional input      
!                                            n_Subsets     = n_Subsets,     &  ! Optional output     
!                                            RCS_Id        = RCS_Id,	    &  ! Revision control    
!                                            Message_Log   = Message_Log    )  ! Error messaging     
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the binary
!                           format TauCoeff_SARTA_Subset data file to read.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:      Set this argument to NOT close the file upon exit.
!                           If == 0, the input file is closed upon exit [DEFAULT]
!                              == 1, the input file is NOT closed upon exit. 
!                           If not specified, the default action is to close the
!                           input file upon exit.
!                           the 
!                           UNITS:	N/A
!                           TYPE:	INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       No_Allocate:        Set this argument to NOT allocate the output TauCoeff_SARTA_Subset
!                           structure in this routine based on the data dimensions
!                           read from the input data file. This assumes that the
!                           structure has already been allocated prior to calling 
!                           this function.
!                           If == 0, the output Cloud structure is allocated [DEFAULT]
!                              == 1, the output Cloud structure is NOT allocated
!                           If not specified, the default action is to allocate 
!                           the output Cloud structure to the dimensions specified
!                           in the input data file.
!                           the 
!                           UNITS:	N/A
!                           TYPE:	INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( IN )
!
!       Message_Log:        Character string specifying a filename in which any
!                           Messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output Messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       TauCoeff_Subset:    Structure containing the transmittance coefficient data
!                           read from the file.
!                           UNITS:      N/A
!                           TYPE:       TauCoeff_SARTA_Subset_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Subsets:          The actual number of subsets read in.
!                           UNITS:	N/A
!                           TYPE:	INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE an unrecoverable read error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Allocate_TauCoeff_Subset:Function to allocate the pointer members
!                                of the TauCoeff_SARTA_Subset structure.
!                                SOURCE: TauCoeff_SARTA_Subset_DEFINE module
!
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: Message_Handler module
!
!       File_Exists:             Function to test for the existance  
!                                of files.			     
!                                SOURCE: FILE_UTILITY module	     
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output TauCoeff_SARTA_Subset argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_TauCoeff_Subset_Scalar( Filename,   &  ! Input
                                 TauCoeff_Subset,   &  ! Output
                                 No_File_Close,     &  ! Optional input
                                 No_Allocate,       &  ! Optional input
                                 n_Subsets,         &  ! Optional output
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_File_Close 
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT )    :: n_Subsets

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_SARTA_Subset_Binary(Scalar)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER( Long ):: n_Input_Subsets
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#
 
    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#
    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get its file id
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      ! -- Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 

      ! -- Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )
 
      IF ( Error_Status /= SUCCESS ) THEN
       Message ='Error opening '//TRIM( Filename )  
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF
 
    END IF


    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                       -- READ THE NUMBER OF SUBSETS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Subsets

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Input_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! ---------------------
    ! Check if n_Subsets > 1
    ! ---------------------

    IF ( n_Input_Subsets > 1 ) THEN
      WRITE( Message, '( "Number of subsets > 1 (",i5,") and output TauCoeff_SARTA_Subset structure ", &
                        &"is scalar." )' ) n_Input_Subsets
      GOTO 1000  ! Clean up
    END IF

 
    !#--------------------------------------------------------------------------#
    !#                        -- READ THE STRUCTURE DATA --                     #
    !#--------------------------------------------------------------------------#
    Error_Status = Read_TauCoeff_Subset_Record( FileID, &
                                      TauCoeff_Subset, &
                                      No_Allocate = No_Allocate, &
                                      Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Cloud record from '//TRIM( Filename )
      GOTO 1000  ! Clean up  
    END IF

    !#--------------------------------------------------------------------------#
    !#                   -- SAVE THE NUMBER OF SUBSETS READ --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = 1

    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN
 

    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    IF ( Associated_TauCoeff_Subset( TauCoeff_Subset ) ) THEN
      Destroy_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset, &
                                           Message_Log = Message_Log )
    END IF
    
  END FUNCTION Read_TauCoeff_Subset_Scalar


  FUNCTION Read_TauCoeff_Subset_Rank1( Filename,    &  ! Input
                                 TauCoeff_Subset,   &  ! Output
                                 No_File_Close,     &  ! Optional input
                                 No_Allocate,       &  ! Optional input
                                 n_Subsets,         &  ! Optional output
                                 RCS_Id,            &  ! Revision control
                                 Message_Log )      &  ! Error messaging
                               RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )     :: Filename

    ! -- Output
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN OUT ) :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_File_Close
    INTEGER,        OPTIONAL, INTENT( IN )     :: No_Allocate

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT )    :: n_Subsets

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_TauCoeff_SARTA_Subset_Binary(Rank-1)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close 
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER( Long )::m, n_Input_Subsets
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


 
    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#
    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get its file id
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      ! -- Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 
  
      ! -- Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
       Message ='Error opening '//TRIM( Filename )  
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF
 
    END IF

    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF

    !#--------------------------------------------------------------------------#
    !#                       -- READ THE NUMBER OF SUBSETS --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Subsets

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Input_Subsets data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! -----------------------------------------
    ! Check if n_Subsets > size of output array
    ! -----------------------------------------

    IF ( n_Input_Subsets > size( TauCoeff_Subset ) ) THEN
      WRITE( Message, '( "Number of subsets ",i5," > size of the output ", &
                         &"structure array, ", i5, "." )' ) &
                      n_Input_Subsets, SIZE( TauCoeff_Subset ) 
      GOTO 1000  ! Clean up
    END IF

    !#--------------------------------------------------------------------------#
    !#                           -- LOOP OVER SUMSETS --                        #
    !#--------------------------------------------------------------------------#

    Subset_Loop: DO m = 1, n_Input_Subsets

      Error_Status = Read_TauCoeff_Subset_Record( FileID, &
                                        TauCoeff_Subset(m), &
                                        No_Allocate = No_Allocate, &
                                        Message_Log = Message_Log )!

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading subset element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF
 
    END DO Subset_Loop

 
    !#--------------------------------------------------------------------------#
    !#                   -- SAVE THE NUMBER OF SUBSETS READ --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Subsets ) ) n_Subsets = n_Input_Subsets

 
    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
 
    Destroy_Status = Destroy_TauCoeff_Subset( TauCoeff_Subset, &
                                           Message_Log = Message_Log )
   
  END FUNCTION Read_TauCoeff_Subset_Rank1




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_TauCoeff_Subset_Binary
!
! PURPOSE:
!       Function to write Binary format TauCoeff_SARTA_Subset files.
!
! CATEGORY:
!       Optical Depth : Coefficients
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_TauCoeff_Subset_Binary( Filename,               &   ! Input
!                                             TauCoeff_Subset,               &   ! Input
!                                             No_File_Close = No_File_Close, &  ! Optional input
!                                             RCS_Id        = RCS_Id,        &   ! Revision control
!                                             Message_Log   = Message_Log    )   ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     TauCoeff_SARTA_Subset format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       TauCoeff_Subset:Structure containing the gas absorption coefficient data.
!                     UNITS:      N/A
!                     TYPE:       TauCoeff_SARTA_Subset_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:Set this argument to NOT close the file upon exit.     
!                     If == 0, the input file is closed upon exit [DEFAULT]  
!                        == 1, the input file is NOT closed upon exit.       
!                     If not specified, the default action is to close the   
!                     input file upon exit.				     
!                     the						     
!                     UNITS:	  N/A					     
!                     TYPE:	  INTEGER				     
!                     DIMENSION:  Scalar				     
!                     ATTRIBUTES: OPTIONAL, INTENT( IN )		     
!
!       Message_Log:  Character string specifying a filename in which any
!                     Messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output Messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE - the input TauCoeff_SARTA_Subset structure contains
!                                     unassociated pointer members, or
!                                   - a unrecoverable write error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_TauCoeff_Subset:     Function to test the association status
!                                of the pointer members of a TauCoeff_SARTA_Subset
!                                structure.
!                                SOURCE: TauCoeff_SARTA_Subset_DEFINE module
!
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output Messages
!                                SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Chen, CSU/CIRA 03-May-2006
!                       Yong.Chen@noaa.gov
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_TauCoeff_Subset_Scalar( Filename,     &  ! Input
                                  TauCoeff_Subset,     &  ! Input
                                  No_File_Close,       &  ! Optional input
                                  RCS_Id,              &  ! Revision control
                                  Message_Log )        &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( TauCoeff_SARTA_Subset_type ),    INTENT( IN )  :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_File_Close

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_SARTA_Subset_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF


    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF

    END IF


    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_TauCoeff_Subset( TauCoeff_Subset ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT TauCoeff_SARTA_Subset pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------------
    ! Check the TauCoeff_SARTA_Subset structure dimensions
    ! ---------------------------------------

    IF ( TauCoeff_Subset%n_Layers           < 1 .OR. &
         TauCoeff_Subset%n_Total_Predictors < 1 .OR. &
         TauCoeff_Subset%n_Absorbers        < 1 .OR. &
         TauCoeff_Subset%n_Channels         < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'One or more dimensions of TauCoeff_SARTA_Subset structure are < or = 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF SUBSETS --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) 1

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Clouds data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_TauCoeff_Subset_Record( FileID, &
                                       TauCoeff_Subset, &
                                       Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing TauCoeff_SARTA_Subset record to '//TRIM( Filename )
      GOTO 1000
    END IF

    !#--------------------------------------------------------------------------#
    !#                 -- CLOSE THE OUTPUT FILE IF REQUIRED --                  #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN


    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_TauCoeff_Subset_Scalar
  

  FUNCTION Write_TauCoeff_Subset_Rank1( Filename,     &  ! Input
                                  TauCoeff_Subset,    &  ! Input
                                  No_File_Close,      &  ! Optional input
                                  RCS_Id,             &  ! Revision control
                                  Message_Log )       &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename
    TYPE( TauCoeff_SARTA_Subset_type ), DIMENSION( : ), INTENT( IN )  :: TauCoeff_Subset

    ! -- Optional input
    INTEGER,        OPTIONAL, INTENT( IN )  :: No_File_Close 

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_TauCoeff_SARTA_Subset_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Yes_File_Close 
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#
 
    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF


    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------------------------
    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    ! ---------------------------------------------

    IF ( .NOT. File_Open( FileName ) ) THEN

      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = SET, &
                                       Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF


    ELSE

      ! -- Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! -- Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF

    END IF

    ! ---------------------------------------
    ! Check the TauCoeff_SARTA_Subset structure dimensions
    ! ---------------------------------------

    IF ( ANY( TauCoeff_Subset%n_Layers           < 1 ) .OR. &
         ANY( TauCoeff_Subset%n_Total_Predictors < 1 ) .OR. &
         ANY( TauCoeff_Subset%n_Absorbers        < 1 ) .OR. &
         ANY( TauCoeff_Subset%n_Channels         < 1 )     ) THEN
      Message = 'One or more dimensions of TauCoeff_SARTA_Subset structure are < or = 0.'
      GOTO 1000
    END IF

    ! -------------------------------------------
    ! Process the optional No_File_Close argument
    ! -------------------------------------------

    ! -- Default action is to close the file on exit....
    Yes_File_Close = .TRUE.

    ! -- ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF


    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF SUBSETS --                    #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) SIZE(TauCoeff_Subset)

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Clouds data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 2000
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#
    
    TauCoeff_Subset_Loop: DO m = 1, SIZE( TauCoeff_Subset )

      Error_Status = Write_TauCoeff_Subset_Record( FileID, &			    
        				 TauCoeff_Subset(m), &			    
        				 Message_Log = Message_Log )		    

      IF ( Error_Status /= SUCCESS ) THEN					    
        WRITE( Message, '( "Error writing Cloud element #", i5, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 1000								    
      END IF									    
     
    END DO TauCoeff_Subset_Loop
  

    !#--------------------------------------------------------------------------#
    !#                 -- CLOSE THE OUTPUT FILE IF REQUIRED --                  #
    !#--------------------------------------------------------------------------#

    IF ( Yes_File_Close ) THEN

      CLOSE( FileID, IOSTAT = IO_Status )

      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF

    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION Write_TauCoeff_Subset_Rank1

END MODULE TauCoeff_SARTA_Subset_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/05/03 19:42:09 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! Revision 1.1  2006/05/03 20:06:03  ychen
! Initial checkin.
!
!
!
!

 
