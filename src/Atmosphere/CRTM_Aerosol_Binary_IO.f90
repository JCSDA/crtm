!
!
! CRTM_Aerosol_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Aerosol files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Aerosol_Binary_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility       , ONLY: File_Exists, File_Open
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Aerosol_Define, ONLY: CRTM_Aerosol_type, &
                                 CRTM_Associated_Aerosol, &
                                 CRTM_Destroy_Aerosol, &
                                 CRTM_Allocate_Aerosol
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Inquire_Aerosol_Binary
  PUBLIC :: CRTM_Read_Aerosol_Binary
  PUBLIC :: CRTM_Write_Aerosol_Binary

  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! ---------------------------------------------
  ! Function to read a single aerosol data record
  ! ---------------------------------------------
  FUNCTION Read_Aerosol_Record( FileID     , &  ! Input
                                Aerosol    , &  ! Output
                                No_Allocate, &  ! Optional input
                                Message_Log) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    INTEGER                , INTENT(IN)     :: FileID
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Allocate
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Aerosol_Binary(Record)'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_Allocate
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: n_Layers

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Default action is to allocate the structure....
    Yes_Allocate = .TRUE.
    ! ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF

    ! Read the dimensions
    ! -------------------
    READ( FileID, IOSTAT = IO_Status ) n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Aerosol data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Allocate the structure if required
    ! ----------------------------------
    IF ( Yes_Allocate ) THEN

      ! Perform the allocation
      Error_Status = CRTM_Allocate_Aerosol( n_Layers, &
                                            Aerosol, &
                                            Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error allocating Aerosol structure.'
        GOTO 1000
      END IF

    ELSE

      ! Structure already allocated. Check the association status
      IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) THEN
        Message = 'Aerosol structure components are not associated.'
        GOTO 1000  ! Clean up
      END IF

      ! Check the dimension values
      IF ( n_Layers /= Aerosol%n_Layers ) THEN 
        Message = 'Aerosol data dimensions are inconsistent with structure definition'
        GOTO 1000  ! Clean up
      END IF

    END IF
    
    ! Read the aerosol data
    ! ---------------------
    READ( FileID, IOSTAT = IO_Status ) Aerosol%Type            , &
                                       Aerosol%Effective_Radius, &
                                       Aerosol%Concentration
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Aerosol data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    IF ( CRTM_Associated_Aerosol( Aerosol ) ) THEN
      Destroy_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                             Message_Log = Message_Log )
    END IF
    CLOSE( FileID, IOSTAT = IO_Status )

  END FUNCTION Read_Aerosol_Record


  ! ----------------------------------------------
  ! Function to write a single aerosol data record
  ! ----------------------------------------------
  FUNCTION Write_Aerosol_Record( FileID     , &  ! Input
                                 Aerosol    , &  ! Input
                                 Message_Log) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    INTEGER                , INTENT(IN)  :: FileID
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Aerosol_Binary(Record)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
 
    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( .NOT. CRTM_Associated_Aerosol( Aerosol ) ) THEN
      Message = 'Some or all INPUT Aerosol pointer members are NOT associated.'
      GOTO 1000
    END IF


    ! Write the dimensions
    ! --------------------
    WRITE( FileID, IOSTAT = IO_Status ) Aerosol%n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Aerosol data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the data
    ! --------------
    WRITE( FileID, IOSTAT = IO_Status ) Aerosol%Type            , &
                                        Aerosol%Effective_Radius, &
                                        Aerosol%Concentration
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Aerosol data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    !=====
    RETURN
    !=====


    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR, IOSTAT = IO_Status )

  END FUNCTION Write_Aerosol_Record


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Inquire_Aerosol_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Aerosol structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Aerosol_Binary( Filename,                 &  ! Input
!                                                   n_Aerosols  = n_Aerosols, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,     &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Aerosols:     The number of Aerosol profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file inquire was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Aerosol_Binary( Filename,     &  ! Input
                                        n_Aerosols,   &  ! Optional output
                                        RCS_Id,       &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Aerosols
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Aerosol_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Aerosols_in_File

    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Open the aerosol data file
    ! --------------------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Read the number of aerosols dimension
    ! -------------------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Aerosols_in_File
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Aerosols data dimension from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF
    
    ! Assign return arguments
    IF ( PRESENT( n_Aerosols ) ) n_Aerosols = n_Aerosols_in_File


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Inquire_Aerosol_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Read_Aerosol_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Aerosol structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Aerosol_Binary( Filename,                      &  ! Input
!                                                Aerosol,                       &  ! Output
!                                                No_File_Close = No_File_Close, &  ! Optional input
!                                                No_Allocate   = No_Allocate,   &  ! Optional input
!                                                n_Aerosols    = n_Aerosols,    &  ! Optional output
!                                                RCS_Id        = RCS_Id,        &  ! Revision control
!                                                Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       No_Allocate:    Set this argument to NOT allocate the output Aerosol
!                       structure in this routine based on the data dimensions
!                       read from the input data file. This assumes that the
!                       structure has already been allocated prior to calling 
!                       this function.
!                       If == 0, the output Aerosol structure is allocated [DEFAULT]
!                          == 1, the output Aerosol structure is NOT allocated
!                       If not specified, the default action is to allocate 
!                       the output Aerosol structure to the dimensions specified
!                       in the input data file.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Aerosol:        Structure containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Aerosols:     The actual number of aerosol profiles read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs:
!       - the input file is closed and,
!       - the output Aerosol structure is deallocated.
!
! COMMENTS:
!       Note the INTENT on the output Aerosol argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_Aerosol_Binary( Filename     , &  ! Input
                                     Aerosol      , &  ! Output
                                     No_File_Close, &  ! Optional input
                                     No_Allocate  , &  ! Optional input
                                     n_Aerosols   , &  ! Optional output
                                     RCS_Id       , &  ! Revision control
                                     Message_Log  ) &  ! Error messaging
                                   RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: Aerosol(:)
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER,       OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Aerosols
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Aerosol_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: m, n_Input_Aerosols

    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    
    ! Check that the file is open. If not, open it.
    ! Otherwise get its FileID.
    IF ( .NOT. File_Open( FileName ) ) THEN

      ! Check that the file exists
      IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
        Message = 'File '//TRIM( Filename )//' not found.'
        GOTO 2000  ! Clean up
      END IF 

      ! Open the file
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )

      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 2000  ! Clean up
      END IF

    END IF

    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == 1 ) Yes_File_Close = .FALSE.
    END IF


    ! Read the number of aerosols dimension
    ! -------------------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Input_Aerosols
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Aerosols data dimension from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check if n_Aerosols > size of output array
    IF ( n_Input_Aerosols > SIZE( Aerosol ) ) THEN
      WRITE( Message, '( "Number of aerosols, ", i0, " > size of the output Aerosol ", &
                        &"structure array, ", i0, "." )' ) &
                      n_Input_Aerosols, SIZE( Aerosol )
      GOTO 1000  ! Clean up
    END IF


    ! Read the aerosol data
    ! ---------------------
    Aerosol_Loop: DO m = 1, n_Input_Aerosols

      Error_Status = Read_Aerosol_Record( FileID, &
                                          Aerosol(m), &
                                          No_Allocate = No_Allocate, &
                                          Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Aerosol element #", i0, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF

    END DO Aerosol_Loop


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT( n_Aerosols ) ) n_Aerosols = n_Input_Aerosols

 
    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT = IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Aerosol( Aerosol, &
                                           Message_Log = Message_Log )

  END FUNCTION CRTM_Read_Aerosol_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_Aerosol_Binary
!
! PURPOSE:
!       Function to write Binary format Aerosol files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Aerosol_Binary( Filename,                     &  ! Input
!                                                 Aerosol,                       &  ! Input
!                                                 No_File_Close = No_File_Close, &  ! Optional input
!                                                 RCS_Id        = RCS_Id,        &  ! Revision control
!                                                 Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of an output
!                       Aerosol format data file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Aerosol:        Structure containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Write_Aerosol_Binary( Filename     , &  ! Input
                                      Aerosol      , &  ! Input
                                      No_File_Close, &  ! Optional input
                                      RCS_Id       , &  ! Revision control
                                      Message_Log  ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol(:)
    INTEGER,       OPTIONAL, INTENT(IN)  :: No_File_Close
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Aerosol_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m
 
    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
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

      ! Inquire for the logical unit number
      INQUIRE( FILE = Filename, NUMBER = FileID )
      
      ! Ensure it's valid
      IF ( FileID == -1 ) THEN
        Message = 'Error inquiring '//TRIM( Filename )//' for its FileID'
        GOTO 1000  ! Clean up
      END IF
      
    END IF

    ! Check the Aerosol structure dimensions
    IF ( ANY( Aerosol%n_Layers < 1 )  ) THEN 
      Message = 'Dimensions of some Aerosol structures is < or = 0.'
      GOTO 1000
    END IF

    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == 1 ) Yes_File_Close = .FALSE.
    END IF


    ! Write the number of aerosols dimension
    ! --------------------------------------    
    WRITE( FileID, IOSTAT = IO_Status ) SIZE( Aerosol )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Aerosols data dimension to ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 2000
    END IF


    ! Write the aerosol data
    ! ---------------------- 
    Aerosol_Loop: DO m = 1, SIZE( Aerosol )
      Error_Status = Write_Aerosol_Record( FileID, &
                                           Aerosol(m), &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Aerosol element #", i0, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 2000
      END IF
    END DO Aerosol_Loop


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT = IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              WARNING, &
                              Message_Log = Message_Log )
      END IF
    END IF

    !=====
    RETURN
    !=====


    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID, IOSTAT = IO_Status )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )

  END FUNCTION CRTM_Write_Aerosol_Binary

END MODULE CRTM_Aerosol_Binary_IO
