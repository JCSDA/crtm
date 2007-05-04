!
! CRTM_Cloud_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Cloud files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Cloud_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds
  USE File_Utility,        ONLY: File_Open, File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Cloud_Define  , ONLY: CRTM_Cloud_type, &
                                 CRTM_Associated_Cloud, &
                                 CRTM_Destroy_Cloud, &
                                 CRTM_Allocate_Cloud
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Inquire_Cloud_Binary
  PUBLIC :: CRTM_Read_Cloud_Binary
  PUBLIC :: CRTM_Write_Cloud_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
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

  ! -------------------------------------------
  ! Function to read a single cloud data record
  ! -------------------------------------------
  FUNCTION Read_Cloud_Record( FileID     , &  ! Input
                              Cloud      , &  ! Output
                              No_Allocate, &  ! Optional input
                              Message_Log) &  ! Error messaging
                            RESULT ( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)     :: FileID
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Allocate
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Cloud_Binary(Record)'
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
    READ( FileID, IOSTAT=IO_Status ) n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Cloud data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Allocate the structure if required
    ! ----------------------------------
    IF ( Yes_Allocate ) THEN

      ! Perform the allocation
      Error_Status = CRTM_Allocate_Cloud( n_Layers, &
                                          Cloud, &
                                          Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error allocating Cloud structure.'
        GOTO 1000
      END IF
      
    ELSE
    
      ! Structure already allocated. Check the association status
      IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) THEN
        Message = 'Cloud structure components are not associated.'
        GOTO 1000  ! Clean up
      END IF
      
      ! Check the dimension values
      IF ( n_Layers /= Cloud%n_Layers ) THEN
        WRITE( Message, '( "Cloud data dimensions, ", i0, &
                          &" are inconsistent with structure definition, ", i0, "." )' ) &
                        n_Layers, Cloud%n_Layers
        GOTO 1000  ! Clean up
      END IF
    END IF

    ! Read the cloud data
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) Cloud%Type, &
                                     Cloud%Effective_Radius, &
                                     Cloud%Effective_Variance, &
                                     Cloud%Water_Content
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Cloud data. IOSTAT = ", i0 )' ) &
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
    IF ( CRTM_Associated_Cloud(Cloud) ) THEN
      Destroy_Status = CRTM_Destroy_Cloud( Cloud, Message_Log=Message_Log )
    END IF
    CLOSE( FileID, IOSTAT=IO_Status )

  END FUNCTION Read_Cloud_Record


  ! --------------------------------------------
  ! Function to write a single cloud data record
  ! ---------------------------------------------
  FUNCTION Write_Cloud_Record( FileID     , &  ! Input
                               Cloud      , &  ! Input
                               Message_Log) &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    INTEGER,                INTENT(IN)  :: FileID
    TYPE(CRTM_Cloud_type),  INTENT(IN)  :: Cloud
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Cloud_Binary(Record)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
 
    ! Set up
    ! -----
    Error_Status = SUCCESS
    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) THEN
      Message = 'Some or all INPUT Cloud pointer members are NOT associated.'
      GOTO 1000
    END IF


    ! Write the dimensions
    ! --------------------
    WRITE( FileID, IOSTAT=IO_Status ) Cloud%n_Layers
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Cloud data dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the data
    ! --------------
    WRITE( FileID, IOSTAT=IO_Status ) Cloud%Type, &
                                      Cloud%Effective_Radius, &
                                      Cloud%Effective_Variance, &
                                      Cloud%Water_Content
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Cloud data. IOSTAT = ", i0 )' ) &
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

  END FUNCTION Write_Cloud_Record


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
!       CRTM_Inquire_Cloud_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Cloud structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Cloud_Binary( Filename,                 &  ! Input
!                                                 n_Clouds    = n_Clouds,   &  ! Optional output
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
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
!       n_Clouds:       The number of cloud profiles in the data file.
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
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the Binary file inquire was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Cloud_Binary( Filename,     &  ! Input
                                      n_Clouds,     &  ! Optional output
                                      RCS_Id,       &  ! Revision control
                                      Message_Log ) &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Clouds
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Cloud_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Clouds_in_File

    ! Set up
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Open the Cloud data file
    ! ------------------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    
    
    ! Read the dimensions
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) n_Clouds_in_File
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Clouds data dimension from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
      CLOSE( FileID, IOSTAT=IO_Status )
      RETURN
    END IF

    ! Assign the return arguments
    IF ( PRESENT( n_Clouds ) ) n_Clouds = n_Clouds_in_File


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

  END FUNCTION CRTM_Inquire_Cloud_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Read_Cloud_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Cloud structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Cloud_Binary( Filename,                      &  ! Input
!                                              Cloud,                         &  ! output
!                                              No_File_Close = No_File_Close, &  ! Optional input
!                                              No_Allocate   = No_Allocate,   &  ! Optional input
!                                              n_Clouds      = n_Clouds,      &  ! Optional output
!                                              RCS_Id        = RCS_Id,        &  ! Revision control
!                                              Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
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
!       No_Allocate:    Set this argument to NOT allocate the output Cloud
!                       structure in this routine based on the data dimensions
!                       read from the input data file. This assumes that the
!                       structure has already been allocated prior to calling 
!                       this function.
!                       If == 0, the output Cloud structure is allocated [DEFAULT]
!                          == 1, the output Cloud structure is NOT allocated
!                       If not specified, the default action is to allocate 
!                       the output Cloud structure to the dimensions specified
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
!       Cloud:          Structure containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
!                       DIMENSION:  Scalar or Rank-1
!                       ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Clouds:       The actual number of clouds read in.
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
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the Binary file read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_Cloud_Binary( Filename     , &  ! Input
                                   Cloud        , &  ! Output
                                   No_File_Close, &  ! Optional input
                                   No_Allocate  , &  ! Optional input
                                   n_Clouds     , &  ! Optional output
                                   RCS_Id       , &  ! Revision control
                                   Message_Log  ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    TYPE(CRTM_Cloud_type),  INTENT(IN OUT) :: Cloud(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER,      OPTIONAL, INTENT(OUT)    :: n_Clouds
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Cloud_Binary'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: m, n_Input_Clouds

    ! Set up
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
                                       Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error opening '//TRIM( Filename )
        GOTO 2000  ! Clean up
      END IF

    ELSE

      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename, NUMBER=FileID )

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


    ! Read the dimensions
    ! -------------------
    READ( FileID, IOSTAT=IO_Status ) n_Input_Clouds
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Clouds data dimension from ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF
    
    ! Check if n_Clouds > size of output array
    IF ( n_Input_Clouds > SIZE( Cloud ) ) THEN
      WRITE( Message, '( "Number of clouds, ", i0, " > size of the output Cloud ", &
                        &"structure array, ", i0, "." )' ) &
                      n_Input_Clouds, SIZE( Cloud )
      GOTO 1000  ! Clean up
    END IF


    ! Read the cloud data
    ! -------------------
    Cloud_Loop: DO m = 1, n_Input_Clouds
    
      Error_Status = Read_Cloud_Record( FileID, &
                                        Cloud(m), &
                                        No_Allocate = No_Allocate, &
                                        Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Cloud element #", i0, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF
      
    END DO Cloud_Loop


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT( n_Clouds ) ) n_Clouds = n_Input_Clouds


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
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
    Destroy_Status = CRTM_Destroy_Cloud( Cloud, Message_Log=Message_Log )

  END FUNCTION CRTM_Read_Cloud_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_Cloud_Binary
!
! PURPOSE:
!       Function to write Binary format Cloud files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Cloud_Binary( Filename,                      &  ! Input
!                                               Cloud,                         &  ! Input
!                                               No_File_Close = No_File_Close, &  ! Optional input
!                                               RCS_Id        = RCS_Id,        &  ! Revision control
!                                               Message_Log   = Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of an output
!                       Cloud format data file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Cloud:          Structure containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
!                       DIMENSION:  Scalar or Rank-1
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
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the Binary file write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during writing, the output file is deleted
!         before returning to the calling routine.
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Write_Cloud_Binary( Filename,      &  ! Input
                                    Cloud,         &  ! Input
                                    No_File_Close, &  ! Optional input
                                    RCS_Id,        &  ! Revision control
                                    Message_Log )  &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type),  INTENT(IN)  :: Cloud(:)
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_File_Close
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Cloud_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(256) :: Message
    LOGICAL :: Yes_File_Close
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m
 
    ! Set up
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file is open. If not, open it.
    ! Otherwise get the file ID.
    IF ( .NOT. File_Open( FileName ) ) THEN
    
      Error_Status = Open_Binary_File( TRIM( Filename ), &
                                       FileID, &
                                       For_Output  = 1, &
                                       Message_Log=Message_Log )

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

    ! Check the Cloud structure dimensions
    IF ( ANY( Cloud%n_Layers < 1 ) ) THEN
      Message = 'Dimensions of some Cloud structures is < or = 0.'
      GOTO 1000
    END IF

    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT( No_File_Close ) ) THEN
      IF ( No_File_Close == 1 ) Yes_File_Close = .FALSE.
    END IF

    ! Write the number of clouds
    ! --------------------------
    WRITE( FileID, IOSTAT=IO_Status ) SIZE( Cloud )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Clouds data dimension to ", a, &
                        &". IOSTAT = ", i0 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS=FILE_STATUS_ON_ERROR )
      GOTO 2000
    END IF


    ! Write the cloud data
    ! --------------------
    Cloud_Loop: DO m = 1, SIZE( Cloud )
      Error_Status = Write_Cloud_Record( FileID, &
                                         Cloud(m), &
                                         Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Cloud element #", i0, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 2000
      END IF
    END DO Cloud_Loop


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( FileID, IOSTAT=IO_Status )
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                        TRIM( Filename ), IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(Message), &
                              WARNING, &
                              Message_Log=Message_Log )
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

  END FUNCTION CRTM_Write_Cloud_Binary

END MODULE CRTM_Cloud_Binary_IO
