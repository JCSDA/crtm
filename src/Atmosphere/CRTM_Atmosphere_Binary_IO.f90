!
! CRTM_Atmosphere_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Atmosphere files.
!
! This module is primarily used for testing purposes only. Eventually 
! it will be removed from the CRTM library.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Atmosphere_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,          ONLY: fp=>fp_kind
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters,     ONLY: ZERO, ONE, SET, YES
  USE CRTM_Atmosphere_Define
  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_Atmosphere_Binary
  PUBLIC :: CRTM_Read_Atmosphere_Binary
  PUBLIC :: CRTM_Write_Atmosphere_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Read_Atmosphere_Binary
    MODULE PROCEDURE Read_Atmosphere_Scalar
    MODULE PROCEDURE Read_Atmosphere_Rank1
  END INTERFACE CRTM_Read_Atmosphere_Binary

  INTERFACE CRTM_Write_Atmosphere_Binary
    MODULE PROCEDURE Write_Atmosphere_Scalar
    MODULE PROCEDURE Write_Atmosphere_Rank1
  END INTERFACE CRTM_Write_Atmosphere_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Atmosphere_Binary_IO.f90,v 2.4 2006/05/25 19:34:49 wd20pd Exp $'


CONTAINS



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Read_Atmosphere_Record( FileID,       &  ! Input
                                   Atmosphere,   &  ! Output
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    INTEGER,                     INTENT(IN)     :: FileID
    TYPE(CRTM_Atmosphere_type),  INTENT(IN OUT) :: Atmosphere
    CHARACTER(*),      OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Record)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: Filename
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: n_Layers 
    INTEGER :: n_Absorbers
    INTEGER :: n_Clouds
    INTEGER :: n_Aerosols


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ------------------------
    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Layers, &
                                       n_Absorbers, &
                                       n_Clouds, &
                                       n_Aerosols
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Atmosphere data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ---------------------------------
    ! Allocate the Atmosphere structure
    ! ---------------------------------
    Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &
                                             n_Absorbers, &
                                             n_Clouds, &
                                             n_Aerosols, &
                                             Atmosphere, &
                                             Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error allocating Atmosphere data structure.'
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------------
    ! Read the level temperature flag
    ! -------------------------------
    READ( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Temperature_Input
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Atmosphere level temperature flag. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ------------------------------------------------
    ! Read the climatology model flag and absorber IDs
    ! ------------------------------------------------
    READ( FileID, IOSTAT = IO_Status ) Atmosphere%Climatology, &
                                       Atmosphere%Absorber_ID, &
                                       Atmosphere%Absorber_Units

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading Atmosphere climatology and absorber IDs. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ---------------------------------
    ! Read the atmospheric profile data
    ! ---------------------------------
    READ( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Pressure, &
                                       Atmosphere%Pressure, &
                                       Atmosphere%Temperature, &
                                       Atmosphere%Absorber
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading atmospheric profile data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! --------------------------------------------
    ! Read the level temperature data if necessary
    ! --------------------------------------------
    IF ( Atmosphere%Level_Temperature_Input == YES ) THEN
      READ( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Temperature
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading level temperature profile data. IOSTAT = ", i5 )' ) &
                        IO_Status
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! -------------------
    ! Read the cloud data
    ! -------------------
    IF ( n_Clouds > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT = FileID, NAME = Filename )
      ! Read the cloud data
      Error_Status = CRTM_Read_Cloud_Binary( Filename, &
                                             Atmosphere%Cloud, &
                                             No_File_Close = SET, &
                                             No_Allocate   = SET, &
                                             Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading Atmosphere Cloud(s)'
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! ---------------------
    ! Read the aerosol data
    ! ---------------------
    IF ( n_Aerosols > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT = FileID, NAME = Filename )
      ! Read the aerosol data
      Error_Status = CRTM_Read_Aerosol_Binary( Filename, &
                                               Atmosphere%Aerosol, &
                                               No_File_Close = SET, &
                                               No_Allocate   = SET, &
                                               Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading Atmosphere Aerosol(s)'
        GOTO 1000  ! Clean up
      END IF

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
    Destroy_Status = CRTM_Destroy_Atmosphere( Atmosphere, &
                                              Message_Log = Message_Log )
    CLOSE( FileID, IOSTAT = IO_Status )

  END FUNCTION Read_Atmosphere_Record


  FUNCTION Write_Atmosphere_Record( FileID,       &  ! Input
                                    Atmosphere,   &  ! Input
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    INTEGER,                     INTENT(IN)  :: FileID
    TYPE(CRTM_Atmosphere_type),  INTENT(IN)  :: Atmosphere
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_CRTM_Atmosphere_Binary(Record)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    CHARACTER( 256 ) :: Filename
    INTEGER :: IO_Status
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS



    ! ------------------------------------------
    ! Check structure pointer association status
    ! ------------------------------------------
    IF ( .NOT. CRTM_Associated_Atmosphere( Atmosphere, &
                                           Skip_Cloud   = SET, &
                                           Skip_Aerosol = SET  ) ) THEN
      Message = 'Some or all INPUT Atmosphere pointer members are NOT associated.'
      GOTO 1000
    END IF


    ! -------------------------
    ! Write the data dimensions
    ! -------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%n_Layers, &
                                        Atmosphere%n_Absorbers, &
                                        Atmosphere%n_Clouds, &
                                        Atmosphere%n_Aerosols
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Atmosphere data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! --------------------------------
    ! Write the level temperature flag
    ! --------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Temperature_Input
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Atmosphere level temperature flag. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------------------------------
    ! Write the climatology model flag and absorber IDs
    ! -------------------------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Climatology, &
                                        Atmosphere%Absorber_ID, &
                                        Atmosphere%Absorber_Units
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing Atmosphere climatology and absorber IDs. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF



    ! ----------------------------------
    ! Write the atmospheric profile data
    ! ----------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Pressure, &
                                        Atmosphere%Pressure, &
                                        Atmosphere%Temperature, &
                                        Atmosphere%Absorber
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing atmospheric profile data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ---------------------------------------------
    ! Write the level temperature data if necessary
    ! ---------------------------------------------
    IF ( Atmosphere%Level_Temperature_Input == YES ) THEN
      WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Temperature
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing level temperature profile data. IOSTAT = ", i5 )' ) &
                        IO_Status
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! --------------------
    ! Write the cloud data
    ! --------------------
    IF ( Atmosphere%n_Clouds > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT = FileID, NAME = Filename )
      ! Write the cloud data
      Error_Status = CRTM_Write_Cloud_Binary( Filename, &
                                              Atmosphere%Cloud, &
                                              No_File_Close = SET, &
                                              Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing Atmosphere Cloud(s)'
        GOTO 1000  ! Clean up
      END IF
    END IF


    ! ---------------------
    ! Read the aerosol data
    ! ---------------------
    IF ( Atmosphere%n_Aerosols > 0 ) THEN
      ! Get the data filename
      INQUIRE( UNIT = FileID, NAME = Filename )
      ! Read the aerosol data
      Error_Status = CRTM_Write_Aerosol_Binary( Filename, &
                                                Atmosphere%Aerosol, &
                                                No_File_Close = SET, &
                                                Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing Atmosphere Aerosol(s)'
        GOTO 1000  ! Clean up
      END IF

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

  END FUNCTION Write_Atmosphere_Record





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
!       CRTM_Inquire_Atmosphere_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Atmosphere structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Atmosphere_Binary( Filename,                 &  ! Input
!                                                      n_Profiles  = n_Profiles, &  ! Optional output
!                                                      RCS_Id      = RCS_Id,     &  ! Revision control
!                                                      Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Profiles:   The number of atmospheric profiles in the data file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file inquire was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Atmosphere_Binary( Filename,     &  ! Input
                                           n_Profiles,   &  ! Optional output
                                           RCS_Id,       &  ! Revision control
                                           Message_Log ) &  ! Error messaging
                                         RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                INTENT(IN)  :: Filename
    INTEGER,           OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*),      OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),      OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Atmosphere_Binary'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Profiles_in_File
 

    ! ------
    ! Set up
    ! ------
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


    ! -------------
    ! Open the file
    ! -------------
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


    ! ---------------------------
    ! Read the number of profiles
    ! ---------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Profiles_in_File
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Profiles data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF

    ! Save the number of profiles
    IF ( PRESENT( n_Profiles ) ) n_Profiles = n_Profiles_in_File


    ! --------------
    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION CRTM_Inquire_Atmosphere_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Read_Atmosphere_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Atmosphere structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Atmosphere_Binary( Filename,                 &  ! Input
!                                                   Atmosphere,               &  ! output
!                                                   Quiet       = Quiet,      &  ! Optional input
!                                                   n_Profiles  = n_Profiles, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,     &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this argument to suppress INFORMATION messages
!                     being printed to standard output (or the message
!                     log file if the Message_Log optional argument is
!                     used.) By default, INFORMATION messages are printed.
!                     If QUIET = 0, INFORMATION messages are OUTPUT.
!                        QUIET = 1, INFORMATION messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   Structure containing the Atmosphere data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Profiles:   The actual number of profiles read in.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! COMMENTS:
!       Note the INTENT on the output Atmosphere argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_Atmosphere_Scalar( Filename,     &  ! Input
                                   Atmosphere,   &  ! Output
                                   Quiet,        &  ! Optional input
                                   n_Profiles,   &  ! Optional output
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)     :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN OUT) :: Atmosphere
    INTEGER,          OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,          OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),     OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Scalar)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: n_Input_Profiles
    INTEGER :: n_Profiles_Read
    TYPE(CRTM_Atmosphere_type) :: Dummy_Atmosphere
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'File '//TRIM( Filename )//' not found.'
      GOTO 2000  ! Clean up
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 2000  ! Clean up
    END IF


    ! ---------------------------
    ! Read the number of profiles      
    ! ---------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Input_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Profiles data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Issue warning message if n_Profiles > 1
    IF ( n_Input_Profiles > 1 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of profiles > 1 and output Atmosphere structure '//&
                            'is scalar. Only the first Atmosphere structure will be read.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------
    ! Initialize profiles read
    ! ------------------------
    n_Profiles_Read = 0


    ! ----------------------------------------------
    ! Read the structure data into a dummy structure
    ! ----------------------------------------------
    Error_Status = Read_Atmosphere_Record( FileID, &
                                           Dummy_Atmosphere, &
                                           Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading Atmosphere record from '//TRIM( Filename )
      GOTO 2000  ! Clean up (file already closed)
    END IF


    ! ------------------------------
    ! Copy dummy structure to output
    ! only if it contains valid data
    ! ------------------------------
    IF ( Dummy_Atmosphere%n_Layers    > 1 .OR. &
         Dummy_Atmosphere%n_Absorbers > 1      ) THEN
      ! Copy the data into the output array
      Error_Status = CRTM_Assign_Atmosphere( Dummy_Atmosphere, &
                                             Atmosphere, &
                                             Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error copying Atmosphere structure.'
        GOTO 1000  ! Clean up
      END IF
      ! Set value for the number of profiles read
      n_Profiles_Read = 1
    END IF


    ! ---------------------------
    ! Destroy the dummy structure
    ! ---------------------------
    Error_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying dummy Atmosphere structure.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    ! --------------------------------
    ! Save the number of profiles read
    ! --------------------------------
    IF ( PRESENT( n_Profiles ) ) n_Profiles = n_Profiles_Read


    ! --------------
    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles read from ", a, ": ", i5 )' ) &
                      TRIM( Filename ), n_Profiles_Read
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#

    1000 CONTINUE
    CLOSE( FileID )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Atmosphere( Atmosphere, Dummy_Atmosphere, &
                                              Message_Log = Message_Log )

  END FUNCTION Read_Atmosphere_Scalar


  FUNCTION Read_Atmosphere_Rank1( Filename,     &  ! Input
                                  Atmosphere,   &  ! Output
                                  Quiet,        &  ! Optional input
                                  n_Profiles,   &  ! Optional output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                             INTENT(IN)     :: Filename
    TYPE(CRTM_Atmosphere_type), DIMENSION(:), INTENT(IN OUT) :: Atmosphere
    INTEGER,                    OPTIONAL,     INTENT(IN)     :: Quiet
    INTEGER,                    OPTIONAL,     INTENT(OUT)    :: n_Profiles
    CHARACTER(*),               OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),               OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Rank-1)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: m, n_Input_Profiles, n_Profiles_Read
    TYPE(CRTM_Atmosphere_type) :: Dummy_Atmosphere
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'File '//TRIM( Filename )//' not found.'
      GOTO 2000  ! Clean up
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 2000  ! Clean up
    END IF


    ! ---------------------------
    ! Read the number of profiles      
    ! ---------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Input_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Profiles data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Issue warning message if n_Profiles > size of output array
    IF ( n_Input_Profiles > SIZE( Atmosphere ) ) THEN
      WRITE( Message, '( "Number of profiles, ", i5, " > size of the output Atmosphere ", &
                        &"structure array, ", i5, ". Only the first ", i5, &
                        &" Atmosphere structures will be read." )' ) &
                      n_Input_Profiles, SIZE( Atmosphere ), SIZE( Atmosphere )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      n_Input_Profiles = SIZE( Atmosphere )
    END IF


    ! ------------------------
    ! Initialize profiles read
    ! ------------------------
    n_Profiles_Read = 0


    ! --------------------------------------------------------
    ! Loop over all the profiles (even potentially empty ones)
    ! --------------------------------------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles


      ! Read the structure data into a dummy structure
      Error_Status = Read_Atmosphere_Record( FileID, &
                                             Dummy_Atmosphere, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Atmosphere element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000  ! Clean up
      END IF

      ! Copy dummy structure to output array
      ! only if it contains valid data.
      IF ( Dummy_Atmosphere%n_Layers    > 1 .OR. &
           Dummy_Atmosphere%n_Absorbers > 1      ) THEN
        ! Increment profiles read
        n_Profiles_Read = n_Profiles_Read + 1
        ! Copy the data into the output array
        Error_Status = CRTM_Assign_Atmosphere( Dummy_Atmosphere, &
                                               Atmosphere( n_Profiles_Read ), &
                                               Message_Log = Message_Log )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error copying Atmosphere element #", i5, "." )' ) m
          GOTO 1000  ! Clean up
        END IF
      END IF

      ! Destroy the dummy structure
      Error_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere, &
                                              Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying dummy Atmosphere structure at profile #", i5, "." )' ) m
        GOTO 1000  ! Clean up
      END IF
    END DO Profile_Loop


    ! --------------------------------
    ! Save the number of profiles read
    ! --------------------------------
    IF ( PRESENT( n_Profiles ) ) n_Profiles = n_Profiles_Read


    ! --------------
    ! Close the file
    ! --------------
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


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles read from ", a, ": ", i5 )' ) &
                      TRIM( Filename ), n_Profiles_Read
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

    RETURN



    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#
    !#                      -= CLEAN UP AFTER AN ERROR -=                       #
    !#-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=#


    1000 CONTINUE
    CLOSE( FileID )

    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( Message ), &
                          Error_Status, &
                          Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Atmosphere( Atmosphere, &
                                              Message_Log = Message_Log )
    Destroy_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere, &
                                              Message_Log = Message_Log )

  END FUNCTION Read_Atmosphere_Rank1


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_Atmosphere_Binary
!
! PURPOSE:
!       Function to write Binary format Atmosphere files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Atmosphere_Binary( Filename,                 &  ! Input
!                                                    Atmosphere,               &  ! Input
!                                                    Quiet       = Quiet,      &  ! Optional input
!                                                    RCS_Id      = RCS_Id,     &  ! Revision control
!                                                    Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Atmosphere format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Atmosphere:   Structure containing the Atmosphere data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this argument to suppress INFORMATION messages
!                     being printed to standard output (or the message
!                     log file if the Message_Log optional argument is
!                     used.) By default, INFORMATION messages are printed.
!                     If QUIET = 0, INFORMATION messages are OUTPUT.
!                        QUIET = 1, INFORMATION messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
!------------------------------------------------------------------------------

  FUNCTION Write_Atmosphere_Scalar( Filename,     &  ! Input
                                    Atmosphere,   &  ! Input
                                    Quiet,        &  ! Optional input
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), INTENT(IN)  :: Atmosphere
    INTEGER,          OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),     OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),     OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(Scalar)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Output_Profiles
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check the Atmosphere structure dimensions
    IF ( Atmosphere%n_Layers    < 1 .OR. &
         Atmosphere%n_Absorbers < 1      ) THEN
      Message = 'n_Layers or n_Absorbers dimension of Atmosphere structure are < or = 0.'
      GOTO 1000
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF


    ! ----------------------------
    ! Write the number of profiles
    ! ----------------------------
    n_Output_Profiles = 1
    WRITE( FileID, IOSTAT = IO_Status ) n_Output_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Profiles data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    ! ------------------------
    ! Write the structure data
    ! ------------------------
    Error_Status = Write_Atmosphere_Record( FileID, &
                                            Atmosphere, &
                                            Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Atmosphere record to '//TRIM( Filename )
      GOTO 1000
    END IF


    ! --------------
    ! Close the file
    ! --------------
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


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles written to ", a, ": ", i5 )' ) &
                      TRIM( Filename ), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
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

  END FUNCTION Write_Atmosphere_Scalar


  FUNCTION Write_Atmosphere_Rank1( Filename,     &  ! Input
                                   Atmosphere,   &  ! Input
                                   Quiet,        &  ! Optional input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                               INTENT(IN)  :: Filename
    TYPE(CRTM_Atmosphere_type), DIMENSION(:), INTENT(IN)  :: Atmosphere
    INTEGER,                      OPTIONAL,       INTENT(IN)  :: Quiet
    CHARACTER(*),               OPTIONAL,       INTENT(OUT) :: RCS_Id
    CHARACTER(*),               OPTIONAL,       INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(Rank-1)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Output_Profiles
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! How many valid profiles?
    n_Output_Profiles = COUNT( Atmosphere%n_Layers    > 0 .AND. &
                               Atmosphere%n_Absorbers > 0       )
    IF ( n_Output_Profiles < 1 ) THEN
      Message = 'No profiles with non-zero dimensions!'
      GOTO 1000
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF


    ! ---------------------------- 
    ! Write the number of profiles
    ! ----------------------------
    WRITE( FileID, IOSTAT = IO_Status ) n_Output_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Profiles data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    ! --------------------------------------------------------
    ! Loop over all the profiles (even potentially empty ones)
    ! --------------------------------------------------------
    Profile_Loop: DO m = 1, SIZE( Atmosphere )

      ! Check the Atmosphere structure dimensions
      IF ( Atmosphere(m)%n_Layers    < 1 .OR. &
           Atmosphere(m)%n_Absorbers < 1      ) CYCLE Profile_Loop

      ! Write the structure data
      Error_Status = Write_Atmosphere_Record( FileID, &
                                              Atmosphere(m), &
                                              Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Atmosphere element #", i5, " to ", a )' ) &
                      m, TRIM( Filename )
        GOTO 1000
      END IF

    END DO Profile_Loop


    ! --------------
    ! Close the file
    ! --------------
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


    ! ----------------------
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles written to ", a, ": ", i5 )' ) &
                      TRIM( Filename ), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( MEssage ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
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

  END FUNCTION Write_Atmosphere_Rank1

END MODULE CRTM_Atmosphere_Binary_IO
