!
! CRTM_Surface_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Surface files.
!
! This module is primarily used for testing purposes only. Eventually 
! it will be removed from the CRTM library.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 21-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Surface_Binary_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds         , ONLY: fp
  USE File_Utility       , ONLY: File_Exists
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: ZERO, ONE, SET
  USE CRTM_Surface_Define
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Only the module routines are public
  PUBLIC :: CRTM_Inquire_Surface_Binary
  PUBLIC :: CRTM_Read_Surface_Binary
  PUBLIC :: CRTM_Write_Surface_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE CRTM_Read_Surface_Binary
    MODULE PROCEDURE Read_Surface_Rank1
    MODULE PROCEDURE Read_Surface_Rank2
  END INTERFACE CRTM_Read_Surface_Binary

  INTERFACE CRTM_Write_Surface_Binary
    MODULE PROCEDURE Write_Surface_Rank1
    MODULE PROCEDURE Write_Surface_Rank2
  END INTERFACE CRTM_Write_Surface_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! ---------------------------------------------
  ! Function to read a single surface data record
  ! ---------------------------------------------
  FUNCTION Read_Surface_Record( FileID     , &  ! Input
                                Surface    , &  ! Output
                                Message_Log) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN)     :: FileID
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: Allocate_Status
    INTEGER :: Type_in_File
    INTEGER :: Type_by_Coverage
    REAL(fp) :: Total_Coverage
    INTEGER :: n_Channels

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Read the gross surface type and check it
    ! ----------------------------------------
    READ( FileID, IOSTAT=IO_Status ) Type_in_File, &
                                     Surface%Land_Coverage, &
                                     Surface%Water_Coverage, &
                                     Surface%Snow_Coverage, &
                                     Surface%Ice_Coverage
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface type and coverage. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Simple check of coverage
    Surface%Land_Coverage  = MAX(Surface%Land_Coverage,  ZERO)
    Surface%Water_Coverage = MAX(Surface%Water_Coverage, ZERO)
    Surface%Snow_Coverage  = MAX(Surface%Snow_Coverage,  ZERO)
    Surface%Ice_Coverage   = MAX(Surface%Ice_Coverage,   ZERO)

    ! Check the total coverage
    Total_Coverage = Surface%Land_Coverage  + &
                     Surface%Water_Coverage + &
                     Surface%Snow_Coverage  + &
                     Surface%Ice_Coverage  
    IF ( Total_Coverage > ONE ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Total coverage fraction sum > 1.0', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF

    ! Compute the coverage type
    Type_by_Coverage = 0
    IF ( Surface%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( Surface%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( Surface%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( Surface%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE

    ! Check the file and coverge surfce types
    IF ( Type_in_File /= Type_by_Coverage ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Coverage surface type, '//TRIM(SURFACE_TYPE_NAME( Type_by_Coverage ))//&
                            ', inconsistent with that specified in file.', &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Read the surface wind speed
    ! ---------------------------
    READ( FileID, IOSTAT=IO_Status ) Surface%Wind_Speed
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface wind speed data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Read the land surface type data
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) Surface%Land_Type, &
                                     Surface%Land_Temperature, &
                                     Surface%Soil_Moisture_Content, &
                                     Surface%Canopy_Water_Content , &
                                     Surface%Vegetation_Fraction, &
                                     Surface%Soil_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading land surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Land_Type < 0 .OR. &
         Surface%Land_Type > N_VALID_LAND_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised land surface type', &
                            Error_Status, &
                            Message_Log=Message_Log )
      Surface%Land_Type = INVALID_LAND
    END IF


    ! Read the water surface type data
    ! --------------------------------
    READ( FileID, IOSTAT=IO_Status ) Surface%Water_Type, &
                                     Surface%Water_Temperature, &
                                     Surface%Wind_Direction, &
                                     Surface%Salinity
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading water surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Water_Type < 0 .OR. Surface%Water_Type > N_VALID_WATER_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised water surface type', &
                            Error_Status, &
                            Message_Log=Message_Log )
      Surface%Water_Type = INVALID_WATER
    END IF


    ! Read the snow surface type data
    ! -------------------------------
    READ( FileID, IOSTAT=IO_Status ) Surface%Snow_Type, &
                                     Surface%Snow_Temperature, &
                                     Surface%Snow_Depth, &
                                     Surface%Snow_Density, &
                                     Surface%Snow_Grain_Size
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading snow surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Snow_Type < 0 .OR. Surface%Snow_Type > N_VALID_SNOW_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised snow surface type', &
                            Error_Status, &
                            Message_Log=Message_Log )
      Surface%Snow_Type = INVALID_SNOW
    END IF


    ! Read the ice surface type data
    ! ------------------------------
    READ( FileID, IOSTAT=IO_Status ) Surface%Ice_Type, &
                                     Surface%Ice_Temperature, &
                                     Surface%Ice_Thickness, &
                                     Surface%Ice_Density, &
                                     Surface%Ice_Roughness
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ice surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Ice_Type < 0 .OR. Surface%Ice_Type > N_VALID_ICE_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised ice surface type', &
                            Error_Status, &
                            Message_Log=Message_Log )
      Surface%Ice_Type = INVALID_ICE
    END IF


    ! Destroy the SensorData structure. This step will be taken
    ! care of in the calling routine, Read_Surface_Binary(), so
    ! the following is a belt-and-braces thing. :o)
    ! ---------------------------------------------------------
    Destroy_Status = CRTM_Destroy_SensorData( Surface%SensorData, &
                                              Message_Log=Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Message = 'Error destroying SensorData structure.'
      GOTO 1000  ! Clean up
    END IF


    ! Read the SensorData dimensions
    ! ------------------------------
    READ( FileID, IOSTAT=IO_Status ) n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading SensorData dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Read the SensorData if required
    ! -------------------------------
    IF ( n_Channels > 0 ) THEN

      ! Allocate the structure
      Allocate_Status = CRTM_Allocate_SensorData( n_Channels, &
                                                  Surface%SensorData, &
                                                  Message_Log=Message_Log )
      IF ( Allocate_Status /= SUCCESS ) THEN
        Message = 'Error allocating SensorData structure.'
        GOTO 1000  ! Clean up
      END IF

      ! Read the Sensor data
      READ( FileID, IOSTAT=IO_Status ) Surface%SensorData%Sensor_ID       , &  
                                       Surface%SensorData%SensorData_ID   , &  
                                       Surface%SensorData%WMO_Satellite_ID, &
                                       Surface%SensorData%WMO_Sensor_ID   , &   
                                       Surface%SensorData%Sensor_Channel  , &
                                       Surface%SensorData%Tb
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SensorData. IOSTAT = ", i0 )' ) IO_Status
        GOTO 1000  ! Clean up
      END IF
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface )
    CLOSE( FileID )

  END FUNCTION Read_Surface_Record


  ! ----------------------------------------------
  ! Function to write a single surface data record
  ! ----------------------------------------------
  FUNCTION Write_Surface_Record( FileID     , &  ! Input
                                 Surface    , &  ! Input
                                 Message_Log) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    INTEGER,                 INTENT(IN) :: FileID
    TYPE(CRTM_Surface_type), INTENT(IN) :: Surface
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: Type_by_Coverage


    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! Write the gross surface type
    ! ----------------------------
    ! Compute the coverage type
    Type_by_Coverage = 0
    IF ( Surface%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( Surface%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( Surface%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( Surface%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE
    WRITE( FileID, IOSTAT=IO_Status ) Type_by_Coverage, &
                                      Surface%Land_Coverage, &
                                      Surface%Water_Coverage, &
                                      Surface%Snow_Coverage, &
                                      Surface%Ice_Coverage
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface type and coverage fractions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the surface wind speed
    ! ----------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%Wind_Speed
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface wind speed data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the land surface type data
    ! --------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%Land_Type, &
                                      Surface%Land_Temperature, &
                                      Surface%Soil_Moisture_Content, &
                                      Surface%Canopy_Water_Content, &
                                      Surface%Vegetation_Fraction, &
                                      Surface%Soil_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing land surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the water surface type data
    ! ---------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%Water_Type, &
                                      Surface%Water_Temperature, &
                                      Surface%Wind_Direction, &
                                      Surface%Salinity
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing water surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the snow surface type data
    ! --------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%Snow_Type, &
                                      Surface%Snow_Temperature, &
                                      Surface%Snow_Depth, &
                                      Surface%Snow_Density, &
                                      Surface%Snow_Grain_Size
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing snow surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the ice surface type data
    ! -------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%Ice_Type, &
                                      Surface%Ice_Temperature, &
                                      Surface%Ice_Thickness, &
                                      Surface%Ice_Density, &
                                      Surface%Ice_Roughness
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ice surface type data. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the SensorData dimensions
    ! -------------------------------
    WRITE( FileID, IOSTAT=IO_Status ) Surface%SensorData%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing SensorData dimensions. IOSTAT = ", i0 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! Write the SensorData
    ! --------------------
    IF ( Surface%SensorData%n_Channels > 0 ) THEN
      WRITE( FileID, IOSTAT=IO_Status ) Surface%SensorData%Sensor_ID       , &
                                        Surface%SensorData%SensorData_ID   , &
                                        Surface%SensorData%WMO_Satellite_ID, &
                                        Surface%SensorData%WMO_Sensor_ID   , &
                                        Surface%SensorData%Sensor_Channel  , &
                                        Surface%SensorData%Tb
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SensorData. IOSTAT = ", i0 )' ) IO_Status
        GOTO 1000  ! Clean up
      END IF
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    CLOSE( FileID, STATUS=WRITE_ERROR_STATUS )

  END FUNCTION Write_Surface_Record





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
!       CRTM_Inquire_Surface_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Surface structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Surface_Binary( Filename               , &  ! Input
!                                                   n_Channels =n_Channels , &  ! Optional output
!                                                   n_Profiles =n_Profiles , &  ! Optional output
!                                                   RCS_Id     =RCS_Id     , &  ! Revision control
!                                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
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
!       n_Channels:   The number of spectral channels for which there is
!                     data in the file. Note that this value will always
!                     be 0 for a profile-only dataset-- it only has meaning
!                     for K-matrix data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of profiles in the data file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the Binary inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Surface_Binary( Filename   , &  ! Input
                                        n_Channels , &  ! Optional output
                                        n_Profiles , &  ! Optional output
                                        RCS_Id     , &  ! Revision control
                                        Message_Log) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Surface_Binary'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Channels_in_File
    INTEGER :: n_Profiles_in_File

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      GOTO 2000  ! Clean up
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000
    END IF


    ! Read the number of profiles
    ! ---------------------------
    CALL Read_Dimensions( Filename, FileID, &
                          n_Channels_in_File, n_Profiles_in_File, &
                          IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT( n_Channels ) ) n_Channels = n_Channels_in_File
    IF ( PRESENT( n_Profiles ) ) n_Profiles = n_Profiles_in_File


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION CRTM_Inquire_Surface_Binary


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Read_Surface_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Surface structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Surface_Binary( Filename               , &  ! Input
!                                                Surface                , &  ! output
!                                                Quiet      =Quiet      , &  ! Optional input
!                                                n_Channels =n_Channels , &  ! Optional output
!                                                n_Profiles =n_Profiles , &  ! Optional output
!                                                RCS_Id     =RCS_Id     , &  ! Revision control
!                                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Surface format data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Surface:      Structure containing the Surface data. Note the
!                     following meanings attributed to the dimensions of
!                     the structure array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
!                     ATTRIBUTES: INTENT(IN OUT)
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
!       n_Channels:   The number of channels for which data was read. Note that
!                     this value will always be 0 for a profile-only dataset--
!                     it only has meaning for K-matrix data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
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
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_Surface_Rank1( Filename    , &  ! Input
                               Surface     , &  ! Output
                               Quiet       , &  ! Optional input
                               n_Channels  , &  ! Optional output
                               n_Profiles  , &  ! Optional output
                               RCS_Id      , &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface(:)  ! M
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: n_File_Channels
    INTEGER :: m, n_File_Profiles, n_Input_Profiles
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      GOTO 2000
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000
    END IF


    ! Read the dimensions     
    ! -------------------
    CALL Read_Dimensions( Filename, FileID, &
                          n_File_Channels, n_File_Profiles, &
                          IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000

    ! Check that n_Channels is zero
    IF ( n_File_Channels /= 0 ) THEN
      WRITE( Message, '("n_Channels dimensions in ",a," is not zero for a rank-1 ",&
                        &"(i.e. profiles only) Surface structure read." )' ) &
                      TRIM(Filename)
      GOTO 1000
    END IF
    
    ! Check if n_Profiles > size of output array
    n_Input_Profiles = SIZE(Surface)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( Message, '( "Number of profiles, ", i0, " > size of the output Surface ", &
                        &"structure array, ", i0, ". Only the first ", i0, &
                        &" Surface structures will be read." )' ) &
                      n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles

      ! Read the structure
      Error_Status = Read_Surface_Record( FileID, &
                                             Surface(m), &
                                             Message_Log=Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Surface element (",i0,") from ", a )' ) &
                        m, TRIM(Filename)
        GOTO 1000  ! Clean up
      END IF

    END DO Profile_Loop


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT(n_Channels) ) n_Channels = 0
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Close the file
    ! --------------
    CLOSE( FileID, IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles read from ", a, ": ", i0 )' ) &
                      TRIM(Filename), n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface, &
                                           Message_Log=Message_Log )
  END FUNCTION Read_Surface_Rank1


  FUNCTION Read_Surface_Rank2( Filename   , &  ! Input
                               Surface    , &  ! Output
                               Quiet      , &  ! Optional input
                               n_Channels , &  ! Optional output
                               n_Profiles , &  ! Optional output
                               RCS_Id     , &  ! Revision control
                               Message_Log) &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface(:,:)  ! L x M
    INTEGER,       OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Channels
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Profiles
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: FileID
    INTEGER :: l, n_File_Channels, n_Input_Channels
    INTEGER :: m, n_File_Profiles, n_Input_Profiles
 

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      Message = 'File '//TRIM(Filename)//' not found.'
      GOTO 2000  ! Clean up
    END IF

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 2000  ! Clean up
    END IF


    ! Read the dimensions     
    ! -------------------
    CALL Read_Dimensions( Filename, FileID, &
                          n_File_Channels, n_File_Profiles, &
                          IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000

    ! Check if n_Channels in file is > size of output array
    n_Input_Channels = SIZE(Surface,1)
    IF ( n_File_Channels > n_Input_Channels ) THEN
      WRITE( Message, '( "Number of channels, ",i0," > size of the output Surface ", &
                        &"structure array dimension, ",i0,". Only the first ",i0, &
                        &" channel Surface structures will be read." )' ) &
                      n_File_Channels, n_Input_Channels, n_Input_Channels
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF
    n_Input_Channels = MIN(n_Input_Channels, n_File_Channels)
    
    ! Check if n_Profiles in file is > size of output array
    n_Input_Profiles = SIZE(Surface,2)
    IF ( n_File_Profiles > n_Input_Profiles ) THEN
      WRITE( Message, '( "Number of profiles, ",i0," > size of the output Surface ", &
                        &"structure array dimension, ",i0,". Only the first ",i0, &
                        &" profile Surface structures will be read." )' ) &
                      n_File_Profiles, n_Input_Profiles, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF
    n_Input_Profiles = MIN(n_Input_Profiles, n_File_Profiles)


    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Input_Profiles
      Channel_Loop: DO l = 1, n_Input_Channels
  
        ! Read the structure
        Error_Status = Read_Surface_Record( FileID, &
                                            Surface(l,m), &
                                            Message_Log=Message_Log )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading Surface element (",i0,",",i0,") from ", a )' ) &
                          l, m, TRIM(Filename)
          GOTO 1000  ! Clean up
        END IF

      END DO Channel_Loop
    END DO Profile_Loop


    ! Save optional return arguments
    ! ------------------------------
    IF ( PRESENT(n_Channels) ) n_Channels = n_Input_Channels
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_Input_Profiles


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '("Number of channels and profiles read from ",a,": ",i0,1x,i0 )' ) &
                      TRIM(Filename), n_Input_Channels, n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    CLOSE( FileID )
    2000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )
    Destroy_Status = CRTM_Destroy_Surface( Surface, &
                                              Message_Log=Message_Log )
  END FUNCTION Read_Surface_Rank2


  ! -------------------------------------------
  ! Utility routine to read the file dimensions
  ! -------------------------------------------
  SUBROUTINE Read_Dimensions( Filename, FileID, &
                              n_Channels, n_Profiles, &
                              IO_Status, Message )
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Filename
    INTEGER,      INTENT(IN)  :: FileID
    INTEGER,      INTENT(OUT) :: n_Channels
    INTEGER,      INTENT(OUT) :: n_Profiles
    INTEGER,      INTENT(OUT) :: IO_Status
    CHARACTER(*), INTENT(OUT) :: Message
    ! Read the dimensions from file    
    READ( FileID, IOSTAT=IO_Status ) n_Channels, n_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error reading data dimensions from ", a, &
                        &". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
    END IF
  END SUBROUTINE Read_Dimensions


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_Surface_Binary
!
! PURPOSE:
!       Function to write Binary format Surface files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Surface_Binary( Filename               , &  ! Input
!                                                 Surface                , &  ! Input
!                                                 Quiet      =Quiet      , &  ! Optional input
!                                                 RCS_Id     =RCS_Id     , &  ! Revision control
!                                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Surface format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      Structure containing the Surface data to write.
!                     Note the following meanings attributed to the
!                     dimensions of the structure array:
!                     Rank-1: M profiles.
!                             Only profile data are to be read in. The file
!                             does not contain channel information. The
!                             dimension of the structure is understood to
!                             be the PROFILE dimension.
!                     Rank-2: L channels  x  M profiles
!                             Channel and profile data are to be read in.
!                             The file contains both channel and profile
!                             information. The first dimension of the 
!                             structure is the CHANNEL dimension, the second
!                             is the PROFILE dimension. This is to allow
!                             K-matrix structures to be read in with the
!                             same function.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Rank-1 (M) or Rank-2 (L x M)
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
!       RCS_Id:       Character string containing the version control Id
!                     field for the module.
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

  FUNCTION Write_Surface_Rank1( Filename   , &  ! Input
                                Surface    , &  ! Input
                                Quiet      , &  ! Optional input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface(:)  ! M
    INTEGER,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(M)'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Get dimensions
    n_Output_Profiles = SIZE(Surface)

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     For_Output =SET, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 1000
    END IF


    ! Write the dimensions
    ! --------------------
    CALL Write_Dimensions( Filename, FileID, 0, n_Output_Profiles, IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000

    
    ! Loop over all the profiles
    ! --------------------------
    Profile_Loop: DO m = 1, n_Output_Profiles

      ! Write the structure data
      Error_Status = Write_Surface_Record( FileID, &
                                           Surface(m), &
                                           Message_Log=Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Surface element (",i0,") to ", a )' ) &
                        m, TRIM(Filename)
        GOTO 1000
      END IF

    END DO Profile_Loop


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '( "Number of profiles written to ", a, ": ", i0 )' ) &
                      TRIM(Filename), n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_Surface_Rank1


  FUNCTION Write_Surface_Rank2( Filename   , &  ! Input
                                Surface    , &  ! Input
                                Quiet      , &  ! Optional input
                                RCS_Id     , &  ! Revision control
                                Message_Log) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface(:,:)  ! L x M
    INTEGER,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(L x M)'
    ! Function variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l, n_Output_Channels
    INTEGER :: m, n_Output_Profiles
 
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Get dimensions
    n_Output_Channels = SIZE(Surface,1)
    n_Output_Profiles = SIZE(Surface,2)

    ! Check Quiet optional argument
    Noisy = .TRUE.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM(Filename), &
                                     FileID, &
                                     For_Output =SET, &
                                     Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM(Filename)
      GOTO 1000
    END IF


    ! Write the dimensions
    ! --------------------
    CALL Write_Dimensions( Filename, FileID, n_Output_Channels, n_Output_Profiles, &
                           IO_Status, Message )
    IF ( IO_Status /= 0 ) GOTO 1000


    ! Loop over all the data
    ! ----------------------
    Profile_Loop: DO m = 1, n_Output_Profiles
      Channel_Loop: DO l = 1, n_Output_Channels

        ! Write the structure data
        Error_Status = Write_Surface_Record( FileID, &
                                             Surface(l,m), &
                                             Message_Log=Message_Log )
        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '("Error writing Surface element (",i0,",",i0,") to ",a)' ) &
                          l, m, TRIM(Filename)
          GOTO 1000
        END IF

      END DO Channel_Loop
    END DO Profile_Loop


    ! Close the file
    ! --------------
    CLOSE( FileID, STATUS='KEEP',   &
                   IOSTAT=IO_Status )
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i0 )' ) &
                      TRIM(Filename), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( Message, '("Number of channels and profiles written to ",a,": ",i0,1x,i0 )' ) &
                      TRIM(Filename), n_Output_Channels, n_Output_Profiles
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

    !=====
    RETURN
    !=====

    ! Clean up after an error
    ! -----------------------
    1000 CONTINUE
    Error_Status = FAILURE
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM(Message), &
                          Error_Status, &
                          Message_Log=Message_Log )

  END FUNCTION Write_Surface_Rank2


  ! --------------------------------------------
  ! Utility routine to write the file dimensions
  ! --------------------------------------------
  SUBROUTINE Write_Dimensions( Filename, FileID, n_Channels, n_Profiles, &
                               IO_Status, Message )
    ! Arguments
    CHARACTER(*), INTENT(IN)  :: Filename
    INTEGER,      INTENT(IN)  :: FileID
    INTEGER,      INTENT(IN)  :: n_Channels
    INTEGER,      INTENT(IN)  :: n_Profiles
    INTEGER,      INTENT(OUT) :: IO_Status
    CHARACTER(*), INTENT(OUT) :: Message
    ! Write the dimensions to file    
    WRITE( FileID, IOSTAT=IO_Status ) n_Channels, n_Profiles
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '("Error writing data dimensions to ", a, &
                        &". IOSTAT = ",i0)' ) TRIM(Filename), IO_Status
      CLOSE( FileID, STATUS=WRITE_ERROR_STATUS )
    END IF
  
  END SUBROUTINE Write_Dimensions

END MODULE CRTM_Surface_Binary_IO
