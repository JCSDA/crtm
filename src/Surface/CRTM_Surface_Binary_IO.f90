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
  USE Type_Kinds,          ONLY: fp=>fp_kind
  USE File_Utility,        ONLY: File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                 Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters,     ONLY: ZERO, ONE, SET
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
    MODULE PROCEDURE Read_Surface_Scalar
    MODULE PROCEDURE Read_Surface_Rank1
  END INTERFACE CRTM_Read_Surface_Binary

  INTERFACE CRTM_Write_Surface_Binary
    MODULE PROCEDURE Write_Surface_Scalar
    MODULE PROCEDURE Write_Surface_Rank1
  END INTERFACE CRTM_Write_Surface_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Surface_Binary_IO.f90,v 1.12 2006/05/25 19:36:53 wd20pd Exp $'


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Read_Surface_Record( FileID,       &  ! Input
                                Surface,      &  ! Output
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    INTEGER,                  INTENT(IN)     :: FileID
    TYPE(CRTM_Surface_type),  INTENT(IN OUT) :: Surface
    CHARACTER(*),   OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Record)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: Destroy_Status
    INTEGER :: Allocate_Status
    INTEGER :: Type_in_File
    INTEGER :: Type_by_Coverage
    REAL(fp) :: Total_Coverage
    INTEGER :: n_Channels


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ---------------------------
    ! Read the gross surface type
    ! ---------------------------
    READ( FileID, IOSTAT = IO_Status ) Type_in_File, &
                                       Surface%Land_Coverage, &
                                       Surface%Water_Coverage, &
                                       Surface%Snow_Coverage, &
                                       Surface%Ice_Coverage
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface type and coverage. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Simple check of coverage
    Surface%Land_Coverage  = MAX( Surface%Land_Coverage,  ZERO )
    Surface%Water_Coverage = MAX( Surface%Water_Coverage, ZERO )
    Surface%Snow_Coverage  = MAX( Surface%Snow_Coverage,  ZERO )
    Surface%Ice_Coverage   = MAX( Surface%Ice_Coverage,   ZERO )

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
                            Message_Log = Message_Log )
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
                            'Coverage surface type, '//TRIM( SURFACE_TYPE_NAME( Type_by_Coverage ) )//&
                            ', inconsistent with that specified in file.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ---------------------------
    ! Read the surface wind speed
    ! ---------------------------
    READ( FileID, IOSTAT = IO_Status ) Surface%Wind_Speed
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading surface wind speed data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------------
    ! Read the land surface type data
    ! -------------------------------
    READ( FileID, IOSTAT = IO_Status ) Surface%Land_Type, &
                                       Surface%Land_Temperature, &
                                       Surface%Soil_Moisture_Content, &
                                       Surface%Canopy_Water_Content , &
                                       Surface%Vegetation_Fraction, &
                                       Surface%Soil_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading land surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Land_Type < 0 .OR. Surface%Land_Type > N_VALID_LAND_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised land surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Land_Type = INVALID_LAND
    END IF


    ! --------------------------------
    ! Read the water surface type data
    ! --------------------------------
    READ( FileID, IOSTAT = IO_Status ) Surface%Water_Type, &
                                       Surface%Water_Temperature, &
                                       Surface%Wind_Direction, &
                                       Surface%Salinity
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading water surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Water_Type < 0 .OR. Surface%Water_Type > N_VALID_WATER_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised water surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Water_Type = INVALID_WATER
    END IF


    ! -------------------------------
    ! Read the snow surface type data
    ! -------------------------------
    READ( FileID, IOSTAT = IO_Status ) Surface%Snow_Type, &
                                       Surface%Snow_Temperature, &
                                       Surface%Snow_Depth, &
                                       Surface%Snow_Density, &
                                       Surface%Snow_Grain_Size
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading snow surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Snow_Type < 0 .OR. Surface%Snow_Type > N_VALID_SNOW_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised snow surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Snow_Type = INVALID_SNOW
    END IF


    ! ------------------------------
    ! Read the ice surface type data
    ! ------------------------------
    READ( FileID, IOSTAT = IO_Status ) Surface%Ice_Type, &
                                       Surface%Ice_Temperature, &
                                       Surface%Ice_Thickness, &
                                       Surface%Ice_Density, &
                                       Surface%Ice_Roughness
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading ice surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF

    ! Check the type
    IF ( Surface%Ice_Type < 0 .OR. Surface%Ice_Type > N_VALID_ICE_TYPES ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Unrecognised ice surface type', &
                            Error_Status, &
                            Message_Log = Message_Log )
      Surface%Ice_Type = INVALID_ICE
    END IF


    ! ---------------------------------------------------------
    ! Destroy the SensorData structure. This step will be taken
    ! care of in the calling routine, Read_Surface_Binary(), so
    ! the following is a belt-and-braces thing. :o)
    ! ---------------------------------------------------------
    Destroy_Status = CRTM_Destroy_SensorData( Surface%SensorData, &
                                              Message_Log = Message_Log )
    IF ( Destroy_Status /= SUCCESS ) THEN
      Message = 'Error destroying SensorData structure.'
      GOTO 1000  ! Clean up
    END IF


    ! ------------------------
    ! Read the data dimensions
    ! ------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading SensorData dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------
    ! Read the data if required
    ! -------------------------
    IF ( n_Channels > 0 ) THEN

      ! Allocate the structure
      Allocate_Status = CRTM_Allocate_SensorData( n_Channels, &
                                                  Surface%SensorData, &
                                                  Message_Log = Message_Log )
      IF ( Allocate_Status /= SUCCESS ) THEN
        Message = 'Error allocating SensorData structure.'
        GOTO 1000  ! Clean up
      END IF

      ! Read the Sensor data
      READ( FileID, IOSTAT = IO_Status ) Surface%SensorData%NCEP_Sensor_ID, &  
                                         Surface%SensorData%WMO_Satellite_ID, &
                                         Surface%SensorData%WMO_Sensor_ID, &   
                                         Surface%SensorData%Sensor_Channel, &
                                         Surface%SensorData%Tb
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error reading SensorData. IOSTAT = ", i5 )' ) IO_Status
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
    Destroy_Status = CRTM_Destroy_Surface( Surface )
    CLOSE( FileID )

  END FUNCTION Read_Surface_Record


  FUNCTION Write_Surface_Record( FileID,       &  ! Input
                                 Surface,      &  ! Input
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    INTEGER,                  INTENT(IN)  :: FileID
    TYPE(CRTM_Surface_type),  INTENT(IN)  :: Surface
    CHARACTER(*),   OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Record)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: Type_by_Coverage
    INTEGER :: l 


    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! ----------------------------
    ! Write the gross surface type
    ! ----------------------------
    ! Compute the coverage type
    Type_by_Coverage = 0
    IF ( Surface%Land_Coverage  > ZERO ) Type_by_Coverage = LAND_SURFACE
    IF ( Surface%Water_Coverage > ZERO ) Type_by_Coverage = Type_by_Coverage + WATER_SURFACE
    IF ( Surface%Snow_Coverage  > ZERO ) Type_by_Coverage = Type_by_Coverage + SNOW_SURFACE
    IF ( Surface%Ice_Coverage   > ZERO ) Type_by_Coverage = Type_by_Coverage + ICE_SURFACE
    WRITE( FileID, IOSTAT = IO_Status ) Type_by_Coverage, &
                                        Surface%Land_Coverage, &
                                        Surface%Water_Coverage, &
                                        Surface%Snow_Coverage, &
                                        Surface%Ice_Coverage
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface type and coverage fractions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ----------------------------
    ! Write the surface wind speed
    ! ----------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%Wind_Speed
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing surface wind speed data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! --------------------------------
    ! Write the land surface type data
    ! --------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%Land_Type, &
                                        Surface%Land_Temperature, &
                                        Surface%Soil_Moisture_Content, &
                                        Surface%Canopy_Water_Content, &
                                        Surface%Vegetation_Fraction, &
                                        Surface%Soil_Temperature
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing land surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! ---------------------------------
    ! Write the water surface type data
    ! ---------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%Water_Type, &
                                        Surface%Water_Temperature, &
                                        Surface%Wind_Direction, &
                                        Surface%Salinity
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing water surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! --------------------------------
    ! Write the snow surface type data
    ! --------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%Snow_Type, &
                                        Surface%Snow_Temperature, &
                                        Surface%Snow_Depth, &
                                        Surface%Snow_Density, &
                                        Surface%Snow_Grain_Size
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing snow surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------------
    ! Write the ice surface type data
    ! -------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%Ice_Type, &
                                        Surface%Ice_Temperature, &
                                        Surface%Ice_Thickness, &
                                        Surface%Ice_Density, &
                                        Surface%Ice_Roughness
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing ice surface type data. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -------------------------------
    ! Write the SensorData dimensions
    ! -------------------------------
    WRITE( FileID, IOSTAT = IO_Status ) Surface%SensorData%n_Channels
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing SensorData dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      GOTO 1000  ! Clean up
    END IF


    ! -----------------------------
    ! Write the sensor ids and data
    ! -----------------------------
    IF ( Surface%SensorData%n_Channels > 0 ) THEN
      WRITE( FileID, IOSTAT = IO_Status ) Surface%SensorData%NCEP_Sensor_ID, &
                                          Surface%SensorData%WMO_Satellite_ID, &
                                          Surface%SensorData%WMO_Sensor_ID, &
                                          Surface%SensorData%Sensor_Channel, &
                                          Surface%SensorData%Tb
      IF ( IO_Status /= 0 ) THEN
        WRITE( Message, '( "Error writing SensorData. IOSTAT = ", i5 )' ) IO_Status
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
    CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )

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
!       Error_Status = CRTM_Inquire_Surface_Binary( Filename,                  &  ! Input
!                                                   n_Locations = n_Locations, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,      &  ! Revision control
!                                                   Message_Log = Message_Log  )  ! Error messaging
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
!       n_Locations:  The number of surface data locations in the file.
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
!                     If == SUCCESS the Binary inquiry was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Surface_Binary( Filename,     &  ! Input
                                        n_Locations,  &  ! Optional output
                                        RCS_Id,       &  ! Revision control
                                        Message_Log ) &  ! Error messaging
                                      RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Locations
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Surface_Binary'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Locations_in_File


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


    ! ---------------------------------
    ! Read the number of data locations
    ! ---------------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Locations_in_File
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, IOSTAT = IO_Status )
      RETURN
    END IF

    ! Save the value
    IF ( PRESENT( n_Locations ) ) n_Locations = n_Locations_in_File


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
!       Error_Status = CRTM_Read_Surface_Binary( Filename,                  &  ! Input
!                                                Surface,                   &  ! output
!                                                n_Locations = n_Locations, &  ! Optional output
!                                                RCS_Id      = RCS_Id,      &  ! Revision control
!                                                Message_Log = Message_Log  )  ! Error messaging
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
! OUTPUT ARGUMENTS:
!       Surface:      Structure containing the Surface data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Locations:  The actual number of surface data locations read in.
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
!       Note the INTENT on the output Surface argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION Read_Surface_Scalar( Filename,     &  ! Input
                                Surface,      &  ! Output
                                n_Locations,  &  ! Optional output
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN OUT) :: Surface
    INTEGER,       OPTIONAL, INTENT(OUT)    :: n_Locations
    CHARACTER(*),  OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Scalar)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Destroy_Status
    INTEGER :: n_Input_Locations
    INTEGER :: n_Locations_Read
    TYPE(CRTM_Surface_type) :: Dummy_Surface
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Message = 'File '//TRIM( Filename )//' not found.'
      GOTO 1000
    END IF


    ! -------------
    ! Open the file
    ! -------------
    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening '//TRIM( Filename )
      GOTO 1000
    END IF


    ! ---------------------------------
    ! Read the number of data locations
    ! ---------------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Input_Locations
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Issue warning message if n_Locations > 1
    IF ( n_Input_Locations > 1 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of locations > 1 and output Surface structure '//&
                            'is scalar. Only the first Surface structure will be read.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------------
    ! Initialize data locations read
    ! ------------------------------
    n_Locations_Read = 0


    ! ----------------------------------------------
    ! Read the structure data into a dummy structure
    ! ----------------------------------------------
    Error_Status = Read_Surface_Record( FileID, &
                                        Dummy_Surface, &
                                        Message_Log = Message_Log )
    IF ( Error_Status == FAILURE ) THEN
      Message = 'Error reading Surface record from '//TRIM( Filename )
      GOTO 1000
    END IF
    IF ( Error_Status == WARNING ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Warning flagged in surface record read.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF


    ! ------------------------------
    ! Copy dummy structure to output
    ! ------------------------------
    ! Copy the data into the output array
    Error_Status = CRTM_Assign_Surface( Dummy_Surface, &
                                        Surface, &
                                        Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error copying Surface structure.'
      GOTO 1000
    END IF
    ! Set value for the number of locations read
    n_Locations_Read = 1


    ! ---------------------------
    ! Destroy the dummy structure
    ! ---------------------------
    Error_Status = CRTM_Destroy_Surface( Dummy_Surface )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying dummy Surface structure.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! --------------------------------------
    ! Save the number of data locations read
    ! --------------------------------------
    IF ( PRESENT( n_Locations ) ) n_Locations = n_Locations_Read


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
    Destroy_Status = CRTM_Destroy_Surface( Surface, Dummy_Surface )
    CLOSE( FileID )

  END FUNCTION Read_Surface_Scalar


  FUNCTION Read_Surface_Rank1( Filename,     &  ! Input
                               Surface,      &  ! Output
                               n_Locations,  &  ! Optional output
                               RCS_Id,       &  ! Revision control
                               Message_Log ) &  ! Error messaging
                             RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                          INTENT(IN)     :: Filename
    TYPE(CRTM_Surface_type), DIMENSION(:), INTENT(IN OUT) :: Surface
    INTEGER,                 OPTIONAL,     INTENT(OUT)    :: n_Locations
    CHARACTER(*),            OPTIONAL,     INTENT(OUT)    :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Surface_Binary(Rank-1)'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: Destroy_Status
    INTEGER :: n_Input_Locations
    INTEGER :: m, n_Locations_Read
    TYPE(CRTM_Surface_type) :: Dummy_Surface
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID

    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status,    &
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
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------------------
    ! Read the number of data locations
    ! ---------------------------------
    READ( FileID, IOSTAT = IO_Status ) n_Input_Locations
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error reading n_Locations data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      GOTO 1000
    END IF

    ! Issue warning message if n_Locations > size of output array
    IF ( n_Input_Locations > SIZE( Surface ) ) THEN
      WRITE( Message, '( "Number of data locations, ", i5, " > size of the output Surface ", &
                        &"structure array, ", i5, ". Only the first ", i5, &
                        &" Surface structures will be read." )' ) &
                      n_Input_Locations, SIZE( Surface ), SIZE( Surface )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      n_Input_Locations = SIZE( Surface )
    END IF


    ! ------------------------
    ! Initialize profiles read
    ! ------------------------
    n_Locations_Read = 0


    ! --------------------------------------------------------------
    ! Loop over all the data locations (even potentially empty ones)
    ! --------------------------------------------------------------
    Location_Loop: DO m = 1, n_Input_Locations


      ! ----------------------------------------------
      ! Read the structure data into a dummy structure
      ! ----------------------------------------------
      Error_Status = Read_Surface_Record( FileID, &
                                          Dummy_Surface, &
                                          Message_Log = Message_Log )
      IF ( Error_Status == FAILURE ) THEN
        WRITE( Message, '( "Error reading Surface element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000
      END IF
      IF ( Error_Status == WARNING ) THEN
        WRITE( Message, '( "Warning flagged in reading surface record #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
      END IF


      ! ------------------------------------
      ! Copy dummy structure to output array
      ! ------------------------------------
      ! Increment profiles read
      n_Locations_Read = n_Locations_Read + 1
      ! Copy the data into the output array
      Error_Status = CRTM_Assign_Surface( Dummy_Surface, &
                                          Surface( n_Locations_Read ), &
                                          Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying Surface element #", i5, "." )' ) m
        GOTO 1000
      END IF


      ! ---------------------------
      ! Destroy the dummy structure
      ! ---------------------------
      Error_Status = CRTM_Destroy_Surface( Dummy_Surface )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying dummy Surface structure at element #", i5, "." )' ) m
        GOTO 1000
      END IF

    END DO Location_Loop


    ! ----------------------
    ! Output an info message
    ! ----------------------
    WRITE( Message, '( "Number of surface data locations read from ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Locations_Read
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )


    ! ---------------------------------------
    ! Assign a value to the optional argument
    ! ---------------------------------------
    IF ( PRESENT( n_Locations ) ) n_Locations = n_Locations_Read


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
    Destroy_Status = CRTM_Destroy_Surface( Surface )
    Destroy_Status = CRTM_Destroy_Surface( Dummy_Surface )
    CLOSE( FileID )


  END FUNCTION Read_Surface_Rank1


!------------------------------------------------------------------------------
!
! NAME:
!       CRTM_Write_Surface_Binary
!
! PURPOSE:
!       Function to write Binary format CRTM Surface files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Surface_Binary( Filename,                 &  ! Input
!                                                 Surface,                  &  ! Input
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Surface format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Surface:      Structure containing the Surface data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Surface_type
!                     DIMENSION:  Scalar or Rank-1
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

  FUNCTION Write_Surface_Scalar( Filename,     &  ! Input
                                 Surface,      &  ! Input
                                 RCS_Id,       &  ! Revision control
                                 Message_Log ) &  ! Error messaging
                               RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), INTENT(IN)  :: Surface
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Scalar)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


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
    ! Write the number of lcations
    ! ----------------------------
    WRITE( FileID, IOSTAT = IO_Status ) 1
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Locations data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    ! ------------------------
    ! Write the structure data
    ! ------------------------
    Error_Status = Write_Surface_Record( FileID, &
                                         Surface, &
                                         Message_Log = Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing Surface record to '//TRIM( Filename )
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

  END FUNCTION Write_Surface_Scalar


  FUNCTION Write_Surface_Rank1( Filename,     &  ! Input
                                Surface,      &  ! Input
                                RCS_Id,       &  ! Revision control
                                Message_Log ) &  ! Error messaging
                              RESULT ( Error_Status )
    ! Arguments
    CHARACTER(*),                          INTENT(IN)  :: Filename
    TYPE(CRTM_Surface_type), DIMENSION(:), INTENT(IN)  :: Surface
    CHARACTER(*),            OPTIONAL,     INTENT(OUT) :: RCS_Id
    CHARACTER(*),            OPTIONAL,     INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Surface_Binary(Rank-1)'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER( 256 ) :: Message
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Output_Locations
 

    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


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


    ! -----------------------------
    ! Write the number of locations
    ! -----------------------------
    n_Output_Locations = SIZE( Surface )
    WRITE( FileID, IOSTAT = IO_Status ) n_Output_Locations
    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error writing n_Locations data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      GOTO 1000
    END IF


    ! ---------------------------
    ! Loop over all the locations
    ! ---------------------------
    Location_Loop: DO m = 1, n_Output_Locations
      Error_Status = Write_Surface_Record( FileID, &
                                           Surface(m), &
                                           Message_Log = Message_Log )
      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing Surface element #", i5, " to ", a )' ) &
                        m, TRIM( Filename )
        GOTO 1000
      END IF
    END DO Location_Loop



    ! ----------------------
    ! Output an info message
    ! ----------------------
    WRITE( Message, '( "Number of surface data elements written to ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Output_Locations
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )


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

  END FUNCTION Write_Surface_Rank1

END MODULE CRTM_Surface_Binary_IO
