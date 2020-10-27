!
! Assemble_Coefficient_File
!
! Program to concatenate individual SENSOR SpcCoeff and TauCoeff files
! into a single file
!
! FILES ACCESSED:
!       INPUT:  - SensorInfo file
!               - Individual instrument coefficient files.
!
!       OUTPUT: Concatenated instrument coefficient files.
!
! SIDE EFFECTS:
!       If any output coefficient files already exist, they are overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 20-Mar-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Assemble_Coefficient_File

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler      , ONLY: SUCCESS, WARNING, FAILURE, INFORMATION, &
                                   Display_Message, Program_Message
  USE SensorInfo_Define    , ONLY: SensorInfo_type, &
                                   Destroy_SensorInfo
  USE SensorInfo_LinkedList, ONLY: SensorInfo_List_type   , &
                                   New_SensorInfo_List    , &
                                   Count_SensorInfo_Nodes , &
                                   GetFrom_SensorInfo_List, &
                                   Destroy_SensorInfo_List
  USE SensorInfo_IO        , ONLY: Read_SensorInfo
  USE TauCoeff_Define      , ONLY: TauCoeff_type               , &
                                   Concatenate_Channel_TauCoeff, &
                                   Destroy_TauCoeff
  USE TauCoeff_netCDF_IO   , ONLY: Read_TauCoeff_netCDF , &
                                   Write_TauCoeff_netCDF
  USE TauCoeff_Binary_IO   , ONLY: Write_TauCoeff_Binary
  USE SpcCoeff_Define      , ONLY: SpcCoeff_Sensor_type, &
                                   Concatenate_SpcCoeff, &
                                   Destroy_SpcCoeff
  USE SpcCoeff_netCDF_IO   , ONLY: Read_SpcCoeff_netCDF , &
                                   Write_SpcCoeff_netCDF
  USE SpcCoeff_Binary_IO   , ONLY: Write_SpcCoeff_Binary
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Assemble_Coefficient_File'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! The type of coefficient assembly
  INTEGER,      PARAMETER :: N_ASSEMBLY_TYPES = 3
  CHARACTER(*), PARAMETER, DIMENSION( N_ASSEMBLY_TYPES ) :: &
    ASSEMBLY_TYPE_DESCRIPTION = (/ 'Spectral coefficients     ', &
                                   'Transmittance coefficients', &
                                   'Both                      ' /)
  ! Allowable output formats.
  ! Format codes MUST start at 1 and increment by 1.
  INTEGER,      PARAMETER :: N_OUTPUT_FORMATS = 2
  INTEGER,      PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_CODE = (/ 1, & ! netCDF
                            2 /) ! CRTM binary
  CHARACTER(*), PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_NAME = (/ 'netCDF', &
                            'Binary' /)
  CHARACTER(*), PARAMETER, DIMENSION( N_OUTPUT_FORMATS ) :: &
    OUTPUT_FORMAT_EXTENSION = (/ '.nc ', &
                                 '.bin' /)
  INTEGER,      PARAMETER :: DEFAULT_OUTPUT_FORMAT = 1


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: SensorInfo_Filename
  CHARACTER(256) :: Coeff_File_Path
  CHARACTER(256) :: SpcCoeff_Filename
  CHARACTER(256) :: TauCoeff_Filename
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Format_Code
  INTEGER :: Assembly_Type
  LOGICAL :: Assemble_SpcCoeff
  LOGICAL :: Assemble_TauCoeff
  INTEGER :: n_Sensors, n
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  TYPE(TauCoeff_type), TARGET  :: TauCoeff_in, TauCoeff_out
  TYPE(TauCoeff_type), POINTER :: TauCoeff
  TYPE(SpcCoeff_Sensor_type), TARGET  :: SpcCoeff_in, SpcCoeff_out
  TYPE(SpcCoeff_Sensor_type), POINTER :: SpcCoeff
  CHARACTER(256)  :: Input_TauCoeff_Sensor_Name,    Input_SpcCoeff_Sensor_Name
  CHARACTER(256)  :: Input_TauCoeff_Platform_Name,  Input_SpcCoeff_Platform_Name
  CHARACTER(256)  :: TauCoeff_ID_Tag
  CHARACTER(5000) :: Output_TauCoeff_Sensor_Name,   Output_SpcCoeff_Sensor_Name
  CHARACTER(5000) :: Output_TauCoeff_Platform_Name, Output_SpcCoeff_Platform_Name


  ! ------
  ! Set up
  ! ------
  ! Create the SensorInfo linked list
  SensorInfo_List = New_SensorInfo_List()

  ! Output descriptive header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to concatenate SENSOR SpcCoeff and TauCoeff '//&
                        'coefficient files for individual instruments into a '//&
                        'single file based on SensorInfo entries.', &
                        '$Revision$' )

  ! Read the SensorInfo file data
  !
  ! Get the SensorInfo filename
  WRITE( *, FMT     = '( /5x, "Enter the SensorInfo filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) SensorInfo_Filename
  SensorInfo_Filename = ADJUSTL( SensorInfo_Filename )
  ! Read the file
  Error_Status = Read_SensorInfo( SensorInfo_Filename, &
                                  SensorInfo_List, &
                                  Quiet = 1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//TRIM( SensorInfo_Filename ), &
                          FAILURE )
    STOP
  END IF
  ! Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF

  ! Get user input on what to assemble
  WRITE( *, FMT = '( /5x, "Select the type of coefficient assembly:" )' )
  DO n = 1, N_ASSEMBLY_TYPES
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) n, TRIM( ASSEMBLY_TYPE_DESCRIPTION( n ) )
  END DO
  WRITE( *, FMT     = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Assembly_Type
  IF ( IO_Status /= 0    .OR. &
       Assembly_Type < 1 .OR. &
       Assembly_Type > n_Assembly_Types ) THEN
    CALL Display_Message( PROGRAM_NAME,    &
                          'Invalid selection', &
                          FAILURE          )
    STOP
  END IF

  ! Set assembly flags
  SELECT CASE ( Assembly_Type )
    CASE (1)
      Assemble_SpcCoeff = .TRUE.
      Assemble_TauCoeff = .FALSE.
    CASE (2)
      Assemble_SpcCoeff = .FALSE.
      Assemble_TauCoeff = .TRUE.
    CASE DEFAULT
      Assemble_SpcCoeff = .TRUE.
      Assemble_TauCoeff = .TRUE.
  END SELECT

  ! Get user input about the output format
  WRITE( *, FMT = '( /5x, "Select the output format type" )' )
  DO n = 1, N_OUTPUT_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) OUTPUT_FORMAT_CODE(n), &
                                             TRIM( OUTPUT_FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Format_Code

  ! Check user input
  !
  ! Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid input. Setting output format to '//&
                          OUTPUT_FORMAT_NAME( DEFAULT_OUTPUT_FORMAT ), &
                          INFORMATION )
    Format_Code = DEFAULT_OUTPUT_FORMAT
  END IF
  ! Invalid value
  IF ( .NOT. ANY( OUTPUT_FORMAT_CODE == Format_Code ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type. Setting output format to '//&
                          OUTPUT_FORMAT_NAME( DEFAULT_OUTPUT_FORMAT ), &
                          INFORMATION )
    Format_Code = DEFAULT_OUTPUT_FORMAT
  END IF
    
  ! Get the instrument coefficient data path
  WRITE( *, FMT     = '( /5x, "Enter the input data file path [e.g. ./coeff_data/]: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Coeff_File_Path
  Coeff_File_Path = ADJUSTL( Coeff_File_Path )

  ! Initialise the global attribute strings
  Output_TauCoeff_Sensor_Name   =' '
  Output_TauCoeff_Platform_Name =' '
  Output_SpcCoeff_Sensor_Name   =' '
  Output_SpcCoeff_Platform_Name =' '


  ! ---------------------------
  ! Begin loop over the sensors
  ! ---------------------------
  Concatenate_loop: DO n = 1, n_Sensors
    WRITE( *, '(/)' )

    ! Get the current SensorInfo data from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------
    ! Concatenate the TauCoeff data
    ! -----------------------------
    TauCoeff_Concatenation: IF ( Assemble_TauCoeff ) THEN

      ! Depending on the sensor number, read the TauCoeff
      ! data into the appropriate structure
      IF ( n > 1 ) THEN
        TauCoeff => TauCoeff_In
      ELSE
        TauCoeff => TauCoeff_Out
      END IF

      ! Construct the filename
      TauCoeff_Filename = TRIM( Coeff_File_Path ) // &
                          TRIM( SensorInfo%File_Prefix ) // &
                          '.TauCoeff.nc'

      ! Read the data into the INPUT structure
      Error_Status = Read_TauCoeff_netCDF( TRIM( TauCoeff_Filename ), &
                                           TauCoeff, &
                                           Sensor_Name   = Input_TauCoeff_Sensor_Name, &
                                           Platform_Name = Input_TauCoeff_Platform_Name, &
                                           ID_Tag        = TauCoeff_ID_Tag )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading TauCoeff file '//TRIM( TauCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Concatenate global attribute strings for netCDF output file
      Output_TauCoeff_Sensor_Name   = TRIM( Output_TauCoeff_Sensor_Name )//TRIM( Input_TauCoeff_Sensor_Name )//':'
      Output_TauCoeff_Platform_Name = TRIM( Output_TauCoeff_Platform_Name )//TRIM( Input_TauCoeff_Platform_Name )//':'

      ! Check the TauCoeff sensor Ids
      IF ( ANY( TauCoeff%NCEP_Sensor_Id   /= SensorInfo%NCEP_Sensor_Id   ) .OR. &
           ANY( TauCoeff%WMO_Satellite_Id /= SensorInfo%WMO_Satellite_Id ) .OR. &
           ANY( TauCoeff%WMO_Sensor_Id    /= SensorInfo%WMO_Sensor_Id    )      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "TauCoeff sensor Ids in ", a, ", ", 3(1x,i0), &
                          &", are different from SensorInfo values, ", 3(1x,i0) )' ) &
               TRIM( TauCoeff_Filename ), &
               TauCoeff%NCEP_Sensor_Id(1), TauCoeff%WMO_Satellite_Id(1), TauCoeff%WMO_Sensor_Id(1), &
               SensorInfo%NCEP_Sensor_Id,  SensorInfo%WMO_Satellite_Id,  SensorInfo%WMO_Sensor_Id
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Concatenate the OUTPUT and INPUT TauCoeff
      ! structures along the CHANNEL dimension
      IF ( n > 1 ) THEN
        Error_Status = Concatenate_Channel_TauCoeff( TauCoeff_Out, &
                                                     TauCoeff_In )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error concatenating TauCoeff structures at file '//&
                                TRIM( TauCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF

        ! Destroy the INPUT TauCoeff structure
        Error_Status = Destroy_TauCoeff( TauCoeff_In )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error destroying the TauCoeff_In structure at file '//&
                                TRIM( TauCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF
      END IF

    END IF TauCoeff_Concatenation


    ! -----------------------------
    ! Concatenate the SpcCoeff data
    ! -----------------------------
    SpcCoeff_Concatenation: IF ( Assemble_SpcCoeff ) THEN

      ! Depending on the sensor number, read the SpcCoeff
      ! data into the appropriate structure
      IF ( n > 1 ) THEN
        SpcCoeff => SpcCoeff_In
      ELSE
        SpcCoeff => SpcCoeff_Out
      END IF

      ! Construct the filename
      SpcCoeff_Filename = TRIM( Coeff_File_Path ) // &
                          TRIM( SensorInfo%File_Prefix ) // &
                          '.SpcCoeff.nc'

      ! Read the data into the INPUT structure
      Error_Status = Read_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                           SpcCoeff, &
                                           Sensor_Name   = Input_SpcCoeff_Sensor_Name, &
                                           Platform_Name = Input_SpcCoeff_Platform_Name )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading SpcCoeff file '//TRIM( SpcCoeff_Filename ), &
                              Error_Status )
        STOP
      END IF

      ! Concatenate global attribute strings for netCDF output file
      Output_SpcCoeff_Sensor_Name   = TRIM( Output_SpcCoeff_Sensor_Name )//TRIM( Input_SpcCoeff_Sensor_Name )//':'
      Output_SpcCoeff_Platform_Name = TRIM( Output_SpcCoeff_Platform_Name )//TRIM( Input_SpcCoeff_Platform_Name )//':'

      ! Check the SpcCoeff sensor Ids
      IF ( ANY( SpcCoeff%NCEP_Sensor_Id   /= SensorInfo%NCEP_Sensor_Id   ) .OR. &
           ANY( SpcCoeff%WMO_Satellite_Id /= SensorInfo%WMO_Satellite_Id ) .OR. &
           ANY( SpcCoeff%WMO_Sensor_Id    /= SensorInfo%WMO_Sensor_Id    )      ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "SpcCoeff sensor Ids in ", a, ", ", 3(1x,i0), &
                          &", are different from SensorInfo values, ", 3(1x,i0) )' ) &
               TRIM( SpcCoeff_Filename ), &
               SpcCoeff%NCEP_Sensor_Id(1), SpcCoeff%WMO_Satellite_Id(1), SpcCoeff%WMO_Sensor_Id(1), &
               SensorInfo%NCEP_Sensor_Id,  SensorInfo%WMO_Satellite_Id,  SensorInfo%WMO_Sensor_Id
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Concatenate the OUTPUT and INPUT SpcCoeff structures
      IF ( n > 1 ) THEN
        Error_Status = Concatenate_SpcCoeff( SpcCoeff_Out, &
                                             SpcCoeff_In )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error concatenating SpcCoeff structures at file '//&
                                TRIM( SpcCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF

        ! Destroy the INPUT SpcCoeff structure
        Error_Status = Destroy_SpcCoeff( SpcCoeff_In )
        IF ( Error_Status /= SUCCESS ) THEN
          CALL Display_Message( PROGRAM_NAME, &
                                'Error destroying the SpcCoeff_In structure at file '//&
                                TRIM( SpcCoeff_Filename ), &
                                Error_Status )
          STOP
        END IF
      END IF

    END IF SpcCoeff_Concatenation


    ! Destroy the current SensorInfo structure
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error destroying SensorInfo structure for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Concatenate_loop


  ! Destroy the SensorInfo linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          Error_Status )
  END IF


  ! ------------------------------------------
  ! Output the concatenated TauCoeff structure
  ! ------------------------------------------
  TauCoeff_Output: IF ( Assemble_TauCoeff ) THEN
    WRITE( *, '( /5x, "Writing the output TauCoeff file..." )' )

    ! Construct the output filename
    TauCoeff_Filename = 'Concatenated.TauCoeff'//TRIM( OUTPUT_FORMAT_EXTENSION( Format_Code ) )

    ! Write the data
    SELECT CASE ( Format_Code )

      ! netCDF output
      CASE ( 1 )
        Error_Status = Write_TauCoeff_netCDF( TRIM( TauCoeff_Filename ), &
                                              TauCoeff_Out, &
                                              Title         = 'Combined transmittance coefficients dataset.', &
                                              History       = PROGRAM_RCS_ID, &
                                              Sensor_Name   = TRIM( Output_TauCoeff_Sensor_Name ), &
                                              Platform_Name = TRIM( Output_TauCoeff_Platform_Name ), &
                                              Comment       = 'Data concatenated in the order shown '//&
                                                              'in the sensor/platform fields.', &
                                              ID_Tag        = TRIM( TauCoeff_ID_Tag ) )

      ! Binary output
      CASE ( 2 )
        Error_Status = Write_TauCoeff_Binary( TRIM( TauCoeff_Filename ), &
                                              TauCoeff_Out )

      ! Something went REALLY wrong.
      CASE DEFAULT
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Invalid format code in TauCoeff output section for output to '//&
                              TRIM( TauCoeff_Filename ), &
                              Error_Status )
    END SELECT

    ! Output write error message.
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing TauCoeff file '//TRIM( TauCoeff_Filename ), &
                            Error_Status )
      STOP
    END IF

    ! Destroy the OUTPUT TauCoeff structure
    Error_Status = Destroy_TauCoeff( TauCoeff_Out )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying the TauCoeff_Out structure.', &
                            Error_Status )
      STOP
    END IF

  END IF TauCoeff_Output


  ! ------------------------------------------
  ! Output the concatenated SpcCoeff structure
  ! ------------------------------------------
  SpcCoeff_Output: IF ( Assemble_SpcCoeff ) THEN
    WRITE( *, '( /5x, "Writing the output SpcCoeff file..." )' )

    ! Construct the output filename
    SpcCoeff_Filename = 'Concatenated.SpcCoeff'//TRIM( OUTPUT_FORMAT_EXTENSION( Format_Code ) )

    ! Write the data
    SELECT CASE ( Format_Code )

      ! netCDF output
      CASE ( 1 )
        Error_Status = Write_SpcCoeff_netCDF( TRIM( SpcCoeff_Filename ), &
                                              SpcCoeff_Out, &
                                              Title         = 'Combined spectral coefficients dataset.', &
                                              History       = PROGRAM_RCS_ID, &
                                              Sensor_Name   = TRIM( Output_SpcCoeff_Sensor_Name ), &
                                              Platform_Name = TRIM( Output_SpcCoeff_Platform_Name ), &
                                              Comment       = 'Data concatenated in the order shown '//&
                                                              'in the sensor/platform fields.' )

      ! Binary output
      CASE ( 2 )
        Error_Status = Write_SpcCoeff_Binary( TRIM( SpcCoeff_Filename ), &
                                              SpcCoeff_Out )

      ! Something went REALLY wrong.
      CASE DEFAULT
        Error_Status = FAILURE
        CALL Display_Message( PROGRAM_NAME, &
                              'Invalid format code in SpcCoeff output section for output to '//&
                              TRIM( SpcCoeff_Filename ), &
                              Error_Status )
    END SELECT

    ! Output write error message.
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error writing SpcCoeff file '//TRIM( SpcCoeff_Filename ), &
                            Error_Status )
      STOP
    END IF

    ! Destroy the OUTPUT SpcCoeff structure
    Error_Status = Destroy_SpcCoeff( SpcCoeff_Out )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying the SpcCoeff_Out structure.', &
                            Error_Status )
    END IF

  END IF SpcCoeff_Output

END PROGRAM Assemble_Coefficient_File
