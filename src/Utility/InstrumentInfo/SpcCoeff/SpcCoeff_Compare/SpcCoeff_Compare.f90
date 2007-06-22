!
! SpcCoeff_Compare
!
! Program to compare SpcCoeff data read from either netCDF or
! CRTM Binary format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Sep-2005
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SpcCoeff_Compare

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Compare'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'

  ! Allowable SpcCoeff data types.
  ! Type codes MUST start at 1 and increment by 1.
  INTEGER,PARAMETER :: N_TYPES = 2
  INTEGER,PARAMETER :: SENSOR_TYPE   = 1
  INTEGER,PARAMETER :: SPECTRAL_TYPE = 2
  INTEGER,PARAMETER, DIMENSION( N_TYPES ) :: &
    TYPE_CODE = (/ SENSOR_TYPE, &
                   SPECTRAL_TYPE /)
  CHARACTER(*), PARAMETER, DIMENSION( N_TYPES ) :: &
    TYPE_NAME = (/ 'Sensor  ', &
                   'Spectral' /)


  ! Allowable SpcCoeff file formats.
  ! Format codes MUST start at 1 and increment by 1.
  INTEGER, PARAMETER :: N_FORMATS = 2
  INTEGER, PARAMETER :: NETCDF_FORMAT = 1
  INTEGER, PARAMETER :: BINARY_FORMAT = 2
  INTEGER, PARAMETER, DIMENSION( N_FORMATS ) :: &
    FORMAT_CODE = (/ NETCDF_FORMAT, &
                     BINARY_FORMAT /)
  CHARACTER(*), PARAMETER, DIMENSION( N_FORMATS ) :: &
    FORMAT_NAME = (/ 'netCDF', &
                     'Binary' /)


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: IO_Status
  INTEGER :: n
  INTEGER :: Data_Type
  CHARACTER(256) :: Filename1
  CHARACTER(256) :: Filename2
  INTEGER :: File1_Format
  INTEGER :: File2_Format
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor1
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor2
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral1
  TYPE( SpcCoeff_Spectral_type ) :: SpcCoeff_Spectral2


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to compare SpcCoeff data read from either netCDF '//&
                        'or CRTM Binary format files.', &
                        '$Revision$' )

  ! Enter the file types
  ! --------------------
  WRITE( *, FMT = '( /5x, "Select the data type" )' )
  DO n = 1, N_TYPES
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) TYPE_CODE(n), &
                                             TRIM( TYPE_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) Data_Type

  ! Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for data type.', &
                          FAILURE )
    STOP
  END IF

  ! Invalid value
  IF ( .NOT. ANY( TYPE_CODE == Data_Type ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid data type.', &
                          FAILURE )
    STOP
  END IF


  ! The first filename
  ! ------------------
  ! Get the filename
  WRITE( *, FMT     = '( /5x, "Enter the FIRST SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename1
  Filename1 = ADJUSTL( Filename1 )
  IF ( .NOT. File_Exists( TRIM( Filename1 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename1 )//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! Get the file format type
  WRITE( *, FMT = '( /5x, "Select the format type for ", a )' ) TRIM( Filename1 )
  DO n = 1, N_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) FORMAT_CODE(n), &
                                             TRIM( FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) File1_Format
  ! Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for '//TRIM( Filename1 )//' format type.', &
                          FAILURE )
    STOP
  END IF
  ! Invalid value
  IF ( .NOT. ANY( FORMAT_CODE == File1_Format ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type for '//TRIM( Filename1 )//'.', &
                          FAILURE )
    STOP
  END IF


  ! The second filename
  ! -------------------
  ! Get the filename
  WRITE( *, FMT     = '( /5x, "Enter the SECOND SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename2
  Filename2 = ADJUSTL( Filename2 )
  IF ( .NOT. File_Exists( TRIM( Filename2 ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename2 )//' not found.', &
                          FAILURE )
    STOP
  END IF

  ! Get the file format type
  WRITE( *, FMT = '( /5x, "Select the format type for ", a )' ) TRIM( Filename2 )
  DO n = 1, N_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) FORMAT_CODE(n), &
                                             TRIM( FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) File2_Format
  ! Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for '//TRIM( Filename2 )//' format type.', &
                          FAILURE )
    STOP
  END IF
  ! Invalid value
  IF ( .NOT. ANY( FORMAT_CODE == File2_Format ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type for '//TRIM( Filename2 )//'.', &
                          FAILURE )
    STOP
  END IF


  ! Compare the SpcCoeff files
  ! --------------------------
  Data_Type_Select: SELECT CASE ( Data_Type )


    ! Process the SENSOR files
    ! ------------------------
    CASE ( SENSOR_TYPE )


      ! Read the FIRST SENSOR SpcCoeff file
      ! -----------------------------------
      WRITE( *, '( /5x, "Reading FIRST SENSOR SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename1 )
      Sensor1_Format_Select: SELECT CASE ( File1_Format )

        CASE ( NETCDF_FORMAT )
          Error_Status = Read_SpcCoeff_netCDF( Filename1, &
                                               SpcCoeff_Sensor1 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SENSOR SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )
          Error_Status = Read_SpcCoeff_Binary( Filename1, &
                                               SpcCoeff_Sensor1 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SENSOR SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Sensor1_Format_Select


      ! Read the SECOND SENSOR SpcCoeff file
      ! ------------------------------------
      WRITE( *, '( /5x, "Reading SECOND SENSOR SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename2 )
      Sensor2_Format_Select: SELECT CASE ( File2_Format )

        CASE ( NETCDF_FORMAT )
          Error_Status = Read_SpcCoeff_netCDF( Filename2, &
                                               SpcCoeff_Sensor2 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SENSOR SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )
          Error_Status = Read_SpcCoeff_Binary( Filename2, &
                                               SpcCoeff_Sensor2 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SENSOR SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Sensor2_Format_Select


      ! Compare the two SENSOR SpcCoeff files
      ! -------------------------------------
      WRITE( *, '( /5x, "Comparing the two SENSOR SpcCoeff structures ..." )' )
      Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor1, SpcCoeff_Sensor2, Check_All = 1 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in SENSOR SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'SENSOR SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! Destroy the structures
      ! ----------------------
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor1 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor1 structure.', &
                              WARNING )
      END IF
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor2 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor2 structure.', &
                              WARNING )
      END IF



    ! Process the SPECTRAL files
    ! --------------------------
    CASE ( SPECTRAL_TYPE )


      ! Read the FIRST SPECTRAL SpcCoeff file
      ! -------------------------------------
      WRITE( *, '( /5x, "Reading FIRST SPECTRAL SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename1 )
      Spectral1_Format_Select: SELECT CASE ( File1_Format )

        CASE ( NETCDF_FORMAT )
          Error_Status = Read_SpcCoeff_netCDF( Filename1, &
                                               SpcCoeff_Spectral1 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )
          Error_Status = Read_SpcCoeff_Binary( Filename1, &
                                               SpcCoeff_Spectral1 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename1 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Spectral1_Format_Select


      ! Read the SECOND SPECTRAL SpcCoeff file
      ! --------------------------------------
      WRITE( *, '( /5x, "Reading SECOND SPECTRAL SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename2 )
      Spectral2_Format_Select: SELECT CASE ( File2_Format )

        CASE ( NETCDF_FORMAT )
          Error_Status = Read_SpcCoeff_netCDF( Filename2, &
                                               SpcCoeff_Spectral2 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )
          Error_Status = Read_SpcCoeff_Binary( Filename2, &
                                               SpcCoeff_Spectral2 )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SPECTRAL SpcCoeff file '//&
                                  TRIM( Filename2 ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Spectral2_Format_Select


      ! Compare the two SPECTRAL SpcCoeff files
      ! ---------------------------------------
      WRITE( *, '( /5x, "Comparing the two SPECTRAL SpcCoeff structures ..." )' )
      Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral1, SpcCoeff_Spectral2, Check_All = 1 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in SPECTRAL SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'SPECTRAL SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! Destroy the structures
      ! ----------------------
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral1 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral1 structure.', &
                              WARNING )
      END IF
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral2 )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral2 structure.', &
                              WARNING )
      END IF

  END SELECT Data_Type_Select

END PROGRAM SpcCoeff_Compare
