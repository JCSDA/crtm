!
! SpcCoeff_NC2BIN
!
! Program to convert netCDF format SpcCoeff files to the Binary format.
!
! FILES ACCESSED:
!       - Input netCDF SpcCoeff data file
!       - Output Binary format SpcCoeff file.
!
! SIDE EFFECTS:
!       The output file is overwritten if it already exists.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SpcCoeff_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE String_Utility
  USE SpcCoeff_Define
  USE SpcCoeff_Binary_IO
  USE SpcCoeff_netCDF_IO
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_NC2BIN'
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

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status, IO_Status
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: BIN_Filename
  INTEGER :: n
  INTEGER :: Data_Type
  TYPE(SpcCoeff_Sensor_type) :: SpcCoeff_Sensor
  TYPE(SpcCoeff_Sensor_type) :: SpcCoeff_Sensor_Test
  TYPE(SpcCoeff_Spectral_type) :: SpcCoeff_Spectral
  TYPE(SpcCoeff_Spectral_type) :: SpcCoeff_Spectral_Test


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert netCDF format SpcCoeff files to the '//&
                        ' CRTM Binary format.', &
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


  ! Enter the filenames
  ! -------------------
  ! INPUT netCDF file
  WRITE( *, FMT     = '( /5x, "Enter the INPUT netCDF SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) NC_Filename
  NC_Filename = ADJUSTL( NC_FileNAME )
  IF ( .NOT. File_Exists( TRIM( NC_Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( NC_Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF
 
  ! OUTPUT Binary file
  WRITE( *, FMT     = '( /5x, "Enter the OUTPUT Binary SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) BIN_Filename
  BIN_Filename = ADJUSTL( BIN_FileNAME )

  ! Check that the netCDF file isn't accidentally overwritten
  IF ( TRIM( NC_Filename ) == TRIM( BIN_Filename ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Output filename is the same as the input filename!', &
                          FAILURE )
    STOP
  END IF


  ! Convert the SpcCoeff file
  ! -------------------------
  Data_Type_Select: SELECT CASE ( Data_Type )


    ! Process the SENSOR files
    ! ------------------------
    CASE ( SENSOR_TYPE )


      ! Read the netCDF SENSOR SpcCoeff file
      ! ------------------------------------
      WRITE( *, '( /5x, "Reading netCDF SENSOR SpcCoeff data ..." )' )
      Error_Status = Read_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                           SpcCoeff_Sensor )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF SENSOR SpcCoeff file '//&
                              TRIM( NC_Filename ), &
                              Error_Status )
        STOP
      END IF

       
      ! Write the Binary SENSOR SpcCoeff file
      ! -------------------------------------
      WRITE( *, '( /5x, "Writing Binary SENSOR SpcCoeff data ..." )' )
      Error_Status = Write_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                            SpcCoeff_Sensor )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing Binary SENSOR SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! Test read the SENSOR Binary file
      ! --------------------------------
      WRITE( *, '( /5x, "Test reading the Binary SENSOR SpcCoeff data file ..." )' )
      Error_Status = Read_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                           SpcCoeff_Sensor_Test )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading Binary SENSOR SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! Compare the netCDF and Binary SENSOR structures
      ! -----------------------------------------------
      WRITE( *, '( /5x, "Comparing the netCDF and Binary SENSOR SpcCoeff structures ..." )' )
      Error_Status = Equal_SpcCoeff( SpcCoeff_Sensor_Test, SpcCoeff_Sensor )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in netCDF and Binary '//&
                              'file SENSOR SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'netCDF and Binary file SENSOR SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! Destroy the SENSOR structures
      ! -----------------------------
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor structure.', &
                              WARNING )
      END IF
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor_Test )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor_Test structure.', &
                              WARNING )
      END IF


    ! Process the SPECTRAL files
    ! --------------------------
    CASE ( SPECTRAL_TYPE )


      ! Read the netCDF SPECTRAL SpcCoeff file
      ! --------------------------------------
      WRITE( *, '( /5x, "Reading netCDF SPECTRAL SpcCoeff data ..." )' )
      Error_Status = Read_SpcCoeff_netCDF( TRIM( NC_Filename ), &
                                           SpcCoeff_Spectral )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading netCDF SPECTRAL SpcCoeff file '//&
                              TRIM( NC_Filename ), &
                              Error_Status )
        STOP
      END IF
       

      ! Write the Binary SPECTRAL SpcCoeff file
      ! ---------------------------------------
      WRITE( *, '( /5x, "Writing Binary SPECTRAL SpcCoeff data ..." )' )
      Error_Status = Write_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                            SpcCoeff_Spectral )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error writing Binary SPECTRAL SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! Test read the SPECTRAL Binary file
      ! ----------------------------------
      WRITE( *, '( /5x, "Test reading the Binary SPECTRAL SpcCoeff data file ..." )' )
      Error_Status = Read_SpcCoeff_Binary( TRIM( BIN_Filename ), &
                                           SpcCoeff_Spectral_Test )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error reading Binary SPECTRAL SpcCoeff file '//&
                              TRIM( BIN_Filename ), &
                              Error_Status )
        STOP
      END IF


      ! Compare the netCDF and Binary SPECTRAL structures
      ! -------------------------------------------------
      WRITE( *, '( /5x, "Comparing the netCDF and Binary SPECTRAL SpcCoeff structures ..." )' )
      Error_Status = Equal_SpcCoeff( SpcCoeff_Spectral_Test, SpcCoeff_Spectral )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Differences found in netCDF and Binary '//&
                              'file SPECTRAL SpcCoeff structure comparison.', &
                              Error_Status )
      ELSE
        CALL Display_Message( PROGRAM_NAME, &
                              'netCDF and Binary file SPECTRAL SpcCoeff structures are equal.', &
                              INFORMATION )
      END IF


      ! Destroy the SPECTRAL structures
      ! -------------------------------
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral structure.', &
                              WARNING )
      END IF
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Spectral_Test )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Spectral_Test structure.', &
                              WARNING )
      END IF

  END SELECT Data_Type_Select

END PROGRAM SpcCoeff_NC2BIN
