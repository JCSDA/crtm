!
! SpcCoeff_Dump
!
! Program to compare SpcCoeff data read from either netCDF or
! CRTM Binary format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 22-Jun-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM SpcCoeff_Dump

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

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_Dump'
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
  CHARACTER(256) :: Filename
  INTEGER :: File_Format
  TYPE( SpcCoeff_Sensor_type ) :: SpcCoeff_Sensor


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to dump SpcCoeff data read from either netCDF '//&
                        'or CRTM Binary format files.', &
                        '$Revision$' )

  ! Enter the file type
  ! -------------------
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


  ! Get the filename
  ! ----------------
  WRITE( *, FMT     = '( /5x, "Enter the SpcCoeff file: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a )' ) Filename
  Filename = ADJUSTL( Filename )
  IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File '//TRIM( Filename )//' not found.', &
                          FAILURE )
    STOP
  END IF


  ! Get the file format type
  ! ------------------------
  WRITE( *, FMT = '( /5x, "Select the format type for ", a )' ) TRIM( Filename )
  DO n = 1, N_FORMATS
    WRITE( *, FMT = '( 10x, i1, ") ", a )' ) FORMAT_CODE(n), &
                                             TRIM( FORMAT_NAME(n) )
  END DO
  WRITE( *, FMT = '( 5x, "Enter choice: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT    = '( i10 )', &
           IOSTAT = IO_Status ) File_Format
  ! Invalid input
  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input error for '//TRIM( Filename )//' format type.', &
                          FAILURE )
    STOP
  END IF
  ! Invalid value
  IF ( .NOT. ANY( FORMAT_CODE == File_Format ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid format type for '//TRIM( Filename )//'.', &
                          FAILURE )
    STOP
  END IF


  ! Dump the SpcCoeff file
  ! ----------------------
  Data_Type_Select: SELECT CASE ( Data_Type )


    ! Process the SENSOR files
    ! ------------------------
    CASE ( SENSOR_TYPE )


      ! Read the SENSOR SpcCoeff file
      ! -----------------------------
      WRITE( *, '( /5x, "Reading SENSOR SpcCoeff datafile ", a, " ..." )' ) TRIM( Filename )
      Sensor_Format_Select: SELECT CASE ( File_Format )

        CASE ( NETCDF_FORMAT )
          Error_Status = Read_SpcCoeff_netCDF( Filename, &
                                               SpcCoeff_Sensor )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF SENSOR SpcCoeff file '//&
                                  TRIM( Filename ), &
                                  Error_Status )
            STOP
          END IF
       
        CASE ( BINARY_FORMAT )
          Error_Status = Read_SpcCoeff_Binary( Filename, &
                                               SpcCoeff_Sensor )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading Binary SENSOR SpcCoeff file '//&
                                  TRIM( Filename ), &
                                  Error_Status )
            STOP
          END IF

      END SELECT Sensor_Format_Select


      ! Dump the file
      ! -------------
      WRITE(*,'(/5x, "Sensor_Descriptor:")' )
      WRITE(*,'(6(1x,a))') SpcCoeff_Sensor%Sensor_Descriptor

      WRITE(*,'(/5x, "Sensor_Type:")' )
      WRITE(*,'(60(1x,i0))') SpcCoeff_Sensor%Sensor_Type

      WRITE(*,'(/5x, "WMO_Satellite_ID:")' )
      WRITE(*,'(20(1x,i0))') SpcCoeff_Sensor%WMO_Satellite_ID

      WRITE(*,'(/5x, "WMO_Sensor_ID:")' )
      WRITE(*,'(20(1x,i0))') SpcCoeff_Sensor%WMO_Sensor_ID

      WRITE(*,'(/5x, "Sensor_Channel:")' )
      WRITE(*,'(20(1x,i4))') SpcCoeff_Sensor%Sensor_Channel

      WRITE(*,'(/5x, "Polarization:")' )
      WRITE(*,'(60(1x,i0))') SpcCoeff_Sensor%Polarization

      WRITE(*,'(/5x, "Is_Solar_Channel:")' )
      WRITE(*,'(60(1x,i0))') SpcCoeff_Sensor%Is_Solar_Channel

      WRITE(*,'(/5x, "Frequency:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Frequency

      WRITE(*,'(/5x, "Wavenumber:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Wavenumber

      WRITE(*,'(/5x, "Planck_C1:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Planck_C1

      WRITE(*,'(/5x, "Planck_C2:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Planck_C2

      WRITE(*,'(/5x, "Band_C1:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Band_C1

      WRITE(*,'(/5x, "Band_C2:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Band_C2

      WRITE(*,'(/5x, "Cosmic_Background_Radiance:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Cosmic_Background_Radiance

      WRITE(*,'(/5x, "Solar_Irradiance:")' )
      WRITE(*,'(7(1x,es13.6))') SpcCoeff_Sensor%Solar_Irradiance


      ! Destroy the structures
      ! ----------------------
      Error_Status = Destroy_SpcCoeff( SpcCoeff_Sensor )
      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( PROGRAM_NAME, &
                              'Error destroying SpcCoeff_Sensor structure.', &
                              WARNING )
      END IF


    ! Process the SPECTRAL files
    ! --------------------------
    CASE ( SPECTRAL_TYPE )

      WRITE( *, '( /5x, "SPECTRAL SpcCoeff dump not implemented")')

  END SELECT Data_Type_Select

END PROGRAM SpcCoeff_Dump
