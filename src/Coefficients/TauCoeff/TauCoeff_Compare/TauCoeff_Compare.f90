!
! TauCoeff_Compare
!
! Program to compare TauCoeff data read from either netCDF or
! CRTM Binary format files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 02-Apr-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM TauCoeff_Compare

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility,       ONLY: File_Exists
  USE Message_Handler,    ONLY: SUCCESS, FAILURE, INFORMATION, &
                                Program_Message, Display_Message
  USE TauCoeff_Define,    ONLY: TauCoeff_type, &
                                Destroy_TauCoeff, Equal_TauCoeff
  USE TauCoeff_Binary_IO, ONLY: Read_TauCoeff_Binary
  USE TauCoeff_netCDF_IO, ONLY: Read_TauCoeff_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'TauCoeff_Compare'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &

  ! Allowable formats.
  ! Format codes MUST start at 1 and increment by 1.
  INTEGER, PARAMETER :: N_FORMATS = 2
  INTEGER, PARAMETER :: NETCDF_FORMAT = 1
  INTEGER, PARAMETER :: BINARY_FORMAT = 2
  INTEGER, PARAMETER, DIMENSION(N_FORMATS) :: &
    FORMAT_CODE = (/ NETCDF_FORMAT, &
                     BINARY_FORMAT /)
  CHARACTER(*), PARAMETER, DIMENSION(N_FORMATS) :: &
    FORMAT_NAME = (/ 'netCDF', &
                     'Binary' /)

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: File1_Name
  CHARACTER(256) :: File2_Name
  INTEGER :: File1_Format
  INTEGER :: File2_Format
  TYPE(TauCoeff_type) :: TauCoeff1
  TYPE(TauCoeff_type) :: TauCoeff2

  ! Output prgram header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to compare TauCoeff data read from either '//&
                        'netCDF or CRTM Binary format files.', &
                        '$Revision$' )

  ! Get the filenames and formats
  CALL Get_File_Name_and_Format( 'FIRST',  File1_Name, File1_Format )
  CALL Get_File_Name_and_Format( 'SECOND', File2_Name, File2_Format )

  ! Read the files
  CALL Read_the_File( 'FIRST',  File1_Name, File1_Format, TauCoeff1 )
  CALL Read_the_File( 'SECOND', File2_Name, File2_Format, TauCoeff2 )

  ! Compare the TauCoeff structures
  WRITE( *, '( /5x, "Comparing the two TauCoeff structures ..." )' )
  Error_Status = Equal_TauCoeff( TauCoeff1, TauCoeff2 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Differences found in TauCoeff structure comparison.', &
                          Error_Status )
  ELSE
    CALL Display_Message( PROGRAM_NAME, &
                          'TauCoeff structures are equal.', &
                          INFORMATION )
  END IF

  ! Destroy the structures
  Error_Status = Destroy_TauCoeff( TauCoeff1 )
  Error_Status = Destroy_TauCoeff( TauCoeff2 )


CONTAINS

  SUBROUTINE Get_File_Name_and_Format(Rank, Filename, FileFormat)
    CHARACTER(*), INTENT(IN)  :: Rank
    CHARACTER(*), INTENT(OUT) :: Filename
    INTEGER,      INTENT(OUT) :: FileFormat
    INTEGER :: n, IO_Status

    ! Get the filename
    WRITE(*, FMT    ='( /5x, "Enter the ",a," TauCoeff file: " )', &
             ADVANCE='NO' ) TRIM(Rank)
    READ(*, '(a)') Filename
    Filename = ADJUSTL(Filename)
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'File '//TRIM(Filename)//' not found.', &
                            FAILURE )
      STOP
    END IF

    ! Get the file format type
    WRITE(*, FMT='(/5x, "Select the format type for ", a)') TRIM(Filename)
    DO n = 1, N_FORMATS
      WRITE(*, FMT='(10x, i1, ") ", a)') FORMAT_CODE(n), &
                                         TRIM(FORMAT_NAME(n))
    END DO
    WRITE(*, FMT='(5x, "Enter choice: ")', &
             ADVANCE='NO')
    READ(*, FMT   ='(i10)', &
            IOSTAT=IO_Status) FileFormat

    ! Invalid input
    IF ( IO_Status /= 0 ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Input error for '//TRIM(Filename)//' format type.', &
                            FAILURE )
      STOP
    END IF

    ! Invalid value
    IF ( .NOT. ANY( FORMAT_CODE == FileFormat ) ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Invalid format type for '//TRIM(Filename)//'.', &
                            FAILURE )
      STOP
    END IF
  END SUBROUTINE Get_File_Name_and_Format


  SUBROUTINE Read_the_File(Rank, Filename, FileFormat, TauCoeff) 
    CHARACTER(*),        INTENT(IN)     :: Rank
    CHARACTER(*),        INTENT(IN)     :: Filename
    INTEGER,             INTENT(IN)     :: FileFormat
    TYPE(TauCoeff_type), INTENT(IN OUT) :: TauCoeff
    INTEGER :: Error_Status
    
    WRITE(*, '(/5x, "Reading ",a," TauCoeff datafile ",a," ...")') TRIM(Rank), TRIM(Filename)

    SELECT CASE ( FileFormat )
      CASE ( NETCDF_FORMAT )
        Error_Status = Read_TauCoeff_netCDF( Filename, &
                                             TauCoeff )
      CASE ( BINARY_FORMAT )
        Error_Status = Read_TauCoeff_Binary( Filename, &
                                             TauCoeff )
    END SELECT

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error reading '//FORMAT_NAME(FileFormat)//&
                            ' TauCoeff file '//TRIM(Filename), &
                            Error_Status )
      STOP
    END IF

  END SUBROUTINE Read_the_File

END PROGRAM TauCoeff_Compare
