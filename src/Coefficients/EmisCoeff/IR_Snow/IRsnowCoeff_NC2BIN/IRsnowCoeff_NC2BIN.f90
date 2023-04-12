!
! ! IRsnowCoeff_NC2BIN
!
! Program to convert a CRTM IRsnowCoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!
!       Written by: Cheng Dang, 05-Mar-2022
!                   dangch@ucar.edu

PROGRAM IRsnowCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility  , ONLY: Create_SignalFile
  USE IRsnowCoeff_Define  , ONLY: IRsnowCoeff_type
  USE IRsnowCoeff_IO      , ONLY: IRsnowCoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'IRsnowCoeff_NC2BIN'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: NC_Filename, BIN_Filename

  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM IRsnowCoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$')
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF IRsnowCoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary IRsnowCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)
  ! ...Sanity check that they're not the same
  IF ( BIN_Filename == NC_Filename ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'IRsnowCoeff netCDF and Binary filenames are the same!', &
                          FAILURE )
    STOP
  END IF

  ! Perform the conversion
  err_stat = IRsnowCoeff_netCDF_to_Binary( NC_Filename, BIN_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'IRsnowCoeff netCDF -> Binary conversion failed!', &
                          FAILURE )
    STOP
  END IF

  ! Create a signal file indicating success
  err_stat = Create_SignalFile( BIN_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(BIN_Filename), &
                          FAILURE )
  END IF

END PROGRAM IRsnowCoeff_NC2BIN
