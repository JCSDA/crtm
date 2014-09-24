!
! CloudCoeff_Convert
!
! Program to convert a CRTM CloudCoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 27-Apr-2007
!                   paul.vandelst@noaa.gov
!

PROGRAM CloudCoeff_Convert

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility, ONLY: Create_SignalFile
  USE CloudCoeff_Define , ONLY: CloudCoeff_type
  USE CloudCoeff_IO     , ONLY: CloudCoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_Convert'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  '$Id$'
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: NC_Filename, BIN_Filename
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM CloudCoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$')
  
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF CloudCoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary CloudCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)
  ! ...Sanity check that they're not the same
  IF ( BIN_Filename == NC_Filename ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'CloudCoeff netCDF and Binary filenames are the same!', &
                          FAILURE )
    STOP
  END IF
  
  
  ! Perform the conversion
  err_stat = CloudCoeff_netCDF_to_Binary( NC_Filename, BIN_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'CloudCoeff netCDF -> Binary conversion failed!', &
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

END PROGRAM CloudCoeff_Convert

