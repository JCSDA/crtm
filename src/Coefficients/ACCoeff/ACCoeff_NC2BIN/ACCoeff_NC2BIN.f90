!
! ACCoeff_NC2BIN
!
! Program to convert a CRTM ACCoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 27-Jan-2011
!                   paul.vandelst@noaa.gov
!

PROGRAM ACCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility, ONLY: Create_SignalFile
  USE ACCoeff_Define    , ONLY: ACCoeff_type
  USE ACCoeff_IO        , ONLY: ACCoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'ACCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: nc_filename, bin_filename, msg
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM ACCoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$')
  
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF ACCoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') nc_filename
  nc_filename = ADJUSTL(nc_filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary ACCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') bin_filename
  bin_filename = ADJUSTL(bin_filename)
  ! ...Sanity check that they're not the same
  IF ( bin_filename == nc_filename ) THEN
    msg = 'ACCoeff netCDF and Binary filenames are the same!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Perform the conversion
  err_stat = ACCoeff_netCDF_to_Binary( nc_filename, bin_filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'ACCoeff netCDF -> Binary conversion failed!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Create a signal file indicating success
  err_stat = Create_SignalFile( bin_filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(bin_filename), &
                          FAILURE )
  END IF

END PROGRAM ACCoeff_NC2BIN

