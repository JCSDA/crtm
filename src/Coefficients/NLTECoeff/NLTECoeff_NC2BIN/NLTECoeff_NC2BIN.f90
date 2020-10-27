!
! NLTECoeff_NC2BIN
!
! Program to convert a CRTM NLTECoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 25-Jan-2011
!                   paul.vandelst@noaa.gov
!

PROGRAM NLTECoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility, ONLY: Create_SignalFile
  USE NLTECoeff_Define  , ONLY: NLTECoeff_type
  USE NLTECoeff_IO      , ONLY: NLTECoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'NLTECoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: nc_filename, bin_filename, msg
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM NLTECoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$')
  
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF NLTECoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') nc_filename
  nc_filename = ADJUSTL(nc_filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary NLTECoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') bin_filename
  bin_filename = ADJUSTL(bin_filename)
  ! ...Sanity check that they're not the same
  IF ( bin_filename == nc_filename ) THEN
    msg = 'NLTECoeff netCDF and Binary filenames are the same!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Perform the conversion
  err_stat = NLTECoeff_netCDF_to_Binary( nc_filename, bin_filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'NLTECoeff netCDF -> Binary conversion failed!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF
  
  
  ! Create a signal file indicating success
  err_stat = Create_SignalFile( bin_filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(bin_filename), &
                          FAILURE )
  END IF

END PROGRAM NLTECoeff_NC2BIN

