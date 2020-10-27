!
! SpcCoeff_NC2BIN
!
! Program to convert netCDF format SpcCoeff files to the Binary format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 27-Jul-2002
!                       paul.vandelst@noaa.gov
!

PROGRAM SpcCoeff_NC2BIN

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE String_Utility    , ONLY: StrLowCase
  USE SignalFile_Utility, ONLY: Create_SignalFile
  USE SpcCoeff_Define   , ONLY: SpcCoeff_type
  USE SpcCoeff_IO       , ONLY: SpcCoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SpcCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: msg
  CHARACTER(256) :: nc_filename
  CHARACTER(256) :: bin_filename
  CHARACTER(256) :: answer
  INTEGER :: version

  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM SpcCoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$' )


  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF SpcCoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') nc_filename
  nc_filename = ADJUSTL(nc_filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary SpcCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') bin_filename
  bin_filename = ADJUSTL(bin_filename)
  ! ...Sanity check that they're not the same
  IF ( bin_filename == nc_filename ) THEN
    msg = 'SpcCoeff netCDF and Binary filenames are the same!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  END IF


  ! Ask if version increment required
  WRITE(*,FMT='(/5x,"Increment the OUTPUT version number? [y/n]: ")', ADVANCE='NO')
  READ(*,'(a)') answer
  answer = StrLowCase(ADJUSTL(answer))
  SELECT CASE( TRIM(answer) )
    CASE('y','yes')
      version = -1
    CASE DEFAULT
      version = 0
  END SELECT
  

  ! Perform the conversion
  err_stat = SpcCoeff_netCDF_to_Binary( nc_filename, bin_filename, Version = version )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'SpcCoeff netCDF -> Binary conversion failed!'
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
  ELSE
    msg = 'SpcCoeff netCDF -> Binary conversion successful!'
    CALL Display_Message( PROGRAM_NAME, msg, err_stat )
  END IF
  
  
  ! Create a signal file indicating success
  err_stat = Create_SignalFile( bin_filename )
  IF ( err_stat /= SUCCESS ) THEN
    msg = 'Error creating signal file for '//TRIM(bin_filename)
    CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
  END IF

END PROGRAM SpcCoeff_NC2BIN
