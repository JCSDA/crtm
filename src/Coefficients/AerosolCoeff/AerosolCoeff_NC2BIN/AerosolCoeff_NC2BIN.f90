!
! AerosolCoeff_NC2BIN
!
! Program to convert a CRTM AerosolCoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, 28-Apr-2007
!                   paul.vandelst@noaa.gov
!

PROGRAM AerosolCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility , ONLY: Create_SignalFile
  USE AerosolCoeff_Define, ONLY: AerosolCoeff_type
  USE AerosolCoeff_IO    , ONLY: AerosolCoeff_netCDF_to_Binary
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AerosolCoeff_NC2BIN'
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: Aerosol_Model, NC_Filename, BIN_Filename
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM AerosolCoeff data file '//&
                        'from netCDF to Binary format.', &
                        '$Revision$')
  ! Get the Aerosol_Model
    WRITE(*,FMT='(/5x,"Enter the aerosol model (GOCART or CMAQ) : ")', ADVANCE='NO')
  READ(*,'(a)') Aerosol_Model
  Aerosol_Model = ADJUSTL(Aerosol_Model)
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT netCDF AerosolCoeff filename : ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT Binary AerosolCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)
  ! ...Sanity check that they're not the same
  IF ( BIN_Filename == NC_Filename ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'AerosolCoeff netCDF and Binary filenames are the same!', &
                          FAILURE )
    STOP
  END IF
  
  
  ! Perform the conversion
  err_stat = AerosolCoeff_netCDF_to_Binary( Aerosol_Model, NC_Filename, BIN_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'AerosolCoeff netCDF -> Binary conversion failed!', &
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

END PROGRAM AerosolCoeff_NC2BIN

