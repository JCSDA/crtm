!
! SEcategory_BIN2NC
!
! Program to convert a CRTM SEcategory data file
! from Binary to netCDF format
!
!
! CREATION HISTORY:
!       Written by: Cheng Dang, 02-12-2022
!                   dangch@ucar.edu

PROGRAM SEcategory_BIN2NC

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE SignalFile_Utility , ONLY: Create_SignalFile
  USE SEcategory_Define  , ONLY: SEcategory_type
  USE SEcategory_IO      , ONLY: SEcategory_Binary_to_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'SEcategory_BIN2NC'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: err_stat
  CHARACTER(256) :: NC_Filename, BIN_Filename

  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert a CRTM SEcategory data file '//&
                        'from Binary to netCDF format.', &
                        '$Revision$')
  ! Get the filenames
  WRITE(*,FMT='(/5x,"Enter the INPUT Binary SEcategory filename : ")', ADVANCE='NO')
  READ(*,'(a)') BIN_Filename
  BIN_Filename = ADJUSTL(BIN_Filename)
  WRITE(*,FMT='(/5x,"Enter the OUTPUT netCDF SEcategory filename: ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  ! ...Sanity check that they're not the same
  IF ( BIN_Filename == NC_Filename ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SEcategory netCDF and Binary filenames are the same!', &
                          FAILURE )
    STOP
  END IF

  ! Perform the conversion
  err_stat = SEcategory_Binary_to_netCDF( BIN_Filename, NC_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SEcategory Binary -> netCDF conversion failed!', &
                          FAILURE )
    STOP
  END IF

  ! Create a signal file indicating success
  err_stat = Create_SignalFile( NC_Filename )
  IF ( err_stat /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating signal file for '//TRIM(NC_Filename), &
                          FAILURE )
  END IF

END PROGRAM SEcategory_BIN2NC
