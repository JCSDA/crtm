!
! IRwaterCoeff_BIN2NC
!
! Program to convert a CRTM IRwaterCoeff data file
! from netCDF to Binary format
!
!
! CREATION HISTORY:
!
!       Written by: Cheng Dang, 05-Mar-2022
!                   dangch@ucar.edu

PROGRAM  IRwaterCoeff_BIN2NC

    ! -----------------
    ! Environment setup
    ! -----------------
    ! Module usage
    USE Message_Handler     , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
    USE SignalFile_Utility  , ONLY: Create_SignalFile
    USE IRwaterCoeff_Define , ONLY: IRwaterCoeff_type
    USE IRwaterCoeff_IO     , ONLY: IRwaterCoeff_Binary_to_netCDF
    ! Disable implicit typing
    IMPLICIT NONE

    ! ----------
    ! Parameters
    ! ----------
    CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'IRwaterCoeff_BIN2NC'

    ! ---------
    ! Variables
    ! ---------
    INTEGER :: err_stat
    CHARACTER(256) :: NC_Filename, BIN_Filename

    ! Program header
    CALL Program_Message( PROGRAM_NAME, &
                          'Program to convert a CRTM IRwaterCoeff data file '//&
                          'from Binary to netCDF format.', &
                          '$Revision$')
    ! Get the filenames
    WRITE(*,FMT='(/5x,"Enter the INPUT Binary IRwaterCoeff filename : ")', ADVANCE='NO')
    READ(*,'(a)') BIN_Filename
    BIN_Filename = ADJUSTL(BIN_Filename)
    WRITE(*,FMT='(/5x,"Enter the OUTPUT netCDF IRwaterCoeff filename: ")', ADVANCE='NO')
    READ(*,'(a)') NC_Filename
    NC_Filename = ADJUSTL(NC_Filename)
    ! ...Sanity check that they're not the same
    IF ( BIN_Filename == NC_Filename ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IRwaterCoeff netCDF and Binary filenames are the same!', &
                            FAILURE )
      STOP
    END IF

    ! Perform the conversion
    err_stat = IRwaterCoeff_Binary_to_netCDF( BIN_Filename, NC_Filename )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'IRwaterCoeff Binary -> netCDF conversion failed!', &
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

  END PROGRAM IRwaterCoeff_BIN2NC
