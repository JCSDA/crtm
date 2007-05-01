!
! CloudCoeff_NC2BIN
!
! Program to read a netCDF format CRTM CloudCoeff data file and
! write a CRTM "Binary" format CloudCoeff data file
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, CIMSS/SSEC, 27-Apr-2007
!                   paul.vandelst@ssec.wisc.edu
!

PROGRAM CloudCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, &
                                  Program_Message, Display_Message
  USE CloudCoeff_Define   , ONLY: CloudCoeff_type, &
                                  Equal_CloudCoeff
  USE CloudCoeff_Binary_IO, ONLY: Write_CloudCoeff_Binary, &
                                  Read_CloudCoeff_Binary
  USE CloudCoeff_netCDF_IO, ONLY: Read_CloudCoeff_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'CloudCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename, BIN_Filename
  TYPE(CloudCoeff_type) :: CloudC, CloudC_Check
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read a netCDF format CRTM CloudCoeff '//&
                        'data file and write a CRTM "Binary" format '//&
                        'CloudCoeff data file.', &
                        '$Revision$')
  
  ! Get the netCDF filename
  WRITE(*,FMT='(/5x,"Enter the netCDF CloudCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  BIN_Filename = TRIM(NC_Filename)//'.bin'
  
  ! Read the netCDF File
  WRITE(*,'(/5x,"Reading the netCDF file...")')
  Error_Status = Read_CloudCoeff_netCDF( NC_Filename, CloudC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF CloudCoeff file '//&
                          TRIM(NC_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Write the Binary File
  WRITE(*,'(/5x,"Writing the Binary file...")')
  Error_Status = Write_CloudCoeff_Binary( BIN_Filename, CloudC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary CloudCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Check the created Binary file
  WRITE(*,'(/5x,"Checking the Binary file...")')
  Error_Status = Read_CloudCoeff_Binary( BIN_Filename, CloudC_Check )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading generated Binary CloudCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  
  Error_Status = Equal_CloudCoeff( CloudC, CloudC_Check )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Check structure is different!', &
                          Error_Status )
    STOP
  END IF

END PROGRAM CloudCoeff_NC2BIN

