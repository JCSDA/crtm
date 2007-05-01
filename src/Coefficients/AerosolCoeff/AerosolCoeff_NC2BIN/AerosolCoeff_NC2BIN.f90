!
! AerosolCoeff_NC2BIN
!
! Program to read a netCDF format CRTM AerosolCoeff data file and
! write a CRTM "Binary" format AerosolCoeff data file
!
!
! CREATION HISTORY:
!       Written by: Paul van Delst, CIMSS/SSEC, 28-Apr-2007
!                   paul.vandelst@ssec.wisc.edu
!

PROGRAM AerosolCoeff_NC2BIN

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, &
                                  Program_Message, Display_Message
  USE AerosolCoeff_Define   , ONLY: AerosolCoeff_type, &
                                  Equal_AerosolCoeff
  USE AerosolCoeff_Binary_IO, ONLY: Write_AerosolCoeff_Binary, &
                                  Read_AerosolCoeff_Binary
  USE AerosolCoeff_netCDF_IO, ONLY: Read_AerosolCoeff_netCDF
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'AerosolCoeff_NC2BIN'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: NC_Filename, BIN_Filename
  TYPE(AerosolCoeff_type) :: AerosolC, AerosolC_Check
  
  ! Program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read a netCDF format CRTM AerosolCoeff '//&
                        'data file and write a CRTM "Binary" format '//&
                        'AerosolCoeff data file.', &
                        '$Revision$')
  
  ! Get the netCDF filename
  WRITE(*,FMT='(/5x,"Enter the netCDF AerosolCoeff filename: ")', ADVANCE='NO')
  READ(*,'(a)') NC_Filename
  NC_Filename = ADJUSTL(NC_Filename)
  BIN_Filename = TRIM(NC_Filename)//'.bin'
  
  ! Read the netCDF File
  WRITE(*,'(/5x,"Reading the netCDF file...")')
  Error_Status = Read_AerosolCoeff_netCDF( NC_Filename, AerosolC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading netCDF AerosolCoeff file '//&
                          TRIM(NC_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Write the Binary File
  WRITE(*,'(/5x,"Writing the Binary file...")')
  Error_Status = Write_AerosolCoeff_Binary( BIN_Filename, AerosolC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Binary AerosolCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  
  ! Check the created Binary file
  WRITE(*,'(/5x,"Checking the Binary file...")')
  Error_Status = Read_AerosolCoeff_Binary( BIN_Filename, AerosolC_Check )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error test reading generated Binary AerosolCoeff file '//&
                          TRIM(BIN_Filename), &
                          Error_Status )
    STOP
  END IF
  
  Error_Status = Equal_AerosolCoeff( AerosolC, AerosolC_Check )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Check structure is different!', &
                          Error_Status )
    STOP
  END IF

END PROGRAM AerosolCoeff_NC2BIN

