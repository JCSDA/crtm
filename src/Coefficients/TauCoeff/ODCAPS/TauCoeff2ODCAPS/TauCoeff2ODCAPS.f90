!
! TauCoeff2ODCAPS
!
! Program to convert ODCAPS (Optical Depth Combining Absorber and Pressure Space) binary files
! from the old TauCoeff format to the new ODCAPS format for use with the
! multiple-algorithm form of the CRTM.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 12-Jun-2008
!                       paul.vandelst@noaa.gov
!
!       Modified by:    Yong Chen, 18-Jul-2008
!                       Yong.Chen@noaa.gov

PROGRAM TauCoeff2ODCAPS

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility      , ONLY: File_Exists
  USE Message_Handler   , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                                Program_Message, Display_Message
  
  USE File_Utility
  USE Binary_File_Utility
 
  USE TauCoeff_SARTA_Define    , ONLY: TauCoeff_SARTA_type, Destroy_TauCoeff_SARTA
  USE TauCoeff_SARTA_Binary_IO , ONLY: Read_TauCoeff_SARTA_Binary, Write_TauCoeff_ODCAPS_Binary
 
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'TauCoeff2ODCAPS'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)
 
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: TauCoeff_Filename
  CHARACTER(256) :: ODCAPS_Filename
  CHARACTER(20)  :: Sensor_Id
  INTEGER        :: Sensor_Type
  CHARACTER(256)  :: Title         
  CHARACTER(5000) :: History       
  CHARACTER(5000) :: Comment       
  CHARACTER(2000) :: Profile_ID_Tag  
  TYPE(TauCoeff_SARTA_type) :: TauCoeff
 
  INTEGER :: i


  ! Output prgram header
  ! --------------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to convert ODCAPS (Optical Depth Combining Absorber and Pressure Space) binary '//&
                        'files from the old TauCoeff format to the new ODCAPS format for '//&
                        'use with the multiple-algorithm form of the CRTM.', &
                        '$Revision: 2170 $' )

  ! Enter a sensor Id and type
  ! --------------------------
  WRITE( *,FMT='(/5x,"Enter a sensor ID: ")',ADVANCE='NO' )
  READ( *,'(a)' ) Sensor_Id
  
  WRITE( *,'(/5x,"Enter a sensor type")')
  DO i = 1, N_SENSOR_TYPES
    WRITE( *,'(7x,i2,") ",a)') i, SENSOR_TYPE_NAME(i)
  END DO
  WRITE( *,FMT='(/5x,"Select choice: ")',ADVANCE='NO' )
  READ( *,'(i10)' ) Sensor_Type
  

  ! Construct the filenames
  ! -----------------------
  TauCoeff_Filename  = TRIM(Sensor_Id)//'.TauCoeff.bin'  
  ODCAPS_Filename    = TRIM(Sensor_Id)//'.ODCAPS.TauCoeff.bin'
  
  
  ! Read the TauCoeff data
  ! ----------------------
  WRITE( *,'(/5x,"Reading TauCoeff file ",a,"...")' ) TRIM(TauCoeff_Filename)
  Error_Status = Read_TauCoeff_SARTA_Binary( TauCoeff_Filename, TauCoeff )
  
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading binary TauCoeff file '//&
                          TRIM(TauCoeff_Filename), &
                          Error_Status )
    STOP
  END IF

  ! Write the new datafile
  ! ----------------------
  Title = 'ODCAPS optical depth coefficients for '//TRIM(Sensor_Id)
  Error_Status = Write_TauCoeff_ODCAPS_Binary( ODCAPS_Filename, Sensor_Id, Sensor_Type, TauCoeff )
                                     
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing binary ODCAPS file '//&
                          TRIM(ODCAPS_Filename), &
                          Error_Status )
    STOP
  END IF


  ! Clean up
  ! --------
  Error_Status = Destroy_TauCoeff_SARTA( TauCoeff )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying TauCoeff structure', &
                          WARNING )
  END IF
 
END PROGRAM TauCoeff2ODCAPS
