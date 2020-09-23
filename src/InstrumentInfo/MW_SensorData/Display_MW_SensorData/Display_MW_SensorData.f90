!
! Display_MW_SensorData
!
! Program to display MW_SensorData structure information.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 05-Apr-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Display_MW_SensorData

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds          , ONLY: fp
  USE Message_Handler     , ONLY: SUCCESS, Display_Message, Program_Message
  USE MW_SensorData_Define, ONLY: MW_SensorData_type, &
                                  Get_MW_SensorData_Sensor_ID, &
                                  Load_MW_SensorData, &
                                  Print_MW_SensorData, &
                                  Destroy_MW_SensorData
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Display_MW_SensorData'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(80) :: Sensor_ID
  INTEGER :: Error_Status
  TYPE(MW_SensorData_type) :: MW_SensorData


  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to display MW_SensorData structure information.', &
                       '$Revision$' )


  ! Begin open loop over sensors
  ! ----------------------------
  Sensor_Loop: DO

    ! Get a sensor Id
    Sensor_ID = Get_MW_SensorData_Sensor_ID()
    IF ( Sensor_ID == ' ' ) EXIT Sensor_Loop

    ! Load the current sensor's data
    Error_Status = Load_MW_SensorData( MW_SensorData, &
                                       Sensor_ID=TRIM(Sensor_ID) )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error loading MW sensor data for Sensor ID '//&
                            TRIM(Sensor_ID), &
                            Error_Status )
      STOP
    END IF

    ! Print the data
    CALL Print_MW_SensorData( MW_SensorData )

    ! Destroy he structure
    Error_Status = Destroy_MW_SensorData( MW_SensorData )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying MW_SensorData structure for Sensor ID '//&
                            TRIM(Sensor_ID), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop

END PROGRAM Display_MW_SensorData
