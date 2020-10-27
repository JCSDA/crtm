!
! Test_CRTM_SensorInfo
!
! Program to test the CRTM_SensorInfo module.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Sep-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_CRTM_SensorInfo

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE SensorInfo_Define
  USE SensorInfo_LinkedList
  USE SensorInfo_IO
  USE CRTM_SensorInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_CRTM_SensorInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: SENSORINFO_FILENAME='SensorInfo'
  INTEGER, PARAMETER :: SL = 20


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n_Sensors, n
  INTEGER :: nChannels
  INTEGER :: nFOVs
  INTEGER :: Detector
  INTEGER :: WMO_SensorID
  INTEGER :: WMO_SatelliteID
  CHARACTER(256) :: SensorName
  CHARACTER(256) :: SatelliteName
  TYPE(SensorInfo_type)      :: SensorInfo
  TYPE(SensorInfo_List_type) :: SensorInfo_List
  
  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the CRTM_SensorInfo module.', &
                       '$Revision: 1.1.2.1 $' )

  ! Read the sensorinfo file
  Error_Status = Read_SensorInfo( SENSORINFO_FILENAME, &
                                  SensorInfo_List, &
                                  Quiet=1 )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SensorInfo file '//SENSORINFO_FILENAME, &
                          FAILURE )
    STOP
  END IF

  ! Count the number of sensors
  n_Sensors = Count_SensorInfo_Nodes( SensorInfo_List )
  IF ( n_Sensors < 1 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'SensorInfo_List is empty.', &
                          FAILURE )
    STOP
  END IF
  WRITE( *, '( 5x, "Number of sensors in list: ", i5 )' ) n_Sensors

  ! Loop over all sensors in the list
  Sensor_Loop: DO n = 1, n_Sensors

    ! Get the current SensorInfo node from the list
    Error_Status = GetFrom_SensorInfo_List( SensorInfo_List, &
                                            n, &
                                            SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error retrieving SensorInfo data for sensor # ", i5 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

    ! Get the SensorAttributes
    Error_Status = CRTM_Get_SensorAttributes(TRIM(SensorInfo%File_Prefix)   , &
                                             nChannels      =nChannels      , &
                                             nFOVs          =nFOVs          , &
                                             Detector       =Detector       , &
                                             WMO_SensorID   =WMO_SensorID   , &
                                             WMO_SatelliteID=WMO_SatelliteID, &
                                             SensorName     =SensorName     , &
                                             SatelliteName  =SatelliteName    ) 
    ! Output results
    WRITE(*,'(/5x,"Sensor ID: ", a)') TRIM(SensorInfo%File_Prefix)
    WRITE(*,'(8x,"nChannels       = ",i0)') nChannels
    WRITE(*,'(8x,"Detector        = ",i0)') nFOVs          
    WRITE(*,'(8x,"nFOVs           = ",i0)') Detector       
    WRITE(*,'(8x,"WMO_SensorID    = ",i0)') WMO_SensorID   
    WRITE(*,'(8x,"WMO_SatelliteID = ",i0)') WMO_SatelliteID
    WRITE(*,'(8x,"SensorName      = ",a)') TRIM(SensorName)
    WRITE(*,'(8x,"SatelliteName   = ",a)') TRIM(SatelliteName)
read(*,*)
    ! Destroy the current SensorInfo node in
    ! preparation for the next node retrieval
    Error_Status = Destroy_SensorInfo( SensorInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SensorInfo data structure.', &
                            Error_status )
      STOP
    END IF

  END DO Sensor_Loop

  ! Destroy the linked list
  Error_Status = Destroy_SensorInfo_List( SensorInfo_List )
  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          '( "Error destroying SensorInfo_List.', &
                          Error_Status )
  END IF

END PROGRAM Test_CRTM_SensorInfo
