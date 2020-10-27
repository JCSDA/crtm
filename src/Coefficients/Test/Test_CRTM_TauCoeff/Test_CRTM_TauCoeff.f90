!
! Test_CRTM_TauCoeff
!
! Program to test the CRTM_TauCoeff module and dependencies.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Nov-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_CRTM_TauCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, Display_Message
  USE CRTM_TauCoeff  , ONLY: CRTM_Load_TauCoeff, CRTM_Destroy_TauCoeff
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_CRTM_TauCoeff'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER, PARAMETER :: NSENSORS=4
  CHARACTER(*), PARAMETER, DIMENSION(NSENSORS) :: SENSORID=&
    (/ 'amsua_n17', &
       'hirs3_n17', &
       'ssmis_f16', &
       'imgr_g11 ' /)
  

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: n



  ! Loop over sensors
  DO n = 1, NSENSORS
    ! Output info
    WRITE(*,'(/5x, "Reading TauCoeff data for:", 10(1x,a,:,","))') SENSORID(1:n)
    ! Message
    WRITE(Message,'("n=",i0)') n
    ! Load the TauCoeff data
    Error_Status = CRTM_Load_TauCoeff( SensorID=SENSORID(1:n) )
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error loading TauCoeff data for '//TRIM(Message), & 
                            Error_Status )
      STOP
    END IF
    ! Destroy the TauCoeff data
    Error_Status = CRTM_Destroy_TauCoeff()
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying TauCoeff data for '//TRIM(Message), & 
                            Error_Status )
      STOP
    END IF
  END DO

  ! Default datafile read
  WRITE(*,'(/5x, "Reading default TauCoeff data file")')
  ! Load the TauCoeff data
  Error_Status = CRTM_Load_TauCoeff()
  IF ( Error_Status/=SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error loading default TauCoeff data', & 
                          Error_Status )
    STOP
  END IF
  ! Destroy the TauCoeff data
  Error_Status = CRTM_Destroy_TauCoeff()
  IF ( Error_Status/=SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying default TauCoeff data', & 
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_CRTM_TauCoeff
