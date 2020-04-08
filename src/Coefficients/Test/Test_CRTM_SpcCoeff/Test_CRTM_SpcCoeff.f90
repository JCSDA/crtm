!
! Test_CRTM_SpcCoeff
!
! Program to test the CRTM_SpcCoeff module and dependencies.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 04-Nov-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_CRTM_SpcCoeff

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, Display_Message
  USE CRTM_SpcCoeff  , ONLY: CRTM_Load_SpcCoeff, CRTM_Destroy_SpcCoeff
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_Forward'
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
    WRITE(*,'(/5x, "Reading SpcCoeff data for:", 10(1x,a,:,","))') SENSORID(1:n)
    ! Message
    WRITE(Message,'("n=",i0)') n
    ! Load the SpcCoeff data
    Error_Status = CRTM_Load_SpcCoeff( SensorID=SENSORID(1:n) )
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error loading SpcCoeff data for '//TRIM(Message), & 
                            Error_Status )
      STOP
    END IF
    ! Destroy the SpcCoeff data
    Error_Status = CRTM_Destroy_SpcCoeff()
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying SpcCoeff data for '//TRIM(Message), & 
                            Error_Status )
      STOP
    END IF
  END DO

  ! Default datafile read
  WRITE(*,'(/5x, "Reading default SpcCoeff data file")')
  ! Load the SpcCoeff data
  Error_Status = CRTM_Load_SpcCoeff()
  IF ( Error_Status/=SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error loading default SpcCoeff data', & 
                          Error_Status )
    STOP
  END IF
  ! Destroy the SpcCoeff data
  Error_Status = CRTM_Destroy_SpcCoeff()
  IF ( Error_Status/=SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying default SpcCoeff data', & 
                          Error_Status )
    STOP
  END IF

END PROGRAM Test_CRTM_SpcCoeff
