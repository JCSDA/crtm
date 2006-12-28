!
! Test_CRTM_LifeCycle
!
! Program to test the CRTM_LifeCycle module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 15-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_CRTM_LifeCycle


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Message_Handler        , ONLY: SUCCESS, Display_Message, Program_Message
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type
  USE CRTM_LifeCycle         , ONLY: CRTM_Init, CRTM_Destroy
  USE CRTM_Test_Utility      , ONLY: Print_ChannelInfo
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME  ='Test_CRTM_LifeCycle'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID=&
    '$Id$'
  CHARACTER(*), PARAMETER :: CLOUDCOEFF_FILE  ='CloudCoeff.bin'
  CHARACTER(*), PARAMETER :: AEROSOLCOEFF_FILE='AerosolCoeff.bin'
  CHARACTER(*), PARAMETER :: EMISCOEFF_FILE   ='EmisCoeff.bin'
  INTEGER,      PARAMETER :: NSENSORS=4
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
  INTEGER :: n, n2
  TYPE(CRTM_ChannelInfo_type), DIMENSION(NSENSORS) :: ChannelInfo


  ! Program message
  CALL Program_Message(PROGRAM_NAME,&
                       'Program to test the initialisation and destruction '//&
                       'procedures in the CRTM_LifeCycle module.',&
                       '$Revision$' )
                       
  ! Loop over sensors
  DO n = 1, NSENSORS
    ! Output info
    WRITE(*,'(//5x, "-----------------------------------------------")')
    WRITE(*,'(  5x, "Testing CRTM Initialisation and Destruction for:", 10(1x,a,:,","))') SENSORID(1:n)
    WRITE(*,'(  5x, "-----------------------------------------------")')
    WRITE(*,'( /5x, "Press <ENTER> to begin test...")')
    READ(*,*)
    ! Message
    WRITE(Message,'("n=",i0)') n
    ! Initialise the CRTM
    WRITE(*,'(/5x, "Initialising CRTM...")')
    Error_Status = CRTM_Init(ChannelInfo(1:n)                   , &
                             SENSORID(1:n)                      , &
                             CloudCoeff_File  =CloudCoeff_File  , &
                             AerosolCoeff_File=AerosolCoeff_File, &
                             EmisCoeff_File   =EmisCoeff_File     )
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error initialising CRTM for '//TRIM(Message), & 
                            Error_Status )
      STOP
    ELSE
      DO n2 = 1, n
        CALL Print_ChannelInfo(TRIM(SENSORID(n2))//'.ChannelInfo', ChannelInfo(n2))
      END DO
      WRITE(*,'(5x, "Initialisation successful!")')
    END IF
    ! Destroy the CRTM
    WRITE(*,'(/5x, "Destroying CRTM...")')
    Error_Status = CRTM_Destroy(ChannelInfo)
    IF ( Error_Status/=SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error destroying CRTM for '//TRIM(Message), & 
                            Error_Status )
      STOP
    ELSE
      WRITE(*,'(5x, "Destruction successful!")')
    END IF
    IF ( n < NSENSORS ) THEN
      WRITE(*,'(/5x, "Press <ENTER> to continue...")')
      READ(*,*)
    END IF
  END DO

END PROGRAM Test_CRTM_LifeCycle
