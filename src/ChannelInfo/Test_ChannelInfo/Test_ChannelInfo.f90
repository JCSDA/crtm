!
! Test_ChannelInfo
!
! Program to test the CRTM ChannelInfo structure manuipulation procedures
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 11-Jul-2005
!                       paul.vandelst@ssec.wisc.edu
!


PROGRAM Test_ChannelInfo

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds             , ONLY: fp
  USE Message_Handler        , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                     Display_Message, Program_Message
  USE CRTM_ChannelInfo_Define, ONLY: CRTM_ChannelInfo_type      , &
                                     CRTM_Associated_ChannelInfo, &
                                     CRTM_Destroy_ChannelInfo   , &
                                     CRTM_Allocate_ChannelInfo  , &
                                     CRTM_Assign_ChannelInfo    , &
                                     CRTM_Equal_ChannelInfo
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_ChannelInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Test loops
  INTEGER, PARAMETER :: MAX_N_LOOPS  = 100
  INTEGER, PARAMETER :: INFO_N_LOOPS = 10


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: i, n
  TYPE(CRTM_ChannelInfo_type) :: cInfo(1), cInfo_Copy(1)


  ! Output header
  ! -------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the CRTM ChannelInfo structure '//&
                        'manipulation procedures.', &
                        '$Revision$' )


  
  ! Test loop
  ! ---------
  WRITE(*,'( /5x, "Looping for ChannelInfo test ..." )' )
  DO n = 1, MAX_N_LOOPS
    ! Allocate the ChannelInfo structure
    Error_Status = CRTM_Allocate_ChannelInfo( (/n/), cInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Allocation failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! Fill structure with pretend data
    cInfo(1)%Sensor_Id        = 'sensor_platform'
    cInfo(1)%WMO_Satellite_Id = 1
    cInfo(1)%WMO_Sensor_Id    = 2
    cInfo(1)%Sensor_Index     = 3
    cInfo(1)%Sensor_Channel = (/(i+20,i=1,n)/)
    cInfo(1)%Channel_Index  = (/(i,i=1,n)/)

    ! Test association
    IF ( ANY( .NOT. CRTM_Associated_ChannelInfo( cInfo ) ) ) THEN
      WRITE( Message, '( "Association test failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! Copy structure
    Error_Status = CRTM_Assign_ChannelInfo( cInfo, cInfo_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Copy failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! Compare structure
    Error_Status = CRTM_Equal_ChannelInfo( cInfo, cInfo_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Comparison failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    IF ( MOD( n, INFO_N_LOOPS ) == 0 ) THEN
      WRITE( Message, '( "Completed loop #", i0, " of ", i0 )' ) n, MAX_N_LOOPS
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            INFORMATION )
    END IF
    
    ! Cleanup
    Error_Status = CRTM_Destroy_ChannelInfo( cInfo )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Destroy failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    Error_Status = CRTM_Destroy_ChannelInfo( cInfo_Copy )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Copy destroy failed on attempt # ", i0 )' ) n
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
  END DO
  
END PROGRAM Test_ChannelInfo
