!
! Test_ChannelProperties
!
! Program to test the AIRS L2 channel properties file I/O functions.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Jan-2003
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_ChannelProperties

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE AIRS_Define
  USE AIRS_ChannelProperties
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Test_ChannelProperties'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER( * ), PARAMETER :: OUTPUT_FILE = 'L2.ChannelProperties.Test'


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  CHARACTER(256) :: Filename
  INTEGER :: l
  TYPE(AIRS_ChannelProperties_type), DIMENSION(N_AIRS_CHANNELS) :: ChannelProperties

  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test AIRS channel properties file I/O.', &
                       '$Revision: 1.3' )

  ! Get AIRS L2 channel properties filename
  WRITE( *, FMT     = '( /5x, "Enter an AIRS L2 channel properties filename: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Filename
  Filename = ADJUSTL( Filename )

  ! ...and read it.
  Error_Status = Read_AIRS_ChannelProperties( Filename, &
                                              ChannelProperties )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AIRS L2 channel properties file '//&
                          TRIM( Filename ), &
                          Error_Status )
    STOP
  END IF

  ! Write a test channel properties file
  Error_Status = Write_AIRS_ChannelProperties( OUTPUT_FILE, &
                                               ChannelProperties )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing AIRS L2 channel properties file '//&
                          TRIM( OUTPUT_FILE ), &
                          Error_Status )
    STOP
  END IF

  ! Read the test channel properties file
  Error_Status = Read_AIRS_ChannelProperties( OUTPUT_FILE, &
                                              ChannelProperties )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading AIRS L2 channel properties file '//&
                          TRIM( OUTPUT_FILE ), &
                          Error_Status )
    STOP
  END IF

  ! Output some data
  WRITE( *, * )
  DO l = 1, N_AIRS_CHANNELS, 50
    WRITE( *, '(i5,f9.3,1x,a5,i5,f7.4,f6.3,f8.4,2f8.1,f6.3,i3,i3,i3,1x,a8)' ) &
      ChannelProperties( l )
  END DO

END PROGRAM Test_ChannelProperties
