
PROGRAM SRF_ASCII_IO_Test


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE SRF_Define
  USE SRF_ASCII_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'SRF_ASCII_IO_Test'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: SRF_ASCII_IO_Test.f90,v 1.5 2006/08/15 20:51:04 wd20pd Exp $'
  CHARACTER( * ), PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message

  INTEGER         :: pn_pos
  CHARACTER( 80 ) :: pn_fmt

  INTEGER :: Error_Status

  CHARACTER( 256 ) :: FileNAME
  INTEGER :: InFileID, OutFileID

  INTEGER                                  :: n_Channels, l
  INTEGER, DIMENSION( MAX_N_SRF_CHANNELS ) :: Channel_List
  CHARACTER( 256 )                         :: Title
  CHARACTER( 256 )                         :: History
  CHARACTER( 256 )                         :: Sensor_Name
  CHARACTER( 256 )                         :: Platform_Name
  CHARACTER( 256 )                         :: Comment

  TYPE( SRF_type ) :: SRF



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a )' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to test the ASCII SRF I/O functions  ")' )
  WRITE( *, '(/5x, " $Revision: 1.5 $")' )
  WRITE( *, '( 5x, a, / )' ) PROGRAM_HEADER




  !#----------------------------------------------------------------------------#
  !#                       -- READ AN ASCII SRF FILE --                         #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter an ASCII SRF filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) FileNAME
  FileNAME = ADJUSTL( FileNAME )


  ! ---------------
  ! Read the header
  ! ---------------

  Error_Status = Read_SRF_ASCII_Header( TRIM( FileNAME ), &
                                        InFileID, &
                                        n_Channels,    &
                                        Channel_List,  &
                                        Title,         &
                                        History,       &
                                        Sensor_Name,   &
                                        Platform_Name, &
                                        Comment        )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading header from '//TRIM( FileNAME ), &
                          FAILURE )
    STOP
  END IF


  ! ----------------
  ! Write the header
  ! ----------------

  Error_Status = Write_SRF_ASCII_Header( TRIM( FileNAME )//'.output', &
                                         OutFileID, &
                                         n_Channels,    &
                                         Channel_List,  &
                                         Title,         &
                                         History,       &
                                         Sensor_Name,   &
                                         Platform_Name, &
                                         Comment        )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error writing header to '//TRIM( FileNAME )//'.output', &
                          FAILURE )
    STOP
  END IF


  ! ------------------
  ! Loop over channels
  ! ------------------

  DO l = 1, n_Channels

    Error_Status = Read_SRF_ASCII( TRIM( FileNAME ), &
                                   InFileID, &
                                   Channel_List(l), &
                                   SRF )
                                       
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading SRF channel ", i5, " from ", a )' ) &
                      Channel_List(l), TRIM( FileNAME )
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    Error_Status = Write_SRF_ASCII( TRIM( FileNAME )//'.output', &
                                    OutFileID, &
                                    SRF )
                                       
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing SRF channel ", i5, " to ", a )' ) &
                      Channel_List(l), TRIM( FileNAME )//'.output'
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    Error_Status = Destroy_SRF( SRF )
                                       
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error destroying channel ", i5, " SRF structure." )' ) &
                      Channel_List(l)
      CALL display_message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO

  CLOSE( InFileID )
  CLOSE( OutFileID )

END PROGRAM SRF_ASCII_IO_Test


