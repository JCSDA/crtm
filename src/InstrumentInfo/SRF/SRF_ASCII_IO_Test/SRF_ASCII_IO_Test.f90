!------------------------------------------------------------------------------
!P+
! NAME:
!       SRF_ASCII_IO_Test
!
! PURPOSE:
!       Program to test the SRF ASCII I/O functions.
!
! CATEGORY:
!       SRF
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:     Module containing definitions for kinds
!                       of variable types.
!
!       Message_Handler:Module to define simple error codes and
!                       handle error conditions
!                       USEs: FILE_UTILITY module
!
!       SRF_Define:     Module defining the SRF data structure and
!                       containing routines to manipulate it.
!                       USEs: TYPE_KINDS module
!                             Message_Handler module
!                             INTEGRATE module
!
!       SRF_ASCII_IO:   Module containing routines to read and
!                       write ASCII format SRF data files.
!                       USEs: TYPE_KINDS module
!                             FILE_UTILITY module
!                             Message_Handler module
!                             STRING_UTILITY module
!                             SRF_DEFINE module
! CONTAINS:
!       None.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       Input:  - ASCII SRF file
!
!       Output: - ASCII SRF file.
!
! SIDE EFFECTS:
!       Any output files that exist are overwritten.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-Aug-2003
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003 Paul van Delst
!
!P-
!------------------------------------------------------------------------------

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


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/08/15 20:51:04 $
!
! $Revision: 1.5 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: SRF_ASCII_IO_Test.f90,v $
! Revision 1.5  2006/08/15 20:51:04  wd20pd
! Additional replacement of Error_Handler with Message_Handler.
!
! Revision 1.4  2006/05/02 16:58:02  dgroff
! *** empty log message ***
!
! Revision 1.3  2004/09/03 20:31:44  paulv
! - Updated program html delimiters.
!
! Revision 1.2  2004/08/23 19:50:38  paulv
! - Updated for Fortran-95 conversion.
!
! Revision 1.1  2003/08/29 18:23:42  paulv
! Initial checkin.
!
!
!
!
