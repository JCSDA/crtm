
!  Test_SatInfo
!
!  Program to test the routines in the SatInfo_IO module
!
!  Written by:     Paul van Delst, CIMSS/SSEC 04-Apr-2002
!                  paul.vandelst@ssec.wisc.edu
!

PROGRAM Test_SatInfo
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE File_Utility
  USE Message_Handler
  USE SatInfo_IO
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME   = 'Test_SatInfo'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id:$'
  CHARACTER(*), PARAMETER :: INPUT_FILENAME   = 'global_satinfo.gsi'
  CHARACTER(*), PARAMETER :: OUTPUT_FILENAME1 = 'global_satinfo.gsi.Test_Write1'
  CHARACTER(*), PARAMETER :: OUTPUT_FILENAME2 = 'global_satinfo.gsi.Test_Write2'
  INTEGER,      PARAMETER :: N_ENTRIES       = 592


  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status
  INTEGER :: FileID
  TYPE(SatInfo_type), DIMENSION(N_ENTRIES) :: SatInfo


  ! --------------
  ! Program header
  ! --------------

  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the routines in the SatInfo_IO module.', &
                        '$Revision:$' )


  ! ----------------------
  ! Test the read function
  ! ----------------------
  WRITE( *, '( /5x, "Reading SatInfo file ", a, "..." )' ) INPUT_FILENAME
  Error_Status = Read_SatInfo( INPUT_FILENAME, SatInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading SatInfo file '//INPUT_FILENAME, &
                          Error_status )
    STOP
  END IF


  ! ------------------------
  ! Test the write functions
  ! ------------------------
  ! Straight output of structure array only
  WRITE( *, '( /5x, "Writing GSI SatInfo file ", a, "..." )' ) OUTPUT_FILENAME1
  Error_Status = Write_SatInfo( OUTPUT_FILENAME1, SatInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing SatInfo file '//OUTPUT_FILENAME1, &
                          Error_status )
    STOP
  END IF

  ! Multiple outputs to the a file opened
  ! outside of the write function.
  WRITE( *, '( /5x, "Writing GSI SatInfo file ", a, "..." )' ) OUTPUT_FILENAME2
  FileID = Get_Lun()
  OPEN( FileID, FILE=OUTPUT_FILENAME2, &
                STATUS = 'REPLACE', &
                ACCESS = 'SEQUENTIAL', &
                FORM   = 'FORMATTED', &
                ACTION = 'WRITE' )

  WRITE( FileID, '( ///,"This is a test of multi outputs....",///)' )
  Error_Status = Write_SatInfo( FileID, SatInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error (1) writing SatInfo array to file '//OUTPUT_FILENAME2, &
                          Error_status )
    STOP
  END IF

  WRITE( FileID, '( ///,"Here is some more output to file....",///)' )
  Error_Status = Write_SatInfo( FileID, SatInfo )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error (2) writing SatInfo array to file '//OUTPUT_FILENAME2, &
                          Error_status )
    STOP
  END IF

  CLOSE( FileID )

END PROGRAM Test_SatInfo
