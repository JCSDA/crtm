!
! Diff_TauProfile
!
! Program to compare values in two TauProfile files and report the
! location of the differences.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Feb-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Diff_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, &
                                  Display_Message, Program_Message
  USE TauProfile_Define   , ONLY: TauProfile_type, &
                                  Equal_TauProfile
  USE TauProfile_netCDF_IO, ONLY: Read_TauProfile_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Diff_TauProfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &


  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: Message
  CHARACTER( 256 ) :: InFile1
  CHARACTER( 256 ) :: InFile2
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  TYPE(TauProfile_type) :: TauProfile1, TauProfile2


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                       'Program to compare the data in two TauProfile files and '//&
                       'report the differences.', &
                       '$Revision$' )


  ! Prompt for filenames to check
  ! -----------------------------
  WRITE( *,FMT    ='(/5x,"Enter TauProfile filename #1: ")', &
           ADVANCE='NO' )
  READ( *,FMT='(a)' ) InFile1
  InFile1 = ADJUSTL(InFile1)
  WRITE( *,FMT    ='(/5x,"Enter TauProfile filename #2: ")', &
           ADVANCE='NO' )
  READ( *,FMT='(a)' ) InFile2
  InFile2 = ADJUSTL(InFile2)

  ! Check that both files exist
  IF ( .NOT. File_Exists( InFile1 ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//TRIM(InFile1)//' not found.', &
                          Error_Status )
    STOP
  END IF
  IF ( .NOT. File_Exists( InFile2 ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//TRIM(InFile2)//' not found.', &
                          Error_Status )
    STOP
  END IF


  ! Read the data
  ! -------------
  ! File #1
  Error_Status = Read_TauProfile_netCDF( InFile1    , &
                                         TauProfile1  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading data from TauProfile file 1, '//TRIM(Infile1), &
                          Error_Status )
    STOP
  END IF

  ! File #2
  Error_Status = Read_TauProfile_netCDF( InFile2    , &
                                         TauProfile2  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading data from TauProfile file 2, '//TRIM(Infile2), &
                          Error_Status )
    STOP
  END IF


  ! Compare the data
  ! ----------------
  Error_Status = Equal_TauProfile( TauProfile1, TauProfile2 )
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'TauProfile data are different'
  ELSE
    Message = 'TauProfile data are equal'
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(Message), &
                        Error_Status )

END PROGRAM Diff_TauProfile
