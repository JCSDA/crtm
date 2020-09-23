!
! Test_DumpFile
!
! Program to test the dumpfile utility module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 03-Nov-2008
!                       paul.vandelst@noaa.gov
!

PROGRAM Test_Dumpfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE Unit_Test
  USE DumpFile_Utility
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_Dumpfile'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  CHARACTER(*), PARAMETER :: TEST_FILE = 'DumpFile.bin'
  ! Record size and number
  INTEGER, PARAMETER :: N_PTS     = 100
  INTEGER, PARAMETER :: N_RECORDS = 5
  ! Keyword set value
  INTEGER,      PARAMETER :: SET = 1
  
  
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: File_Id
  INTEGER :: i
  REAL(fp)                 :: Real_Out(N_PTS)
  COMPLEX(fp)              :: Complex_Out(N_PTS)
  REAL(fp)   , ALLOCATABLE :: Real_In(:)
  COMPLEX(fp), ALLOCATABLE :: Complex_In(:)
  TYPE(DumpFile_Hdr_type) :: Hdr_Out, Hdr_In
  TYPE(UTest_type) :: UTest


  ! Output program header
  ! ---------------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the DumpFile_Utility procedures.', &
                       '$Revision$' )


  ! Initialise the tests
  ! --------------------
  CALL Init_AllTests(UTest)


  ! Test WRITE functionality
  ! ------------------------
  WRITE( *,'(5x,"Writing test data file ",a,"...")' ) TEST_FILE
  ! Open a file for output
  File_Id = Open_DumpFile(TEST_FILE, For_Output=SET)
  IF ( File_Id < 0 ) THEN
    Message = 'Error opening file for output'
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
    STOP
  END IF
  ! Write the header
  Hdr_Out%Record_Length = N_PTS
  Hdr_Out%n_Records     = N_RECORDS
  Error_Status = Write_DumpFile(File_Id, Hdr_Out)
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error writing header'
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
    STOP
  END IF
  ! Write some data
  Real_Out    = 1.0_fp
  Complex_Out = (2.0_fp,3.0_fp)
  DO i = 1, N_RECORDS
    WRITE( Message,'("Writing record #",i0)' ) i
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), INFORMATION)
    ! Real data write
    Error_Status = Write_DumpFile(File_Id, Real_Out)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing real data'
      CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
      STOP
    END IF
    ! Complex data write
    Error_Status = Write_DumpFile(File_Id, Complex_Out)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing complex data'
      CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
      STOP
    END IF
  END DO
  CLOSE(File_Id)


  ! Test READ functionality
  ! -----------------------
  WRITE( *,'(/5x,"Reading test data file ",a,"...")' ) TEST_FILE
  ! Open a file for input
  File_Id = Open_DumpFile(TEST_FILE)
  IF ( File_Id < 0 ) THEN
    Message = 'Error opening file for input'
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
    STOP
  END IF
  ! Read the header
  Error_Status = Read_DumpFile(File_Id, Hdr_In)
  IF ( Error_Status /= SUCCESS ) THEN
    Message = 'Error reading header'
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
    STOP
  END IF
  ! Allocate data arrays
  ALLOCATE(Real_In(Hdr_In%Record_Length), Complex_In(Hdr_In%Record_Length), STAT=Error_Status)
  IF ( Error_Status /= 0 ) THEN
    Message = 'Error allocating input arrays'
    CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
    STOP
  END IF
  ! Read some data
  DO i = 1, N_RECORDS
    ! Init this record test
    WRITE( Message,'("Record #",i0," read test")' ) i
    CALL Init_Test(UTest,Message,Caller=PROGRAM_NAME)
    ! Read real data
    Error_Status = Read_DumpFile(File_Id, Real_In)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading real data'
      CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
      STOP
    END IF
    CALL Is_Equal( Real_In, Real_Out, UTest)
    ! Read complex data
    Error_Status = Read_DumpFile(File_Id, Complex_In)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading complex data'
      CALL Display_Message(PROGRAM_NAME, TRIM(Message), FAILURE)
      STOP
    END IF
    CALL Is_Equal( Complex_In, Complex_Out, UTest)
    ! Issue record test report
    CALL Report_Test(UTest)
  END DO
  CLOSE(File_Id)


  ! Report test results
  ! -------------------
  CALL Report_AllTests(UTest)
  
  
  ! Clean up
  ! --------
  DEALLOCATE( Real_In, Complex_In )
  
END PROGRAM Test_DumpFile
