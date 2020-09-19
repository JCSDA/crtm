!
! DumpFile_Utility
!
! Module containing utility routines for writing and reading simple binary files.
! These utilities are intended to be used for intermediate debug output as the
! file format should not be considered portable across compilers/systems.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Oct-2007
!                       paul.vandelst@noaa.gov
!

MODULE DumpFile_Utility

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: fp
  USE File_Utility   , ONLY: Get_Lun
  USE Message_Handler, ONLY: SUCCESS, FAILURE
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Visibility
  ! ----------
  ! Everything is default private
  PRIVATE
  ! Defined type
  PUBLIC :: DumpFile_Hdr_type
  ! Public procedures
  PUBLIC :: Open_DumpFile
  PUBLIC :: Write_DumpFile
  PUBLIC :: Read_DumpFile

  ! ---------
  ! Overloads
  ! ---------
  INTERFACE Write_DumpFile
    MODULE PROCEDURE Write_Hdr
    MODULE PROCEDURE Write_Real
    MODULE PROCEDURE Write_Complex
  END INTERFACE Write_DumpFile
  
  INTERFACE Read_DumpFile
    MODULE PROCEDURE Read_Hdr
    MODULE PROCEDURE Read_Real
    MODULE PROCEDURE Read_Complex
  END INTERFACE Read_DumpFile
  
  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  
    
  ! -------------------------------
  ! Dumpfile header type definition
  ! -------------------------------
  TYPE :: DumpFile_Hdr_type
    INTEGER :: Record_Length = -1
    INTEGER :: n_Records     = -1
  END TYPE DumpFile_Hdr_type

  
CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                           ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Open_DumpFile( Filename     , &  ! Input
                          For_Output   ) &  ! Optional input
                        RESULT( FileID )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER, OPTIONAL, INTENT(IN)  :: For_Output
    ! Function result
    INTEGER :: FileID
    ! Local variables
    INTEGER :: Lun
    LOGICAL :: File_Input
    INTEGER :: IO_Status
    CHARACTER(7) :: File_Status, File_Action
    
    ! Set up
    FileID = -1

    ! Default action is to READ file
    File_Input = .TRUE.
    ! ...unless the For_Output keyword is set
    IF ( PRESENT(For_Output) ) THEN
      IF ( For_Output == SET ) File_Input = .FALSE.
    END IF

    ! Branch depending on type of file I/O
    IF ( File_Input ) THEN
      ! Set OPEN keywords for READING
      File_Status = 'OLD'
      File_Action = 'READ'
    ELSE
      ! Set OPEN keywords for WRITING
      File_Status = 'REPLACE'
      File_Action = 'WRITE'
    END IF

    ! Get a free unit number
    Lun = Get_Lun()
    IF ( Lun < 0 ) RETURN

    ! Open the file
    OPEN( Lun, FILE   = TRIM(Filename), &
               STATUS = TRIM(File_Status), &
               ACTION = TRIM(File_Action), &
               ACCESS = 'SEQUENTIAL', &
               FORM   = 'UNFORMATTED', &
               IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) RETURN
    
    ! Set function return value
    FileID = Lun

  END FUNCTION Open_DumpFile    


  ! --------------
  ! Write routines
  ! --------------
  FUNCTION Write_Hdr( FileID, Hdr ) RESULT( Error_Status )
    INTEGER                , INTENT(IN) :: FileID
    TYPE(DumpFile_Hdr_type), INTENT(IN) :: Hdr
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    WRITE( FileID, IOSTAT=IO_Status ) Hdr
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Write_Hdr

  FUNCTION Write_Real( FileID, DataRec ) RESULT( Error_Status )
    INTEGER , INTENT(IN) :: FileID
    REAL(fp), INTENT(IN) :: DataRec(:)
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    WRITE( FileID, IOSTAT=IO_Status ) DataRec
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Write_Real

  FUNCTION Write_Complex( FileID, DataRec ) RESULT( Error_Status )
    INTEGER    , INTENT(IN) :: FileID
    COMPLEX(fp), INTENT(IN) :: DataRec(:)
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    WRITE( FileID, IOSTAT=IO_Status ) DataRec
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Write_Complex



  ! -------------
  ! Read routines
  ! -------------
  FUNCTION Read_Hdr( FileID, Hdr ) RESULT( Error_Status )
    INTEGER                , INTENT(IN)  :: FileID
    TYPE(DumpFile_Hdr_type), INTENT(OUT) :: Hdr
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    READ( FileID, IOSTAT=IO_Status ) Hdr
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Read_Hdr

  FUNCTION Read_Real( FileID, DataRec ) RESULT( Error_Status )
    INTEGER , INTENT(IN)  :: FileID
    REAL(fp), INTENT(OUT) :: DataRec(:)
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    READ( FileID, IOSTAT=IO_Status ) DataRec
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Read_Real

  FUNCTION Read_Complex( FileID, DataRec ) RESULT( Error_Status )
    INTEGER    , INTENT(IN)  :: FileID
    COMPLEX(fp), INTENT(OUT) :: DataRec(:)
    INTEGER :: Error_Status
    INTEGER :: IO_Status
    Error_Status = SUCCESS
    READ( FileID, IOSTAT=IO_Status ) DataRec
    IF ( IO_Status /= 0 ) Error_Status = FAILURE
  END FUNCTION Read_Complex

END MODULE DumpFile_Utility
