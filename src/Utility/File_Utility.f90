!
!  File_Utility
!
!  Module containing generic file utility routines
!
!
!  Written by:     Paul van Delst, CIMSS/SSEC 12-Jul-2000
!                  paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000, 2006 Paul van Delst
!

MODULE File_Utility


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Get_Lun
  PUBLIC :: File_Exists
  PUBLIC :: File_Open
  PUBLIC :: Count_Lines_in_File


  ! --------------------
  ! Function overloading
  ! --------------------

  INTERFACE File_Exists
    MODULE PROCEDURE File_Unit_Exists
    MODULE PROCEDURE File_Name_Exists
  END INTERFACE File_Exists

  INTERFACE File_Open
    MODULE PROCEDURE File_Open_by_Unit
    MODULE PROCEDURE File_Open_by_Name
  END INTERFACE File_Open


CONTAINS



  FUNCTION Get_Lun() RESULT( Lun )
    INTEGER :: Lun

    ! Initialise logical unit number
    Lun = 9

    ! Start open loop for Lun Search
    Lun_Search: DO
      Lun = Lun + 1
      IF ( .NOT. File_Exists( Lun ) ) THEN
        Lun = -1
        EXIT Lun_Search
      END IF
      IF ( .NOT. File_Open( Lun ) ) EXIT Lun_Search
    END DO Lun_Search

  END FUNCTION Get_Lun




  FUNCTION File_Unit_Exists( FileID ) RESULT ( Existence )
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Existence
    INQUIRE( UNIT = FileID, EXIST = Existence )
  END FUNCTION File_Unit_Exists


  FUNCTION File_Name_Exists( Filename ) RESULT ( Existence )
    CHARACTER( * ), INTENT( IN ) :: Filename
    LOGICAL :: Existence
    INQUIRE( FILE = Filename, EXIST = Existence )
  END FUNCTION File_Name_Exists




  FUNCTION File_Open_by_Unit( FileID ) RESULT ( Is_Open )
    INTEGER, INTENT( IN ) :: FileID
    LOGICAL :: Is_Open
    INQUIRE( UNIT = FileID, OPENED = Is_Open )
  END FUNCTION File_Open_by_Unit


  FUNCTION File_Open_by_Name( Filename ) RESULT ( Is_Open )
    CHARACTER( * ), INTENT( IN ) :: Filename
    LOGICAL :: Is_Open
    INQUIRE( FILE = Filename, OPENED = Is_Open )
  END FUNCTION File_Open_by_Name




  FUNCTION Count_Lines_in_File( Filename, NoComment, NoBlank ) RESULT ( nLines )

    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    CHARACTER(*), OPTIONAL, INTENT(IN) :: NoComment
    INTEGER,      OPTIONAL, INTENT(IN) :: NoBlank

    ! Function result
    INTEGER :: nLines

    ! Local variables
    CHARACTER(1) :: cChar
    LOGICAL :: SkipComment
    LOGICAL :: SkipBlank
    CHARACTER(5000) :: Buffer
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n

    ! Set default return value
    nLines = 0

    ! Check arguments
    IF ( .NOT. File_Exists( Filename ) ) RETURN

    SkipComment = .FALSE.
    IF ( PRESENT(NoComment) ) THEN
      IF ( LEN(NoComment) > 0 ) THEN
        cChar = NoComment(1:1)
        SkipComment = .TRUE.
      END IF
    END IF

    SkipBlank = .FALSE.
    IF ( PRESENT(NoBlank) ) THEN
      IF ( NoBlank /= 0 ) SkipBlank = .TRUE.
    END IF

    ! Open the file for reading only
    FileID = Get_Lun()
    IF ( FileID < 0 ) RETURN
    OPEN( FileID, FILE   = Filename, &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) RETURN

    ! Initialise line counter
    n = 0

    ! Begin open loop
    Count_Loop: DO

      ! Read a line of the file
      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Buffer

      ! Check for an error
      IF ( IO_Status > 0 ) THEN
        CLOSE( FileID )
        RETURN
      END IF

      ! Check for end-of-file
      IF ( IO_Status < 0 ) THEN
        CLOSE( FileID )
        EXIT Count_Loop
      END IF

      ! Check for comment
      IF ( SkipComment ) THEN
        IF ( Buffer(1:1) == cChar ) CYCLE Count_Loop
      END IF

      ! Check for blank line
      IF ( SkipBlank ) THEN
        IF ( LEN_TRIM(Buffer) == 0 ) CYCLE Count_Loop
      END IF

      ! Update line count
      n = n + 1

    END DO Count_Loop

    ! Assign the final count
    nLines = n

  END FUNCTION Count_Lines_in_File

END MODULE File_Utility



