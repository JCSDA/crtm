!
! ListFile_Utility
!
! Module containing routines for reading list files, i.e. ASCII files
! that contain lists of character or integer data, one item per line.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 07-Feb-2003
!                       paul.vandelst@noaa.gov
!

MODULE ListFile_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: ListFile_Associated
  PUBLIC :: ListFile_Destroy
  PUBLIC :: ListFile_Inspect
  PUBLIC :: ListFile_GetSize
  PUBLIC :: ListFile_GetEntry
  PUBLIC :: ListFile_DefineVersion
  PUBLIC :: ListFile_ReadFile


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  ! Public procedures
  INTERFACE ListFile_Associated
    MODULE PROCEDURE Character_ListFile_Associated
    MODULE PROCEDURE Integer_ListFile_Associated
  END INTERFACE ListFile_Associated

  INTERFACE ListFile_Destroy
    MODULE PROCEDURE Character_ListFile_Destroy
    MODULE PROCEDURE Integer_ListFile_Destroy
  END INTERFACE ListFile_Destroy

  INTERFACE ListFile_Inspect
    MODULE PROCEDURE Character_ListFile_Inspect
    MODULE PROCEDURE Integer_ListFile_Inspect
  END INTERFACE ListFile_Inspect

  INTERFACE ListFile_GetSize
    MODULE PROCEDURE Character_ListFile_GetSize
    MODULE PROCEDURE Integer_ListFile_GetSize
  END INTERFACE ListFile_GetSize

  INTERFACE ListFile_GetEntry
    MODULE PROCEDURE Character_ListFile_GetEntry
    MODULE PROCEDURE Integer_ListFile_GetEntry
  END INTERFACE ListFile_GetEntry

  INTERFACE ListFile_ReadFile
    MODULE PROCEDURE Character_ListFile_ReadFile
    MODULE PROCEDURE Integer_ListFile_ReadFile
  END INTERFACE ListFile_ReadFile

  ! Private procedures
  INTERFACE ListFile_Create
    MODULE PROCEDURE Character_ListFile_Create
    MODULE PROCEDURE Integer_ListFile_Create
  END INTERFACE ListFile_Create


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: MODULE_VERSION_ID = &
  ! String lengths
  INTEGER, PARAMETER :: STRLEN = 5000
  INTEGER, PARAMETER :: ML = 256
  ! Init values for entries
  CHARACTER(*), PARAMETER :: INIT_CHARACTER = ''
  INTEGER     , PARAMETER :: INIT_INTEGER   = HUGE(0)


  ! -------------------------
  ! Derived type declarations
  ! -------------------------
  TYPE, PUBLIC :: Character_ListFile_type
    PRIVATE
    LOGICAL :: Is_Allocated = .FALSE.
    INTEGER :: String_Length = STRLEN
    INTEGER :: n_Entries = 0
    CHARACTER(STRLEN), ALLOCATABLE :: Entry(:)
  END TYPE Character_ListFile_type

  TYPE, PUBLIC :: Integer_ListFile_type
    PRIVATE
    LOGICAL :: Is_Allocated = .FALSE.
    INTEGER :: n_Entries = 0
    INTEGER, ALLOCATABLE :: Entry(:)
  END TYPE Integer_ListFile_type


CONTAINS





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of a ListFile object.
!
! CALLING SEQUENCE:
!       Status = ListFile_Associated( List )
!
! OBJECTS:
!       List:    List structure which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       Character_ListFile_type 
!                              OR                     
!                            Integer_ListFile_type   
!                DIMENSION:  Scalar or any rank
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of theList members.
!                  .TRUE.  - if the array components are allocated.
!                  .FALSE. - if the array components are not allocated.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Same as input List argument
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Character_ListFile_Associated( self ) RESULT( Status )
    TYPE(Character_ListFile_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION Character_ListFile_Associated

  ELEMENTAL FUNCTION Integer_ListFile_Associated( self ) RESULT( Status )
    TYPE(Integer_ListFile_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION Integer_ListFile_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize ListFile objects.
!
! CALLING SEQUENCE:
!       CALL ListFile_Destroy( List )
!
! OBJECTS:
!       List:  Re-initialized ListFile object.
!              UNITS:      N/A
!              TYPE:       Character_ListFile_type 
!                            OR                     
!                          Integer_ListFile_type   
!              DIMENSION:  Scalar or any rank
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Character_ListFile_Destroy( self )
    TYPE(Character_ListFile_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE Character_ListFile_Destroy

  ELEMENTAL SUBROUTINE Integer_ListFile_Destroy( self )
    TYPE(Integer_ListFile_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE Integer_ListFile_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of a ListFile object to stdout.
!
! CALLING SEQUENCE:
!       CALL ListFile_Inspect( List )
!
! OBJECTS:
!       List:       ListFile object to display.
!                   UNITS:      N/A
!                   TYPE:       Character_ListFile_type 
!                                 OR                     
!                               Integer_ListFile_type   
!                   DIMENSION:  Scalar
!                   ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Character_ListFile_Inspect( self )
    TYPE(Character_ListFile_type), INTENT(IN) :: self
    INTEGER :: n
    WRITE(*, '(1x,"Character_ListFile OBJECT")')
    WRITE(*, '(3x,"n_Entries :",1x,i0)') self%n_Entries
    IF ( .NOT. ListFile_Associated(self) ) RETURN
    DO n = 1, self%n_Entries
      WRITE(*,'(1x,"Entry ",i7,": ",a)') n, TRIM(self%Entry(n))
    END DO
  END SUBROUTINE Character_ListFile_Inspect

  SUBROUTINE Integer_ListFile_Inspect( self )
    TYPE(Integer_ListFile_type), INTENT(IN) :: self
    INTEGER :: n
    WRITE(*, '(1x,"Integer_ListFile OBJECT")')
    WRITE(*, '(3x,"n_Entries :",1x,i0)') self%n_Entries
    IF ( .NOT. ListFile_Associated(self) ) RETURN
    DO n = 1, self%n_Entries
      WRITE(*,'(1x,"Entry ",i7,": ",i0)') n, self%Entry(n)
    END DO
  END SUBROUTINE Integer_ListFile_Inspect



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_GetSize
!
! PURPOSE:
!       Elemental function to return the number of entries in a ListFile object.
!
! CALLING SEQUENCE:
!       n_Entries = ListFile_GetSize( List )
!
! OBJECTS:
!       List:           ListFile object to inquire.
!                       UNITS:      N/A
!                       TYPE:       Character_ListFile_type 
!                                     OR                     
!                                   Integer_ListFile_type   
!                       DIMENSION:  Scalar or any rank.
!                       ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Entries:      The number of entries in the list file structure.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with input argument.
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Character_ListFile_GetSize( self ) RESULT( n_Entries )
    TYPE(Character_ListFile_type), INTENT(IN) :: self
    INTEGER :: n_Entries
    n_Entries = self%n_Entries
  END FUNCTION Character_ListFile_GetSize

  ELEMENTAL FUNCTION Integer_ListFile_GetSize( self ) RESULT( n_Entries )
    TYPE(Integer_ListFile_type), INTENT(IN) :: self
    INTEGER :: n_Entries
    n_Entries = self%n_Entries
  END FUNCTION Integer_ListFile_GetSize



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_GetEntry
!
! PURPOSE:
!       Function to return entries from a ListFile object.
!
! CALLING SEQUENCE:
!       Entry = ListFile_GetEntry( List, n )
!
! OBJECTS:
!       List:   ListFile object to inquire.
!               UNITS:      N/A
!               TYPE:       Character_ListFile_type 
!                             OR                     
!                           Integer_ListFile_type   
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       n:      The ListFile object entry to retrieve
!               UNITS:      N/A
!               TYPE:       INTEGER
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Entry:  The retrieved entry from the ListFile object.
!               UNITS:      N/A
!               TYPE:       Depends on List input argument.
!                             If:   List  is TYPE(Character_ListFile_type)
!                             Then: Entry is CHARACTER(*)
!                               OR                     
!                             If:   List  is TYPE(Integer_ListFile_type)
!                             Then: Entry is INTEGER
!               DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Character_ListFile_GetEntry( List, n ) RESULT( Entry )
    ! Arguments
    TYPE(Character_ListFile_type), INTENT(IN) :: List
    INTEGER,                       INTENT(IN) :: n
    ! Function result
    CHARACTER(STRLEN) :: Entry
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_GetEntry(Character)'
    ! Function variables
    CHARACTER(ML) :: msg
    
    ! Setup
    Entry = INIT_CHARACTER

    ! Is the requested entry valid?
    IF ( n < 1 .OR. n > ListFile_GetSize( List ) ) THEN
      WRITE(msg,'("Invalid entry number argument, ",i0,", specified.")') n
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF

    ! Assign the requested entry
    Entry = List%Entry( n )

  END FUNCTION Character_ListFile_GetEntry

  FUNCTION Integer_ListFile_GetEntry( List, n ) RESULT( Entry )
    ! Arguments
    TYPE(Integer_ListFile_type), INTENT(IN) :: List
    INTEGER,                     INTENT(IN) :: n
    ! Function result
    INTEGER :: Entry
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_GetEntry(Integer)'
    ! Function variables
    CHARACTER(ML) :: msg
    
    ! Setup
    Entry = INIT_INTEGER

    ! Is the requested entry valid?
    IF ( n < 1 .OR. n > ListFile_GetSize( List ) ) THEN
      WRITE(msg,'("Invalid entry number argument, ",i0,", specified.")') n
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF

    ! Assign the requested entry
    Entry = List%Entry( n )

  END FUNCTION Integer_ListFile_GetEntry



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL ListFile_DefineVersion( Id )
!
! OUTPUTS:
!       Id:   Character string containing the version Id information
!             for the module.
!             UNITS:      N/A
!             TYPE:       CHARACTER(*)
!             DIMENSION:  Scalar
!             ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE ListFile_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE ListFile_DefineVersion



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       ListFile_ReadFile
!
! PURPOSE:
!       Function to read a list file into a ListFile object.
!
! CALLING SEQUENCE:
!       Error_Status = ListFile_ReadFile( List, Filename )
!
! OBJECTS:
!       List:            List object to populate with data from file.
!                        UNITS:      N/A
!                        TYPE:       Character_ListFile_type 
!                                      OR                     
!                                    Integer_ListFile_type   
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Filename:        The name of the List File
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the Message_Handler module. Values returned by
!                        this function are:
!                          SUCCESS == the read was successful.
!                          FAILURE == an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! COMMENTS:
!       - For character ist files, the maximum line length that can be read
!         is 5000 characters. If there is any data beyond this limit, the
!         line is considered blank.
!       - Any line in list file with a "!" character in the first column
!         is treated as a comment line and is not read. Similarly for
!         blank lines.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Character_ListFile_ReadFile( &
    List,      &  ! Output
    Filename ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(Character_ListFile_type), INTENT(OUT) :: List
    CHARACTER(*),                  INTENT(IN)  :: Filename
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_ReadFile(Character)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(STRLEN) :: line_buffer
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_entries
    INTEGER :: line_count
    INTEGER :: entry_count


    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Cleanup(); RETURN
    END IF


    ! Count the number of list file entries
    n_entries = ListFile_CountEntries( Filename )
    IF ( n_entries < 0 ) THEN
      msg = 'Error counting entries in the list file '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Create the ListFile object
    CALL ListFile_Create( List, n_entries )
    IF ( .NOT. ListFile_Associated( List ) ) THEN
      msg = 'Error allocating list structure'
      CALL Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = ListFile_OpenFile( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Initialise counters
    line_count  = 0
    entry_count = 0


    ! Read the entries
    Read_Entries_Loop: DO

      ! ...Increment line count
      line_count = line_count + 1

      ! ...Read a line of the file
      READ( fid,FMT='(a)',IOSTAT=io_stat,IOMSG=io_msg ) line_buffer

      ! ...Check for error
      IF ( io_stat > 0 ) THEN
        WRITE(msg,'("Error reading list file at line #",i0," - ",a)' ) line_count, TRIM(io_msg)
        CALL Cleanup(); RETURN
      END IF

      ! ...Check for end-of-file
      IF ( io_stat < 0 ) EXIT Read_Entries_Loop

      ! ...Update entry counter if this is NOT a comment or blank line
      IF ( line_buffer(1:1) /= '!' .AND. LEN_TRIM(line_buffer) /= 0 ) THEN
        entry_count = entry_count + 1
        
        ! ...Too many list entries?
        IF ( entry_count > n_entries ) THEN
          WRITE(msg,'("Number of list entries, ",i0,", is greater than the size ",&
                     &"of the list structure, ",i0)' ) entry_count, n_entries
          CALL Cleanup(); RETURN
        END IF

        ! ...Assign the entry
        List%Entry(entry_count) = line_buffer
        
      END IF

    END DO Read_Entries_Loop


    ! Too few list entries?
    IF ( entry_count /= n_entries ) THEN
      WRITE(msg,'("Number of list entries, ",i0,", is less than the size ",&
                 &"of the list structure, ",i0)' ) entry_count, n_entries
      CALL Cleanup(); RETURN
    END IF
    
    
    ! Close the file
    CLOSE(fid,IOSTAT=io_stat,IOMSG=io_msg)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL ListFile_Destroy( List )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION Character_ListFile_ReadFile


  FUNCTION Integer_ListFile_ReadFile( &
    List,      &  ! Output
    Filename ) &  ! Input
  RESULT( err_stat )
    ! Arguments
    TYPE(Integer_ListFile_type), INTENT(OUT) :: List
    CHARACTER(*),                INTENT(IN)  :: Filename
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_ReadFile(Integer)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(STRLEN) :: line_buffer
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_entries
    INTEGER :: line_count
    INTEGER :: entry_count


    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( Filename ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Cleanup(); RETURN
    END IF


    ! Count the number of list file entries
    n_entries = ListFile_CountEntries( Filename )
    IF ( n_entries < 0 ) THEN
      msg = 'Error counting entries in the list file '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Create the ListFile object
    CALL ListFile_Create( List, n_entries )
    IF ( .NOT. ListFile_Associated( List ) ) THEN
      msg = 'Error allocating list structure'
      CALL Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = ListFile_OpenFile( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Initialise counters
    line_count  = 0
    entry_count = 0


    ! Read the entries
    Read_Entries_Loop: DO

      ! ...Increment line count
      line_count = line_count + 1

      ! ...Read a line of the file
      READ( fid,FMT='(a)',IOSTAT=io_stat,IOMSG=io_msg ) line_buffer

      ! ...Check for error
      IF ( io_stat > 0 ) THEN
        WRITE(msg,'("Error reading list file at line #",i0," - ",a)' ) line_count, TRIM(io_msg)
        CALL Cleanup(); RETURN
      END IF

      ! ...Check for end-of-file
      IF ( io_stat < 0 ) EXIT Read_Entries_Loop

      ! ...Update entry counter if this is NOT a comment or blank line
      IF ( line_buffer(1:1) /= '!' .AND. LEN_TRIM(line_buffer) /= 0 ) THEN
        entry_count = entry_count + 1
        
        ! ...Too many list entries?
        IF ( entry_count > n_entries ) THEN
          WRITE(msg,'("Number of list entries, ",i0,", is greater than the size ",&
                     &"of the list structure, ",i0)' ) entry_count, n_entries
          CALL Cleanup(); RETURN
        END IF

        ! ...Assign the entry
        line_buffer = ADJUSTL(line_buffer)
        READ(line_buffer,'(i10)') List%Entry(entry_count)
        
      END IF

    END DO Read_Entries_Loop


    ! Too few list entries?
    IF ( entry_count /= n_entries ) THEN
      WRITE(msg,'("Number of list entries, ",i0,", is less than the size ",&
                 &"of the list structure, ",i0)' ) entry_count, n_entries
      CALL Cleanup(); RETURN
    END IF
    
    
    ! Close the file
    CLOSE(fid,IOSTAT=io_stat,IOMSG=io_msg)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL ListFile_Destroy( List )
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION Integer_ListFile_ReadFile



!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       ListFile_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of a ListFile object.
!
! CALLING SEQUENCE:
!       CALL ListFile_Create( List, n_Entries )
!
! OBJECTS:
!       List:       ListFile object.
!                   UNITS:      N/A
!                   TYPE:       Character_ListFile_type 
!                                 OR                     
!                               Integer_ListFile_type   
!                   DIMENSION:  Scalar or any rank
!                   ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Entries:  Number of entries in the List.
!                   Must be > 0.
!                   UNITS:      N/A
!                   TYPE:       INTEGER
!                   DIMENSION:  Same as List object
!                   ATTRIBUTES: INTENT(IN)
!
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Character_ListFile_Create( self, n_Entries )
    ! Arguments
    TYPE(Character_ListFile_type), INTENT(OUT) :: self
    INTEGER,                       INTENT(IN)  :: n_Entries
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Entries < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%Entry( n_Entries ), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    self%n_Entries = n_Entries
    self%Entry     = INIT_CHARACTER

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE Character_ListFile_Create

  ELEMENTAL SUBROUTINE Integer_ListFile_Create( self, n_Entries )
    ! Arguments
    TYPE(Integer_ListFile_type), INTENT(OUT) :: self
    INTEGER,                     INTENT(IN)  :: n_Entries
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_Entries < 1 ) RETURN

    ! Perform the allocation
    ALLOCATE( self%Entry( n_Entries ), STAT=alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    self%n_Entries = n_Entries
    self%Entry     = INIT_INTEGER

    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE Integer_ListFile_Create


!------------------------------------------------------------------------------
!
! NAME:
!       ListFile_OpenFile
!
! PURPOSE:
!       Function to open a List File.
!
! CALLING SEQUENCE:
!       Error_Status = ListFile_OpenFile( Filename,       &
!                                         FileID,         &
!                                         Output = Output )
!
! INPUTS:
!       Filename:         Name of the List File to open.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Output:           Set this logical argument to open a new file for
!                         writing. Default action is to open an existing file
!                         for read access. Note, if the file already exists and
!                         it is opened with this keyword set, the file is
!                         overwritten.
!                         If == .FALSE., existing file is opened for READ access (DEFAULT)
!                                        ACTION='READ', STATUS='OLD'
!                            == .TRUE. , new file is opened for WRITE access.
!                                        ACTION='WRITE', STATUS='REPLACE'
!                         UNITS:      N/A
!                         TYPE:       LOGICAL
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OUTPUTS:
!       FileID:           File unit number.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the Message_Handler module. Values returned by
!                         this function are:
!                           SUCCESS == file open was successful
!                           FAILURE == an unrecoverable error occurred
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ListFile_OpenFile( &
    Filename, &  ! Input
    FileID,   &  ! Output
    Output  ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Filename
    INTEGER,           INTENT(OUT) :: FileID
    LOGICAL, OPTIONAL, INTENT(IN)  :: Output
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_OpenFile'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: input
    INTEGER :: io_stat
    CHARACTER(7) :: file_status
    CHARACTER(5) :: file_action

    ! Set up
    err_stat = SUCCESS
    ! ...Check the Output argument
    input = .TRUE.
    IF ( PRESENT(Output) ) input = .NOT. Output


    ! Branch depending on type of file I/O
    IF ( input ) THEN
      ! File is to be READ. If the file
      ! does not exist, return an error
      IF ( .NOT. File_Exists( Filename ) ) THEN
        err_stat = FAILURE
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL CleanUp(); RETURN
      END IF
      ! Set OPEN keywords for READING
      file_status = 'OLD'
      file_action = 'READ'
    ELSE
      ! File is to be WRITTEN.
      ! Set OPEN keywords for WRITING
      file_status = 'REPLACE'
      file_action = 'WRITE'
    END IF


    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL CleanUp(); RETURN
    END IF


    ! Open the file
    OPEN( FileID, FILE   = Filename    , &
                  STATUS = file_status , &
                  ACTION = file_action , &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED' , &
                  IOSTAT = io_stat     , &
                  IOMSG  = io_msg        )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error opening '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL CleanUp(); RETURN
    END IF

  CONTAINS
   
     SUBROUTINE CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( FileID, IOSTAT=io_stat, IOMSG=io_msg )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE CleanUp

  END FUNCTION ListFile_OpenFile


!------------------------------------------------------------------------------
!
! NAME:
!       ListFile_CountEntries
!
! PURPOSE:
!       Function to count the number of entries in a list file
!
! CALLING SEQUENCE:
!       n_Entries = ListFile_CountEntries( Filename )
!
! INPUT ARGUMENTS:
!       Filename:        The name of the list file
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       n_Entries:       The number of entries in the list file.
!                        If an error occurs, -1 is returned.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ListFile_CountEntries( Filename ) RESULT( n_Entries )
    ! Arguments
    CHARACTER(*), INTENT(IN) :: Filename
    ! Function result
    INTEGER :: n_Entries
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ListFile_CountEntries'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    CHARACTER(STRLEN) :: line_buffer
    INTEGER :: err_stat
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: line_count
    INTEGER :: entry_count

    ! Setup
    n_Entries = -1
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Cleanup(); RETURN
    END IF


    ! Open the list file
    err_stat = ListFile_OpenFile( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Initialise counters
    line_count  = 0
    entry_count = 0


    ! Count the entries
    Count_Entries_Loop: DO

      ! ...Increment line count
      line_count = line_count + 1

      ! ...Read a line of the file
      READ( fid,FMT='(a)',IOSTAT=io_stat,IOMSG=io_msg ) line_buffer

      ! ...Check for error
      IF ( io_stat > 0 ) THEN
        WRITE(msg,'("Error reading list file at line #",i0," - ",a)' ) line_count, TRIM(io_msg)
        CALL Cleanup(); RETURN
      END IF

      ! ...Check for end-of-file
      IF ( io_stat < 0 ) THEN
        CLOSE( fid )
        EXIT Count_Entries_Loop
      END IF

      ! ...Update entry counter if this is NOT a comment or blank line
      IF ( line_buffer(1:1) /= '!' .AND. LEN_TRIM(line_buffer) /= 0 ) THEN
        entry_count = entry_count + 1
      END IF

    END DO Count_Entries_Loop


    ! Assign the final entry count
    n_Entries = entry_count


    ! Close the file
    CLOSE(fid,IOSTAT=io_stat,IOMSG=io_msg)
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Cleanup(); RETURN
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      n_Entries = -1
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION ListFile_CountEntries

END MODULE ListFile_Utility
