!
! Binary_File_Utility
!
! Module for utility routines for "Binary" datafiles (unformatted,
! sequential) that conform to the required format.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jun-2000
!                       paul.vandelst@ssec.wisc.edu
!

MODULE Binary_File_Utility

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds,      ONLY: Long, n_Bytes_Long
  USE File_Utility,    ONLY: Get_Lun, File_Exists, File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Endian_Utility,  ONLY: Swap_Endian
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Open_Binary_File


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id$'
  ! Magic number header value for byte-swap checks
  INTEGER(Long), PARAMETER :: MAGIC_NUMBER = 123456789_Long
  ! Message string length
  INTEGER, PARAMETER :: ML = 256
  

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
!       Open_Binary_File
!
! PURPOSE:
!       Function to open unformatted, sequential access "Binary" files
!
! CALLING SEQUENCE:
!       Error_Status = Open_Binary_File( Filename,                &
!                                        FileID,                  &
!                                        For_Output = For_Output, &
!                                        No_Check   = No_Check    )
!
! INPUTS:
!       Filename:         Name of the Binary file to open.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       For_Output:       Set this logical argument to open a new file for
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
!       No_Check:         Set this logical argument to suppress the byte-order
!                         check made on an existing file by NOT reading the file
!                         header magic number.  Default action is to check the
!                         file. This argument is ignored if the FOR_OUTPUT 
!                         optional argument is set.
!                         If == .FALSE., existing file magic number is read and the
!                                        byte order is checked (DEFAULT)
!                            == .TRUE.,  magic number is *NOT* read from file and
!                                        checked for validity.
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
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Open_Binary_File( &
    Filename,   &  ! Input
    FileID,     &  ! Output
    For_Output, &  ! Optional input
    No_Check  ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER,                INTENT(OUT) :: FileID
    LOGICAL,      OPTIONAL, INTENT(IN)  :: For_Output
    LOGICAL,      OPTIONAL, INTENT(IN)  :: No_Check
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Open_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: file_check
    LOGICAL :: file_input
    INTEGER :: io_stat
    INTEGER(Long) :: magic_number_read
    CHARACTER(7) :: file_status
    CHARACTER(5) :: file_action

    ! Set up
    err_stat = SUCCESS
    ! ...Check the For_Output argument
    file_input = .TRUE.
    IF ( PRESENT(For_Output) ) file_input = .NOT. For_Output
    ! ...Check the No_Check argument
    file_check = file_input
    IF ( PRESENT(No_Check) ) file_check = (.NOT. No_Check) .AND. file_input


    ! Branch depending on type of file I/O
    IF ( file_input ) THEN
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


    ! Check the file byte order
    IF ( file_check ) THEN
      err_stat = Check_Binary_File( Filename )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error checking '//TRIM(Filename)//' file byte order'
        CALL CleanUp(); RETURN
      END IF
    END IF


    ! Get a free unit number
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL CleanUp(); RETURN
    END IF


    ! Open the file
    OPEN( FileID, FILE   = Filename, &
                  STATUS = file_status, &
                  ACTION = file_action, &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'UNFORMATTED', &
                  IOSTAT = io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error opening ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL CleanUp(); RETURN
    END IF


    ! Skip past, or write the magic number
    IF ( File_Input ) THEN
      READ( FileID, IOSTAT=io_stat ) magic_number_read
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error reading magic number from ",a,". IOSTAT = ",i0)' ) &
               TRIM(Filename), io_stat
        CALL CleanUp(); RETURN
      END IF
    ELSE
      WRITE( FileID, IOSTAT=io_stat ) MAGIC_NUMBER
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error writing magic number to ",a,". IOSTAT = ",i0)' ) &
               TRIM(Filename), io_stat
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
   
     SUBROUTINE CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( FileID,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing file during error cleanup.'
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE CleanUp

  END FUNCTION Open_Binary_File


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
!       Check_Binary_File
!
! PURPOSE:
!       Function to determine if the unformatted Binary file is in the correct
!       byte order.
!
! CALLING SEQUENCE:
!       Error_Status = Check_Binary_File( Filename )
!
! INPUTS:
!       Filename:         Name of the Binary file to check.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Message_Log:      Character string specifying a filename in which any
!                         Messages will be logged. If not specified, or if an
!                         error occurs opening the log file, the default action
!                         is to output Messages to the screen.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the
!                         error status. The error codes are defined in
!                         the Message_Handler module. Values returned by
!                         this function are:
!                           SUCCESS == file check was successful
!                           FAILURE == - error occurred reading a file record,
!                                      - 8- and/or 32-bit integers not supported.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Check_Binary_File( Filename ) RESULT( err_stat )
    ! Arguments
    CHARACTER(*), INTENT(IN) :: Filename
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*),  PARAMETER :: ROUTINE_NAME = 'Check_Binary_File'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: fid
    INTEGER :: io_stat
    INTEGER(Long) :: magic_number_read
    INTEGER(Long) :: magic_number_swapped

    ! Set up
    err_stat = SUCCESS


    ! Check that 4-byte integers are supported
    IF ( BIT_SIZE( 1_Long ) /= 32 ) THEN
      msg = '32-bit integers not supported. Unable to determine endian-ness'
      CALL CleanUp(); RETURN
    END IF


    ! Get a free unit number
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      msg = 'Error obtaining file unit number for '//TRIM(Filename)
      CALL CleanUp(); RETURN
    END IF


    ! Open the file as direct access
    OPEN( fid, FILE   = Filename, &
               STATUS = 'OLD', &
               ACTION = 'READ', &
               ACCESS = 'DIRECT', &
               FORM   = 'UNFORMATTED', &
               RECL   = n_Bytes_Long, &
               IOSTAT = io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error opening ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL CleanUp(); RETURN
    END IF


    ! Read the magic number
    READ( fid, REC=2, IOSTAT=io_stat ) magic_number_read
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading file magic number. IOSTAT = ",i0)' ) io_stat
      CALL CleanUp(); RETURN
    END IF


    ! Close the file
    CLOSE( fid )


    ! Compare the magic numbers
    IF ( magic_number_read /= MAGIC_NUMBER ) THEN

      ! Byte swap the magic number
      magic_number_swapped = Swap_Endian( magic_number_read )
      IF ( magic_number_swapped /= MAGIC_NUMBER ) THEN
        msg = 'Unrecognised file format. Invalid magic number.'
        CALL CleanUp(); RETURN
      END IF

      ! If we get here then the data does need to be byte-swapped
      msg = 'Data file needs to be byte-swapped.'
      CALL CleanUp(); RETURN

    END IF
    
  CONTAINS
   
     SUBROUTINE CleanUp()
       IF ( File_Open(Filename) ) THEN
         CLOSE( fid,IOSTAT=io_stat )
         IF ( io_stat /= 0 ) &
           msg = TRIM(msg)//'; Error closing file during error cleanup.'
       END IF
       err_stat = FAILURE
       CALL Display_Message( ROUTINE_NAME, msg, err_stat )
     END SUBROUTINE CleanUp

  END FUNCTION Check_Binary_File

END MODULE Binary_File_Utility
