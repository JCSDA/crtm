!
! CRTM_Cloud_Binary_IO
!
! Module containing routines to inquire, read, and write Binary format
! CRTM_Cloud files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 16-Mar-2005
!                       paul.vandelst@ssec.wisc.edu
!

MODULE CRTM_Cloud_Binary_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE File_Utility,        ONLY: File_Open, File_Exists
  USE Message_Handler,     ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Parameters    , ONLY: SET
  USE CRTM_Cloud_Define  , ONLY: CRTM_Cloud_type, &
                                 CRTM_Associated_Cloud, &
                                 CRTM_Destroy_Cloud, &
                                 CRTM_Allocate_Cloud
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Inquire_Cloud_Binary
  PUBLIC :: CRTM_Read_Cloud_Binary
  PUBLIC :: CRTM_Write_Cloud_Binary


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256


CONTAINS

!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Inquire_Cloud_Binary
!
! PURPOSE:
!       Function to inquire Binary format CRTM Cloud structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Inquire_Cloud_Binary( Filename              , &  ! Input
!                                                 n_Clouds   =n_Clouds  , &  ! Optional output
!                                                 RCS_Id     =RCS_Id    , &  ! Revision control
!                                                 Message_Log=Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Clouds:       The number of Cloud profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file inquire was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Inquire_Cloud_Binary( Filename   , &  ! Input
                                      n_Clouds   , &  ! Optional output
                                      RCS_Id     , &  ! Revision control
                                      Message_Log) &  ! Error messaging
                                    RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Clouds
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Inquire_Cloud_Binary'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: na

    ! Setup
    ! -----
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the cloud data file
    ! --------------------------
    err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of clouds dimension
    ! -------------------------------------
    READ( fid,IOSTAT=io_stat ) na
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading n_Clouds dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    
    
    ! Close the file
    ! --------------
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF
    
    
    ! Set the return arguments
    ! ------------------------
    IF ( PRESENT(n_Clouds) ) n_Clouds = na

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= SUCCESS ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Inquire_Cloud_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Read_Cloud_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Cloud structure files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Cloud_Binary( Filename                   , &  ! Input
!                                              Cloud                      , &  ! Output
!                                              Quiet        =Quiet        , &  ! Optional input
!                                              No_File_Close=No_File_Close, &  ! Optional input
!                                              No_Allocate  =No_Allocate  , &  ! Optional input
!                                              n_Clouds     =n_Clouds     , &  ! Optional output
!                                              RCS_Id       =RCS_Id       , &  ! Revision control
!                                              Message_Log  =Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of a
!                       Cloud format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Cloud:          Structure containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:          Set this argument to suppress INFORMATION messages
!                       being printed to standard output (or the message
!                       log file if the Message_Log optional argument is
!                       used.)
!                       If == 0, INFORMATION messages are OUTPUT [DEFAULT].
!                          == 1, INFORMATION messages are SUPPRESSED.
!                       If not specified, information messages are output.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       No_Allocate:    Set this argument to NOT allocate the output Cloud
!                       structure in this routine based on the data dimensions
!                       read from the input data file. This assumes that the
!                       structure has already been allocated prior to calling 
!                       this function.
!                       If == 0, the output Cloud structure is allocated [DEFAULT]
!                          == 1, the output Cloud structure is NOT allocated
!                       If not specified, the default action is to allocate 
!                       the output Cloud structure to the dimensions specified
!                       in the input data file.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Clouds:       The actual number of cloud profiles read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If an error occurs:
!       - the input file is closed and,
!       - the output Cloud structure is deallocated.
!
! COMMENTS:
!       Note the INTENT on the output Cloud argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Read_Cloud_Binary( Filename     , &  ! Input
                                   Cloud        , &  ! Output
                                   Quiet        , &  ! Optional input
                                   No_File_Close, &  ! Optional input
                                   No_Allocate  , &  ! Optional input
                                   n_Clouds     , &  ! Optional output
                                   RCS_Id       , &  ! Revision control
                                   Message_Log  , &  ! Error messaging
                                   Debug        ) &  ! Debug output control
                                 RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)     :: Filename
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: Cloud(:)
    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_File_Close
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Allocate
    INTEGER,      OPTIONAL, INTENT(OUT)    :: n_Clouds
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    INTEGER,      OPTIONAL, INTENT(IN)     :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Cloud_Binary'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_File_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, nc

    ! Setup
    ! -----
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
    END IF
    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT(No_File_Close) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF

    
    ! Check if the file is open
    ! -------------------------
    IF ( File_Open( FileName ) ) THEN
      ! Get the file id
      ! ---------------
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename,NUMBER=fid )
      ! Ensure it's valid
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its fid'
        CALL Read_Cleanup(); RETURN
      END IF
    ELSE
      ! Open the file
      ! -------------
      ! Check that the file exists
      IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_Cleanup(); RETURN
      END IF 
      ! Open the file
      err_stat = Open_Binary_File( Filename, fid, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Read the number of clouds dimension
    ! -------------------------------------
    READ( fid,IOSTAT=io_stat ) nc
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading n_Clouds data dimension from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! Check if n_Clouds > size of output array
    IF ( nc > SIZE(Cloud) ) THEN
      WRITE( msg,'("Number of clouds, ",i0," > size of the output ",&
                  &"Cloud structure array, ",i0,".")' ) &
                  nc, SIZE(Cloud)
      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Read the cloud data
    ! ---------------------
    Cloud_Loop: DO m = 1, nc
      err_stat = Read_Record( fid, Cloud(m), &
                              No_Allocate=No_Allocate, &
                              Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Cloud element #",i0," from ",a)' ) &
                   m, TRIM(Filename)
        CALL Read_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO Cloud_Loop


    ! Close the file
    ! --------------
    IF ( Yes_File_Close ) THEN
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Set the return values
    ! ---------------------
    IF ( PRESENT(n_Clouds) ) n_Clouds = nc

 
    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of clouds read from ",a,": ",i0)' ) TRIM(Filename), nc
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure
      err_stat = CRTM_Destroy_Cloud( Cloud, Message_Log=Message_Log)
      IF ( err_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error destroying Atmosphere structure during error cleanup.'
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Read_Cloud_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Write_Cloud_Binary
!
! PURPOSE:
!       Function to write Binary format Cloud files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Write_Cloud_Binary( Filename                   , &  ! Input
!                                               Cloud                      , &  ! Input
!                                               Quiet        =Quiet        , &  ! Optional input
!                                               No_File_Close=No_File_Close, &  ! Optional input
!                                               RCS_Id       =RCS_Id       , &  ! Revision control
!                                               Message_Log  =Message_Log    )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:       Character string specifying the name of an output
!                       Cloud format data file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Cloud:          Structure containing the Cloud data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Cloud_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:          Set this argument to suppress INFORMATION messages
!                       being printed to standard output (or the message
!                       log file if the Message_Log optional argument is
!                       used.)
!                       If == 0, INFORMATION messages are OUTPUT [DEFAULT].
!                          == 1, INFORMATION messages are SUPPRESSED.
!                       If not specified, information messages are output.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_File_Close:  Set this argument to NOT close the file upon exit.
!                       If == 0, the input file is closed upon exit [DEFAULT]
!                          == 1, the input file is NOT closed upon exit. 
!                       If not specified, the default action is to close the
!                       input file upon exit.
!                       the 
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(IN)
!
!       Message_Log:    Character string specifying a filename in which any
!                       messages will be logged. If not specified, or if an
!                       error occurs opening the log file, the default action
!                       is to output messages to standard output.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:         Character string containing the Revision Control
!                       System Id field for the module.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the ERROR_HANDLER module.
!                       If == SUCCESS the Binary file write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Write_Cloud_Binary( Filename     , &  ! Input
                                    Cloud        , &  ! Input
                                    Quiet        , &  ! Optional input
                                    No_File_Close, &  ! Optional input
                                    RCS_Id       , &  ! Revision control
                                    Message_Log  , &  ! Error messaging
                                    Debug        ) &  ! Debug output control
                                  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: Cloud(:)
    INTEGER,      OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,      OPTIONAL, INTENT(IN)  :: No_File_Close
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    INTEGER,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Cloud_Binary'
    CHARACTER(*), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    LOGICAL :: Yes_File_Close
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, nc
 
    ! Setup
    ! -----
    err_stat = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
    ! Default action is to output info messages...
    Noisy = .TRUE.
    ! ...unless the Quiet optional argument is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF
    ! The Quiet optional argument is overridden
    ! if the Debug optional argument is set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug == SET ) Noisy = .TRUE.
    END IF
    ! Default action is to close the file on exit....
    Yes_File_Close = .TRUE.
    ! ...unless the No_File_Close optional argument is set.
    IF ( PRESENT(No_File_Close) ) THEN
      IF ( No_File_Close == SET ) Yes_File_Close = .FALSE.
    END IF
    ! Check the Cloud structure dimensions
    IF ( ANY(Cloud%n_Layers < 1) ) THEN 
      msg = 'Dimensions of some Cloud structures is < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open
    ! -------------------------
    IF ( File_Open( FileName ) ) THEN
      ! Get the file id
      ! ---------------
      ! Inquire for the logical unit number
      INQUIRE( FILE=Filename,NUMBER=fid )
      ! Ensure it's valid
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its fid'
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! Open the file
      ! -------------
      err_stat = Open_Binary_File( Filename, fid, For_Output=SET, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the number of clouds dimension
    ! --------------------------------------
    nc = SIZE(Cloud)    
    WRITE( fid,IOSTAT=io_stat) nc
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing n_Clouds data dimension to ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Write the cloud data
    ! ---------------------- 
    Cloud_Loop: DO m = 1, nc
      err_stat = Write_Record( fid, Cloud(m), Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Cloud element #",i0," to ",a)' ) &
                   m, TRIM(Filename)
        CALL Write_Cleanup(Close_File=.TRUE.); RETURN
      END IF
    END DO Cloud_Loop


    ! Close the file (if error, no delete)
    ! ------------------------------------
    IF ( Yes_File_Close ) THEN
      CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat )
      IF ( io_stat /= 0 ) THEN
        WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
        CALL Write_Cleanup(); RETURN
      END IF
    END IF

    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      WRITE( msg,'("Number of clouds written to ",a,": ",i0)' ) TRIM(Filename), nc
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION, Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
          IF ( io_stat /= 0 ) &
            msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Write_Cloud_Binary


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single cloud data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID                 , &  ! Input
!                                   Cloud                  , &  ! Output
!                                   Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Cloud:        CRTM Cloud structure containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( fid        , &  ! Input
                        cloud      , &  ! Output
                        No_Allocate, &  ! Optional input
                        Message_Log) &  ! Error messaging
                      RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)     :: fid
    TYPE(CRTM_Cloud_type) , INTENT(IN OUT) :: cloud
    INTEGER,      OPTIONAL, INTENT(IN)     :: No_Allocate
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Cloud_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: Yes_Allocate
    INTEGER :: io_stat
    INTEGER :: n_Layers

    ! Set up
    ! ------
    err_stat = SUCCESS
    ! Default action is to allocate the structure....
    Yes_Allocate = .TRUE.
    ! ...unless the No_Allocate optional argument is set.
    IF ( PRESENT( No_Allocate ) ) THEN
      IF ( No_Allocate == SET ) Yes_Allocate = .FALSE.
    END IF

    ! Read the dimensions
    ! -------------------
    READ( fid,IOSTAT=io_stat ) n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading cloud data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

    ! Allocate the structure if required
    ! ----------------------------------
    IF ( Yes_Allocate ) THEN
      err_stat = CRTM_Allocate_Cloud( n_Layers, cloud, Message_Log=Message_Log )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error allocating cloud structure.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
    ELSE
      ! Structure already allocated. Check the association status
      IF ( .NOT. CRTM_Associated_Cloud( cloud ) ) THEN
        msg = 'Cloud structure components are not associated.'
        CALL Read_Record_Cleanup(); RETURN
      END IF
      ! Check the dimension values
      IF ( n_Layers /= Cloud%n_Layers ) THEN 
        msg = 'Cloud data dimensions are inconsistent with structure definition'
        CALL Read_Record_Cleanup(); RETURN
      END IF

    END IF
    
    ! Read the cloud data
    ! ---------------------
    READ( fid,IOSTAT=io_stat ) Cloud%Type, &
                               Cloud%Effective_Radius, &
                               Cloud%Effective_Variance, &
                               Cloud%Water_Content
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading Cloud data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      ! Deallocate cloud structure if necessary
      IF ( CRTM_Associated_Cloud( cloud ) ) THEN
        err_stat = CRTM_Destroy_Cloud( cloud, Message_Log=Message_Log )
        IF ( err_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error destroying cloud structure during error cleanup'
      END IF
      ! Close input file
      CLOSE( fid,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single cloud data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID                 , &  ! Input
!                                    Cloud                  , &  ! Input
!                                    Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       Cloud:        CRTM Cloud structure containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Cloud_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the record write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Write_Record( fid        , &  ! Input
                         Cloud      , &  ! Input
                         Message_Log) &  ! Error messaging
                       RESULT( err_stat )
    ! Arguments
    INTEGER               , INTENT(IN)  :: fid
    TYPE(CRTM_Cloud_type) , INTENT(IN)  :: cloud
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Cloud_Binary(Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
 
    ! Setup
    ! -----
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Associated_Cloud( Cloud ) ) THEN
      msg = 'Some or all INPUT Cloud pointer members are NOT associated.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    ! --------------------
    WRITE( fid,IOSTAT=io_stat ) Cloud%n_Layers
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Cloud data dimensions. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    ! --------------
    WRITE( fid,IOSTAT=io_stat ) Cloud%Type, &
                                Cloud%Effective_Radius, &
                                Cloud%Effective_Variance, &
                                Cloud%Water_Content
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing Cloud data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      ! Close and delete output file
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      ! Report error(s)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat, Message_Log=Message_Log )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_Cloud_Binary_IO
