!
!
! CRTM_Aerosol_IO
!
! Module containing routines to inquire, read, and write CRTM
! Aerosol object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 16-Mar-2005
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_Aerosol_IO


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility       , ONLY: File_Exists, File_Open
  USE Message_Handler    , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility, ONLY: Open_Binary_File
  USE CRTM_Aerosol_Define, ONLY: CRTM_Aerosol_type, &
                                 CRTM_Aerosol_Associated, &
                                 CRTM_Aerosol_Destroy, &
                                 CRTM_Aerosol_Create
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_Aerosol_InquireFile
  PUBLIC :: CRTM_Aerosol_ReadFile
  PUBLIC :: CRTM_Aerosol_WriteFile
  PUBLIC :: CRTM_Aerosol_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Default message length
  INTEGER, PARAMETER :: ML = 256
  ! File status on close after write error
  CHARACTER(*), PARAMETER :: WRITE_ERROR_STATUS = 'DELETE'


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
!       CRTM_Aerosol_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_InquireFile( Filename               , &
!                                                n_Aerosols = n_Aerosols  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM Aerosol data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Aerosols:     The number of Aerosol profiles in the data file.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file inquire was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_InquireFile( &
    Filename  , &  ! Input
    n_Aerosols) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Aerosols
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: na

    ! Setup
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Open the aerosol data file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Read the number of aerosols dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Aerosols dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

    
    ! Close the file
    CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Inquire_Cleanup(); RETURN
    END IF

    
    ! Set the return arguments
    IF ( PRESENT(n_Aerosols) ) n_Aerosols = na

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_Aerosol_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_ReadFile
!
! PURPOSE:
!       Function to read CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_ReadFile( Filename               , &
!                                             Aerosol                , &
!                                             Quiet      = Quiet     , &
!                                             No_Close   = No_Close  , &
!                                             n_Aerosols = n_Aerosols  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       Aerosol format data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Aerosol:        CRTM Aerosol object array containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit. 
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Aerosols:     The actual number of aerosol profiles read in.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file read was successful
!                          == FAILURE, an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_Aerosol_Readfile( &
    Filename  , &  ! Input
    Aerosol   , &  ! Output
    Quiet     , &  ! Optional input
    No_Close  , &  ! Optional input
    n_Aerosols, &  ! Optional output
    Debug     ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(OUT) :: Aerosol(:)
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: No_Close
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Aerosols
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m
    INTEGER :: na

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check file close argument
    yes_close = .TRUE.
    IF ( PRESENT(No_Close) ) yes_close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug

    
    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      ! ...Get the file id
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its unit number'
        CALL Read_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      ! ...Check that the file exists
      IF ( .NOT. File_Exists( Filename ) ) THEN
        msg = 'File '//TRIM(Filename)//' not found.'
        CALL Read_Cleanup(); RETURN
      END IF 
      ! ...Open the file
      err_stat = Open_Binary_File( Filename, fid )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF


    ! Read the number of aerosols dimension
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Aerosols data dimension from '//TRIM(Filename)//' - '//TRIM(io_msg)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if output array large enough
    IF ( na > SIZE(Aerosol) ) THEN
      WRITE( msg,'("Number of aerosols, ",i0," > size of the output ",&
             &"Aerosol object array, ",i0,".")' ) na, SIZE(Aerosol)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the aerosol data
    Aerosol_Loop: DO m = 1, na
      err_stat = Read_Record( fid, Aerosol(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading Aerosol element #",i0," from ",a)' ) m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Aerosol_Loop


    ! Close the file
    IF ( yes_close ) THEN
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//' - '//TRIM(io_msg)
        CALL Read_Cleanup(); RETURN
      END IF
    END IF
    
    
    ! Set the optional return values
    IF ( PRESENT(n_Aerosols) ) n_Aerosols = na

 
    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of aerosols read from ",a,": ",i0)' ) TRIM(Filename), na
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//TRIM(io_msg)
      END IF
      CALL CRTM_Aerosol_Destroy( Aerosol )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_Aerosol_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_WriteFile
!
! PURPOSE:
!       Function to write CRTM Aerosol object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Aerosol_WriteFile( Filename           , &
!                                              Aerosol            , &
!                                              Quiet    = Quiet   , &
!                                              No_Close = No_Close  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       Aerosol format data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       Aerosol:        CRTM Aerosol object array containing the Aerosol data.
!                       UNITS:      N/A
!                       TYPE:       CRTM_Aerosol_type
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:          Set this logical argument to suppress INFORMATION
!                       messages being printed to stdout
!                       If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                          == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       No_Close:       Set this logical argument to NOT close the file upon exit.
!                       If == .FALSE., the input file is closed upon exit [DEFAULT]
!                          == .TRUE.,  the input file is NOT closed upon exit. 
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS, the file write was successful
!                          == FAILURE, an unrecoverable error occurred.
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

  FUNCTION CRTM_Aerosol_WriteFile( &
    Filename, &  ! Input
    Aerosol , &  ! Input
    Quiet   , &  ! Optional input
    No_Close, &  ! Optional input
    Debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: Aerosol(:)
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: No_Close
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    LOGICAL :: noisy
    LOGICAL :: yes_close
    INTEGER :: fid
    INTEGER :: m, na
 
    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check file close argument
    yes_close = .TRUE.
    IF ( PRESENT(No_Close) ) yes_close = .NOT. No_Close
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) noisy = Debug

    ! Check the Aerosol structure dimensions
    IF ( ANY(Aerosol%n_Layers < 1) ) THEN 
      msg = 'Dimensions of Aerosol structures are < or = 0.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Check if the file is open
    IF ( File_Open( FileName ) ) THEN
      ! Yes, the file is already open
      INQUIRE( FILE=Filename,NUMBER=fid )
      IF ( fid == -1 ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for its unit number'
        CALL Write_Cleanup(); RETURN
      END IF
    ELSE
      ! No, the file is not open
      err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error opening '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the number of aerosols dimension
    na = SIZE(Aerosol)    
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) na
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing n_Aerosols data dimension to '//TRIM(Filename)//'- '//TRIM(io_msg)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the aerosol data
    Aerosol_Loop: DO m = 1, na
      err_stat = Write_Record( fid, Aerosol(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing Aerosol element #",i0," to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Aerosol_Loop


    ! Close the file (if error, no delete)
    IF ( yes_close ) THEN
      CLOSE( fid,STATUS='KEEP',IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= 0 ) THEN
        msg = 'Error closing '//TRIM(Filename)//'- '//TRIM(io_msg)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of aerosols written to ",a,": ",i0)' ) TRIM(Filename), na
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open(fid) ) THEN
        CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup - '//TRIM(io_msg)
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_Aerosol_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_Aerosol_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_Aerosol_IOVersion( Id )
!
! OUTPUT ARGUMENTS:
!       Id:            Character string containing the version Id information
!                      for the module.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE CRTM_Aerosol_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE CRTM_Aerosol_IOVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PRIVATE MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single CRTM Aerosol object in binary format
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, Aerosol )
!

  FUNCTION Read_Record( &
    fid    , &  ! Input
    aerosol) &  ! Output
  RESULT( err_stat )
    ! Arguments
    INTEGER                , INTENT(IN)     :: fid
    TYPE(CRTM_Aerosol_type), INTENT(IN OUT) :: aerosol
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_ReadFile(Binary Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
    INTEGER :: n_layers

    ! Set up
    err_stat = SUCCESS


    ! Read the dimensions
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) n_layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading n_Layers dimension - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF


    ! Allocate the structure
    CALL CRTM_Aerosol_Create( aerosol, n_layers )
    IF ( .NOT. CRTM_Aerosol_Associated( aerosol ) ) THEN
      msg = 'Aerosol object allocation failed.'
      CALL Read_Record_Cleanup(); RETURN
    END IF

    
    ! Read the aerosol data
    READ( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      aerosol%Type, &
      aerosol%Effective_Radius, &
      aerosol%Concentration
    IF ( io_stat /= 0 ) THEN
      msg = 'Error reading Aerosol data - '//TRIM(io_msg)
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_Aerosol_Destroy( aerosol )
      CLOSE( fid,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup - '//TRIM(io_msg)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single CRTM Aerosol object in binary format
!

  FUNCTION Write_Record( &
    fid    , &  ! Input
    aerosol) &  ! Input
  RESULT( err_stat )
    ! Arguments
    INTEGER                , INTENT(IN)  :: fid
    TYPE(CRTM_Aerosol_type), INTENT(IN)  :: aerosol
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Aerosol_WriteFile(Binary Record)'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    INTEGER :: io_stat
 
    ! Setup
    err_stat = SUCCESS
    IF ( .NOT. CRTM_Aerosol_Associated( aerosol ) ) THEN
      msg = 'Input Aerosol object is not used.'
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) Aerosol%n_Layers
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing dimensions - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF


    ! Write the data
    WRITE( fid,IOSTAT=io_stat,IOMSG=io_msg ) &
      Aerosol%Type, &
      Aerosol%Effective_Radius, &
      Aerosol%Concentration
    IF ( io_stat /= 0 ) THEN
      msg = 'Error writing Aerosol data - '//TRIM(io_msg)
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid,STATUS=WRITE_ERROR_STATUS,IOSTAT=io_stat,IOMSG=io_msg )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_Aerosol_IO
