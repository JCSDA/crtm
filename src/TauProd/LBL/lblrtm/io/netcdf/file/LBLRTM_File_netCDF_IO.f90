!
! LBLRTM_File_netCDF_IO
!
! Module containing routine to read and write LBLRTM File objects from/to
! a netCDF format file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Feb-2014
!                       paul.vandelst@noaa.gov
!

MODULE LBLRTM_File_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds            , ONLY: FP, IP, DP => Double, Long
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE String_Utility        , ONLY: StrClean
  USE LBLRTM_File_Define    , ONLY: LBLRTM_File_type      , &
                                    LBLRTM_File_SetValid  , &
                                    LBLRTM_File_Associated, &
                                    LBLRTM_File_Destroy   , &
                                    LBLRTM_File_Create    , &
                                    LBLRTM_File_Inspect
  USE LBLRTM_Layer_netCDF_IO, ONLY: LBLRTM_Layer_netCDF_WriteGroup, &
                                    LBLRTM_Layer_netCDF_ReadGroup , &
                                    LBLRTM_Layer_netCDF_IOVersion
  USE netcdf  
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_File_netCDF_Inquire
  PUBLIC :: LBLRTM_File_netCDF_Write
  PUBLIC :: LBLRTM_File_netCDF_Read
  PUBLIC :: LBLRTM_File_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(DP), PARAMETER :: ZERO = 0.0_DP
  REAL(DP), PARAMETER :: ONE  = 1.0_DP
  ! Extra parameters not in netCDF(?)
  INTEGER, PARAMETER :: MAX_N_GROUPS = 8096
  ! Direction flags for calculation
  INTEGER, PARAMETER :: UPWELLING   = 1
  INTEGER, PARAMETER :: DOWNWELLING = 2
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(2) = ['Upwelling  ','Downwelling']

   ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'Title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'Comment'

  ! Dimension names
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME = 'n_Layers'

  ! Variable names
  CHARACTER(*), PARAMETER :: DIRECTION_VARNAME = 'Direction'

  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  CHARACTER(*), PARAMETER :: DIRECTION_LONGNAME = 'Calculation Direction'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  CHARACTER(*), PARAMETER :: DIRECTION_DESCRIPTION = &
    'Direction flag for calculation. Upwelling(SFC->TOA)=1, Downwelling(TOA->SFC)=2'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  CHARACTER(*), PARAMETER :: DIRECTION_UNITS = 'N/A'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  INTEGER(Long), PARAMETER :: DIRECTION_FILLVALUE = 0

  ! Variable netCDF datatypes
  INTEGER(Long), PARAMETER :: DIRECTION_TYPE = NF90_INT



CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_netCDF_Inquire
!
! PURPOSE:
!       Function to inquire LBLRTM netCDF format files.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_netCDF_Inquire( &
!                        Filename, &
!                        n_Layers = n_Layers, &
!                        Title    = Title   , &
!                        History  = History , &
!                        Comment  = Comment   )
!
! INPUTS:
!       Filename:           Character string specifying the name of the
!                           LBLRTM netCDF data file to inquire.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Layers:           The number of layers of data in the LBLRTM netCDF
!                           file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the LBLRTM netCDF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the LBLRTM netCDF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the LBLRTM netCDF file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_netCDF_Inquire( &
    Filename , &  ! Input
    n_Layers , &  ! Optional Output
    Title    , &  ! Optional output
    History  , &  ! Optional output
    Comment  ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_netCDF_IO::Inquire'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: fileid
    INTEGER(Long) :: groupid(MAX_N_GROUPS)
    INTEGER(Long) :: n_groups

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.


    ! Open the file
    nf90_stat = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '// &
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Get the number of layers
    nf90_stat = NF90_INQ_GRPS( fileid,n_groups,groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring group IDs in '//TRIM(Filename)//' - '// &
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the global attributes
    err_stat = ReadGAtts( &
      fileid, &
      Title   = Title  , &
      History = History, &
      Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid )
    close_file = .FALSE.
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Layers) ) n_Layers = n_groups

  CONTAINS

    SUBROUTINE Inquire_CleanUp()
      IF ( close_file ) THEN
        nf90_stat = NF90_CLOSE( fileid )
        IF ( nf90_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION LBLRTM_File_netCDF_Inquire


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_netCDF_Write
!
! PURPOSE:
!       Function to write LBLRTM File objects in netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_netCDF_Write( &
!                        LBLRTM_File      , &
!                        Filename         , &
!                        Quiet   = Quiet  , &
!                        Clobber = Clobber, &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! OBJECTS:
!       LBLRTM:         LBLRTM File object containing the data to write
!                       to netCDF file.
!                       UNITS:      N/A
!                       TYPE:       LBLRTM_File_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       netCDF LBLRTM data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
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
!       Clobber:        Set this logical argument to overwrite an existing filename
!                       If == .FALSE., an existing file is NOT overwritten, and the
!                                      function returns with an error [DEFAULT].
!                          == .TRUE.,  an existing file is overwritten with the new data.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:          Character string written into the TITLE global
!                       attribute field of the LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the LBLRTM file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_netCDF_Write( &
    LBLRTM_File, &  ! Input
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Clobber    , &  ! Optional input
    Title      , &  ! Optional input
    History    , &  ! Optional input
    Comment    , &  ! Optional input
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_File_type), INTENT(IN) :: LBLRTM_File
    CHARACTER(*),           INTENT(IN) :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: Clobber
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_netCDF_IO::Write'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: groupname
    LOGICAL :: noisy
    LOGICAL :: debug_output
    LOGICAL :: no_clobber
    LOGICAL :: new_file
    INTEGER :: k
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: clobber_flag
    INTEGER(Long) :: fileid

    ! Set up
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. LBLRTM_File_Associated( LBLRTM_File ) ) THEN
      msg = 'LBLRTM_File object is empty. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Check Clobber argument
    no_clobber = .TRUE.
    IF ( PRESENT(Clobber) ) no_clobber = .NOT. Clobber
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(Debug) ) debug_output = Debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Open the file
    new_file = .TRUE.
    ! ...Set the clobber flag
    IF ( no_clobber ) THEN
      clobber_flag = NF90_NOCLOBBER
    ELSE
      clobber_flag = NF90_CLOBBER
    END IF
    ! ...Create the file
    nf90_stat = NF90_CREATE( &
      Filename, &
      clobber_flag+NF90_NETCDF4, &
      fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      ! Was creation failure due to existing file?
      IF ( nf90_stat == NF90_EEXIST ) THEN
        ! ...Yes, so just open it
        nf90_stat = NF90_OPEN( &
          Filename  , &
          NF90_WRITE, &  ! Test with NF90_SHARE?
          fileid      )
        IF ( nf90_stat /= NF90_NOERR ) THEN
          msg = 'Error opening existing file, '//TRIM(Filename)//', for write access - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
          CALL Write_Cleanup(); RETURN
        END IF
        new_file = .FALSE.
      ELSE
        ! ...No, so toss an error
        msg = 'Error creating '//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write the global attributes
    IF ( new_file ) THEN
      err_stat = WriteGAtts( &
        fileid  , &
        Title   = Title  , &
        History = History, &
        Comment = Comment  )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing global attribute to '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END IF


    ! Write each layer as a separate group
    DO k = 1, LBLRTM_File%n_Layers
    
      WRITE(groupname,'("Layer-",i3.3)') k
      
      err_stat = LBLRTM_Layer_netCDF_WriteGroup( &
        LBLRTM_File%Layer(k), &
        fileid, &
        GroupName = groupname, &
        Quiet     = Quiet    , &
        Debug     = Debug      )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing '//TRIM(groupname)//' group data to '//TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
      
    END DO


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//' written', INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION LBLRTM_File_netCDF_Write


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_netCDF_Read
!
! PURPOSE:
!       Function to read LBLRTM netCDF format files
!
! CALLING SEQUENCE:
!       Error_Status = LBLRTM_File_netCDF_Read( &
!                        LBLRTM_File      , &
!                        Filename         , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! OBJECTS:
!       LBLRTM_File:    LBLRTM File object to contain the data read
!                       from file.
!                       UNITS:      N/A
!                       TYPE:       LBLRTM_File_type
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       LBLRTM netCDF data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
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
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the LBLRTM netCDF file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the LBLRTM netCDF file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the LBLRTM netCDF file.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data write was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_File_netCDF_Read( &
    LBLRTM_File, &  ! Output
    Filename   , &  ! Input
    Quiet      , &  ! Optional input
    Title      , &  ! Optional output
    History    , &  ! Optional output
    Comment    , &  ! Optional output
    Debug      ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_File_type), INTENT(OUT) :: LBLRTM_File
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_File_netCDF_IO::Read'
    ! Function variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: groupname
    LOGICAL :: close_file
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER :: alloc_stat
    INTEGER :: k, n_layers
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: fileid


    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(Debug) ) debug_output = Debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Inquire the file to get the number of layers
    err_stat = LBLRTM_File_netCDF_Inquire( Filename, n_Layers = n_layers )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error obtaining layer count from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Create the file object
    CALL LBLRTM_File_Create( LBLRTM_File,n_layers )
    IF ( .NOT. LBLRTM_File_Associated(LBLRTM_File) ) THEN
      msg = 'File object allocation failed'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file for reading
    nf90_stat = NF90_OPEN( Filename,NF90_NOWRITE,fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
      fileid  , &
      Title   = Title  , &
      History = History, &
      Comment = Comment  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read each layer as a separate group
    DO k = 1, LBLRTM_File%n_Layers
    
      WRITE(groupname,'("Layer-",i3.3)') k
      
      err_stat = LBLRTM_Layer_netCDF_ReadGroup( &
        LBLRTM_File%Layer(k), &
        fileid, &
        GroupName = groupname, &
        Quiet     = Quiet    , &
        Debug     = Debug      )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading '//TRIM(groupname)//' group data from '//TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
      
    END DO


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid ); close_file = .FALSE.
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Tag object as valid
    CALL LBLRTM_File_SetValid(LBLRTM_File)
    IF ( debug_output ) CALL LBLRTM_File_Inspect(LBLRTM_File)

    ! Output an info message
    IF ( noisy ) THEN
      CALL Display_Message( ROUTINE_NAME, 'FILE: '//TRIM(Filename)//' read', INFORMATION )
    END IF

  CONTAINS

    SUBROUTINE Read_CleanUp()
      IF ( close_file ) THEN
        nf90_stat = NF90_CLOSE( fileid )
        IF ( nf90_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_stat ))
      END IF
      CALL LBLRTM_File_Destroy( LBLRTM_File )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_File_netCDF_Read


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_File_netCDF_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the netCDF
!       I/O module(s).
!
! CALLING SEQUENCE:
!       CALL LBLRTM_File_netCDF_IOVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               netCDF I/O module(s). If the string length is sufficient, the
!               version information for all the modules (this, and those for any
!               embedded object components) are concatenated. Otherwise only the
!               version id for this module is returned.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_File_netCDF_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_File_netCDF_IOVersion



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  INCLUDE 'LBLRTM_File_netCDF_IO.inc'

END MODULE LBLRTM_File_netCDF_IO
