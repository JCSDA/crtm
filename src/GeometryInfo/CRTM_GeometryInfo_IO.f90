!
! CRTM_GeometryInfo_IO
!
! Module containing routines to inquire, read, and write CRTM
! GeometryInfo object datafiles.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 13-Apr-2009
!                       paul.vandelst@noaa.gov
!

MODULE CRTM_GeometryInfo_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE File_Utility            , ONLY: File_Open, File_Exists
  USE Message_Handler         , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE Binary_File_Utility     , ONLY: Open_Binary_File
  USE CRTM_Geometry_IO        , ONLY: CRTM_Geometry_ReadRecord, &
                                      CRTM_Geometry_WriteRecord, &
                                      CRTM_Geometry_IOVersion
  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type, &
                                      CRTM_GeometryInfo_Destroy
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_GeometryInfo_InquireFile
  PUBLIC :: CRTM_GeometryInfo_ReadFile
  PUBLIC :: CRTM_GeometryInfo_WriteFile
  PUBLIC :: CRTM_GeometryInfo_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
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
!       CRTM_GeometryInfo_InquireFile
!
! PURPOSE:
!       Function to inquire CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_InquireFile( &
!                        Filename               , &
!                        n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:       Character string specifying the name of a
!                       CRTM GeometryInfo data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Profiles:     The number of profiles for which there is geometry 
!                       information in the data file.
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

  FUNCTION CRTM_GeometryInfo_InquireFile( &
    Filename  , &  ! Input
    n_Profiles) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_InquireFile'
    ! Function variables
    CHARACTER(ML) :: msg
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Read the number of profiles
    READ( fid,IOSTAT=io_stat ) m
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Inquire_Cleanup(); RETURN
    END IF

    ! Set the return arguments
    IF ( PRESENT(n_Profiles) ) n_Profiles = m

  CONTAINS
  
    SUBROUTINE Inquire_CleanUp()
      ! Close file if necessary
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= SUCCESS ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup'
      END IF
      ! Set error status and print error message
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION CRTM_GeometryInfo_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_ReadFile
!
! PURPOSE:
!       Function to read CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_ReadFile( &
!                        Filename               , &
!                        GeometryInfo           , &
!                        Quiet      = Quiet     , &
!                        n_Profiles = n_Profiles  )
!
! INPUTS:
!       Filename:     Character string specifying the name of an
!                     a GeometryInfo data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       GeometryInfo: CRTM GeometryInfo object array containing the
!                     data read from file.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Profiles:   The number of profiles for which data was read.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_ReadFile( &
    Filename    , &  ! Input
    GeometryInfo, &  ! Output
    Quiet       , &  ! Optional input
    n_Profiles  , &  ! Optional output
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                 INTENT(IN)  :: Filename
    TYPE(CRTM_GeometryInfo_type), INTENT(OUT) :: GeometryInfo(:)
    LOGICAL,            OPTIONAL, INTENT(IN)  :: Quiet
    INTEGER,            OPTIONAL, INTENT(OUT) :: n_Profiles
    LOGICAL,            OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_Geometry_ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: n_file_profiles
    INTEGER :: m, n_input_profiles
 

    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    ! ...Check that the file exists
    IF ( .NOT. File_Exists( TRIM(Filename) ) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the dimensions     
    READ( fid,IOSTAT=io_stat ) n_file_profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading dimensions from ",a,". IOSTAT = ",i0)' ) &
                 TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check if n_Profiles in file is > size of output array
    n_input_profiles = SIZE(GeometryInfo)
    IF ( n_file_profiles > n_input_profiles ) THEN
      WRITE( msg,'("Number of profiles, ",i0," > size of the output GeometryInfo ", &
                  &" array, ",i0,". Only the first ",i0, &
                  &" profiles will be read.")' ) &
                  n_file_profiles, n_input_profiles, n_input_profiles
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF
    n_input_profiles = MIN(n_input_profiles, n_file_profiles)


    ! Loop over all the profiles
    Profile_Loop: DO m = 1, n_input_profiles
      err_stat = Read_Record( fid, GeometryInfo(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error reading GeometryInfo element (",i0,") from ",a)' ) &
               m, TRIM(Filename)
        CALL Read_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file
    CLOSE( fid, IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Read_Cleanup(); RETURN
    END IF


    ! Set the return values
    IF ( PRESENT(n_Profiles) ) n_Profiles = n_input_profiles


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles read from ",a,": ",i0)' ) TRIM(Filename), n_Input_Profiles
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid,IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup.'
      END IF
      CALL CRTM_GeometryInfo_Destroy( GeometryInfo )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_CleanUp
  
  END FUNCTION CRTM_GeometryInfo_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_WriteFile
!
! PURPOSE:
!       Function to write CRTM GeometryInfo object files.
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_GeometryInfo_WriteFile( &
!                        Filename     , &
!                        Geometry     , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     GeometryInfo format data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo: CRTM GeometryInfo object array containing the
!                     data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Geometry_type
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this logical argument to suppress INFORMATION
!                     messages being printed to stdout
!                     If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                        == .TRUE.,  INFORMATION messages are SUPPRESSED.
!                     If not specified, default is .FALSE.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the file write was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs during *writing*, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CRTM_GeometryInfo_WriteFile( &
    Filename    , &  ! Input
    GeometryInfo, &  ! Input
    Quiet       , &  ! Optional input
    Debug       ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),                 INTENT(IN) :: Filename
    TYPE(CRTM_GeometryInfo_type), INTENT(IN) :: GeometryInfo(:)
    LOGICAL,            OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,            OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_WriteFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    INTEGER :: io_stat
    INTEGER :: fid
    INTEGER :: m, n_output_profiles
 
    ! Set up
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(Debug) ) THEN
      IF ( Debug ) noisy = .TRUE.
    END IF
    n_output_profiles = SIZE(GeometryInfo)


    ! Open the file
    err_stat = Open_Binary_File( Filename, fid, For_Output = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error opening '//TRIM(Filename)
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the dimensions
    WRITE( fid, IOSTAT=io_stat ) n_output_profiles
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing data dimensions to ",a,". IOSTAT = ",i0)' ) &
             TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF

    
    ! Write the data
    Profile_Loop: DO m = 1, n_output_profiles
      err_stat = Write_Record( fid, GeometryInfo(m) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error writing GeometryInfo element (",i0,") to ",a)' ) m, TRIM(Filename)
        CALL Write_Cleanup(); RETURN
      END IF
    END DO Profile_Loop


    ! Close the file (if error, no delete)
    CLOSE( fid, STATUS='KEEP', IOSTAT=io_stat )
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error closing ",a,". IOSTAT = ",i0)' ) TRIM(Filename), io_stat
      CALL Write_Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      WRITE( msg,'("Number of profiles written to ",a,": ",i0)' ) &
             TRIM(Filename), n_output_profiles
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp()
      IF ( File_Open( Filename ) ) THEN
        CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat )
        IF ( io_stat /= 0 ) &
          msg = TRIM(msg)//'; Error deleting output file during error cleanup.'
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION CRTM_GeometryInfo_WriteFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CRTM_GeometryInfo_IOVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL CRTM_GeometryInfo_IOVersion( Id )
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

  SUBROUTINE CRTM_GeometryInfo_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    CHARACTER(ML)   :: geometry_id
    CHARACTER(ML*2) :: io_id
    CALL CRTM_Geometry_IOVersion( geometry_id )
    io_id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//'  '//&
            TRIM(geometry_id)
    IF ( LEN_TRIM(io_id) <= LEN(Id) ) THEN
      Id = io_id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE CRTM_GeometryInfo_IOVersion


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!----------------------------------------------------------------------------------
!
! NAME:
!       Read_Record
!
! PURPOSE:
!       Utility function to read a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Read_Record( FileID, GeometryInfo )
!
! INPUTS:
!       FileID:       Logical unit number from which to read data.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       GeometryInfo: CRTM GeometryInfo object containing the data read in.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS, the read was successful
!                        == FAILURE, an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!----------------------------------------------------------------------------------

  FUNCTION Read_Record( fid, ginfo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)  :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(OUT) :: ginfo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_ReadFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat

    ! Set up
    err_stat = SUCCESS


    ! Read the embedded Geometry structure
    err_stat = CRTM_Geometry_ReadRecord( fid, ginfo%user )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading embedded Geometry data'
      CALL Read_Record_Cleanup(); RETURN
    END IF
    
    
    ! Read the data record
    READ( fid, IOSTAT=io_stat ) &
      ginfo%Distance_Ratio       , &
      ginfo%Sensor_Scan_Radian   , &
      ginfo%Sensor_Zenith_Radian , &
      ginfo%Sensor_Azimuth_Radian, &
      ginfo%Secant_Sensor_Zenith , &
      ginfo%Trans_Zenith_Radian  , &
      ginfo%Secant_Trans_Zenith  , &
      ginfo%Source_Zenith_Radian , &
      ginfo%Source_Azimuth_Radian, &
      ginfo%Secant_Source_Zenith , &
      ginfo%Flux_Zenith_Radian   , &
      ginfo%Secant_Flux_Zenith   , &
      ginfo%AU_ratio2            
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error reading GeometryInfo data. IOSTAT = ",i0)' ) io_stat
      CALL Read_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_Record_Cleanup()
      CALL CRTM_GeometryInfo_Destroy( ginfo )
      CLOSE( fid, IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Record_Cleanup

  END FUNCTION Read_Record


!----------------------------------------------------------------------------------
!
! NAME:
!       Write_Record
!
! PURPOSE:
!       Function to write a single GeometryInfo data record
!
! CALLING SEQUENCE:
!       Error_Status = Write_Record( FileID, GeometryInfo )
!
! INPUTS:
!       FileID:       Logical unit number to which data is written
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!       GeometryInfo: CRTM GeometryInfo object containing the data to write.
!                     UNITS:      N/A
!                     TYPE:       CRTM_GeometryInfo_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
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

  FUNCTION Write_Record( fid, ginfo ) RESULT( err_stat )
    ! Arguments
    INTEGER,                      INTENT(IN)  :: fid
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)  :: ginfo
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CRTM_GeometryInfo_WriteFile(Record)'
    ! Function variables
    CHARACTER(ML)  :: msg
    INTEGER :: io_stat
 
    ! Set up
    err_stat = SUCCESS


    ! Write the embedded Geometry structure
    err_stat = CRTM_Geometry_WriteRecord( fid, ginfo%user )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing embedded Geometry data'
      CALL Write_Record_Cleanup(); RETURN
    END IF
    
    
    ! Write the data record
    WRITE( fid, IOSTAT=io_stat ) &
      ginfo%Distance_Ratio       , &
      ginfo%Sensor_Scan_Radian   , &
      ginfo%Sensor_Zenith_Radian , &
      ginfo%Sensor_Azimuth_Radian, &
      ginfo%Secant_Sensor_Zenith , &
      ginfo%Trans_Zenith_Radian  , &
      ginfo%Secant_Trans_Zenith  , &
      ginfo%Source_Zenith_Radian , &
      ginfo%Source_Azimuth_Radian, &
      ginfo%Secant_Source_Zenith , &
      ginfo%Flux_Zenith_Radian   , &
      ginfo%Secant_Flux_Zenith   , &
      ginfo%AU_ratio2            
    IF ( io_stat /= 0 ) THEN
      WRITE( msg,'("Error writing GeometryInfo data. IOSTAT = ",i0)' ) io_stat
      CALL Write_Record_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Write_Record_Cleanup()
      CLOSE( fid, STATUS=WRITE_ERROR_STATUS, IOSTAT=io_stat )
      IF ( io_stat /= SUCCESS ) &
        msg = TRIM(msg)//'; Error closing file during error cleanup'
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Write_Record_Cleanup
    
  END FUNCTION Write_Record

END MODULE CRTM_GeometryInfo_IO
