!
! LBLRTM_Layer_netCDF_IO
!
! Module containing routine to read and write LBLRTM Layer objects as
! groups to a netCDF format file.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 19-Feb-2014
!                       paul.vandelst@noaa.gov
!

MODULE LBLRTM_Layer_netCDF_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double, Long
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE String_Utility       , ONLY: StrClean
  USE LBLRTM_Fhdr_Define   , ONLY: LBLRTM_Fhdr_type
  USE LBLRTM_Layer_Define  , ONLY: LBLRTM_Layer_type      , &
                                   LBLRTM_Layer_IsValid   , &
                                   LBLRTM_Layer_SetValid  , &
                                   LBLRTM_Layer_Associated, &
                                   LBLRTM_Layer_Destroy   , &
                                   LBLRTM_Layer_Create    , &
                                   LBLRTM_Layer_Inspect
  USE LBLRTM_Fhdr_netCDF_IO, ONLY: LBLRTM_Fhdr_netCDF_WriteGroup, &
                                   LBLRTM_Fhdr_netCDF_ReadGroup !, &
                                   !LBLRTM_Fhdr_netCDF_IOVersion
  USE netcdf
  ! Disable all implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Procedures
  PUBLIC :: LBLRTM_Layer_netCDF_WriteGroup
  PUBLIC :: LBLRTM_Layer_netCDF_ReadGroup
  !PUBLIC :: LBLRTM_Layer_netCDF_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = 'Dummy'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 1024
  ! Literal constants
  REAL(DP), PARAMETER :: ZERO = 0.0_DP
  REAL(DP), PARAMETER :: ONE  = 1.0_DP
  ! Extra parameters not in netCDF(?)
  INTEGER, PARAMETER :: MAX_N_GROUPS = 8096

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME = 'Version'
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME   = 'Title'
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME = 'History'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME = 'Comment'


  ! Dimension names
  CHARACTER(*), PARAMETER :: POINTS_DIMNAME   = 'n_Points'
  CHARACTER(*), PARAMETER :: SPECTRA_DIMNAME  = 'n_Spectra'


  ! Variable names
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_VARNAME    = 'Begin_Frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_VARNAME      = 'End_Frequency'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_VARNAME = 'Frequency_Interval'
  CHARACTER(*), PARAMETER :: SPECTRUM_VARNAME      = 'Spectrum'


  ! Variable long name attribute
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: BEGIN_FREQ_LONGNAME    = 'Begin Frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_LONGNAME      = 'End Frequency'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_LONGNAME = 'Frequency Interval'
  CHARACTER(*), PARAMETER :: SPECTRUM_LONGNAME      = 'Spectral Data'


  ! Variable description attribute
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: BEGIN_FREQ_DESCRIPTION    = 'Calculation begin frequency'
  CHARACTER(*), PARAMETER :: END_FREQ_DESCRIPTION      = 'Calculation end frequency'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_DESCRIPTION = 'Calculation frequency interval'
  CHARACTER(*), PARAMETER :: SPECTRUM_DESCRIPTION      = 'Calculated spectra'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: BEGIN_FREQ_UNITS    = 'cm^-1'
  CHARACTER(*), PARAMETER :: END_FREQ_UNITS      = 'cm^-1'
  CHARACTER(*), PARAMETER :: FREQ_INTERVAL_UNITS = 'cm^-1'
  CHARACTER(*), PARAMETER :: SPECTRUM_UNITS      = 'Variable'


  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  REAL(DP)    , PARAMETER :: BEGIN_FREQ_FILLVALUE    = 0.0_DP
  REAL(DP)    , PARAMETER :: END_FREQ_FILLVALUE      = 0.0_DP
  REAL(FP)    , PARAMETER :: FREQ_INTERVAL_FILLVALUE = 0.0_FP
  REAL(FP)    , PARAMETER :: SPECTRUM_FILLVALUE      = 0.0_FP


  ! Variable netCDF datatypes
  INTEGER(Long), PARAMETER :: BEGIN_FREQ_TYPE    = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: END_FREQ_TYPE      = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: FREQ_INTERVAL_TYPE = NF90_DOUBLE
  INTEGER(Long), PARAMETER :: SPECTRUM_TYPE      = NF90_DOUBLE


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PUBLIC MODULE ROUTINES ##                      ##
!##                                                                            ##
!################################################################################
!################################################################################

  !----------------------------------------------------
  ! Function to write an LBLRTM Layer object as a group
  !----------------------------------------------------
  FUNCTION LBLRTM_Layer_netCDF_WriteGroup( &
    Layer    , &  ! Input
    FileId   , &  ! Input
    GroupName, &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: Layer
    INTEGER(Long),           INTENT(IN) :: FileId
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: GroupName
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN) :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_netCDF_IO::WriteGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: groupid
    INTEGER(Long) :: n_points_dimid
    INTEGER(Long) :: n_spectra_dimid

    ! Setup
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. (LBLRTM_Layer_IsValid( Layer )) ) THEN
      msg = 'LBLRTM Layer object is invalid. Nothing to do!'
      CALL Write_CleanUp(); RETURN
    END IF
    ! ...Check GroupName argument, defining default.
    group_name = 'Layer'
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Create a new group for the file header data
    nf90_stat = NF90_DEF_GRP( &
      fileid, &
      group_name, &
      groupid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(group_name)//' group - '//&
            ' - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the Fhdr data for the layer
    err_stat = LBLRTM_Fhdr_netCDF_WriteGroup( &
      Layer%Header, &
      groupid, &
      Quiet = quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Fhdr subgroup for the '//TRIM(group_name)//' group.'
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the dimensions for the group
    err_stat = DefineDimensions( &
      Layer          , &
      groupid        , &
      n_points_dimid , &
      n_spectra_dimid  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining dimensions for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Define the variables for the group
    err_stat = DefineVariables( &
      groupid        , &
      n_points_dimid , &
      n_spectra_dimid  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_stat = NF90_ENDDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error taking file out of define mode to write the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Write the variables for the group
    err_stat = WriteVariables( Layer, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF


    ! Put netCDF file back into define mode
    nf90_stat = NF90_REDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error putting file back into define mode after writing the '//&
            TRIM(group_name)//' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Write_Cleanup(); RETURN
    END IF

  CONTAINS

    SUBROUTINE Write_CleanUp()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Write_CleanUp

  END FUNCTION LBLRTM_Layer_netCDF_WriteGroup


  !---------------------------------------------------
  ! Function to read an LBLRTM Layer object as a group
  !---------------------------------------------------
  FUNCTION LBLRTM_Layer_netCDF_ReadGroup( &
    Layer    , &  ! Output
    FileId   , &  ! Input
    GroupName, &  ! Optional input
    Quiet    , &  ! Optional input
    Debug    ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(OUT) :: Layer
    INTEGER(Long),           INTENT(IN)  :: FileId
    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: GroupName
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_netCDF_IO::ReadGroup'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: group_name
    LOGICAL :: noisy
    LOGICAL :: debug_output
    INTEGER(Long) :: nf90_stat
    INTEGER(Long) :: groupid
    INTEGER :: n_points, n_spectra
    TYPE(LBLRTM_Fhdr_type) :: fhdr

    ! Setup
    err_stat = SUCCESS
    ! ...Check GroupName argument, defining default.
    group_name = 'Layer'
    IF ( PRESENT(GroupName) ) THEN
      group_name = ADJUSTL(GroupName)
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet
    ! ...Set debug option
    debug_output = .FALSE.
    IF ( PRESENT(debug) ) debug_output = debug
    IF ( debug_output ) THEN
      CALL Display_Message(ROUTINE_NAME,'Entering...',INFORMATION)
      noisy = .TRUE.
    END IF


    ! Get the group id
    nf90_stat = NF90_INQ_GRP_NCID(fileid, group_name, groupid)
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(group_name)//' group for its group id - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the file header data for the group
    err_stat = LBLRTM_Fhdr_netCDF_ReadGroup( &
      fhdr   , &
      groupid, &
      Quiet = quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Fhdr subgroup for the '//TRIM(group_name)//' group.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Get the group dimensions
    err_stat = ReadDimensions( &
      groupid  , &
      n_points , &
      n_spectra  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading dimensions for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the current layer object
    CALL LBLRTM_Layer_Create( Layer, fhdr, n_spectra )
    IF ( .NOT. LBLRTM_Layer_Associated(Layer) ) THEN
      msg = 'Error allocating Layer object for group '//TRIM(group_name)
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Check that the number of points is the same!!
    IF ( Layer%n_Points /= n_points ) THEN
      msg = 'Calculated number of Layer points is different from file for group '//TRIM(group_name)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Read the variables for the group
    err_stat = ReadVariables( Layer, groupid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading variables for the '//TRIM(group_name)//&
            ' group - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Read_Cleanup(); RETURN
    END IF


    ! Tag object as valid
    CALL LBLRTM_Layer_SetValid(Layer)
    IF ( debug_output ) CALL LBLRTM_Layer_Inspect(Layer)

  CONTAINS

    SUBROUTINE Read_CleanUp()
      CALL LBLRTM_Layer_Destroy(Layer)
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Read_CleanUp

  END FUNCTION LBLRTM_Layer_netCDF_ReadGroup


  !------------------------------------------------
  ! Subroutine to return module version information
  !------------------------------------------------
  !SUBROUTINE LBLRTM_Layer_netCDF_IOVersion( Id )
  !  CHARACTER(*), INTENT(OUT) :: Id
  !  Id = MODULE_VERSION_ID
  !END SUBROUTINE LBLRTM_Layer_netCDF_IOVersion


!################################################################################
!################################################################################
!##                                                                            ##
!##                          ## PRIVATE MODULE ROUTINES ##                     ##
!##                                                                            ##
!################################################################################
!################################################################################

  INCLUDE 'LBLRTM_Layer_netCDF_IO.inc'

END MODULE LBLRTM_Layer_netCDF_IO
