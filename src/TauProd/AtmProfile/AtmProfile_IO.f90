!
! AtmProfile_IO
!
! Container module for Binary and netCDF AtmProfile I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-May-2010
!                       paul.vandelst@noaa.gov
!

MODULE AtmProfile_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds          , ONLY: fp
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility        , ONLY: File_Exists
  USE AtmProfile_Define   , ONLY: AtmProfile_type, OPERATOR(==)
! *** NOT IMPLEMENTED ***
!  USE AtmProfile_Binary_IO, ONLY: AtmProfile_Binary_InquireFile, &
!                                  AtmProfile_Binary_ReadFile   , &
!                                  AtmProfile_Binary_WriteFile  , &
!                                  AtmProfile_Binary_IOVersion
! *** NOT IMPLEMENTED ***
  USE AtmProfile_netCDF_IO, ONLY: AtmProfile_netCDF_InquireFile, &
                                  AtmProfile_netCDF_ReadFile   , &
                                  AtmProfile_netCDF_WriteFile  , &
                                  AtmProfile_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: AtmProfile_InquireFile
  PUBLIC :: AtmProfile_ReadFile
  PUBLIC :: AtmProfile_WriteFile
! *** NOT IMPLEMENTED ***
!  PUBLIC :: AtmProfile_netCDF_to_Binary
! *** NOT IMPLEMENTED ***
  PUBLIC :: AtmProfile_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  

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
!       AtmProfile_InquireFile
!
! PURPOSE:
!       Function to inquire AtmProfile object files.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_InquireFile( &
!                        Filename, &
!                        netCDF         = netCDF        , &
!                        n_Layers       = n_Layers      , &
!                        n_Absorbers    = n_Absorbers   , &
!                        n_Profiles     = n_Profiles    , &
!                        Release        = Release       , &
!                        Version        = Version       , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          AtmProfile data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!       netCDF:            Set this logical argument to access netCDF format
!                          AtmProfile datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!
! OPTIONAL OUTPUTS:
!       n_Layers:          The number of atmospheric layers dimension of the
!                          atmospheric profile data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:       The number of molecular absorbers dimension of the
!                          atmospheric profile data.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Profiles:        The number of profiles contained in the netCDF
!                          dataset.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the AtmProfile file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the AtmProfile file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the AtmProfile file.
!                          This argument is ignored for non-netCDF files.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the AtmProfile file.
!                          This argument is ignored for non-netCDF files.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the AtmProfile file.
!                          This argument is ignored for non-netCDF files.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_Id:    Character string written into the PROFILE_SET_ID global
!                          attribute field of the AtmProfile file.
!                          This argument is ignored for non-netCDF files.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error
!                          status. The error codes are defined in the
!                          Message_Handler module.
!                          If == SUCCESS the file inquiry was successful
!                             == FAILURE an error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AtmProfile_InquireFile( &
    Filename      , &  ! Input
    netCDF        , &  ! Optional input
    n_Layers      , &  ! Optional output
    n_Absorbers   , &  ! Optional output
    n_Profiles    , &  ! Optional output
    Release       , &  ! Optional output
    Version       , &  ! Optional output
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    Profile_Set_Id) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Profiles
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release         
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    Binary = .FALSE.
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***


    ! Call the appropriate function
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!    IF ( Binary ) THEN
!      err_stat = AtmProfile_Binary_InquireFile( &
!                   Filename, &
!                   n_Layers    = n_Layers   , &
!                   n_Absorbers = n_Absorbers, &
!                   n_Profiles  = n_Profiles , &
!                   Release     = Release    , &
!                   Version     = Version      )
!    ELSE
!      err_stat = AtmProfile_netCDF_InquireFile( &
!                   Filename, &
!                   n_Layers       = n_Layers      , &
!                   n_Absorbers    = n_Absorbers   , &
!                   n_Profiles     = n_Profiles    , &
!                   Release        = Release       , &
!                   Version        = Version       , &
!                   Title          = Title         , &
!                   History        = History       , &
!                   Comment        = Comment       , &
!                   Profile_Set_Id = Profile_Set_Id  )
!    END IF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
      err_stat = AtmProfile_netCDF_InquireFile( &
                   Filename, &
                   n_Layers       = n_Layers      , &
                   n_Absorbers    = n_Absorbers   , &
                   n_Profiles     = n_Profiles    , &
                   Release        = Release       , &
                   Version        = Version       , &
                   Title          = Title         , &
                   History        = History       , &
                   Comment        = Comment       , &
                   Profile_Set_Id = Profile_Set_Id  )

  END FUNCTION AtmProfile_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_ReadFile
!
! PURPOSE:
!       Function to read AtmProfile object files.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_ReadFile( &
!                        Filename  , &
!                        AtmProfile, &
!                        netCDF         = netCDF        , &
!                        Quiet          = Quiet         , &
!                        Reverse        = Reverse       , &
!                        Profile_List   = Profile_List  , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AtmProfile:     Array of objects, each element of which contains
!                       atmospheric profile data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!       netCDF:         Set this logical argument to access netCDF format
!                       AtmProfile datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!
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
!       Reverse:        Set this logical keyword to reverse the order of the
!                       profile data arrays in the K index (vertical) dimension.
!                       If REVERSE = .FALSE., arrays are returned as they are
!                                       stored in the netCDF input file (DEFAULT)
!                          REVERSE = .TRUE., arrays are returned in reverse order
!                                       to how they are stored in the input
!                                       netCDF file.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_List:   Index array specifying the list of profiles to be
!                       read from the file. If not specified, the indices 
!                       of the profile read are:
!                         1, 2, 3, ..., MIN(SIZE(AtmProfile),n_Profiles)
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!
!       Profile_Set_Id: Character string written into the PROFILE_SET_ID global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored for non-netCDF files.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AtmProfile_ReadFile( &
    Filename      , &  ! Input
    AtmProfile    , &  ! Output
    netCDF        , &  ! Optional input
    Quiet         , &  ! Optional input
    Reverse       , &  ! Optional input
    Profile_List  , &  ! Optional input
    Title         , &  ! Optional output
    History       , &  ! Optional output
    Comment       , &  ! Optional output
    Profile_Set_Id) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(AtmProfile_type),  INTENT(OUT) :: AtmProfile(:)
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Reverse
    INTEGER,      OPTIONAL, INTENT(IN)  :: Profile_List(:)
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    Binary = .FALSE.
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***

    ! Call the appropriate function
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!    IF ( Binary ) THEN
!      err_stat = AtmProfile_Binary_ReadFile( &
!                   Filename  , &
!                   AtmProfile, &
!                   Quiet          = Quiet       , &
!                   Reverse        = Reverse     , &
!                   Profile_List   = Profile_List  )
!    ELSE
!      err_stat = AtmProfile_netCDF_ReadFile( &
!                   Filename  , &
!                   AtmProfile, &
!                   Quiet          = Quiet         , &
!                   Reverse        = Reverse       , &
!                   Profile_List   = Profile_List  , &
!                   Title          = Title         , &
!                   History        = History       , &
!                   Comment        = Comment       , &
!                   Profile_Set_Id = Profile_Set_Id  )
!    END IF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    err_stat = AtmProfile_netCDF_ReadFile( &
                 Filename  , &
                 AtmProfile, &
                 Quiet          = Quiet         , &
                 Reverse        = Reverse       , &
                 Profile_List   = Profile_List  , &
                 Title          = Title         , &
                 History        = History       , &
                 Comment        = Comment       , &
                 Profile_Set_Id = Profile_Set_Id  )

  END FUNCTION AtmProfile_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_WriteFile
!
! PURPOSE:
!       Function to write AtmProfile object files.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_WriteFile( &
!                        Filename  , &
!                        AtmProfile, &
!                        netCDF         = netCDF        , &
!                        Quiet          = Quiet         , &
!                        Title          = Title         , &
!                        History        = History       , &
!                        Comment        = Comment       , &
!                        Profile_Set_Id = Profile_Set_Id  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       AtmProfile data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AtmProfile:     Array of objects, each element of which contains
!                       atmospheric profile data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AtmProfile_type)
!                       DIMENSION:  Rank-1
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!       netCDF:         Set this logical argument to access netCDF format
!                       AtmProfile datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!
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
!       Title:          Character string written into the TITLE global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored for non-netCDF files.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored for non-netCDF files.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored for non-netCDF files.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_Id: Character string written into the PROFILE_SET_ID global
!                       attribute field of the AtmProfile file.
!                       This argument is ignored for non-netCDF files.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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

  FUNCTION AtmProfile_WriteFile( &
    Filename      , &  ! Input
    AtmProfile    , &  ! Input
    netCDF        , &  ! Optional input
    Quiet         , &  ! Optional input
    Title         , &  ! Optional input
    History       , &  ! Optional input
    Comment       , &  ! Optional output
    Profile_Set_Id) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(AtmProfile_type),  INTENT(IN) :: AtmProfile(:)
    LOGICAL,      OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    ! Function result
    INTEGER :: err_stat
    ! Local variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    Binary = .FALSE.
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***

    ! Call the appropriate function
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!    IF ( Binary ) THEN
!      err_stat = AtmProfile_Binary_WriteFile( &
!                   Filename  , &
!                   AtmProfile, &
!                   Quiet = Quiet )
!    ELSE
!      err_stat = AtmProfile_netCDF_WriteFile( &
!                   Filename  , &
!                   AtmProfile, &
!                   Quiet          = Quiet         , &
!                   Title          = Title         , &
!                   History        = History       , &
!                   Comment        = Comment       , &
!                   Profile_Set_Id = Profile_Set_Id  )
!    END IF
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    err_stat = AtmProfile_netCDF_WriteFile( &
                 Filename  , &
                 AtmProfile, &
                 Quiet          = Quiet         , &
                 Title          = Title         , &
                 History        = History       , &
                 Comment        = Comment       , &
                 Profile_Set_Id = Profile_Set_Id  )

  END FUNCTION AtmProfile_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF AtmProfile file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = AtmProfile_netCDF_to_Binary( &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format AtmProfile data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format AtmProfile data file to write.
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
! FUNCTION RESULT:
!       Error_Status:   The return value is an integer defining the error status.
!                       The error codes are defined in the Message_Handler module.
!                       If == SUCCESS the file conversion was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs, the output file is deleted before
!         returning to the calling routine.
!
!:sdoc-:
!------------------------------------------------------------------------------

!  FUNCTION AtmProfile_netCDF_to_Binary( &
!    NC_Filename , &  ! Input
!    BIN_Filename, &  ! Input
!    Quiet       ) &  ! Optional input
!  RESULT( err_stat )
!    ! Arguments
!    CHARACTER(*),      INTENT(IN)  :: NC_Filename
!    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
!    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
!    ! Function result
!    INTEGER :: err_stat
!    ! Function parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AtmProfile_netCDF_to_Binary'
!    ! Function variables
!    CHARACTER(256) :: msg
!    TYPE(AtmProfile_type) :: cc, cc_copy
!    
!    ! Set up
!    err_stat = SUCCESS
!
!    ! Read the netCDF file
!    err_stat = AtmProfile_ReadFile( NC_Filename, cc, Quiet = Quiet, netCDF = .TRUE. )
!    IF ( err_stat /= SUCCESS ) THEN
!      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
!      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
!      RETURN
!    END IF
!
!    ! Write the Binary file
!    err_stat = AtmProfile_WriteFile( BIN_Filename, cc, Quiet = Quiet )
!    IF ( err_stat /= SUCCESS ) THEN
!      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
!      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
!      RETURN
!    END IF
!
!    ! Check the write was successful
!    ! ...Read the Binary file
!    err_stat = AtmProfile_ReadFile( BIN_Filename, cc_copy, Quiet = Quiet )
!    IF ( err_stat /= SUCCESS ) THEN
!      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
!      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
!      RETURN
!    END IF
!    ! ...Compare the AtmProfile objects
!    IF ( .NOT. (cc == cc_copy) ) THEN
!      msg = 'AtmProfile object comparison failed.'
!      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
!      RETURN
!    END IF
!
!  END FUNCTION AtmProfile_netCDF_to_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AtmProfile_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL AtmProfile_IOVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information
!               for the I/O module(s). If the string length is sufficient,
!               the version information for all the modules (this, the
!               Binary I/O, and netCDF I/O modules) are concatenated. Otherwise
!               only the version id for this module is returned.
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE AtmProfile_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
!    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
    CHARACTER(SL)   :: netCDF_IO_Id
    CHARACTER(SL*3) :: IO_Id
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!    CALL AtmProfile_Binary_IOVersion( Binary_IO_Id )
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    CALL AtmProfile_netCDF_IOVersion( netCDF_IO_Id )
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
!    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
!            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
!            '  '//TRIM(netCDF_IO_Id)
! *** NOT IMPLEMENTED. ONLY netCDF ACCESS, NO BINARY ***
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(netCDF_IO_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE AtmProfile_IOVersion

END MODULE AtmProfile_IO
