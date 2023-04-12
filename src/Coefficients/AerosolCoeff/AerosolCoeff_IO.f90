
! AerosolCoeff_IO
!
! Container module for Binary and netCDF AerosolCoeff I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 28-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE AerosolCoeff_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds            , ONLY: fp
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility          , ONLY: File_Exists
  USE AerosolCoeff_Define   , ONLY: AerosolCoeff_type, OPERATOR(==)
  USE AerosolCoeff_Binary_IO, ONLY: AerosolCoeff_Binary_InquireFile, &
                                    AerosolCoeff_Binary_ReadFile   , &
                                    AerosolCoeff_Binary_WriteFile
                                    !AerosolCoeff_netCDF_IOVersion
  USE AerosolCoeff_netCDF_IO, ONLY: AerosolCoeff_netCDF_InquireFile, &
                                    AerosolCoeff_netCDF_ReadFile   , &
                                    AerosolCoeff_netCDF_WriteFile
                                    !AerosolCoeff_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: AerosolCoeff_InquireFile
  PUBLIC :: AerosolCoeff_ReadFile
  PUBLIC :: AerosolCoeff_WriteFile
  PUBLIC :: AerosolCoeff_netCDF_to_Binary
  !PUBLIC :: AerosolCoeff_IOVersion



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
!       AerosolCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire AerosolCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_InquireFile( &
!                        Aerosol_Model, &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_Wavelengths    = n_Wavelengths   , &
!                        n_Radii          = n_Radii         , &
!                        n_Types          = n_Types         , &
!                        n_RH             = n_RH            , &
!                        n_RH_Radii       = n_RH_Radii      , &
!                        n_Legendre_Terms = n_Legendre_Terms, &
!                        n_Phase_Elements = n_Phase_Elements, &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Aerosol_Model:     Name of the aerosol scheme for scattering calculation
!                          Available aerosol scheme:
!                          - CRTM  [DEFAULT]
!                          - CMAQ
!                          - GOCART-GEOS5
!                          - NAAPS
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Filename:          Character string specifying the name of a
!                          AerosolCoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          AerosolCoeff datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_Wavelengths:     The number of wavelengths in the look-up
!                          table (LUT). Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Radii:           The number of discrete effective radii for
!                          scatterers in the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Types:           The number of different aerosol types in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_RH:              The number of relative humidity entries in
!                          the LUT. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_RH_Radii:        The number of relative humidity entries in
!                          the LUT.
!                          used only for RH in CRTM and CMAQ tables. Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT. Can be = 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          Must be > 0.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the AerosolCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the AerosolCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the AerosolCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the AerosolCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the AerosolCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
! FUNCTION RESULT:
!       Error_Status:      The return value is an integer defining the error status.
!                          The error codes are defined in the Message_Handler module.
!                          If == SUCCESS, the file inquire was successful
!                             == FAILURE, an unrecoverable error occurred.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION AerosolCoeff_InquireFile( &
    Aerosol_Model   , &  ! Input
    Filename        , &  ! Input
    netCDF          , &  ! Optional input
    n_Wavelengths   , &  ! Optional output
    n_Radii         , &  ! Optional output
    n_Sigma         , &  ! Optional output
    n_Types         , &  ! Optional output
    n_RH            , &  ! Optional output
    n_RH_Radii      , &  ! Optional output
    n_Legendre_Terms, &  ! Optional output
    n_Phase_Elements, &  ! Optional output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Aerosol_Model
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Wavelengths
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Radii
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Sigma
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Types
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_RH
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_RH_Radii
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Legendre_Terms
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Phase_Elements
    INTEGER,      OPTIONAL, INTENT(OUT) :: Release
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF


    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = AerosolCoeff_Binary_InquireFile( &
                   Aerosol_Model, &
                   Filename, &
                   n_Wavelengths    = n_Wavelengths   , &
                   n_Radii          = n_Radii         , &
                   n_Sigma          = n_Sigma         , &
                   n_Types          = n_Types         , &
                   n_RH             = n_RH            , &
                   n_RH_Radii       = n_RH_Radii      , &
                   n_Legendre_Terms = n_Legendre_Terms, &
                   n_Phase_Elements = n_Phase_Elements, &
                   Release          = Release         , &
                   Version          = Version           )
    ELSE
      err_stat = AerosolCoeff_netCDF_InquireFile( &
                   Aerosol_Model, &
                   Filename, &
                   n_Wavelengths    = n_Wavelengths   , &
                   n_Radii          = n_Radii         , &
                   n_Sigma          = n_Sigma         , &
                   n_Types          = n_Types         , &
                   n_RH             = n_RH            , &
                   n_RH_Radii       = n_RH_Radii      , &
                   n_Legendre_Terms = n_Legendre_Terms, &
                   n_Phase_Elements = n_Phase_Elements, &
                   Release          = Release         , &
                   Version          = Version         , &
                   Title            = Title           , &
                   History          = History         , &
                   Comment          = Comment           )
    END IF

  END FUNCTION AerosolCoeff_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_ReadFile
!
! PURPOSE:
!       Function to read AerosolCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_ReadFile( &
!                        Aerosol_Model      , &
!                        Filename           , &
!                        AerosolCoeff       , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Aerosol_Model:     Name of the aerosol scheme for scattering calculation
!                          Available aerosol scheme:
!                          - CRTM  [DEFAULT]
!                          - CMAQ
!                          - GOCART-GEOS5
!                          - NAAPS
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Filename:       Character string specifying the name of the
!                       AerosolCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       AerosolCoeff:   Object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       AerosolCoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
! OPTIONAL OUTPUTS:
!       Title:          Character string written into the TITLE global
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
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

  FUNCTION AerosolCoeff_ReadFile( &
    Aerosol_Model, & ! Input
    Filename    , &  ! Input
    AerosolCoeff, &  ! Output
    netCDF      , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional output
    History     , &  ! Optional output
    Comment     ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN)  :: Aerosol_Model
    CHARACTER(*),            INTENT(IN)  :: Filename
    TYPE(AerosolCoeff_type), INTENT(OUT) :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,       OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Function variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = AerosolCoeff_Binary_ReadFile( &
                   Aerosol_Model, &
                   Filename, &
                   AerosolCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = AerosolCoeff_netCDF_ReadFile( &
                   Aerosol_Model, &
                   Filename, &
                   AerosolCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION AerosolCoeff_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_WriteFile
!
! PURPOSE:
!       Function to write AerosolCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_WriteFile( &
!                        Aerosol_Model, &
!                        Filename, &
!                        AerosolCoeff, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Aerosol_Model:     Name of the aerosol scheme for scattering calculation
!                          Available aerosol scheme:
!                          - CRTM  [DEFAULT]
!                          - CMAQ
!                          - GOCART-GEOS5
!                          - NAAPS
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
!       Filename:       Character string specifying the name of the
!                       AerosolCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       AerosolCoeff:   Object containing the aerosol coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(AerosolCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       AerosolCoeff datafiles.
!                       If == .FALSE., file format is BINARY [DEFAULT].
!                          == .TRUE.,  file format is NETCDF.
!                       If not specified, default is .FALSE.
!                       UNITS:      N/A
!                       TYPE:       LOGICAL
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
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
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the AerosolCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
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

  FUNCTION AerosolCoeff_WriteFile( &
    Aerosol_Model, & ! Input
    Filename    , &  ! Input
    AerosolCoeff, &  ! Input
    netCDF      , &  ! Optional input
    Quiet       , &  ! Optional input
    Title       , &  ! Optional input
    History     , &  ! Optional input
    Comment     ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),            INTENT(IN) :: Aerosol_Model
    CHARACTER(*),            INTENT(IN) :: Filename
    TYPE(AerosolCoeff_type), INTENT(IN) :: AerosolCoeff
    LOGICAL,       OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,       OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: History
    CHARACTER(*),  OPTIONAL, INTENT(IN) :: Comment
    ! Function result
    INTEGER :: err_stat
    ! Local variables
    LOGICAL :: Binary

    ! Set up
    err_stat = SUCCESS
    ! ...Check netCDF argument
    Binary = .TRUE.
    IF ( PRESENT(netCDF) ) Binary = .NOT. netCDF

    ! Call the appropriate function
    IF ( Binary ) THEN
      err_stat = AerosolCoeff_Binary_WriteFile( &
                   Aerosol_Model, &
                   Filename, &
                   AerosolCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = AerosolCoeff_netCDF_WriteFile( &
                   Aerosol_Model, &
                   Filename, &
                   AerosolCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION AerosolCoeff_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF AerosolCoeff file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = AerosolCoeff_netCDF_to_Binary( &
!                        Aerosol_Model, &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format AerosolCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format AerosolCoeff data file to write.
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

  FUNCTION AerosolCoeff_netCDF_to_Binary( &
    Aerosol_Model, & ! Input
    NC_Filename , &  ! Input
    BIN_Filename, &  ! Input
    Quiet       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: Aerosol_Model
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AerosolCoeff_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(AerosolCoeff_type) :: cc, cc_copy

    ! Set up
    err_stat = SUCCESS

    ! Read the netCDF file
    err_stat = AerosolCoeff_ReadFile( Aerosol_Model, NC_Filename, cc, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the Binary file
    err_stat = AerosolCoeff_WriteFile( Aerosol_Model, BIN_Filename, cc, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the Binary file
    err_stat = AerosolCoeff_ReadFile( Aerosol_Model, BIN_Filename, cc_copy, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the AerosolCoeff objects
    IF ( .NOT. (cc == cc_copy) ) THEN
      msg = 'AerosolCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION AerosolCoeff_netCDF_to_Binary


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       AerosolCoeff_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL AerosolCoeff_IOVersion( Id )
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

 ! SUBROUTINE AerosolCoeff_IOVersion( Id )
 !   CHARACTER(*), INTENT(OUT) :: Id
 !   INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
 !   INTEGER, PARAMETER :: LINEFEED = 10
 !   INTEGER, PARAMETER :: SL = 256
 !   CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
 !   CHARACTER(SL*3) :: IO_Id
 !   CALL AerosolCoeff_Binary_IOVersion( Binary_IO_Id )
 !   CALL AerosolCoeff_netCDF_IOVersion( netCDF_IO_Id )
 !   IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
 !           '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
 !           '  '//TRIM(netCDF_IO_Id)
 !   IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
 !     Id = IO_Id
 !   ELSE
 !     Id = MODULE_VERSION_ID
 !   END IF
 ! END SUBROUTINE AerosolCoeff_IOVersion

END MODULE AerosolCoeff_IO
