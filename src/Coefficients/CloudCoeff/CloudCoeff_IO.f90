!
! CloudCoeff_IO
!
! Container module for Binary and netCDF CloudCoeff I/O modules.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 24-Feb-2010
!                       paul.vandelst@noaa.gov
!

MODULE CloudCoeff_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds          , ONLY: fp
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility        , ONLY: File_Exists
  USE CloudCoeff_Define   , ONLY: CloudCoeff_type, OPERATOR(==)
  USE CloudCoeff_Binary_IO, ONLY: CloudCoeff_Binary_InquireFile, &
                                  CloudCoeff_Binary_ReadFile   , &
                                  CloudCoeff_Binary_WriteFile  , &
                                  CloudCoeff_Binary_IOVersion
  USE CloudCoeff_netCDF_IO, ONLY: CloudCoeff_netCDF_InquireFile, &
                                  CloudCoeff_netCDF_ReadFile   , &
                                  CloudCoeff_netCDF_WriteFile  , &
                                  CloudCoeff_netCDF_IOVersion
  ! Disable implicit typing
  IMPLICIT NONE
  
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CloudCoeff_InquireFile
  PUBLIC :: CloudCoeff_ReadFile
  PUBLIC :: CloudCoeff_WriteFile
  PUBLIC :: CloudCoeff_netCDF_to_Binary
  PUBLIC :: CloudCoeff_Binary_to_netCDF
  PUBLIC :: CloudCoeff_IOVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_VERSION_ID = &
  

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
!       CloudCoeff_InquireFile
!
! PURPOSE:
!       Function to inquire CloudCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_InquireFile( &
!                        Filename, &
!                        netCDF           = netCDF          , &
!                        n_MW_Frequencies = n_MW_Frequencies, &
!                        n_MW_Radii       = n_MW_Radii      , &
!                        n_IR_Frequencies = n_IR_Frequencies, &
!                        n_IR_Radii       = n_IR_Radii      , &
!                        n_Temperatures   = n_Temperatures  , &
!                        n_Densities      = n_Densities     , &
!                        n_Legendre_Terms = n_Legendre_Terms, &
!                        n_Phase_Elements = n_Phase_Elements, &
!                        Release          = Release         , &
!                        Version          = Version         , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! INPUTS:
!       Filename:          Character string specifying the name of a
!                          CloudCoeff data file to read.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:            Set this logical argument to access netCDF format
!                          CloudCoeff datafiles.
!                          If == .FALSE., file format is BINARY [DEFAULT].
!                             == .TRUE.,  file format is NETCDF.
!                          If not specified, default is .FALSE.
!                          UNITS:      N/A
!                          TYPE:       LOGICAL
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       n_MW_Frequencies:  The number of microwave frequencies in
!                          the look-up table (LUT)
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_MW_Radii:        The number of discrete effective radii 
!                          for MW scatterers in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Frequencies:  The number of infrared frequencies in
!                          the LUT 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_IR_Radii:        The number of discrete effective radii 
!                          for IR scatterers in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Temperatures:    The number of discrete layer temperatures
!                          in the LUT. 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Densities:       The number of fixed densities for snow, graupel,
!                          and hail/ice in the LUT. 
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Legendre_Terms:  The maximum number of Legendre polynomial
!                          terms in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Phase_Elements:  The maximum number of phase elements in the LUT.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:           The release number of the CloudCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:           The version number of the CloudCoeff file.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:             Character string written into the TITLE global
!                          attribute field of the CloudCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:           Character string written into the HISTORY global
!                          attribute field of the CloudCoeff file.
!                          This argument is ignored if the netCDF argument
!                          is not supplied or set.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER(*)
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:           Character string written into the COMMENT global
!                          attribute field of the CloudCoeff file.
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

  FUNCTION CloudCoeff_InquireFile( &
    Filename        , &  ! Input
    netCDF          , &  ! Optional input
    n_MW_Frequencies, &  ! Optional Output
    n_MW_Radii      , &  ! Optional Output
    n_IR_Frequencies, &  ! Optional Output
    n_IR_Radii      , &  ! Optional Output
    n_Temperatures  , &  ! Optional Output
    n_Densities     , &  ! Optional Output
    n_Legendre_Terms, &  ! Optional Output
    n_Phase_Elements, &  ! Optional Output
    Release         , &  ! Optional output
    Version         , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Frequencies    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_MW_Radii          
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Frequencies    
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_IR_Radii          
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Temperatures      
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Densities         
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
      err_stat = CloudCoeff_Binary_InquireFile( &
                   Filename, &
                   n_MW_Frequencies = n_MW_Frequencies, &
                   n_MW_Radii       = n_MW_Radii      , &
                   n_IR_Frequencies = n_IR_Frequencies, &
                   n_IR_Radii       = n_IR_Radii      , &
                   n_Temperatures   = n_Temperatures  , &
                   n_Densities      = n_Densities     , &
                   n_Legendre_Terms = n_Legendre_Terms, &
                   n_Phase_Elements = n_Phase_Elements, &
                   Release          = Release         , &
                   Version          = Version           )
    ELSE
      err_stat = CloudCoeff_netCDF_InquireFile( &
                   Filename, &
                   n_MW_Frequencies = n_MW_Frequencies, &
                   n_MW_Radii       = n_MW_Radii      , &
                   n_IR_Frequencies = n_IR_Frequencies, &
                   n_IR_Radii       = n_IR_Radii      , &
                   n_Temperatures   = n_Temperatures  , &
                   n_Densities      = n_Densities     , &
                   n_Legendre_Terms = n_Legendre_Terms, &
                   n_Phase_Elements = n_Phase_Elements, &
                   Release          = Release         , &
                   Version          = Version         , &
                   Title            = Title           , &
                   History          = History         , &
                   Comment          = Comment           )
    END IF

  END FUNCTION CloudCoeff_InquireFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_ReadFile
!
! PURPOSE:
!       Function to read CloudCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_ReadFile( &
!                        Filename         , &
!                        CloudCoeff       , &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CloudCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       CloudCoeff:     CloudCoeff object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       CloudCoeff datafiles.
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
!                       attribute field of the CloudCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CloudCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CloudCoeff file.
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
!                       If == SUCCESS the data read was successful
!                          == FAILURE an unrecoverable error occurred.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION CloudCoeff_ReadFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Output
    netCDF    , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional output
    History   , &  ! Optional output
    Comment   ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: Filename
    TYPE(CloudCoeff_type),  INTENT(OUT) :: CloudCoeff
    LOGICAL,      OPTIONAL, INTENT(IN)  :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN)  :: Quiet
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
      err_stat = CloudCoeff_Binary_ReadFile( &
                   Filename  , &
                   CloudCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = CloudCoeff_netCDF_ReadFile( &
                   Filename  , &
                   CloudCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION CloudCoeff_ReadFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_WriteFile
!
! PURPOSE:
!       Function to write CloudCoeff object files.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_WriteFile( &
!                        Filename  , &
!                        CloudCoeff, &
!                        netCDF  = netCDF , &
!                        Quiet   = Quiet  , &
!                        Title   = Title  , &
!                        History = History, &
!                        Comment = Comment  )
!
! INPUTS:
!       Filename:       Character string specifying the name of the
!                       CloudCoeff data file to write.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       CloudCoeff:     Object containing the cloud coefficient data.
!                       UNITS:      N/A
!                       TYPE:       TYPE(CloudCoeff_type)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       netCDF:         Set this logical argument to access netCDF format
!                       CloudCoeff datafiles.
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
!                       attribute field of the CloudCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:        Character string written into the HISTORY global
!                       attribute field of the CloudCoeff file.
!                       This argument is ignored if the netCDF argument
!                       is not supplied or set.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:        Character string written into the COMMENT global
!                       attribute field of the CloudCoeff file.
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

  FUNCTION CloudCoeff_WriteFile( &
    Filename  , &  ! Input
    CloudCoeff, &  ! Input
    netCDF    , &  ! Optional input
    Quiet     , &  ! Optional input
    Title     , &  ! Optional input
    History   , &  ! Optional input
    Comment   ) &  ! Optional input
  RESULT ( err_stat )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: Filename
    TYPE(CloudCoeff_type),  INTENT(IN) :: CloudCoeff
    LOGICAL,      OPTIONAL, INTENT(IN) :: netCDF
    LOGICAL,      OPTIONAL, INTENT(IN) :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
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
      err_stat = CloudCoeff_Binary_WriteFile( &
                   Filename  , &
                   CloudCoeff, &
                   Quiet = Quiet )
    ELSE
      err_stat = CloudCoeff_netCDF_WriteFile( &
                   Filename  , &
                   CloudCoeff, &
                   Quiet   = Quiet  , &
                   Title   = Title  , &
                   History = History, &
                   Comment = Comment  )
    END IF

  END FUNCTION CloudCoeff_WriteFile


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_netCDF_to_Binary
!
! PURPOSE:
!       Function to convert a netCDF CloudCoeff file to Binary format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_netCDF_to_Binary( &
!                        NC_Filename  , &
!                        BIN_Filename , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format CloudCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format CloudCoeff data file to write.
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

  FUNCTION CloudCoeff_netCDF_to_Binary( &
    NC_Filename , &  ! Input
    BIN_Filename, &  ! Input
    Quiet       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_netCDF_to_Binary'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(CloudCoeff_type) :: cc, cc_copy
    
    ! Set up
    err_stat = SUCCESS

    ! Read the netCDF file
    err_stat = CloudCoeff_ReadFile( NC_Filename, cc, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the Binary file
    err_stat = CloudCoeff_WriteFile( BIN_Filename, cc, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the Binary file
    err_stat = CloudCoeff_ReadFile( BIN_Filename, cc_copy, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the CloudCoeff objects
    IF ( .NOT. (cc == cc_copy) ) THEN
      msg = 'CloudCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION CloudCoeff_netCDF_to_Binary


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_Binary_to_netCDF
!
! PURPOSE:
!       Function to convert a Binary CloudCoeff file to netCDF format.
!
! CALLING SEQUENCE:
!       Error_Status = CloudCoeff_Binary_to_netCDF( &
!                        BIN_Filename , &
!                        NC_Filename  , &
!                        Quiet = Quiet  )
!
! INPUTS:
!       BIN_Filename:   Character string specifying the name of the
!                       Binary format CloudCoeff data file to read.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Scalar
!                       ATTRIBUTES: INTENT(IN)
!
!       NC_Filename:    Character string specifying the name of the
!                       netCDF format CloudCoeff data file to write.
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

  FUNCTION CloudCoeff_Binary_to_netCDF( &
    BIN_Filename, &  ! Input
    NC_Filename , &  ! Input
    Quiet       ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    CHARACTER(*),      INTENT(IN)  :: BIN_Filename
    CHARACTER(*),      INTENT(IN)  :: NC_Filename
    LOGICAL, OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CloudCoeff_Binary_to_netCDF'
    ! Function variables
    CHARACTER(256) :: msg
    TYPE(CloudCoeff_type) :: cc, cc_copy
    
    ! Set up
    err_stat = SUCCESS

    ! Read the Binary file
    err_stat = CloudCoeff_ReadFile( BIN_Filename, cc, Quiet = Quiet )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading Binary file '//TRIM(BIN_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Write the netCDF file
    err_stat = CloudCoeff_WriteFile( NC_Filename, cc, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing netCDF file '//TRIM(NC_Filename)
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

    ! Check the write was successful
    ! ...Read the netCDF file
    err_stat = CloudCoeff_ReadFile( NC_Filename, cc_copy, Quiet = Quiet, netCDF = .TRUE. )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading netCDF file '//TRIM(NC_Filename)//' for test'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF
    ! ...Compare the CloudCoeff objects
    IF ( .NOT. (cc == cc_copy) ) THEN
      msg = 'CloudCoeff object comparison failed.'
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
      RETURN
    END IF

  END FUNCTION CloudCoeff_Binary_to_netCDF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       CloudCoeff_IOVersion
!
! PURPOSE:
!       Subroutine to return the version information for the I/O modules.
!
! CALLING SEQUENCE:
!       CALL CloudCoeff_IOVersion( Id )
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

  SUBROUTINE CloudCoeff_IOVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    INTEGER, PARAMETER :: SL = 256
    CHARACTER(SL)   :: Binary_IO_Id, netCDF_IO_Id
    CHARACTER(SL*3) :: IO_Id
    CALL CloudCoeff_Binary_IOVersion( Binary_IO_Id )
    CALL CloudCoeff_netCDF_IOVersion( netCDF_IO_Id )
    IO_Id = MODULE_VERSION_ID//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(Binary_IO_Id)//';'//ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED)//&
            '  '//TRIM(netCDF_IO_Id)
    IF ( LEN_TRIM(IO_Id) <= LEN(Id) ) THEN
      Id = IO_Id
    ELSE
      Id = MODULE_VERSION_ID
    END IF
  END SUBROUTINE CloudCoeff_IOVersion

END MODULE CloudCoeff_IO
