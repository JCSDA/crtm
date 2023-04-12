!
! ODAS_netCDF_IO
!
! Module containing routines to create, inquire, read and write netCDF
! format ODAS files.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 05-Jun-2008
!                       paul.vandelst@noaa.gov
!

MODULE ODAS_netCDF_IO

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE Type_Kinds     , ONLY: Long, Double
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE ODAS_Define    , ONLY: INVALID_WMO_SATELLITE_ID, &
                             INVALID_WMO_SENSOR_ID   , &
                             INVALID_SENSOR          , &
                             ODAS_Type               , &
                             Associated_ODAS         , &
                             Allocate_ODAS           , &
                             Destroy_ODAS            , &
                             CheckRelease_ODAS       , &
                             Info_ODAS 
  USE netcdf
  USE netCDF_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Inquire_ODAS_netCDF
  PUBLIC :: Write_ODAS_netCDF
  PUBLIC :: Read_ODAS_netCDF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = 'Placeholder'
  ! Numeric literals
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Default message string length
  INTEGER, PARAMETER :: ML = 256

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment'
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: ALGORITHM_GATTNAME        = 'Algorithm'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  CHARACTER(*), PARAMETER :: PROFILE_SET_ID_GATTNAME   = 'Profile_Set_Id' 

  ! Dimension names
  CHARACTER(*), PARAMETER :: PREDICTOR_DIMNAME  = 'n_Predictors'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME   = 'n_Absorbers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME    = 'n_Channels'
  CHARACTER(*), PARAMETER :: ALPHA_DIMNAME      = 'n_Alphas'
  CHARACTER(*), PARAMETER :: COEFF_DIMNAME      = 'n_Coeffs'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_VARNAME       = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME    = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_VARNAME       = 'Absorber_ID'
  CHARACTER(*), PARAMETER :: MAX_ORDER_VARNAME         = 'Maximum_Order'
  CHARACTER(*), PARAMETER :: ALPHA_VARNAME             = 'Alpha'
  CHARACTER(*), PARAMETER :: ORDER_VARNAME             = 'Order'
  CHARACTER(*), PARAMETER :: PRE_INDEX_VARNAME         = 'Predictor_Index'
  CHARACTER(*), PARAMETER :: POS_INDEX_VARNAME         = 'Position_Index'
  CHARACTER(*), PARAMETER :: COEFF_VARNAME             = 'Tau_Coefficients'

  ! Description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_DESCRIPTION      = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION   = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_DESCRIPTION      = 'List of absorber ID values'
  CHARACTER(*), PARAMETER :: MAX_ORDER_DESCRIPTION        = 'Maximum order of the polynomial functions for all absorbers.'
  CHARACTER(*), PARAMETER :: ALPHA_DESCRIPTION            = 'Alpha values used to generate the absorber space levels.'
  CHARACTER(*), PARAMETER :: ORDER_DESCRIPTION            = 'List of polynomial orders.'
  CHARACTER(*), PARAMETER :: PRE_INDEX_DESCRIPTION        = 'List of predictors.'
  CHARACTER(*), PARAMETER :: POS_INDEX_DESCRIPTION        = 'List of starting position indexes for the tau coeff. '//&
                                                            'array, given abosrber and channel .'
  CHARACTER(*), PARAMETER :: COEFF_DESCRIPTION            = 'Regression model gas absorption coefficients. '//&
   'The 1D array data structure is give by ps, np, no, j and l, where ps = Pos_Index(j,l), '//&
   'np = Pre_Index(0,j,l), no = Order(j,l), j = absorber index and l = channel index. '//&
   'The set of coeffs for given j and l in the array starts at ps and ends at ps+(np+1)*(no+1)-1. '//&
   'The structure of this subset is arranged into np+1 consecutive equal segments used to compute '//&
   'the np+1 B coefficients, respectively, with a segment size no+1.' 

  ! Long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_LONGNAME      = 'Sensor Type'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME   = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_LONGNAME      = 'Absorber Id'
  CHARACTER(*), PARAMETER :: MAX_ORDER_LONGNAME        = 'Maximum polynomial order'
  CHARACTER(*), PARAMETER :: ALPHA_LONGNAME            = 'Alpha coefficients'
  CHARACTER(*), PARAMETER :: ORDER_LONGNAME            = 'Polynomial Order'
  CHARACTER(*), PARAMETER :: PRE_INDEX_LONGNAME        = 'Predictor Index'
  CHARACTER(*), PARAMETER :: POS_INDEX_LONGNAME        = 'Coefficient array starting position indices'
  CHARACTER(*), PARAMETER :: COEFF_LONGNAME            = 'Regression coefficients'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: SENSOR_TYPE_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS   = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: MAX_ORDER_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: ALPHA_UNITS            = 'Absorber dependent.'
  CHARACTER(*), PARAMETER :: ORDER_UNITS            = 'N/A'
  CHARACTER(*), PARAMETER :: PRE_INDEX_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: POS_INDEX_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: COEFF_UNITS            = 'Absorber and predictor dependent.'

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'

  INTEGER(Long), PARAMETER :: SENSOR_TYPE_FILLVALUE      = INVALID_SENSOR
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE   = 0
  INTEGER(Long), PARAMETER :: ABSORBER_ID_FILLVALUE      = 0
  INTEGER(Long), PARAMETER :: MAX_ORDER_FILLVALUE        = -1
  REAL(Double),  PARAMETER :: ALPHA_FILLVALUE            = ZERO
  INTEGER(Long), PARAMETER :: ORDER_FILLVALUE            = -1
  INTEGER(Long), PARAMETER :: PRE_INDEX_FILLVALUE        = -1
  INTEGER(Long), PARAMETER :: POS_INDEX_FILLVALUE        = -1
  REAL(Double),  PARAMETER :: COEFF_FILLVALUE            = ZERO


  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: SENSOR_TYPE_TYPE      = NF90_INT
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE   = NF90_INT
  INTEGER, PARAMETER :: ABSORBER_ID_TYPE      = NF90_INT
  INTEGER, PARAMETER :: MAX_ORDER_TYPE        = NF90_INT
  INTEGER, PARAMETER :: ALPHA_TYPE            = NF90_DOUBLE
  INTEGER, PARAMETER :: ORDER_TYPE            = NF90_INT
  INTEGER, PARAMETER :: PRE_INDEX_TYPE        = NF90_INT
  INTEGER, PARAMETER :: POS_INDEX_TYPE        = NF90_INT
  INTEGER, PARAMETER :: COEFF_TYPE            = NF90_DOUBLE


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       Inquire_ODAS_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF ODAS format file to obtain the
!       dimension values and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ODAS_netCDF( NC_Filename                        , &  ! Input
!                                           n_Predictors     = n_Predictors    , &  ! Optional output
!                                           n_Absorbers      = n_Absorbers     , &  ! Optional output
!                                           n_Channels       = n_Channels      , &  ! Optional output
!                                           n_Alphas         = n_Alphas        , &  ! Optional output
!                                           n_Coeffs         = n_Coeffs        , &  ! Optional output
!                                           Release          = Release         , &  ! Optional Output
!                                           Version          = Version         , &  ! Optional Output
!                                           Sensor_Id        = Sensor_Id       , &  ! Optional output
!                                           WMO_Satellite_Id = WMO_Satellite_Id, &  ! Optional output
!                                           WMO_Sensor_Id    = WMO_Sensor_Id   , &  ! Optional output
!                                           Title            = Title           , &  ! Optional output
!                                           History          = History         , &  ! Optional output
!                                           Comment          = Comment         , &  ! Optional output
!                                           Profile_Set_Id   = Profile_Set_Id  , &  ! Optional output
!                                           RCS_Id           = RCS_Id          , &  ! Revision control
!                                           Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the netCDF ODAS
!                           format data file. Used only for message output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!
!       n_Predictors:       The number of predictor functions used in generating
!                           the ODAS data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Alphas:           The number of alpha coefficients used to compute the absorber level.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Coeffs:           The number of the C coeffcients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The ODAS data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF ODAS file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:     Character string written into the PROFILE_SET_ID
!                           global attribute field of the netCDF ODAS file.
!                           Should contain a short tag used to identify the
!                           dependent profile set used to generate the 
!                           coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file inquiry was successful
!                              == FAILURE an error occurred reading any of the
!                                         requested data.
!                              == WARNING an error occurred closing the netCDF
!                                         file after a successful read.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Inquire_ODAS_netCDF( NC_Filename     , &  ! Input
                                n_Predictors    , &  ! Optional output
                                n_Absorbers     , &  ! Optional output
                                n_Channels      , &  ! Optional output
                                n_Alphas        , &  ! Optional output  
                                n_Coeffs        , &  ! Optional output  
                                Release         , &  ! Optional Output
                                Version         , &  ! Optional Output
                                Sensor_Id       , &  ! Optional output
                                WMO_Satellite_Id, &  ! Optional output
                                WMO_Sensor_Id   , &  ! Optional output
                                Title           , &  ! Optional output
                                History         , &  ! Optional output
                                Comment         , &  ! Optional output
                                Profile_Set_Id  , &  ! Optional output
                                RCS_Id          , &  ! Revision control
                                Message_Log     ) &  ! Error messaging
                              RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Predictors     
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers      
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels       
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Alphas
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Coeffs
    INTEGER     , OPTIONAL, INTENT(OUT) :: Release          
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version          
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id        
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id 
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title            
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History          
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment          
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id           
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log      
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ODAS_netCDF'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    TYPE(ODAS_type) :: ODAS  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT( RCS_Id ) ) RCS_Id = MODULE_RCS_ID


    ! Open the file
    ! -------------
    Error_Status = Open_netCDF( TRIM(NC_Filename), &
                                NC_FileID, &
                                Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF ODAS data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Get the dimensions
    ! ------------------
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         PREDICTOR_DIMNAME, &
                                         ODAS%n_Predictors, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//PREDICTOR_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ABSORBER_DIMNAME, &
                                         ODAS%n_Absorbers, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ABSORBER_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         ODAS%n_Channels, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ALPHA_DIMNAME, &
                                         ODAS%n_Alphas, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ALPHA_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         COEFF_DIMNAME, &
                                         ODAS%n_Coeffs, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//COEFF_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                            , &
                              NC_FileID                              , &
                              Version          =ODAS%Version         , &
                              Sensor_Id        =ODAS%Sensor_Id       , &
                              WMO_Satellite_Id =ODAS%WMO_Satellite_Id, &
                              WMO_Sensor_Id    =ODAS%WMO_Sensor_Id   , &
                              Title            =Title                , &
                              History          =History              , &
                              Comment          =Comment              , &
                              Profile_Set_Id   =Profile_Set_Id       , &
                              Message_Log      =Message_Log            )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Message = 'Error closing netCDF ODAS data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    ! ---------------------
    ! Dimensions
    IF ( PRESENT(n_Predictors) ) n_Predictors = ODAS%n_Predictors-1
    IF ( PRESENT(n_Absorbers ) ) n_Absorbers  = ODAS%n_Absorbers    
    IF ( PRESENT(n_Channels  ) ) n_Channels   = ODAS%n_Channels  
    IF ( PRESENT(n_Alphas  ) )   n_Alphas     = ODAS%n_Alphas  
    IF ( PRESENT(n_Coeffs  ) )   n_Coeffs     = ODAS%n_Coeffs  
    
    ! Release/Version information
    IF ( PRESENT(Release) ) Release = ODAS%Release
    IF ( PRESENT(Version) ) Version = ODAS%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = ODAS%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(ODAS%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = ODAS%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = ODAS%WMO_Sensor_Id   
    
  CONTAINS
  
    SUBROUTINE Inquire_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Inquire_CleanUp

  END FUNCTION Inquire_ODAS_netCDF


!--------------------------------------------------------------------------------
!
! NAME:
!       Write_ODAS_netCDF
!
! PURPOSE:
!       Function to write an ODAS structure to a netCDF format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ODAS_netCDF( NC_Filename                      , &  ! Input
!                                         ODAS                             , &  ! Input
!                                         Title          = Title           , &  ! Optional output
!                                         History        = History         , &  ! Optional output
!                                         Comment        = Comment         , &  ! Optional output
!                                         Profile_Set_Id = Profile_Set_Id  , &  ! Optional output
!                                         Quiet          = Quiet           , &  ! Optional input
!                                         RCS_Id         = RCS_Id          , &  ! Revision control
!                                         Message_Log    = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the output
!                        netCDF ODAS format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ODAS:            Structure to write to file.
!                        UNITS:      N/A
!                        TYPE:       ODAS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ODAS file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ODAS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ODAS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_ID:  Character string written into the PROFILE_SET_ID
!                        global attribute field of the netCDF ODAS file.
!                        Should contain a short tag used to identify the
!                        dependent profile set used to generate the 
!                        coefficient data.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file write was successful
!                           == FAILURE - the input ODAS structure contains
!                                        unassociated pointer members, or
!                                      - a unrecoverable write error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the output file exists, it is overwritten.
!
!--------------------------------------------------------------------------------

  FUNCTION Write_ODAS_netCDF( NC_Filename   , &  ! Input
                              ODAS          , &  ! Input
                              Title         , &  ! Optional input
                              History       , &  ! Optional input
                              Comment       , &  ! Optional input
                              Profile_Set_Id, &  ! Optional input
                              Quiet         , &  ! Optional input
                              RCS_Id        , &  ! Revision control
                              Message_Log   ) &  ! Error messaging
                            RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Profile_Set_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODAS_netCDF'
    ! Local variables
    CHARACTER(ML) :: Message
    LOGICAL :: Noisy
    INTEGER :: NC_FileID
    INTEGER :: Close_Status

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT(Quiet) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Check structure association
    IF ( .NOT. Associated_ODAS( ODAS ) ) THEN
      Message = 'Some or all INPUT ODAS pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Create the output data file
    ! ---------------------------
    Error_Status = CreateFile( NC_Filename                           , &  ! Input
                               ODAS%n_Predictors                     , &  ! Input
                               ODAS%n_Absorbers                      , &  ! Input
                               ODAS%n_Channels                       , &  ! Input
                               ODAS%n_Alphas                         , &  ! Input
                               ODAS%n_Coeffs                         , &  ! Input
                               NC_FileID                             , &  ! Output
                               Version         =ODAS%Version         , &  ! Optional input
                               Sensor_Id       =ODAS%Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id=ODAS%WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id   =ODAS%WMO_Sensor_Id   , &  ! Optional input
                               Title           =Title                , &  ! Optional input
                               History         =History              , &  ! Optional input
                               Comment         =Comment              , &  ! Optional input
                               Profile_Set_Id  =Profile_Set_Id       , &  ! Optional input
                               Message_Log     =Message_Log            )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF



    ! Write the ODAS data
    ! -------------------
    Error_Status = WriteVar( NC_Filename, &
                             NC_FileID  , &
                             ODAS       , &
                             Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing variables to output file '//TRIM(NC_Filename)
      CALL Write_Cleanup( Close_File=SET ); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ODAS data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODAS( ODAS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Write_CleanUp( Close_File )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Write_CleanUp

  END FUNCTION Write_ODAS_netCDF


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_ODAS_netCDF
!
! PURPOSE:
!       Function to read data into an ODAS structure from a netCDF format file.
!
! CALLING SEQUENCE:
!         Error_Status = Read_ODAS_netCDF( NC_Filename                    , &  ! Input
!                                          ODAS                           , &  ! Output
!                                          Quiet          = Quiet         , &  ! Optional input
!                                          Title          = Title         , &  ! Optional output
!                                          History        = History       , &  ! Optional output
!                                          Comment        = Comment       , &  ! Optional output
!                                          Profile_Set_Id = Profile_Set_Id, &  ! Optional output
!                                          RCS_Id         = RCS_Id        , &  ! Revision control
!                                          Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF ODAS
!                        format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS:            Structure to contain the gas absorption coefficient
!                        data read from the file.
!                        UNITS:      N/A
!                        TYPE:       ODAS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:           Set this keyword to suppress information messages being
!                        printed to standard output (or the message log file if
!                        the MESSAGE_LOG optional argument is used.) By default,
!                        information messages are printed.
!                        If QUIET = 0, information messages are OUTPUT.
!                           QUIET = 1, information messages are SUPPRESSED.
!                        UNITS:      N/A
!                        TYPE:       Integer
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to standard output.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ODAS file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ODAS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ODAS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:  Character string written into the PROFILE_SET_ID
!                        global attribute field of the netCDF ODAS file.
!                        Should contain a short tag used to identify the
!                        dependent profile set used to generate the 
!                        coefficient data.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       RCS_Id:          Character string containing the Revision Control
!                        System Id field for the module.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF file read was successful
!                           == FAILURE an unrecoverable read error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       If the ODAS argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output ODAS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Read_ODAS_netCDF( NC_Filename   , &  ! Input
                             ODAS          , &  ! Output
                             Quiet         , &  ! Optional input
                             Title         , &  ! Optional output
                             History       , &  ! Optional output
                             Comment       , &  ! Optional output
                             Profile_Set_Id, &  ! Optional output
                             RCS_Id        , &  ! Revision control
                             Message_Log   ) &  ! Error messaging
                           RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    INTEGER     , OPTIONAL, INTENT(IN)     :: Quiet
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Title         
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: History       
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Comment       
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODAS_netCDF'
    ! Function variables
    CHARACTER(1000)  :: Message
    LOGICAL :: Noisy
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Predictors
    INTEGER :: n_Absorbers
    INTEGER :: n_Channels
    INTEGER :: n_Alphas
    INTEGER :: n_Coeffs

    ! Set up
    ! ------
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Output informational messages....
    Noisy = .TRUE.
    ! ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF

    ! Allocate the structure for the netCDF read
    ! ------------------------------------------
    ! Read the dimension values
    Error_Status = Inquire_ODAS_netCDF( NC_Filename, &
                                        n_Predictors = n_Predictors  , &
                                        n_Absorbers  = n_Absorbers   , &
                                        n_Channels   = n_Channels    , &
                                        n_Alphas     = n_Alphas      , &
                                        n_Coeffs     = n_Coeffs      , &
                                        Message_Log  = Message_Log     )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error inquiring the file '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_ODAS( n_Predictors, &
                                  n_Absorbers , &
                                  n_Channels  , &
                                  n_Alphas    , &
                                  n_Coeffs    , &
                                  ODAS        , &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating ODAS structure.'
      CALL Read_Cleanup(); RETURN
    END IF


    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_netCDF( NC_Filename, &
                                NC_FileID, &
                                Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF ODAS data file '//TRIM(NC_Filename)
      CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
    END IF


    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename                             , &
                              NC_FileID                               , &
                              Version          = ODAS%Version         , &
                              Sensor_Id        = ODAS%Sensor_Id       , &
                              WMO_Satellite_Id = ODAS%WMO_Satellite_Id, &
                              WMO_Sensor_Id    = ODAS%WMO_Sensor_Id   , &
                              Title            = Title                , &
                              History          = History              , &
                              Comment          = Comment              , &
                              Profile_Set_Id   = Profile_Set_Id       , &
                              Message_Log      = Message_Log            )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Read the ODAS data
    ! ------------------
    Error_Status = ReadVar( NC_Filename, &
                            NC_FileID  , &
                            ODAS       , &
                            Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading ODAS variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ODAS data file '//TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODAS( ODAS, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(Message), &
                            INFORMATION, &
                            Message_Log=Message_Log )
    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File, Destroy_Structure )
      INTEGER, OPTIONAL, INTENT(IN) :: Close_File
      INTEGER, OPTIONAL, INTENT(IN) :: Destroy_Structure
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File == SET ) THEN
          Close_Status = Close_netCDF(NC_FileID)
          IF ( Close_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error closing input file during error cleanup.'
        END IF
      END IF
      ! Destroy the structure if necessary
      IF ( PRESENT(Destroy_Structure) ) THEN
        IF ( Destroy_Structure == SET ) THEN
          Destroy_Status = Destroy_ODAS(ODAS, Message_Log=Message_Log)
          IF ( Destroy_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error destroying ODAS structure during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ODAS_netCDF


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!       CreateFile
!
! PURPOSE:
!       Function to create a netCDF ODAS data file for writing.
!
! CALLING SEQUENCE:
!       Error_Status = CreateFile( NC_Filename                      , &  ! Input
!                                  n_Orders                         , &  ! Input
!                                  n_Predictors                     , &  ! Input
!                                  n_Absorbers                      , &  ! Input
!                                  n_Channels                       , &  ! Input
!                                  NC_FileID                        , &  ! Output
!                                  Version         =Version         , &  ! Optional input
!                                  Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                  Title           =Title           , &  ! Optional input
!                                  History         =History         , &  ! Optional input
!                                  Comment         =Comment         , &  ! Optional input
!                                  Profile_Set_Id  =Profile_Set_Id  , &  ! Optional input
!                                  Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:        Character string specifying the name of the
!                           netCDF ODAS format data file to create.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Predictors:       The number of predictor functions used in generating
!                           the ODAS data.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_Predictors, where the 0'th
!                                 term is the offset. Therefore the actual number
!                                 of array elements along this dimension is
!                                 n_Predictors+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers:        The number of absorbers dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels:         The number of channels dimension of the ODAS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Alphas:           The number of alpha coefficients used to compute the absorber level.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Coeffs:           The number of the C coeffcients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       NC_FileID:          NetCDF file ID number to be used for subsequent
!                           writing to the output file.
!                           UNITS:      N/A
!                           TYPE:       Integer
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:            The version number of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:          Character string sensor/platform identifier.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the netCDF ODAS file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ODAS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_ID:     Character string written into the PROFILE_SET_ID
!                           global attribute field of the netCDF ODAS file.
!                           Should contain a short tag used to identify the
!                           dependent profile set used to generate the 
!                           coefficient data.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the netCDF file creation was successful.
!                              == FAILURE an unrecoverable error occurred.
!                              == WARNING an error occurred writing any of the requested
!                                         global file attributes.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       - If the output data file already exists, it is overwritten.
!       - The created netCDF file is in DATA mode upon exiting this function.
!       - If a FAILURE error occurs, the created netCDF file is closed.
!
!--------------------------------------------------------------------------------

  FUNCTION CreateFile( NC_Filename     , &  ! Input
                       n_Predictors    , &  ! Input
                       n_Absorbers     , &  ! Input
                       n_Channels      , &  ! Input
                       n_Alphas        , &  ! Input
                       n_Coeffs        , &  ! Input
                       NC_FileID       , &  ! Output
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Profile_Set_Id  , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename     
    INTEGER               , INTENT(IN)  :: n_Predictors    
    INTEGER               , INTENT(IN)  :: n_Absorbers     
    INTEGER               , INTENT(IN)  :: n_Channels      
    INTEGER               , INTENT(IN)  :: n_Alphas        
    INTEGER               , INTENT(IN)  :: n_Coeffs        
    INTEGER               , INTENT(OUT) :: NC_FileID       
    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title           
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment         
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Profile_Set_Id  
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log     
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CreateFile'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: Define_Status
    INTEGER :: Write_Status
    INTEGER :: n_Predictors_DimID
    INTEGER :: n_Absorbers_DimID 
    INTEGER :: n_Channels_DimID  
    INTEGER :: n_Alphas_DimID    
    INTEGER :: n_Coeffs_DimID    

    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check dimensions
    IF ( n_Predictors < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1 .OR. &
         n_Alphas     < 1 .OR. &
         n_Coeffs     < 1 ) THEN
      Message = 'Invalid dimension input detected.'
      CALL Create_Cleanup(); RETURN
    END IF

    ! Create the data file
    ! --------------------
    NF90_Status = NF90_CREATE( NC_Filename, &
                               NF90_CLOBBER, &
                               NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error creating '//TRIM(NC_Filename)//' - '//&
                TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF


    ! Define the dimensions
    ! ---------------------
    Message = 'Error defining dimensions in '//TRIM(NC_Filename)

    ! The number of predictors. Note that the defined
    ! dimension value is "n + 1" as the array elements for
    ! this dimension ranges from 0 -> n.
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               PREDICTOR_DIMNAME, n_Predictors+1, n_Predictors_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of absorbers
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               ABSORBER_DIMNAME, n_Absorbers, n_Absorbers_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of sensor channels
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               CHANNEL_DIMNAME, n_Channels, n_Channels_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of alphas
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               ALPHA_DIMNAME, n_Alphas, n_Alphas_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of C coefficients
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               COEFF_DIMNAME, n_Coeffs, n_Coeffs_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! Write the global attributes
    ! ---------------------------
    Write_Status = WriteGAtts( NC_Filename                      , &  ! Input
                               NC_FileID                        , &  ! Input
                               Version         =Version         , &  ! Optional input
                               Sensor_Id       =Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
                               Title           =Title           , &  ! Optional input
                               History         =History         , &  ! Optional input
                               Comment         =Comment         , &  ! Optional input
                               Profile_Set_Id  =Profile_Set_Id  , &  ! Optional input
                               Message_Log     =Message_Log       )  ! Error messaging
    IF ( Write_Status /= SUCCESS ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing global attributes to '// &
                            TRIM(NC_Filename), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END IF


    ! Define the variables
    ! --------------------
    Define_Status = DefineVar( NC_Filename            , &  ! Input
                               NC_FileID              , &  ! Input
                               n_Predictors_DimID     , &  ! Input
                               n_Absorbers_DimID      , &  ! Input
                               n_Channels_DimID       , &  ! Input
                               n_Alphas_DimID         , &  ! Input
                               n_Coeffs_DimID         , &  ! Input
                               Message_Log=Message_Log  )  ! Error messaging
    IF ( Define_Status /= SUCCESS ) THEN
      Message = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF

    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status = NF90_ENDDEF( NC_FileID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      NF90_Status = NF90_CLOSE( NC_FileID )
      Message = 'Error taking '//TRIM(NC_Filename)//' out of define mode.'
      CALL Create_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Create_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Create_CleanUp

  END FUNCTION CreateFile


!------------------------------------------------------------------------------
!
! NAME:
!       WriteGAtts
!
! PURPOSE:
!       Function to write the global attributes to a netCDF ODAS
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = WriteGAtts( NC_Filename                      , &  ! Input
!                                  NC_FileID                        , &  ! Input
!                                  Version         =Version         , &  ! Optional input
!                                  Sensor_Id       =Sensor_Id       , &  ! Optional input
!                                  WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!                                  Title           =Title           , &  ! Optional input
!                                  History         =History         , &  ! Optional input
!                                  Comment         =Comment         , &  ! Optional input
!                                  Profile_Set_Id  =Profile_Set_Id  , &  ! Optional input
!                                  Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ODAS format data file to create.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number returned from the
!                         Open_netCDF() function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Version:          The version number of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ODAS file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_ID:   Character string written into the PROFILE_SET_ID
!                         global attribute field of the netCDF ODAS file.
!                         Should contain a short tag used to identify the
!                         dependent profile set used to generate the 
!                         coefficient data.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute write was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
                       NC_FileID       , &  ! Input
                       Version         , &  ! Optional input
                       Sensor_Id       , &  ! Optional input
                       WMO_Satellite_Id, &  ! Optional input
                       WMO_Sensor_Id   , &  ! Optional input
                       Title           , &  ! Optional input
                       History         , &  ! Optional input
                       Comment         , &  ! Optional input
                       Profile_Set_Id  , &  ! Optional input
                       Message_Log     ) &  ! Error messaging
                     RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN) :: NC_Filename
    INTEGER     ,           INTENT(IN) :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: Message
    CHARACTER(ML) :: GAttName
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: Ver
    INTEGER :: NF90_Status
    TYPE(ODAS_type) :: ODAS_Default  

    ! Set up
    ! ------
    Error_Status = SUCCESS
    Message = ' '


    ! Mandatory global attributes
    ! ---------------------------
    ! Software ID
    GAttName = WRITE_MODULE_HISTORY_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                MODULE_RCS_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF
    
    ! Creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    GAttName = CREATION_DATE_AND_TIME_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                                czone//'UTC' )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Algorithm Id
    GAttName = ALGORITHM_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                ODAS_Default%Algorithm )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                ODAS_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = ODAS_Default%Version
    END IF
    GAttName = VERSION_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Ver )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Title )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  History )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Comment )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Profile_Set_Id
    IF ( PRESENT(Profile_Set_Id) ) THEN
      GAttName = PROFILE_SET_ID_GATTNAME
      NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Profile_Set_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF

  CONTAINS
  
    SUBROUTINE WriteGAtts_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(GAttName)//' attribute to '//&
                            TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status ) )//TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteGAtts_CleanUp
    
  END FUNCTION WriteGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       ReadGAtts
!
! PURPOSE:
!       Function to read the global attributes from a netCDF ODAS
!       data file.
!
! CALLING SEQUENCE:
!       Error_Status = ReadGAtts( NC_Filename                      , &  ! Input
!                                 NC_FileID                        , &  ! Input
!                                 Version         =Version         , &  ! Optional output
!                                 Sensor_Id       =Sensor_Id       , &  ! Optional output
!                                 WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional output
!                                 WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional output
!                                 Title           =Title           , &  ! Optional output
!                                 History         =History         , &  ! Optional output
!                                 Comment         =Comment         , &  ! Optional output
!                                 Profile_Set_Id  =Profile_Set_Id  , &  ! Optional output
!                                 Message_Log     =Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:      Character string specifying the name of the
!                         netCDF ODAS format data file to read from.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:        NetCDF file ID number.
!                         function.
!                         UNITS:      N/A
!                         TYPE:       Integer
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:      Character string specifying a filename in which
!                         any messages will be logged. If not specified,
!                         or if an error occurs opening the log file, the
!                         default action is to output messages to standard
!                         output.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Version:          The version number of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Id:        Character string sensor/platform identifier.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:            Character string written into the TITLE global
!                         attribute field of the netCDF ODAS file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:   Character string written into the PROFILE_SET_ID
!                         global attribute field of the netCDF ODAS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:     The return value is an integer defining the error status.
!                         The error codes are defined in the Message_Handler module.
!                         If == SUCCESS the global attribute read was successful.
!                            == FAILURE an error occurred.
!                         UNITS:      N/A
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION ReadGAtts( NC_Filename     , &  ! Input
                      NC_FileID       , &  ! Input
                      Version         , &  ! Optional output
                      Sensor_Id       , &  ! Optional output
                      WMO_Satellite_Id, &  ! Optional output
                      WMO_Sensor_Id   , &  ! Optional output
                      Title           , &  ! Optional output
                      History         , &  ! Optional output
                      Comment         , &  ! Optional output
                      Profile_Set_Id  , &  ! Optional output
                      Message_Log     ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER,                INTENT(IN)  :: NC_FileID
    INTEGER     , OPTIONAL, INTENT(OUT) :: Version         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id       
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id
    INTEGER     , OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id   
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Profile_Set_Id
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: msg
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Algorithm
    INTEGER :: Release
    INTEGER :: NF90_Status
    TYPE(ODAS_type) :: ODAS_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Algorithm
    Algorithm = ODAS_Default%Algorithm
    GAttName  = ALGORITHM_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Algorithm )
    ! ...netCDF API error
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = TRIM(NF90_STRERROR( NF90_Status ) )
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    ! ...Invalid algorithm error
    IF ( Algorithm /= ODAS_Default%Algorithm ) THEN
      msg = 'Invalid Algorithm ID attribute'
      CALL ReadGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    Release  = ODAS_Default%Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Release )
    ! ...netCDF API error
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = TRIM(NF90_STRERROR( NF90_Status ) )
      CALL ReadGAtts_Cleanup(); RETURN
    END IF
    ! ...Invalid release error
    IF ( Release /= ODAS_Default%Release ) THEN
      msg = 'Invalid Release attribute'
      CALL ReadGAtts_Cleanup(); RETURN
    END IF


    ! The optional GAtts
    ! ------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      GAttName = VERSION_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  Version )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Sensor_Id
    IF ( PRESENT(Sensor_Id) ) THEN
      GAttString = ' '; Sensor_Id = ' '
      GAttName = SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Sensor_Id = GAttString(1:MIN( LEN(Sensor_Id), LEN_TRIM(GAttString) ))
    END IF

    ! The WMO_Satellite_Id
    IF ( PRESENT(WMO_Satellite_Id) ) THEN
      GAttName = WMO_SATELLITE_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Satellite_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The WMO_Sensor_Id
    IF ( PRESENT(WMO_Sensor_Id) ) THEN
      GAttName = WMO_SENSOR_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  WMO_Sensor_Id )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
    END IF

    ! The Title
    IF ( PRESENT(Title) ) THEN
      GAttString = ' '; Title = ' '
      GAttName = TITLE_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Title = GAttString(1:MIN( LEN(Title), LEN_TRIM(GAttString) ))
    END IF

    ! The History
    IF ( PRESENT(History) ) THEN
      GAttString = ' '; History = ' '
      GAttName = HISTORY_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      History = GAttString(1:MIN( LEN(History), LEN_TRIM(GAttString) ))
    END IF

    ! The Comment
    IF ( PRESENT(Comment) ) THEN
      GAttString = ' '; Comment = ' '
      GAttName = COMMENT_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Comment = GAttString(1:MIN( LEN(Comment), LEN_TRIM(GAttString) ))
    END IF

    ! The Profile_Set_Id
    IF ( PRESENT(Profile_Set_Id) ) THEN
      GAttString = ' '; Profile_Set_Id = ' '
      GAttName = PROFILE_SET_ID_GATTNAME
      NF90_Status = NF90_GET_ATT( NC_FileID, &
                                  NF90_GLOBAL, &
                                  TRIM(GAttName), &
                                  GAttString )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = TRIM(NF90_STRERROR( NF90_Status ) )
        CALL ReadGAtts_Cleanup(); RETURN
      END IF
      CALL Remove_NULL_Characters( GAttString )
      Profile_Set_Id = GAttString(1:MIN( LEN(Profile_Set_Id), LEN_TRIM(GAttString) ))
    END IF

  CONTAINS
  
    SUBROUTINE ReadGAtts_CleanUp()
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute from '//TRIM(NC_Filename)//' - '// &
                            TRIM(msg), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadGAtts_CleanUp

  END FUNCTION ReadGAtts


!------------------------------------------------------------------------------
!
! NAME:
!       DefineDim
!
! PURPOSE:
!       Function to define the ODAS TauCoeff dimensions in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineDim( NC_Filename            , &  ! Input
!                                 NC_FileID              , &  ! Input
!                                 Dim_Name               , &  ! Input
!                                 Dim_Size               , &  ! Input
!                                 Dim_ID                 , &  ! Output
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODAS format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be defned.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Dim_Name:           The name of the netCDF dimension to define.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       Dim_Size:           The size of the netCDF dimension to define.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS
!       Dim_ID:             The ID of the defined dimension.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineDim( NC_Filename, &  ! Input
                      NC_FileID  , &  ! Input
                      Dim_Name   , &  ! Input
                      Dim_Size   , &  ! Input
                      Dim_ID     , &  ! Output
                      Message_Log) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    CHARACTER(*)          , INTENT(IN)  :: Dim_Name
    INTEGER               , INTENT(IN)  :: Dim_Size
    INTEGER               , INTENT(OUT) :: Dim_ID
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineDim'
    ! Local variables
    INTEGER :: NF90_Status
    
    ! Set up
    ! ------
    Error_Status = SUCCESS
    
    ! Define the dimension
    ! --------------------
    NF90_Status = NF90_DEF_DIM( NC_FileID, &
                                TRIM(Dim_Name), &
                                Dim_Size, &
                                Dim_ID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error defining the '//TRIM(Dim_Name)//' dimension in '// &
                            TRIM(NC_Filename)//' - '// &
                            TRIM(NF90_STRERROR( NF90_Status )), &
                            Error_Status, &
                            Message_Log=Message_Log )
      NF90_Status = NF90_CLOSE( NC_FileID )
    END IF
    
  END FUNCTION DefineDim


!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar
!
! PURPOSE:
!       Function to define the ODAS TauCoeff variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar( NC_Filename            , &  ! Input
!                                 NC_FileID              , &  ! Input
!                                 n_Orders_DimID         , &  ! Input
!                                 n_Predictors_DimID     , &  ! Input
!                                 n_Absorbers_DimID      , &  ! Input
!                                 n_Channels_DimID       , &  ! Input
!                                 Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODAS format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be defned.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!
!       n_Predictors_DimID: NetCDF dimension ID of the number of predictors
!                           plus one - since all the predictor arrays are
!                           are dimensioned (0:n_Predictors).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Absorbers_DimID:  NetCDF dimension ID of the number of
!                           absorbers (n_Absorbers).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Channels_DimID:   NetCDF dimension ID of the number of sensor
!                           channels (n_Channels).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Alphas_DimID:     NetCDF dimension ID of the number of Alpha coefficients
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Coeffs_DimID:     NetCDF dimension ID of the number of C coefficients
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!------------------------------------------------------------------------------

  FUNCTION DefineVar( NC_Filename       , &  ! Input
                      NC_FileID         , &  ! Input
                      n_Predictors_DimID, &  ! Input
                      n_Absorbers_DimID , &  ! Input
                      n_Channels_DimID  , &  ! Input
                      n_Alphas_DimID    , &  ! Input
                      n_Coeffs_DimID    , &  ! Input
                      Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Predictors_DimID
    INTEGER     ,           INTENT(IN)  :: n_Absorbers_DimID 
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID  
    INTEGER     ,           INTENT(IN)  :: n_Alphas_DimID    
    INTEGER     ,           INTENT(IN)  :: n_Coeffs_DimID    
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      


    ! Define the sensor type
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_TYPE_VARNAME, &
                                SENSOR_TYPE_TYPE, &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SENSOR_TYPE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SENSOR_TYPE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SENSOR_TYPE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SENSOR_TYPE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SENSOR_TYPE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SENSOR_TYPE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    ! Define the sensor channels
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                SENSOR_CHANNEL_VARNAME, &
                                SENSOR_CHANNEL_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  SENSOR_CHANNEL_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  SENSOR_CHANNEL_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  SENSOR_CHANNEL_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    ! Define the absorber id
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ABSORBER_ID_VARNAME, &
                                ABSORBER_ID_TYPE, &
                                dimIDs=(/n_Absorbers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ABSORBER_ID_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ABSORBER_ID_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ABSORBER_ID_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ABSORBER_ID_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ABSORBER_ID_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ABSORBER_ID_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the max order variable
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                MAX_ORDER_VARNAME, &
                                MAX_ORDER_TYPE, &
                                dimIDs=(/n_Absorbers_DimID/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//MAX_ORDER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  MAX_ORDER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  MAX_ORDER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  MAX_ORDER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  MAX_ORDER_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//MAX_ORDER_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define Alpha
    ! ------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_VARNAME, &
                                ALPHA_TYPE, &
                                dimIDs=(/n_Alphas_DimID, n_Absorbers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ALPHA_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ALPHA_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ALPHA_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ALPHA_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ALPHA_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ALPHA_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF



    ! Define Order
    ! ------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ORDER_VARNAME, &
                                ORDER_TYPE, &
                                dimIDs=(/n_Absorbers_DimID , &
                                         n_Channels_DimID   /), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ORDER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ORDER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ORDER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ORDER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ORDER_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ORDER_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    ! Define Pre_Index
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                PRE_INDEX_VARNAME, &
                                PRE_INDEX_TYPE, &
                                dimIDs=(/n_Predictors_DimID, &
                                         n_Absorbers_DimID , &
                                         n_Channels_DimID   /), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//PRE_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  PRE_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  PRE_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  PRE_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  PRE_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//PRE_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define Pos_Index
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                POS_INDEX_VARNAME, &
                                POS_INDEX_TYPE, &
                                dimIDs=(/n_Absorbers_DimID , &
                                         n_Channels_DimID   /), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//POS_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  POS_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  POS_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  POS_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  POS_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//POS_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define Tau_Coefficients
    ! -----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                COEFF_VARNAME, &
                                COEFF_TYPE, &
                                dimIDs=(/n_Coeffs_DimID/) , &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//COEFF_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  COEFF_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  COEFF_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  COEFF_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  COEFF_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//COEFF_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar


!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the ODAS TauCoeff variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &  ! Input
!                                NC_FileID              , &  ! Input
!                                ODAS                   , &  ! Input
!                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODAS format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       ODAS:               Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODAS_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
!------------------------------------------------------------------------------

  FUNCTION WriteVar( NC_Filename, &  ! Input
                     NC_FileID  , &  ! Input
                     ODAS       , &  ! Input
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(ODAS_type)       , INTENT(IN)  :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      


    ! Write the sensor type
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        ODAS%Sensor_Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    
    ! Write the Sensor_Channel data
    ! -----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ODAS%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    

    ! Write the absorber id data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        ODAS%Absorber_Id )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ABSORBER_ID_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the absorber id data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        MAX_ORDER_VARNAME, &
                                        ODAS%Max_Order )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//MAX_ORDER_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Alpha data
    ! --------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ALPHA_VARNAME, &
                                        ODAS%Alpha )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ALPHA_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF    

    ! Write the Order data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ORDER_VARNAME, &
                                        ODAS%Order )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ORDER_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    

    ! Write the Pre_Index data
    ! ------------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        PRE_INDEX_VARNAME, &
                                        ODAS%Pre_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//PRE_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the Pre_Index data
    ! ------------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        POS_INDEX_VARNAME, &
                                        ODAS%Pos_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//POS_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Tau_Coefficients data
    ! -------------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        COEFF_VARNAME, &
                                        ODAS%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//COEFF_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE WriteVar_CleanUp

  END FUNCTION WriteVar


!------------------------------------------------------------------------------
!
! NAME:
!       ReadVar
!
! PURPOSE:
!       Function to read the ODAS TauCoeff variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &  ! Input
!                               NC_FileID              , &  ! Input
!                               ODAS                   , &  ! Output
!                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODAS format file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       NC_FileID:          NetCDF file ID number of the file in which
!                           the variables are to be written.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODAS:               Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODAS_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUT ARGUMENTS
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! SIDE EFFECTS:
!       If an error occurs, the netCDF file is closed.
!
! COMMENTS:
!       The INTENT on the output ODAS argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    ODAS       , &  ! Output
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(ODAS_type)       , INTENT(IN OUT) :: ODAS
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'ReadVar'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      


    ! Read the sensor type
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        ODAS%Sensor_Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_TYPE_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    
    ! Read the Sensor_Channel data
    ! -----------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ODAS%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_CHANNEL_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    

    ! Read the absorber id data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        ODAS%Absorber_Id )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ABSORBER_ID_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the Max_Order data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        MAX_ORDER_VARNAME, &
                                        ODAS%Max_Order )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//MAX_ORDER_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Alpha data
    ! --------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ALPHA_VARNAME, &
                                        ODAS%Alpha )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ALPHA_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
        

    ! Read the Order data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ORDER_VARNAME, &
                                        ODAS%Order )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ORDER_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    

    ! Read the Pre_Index data
    ! ------------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        PRE_INDEX_VARNAME, &
                                        ODAS%Pre_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//PRE_INDEX_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the Pos_Index data
    ! ------------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        POS_INDEX_VARNAME, &
                                        ODAS%Pos_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//POS_INDEX_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Tau_Coefficients data
    ! -------------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        COEFF_VARNAME, &
                                        ODAS%C )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//COEFF_VARNAME//' from '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = TRIM(Message)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar

END MODULE ODAS_netCDF_IO
