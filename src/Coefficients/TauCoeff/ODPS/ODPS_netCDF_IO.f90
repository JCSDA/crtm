!
! ODPS_netCDF_IO
!
! Module containing routines to create, inquire, read and write netCDF
! format ODPS TauCoeff files.
!
!
! CREATION HISTORY:
!       Modified by:    Yong Chen, CIRA/CSU/JCSDA, NOAA/NESDIS 25-Feb-2009
!                       Yong.Chen@noaa.gov
!                       Based on Paul van Delst's framework
!
!
MODULE ODPS_netCDF_IO
  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds       , ONLY: Long, Double, Single, fp
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE ODPS_Define      , ONLY: INVALID_WMO_SATELLITE_ID, &
                               INVALID_WMO_SENSOR_ID   , &
                               INVALID_SENSOR          , &
                               ODPS_Type            , &  
                               Associated_ODPS      , &  
                               Allocate_ODPS        , &  
                               Allocate_ODPS_OPTRAN , &  
                               Destroy_ODPS         , &  
                               CheckRelease_ODPS    , &  
                               CheckAlgorithm_ODPS  , &  
                               Info_ODPS                 
  
  USE netcdf
  USE netCDF_Utility
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Structure procedures
  PUBLIC :: Inquire_ODPS_netCDF
  PUBLIC :: Read_ODPS_netCDF
  PUBLIC :: Write_ODPS_netCDF

  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Message character length
  INTEGER, PARAMETER :: ML = 1024


  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history'
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment'
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: ALGORITHM_GATTNAME        = 'Algorithm'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  CHARACTER(*), PARAMETER :: PROFILE_SET_ID_GATTNAME   = 'Profile_Set_Id' 
 
  ! Dimension names
  CHARACTER(*), PARAMETER :: LAYER_DIMNAME             = 'n_Layers'
  CHARACTER(*), PARAMETER :: LEVEL_DIMNAME             = 'n_Levels'
  CHARACTER(*), PARAMETER :: COMPONENT_DIMNAME         = 'n_Components'
  CHARACTER(*), PARAMETER :: ABSORBER_DIMNAME          = 'n_Absorbers'
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME           = 'n_Channels'
  CHARACTER(*), PARAMETER :: COEFF_DIMNAME             = 'n_Coeffs'
  CHARACTER(*), PARAMETER :: ODASPRED_DIMNAME          = 'n_OPIndex'
  CHARACTER(*), PARAMETER :: ODASCOEFF_DIMNAME         = 'n_OCoeffs'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: GROUP_INDEX_VARNAME       = 'Group_Index'
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_VARNAME       = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: REF_LEVEL_PRESSURE_VARNAME= 'Ref_Level_Pressure'
  CHARACTER(*), PARAMETER :: REF_PRESSURE_VARNAME      = 'Ref_Pressure'
  CHARACTER(*), PARAMETER :: REF_TEMPERATURE_VARNAME   = 'Ref_Temperature'
  CHARACTER(*), PARAMETER :: REF_ABSORBER_VARNAME      = 'Ref_Absorber'
  CHARACTER(*), PARAMETER :: MIN_ABSORBER_VARNAME      = 'Min_Absorber'
  CHARACTER(*), PARAMETER :: MAX_ABSORBER_VARNAME      = 'Max_Absorber'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME    = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: COMPONENT_ID_VARNAME      = 'Component_ID'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_VARNAME       = 'Absorber_ID'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_VARNAME      = 'n_Predictors'
  CHARACTER(*), PARAMETER :: POS_INDEX_VARNAME         = 'Pos_Index'
  CHARACTER(*), PARAMETER :: ODPS_COEFFICIENTS_VARNAME = 'ODPS_Coefficients'
  CHARACTER(*), PARAMETER :: OSIGNIFICANCE_VARNAME     = 'OSignificance'
  CHARACTER(*), PARAMETER :: ORDER_VARNAME             = 'Order'
  CHARACTER(*), PARAMETER :: OP_INDEX_VARNAME          = 'OP_Index'
  CHARACTER(*), PARAMETER :: OPOS_INDEX_VARNAME        = 'OPos_Index'
  CHARACTER(*), PARAMETER :: ODAS_COEFFICIENTS_VARNAME = 'OC'
  CHARACTER(*), PARAMETER :: ALPHA_VARNAME             = 'Alpha'
  CHARACTER(*), PARAMETER :: ALPHA_C1_VARNAME          = 'Alpha_C1'
  CHARACTER(*), PARAMETER :: ALPHA_C2_VARNAME          = 'Alpha_C2'
  CHARACTER(*), PARAMETER :: OCOMPONENT_INDEX_VARNAME  = 'OComponent_Index'

  ! Description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'

  CHARACTER(*), PARAMETER :: GROUP_INDEX_DESCRIPTION       = 'Group index to identify different TC groups'
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_DESCRIPTION       = 'Sensor type to identify uW, IR, VIS, UV, etc sensor channels'
  CHARACTER(*), PARAMETER :: REF_LEVEL_PRESSURE_DESCRIPTION= 'Reference profile level pressure'
  CHARACTER(*), PARAMETER :: REF_PRESSURE_DESCRIPTION      = 'Reference profile pressure'
  CHARACTER(*), PARAMETER :: REF_TEMPERATURE_DESCRIPTION   = 'Reference profile temperature'
  CHARACTER(*), PARAMETER :: REF_ABSORBER_DESCRIPTION      = 'Reference profile absorber'
  CHARACTER(*), PARAMETER :: MIN_ABSORBER_DESCRIPTION      = 'Training profiles minimum absorber'
  CHARACTER(*), PARAMETER :: MAX_ABSORBER_DESCRIPTION      = 'Training profiles maximum absorber'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION    = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: COMPONENT_ID_DESCRIPTION      = 'List of component ID'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_DESCRIPTION       = 'List of absorber ID'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_DESCRIPTION      = 'List of the number of predictors'
  CHARACTER(*), PARAMETER :: POS_INDEX_DESCRIPTION         = 'List of the starting position'
  CHARACTER(*), PARAMETER :: ODPS_COEFFICIENTS_DESCRIPTION = 'Regression model ODPS gas absorption coefficients'
  CHARACTER(*), PARAMETER :: OSIGNIFICANCE_DESCRIPTION     = 'Flag to indicating ODAS be applied'
  CHARACTER(*), PARAMETER :: ORDER_DESCRIPTION             = 'List of polynomial orders'
  CHARACTER(*), PARAMETER :: OP_INDEX_DESCRIPTION          = 'List of predictor indexes'
  CHARACTER(*), PARAMETER :: OPOS_INDEX_DESCRIPTION        = 'List of the starting position for ODAS'
  CHARACTER(*), PARAMETER :: ODAS_COEFFICIENTS_DESCRIPTION = 'Regression model ODAS gas absorption coefficients'
  CHARACTER(*), PARAMETER :: ALPHA_DESCRIPTION             = 'Alpha values used to generate the absorber space levels'
  CHARACTER(*), PARAMETER :: ALPHA_C1_DESCRIPTION          = 'First constant (slope) for Alpha to absorber space'
  CHARACTER(*), PARAMETER :: ALPHA_C2_DESCRIPTION          = 'Second constant (offset) for Alpha to absorber space'
  CHARACTER(*), PARAMETER :: OComponent_Index_DESCRIPTION  = 'OComponent Index for water line absorption'

  ! Long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'

  CHARACTER(*), PARAMETER :: GROUP_INDEX_LONGNAME       = 'Group Index'
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_LONGNAME       = 'Sensor Type'
  CHARACTER(*), PARAMETER :: REF_LEVEL_PRESSURE_LONGNAME= 'Ref Level Pressure'
  CHARACTER(*), PARAMETER :: REF_PRESSURE_LONGNAME      = 'Ref Pressure'
  CHARACTER(*), PARAMETER :: REF_TEMPERATURE_LONGNAME   = 'Ref Temperature'
  CHARACTER(*), PARAMETER :: REF_ABSORBER_LONGNAME      = 'Ref Absorber'
  CHARACTER(*), PARAMETER :: MIN_ABSORBER_LONGNAME      = 'Min Absorber'
  CHARACTER(*), PARAMETER :: MAX_ABSORBER_LONGNAME      = 'Max Absorber'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME    = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: COMPONENT_ID_LONGNAME      = 'Component ID'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_LONGNAME       = 'Absorber ID'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_LONGNAME      = 'number of Predictors'
  CHARACTER(*), PARAMETER :: POS_INDEX_LONGNAME         = 'Position Index'
  CHARACTER(*), PARAMETER :: ODPS_COEFFICIENTS_LONGNAME = 'ODPS Coefficients'
  CHARACTER(*), PARAMETER :: OSIGNIFICANCE_LONGNAME     = 'ODAS Significance'
  CHARACTER(*), PARAMETER :: ORDER_LONGNAME             = 'Polynomial Order Index'
  CHARACTER(*), PARAMETER :: OP_INDEX_LONGNAME          = 'Predictor indexes'
  CHARACTER(*), PARAMETER :: OPOS_INDEX_LONGNAME        = 'Starting Position Index'
  CHARACTER(*), PARAMETER :: ODAS_COEFFICIENTS_LONGNAME = 'ODAS coefficients'
  CHARACTER(*), PARAMETER :: ALPHA_LONGNAME             = 'Alpha'
  CHARACTER(*), PARAMETER :: ALPHA_C1_LONGNAME          = 'Alpha Slope'
  CHARACTER(*), PARAMETER :: ALPHA_C2_LONGNAME          = 'Alpha Offset'
  CHARACTER(*), PARAMETER :: OComponent_Index_LONGNAME  = 'ODAS Component Index'

  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'

  CHARACTER(*), PARAMETER :: GROUP_INDEX_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: REF_LEVEL_PRESSURE_UNITS= 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: REF_PRESSURE_UNITS      = 'hectoPascals (hPa)'
  CHARACTER(*), PARAMETER :: REF_TEMPERATURE_UNITS   = 'Kelvin (K)'
  CHARACTER(*), PARAMETER :: REF_ABSORBER_UNITS      = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: MIN_ABSORBER_UNITS      = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: MAX_ABSORBER_UNITS      = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS    = 'N/A' 
  CHARACTER(*), PARAMETER :: COMPONENT_ID_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: ABSORBER_ID_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: N_PREDICTORS_UNITS      = 'N/A'
  CHARACTER(*), PARAMETER :: POS_INDEX_UNITS         = 'N/A'
  CHARACTER(*), PARAMETER :: ODPS_COEFFICIENTS_UNITS = 'Absorber and predictor dependent'
  CHARACTER(*), PARAMETER :: OSIGNIFICANCE_UNITS     = 'N/A'
  CHARACTER(*), PARAMETER :: ORDER_UNITS             = 'N/A'
  CHARACTER(*), PARAMETER :: OP_INDEX_UNITS          = 'N/A'
  CHARACTER(*), PARAMETER :: OPOS_INDEX_UNITS        = 'N/A'
  CHARACTER(*), PARAMETER :: ODAS_COEFFICIENTS_UNITS = 'Absorber and predictor dependent'
  CHARACTER(*), PARAMETER :: ALPHA_UNITS             = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: ALPHA_C1_UNITS          = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: ALPHA_C2_UNITS          = 'Absorber dependent'
  CHARACTER(*), PARAMETER :: OComponent_Index_UNITS  = 'N/A'
  
  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
     
  INTEGER(Long), PARAMETER :: GROUP_INDEX_FILLVALUE       = 0
  INTEGER(Long), PARAMETER :: SENSOR_TYPE_FILLVALUE       = INVALID_SENSOR
  REAL(fp)     , PARAMETER :: REF_LEVEL_PRESSURE_FILLVALUE= ZERO
  REAL(fp)     , PARAMETER :: REF_PRESSURE_FILLVALUE      = ZERO
  REAL(fp)     , PARAMETER :: REF_TEMPERATURE_FILLVALUE   = ZERO
  REAL(fp)     , PARAMETER :: REF_ABSORBER_FILLVALUE      = ZERO
  REAL(fp)     , PARAMETER :: MIN_ABSORBER_FILLVALUE      = ZERO
  REAL(fp)     , PARAMETER :: MAX_ABSORBER_FILLVALUE      = ZERO
  INTEGER(Long), PARAMETER :: SENSOR_CHANNEL_FILLVALUE    = 0
  INTEGER(Long), PARAMETER :: COMPONENT_ID_FILLVALUE      = 0
  INTEGER(Long), PARAMETER :: ABSORBER_ID_FILLVALUE       = 0
  INTEGER(Long), PARAMETER :: N_PREDICTORS_FILLVALUE      = 0
  INTEGER(Long), PARAMETER :: POS_INDEX_FILLVALUE         = 0
  REAL(Single) , PARAMETER :: ODPS_COEFFICIENTS_FILLVALUE = 0_Single
  INTEGER(Long), PARAMETER :: OSIGNIFICANCE_FILLVALUE     = 0
  INTEGER(Long), PARAMETER :: ORDER_FILLVALUE             = 0
  INTEGER(Long), PARAMETER :: OP_INDEX_FILLVALUE          = 0
  INTEGER(Long), PARAMETER :: OPOS_INDEX_FILLVALUE        = 0
  REAL(fp)     , PARAMETER :: ODAS_COEFFICIENTS_FILLVALUE = ZERO
  REAL(fp)     , PARAMETER :: ALPHA_FILLVALUE             = ZERO
  REAL(fp)     , PARAMETER :: ALPHA_C1_FILLVALUE          = ZERO
  REAL(fp)     , PARAMETER :: ALPHA_C2_FILLVALUE          = ZERO
  INTEGER(Long), PARAMETER :: OComponent_Index_FILLVALUE  = 0

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: GROUP_INDEX_TYPE       = NF90_INT
  INTEGER, PARAMETER :: SENSOR_TYPE_TYPE       = NF90_INT
  INTEGER, PARAMETER :: REF_LEVEL_PRESSURE_TYPE= NF90_DOUBLE
  INTEGER, PARAMETER :: REF_PRESSURE_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: REF_TEMPERATURE_TYPE   = NF90_DOUBLE
  INTEGER, PARAMETER :: REF_ABSORBER_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: MIN_ABSORBER_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: MAX_ABSORBER_TYPE      = NF90_DOUBLE
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE    = NF90_INT
  INTEGER, PARAMETER :: COMPONENT_ID_TYPE      = NF90_INT
  INTEGER, PARAMETER :: ABSORBER_ID_TYPE       = NF90_INT
  INTEGER, PARAMETER :: N_PREDICTORS_TYPE      = NF90_INT
  INTEGER, PARAMETER :: POS_INDEX_TYPE         = NF90_INT
  INTEGER, PARAMETER :: ODPS_COEFFICIENTS_TYPE = NF90_FLOAT   
  INTEGER, PARAMETER :: OSIGNIFICANCE_TYPE     = NF90_INT
  INTEGER, PARAMETER :: ORDER_TYPE             = NF90_INT
  INTEGER, PARAMETER :: OP_INDEX_TYPE          = NF90_INT
  INTEGER, PARAMETER :: OPOS_INDEX_TYPE        = NF90_INT
  INTEGER, PARAMETER :: ODAS_COEFFICIENTS_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: ALPHA_TYPE             = NF90_DOUBLE
  INTEGER, PARAMETER :: ALPHA_C1_TYPE          = NF90_DOUBLE            
  INTEGER, PARAMETER :: ALPHA_C2_TYPE          = NF90_DOUBLE
  INTEGER, PARAMETER :: OComponent_Index_TYPE  = NF90_INT

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
!       Inquire_ODPS_netCDF
!
! PURPOSE:
!       Function to inquire a netCDF ODPS format file to obtain the
!       dimension values and global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_ODPS_netCDF( NC_Filename                        , &  ! Input
!                                           n_Layers         = n_Layers        , &  ! Optional output
!                                           n_Components     = n_Components    , &  ! Optional output
!                                           n_Absorbers      = n_Absorbers     , &  ! Optional output
!                                           n_Channels       = n_Channels      , &  ! Optional output
!                                           n_Coeffs         = n_Coeffs        , &  ! Optional output
!                                           n_OPIndex        = n_OPIndex       , &  ! Optional output
!                                           n_OCoeffs        = n_OCoeffs       , &  ! Optional output
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
!       NC_Filename:        Character string specifying the name of the netCDF ODPS
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
!       n_Layers:          The number of layers dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Components:       The number of component dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Coeffs:           The number of Coefficients dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_OPIndex:          The number of ODAS predictor dimension.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_OPIndex, where the
!                                 0'th term is the indict the total predictors. Therefore the actual
!                                 number of array elements along this dimension
!                                 is n_OPIndex+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_OCoeffs:          The number of Coefficients dimension of the ODAS data.  
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Release:            The ODPS data/file release number. Used to check
!                           for data/software mismatch.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the netCDF ODPS file.
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
!                           attribute field of the netCDF ODPS file.
!                           Should contain a succinct description of what
!                           is in the netCDF datafile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the netCDF ODPS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the netCDF ODPS file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:     Character string written into the PROFILE_SET_ID
!                           global attribute field of the netCDF ODPS file.
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

  FUNCTION Inquire_ODPS_netCDF( NC_Filename     , &  ! Input
                                n_Layers        , &  ! Optional output
                                n_Components    , &  ! Optional output
                                n_Absorbers     , &  ! Optional output
                                n_Channels      , &  ! Optional output
                                n_Coeffs        , &  ! Optional output
                                n_OPIndex       , &  ! Optional output
                                n_OCoeffs       , &  ! Optional output 
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
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Layers         
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Components     
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Absorbers      
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Channels       
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_Coeffs         
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_OPIndex        
    INTEGER     , OPTIONAL, INTENT(OUT) :: n_OCoeffs        
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_ODPS_netCDF'
    ! Function variables
    CHARACTER(ML) :: Message
    INTEGER :: Close_Status
    INTEGER :: NF90_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Dims
    INTEGER :: n_File_OPIndex
    INTEGER :: n_File_OCoeffs
    INTEGER :: n_File_n_Coeffs    
    
    TYPE(ODPS_type) :: ODPS  

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
    ! How many are there?
    NF90_Status = NF90_INQUIRE( NC_FileID, &
                                nDimensions = n_Dims )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error obtaining dimension information from '//TRIM(NC_Filename)//&
                ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF

    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         LAYER_DIMNAME, &
                                         ODPS%n_Layers, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//LAYER_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         COMPONENT_DIMNAME, &
                                         ODPS%n_Components, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//COMPONENT_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         ABSORBER_DIMNAME, &
                                         ODPS%n_Absorbers, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//ABSORBER_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF
    Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                         CHANNEL_DIMNAME, &
                                         ODPS%n_Channels, &
                                         Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(Close_File=SET); RETURN
    END IF

    ! The number of  n_Coeffs
    n_File_n_Coeffs  = 0
    IF ( n_Dims >= 6 ) THEN 
     Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                          COEFF_DIMNAME, &
                                          n_File_n_Coeffs, &
                                          Message_Log=Message_Log )
     IF ( Error_Status /= SUCCESS ) THEN
       Message = 'Error obtaining '//COEFF_DIMNAME//' dimension from '//TRIM(NC_Filename)
       CALL Inquire_Cleanup(Close_File=SET); RETURN
     END IF
    END IF

    ! The number of n_OPindex.
    n_File_OPIndex = 0
    n_File_OCoeffs = 0
    IF ( n_Dims > 6 ) THEN
    
     Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                          ODASPRED_DIMNAME, &
                                          n_File_OPIndex, &
                                          Message_Log=Message_Log )
     IF ( Error_Status /= SUCCESS ) THEN
       Message = 'Error obtaining '//ODASPRED_DIMNAME//' dimension from '//TRIM(NC_Filename)
       CALL Inquire_Cleanup(Close_File=SET); RETURN
     END IF
     Error_Status = Get_netCDF_Dimension( NC_FileID, &
                                          ODASCOEFF_DIMNAME, &
                                          n_File_OCoeffs, &
                                          Message_Log=Message_Log )
     IF ( Error_Status /= SUCCESS ) THEN
       Message = 'Error obtaining '//ODASCOEFF_DIMNAME//' dimension from '//TRIM(NC_Filename)
       CALL Inquire_Cleanup(Close_File=SET); RETURN
     END IF
    
    END IF

    ! Get the global attributes
    ! -------------------------
    Error_Status = ReadGAtts( NC_Filename                             , &
                              NC_FileID                               , &
                              Version          =ODPS%Version         , &
                              Sensor_Id        =ODPS%Sensor_Id       , &
                              WMO_Satellite_Id =ODPS%WMO_Satellite_Id, &
                              WMO_Sensor_Id    =ODPS%WMO_Sensor_Id   , &
                              Title            =Title                 , &
                              History          =History               , &
                              Comment          =Comment               , &
                              Profile_Set_Id   =Profile_Set_Id        , &
                              Message_Log      =Message_Log             )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      Message = 'Error closing netCDF ODPS data file '//TRIM(NC_Filename)
      CALL Inquire_Cleanup(); RETURN
    END IF


    ! Set the return values
    ! ---------------------
    ! Dimensions
    IF ( PRESENT(n_Layers     ) )  n_Layers      = ODPS%n_Layers 
    IF ( PRESENT(n_Components ) )  n_Components  = ODPS%n_Components 
    IF ( PRESENT(n_Absorbers  ) )  n_Absorbers   = ODPS%n_Absorbers    
    IF ( PRESENT(n_Channels   ) )  n_Channels    = ODPS%n_Channels  
    IF ( PRESENT(n_Coeffs     ) )  n_Coeffs      = n_File_n_Coeffs
    IF ( PRESENT(n_OPIndex    ) )  n_OPIndex     = n_File_OPIndex-1 
    IF ( PRESENT(n_OCoeffs    ) )  n_OCoeffs     = n_File_OCoeffs
     
    ! Release/Version information
    IF ( PRESENT(Release) ) Release = ODPS%Release
    IF ( PRESENT(Version) ) Version = ODPS%Version

    ! Sensor ids
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = ODPS%Sensor_Id(1:MIN(LEN(Sensor_Id),LEN_TRIM(ODPS%Sensor_Id)))
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = ODPS%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = ODPS%WMO_Sensor_Id   
    
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

  END FUNCTION Inquire_ODPS_netCDF

!--------------------------------------------------------------------------------
!
! NAME:
!       Write_ODPS_netCDF
!
! PURPOSE:
!       Function to write an ODPS structure to a netCDF format file.
!
! CALLING SEQUENCE:
!       Error_Status = Write_ODPS_netCDF( NC_Filename                      , &  ! Input
!                                         ODPS                             , &  ! Input
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
!                        netCDF ODPS format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       ODPS:            Structure to write to file.
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Title:           Character string written into the TITLE global
!                        attribute field of the netCDF ODPS file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ODPS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ODPS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_ID:  Character string written into the PROFILE_SET_ID
!                        global attribute field of the netCDF ODPS file.
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
!                           == FAILURE - the input ODPS structure contains
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

  FUNCTION Write_ODPS_netCDF( NC_Filename   , &  ! Input
                              ODPS          , &  ! Input
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
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_ODPS_netCDF'
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
    IF ( .NOT. Associated_ODPS( ODPS ) ) THEN
      Message = 'Some or all INPUT ODPS pointer members are NOT associated.'
      CALL Write_Cleanup(); RETURN
    END IF

    ! Create the output data file
    ! ---------------------------
    Error_Status = CreateFile( NC_Filename                           , &  ! Input
                               ODPS%n_Layers                         , &  ! Input
                               ODPS%n_Components                     , &  ! Input
                               ODPS%n_Absorbers                      , &  ! Input
                               ODPS%n_Channels                       , &  ! Input
                               ODPS%n_Coeffs                         , &  ! Input
                               ODPS%n_OPIndex                        , &  ! Input
                               ODPS%n_OCoeffs                        , &  ! Input
                               NC_FileID                             , &  ! Output
                               Version         =ODPS%Version         , &  ! Optional input
                               Sensor_Id       =ODPS%Sensor_Id       , &  ! Optional input
                               WMO_Satellite_Id=ODPS%WMO_Satellite_Id, &  ! Optional input
                               WMO_Sensor_Id   =ODPS%WMO_Sensor_Id   , &  ! Optional input
                               Title           =Title                , &  ! Optional input
                               History         =History              , &  ! Optional input
                               Comment         =Comment              , &  ! Optional input
                               Profile_Set_Id  =Profile_Set_Id       , &  ! Optional input
                               Message_Log     =Message_Log            )  ! Error messaging
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error creating output file '//TRIM(NC_Filename)
      CALL Write_Cleanup(); RETURN
    END IF



    ! Write the ODPS data
    ! -------------------
    Error_Status = WriteVar( NC_Filename, &
                             NC_FileID  , &
                             ODPS       , &
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
                            'Error closing netCDF ODPS data file '// &
                            TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODPS( ODPS, Message )
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

  END FUNCTION Write_ODPS_netCDF
  
!--------------------------------------------------------------------------------
!
! NAME:
!       Read_ODPS_netCDF
!
! PURPOSE:
!       Function to read data into an ODPS structure from a netCDF format file.
!
! CALLING SEQUENCE:
!         Error_Status = Read_ODPS_netCDF( NC_Filename                    , &  ! Input
!                                          ODPS                           , &  ! Output
!                                          Quiet          = Quiet         , &  ! Optional input
!                                          Title          = Title         , &  ! Optional output
!                                          History        = History       , &  ! Optional output
!                                          Comment        = Comment       , &  ! Optional output
!                                          Profile_Set_Id = Profile_Set_Id, &  ! Optional output
!                                          RCS_Id         = RCS_Id        , &  ! Revision control
!                                          Message_Log    = Message_Log     )  ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_Filename:     Character string specifying the name of the netCDF ODPS
!                        format data file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       ODPS:            Structure to contain the gas absorption coefficient
!                        data read from the file.
!                        UNITS:      N/A
!                        TYPE:       ODPS_type
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
!                        attribute field of the netCDF ODPS file.
!                        Should contain a succinct description of what
!                        is in the netCDF datafile.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:         Character string written into the HISTORY global
!                        attribute field of the netCDF ODPS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:         Character string written into the COMMENT global
!                        attribute field of the netCDF ODPS file.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:  Character string written into the PROFILE_SET_ID
!                        global attribute field of the netCDF ODPS file.
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
!       If the ODPS argument is defined upon input, it is redefined (or
!       reinitialised) at output.
!
! COMMENTS:
!       Note the INTENT on the output ODPS argument is IN OUT rather than
!       just OUT. This is necessary because the argument may be defined upon
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!--------------------------------------------------------------------------------

  FUNCTION Read_ODPS_netCDF( NC_Filename   , &  ! Input
                             ODPS          , &  ! Output
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
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_ODPS_netCDF'
    ! Function variables
    CHARACTER(1000)  :: Message
    LOGICAL :: Noisy
    INTEGER :: Destroy_Status
    INTEGER :: Close_Status
    INTEGER :: NC_FileID
    INTEGER :: n_Layers     
    INTEGER :: n_Components 
    INTEGER :: n_Absorbers  
    INTEGER :: n_Channels   
    INTEGER :: n_Coeffs     
    INTEGER :: n_OPIndex    
    INTEGER :: n_OCoeffs    

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
    Error_Status = Inquire_ODPS_netCDF( NC_Filename, &
                                        n_Layers       = n_Layers      , &
                                        n_Components   = n_Components  , &
                                        n_Absorbers    = n_Absorbers   , &
                                        n_Channels     = n_Channels    , &
                                        n_Coeffs       = n_Coeffs      , &
                                        n_OPIndex      = n_OPIndex     , &
                                        n_OCoeffs      = n_OCoeffs     , &
                                        Release        = ODPS%Release  , &
                                        Version        = ODPS%Version  , &
                                        Title          = Title         , &
                                        History        = History       , &
                                        Comment        = Comment       , &
                                        Profile_Set_Id = Profile_Set_Id, &
                                        Message_Log    = Message_Log     )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error obtaining ZTauCoeff dimensions from '//TRIM(NC_Filename)
      CALL Read_Cleanup(); RETURN
    END IF

    ! Allocate the structure
    Error_Status = Allocate_ODPS( n_Layers    , &
                                  n_Components, &
                                  n_Absorbers , &
                                  n_Channels  , &
                                  n_Coeffs    , &
                                  ODPS        , &
                                  Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error occurred allocating ODPS structure.'
      CALL Read_Cleanup(); RETURN
    END IF

    IF( n_OCoeffs > 0 )THEN

      Error_Status = Allocate_ODPS_OPTRAN( n_OCoeffs    , &
                                           ODPS         , &
                                    Message_Log=Message_Log)
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'ODPS OPTRAN array allocation failed'
        CALL Read_Cleanup(); RETURN
      END IF
    END IF

    ! Open the netCDF file for reading
    ! --------------------------------
    Error_Status = Open_netCDF( NC_Filename, &
                                NC_FileID, &
                                Mode='READ' )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error opening netCDF ODPS data file '//TRIM(NC_Filename)
      CALL Read_Cleanup( Destroy_Structure=SET ); RETURN
    END IF


    ! Read the global attributes
    ! --------------------------
    Error_Status = ReadGAtts( NC_Filename                             , &
                              NC_FileID                               , &
                              Version          = ODPS%Version         , &
                              Sensor_Id        = ODPS%Sensor_Id       , &
                              WMO_Satellite_Id = ODPS%WMO_Satellite_Id, &
                              WMO_Sensor_Id    = ODPS%WMO_Sensor_Id   , &
                              Title            = Title                , &
                              History          = History              , &
                              Comment          = Comment              , &
                              Profile_Set_Id   = Profile_Set_Id       , &
                              Message_Log      = Message_Log            )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading global attribute from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF

    ! Check the release
    Error_Status = CheckRelease_ODPS( ODPS, Message_Log=Message_Log )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'ODPS Release check failed for '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Read the ODPS data
    ! ------------------
    Error_Status = ReadVar( NC_Filename, &
                            NC_FileID  , &
                            ODPS       , &
                            Message_Log=Message_Log)
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading ODPS variables from '//TRIM(NC_Filename)
      CALL Read_Cleanup( Close_File=SET, Destroy_Structure=SET ); RETURN
    END IF


    ! Close the file
    ! --------------
    Close_Status = Close_netCDF( NC_FileID )
    IF ( Close_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF ODPS data file '//TRIM(NC_Filename), &
                            WARNING, &
                            Message_Log=Message_Log )
    END IF


    ! Output an info message
    ! ----------------------
    IF ( Noisy ) THEN
      CALL Info_ODPS( ODPS, Message )
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
          Destroy_Status = Destroy_ODPS(ODPS, Message_Log=Message_Log)
          IF ( Destroy_Status /= SUCCESS ) &
            Message = TRIM(Message)//'; Error destroying ODPS structure during error cleanup.'
        END IF
      END IF
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_ODPS_netCDF

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
!                                  n_Layers                         , &  ! Input
!                                  n_Components                     , &  ! Input
!                                  n_Absorbers                      , &  ! Input
!                                  n_Channels                       , &  ! Input
!                                  n_Coeffs                         , &  ! Input
!                                  n_OPIndex                        , &  ! Input
!                                  n_OCoeffs                        , &  ! Input
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
!       n_Layers:          The number of layers dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Components:       The number of component dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Absorbers:        The number of absorbers dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Channels:         The number of channels dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Coeffs:           The number of Coefficients dimension of the ODPS data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_OPIndex:          The number of ODAS predictor dimension.
!                           NOTE: The data arrays using this dimension value are
!                                 dimensioned as 0:n_OPIndex, where the
!                                 0'th term is the indict the total predictors. Therefore the actual
!                                 number of array elements along this dimension
!                                 is n_OPIndex+1
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_OCoeffs:          The number of Coefficients dimension of the ODAS data.  
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
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
                       n_Layers        , &  ! Input
                       n_Components    , &  ! Input
                       n_Absorbers     , &  ! Input
                       n_Channels      , &  ! Input
                       n_Coeffs        , &  ! Input
                       n_OPIndex       , &  ! Input
                       n_OCoeffs       , &  ! Input
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
    INTEGER               , INTENT(IN)  :: n_Layers        
    INTEGER               , INTENT(IN)  :: n_Components    
    INTEGER               , INTENT(IN)  :: n_Absorbers     
    INTEGER               , INTENT(IN)  :: n_Channels      
    INTEGER               , INTENT(IN)  :: n_Coeffs        
    INTEGER               , INTENT(IN)  :: n_OPIndex       
    INTEGER               , INTENT(IN)  :: n_OCoeffs       
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
    INTEGER :: n_Layers_DimID    
    INTEGER :: n_Levels_DimID    
    INTEGER :: n_Components_DimID
    INTEGER :: n_Absorbers_DimID 
    INTEGER :: n_Channels_DimID  
    INTEGER :: n_Coeffs_DimID    
    INTEGER :: n_OPIndex_DimID
    INTEGER :: n_OCoeffs_DimID 
 
    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check dimensions
    IF ( n_Layers     < 1 .OR. &
         n_Components < 1 .OR. &
         n_Absorbers  < 1 .OR. &
         n_Channels   < 1 .OR. &
         n_Coeffs     < 0    ) THEN
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

    ! The number of layers
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               LAYER_DIMNAME, n_Layers, n_Layers_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of levels
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               LEVEL_DIMNAME, n_Layers+1, n_Levels_DimID, &
                               Message_Log=Message_Log )
    IF ( Define_Status /= SUCCESS ) THEN 
      CALL Create_Cleanup(); RETURN
    END IF

    ! The number of components
    Define_Status = DefineDim( NC_Filename, NC_FileID, &
                               COMPONENT_DIMNAME, n_Components, n_Components_DimID, &
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
                               n_Layers_DimID         , &  ! Input
                               n_Levels_DimID         , &  ! Input
                               n_Components_DimID     , &  ! Input
                               n_Absorbers_DimID      , &  ! Input
                               n_Channels_DimID       , &  ! Input
                               Message_Log=Message_Log  )  ! Error messaging
    IF ( Define_Status /= SUCCESS ) THEN
      Message = 'Error defining variables in '//TRIM(NC_Filename)
      CALL Create_Cleanup(); RETURN
    END IF

    ! Define the ODPS coefficients dimension and variables
    IF (n_Coeffs > 0) THEN
    
    ! The number of coeffs
      Define_Status = DefineDim( NC_Filename, NC_FileID, &
                                 COEFF_DIMNAME, n_Coeffs, n_Coeffs_DimID, &
                                 Message_Log=Message_Log )
      IF ( Define_Status /= SUCCESS ) THEN 
        CALL Create_Cleanup(); RETURN
      END IF
      Define_Status = DefineVar_Coeff(NC_Filename            , &  ! Input
                                      NC_FileID              , &  ! Input
                                      n_Coeffs_DimID        , &  ! Input
                                      Message_Log=Message_Log  )  ! Error messaging

      IF ( Define_Status /= SUCCESS ) THEN
        Message = 'Error defining variables in '//TRIM(NC_Filename)
        CALL Create_Cleanup(); RETURN
      END IF
    
    END IF
    
    ! Define the ODAS dimension and variables if necessary
    ! ----------------------------------------------------
    IF (n_OCoeffs > 0) THEN
      ! The number of ODAS predictors 
      Define_Status = DefineDim( NC_Filename, NC_FileID, &
                                 ODASPRED_DIMNAME, n_OPIndex+1, n_OPIndex_DimID, &
                                 Message_Log=Message_Log )
      IF ( Define_Status /= SUCCESS ) THEN 
        CALL Create_Cleanup(); RETURN
      END IF

      ! The number of ODAS coeffs
      Define_Status = DefineDim( NC_Filename, NC_FileID, &
                                 ODASCOEFF_DIMNAME, n_OCoeffs, n_OCoeffs_DimID, &
                                 Message_Log=Message_Log )
      IF ( Define_Status /= SUCCESS ) THEN 
        CALL Create_Cleanup(); RETURN
      END IF

      Define_Status = DefineVar_ODAS( NC_Filename            , &  ! Input
                                      NC_FileID              , &  ! Input
                                      n_Channels_DimID       , &  ! Input
                                      n_OPIndex_DimID        , &  ! Input
                                      n_OCoeffs_DimID        , &  ! Input
                                      Message_Log=Message_Log  )  ! Error messaging

      IF ( Define_Status /= SUCCESS ) THEN
        Message = 'Error defining variables in '//TRIM(NC_Filename)
        CALL Create_Cleanup(); RETURN
      END IF
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
!       Function to write the global attributes to a netCDF ODPS
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
!                         netCDF ODPS format data file to create.
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
!       Version:          The version number of the netCDF ODPS file.
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
!                         attribute field of the netCDF ODPS file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ODPS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ODPS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Profile_Set_ID:   Character string written into the PROFILE_SET_ID
!                         global attribute field of the netCDF ODPS file.
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
    TYPE(ODPS_type) :: ODPS_Default  

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
                                ODPS_Default%Algorithm )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_PUT_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                ODPS_Default%Release )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      CALL WriteGAtts_Cleanup(); RETURN
    END IF


    ! Optional global attributes
    ! --------------------------
    ! The Version
    IF ( PRESENT(Version) ) THEN
      Ver = Version
    ELSE
      Ver = ODPS_Default%Version
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
!       Function to read the global attributes from a netCDF ODPS
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
!                         netCDF ODPS format data file to read from.
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
!       Version:          The version number of the netCDF ODPS file.
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
!                         attribute field of the netCDF ODPS file.
!                         Should contain a succinct description of what
!                         is in the netCDF datafile.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:          Character string written into the HISTORY global
!                         attribute field of the netCDF ODPS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:          Character string written into the COMMENT global
!                         attribute field of the netCDF ODPS file.
!                         UNITS:      N/A
!                         TYPE:       CHARACTER(*)
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Profile_Set_ID:   Character string written into the PROFILE_SET_ID
!                         global attribute field of the netCDF ODPS file.
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
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: Algorithm
    INTEGER :: Release
    INTEGER :: NF90_Status
    TYPE(ODPS_type) :: ODPS_Default

    ! Set up
    ! ------
    Error_Status = SUCCESS


    ! The mandatory GAtts for checking
    ! --------------------------------
    ! The Algorithm
    Algorithm = ODPS_Default%Algorithm
    GAttName  = ALGORITHM_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Algorithm )
    IF ( NF90_Status /= NF90_NOERR .OR. Algorithm /= ODPS_Default%Algorithm ) THEN
      CALL ReadGAtts_Cleanup(); RETURN
    END IF

    ! The Release
    Release  = ODPS_Default%Release
    GAttName = RELEASE_GATTNAME
    NF90_Status = NF90_GET_ATT( NC_FileID, &
                                NF90_GLOBAL, &
                                TRIM(GAttName), &
                                Release )
    IF ( NF90_Status /= NF90_NOERR .OR. Release /= ODPS_Default%Release) THEN
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
                            TRIM(NF90_STRERROR( NF90_Status ) ), &
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
!                                 n_Layers_DimID         , &  ! Input
!                                 n_Levels_DimID         , &  ! Input
!                                 n_Components_DimID     , &  ! Input
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
!       n_Layers_DimID:     NetCDF dimension ID of the number of layers.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Levels_DimID:     NetCDF dimension ID of the number of levels.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_Components_DimID: NetCDF dimension ID of the number of component.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
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
                      n_Layers_DimID    , &  ! Input
                      n_Levels_DimID    , &  ! Input
                      n_Components_DimID, &  ! Input
                      n_Absorbers_DimID , &  ! Input
                      n_Channels_DimID  , &  ! Input
                      Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Layers_DimID    
    INTEGER     ,           INTENT(IN)  :: n_Levels_DimID   
    INTEGER     ,           INTENT(IN)  :: n_Components_DimID
    INTEGER     ,           INTENT(IN)  :: n_Absorbers_DimID 
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID  
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

    ! Define the group index
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                GROUP_INDEX_VARNAME, &
                                GROUP_INDEX_TYPE, &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//GROUP_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  GROUP_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  GROUP_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  GROUP_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  GROUP_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//GROUP_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

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

    ! Define the Ref_Level_Pressure
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                REF_LEVEL_PRESSURE_VARNAME, &
                                REF_LEVEL_PRESSURE_TYPE, &
                                dimIDs=(/n_Levels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//REF_LEVEL_PRESSURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  REF_LEVEL_PRESSURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  REF_LEVEL_PRESSURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  REF_LEVEL_PRESSURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  REF_LEVEL_PRESSURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//REF_LEVEL_PRESSURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the Ref_Pressure
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                REF_PRESSURE_VARNAME, &
                                REF_PRESSURE_TYPE, &
                                dimIDs=(/n_Layers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//REF_PRESSURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  REF_PRESSURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  REF_PRESSURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  REF_PRESSURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  REF_PRESSURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//REF_PRESSURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    ! Define the REF_TEMPERATURE
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                REF_TEMPERATURE_VARNAME, &
                                REF_TEMPERATURE_TYPE, &
                                dimIDs=(/n_Layers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//REF_TEMPERATURE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  REF_TEMPERATURE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  REF_TEMPERATURE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  REF_TEMPERATURE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  REF_TEMPERATURE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//REF_TEMPERATURE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    ! Define the REF_ABSORBER
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                REF_ABSORBER_VARNAME, &
                                REF_ABSORBER_TYPE, &
                                dimIDs=(/n_Layers_DimID, n_Absorbers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//REF_ABSORBER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  REF_ABSORBER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  REF_ABSORBER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  REF_ABSORBER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  REF_ABSORBER_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//REF_ABSORBER_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the MIN_ABSORBER
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                MIN_ABSORBER_VARNAME, &
                                MIN_ABSORBER_TYPE, &
                                dimIDs=(/n_Layers_DimID, n_Absorbers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//MIN_ABSORBER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  MIN_ABSORBER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  MIN_ABSORBER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  MIN_ABSORBER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  MIN_ABSORBER_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//MIN_ABSORBER_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF
    
    ! Define the MAX_ABSORBER
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                MAX_ABSORBER_VARNAME, &
                                MAX_ABSORBER_TYPE, &
                                dimIDs=(/n_Layers_DimID, n_Absorbers_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//MAX_ABSORBER_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  MAX_ABSORBER_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  MAX_ABSORBER_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  MAX_ABSORBER_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  MAX_ABSORBER_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//MAX_ABSORBER_VARNAME//&
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

    ! Define the component id
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                COMPONENT_ID_VARNAME, &
                                COMPONENT_ID_TYPE, &
                                dimIDs=(/n_Components_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//COMPONENT_ID_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  COMPONENT_ID_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  COMPONENT_ID_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  COMPONENT_ID_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  COMPONENT_ID_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//COMPONENT_ID_VARNAME//&
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

    ! Define the n_Predictors 
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                N_PREDICTORS_VARNAME, &
                                N_PREDICTORS_TYPE, &
                                dimIDs=(/n_Components_DimID, n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//N_PREDICTORS_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  N_PREDICTORS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  N_PREDICTORS_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  N_PREDICTORS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  N_PREDICTORS_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//N_PREDICTORS_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the Pos_Index 
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                POS_INDEX_VARNAME, &
                                POS_INDEX_TYPE, &
                                dimIDs=(/n_Components_DimID, n_Channels_DimID/), &
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
      Message = 'Error writing '//N_PREDICTORS_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
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
!       DefineVar_Coeff
!
! PURPOSE:
!       Function to define the TauCoeff Coefficients variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar_Coeff(NC_Filename            , &  ! Input
!                                      NC_FileID              , &  ! Input
!                                      n_Coeffs_DimID         , &  ! Input
!                                      Message_Log=Message_Log  )  ! Error messaging
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
!       n_Coeffs_DimID:     NetCDF dimension ID of the number of Coefficients.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
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

  FUNCTION DefineVar_Coeff(NC_Filename       , &  ! Input
                           NC_FileID         , &  ! Input
                           n_Coeffs_DimID    , &  ! Input
                           Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Coeffs_DimID    
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar_Coeff'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      

    ! Define the ODPS coefficients 
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ODPS_COEFFICIENTS_VARNAME, &
                                ODPS_COEFFICIENTS_TYPE, &
                                dimIDs=(/n_Coeffs_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ODPS_COEFFICIENTS_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ODPS_COEFFICIENTS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ODPS_COEFFICIENTS_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ODPS_COEFFICIENTS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ODPS_COEFFICIENTS_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//N_PREDICTORS_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar_Coeff

!------------------------------------------------------------------------------
!
! NAME:
!       DefineVar_ODAS
!
! PURPOSE:
!       Function to define the ODAS TauCoeff variables in an output
!       netCDF file.
!
! CALLING SEQUENCE:
!       Error_Status = DefineVar_ODAS( NC_Filename            , &  ! Input
!                                      NC_FileID              , &  ! Input
!                                      n_Channels_DimID       , &  ! Input
!                                      n_OPIndex_DimID        , &  ! Input
!                                      n_Ocoeffs_DimID        , &  ! Input
!                                      Message_Log=Message_Log  )  ! Error messaging
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
!       n_Channels_DimID:   NetCDF dimension ID of the number of sensor
!                           channels (n_Channels).
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
!       n_OPIndex_DimID:    NetCDF dimension ID of the number.ODAS predictor
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       n_OCoeffs_DimID:    NetCDF dimension ID of the number.of ODAS Coefficients  
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
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

  FUNCTION DefineVar_ODAS( NC_Filename       , &  ! Input
                           NC_FileID         , &  ! Input
                           n_Channels_DimID  , &  ! Input
                           n_OPIndex_DimID   , &  ! Input
                           n_OCoeffs_DimID   , &  ! Input
                           Message_Log       ) &  ! Error messaging
                    RESULT( Error_Status )
    ! Arguments
    CHARACTER(*),           INTENT(IN)  :: NC_Filename
    INTEGER     ,           INTENT(IN)  :: NC_FileID
    INTEGER     ,           INTENT(IN)  :: n_Channels_DimID  
    INTEGER     ,           INTENT(IN)  :: n_OPIndex_DimID   
    INTEGER     ,           INTENT(IN)  :: n_OCoeffs_DimID   
    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'DefineVar_ODAS'
    ! Local variables
    CHARACTER(ML) :: Message
    INTEGER :: NF90_Status
    INTEGER :: varID
    INTEGER :: Put_Status(4)
                               
    ! Set up
    ! ------
    Error_Status = SUCCESS                                      


    ! Define Alpha
    ! ------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_VARNAME, &
                                ALPHA_TYPE, &
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


    ! Define Alpha_C1
    ! ---------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_C1_VARNAME, &
                                ALPHA_C1_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ALPHA_C1_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ALPHA_C1_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ALPHA_C1_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ALPHA_C1_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ALPHA_C1_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ALPHA_C1_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    ! Define Alpha_C2
    ! ---------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ALPHA_C2_VARNAME, &
                                ALPHA_C2_TYPE, &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ALPHA_C2_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ALPHA_C2_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ALPHA_C2_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ALPHA_C2_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ALPHA_C2_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ALPHA_C2_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the Ocomponent index
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OCOMPONENT_INDEX_VARNAME, &
                                OCOMPONENT_INDEX_TYPE, &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//OCOMPONENT_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  OCOMPONENT_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  OCOMPONENT_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  OCOMPONENT_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  OCOMPONENT_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//OCOMPONENT_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define the sensor channels
    ! --------------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OSIGNIFICANCE_VARNAME, &
                                OSIGNIFICANCE_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//OSIGNIFICANCE_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  OSIGNIFICANCE_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  OSIGNIFICANCE_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  OSIGNIFICANCE_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  OSIGNIFICANCE_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//OSIGNIFICANCE_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define Order
    ! ------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ORDER_VARNAME, &
                                ORDER_TYPE, &
                                dimIDs=(/n_Channels_DimID   /), &
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


    ! Define OP_INDEX
    ! ----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OP_INDEX_VARNAME, &
                                OP_INDEX_TYPE, &
                                dimIDs=(/n_OPIndex_DimID, &
                                         n_Channels_DimID   /), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//OP_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  OP_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  OP_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  OP_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  OP_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//OP_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

    ! Define OPOS_INDEX
    ! ------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                OPOS_INDEX_VARNAME, &
                                OPOS_INDEX_TYPE, &
                                dimIDs=(/n_Channels_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//OPOS_INDEX_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  OPOS_INDEX_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  OPOS_INDEX_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  OPOS_INDEX_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  OPOS_INDEX_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//OPOS_INDEX_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF


    ! Define ODAS_COEFFICIENTS
    ! -----------------------
    NF90_Status = NF90_DEF_VAR( NC_FileID, &
                                ODAS_COEFFICIENTS_VARNAME, &
                                ODAS_COEFFICIENTS_TYPE, &
                                dimIDs=(/n_OCoeffs_DimID/), &
                                varID =VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      Message = 'Error defining '//ODAS_COEFFICIENTS_VARNAME//' variable in '//&
                TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL DefineVar_Cleanup(); RETURN
    END IF

    Put_Status(1) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  LONGNAME_ATTNAME, &
                                  ODAS_COEFFICIENTS_LONGNAME )
    Put_Status(2) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  DESCRIPTION_ATTNAME, &
                                  ODAS_COEFFICIENTS_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  UNITS_ATTNAME, &
                                  ODAS_COEFFICIENTS_UNITS )
    Put_Status(4) = NF90_PUT_ATT( NC_FileID, &
                                  VarID, &
                                  FILLVALUE_ATTNAME, &
                                  ODAS_COEFFICIENTS_FILLVALUE )
    IF ( ANY(Put_Status /= SUCCESS) ) THEN
      Message = 'Error writing '//ODAS_COEFFICIENTS_VARNAME//&
                ' variable attributes to '//TRIM(NC_Filename)
      CALL DefineVar_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE DefineVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE DefineVar_CleanUp

  END FUNCTION DefineVar_ODAS

!------------------------------------------------------------------------------
!
! NAME:
!       WriteVar
!
! PURPOSE:
!       Function to write the ODPS TauCoeff variables in an output
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = WriteVar( NC_Filename            , &  ! Input
!                                NC_FileID              , &  ! Input
!                                ODPS                   , &  ! Input
!                                Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODPS format file.
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
!       ODPS:               Structure containing the data to write to file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODPS_type)
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
                     ODPS       , &  ! Input
                     Message_Log) &  ! Error messaging
                   RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)  :: NC_Filename
    INTEGER               , INTENT(IN)  :: NC_FileID
    TYPE(ODPS_type)       , INTENT(IN)  :: ODPS
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

    ! Write the group index
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        GROUP_INDEX_VARNAME, &
                                        ODPS%Group_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//GROUP_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the sensor type
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        ODPS%Sensor_Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Ref_Level_Pressure
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        REF_LEVEL_PRESSURE_VARNAME, &
                                        ODPS%Ref_Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//REF_LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Ref_Pressure
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        REF_PRESSURE_VARNAME, &
                                        ODPS%Ref_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//REF_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the Ref_Temperature
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        REF_TEMPERATURE_VARNAME, &
                                        ODPS%Ref_Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//REF_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the Ref_Absorber
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        REF_ABSORBER_VARNAME, &
                                        ODPS%Ref_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//REF_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Min_Absorber
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        Min_ABSORBER_VARNAME, &
                                        ODPS%Min_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//Min_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Max_Absorber
    ! ---------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        Max_ABSORBER_VARNAME, &
                                        ODPS%Max_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//Max_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Sensor_Channel data
    ! -----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ODPS%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the component id data
    ! -----------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        COMPONENT_ID_VARNAME, &
                                        ODPS%Component_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//COMPONENT_ID_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the absorber id data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        ODPS%Absorber_Id )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//ABSORBER_ID_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF
    
    ! Write the n_Predictors data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        N_PREDICTORS_VARNAME, &
                                        ODPS%n_Predictors )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//N_PREDICTORS_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    ! Write the Pos_Index data
    ! --------------------------
    Error_Status = Put_netCDF_Variable( NC_FileID, &
                                        POS_INDEX_VARNAME, &
                                        ODPS%Pos_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error writing '//POS_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL WriteVar_Cleanup(); RETURN
    END IF

    IF (ODPS%n_Coeffs > 0 ) THEN
      ! Write the ODPS Tau_Coefficients data
      ! -------------------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ODPS_COEFFICIENTS_VARNAME, &
                                          ODPS%C )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ODPS_COEFFICIENTS_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
    END IF

    IF (ODPS%n_OCoeffs > 0 ) THEN
      ! Write the Alpha data
      ! --------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ALPHA_VARNAME, &
                                          ODPS%Alpha )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ALPHA_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      

      ! Write the Alpha_C1 data
      ! -----------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ALPHA_C1_VARNAME, &
                                          ODPS%Alpha_C1 )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ALPHA_C1_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      

      ! Write the Alpha_C2 data
      ! -----------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ALPHA_C2_VARNAME, &
                                          ODPS%Alpha_C2 )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ALPHA_C2_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      
      ! Write the Ocomponent index data
      ! -----------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          OCOMPONENT_INDEX_VARNAME, &
                                          ODPS%OComponent_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//OCOMPONENT_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      
      ! Write the OSignificance data
      ! -----------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          OSIGNIFICANCE_VARNAME, &
                                          ODPS%OSignificance )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//OSIGNIFICANCE_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF

      ! Write the Order data
      ! --------------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ORDER_VARNAME, &
                                          ODPS%Order )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ORDER_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      

      ! Write the OP_Index data
      ! ------------------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          OP_INDEX_VARNAME, &
                                          ODPS%OP_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//OP_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      
      ! Write the OPos_Index data
      ! ------------------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          OPOS_INDEX_VARNAME, &
                                          ODPS%OPos_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//OPOS_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      

      ! Write the ODAS Tau_Coefficients data
      ! -------------------------------
      Error_Status = Put_netCDF_Variable( NC_FileID, &
                                          ODAS_COEFFICIENTS_VARNAME, &
                                          ODPS%OC )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error writing '//ODAS_COEFFICIENTS_VARNAME//' to '//TRIM(NC_Filename)
        CALL WriteVar_Cleanup(); RETURN
      END IF
      
    ENDIF
    
  CONTAINS
  
    SUBROUTINE WriteVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
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
!       Function to read the ODPS TauCoeff variables from any input
!       netCDF file in which they have been defined.
!
! CALLING SEQUENCE:
!       Error_Status = ReadVar( NC_Filename            , &  ! Input
!                               NC_FileID              , &  ! Input
!                               ODPS                   , &  ! Output
!                               Message_Log=Message_Log  )  ! Error messaging
!
! INPUT ARGUMENTS
!       NC_Filename:        Character string specifying the name of the
!                           already created netCDF ODPS format file.
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
!       ODPS:               Structure containing the data that was read
!                           from file.
!                           UNITS:      N/A
!                           TYPE:       TYPE(ODPS_type)
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
!       The INTENT on the output ODPS argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined
!       upon input. To prevent memory leaks, the IN OUT INTENT is a must.
!
!------------------------------------------------------------------------------

  FUNCTION ReadVar( NC_Filename, &  ! Input
                    NC_FileID  , &  ! Input
                    ODPS       , &  ! Output
                    Message_Log) &  ! Error messaging
                  RESULT( Error_Status )
    ! Arguments
    CHARACTER(*)          , INTENT(IN)     :: NC_Filename
    INTEGER               , INTENT(IN)     :: NC_FileID
    TYPE(ODPS_type)       , INTENT(IN OUT) :: ODPS
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


    ! Read the group index
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        GROUP_INDEX_VARNAME, &
                                        ODPS%Group_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//GROUP_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the sensor type
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_TYPE_VARNAME, &
                                        ODPS%Sensor_Type )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Ref_Level_Pressure
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        REF_LEVEL_PRESSURE_VARNAME, &
                                        ODPS%Ref_Level_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//REF_LEVEL_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Ref_Pressure
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        REF_PRESSURE_VARNAME, &
                                        ODPS%Ref_Pressure )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//REF_PRESSURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the Ref_Temperature
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        REF_TEMPERATURE_VARNAME, &
                                        ODPS%Ref_Temperature )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//REF_TEMPERATURE_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the Ref_Absorber
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        REF_ABSORBER_VARNAME, &
                                        ODPS%Ref_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//REF_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Min_Absorber
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        MIN_ABSORBER_VARNAME, &
                                        ODPS%Min_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//MIN_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Max_Absorber
    ! ---------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        MAX_ABSORBER_VARNAME, &
                                        ODPS%MAX_Absorber )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//MAX_ABSORBER_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Sensor_Channel data
    ! -----------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        SENSOR_CHANNEL_VARNAME, &
                                        ODPS%Sensor_Channel )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the component id data
    ! -----------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        COMPONENT_ID_VARNAME, &
                                        ODPS%Component_ID )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//COMPONENT_ID_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the absorber id data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        ABSORBER_ID_VARNAME, &
                                        ODPS%Absorber_Id )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//ABSORBER_ID_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF
    
    ! Read the n_Predictors data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        N_PREDICTORS_VARNAME, &
                                        ODPS%n_Predictors )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//N_PREDICTORS_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    ! Read the Pos_Index data
    ! --------------------------
    Error_Status = Get_netCDF_Variable( NC_FileID, &
                                        POS_INDEX_VARNAME, &
                                        ODPS%Pos_Index )
    IF ( Error_Status /= SUCCESS ) THEN
      Message = 'Error reading '//POS_INDEX_VARNAME//' to '//TRIM(NC_Filename)
      CALL ReadVar_Cleanup(); RETURN
    END IF

    IF (ODPS%n_Coeffs > 0 ) THEN
      ! Read the ODPS Tau_Coefficients data
      ! -------------------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ODPS_COEFFICIENTS_VARNAME, &
                                          ODPS%C )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ODPS_COEFFICIENTS_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
    END IF

    IF (ODPS%n_OCoeffs > 0 ) THEN
      ! Read the Alpha data
      ! --------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ALPHA_VARNAME, &
                                          ODPS%Alpha )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ALPHA_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      

      ! Read the Alpha_C1 data
      ! -----------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ALPHA_C1_VARNAME, &
                                          ODPS%Alpha_C1 )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ALPHA_C1_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      

      ! Read the Alpha_C2 data
      ! -----------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ALPHA_C2_VARNAME, &
                                          ODPS%Alpha_C2 )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ALPHA_C2_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      
      ! Read the Ocomponent index data
      ! -----------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          OCOMPONENT_INDEX_VARNAME, &
                                          ODPS%OComponent_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//OCOMPONENT_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      
      ! Read the OSignificance data
      ! -----------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          OSIGNIFICANCE_VARNAME, &
                                          ODPS%OSignificance )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//OSIGNIFICANCE_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF

      ! Read the Order data
      ! --------------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ORDER_VARNAME, &
                                          ODPS%Order )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ORDER_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      

      ! Read the OP_Index data
      ! ------------------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          OP_INDEX_VARNAME, &
                                          ODPS%OP_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//OP_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      
      ! Read the OPos_Index data
      ! ------------------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          OPOS_INDEX_VARNAME, &
                                          ODPS%OPos_Index )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//OPOS_INDEX_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      

      ! Read the ODAS Tau_Coefficients data
      ! -------------------------------
      Error_Status = Get_netCDF_Variable( NC_FileID, &
                                          ODAS_COEFFICIENTS_VARNAME, &
                                          ODPS%OC )
      IF ( Error_Status /= SUCCESS ) THEN
        Message = 'Error reading '//ODAS_COEFFICIENTS_VARNAME//' to '//TRIM(NC_Filename)
        CALL ReadVar_Cleanup(); RETURN
      END IF
      
    ENDIF
 
  CONTAINS
  
    SUBROUTINE ReadVar_CleanUp()
      ! Close file
      NF90_Status = NF90_CLOSE( NC_FileID )
      IF ( NF90_Status /= NF90_NOERR ) &
        Message = '; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR( NF90_Status ) )
      ! Set error status and print error message
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM(Message), &
                            Error_Status, &
                            Message_Log=Message_Log )
    END SUBROUTINE ReadVar_CleanUp

  END FUNCTION ReadVar


END MODULE ODPS_netCDF_IO
