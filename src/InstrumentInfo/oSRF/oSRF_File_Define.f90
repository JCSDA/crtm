!
! oSRF_File_Define
!
! Module defining the oSRF_File object.
!       
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 31-Jul-2009
!                       paul.vandelst@noaa.gov
 
MODULE oSRF_File_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, Display_Message
  USE String_Utility       , ONLY: StrClean
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   , &
                                   N_SENSOR_TYPES          , &
                                   INVALID_SENSOR          , &
                                   MICROWAVE_SENSOR        , &
                                   INFRARED_SENSOR         , &
                                   VISIBLE_SENSOR          , &
                                   ULTRAVIOLET_SENSOR      , &
                                   SENSOR_TYPE_NAME
  USE oSRF_Define          , ONLY: OSRF_RELEASE                   , &
                                   OSRF_VERSION                   , &
                                   oSRF_type                      , &
                                   oSRF_Associated                , &
                                   oSRF_Destroy                   , &
                                   oSRF_Create                    , &
                                   oSRF_SetValue                  , &
                                   oSRF_GetValue                  , &
                                   oSRF_Inspect                   , &
                                   oSRF_Info                      , &
                                   oSRF_Integrate                 , &
                                   oSRF_Central_Frequency         , &
                                   oSRF_Planck_Coefficients       , &
                                   oSRF_Polychromatic_Coefficients
  USE oSRF_Parameters
  USE netcdf
  ! Usage include files
  INCLUDE 'oSRF_Flag_Usage.inc'
  INCLUDE 'oSRF_Sensor_Usage.inc'
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Derived type definitions
  PUBLIC :: oSRF_File_type
  ! Public procedures
  PUBLIC :: oSRF_File_Associated
  PUBLIC :: oSRF_File_Destroy
  PUBLIC :: oSRF_File_Create
  PUBLIC :: oSRF_File_SetValue
  PUBLIC :: oSRF_File_GetValue
  PUBLIC :: oSRF_File_Inspect
  PUBLIC :: oSRF_File_AddTo
  PUBLIC :: oSRF_File_GetFrom
  PUBLIC :: oSRF_File_Info
  PUBLIC :: oSRF_File_Read
  PUBLIC :: oSRF_File_Write
  PUBLIC :: oSRF_File_DefineVersion


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module version id
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Invalid flag
  INTEGER, PARAMETER :: INVALID = -1
  ! Keyword set values
  INTEGER, PARAMETER :: UNSET = 0
  INTEGER, PARAMETER ::   SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER ::  ONE = 1.0_fp
  ! String lengths
  INTEGER, PARAMETER :: SL = 20    ! Sensor id length
  INTEGER, PARAMETER :: ML = 512   ! Message length
  INTEGER, PARAMETER :: FL = 512   ! Filename length
  INTEGER, PARAMETER :: GL = 5000  ! Global attribute string length

  ! Global attribute names. Case sensitive
  CHARACTER(*), PARAMETER :: TITLE_GATTNAME            = 'title' 
  CHARACTER(*), PARAMETER :: HISTORY_GATTNAME          = 'history' 
  CHARACTER(*), PARAMETER :: COMMENT_GATTNAME          = 'comment' 
  CHARACTER(*), PARAMETER :: RELEASE_GATTNAME          = 'Release'
  CHARACTER(*), PARAMETER :: VERSION_GATTNAME          = 'Version'
  CHARACTER(*), PARAMETER :: SENSOR_ID_GATTNAME        = 'Sensor_Id'
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_GATTNAME      = 'Sensor_Type'
  CHARACTER(*), PARAMETER :: WMO_SATELLITE_ID_GATTNAME = 'WMO_Satellite_Id'
  CHARACTER(*), PARAMETER :: WMO_SENSOR_ID_GATTNAME    = 'WMO_Sensor_Id'
  
  ! Dimension names. Case sensitive
  CHARACTER(*), PARAMETER :: CHANNEL_DIMNAME              = 'n_Channels'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_DIMNAME        = 'n_Planck_Coeffs'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_DIMNAME = 'n_Polychromatic_Coeffs'
  CHARACTER(*), PARAMETER :: TEMPERATURES_DIMNAME         = 'n_Temperatures'

  ! Variable names. Case sensitive.
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_VARNAME       = 'Sensor_Channel'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_VARNAME       = 'Integrated_SRF'
  CHARACTER(*), PARAMETER :: FLAGS_VARNAME                = 'Flags'
  CHARACTER(*), PARAMETER :: CENTRAL_FREQUENCY_VARNAME    = 'Central_Frequency'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_VARNAME        = 'Planck_Coeffs'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_VARNAME = 'Polychromatic_Coeffs'
 
  ! Variable long name attribute.
  CHARACTER(*), PARAMETER :: LONGNAME_ATTNAME = 'long_name'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_LONGNAME       = 'Sensor Channel'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_LONGNAME       = 'Integrated SRF value'
  CHARACTER(*), PARAMETER :: FLAGS_LONGNAME                = 'Processing Bit Flags'
  CHARACTER(*), PARAMETER :: CENTRAL_FREQUENCY_LONGNAME    = 'Central Frequency'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_LONGNAME        = 'Planck Coefficients'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_LONGNAME = 'Polychromatic Correction Coefficients'
  CHARACTER(*), PARAMETER :: F1_LONGNAME                   = 'Band Begin Frequency'
  CHARACTER(*), PARAMETER :: F2_LONGNAME                   = 'Band End Frequency'
  CHARACTER(*), PARAMETER :: N_POINTS_LONGNAME             = 'Number of band spectral points'
  CHARACTER(*), PARAMETER :: FREQUENCY_LONGNAME            = 'Band Frequency'
  CHARACTER(*), PARAMETER :: RESPONSE_LONGNAME             = 'Band Relative Response'
  CHARACTER(*), PARAMETER :: TEMPERATURE_LONGNAME          = 'Temperature'
  CHARACTER(*), PARAMETER :: EFF_TEMPERATURE_LONGNAME      = 'Effective Temperature'
  CHARACTER(*), PARAMETER :: FIT_TEMPERATURE_LONGNAME      = 'Fit to Effective Temperature'


  ! Variable description attribute.
  CHARACTER(*), PARAMETER :: DESCRIPTION_ATTNAME = 'description'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_DESCRIPTION       = 'List of sensor channel numbers'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_DESCRIPTION       = 'Integral of SRF for normalisation'
  CHARACTER(*), PARAMETER :: FLAGS_DESCRIPTION                = 'Bit Flags set during SRF processing'
  CHARACTER(*), PARAMETER :: CENTRAL_FREQUENCY_DESCRIPTION    = 'First moment of the SRF'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_DESCRIPTION        = 'Planck function coefficients'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_DESCRIPTION = 'Correction coefficients for non-monochromatic bandwidths'
  CHARACTER(*), PARAMETER :: F1_DESCRIPTION                   = 'Band Begin Frequency'
  CHARACTER(*), PARAMETER :: F2_DESCRIPTION                   = 'Band End Frequency'
  CHARACTER(*), PARAMETER :: N_POINTS_DESCRIPTION             = 'Number of spectral points in a band'
  CHARACTER(*), PARAMETER :: FREQUENCY_DESCRIPTION            = 'Spectral ordinate for channel responses'
  CHARACTER(*), PARAMETER :: RESPONSE_DESCRIPTION             = 'Relative Spectral Response Function (SRF)'
  CHARACTER(*), PARAMETER :: TEMPERATURE_DESCRIPTION          = 'True Temperature'
  CHARACTER(*), PARAMETER :: EFF_TEMPERATURE_DESCRIPTION      = 'Effective temperature due to band polychromaticity'
  CHARACTER(*), PARAMETER :: FIT_TEMPERATURE_DESCRIPTION      = 'Polynomial fit to Teff=f(T) data'


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: FLAGS_UNITS                = 'N/A'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_UNITS        = '[W.m^2, K.m]'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_UNITS = '[K, K/K]'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS            = 'Inverse centimetres (cm^-1)'
  CHARACTER(*), PARAMETER :: RESPONSE_UNITS             = 'N/A'
  CHARACTER(*), PARAMETER :: TEMPERATURE_UNITS          = 'Kelvin'
  

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER,  PARAMETER :: SENSOR_CHANNEL_FILLVALUE       = INVALID
  REAL(fp), PARAMETER :: INTEGRATED_SRF_FILLVALUE       = ZERO
  INTEGER,  PARAMETER :: FLAGS_FILLVALUE                = -1
  REAL(fp), PARAMETER :: PLANCK_COEFFS_FILLVALUE        = ZERO
  REAL(fp), PARAMETER :: POLYCHROMATIC_COEFFS_FILLVALUE = ZERO
  REAL(fp), PARAMETER :: FREQUENCY_FILLVALUE            = ZERO
  REAL(fp), PARAMETER :: RESPONSE_FILLVALUE             = ZERO
  REAL(fp), PARAMETER :: TEMPERATURE_FILLVALUE          = ZERO
  

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE       = NF90_INT
  INTEGER, PARAMETER :: INTEGRATED_SRF_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: FLAGS_TYPE                = NF90_INT
  INTEGER, PARAMETER :: PLANCK_COEFFS_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: POLYCHROMATIC_COEFFS_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE            = NF90_DOUBLE
  INTEGER, PARAMETER :: RESPONSE_TYPE             = NF90_DOUBLE
  INTEGER, PARAMETER :: TEMPERATURE_TYPE          = NF90_DOUBLE


  ! -------------------------------
  ! oSRF_File data type definitions
  ! -------------------------------
  TYPE :: oSRF_File_type
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Filename from which any contained oSRFs were read
    CHARACTER(FL) :: Filename
    ! Release and version information
    INTEGER :: Release = OSRF_RELEASE
    INTEGER :: Version = OSRF_VERSION
    ! Dimension values
    INTEGER :: n_Channels = 0
    ! Channel independent data
    CHARACTER(SL) :: Sensor_ID        = ''
    INTEGER       :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER       :: Sensor_Type      = INVALID_SENSOR
    ! File global attributes
    CHARACTER(GL) :: Title   = ''
    CHARACTER(GL) :: History = ''
    CHARACTER(GL) :: Comment = ''
    ! Channel oSRF data
    TYPE(oSRF_type), ALLOCATABLE :: oSRF(:) ! n_Channels
  END TYPE oSRF_File_type


CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PUBLIC MODULE ROUTINES ##                        ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Associated
! 
! PURPOSE:
!       Elemental function to test if an oSRF_File container has been allocated.
!
! CALLING SEQUENCE:
!       Allocation_Status = oSRF_File_Associated( oSRF_File )
!
! OBJECT:
!       oSRF_File:    oSRF_File structure which is to have its allocatable
!                     member's status tested.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(in)
!
! FUNCTION RESULT:
!       Allocation_Status:   The return value is a logical value indicating the
!                            allocation status of the requisite members.
!                            .TRUE.  - if the structure allocatable members are
!                                      allocated.
!                            .FALSE. - some or all of the oSRF_File allocatable
!                                      members are NOT allocated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_File_Associated(self) RESULT(alloc_status)
    TYPE(oSRF_File_type), INTENT(IN) :: self
    LOGICAL :: alloc_status
    alloc_status = self%Is_Allocated
  END FUNCTION oSRF_File_Associated


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Destroy
! 
! PURPOSE:
!       Elemental Subroutine to destroy an oSRF_File container and all
!       of its contents.
!
! CALLING SEQUENCE:
!       CALL oSRF_File_Destroy(oSRF_File)
!
! OBJECT:
!       oSRF_File:    oSRF_File structure to destroy.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_File_Destroy(self)
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Channels = 0
  END SUBROUTINE oSRF_File_Destroy


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Create
! 
! PURPOSE:
!       Elemental function to create an instance of an oSRF_File container.
!
! CALLING SEQUENCE:
!       CALL oSRF_File_Create(oSRF_File, n_Channels)
!
! OBJECT:
!       oSRF_File:    oSRF_File structure with allocated members
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Channels:   The number of channels of data to be placed in the
!                     container
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_File_Create(self, n)
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    INTEGER,              INTENT(IN)  :: n
    ! Local variables
    INTEGER :: alloc_status

    ! Check dimension inputs
    IF ( n < 1 ) RETURN
    
    ! Allocate
    ALLOCATE( self%oSRF(n), STAT=alloc_status )
    IF ( alloc_status /= 0 ) RETURN

    ! Initialise
    self%n_Channels = n

    ! Set the allocation flag
    self%Is_Allocated = .TRUE.
     
  END SUBROUTINE oSRF_File_Create
  

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_SetValue
!
! PURPOSE:
!       Function to set the value of an oSRF_File container object component.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_SetValue( &
!                        oSRF_File                          , &
!                        Filename         = Filename        , &
!                        Version          = Version         , &
!                        Sensor_Id        = Sensor_Id       , &
!                        WMO_Satellite_Id = WMO_Satellite_Id, &
!                        WMO_Sensor_Id    = WMO_Sensor_Id   , &
!                        Sensor_Type      = Sensor_Type     , &
!                        Title            = Title           , &
!                        History          = History         , &
!                        Comment          = Comment           )
!
! OBJECT:
!       oSRF_File:          oSRF_File object which is to have its
!                           properties modifed.
!                           UNITS:      N/A
!                           TYPE:       TYPE(oSRF_File_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN OUT)
!
! OPTIONAL INPUTS:
!       Filename:           A character string identifying the filename with
!                           which the oSRF_File container is associated.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:            The version number of the SRF data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_ID:          A character string identifying the sensor and
!                           satellite platform used to contruct filenames.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Title:              Character string for the TITLE global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       History:            Character string for the HISTORY global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Comment:            Character string for the COMMENT global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the object property set was successful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_SetValue( &
    self            , &  ! In/output
    Filename        , &  ! Optional input
    Version         , &  ! Optional input
    Sensor_Id       , &  ! Optional input
    WMO_Satellite_Id, &  ! Optional input
    WMO_Sensor_Id   , &  ! Optional input
    Sensor_Type     , &  ! Optional input
    Title           , &  ! Optional input
    History         , &  ! Optional input
    Comment         ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type),   INTENT(IN OUT) :: self
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Filename            
    INTEGER,      OPTIONAL, INTENT(IN)     :: Version             
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Sensor_Id           
    INTEGER,      OPTIONAL, INTENT(IN)     :: WMO_Satellite_Id    
    INTEGER,      OPTIONAL, INTENT(IN)     :: WMO_Sensor_Id       
    INTEGER,      OPTIONAL, INTENT(IN)     :: Sensor_Type         
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Title             
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: History           
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Comment           
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File_SetValue'
    ! Set up
    err_stat = SUCCESS
    ! Set property data
    IF ( PRESENT(Filename        ) ) self%Filename         = ADJUSTL(Filename)
    IF ( PRESENT(Version         ) ) self%Version          = Version         
    IF ( PRESENT(Sensor_Id       ) ) self%Sensor_Id        = ADJUSTL(Sensor_Id)
    IF ( PRESENT(WMO_Satellite_Id) ) self%WMO_Satellite_Id = WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) self%WMO_Sensor_Id    = WMO_Sensor_Id   
    IF ( PRESENT(Sensor_Type     ) ) self%Sensor_Type      = Sensor_Type     
    IF ( PRESENT(Title           ) ) self%Title            = ADJUSTL(Title)
    IF ( PRESENT(History         ) ) self%History          = ADJUSTL(History)
    IF ( PRESENT(Comment         ) ) self%Comment          = ADJUSTL(Comment)
  END FUNCTION oSRF_File_SetValue


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_GetValue
!
! PURPOSE:
!       Function to get the value of an oSRF_File container object component.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_GetValue( &
!                        oSRF_File                          , & 
!                        n_Channels       = n_Channels      , & 
!                        Filename         = Filename        , & 
!                        Version          = Version         , & 
!                        Sensor_Id        = Sensor_Id       , & 
!                        WMO_Satellite_Id = WMO_Satellite_Id, & 
!                        WMO_Sensor_Id    = WMO_Sensor_Id   , & 
!                        Sensor_Type      = Sensor_Type     , & 
!                        Title            = Title           , & 
!                        History          = History         , & 
!                        Comment          = Comment           ) 
!
! OBJECT:
!       oSRF_File:          oSRF_File object that is to have its properties
!                           retrieved.
!                           UNITS:      N/A
!                           TYPE:       TYPE(oSRF_File_type)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       n_Channels:         The number of channels the container has been
!                           allocated to hold.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Filename:           A character string identifying the filename with
!                           which the oSRF_File container is associated.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Version:            The version number of the SRF data.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_ID:          A character string identifying the sensor and
!                           satellite platform used to contruct filenames.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_ID:      The WMO code used to identify sensors.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Title:              Character string written into the TITLE global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       History:            Character string written into the HISTORY global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Comment:            Character string written into the COMMENT global
!                           attribute field of the oSRF_File container.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           Message_Handler module.
!                           If == SUCCESS the object property retrieval was successful
!                              == FAILURE an unrecoverable error occurred
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_GetValue( &
    self            , &  ! In/output
    n_Channels      , &  ! Optional output
    Filename        , &  ! Optional output
    Version         , &  ! Optional output
    Sensor_Id       , &  ! Optional output
    WMO_Satellite_Id, &  ! Optional output
    WMO_Sensor_Id   , &  ! Optional output
    Sensor_Type     , &  ! Optional output
    Title           , &  ! Optional output
    History         , &  ! Optional output
    Comment         ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type),   INTENT(IN)  :: self
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Channels         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Filename            
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version             
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id           
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id    
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id       
    INTEGER,      OPTIONAL, INTENT(OUT) :: Sensor_Type         
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Title             
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: History           
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Comment           
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File_GetValue'
    ! Set up
    err_stat = SUCCESS
    ! Get property data
    IF ( PRESENT(n_Channels      ) ) n_Channels       = self%n_Channels         
    IF ( PRESENT(Filename        ) ) Filename         = self%Filename        
    IF ( PRESENT(Version         ) ) Version          = self%Version         
    IF ( PRESENT(Sensor_Id       ) ) Sensor_Id        = self%Sensor_Id       
    IF ( PRESENT(WMO_Satellite_Id) ) WMO_Satellite_Id = self%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id   ) ) WMO_Sensor_Id    = self%WMO_Sensor_Id   
    IF ( PRESENT(Sensor_Type     ) ) Sensor_Type      = self%Sensor_Type     
    IF ( PRESENT(Title           ) ) Title            = self%Title           
    IF ( PRESENT(History         ) ) History          = self%History         
    IF ( PRESENT(Comment         ) ) Comment          = self%Comment         
  END FUNCTION oSRF_File_GetValue


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Inspect
!
! PURPOSE:
!       Subroutine to inspect the contents of an oSRF_File container.
!
! CALLING SEQUENCE:
!       CALL oSRF_File_Inspect( oSRF_File )
!
! OBJECT:
!       oSRF_File:    oSRF_File structure to inspect.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       oSRF:         oSRF structure to add to the oSRF_File container.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  SUBROUTINE oSRF_File_Inspect( self )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    ! Local arguments
    INTEGER :: n
    ! Output the oSRF_File components     
    WRITE(*,'(1x,"oSRF_File OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_Channels       :",1x,i0)') self%n_Channels
    ! Scalar data
    WRITE(*,'(3x,"Filename         : ",  a)' ) TRIM(self%Filename)
    WRITE(*,'(3x,"Release          : ", i0)' ) self%Release
    WRITE(*,'(3x,"Version          : ", i0)' ) self%Version  
    WRITE(*,'(3x,"Sensor_Id        : ",  a)' ) TRIM(self%Sensor_Id)       
    WRITE(*,'(3x,"WMO_Satellite_Id : ", i0)' ) self%WMO_Satellite_Id
    WRITE(*,'(3x,"WMO_Sensor_Id    : ", i0)' ) self%WMO_Sensor_Id   
    WRITE(*,'(3x,"Sensor_Type      : ",  a)' ) SENSOR_TYPE_NAME(self%Sensor_Type)
    WRITE(*,'(3x,"Title            : ",  a)' ) TRIM(self%Title  )
    WRITE(*,'(3x,"History          : ",  a)' ) TRIM(self%History)
    WRITE(*,'(3x,"Comment          : ",  a)' ) TRIM(self%Comment)
    IF ( self%n_Channels == 0 ) RETURN
    DO n = 1, self%n_Channels
      WRITE(*,'(3x,"OSRF POSITION NUMBER ",i0)' ) n
      CALL oSRF_Inspect( self%oSRF(n) )
    END DO
  END SUBROUTINE oSRF_File_Inspect
  
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_AddTo
!
! PURPOSE:
!       Function to add an oSRF object to an oSRF_File container.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_AddTo( &
!                        oSRF_File, &
!                        oSRF     , &
!                        pos = pos  )
!
! OBJECT:
!       oSRF_File:    oSRF_File object to which the oSRF object
!                     is added.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN OUT)
!
! INPUTS:
!       oSRF:         oSRF object to add to the oSRF_File container.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       pos:          Set this keyword to the index of the position at which
!                     oSRF object is to be added to the oSRF_File container.
!                     If not supplied the default is to add the object to the
!                     start of the list of contained objects.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the object add was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_AddTo( &
    self, &  ! In/output
    oSRF, &  ! Input
    pos ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN OUT) :: self
    TYPE(oSRF_type),      INTENT(IN)     :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)     :: pos
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::AddTo'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_pos
    
    ! Set up
    err_stat = SUCCESS
    
    ! Check position argument
    IF ( PRESENT(pos) ) THEN
      l_pos = pos
    ELSE
      l_pos = 1
    END IF
    IF ( l_pos < 1 .OR. l_pos > self%n_Channels ) THEN
      WRITE( msg,'("Invalid position, ",i0,", specified for oSRF_File")' ) l_pos
      CALL CleanUp(); RETURN
    END IF
    
    ! Copy oSRF to required position
    IF ( oSRF_File_Associated(self) ) THEN
      self%oSRF(l_pos) = oSRF
      IF ( .NOT. oSRF_Associated(self%oSRF(l_pos)) ) THEN
        WRITE( msg,'("Error adding oSRF at position ",i0," in oSRF_File")' ) l_pos
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_stat )
    END SUBROUTINE CleanUp
    
  END FUNCTION oSRF_File_AddTo


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_GetFrom
!
! PURPOSE:
!       Function to retrieve an oSRF object from an oSRF_File container.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_GetFrom( &
!                        oSRF_File, &
!                        oSRF     , &
!                        pos = pos  )
!
! OBJECT:
!       oSRF_File:    oSRF_File container object from which an oSRF object
!                     is requested.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       oSRF:         oSRF object retrieved from the oSRF_File container.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUTS:
!       pos:          Set this keyword to the 1-based index of the position of
!                     the oSRF object to retrieve from the oSRF_File container.
!                     If not supplied the default is to return the first oSRF
!                     object.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the object retrieval was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_GetFrom( &
    self, &  ! Input
    oSRF, &  ! Output
    pos ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN)  :: self
    TYPE(oSRF_type),      INTENT(OUT) :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)  :: pos
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::GetFrom'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_pos
    
    ! Set up
    err_stat = SUCCESS
    
    ! Check position argument
    IF ( PRESENT(pos) ) THEN
      l_pos = pos
    ELSE
      l_pos = 1
    END IF
    IF ( l_pos < 1 .OR. l_pos > self%n_Channels ) THEN
      WRITE( msg,'("Invalid position, ",i0,", specified for oSRF_File")' ) l_pos
      CALL CleanUp(); RETURN
    END IF
    
    ! Copy oSRF from requested position
    IF ( oSRF_File_Associated(self) ) THEN
      oSRF = self%oSRF(l_pos)
      IF ( .NOT. oSRF_Associated(oSRF) ) THEN
        WRITE( msg,'("Error getting oSRF from position ",i0," in oSRF_File")' ) l_pos
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      CALL oSRF_Destroy( oSRF )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE CleanUp
    
  END FUNCTION oSRF_File_GetFrom


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the oSRF_File object.
!
! CALLING SEQUENCE:
!       CALL oSRF_File_Info( oSRF_File, Info )
!
! OBJECT:
!       oSRF_File:     oSRF_File structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_File_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Info:          String containing version and dimension information
!                      about the passed oSRF data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE oSRF_File_Info( self, Info )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN)  :: self
    CHARACTER(*)        , INTENT(OUT) :: Info
    ! Local parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Setup
    Info = ' '
    IF ( .NOT. oSRF_File_Associated(self) ) RETURN
    
    
    ! Write the required data to the local string
    WRITE( LongString,'(a,1x,"oSRF_File RELEASE.VERSION: ",i0,".",i2.2,2x,a,1x,&
                       &"N_CHANNELS=",i0)' ) &
                       ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                       self%Release, self%Version, &
                       TRIM(self%Sensor_ID), &
                       self%n_Channels


    ! Trim the output based on the
    ! dummy argument string length
    Info = LongString(1:MIN(LEN(Info), LEN_TRIM(LongString)))

  END SUBROUTINE oSRF_File_Info


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL oSRF_File_DefineVersion( Id )
!
! OUTPUTS:
!       Id:    Character string containing the version Id information
!              for the module.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE oSRF_File_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE oSRF_File_DefineVersion


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Read
!
! PURPOSE:
!       Function to read oSRF data from file and fill the oSRF_File
!       container with all the oSRF objects in the file.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_Read( &
!                        oSRF_File    , &
!                        Filename     , &
!                        Quiet = Quiet  )
!
! OBJECT:
!       oSRF_File:    oSRF_File object container to fill with data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     oSRF_File data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     output.
!                     If QUIET = .FALSE., info messages are OUTPUT. [*DEFAULT*]
!                        QUIET = .TRUE.,  info messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the data read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_Read( &
    self    , &  ! Output
    Filename, &  ! Input
    Quiet   ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    CHARACTER(*),         INTENT(IN)  :: Filename
    LOGICAL,    OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: alloc_msg
    CHARACTER(ML) :: sensor_id       
    CHARACTER(ML) :: n_bands_dimname
    CHARACTER(ML) :: n_points_dimname
    CHARACTER(ML) :: n_temperatures_dimname
    CHARACTER(ML) :: frequency_varname
    CHARACTER(ML) :: response_varname
    CHARACTER(ML) :: t_varname             
    CHARACTER(ML) :: teff_varname          
    CHARACTER(ML) :: tfit_varname
    LOGICAL :: noisy
    LOGICAL :: close_file
    INTEGER :: fileid
    INTEGER :: nc_stat
    INTEGER :: alloc_stat
    INTEGER :: version         
    INTEGER :: wmo_satellite_id
    INTEGER :: wmo_sensor_id   
    INTEGER :: sensor_type
    INTEGER :: varid
    INTEGER :: n, n_channels
    INTEGER :: i, n_bands
    INTEGER :: n_planck_coeffs
    INTEGER :: n_polychromatic_coeffs
    INTEGER :: n_temperatures
    INTEGER :: channel_varid
    INTEGER :: channel
    INTEGER :: Flags
    REAL(fp) :: integral, integral_test
    REAL(fp) :: f0, f0_test
    INTEGER , ALLOCATABLE :: n_points(:) 
    REAL(fp), ALLOCATABLE :: planck_coeffs(:), planck_coeffs_test(:)
    REAL(fp), ALLOCATABLE :: polychromatic_coeffs(:), polychromatic_coeffs_test(:)
    REAL(fp), ALLOCATABLE :: frequency(:)
    REAL(fp), ALLOCATABLE :: response(:)
    REAL(fp), ALLOCATABLE :: temperature(:)
    TYPE(oSRF_type) :: oSRF    
    TYPE(oSRF_type) :: osrf_test

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Determine info message output
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Open the file for reading
    nc_stat = NF90_OPEN( Filename, NF90_NOWRITE, fileid )
    IF ( nc_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR(nc_stat))
      CALL Cleanup(); RETURN
    END IF
    close_file = .TRUE.
    
    
    ! Get the number of channels dimension
    err_stat = Read_Dim( fileid, CHANNEL_DIMNAME, n_channels )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Allocate the output container
    CALL oSRF_File_Create( self, n_channels )    
    IF ( .NOT. oSRF_File_Associated( self ) ) THEN
      msg = 'Error allocating oSRF_File output'
      CALL Cleanup(); RETURN
    END IF
    ! ...Set the filename
    err_stat = oSRF_File_SetValue( self, Filename = Filename )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error setting filename property in oSRF_File'
      CALL Cleanup(); RETURN
    END IF
    
    
    ! Read the global attributes
    err_stat = ReadGAtts( self, fileid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ! ...Save them to local variables for insertion into oSRF objects
    err_stat = oSRF_File_GetValue( &
      self, &
      version          = version         , &
      sensor_id        = sensor_id       , &
      wmo_satellite_id = wmo_satellite_id, &
      wmo_sensor_id    = wmo_sensor_id   , &
      sensor_type      = sensor_type       )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error getting GAtt properties from oSRF_File object.'
      CALL Cleanup(); RETURN
    END IF


    ! Read the channel independent dimensions and allocate local arrays
    ! ...The Planck coefficients
    err_stat = Read_Dim( fileid, PLANCK_COEFFS_DIMNAME, n_planck_coeffs )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading '//PLANCK_COEFFS_DIMNAME//' dimension from '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ALLOCATE(planck_coeffs(n_planck_coeffs), &
             planck_coeffs_test(n_planck_coeffs), &
             STAT=alloc_stat, ERRMSG=alloc_msg)
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating local '//PLANCK_COEFFS_VARNAME//' arrays - '//TRIM(alloc_msg)
      CALL Cleanup(); RETURN
    END IF
    ! ...The polychromatic coefficients
    err_stat = Read_Dim( fileid, POLYCHROMATIC_COEFFS_DIMNAME, n_polychromatic_coeffs )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading '//POLYCHROMATIC_COEFFS_DIMNAME//' dimension from '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ALLOCATE(polychromatic_coeffs(n_polychromatic_coeffs), &
             polychromatic_coeffs_test(n_polychromatic_coeffs), &
             STAT=alloc_stat, ERRMSG=alloc_msg)
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating local '//POLYCHROMATIC_COEFFS_VARNAME//' array - '//TRIM(alloc_msg)
      CALL Cleanup(); RETURN
    END IF


    ! Get the sensor channel variable id
    nc_stat = NF90_INQ_VARID( fileid, SENSOR_CHANNEL_VARNAME, channel_varid )
    IF ( nc_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
      CALL Cleanup(); RETURN
    END IF


    ! Loop over the number of channels 
    Channel_Loop: DO n = 1, n_channels
    
      ! Read the current channel number
      nc_stat = NF90_GET_VAR( fileid, channel_variD, channel, START=[ n ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading sensor channel from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF


      ! Create the band dimension name
      CALL Create_Names( channel, n_Bands_DimName = n_Bands_DimName )
        
        
      ! Read the current channel dimensions
      err_stat = Read_Dim( fileid, TRIM(n_Bands_DimName), n_Bands )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading '//TRIM(n_Bands_DimName)//' dimension from '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      
      
      ! Allocate the current channel oSRF
      ! ...Allocate the n_Points array
      ALLOCATE( n_points(n_bands), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg, '("Error allocating n_Points array for channel ",i0," - ",a)' ) &
                    channel, TRIM(alloc_msg)
        CALL Cleanup(); RETURN
      END IF
      ! ...Get the number of points for each band
      DO i = 1, n_Bands
        ! ...Create the dimension name
        CALL Create_Names( channel, Band = i, n_Points_DimName = n_Points_DimName )
        ! ...Read the dimension value
        err_stat = Read_Dim( fileid, TRIM(n_Points_DimName), n_Points(i) )
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error reading '//TRIM(n_Points_DimName)//' dimension from '//TRIM(Filename)
          CALL Cleanup(); RETURN
        END IF
      END DO
      ! ...Create the current oSRF object
      CALL oSRF_Create( oSRF, n_Bands )
      IF ( .NOT. oSRF_Associated( oSRF ) ) THEN
        WRITE( msg, '("Error creating oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      ! ...Add current properties
      err_stat = oSRF_SetValue( &
        oSRF, &
        channel          = channel         , &
        version          = version         , &
        sensor_id        = sensor_id       , &
        wmo_satellite_id = wmo_satellite_id, &
        wmo_sensor_id    = wmo_sensor_id   , &
        sensor_type      = sensor_type       )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error setting general properties of oSRF for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      
      
      ! Read the channel dependent data
      ! ...The integrated SRF value
      nc_stat = NF90_INQ_VARID( fileid, INTEGRATED_SRF_VARNAME, varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//INTEGRATED_SRF_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, VariD, integral, START=[ n ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//INTEGRATED_SRF_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Integral = integral )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//INTEGRATED_SRF_VARNAME//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...The processing flags
      nc_stat = NF90_INQ_VARID( fileid, FLAGS_VARNAME, varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//FLAGS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, VariD, Flags, START=[ n ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//FLAGS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Flags = Flags )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//FLAGS_VARNAME//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...The central frequency
      nc_stat = NF90_INQ_VARID( fileid, CENTRAL_FREQUENCY_VARNAME, varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//CENTRAL_FREQUENCY_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, VariD, f0, START=[ n ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//CENTRAL_FREQUENCY_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, f0 = f0 )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//CENTRAL_FREQUENCY_VARNAME//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...The Planck coefficients
      nc_stat = NF90_INQ_VARID( fileid, PLANCK_COEFFS_VARNAME, varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, VariD, planck_coeffs, &
                              START=[ 1,n ], &
                              COUNT=[ n_planck_coeffs,1 ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//PLANCK_COEFFS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Planck_Coeffs = planck_coeffs )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//PLANCK_COEFFS_VARNAME//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...The Polychromatic coefficients
      nc_stat = NF90_INQ_VARID( fileid, POLYCHROMATIC_COEFFS_VARNAME, varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//POLYCHROMATIC_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, VariD, polychromatic_coeffs, &
                              START=[ 1,n ], &
                              COUNT=[ n_polychromatic_coeffs,1 ] )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//POLYCHROMATIC_COEFFS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Polychromatic_Coeffs = polychromatic_coeffs )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//POLYCHROMATIC_COEFFS_VARNAME//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      
      
      ! Read the temperature data
      ! ...Create the dimension and variable names
      CALL Create_Names( &
         channel, &
         n_Temperatures_DimName = n_temperatures_dimname, &
         T_VarName              = t_varname             , &
         Teff_VarName           = teff_varname          , &
         Tfit_VarName           = tfit_varname            )
      ! ...Read the dimension value
      err_stat = Read_Dim( fileid, TRIM(n_temperatures_dimname), n_temperatures )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error reading '//TRIM(n_temperatures_dimname)//' dimension from '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      ! ...Allocate the temperature array
      ALLOCATE( temperature(n_temperatures), STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg, '("Error allocating temperature array for channel ",i0," - ",a)' ) &
                    channel, TRIM(alloc_msg)
        CALL Cleanup(); RETURN
      END IF
      ! ...Read the temperature data and add to object
      nc_stat = NF90_INQ_VARID( fileid, TRIM(t_varname), varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(t_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, varid, temperature )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//TRIM(t_varname)//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Temperature=temperature )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//TRIM(t_varname)//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...Read the effective temperature data and add to object
      nc_stat = NF90_INQ_VARID( fileid, TRIM(teff_varname), varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(teff_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, varid, temperature )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//TRIM(teff_varname)//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Effective_Temperature=temperature )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//TRIM(teff_varname)//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF
      ! ...Read the fit temperature data and add to object
      nc_stat = NF90_INQ_VARID( fileid, TRIM(tfit_varname), varid )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(tfit_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      nc_stat = NF90_GET_VAR( fileid, varid, temperature )
      IF ( nc_stat /= NF90_NOERR ) THEN
        msg = 'Error reading '//TRIM(tfit_varname)//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_stat))
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_SetValue( oSRF, Fit_Temperature=temperature )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error setting '//TRIM(tfit_varname)//' property in oSRF object'
        CALL Cleanup(); RETURN
      END IF


      ! Read the band dependent data
      Band_Loop: DO i = 1, n_Bands
        ! ...Create the dimension and variable names
        CALL Create_Names( &
           channel, &
           Band = i, &
           Frequency_VarName = Frequency_VarName, &
           Response_VarName  = Response_VarName   )
        ! ...Allocate the data array
        ALLOCATE( frequency(n_Points(i)), response(n_Points(i)), &
                  STAT=alloc_stat, ERRMSG=alloc_msg )
        IF ( alloc_stat /= 0 ) THEN
          WRITE( msg,'("Error allocating frequency/response array for channel ",i0,", band ",i0," - ",a)' ) &
                      channel, i, TRIM(alloc_msg)
          CALL Cleanup(); RETURN
        END IF
        ! ...Read the frequency data
        nc_stat = NF90_INQ_VARID( fileid, TRIM(Frequency_VarName), varid )
        IF ( nc_stat /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Frequency_VarName)//&
                ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
          CALL Cleanup(); RETURN
        END IF
        nc_stat = NF90_GET_VAR( fileid, VariD, frequency )
        IF ( nc_stat /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(Frequency_VarName)//' from'//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR(nc_stat))
          CALL Cleanup(); RETURN
        END IF
        ! ...Read the Response data
        nc_stat = NF90_INQ_VARID( fileid, TRIM(Response_VarName), varid )
        IF ( nc_stat /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Response_VarName)//&
                ' variable ID - '//TRIM(NF90_STRERROR(nc_stat))
          CALL Cleanup(); RETURN
        END IF
        nc_stat = NF90_GET_VAR( fileid, VariD, response )
        IF ( nc_stat /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(Response_VarName)//' from'//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR(nc_stat))
          CALL Cleanup(); RETURN
        END IF
        ! ...Set the data
        err_stat = oSRF_SetValue( oSRF, Band=i, Frequency=frequency, Response=response )
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error setting '//TRIM(Frequency_VarName)//' and '//TRIM(Response_VarName)//&
                ' property in oSRF object'
          CALL Cleanup(); RETURN
        END IF
        ! ...Deallocate the data array
        DEALLOCATE( frequency, response, STAT=alloc_stat, ERRMSG=alloc_msg )
        IF ( alloc_stat /= 0 ) THEN
          WRITE( msg, '("Error deallocating frequency and response arrays for channel ",i0,&
                       &", band ",i0," - ",a)' ) channel, i, TRIM(alloc_msg)
          CALL Cleanup(); RETURN
        END IF
      END DO Band_Loop


      ! Copy the oSRF, reprocess, and compare results
      osrf_test = oSRF
      ! ...Compute the integral and compare
      err_stat = oSRF_Integrate( osrf_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error integrating test oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_GetValue( osrf_test, Integral = integral_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving integrated value from test oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      IF ( ABS(Integral - integral_test) > INTEGRATE_TOLERANCE ) THEN
        WRITE(msg,'("Integral difference, channel ",i0," :",3(1x,es13.6))') &
                  channel, Integral, integral_test, Integral - integral_test
        CALL Display_Message(ROUTINE_NAME, msg, WARNING)
      END IF
      ! ...Compute the central frequency and compare
      err_stat = oSRF_Central_Frequency( osrf_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error computing channel ",i0," f0 for test oSRF object")' ) channel
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_GetValue( osrf_test, f0 = f0_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving f0 from test oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      IF ( ABS(f0 - f0_test) > F0_TOLERANCE ) THEN
        WRITE(msg,'("f0 difference, channel ",i0," :",3(1x,es13.6))') &
                  channel, f0, f0_test, f0 - f0_test
        CALL Display_Message(ROUTINE_NAME, msg, WARNING)
      END IF
      ! ...Compute the Planck coefficients and compare
      err_stat = oSRF_Planck_Coefficients( osrf_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error computing channel ",i0," Planck coefficients for test oSRF object")' ) channel
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_GetValue( osrf_test, Planck_Coeffs = planck_coeffs_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving Planck coefficients from test oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      DO i = 1, n_planck_coeffs
        IF ( ABS(planck_coeffs(i) - planck_coeffs_test(i)) > PLANCK_COEFF_TOLERANCE ) THEN
          WRITE(msg,'("Planck_Coeff(",i0,") difference, channel ",i0," :",3(1x,es13.6))') &
                    i, channel, planck_coeffs(i), planck_coeffs_test(i), planck_coeffs(i) - planck_coeffs_test(i)
          CALL Display_Message(ROUTINE_NAME, msg, WARNING)
        END IF
      END DO
      ! ...Compute the Polychromatic coefficients and compare
      err_stat = oSRF_Polychromatic_Coefficients( osrf_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error computing channel ",i0," polychromatic coefficients for test oSRF object")' ) channel
        CALL Cleanup(); RETURN
      END IF
      err_stat = oSRF_GetValue( osrf_test, Polychromatic_Coeffs = polychromatic_coeffs_test )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error retrieving polychromatic coefficients from test oSRF object for channel ",i0)' ) channel
        CALL Cleanup(); RETURN
      END IF
      DO i = 1, n_polychromatic_coeffs
        IF ( ABS(polychromatic_coeffs(i) - polychromatic_coeffs_test(i)) > POLY_COEFF_TOLERANCE ) THEN
          WRITE(msg,'("Polychromatic_Coeff(",i0,") difference, channel ",i0," :",3(1x,es13.6))') &
                    i, channel, polychromatic_coeffs(i), polychromatic_coeffs_test(i), &
                    polychromatic_coeffs(i) - polychromatic_coeffs_test(i)
          CALL Display_Message(ROUTINE_NAME, msg, WARNING)
        END IF
      END DO


      ! Add the oSRF to the OSRF_File object
      err_stat = oSRF_File_AddTo( self, oSRF, pos = n )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error adding channel ",i0," oSRF to oSRF_File container.")' ) channel
        CALL Cleanup(); RETURN
      END IF

      
      ! Clean up for next channels
      CALL oSRF_Destroy( osrf_test )
      CALL oSRF_Destroy( oSRF )
      DEALLOCATE( n_points, temperature, &
                  STAT=alloc_stat, ERRMSG=alloc_msg )
      IF ( alloc_stat /= 0 ) THEN
        WRITE( msg,'("Error deallocating arrays for channel ",i0," - ",a)' ) &
                    channel, TRIM(alloc_msg)
        CALL Cleanup(); RETURN
      END IF
      
    END DO Channel_Loop


    ! Clean up the rest (but don't exit if failure)
    DEALLOCATE( planck_coeffs, planck_coeffs_test, &
                polychromatic_coeffs, polychromatic_coeffs_test, &
                STAT=alloc_stat, ERRMSG=alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error deallocating local work arrays - '//TRIM(alloc_msg)
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF


    ! Close the file (but don't exit if failure)
    nc_stat = NF90_CLOSE( fileid )
    IF ( nc_stat /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR(nc_stat))
      CALL Display_Message( ROUTINE_NAME, msg, WARNING )
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      CALL oSRF_File_Info( self, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(msg), &
                            INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Cleanup()
      IF ( close_file ) THEN
        nc_stat = NF90_CLOSE(fileid)
        IF ( nc_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                TRIM(NF90_STRERROR(nc_stat))
      END IF
      CALL oSRF_File_Destroy( self )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Cleanup

  END FUNCTION oSRF_File_Read


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_File_Write
!
! PURPOSE:
!       Function to write oSRF data files given an oSRF_File
!       container.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_File_Write( &
!                        oSRF_File    , &
!                        Filename     , &
!                        Quiet = Quiet  )
!
! OBJECT:
!       oSRF_File:    oSRF_File object container filled with data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! INPUTS:
!       Filename:     Character string specifying the name of the
!                     oSRF_File data file to write.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     output.
!                     If QUIET = .FALSE., info messages are OUTPUT. [*DEFAULT*]
!                        QUIET = .TRUE.,  info messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       LOGICAL
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error
!                     status. The error codes are defined in the
!                     Message_Handler module.
!                     If == SUCCESS the data read was successful
!                        == FAILURE an unrecoverable error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_File_Write( &
    self    , & ! Input
    Filename, & ! Input
    Quiet   ) & ! Optional Input
  RESULT( err_stat )  
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    CHARACTER(*),         INTENT(IN) :: Filename
    LOGICAL,    OPTIONAL, INTENT(IN) :: Quiet
    ! Function result
    INTEGER :: err_stat
    ! Parameter
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Write'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local Variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: n_bands_dimname
    CHARACTER(ML) :: n_points_dimname
    CHARACTER(ML) :: n_temperatures_dimname
    CHARACTER(ML) :: f1_varname
    CHARACTER(ML) :: f2_varname
    CHARACTER(ML) :: frequency_varname
    CHARACTER(ML) :: response_varname
    CHARACTER(ML) :: t_varname             
    CHARACTER(ML) :: teff_varname          
    CHARACTER(ML) :: tfit_varname
    LOGICAL :: noisy
    LOGICAL :: close_file
    INTEGER :: fileid
    INTEGER :: varid
    INTEGER :: nc_stat(4)
    INTEGER :: n, i
    INTEGER :: channel_dimid
    INTEGER :: polychromatic_coeffs_dimid
    INTEGER :: planck_coeffs_dimid
    INTEGER :: temperature_dimid
    INTEGER :: band_dimid
    INTEGER :: n_points_dimid
    INTEGER :: n_bands
    INTEGER :: n_polychromatic_coeffs
    INTEGER :: n_temperatures
    INTEGER :: channel
    INTEGER :: flags
    REAL(fp) :: integral
    REAL(fp) :: f0
    REAL(fp) :: f1, f2
    INTEGER , ALLOCATABLE :: n_points(:)
    REAL(fp), ALLOCATABLE :: planck_coeffs(:)
    REAL(fp), ALLOCATABLE :: polychromatic_coeffs(:)
    REAL(fp), ALLOCATABLE :: temperature(:)
    REAL(fp), ALLOCATABLE :: eff_temperature(:)
    REAL(fp), ALLOCATABLE :: fit_temperature(:)
    REAL(fp), ALLOCATABLE :: frequency(:)
    REAL(fp), ALLOCATABLE :: response(:)
    
    TYPE(oSRF_type) :: osrf
     
    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Determine info message output
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet

    
    ! Set the channel independent dimensions values
    ! ...Grab the first oSRF object in the file container
    err_stat = osrf_File_GetFrom( self, osrf, pos = 1 )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error retrieving first oSRF from file container'
      CALL Cleanup(); RETURN
    END IF
    ! ...Now grab the dimension values  
    err_stat = oSRF_GetValue( osrf, &
                              n_Polychromatic_Coeffs = n_polychromatic_coeffs )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error retrieving channel-independent dimensions'
      CALL Cleanup(); RETURN
    END IF

    
    ! Create the output data file
    nc_stat(1) = NF90_CREATE( Filename, NF90_CLOBBER, fileid )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//&
             TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    close_file = .TRUE.

    
    ! Define the dimensions that have channel independent names
    ! ...The number of channels (duh!)
    nc_stat(1) = NF90_DEF_DIM( &
      fileid         , &
      CHANNEL_DIMNAME, &
      NF90_UNLIMITED , &
      channel_dimid    )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    ! ...The number of Planck coefficients
    nc_stat(1) = NF90_DEF_DIM( &
      fileid               , &
      PLANCK_COEFFS_DIMNAME, &
      N_PLANCK_COEFFS      , &
      planck_coeffs_dimid    )       
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_COEFFS_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF 
    ! ...The number of polychromatic correction coefficients
    nc_stat(1) = NF90_DEF_DIM( &
      fileid                      , &
      POLYCHROMATIC_COEFFS_DIMNAME, &
      n_polychromatic_coeffs      , &
      polychromatic_coeffs_dimid    )       
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLYCHROMATIC_COEFFS_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF

    
    ! Define the variables that have channel independent names
    ! ...The sensor channel list
    nc_stat(1) = NF90_DEF_VAR( &
      fileid                    , &
      SENSOR_CHANNEL_VARNAME    , &
      SENSOR_CHANNEL_TYPE       , &
      dimIDs = [ channel_dimid ], &
      varID  = varid              )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , SENSOR_CHANNEL_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, SENSOR_CHANNEL_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , SENSOR_CHANNEL_UNITS       )
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , SENSOR_CHANNEL_FILLVALUE   )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ! ...The integrated SRF value
    nc_stat(1) = NF90_DEF_VAR( &
      fileid                    , &
      INTEGRATED_SRF_VARNAME    , &
      INTEGRATED_SRF_TYPE       , &
      dimIDs = [ channel_dimid ], &
      varID  = varid              )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , INTEGRATED_SRF_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, INTEGRATED_SRF_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , INTEGRATED_SRF_UNITS       )
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , INTEGRATED_SRF_FILLVALUE   )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//INTEGRATED_SRF_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF   
    ! ...The processing bitflags
    nc_stat(1) = NF90_DEF_VAR( &
      FileID                    , &
      FLAGS_VARNAME             , &
      FLAGS_TYPE                , &
      dimIDs = [ channel_dimid ], &
      varID  = varid              )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//FLAGS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , FLAGS_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, FLAGS_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , FLAGS_UNITS       )
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , FLAGS_FILLVALUE   )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//FLAGS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF 
    ! ...The channel central frequency
    nc_stat(1) = NF90_DEF_VAR( &
      fileid                    , &
      CENTRAL_FREQUENCY_VARNAME , &
      FREQUENCY_TYPE            , &
      dimIDs = [ channel_dimid ], &
      varID  = varid              )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CENTRAL_FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , CENTRAL_FREQUENCY_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, CENTRAL_FREQUENCY_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , FREQUENCY_UNITS               ) 
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE           )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//CENTRAL_FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ! ...The channel Planck coefficients
    nc_stat(1) = NF90_DEF_VAR( &
      fileid                                         , &
      PLANCK_COEFFS_VARNAME                          , &
      PLANCK_COEFFS_TYPE                             , &
      dimIDs = [ planck_coeffs_dimid, channel_dimid ], &
      varID  = varid                                   )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_COEFFS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , PLANCK_COEFFS_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, PLANCK_COEFFS_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , PLANCK_COEFFS_UNITS       )
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , PLANCK_COEFFS_FILLVALUE   )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//PLANCK_COEFFS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    ! ...The channel Polychromatic coefficients
    nc_stat(1) = NF90_DEF_VAR( &
      fileid                                                , &
      POLYCHROMATIC_COEFFS_VARNAME                          , &
      POLYCHROMATIC_COEFFS_TYPE                             , &
      dimIDs = [ polychromatic_coeffs_dimid, channel_dimid ], &
      varID  = varid                                          )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLYCHROMATIC_COEFFS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
      CALL Cleanup(); RETURN
    END IF
    nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , POLYCHROMATIC_COEFFS_LONGNAME    )
    nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, POLYCHROMATIC_COEFFS_DESCRIPTION )
    nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , POLYCHROMATIC_COEFFS_UNITS       )
    nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , POLYCHROMATIC_COEFFS_FILLVALUE   )
    IF ( ANY(nc_stat /= SUCCESS) ) THEN
      msg = 'Error writing '//POLYCHROMATIC_COEFFS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! ===============================================================
    ! Define all the channel- and band-dependent dimensions/variables
    ! ===============================================================

    ! Begin loop over channels
    Define_Channel_Loop: DO n = 1, self%n_Channels


      ! Extract the current oSRF object from the file container
      err_stat = oSRF_File_GetFrom( self, osrf, pos = n )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error retrieving oSRF at index #",i0," from file container")') n
        CALL Cleanup(); RETURN
      END IF


      ! Get the required values for this channel
      err_stat = oSRF_GetValue( &
        osrf, &
        n_Bands        = n_bands       , &
        Channel        = channel       , &
        n_Points       = n_points      , &
        n_Temperatures = n_temperatures  )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error retrieving data from oSRF at index #",i0)') n
        CALL Cleanup(); RETURN
      END IF


      ! Create the channel-dependent dimension and variable names
      CALL Create_Names( &
        channel, &
        n_Bands_DimName        = n_bands_dimname       , &
        n_Temperatures_DimName = n_temperatures_dimname, &
        f1_Varname             = f1_varname            , &
        f2_Varname             = f2_varname            , &
        T_VarName              = t_varname             , &
        Teff_VarName           = teff_varname          , &
        Tfit_VarName           = tfit_varname            )


      ! Define dimensions that have channel-dependent names
      ! ...The number of passbands
      nc_stat(1) = NF90_DEF_DIM( &
        fileid         , &
        n_bands_dimname, &
        n_bands        , &
        band_dimid       )       
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(n_bands_dimname)//' dimension in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF      
      ! ...The number of temperatures
      nc_stat(1) = NF90_DEF_DIM( &
        fileid                , &
        n_temperatures_dimname, &
        n_temperatures        , &
        temperature_dimid       )       
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(n_temperatures_dimname)//' dimension in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF      

      
      ! Define the variables that have channel-dependent names and dimensions
      ! ...The passband begin frequencies
      nc_stat(1) = NF90_DEF_VAR( &
        fileid                 , &
        f1_varname             , &
        FREQUENCY_TYPE         , &
        dimIDs = [ band_dimid ], &
        varID  = varid           )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(f1_varname)//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , F1_LONGNAME         )
      nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, F1_DESCRIPTION      )
      nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , FREQUENCY_UNITS     )
      nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE )
      IF ( ANY(nc_stat /= SUCCESS) ) THEN
        msg = 'Error writing '//TRIM(f1_varname)//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      ! ...The passband end frequencies
      nc_stat(1) = NF90_DEF_VAR( &
        fileid                 , &
        f2_varname             , &
        FREQUENCY_TYPE         , &
        dimIDs = [ band_dimid ], &
        varID  = varid           )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(f2_varname)//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , F2_LONGNAME         )
      nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, F2_DESCRIPTION      )
      nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , FREQUENCY_UNITS     )
      nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE )
      IF ( ANY(nc_stat /= SUCCESS) ) THEN
        msg = 'Error writing '//TRIM(f2_varname)//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      ! ...The channel temperature data
      nc_stat(1) = NF90_DEF_VAR( &
        fileid                        , &
        t_varname                     , &
        TEMPERATURE_TYPE              , &
        dimIDs = [ temperature_dimid ], &
        varID  = varid                  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(t_Varname)//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , TEMPERATURE_LONGNAME    )
      nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, TEMPERATURE_DESCRIPTION )
      nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , TEMPERATURE_UNITS       )
      nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , TEMPERATURE_FILLVALUE   )
      IF ( ANY(nc_stat /= SUCCESS) ) THEN
        msg = 'Error writing '//TRIM(t_Varname)//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      ! ...The channel effective temperature data
      nc_stat(1) = NF90_DEF_VAR( &
        fileid                        , &
        teff_varname                  , &
        TEMPERATURE_TYPE              , &
        dimIDs = [ temperature_dimid ], &
        varID  = varid                  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(teff_Varname)//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , EFF_TEMPERATURE_LONGNAME    )
      nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, EFF_TEMPERATURE_DESCRIPTION )
      nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , TEMPERATURE_UNITS           )
      nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , TEMPERATURE_FILLVALUE       )
      IF ( ANY(nc_stat /= SUCCESS) ) THEN
        msg = 'Error writing '//TRIM(teff_Varname)//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      ! ...The fit to the channel effective temperature data
      nc_stat(1) = NF90_DEF_VAR( &
        fileid                        , &
        tfit_varname                  , &
        TEMPERATURE_TYPE              , &
        dimIDs = [ temperature_dimid ], &
        varID  = varid                  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//TRIM(tfit_Varname)//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , FIT_TEMPERATURE_LONGNAME    )
      nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, FIT_TEMPERATURE_DESCRIPTION )
      nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , TEMPERATURE_UNITS           )
      nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , TEMPERATURE_FILLVALUE       )
      IF ( ANY(nc_stat /= SUCCESS) ) THEN
        msg = 'Error writing '//TRIM(tfit_Varname)//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF


      ! Begin loop over passbands
      Define_Band_Loop: DO i = 1, n_bands

        ! Create the band-dependent dimension and variable names
        CALL Create_Names( channel, &
                           Band              = i                , &
                           n_Points_DimName  = n_points_dimname , &
                           Frequency_Varname = frequency_varname, &
                           Response_Varname  = response_varname   )

      
        ! Define dimensions that have band-dependent names
        ! ...The number of points for this passband
        nc_stat(1) = NF90_DEF_DIM( &
          fileid          , &
          n_points_dimname, &
          n_points(i)     , &
          n_points_dimid    )       
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//TRIM(n_points_dimname)//' dimension in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
          CALL Cleanup(); RETURN
        END IF

        
        ! Define the variables that have band-dependent names and dimensions
        ! ...The passband frequency
        nc_stat(1) = NF90_DEF_VAR( &
          fileid                     , &
          frequency_varname          , &
          FREQUENCY_TYPE             , &
          dimIDs = [ n_points_dimid ], &
          varID  = varid               )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//TRIM(frequency_varname)//' variable in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , FREQUENCY_LONGNAME )
        nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, FREQUENCY_DESCRIPTION )
        nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , FREQUENCY_UNITS )
        nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , FREQUENCY_FILLVALUE )
        IF ( ANY(nc_stat /= SUCCESS) ) THEN
          msg = 'Error writing '//TRIM(frequency_varname)//' variable attributes to '//TRIM(Filename)
          CALL Cleanup(); RETURN
        END IF
        ! ...The passband frequency
        nc_stat(1) = NF90_DEF_VAR( &
          fileid                     , &
          response_varname           , &
          RESPONSE_TYPE              , &
          dimIDs = [ n_points_dimid ], &
          varID  = varid               )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//TRIM(response_varname)//' variable in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR(nc_stat(1)))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_ATT( fileid, varid, LONGNAME_ATTNAME   , response_LONGNAME )
        nc_stat(2) = NF90_PUT_ATT( fileid, varid, DESCRIPTION_ATTNAME, response_DESCRIPTION )
        nc_stat(3) = NF90_PUT_ATT( fileid, varid, UNITS_ATTNAME      , response_UNITS )
        nc_stat(4) = NF90_PUT_ATT( fileid, varid, FILLVALUE_ATTNAME  , response_FILLVALUE )
        IF ( ANY(nc_stat /= SUCCESS) ) THEN
          msg = 'Error writing '//TRIM(response_varname)//' variable attributes to '//TRIM(Filename)
          CALL Cleanup(); RETURN
        END IF
              
      END DO Define_Band_Loop
      
    END DO Define_Channel_Loop
    
    
    ! Write the global attributes
    err_stat = WriteGAtts( self, fileid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing global attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nc_stat(1) = NF90_ENDDEF( fileid )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(Filename)//' out of define mode.'
      CALL Cleanup(); RETURN
    END IF

    
    ! ===================================================
    ! Write all the channel- and band-dependent variables
    ! ===================================================

    ! Begin loop over channels
    Write_Channel_Loop: DO n = 1, self%n_Channels


      ! Extract the current oSRF object from the file container
      err_stat = oSRF_File_GetFrom( self, osrf, pos = n )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error retrieving oSRF at index #",i0," from file container for writing")') n
        CALL Cleanup(); RETURN
      END IF


      ! Get the required values for this channel
      err_stat = oSRF_GetValue( &
        osrf, &
        n_Bands               = n_bands             , &
        Channel               = channel             , &
        Integral              = integral            , &
        Flags                 = flags               , &
        f0                    = f0                  , &
        Planck_Coeffs         = planck_coeffs       , &
        Polychromatic_Coeffs  = polychromatic_coeffs, &
        n_Points              = n_points            , &
        Temperature           = temperature         , &
        Effective_Temperature = eff_temperature     , &
        Fit_Temperature       = fit_temperature       )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE(msg,'("Error retrieving data from oSRF at index #",i0)') n
        CALL Cleanup(); RETURN
      END IF


      ! Create the channel-dependent variable names
      CALL Create_Names( &
        channel, &
        f1_Varname   = f1_varname  , &
        f2_Varname   = f2_varname  , &
        T_VarName    = t_varname   , &
        Teff_VarName = teff_varname, &
        Tfit_VarName = tfit_varname  )


      ! Write the channel-dependent data
      ! ...The sensor channel list
      nc_stat(1) = NF90_INQ_VARID( &
        fileid                , &
        SENSOR_CHANNEL_VARNAME, &
        varid                   )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        FileId     , &
        VarID      , &
        channel    , &
        start = [n]  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR(nc_stat(1)))
        CALL Cleanup(); RETURN
      END IF
      ! ...The integrated SRF value
      nc_stat(1) = NF90_INQ_VARID( &
        fileid                , &
        INTEGRATED_SRF_VARNAME, &
        varid                   )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//INTEGRATED_SRF_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid     , &
        varid      , &
        integral   , &
        start = [n]  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//INTEGRATED_SRF_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The processing flags
      nc_stat(1) = NF90_INQ_VARID( &
        fileid       , &
        FLAGS_VARNAME, &
        varid          )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//FLAGS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid     , &
        varid      , &
        flags      , &
        start = [n]  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//FLAGS_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The central frequency
      nc_stat(1) = NF90_INQ_VARID( &
        fileid                   , &
        CENTRAL_FREQUENCY_VARNAME, &
        varid                      )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//CENTRAL_FREQUENCY_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid     , &
        varid      , &
        f0         , &
        start = [n]  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//CENTRAL_FREQUENCY_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The Planck coefficients
      nc_stat(1) = NF90_INQ_VARID( &
        fileid               , &
        PLANCK_COEFFS_VARNAME, &
        varid                  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid       , &
        varid        , &
        planck_coeffs, &
        start = [1,n]  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//PLANCK_COEFFS_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The polychromatic coefficients
      nc_stat(1) = NF90_INQ_VARID( &
        fileid                      , &
        POLYCHROMATIC_COEFFS_VARNAME, &
        varid                         )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//POLYCHROMATIC_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid              , &
        varid               , &
        polychromatic_coeffs, &
        start = [1,n]         )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//POLYCHROMATIC_COEFFS_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The actual temperature
      nc_stat(1) = NF90_INQ_VARID( &
        fileid   , &
        t_varname, &
        varid      )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(t_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid     , &
        varid      , &
        temperature  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//TRIM(t_varname)//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The effective temperature
      nc_stat(1) = NF90_INQ_VARID( &
        fileid      , &
        teff_varname, &
        varid         )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(teff_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid         , &
        varid          , &
        eff_temperature  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//TRIM(teff_varname)//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      ! ...The fit to the effective temperature
      nc_stat(1) = NF90_INQ_VARID( &
        fileid      , &
        tfit_varname, &
        varid         )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(tfit_varname)//&
              ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      nc_stat(1) = NF90_PUT_VAR( &
        fileid         , &
        varid          , &
        fit_temperature  )
      IF ( nc_stat(1) /= NF90_NOERR ) THEN
        msg = 'Error writing '//TRIM(tfit_varname)//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
        CALL Cleanup(); RETURN
      END IF
      

      ! Begin loop over passbands
      Write_Band_Loop: DO i = 1, n_bands
      
        ! Get the required values for this channel
        err_stat = oSRF_GetValue( &
          osrf, &
          Band = i , &
          f1   = f1, &
          f2   = f2, &
          Frequency = frequency, &
          Response  = response)
        IF ( err_stat /= SUCCESS ) THEN
          WRITE(msg,'("Error retrieving data from oSRF at index #",i0)') n
          CALL Cleanup(); RETURN
        END IF
        
        
        ! Create the band-dependent variable names
        CALL Create_Names( &
          channel, &
          Band = i, &
          Frequency_Varname = frequency_varname, &
          Response_Varname  = response_varname   )

        
        ! Write the variables that have band-dependent data
        ! ...The passband begin frequencies
        nc_stat(1) = NF90_INQ_VARID( &
          fileid    , &
          f1_varname, &
          varid       )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(f1_varname)//&
                ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_VAR( &
          fileid     , &
          varid      , &
          f1         , &
          start = [i]  )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error writing '//TRIM(f1_varname)//' to '//TRIM(Filename)//&
                ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        ! ...The passband end frequencies
        nc_stat(1) = NF90_INQ_VARID( &
          fileid    , &
          f2_varname, &
          varid       )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(f2_varname)//&
                ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_VAR( &
          fileid     , &
          varid      , &
          f2         , &
          start = [i]  )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error writing '//TRIM(f2_varname)//' to '//TRIM(Filename)//&
                ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        ! ...The passband frequency
        nc_stat(1) = NF90_INQ_VARID( &
          fileid           , &
          frequency_varname, &
          varid              )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(frequency_varname)//&
                ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_VAR( &
          fileid   , &
          varid    , &
          frequency  )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error writing '//TRIM(frequency_varname)//' to '//TRIM(Filename)//&
                ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        ! ...The passband response
        nc_stat(1) = NF90_INQ_VARID( &
          fileid          , &
          response_varname, &
          varid             )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(response_varname)//&
                ' variable ID - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
        nc_stat(1) = NF90_PUT_VAR( &
          fileid  , &
          varid   , &
          response  )
        IF ( nc_stat(1) /= NF90_NOERR ) THEN
          msg = 'Error writing '//TRIM(response_varname)//' to '//TRIM(Filename)//&
                ' - '//TRIM(NF90_STRERROR( nc_stat(1) ))
          CALL Cleanup(); RETURN
        END IF
                
      END DO Write_Band_Loop
      
    END DO Write_Channel_Loop

              
    ! Close the file
    nc_stat(1) = NF90_CLOSE( FileId )
    IF ( nc_stat(1) /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nc_stat(1) ))
      CALL Cleanup(); RETURN
    END IF
    
  CONTAINS
    
    SUBROUTINE Cleanup()
      IF ( close_file ) THEN
        nc_stat(1) = NF90_CLOSE(fileid)
        IF ( nc_stat(1) /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing output file during error cleanup - '//&
                TRIM(NF90_STRERROR(nc_stat(1)))
      END IF
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Cleanup
  
  END FUNCTION oSRF_File_Write       



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Function to read the oSRF_File global attributes.

  FUNCTION ReadGAtts( self, FileId ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN OUT) :: self
    INTEGER,              INTENT(IN)     :: FileId
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::ReadGAtts'
    ! Local variables
    CHARACTER(256)  :: gattname
    CHARACTER(5000) :: gattstring
    INTEGER :: gattinteger
    INTEGER :: nc_stat

    ! Set up
    err_stat = SUCCESS


    ! The Release value, just for checking
    gattname = RELEASE_GATTNAME
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattinteger )
    IF ( nc_stat /= NF90_NOERR .OR. gattinteger /= OSRF_RELEASE ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The Version
    ! ...Get it
    gattname = VERSION_GATTNAME
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattinteger )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    err_stat = oSRF_File_SetValue( self, Version=gattinteger )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The Sensor_Id
    ! ...Get it
    gattname = SENSOR_ID_GATTNAME; gattstring = ' '
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattstring )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( gattstring )
    err_stat = oSRF_File_SetValue( self, Sensor_Id=gattstring )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The WMO_Satellite_Id
    ! ...Get it
    gattname = WMO_SATELLITE_ID_GATTNAME
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattinteger )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    err_stat = oSRF_File_SetValue( self, WMO_Satellite_Id=gattinteger )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The WMO_Sensor_Id
    ! ...Get it
    gattname = WMO_SENSOR_ID_GATTNAME
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattinteger )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    err_stat = oSRF_File_SetValue( self, WMO_Sensor_Id=gattinteger )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The Sensor_Type
    ! ...Get it
    gattname = SENSOR_TYPE_GATTNAME
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattinteger )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    err_stat = oSRF_File_SetValue( self, Sensor_Type=gattinteger )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The Title
    ! ...Get it
    gattname = TITLE_GATTNAME; gattstring = ' '
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattstring )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( gattstring )
    err_stat = oSRF_File_SetValue( self, Title=gattstring )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The History
    ! ...Get it
    gattname = HISTORY_GATTNAME; gattstring = ' '
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattstring )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( gattstring )
    err_stat = oSRF_File_SetValue( self, History=gattstring )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The Comment
    ! ...Get it
    gattname = COMMENT_GATTNAME; gattstring = ' '
    nc_stat = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(gattname), gattstring )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( gattstring )
    err_stat = oSRF_File_SetValue( self, Comment=gattstring )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(gattname)//&
                            ' attribute - '//TRIM(NF90_STRERROR(nc_stat)), &
                            err_stat )
    END SUBROUTINE CleanUp

  END FUNCTION ReadGAtts


  ! Function to write the oSRF_File global attributes.

  FUNCTION WriteGAtts( self, fileid ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    INTEGER,              INTENT(IN) :: fileid
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::WriteGAtts'
    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history'
    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time'
    ! Local variables
    CHARACTER(ML) :: gattname
    CHARACTER(8)  :: cdate
    CHARACTER(10) :: ctime
    CHARACTER(5)  :: czone
    INTEGER :: nc_stat

    ! Set up
    err_stat = SUCCESS


    ! The software ID
    gattname = WRITE_MODULE_HISTORY_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), MODULE_VERSION_ID )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    
    
    ! The file creation date
    CALL DATE_AND_TIME( cdate, ctime, czone )
    gattname = CREATION_DATE_AND_TIME_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), &
                            cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
                            ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
                            czone//'UTC' )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    
    
    ! The file release value
    gattname = RELEASE_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Release )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The data version value
    gattname = VERSION_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Version )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF

    
    ! The Sensor_Id
    gattname = SENSOR_ID_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Sensor_Id )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The WMO_Satellite_Id
    gattname = WMO_SATELLITE_ID_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%WMO_Satellite_Id )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! The WMO_Sensor_Id
    gattname = WMO_SENSOR_ID_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%WMO_Sensor_Id )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    
    
    ! The Sensor_Type
    gattname = SENSOR_TYPE_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Sensor_Type )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    
    
    ! The title
    gattname = TITLE_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Title )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! ...The history
    gattname = HISTORY_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%History )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF


    ! ...The comment
    gattname = COMMENT_GATTNAME
    nc_stat = NF90_PUT_ATT( fileid, NF90_GLOBAL, TRIM(gattname), self%Comment )
    IF ( nc_stat /= NF90_NOERR ) THEN
      CALL Cleanup(); RETURN
    END IF
    
 CONTAINS
  
    SUBROUTINE Cleanup()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM(gattname)//&
                            ' attribute - '//TRIM(NF90_STRERROR(nc_stat)), &
                            err_stat )
    END SUBROUTINE Cleanup
    
  END FUNCTION WriteGAtts





!------------------------------------------------------------------------------
!
! NAME:
!       Create_Names
!
! PURPOSE:
!       Utility subroutine to construct dimension and variable names based
!       on oSRF channel number and the number of bands.
!
! CALLING SEQUENCE:
!       CALL Create_Names( &
!         Channel                                        , &  ! Input
!         Band                   = Band                  , &  ! Optional input 
!         n_Bands_DimName        = n_Bands_DimName       , &  ! Optional output
!         n_Points_DimName       = n_Points_DimName      , &  ! Optional output
!         n_Temperatures_DimName = n_Temperatures_DimName, &  ! Optional output
!         f1_VarName             = f1_VarName            , &  ! Optional output
!         f2_VarName             = f2_VarName            , &  ! Optional output
!         Frequency_VarName      = Frequency_VarName     , &  ! Optional output
!         Response_VarName       = Response_VarName      , &  ! Optional output
!         T_VarName              = T_VarName             , &  ! Optional output
!         Teff_VarName           = Teff_VarName          , &  ! Optional output
!         Tfit_VarName           = Tfit_VarName            )  ! Optional output
!
! INPUT:
!       Channel:                 The sensor channel number.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT:
!       Band:                    The band number for the sensor channel SRF.
!                                If not specified, the default value is 1.
!                                UNITS:      N/A
!                                TYPE:       INTEGER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT:
!       n_Bands_DimName:         The dimension name for the specified SRF band
!                                and sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Points_DimName:        The dimension names for the number of points used
!                                to specfiy the SRF data for the specified band and
!                                sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Temperatures_DimName:  The dimension names for the number of temperatures
!                                used to compute the channel polychromatic coefficients.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f1_VarName:              The variable name for the band begin frequencies
!                                of the specified sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f2_VarName:              The variable name for the band end frequencies
!                                of the specified sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency_VarName:       The variable names for the SRF frequency array
!                                of the specified band and sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Response_VarName:        The variable names for the SRF response arrays
!                                of the specified band and sensor channel.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       T_VarName:               The variable names for the temperature array
!                                used to compute the channel polychromatic
!                                coefficients.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Teff_VarName:            The variable names for the effective temperature
!                                array used to compute the channel polychromatic
!                                coefficients.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Tfit_VarName:            The variable names for the fitted effective
!                                temperature computed using the channel polychromatic
!                                coefficients.
!                                UNITS:      N/A
!                                TYPE:       CHARACTER
!                                DIMENSION:  Scalar
!                                ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE Create_Names( &
    Channel               , &  ! Input
    Band                  , &  ! Optional input    
    n_Bands_DimName       , &  ! Optional output
    n_Points_DimName      , &  ! Optional output
    n_Temperatures_DimName, &  ! Optional output
    f1_VarName            , &  ! Optional output
    f2_VarName            , &  ! Optional output
    Frequency_VarName     , &  ! Optional output
    Response_VarName      , &  ! Optional output
    T_VarName             , &  ! Optional output
    Teff_VarName          , &  ! Optional output
    Tfit_VarName            )  ! Optional output
    ! Arguments
    INTEGER,                INTENT(IN)  :: Channel
    INTEGER,      OPTIONAL, INTENT(IN)  :: Band
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: n_Bands_DimName  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: n_Points_DimName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: n_Temperatures_DimName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f1_VarName       
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f2_VarName       
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Frequency_VarName
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Response_VarName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: T_VarName    
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Teff_VarName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Tfit_VarName 
    ! Local variables
    CHARACTER(256) :: ch, b
    INTEGER :: l_Band

    ! Check Band argument
    l_Band = 1
    IF ( PRESENT(Band) ) THEN
      l_Band = MAX(ABS(Band), l_Band)
    ENDIF
    
    
    ! Construct the dimension and variable names
    WRITE( ch,'(i0)' ) Channel
    ch = ADJUSTL(ch)
    WRITE( b,'(i0)' ) l_Band
    b = ADJUSTL(b)
    ! ...Channel only
    IF ( PRESENT(n_Bands_DimName       ) ) n_Bands_DimName        = 'ch'//TRIM(ch)//'_n_Bands'
    IF ( PRESENT(n_Temperatures_DimName) ) n_Temperatures_DimName = 'ch'//TRIM(ch)//'_n_Temperatures'
    IF ( PRESENT(f1_VarName            ) ) f1_VarName             = 'ch'//TRIM(ch)//'_f1'
    IF ( PRESENT(f2_VarName            ) ) f2_VarName             = 'ch'//TRIM(ch)//'_f2'
    IF ( PRESENT(T_VarName             ) ) T_VarName              = 'ch'//TRIM(ch)//'_T'
    IF ( PRESENT(Teff_VarName          ) ) Teff_VarName           = 'ch'//TRIM(ch)//'_Teff'
    IF ( PRESENT(Tfit_VarName          ) ) Tfit_VarName           = 'ch'//TRIM(ch)//'_Tfit'
    ! ...Channel and band
    IF ( PRESENT(n_Points_DimName ) ) n_Points_DimName  = 'ch'//TRIM(ch)//'_b'//TRIM(b)//'_n_Points'
    IF ( PRESENT(Frequency_VarName) ) Frequency_VarName = 'ch'//TRIM(ch)//'_b'//TRIM(b)//'_Frequency'
    IF ( PRESENT(Response_VarName ) ) Response_VarName  = 'ch'//TRIM(ch)//'_b'//TRIM(b)//'_Response'

  END SUBROUTINE Create_Names


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_Dim
!
! PURPOSE:
!       Utility function to retrieve an oSRF_File dimension by name.
!
! CALLING SEQUENCE:
!       Error_Status = Read_Dim( &
!         FileId       , &  ! Input
!         DimName      , &  ! Input
!         DimValue     , &  ! Output
!         DimId = DimId  )  ! Optional Output
!
! INPUT:
!       FileId:          File id for an open oSRF_File file.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
!       DimName:         Name of the file dimension to retrieve.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER(*)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(IN)
!
! OUTPUT:
!       DimValue:        Value of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL OUTPUTS:
!       DimID:           Id of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!                          
! FUNCTION RESULT
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the dimension retrieval was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  FUNCTION Read_Dim( &
    FileId  , & ! Input
    DimName , & ! Input
    DimValue, & ! Output
    DimID   ) & ! Optional Output
  RESULT( err_stat )
    ! Arguments
    INTEGER,           INTENT(IN)  :: FileId  
    CHARACTER(*),      INTENT(IN)  :: DimName 
    INTEGER,           INTENT(OUT) :: DimValue
    INTEGER, OPTIONAL, INTENT(OUT) :: DimID   
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_Dim'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: nc_stat
    INTEGER :: id
    
    ! Setup
    err_stat = SUCCESS


    ! Get the dimension id
    nc_stat = NF90_INQ_DIMID( FileId, TRIM(DimName), id )
    IF ( nc_stat /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TRIM(DimName)//' - '//TRIM(NF90_STRERROR(nc_stat))
      CALL Read_Dim_CleanUp(); RETURN
    END IF


    ! Get the dimension value
    nc_stat = NF90_INQUIRE_DIMENSION( FileId, id, Len = DimValue )
    IF ( nc_stat /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TRIM(DimName)//' - '//TRIM(NF90_STRERROR(nc_stat))
      CALL Read_Dim_CleanUp(); RETURN
    END IF


    ! Assign return arguments
    IF ( PRESENT(DimId) ) Dimid = id

  CONTAINS
  
    SUBROUTINE Read_Dim_CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Read_Dim_CleanUp
    
  END FUNCTION Read_Dim
 
END MODULE oSRF_File_Define
 
