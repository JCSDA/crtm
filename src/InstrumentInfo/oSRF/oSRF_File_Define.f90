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
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                   Display_Message
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
  USE oSRF_Define          , ONLY: OSRF_RELEASE   , &           
                                   OSRF_VERSION   , &           
                                   oSRF_type      , &           
                                   oSRF_Associated, &           
                                   oSRF_Destroy   , &           
                                   oSRF_Create    , &           
                                   oSRF_SetValue  , &           
                                   oSRF_Inspect   , &           
                                   oSRF_Info                 
                                   
  USE netcdf
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
  '$Id$'
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


  ! Variable units attribute.
  CHARACTER(*), PARAMETER :: UNITS_ATTNAME = 'units'
  
  CHARACTER(*), PARAMETER :: SENSOR_CHANNEL_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: INTEGRATED_SRF_UNITS       = 'N/A'
  CHARACTER(*), PARAMETER :: FLAGS_UNITS                = 'N/A'
  CHARACTER(*), PARAMETER :: PLANCK_COEFFS_UNITS        = '[W.m^2, K.m]'
  CHARACTER(*), PARAMETER :: POLYCHROMATIC_COEFFS_UNITS = '[K, K/K]'
  CHARACTER(*), PARAMETER :: FREQUENCY_UNITS            = 'N/A' !SENSOR_FREQUENCY_UNITS ; From osrf_parameters include file
  CHARACTER(*), PARAMETER :: RESPONSE_UNITS             = 'N/A'
  

  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
  
  INTEGER,  PARAMETER :: SENSOR_CHANNEL_FILLVALUE       = INVALID
  REAL(fp), PARAMETER :: INTEGRATED_SRF_FILLVALUE       = ZERO
  INTEGER,  PARAMETER :: FLAGS_FILLVALUE                = -1
  REAL(fp), PARAMETER :: PLANCK_COEFFS_FILLVALUE        = ZERO
  REAL(fp), PARAMETER :: POLYCHROMATIC_COEFFS_FILLVALUE = ZERO
  REAL(fp), PARAMETER :: FREQUENCY_FILLVALUE            = ZERO
  REAL(fp), PARAMETER :: RESPONSE_FILLVALUE             = ZERO
  

  ! Variable netCDF datatypes
  INTEGER, PARAMETER :: SENSOR_CHANNEL_TYPE       = NF90_INT
  INTEGER, PARAMETER :: INTEGRATED_SRF_TYPE       = NF90_DOUBLE
  INTEGER, PARAMETER :: FLAGS_TYPE                = NF90_INT
  INTEGER, PARAMETER :: PLANCK_COEFFS_TYPE        = NF90_DOUBLE
  INTEGER, PARAMETER :: POLYCHROMATIC_COEFFS_TYPE = NF90_DOUBLE
  INTEGER, PARAMETER :: FREQUENCY_TYPE            = NF90_DOUBLE
  INTEGER, PARAMETER :: RESPONSE_TYPE             = NF90_DOUBLE


  ! -------------------------------
  ! oSRF_File data type definitions
  ! -------------------------------
  TYPE :: oSRF_File_type
    CHARACTER(FL) :: Filename
    ! Release and version information
    INTEGER :: Release = oSRF_RELEASE
    INTEGER :: Version = oSRF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: n_Channels = 0
    ! Channel independent data
    CHARACTER(SL) :: Sensor_ID        = ' '
    INTEGER       :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER       :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER       :: Sensor_Type      = INVALID_SENSOR
    ! File global attributes
    CHARACTER(GL) :: Title   = ' '
    CHARACTER(GL) :: History = ' '
    CHARACTER(GL) :: Comment = ' '
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
  RESULT( err_status )
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
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Set_Property'
    ! Set up
    err_status = SUCCESS
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
  RESULT( err_status )
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
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Get_Property'
    ! Set up
    err_status = SUCCESS
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
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN OUT) :: self
    TYPE(oSRF_type),      INTENT(IN)     :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)     :: pos
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::AddTo'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_pos
    
    ! Set up
    err_status = SUCCESS
    
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
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
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
!       pos:          Set this keyword to the index of the position of the
!                     oSRF object to retrieve from the oSRF_File container.
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
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN)  :: self
    TYPE(oSRF_type),      INTENT(OUT) :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)  :: pos
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::GetFrom'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_pos
    
    ! Set up
    err_status = SUCCESS
    
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
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
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
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    CHARACTER(*),         INTENT(IN)  :: Filename
    LOGICAL,    OPTIONAL, INTENT(IN)  :: Quiet
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Read'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: FileId
    INTEGER :: nc_status
    INTEGER :: alloc_status
    INTEGER :: Version         
    CHARACTER(80) :: Sensor_ID       
    INTEGER :: WMO_Satellite_ID
    INTEGER :: WMO_Sensor_ID   
    INTEGER :: Sensor_Type
    INTEGER :: VarId
    INTEGER :: n, n_Channels
    INTEGER :: i, n_Bands
    INTEGER :: n_Planck_Coeffs
    INTEGER :: n_Polychromatic_Coeffs
    INTEGER :: Channel_VarId
    INTEGER :: Channel
    CHARACTER(80) :: n_Bands_DimName
    CHARACTER(80) :: n_Points_DimName
    INTEGER, ALLOCATABLE :: n_Points(:) 
    CHARACTER(80) :: Frequency_VarName
    CHARACTER(80) :: Response_VarName
    REAL(fp) :: Integral
    INTEGER  :: Flags
    REAL(fp) :: f0
    REAL(fp) :: Planck_Coeffs(5)        ! Default size
    REAL(fp) :: Polychromatic_Coeffs(5) ! Default size
    REAL(fp), ALLOCATABLE :: fr(:)
    TYPE(oSRF_type) :: oSRF    

    ! Set up
    err_status = SUCCESS
    ! ...Determine info message output
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet


    ! Open the file for reading
    nc_status = NF90_OPEN( Filename, NF90_NOWRITE, FileId )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(Filename)//' for read access - '//&
            TRIM(NF90_STRERROR(nc_status))
      CALL Cleanup(); RETURN
    END IF
    
    
    ! Get the number of channels dimension
    err_status = Read_Dim( FileId, CHANNEL_DIMNAME, n_Channels )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(Filename)
      CALL Cleanup(Close_File=.TRUE.); RETURN
    END IF


    ! Allocate the output container
    CALL oSRF_File_Create( self, n_Channels )    
    IF ( .NOT. oSRF_File_Associated( self ) ) THEN
      msg = 'Error allocating oSRF_File output'
      CALL Cleanup(Close_File=.TRUE.); RETURN
    END IF
    ! ...Set the filename
    err_status = oSRF_File_SetValue( self, Filename = Filename )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error setting filename property in oSRF_File'
      CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
    END IF
    
    
    ! Read the global attributes
    err_status = Read_GAtts( self, FileId )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
    END IF
    ! ...Save them to local variables for insertion into oSRF objects
    err_status = oSRF_File_GetValue( &
      self, &
      Version          = Version         , &
      Sensor_Id        = Sensor_Id       , &
      WMO_Satellite_Id = WMO_Satellite_Id, &
      WMO_Sensor_Id    = WMO_Sensor_Id   , &
      Sensor_Type      = Sensor_Type       )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error getting GAtt properties from oSRF_File object.'
      CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
    END IF


    ! Get the sensor channel variable id
    nc_status = NF90_INQ_VARID( FileId, SENSOR_CHANNEL_VARNAME, Channel_VarId )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
            ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
      CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
    END IF

    ! Loop over the number of channels 
    DO n = 1, n_Channels
    
      ! Read the current channel number
      nc_status = NF90_GET_VAR( FileId, Channel_VariD, Channel, START=(/n/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading sensor channel from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF


      ! Create the band dimension name
      CALL Create_Names( Channel, n_Bands_DimName = n_Bands_DimName )
        
        
      ! Read the current channel band dimension
      err_status = Read_Dim( FileId, TRIM(n_Bands_DimName), n_Bands )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error reading '//TRIM(n_Bands_DimName)//' dimension from '//TRIM(Filename)
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      
      
      ! Allocate the current channel oSRF
      ! ...Allocate the n_Points array
      ALLOCATE( n_Points(n_Bands), STAT=alloc_status )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg, '("Error allocating n_Points array for channel ",i0,". STAT = ",i0)' ) &
                    Channel, alloc_status
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...Get the number of points for each band
      DO i = 1, n_Bands
        ! ...Create the dimension name
        CALL Create_Names( Channel, Band = i, n_Points_DimName = n_Points_DimName )
        ! ...Read the dimension value
        err_status = Read_Dim( FileId, TRIM(n_Points_DimName), n_Points(i) )
        IF ( err_status /= SUCCESS ) THEN
          msg = 'Error reading '//TRIM(n_Points_DimName)//' dimension from '//TRIM(Filename)
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
      END DO
      ! ...Create the current oSRF object
      CALL oSRF_Create( oSRF, n_Points )
      IF ( .NOT. oSRF_Associated( oSRF ) ) THEN
        WRITE( msg, '("Error creating oSRF object for channel ",i0)' ) Channel
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...Add current properties
      err_status = oSRF_SetValue( &
        oSRF, &
        Channel          = Channel         , &
        Version          = Version         , &
        Sensor_Id        = Sensor_Id       , &
        WMO_Satellite_Id = WMO_Satellite_Id, &
        WMO_Sensor_Id    = WMO_Sensor_Id   , &
        Sensor_Type      = Sensor_Type       )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error setting general properties of oSRF for channel ",i0)' ) Channel
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      
      
      ! Read the channel dependent data
      ! ...The integrated SRF value
      nc_status = NF90_INQ_VARID( FileId, INTEGRATED_SRF_VARNAME, VarId )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//INTEGRATED_SRF_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_GET_VAR( FileId, VariD, Integral, START=(/n/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//INTEGRATED_SRF_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      err_status = oSRF_SetValue( oSRF, Integral = Integral )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error setting '//INTEGRATED_SRF_VARNAME//' property in oSRF object'
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...The processing flags
      nc_status = NF90_INQ_VARID( FileId, FLAGS_VARNAME, VarId )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//FLAGS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_GET_VAR( FileId, VariD, Flags, START=(/n/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//FLAGS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      err_status = oSRF_SetValue( oSRF, Flags = Flags )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error setting '//FLAGS_VARNAME//' property in oSRF object'
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...The central frequency
      nc_status = NF90_INQ_VARID( FileId, CENTRAL_FREQUENCY_VARNAME, VarId )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//CENTRAL_FREQUENCY_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_GET_VAR( FileId, VariD, f0, START=(/n/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//CENTRAL_FREQUENCY_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      err_status = oSRF_SetValue( oSRF, f0 = f0 )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error setting '//CENTRAL_FREQUENCY_VARNAME//' property in oSRF object'
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...The Planck coefficients
      err_status = Read_Dim( FileId, PLANCK_COEFFS_DIMNAME, n_Planck_Coeffs )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error reading '//PLANCK_COEFFS_DIMNAME//' dimension from '//TRIM(Filename)
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_INQ_VARID( FileId, PLANCK_COEFFS_VARNAME, VarId )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//PLANCK_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_GET_VAR( FileId, VariD, Planck_Coeffs(1:n_Planck_Coeffs), &
                                START=(/1,n/), &
                                COUNT=(/n_Planck_Coeffs,1/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//PLANCK_COEFFS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      err_status = oSRF_SetValue( oSRF, Planck_Coeffs = Planck_Coeffs(1:n_Planck_Coeffs) )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error setting '//PLANCK_COEFFS_VARNAME//' property in oSRF object'
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      ! ...The Polychromatic coefficients
      err_status = Read_Dim( FileId, POLYCHROMATIC_COEFFS_DIMNAME, n_Polychromatic_Coeffs )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error reading '//POLYCHROMATIC_COEFFS_DIMNAME//' dimension from '//TRIM(Filename)
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_INQ_VARID( FileId, POLYCHROMATIC_COEFFS_VARNAME, VarId )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//POLYCHROMATIC_COEFFS_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      nc_status = NF90_GET_VAR( FileId, VariD, Polychromatic_Coeffs(1:n_Polychromatic_Coeffs), &
                                START=(/1,n/), &
                                COUNT=(/n_Polychromatic_Coeffs,1/) )
      IF ( nc_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//POLYCHROMATIC_COEFFS_VARNAME//' from'//TRIM(Filename)//' - '//&
              TRIM(NF90_STRERROR(nc_status))
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      err_status = oSRF_SetValue( oSRF, Polychromatic_Coeffs = Polychromatic_Coeffs(1:n_Polychromatic_Coeffs) )
      IF ( err_status /= SUCCESS ) THEN
        msg = 'Error setting '//POLYCHROMATIC_COEFFS_VARNAME//' property in oSRF object'
        CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
      END IF
      

      ! Read the band dependent data
      DO i = 1, n_Bands
        ! ...Create the dimension and variable names
        CALL Create_Names( &
           Channel, &
           Band = i, &
           Frequency_VarName = Frequency_VarName, &
           Response_VarName  = Response_VarName   )
        ! ...Allocate the data array
        ALLOCATE( fr( n_Points(i) ), STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          WRITE( msg, '("Error allocating f/r array for channel ",i0,", band ",i0,". STAT = ",i0)' ) &
                      Channel, i, alloc_status
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
        ! ...Read the frequency data
        nc_status = NF90_INQ_VARID( FileId, TRIM(Frequency_VarName), VarId )
        IF ( nc_status /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Frequency_VarName)//&
                ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
        nc_status = NF90_GET_VAR( FileId, VariD, fr )
        IF ( nc_status /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(Frequency_VarName)//' from'//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR(nc_status))
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
        err_status = oSRF_SetValue( oSRF, Band = i, Frequency = fr )
        IF ( err_status /= SUCCESS ) THEN
          msg = 'Error setting '//TRIM(Frequency_VarName)//' property in oSRF object'
          CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
        END IF
        ! ...Read the Response data
        nc_status = NF90_INQ_VARID( FileId, TRIM(Response_VarName), VarId )
        IF ( nc_status /= NF90_NOERR ) THEN
          msg = 'Error inquiring '//TRIM(Filename)//' for '//TRIM(Response_VarName)//&
                ' variable ID - '//TRIM(NF90_STRERROR(nc_status))
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
        nc_status = NF90_GET_VAR( FileId, VariD, fr )
        IF ( nc_status /= NF90_NOERR ) THEN
          msg = 'Error reading '//TRIM(Response_VarName)//' from'//TRIM(Filename)//' - '//&
                TRIM(NF90_STRERROR(nc_status))
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
        err_status = oSRF_SetValue( oSRF, Band = i, Response = fr )
        IF ( err_status /= SUCCESS ) THEN
          msg = 'Error setting '//TRIM(Response_VarName)//' property in oSRF object'
          CALL Cleanup(Close_File=.TRUE., Destroy_Obj=.TRUE.); RETURN
        END IF
        ! ...Deallocate the data array
        DEALLOCATE( fr, STAT = alloc_status )
        IF ( alloc_status /= 0 ) THEN
          WRITE( msg, '("Error deallocating f/r array for channel ",i0,", band ",i0,". STAT = ",i0)' ) &
                      Channel, i, alloc_status
          CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
        END IF
      END DO      
      

      ! Add the oSRF to the OSRF_File object
      err_status = oSRF_File_AddTo( self, oSRF, pos = n )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error adding channel ",i0," oSRF to oSRF_File container.")' ) Channel
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF

      
      ! Clean up for next channels
      CALL oSRF_Destroy( oSRF )
      DEALLOCATE( n_Points, STAT=alloc_status )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg, '("Error deallocating n_Points array for channel ",i0,". STAT = ",i0)' ) &
                    Channel, alloc_status
        CALL Cleanup(Close_File=.TRUE.,Destroy_Obj=.TRUE.); RETURN
      END IF
      
    END DO


    ! Close the file
    nc_status = NF90_CLOSE( FileId )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR(nc_status))
      CALL Cleanup(); RETURN
    END IF


    ! Output an info message
    IF ( Noisy ) THEN
      CALL oSRF_File_Info( self, msg )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM(Filename)//'; '//TRIM(msg), &
                            INFORMATION )
    END IF

  CONTAINS
  
    SUBROUTINE Cleanup( Close_File, Destroy_Obj )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Obj
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          nc_status = NF90_CLOSE(FileId)
          IF ( nc_status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR(nc_status))
        END IF
      END IF
      ! Destroy oSRF_File object if necessary
      IF ( PRESENT(Destroy_Obj) ) THEN
        IF ( Destroy_Obj ) THEN
          CALL oSRF_File_Destroy( self )
        END IF
      END IF
      ! Set error status and print error message
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_status )
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
    self      , & ! Input
    Filename  , & ! Input
    Quiet     ) & ! Optional Input
  RESULT ( err_status )  
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    CHARACTER(*),         INTENT(IN) :: Filename
    LOGICAL,    OPTIONAL, INTENT(IN) :: Quiet
    ! Function result
    INTEGER :: err_status
    ! Parameter
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Write'
    ! Local Variables
    CHARACTER(ML) :: msg
    LOGICAL :: Noisy
    INTEGER :: FileId
    INTEGER :: VarId
    INTEGER :: nc_status
    INTEGER :: NF90_Status(4)
    INTEGER :: l, i
    INTEGER :: Channel_DimID
    INTEGER :: PolyChromatic_Coeffs_DimID
    INTEGER :: Planck_Coeffs_DimID
    INTEGER :: Band_DimID
    INTEGER :: n_Points_DimID
    INTEGER :: n_Channels
    INTEGER :: n_Planck_Coeffs
    INTEGER :: n_Polychromatic_Coeffs
    CHARACTER(80) :: n_Bands_DimName
    CHARACTER(80) :: n_Points_DimName
    CHARACTER(80) :: f1_Varname
    CHARACTER(80) :: f2_Varname
    CHARACTER(80) :: Frequency_Varname
    CHARACTER(80) :: Response_Varname
     
    ! Set up
    err_status = SUCCESS
    ! ...Determine info message output
    Noisy = .TRUE.
    IF ( PRESENT(Quiet) ) Noisy = .NOT. Quiet
    
    ! Create the output data file
    ! ---------------------------
    NF90_Status(1) = NF90_CREATE( Filename,NF90_CLOBBER,FileID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error creating '//TRIM(Filename)//' - '//&
             TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    
    ! Define the dimensions that
    ! have channel independent names
    NF90_Status(1) = NF90_DEF_DIM( FileID,CHANNEL_DIMNAME,NF90_UNLIMITED,Channel_DimID )       
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CHANNEL_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    n_Polychromatic_Coeffs = SIZE(self%oSRF(1)%Polychromatic_Coeffs)
    n_Planck_Coeffs = SIZE(self%oSRF(1)%Planck_Coeffs)
    NF90_Status(1) = NF90_DEF_DIM( FileID,POLYCHROMATIC_COEFFS_DIMNAME, &
                                   n_Polychromatic_Coeffs,              &
                                   PolyChromatic_Coeffs_DimID           )       
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLYCHROMATIC_COEFFS_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_DIM( FileID,PLANCK_COEFFS_DIMNAME,        &
                                   n_Planck_Coeffs, Planck_Coeffs_DimID )       
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_COEFFS_DIMNAME//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF 
    
    ! Define the variables that have
    ! channel independent names
    NF90_Status(1) = NF90_DEF_VAR( FileID,SENSOR_CHANNEL_VARNAME,SENSOR_CHANNEL_TYPE, &
                                   dimIDs=(/Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,SENSOR_CHANNEL_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,SENSOR_CHANNEL_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,SENSOR_CHANNEL_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( FileID,INTEGRATED_SRF_VARNAME,INTEGRATED_SRF_TYPE, &
                                   dimIDs=(/Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,INTEGRATED_SRF_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,INTEGRATED_SRF_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,INTEGRATED_SRF_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,INTEGRATED_SRF_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//INTEGRATED_SRF_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF   
    
    NF90_Status(1) = NF90_DEF_VAR( FileID,FLAGS_VARNAME,FLAGS_TYPE, &
                                   dimIDs=(/Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//FLAGS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,FLAGS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,FLAGS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,FLAGS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,FLAGS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//FLAGS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF 
    
    NF90_Status(1) = NF90_DEF_VAR( FileID,CENTRAL_FREQUENCY_VARNAME,FREQUENCY_TYPE, &
                                   dimIDs=(/Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//CENTRAL_FREQUENCY_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,CENTRAL_FREQUENCY_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,CENTRAL_FREQUENCY_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,FREQUENCY_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//CENTRAL_FREQUENCY_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( FileID,PLANCK_COEFFS_VARNAME,PLANCK_COEFFS_TYPE, &
                                   dimIDs=(/Planck_Coeffs_DimID,Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//PLANCK_COEFFS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,PLANCK_COEFFS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,PLANCK_COEFFS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,PLANCK_COEFFS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,PLANCK_COEFFS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//PLANCK_COEFFS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF
    
    NF90_Status(1) = NF90_DEF_VAR( FileID,POLYCHROMATIC_COEFFS_VARNAME,POLYCHROMATIC_COEFFS_TYPE, &
                                   dimIDs=(/Polychromatic_Coeffs_DimID,Channel_DimID/),varID=VarID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error defining '//POLYCHROMATIC_COEFFS_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
    NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,POLYCHROMATIC_COEFFS_LONGNAME )
    NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,POLYCHROMATIC_COEFFS_DESCRIPTION )
    NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,POLYCHROMATIC_COEFFS_UNITS )
    NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,POLYCHROMATIC_COEFFS_FILLVALUE )
    IF ( ANY(NF90_Status /= SUCCESS) ) THEN
      msg = 'Error writing '//POLYCHROMATIC_COEFFS_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Cleanup(); RETURN
    END IF

    l=1
    DO l = l, self%n_Channels

      CALL Create_Names( l, n_Bands_DimName = n_Bands_DimName, &
                         f1_Varname = f1_Varname, f2_Varname = f2_Varname )
      
      ! Define dimensions that
      ! have channel dependent names
      NF90_Status(1) = NF90_DEF_DIM( FileID,n_Bands_DimName,        &
                                     self%oSRF(l)%n_Bands,Band_DimID )       
      IF ( NF90_Status(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//n_Bands_DimName//' dimension in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
        CALL Cleanup(); RETURN
      END IF      
      
      ! Define the variables that have
      ! channel dependent names and dimensions
      NF90_Status(1) = NF90_DEF_VAR( FileID,f1_Varname,FREQUENCY_TYPE, &
                                     dimIDs=(/Band_DimID/),varID=VarID )
      IF ( NF90_Status(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//f1_Varname//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
        CALL Cleanup(); RETURN
      END IF
      NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,F1_LONGNAME )
      NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,F1_DESCRIPTION )
      NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,FREQUENCY_UNITS )
      NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
      IF ( ANY(NF90_Status /= SUCCESS) ) THEN
        msg = 'Error writing '//f1_Varname//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF
      
      NF90_Status(1) = NF90_DEF_VAR( FileID,f2_Varname,FREQUENCY_TYPE, &
                                     dimIDs=(/Band_DimID/),varID=VarID )
      IF ( NF90_Status(1) /= NF90_NOERR ) THEN
        msg = 'Error defining '//f2_Varname//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
        CALL Cleanup(); RETURN
      END IF
      NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,F2_LONGNAME )
      NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,F2_DESCRIPTION )
      NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,FREQUENCY_UNITS )
      NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
      IF ( ANY(NF90_Status /= SUCCESS) ) THEN
        msg = 'Error writing '//f2_Varname//' variable attributes to '//TRIM(Filename)
        CALL Cleanup(); RETURN
      END IF

      ! Define the band dependent 
      ! dimensions for the channel
      DO i = 1, self%oSRF(l)%n_Bands
      
        CALL Create_Names( l, Band = i,                           &
                           n_Points_DimName = n_Points_DimName,   &
                           Frequency_Varname = Frequency_Varname, &
                           Response_Varname = Response_Varname    )
      
        NF90_Status(1) = NF90_DEF_DIM( FileID, n_Points_DimName, &
                                       self%oSRF(l)%n_Points(i), n_Points_DimID )       
        IF ( NF90_Status(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//n_Points_DimName//' dimension in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
          CALL Cleanup(); RETURN
        END IF
        
        NF90_Status(1) = NF90_DEF_VAR( FileID,Frequency_Varname,FREQUENCY_TYPE, &
                                       dimIDs=(/n_Points_DimID/),varID=VarID )
        IF ( NF90_Status(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//Frequency_Varname//' variable in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
          CALL Cleanup(); RETURN
        END IF
        NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,FREQUENCY_LONGNAME )
        NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,FREQUENCY_DESCRIPTION )
        NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,FREQUENCY_UNITS )
        NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
        IF ( ANY(NF90_Status /= SUCCESS) ) THEN
          msg = 'Error writing '//Frequency_Varname//' variable attributes to '//TRIM(Filename)
          CALL Cleanup(); RETURN
        END IF
        
        NF90_Status(1) = NF90_DEF_VAR( FileID,Response_Varname,RESPONSE_TYPE, &
                                       dimIDs=(/n_Points_DimID/),varID=VarID )
        IF ( NF90_Status(1) /= NF90_NOERR ) THEN
          msg = 'Error defining '//Response_Varname//' variable in '//&
                TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
          CALL Cleanup(); RETURN
        END IF
        NF90_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME,RESPONSE_LONGNAME )
        NF90_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,RESPONSE_DESCRIPTION )
        NF90_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME,RESPONSE_UNITS )
        NF90_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME,RESPONSE_FILLVALUE )
        IF ( ANY(NF90_Status /= SUCCESS) ) THEN
          msg = 'Error writing '//Response_Varname//' variable attributes to '//TRIM(Filename)
          CALL Cleanup(); RETURN
        END IF
              
      END DO 
      
    END DO

    ! Take netCDF file out of define mode
    ! -----------------------------------
    NF90_Status(1) = NF90_ENDDEF( FileID )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(Filename)//' out of define mode.'
      CALL Cleanup(); RETURN
    END IF
    
    ! Write the data
    ! --------------
       
    
    ! Close the file
    NF90_Status(1) = NF90_CLOSE( FileId )
    IF ( NF90_Status(1) /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( NF90_Status(1) ))
      CALL Cleanup(); RETURN
    END IF
  CONTAINS
    
    SUBROUTINE Cleanup( )
      ! Set error status and print error message
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_status )
    END SUBROUTINE Cleanup
    ! Write the global attributes
        
    !Filename = Filename
  
  END FUNCTION oSRF_File_Write       

!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!
! NAME:
!       Read_GAtts
!
! PURPOSE:
!       Function to read the oSRF_File global attributes.
!
! CALLING SEQUENCE:
!       Error_Status = Read_GAtts( &
!         oSRF_File, &  ! Object
!         FileId     )  ! Input
!
! OBJECT:
!       oSRF_File:    oSRF_File structure in which the global attributes
!                     will reside
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       FileId:       File id for an open oSRF_File file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the global attribute read was successful.
!                        == FAILURE an error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!------------------------------------------------------------------------------

  FUNCTION Read_GAtts( self, FileId ) RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN OUT) :: self
    INTEGER,              INTENT(IN)     :: FileId
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_File::Read_GAtts'
    ! Local variables
    CHARACTER(256)  :: GAttName
    CHARACTER(5000) :: GAttString
    INTEGER :: GAttInteger
    INTEGER :: nc_status

    ! Set up
    err_status = SUCCESS


    ! The Release value, just for checking
    GAttName = RELEASE_GATTNAME
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttInteger )
    IF ( nc_status /= NF90_NOERR .OR. GAttInteger /= OSRF_RELEASE ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The Version
    ! ...Get it
    GAttName = VERSION_GATTNAME
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttInteger )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    err_status = oSRF_File_SetValue( self, Version=GAttInteger )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The Sensor_Id
    ! ...Get it
    GAttName = SENSOR_ID_GATTNAME; GAttString = ' '
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttString )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( GAttString )
    err_status = oSRF_File_SetValue( self, Sensor_Id=GAttString )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The WMO_Satellite_Id
    ! ...Get it
    GAttName = WMO_SATELLITE_ID_GATTNAME
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttInteger )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    err_status = oSRF_File_SetValue( self, WMO_Satellite_Id=GAttInteger )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The WMO_Sensor_Id
    ! ...Get it
    GAttName = WMO_SENSOR_ID_GATTNAME
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttInteger )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    err_status = oSRF_File_SetValue( self, WMO_Sensor_Id=GAttInteger )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The Title
    ! ...Get it
    GAttName = TITLE_GATTNAME; GAttString = ' '
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttString )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( GAttString )
    err_status = oSRF_File_SetValue( self, Title=GAttString )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The History
    ! ...Get it
    GAttName = HISTORY_GATTNAME; GAttString = ' '
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttString )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( GAttString )
    err_status = oSRF_File_SetValue( self, History=GAttString )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF


    ! The Comment
    ! ...Get it
    GAttName = COMMENT_GATTNAME; GAttString = ' '
    nc_status = NF90_GET_ATT( FileId, NF90_GLOBAL, TRIM(GAttName), GAttString )
    IF ( nc_status /= NF90_NOERR ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF
    ! ...Save it
    CALL StrClean( GAttString )
    err_status = oSRF_File_SetValue( self, Comment=GAttString )
    IF ( err_status /= SUCCESS ) THEN
      CALL Read_GAtts_Cleanup(); RETURN
    END IF

  CONTAINS
  
    SUBROUTINE Read_GAtts_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM(GAttName)//&
                            ' attribute - '//TRIM(NF90_STRERROR(nc_status)), &
                            err_status )
    END SUBROUTINE Read_GAtts_CleanUp

  END FUNCTION Read_GAtts



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
!         Channel                              , &  ! Input
!         n_Bands           = n_Bands          , &  ! Optional input         
!         n_Bands_DimName   = n_Bands_DimName  , &  ! Optional output
!         n_Points_DimName  = n_Points_DimName , &  ! Optional output
!         f1_VarName        = f1_VarName       , &  ! Optional output
!         f2_VarName        = f2_VarName       , &  ! Optional output
!         Frequency_VarName = Frequency_VarName, &  ! Optional output
!         Response_VarName  = Response_VarName   )  ! Optional output
!
! INPUT:
!       Channel:           The sensor channel number.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT:
!       Band:              The band number for the sensor channel SRF.
!                          If not specified, the default value is 1.
!                          UNITS:      N/A
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT:
!       n_Bands_DimName:   The dimension name for the specified SRF band
!                          and sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Points_DimName:  The dimension names for the number of points used
!                          to specfiy the SRF data for the specified band and
!                          sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f1_VarName:        The variable name for the band begin frequencies
!                          of the specified sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f2_VarName:        The variable name for the band end frequencies
!                          of the specified sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency_VarName: The variable names for the SRF frequency array
!                          of the specified band and sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Response_VarName:  The variable names for the SRF response arrays
!                          of the specified band and sensor channel.
!                          UNITS:      N/A
!                          TYPE:       CHARACTER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!------------------------------------------------------------------------------

  SUBROUTINE Create_Names( &
    Channel          , &  ! Input
    Band             , &  ! Optional input         
    n_Bands_DimName  , &  ! Optional output
    n_Points_DimName , &  ! Optional output
    f1_VarName       , &  ! Optional output
    f2_VarName       , &  ! Optional output
    Frequency_VarName, &  ! Optional output
    Response_VarName   )  ! Optional output
    ! Arguments
    INTEGER,                INTENT(IN)  :: Channel
    INTEGER,      OPTIONAL, INTENT(IN)  :: Band
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: n_Bands_DimName  
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: n_Points_DimName 
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f1_VarName       
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: f2_VarName       
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Frequency_VarName
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Response_VarName 
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
    IF ( PRESENT(n_Bands_DimName) ) n_Bands_DimName = 'ch'//TRIM(ch)//'_n_Bands'
    IF ( PRESENT(f1_VarName     ) ) f1_VarName      = 'ch'//TRIM(ch)//'_f1'
    IF ( PRESENT(f2_VarName     ) ) f2_VarName      = 'ch'//TRIM(ch)//'_f2'
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
  RESULT( err_status )
    ! Arguments
    INTEGER,           INTENT(IN)  :: FileId  
    CHARACTER(*),      INTENT(IN)  :: DimName 
    INTEGER,           INTENT(OUT) :: DimValue
    INTEGER, OPTIONAL, INTENT(OUT) :: DimID   
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_Dim'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: nc_status
    INTEGER :: id
    
    ! Setup
    err_status = SUCCESS


    ! Get the dimension id
    nc_status = NF90_INQ_DIMID( FileId, TRIM(DimName), id )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//TRIM(DimName)//' - '//TRIM(NF90_STRERROR(nc_status))
      CALL Read_Dim_CleanUp(); RETURN
    END IF


    ! Get the dimension value
    nc_status = NF90_INQUIRE_DIMENSION( FileId, id, Len = DimValue )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//TRIM(DimName)//' - '//TRIM(NF90_STRERROR(nc_status))
      CALL Read_Dim_CleanUp(); RETURN
    END IF


    ! Assign return arguments
    IF ( PRESENT(DimId) ) Dimid = id

  CONTAINS
  
    SUBROUTINE Read_Dim_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_status )
    END SUBROUTINE Read_Dim_CleanUp
    
  END FUNCTION Read_Dim
 
END MODULE oSRF_File_Define
 
