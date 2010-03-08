!
! oSRF_Define
!
! Module defining the oSRF object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2009
!                       paul.vandelst@noaa.gov
 
MODULE oSRF_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE Integrate_Utility    , ONLY: Integral
  USE SensorInfo_Parameters, ONLY: INVALID_WMO_SATELLITE_ID, &
                                   INVALID_WMO_SENSOR_ID   , &
                                   N_SENSOR_TYPES          , &
                                   INVALID_SENSOR          , &
                                   MICROWAVE_SENSOR        , &
                                   INFRARED_SENSOR         , &
                                   VISIBLE_SENSOR          , &
                                   ULTRAVIOLET_SENSOR      , &  
                                   SENSOR_TYPE_NAME
  USE PtrArr_Define        , ONLY: PtrArr_type      , &
                                   OPERATOR(==)     , &
                                   PtrArr_Associated, &
                                   PtrArr_Destroy   , &
                                   PtrArr_Create    , &
                                   PtrArr_SetValue  , &
                                   PtrArr_GetValue  , &
                                   PtrArr_Inspect
  USE Planck_Functions     , ONLY: Planck_Temperature, &
                                   Planck_Radiance
  USE Fundamental_Constants , ONLY: C_1, &
                                    C_2
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Derived type definitions
  PUBLIC :: oSRF_type
  ! Parameters
  PUBLIC :: OSRF_RELEASE
  PUBLIC :: OSRF_VERSION
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! Procedures
  PUBLIC :: oSRF_IsFlagSet, oSRF_SetFlag, oSRF_ClearFlag, oSRF_DefaultFlags
  PUBLIC :: oSRF_Associated
  PUBLIC :: oSRF_Destroy
  PUBLIC :: oSRF_Create
  PUBLIC :: oSRF_SetValue
  PUBLIC :: oSRF_GetValue
  PUBLIC :: oSRF_Inspect
  PUBLIC :: oSRF_Info
  PUBLIC :: oSRF_Integrate
  PUBLIC :: oSRF_DefineVersion
  PUBLIC :: oSRF_Central_Frequency
  PUBLIC :: oSRF_Polychromatic_Coefficients
  PUBLIC :: oSRF_Convolve
  


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE oSRF_Equal
  END INTERFACE OPERATOR(==)
  
  
  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL = 20  ! Sensor Id length
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: OSRF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: OSRF_VERSION = 1  ! This is just the data version.
  ! Some internal dimensions
  INTEGER, PARAMETER :: MAX_N_PLANCK_COEFFS = 2
  INTEGER, PARAMETER :: MAX_N_POLYCHROMATIC_COEFFS = 2
  ! First Planck function constant (C1) scale factors. Units of C1 are W.m^2.
  ! Length scaling: To convert to W/(m^2.cm^-4) requires a scaling of m->cm,
  !                 which is 100, to the fourth power, which is 1.0e+08.
  ! Power scaling:  To convert to mW.m^2 requires a scaling of 1000.
  REAL(fp), PARAMETER :: C1_LENGTH_SCALE_FACTOR = 1.0e+08_fp
  REAL(fp), PARAMETER :: C1_POWER_SCALE_FACTOR  = 1.0e+03_fp
  REAL(fp), PARAMETER :: C1_SCALE_FACTOR = C1_LENGTH_SCALE_FACTOR * C1_POWER_SCALE_FACTOR
  ! Second Planck function constant (C2) scale factor. Units of C2 are K.m,
  ! So to convert to K.cm, a scaling of 100 is applied.
  REAL(fp), PARAMETER :: C2_SCALE_FACTOR = 100.0_fp
  ! Bit positions of the flags
  INTEGER, PARAMETER :: INTERPOLATED_FLAG    = 0  ! 0==no    , 1==yes
  INTEGER, PARAMETER :: INTEGRATED_FLAG      = 1  ! 0==no    , 1==yes
  INTEGER, PARAMETER :: F0_COMPUTED_FLAG     = 2  ! 0==no    , 1==yes
  INTEGER, PARAMETER :: FREQUENCY_UNITS_FLAG = 3  ! 0==cm^-1 , 1==GHz


  ! --------------------------
  ! oSRF data type definitions
  ! --------------------------
  TYPE :: oSRF_type
    ! Release and version information
    INTEGER :: Release = oSRF_RELEASE
    INTEGER :: Version = oSRF_VERSION
    ! Allocation indicator
    LOGICAL :: Is_Allocated = .FALSE.
    ! Dimension values
    INTEGER :: n_Bands = 0  ! nB
    ! Non-pointer components
    CHARACTER(SL) :: Sensor_ID  = ' '
    INTEGER  :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER  :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER  :: Sensor_Type      = INVALID_SENSOR
    INTEGER  :: Channel          = 0
    REAL(fp) :: Integral        = ZERO
    INTEGER  :: Flags           = 0
    REAL(fp) :: f0              = ZERO
    REAL(fp) :: Planck_Coeffs(MAX_N_PLANCK_COEFFS)               = ZERO
    REAL(fp) :: Polychromatic_Coeffs(MAX_N_POLYCHROMATIC_COEFFS) = ZERO
    ! Pointer components
    INTEGER,           ALLOCATABLE :: n_Points(:)  ! nB
    REAL(fp),          ALLOCATABLE :: f1(:)        ! nB
    REAL(fp),          ALLOCATABLE :: f2(:)        ! nB
    TYPE(PtrArr_type), ALLOCATABLE :: Frequency(:) ! nB
    TYPE(PtrArr_type), ALLOCATABLE :: Response(:)  ! nB
  END TYPE oSRF_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ELEMENTAL FUNCTION oSRF_IsFlagSet(self, flag) RESULT(Is_Set)
    TYPE(oSRF_type), INTENT(IN) :: self
    INTEGER        , INTENT(IN) :: flag
    LOGICAL :: Is_Set
    Is_Set = BTEST(self%Flags,flag)
  END FUNCTION oSRF_IsFlagSet

 
  ELEMENTAL SUBROUTINE oSRF_SetFlag(self, flag)
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    INTEGER        , INTENT(IN)     :: flag
    self%Flags = IBSET(self%Flags,flag)
  END SUBROUTINE oSRF_SetFlag
 

  ELEMENTAL SUBROUTINE oSRF_ClearFlag(self, flag)
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    INTEGER        , INTENT(IN)     :: flag
    self%Flags = IBCLR(self%Flags,flag)
  END SUBROUTINE oSRF_ClearFlag
  
  
  ELEMENTAL SUBROUTINE oSRF_DefaultFlags(self)
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    self%Flags = 0
  END SUBROUTINE oSRF_DefaultFlags
  
  
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Associated
!
! PURPOSE:
!       Function to test the status of the allocatable components
!       of an oSRF object.
!
! CALLING SEQUENCE:
!       Status = oSRF_Associated( oSRF )
!
! OBJECT:
!       oSRF:    oSRF structure which is to have its member's
!                status tested.
!                UNITS:      N/A
!                TYPE:       TYPE(oSRF_type)
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:  The return value is a logical value indicating the
!                status of the oSRF members.
!                .TRUE.  - if ANY of the oSRF allocatable or
!                          pointer members are in use.
!                .FALSE. - if ALL of the oSRF allocatable or
!                          pointer members are not in use.
!                UNITS:      N/A
!                TYPE:       LOGICAL
!                DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  PURE FUNCTION oSRF_Associated(self) RESULT(alloc_status)
    TYPE(oSRF_type), INTENT(IN) :: self
    LOGICAL :: alloc_status
    alloc_status = self%Is_Allocated
  END FUNCTION oSRF_Associated



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Destroy
! 
! PURPOSE:
!       Subroutine to re-initialize oSRF objects.
!
! CALLING SEQUENCE:
!       CALL oSRF_Destroy( oSRF )
!
! OBJECT:
!       oSRF:  Re-initialised oSRF structure.
!              UNITS:      N/A
!              TYPE:       TYPE(oSRF_type)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE SUBROUTINE oSRF_Destroy(self)
    TYPE(oSRF_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%n_Bands = 0
  END SUBROUTINE oSRF_Destroy


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Create
! 
! PURPOSE:
!       Subroutine to create an instance of the oSRF object.
!
! CALLING SEQUENCE:
!       CALL oSRF_Create(oSRF, n_Points)
!
! OBJECT:
!       oSRF:         oSRF object.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       n_Points:     The array of the number of data points to which the
!                     oSRF band data arrays are to be allocated. The number 
!                     of oSRF bands is taken from the size of the n_Points
!                     array. Each element must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  PURE SUBROUTINE oSRF_Create( self, n_Points )
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: self
    INTEGER,         INTENT(IN)  :: n_Points(:)
    ! Local variables
    INTEGER :: alloc_stat
    INTEGER :: i, n_Bands

    ! Check dimension inputs
    IF ( ANY(n_Points < 1) ) RETURN
    
    
    ! Set the number of oSRF bands
    n_Bands = SIZE(n_Points)


    ! Perform the main array allocations
    ALLOCATE( self%n_Points( n_Bands ) , &
              self%f1( n_Bands )       , &
              self%f2( n_Bands )       , &
              self%Frequency( n_Bands ), &
              self%Response( n_Bands ) , &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Allocate the individual band elements
    DO i = 1, n_Bands
      ! Frequency arrays
      CALL PtrArr_Create( self%Frequency(i), n_Points(i) )
      IF ( .NOT. PtrArr_Associated( self%Frequency(i)) ) THEN
        CALL oSRF_Destroy(self); RETURN
      END IF
      ! Response array
      CALL PtrArr_Create( self%Response(i), n_Points(i) )
      IF ( .NOT. PtrArr_Associated( self%Response(i)) ) THEN
        CALL oSRF_Destroy(self); RETURN
      END IF
      ! Assign the n_Points value
      self%n_Points(i) = n_Points(i)
    END DO


    ! Assign the band dimension and initialise arrays
    self%n_Bands = n_Bands
    self%f1 = ZERO
    self%f2 = ZERO


    ! Set the allocation flag
    self%Is_Allocated = .TRUE.
     
  END SUBROUTINE oSRF_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_SetValue
!
! PURPOSE:
!       Function to set the value of an oSRF object component.
!
! CALLING SEQUENCE:
!       Error_Status = CALL oSRF_SetValue( &
!                        oSRF                                       , &
!                        Band                 = Band                , &
!                        Version              = Version             , &
!                        Sensor_Id            = Sensor_Id           , &
!                        WMO_Satellite_Id     = WMO_Satellite_Id    , &
!                        WMO_Sensor_Id        = WMO_Sensor_Id       , &
!                        Sensor_Type          = Sensor_Type         , &
!                        Channel              = Channel             , &
!                        Integral             = Integral            , &
!                        Flags                = Flags               , &
!                        f0                   = f0                  , &
!                        Planck_Coeffs        = Planck_Coeffs       , &
!                        Polychromatic_Coeffs = Polychromatic_Coeffs, &
!                        f1                   = f1                  , &
!                        f2                   = f2                  , &
!                        Frequency            = Frequency           , &
!                        Response             = Response              )
!
! OBJECT:
!       oSRF:                  oSRF object that is to have its properties
!                              modified.
!                              UNITS:      N/A
!                              TYPE:       TYPE(oSRF_type)
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUTS:
!       Band:                  The band number to which the frequency and
!                              response data refer.
!                              If not specified, default value is 1.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  SCALAR
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Version:               The version number of the SRF data.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Sensor_ID:             A character string identifying the sensor and
!                              satellite platform used to contruct filenames.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Satellite_ID:      The WMO code used to identify satellite platforms.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       WMO_Sensor_ID:         The WMO code used to identify sensors.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!       Sensor_Type:           The flag indicating the type of sensor (IR, MW, etc)
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Channel:               The sensor channel for the currenobject.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Integral:              The integrated SRF value.
!                              UNITS:      N/A
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Flags:                 Bit flags set/cleared during SRF processing.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       f0:                    The central frequency of the SRF.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Planck_Coeffs:         Vector of Planck function coefficients for the SRF.
!                              UNITS:      Variable
!                              TYPE:       REAL
!                              DIMENSION:  Rank-1
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Polychromatic_Coeffs:  Vector of polychromatic correction coefficient for the SRF.
!                              UNITS:      Variable
!                              TYPE:       REAL
!                              DIMENSION:  Rank-1
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       f1:                    The begin frequency of the SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       f2:                    The end frequency of the SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Frequency:             The frequency grid for an SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1)
!                              TYPE:       REAL
!                              DIMENSION:  n_Points
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Response:              The response data for an SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      N/A
!                              TYPE:       REAL
!                              DIMENSION:  n_Points
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error
!                              status. The error codes are defined in the
!                              Message_Handler module.
!                              If == SUCCESS the property set succeeded
!                                 == FAILURE an error occurred
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION oSRF_SetValue( &
    self                , &  ! In/output
    Band                , &  ! Optional input
    Version             , &  ! Optional input
    Sensor_Id           , &  ! Optional input
    WMO_Satellite_Id    , &  ! Optional input
    WMO_Sensor_Id       , &  ! Optional input
    Sensor_Type         , &  ! Optional input
    Channel             , &  ! Optional input
    Integral            , &  ! Optional input
    Flags               , &  ! Optional input
    f0                  , &  ! Optional input
    Planck_Coeffs       , &  ! Optional input
    Polychromatic_Coeffs, &  ! Optional input
    f1                  , &  ! Optional input
    f2                  , &  ! Optional input
    Frequency           , &  ! Optional input
    Response            ) &  ! Optional input
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type),        INTENT(IN OUT) :: self
    INTEGER,      OPTIONAL, INTENT(IN)     :: Band                
    INTEGER,      OPTIONAL, INTENT(IN)     :: Version             
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Sensor_Id           
    INTEGER,      OPTIONAL, INTENT(IN)     :: WMO_Satellite_Id    
    INTEGER,      OPTIONAL, INTENT(IN)     :: WMO_Sensor_Id       
    INTEGER,      OPTIONAL, INTENT(IN)     :: Sensor_Type         
    INTEGER,      OPTIONAL, INTENT(IN)     :: Channel             
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Integral            
    INTEGER,      OPTIONAL, INTENT(IN)     :: Flags               
    REAL(fp),     OPTIONAL, INTENT(IN)     :: f0                  
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Planck_Coeffs(SIZE(self%Planck_Coeffs))       
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Polychromatic_Coeffs(SIZE(self%Polychromatic_Coeffs))
    REAL(fp),     OPTIONAL, INTENT(IN)     :: f1                  
    REAL(fp),     OPTIONAL, INTENT(IN)     :: f2                  
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Frequency(:)           
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Response(:)            
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_Band
    
    ! Set up
    err_stat = SUCCESS
    
    
    ! Check band argument
    l_Band = 1
    IF ( PRESENT(Band) ) THEN
      l_Band = Band
      IF ( l_Band < 1 .OR. l_Band > self%n_Bands ) THEN
        WRITE( msg, '("Invalid band, ",i0,", specified for oSRF")' ) l_Band
        CALL CleanUp(); RETURN
      END IF
    END IF


    ! Set data with defined sizes
    IF ( PRESENT(Version             ) ) self%Version              = Version  
    IF ( PRESENT(Sensor_Id           ) ) self%Sensor_Id            = Sensor_Id       
    IF ( PRESENT(WMO_Satellite_Id    ) ) self%WMO_Satellite_Id     = WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id       ) ) self%WMO_Sensor_Id        = WMO_Sensor_Id   
    IF ( PRESENT(Sensor_Type         ) ) self%Sensor_Type          = Sensor_Type     
    IF ( PRESENT(Channel             ) ) self%Channel              = Channel         
    IF ( PRESENT(Integral            ) ) self%Integral             = Integral        
    IF ( PRESENT(Flags               ) ) self%Flags                = Flags           
    IF ( PRESENT(f0                  ) ) self%f0                   = f0              
    IF ( PRESENT(Planck_Coeffs       ) ) self%Planck_Coeffs        = Planck_Coeffs                       
    IF ( PRESENT(Polychromatic_Coeffs) ) self%Polychromatic_Coeffs = Polychromatic_Coeffs                
    IF ( PRESENT(f1                  ) ) self%f1(l_Band)           = f1            
    IF ( PRESENT(f2                  ) ) self%f2(l_Band)           = f2            


    ! Set frequency data
    IF ( PRESENT(Frequency) ) THEN
      err_stat = PtrArr_SetValue( self%Frequency(l_Band), Arr=Frequency )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error setting frequency for band ",i0)' ) l_Band
        CALL CleanUp(); RETURN
      END IF
      ! ...Set the frequency limits
      self%f1(l_Band) = Frequency(1)
      self%f2(l_Band) = Frequency(SIZE(Frequency))
    END IF
    
    
    ! Set Response data
    IF ( PRESENT(Response) ) THEN
      err_stat = PtrArr_SetValue( self%Response(l_Band), Arr=Response )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error setting Response for band ",i0)' ) l_Band
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp    
    
  END FUNCTION oSRF_SetValue
  



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_GetValue
!
! PURPOSE:
!       Function to get the value of an oSRF object component.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_GetValue( &
!                        oSRF                                           , &
!                        Band                   = Band                  , &
!                        Version                = Version               , &
!                        Sensor_Id              = Sensor_Id             , &
!                        WMO_Satellite_Id       = WMO_Satellite_Id      , &
!                        WMO_Sensor_Id          = WMO_Sensor_Id         , &
!                        Sensor_Type            = Sensor_Type           , &
!                        Channel                = Channel               , &
!                        Integral               = Integral              , &
!                        Flags                  = Flags                 , &
!                        f0                     = f0                    , &
!                        n_Planck_Coeffs        = n_Planck_Coeffs       , &
!                        n_Polychromatic_Coeffs = n_Polychromatic_Coeffs, &
!                        Planck_Coeffs          = Planck_Coeffs         , &
!                        Polychromatic_Coeffs   = Polychromatic_Coeffs  , &
!                        n_Points               = n_Points              , &
!                        f1                     = f1                    , &
!                        f2                     = f2                    , &
!                        Frequency              = Frequency             , &
!                        Response               = Response                )  
!
! OBJECT:
!       oSRF:                   oSRF object that is to have its properties
!                               retrieved.
!                               UNITS:      N/A
!                               TYPE:       TYPE(oSRF_type)
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!       Band:                   The band number to which the frequency and
!                               response data refer.
!                               If not specified, default value is 1.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  SCALAR
!                               ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUTS:
!       Version:                The version number of the SRF data.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_ID:              A character string identifying the sensor and
!                               satellite platform used to contruct filenames.
!                               UNITS:      N/A
!                               TYPE:       CHARACTER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_ID:       The WMO code used to identify satellite platforms.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_ID:          The WMO code used to identify sensors.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!       Sensor_Type:            The flag indicating the type of sensor (IR, MW, etc)
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Channel:                The sensor channel for the currenobject.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Integral:               The integrated SRF value.
!                               UNITS:      N/A
!                               TYPE:       REAL
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Flags:                  Bit flags set/cleared during SRF processing.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f0:                     The central frequency of the SRF.
!                               UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                               TYPE:       REAL
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Planck_Coeffs:        Number of Planck function coefficients for the SRF.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Polychromatic_Coeffs: Number of polychromatic correction coefficient for the SRF.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Planck_Coeffs:          Vector of Planck function coefficients for the SRF.
!                               UNITS:      Variable
!                               TYPE:       REAL
!                               DIMENSION:  Rank-1
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Polychromatic_Coeffs:   Vector of polychromatic correction coefficient for the SRF.
!                               UNITS:      Variable
!                               TYPE:       REAL
!                               DIMENSION:  Rank-1
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Points:               The number of points that specify the band frequency
!                               and responmse data.
!                               Used in conjunction with the Band keyword argument.
!                               UNITS:      N/A
!                               TYPE:       INTEGER
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f1:                     The begin frequency of the SRF band.
!                               Used in conjunction with the Band keyword argument.
!                               UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                               TYPE:       REAL
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f2:                     The end frequency of the SRF bands.
!                               Used in conjunction with the Band keyword argument.
!                               UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                               TYPE:       REAL
!                               DIMENSION:  Scalar
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency:              The frequency grid for an SRF band.
!                               Used in conjunction with the Band keyword argument.
!                               UNITS:      Inverse centimetres (cm^-1)
!                               TYPE:       REAL
!                               DIMENSION:  n_Points
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Response:               The response data for an SRF band.
!                               Used in conjunction with the Band keyword argument.
!                               UNITS:      N/A
!                               TYPE:       REAL
!                               DIMENSION:  n_Points
!                               ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:          The return value is an integer defining the error
!                              status. The error codes are defined in the
!                              Message_Handler module.
!                              If == SUCCESS the property get succeeded
!                                 == FAILURE an error occurred
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION oSRF_GetValue( &
    self                  , &  ! Input
    Band                  , &  ! Optional input
    Version               , &  ! Optional output
    Sensor_Id             , &  ! Optional output
    WMO_Satellite_Id      , &  ! Optional output
    WMO_Sensor_Id         , &  ! Optional output
    Sensor_Type           , &  ! Optional output
    Channel               , &  ! Optional output
    Integral              , &  ! Optional output
    Flags                 , &  ! Optional output
    f0                    , &  ! Optional output
    n_Planck_Coeffs       , &  ! Optional output
    n_Polychromatic_Coeffs, &  ! Optional output
    Planck_Coeffs         , &  ! Optional output
    Polychromatic_Coeffs  , &  ! Optional output
    n_Points              , &  ! Optional output
    f1                    , &  ! Optional output
    f2                    , &  ! Optional output
    Frequency             , &  ! Optional output
    Response              ) &  ! Optional output
  RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type),        INTENT(IN)  :: self
    INTEGER,      OPTIONAL, INTENT(IN)  :: Band                
    INTEGER,      OPTIONAL, INTENT(OUT) :: Version             
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Sensor_Id           
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Satellite_Id    
    INTEGER,      OPTIONAL, INTENT(OUT) :: WMO_Sensor_Id       
    INTEGER,      OPTIONAL, INTENT(OUT) :: Sensor_Type         
    INTEGER,      OPTIONAL, INTENT(OUT) :: Channel             
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Integral            
    INTEGER,      OPTIONAL, INTENT(OUT) :: Flags               
    REAL(fp),     OPTIONAL, INTENT(OUT) :: f0                  
    REAL(fp),     OPTIONAL, INTENT(OUT) :: n_Planck_Coeffs
    REAL(fp),     OPTIONAL, INTENT(OUT) :: n_Polychromatic_Coeffs
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Planck_Coeffs(SIZE(self%Planck_Coeffs))       
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Polychromatic_Coeffs(SIZE(self%Polychromatic_Coeffs))
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Points
    REAL(fp),     OPTIONAL, INTENT(OUT) :: f1                  
    REAL(fp),     OPTIONAL, INTENT(OUT) :: f2                  
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Frequency(:)           
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Response(:)            
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF_GetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_Band
    
    ! Set up
    err_stat = SUCCESS
    
    
    ! Check band argument
    l_Band = 1
    IF ( PRESENT(Band) ) THEN
      l_Band = Band
      IF ( l_Band < 1 .OR. l_Band > self%n_Bands ) THEN
        WRITE( msg, '("Invalid band, ",i0,", specified for input oSRF")' ) l_Band
        CALL CleanUp(); RETURN
      END IF
    END IF


    ! Get data with defined sizes
    IF ( PRESENT(Version               ) ) Version                = self%Version  
    IF ( PRESENT(Sensor_Id             ) ) Sensor_Id              = self%Sensor_Id       
    IF ( PRESENT(WMO_Satellite_Id      ) ) WMO_Satellite_Id       = self%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id         ) ) WMO_Sensor_Id          = self%WMO_Sensor_Id   
    IF ( PRESENT(Sensor_Type           ) ) Sensor_Type            = self%Sensor_Type     
    IF ( PRESENT(Channel               ) ) Channel                = self%Channel         
    IF ( PRESENT(Integral              ) ) Integral               = self%Integral        
    IF ( PRESENT(Flags                 ) ) Flags                  = self%Flags           
    IF ( PRESENT(f0                    ) ) f0                     = self%f0              
    IF ( PRESENT(n_Planck_Coeffs       ) ) n_Planck_Coeffs        = MAX_N_PLANCK_COEFFS                       
    IF ( PRESENT(n_Polychromatic_Coeffs) ) n_Polychromatic_Coeffs = MAX_N_POLYCHROMATIC_COEFFS                
    IF ( PRESENT(Planck_Coeffs         ) ) Planck_Coeffs          = self%Planck_Coeffs                   
    IF ( PRESENT(Polychromatic_Coeffs  ) ) Polychromatic_Coeffs   = self%Polychromatic_Coeffs            
    IF ( PRESENT(n_Points              ) ) n_Points               = self%n_Points(l_Band)    
    IF ( PRESENT(f1                    ) ) f1                     = self%f1(l_Band)    
    IF ( PRESENT(f2                    ) ) f2                     = self%f2(l_Band)    


    ! Get frequency data
    IF ( PRESENT(Frequency) ) THEN
      err_stat = PtrArr_GetValue( self%Frequency(l_Band), Arr=Frequency )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error getting frequency for band ",i0)' ) l_Band
        CALL CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Get Response data
    IF ( PRESENT(Response) ) THEN
      err_stat = PtrArr_GetValue( self%Response(l_Band), Arr=Response )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg, '("Error Getting Response for band ",i0)' ) l_Band
        CALL CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE CleanUp()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE CleanUp
    
  END FUNCTION oSRF_GetValue
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Inspect
!
! PURPOSE:
!       Function to view the contents of an oSRF structure.
!
! CALLING SEQUENCE:
!       CALL oSRF_Inspect( oSRF )
!
! OBJECT:
!       oSRF:          oSRF structure to inspect.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE oSRF_Inspect( self )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN) :: self
    ! Local arguments
    INTEGER :: n
    ! Output the oSRF components     
    WRITE(*,'(1x,"oSRF OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n_Bands              :",1x,i0)') self%n_Bands
    ! Scalar data
    WRITE(*,'(3x,"Release              : ",i0)'            ) self%Release
    WRITE(*,'(3x,"Version              : ",i0)'            ) self%Version  
    WRITE(*,'(3x,"Sensor_Id            : ", a)'            ) self%Sensor_Id       
    WRITE(*,'(3x,"WMO_Satellite_Id     : ",i0)'            ) self%WMO_Satellite_Id
    WRITE(*,'(3x,"WMO_Sensor_Id        : ",i0)'            ) self%WMO_Sensor_Id   
    WRITE(*,'(3x,"Sensor_Type          : ", a)'            ) SENSOR_TYPE_NAME(self%Sensor_Type)
    WRITE(*,'(3x,"Channel              : ",i0)'            ) self%Channel         
    WRITE(*,'(3x,"Integral             : ",es13.6)'        ) self%Integral        
    WRITE(*,'(3x,"Flags                : ",i0)'            ) self%Flags           
    WRITE(*,'(3x,"f0                   : ",es13.6)'        ) self%f0              
    WRITE(*,'(3x,"Planck_Coeffs        : ",2(es13.6,1x))'  ) self%Planck_Coeffs       
    WRITE(*,'(3x,"Polychromatic_Coeffs : ",3(es13.6,:,1x))') self%Polychromatic_Coeffs
    ! Band data
    DO n = 1, self%n_Bands
      WRITE(*,'(3x,"BAND NUMBER ",i0)') n
      WRITE(*,'(5x,"n_Points  : ",i0)'    ) self%n_Points(n)
      WRITE(*,'(5x,"f1        : ",es13.6)') self%f1(n)
      WRITE(*,'(5x,"f2        : ",es13.6)') self%f2(n)
      WRITE(*,'(5x,"Frequency :")')
      CALL PtrArr_Inspect( self%Frequency(n) )
      WRITE(*,'(5x,"Response  :")')
      CALL PtrArr_Inspect( self%Response(n) )
    END DO
  END SUBROUTINE oSRF_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Info
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the oSRF data structure.
!
! CALLING SEQUENCE:
!       CALL oSRF_Info( oSRF, Info )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
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

  SUBROUTINE oSRF_Info( self, Info )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN)  :: self
    CHARACTER(*)   , INTENT(OUT) :: Info
    ! Parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Setup
    Info = ' '
    IF ( .NOT. oSRF_Associated(self) ) RETURN
    
    
    ! Write the required data to the local string
    WRITE( LongString,'(a,1x,"oSRF RELEASE.VERSION: ",i0,".",i2.2,2x,a,1x,&
                       &"CHANNEL:",i0,2x,&
                       &"N_BANDS=",i0,2x,&
                       &"N_POINTS=",99(i0,:,","))' ) &
                       ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                       self%Release, self%Version, &
                       TRIM(self%Sensor_ID), &
                       self%Channel, &
                       self%n_Bands, &
                       self%n_Points


    ! Trim the output based on the
    ! dummy argument string length
    Info = LongString(1:MIN(LEN(Info), LEN_TRIM(LongString)))

  END SUBROUTINE oSRF_Info


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Integrate
!
! PURPOSE:
!       Function to integrate the response and set the integrated value
!       in an oSRF object.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_Integrate( oSRF )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The INTEGRAL component of the oSRF object is set in this function.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_Integrate( self ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF::Integrate'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat
    INTEGER :: n, n_Points
    REAL(fp), ALLOCATABLE :: f(:), r(:)
    REAL(fp):: Int_SRF, Int_Band

    ! Setup
    err_stat = SUCCESS
    Int_SRF = ZERO
    ! ...Check object contains something
    IF ( .NOT. oSRF_Associated(self) ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, 'Input oSRF object is empty.', err_stat )
      RETURN
    END IF


    ! Sum up band integrals
    Band_Loop: DO n = 1, self%n_Bands
    
      ! Get band response
      ! ...Number of band points
      err_stat = oSRF_GetValue( self, n, n_Points=n_Points )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error occurred retrieving band#",i0," n_Points")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      ! ...Allocate local arrays
      ALLOCATE( f(n_Points), r(n_Points), STAT=alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        err_stat = FAILURE
        WRITE( msg,'("Error allocating band#",i0," arrays. STAT=",i0)' ) n, alloc_stat
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      ! ...Number of band points
      err_stat = oSRF_GetValue( self, n, Frequency=f, Response=r )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error occurred retrieving band#",i0," data")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      
      ! Integrate the band
      err_stat = Integral(f, r, Int_Band)
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error integrating band#",i0," response")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      
      ! Accumulate
      Int_SRF = Int_SRF + Int_Band
      
      ! Clean up
      DEALLOCATE( f, r )
      
    END DO Band_Loop
    
    ! Save the integrated value
    err_stat = oSRF_SetValue( self, Integral=Int_SRF )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error occurred saving the oSRF integral',err_stat )
      RETURN
    END IF
    CALL oSRF_SetFlag( self, INTEGRATED_FLAG )

  END FUNCTION oSRF_Integrate
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Planck_Coefficients
!
! PURPOSE:
!       Function to compute Planck coefficients for an oSRF_type.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_Planck_Coefficients( oSRF )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!      
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The Planck Coefficient components are set in oSRF.
!
!:sdoc-:
!------------------------------------------------------------------------------    

!  FUNCTION oSRF_Planck_Coefficients( self ) RESULT( err_stat )
!    ! Arguments
!    TYPE(oSRF_type), INTENT(IN OUT) :: self
!    ! Function result
!    INTEGER :: err_stat
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF::Planck_Coefficients'
!    ! Local variables
!    REAL(fp) :: Planck_Coeffs(MAX_N_PLANCK_COEFFS)
!
!    ! Set up
!    err_stat = SUCCESS
!    ! ...Check object contains something
!    IF ( .NOT. oSRF_Associated(self) ) THEN
!      err_stat = FAILURE
!      CALL Display_Message( ROUTINE_NAME, 'Input oSRF object is empty.', err_stat )
!      RETURN
!    END IF
!
!    ! Compute the central frequency if necessary
!    IF ( .NOT. oSRF_IsFlagSet(self, F0_COMPUTED_FLAG) ) &
!      CALL oSRF_Central_Frequency( self )
!
!    ! Compute the Planck coefficients
!    Planck_Coeffs(1) = C1_SCALE_FACTOR * C_1 * ( self%f0**3 )
!    Planck_Coeffs(2) = C2_SCALE_FACTOR * C_2 *   self%f0 
!    ! Save the computed Planck coefficients
!    err_stat = oSRF_SetValue( self, Planck_Coeffs = Planck_Coeffs )
!    IF ( err_stat /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME,'Error occurred saving the oSRF Planck Coefficients',err_stat )
!      RETURN
!    END IF
!    
!    
!  END FUNCTION oSRF_Planck_Coefficients
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Central_Frequency
!
! PURPOSE:
!       Function to compute the central frequency component of an oSRF_type.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_Central_Frequency( oSRF )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!      
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! SIDE EFFECTS:
!       The central frequency component is set in oSRF and the corresponding
!       bit flag is set to 1.
!
!:sdoc-:
!------------------------------------------------------------------------------    
  
  FUNCTION oSRF_Central_Frequency( self ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF::Central_Frequency'
    ! Local variables
    REAL(fp) :: f0

    ! Set up
    err_stat = SUCCESS
    ! ...Check object contains something
    IF ( .NOT. oSRF_Associated(self) ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, 'Input oSRF object is empty.', err_stat )
      RETURN
    END IF

    ! Compute the oSRF first moment
    err_stat = oSRF_Convolve( self, self%Frequency, f0 )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Error convolving oSRF with frequency.', err_stat )
      RETURN
    END IF
    
    ! Save the central frequency value
    err_stat = oSRF_SetValue( self, f0 = f0 )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error occurred saving the oSRF central frequency',err_stat )
      RETURN
    END IF
    CALL oSRF_SetFlag( self, F0_COMPUTED_FLAG )
    
  END FUNCTION oSRF_Central_Frequency
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Convolve
!
! PURPOSE:
!       Function to convolve quantities with oSRF.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_Convolve( oSRF, p, y )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
! INPUT:
!       p:             Structure array holding quantities
!                      to be convolved.
!                      UNITS:      N/A
!                      TYPE:       TYPE(PtrArr_type)
!                      DIMENSION:  Rank-1
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT:    
!      y:              Convolved quantity
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!      
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------  
  
  FUNCTION oSRF_Convolve( self, p, y ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type)  , INTENT(IN OUT) :: self
    TYPE(PtrArr_type), INTENT(IN)     :: p(:)
    REAL(fp)         , INTENT(OUT)    :: y
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF::Convolve'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: err_stat
    INTEGER :: alloc_stat
    INTEGER :: n, n_Points
    REAL(fp), ALLOCATABLE :: f(:), r(:)
    REAL(fp):: Int_SRF, Int_Band

    ! Setup
    y = ZERO
    ! ...Check object contains something
    IF ( .NOT. oSRF_Associated(self) ) RETURN
    ! ...Check the band count is the same
    IF ( self%n_Bands /= SIZE(p) ) THEN
      CALL Display_Message( ROUTINE_NAME, 'Input data does not conform with oSRF.', FAILURE )
      RETURN
    END IF
    ! ...Integrate oSRF if necessary
    IF ( .NOT. oSRF_IsFlagSet(self, INTEGRATED_FLAG) ) &
      CALL oSRF_Integrate( self )
    

    ! Sum up band integrals
    Band_Loop: DO n = 1, self%n_Bands
    
      ! Get band response
      ! ...Number of band points
      err_stat = oSRF_GetValue( self, n, n_Points=n_Points )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error occurred retrieving band#",i0," n_Points")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      ! ...Check that the number of bands agrees with the input
      IF ( p(n)%n /= n_Points ) THEN
        WRITE( msg,'("oSRF and input data have different sizes for band#",i0)' ) n
        CALL Display_Message( ROUTINE_NAME,msg,FAILURE )
        RETURN
      END IF
      ! ...Allocate local arrays
      ALLOCATE( f(n_Points), r(n_Points), STAT=alloc_stat )
      IF ( alloc_stat /= 0 ) THEN
        err_stat = FAILURE
        WRITE( msg,'("Error allocating band#",i0," arrays. STAT=",i0)' ) n, alloc_stat
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      ! ...The band data
      err_stat = oSRF_GetValue( self, n, Frequency=f, Response=r )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error occurred retrieving band#",i0," data")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      
      ! Integrate the band
      err_stat = Integral(f, (p(n)%arr * r), Int_Band)
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error integrating band#",i0," response")' ) n
        CALL Display_Message( ROUTINE_NAME,msg,err_stat )
        RETURN
      END IF
      
      ! Accumulate
      y = y + Int_Band
      
      ! Clean up
      DEALLOCATE( f, r )
      
    END DO Band_Loop
    
    
    ! Normalise the integrated value
    err_stat = oSRF_GetValue( self, Integral=Int_SRF )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error occurred saving the oSRF integral',err_stat )
      RETURN
    END IF
    y = y / Int_SRF

  END FUNCTION oSRF_Convolve
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_Polychromatic_Coefficients
!
! PURPOSE:
!       Function to compute polychromatic coeffs for an oSRF object.
!
! CALLING SEQUENCE:
!       Error_Status = oSRF_Polychromatic_Coefficients( oSRF )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!      
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the integration was successful
!                        == FAILURE an error occurred processing the input
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
! SIDE EFFECTS:
!       The polychromatic component of the oSRF object is set.
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION oSRF_Polychromatic_Coefficients( self ) RESULT( err_stat )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSRF::Polychromatic_Coefficients'
    INTEGER,  PARAMETER :: N_TEMPERATURES  = 17
    REAL(fp), PARAMETER :: MIN_TEMPERATURE = 180.0_fp
    REAL(fp), PARAMETER :: MAX_TEMPERATURE = 340.0_fp
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER  :: alloc_stat
    INTEGER  :: i, n
    REAL(fp) :: Convolved_Radiance
    REAL(fp) :: x_Temperature(N_TEMPERATURES)
    REAL(fp) :: y_Effective_Temperature(N_TEMPERATURES) 
    REAL(fp) :: PolyChromatic_Coeffs(MAX_N_POLYCHROMATIC_COEFFS)
    TYPE(PtrArr_type) :: Radiance(self%n_Bands)
    
    ! Setup
    err_stat = SUCCESS
    ! ...Check object contains something
    IF ( .NOT. oSRF_Associated(self) ) THEN
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, 'Input oSRF object is empty.', err_stat )
      RETURN
    END IF
    
    ! Generate the monochromatic temperatures
    ! ---------------------------------------
    x_Temperature = (/(REAL(i-1,fp),i=1,N_TEMPERATURES)/) / REAL(N_TEMPERATURES-1,fp)
    x_Temperature = (x_Temperature * ( MAX_TEMPERATURE-MIN_TEMPERATURE )) + MIN_TEMPERATURE
    
    ! Allocate the radiance dimensions for each band
    DO n = 1, self%n_Bands
      CALL PtrArr_Create(Radiance(n), self%n_Points(n))
    END DO
    
    ! Generate the polychromatic temperatures
    ! ---------------------------------------
    Temperature_Loop: DO i = 1, N_TEMPERATURES 
    
      ! Compute polychromatic radiances
      Band_Loop: DO n = 1, self%n_Bands
        
        err_stat = Planck_Radiance( self%Frequency(n)%Arr, &
                                    x_Temperature(i),      &
                                    Radiance(n)%Arr        )
        IF ( err_stat /= SUCCESS ) THEN
          WRITE( msg,'("Error calculating radiances at T = ",f5.1," K.")' ) &
                          x_Temperature(i)
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM(msg), &
                                FAILURE )
          RETURN
        END IF    
        
      END DO Band_Loop
      
      ! Convolve the polychromatic radiances
      err_stat = oSRF_Convolve( self, Radiance, Convolved_Radiance )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error calculating the convolved radiance at T = ",f5.1," K.")' ) &
                        x_Temperature(i)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(msg), &
                              FAILURE )
        RETURN
      END IF    
      
      ! ...compute central frequency if necessary
      IF ( .NOT. oSRF_IsFlagSet(self, F0_COMPUTED_FLAG) ) &
      CALL oSRF_Integrate( self )
      
      ! Compute the effective temperature
      err_stat = Planck_Temperature( self%f0, &
                                     Convolved_Radiance, &
                                     y_Effective_Temperature(i) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error calculating the polychromatic temperature at T = ",f5.1," K.")' ) &
                        x_Temperature(i)
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM(msg), &
                              FAILURE )
        RETURN
      END IF 
      
    END DO Temperature_Loop
      
    ! Fit the mono- and polychromatic temperatures
    err_stat = Least_Squares_Linear_Fit( x_Temperature, &
                                         y_Effective_Temperature, &
                                         Polychromatic_Coeffs(1), &
                                         Polychromatic_Coeffs(2)  )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error calculating polychromatic coefficients', &
                            FAILURE )
      RETURN
    END IF
    
    ! Save the polychromatic coefficients
    err_stat = oSRF_SetValue( self, Polychromatic_Coeffs = Polychromatic_Coeffs )
    IF ( err_stat /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME,'Error occurred saving the oSRF Polychromatic coeffs',err_stat )
      RETURN
    END IF
    
  END FUNCTION oSRF_Polychromatic_Coefficients
    
!--------------------------------------------------------------------------------
!
! NAME:
!       Least_Squares_Linear_Fit
!
! PURPOSE:
!       Function to perform a least squares linear fit on the input 
!       polychromatic and monochromatic temperature data.
!
! CALLING SEQUENCE:
!       Error_Status = Least_Squares_Linear_Fit( x, y,        &  ! Input
!                                                a, b,        &  ! Output
!                                                yFit = yFit, &  ! Optional output
!                                                SSE  = SSE,  &  ! Optional output
!                                                MSE  = MSE,  &  ! Optional output
!                                                Message_Log = Message_Log ) !  Error messaging
!
! INPUT ARGUMENTS:
!       x:               Input ordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the true temperature.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1
!                        ATTRIBUTES: INTENT( IN )
!
!       y:               Input coordinate data on which to perform the fit
!                          y = a + bx
!                        Corresponds to the effective temperature due to
!                        polychromaticity.
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (Same size as x)
!                        ATTRIBUTES: INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       a:               Offset coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
!       b:               Slope coefficient that satisfies the fit criteria
!                        for the relationship
!                          y = a + bx
!                        UNITS:      Kelvin/Kelvin (K/K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       yFit:            Predicted coordinate (effective temperature) data,
!                          yFit = a + bx
!                        UNITS:      Kelvin (K)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Rank-1 (Same size as y)
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       SSE:             The residual sum of the squares of the fit to the
!                        input data,
!                                 __  N
!                                \                    2
!                          SSE =  > ( Y(i) - YFit(i) )
!                                /__
!                                    i=1
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       MSE:             The residual mean square of the fit to the
!                        input data,
!                                
!                                 SSE
!                          MSE = -----
!                                 N-2
!
!                        where N == number of input data points
!
!                        UNITS:      Kelvin^2 (K^2)
!                        TYPE:       REAL(fp)
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:    The return value is an integer defining the
!                        error status. The error codes are defined in
!                        the ERROR_HANDLER module.
!                        If == SUCCESS the regression fit was successful.
!                           == FAILURE  an error occurred
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------
  FUNCTION Least_Squares_Linear_Fit( x, y,         &  ! Input
                                     a, b,         &  ! Output
                                     yFit,         &  ! Optional output
                                     SSE,          &  ! Optional output
                                     MSE,          &  ! Optional output
                                     Message_Log ) &  ! Error messaging
                                   RESULT( Error_Status )
    ! Arguments
    REAL(fp),                INTENT(IN)  :: x(:)
    REAL(fp),                INTENT(IN)  :: y(:)
    REAL(fp),                INTENT(OUT) :: a
    REAL(fp),                INTENT(OUT) :: b
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: yFit(:)
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: SSE
    REAL(fp),     OPTIONAL,  INTENT(OUT) :: MSE
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Least_Squares_Linear_Fit'
    REAL(fp),     PARAMETER :: TOLERANCE = EPSILON(1.0_fp)
    ! Local variables
    INTEGER :: n
    REAL(fp) :: xAverage
    REAL(fp) :: yAverage
    REAL(fp) :: sum_dx2
    REAL(fp) :: yCalculated(SIZE(y))
    REAL(fp) :: Residual_Sum_of_Squares
    REAL(fp) :: Residual_Mean_Square   


    ! Set up
    ! ------
    Error_Status = SUCCESS

    ! Check input
    n = SIZE(x)
    IF ( n < 3 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input data must be at least 3 points.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( SIZE(y) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sizes of input X,Y arguments are inconsistent', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
    IF ( PRESENT(yFit) ) THEN
      IF ( SIZE(yFit) /= n ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Sizes of output YFIT argument is inconsistent', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! Calculate averages
    ! ------------------
    xAverage = SUM(x) / REAL(n,fp)
    yAverage = SUM(y) / REAL(n,fp)


    ! Calculate the sums of the square of the mean difference for X
    ! -------------------------------------------------------------
    sum_dx2 = SUM(( x-xAverage )**2)
    IF ( sum_dx2 < TOLERANCE ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Sum of the squares of mean difference for X is zero.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! Calculate coefficients
    ! ----------------------
    b = SUM(( x-xAverage )*( y-yAverage )) / sum_dx2
    a = yAverage - ( b*xAverage )


    ! Calculate the regression Y values
    ! ---------------------------------
    yCalculated = a + ( b*x )
    Residual_Sum_of_Squares = SUM(( y-yCalculated )**2)
    Residual_Mean_Square    = Residual_Sum_of_Squares / REAL(n-2,fp)


    ! Assign optional arguments
    ! -------------------------
    IF ( PRESENT(yFit) ) yFit = yCalculated
    IF ( PRESENT(SSE)  ) SSE  = Residual_Sum_of_Squares
    IF ( PRESENT(MSE)  ) MSE  = Residual_Mean_Square

  END FUNCTION Least_Squares_Linear_Fit      
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       oSRF_DefineVersion
!
! PURPOSE:
!       Subroutine to return the module version information.
!
! CALLING SEQUENCE:
!       CALL oSRF_DefineVersion( Id )
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

  SUBROUTINE oSRF_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE oSRF_DefineVersion


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
!       oSRF_Equal
!
! PURPOSE:
!       Function to test the equality of two oSRF objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = oSRF_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two oSRF objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:      Logical value indicating whether the inputs are equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Scalar
!
!--------------------------------------------------------------------------------

  PURE FUNCTION oSRF_Equal( x, y ) RESULT( is_equal )
    TYPE(oSRF_type) , INTENT(IN)  :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
    
    ! Check the structure association status
    IF ( (.NOT. oSRF_Associated(x)) .OR. &
         (.NOT. oSRF_Associated(y))      ) RETURN

    ! Check contents
    ! ...Scalar Integers/characters
    IF ( (x%Version          /= y%Version         ) .OR. &
         (x%n_Bands          /= y%n_Bands         ) .OR. &
         (x%WMO_Satellite_Id /= y%WMO_Satellite_Id) .OR. &
         (x%WMO_Sensor_Id    /= y%WMO_Sensor_Id   ) .OR. &
         (x%Sensor_Type      /= y%Sensor_Type     ) .OR. &
         (x%Channel          /= y%Channel         ) .OR. &
         (x%Flags            /= y%Flags           ) ) RETURN
    ! ...Reals
    IF ( (x%Integral .EqualTo. y%Integral ) .AND. &
         (x%f0       .EqualTo. y%f0       ) .AND. &
         ALL(x%Planck_Coeffs        .EqualTo. y%Planck_Coeffs       ) .AND. &
         ALL(x%Polychromatic_Coeffs .EqualTo. y%Polychromatic_Coeffs) .AND. &
         ALL(x%n_Points                ==     y%n_Points            ) .AND. &
         ALL(x%Planck_Coeffs        .EqualTo. y%Planck_Coeffs       ) .AND. &
         ALL(x%Planck_Coeffs        .EqualTo. y%Planck_Coeffs       ) .AND. &
         ALL(x%f1                   .EqualTo. y%f1                  ) .AND. &
         ALL(x%f2                   .EqualTo. y%f2                  ) ) is_equal = .TRUE.
    ! ...Structures
    is_equal = is_equal .AND. &
               ALL(x%Frequency == y%Frequency) .AND. &
               ALL(x%Response  == y%Response )
    
  END FUNCTION oSRF_Equal
 
END MODULE oSRF_Define

 
