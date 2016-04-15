!
! oODPS_Define
!
! Module containing the ODPS (Optical Depth in Pressure Space) object definition.
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, 13-Apr-2016
!                   paul.vandelst@noaa.gov
!                   Based on ODPS_Define modeule.
!

MODULE oODPS_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Intrinsic modules
  USE ISO_Fortran_Env, ONLY: OUTPUT_UNIT
  ! Module usage
  USE Type_Kinds,            ONLY: Long, Single, Double, fp
  USE Message_Handler,       ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE Sort_Utility,          ONLY: InsertionSort
  USE CRTM_Parameters,       ONLY: ODPS_ALGORITHM
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: oODPS_type

  
  ! Public parameters
  ! -----------------
  ! Sensor Id defaults
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  ! Allowable sensor type values and names
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME
  ! The Global unique algorithm ID
  PUBLIC :: ODPS_ALGORITHM


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Literal constants
  REAL(Double), PARAMETER :: DZERO = 0.0_Double
  ! String lengths
  INTEGER, PARAMETER :: SL = 20   ! Sensor Id
  INTEGER, PARAMETER :: ML = 256  ! Messages
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: ODPS_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: ODPS_VERSION = 1  ! This is just the data version.
  ! The optical depth algorithm name
  CHARACTER(*), PARAMETER :: ODPS_ALGORITHM_NAME = 'ODPS'
  ! Invalid sensor ids
  INTEGER, PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER, PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER, DIMENSION( 0:N_SENSOR_TYPES ) :: &
    SENSOR_TYPE_NAME = (/ 'Invalid    ', &
                          'Microwave  ', &
                          'Infrared   ', &
                          'Visible    ', &
                          'Ultraviolet' /)
  ! number of predictors used to compute optran absorption coefficients
  INTEGER,         PARAMETER :: N_PREDICTOR_USED_OPTRAN = 6
  INTEGER, PUBLIC, PARAMETER :: SIGNIFICANCE_OPTRAN = 1


  ! -----------------------
  ! oODPS object definition
  ! -----------------------
  TYPE :: oODPS_type
    LOGICAL :: usable = .FALSE.

    ! Release and version information
    INTEGER(Long) :: Release = ODPS_RELEASE
    INTEGER(Long) :: Version = ODPS_VERSION

    ! Algorithm identifer
    INTEGER(Long) :: Algorithm = ODPS_ALGORITHM

    ! Dimensions
    INTEGER(Long) :: n_Layers     = 0    ! Iorder
    INTEGER(Long) :: n_Components = 0    ! J  - Tau component dimension
    INTEGER(Long) :: n_Absorbers  = 0    ! Jm - (Molecular) absorber dimension
    INTEGER(Long) :: n_Channels   = 0    ! L
    INTEGER(Long) :: n_Coeffs     = 0    ! Iuse
    ! ...Dimensions for OPTRAN component
    INTEGER(Long) :: n_OPIndex  = N_PREDICTOR_USED_OPTRAN    ! OI, should not be changed
    INTEGER(Long) :: n_OCoeffs  = 0    ! OC

    ! Sensor/Satellite IDs and type
    CHARACTER(SL) :: Sensor_Id        = ''
    INTEGER(Long) :: WMO_Satellite_ID = INVALID_WMO_SATELLITE_ID
    INTEGER(Long) :: WMO_Sensor_ID    = INVALID_WMO_SENSOR_ID
    INTEGER(Long) :: Sensor_Type      = INVALID_SENSOR

    ! Group ID. TauCoeffs in the same group have the same dimensions: 
    ! n_Components and n_Absorbers.
    INTEGER(Long) :: Group_Index = 0

    ! Reference pressures at the layer boundaries
    REAL(Double), ALLOCATABLE :: Ref_Level_Pressure(:)    ! 0:K
    ! Reference layer (mean) pressure and temperature
    REAL(Double), ALLOCATABLE :: Ref_Pressure(:)          ! K
    REAL(Double), ALLOCATABLE :: Ref_Temperature(:)       ! K

    ! Reference molecular content profile. The sequence of the molecules in the Jm dimension
    ! must be consistent with that of the Absorber_ID array
    REAL(Double), ALLOCATABLE :: Ref_Absorber(:,:)        ! K x Jm
    ! Training set molecular content ranges
    REAL(Double), ALLOCATABLE :: Min_Absorber(:,:)        ! K x Jm
    REAL(Double), ALLOCATABLE :: Max_Absorber(:,:)        ! K x Jm

    ! The actual sensor channel numbers
    INTEGER(Long), ALLOCATABLE :: Sensor_Channel(:)       ! L 
    ! The Tau component ID 
    INTEGER(Long), ALLOCATABLE :: Component_ID(:)         ! J 
    ! Molecular IDs (variable absorbers):
    INTEGER(Long), ALLOCATABLE :: Absorber_ID(:)          ! Jm


    !---------------------------------------------------------------------------
    !  The array C contains the Tau coefficient. It is structured
    !  with Pos_Index and n_Predictors, as the following,
    !    For channel l and component j,
    !      Pos_Index(j, l) is the starting position in array C for that 
    !            channel and component, and
    !      n_Predictors(j, l) is the number of predictors for that channel
    !            and component.
    !  The size of the coefficient data at j and l is given by
    !     Pos_Index(j+1, l) - Pos_Index(j, l)
    !  and the sub-structure of the data at j and l depends on the algorithm. The
    !  following is an example of the sub-structure:
    !    As the number layers is fixed and known for all channels and components,
    !    the positions of the coeffs for a particular layer are known. Let i be the 
    !    index to the array C for channel l, component j, layer k and coefficient m, 
    !    then
    !       i = Pos_Index(j, l) + (m-1)*n_Predictors(j, l) + k
    !    Thus, accessing C(i) is equivalent to that given by C(m, k, j, l) if C is 
    !    a 4-D array.
    !
    !    Notice: the value of n_Predictors(j, l) can be zero, which means the 
    !            coeff data for j, l does not exist. Thus, this value should
    !            be checked before accessing C.
    !---------------------------------------------------------------------------           
    INTEGER(Long), ALLOCATABLE :: n_Predictors(:,:)  ! J x L
    INTEGER(Long), ALLOCATABLE :: Pos_Index(:,:)     ! J x L
    REAL(Single),  ALLOCATABLE :: C(:)               ! Iuse

    !----------------------------------------------------------------
    ! Compact OPTRAN water vapor line
    ! OSignificance - an integer number indicating if for this channel
    !                 OPTRAN should be applied.
    ! Order    - order of the polynomial
    ! OP_Index - Predictor indexes (OP_Index(0) is the number of predictors)
    ! OPos_Index - starting position for the coefficient data in the OC
    !              array
    ! OC - Coefficients
    !----------------------------------------------------------------
    INTEGER(Long), ALLOCATABLE :: OSignificance(:)   ! L
    INTEGER(Long), ALLOCATABLE :: Order(:)           ! L
    INTEGER(Long), ALLOCATABLE :: OP_Index(:,:)      ! 0:OI x L
    INTEGER(Long), ALLOCATABLE :: OPos_Index(:)      ! J x L
    REAL(Double),  ALLOCATABLE :: OC(:)              ! OC
    REAL(Double)               :: Alpha    = DZERO
    REAL(Double)               :: Alpha_C1 = DZERO
    REAL(Double)               :: Alpha_C2 = DZERO
    INTEGER(Long)              :: OComponent_Index = -1

  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS(this) :: Is_Usable
    PROCEDURE, PUBLIC, PASS(this) :: Destroy
    PROCEDURE, PUBLIC, PASS(this) :: Create
    PROCEDURE, PUBLIC, PASS(this) :: Valid_Release
    PROCEDURE, PUBLIC, PASS(this) :: Valid_Algorithm
    PROCEDURE, PUBLIC, PASS(this) :: Info
    PROCEDURE :: Concatenate_by_Channel
    PROCEDURE :: Concatenate_by_Absorber
    PROCEDURE :: Equal
    PROCEDURE :: Not_Equal
    GENERIC, PUBLIC :: OPERATOR(.ConcatChannel.)  => Concatenate_by_Channel
    GENERIC, PUBLIC :: OPERATOR(.ConcatAbsorber.) => Concatenate_by_Absorber
    GENERIC, PUBLIC :: OPERATOR(==) => Equal
    GENERIC, PUBLIC :: OPERATOR(/=) => Not_Equal
  END TYPE oODPS_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Is_Usable
!
! PURPOSE:
!   Elemental function method to test the status of oODPS objects to
!   determine if they are usable.
!
! CALLING SEQUENCE:
!   status = o_obj%Is_Usable()
!
! OBJECTS:
!   o_obj:   Object which is to have its usability tested.
!            UNITS:      N/A
!            CLASS:      oODPS_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   status:  The return value is a logical value indicating the
!            usable status of the object.
!              .TRUE.  - if the object is usable.
!              .FALSE. - if the object is NOT usable.
!            UNITS:      N/A
!            TYPE:       LOGICAL
!            DIMENSION:  Same as object
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Usable( this )
    CLASS(oODPS_type), INTENT(IN) :: this
    LOGICAL :: Is_Usable
    Is_Usable = this%usable
  END FUNCTION Is_Usable


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Destroy
!
! PURPOSE:
!   Elemental subroutine method to re-initialize oODPS objects.
!
! CALLING SEQUENCE:
!   CALL o_obj%Destroy()
!
! OBJECTS:
!   o_obj:   Re-initialized oODPS object(s).
!            UNITS:      N/A
!            CLASS:      oODPS_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Destroy( this )
    CLASS(oODPS_type), INTENT(OUT) :: this
    this%usable = .FALSE.
  END SUBROUTINE Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Create
!
! PURPOSE:
!   Private elemental subroutine method to create instances of oODPS objects.
!
! CALLING SEQUENCE:
!   CALL o_obj%Create( n_layers         , &
!                      n_components     , &
!                      n_absorbers      , &
!                      n_channels       , &
!                      n_coefficients   , &
!                      n_co_coefficients, &
!                      error_message = error_message )
!
! OBJECTS:
!   o_obj:              oODPS object
!                       UNITS:      N/A
!                       CLASS:      oODPS_type
!                       DIMENSION:  Scalar or any rank
!                       ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   n_layers:           The number of profile layers
!                       Must be > 0.           
!                       UNITS:      N/A                        
!                       TYPE:       INTEGER                    
!                       DIMENSION:  Conformable with object.                     
!                       ATTRIBUTES: INTENT(IN)      
!
!   n_components:       The number of transmittance components
!                       Must be > 0.           
!                       UNITS:      N/A                                          
!                       TYPE:       INTEGER                                      
!                       DIMENSION:  Conformable with object.                                     
!                       ATTRIBUTES: INTENT(IN)                            
!
!   n_absorbers:        The number of absorbers dimension
!                       Must be > 0.           
!                       UNITS:      N/A                                        
!                       TYPE:       INTEGER                                    
!                       DIMENSION:  Conformable with object.                                  
!                       ATTRIBUTES: INTENT(IN)                      
!
!   n_channels:         Number of channels dimension.
!                       Must be > 0.
!                       UNITS:      N/A
!                       TYPE:       INTEGER
!                       DIMENSION:  Conformable with object.
!                       ATTRIBUTES: INTENT(IN)
!
!   n_coefficients:     The total number of tau coefficients.                      
!                       Must be >= 0.           
!                       UNITS:      N/A                                          
!                       TYPE:       INTEGER                                      
!                       DIMENSION:  Conformable with object.                                      
!                       ATTRIBUTES: INTENT(IN)
!
!   n_co_coefficients:  The total number of compact OPTRAN tau coefficients.                         
!                       Must be > 0.           
!                       UNITS:      N/A                                            
!                       TYPE:       INTEGER                                        
!                       DIMENSION:  Conformable with object.                                         
!                       ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!   error_message:      If an error occurred creating the object, this
!                       argument will contain error information.
!                       UNITS:      N/A
!                       TYPE:       CHARACTER(*)
!                       DIMENSION:  Conformable with object.
!                       ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Create( &
    this             , &
    n_layers         , &
    n_components     , &
    n_absorbers      , &
    n_channels       , &
    n_coefficients   , &
    n_co_coefficients, &
    error_message      )
    ! Arguments
    CLASS(oODPS_type)     , INTENT(OUT) :: this
    INTEGER               , INTENT(IN)  :: n_layers
    INTEGER               , INTENT(IN)  :: n_components
    INTEGER               , INTENT(IN)  :: n_absorbers
    INTEGER               , INTENT(IN)  :: n_channels
    INTEGER               , INTENT(IN)  :: n_coefficients
    INTEGER               , INTENT(IN)  :: n_co_coefficients
    CHARACTER(*), OPTIONAL, INTENT(OUT) :: Error_Message
    ! Local variables
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_layers          < 1 .OR. &
         n_components      < 1 .OR. &
         n_absorbers       < 1 .OR. &
         n_channels        < 1 .OR. &
         n_coefficients    < 0 .OR. &
         n_co_coefficients < 1      ) THEN
      IF ( PRESENT(error_message) ) error_message = 'Invalid dimension inputs'
      RETURN
    END IF


  ! Maybe the number of ODAS coefficients needs to be tested separately?
  ! That is, if it is = 0, then ODAS allocation is NOT performed
  !          if it is > 0, then ODAS allocation is performed
  !
  ! Or, maybe change n_co_coefficients test to same as n_coefficients so
  ! that even zero coefficients still causes allocation?
  
  
    ! Perform the allocations
    ALLOCATE( this%Sensor_Channel( n_channels )                     , &
              this%Component_ID( n_components )                     , &
              this%Absorber_ID( n_absorbers )                       , &
              this%Ref_Level_Pressure( 0:n_layers )                 , &
              this%Ref_Pressure( n_layers )                         , &
              this%Ref_Temperature( n_layers )                      , &
              this%Ref_Absorber( n_layers, n_absorbers )            , &
              this%Min_Absorber( n_layers, n_absorbers )            , &
              this%Max_Absorber( n_layers, n_absorbers )            , &
              this%n_Predictors( n_components, n_channels )         , &
              this%Pos_Index( n_components, n_channels )            , &
              this%C( n_coefficients )                              , &
              ! Compact OPTRAN data
              this%OSignificance( n_channels )                      , &
              this%Order( n_channels )                              , &
              this%OP_Index( 0:N_PREDICTOR_USED_OPTRAN, n_channels ), &
              this%OPos_Index( n_channels)                          , &
              this%OC( n_co_coefficients )                          , &
              STAT = alloc_stat, ERRMSG = alloc_msg )
    IF ( alloc_stat /= 0 ) THEN
      IF ( PRESENT(error_message) ) error_message = 'Error allocating oODPS object - '//TRIM(alloc_msg)
      RETURN
    END IF

    ! Initialise
    this%n_Layers     = n_layers
    this%n_Components = n_components
    this%n_Absorbers  = n_absorbers
    this%n_Channels   = n_channels
    this%n_Coeffs     = n_coefficients
    this%n_OCoeffs    = n_co_coefficients

    this%Sensor_Channel = 0
    this%Component_ID   = -1
    this%n_Predictors   = 0
    this%Pos_Index      = 0


    ! Set usability
    this%usable = .TRUE.

  END SUBROUTINE Create


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Valid_Release
!
! PURPOSE:
!   Function method to check the oODPS object release.
!
!   The release value is incremented whenever the object components and datafile
!   format changes.
!
! CALLING SEQUENCE:
!   is_valid = o_obj%Valid_Release()
!
! OBJECTS:
!   o_obj:     oODPS object
!              UNITS:      N/A
!              CLASS:      oODPS_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! RESULT:
!   is_valid:  Logical result indicating if the release is valid
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Valid_Release( this ) RESULT( is_valid )
    ! Arguments
    CLASS(oODPS_type), INTENT(IN) :: this
    ! Function result
    LOGICAL :: is_valid
    ! Local parameters
    CHARACTER(*), PARAMETER :: METHOD_NAME = 'oODPS_Define::Valid_Release'
    ! Local variables
    CHARACTER(ML) :: err_msg

    ! Simple check
    is_valid = (this%Release == ODPS_RELEASE)

    
    ! Check if it failed, and why...
    IF ( .NOT. is_valid ) THEN
      ! Construct part of error message
      WRITE( err_msg,'("ODPS release is ",i0,". Valid release is ",i0,"." )' ) &
                     this%Release, ODPS_RELEASE
      ! Is release value too old?
      IF ( this%Release < ODPS_RELEASE ) &
        err_msg = 'An ODPS data update is needed. '//TRIM(err_msg)
      ! Is release value too new?
      IF ( this%Release > ODPS_RELEASE ) &
        err_msg = 'An ODPS software update is needed. '//TRIM(err_msg)
      ! Output error
      CALL Display_Message( METHOD_NAME, err_msg, FAILURE )
    END IF

  END FUNCTION Valid_Release


!----------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Valid_Algorithm
!
! PURPOSE:
!   Function method to check the oODPS object algorithm.
!
! CALLING SEQUENCE:
!   is_valid = o_obj%Valid_Algorithm()
!
! OBJECTS:
!   o_obj:     oODPS object
!              UNITS:      N/A
!              CLASS:      oODPS_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! RESULT:
!   is_valid:  Logical result indicating if the algorithm is for the ODPS 
!              transmittance model.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!:sdoc-:
!----------------------------------------------------------------------------------

  FUNCTION Valid_Algorithm( this ) RESULT( is_valid )
    ! Arguments
    CLASS(oODPS_type), INTENT(IN) :: this
    ! Function result
    LOGICAL :: is_valid
    ! Local parameters
    CHARACTER(*), PARAMETER :: METHOD_NAME = 'oODPS_Define::Valid_Algorithm'
    ! Local variables
    CHARACTER(ML) :: err_msg

    ! Simple check
    is_valid = (this%Algorithm == ODPS_ALGORITHM)

    
    ! Check if it failed, and indicate
    IF ( .NOT. is_valid ) THEN
      err_msg = 'ODPS Algorithm ID check failed. The object is not a valid ODPS object'
      CALL Display_Message( METHOD_NAME, err_msg, FAILURE )
    END IF

  END FUNCTION Valid_Algorithm


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Info
!
! PURPOSE:
!   Function method to return a string containing version and dimension
!   information about the oODPS object.
!
! CALLING SEQUENCE:
!   info_string = o_obj%Info()
!
! OBJECTS:
!   o_obj:        oODPS object
!                 UNITS:      N/A
!                 CLASS:      oODPS_type
!                 DIMENSION:  Scalar
!                 ATTRIBUTES: INTENT(IN)
!
! RESULT:
!   info_string:  Character string containing the object information.
!                 UNITS:      N/A
!                 TYPE:       CHARACTER
!                 DIMENSION:  Scalar
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Info( this ) RESULT( info_string )
    ! Arguments
    CLASS(oODPS_type), INTENT(IN) :: this
    ! Function result
    CHARACTER(:), ALLOCATABLE :: info_string
    ! Local parameters
    INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
    INTEGER, PARAMETER :: LINEFEED = 10
    ! Local variables
    CHARACTER(2000) :: long_string

    ! Write the required data to the local string
    WRITE( long_string,'(a,3x,"ODPS RELEASE.VERSION: ",i0,".",i0,2x,&
                      &"N_LAYERS=",i0,2x,&
                      &"N_COMPONENTS=",i0,2x,&
                      &"N_ABSORBERS=",i0,2x,&
                      &"N_CHANNELS=",i0,2x, &
                      &"N_COEFFS=",i0)' ) &
                      ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                      this%Release, this%Version, &
                      this%n_Layers,     &
                      this%n_Components, &
                      this%n_Absorbers,  &
                      this%n_Channels,   &
                      this%n_Coeffs

    ! Assign the result
    info_string = TRIM(long_string)

  END FUNCTION Info



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                              ## OPERATOR METHODS ##                          ##
!##                                                                              ##
!##################################################################################
!##################################################################################


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   .ConcatChannel.
!
! PURPOSE:
!   Operator method to concatenate two valid oODPS objects along
!   the channel dimension.
!
! CALLING SEQUENCE:
!   c_obj = x .ConcatChannel. y
!
! OBJECTS:
!   x, y:   The oODPS objects to concatenate.
!           UNITS:      N/A
!           CLASS:      oODPS_type
!           DIMENSION:  Scalar or any rank
!           ATTRIBUTES: INTENT(IN)
!
! RESULT:
!   c_obj:  The concatenated oODPS object
!           UNITS:      N/A
!           TYPE:       oODPS_type
!           DIMENSION:  Conformable with input objects
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Concatenate_by_Channel( o1, o2 ) RESULT( ocat )
    ! Arguments
    CLASS(oODPS_type), INTENT(IN) :: o1, o2
    ! Function result
    TYPE(oODPS_type) :: ocat
    ! Local variables
    INTEGER :: n_channels       
    INTEGER :: n_coefficients   
    INTEGER :: n_co_coefficients
    INTEGER :: l1, l2


    ! Check input
    ! ...If input objects are not used, do nothing
    IF ( .NOT. o1%Is_Usable() .OR. &
         .NOT. o2%Is_Usable()      ) RETURN
    ! ...If input objects have incompatibilities, do nothing
    IF ( o1%Release           /= o2%Release          .OR. &
         o1%n_Layers          /= o2%n_Layers         .OR. &
         o1%n_Components      /= o2%n_Components     .OR. &      
         o1%n_Absorbers       /= o2%n_Absorbers      .OR. &
         o1%Group_Index       /= o2%Group_Index      .OR. &
         o1%Sensor_ID         /= o2%Sensor_ID        .OR. &
         o1%WMO_Satellite_ID  /= o2%WMO_Satellite_ID .OR. &
         o1%WMO_Sensor_ID     /= o2%WMO_Sensor_ID    .OR. &
         ANY(o1%Component_ID  /= o2%Component_ID)    .OR. &
         ANY(o1%Absorber_ID   /= o2%Absorber_ID)          ) RETURN


    ! Determine new channel dimension
    n_channels        = o1%n_Channels + o2%n_Channels
    n_coefficients    = o1%n_Coeffs   + o2%n_Coeffs
    n_co_coefficients = o1%n_OCoeffs  + o2%n_OCoeffs
    

    ! Create result object
    CALL ocat%Create( o1%n_Layers      , &
                      o1%n_Components  , &
                      o1%n_Absorbers   , &
                      n_channels       , &
                      n_coefficients   , &
                      n_co_coefficients  )
    IF ( .NOT. ocat%Is_Usable() ) RETURN


    ! Assign the non-channel data
    ! ...ODPS data
    ocat%Version            = MAX(o1%Version, o2%Version)
    ocat%Group_Index        = o1%Group_Index
    ocat%Sensor_ID          = o1%Sensor_ID
    ocat%Sensor_type        = o1%Sensor_type
    ocat%WMO_Satellite_ID   = o1%WMO_Satellite_ID
    ocat%WMO_Sensor_ID      = o1%WMO_Sensor_ID
    ocat%Component_ID       = o1%Component_ID
    ocat%Absorber_ID        = o1%Absorber_ID
    ocat%Ref_Level_Pressure = o1%Ref_Level_Pressure
    ocat%Ref_Pressure       = o1%Ref_Pressure
    ocat%Ref_Temperature    = o1%Ref_Temperature
    ocat%Ref_Absorber       = o1%Ref_Absorber
    ocat%Min_Absorber       = o1%Min_Absorber
    ocat%Max_Absorber       = o1%Max_Absorber
    ! ...Compact-OPTRAN data
    ocat%OComponent_Index   = o1%OComponent_Index
    ocat%Alpha              = o1%Alpha
    ocat%Alpha_C1           = o1%Alpha_C1
    ocat%Alpha_C2           = o1%Alpha_C2

     
    ! Concatenate the channel array data
    ! ...The first object
    l1 = 1
    l2 = o1%n_Channels
    ocat%Sensor_Channel(l1:l2) = o1%Sensor_Channel
    ocat%n_Predictors(:,l1:l2) = o1%n_Predictors
    ocat%Pos_Index(:,l1:l2)    = o1%Pos_Index
    IF ( o1%n_Coeffs > 0 ) ocat%C(l1:o1%n_Coeffs) = o1%C
    IF ( o1%n_OCoeffs > 0 ) THEN  ! The Compact-OPTRAN part
      ocat%OC(l1:o1%n_OCoeffs)  = o1%OC
      ocat%OSignificance(l1:l2) = o1%OSignificance
      ocat%Order(l1:l2)         = o1%Order
      ocat%OP_Index(:,l1:l2)    = o1%OP_Index      
      ocat%OPos_Index(l1:l2)    = o1%OPos_Index
    END IF
    ! ...and the second object
    l1 = l2 + 1
    l2 = n_channels
    ocat%Sensor_Channel(l1:l2) = o2%Sensor_Channel
    ocat%n_Predictors(:,l1:l2) = o2%n_Predictors
    ocat%Pos_Index(:,l1:l2)    = o2%Pos_Index + o1%n_Coeffs
    IF ( o2%n_Coeffs > 0 ) ocat%C(o1%n_Coeffs+1:n_coefficients) = o2%C
    IF ( o2%n_OCoeffs > 0 ) THEN  ! The Compact-OPTRAN part 
      ocat%OC(o1%n_OCoeffs+1:n_co_coefficients) = o2%OC
      ocat%OSignificance(l1:l2)                 = o2%OSignificance
      ocat%Order(l1:l2)                         = o2%Order
      ocat%OP_Index(:,l1:l2)                    = o2%OP_Index    
      ocat%OPos_Index(l1:l2)                    = o2%OPos_Index
    END IF

  END FUNCTION Concatenate_by_Channel




!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   .ConcatAbsorber.
!
! PURPOSE:
!   Operator method to concatenate two valid oODPS objects along
!   the absorber dimension.
!
! CALLING SEQUENCE:
!   c_obj = x .ConcatAbsorber. y
!
! OBJECTS:
!   x, y:   The oODPS objects to concatenate.
!           UNITS:      N/A
!           CLASS:      oODPS_type
!           DIMENSION:  Scalar or any rank
!           ATTRIBUTES: INTENT(IN)
!
! RESULT:
!   c_obj:  The concatenated oODPS object
!           UNITS:      N/A
!           TYPE:       oODPS_type
!           DIMENSION:  Conformable with input objects
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Concatenate_by_Absorber( o1, o2 ) RESULT( ocat )
    ! Arguments
    CLASS(oODPS_type), INTENT(IN) :: o1, o2
    ! Function result
    TYPE(oODPS_type) :: ocat
    ! Local variables
    INTEGER :: n_components       
    INTEGER :: n_layers
    INTEGER :: n_absorbers
    INTEGER :: n_coefficients   
    INTEGER :: i, j, l, j1, j2, m, n
    INTEGER :: indx(32)


    ! Check input
    ! ...If input objects are not used, do nothing
    IF ( .NOT. o1%Is_Usable() .OR. &
         .NOT. o2%Is_Usable()      ) RETURN
    ! ...If input objects have incompatibilities, do nothing
    IF ( o1%Release           /= o2%Release          .OR. &
         o1%n_Layers          /= o2%n_Layers         .OR. &
         o1%n_Channels        /= o2%n_Channels       .OR. &
         o1%Group_Index       /= o2%Group_Index      .OR. &
         o1%Sensor_ID         /= o2%Sensor_ID        .OR. &
         o1%WMO_Satellite_ID  /= o2%WMO_Satellite_ID .OR. &
         o1%WMO_Sensor_ID     /= o2%WMO_Sensor_ID    .OR. &
         ANY((o1%Sensor_Channel - o2%Sensor_Channel) /= 0) ) RETURN


    ! Get indices for the union of the absorber ID for the reference
    ! absorber profile array
    n_absorbers = o1%n_Absorbers
    n = 0
    DO i = 1, o2%n_Absorbers
      DO j = 1, o1%n_Absorbers
        IF ( o2%Absorber_ID(i) == o1%Absorber_ID(j) ) EXIT
      END DO
      ! An absorber ID in o2 was not found in o1, so, add the ID in the union 
      IF ( j > o1%n_Absorbers ) THEN  
        n = n + 1
        indx(n) = i
      END IF
    END DO
    
    
    ! Determine new absorber dimensions
    n_absorbers    = n_absorbers     + n
    n_components   = o1%n_Components + o2%n_Components
    n_coefficients = o1%n_Coeffs     + o2%n_Coeffs


    ! Create result object
    CALL ocat%Create( o1%n_Layers   , &
                      n_components  , &
                      n_absorbers   , &
                      o1%n_channels , &
                      n_coefficients, &
                      0 ) !n_co_coefficients  )
    IF ( .NOT. ocat%Is_Usable() ) RETURN
    

    ! Assign the non-absorber data
    ! ...ODPS data
    ocat%Version            = MAX(o1%Version, o2%Version)
    ocat%Group_Index        = o1%Group_Index
    ocat%Sensor_ID          = o1%Sensor_ID
    ocat%Sensor_type        = o1%Sensor_type
    ocat%WMO_Satellite_ID   = o1%WMO_Satellite_ID
    ocat%WMO_Sensor_ID      = o1%WMO_Sensor_ID
    ocat%Sensor_Channel     = o1%Sensor_Channel
    ocat%Ref_Level_Pressure = o1%Ref_Level_Pressure
    ocat%Ref_Pressure       = o1%Ref_Pressure
    ocat%Ref_Temperature    = o1%Ref_Temperature
    ! ...Compact-OPTRAN data
    ocat%OComponent_Index   = o1%OComponent_Index
    ocat%Alpha              = o1%Alpha
    ocat%Alpha_C1           = o1%Alpha_C1
    ocat%Alpha_C2           = o1%Alpha_C2


    ! Concatenate the absorber profile data
    ocat%Ref_Absorber(:, 1:o1%n_Absorbers) = o1%Ref_Absorber
    ocat%Min_Absorber(:, 1:o1%n_Absorbers) = o1%Min_Absorber
    ocat%Max_Absorber(:, 1:o1%n_Absorbers) = o1%Max_Absorber
    ocat%Absorber_ID(    1:o1%n_Absorbers) = o1%Absorber_ID
    DO j = 1, n
      ocat%Ref_Absorber(:, o1%n_Absorbers + j) = o2%Ref_Absorber(:, indx(j))           
      ocat%Min_Absorber(:, o1%n_Absorbers + j) = o2%Min_Absorber(:, indx(j))           
      ocat%Max_Absorber(:, o1%n_Absorbers + j) = o2%Max_Absorber(:, indx(j))           
      ocat%Absorber_ID(    o1%n_Absorbers + j) = o2%Absorber_ID(    indx(j))
    END DO

    
    ! Concatenate the absorber component id and predictor data
    ! ...The first object
    j1 = 1                                                         
    j2 = o1%n_Components                                      
    ocat%Component_ID(j1:j2)   = o1%Component_ID         
    ocat%n_Predictors(j1:j2,:) = o1%n_Predictors
    ! ...The second object                              
    j1 = o1%n_Components + 1                          
    j2 = n_Components                                       
    ocat%Component_ID(j1:j2)   = o2%Component_ID        
    ocat%n_Predictors(j1:j2,:) = o2%n_Predictors  


    ! Concatenate the coefficient and position index arrays
    m = 1
    n_layers = o1%n_Layers
    DO l = 1, ocat%n_Channels
      ! ...The first object
      DO j = 1, o1%n_Components
        n = n_layers * o1%n_Predictors(j,l)
        IF ( n > 0 ) THEN 
          j1 = o1%Pos_Index(j,l)
          j2 = j1 + n - 1
          ocat%Pos_Index(j,l) = m
          ocat%C(m:m+n-1)     = o1%C(j1:j2)
          m = m + n
        END IF         
      END DO
      ! ...The second object
      DO j = 1, o2%n_Components
        n = n_layers * o2%n_Predictors(j,l) 
        IF( n > 0 )THEN 
          j1 = o2%Pos_Index(j,l)
          j2 = j1 + n - 1
          ocat%Pos_Index(o1%n_Components+j,l) = m
          ocat%C(m:m+n-1)                     = o2%C(j1:j2)
          m = m + n
        END IF
      END DO
    END DO

  END FUNCTION Concatenate_by_Absorber


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   ==
!
! PURPOSE:
!   Operator method to test the equality of two oODPS objects.
!
! CALLING SEQUENCE:
!   IF ( x == y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:  Two oODPS objects to be compared.
!          UNITS:      N/A
!          TYPE:       oODPS_type
!          DIMENSION:  Scalar or any rank
!          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Equal( x, y ) RESULT( is_equal )
    CLASS(oODPS_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) RETURN

    ! Check release and version
    IF ( x%Release /= y%Release .OR. &
         x%Version /= y%Version ) RETURN

    ! Check dimensions
    IF ( x%n_Layers     /= y%n_Layers     .OR. &
         x%n_Components /= y%n_Components .OR. &
         x%n_Absorbers  /= y%n_Absorbers  .OR. &
         x%n_Channels   /= y%n_Channels   .OR. &
         x%n_Coeffs     /= y%n_Coeffs     .OR. &
         x%n_OCoeffs    /= y%n_OCoeffs         ) RETURN

    ! Check scalars
    ! ...Generic
    IF ( x%Sensor_Id        /= y%Sensor_Id         .OR. &
         x%Sensor_Type      /= y%Sensor_Type       .OR. &
         x%WMO_Satellite_ID /= y%WMO_Satellite_ID  .OR. &
         x%WMO_Sensor_ID    /= y%WMO_Sensor_ID          ) RETURN
    ! ...ODPS
    IF ( x%Group_Index /= y%Group_Index ) RETURN
    ! ...Compact-OPTRAN
    IF ( x%OComponent_Index /= y%OComponent_Index  .OR. &
         ( .NOT. (x%Alpha    .EqualTo. y%Alpha   ) ) .OR. &
         ( .NOT. (x%Alpha_C1 .EqualTo. y%Alpha_C1) ) .OR. &
         ( .NOT. (x%Alpha_C2 .EqualTo. y%Alpha_C2) ) ) RETURN

    ! Check arrays
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      ! ...Generic
      IF ( .NOT. (ALL(x%Sensor_Channel == y%Sensor_Channel)) ) RETURN
      ! ...ODPS
      IF ( .NOT. (ALL(x%Component_Id    ==     y%Component_Id) .AND. &
                  ALL(x%n_Predictors    ==     y%n_Predictors) .AND. &
                  ALL(x%Pos_Index       ==     y%Pos_Index   ) .AND. &
                  ALL(x%C            .EqualTo. y%C           )) ) RETURN
      ! ...Compact-OPTRAN
      IF ( .NOT. (ALL(x%OSignificance    ==     y%OSignificance) .AND. &
                  ALL(x%Order            ==     y%Order        ) .AND. &
                  ALL(x%OP_Index         ==     y%OP_Index     ) .AND. &
                  ALL(x%OPos_Index       ==     y%OPos_Index   ) .AND. &
                  ALL(x%OC            .EqualTo. y%OC           )) ) RETURN
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION Equal


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   /=
!
! PURPOSE:
!   Operator method to test the inequality of two oODPS objects.
!
! CALLING SEQUENCE:
!   IF ( x /= y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:  Two oODPS objects to be compared.
!          UNITS:      N/A
!          TYPE:       oODPS_type
!          DIMENSION:  Scalar or any rank
!          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Not_Equal( x, y ) RESULT( notequal )
    CLASS(oODPS_type), INTENT(IN) :: x, y
    LOGICAL :: notequal
    notequal = .NOT. (x == y)
  END FUNCTION Not_Equal

END MODULE oODPS_Define
