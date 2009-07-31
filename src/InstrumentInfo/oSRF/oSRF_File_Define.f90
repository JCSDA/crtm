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
  USE Type_Kinds,        ONLY: fp
  USE Message_Handler,   ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                               Display_Message
  USE String_Utility,    ONLY: StrClean
  USE oSRF_Define,       ONLY: OSRF_RELEASE, &
                               OSRF_VERSION, &
                               INVALID_WMO_SATELLITE_ID, &
                               INVALID_WMO_SENSOR_ID   , &
                               N_SENSOR_TYPES    , &
                               INVALID_SENSOR    , &    
                               MICROWAVE_SENSOR  , &  
                               INFRARED_SENSOR   , &   
                               VISIBLE_SENSOR    , &   
                               ULTRAVIOLET_SENSOR, & 
                               SENSOR_TYPE_NAME  , &
                               oSRF_type     , &
                               Allocated_oSRF, &
                               Destroy_oSRF  , &
                               Create_oSRF   , &
                               Assign_oSRF   , &
                               Inspect_oSRF  , &
                               Info_oSRF
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
  PUBLIC :: Allocated_oSRF_File
  PUBLIC :: Destroy_oSRF_File
  PUBLIC :: Create_oSRF_File
  PUBLIC :: Set_Property_oSRF_File
  PUBLIC :: Get_Property_oSRF_File
  PUBLIC :: Inspect_oSRF_File
  PUBLIC :: AddTo_oSRF_File
  PUBLIC :: GetFrom_oSRF_File
  PUBLIC :: Read_oSRF_File
!  PUBLIC :: Inquire_oSRF
!  PUBLIC :: Write_oSRF
!  PUBLIC :: Read_oSRF


  ! -----------------
  ! Module parameters
  ! -----------------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
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


  ELEMENTAL FUNCTION Allocated_oSRF_File(self) RESULT(alloc_status)
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: alloc_status
    ! Set up
    alloc_status = .FALSE.
    ! Test the members
    IF ( ALLOCATED( self%oSRF ) ) THEN
      alloc_status = .TRUE.
    ELSE
      alloc_status = .FALSE.
    END IF
  END FUNCTION Allocated_oSRF_File


  FUNCTION Destroy_oSRF_File(self) RESULT(err_status)
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Reinitialise
    self%n_Channels = 0
  END FUNCTION Destroy_oSRF_File


  FUNCTION Create_oSRF_File(self, n) RESULT(err_status)
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: self
    INTEGER,              INTENT(IN)  :: n
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME='Create_oSRF_File'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status
    ! Set up
    err_status = SUCCESS
    ! Allocate
    ALLOCATE( self%oSRF(n), STAT=alloc_status )
    IF ( alloc_status /= 0 ) THEN
      err_status = FAILURE
      WRITE( msg,'("Error allocating. STAT = ",i0)' ) alloc_status
      CALL Display_Message(ROUTINE_NAME, TRIM(msg), err_status)
      RETURN
    END IF
    ! Initialise
    self%n_Channels = n
  END FUNCTION Create_oSRF_File
  

  FUNCTION Set_Property_oSRF_File( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Set_Property_oSRF_File'
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
  END FUNCTION Set_Property_oSRF_File


  FUNCTION Get_Property_oSRF_File( &
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
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_Property_oSRF_File'
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
  END FUNCTION Get_Property_oSRF_File


  SUBROUTINE Inspect_oSRF_File( self, verbose )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN) :: self
    LOGICAL,    OPTIONAL, INTENT(IN) :: verbose
    ! Local arguments
    INTEGER :: n
    ! Output the oSRF components     
    WRITE( *,'(/2x,"oSRF_File INSPECT")' )
    WRITE( *,'( 2x,"=================")' )
    WRITE( *,'(2x,"Filename         : ",  a)' ) TRIM(self%Filename)
    WRITE( *,'(2x,"Release          : ", i0)' ) self%Release
    WRITE( *,'(2x,"Version          : ", i0)' ) self%Version  
    WRITE( *,'(2x,"Sensor_Id        : ",  a)' ) TRIM(self%Sensor_Id)       
    WRITE( *,'(2x,"WMO_Satellite_Id : ", i0)' ) self%WMO_Satellite_Id
    WRITE( *,'(2x,"WMO_Sensor_Id    : ", i0)' ) self%WMO_Sensor_Id   
    WRITE( *,'(2x,"Sensor_Type      : ",  a)' ) SENSOR_TYPE_NAME(self%Sensor_Type)
    WRITE( *,'(2x,"Title            : ",  a)' ) TRIM(self%Title  )
    WRITE( *,'(2x,"History          : ",  a)' ) TRIM(self%History)
    WRITE( *,'(2x,"Comment          : ",  a)' ) TRIM(self%Comment)
    WRITE( *,'(2x,"n_Channels       : ", i0)' ) self%n_Channels
    IF ( self%n_Channels == 0 ) THEN
      WRITE( *,'(10x,"Press <ENTER> to continue...")', ADVANCE='NO' )
      READ(*,*)
    END IF
    IF ( PRESENT(verbose) ) THEN
      IF ( verbose ) THEN
        DO n = 1, self%n_Channels
          WRITE( *,'(/2x,"OSRF POSITION NUMBER ",i0)' ) n
          CALL Inspect_oSRF( self%oSRF(n) )
        END DO
      END IF
    END IF
  END SUBROUTINE Inspect_oSRF_File
  
  
  FUNCTION AddTo_oSRF_File( &
    self, &
    oSRF, &
    pos ) &
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN OUT) :: self
    TYPE(oSRF_type),      INTENT(IN)     :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)     :: pos
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'AddTo_oSRF_File'
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
      CALL AddTo_CleanUp(); RETURN
    END IF
    ! Copy oSRF to required position
    IF ( Allocated_oSRF_File(self) ) THEN
      err_status = Assign_oSRF( oSRF, self%oSRF(l_pos) )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg,'("Error adding oSRF at position ",i0," in oSRF_File")' ) l_pos
        CALL AddTo_CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE AddTo_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE AddTo_CleanUp
    
  END FUNCTION AddTo_oSRF_File


  FUNCTION GetFrom_oSRF_File( &
    self, &
    oSRF, &
    pos ) &
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(IN)  :: self
    TYPE(oSRF_type),      INTENT(OUT) :: oSRF
    INTEGER,    OPTIONAL, INTENT(IN)  :: pos
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'GetFrom_oSRF_File'
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
      CALL GetFrom_CleanUp(); RETURN
    END IF
    ! Copy oSRF from requested position
    IF ( Allocated_oSRF_File(self) ) THEN
      err_status = Assign_oSRF( self%oSRF(l_pos), oSRF )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg,'("Error getting oSRF from position ",i0," in oSRF_File")' ) l_pos
        CALL GetFrom_CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE GetFrom_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE GetFrom_CleanUp
    
  END FUNCTION GetFrom_oSRF_File







!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Read_oSRF_File
!
! PURPOSE:
!       Function to read oSRF data from file and fill the oSRF_File
!       container with all the oSRF objects in the file.
!
! CALLING SEQUENCE:
!       Error_Status = Read_oSRF_File( &
!         oSRF_File    , &
!         Filename     , &  ! Input  
!         Quiet = Quiet  )  ! Optional input
!
! OBJECT:
!       oSRF_File:    oSRF_File structure to fill with data.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_File_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of the
!                     oSRF_File data file to read.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER(*)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
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

  FUNCTION Read_oSRF_File( &
    oSRF_File, &
    Filename , &
    Quiet    ) &
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_File_type), INTENT(OUT) :: oSRF_File
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
    INTEGER :: n_Channels
!    
!    
!    INTEGER :: i, n
!    INTEGER :: Version         
!    CHARACTER(256) :: Sensor_ID       
!    INTEGER :: WMO_Satellite_ID
!    INTEGER :: WMO_Sensor_ID   
!    INTEGER :: Sensor_Type
!    INTEGER :: n_Channels, n_Points, n_Bands
!    INTEGER :: Channel_Idx(1)
!    INTEGER, ALLOCATABLE :: Sensor_Channel(:)
!    CHARACTER(256) :: Channel_Name     
!    CHARACTER(256) :: Point_DimName    
!    CHARACTER(256) :: Band_DimName     
!    CHARACTER(256) :: Response_VarName 
!    CHARACTER(256) :: f1_Band_VarName  
!    CHARACTER(256) :: f2_Band_VarName  
!    CHARACTER(256) :: npts_Band_VarName
!    INTEGER :: n_Points_DimId
!    INTEGER :: n_Bands_DimId
!    INTEGER :: Reponse_VarId
!    INTEGER :: f1_Band_VarId
!    INTEGER :: f2_Band_VarId
!    INTEGER :: npts_Band_VarId
!    INTEGER :: VarId

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
      CALL Read_Cleanup(); RETURN
    END IF
    
    
    ! Get the number of channels dimension
    err_status = Read_Dim( FileId, CHANNEL_DIMNAME, n_Channels )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF


    ! Allocate the output container
    err_status = Create_oSRF_File( oSRF_File, n_Channels )    
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error allocating oSRF_File output'
      CALL Read_Cleanup(); RETURN
    END IF
    ! ...Set the filename
    
    
    
    ! Read the global attributes
    err_status = Read_GAtts( oSRF_File, FileId )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error reading global attributes from '//TRIM(Filename)
      CALL Read_Cleanup(); RETURN
    END IF




!    ! Check that the SRF channel is valid for the file
!    ! ------------------------------------------------
!    ! Get the channel dimension
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,n_Channels=n_Channels,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(); RETURN
!    END IF
!    ! Allocate a sensor channel array
!    ALLOCATE( Sensor_Channel(n_Channels),STAT=Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      WRITE( msg,'("Error allocating sensor channel array. STAT = ", i5 )' ) Allocate_Status
!      CALL Read_Cleanup(); RETURN
!    END IF
!    ! Read the sensor channel list
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Check if the requested channel is in the list at all, or more than once
!    n = COUNT(Sensor_Channel == Channel)
!    IF ( n < 1 ) THEN
!      WRITE( msg,'("SRF channel ",i0," is not in the sensor channel list for ",a)' ) &
!                 Channel, TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    IF ( n > 1 ) THEN
!      WRITE( msg,'("Check ",a," file! SRF channel ",i0,&
!                  &" occurs multiple times in the sensor channel list")' ) &
!                 SRF%Channel, TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Get the index of the current channel in the sensor channel list
!    Channel_Idx = PACK((/(i,i=1,n_Channels)/),Sensor_Channel == Channel)
!    ! Deallocate the sensor channel list array
!    DEALLOCATE( Sensor_Channel )
!
!
!    ! Read some of the global attributes
!    ! ----------------------------------
!    Error_Status = Inquire_SRF_netCDF( NC_Filename                        , &  ! Input
!                                       Version          = Version         , &  ! Optional output
!                                       Sensor_ID        = Sensor_ID       , &  ! Optional output
!                                       WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
!                                       WMO_Sensor_ID    = WMO_Sensor_ID   , &  ! Optional output
!                                       Sensor_Type      = Sensor_Type     , &  ! Optional output
!                                       Message_Log      = Message_Log       )  ! Error messaging
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error occurred reading global attributes from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(); RETURN
!    END IF
!    
!    
!    ! Create the SRF dimension and variable names for the current channel
!    ! -------------------------------------------------------------------
!    CALL CreateNames( Channel, &
!                      Channel_Name     =Channel_Name     , &
!                      Point_DimName    =Point_DimName    , &
!                      Band_DimName     =Band_DimName     , &
!                      Response_VarName =Response_VarName , &
!                      f1_Band_VarName  =f1_Band_VarName  , &
!                      f2_Band_VarName  =f2_Band_VarName  , &
!                      npts_Band_VarName=npts_Band_VarName  )
!                      
!                      
!    ! Open the file
!    ! -------------
!    nc_status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error opening '//TRIM(NC_Filename)//' for channel '//TRIM(Channel_Name)//&
!            ' read access - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(); RETURN
!    END IF
!
!
!    ! Retrieve channel dimension values
!    ! ---------------------------------                        
!    ! Retrieve the number of points dimension value
!    Error_Status = ReadDim( NC_FileId,Point_DimName,n_Points,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//TRIM(Point_DimName)//&
!            ' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    ! Retrieve the number of bands dimension value
!    Error_Status = ReadDim( NC_FileId,Band_DimName,n_Bands,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//TRIM(Band_DimName)//&
!            ' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Allocate the output SRF structure
!    ! ---------------------------------
!    Error_Status = Allocate_SRF( n_Points,SRF,n_Bands=n_Bands,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error occurred allocating SRF structure.'
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Set the sensor and channel values
!    ! ---------------------------------
!    SRF%Version          = Version
!    SRF%Sensor_ID        = TRIM(Sensor_ID)
!    SRF%WMO_Satellite_ID = WMO_Satellite_ID
!    SRF%WMO_Sensor_ID    = WMO_Sensor_ID   
!    SRF%Sensor_Type      = Sensor_Type
!    SRF%Channel          = Channel
!
!      
!    ! Read the channel dependent data
!    ! -------------------------------
!    ! The integrated SRF value
!    nc_status = NF90_INQ_VARID( NC_FileId,INTEGRATED_SRF_VARNAME,VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INTEGRATED_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Integrated_SRF,START=Channel_Idx )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//INTEGRATED_SRF_VARNAME//&
!            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    
!    ! The summed SRF value
!    nc_status = NF90_INQ_VARID( NC_FileId,SUMMATION_SRF_VARNAME,VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SUMMATION_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Summation_SRF,START=Channel_Idx )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//SUMMATION_SRF_VARNAME//&
!            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    
!    ! Read the band dependent data
!    ! ----------------------------
!    ! The band begin frequencies
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f1_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f1_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%f1_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(f1_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!    ! The band end frequencies
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f2_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f2_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%f2_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(f2_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!    ! The number of band points
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(npts_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(npts_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%npts_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(npts_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!
!    ! Read the SRF response
!    ! ---------------------
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(Response_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(Response_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Response )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(Response_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
! 
    ! Close the file
    nc_status = NF90_CLOSE( FileId )
    IF ( nc_status /= NF90_NOERR ) THEN
      msg = 'Error closing input file - '//TRIM(NF90_STRERROR(nc_status))
      CALL Read_Cleanup(); RETURN
    END IF


!    ! Output an info message
!    IF ( Noisy ) THEN
!      CALL Info_SRF( SRF, msg )
!      CALL Display_Message( ROUTINE_NAME, &
!                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
!                            INFORMATION, &
!                            Message_Log=Message_Log )
!    END IF

  CONTAINS
  
    SUBROUTINE Read_CleanUp( Close_File )
      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
      ! Close file if necessary
      IF ( PRESENT(Close_File) ) THEN
        IF ( Close_File ) THEN
          nc_status = NF90_CLOSE(FileId)
          IF ( nc_status /= NF90_NOERR ) &
            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
                  TRIM(NF90_STRERROR(nc_status))
        END IF
      END IF
      ! Set error status and print error message
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME, TRIM(msg), err_status )
    END SUBROUTINE Read_CleanUp

  END FUNCTION Read_oSRF_File



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
!         oSRF_File, &  ! In/Output
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
! INPUT ARGUMENTS:
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
    err_status = Set_Property_oSRF_File( self, Version=GAttInteger )
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
    err_status = Set_Property_oSRF_File( self, Sensor_Id=GAttString )
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
    err_status = Set_Property_oSRF_File( self, WMO_Satellite_Id=GAttInteger )
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
    err_status = Set_Property_oSRF_File( self, WMO_Sensor_Id=GAttInteger )
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
    err_status = Set_Property_oSRF_File( self, Title=GAttString )
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
    err_status = Set_Property_oSRF_File( self, History=GAttString )
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
    err_status = Set_Property_oSRF_File( self, Comment=GAttString )
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
!       Subroutine to construct dimension and variable names based on oSRF
!       channel number and the number of bands.
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
    IF ( PRESENT(n_Bands_DimName) ) n_Bands_DimName = 'ch'//ch//'_n_Bands'
    IF ( PRESENT(f1_VarName     ) ) f1_VarName      = 'ch'//ch//'_f1'
    IF ( PRESENT(f2_VarName     ) ) f2_VarName      = 'ch'//ch//'_f2'
    ! ...Channel and band
    IF ( PRESENT(n_Points_DimName ) ) n_Points_DimName  = 'ch'//ch//'_b'//b//'_n_Bands'
    IF ( PRESENT(Frequency_VarName) ) Frequency_VarName = 'ch'//ch//'_b'//b//'_f1'
    IF ( PRESENT(Response_VarName ) ) Response_VarName  = 'ch'//ch//'_b'//b//'_f2'

  END SUBROUTINE Create_Names


!--------------------------------------------------------------------------------
!
! NAME:
!       Read_Dim
!
! PURPOSE:
!       Function to retrieve an oSRF_File dimension by name.
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
! OPTIONAL OUTPUT ARGUMENTS:
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




!!------------------------------------------------------------------------------
!!
!! NAME:
!!       WriteGAtts
!!
!! PURPOSE:
!!       Function to write the global attributes to a netCDF SRF data file.
!!
!! CALLING SEQUENCE:
!!       Error_Status = WriteGAtts( NC_Filename                      , &  ! Input
!!                                  NC_FileId                        , &  ! Input
!!                                  Version         =Version         , &  ! Optional input
!!                                  Sensor_Id       =Sensor_Id       , &  ! Optional input
!!                                  WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!!                                  WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!!                                  Title           =Title           , &  ! Optional input
!!                                  History         =History         , &  ! Optional input
!!                                  Comment         =Comment         , &  ! Optional input
!!                                  Message_Log     =Message_Log       )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       NC_Filename:      Character string specifying the name of the
!!                         netCDF SRF format data file to write to.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN)
!!
!!       NC_FileId:        NetCDF file ID number.
!!                         function.
!!                         UNITS:      N/A
!!                         TYPE:       INTEGER
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN)
!!
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Version:          The version number of the netCDF SRF file.
!!                         UNITS:      N/A
!!                         TYPE:       INTEGER
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Sensor_Id:        Character string sensor/platform identifier.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       WMO_Satellite_Id: The WMO code used to identify satellite platforms.
!!                         UNITS:      N/A
!!                         TYPE:       INTEGER
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       WMO_Sensor_Id:    The WMO code used to identify sensors.
!!                         UNITS:      N/A
!!                         TYPE:       INTEGER
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Title:            Character string written into the TITLE global
!!                         attribute field of the netCDF SRF file.
!!                         Should contain a succinct description of what
!!                         is in the netCDF datafile.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       History:          Character string written into the HISTORY global
!!                         attribute field of the netCDF SRF file.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Comment:          Character string written into the COMMENT global
!!                         attribute field of the netCDF SRF file.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:      Character string specifying a filename in which
!!                         any messages will be logged. If not specified,
!!                         or if an error occurs opening the log file, the
!!                         default action is to output messages to standard
!!                         output.
!!                         UNITS:      N/A
!!                         TYPE:       CHARACTER(*)
!!                         DIMENSION:  Scalar
!!                         ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:     The return value is an integer defining the error status.
!!                         The error codes are defined in the Message_Handler module.
!!                         If == SUCCESS the global attribute write was successful
!!                            == FAILURE an error occurred writing the supplied
!!                                       global attributes.
!!                         UNITS:      N/A
!!                         TYPE:       INTEGER
!!                         DIMENSION:  Scalar
!!
!! SIDE EFFECTS:
!!       If a FAILURE error occurs, the netCDF file is closed.
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!!                       paul.vandelst@ssec.wisc.edu
!!
!!------------------------------------------------------------------------------
!
!  FUNCTION WriteGAtts( NC_Filename     , &  ! Input
!                       NC_FileId       , &  ! Input
!                       Version         , &  ! Optional input
!                       Sensor_Id       , &  ! Optional input
!                       WMO_Satellite_Id, &  ! Optional input
!                       WMO_Sensor_Id   , &  ! Optional input
!                       Title           , &  ! Optional input
!                       History         , &  ! Optional input
!                       Comment         , &  ! Optional input
!                       Message_Log     ) &  ! Error messaging
!                     RESULT( Error_Status )
!    ! Arguments
!    CHARACTER(*),           INTENT(IN) :: NC_Filename
!    INTEGER     ,           INTENT(IN) :: NC_FileId
!    INTEGER     , OPTIONAL, INTENT(IN) :: Version         
!    CHARACTER(*), OPTIONAL, INTENT(IN) :: Sensor_Id       
!    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Satellite_Id
!    INTEGER     , OPTIONAL, INTENT(IN) :: WMO_Sensor_Id   
!    CHARACTER(*), OPTIONAL, INTENT(IN) :: Title
!    CHARACTER(*), OPTIONAL, INTENT(IN) :: History
!    CHARACTER(*), OPTIONAL, INTENT(IN) :: Comment
!    CHARACTER(*), OPTIONAL, INTENT(IN) :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'WriteGAtts'
!    CHARACTER(*), PARAMETER :: WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
!    CHARACTER(*), PARAMETER :: CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 
!    ! Local variables
!    CHARACTER(ML) :: msg
!    CHARACTER(ML) :: GAttName
!    CHARACTER(8)  :: cdate
!    CHARACTER(10) :: ctime
!    CHARACTER(5)  :: czone
!    INTEGER :: Ver
!    INTEGER :: nc_status
!    TYPE(SRF_type) :: SRF_Default
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    msg = ' '
!
!
!    ! Mandatory global attributes
!    ! ---------------------------
!    ! Software ID
!    GAttName = WRITE_MODULE_HISTORY_GATTNAME
!    nc_status = NF90_PUT_ATT( NC_FileId, &
!                                NF90_GLOBAL, &
!                                TRIM(GAttName), &
!                                MODULE_RCS_ID )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      CALL WriteGAtts_Cleanup(); RETURN
!    END IF
!    
!    ! Creation date
!    CALL DATE_AND_TIME( cdate, ctime, czone )
!    GAttName = CREATION_DATE_AND_TIME_GATTNAME
!    nc_status = NF90_PUT_ATT( NC_FileId, &
!                                NF90_GLOBAL, &
!                                TRIM(GAttName), &
!                                cdate(1:4)//'/'//cdate(5:6)//'/'//cdate(7:8)//', '// &
!                                ctime(1:2)//':'//ctime(3:4)//':'//ctime(5:6)//' '// &
!                                czone//'UTC' )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      CALL WriteGAtts_Cleanup(); RETURN
!    END IF
!
!    ! The Release
!    GAttName = RELEASE_GATTNAME
!    nc_status = NF90_PUT_ATT( NC_FileId, &
!                                NF90_GLOBAL, &
!                                TRIM(GAttName), &
!                                SRF_Default%Release )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      CALL WriteGAtts_Cleanup(); RETURN
!    END IF
!
!
!    ! Optional global attributes
!    ! --------------------------
!    ! The Version
!    IF ( PRESENT(Version) ) THEN
!      Ver = Version
!    ELSE
!      Ver = SRF_Default%Version
!    END IF
!    GAttName = VERSION_GATTNAME
!    nc_status = NF90_PUT_ATT( NC_FileId, &
!                                NF90_GLOBAL, &
!                                TRIM(GAttName), &
!                                Ver )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      CALL WriteGAtts_Cleanup(); RETURN
!    END IF
!
!    ! The Sensor_Id
!    IF ( PRESENT(Sensor_Id) ) THEN
!      GAttName = SENSOR_ID_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  Sensor_Id )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!    ! The WMO_Satellite_Id
!    IF ( PRESENT(WMO_Satellite_Id) ) THEN
!      GAttName = WMO_SATELLITE_ID_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  WMO_Satellite_Id )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!    ! The WMO_Sensor_Id
!    IF ( PRESENT(WMO_Sensor_Id) ) THEN
!      GAttName = WMO_SENSOR_ID_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  WMO_Sensor_Id )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!    ! The Title
!    IF ( PRESENT(Title) ) THEN
!      GAttName = TITLE_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  Title )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!    ! The History
!    IF ( PRESENT(History) ) THEN
!      GAttName = HISTORY_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  History )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!    ! The Comment
!    IF ( PRESENT(Comment) ) THEN
!      GAttName = COMMENT_GATTNAME
!      nc_status = NF90_PUT_ATT( NC_FileId, &
!                                  NF90_GLOBAL, &
!                                  TRIM(GAttName), &
!                                  Comment )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        CALL WriteGAtts_Cleanup(); RETURN
!      END IF
!    END IF
!
!  CONTAINS
!  
!    SUBROUTINE WriteGAtts_CleanUp()
!      ! Close file
!      nc_status = NF90_CLOSE( NC_FileId )
!      IF ( nc_status /= NF90_NOERR ) &
!        msg = '; Error closing input file during error cleanup - '//&
!                  TRIM(NF90_STRERROR( nc_status ) )
!      ! Set error status and print error message
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error writing '//TRIM(GAttName)//' attribute to '//&
!                            TRIM(NC_Filename)//' - '// &
!                            TRIM(NF90_STRERROR( nc_status ) )//TRIM(msg), &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END SUBROUTINE WriteGAtts_CleanUp
!    
!  END FUNCTION WriteGAtts
!
!
!
!
!!################################################################################
!!################################################################################
!!##                                                                            ##
!!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!!##                                                                            ##
!!################################################################################
!!################################################################################
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Create_SRF_netCDF
!!
!! PURPOSE:
!!       Function to create a netCDF SRF data file for writing.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Create_SRF_netCDF( NC_Filename                      , &  ! Input
!!                                         Sensor_Type                      , &  ! Input
!!                                         Sensor_Channel                   , &  ! Input
!!                                         Version         =Version         , &  ! Optional input
!!                                         Sensor_Id       =Sensor_Id       , &  ! Optional input
!!                                         WMO_Satellite_Id=WMO_Satellite_Id, &  ! Optional input
!!                                         WMO_Sensor_Id   =WMO_Sensor_Id   , &  ! Optional input
!!                                         Title           =Title           , &  ! Optional input
!!                                         History         =History         , &  ! Optional input
!!                                         Comment         =Comment         , &  ! Optional input
!!                                         RCS_Id          =RCS_Id,           &  ! Revision control
!!                                         Message_Log     =Message_Log       )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       NC_Filename:        Character string specifying the name of the
!!                           netCDF SRF format data file to create.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!!       Sensor_Channel:     The list of channel numbers to be written
!!                           to the SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Rank-1
!!                           ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Version:            The version number of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Sensor_Id:          Character string sensor/platform identifier.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       WMO_Satellite_Id:   The WMO code used to identify satellite platforms.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       WMO_Sensor_Id:      The WMO code used to identify sensors.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Title:              Character string written into the TITLE global
!!                           attribute field of the netCDF SRF file.
!!                           Should contain a succinct description of what
!!                           is in the netCDF datafile.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       History:            Character string written into the HISTORY global
!!                           attribute field of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Comment:            Character string written into the COMMENT global
!!                           attribute field of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:        Character string specifying a filename in which
!!                           any messages will be logged. If not specified,
!!                           or if an error occurs opening the log file, the
!!                           default action is to output messages to standard
!!                           output.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:             Character string containing the Revision Control
!!                           System Id field for the module.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:       The return value is an integer defining the error status.
!!                           The error codes are defined in the Message_Handler module.
!!                           If == SUCCESS the SRF netCDF file creation was successful
!!                              == FAILURE an unrecoverable error occurred 
!!                              == WARNING - an error occurred writing any of the
!!                                           supplied global attributes.
!!                                         - an error occurred closing the netCDF file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!
!! CREATION HISTORY:
!!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!!                       paul.vandelst@ssec.wisc.edu
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Create_SRF_netCDF( NC_Filename     , &  ! Input
!                              Sensor_Type     , &  ! Input
!                              Sensor_Channel  , &  ! Input
!                              Version         , &  ! Optional input
!                              Sensor_Id       , &  ! Optional input
!                              WMO_Satellite_Id, &  ! Optional input
!                              WMO_Sensor_Id   , &  ! Optional input
!                              Title           , &  ! Optional input
!                              History         , &  ! Optional input
!                              Comment         , &  ! Optional input
!                              RCS_Id          , &  ! Revision control
!                              Message_Log     ) &  ! Error messaging
!                            RESULT( Error_Status )
!
!    ! Arguments
!    CHARACTER(*),           INTENT(IN)  :: NC_Filename
!    INTEGER     ,           INTENT(IN)  :: Sensor_Type
!    INTEGER     ,           INTENT(IN)  :: Sensor_Channel(:)
!    INTEGER     , OPTIONAL, INTENT(IN)  :: Version         
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Sensor_ID
!    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Satellite_ID
!    INTEGER     , OPTIONAL, INTENT(IN)  :: WMO_Sensor_ID
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Title
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: History
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Comment
!    CHARACTER(*), INTENT(OUT), OPTIONAL :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_SRF_netCDF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    INTEGER :: NC_FileId
!    INTEGER :: nc_status(4)
!    INTEGER :: n_Channels, DimId
!    INTEGER :: Sensor_Type_VarId, Sensor_Channel_VarId, VarID
! 
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!  
!    ! Check the sensor type
!    IF ( Sensor_Type < 1 .OR. Sensor_Type > N_SENSOR_TYPES ) THEN
!      msg = 'Invalid SENSOR_TYPE input.'
!      CALL Create_Cleanup(); RETURN
!    END IF
!    
!    ! Check channel input
!    n_Channels = SIZE(Sensor_Channel)
!    IF ( n_Channels < 1 ) THEN
!      msg = 'SENSOR_CHANNEL array must be non-zero size.'
!      CALL Create_Cleanup(); RETURN
!    END IF
!    IF ( ANY(Sensor_Channel < 1) ) THEN
!      msg = 'Invalid SENSOR_CHANNEL value found.'
!      CALL Create_Cleanup(); RETURN
!    END IF
!
!
!    ! Create the data file
!    ! --------------------
!    nc_status(1) = NF90_CREATE( NC_Filename,NF90_CLOBBER,NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error creating '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(); RETURN
!    END IF
!
!
!    ! Define the dimensions
!    ! ----------------------
!    ! The number of channels
!    nc_status(1) = NF90_DEF_DIM( NC_FileId,CHANNEL_DIMNAME,n_Channels,DimId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining the '//CHANNEL_DIMNAME//' dimension in '// &
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Write the global attributes
!    ! ---------------------------
!    Error_Status = WriteGAtts( NC_Filename                      , &
!                               NC_FileId                        , &
!                               Version         =Version         , &
!                               Sensor_Id       =Sensor_Id       , &
!                               WMO_Satellite_Id=WMO_Satellite_Id, &
!                               WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                               Title           =Title           , &
!                               History         =History         , &
!                               Comment         =Comment         , &
!                               Message_Log     =Message_Log       )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error writing global attributes to '//TRIM(NC_Filename)
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! Define the sensor type scalar variable
!    ! --------------------------------------
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,SENSOR_TYPE_VARNAME,SENSOR_TYPE_TYPE,varID=Sensor_Type_VarID )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//SENSOR_TYPE_VARNAME//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,LONGNAME_ATTNAME,SENSOR_TYPE_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,DESCRIPTION_ATTNAME,SENSOR_TYPE_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,UNITS_ATTNAME,SENSOR_TYPE_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,Sensor_Type_VarID,FILLVALUE_ATTNAME,SENSOR_TYPE_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//SENSOR_TYPE_VARNAME//' variable attributes to '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    
!    ! Define the channel-dimensioned variables and attributes
!    ! -------------------------------------------------------
!    ! The Sensor_Channel
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,SENSOR_CHANNEL_VARNAME,SENSOR_CHANNEL_TYPE,&
!                                   dimIDs=DimId,varID=Sensor_Channel_VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//SENSOR_CHANNEL_VARNAME//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,LONGNAME_ATTNAME,SENSOR_CHANNEL_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,DESCRIPTION_ATTNAME,SENSOR_CHANNEL_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,UNITS_ATTNAME,SENSOR_CHANNEL_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,Sensor_Channel_VarID,FILLVALUE_ATTNAME,SENSOR_CHANNEL_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//SENSOR_CHANNEL_VARNAME//' variable attributes to '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The Integrated_SRF
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,INTEGRATED_SRF_VARNAME,INTEGRAL_SRF_TYPE,&
!                                   dimIDs=DimId,varID=VarID )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//INTEGRATED_SRF_VARNAME//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,VarID,LONGNAME_ATTNAME,INTEGRATED_SRF_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,VarID,DESCRIPTION_ATTNAME,INTEGRATED_SRF_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,VarID,UNITS_ATTNAME,INTEGRAL_SRF_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,VarID,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//INTEGRATED_SRF_VARNAME//' variable attributes to '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The Summation_SRF
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,SUMMATION_SRF_VARNAME,INTEGRAL_SRF_TYPE,&
!                                   dimIDs=DimId,varID=VarID )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//SUMMATION_SRF_VARNAME//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,VarID,LONGNAME_ATTNAME,SUMMATION_SRF_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,VarID,DESCRIPTION_ATTNAME,SUMMATION_SRF_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,VarID,UNITS_ATTNAME,INTEGRAL_SRF_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,VarID,FILLVALUE_ATTNAME,INTEGRAL_SRF_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//SUMMATION_SRF_VARNAME//' variable attributes to '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Take netCDF file out of define mode
!    ! -----------------------------------
!    nc_status(1) = NF90_ENDDEF( NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg =  'Error taking file '//TRIM(NC_Filename)// &
!             ' out of define mode - '//TRIM(NF90_STRERROR( nc_status(1) )) 
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Write the defining data
!    ! -----------------------
!    ! The sensor type
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,Sensor_Type_VarID,Sensor_Type )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg =  'Error writing '//SENSOR_TYPE_VARNAME//' to '//TRIM(NC_Filename)// &
!             ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The sensor channel list
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,Sensor_Channel_VarID,Sensor_Channel )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg =  'Error writing '//SENSOR_CHANNEL_VARNAME//' to '//TRIM(NC_Filename)// &
!             ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Close the file
!    ! --------------
!    nc_status(1) = NF90_CLOSE( NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Create_Cleanup(); RETURN
!    END IF
!
!  CONTAINS
!  
!    SUBROUTINE Create_CleanUp(Close_File)
!      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
!      ! Close file
!      IF ( PRESENT(Close_File) ) THEN
!        IF ( Close_File ) THEN
!          nc_status(1) = NF90_CLOSE( NC_FileId )
!          IF ( nc_status(1) /= NF90_NOERR ) &
!            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
!                  TRIM(NF90_STRERROR( nc_status(1) ))
!        END IF
!      END IF
!      ! Set error status and print error message
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!    END SUBROUTINE Create_CleanUp
!
!  END FUNCTION Create_SRF_netCDF
!
!
!
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Inquire_SRF_netCDF
!!
!! PURPOSE:
!!       Function to inquire a netCDF SRF format file to obtain the dimensions,
!!       channel list, sensor IDs, and global attributes.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Inquire_SRF_netCDF( NC_Filename                        , &  ! Input
!!                                          n_Channels       = n_Channels      , &  ! Optional output
!!                                          n_Points         = n_Points        , &  ! Optional output
!!                                          n_Bands          = n_Bands         , &  ! Optional output
!!                                          Sensor_Type      = Sensor_Type     , &  ! Optional output
!!                                          Sensor_Channel   = Sensor_Channel  , &  ! Optional output
!!                                          Begin_Frequency  = Begin_Frequency , &  ! Optional output
!!                                          End_Frequency    = End_Frequency   , &  ! Optional output
!!                                          Version          = Version         , &  ! Optional output
!!                                          Sensor_ID        = Sensor_ID       , &  ! Optional output
!!                                          WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
!!                                          WMO_Sensor_ID    = WMO_Sensor_ID   , &  ! Optional output
!!                                          Title            = Title           , &  ! Optional output
!!                                          History          = History         , &  ! Optional output
!!                                          Comment          = Comment         , &  ! Optional output
!!                                          RCS_Id           = RCS_Id          , &  ! Revision control
!!                                          Message_Log      = Message_Log )     )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       NC_Filename:        Character string specifying the name of the
!!                           SRF netCDF format data file to inquire.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:        Character string specifying a filename in which any
!!                           messages will be logged. If not specified, or if an
!!                           error occurs opening the log file, the default action
!!                           is to output messages to standard output.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       n_Channels:         The number of channels dimension of the
!!                           SRF data data.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       n_Points:           The number of spectral points used to represent the
!!                           SRF for each channel.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Rank-1, n_Channels
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       n_Bands:            The number of bands used to represent the
!!                           SRF for each channel.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Rank-1, n_Channels
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Sensor_Type:        The flag indicating the type of sensor (IR, MW, etc)
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Sensor_Channel:     The list of channel numbers present in the netCDF
!!                           SRF file. The list may not necessarily
!!                           start at 1 or contain contiguous values.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Rank-1, n_Channels
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Begin_Frequency:    The list of the begin frequency limits for
!!                           each channel's SRF.
!!                           UNITS:      Inverse centimetres (cm^-1)
!!                           TYPE:       REAL(fp)
!!                           DIMENSION:  Rank-1, n_Channels
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       End_Frequency:      The list of the end frequency limits for
!!                           each channel's SRF.
!!                           UNITS:      Inverse centimetres (cm^-1)
!!                           TYPE:       REAL(fp)
!!                           DIMENSION:  Rank-1, n_Channels
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Version:            The version number of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Sensor_ID:          A character string identifying the sensor and
!!                           satellite platform used to contruct filenames.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       WMO_Satellite_ID:   The WMO code used to identify satellite platforms.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       WMO_Sensor_ID:      The WMO code used to identify sensors.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Title:              Character string written into the TITLE global
!!                           attribute field of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       History:            Character string written into the HISTORY global
!!                           attribute field of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       Comment:            Character string written into the COMMENT global
!!                           attribute field of the netCDF SRF file.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!       RCS_Id:             Character string containing the Revision Control
!!                           System Id field for the module.
!!                           UNITS:      N/A
!!                           TYPE:       CHARACTER(*)
!!                           DIMENSION:  Scalar
!!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:       The return value is an integer defining the error
!!                           status. The error codes are defined in the
!!                           Message_Handler module.
!!                           If == SUCCESS the netCDF file inquiry was successful
!!                              == FAILURE - an error occurred opening the netCDF file, or
!!                                         - an error occurred reading any of the
!!                                           requested dimension or variable data.
!!                              == WARNING - an error occurred reading any of the
!!                                           requested global attributes, or
!!                                         - an error occurred closing the netCDF file.
!!                           UNITS:      N/A
!!                           TYPE:       INTEGER
!!                           DIMENSION:  Scalar
!!
!! RESTRICTIONS:
!!       To successfully return any of the channel dimensioned arrays, the
!!       dummy arguments must have at least same size as the dataset in the
!!       netCDF file.
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Inquire_SRF_netCDF( NC_Filename     , &  ! Input
!                               n_Channels      , &  ! Optional output
!                               n_Points        , &  ! Optional output
!                               n_Bands         , &  ! Optional output
!                               Sensor_Type     , &  ! Optional output
!                               Sensor_Channel  , &  ! Optional output
!                               Begin_Frequency , &  ! Optional output
!                               End_Frequency   , &  ! Optional output
!                               Version         , &  ! Optional output
!                               Sensor_ID       , &  ! Optional output
!                               WMO_Satellite_ID, &  ! Optional output
!                               WMO_Sensor_ID   , &  ! Optional output
!                               Title           , &  ! Optional output
!                               History         , &  ! Optional output
!                               Comment         , &  ! Optional output
!                               RCS_Id          , &  ! Revision control
!                               Message_Log     ) &  ! Error messaging
!                             RESULT( Error_Status )
!    ! Arguments
!    CHARACTER(*),            INTENT(IN)  :: NC_Filename
!    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Channels
!    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Points(:)
!    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Bands(:)
!    INTEGER,       OPTIONAL, INTENT(OUT) :: Sensor_Type
!    INTEGER,       OPTIONAL, INTENT(OUT) :: Sensor_Channel(:)
!    REAL(fp),      OPTIONAL, INTENT(OUT) :: Begin_Frequency(:)
!    REAL(fp),      OPTIONAL, INTENT(OUT) :: End_Frequency(:)
!    INTEGER,       OPTIONAL, INTENT(OUT) :: Version
!    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Sensor_ID   
!    INTEGER,       OPTIONAL, INTENT(OUT) :: WMO_Satellite_ID 
!    INTEGER,       OPTIONAL, INTENT(OUT) :: WMO_Sensor_ID
!    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Title
!    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: History
!    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Comment
!    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*),  OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Inquire_SRF_netCDF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    INTEGER :: NC_FileId
!    INTEGER :: nc_status
!    INTEGER :: Allocate_Status
!    INTEGER :: VarId
!    INTEGER :: i, n
!    INTEGER,  ALLOCATABLE :: Local_n_Points(:)
!    INTEGER,  ALLOCATABLE :: Local_n_Bands(:)
!    INTEGER,  ALLOCATABLE :: Local_Sensor_Channel(:)
!    REAL(fp), ALLOCATABLE :: Local_Begin_Frequency(:)
!    REAL(fp), ALLOCATABLE :: Local_End_Frequency(:)
!    REAL(fp), ALLOCATABLE :: f_Band(:)
!    CHARACTER(256) :: Point_DimName  
!    CHARACTER(256) :: Band_DimName   
!    CHARACTER(256) :: f1_Band_VarName
!    CHARACTER(256) :: f2_Band_VarName
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) )  RCS_Id = MODULE_RCS_ID
!
!
!    ! Open the file
!    ! -------------
!    nc_status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error opening '//TRIM(NC_Filename)//' for read access - '// &
!            TRIM(NF90_STRERROR( nc_status ))
!      CALL Inquire_Cleanup(); RETURN
!    END IF
!
!
!    ! Get the number of channels dimension
!    ! ------------------------------------
!    Error_Status = ReadDim( NC_FileId,CHANNEL_DIMNAME,n,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//CHANNEL_DIMNAME//&
!            ' dimension from '//TRIM(NC_Filename)
!      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    ! Set the dimension return value
!    IF ( PRESENT(n_Channels) ) n_Channels = n
!
!
!    ! Allocate the local arrays
!    ! -------------------------
!    ALLOCATE( Local_n_Points(n), &
!              Local_n_Bands(n), &
!              Local_Sensor_Channel(n), &
!              Local_Begin_Frequency(n), &
!              Local_End_Frequency(n), &
!              STAT=Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      WRITE( msg,'("Error allocating local data arrays. STAT = ",i0)' ) Allocate_Status
!      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Get the sensor type if necessary
!    ! --------------------------------
!    IF ( PRESENT(Sensor_Type) ) THEN
!      ! Get the variable Id
!      nc_status = NF90_INQ_VARID( NC_FileId,SENSOR_TYPE_VARNAME,VarId )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_TYPE_VARNAME//&
!              ' variable id - '//TRIM(NF90_STRERROR( nc_status ))
!        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!      END IF
!      ! Get the data
!      nc_status = NF90_GET_VAR( NC_FileId,VarId,Sensor_Type )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        msg = 'Error reading '//SENSOR_TYPE_VARNAME//' data from '//TRIM(NC_Filename)//&
!              ' - '//TRIM(NF90_STRERROR( nc_status ))
!        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!      END IF
!    END IF
!    
!    ! Get the sensor channel data if necessary
!    ! ----------------------------------------
!    IF ( PRESENT(Sensor_Channel) ) THEN
!      ! Get the variable Id
!      nc_status = NF90_INQ_VARID( NC_FileId,SENSOR_CHANNEL_VARNAME,VarId )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SENSOR_CHANNEL_VARNAME//&
!              ' variable id - '//TRIM(NF90_STRERROR( nc_status ))
!        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!      END IF
!      ! Get the data
!      nc_status = NF90_GET_VAR( NC_FileId,VarId,Local_Sensor_Channel )
!      IF ( nc_status /= NF90_NOERR ) THEN
!        msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' data from '//TRIM(NC_Filename)//&
!              ' - '//TRIM(NF90_STRERROR( nc_status ))
!        CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!      END IF
!      ! Set the return value
!      IF ( SIZE(Sensor_Channel) < n ) THEN
!        msg = 'Sensor_Channel array too small to hold data.'
!        CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
!      END IF
!      Sensor_Channel = SENSOR_CHANNEL_FILLVALUE
!      Sensor_Channel(1:n) = Local_Sensor_Channel
!    END IF
!
!
!    ! Get the channel specific data if necessary. Note that the assumption
!    ! here is that if any of these dimensions or variables are defined,
!    ! then they are all defined.
!    ! --------------------------------------------------------------------
!    IF ( PRESENT(n_Points       ) .OR. &
!         PRESENT(n_Bands        ) .OR. &
!         PRESENT(Begin_Frequency) .OR. &
!         PRESENT(End_Frequency  )      ) THEN
!         
!      ! Loop over channels
!      ! ------------------
!      Channel_Loop: DO i = 1, n
!    
!        ! Create the various dim and var names for this channel
!        ! -----------------------------------------------------
!        CALL CreateNames( Local_Sensor_Channel(i), &
!                          Point_DimName  =Point_DimName  , &
!                          Band_DimName   =Band_DimName   , &
!                          f1_Band_VarName=f1_Band_VarName, &
!                          f2_Band_VarName=f2_Band_VarName  )
!                          
!        ! Retrieve dimension values
!        ! -------------------------                        
!        ! Retrieve the number of points dimension value
!        Error_Status = ReadDim( NC_FileId,Point_DimName,Local_n_Points(i),Message_Log=Message_Log )
!        IF ( Error_Status /= SUCCESS ) THEN
!          msg = 'Error obtaining '//TRIM(Point_DimName)//' dimension from '//TRIM(NC_Filename)
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        ! Retrieve the number of bands dimension value
!        Error_Status = ReadDim( NC_FileId,Band_DimName,Local_n_Bands(i),Message_Log=Message_Log )
!        IF ( Error_Status /= SUCCESS ) THEN
!          msg = 'Error obtaining '//TRIM(Band_DimName)//' dimension from '//TRIM(NC_Filename)
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!
!        ! Allocate a band-specific array for this channel
!        ! -----------------------------------------------
!        ALLOCATE( f_Band(Local_n_Bands(i)),STAT=Error_Status )
!        IF ( Error_Status /= 0 ) THEN
!          WRITE( msg,'("Error allocating band frequency array for channel ",i0,". STAT = ",i0)' ) &
!                     Local_Sensor_Channel(i), Error_Status
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        
!        ! Retrieve the band begin frequency values
!        ! ----------------------------------------
!        ! Get the variable id
!        nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f1_Band_Varname),VarId )
!        IF ( nc_status /= NF90_NOERR ) THEN
!          msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f1_Band_Varname)//&
!                ' variable id - '//TRIM(NF90_STRERROR( nc_status ))
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        ! Get the data
!        nc_status = NF90_GET_VAR( NC_FileId,VarId,f_Band )
!        IF ( nc_status /= NF90_NOERR ) THEN
!          msg = 'Error reading '//TRIM(f1_Band_Varname)//' data from '//TRIM(NC_Filename)//&
!                ' - '//TRIM(NF90_STRERROR( nc_status ))
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        ! Save the SRF begin frequency
!        Local_Begin_Frequency(i) = f_Band(1)
!        
!        ! Retrieve the band end frequency values
!        ! ----------------------------------------
!        ! Get the variable id
!        nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f2_Band_Varname),VarId )
!        IF ( nc_status /= NF90_NOERR ) THEN
!          msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f2_Band_Varname)//&
!                ' variable id - '//TRIM(NF90_STRERROR( nc_status ))
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        ! Get the data
!        nc_status = NF90_GET_VAR( NC_FileId,VarId,f_Band )
!        IF ( nc_status /= NF90_NOERR ) THEN
!          msg = 'Error reading '//TRIM(f2_Band_Varname)//' data from '//TRIM(NC_Filename)//&
!                ' - '//TRIM(NF90_STRERROR( nc_status ))
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        ! Save the SRF end frequency
!        Local_End_Frequency(i) = f_Band(Local_n_Bands(i))
!        
!        ! Deallocate band specific array for this channel
!        ! -----------------------------------------------
!        DEALLOCATE( f_Band,STAT=Allocate_Status )
!        IF ( Allocate_Status /= 0 ) THEN
!          WRITE( msg,'("Error deallocating band frequency array for channel ",i0,". STAT = ",i0)' ) &
!                     Local_Sensor_Channel(i), Allocate_Status
!          CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!        END IF
!        
!      END DO Channel_Loop
!
!      ! Set the optional return arguments
!      ! ---------------------------------
!      ! Set the n_Points return value
!      IF ( PRESENT(n_Points) ) THEN
!        IF ( SIZE(n_Points) < n ) THEN
!          msg = 'n_Points array too small to hold data.'
!          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
!        END IF
!        n_Points = NPTS_BAND_FILLVALUE
!        n_Points(1:n) = Local_n_Points
!      END IF
!
!      ! Set the n_Bands return value
!      IF ( PRESENT(n_Bands) ) THEN
!        IF ( SIZE(n_Bands) < n ) THEN
!          msg = 'n_Bands array too small to hold data.'
!          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
!        END IF
!        n_Bands = NPTS_BAND_FILLVALUE
!        n_Bands(1:n) = Local_n_Bands
!      END IF
!
!      ! Set the Begin_Frequency return value
!      IF ( PRESENT(Begin_Frequency) ) THEN
!        IF ( SIZE(Begin_Frequency) < n ) THEN
!          msg = 'Begin_Frequency array too small to hold data.'
!          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
!        END IF
!        Begin_Frequency = FREQUENCY_FILLVALUE
!        Begin_Frequency(1:n) = Local_Begin_Frequency
!      END IF
!
!      ! Set the End_Frequency return value
!      IF ( PRESENT(End_Frequency) ) THEN
!        IF ( SIZE(End_Frequency) < n ) THEN
!          msg = 'End_Frequency array too small to hold data.'
!          CALL Inquire_CleanUp(Close_File=.TRUE.); RETURN
!        END IF
!        End_Frequency = FREQUENCY_FILLVALUE
!        End_Frequency(1:n) = Local_End_Frequency
!      END IF
!
!    END IF
!    
!
!    ! Read the global attributes
!    ! --------------------------
!    Error_Status = ReadGAtts( NC_Filename, &
!                              NC_FileId, &
!                              Version         =Version         , &
!                              Sensor_Id       =Sensor_Id       , &
!                              WMO_Satellite_Id=WMO_Satellite_Id, &
!                              WMO_Sensor_Id   =WMO_Sensor_Id   , &
!                              Title           =Title           , &
!                              History         =History         , &
!                              Comment         =Comment         , &
!                              Message_Log     =Message_Log       )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error reading global attribute from '//TRIM(NC_Filename)
!      CALL Inquire_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Close the file
!    ! --------------
!    nc_status = NF90_CLOSE( NC_FileId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Inquire_Cleanup(); RETURN
!    END IF
!
!
!    ! Deallocate all the local channel dimensioned arrays
!    ! ---------------------------------------------------
!    DEALLOCATE( Local_n_Points       , &
!                Local_n_Bands        , &
!                Local_Sensor_Channel , &
!                Local_Begin_Frequency, &
!                Local_End_Frequency  )
!
!  CONTAINS
!  
!    SUBROUTINE Inquire_CleanUp( Close_File )
!      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
!      ! Deallocate local arrays if necessary
!      IF ( ALLOCATED(Local_n_Points       ) ) DEALLOCATE(Local_n_Points       )
!      IF ( ALLOCATED(Local_n_Bands        ) ) DEALLOCATE(Local_n_Bands        )
!      IF ( ALLOCATED(Local_Sensor_Channel ) ) DEALLOCATE(Local_Sensor_Channel )
!      IF ( ALLOCATED(Local_Begin_Frequency) ) DEALLOCATE(Local_Begin_Frequency)
!      IF ( ALLOCATED(Local_End_Frequency  ) ) DEALLOCATE(Local_End_Frequency  )
!      IF ( ALLOCATED(f_Band               ) ) DEALLOCATE(f_Band               )
!      ! Close file if necessary
!      IF ( PRESENT(Close_File) ) THEN
!        IF ( Close_File ) THEN
!          nc_status = NF90_CLOSE( NC_FileId )
!          IF ( nc_status /= NF90_NOERR ) &
!            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
!                  TRIM(NF90_STRERROR( nc_status ))
!        END IF
!      END IF
!      ! Set error status and print error message
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM(msg), &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END SUBROUTINE Inquire_CleanUp
!
!  END FUNCTION Inquire_SRF_netCDF
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Write_SRF_netCDF
!!
!! PURPOSE:
!!       Function to write data in an SRF structure to a netCDF format
!!       SRF file.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Write_SRF_netCDF( NC_Filename            , &  ! Input
!!                                        SRF                    , &  ! Input
!!                                        RCS_Id     =RCS_Id     , &  !  Revision control
!!                                        Message_Log=Message_Log  )  !  Error messaging
!!
!! INPUT ARGUMENTS:
!!       NC_Filename:     Character string specifying the name of the netCDF
!!                        format SRF data file to write to.
!!                        UNITS:      N/A
!!                        TYPE:       CHARACTER(*)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN)
!!
!!       SRF:             Structure containing the SRF data to write to file.
!!                        UNITS:      N/A
!!                        TYPE:       TYPE(SRF_type)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:     Character string specifying a filename in which any
!!                        messages will be logged. If not specified, or if an
!!                        error occurs opening the log file, the default action
!!                        is to output messages to standard output.
!!                        UNITS:      N/A
!!                        TYPE:       CHARACTER(*)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:          Character string containing the Revision Control
!!                        System Id field for the module.
!!                        UNITS:      N/A
!!                        TYPE:       CHARACTER(*)
!!                        DIMENSION:  Scalar
!!                        ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:    The return value is an integer defining the error
!!                        status. The error codes are defined in the
!!                        Message_Handler module.
!!                        If == SUCCESS the netCDF data write was successful
!!                           == FAILURE an unrecoverable error occurred
!!                        UNITS:      N/A
!!                        TYPE:       INTEGER
!!                        DIMENSION:  Scalar
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Write_SRF_netCDF( NC_Filename , &  ! Input
!                             SRF         , &  ! Input
!                             Quiet       , &  ! Optional input
!                             RCS_Id      , &  ! Revision control
!                             Message_Log ) &  ! Error messaging
!                           RESULT( Error_Status )
!    ! Arguments
!    CHARACTER(*),           INTENT(IN)  :: NC_Filename
!    TYPE(SRF_type),         INTENT(IN)  :: SRF
!    INTEGER     , OPTIONAL, INTENT(IN)  :: Quiet
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Write_SRF_netCDF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    LOGICAL :: Noisy
!    INTEGER :: NC_FileId
!    INTEGER :: nc_status(4)
!    INTEGER :: Allocate_Status
!    INTEGER :: i, n
!    INTEGER :: n_Channels
!    INTEGER :: Channel_Idx(1)
!    INTEGER, ALLOCATABLE :: Sensor_Channel(:)
!    INTEGER :: Sensor_Type
!    CHARACTER(256) :: Frequency_Units
!    CHARACTER(256) :: Channel_Name     
!    CHARACTER(256) :: Point_DimName    
!    CHARACTER(256) :: Band_DimName     
!    CHARACTER(256) :: Response_VarName 
!    CHARACTER(256) :: f1_Band_VarName  
!    CHARACTER(256) :: f2_Band_VarName  
!    CHARACTER(256) :: npts_Band_VarName
!    INTEGER :: n_Points_DimId
!    INTEGER :: n_Bands_DimId
!    INTEGER :: Response_VarId
!    INTEGER :: f1_Band_VarId
!    INTEGER :: f2_Band_VarId
!    INTEGER :: npts_Band_VarId
!    INTEGER :: VarId
!
!
!    ! Setup
!    ! -----
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
! 
!    ! Output informational messages....
!    Noisy = .TRUE.
!    ! ....unless the QUIET keyword is set.
!    IF ( PRESENT( Quiet ) ) THEN
!      IF ( Quiet == SET ) Noisy = .FALSE.
!    END IF
!
!    ! Check structure association
!    IF ( .NOT. Associated_SRF( SRF ) ) THEN
!      msg = 'Some or all INPUT SRF pointer members are NOT associated.'
!      CALL Write_CleanUp(); RETURN
!    END IF
!
!    ! Check SRF channel is valid
!    IF ( SRF%Channel < 1 ) THEN
!      WRITE( msg,'("Invalid SRF channel, ",i0,". Must be > 0.")' ) SRF%Channel
!      CALL Write_CleanUp(); RETURN
!    END IF
!
!    ! Check SRF array sizes
!    IF ( SRF%n_Points < 1 ) THEN
!      WRITE( msg,'("Invalid no. of SRF points, ",i0,". Must be > 0.")' ) SRF%n_Points
!      CALL Write_CleanUp(); RETURN
!    END IF
!    IF ( SRF%n_Bands < 1 ) THEN
!      WRITE( msg,'("Invalid no. of SRF bands, ",i0,". Must be > 0.")' ) SRF%n_Bands
!      CALL Write_CleanUp(); RETURN
!    END IF
!
!    ! Select the frequency units string
!    SELECT CASE(SRF%Sensor_Type)
!      CASE(MICROWAVE_SENSOR)
!        Frequency_Units = 'Gigahertz (GHz)'
!      CASE(INFRARED_SENSOR,VISIBLE_SENSOR,ULTRAVIOLET_SENSOR)
!        Frequency_Units = 'Inverse centimetres (cm^-1)'
!      CASE DEFAULT
!        msg = 'Invalid sensor type'
!        CALL Write_CleanUp(); RETURN
!    END SELECT
!
!
!    ! Check that the SRF sensor type is consistent for the file
!    ! ---------------------------------------------------------
!    ! Get the sensor type
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Type=Sensor_Type,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//SENSOR_TYPE_VARNAME//' from '//TRIM(NC_Filename)
!      CALL Write_CleanUp(); RETURN
!    END IF
!    ! Check if it's the same
!    IF ( SRF%Sensor_Type /= Sensor_Type ) THEN
!      msg = 'File and structure sensor type flags are different!'
!      CALL Write_CleanUp(); RETURN
!    END IF
!    
!    ! Check that the SRF channel is valid for the file
!    ! ------------------------------------------------
!    ! Get the channel dimension
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,n_Channels=n_Channels,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
!      CALL Write_CleanUp(); RETURN
!    END IF
!    ! Allocate a sensor channel array
!    ALLOCATE( Sensor_Channel(n_Channels),STAT=Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      WRITE( msg,'("Error allocating sensor channel array. STAT = ", i5 )' ) Allocate_Status
!      CALL Write_CleanUp(); RETURN
!    END IF
!    ! Read the sensor channel list
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error reading '//SENSOR_CHANNEL_VARNAME//' data from '//TRIM(NC_Filename)
!      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Check if the SRF channel is in the list at all, or more than once
!    n = COUNT(Sensor_Channel == SRF%Channel)
!    IF ( n < 1 ) THEN
!      WRITE( msg,'("SRF channel ",i0," is not in the sensor channel list for ",a)' ) &
!                 SRF%Channel, TRIM(NC_Filename)
!      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    IF ( n > 1 ) THEN
!      WRITE( msg,'("Check ",a," file! SRF channel ",i0,&
!                  &" occurs multiple times in the sensor channel list")' ) &
!                 SRF%Channel, TRIM(NC_Filename)
!      CALL Write_CleanUp(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Get the index of the current channel in the sensor channel list
!    Channel_Idx = PACK((/(i,i=1,n_Channels)/),Sensor_Channel == SRF%Channel)
!    ! Deallocate the sensor channel list array
!    DEALLOCATE( Sensor_Channel )
!
!
!    ! Create the SRF dimension and variable names for the current channel
!    ! -------------------------------------------------------------------
!    CALL CreateNames( SRF%Channel, &
!                      Channel_Name     =Channel_Name     , &
!                      Point_DimName    =Point_DimName    , &
!                      Band_DimName     =Band_DimName     , &
!                      Response_VarName =Response_VarName , &
!                      f1_Band_VarName  =f1_Band_VarName  , &
!                      f2_Band_VarName  =f2_Band_VarName  , &
!                      npts_Band_VarName=npts_Band_VarName  )
!
!
!    ! Open the file
!    ! -------------
!    nc_status(1) = NF90_OPEN( NC_Filename,NF90_WRITE,NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error opening '//TRIM(NC_Filename)//' for channel '//TRIM(Channel_Name)//&
!            ' write access - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(); RETURN
!    END IF
!
!
!    ! Put netcdf file into define mode 
!    !----------------------------------
!    nc_status(1) = NF90_REDEF( NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error putting file '//TRIM(NC_Filename)//' into define mode for channel '//&
!            TRIM(Channel_Name)//'- '//TRIM( NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Define the dimensions for this channel
!    ! --------------------------------------
!    ! The n_Bands dimension
!    nc_status(1) = NF90_DEF_DIM( NC_FileId,TRIM(Band_DimName),SRF%n_Bands,n_Bands_DimId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining the '//TRIM(Band_DimName)//' dimension in '// &
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    ! The n_Points dimension
!    nc_status(1) = NF90_DEF_DIM( NC_FileId,TRIM(Point_DimName),SRF%n_Points,n_Points_DimId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining the '//TRIM(Point_DimName)//' dimension in '// &
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    
!    ! Define the band variables
!    ! -------------------------
!    ! The band begin frequency variable
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,TRIM(f1_Band_VarName),FREQUENCY_TYPE,&
!                                   dimIDs=n_Bands_DimId,varID=f1_Band_VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//TRIM(f1_Band_VarName)//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,LONGNAME_ATTNAME,F1_BAND_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,DESCRIPTION_ATTNAME,F1_BAND_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,UNITS_ATTNAME,TRIM(Frequency_Units) )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,f1_Band_VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//TRIM(f1_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    
!    ! The band end frequency variable
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,TRIM(f2_Band_VarName),FREQUENCY_TYPE,&
!                                   dimIDs=n_Bands_DimId,varID=f2_Band_VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//TRIM(f2_Band_VarName)//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,LONGNAME_ATTNAME,F2_BAND_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,DESCRIPTION_ATTNAME,F2_BAND_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,UNITS_ATTNAME,TRIM(Frequency_Units) )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,f2_Band_VarID,FILLVALUE_ATTNAME,FREQUENCY_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//TRIM(f2_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The band npts variable
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,TRIM(npts_Band_VarName),NPTS_BAND_TYPE,&
!                                   dimIDs=n_Bands_DimId,varID=npts_Band_VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//TRIM(npts_Band_VarName)//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,LONGNAME_ATTNAME,NPTS_BAND_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,DESCRIPTION_ATTNAME,NPTS_BAND_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,UNITS_ATTNAME,NPTS_BAND_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,npts_Band_VarID,FILLVALUE_ATTNAME,NPTS_BAND_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//TRIM(npts_Band_VarName)//' variable attributes to '//TRIM(NC_Filename)
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Define the response variable
!    ! ----------------------------
!    nc_status(1) = NF90_DEF_VAR( NC_FileId,TRIM(Response_VarName),RESPONSE_TYPE,&
!                                   dimIDs=n_Points_DimId,varID=Response_VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error defining '//TRIM(Response_VarName)//' variable in '//&
!            TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_ATT( NC_FileId,Response_VarID,LONGNAME_ATTNAME,RESPONSE_LONGNAME )
!    nc_status(2) = NF90_PUT_ATT( NC_FileId,Response_VarID,DESCRIPTION_ATTNAME,RESPONSE_DESCRIPTION )
!    nc_status(3) = NF90_PUT_ATT( NC_FileId,Response_VarID,UNITS_ATTNAME,RESPONSE_UNITS )
!    nc_status(4) = NF90_PUT_ATT( NC_FileId,Response_VarID,FILLVALUE_ATTNAME,RESPONSE_FILLVALUE )
!    IF ( ANY(nc_status /= NF90_NOERR) ) THEN
!      msg = 'Error writing '//TRIM(Response_VarName)//' variable attributes to '//TRIM(NC_Filename)
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Put the file into data mode 
!    !----------------------------
!    nc_status(1) = NF90_ENDDEF( NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error placing '//TRIM(NC_Filename)//' in DATA mode for channel '//&
!            TRIM(Channel_Name)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Write the channel dependent data
!    ! --------------------------------
!    ! The integrated SRF value
!    nc_status(1) = NF90_INQ_VARID( NC_FileId,INTEGRATED_SRF_VARNAME,VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INTEGRATED_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,VarID,SRF%Integrated_SRF,START=Channel_Idx )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing channel '//TRIM(Channel_Name)//' '//INTEGRATED_SRF_VARNAME//&
!            ' to '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    
!    ! The summed SRF value
!    nc_status(1) = NF90_INQ_VARID( NC_FileId,SUMMATION_SRF_VARNAME,VarId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SUMMATION_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,VarID,SRF%Summation_SRF,START=Channel_Idx )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing channel '//TRIM(Channel_Name)//' '//SUMMATION_SRF_VARNAME//&
!            ' to '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    
!
!    ! Write the band dependent data
!    ! -----------------------------
!    ! The band begin frequencies
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,f1_Band_VarID,SRF%f1_Band )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing '//TRIM(f1_Band_VarName)//' to '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The band end frequencies
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,f2_Band_VarID,SRF%f2_Band )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing '//TRIM(f2_Band_VarName)//' to '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    ! The number of band points
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,npts_Band_VarID,SRF%npts_Band )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing '//TRIM(npts_Band_VarName)//' to '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Write the SRF response
!    ! ----------------------
!    nc_status(1) = NF90_PUT_VAR( NC_FileId,Response_VarID,SRF%Response )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error writing '//TRIM(Response_VarName)//' to '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!    
!    ! Close the file
!    ! --------------
!    nc_status(1) = NF90_CLOSE( NC_FileId )
!    IF ( nc_status(1) /= NF90_NOERR ) THEN
!      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nc_status(1) ))
!      CALL Write_Cleanup(); RETURN
!    END IF
!
!
!    ! Output an info message
!    ! ----------------------
!    IF ( Noisy ) THEN
!      CALL Info_SRF( SRF, msg )
!      CALL Display_Message( ROUTINE_NAME, &
!                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
!                            INFORMATION, &
!                            Message_Log=Message_Log )
!    END IF
!
!  CONTAINS
!
!    SUBROUTINE Write_CleanUp( Dealloc_Arrays,Close_File )
!      LOGICAL, OPTIONAL, INTENT(IN) :: Dealloc_Arrays
!      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
!      ! Deallocate local arrays if necessary
!      IF ( PRESENT(Dealloc_Arrays) ) THEN
!        IF ( Dealloc_Arrays ) THEN
!          DEALLOCATE( Sensor_Channel,STAT=Allocate_Status )
!          IF ( Allocate_Status /= 0 ) &
!            WRITE( msg,'(a,"; Error deallocating local arrays during error cleanup. STAT=",i0)') &
!                       TRIM(msg), Allocate_Status
!        END IF
!      END IF
!      ! Close file if necessary
!      IF ( PRESENT(Close_File) ) THEN
!        IF ( Close_File ) THEN
!          nc_status(1) = NF90_CLOSE( NC_FileId )
!          IF ( nc_status(1) /= NF90_NOERR ) &
!            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
!                  TRIM(NF90_STRERROR( nc_status(1) ))
!        END IF
!      END IF
!      ! Set error status and print error message
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM(msg), &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END SUBROUTINE Write_CleanUp
!
!  END FUNCTION Write_SRF_netCDF
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Read_SRF_netCDF
!!
!! PURPOSE:
!!       Function to read a selected channels SRF data from a netCDF SRF
!!       format file.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Read_SRF_netCDF( NC_Filename            , &  ! Input  
!!                                       Channel                , &  ! Input  
!!                                       SRF                    , &  ! Output 
!!                                       Quiet      =Quiet      , &  ! Optional input
!!                                       RCS_Id     = RCS_Id    , &  ! Revision control
!!                                       Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       NC_Filename:  Character string specifying the name of the netCDF
!!                     SRF format data file to read.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN)
!!
!!       Channel:      Channel number for which the SRF data is required.
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN)
!!
!! OUTPUT ARGUMENTS:
!!       SRF:          Structure containing the requested SRF data.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(SRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Quiet:        Set this keyword to suppress information messages being
!!                     printed to standard output (or the message log file if
!!                     the MESSAGE_LOG optional argument is used.) By default,
!!                     information messages are printed.
!!                     If QUIET = 0, information messages are OUTPUT.
!!                        QUIET = 1, information messages are SUPPRESSED.
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:  Character string specifying a filename in which any
!!                     messages will be logged. If not specified, or if an
!!                     error occurs opening the log file, the default action
!!                     is to output messages to standard output.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:       Character string containing the Revision Control
!!                     System Id field for the module.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status: The return value is an integer defining the error
!!                     status. The error codes are defined in the
!!                     Message_Handler module.
!!                     If == SUCCESS the netCDF data read was successful
!!                        == FAILURE an unrecoverable error occurred
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output SRF argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined on
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Read_SRF_netCDF( NC_Filename , &  ! Input
!                            Channel     , &  ! Input
!                            SRF         , &  ! Output
!                            Quiet       , &  ! Optional input
!                            RCS_Id      , &  ! Revision control
!                            Message_Log ) &  ! Error messaging
!                          RESULT( Error_Status )
!    ! Arguments
!    CHARACTER(*),           INTENT(IN)     :: NC_Filename
!    INTEGER,                INTENT(IN)     :: Channel
!    TYPE(SRF_type),         INTENT(IN OUT) :: SRF
!    INTEGER,      OPTIONAL, INTENT(IN)     :: Quiet
!    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Read_SRF_netCDF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    LOGICAL :: Noisy
!    INTEGER :: NC_FileId
!    INTEGER :: nc_status
!    INTEGER :: Allocate_Status
!    INTEGER :: i, n
!    INTEGER :: Version         
!    CHARACTER(256) :: Sensor_ID       
!    INTEGER :: WMO_Satellite_ID
!    INTEGER :: WMO_Sensor_ID   
!    INTEGER :: Sensor_Type
!    INTEGER :: n_Channels, n_Points, n_Bands
!    INTEGER :: Channel_Idx(1)
!    INTEGER, ALLOCATABLE :: Sensor_Channel(:)
!    CHARACTER(256) :: Channel_Name     
!    CHARACTER(256) :: Point_DimName    
!    CHARACTER(256) :: Band_DimName     
!    CHARACTER(256) :: Response_VarName 
!    CHARACTER(256) :: f1_Band_VarName  
!    CHARACTER(256) :: f2_Band_VarName  
!    CHARACTER(256) :: npts_Band_VarName
!    INTEGER :: n_Points_DimId
!    INTEGER :: n_Bands_DimId
!    INTEGER :: Reponse_VarId
!    INTEGER :: f1_Band_VarId
!    INTEGER :: f2_Band_VarId
!    INTEGER :: npts_Band_VarId
!    INTEGER :: VarId
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!
!    ! Output informational messages....
!    Noisy = .TRUE.
!    ! ....unless the QUIET keyword is set.
!    IF ( PRESENT(Quiet) ) THEN
!      IF ( Quiet == SET ) Noisy = .FALSE.
!    END IF
!
!    ! Check that the SRF channel is valid for the file
!    ! ------------------------------------------------
!    ! Get the channel dimension
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,n_Channels=n_Channels,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(); RETURN
!    END IF
!    ! Allocate a sensor channel array
!    ALLOCATE( Sensor_Channel(n_Channels),STAT=Allocate_Status )
!    IF ( Allocate_Status /= 0 ) THEN
!      WRITE( msg,'("Error allocating sensor channel array. STAT = ", i5 )' ) Allocate_Status
!      CALL Read_Cleanup(); RETURN
!    END IF
!    ! Read the sensor channel list
!    Error_Status = Inquire_SRF_netCDF( NC_Filename,Sensor_Channel=Sensor_Channel,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error reading '//CHANNEL_DIMNAME//' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Check if the requested channel is in the list at all, or more than once
!    n = COUNT(Sensor_Channel == Channel)
!    IF ( n < 1 ) THEN
!      WRITE( msg,'("SRF channel ",i0," is not in the sensor channel list for ",a)' ) &
!                 Channel, TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    IF ( n > 1 ) THEN
!      WRITE( msg,'("Check ",a," file! SRF channel ",i0,&
!                  &" occurs multiple times in the sensor channel list")' ) &
!                 SRF%Channel, TRIM(NC_Filename)
!      CALL Read_Cleanup(Dealloc_Arrays=.TRUE.); RETURN
!    END IF
!    ! Get the index of the current channel in the sensor channel list
!    Channel_Idx = PACK((/(i,i=1,n_Channels)/),Sensor_Channel == Channel)
!    ! Deallocate the sensor channel list array
!    DEALLOCATE( Sensor_Channel )
!
!
!    ! Read some of the global attributes
!    ! ----------------------------------
!    Error_Status = Inquire_SRF_netCDF( NC_Filename                        , &  ! Input
!                                       Version          = Version         , &  ! Optional output
!                                       Sensor_ID        = Sensor_ID       , &  ! Optional output
!                                       WMO_Satellite_ID = WMO_Satellite_ID, &  ! Optional output
!                                       WMO_Sensor_ID    = WMO_Sensor_ID   , &  ! Optional output
!                                       Sensor_Type      = Sensor_Type     , &  ! Optional output
!                                       Message_Log      = Message_Log       )  ! Error messaging
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error occurred reading global attributes from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(); RETURN
!    END IF
!    
!    
!    ! Create the SRF dimension and variable names for the current channel
!    ! -------------------------------------------------------------------
!    CALL CreateNames( Channel, &
!                      Channel_Name     =Channel_Name     , &
!                      Point_DimName    =Point_DimName    , &
!                      Band_DimName     =Band_DimName     , &
!                      Response_VarName =Response_VarName , &
!                      f1_Band_VarName  =f1_Band_VarName  , &
!                      f2_Band_VarName  =f2_Band_VarName  , &
!                      npts_Band_VarName=npts_Band_VarName  )
!                      
!                      
!    ! Open the file
!    ! -------------
!    nc_status = NF90_OPEN( NC_Filename,NF90_NOWRITE,NC_FileId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error opening '//TRIM(NC_Filename)//' for channel '//TRIM(Channel_Name)//&
!            ' read access - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(); RETURN
!    END IF
!
!
!    ! Retrieve channel dimension values
!    ! ---------------------------------                        
!    ! Retrieve the number of points dimension value
!    Error_Status = ReadDim( NC_FileId,Point_DimName,n_Points,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//TRIM(Point_DimName)//&
!            ' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!    ! Retrieve the number of bands dimension value
!    Error_Status = ReadDim( NC_FileId,Band_DimName,n_Bands,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error obtaining '//TRIM(Band_DimName)//&
!            ' dimension from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Allocate the output SRF structure
!    ! ---------------------------------
!    Error_Status = Allocate_SRF( n_Points,SRF,n_Bands=n_Bands,Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error occurred allocating SRF structure.'
!      CALL Read_Cleanup(Close_File=.TRUE.); RETURN
!    END IF
!
!
!    ! Set the sensor and channel values
!    ! ---------------------------------
!    SRF%Version          = Version
!    SRF%Sensor_ID        = TRIM(Sensor_ID)
!    SRF%WMO_Satellite_ID = WMO_Satellite_ID
!    SRF%WMO_Sensor_ID    = WMO_Sensor_ID   
!    SRF%Sensor_Type      = Sensor_Type
!    SRF%Channel          = Channel
!
!      
!    ! Read the channel dependent data
!    ! -------------------------------
!    ! The integrated SRF value
!    nc_status = NF90_INQ_VARID( NC_FileId,INTEGRATED_SRF_VARNAME,VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//INTEGRATED_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Integrated_SRF,START=Channel_Idx )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//INTEGRATED_SRF_VARNAME//&
!            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    
!    ! The summed SRF value
!    nc_status = NF90_INQ_VARID( NC_FileId,SUMMATION_SRF_VARNAME,VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//SUMMATION_SRF_VARNAME//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Summation_SRF,START=Channel_Idx )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading channel '//TRIM(Channel_Name)//' '//SUMMATION_SRF_VARNAME//&
!            ' from '//TRIM(NC_Filename)//' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    
!    ! Read the band dependent data
!    ! ----------------------------
!    ! The band begin frequencies
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f1_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f1_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%f1_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(f1_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!    ! The band end frequencies
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(f2_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(f2_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%f2_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(f2_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!    ! The number of band points
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(npts_Band_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(npts_Band_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%npts_Band )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(npts_Band_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!
!    ! Read the SRF response
!    ! ---------------------
!    nc_status = NF90_INQ_VARID( NC_FileId,TRIM(Response_VarName),VarId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error inquiring '//TRIM(NC_Filename)//' for '//TRIM(Response_VarName)//&
!            ' variable ID - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!    nc_status = NF90_GET_VAR( NC_FileId,VarID,SRF%Response )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error reading '//TRIM(Response_VarName)//' from '//TRIM(NC_Filename)//&
!            ' - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Close_File=.TRUE.,Destroy_Structure=.TRUE.); RETURN
!    END IF
!
! 
!    ! Close the file
!    ! --------------
!    nc_status = NF90_CLOSE( NC_FileId )
!    IF ( nc_status /= NF90_NOERR ) THEN
!      msg = 'Error closing input file - '//TRIM(NF90_STRERROR( nc_status ))
!      CALL Read_Cleanup(Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!
!    ! Compute the SRF frequency grid 
!    !-------------------------------
!    Error_Status = Frequency_SRF( SRF )
!    IF ( Error_Status /= SUCCESS ) THEN
!      msg = 'Error computing frequency grid for channel '//TRIM(Channel_Name)//&
!            ' SRF from '//TRIM(NC_Filename)
!      CALL Read_Cleanup(Destroy_Structure=.TRUE.); RETURN
!    END IF
!
!
!    ! Output an info message
!    ! ----------------------
!    IF ( Noisy ) THEN
!      CALL Info_SRF( SRF, msg )
!      CALL Display_Message( ROUTINE_NAME, &
!                            'FILE: '//TRIM(NC_Filename)//'; '//TRIM(msg), &
!                            INFORMATION, &
!                            Message_Log=Message_Log )
!    END IF
!
!  CONTAINS
!  
!    SUBROUTINE Read_CleanUp( Dealloc_Arrays, Close_File, Destroy_Structure )
!      LOGICAL, OPTIONAL, INTENT(IN) :: Dealloc_Arrays
!      LOGICAL, OPTIONAL, INTENT(IN) :: Close_File
!      LOGICAL, OPTIONAL, INTENT(IN) :: Destroy_Structure
!      ! Deallocate local arrays if necessary
!      IF ( PRESENT(Dealloc_Arrays) ) THEN
!        IF ( Dealloc_Arrays ) THEN
!          DEALLOCATE( Sensor_Channel,STAT=Allocate_Status )
!          IF ( Allocate_Status /= 0 ) &
!            WRITE( msg,'(a,"; Error deallocating local arrays during error cleanup. STAT=",i0)') &
!                       TRIM(msg), Allocate_Status
!        END IF
!      END IF
!      ! Close file if necessary
!      IF ( PRESENT(Close_File) ) THEN
!        IF ( Close_File ) THEN
!          nc_status = NF90_CLOSE( NC_FileId )
!          IF ( nc_status /= NF90_NOERR ) &
!            msg = TRIM(msg)//'; Error closing input file during error cleanup - '//&
!                  TRIM(NF90_STRERROR( nc_status ))
!        END IF
!      END IF
!      ! Destroy the structure if necessary
!      IF ( PRESENT(Destroy_Structure) ) THEN
!        IF ( Destroy_Structure ) THEN
!          Error_Status = Destroy_SRF(SRF, Message_Log=Message_Log)
!          IF ( Error_Status /= SUCCESS ) &
!            msg = TRIM(msg)//'; Error destroying SRF structure during error cleanup.'
!        END IF
!      END IF
!      ! Set error status and print error message
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM(msg), &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END SUBROUTINE Read_CleanUp
!
!  END FUNCTION Read_SRF_netCDF

END MODULE oSRF_File_Define
 
