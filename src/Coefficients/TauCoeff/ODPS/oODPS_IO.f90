!
! oODPS_IO
!
! Module containing object and methods for netCDF I/O of ODPS TauCoeff files.
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, 13-Apr-2016
!                   paul.vandelst@noaa.gov
!                   Based on ODPS_netCDF_IO module.
!

MODULE oODPS_IO

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds     , ONLY: Long, Double, Single, fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: File_Exists
  USE String_Utility , ONLY: StrClean
  USE oODPS_Define  
  USE netcdf
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: oODPS_File_type


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
    '$Id$'
  ! Literal constants
  REAL(Double), PARAMETER :: ZERO = 0.0_Double
  REAL(Double), PARAMETER :: ONE  = 1.0_Double
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
  CHARACTER(*), PARAMETER :: OCOMPONENT_INDEX_DESCRIPTION  = 'OComponent Index for water line absorption'

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
  CHARACTER(*), PARAMETER :: OCOMPONENT_INDEX_LONGNAME  = 'ODAS Component Index'

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
  CHARACTER(*), PARAMETER :: OCOMPONENT_INDEX_UNITS  = 'N/A'
  
  ! Variable _FillValue attribute.
  CHARACTER(*), PARAMETER :: FILLVALUE_ATTNAME = '_FillValue'
     
  INTEGER(Long), PARAMETER :: GROUP_INDEX_FILLVALUE       = 0
  INTEGER(Long), PARAMETER :: SENSOR_TYPE_FILLVALUE       = INVALID_SENSOR
  REAL(Double) , PARAMETER :: REF_LEVEL_PRESSURE_FILLVALUE= ZERO
  REAL(Double) , PARAMETER :: REF_PRESSURE_FILLVALUE      = ZERO
  REAL(Double) , PARAMETER :: REF_TEMPERATURE_FILLVALUE   = ZERO
  REAL(Double) , PARAMETER :: REF_ABSORBER_FILLVALUE      = ZERO
  REAL(Double) , PARAMETER :: MIN_ABSORBER_FILLVALUE      = ZERO
  REAL(Double) , PARAMETER :: MAX_ABSORBER_FILLVALUE      = ZERO
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
  REAL(Double) , PARAMETER :: ODAS_COEFFICIENTS_FILLVALUE = ZERO
  REAL(Double) , PARAMETER :: ALPHA_FILLVALUE             = ZERO
  REAL(Double) , PARAMETER :: ALPHA_C1_FILLVALUE          = ZERO
  REAL(Double) , PARAMETER :: ALPHA_C2_FILLVALUE          = ZERO
  INTEGER(Long), PARAMETER :: OCOMPONENT_INDEX_FILLVALUE  = 0

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
  INTEGER, PARAMETER :: OCOMPONENT_INDEX_TYPE  = NF90_INT


  ! ----------------------------
  ! oODPS_File object definition
  ! ----------------------------
  TYPE :: oODPS_File_type
    INTEGER :: fileid = -1
    CHARACTER(:), ALLOCATABLE :: filename
    CHARACTER(:), ALLOCATABLE :: title
    CHARACTER(:), ALLOCATABLE :: history
    CHARACTER(:), ALLOCATABLE :: comment
    CHARACTER(:), ALLOCATABLE :: profile_set_id
    TYPE(oODPS_type) :: odps
  CONTAINS
    PRIVATE
    PROCEDURE, PUBLIC, PASS(this) :: Destroy
    PROCEDURE, PUBLIC, PASS(this) :: ReadFile
    PROCEDURE, PUBLIC, PASS(this) :: WriteFile
  END TYPE oODPS_File_type


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
!   Destroy
!
! PURPOSE:
!   Elemental subroutine method to re-initialize oODPS_File objects.
!
! CALLING SEQUENCE:
!   CALL f_obj%Destroy()
!
! OBJECTS:
!   f_obj:   Re-initialized oODPS_File object(s).
!            UNITS:      N/A
!            CLASS:      oODPS_File_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Destroy( this )
    CLASS(oODPS_File_type), INTENT(OUT) :: this
    this%fileid = -1
  END SUBROUTINE Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Readfile
!
! PURPOSE:
!   Function method to read netCDF ODPS datafiles.
!
! CALLING SEQUENCE:
!   err_stat = f_obj%Readfile( filename, quiet = quiet )
!
! OBJECTS:
!   f_obj:     oODPS_File object.
!              UNITS:      N/A
!              CLASS:      oODPS_File_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT)
!
! INPUT ARGUMENTS:
!   filename:  The name of the netCDF ODPS file to read.
!              UNITS:      N/A
!              TYPE:       CHARACTER(*)
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   quiet:     Set this keyword to suppress information messages being
!              printed to standard output. By default, information messages
!              are printed.
!              If QUIET = .FALSE., information messages are OUTPUT. [DEFAULT)
!                 QUIET = .TRUE. , information messages are SUPPRESSED.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   err_stat:  The return value is an integer defining the error status.
!              The error codes are defined in the Message_Handler module.
!              If == SUCCESS the netCDF file read was successful
!                 == FAILURE an unrecoverable read error occurred.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION ReadFile( &
    this    , &  ! Output
    filename, &  ! Input
    quiet   , &  ! Optional input
    debug   ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CLASS(oODPS_File_type), INTENT(OUT) :: this
    CHARACTER(*),           INTENT(IN)  :: filename
    LOGICAL,      OPTIONAL, INTENT(IN)  :: quiet
    LOGICAL,      OPTIONAL, INTENT(IN)  :: debug
    ! Function result
    INTEGER :: err_stat
    ! Function parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oODPS_IO::ReadFile'
    ! Function variables
    CHARACTER(ML) :: msg
    LOGICAL :: close_file
    LOGICAL :: noisy
    INTEGER :: nf90_stat

    INTEGER :: n_layers    
    INTEGER :: n_levels    
    INTEGER :: n_components
    INTEGER :: n_absorbers 
    INTEGER :: n_channels  
    INTEGER :: n_coefficients    
    INTEGER :: n_opindex   
    INTEGER :: n_co_coefficients

    TYPE(oODPS_type) :: dummy
    CHARACTER(5000) :: title
    CHARACTER(5000) :: history
    CHARACTER(5000) :: comment
    CHARACTER(5000) :: profile_set_id

    ! Set up
    err_stat = SUCCESS
    close_file = .FALSE.
    ! ...Check that the file exists
    IF ( .NOT. File_Exists(Filename) ) THEN
      msg = 'File '//TRIM(Filename)//' not found.'
      CALL Cleanup(); RETURN
    END IF
    this%filename = TRIM(filename)
    ! ...Check quiet argument
    noisy = .TRUE.
    IF ( PRESENT(quiet) ) noisy = .NOT. quiet
    ! ...Override quiet settings if debug set.
    IF ( PRESENT(debug) ) THEN
      IF ( debug ) noisy = .TRUE.
    END IF


    ! Open the file for reading
    nf90_stat = NF90_OPEN( this%filename,NF90_NOWRITE,this%fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error opening '//TRIM(this%filename)//' for read access - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF
    ! ...Close the file if any error from here on
    close_file = .TRUE.


    ! Read the global attributes
    err_stat = ReadGAtts( &
      this%fileid, &
      Release          = dummy%Release         , &
      Version          = dummy%Version         , &
      Sensor_Id        = dummy%Sensor_Id       , &
      Algorithm        = dummy%Algorithm       , &
      WMO_Sensor_Id    = dummy%WMO_Sensor_Id   , &
      WMO_Satellite_Id = dummy%WMO_Satellite_Id, &
      Title            = title                 , &
      History          = history               , &
      Comment          = comment               , &
      Profile_Set_Id   = profile_set_id          )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading global attribute from '//TRIM(this%filename)
      CALL Cleanup(); RETURN
    END IF
    ! ...Check if release is valid
    IF ( .NOT. dummy%Valid_Release() ) THEN
      msg = 'Release check failed.'
      CALL Cleanup(); RETURN
    END IF
    ! ...Check if algorithm is valid
    IF ( .NOT. dummy%Valid_Algorithm() ) THEN
      msg = 'Algorithm check failed.'
      CALL Cleanup(); RETURN
    END IF
    ! ...Save the file attributes
    this%title          = TRIM(title)         
    this%history        = TRIM(history)       
    this%comment        = TRIM(comment)       
    this%profile_set_id = TRIM(profile_set_id)


    ! Get the dimensions
    err_stat = ReadDimensions( &
      this%fileid, &
      n_layers         , &
      n_levels         , &
      n_components     , &
      n_absorbers      , &
      n_channels       , &
      n_coefficients   , &
      n_opindex        , &
      n_co_coefficients  )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading dimensions  - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF


    ! Allocate the current oODPS object
    CALL this%odps%Create( &
           n_layers         , &
           n_components     , &
           n_absorbers      , &
           n_channels       , &
           n_coefficients   , &
           n_co_coefficients  )
    IF ( .NOT. this%odps%Is_Usable() ) THEN
      msg = 'Error allocating output oODPS object'
      CALL Cleanup(); RETURN
    END IF
    ! ...Assign the object attributes
    this%odps%Version          = dummy%Version
    this%odps%Sensor_Id        = dummy%Sensor_Id
    this%odps%WMO_Sensor_Id    = dummy%WMO_Sensor_Id
    this%odps%WMO_Satellite_Id = dummy%WMO_Satellite_Id


    ! Read the variables
    err_stat = ReadVariables( this%odps, this%fileid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error reading variables - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_stat = NF90_CLOSE( this%fileid ); close_file = .FALSE.
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL Display_Message( ROUTINE_NAME, &
                             'FILE: '//TRIM(this%filename)//'; '//this%odps%Info(), &
                             INFORMATION )
     END IF

  CONTAINS

    SUBROUTINE Cleanup()
      IF ( close_file ) THEN
        nf90_stat = NF90_CLOSE( this%fileid )
        IF ( nf90_stat /= NF90_NOERR ) &
          msg = TRIM(msg)//'; Error closing input file during error cleanup- '//&
                TRIM(NF90_STRERROR( nf90_stat ))
      END IF
      CALL this%Destroy()
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME, msg, err_stat )
    END SUBROUTINE Cleanup

  END FUNCTION ReadFile


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Writefile
!
! PURPOSE:
!   Function method to write netCDF ODPS datafiles.
!
! CALLING SEQUENCE:
!   err_stat = f_obj%Writefile( quiet = quiet, clobber = clobber )
!
! OBJECTS:
!   f_obj:     oODPS_File object.
!              UNITS:      N/A
!              CLASS:      oODPS_File_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   quiet:     Set this logical argument to suppress INFORMATION
!              messages being printed to stdout
!              If == .FALSE., INFORMATION messages are OUTPUT [DEFAULT].
!                 == .TRUE.,  INFORMATION messages are SUPPRESSED.
!              If not specified, default is .FALSE.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN), OPTIONAL
!
!   clobber:   Set this logical argument to overwrite an existing filename
!              If == .FALSE., an existing file is NOT overwritten, and the
!                             function returns with an error [DEFAULT].
!                 == .TRUE.,  an existing file is overwritten with the new data.
!              If not specified, default is .FALSE.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!   err_stat:  The return value is an integer defining the error status.
!              The error codes are defined in the Message_Handler module.
!              If == SUCCESS the netCDF file read was successful
!                 == FAILURE an unrecoverable read error occurred.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION WriteFile( &
    this   , &  ! Input
    quiet  , &  ! Optional input
    clobber, &  ! Optional input
    debug  ) &  ! Optional input (Debug output control)
  RESULT( err_stat )
    ! Arguments
    CLASS(oODPS_File_type), INTENT(IN) :: this
    LOGICAL,      OPTIONAL, INTENT(IN) :: quiet
    LOGICAL,      OPTIONAL, INTENT(IN) :: clobber
    LOGICAL,      OPTIONAL, INTENT(IN) :: debug
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oODPS_IO::WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg
    LOGICAL :: noisy
    LOGICAL :: no_clobber
    LOGICAL :: new_file
    INTEGER :: nf90_stat
    INTEGER :: clobber_flag
    INTEGER :: fileid
    INTEGER :: n_layers_dimid
    INTEGER :: n_levels_dimid
    INTEGER :: n_components_dimid
    INTEGER :: n_absorbers_dimid
    INTEGER :: n_channels_dimid
    INTEGER :: n_coeffs_dimid
    INTEGER :: n_opindex_dimid
    INTEGER :: n_ocoeffs_dimid

    ! Set up
    err_stat = SUCCESS
    ! ...Check structure
    IF ( .NOT. this%odps%Is_Usable() ) THEN
      msg = 'No data to write!'
      CALL Cleanup(); RETURN
    END IF
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(quiet) ) noisy = .NOT. quiet
    ! ...Check Clobber argument
    no_clobber = .TRUE.
    IF ( PRESENT(clobber) ) no_clobber = .NOT. clobber
    ! ...Override Quiet settings if debug set.
    IF ( PRESENT(debug) ) THEN
      IF ( debug ) noisy = .TRUE.
    END IF


    ! Open the file
    new_file = .TRUE.
    ! ...Set the clobber flag
    IF ( no_clobber ) THEN
      clobber_flag = NF90_NOCLOBBER
    ELSE
      clobber_flag = NF90_CLOBBER
    END IF
    ! ...Create the file
    nf90_stat = NF90_CREATE( &
      this%filename, &
      clobber_flag+NF90_NETCDF4, &
      fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      ! Was creation failure due to existing file?
      IF ( nf90_stat == NF90_EEXIST ) THEN
        ! ...Yes, so just open it
        nf90_stat = NF90_OPEN( &
          this%filename  , &
          NF90_WRITE, &  ! Test with NF90_SHARE?
          fileid      )
        IF ( nf90_stat /= NF90_NOERR ) THEN
          msg = 'Error opening existing file, '//TRIM(this%filename)//', for write access - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
          CALL Cleanup(); RETURN
        END IF
        new_file = .FALSE.
      ELSE
        ! ...No, so toss an error
        msg = 'Error creating '//TRIM(this%filename)//' - '//&
                TRIM(NF90_STRERROR( nf90_stat ))
        CALL Cleanup(); RETURN
      END IF
    END IF


    ! Write the global attributes
    IF ( new_file ) THEN
      err_stat = WriteGAtts( &
        fileid  , &
        Release          = this%odps%Release         , &
        Version          = this%odps%Version         , &
        Sensor_Id        = this%odps%Sensor_Id       , &
        Algorithm        = this%odps%Algorithm       , &
        WMO_Sensor_Id    = this%odps%WMO_Sensor_Id   , &
        WMO_Satellite_Id = this%odps%WMO_Satellite_Id, &
        Title            = this%title                , &
        History          = this%history              , &
        Comment          = this%comment              , &
        Profile_Set_Id   = this%profile_set_id         )
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing global attribute to '//TRIM(this%filename)
        CALL Cleanup(); RETURN
      END IF
    END IF


    ! Define the dimensions for the group
    err_stat = DefineDimensions( &
      this%odps         , &
      fileid            , &
      n_layers_dimid    , &
      n_levels_dimid    , &
      n_components_dimid, &
      n_absorbers_dimid , &
      n_channels_dimid  , &
      n_coeffs_dimid    , &
      n_opindex_dimid   , &
      n_ocoeffs_dimid   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining dimensions for '//TRIM(this%filename)
      CALL Cleanup(); RETURN
    END IF


    ! Define the variables for the group
    err_stat = DefineVariables( &
      fileid, &
      n_layers_dimid    , &
      n_levels_dimid    , &
      n_components_dimid, &
      n_absorbers_dimid , &
      n_channels_dimid  , &
      n_coeffs_dimid    , &
      n_opindex_dimid   , &
      n_ocoeffs_dimid   )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error defining variables for '//TRIM(this%filename)
      CALL Cleanup(); RETURN
    END IF


    ! Take netCDF file out of define mode
    nf90_stat = NF90_ENDDEF( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error taking '//TRIM(this%filename)//' out of define mode - '//&
            TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF


    ! Write the variables for the group
    err_stat = WriteVariables( this%odps, fileid )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing variables for '//TRIM(this%filename)
      CALL Cleanup(); RETURN
    END IF


    ! Close the file
    nf90_stat = NF90_CLOSE( fileid )
    IF ( nf90_stat /= NF90_NOERR ) THEN
      msg = 'Error closing output file - '//TRIM(NF90_STRERROR( nf90_stat ))
      CALL Cleanup(); RETURN
    END IF


    ! Output an info message
     IF ( noisy ) THEN
       CALL Display_Message( ROUTINE_NAME, &
                             'FILE: '//TRIM(this%filename)//'; '//this%odps%Info(), &
                             INFORMATION )
     END IF

  CONTAINS

    SUBROUTINE Cleanup()
      nf90_stat = NF90_CLOSE( fileid )
      err_stat = FAILURE
      CALL Display_Message( ROUTINE_NAME,msg,err_stat )
    END SUBROUTINE Cleanup

  END FUNCTION WriteFile

  INCLUDE 'oODPS_IO.inc'
  
END MODULE oODPS_IO
