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
  USE Type_Kinds     , ONLY: fp
  USE Message_Handler, ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE PtrArr_Define  , ONLY: PtrArr_type     , &
                             Allocated_PtrArr, &
                             Destroy_PtrArr  , &
                             Create_PtrArr   , &
                             Assign_PtrArr   , &
                             Equal_PtrArr
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Derived type definitions
  PUBLIC :: oSRF_type
  ! Public procedures
  PUBLIC :: Allocated_oSRF
  PUBLIC :: Destroy_oSRF
  PUBLIC :: Create_oSRF
!  PUBLIC :: Assign_oSRF
!  PUBLIC :: Equal_oSRF
!  PUBLIC :: CheckRelease_oSRF
!  PUBLIC :: Info_oSRF
  ! Public parameters
  PUBLIC :: INVALID_WMO_SATELLITE_ID
  PUBLIC :: INVALID_WMO_SENSOR_ID
  PUBLIC :: N_SENSOR_TYPES
  PUBLIC :: INVALID_SENSOR  
  PUBLIC :: MICROWAVE_SENSOR
  PUBLIC :: INFRARED_SENSOR 
  PUBLIC :: VISIBLE_SENSOR  
  PUBLIC :: ULTRAVIOLET_SENSOR  
  PUBLIC :: SENSOR_TYPE_NAME


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Destroy_oSRF
    MODULE PROCEDURE Destroy_scalar
    MODULE PROCEDURE Destroy_rank1
  END INTERFACE Destroy_oSRF


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id:$'
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! msg length
  INTEGER,  PARAMETER :: SL = 20  ! Sensor Id length
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Current valid release and version numbers
  INTEGER, PARAMETER :: OSRF_RELEASE = 2  ! This determines structure and file formats.
  INTEGER, PARAMETER :: OSRF_VERSION = 1  ! This is just the data version.
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10
  ! Invalid oSRF values
  INTEGER,  PARAMETER :: INVALID = -1
  ! Invalid WMO sensor ids
  INTEGER,  PARAMETER :: INVALID_WMO_SATELLITE_ID = 1023
  INTEGER,  PARAMETER :: INVALID_WMO_SENSOR_ID    = 2047
  ! The instrument types
  INTEGER, PARAMETER :: N_SENSOR_TYPES     = 4
  INTEGER, PARAMETER :: INVALID_SENSOR     = 0
  INTEGER, PARAMETER :: MICROWAVE_SENSOR   = 1
  INTEGER, PARAMETER :: INFRARED_SENSOR    = 2
  INTEGER, PARAMETER :: VISIBLE_SENSOR     = 3
  INTEGER, PARAMETER :: ULTRAVIOLET_SENSOR = 4
  CHARACTER(*), PARAMETER :: SENSOR_TYPE_NAME(0:N_SENSOR_TYPES) = (/ 'Invalid    ', &
                                                                     'Microwave  ', &
                                                                     'Infrared   ', &
                                                                     'Visible    ', &
                                                                     'Ultraviolet' /)
  ! Some internal dimensions
  INTEGER, PARAMETER :: N_PLANCK_COEFFS = 2
  INTEGER, PARAMETER :: N_POLYCHROMATIC_COEFFS = 2

  ! --------------------------
  ! oSRF data type definitions
  ! --------------------------
  TYPE :: oSRF_type
    ! Release and version information
    INTEGER :: Release = oSRF_RELEASE
    INTEGER :: Version = oSRF_VERSION
    ! Dimension values
    INTEGER :: n_Bands = 0  ! nB
    ! Non-pointer components
    CHARACTER(SL) :: Sensor_ID  = ' '
    INTEGER  :: WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    INTEGER  :: WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    INTEGER  :: Sensor_Type      = INVALID_SENSOR
    INTEGER  :: Channel          = INVALID
    REAL(fp) :: Integral        = ZERO
    INTEGER  :: Flags           = 0
    REAL(fp) :: f0              = ZERO
    REAL(fp) :: Planck_Coeffs(N_PLANCK_COEFFS)               = ZERO
    REAL(fp) :: Polychromatic_Coeffs(N_POLYCHROMATIC_COEFFS) = ZERO
    ! Pointer components
    REAL(fp),          ALLOCATABLE :: f1(:)        ! nB
    REAL(fp),          ALLOCATABLE :: f2(:)        ! nB
    INTEGER,           ALLOCATABLE :: n_Points(:)  ! nB
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

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Allocated_oSRF
!
! PURPOSE:
!       Elemental function to test if the allocatable members of an oSRF
!       structure are allocated.
!
! CALLING SEQUENCE:
!       Allocation_Status = Allocated_oSRF( oSRF )
!
! INPUT ARGUMENTS:
!       oSRF:                oSRF structure which is to have its allocatable
!                            member's status tested.
!                            UNITS:      N/A
!                            TYPE:       TYPE(oSRF_type)
!                            DIMENSION:  Scalar or any rank array
!                            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Allocation_Status:   The return value is a logical value indicating the
!                            allocation status of the requisite members.
!                            .TRUE.  - if the structure allocatable members are
!                                      allocated.
!                            .FALSE. - some or all of the oSRF allocatable
!                                      members are NOT allocated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Allocated_oSRF( oSRF ) RESULT( alloc_status )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    ! Function result
    LOGICAL :: alloc_status
    ! Set up
    alloc_status = .FALSE.


    ! Test the members
    alloc_status = .FALSE.
    IF ( ALLOCATED( oSRF%f1        ) .AND. &
         ALLOCATED( oSRF%f2        ) .AND. &
         ALLOCATED( oSRF%n_Points  ) .AND. &
         ALLOCATED( oSRF%Frequency ) .AND. &
         ALLOCATED( oSRF%Response  )       ) THEN
      IF ( ALL(Allocated_PtrArr(oSRF%Frequency)) .AND. &
           ALL(Allocated_PtrArr(oSRF%Response ))       ) THEN
        alloc_status = .TRUE.
      END IF
    END IF

  END FUNCTION Allocated_oSRF



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_oSRF
! 
! PURPOSE:
!       Function to re-initialize the members of oSRF data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_oSRF( oSRF )
!
! OUTPUT ARGUMENTS:
!       oSRF:         Re-initialised oSRF structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar or Rank-1 array
!                     ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure re-initialisation was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Destroy_scalar( &
    oSRF ) &  ! Output
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: oSRF
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    oSRF%n_Bands = 0
  END FUNCTION Destroy_scalar


  FUNCTION Destroy_rank1( &
    oSRF ) &  ! Output
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: oSRF(:)
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    oSRF%n_Bands = 0
  END FUNCTION Destroy_rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Create_oSRF
! 
! PURPOSE:
!       Function to create an oSRF structure by allocating its array members.
!
! CALLING SEQUENCE:
!       Error_Status = Create_oSRF( n_Points, oSRF )
!
! INPUT ARGUMENTS:
!       n_Points:     The array of the number of data points to which the
!                     oSRF band data arrays are to be allocated. The number 
!                     of oSRF bands is taken from the size of the n_Points
!                     array. Each element must be > 0.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Rank-1
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       oSRF:         oSRF structure with allocated members
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure allocation was successful
!                        == FAILURE an error occurre.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Create_oSRF( &
    n_Points   , &  ! Input
    oSRF       ) &  ! Output
  RESULT( err_status )
    ! Arguments
    INTEGER,                INTENT(IN)  :: n_Points(:)
    TYPE(oSRF_type),        INTENT(OUT) :: oSRF
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_oSRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status
    INTEGER :: i, n_Bands

    ! Set up
    err_status = SUCCESS
    ! ...Check dimension inputs
    IF ( ANY(n_Points < 1) ) THEN
      msg = 'Input N_POINTS must be > 0.'
      CALL Create_CleanUp(); RETURN
    END IF
    
    
    ! Set the number of oSRF bands
    n_Bands = SIZE(n_Points)


    ! Perform the allocations
    ! ...The main arrays
    ALLOCATE(  oSRF%f1( n_Bands )       , &
               oSRF%f2( n_Bands )       , &
               oSRF%n_Points( n_Bands ) , &
               oSRF%Frequency( n_Bands ), &
               oSRF%Response( n_Bands ) , &
               STAT = alloc_status        )
    IF ( alloc_status /= 0 ) THEN
      WRITE( msg,'("Error allocating oSRF data arrays. STAT = ",i0)' ) alloc_status
      CALL Create_CleanUp(); RETURN
    END IF
    ! ...The individual band elements
    DO i = 1, n_Bands
      ! Frequency arrays
      alloc_status = Create_PtrArr( n_Points(i), oSRF%Frequency(i) )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg,'("Error allocating Frequency band ",i0," component. STAT = ",i0)' ) i, alloc_status
        CALL Create_CleanUp(); RETURN
      END IF
      ! Response array
      alloc_status = Create_PtrArr( n_Points(i), oSRF%Response(i) )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg,'("Error allocating Response band ",i0," component. STAT = ",i0)' ) i, alloc_status
        CALL Create_CleanUp(); RETURN
      END IF
      ! Assign the n_Points value
      oSRF%n_Points(i) = n_Points(i)
    END DO


    ! Assign the band dimension and initialise arrays
    oSRF%n_Bands = n_Bands
    oSRF%f1 = ZERO
    oSRF%f2 = ZERO
 
  CONTAINS
  
    SUBROUTINE Create_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION Create_oSRF


!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Assign_oSRF
!!
!! PURPOSE:
!!       Function to copy valid oSRF structures.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Assign_oSRF( oSRF_in                 , &  ! Input
!!                                  oSRF_out                , &  ! Output
!!                                  RCS_Id     =RCS_Id     , &  ! Revision control
!!                                  Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       oSRF_in:       oSRF structure which is to be copied.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN)
!!
!! OUTPUT ARGUMENTS:
!!       oSRF_out:      Copy of the input structure, oSRF_in.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:  Character string specifying a filename in which any
!!                     messages will be logged. If not specified, or if an
!!                     error occurs opening the log file, the default action
!!                     is to output messages to standard output.
!!                     UNITS:      N/A
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:       Character string containing the Revision Control
!!                     System Id field for the module.
!!                     UNITS:      None
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status: The return value is an integer defining the error status.
!!                     The error codes are defined in the Message_Handler module.
!!                     If == SUCCESS the structure assignment was successful
!!                        == FAILURE an error occurred
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!
!! COMMENTS:
!!       Note the INTENT on the output oSRF argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Assign_oSRF( oSRF_in     , &  ! Input
!                       oSRF_out    , &  ! Output
!                       RCS_Id     , &  ! Revision control
!                       Message_Log) &  ! Error messaging
!                     RESULT( Error_Status )
!    ! Arguments
!    TYPE(oSRF_type),         INTENT(IN)     :: oSRF_in
!    TYPE(oSRF_type),         INTENT(IN OUT) :: oSRF_out
!    CHARACTER(*), OPTIONAL, INTENT(OUT)    :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_oSRF'
!    ! Local variables
!
!    ! Setup
!    ! -----
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!
!    ! ALL *input* pointers must be associated
!    IF ( .NOT. Associated_oSRF( oSRF_In ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT oSRF pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!
!    ! Allocate the structure
!    ! ----------------------
!    Error_Status = Allocate_oSRF( oSRF_In%n_Points, &
!                                 oSRF_Out, &
!                                 n_Bands=oSRF_In%n_Bands, &
!                                 Message_Log=Message_Log )
!    IF ( Error_Status /= SUCCESS ) THEN
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Error allocating output oSRF arrays.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!
!    ! Assign intrinsic data types
!    ! ---------------------------
!    oSRF_out%Release = oSRF_in%Release
!    oSRF_out%Version = oSRF_in%Version
!    
!    oSRF_out%Sensor_ID        = oSRF_in%Sensor_ID  
!    oSRF_out%WMO_Satellite_Id = oSRF_in%WMO_Satellite_Id
!    oSRF_out%WMO_Sensor_Id    = oSRF_in%WMO_Sensor_Id
!    oSRF_out%Sensor_Type      = oSRF_in%Sensor_Type   
!    oSRF_out%Channel          = oSRF_in%Channel
!    oSRF_out%Integrated_oSRF   = oSRF_in%Integrated_oSRF
!    oSRF_out%Summation_oSRF    = oSRF_in%Summation_oSRF
!    oSRF_out%f1_Band          = oSRF_in%f1_Band 
!    oSRF_out%f2_Band          = oSRF_in%f2_Band 
!    oSRF_out%npts_Band        = oSRF_in%npts_Band 
!    oSRF_out%Frequency        = oSRF_in%Frequency
!    oSRF_out%Response         = oSRF_in%Response
!
!  END FUNCTION Assign_oSRF
!
!
!!--------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Equal_oSRF
!!
!! PURPOSE:
!!       Function to test if two oSRF structures are equal.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Equal_oSRF( oSRF_LHS                , &  ! Input
!!                                 oSRF_RHS                , &  ! Input
!!                                 ULP_Scale  =ULP_Scale  , &  ! Optional input
!!                                 Check_All  =Check_All  , &  ! Optional input
!!                                 RCS_Id     =RCS_Id     , &  ! Revision control
!!                                 Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       oSRF_LHS:       oSRF structure to be compared; equivalent to the
!!                      left-hand side of a lexical comparison, e.g.
!!                        IF ( oSRF_LHS == oSRF_RHS ).
!!                      UNITS:      N/A
!!                      TYPE:       TYPE(oSRF_type)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN)
!!
!!       oSRF_RHS:       oSRF structure to be compared to; equivalent to
!!                      right-hand side of a lexical comparison, e.g.
!!                        IF ( oSRF_LHS == oSRF_RHS ).
!!                      UNITS:      N/A
!!                      TYPE:       Same as oSRF_LHS
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       ULP_Scale:     Unit of data precision used to scale the floating
!!                      point comparison. ULP stands for "Unit in the Last Place,"
!!                      the smallest possible increment or decrement that can be
!!                      made using a machine's floating point arithmetic.
!!                      Value must be positive - if a negative value is supplied,
!!                      the absolute value is used. If not specified, the default
!!                      value is 1.
!!                      UNITS:      N/A
!!                      TYPE:       INTEGER
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Check_All:     Set this argument to check ALL the floating point
!!                      channel data of the oSRF structures. The default
!!                      action is return with a FAILURE status as soon as
!!                      any difference is found. This optional argument can
!!                      be used to get a listing of ALL the differences
!!                      between data in oSRF structures.
!!                      If == 0, Return with FAILURE status as soon as
!!                               ANY difference is found  *DEFAULT*
!!                         == 1, Set FAILURE status if ANY difference is
!!                               found, but continue to check ALL data.
!!                      UNITS:      N/A
!!                      TYPE:       INTEGER
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!!       Message_Log:   Character string specifying a filename in which any
!!                      messages will be logged. If not specified, or if an
!!                      error occurs opening the log file, the default action
!!                      is to output messages to standard output.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:        Character string containing the Revision Control
!!                      System Id field for the module.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:  The return value is an integer defining the error status.
!!                      The error codes are defined in the Message_Handler module.
!!                      If == SUCCESS the structures were equal
!!                         == FAILURE - an error occurred, or
!!                                    - the structures were different.
!!                      UNITS:      N/A
!!                      TYPE:       INTEGER
!!                      DIMENSION:  Scalar
!!
!!:sdoc-:
!!--------------------------------------------------------------------------------
!
!  FUNCTION Equal_oSRF( oSRF_LHS, &  ! Input
!                      oSRF_RHS, &  ! Input
!                      ULP_Scale   , &  ! Optional input
!                      Check_All   , &  ! Optional input
!                      RCS_Id      , &  ! Revision control
!                      Message_Log ) &  ! Error messaging
!                    RESULT( Error_Status )
!    ! Arguments
!    TYPE(oSRF_type)        , INTENT(IN)  :: oSRF_LHS
!    TYPE(oSRF_type)        , INTENT(IN)  :: oSRF_RHS
!    INTEGER     , OPTIONAL, INTENT(IN)  :: ULP_Scale
!    INTEGER     , OPTIONAL, INTENT(IN)  :: Check_All
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_oSRF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    INTEGER :: ULP
!    LOGICAL :: Check_Once
!    INTEGER :: n
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!
!    ! Default precision is a single unit in last place
!    ULP = 1
!    ! ... unless the ULP_Scale argument is set and positive
!    IF ( PRESENT( ULP_Scale ) ) THEN
!      IF ( ULP_Scale > 0 ) ULP = ULP_Scale
!    END IF
!
!    ! Default action is to return on ANY difference...
!    Check_Once = .TRUE.
!    ! ...unless the Check_All argument is set
!    IF ( PRESENT( Check_All ) ) THEN
!      IF ( Check_All == SET ) Check_Once = .FALSE.
!    END IF
!
!    ! Check the structure association status
!    IF ( .NOT. Associated_oSRF( oSRF_LHS ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Some or all INPUT oSRF_LHS pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!    IF ( .NOT. Associated_oSRF( oSRF_RHS ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME,    &
!                            'Some or all INPUT oSRF_RHS pointer '//&
!                            'members are NOT associated.', &
!                            Error_Status,    &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check Release/Version info
!    ! --------------------------
!    IF ( ( oSRF_LHS%Release /= oSRF_RHS%Release ) .OR. &
!         ( oSRF_LHS%Version /= oSRF_RHS%Version )      ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Release/Version numbers are different : ",&
!                 &i2,".",i2.2," vs. ",i2,".",i2.2)' ) &
!                      oSRF_LHS%Release, oSRF_LHS%Version, &
!                      oSRF_RHS%Release, oSRF_RHS%Version
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!
!    ! Check dimensions
!    ! ----------------
!    IF ( oSRF_LHS%n_Points /= oSRF_RHS%n_Points ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("n_Points dimensions are different : ",i0," vs. ",i0)' ) &
!                 oSRF_LHS%n_Points, oSRF_RHS%n_Points
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      RETURN
!    END IF
!    IF ( oSRF_LHS%n_Bands /= oSRF_RHS%n_Bands ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("n_Bands dimensions are different : ",i0," vs. ",i0)' ) &
!                 oSRF_LHS%n_Bands, oSRF_RHS%n_Bands
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      RETURN
!    END IF
!
!
!    ! Compare the values
!    ! ------------------
!    ! The Sensor_ID
!    IF ( oSRF_LHS%Sensor_Id /= oSRF_RHS%Sensor_Id ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Sensor_ID values are different, ",a," vs. ",a)' ) &
!                 TRIM( oSRF_LHS%Sensor_Id), TRIM( oSRF_RHS%Sensor_Id)
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The WMO Satellite ID
!    IF ( oSRF_LHS%WMO_Satellite_ID /= oSRF_RHS%WMO_Satellite_ID ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("WMO_Satellite_ID values are different, ",i0," vs. ",i0 )' ) &
!                 oSRF_LHS%WMO_Satellite_ID, oSRF_RHS%WMO_Satellite_ID
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The WMO Sensor ID
!    IF ( oSRF_LHS%WMO_Sensor_ID /= oSRF_RHS%WMO_Sensor_ID ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("WMO_Sensor_ID values are different, ",i0," vs. ",i0 )' ) &
!                 oSRF_LHS%WMO_Sensor_ID, oSRF_RHS%WMO_Sensor_ID
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The Sensor_Type
!    IF ( oSRF_LHS%Sensor_Type /= oSRF_RHS%Sensor_Type ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Sensor types are different, ",i0,"(",a,") vs. ",i0,"(",a,")")' ) &
!                 oSRF_LHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(oSRF_LHS%Sensor_Type)), &
!                 oSRF_RHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(oSRF_RHS%Sensor_Type))
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The Channel
!    IF ( oSRF_LHS%Channel /= oSRF_RHS%Channel ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Channel values are different, ",i0," vs. ",i0 )' ) &
!                 oSRF_LHS%Channel, oSRF_RHS%Channel
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The Integrated_oSRF
!    IF ( .NOT. Compare_Float( oSRF_LHS%Integrated_oSRF,oSRF_RHS%Integrated_oSRF,ULP=ULP ) ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Integrated_oSRF values are different, ",es13.6," vs. ",es13.6)' ) &
!                 oSRF_LHS%Integrated_oSRF,oSRF_RHS%Integrated_oSRF
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The Summation_oSRF
!    IF ( .NOT. Compare_Float( oSRF_LHS%Summation_oSRF,oSRF_RHS%Summation_oSRF,ULP=ULP ) ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("Summation_oSRF values are different, ",es13.6," vs. ",es13.6)' ) &
!                 oSRF_LHS%Summation_oSRF,oSRF_RHS%Summation_oSRF
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      IF ( Check_Once ) RETURN
!    END IF
!
!    ! The f1_Band values
!    DO n = 1, oSRF_RHS%n_Bands
!      IF ( .NOT. Compare_Float( oSRF_LHS%f1_Band(n),oSRF_RHS%f1_Band(n),ULP=ULP ) ) THEN
!        Error_Status = FAILURE
!        WRITE( msg,'("f1_Band ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
!                   n, oSRF_LHS%f1_Band(n),oSRF_RHS%f1_Band(n)
!        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO
!
!    ! The f2_Band values
!    DO n = 1, oSRF_RHS%n_Bands
!      IF ( .NOT. Compare_Float( oSRF_LHS%f2_Band(n),oSRF_RHS%f2_Band(n),ULP=ULP ) ) THEN
!        Error_Status = FAILURE
!        WRITE( msg,'("f2_Band ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
!                   n, oSRF_LHS%f2_Band(n),oSRF_RHS%f2_Band(n)
!        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO
!
!    ! The npts_Band values
!    DO n = 1, oSRF_RHS%n_Bands
!      IF ( oSRF_LHS%npts_Band(n) /= oSRF_RHS%npts_Band(n) ) THEN
!        Error_Status = FAILURE
!        WRITE( msg,'("npts_Band ",i0," values are different, ",i0," vs. ",i0)' ) &
!                   n, oSRF_LHS%npts_Band(n),oSRF_RHS%npts_Band(n)
!        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO
!
!    ! The Frequency values
!    DO n = 1, oSRF_RHS%n_Points
!      IF ( .NOT. Compare_Float( oSRF_LHS%Frequency(n),oSRF_RHS%Frequency(n),ULP=ULP ) ) THEN
!        Error_Status = FAILURE
!        WRITE( msg,'("Frequency values are different, ",es13.6," vs. ",es13.6," at point ",i0)' ) &
!                   oSRF_LHS%Frequency(n),oSRF_RHS%Frequency(n), n
!        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!        IF ( Check_Once ) RETURN
!      END IF
!    END DO
!  
!  END FUNCTION Equal_oSRF
!
!
!!----------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       CheckRelease_oSRF
!!
!! PURPOSE:
!!       Function to check the oSRF Release value.
!!
!! CALLING SEQUENCE:
!!       Error_Status = CheckRelease_oSRF( oSRF                    , &  ! Input
!!                                        RCS_Id     =RCS_Id     , &  ! Revision control
!!                                        Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       oSRF:           oSRF structure for which the Release member
!!                      is to be checked.
!!                      UNITS:      N/A
!!                      TYPE:       TYPE(oSRF_type)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:   Character string specifying a filename in which any
!!                      messages will be logged. If not specified, or if an
!!                      error occurs opening the log file, the default action
!!                      is to output messages to standard output.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:        Character string containing the Revision Control
!!                      System Id field for the module.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status:  The return value is an integer defining the error status.
!!                      The error codes are defined in the Message_Handler module.
!!                      If == SUCCESS the structure Release value is valid.
!!                         == FAILURE the structure Release value is NOT valid
!!                                    and either a data file file or software
!!                                    update is required.
!!                      UNITS:      N/A
!!                      TYPE:       INTEGER
!!                      DIMENSION:  Scalar
!!
!!:sdoc-:
!!----------------------------------------------------------------------------------
!
!  FUNCTION CheckRelease_oSRF( oSRF        , &  ! Input
!                             RCS_Id     , &  ! Revision control
!                             Message_Log) &  ! Error messaging
!                           RESULT( Error_Status )
!    ! Arguments
!    TYPE(oSRF_type)        , INTENT(IN)  :: oSRF
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    CHARACTER(*), OPTIONAL, INTENT(IN)  :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'CheckRelease_oSRF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!
!    ! Set up
!    ! ------
!    Error_Status = SUCCESS
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!
!
!    ! Check the release
!    ! -----------------
!    ! Check that release is not too old
!    IF ( oSRF%Release < oSRF_RELEASE ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("An oSRF data update is needed. oSRF release is ",i2,". Valid release is ",i2)' ) &
!                 oSRF%Release, oSRF_RELEASE
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check that release is not too new
!    IF ( oSRF%Release > oSRF_RELEASE ) THEN
!      Error_Status = FAILURE
!      WRITE( msg,'("An oSRF software update is needed. oSRF release is ",i2,". Valid release is ",i2)' ) &
!                 oSRF%Release, oSRF_RELEASE
!      CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!      RETURN
!    END IF
!
!  END FUNCTION CheckRelease_oSRF
!
!
!!--------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Info_oSRF
!!
!! PURPOSE:
!!       Subroutine to return a string containing version and dimension
!!       information about the oSRF data structure.
!!
!! CALLING SEQUENCE:
!!       CALL Info_oSRF( oSRF          , &  ! Input
!!                      Info         , &  ! Output
!!                      RCS_Id=RCS_Id  )  ! Revision control
!!
!! INPUT ARGUMENTS:
!!       oSRF:           oSRF structure.
!!                      UNITS:      N/A
!!                      TYPE:       TYPE(oSRF_type)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(IN)
!!
!! OUTPUT ARGUMENTS:
!!       Info:          String containing version and dimension information
!!                      about the passed oSRF data structure.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(OUT)
!!
!! OPTIONAL OUTPUT ARGUMENTS:
!!       RCS_Id:        Character string containing the Revision Control
!!                      System Id field for the module.
!!                      UNITS:      N/A
!!                      TYPE:       CHARACTER(*)
!!                      DIMENSION:  Scalar
!!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!!
!!:sdoc-:
!!--------------------------------------------------------------------------------
!
!  SUBROUTINE Info_oSRF( oSRF   , &  ! Input
!                       Info  , &  ! Output
!                       RCS_Id  )  ! Revision control
!    ! Arguments
!    TYPE(oSRF_type)        , INTENT(IN)  :: oSRF
!    CHARACTER(*)          , INTENT(OUT) :: Info
!    CHARACTER(*), OPTIONAL, INTENT(OUT) :: RCS_Id
!    ! Local variables
!    CHARACTER(2000) :: LongString
!
!    ! Set up
!    ! ------
!    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID
!
!    ! Write the required data to the local string
!    ! -------------------------------------------
!    WRITE( LongString,'(a,1x,"oSRF RELEASE.VERSION: ",i0,".",i2.2,2x,&
!                       &a," CHANNEL:",i0,2x,&
!                       &"N_BANDS=",i0,2x,&
!                       &"N_POINTS=",i0)' ) &
!                       ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
!                       oSRF%Release, oSRF%Version, &
!                       TRIM(oSRF%Sensor_ID), oSRF%Channel, &
!                       oSRF%n_Bands, oSRF%n_Points
!
!    ! Trim the output based on the
!    ! dummy argument string length
!    ! ----------------------------
!    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))
!
!  END SUBROUTINE Info_oSRF
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Frequency_oSRF
!!
!! PURPOSE:
!!       Function to compute the frequency grid for a supplied oSRF data
!!       structure.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Frequency_oSRF( oSRF                    , &  ! In/Output
!!                                     Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       oSRF:          oSRF structure with fields containing the begin and
!!                     end frequencies of the frequency grid to compute.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OUTPUT ARGUMENTS:
!!       oSRF:          oSRF structure with the frequency component filled.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:  Character string specifying a filename in which any
!!                     messages will be logged. If not specified, or if an
!!                     error occurs opening the log file, the default action
!!                     is to output messages to standard output.
!!                     UNITS:      None
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status: The return value is an integer defining the error status.
!!                     The error codes are defined in the Message_Handler module.
!!                     If == SUCCESS the frequency grid calculation was successful
!!                        == FAILURE an error occurred processing the input
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!
!! SIDE EFFECTS:
!!       The FREQUENCY field of the input oSRF structure is filled.
!!
!! RESTRICTIONS:
!!       oSRF structure must contain at least 2 points of frequency and response
!!       data.
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Frequency_oSRF( oSRF        , &  ! In/Output
!                          Message_Log) &  ! Error messaging
!                        RESULT( Error_Status )
!    ! Arguments
!    TYPE(oSRF_type),         INTENT(IN OUT) :: oSRF
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Frequency_oSRF'
!    ! Local variables
!    INTEGER :: i1, i2, i, m, n
!
!    ! Setup
!    ! -----
!    Error_Status = SUCCESS
!
!    ! ALL pointers must be associated
!    IF ( .NOT. Associated_oSRF( oSRF ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Some or all INPUT oSRF pointer members are NOT associated.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of bands
!    IF ( oSRF%n_Bands < 1 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF structure must contain at least 1 band', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of points
!    IF ( oSRF%n_Points < 2 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF structure must contain at least 2 points.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of points in each band
!    IF ( ANY(oSRF%npts_Band < 2) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF must contain at least 2 points for each band.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the total points                                                   
!    IF ( SUM(oSRF%npts_Band) /= oSRF%n_Points ) THEN                            
!      Error_Status = FAILURE                                             
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF must have consistent data points.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END IF                                                               
!
!
!    ! Compute the oSRF frequency grid
!    ! ------------------------------
!    ! Initialise the offset counter
!    n = 0
!    ! Loop over the number of bands
!    DO m = 1, oSRF%n_Bands
!      ! The point limits for this band
!      i1 = n + 1
!      i2 = oSRF%npts_Band(m) + n
!      ! Construct a frequency grid of 0->1 for this band
!      oSRF%Frequency(i1:i2) = (/(REAL(i-1,fp),i=1,oSRF%npts_Band(m))/) / REAL(oSRF%npts_Band(m)-1,fp)
!      oSRF%Frequency(i1:i2) = oSRF%f1_Band(m) + &
!                             ( oSRF%Frequency(i1:i2) * (oSRF%f2_Band(m)-oSRF%f1_Band(m)) )
!      ! Update the offset counter
!      n = n + oSRF%npts_Band(m)
!    END DO
!
!  END FUNCTION Frequency_oSRF
!
!
!!------------------------------------------------------------------------------
!!:sdoc+:
!!
!! NAME:
!!       Integrate_oSRF
!!
!! PURPOSE:
!!       Function to integrate the response supplied in an oSRF data
!!       structure.
!!
!! CALLING SEQUENCE:
!!       Error_Status = Integrate_oSRF( oSRF                    , &  ! In/Output
!!                                     Message_Log=Message_Log  )  ! Error messaging
!!
!! INPUT ARGUMENTS:
!!       oSRF:          oSRF structure with fields containing the frequency
!!                     and response arrays.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OUTPUT ARGUMENTS:
!!       oSRF:          oSRF structure with the integration components filled.
!!                     UNITS:      N/A
!!                     TYPE:       TYPE(oSRF_type)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN OUT)
!!
!! OPTIONAL INPUT ARGUMENTS:
!!       Message_Log:  Character string specifying a filename in which any
!!                     messages will be logged. If not specified, or if an
!!                     error occurs opening the log file, the default action
!!                     is to output messages to standard output.
!!                     UNITS:      None
!!                     TYPE:       CHARACTER(*)
!!                     DIMENSION:  Scalar
!!                     ATTRIBUTES: INTENT(IN), OPTIONAL
!!
!! FUNCTION RESULT:
!!       Error_Status: The return value is an integer defining the error status.
!!                     The error codes are defined in the Message_Handler module.
!!                     If == SUCCESS the integration was successful
!!                        == FAILURE an error occurred processing the input
!!                     UNITS:      N/A
!!                     TYPE:       INTEGER
!!                     DIMENSION:  Scalar
!!
!! SIDE EFFECTS:
!!       The INTEGRATED_oSRF and SUMMATION_oSRF fields of the input oSRF structure
!!       are filled.
!!
!! RESTRICTIONS:
!!       oSRF structure must contain at least 2 points of frequency and response
!!       data.
!!
!!:sdoc-:
!!------------------------------------------------------------------------------
!
!  FUNCTION Integrate_oSRF( oSRF        , &  ! In/Output
!                          Message_Log) &  ! Error messaging
!                        RESULT( Error_Status )
!    ! Arguments
!    TYPE(oSRF_type),         INTENT(IN OUT) :: oSRF
!    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Message_Log
!    ! Function result
!    INTEGER :: Error_Status
!    ! Local parameters
!    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Integrate_oSRF'
!    ! Local variables
!    CHARACTER(ML) :: msg
!    REAL(fp) :: dF
!    INTEGER :: i1, i2, m, n
!    REAL(fp):: Int_oSRF, Sum_oSRF
!
!    ! Setup
!    ! -----
!    Error_Status = SUCCESS
!    
!    ! ALL pointers must be associated
!    IF ( .NOT. Associated_oSRF( oSRF ) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'Some or all INPUT oSRF pointer members are NOT associated.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of bands
!    IF ( oSRF%n_Bands < 1 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF structure must contain at least 1 band', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of points
!    IF ( oSRF%n_Points < 2 ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF structure must contain at least 2 points.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the number of points in each band
!    IF ( ANY(oSRF%npts_Band < 2) ) THEN
!      Error_Status = FAILURE
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF must contain at least 2 points for each band.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!      RETURN
!    END IF
!
!    ! Check the total points                                                   
!    IF ( SUM(oSRF%npts_Band) /= oSRF%n_Points ) THEN                            
!      Error_Status = FAILURE                                             
!      CALL Display_Message( ROUTINE_NAME, &
!                            'oSRF must have consistent data points.', &
!                            Error_Status, &
!                            Message_Log=Message_Log )
!    END IF
!
!
!    ! Check the number of points
!    n = oSRF%n_Points
!    m = oSRF%n_Bands
!
!
!    ! Compute the oSRF integrals
!    ! -------------------------
!    ! Initialisation of sums
!    oSRF%Integrated_oSRF = ZERO
!    oSRF%Summation_oSRF  = ZERO
!    ! Initialise the offset counter
!    n = 0
!
!    ! Loop over the bands
!    DO m = 1, oSRF%n_Bands
!    
!      ! The point limits for this band
!      i1 = n + 1
!      i2 = oSRF%npts_Band(m) + n
!      
!      ! Integration using Simpson's rule
!      ! --------------------------------                                            
!      Error_Status = Simpsons_Integral( oSRF%Frequency(i1:i2), &
!                                        oSRF%Response(i1:i2), &
!                                        Int_oSRF )
!      IF ( Error_Status /= SUCCESS ) THEN
!        WRITE( msg,'("Error occurred integrating channel ",i0," oSRF")' ) oSRF%Channel
!        CALL Display_Message( ROUTINE_NAME,TRIM(msg),Error_Status,Message_Log=Message_Log )
!        RETURN
!      END IF
!      
!      ! Integration by simple summation
!      ! -------------------------------
!      ! Compute the average frequency grid interval
!      dF = SUM(oSRF%Frequency(i1+1:i2 ) - oSRF%Frequency(i1:i2-1)) / REAL(oSRF%npts_Band(m)-1,fp)
!      ! Do the summation
!      Sum_oSRF = SUM(oSRF%Response(i1:i2)) * dF
!      
!      ! Accumulate the band sums                                                                        
!      oSRF%Integrated_oSRF = oSRF%Integrated_oSRF + Int_oSRF
!      oSRF%Summation_oSRF  = oSRF%Summation_oSRF  + Sum_oSRF
!
!      ! Update the offset counter
!      n = n + oSRF%npts_Band(m)
!    END DO
!
!  END FUNCTION Integrate_oSRF
!
!
!!##################################################################################
!!##################################################################################
!!##                                                                              ##
!!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!!##                                                                              ##
!!##################################################################################
!!##################################################################################
!
!!------------------------------------------------------------------------------------
!!
!! NAME:
!!       Clear_oSRF
!!
!! PURPOSE:
!!       Subroutine to clear the scalar members of an oSRF structure.
!!
!! CALLING SEQUENCE:
!!       CALL Clear_oSRF( oSRF )
!!
!! OUTPUT ARGUMENTS:
!!       oSRF:         oSRF structure for which the scalar members have
!!                    been cleared.
!!                    UNITS:      N/A
!!                    TYPE:       TYPE(oSRF_type)
!!                    DIMENSION:  Scalar
!!                    ATTRIBUTES: INTENT(IN OUT)
!!
!! COMMENTS:
!!       Note the INTENT on the output oSRF argument is IN OUT rather than
!!       just OUT. This is necessary because the argument may be defined upon
!!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!!
!!------------------------------------------------------------------------------------
!
!  SUBROUTINE Clear_oSRF( oSRF )
!    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
!    
!    oSRF%Release = oSRF_RELEASE
!    oSRF%Version = oSRF_VERSION
!
!    oSRF%Sensor_ID        = ' '
!    oSRF%WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
!    oSRF%WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
!    oSRF%Sensor_Type      = INVALID_SENSOR
!    
!    oSRF%Channel      = INVALID
!
!    oSRF%Integrated_oSRF = ZERO
!    oSRF%Summation_oSRF  = ZERO
! 
!  END SUBROUTINE Clear_oSRF

END MODULE oSRF_Define

 
