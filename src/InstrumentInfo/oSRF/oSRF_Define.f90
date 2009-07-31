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
  USE Compare_Float_Numbers, ONLY: Compare_Float
  USE PtrArr_Define        , ONLY: PtrArr_type     , &
                                   Allocated_PtrArr, &
                                   Destroy_PtrArr  , &
                                   Create_PtrArr   , &
                                   Assign_PtrArr   , &
                                   Equal_PtrArr    , &
                                   Set_PtrArr      , &
                                   Get_PtrArr      , &
                                   Inspect_PtrArr
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
  PUBLIC :: Assign_oSRF
  PUBLIC :: Equal_oSRF
  PUBLIC :: Set_oSRF
  PUBLIC :: Get_oSRF
  PUBLIC :: Inspect_oSRF
  PUBLIC :: Info_oSRF
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
    '$Id$'
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
! OBJECT:
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

  ELEMENTAL FUNCTION Allocated_oSRF(self) RESULT(alloc_status)
    ! Arguments
    TYPE(oSRF_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: alloc_status
    ! Set up
    alloc_status = .FALSE.


    ! Test the members
    alloc_status = .FALSE.
    IF ( ALLOCATED( self%f1        ) .AND. &
         ALLOCATED( self%f2        ) .AND. &
         ALLOCATED( self%n_Points  ) .AND. &
         ALLOCATED( self%Frequency ) .AND. &
         ALLOCATED( self%Response  )       ) THEN
      IF ( ALL(Allocated_PtrArr(self%Frequency)) .AND. &
           ALL(Allocated_PtrArr(self%Response ))       ) THEN
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
! OBJECT:
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

  FUNCTION Destroy_scalar(self) RESULT(err_status)
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: self
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    self%n_Bands = 0
  END FUNCTION Destroy_scalar


  FUNCTION Destroy_rank1(self) RESULT(err_status)
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: self(:)
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    self%n_Bands = 0
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
!       Error_Status = Create_oSRF(oSRF, n_Points)
!
! OBJECT:
!       oSRF:         oSRF structure with allocated members
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
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
    self    , &  ! Input
    n_Points) &  ! Output
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_type), INTENT(OUT) :: self
    INTEGER,         INTENT(IN)  :: n_Points(:)
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


    ! Perform the main array allocations
    ALLOCATE( self%n_Points( n_Bands ) , &
              self%f1( n_Bands )       , &
              self%f2( n_Bands )       , &
              self%Frequency( n_Bands ), &
              self%Response( n_Bands ) , &
              STAT = alloc_status        )
    IF ( alloc_status /= 0 ) THEN
      WRITE( msg,'("Error allocating oSRF data arrays. STAT = ",i0)' ) alloc_status
      CALL Create_CleanUp(); RETURN
    END IF


    ! Allocate the individual band elements
    DO i = 1, n_Bands
      ! Frequency arrays
      alloc_status = Create_PtrArr( self%Frequency(i), n_Points(i) )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg,'("Error allocating Frequency band ",i0," component. STAT = ",i0)' ) i, alloc_status
        CALL Create_CleanUp(); RETURN
      END IF
      ! Response array
      alloc_status = Create_PtrArr( self%Response(i), n_Points(i) )
      IF ( alloc_status /= 0 ) THEN
        WRITE( msg,'("Error allocating Response band ",i0," component. STAT = ",i0)' ) i, alloc_status
        CALL Create_CleanUp(); RETURN
      END IF
      ! Assign the n_Points value
      self%n_Points(i) = n_Points(i)
    END DO


    ! Assign the band dimension and initialise arrays
    self%n_Bands = n_Bands
    self%f1 = ZERO
    self%f2 = ZERO
 
  CONTAINS
  
    SUBROUTINE Create_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION Create_oSRF


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_oSRF
!
! PURPOSE:
!       Function to copy valid oSRF structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_oSRF(oSRF, copy)
!
! OBJECT:
!       oSRF:         oSRF structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       copy:         Copy of the oSRF structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE(oSRF_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the Message_Handler module.
!                     If == SUCCESS the structure assignment was successful
!                        == FAILURE an error occurred
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Assign_oSRF(self, copy) RESULT( err_status )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN)  :: self
    TYPE(oSRF_type), INTENT(OUT) :: copy
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_oSRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i
    
    ! Setup
    err_status = SUCCESS
    ! ...ALL *input* components must be allocated
    IF ( .NOT. Allocated_oSRF( self ) ) THEN
      msg = 'Some or all INPUT oSRF pointer members are NOT allocated.'
      CALL Assign_CleanUp(); RETURN
    END IF


    ! Allocate the structure
    err_status = Create_oSRF( copy, self%n_Points )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error allocating output oSRF arrays.'
      CALL Assign_CleanUp(); RETURN
    END IF


    ! Copy data
    copy%Sensor_ID            = self%Sensor_ID  
    copy%WMO_Satellite_Id     = self%WMO_Satellite_Id    
    copy%WMO_Sensor_Id        = self%WMO_Sensor_Id       
    copy%Sensor_Type          = self%Sensor_Type         
    copy%Channel              = self%Channel             
    copy%Integral             = self%Integral            
    copy%Flags                = self%Flags               
    copy%f0                   = self%f0                  
    copy%Planck_Coeffs        = self%Planck_Coeffs       
    copy%Polychromatic_Coeffs = self%Polychromatic_Coeffs
    copy%n_Points             = self%n_Points
    copy%f1                   = self%f1
    copy%f2                   = self%f2
    DO i = 1, self%n_Bands
      ! Copy the frequency data
      err_status = Assign_PtrArr( self%Frequency(i), copy%Frequency(i) )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error assigning output frequency for band #",i0)' ) i
        CALL Assign_CleanUp(); RETURN
      END IF
      ! Copy the response data
      err_status = Assign_PtrArr( self%Response(i), copy%Response(i) )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error assigning output frequency for band #",i0)' ) i
        CALL Assign_CleanUp(); RETURN
      END IF
    END DO
 
  CONTAINS
  
    SUBROUTINE Assign_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Assign_CleanUp
    
  END FUNCTION Assign_oSRF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_oSRF
!
! PURPOSE:
!       Function to test if two oSRF structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_oSRF( oSRF_LHS             , &  ! Input
!                                  oSRF_RHS             , &  ! Input
!                                  ULP_Scale  =ULP_Scale, &  ! Optional input
!                                  Check_All  =Check_All  )  ! Optional input
!
! INPUT ARGUMENTS:
!       oSRF_LHS:      oSRF structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( oSRF_LHS == oSRF_RHS ).
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       oSRF_RHS:      oSRF structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( oSRF_LHS == oSRF_RHS ).
!                      UNITS:      N/A
!                      TYPE:       Same as oSRF_LHS
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       ULP_Scale:     Unit of data precision used to scale the floating
!                      point comparison. ULP stands for "Unit in the Last Place,"
!                      the smallest possible increment or decrement that can be
!                      made using a machine's floating point arithmetic.
!                      Value must be positive - if a negative value is supplied,
!                      the absolute value is used. If not specified, the default
!                      value is 1.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Check_All:     Set this argument to check ALL the floating point
!                      channel data of the oSRF structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in oSRF structures.
!                      If .FALSE., Return with FAILURE status as soon as
!                                  ANY difference is found  [*DEFAULT*]
!                         .TRUE.,  Set FAILURE status if ANY difference is
!                                  found, but continue to check ALL data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the structures were equal
!                         == FAILURE - an error occurred, or
!                                    - the structures were different.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Equal_oSRF( &
    oSRF_LHS , &
    oSRF_RHS , &
    ULP_Scale, &
    Check_All) &
  RESULT( err_status )
    ! Arguments
    TYPE(oSRF_type)  , INTENT(IN)  :: oSRF_LHS
    TYPE(oSRF_type)  , INTENT(IN)  :: oSRF_RHS
    INTEGER, OPTIONAL, INTENT(IN)  :: ULP_Scale
    LOGICAL, OPTIONAL, INTENT(IN)  :: Check_All
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_oSRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: n
    INTEGER :: equal_status

    ! Set up
    err_status = SUCCESS
    ! ...Default precision is a single unit in last place
    ULP = 1
    IF ( PRESENT(ULP_Scale) ) ULP = ABS(ULP_Scale)
    ! ...Default action is to return on ANY difference
    Check_Once = .TRUE.
    IF ( PRESENT(Check_All) ) Check_Once = .NOT. Check_All
    ! ...Check the structure association status
    IF ( .NOT. Allocated_oSRF( oSRF_LHS ) .OR. &
         .NOT. Allocated_oSRF( oSRF_RHS )      ) THEN
      msg = 'Input oSRF arguments are NOT allocated'
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Check Release/Version info
    IF ( ( oSRF_LHS%Release /= oSRF_RHS%Release ) .OR. &
         ( oSRF_LHS%Version /= oSRF_RHS%Version )      ) THEN
      WRITE( msg,'("Release/Version numbers are different : ",&
                 &i2,".",i2.2," vs. ",i2,".",i2.2)' ) &
                 oSRF_LHS%Release, oSRF_LHS%Version, &
                 oSRF_RHS%Release, oSRF_RHS%Version
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Check dimensions
    IF ( oSRF_LHS%n_Bands /= oSRF_RHS%n_Bands ) THEN
      WRITE( msg,'("n_Bands dimensions are different : ",i0," vs. ",i0)' ) &
                 oSRF_LHS%n_Bands, oSRF_RHS%n_Bands
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Compare the values
    ! ...The Sensor_ID
    IF ( oSRF_LHS%Sensor_Id /= oSRF_RHS%Sensor_Id ) THEN
      WRITE( msg,'("Sensor_ID values are different, ",a," vs. ",a)' ) &
                 TRIM( oSRF_LHS%Sensor_Id), TRIM( oSRF_RHS%Sensor_Id)
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The WMO Satellite ID
    IF ( oSRF_LHS%WMO_Satellite_ID /= oSRF_RHS%WMO_Satellite_ID ) THEN
      WRITE( msg,'("WMO_Satellite_ID values are different, ",i0," vs. ",i0 )' ) &
                 oSRF_LHS%WMO_Satellite_ID, oSRF_RHS%WMO_Satellite_ID
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The WMO Sensor ID
    IF ( oSRF_LHS%WMO_Sensor_ID /= oSRF_RHS%WMO_Sensor_ID ) THEN
      WRITE( msg,'("WMO_Sensor_ID values are different, ",i0," vs. ",i0 )' ) &
                 oSRF_LHS%WMO_Sensor_ID, oSRF_RHS%WMO_Sensor_ID
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The Sensor_Type
    IF ( oSRF_LHS%Sensor_Type /= oSRF_RHS%Sensor_Type ) THEN
      WRITE( msg,'("Sensor types are different, ",i0,"(",a,") vs. ",i0,"(",a,")")' ) &
                 oSRF_LHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(oSRF_LHS%Sensor_Type)), &
                 oSRF_RHS%Sensor_Type, TRIM(SENSOR_TYPE_NAME(oSRF_RHS%Sensor_Type))
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The Channel
    IF ( oSRF_LHS%Channel /= oSRF_RHS%Channel ) THEN
      WRITE( msg,'("Channel values are different, ",i0," vs. ",i0 )' ) &
                 oSRF_LHS%Channel, oSRF_RHS%Channel
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The Integral
    IF ( .NOT. Compare_Float( oSRF_LHS%Integral,oSRF_RHS%Integral,ULP=ULP ) ) THEN
      WRITE( msg,'("Integral values are different, ",es13.6," vs. ",es13.6)' ) &
                 oSRF_LHS%Integral,oSRF_RHS%Integral
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The flags
    IF ( oSRF_LHS%Flags /= oSRF_RHS%Flags ) THEN
      WRITE( msg,'("Flags values are different : ",i0," vs. ",i0)' ) &
                 oSRF_LHS%Flags, oSRF_RHS%Flags
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The central frequency
    IF ( .NOT. Compare_Float( oSRF_LHS%f0,oSRF_RHS%f0,ULP=ULP ) ) THEN
      WRITE( msg,'("f0 values are different, ",es13.6," vs. ",es13.6)' ) &
                 oSRF_LHS%f0,oSRF_RHS%f0
      CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
    END IF
    ! ...The Planck_Coeffs
    DO n = 1, N_PLANCK_COEFFS
      IF ( .NOT. Compare_Float( oSRF_LHS%Planck_Coeffs(n), &
                                oSRF_RHS%Planck_Coeffs(n), &
                                ULP=ULP ) ) THEN
        WRITE( msg,'("Planck_Coeffs ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, oSRF_LHS%Planck_Coeffs(n),oSRF_RHS%Planck_Coeffs(n)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The Polychromatic_Coeffs
    DO n = 1, N_POLYCHROMATIC_COEFFS
      IF ( .NOT. Compare_Float( oSRF_LHS%Polychromatic_Coeffs(n), &
                                oSRF_RHS%Polychromatic_Coeffs(n), &
                                ULP=ULP ) ) THEN
        WRITE( msg,'("Polychromatic_Coeffs ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, oSRF_LHS%Polychromatic_Coeffs(n),oSRF_RHS%Polychromatic_Coeffs(n)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The n_Points values
    DO n = 1, oSRF_RHS%n_Bands
      IF ( oSRF_LHS%n_Points(n) /= oSRF_RHS%n_Points(n) ) THEN
        WRITE( msg,'("n_Points ",i0," values are different, ",i0," vs. ",i0)' ) &
                   n, oSRF_LHS%n_Points(n),oSRF_RHS%n_Points(n)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The f1 values
    DO n = 1, oSRF_RHS%n_Bands
      IF ( .NOT. Compare_Float( oSRF_LHS%f1(n),oSRF_RHS%f1(n),ULP=ULP ) ) THEN
        WRITE( msg,'("f1 ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, oSRF_LHS%f1(n),oSRF_RHS%f1(n)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The f2 values
    DO n = 1, oSRF_RHS%n_Bands
      IF ( .NOT. Compare_Float( oSRF_LHS%f2(n),oSRF_RHS%f2(n),ULP=ULP ) ) THEN
        WRITE( msg,'("f2 ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   n, oSRF_LHS%f2(n),oSRF_RHS%f2(n)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The Frequency values
    DO n = 1, oSRF_RHS%n_Bands
      equal_status = Equal_PtrArr( oSRF_LHS%Frequency(n), &
                                   oSRF_RHS%Frequency(n), &
                                   ULP_Scale=ULP_Scale, &
                                   Check_All=Check_All )
      IF ( equal_status /= SUCCESS ) THEN
        WRITE( msg,'("Frequency ",i0," values are different")' ) n
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...The Response values
    DO n = 1, oSRF_RHS%n_Bands
      equal_status = Equal_PtrArr( oSRF_LHS%Response(n), &
                                   oSRF_RHS%Response(n), &
                                   ULP_Scale=ULP_Scale, &
                                   Check_All=Check_All )
      IF ( equal_status /= SUCCESS ) THEN
        WRITE( msg,'("Response ",i0," values are different")' ) n
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
  
  CONTAINS
  
    SUBROUTINE Equal_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Equal_CleanUp
    
  END FUNCTION Equal_oSRF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Set_oSRF
!
! PURPOSE:
!       Function to sets the value of a property or a group of properties for
!       an oSRF structure.
!
! CALLING SEQUENCE:
!       Error_Status = Set_oSRF( &
!         oSRF                                       , &  ! Output
!         Band                 = Band                , &  ! Optional input
!         Version              = Version             , &  ! Optional input
!         Sensor_Id            = Sensor_Id           , &  ! Optional input
!         WMO_Satellite_Id     = WMO_Satellite_Id    , &  ! Optional input
!         WMO_Sensor_Id        = WMO_Sensor_Id       , &  ! Optional input
!         Sensor_Type          = Sensor_Type         , &  ! Optional input
!         Channel              = Channel             , &  ! Optional input
!         Integral             = Integral            , &  ! Optional input
!         Flags                = Flags               , &  ! Optional input
!         f0                   = f0                  , &  ! Optional input
!         Planck_Coeffs        = Planck_Coeffs       , &  ! Optional input
!         Polychromatic_Coeffs = Polychromatic_Coeffs, &  ! Optional input
!         R                    = R                   , &  ! Optional input
!         T                    = T                   , &  ! Optional input
!         f1                   = f1                  , &  ! Optional input
!         f2                   = f2                  , &  ! Optional input
!         Frequency            = Frequency           , &  ! Optional input
!         Response             = Response            )    ! Optional input
!
! OBJECT:
!       oSRF:          oSRF structure that is to have it properties modified.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
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
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the property set succeeded
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Set_oSRF( &
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
  RESULT( err_status )
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
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Set_oSRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_Band
    
    ! Set up
    err_status = SUCCESS
    
    
    ! Check band argument
    l_Band = 1
    IF ( PRESENT(Band) ) THEN
      l_Band = Band
      IF ( l_Band < 1 .OR. l_Band > self%n_Bands ) THEN
        WRITE( msg, '("Invalid band, ",i0,", specified for input oSRF")' ) l_Band
        CALL Set_CleanUp(); RETURN
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
      err_status = Set_PtrArr( self%Frequency(l_Band), Arr=Frequency )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error setting frequency for band ",i0)' ) l_Band
        CALL Set_CleanUp(); RETURN
      END IF
      ! ...Set the frequency limits
      self%f1(l_Band) = Frequency(1)
      self%f2(l_Band) = Frequency(SIZE(Frequency))
    END IF
    
    
    ! Set Response data
    IF ( PRESENT(Response) ) THEN
      err_status = Set_PtrArr( self%Response(l_Band), Arr=Response )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error setting Response for band ",i0)' ) l_Band
        CALL Set_CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE Set_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Set_CleanUp
    
    
  END FUNCTION Set_oSRF
  



!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Get_oSRF
!
! PURPOSE:
!       Function to get the value of a property or a group of properties for
!       an oSRF structure.
!
! CALLING SEQUENCE:
!       Error_Status = Get_oSRF( &
!         oSRF                                       , &  ! Output
!         Band                 = Band                , &  ! Optional input
!         Version              = Version             , &  ! Optional output
!         Sensor_Id            = Sensor_Id           , &  ! Optional output
!         WMO_Satellite_Id     = WMO_Satellite_Id    , &  ! Optional output
!         WMO_Sensor_Id        = WMO_Sensor_Id       , &  ! Optional output
!         Sensor_Type          = Sensor_Type         , &  ! Optional output
!         Channel              = Channel             , &  ! Optional output
!         Integral             = Integral            , &  ! Optional output
!         Flags                = Flags               , &  ! Optional output
!         f0                   = f0                  , &  ! Optional output
!         Planck_Coeffs        = Planck_Coeffs       , &  ! Optional output
!         Polychromatic_Coeffs = Polychromatic_Coeffs, &  ! Optional output
!         f1                   = f1                  , &  ! Optional output
!         f2                   = f2                  , &  ! Optional output
!         Frequency            = Frequency           , &  ! Optional output
!         Response             = Response            )    ! Optional output
!
! OBJECT:
!       oSRF:          oSRF structure that is to have it properties obtained.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUT ARGUMENTS:
!       Band:                  The band number to which the frequency and
!                              response data refer.
!                              If not specified, default value is 1.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  SCALAR
!                              ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Version:               The version number of the SRF data.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Sensor_ID:             A character string identifying the sensor and
!                              satellite platform used to contruct filenames.
!                              UNITS:      N/A
!                              TYPE:       CHARACTER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Satellite_ID:      The WMO code used to identify satellite platforms.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       WMO_Sensor_ID:         The WMO code used to identify sensors.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!       Sensor_Type:           The flag indicating the type of sensor (IR, MW, etc)
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Channel:               The sensor channel for the currenobject.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Integral:              The integrated SRF value.
!                              UNITS:      N/A
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Flags:                 Bit flags set/cleared during SRF processing.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f0:                    The central frequency of the SRF.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Planck_Coeffs:         Vector of Planck function coefficients for the SRF.
!                              UNITS:      Variable
!                              TYPE:       REAL
!                              DIMENSION:  Rank-1
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Polychromatic_Coeffs:  Vector of polychromatic correction coefficient for the SRF.
!                              UNITS:      Variable
!                              TYPE:       REAL
!                              DIMENSION:  Rank-1
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Points:              The number of points that specify the band frequency
!                              and responmse data.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      N/A
!                              TYPE:       INTEGER
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f1:                    The begin frequency of the SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       f2:                    The end frequency of the SRF bands.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1) or gigahertz (GHz)
!                              TYPE:       REAL
!                              DIMENSION:  Scalar
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency:             The frequency grid for an SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      Inverse centimetres (cm^-1)
!                              TYPE:       REAL
!                              DIMENSION:  n_Points
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Response:              The response data for an SRF band.
!                              Used in conjunction with the Band keyword argument.
!                              UNITS:      N/A
!                              TYPE:       REAL
!                              DIMENSION:  n_Points
!                              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:  The return value is an integer defining the error status.
!                      The error codes are defined in the Message_Handler module.
!                      If == SUCCESS the property get succeeded
!                         == FAILURE an error occurred
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  FUNCTION Get_oSRF( &
    self                , &  ! Input
    Band                , &  ! Optional input
    Version             , &  ! Optional output
    Sensor_Id           , &  ! Optional output
    WMO_Satellite_Id    , &  ! Optional output
    WMO_Sensor_Id       , &  ! Optional output
    Sensor_Type         , &  ! Optional output
    Channel             , &  ! Optional output
    Integral            , &  ! Optional output
    Flags               , &  ! Optional output
    f0                  , &  ! Optional output
    Planck_Coeffs       , &  ! Optional output
    Polychromatic_Coeffs, &  ! Optional output
    n_Points            , &  ! Optional output
    f1                  , &  ! Optional output
    f2                  , &  ! Optional output
    Frequency           , &  ! Optional output
    Response            ) &  ! Optional output
  RESULT( err_status )
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
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Planck_Coeffs(SIZE(self%Planck_Coeffs))       
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Polychromatic_Coeffs(SIZE(self%Polychromatic_Coeffs))
    INTEGER,      OPTIONAL, INTENT(OUT) :: n_Points
    REAL(fp),     OPTIONAL, INTENT(OUT) :: f1                  
    REAL(fp),     OPTIONAL, INTENT(OUT) :: f2                  
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Frequency(:)           
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Response(:)            
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_oSRF'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: l_Band
    
    ! Set up
    err_status = SUCCESS
    
    
    ! Check band argument
    l_Band = 1
    IF ( PRESENT(Band) ) THEN
      l_Band = Band
      IF ( l_Band < 1 .OR. l_Band > self%n_Bands ) THEN
        WRITE( msg, '("Invalid band, ",i0,", specified for input oSRF")' ) l_Band
        CALL Set_CleanUp(); RETURN
      END IF
    END IF


    ! Get data with defined sizes
    IF ( PRESENT(Version             ) ) Version              = self%Version  
    IF ( PRESENT(Sensor_Id           ) ) Sensor_Id            = self%Sensor_Id       
    IF ( PRESENT(WMO_Satellite_Id    ) ) WMO_Satellite_Id     = self%WMO_Satellite_Id
    IF ( PRESENT(WMO_Sensor_Id       ) ) WMO_Sensor_Id        = self%WMO_Sensor_Id   
    IF ( PRESENT(Sensor_Type         ) ) Sensor_Type          = self%Sensor_Type     
    IF ( PRESENT(Channel             ) ) Channel              = self%Channel         
    IF ( PRESENT(Integral            ) ) Integral             = self%Integral        
    IF ( PRESENT(Flags               ) ) Flags                = self%Flags           
    IF ( PRESENT(f0                  ) ) f0                   = self%f0              
    IF ( PRESENT(Planck_Coeffs       ) ) Planck_Coeffs        = self%Planck_Coeffs                       
    IF ( PRESENT(Polychromatic_Coeffs) ) Polychromatic_Coeffs = self%Polychromatic_Coeffs                
    IF ( PRESENT(n_Points            ) ) n_Points             = self%n_Points(l_Band)    
    IF ( PRESENT(f1                  ) ) f1                   = self%f1(l_Band)    
    IF ( PRESENT(f2                  ) ) f2                   = self%f2(l_Band)    


    ! Get frequency data
    IF ( PRESENT(Frequency) ) THEN
      err_status = Get_PtrArr( self%Frequency(l_Band), Arr=Frequency )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error getting frequency for band ",i0)' ) l_Band
        CALL Set_CleanUp(); RETURN
      END IF
    END IF
    
    
    ! Get Response data
    IF ( PRESENT(Response) ) THEN
      err_status = Get_PtrArr( self%Response(l_Band), Arr=Response )
      IF ( err_status /= SUCCESS ) THEN
        WRITE( msg, '("Error Getting Response for band ",i0)' ) l_Band
        CALL Set_CleanUp(); RETURN
      END IF
    END IF
    
  CONTAINS
  
    SUBROUTINE Set_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Set_CleanUp
    
    
  END FUNCTION Get_oSRF
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Inspect_oSRF
!
! PURPOSE:
!       Function to view the contents of an oSRF structure.
!
! CALLING SEQUENCE:
!       CALL Inspect_oSRF( oSRF )
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

  SUBROUTINE Inspect_oSRF( self )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN) :: self
    ! Local arguments
    INTEGER :: n
    ! Output the oSRF components     
    WRITE( *,'(/2x,"oSRF INSPECT")' )
    WRITE( *,'( 2x,"============")' )
    WRITE( *,'(2x,"Release              : ", i0)'             ) self%Release
    WRITE( *,'(2x,"Version              : ", i0)'             ) self%Version  
    WRITE( *,'(2x,"Sensor_Id            : ",  a)'             ) self%Sensor_Id       
    WRITE( *,'(2x,"WMO_Satellite_Id     : ", i0)'             ) self%WMO_Satellite_Id
    WRITE( *,'(2x,"WMO_Sensor_Id        : ", i0)'             ) self%WMO_Sensor_Id   
    WRITE( *,'(2x,"Sensor_Type          : ",  a)'             ) SENSOR_TYPE_NAME(self%Sensor_Type)
    WRITE( *,'(2x,"Channel              : ", i0)'             ) self%Channel         
    WRITE( *,'(2x,"Integral             : ", es13.6)'         ) self%Integral        
    WRITE( *,'(2x,"Flags                : ", i0)'             ) self%Flags           
    WRITE( *,'(2x,"f0                   : ", es13.6)'         ) self%f0              
    WRITE( *,'(2x,"Planck_Coeffs        : ", 2(es13.6,1x))'   ) self%Planck_Coeffs       
    WRITE( *,'(2x,"Polychromatic_Coeffs : ", 3(es13.6,:,1x))' ) self%Polychromatic_Coeffs
    WRITE( *,'(2x,"n_Bands              : ", i0)'             ) self%n_Bands
    IF ( self%n_Bands == 0 ) THEN
      WRITE( *,'(10x,"Press <ENTER> to continue...")', ADVANCE='NO' )
      READ(*,*)
    END IF
    DO n = 1, self%n_Bands
      WRITE( *,'(/2x,"BAND NUMBER ",i0)' ) n
      WRITE( *,'(4x,"n_Points : ", i0)'     ) self%n_Points(n)
      WRITE( *,'(4x,"f1       : ", es13.6)' ) self%f1(n)
      WRITE( *,'(4x,"f2       : ", es13.6)' ) self%f2(n)
      WRITE( *,'(/4x,"FREQUENCY")' )
      CALL Inspect_PtrArr( self%Frequency(n) )
      WRITE( *,'(/4x,"RESPONSE")' )
      CALL Inspect_PtrArr( self%Response(n) )
      WRITE( *,'(10x,"Press <ENTER> to continue...")', ADVANCE='NO' ); READ(*,*)
    END DO
  END SUBROUTINE Inspect_oSRF


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_oSRF
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the oSRF data structure.
!
! CALLING SEQUENCE:
!       CALL Info_oSRF( oSRF, Info )
!
! OBJECT:
!       oSRF:          oSRF structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(oSRF_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed oSRF data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Info_oSRF( self, Info )
    ! Arguments
    TYPE(oSRF_type), INTENT(IN)  :: self
    CHARACTER(*)   , INTENT(OUT) :: Info
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Setup
    Info = ' '
    IF ( .NOT. Allocated_oSRF(self) ) RETURN
    
    
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
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_oSRF

END MODULE oSRF_Define

 
