!
! IRLSE_NPOESS_Define
!
! Module defining the IRLSE_NPOESS object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Aug-2009
!                       paul.vandelst@noaa.gov
 
MODULE IRLSE_NPOESS_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds           , ONLY: fp, Long, Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, WARNING, Display_Message
  USE Compare_Float_Numbers, ONLY: Compare_Float
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Derived type definitions
  PUBLIC :: IRLSE_NPOESS_type
  ! Public procedures
  PUBLIC :: Allocated_IRLSE_NPOESS
  PUBLIC :: Destroy_IRLSE_NPOESS
  PUBLIC :: Create_IRLSE_NPOESS
  PUBLIC :: Assign_IRLSE_NPOESS
  PUBLIC :: Equal_IRLSE_NPOESS
  PUBLIC :: Set_Property_IRLSE_NPOESS
  PUBLIC :: Get_Property_IRLSE_NPOESS
  PUBLIC :: Inspect_IRLSE_NPOESS
  PUBLIC :: Info_IRLSE_NPOESS
  ! Public parameters
  PUBLIC :: IRLSE_NPOESS_RELEASE
  PUBLIC :: IRLSE_NPOESS_VERSION


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Destroy_IRLSE_NPOESS
    MODULE PROCEDURE Destroy_scalar
    MODULE PROCEDURE Destroy_rank1
  END INTERFACE Destroy_IRLSE_NPOESS


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! String lengths
  INTEGER,  PARAMETER :: ML = 256 ! Message length
  INTEGER,  PARAMETER :: SL =  20 ! Surface type name string length
  ! Keyword set value
  INTEGER,  PARAMETER :: SET = 1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  ! Current valid release, version and algorithm identifiers
  INTEGER, PARAMETER :: IRLSE_NPOESS_RELEASE   = 1  ! This determines structure and file formats.
  INTEGER, PARAMETER :: IRLSE_NPOESS_VERSION   = 1  ! This is just the data version.
  INTEGER, PARAMETER :: IRLSE_NPOESS_ALGORITHM = 1  ! Switch to select algorithm
  ! ASCII codes for Version routine
  INTEGER, PARAMETER :: CARRIAGE_RETURN = 13
  INTEGER, PARAMETER :: LINEFEED = 10


  ! ----------------------------------
  ! IRLSE_NPOESS data type definitions
  ! ----------------------------------
  TYPE :: IRLSE_NPOESS_type
    ! Release and version information
    INTEGER(Long) :: Release = IRLSE_NPOESS_RELEASE
    INTEGER(Long) :: Version = IRLSE_NPOESS_VERSION
    ! Algorithm identifer
    INTEGER(Long) :: Algorithm = IRLSE_NPOESS_ALGORITHM
    ! Dimensions
    INTEGER :: n_Frequencies = 0  ! L dim.
    INTEGER :: n_Types       = 0  ! N dim.
    ! Dimensional vectors
    REAL(Double),  ALLOCATABLE :: Frequency(:)      ! Lx1
    CHARACTER(SL), ALLOCATABLE :: Type_Name(:)      ! Nx1
    ! Reflectance LUT data
    REAL(Double),  ALLOCATABLE :: Reflectance(:,:)  ! LxN
  END TYPE IRLSE_NPOESS_type


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
!       Allocated_IRLSE_NPOESS
!
! PURPOSE:
!       Elemental function to test if the allocatable members of an IRLSE_NPOESS
!       structure are allocated.
!
! CALLING SEQUENCE:
!       Allocation_Status = Allocated_IRLSE_NPOESS( IRLSE_NPOESS )
!
! OBJECT:
!       IRLSE_NPOESS:        IRLSE_NPOESS structure which is to have its allocatable
!                            member's status tested.
!                            UNITS:      N/A
!                            TYPE:       TYPE(IRLSE_NPOESS_type)
!                            DIMENSION:  Scalar or any rank array
!                            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Allocation_Status:   The return value is a logical value indicating the
!                            allocation status of the requisite members.
!                            .TRUE.  - if the structure allocatable members are
!                                      allocated.
!                            .FALSE. - some or all of the IRLSE_NPOESS allocatable
!                                      members are NOT allocated.
!                            UNITS:      N/A
!                            TYPE:       LOGICAL
!                            DIMENSION:  Scalar
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Allocated_IRLSE_NPOESS(self) RESULT(alloc_status)
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: self
    ! Function result
    LOGICAL :: alloc_status
    ! Set up
    alloc_status = .FALSE.

    ! Test the members
    alloc_status = .FALSE.
    IF ( ALLOCATED( self%Frequency   ) .AND. &
         ALLOCATED( self%Type_Name   ) .AND. &
         ALLOCATED( self%Reflectance )       ) THEN
      alloc_status = .TRUE.
    END IF

  END FUNCTION Allocated_IRLSE_NPOESS



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Destroy_IRLSE_NPOESS
! 
! PURPOSE:
!       Function to re-initialize the members of IRLSE_NPOESS data structures.
!
! CALLING SEQUENCE:
!       Error_Status = Destroy_IRLSE_NPOESS( IRLSE_NPOESS )
!
! OBJECT:
!       IRLSE_NPOESS: Re-initialised IRLSE_NPOESS structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE(IRLSE_NPOESS_type)
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
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: self
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    self%n_Frequencies = 0
    self%n_Types       = 0
  END FUNCTION Destroy_scalar


  FUNCTION Destroy_rank1(self) RESULT(err_status)
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: self(:)
    ! Function result
    INTEGER :: err_status
    ! Set up
    err_status = SUCCESS
    ! Do something with the structure
    self%n_Frequencies = 0
    self%n_Types       = 0
  END FUNCTION Destroy_rank1


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Create_IRLSE_NPOESS
! 
! PURPOSE:
!       Function to create an IRLSE_NPOESS structure by allocating its
!       array members.
!
! CALLING SEQUENCE:
!       Error_Status = Create_IRLSE_NPOESS( IRLSE_NPOESS , &
!                                           n_Frequencies, &
!                                           n_Types        )
!
! OBJECT:
!       IRLSE_NPOESS:  IRLSE_NPOESS structure with allocated members
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
! INPUT ARGUMENTS:
!       n_Frequencies: The number of frequencies for which we have data.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       n_Types:       The number of surface types for which we have data.
!                      Must be > 0.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
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

  FUNCTION Create_IRLSE_NPOESS( &
    self         , &
    n_Frequencies, &
    n_Types      ) &
  RESULT( err_status )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: self
    INTEGER,                 INTENT(IN)  :: n_Frequencies
    INTEGER,                 INTENT(IN)  :: n_Types
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Create_IRLSE_NPOESS'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_status

    ! Set up
    err_status = SUCCESS
    ! ...Check dimension inputs
    IF ( n_Frequencies < 1 .OR. n_Types < 1 ) THEN
      msg = 'Input dimensions must be > 0.'
      CALL Create_CleanUp(); RETURN
    END IF
    
    
    ! Perform the main array allocations
    ALLOCATE( self%Frequency( n_Frequencies ), &
              self%Type_Name( n_Types )      , &
              self%Reflectance( n_Frequencies, n_Types ), &
              STAT = alloc_status        )
    IF ( alloc_status /= 0 ) THEN
      WRITE( msg,'("Error allocating IRLSE_NPOESS data arrays. STAT = ",i0)' ) alloc_status
      CALL Create_CleanUp(); RETURN
    END IF


    ! Assign the dimensions and initialise arrays
    self%n_Frequencies = n_Frequencies
    self%n_Types       = n_Types      
    self%Frequency   = ZERO
    self%Type_Name   = ' '
    self%Reflectance = ZERO
    
  CONTAINS
  
    SUBROUTINE Create_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Create_CleanUp
    
  END FUNCTION Create_IRLSE_NPOESS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Assign_IRLSE_NPOESS
!
! PURPOSE:
!       Function to copy valid IRLSE_NPOESS structures.
!
! CALLING SEQUENCE:
!       Error_Status = Assign_IRLSE_NPOESS(IRLSE_NPOESS, copy)
!
! OBJECT:
!       IRLSE_NPOESS: IRLSE_NPOESS structure which is to be copied.
!                     UNITS:      N/A
!                     TYPE:       TYPE(IRLSE_NPOESS_type)
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       copy:         Copy of the IRLSE_NPOESS structure.
!                     UNITS:      N/A
!                     TYPE:       TYPE(IRLSE_NPOESS_type)
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

  FUNCTION Assign_IRLSE_NPOESS(self, copy) RESULT( err_status )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN)  :: self
    TYPE(IRLSE_NPOESS_type), INTENT(OUT) :: copy
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Assign_IRLSE_NPOESS'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: i
    
    ! Setup
    err_status = SUCCESS
    ! ...ALL *input* components must be allocated
    IF ( .NOT. Allocated_IRLSE_NPOESS( self ) ) THEN
      msg = 'Some or all INPUT IRLSE_NPOESS pointer members are NOT allocated.'
      CALL Assign_CleanUp(); RETURN
    END IF


    ! Allocate the structure
    err_status = Create_IRLSE_NPOESS( copy, self%n_Frequencies, self%n_Types )
    IF ( err_status /= SUCCESS ) THEN
      msg = 'Error allocating output IRLSE_NPOESS arrays.'
      CALL Assign_CleanUp(); RETURN
    END IF


    ! Copy data
    copy%Frequency   = self%Frequency  
    copy%Type_Name   = self%Type_Name  
    copy%Reflectance = self%Reflectance
 
  CONTAINS
  
    SUBROUTINE Assign_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Assign_CleanUp
    
  END FUNCTION Assign_IRLSE_NPOESS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Equal_IRLSE_NPOESS
!
! PURPOSE:
!       Function to test if two IRLSE_NPOESS structures are equal.
!
! CALLING SEQUENCE:
!       Error_Status = Equal_IRLSE_NPOESS( lhs, rhs             , &  ! Input
!                                          ULP_Scale  =ULP_Scale, &  ! Optional input
!                                          Check_All  =Check_All  )  ! Optional input
!
! INPUT ARGUMENTS:
!       lhs:           IRLSE_NPOESS structure to be compared; equivalent to the
!                      left-hand side of a lexical comparison, e.g.
!                        IF ( lhs == rhs ).
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!       rhs:           IRLSE_NPOESS structure to be compared to; equivalent to
!                      right-hand side of a lexical comparison, e.g.
!                        IF ( lhs == rhs ).
!                      UNITS:      N/A
!                      TYPE:       Same as IRLSE_NPOESS_LHS
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
!                      channel data of the IRLSE_NPOESS structures. The default
!                      action is return with a FAILURE status as soon as
!                      any difference is found. This optional argument can
!                      be used to get a listing of ALL the differences
!                      between data in IRLSE_NPOESS structures.
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

  FUNCTION Equal_IRLSE_NPOESS( &
    lhs , &
    rhs , &
    ULP_Scale, &
    Check_All) &
  RESULT( err_status )
    ! Arguments
    TYPE(IRLSE_NPOESS_type)  , INTENT(IN)  :: lhs
    TYPE(IRLSE_NPOESS_type)  , INTENT(IN)  :: rhs
    INTEGER, OPTIONAL, INTENT(IN)  :: ULP_Scale
    LOGICAL, OPTIONAL, INTENT(IN)  :: Check_All
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Equal_IRLSE_NPOESS'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: ULP
    LOGICAL :: Check_Once
    INTEGER :: i, j

    ! Set up
    err_status = SUCCESS
    ! ...Default precision is a single unit in last place
    ULP = 1
    IF ( PRESENT(ULP_Scale) ) ULP = ABS(ULP_Scale)
    ! ...Default action is to return on ANY difference
    Check_Once = .TRUE.
    IF ( PRESENT(Check_All) ) Check_Once = .NOT. Check_All
    ! ...Check the structure association status
    IF ( .NOT. Allocated_IRLSE_NPOESS( lhs ) .OR. &
         .NOT. Allocated_IRLSE_NPOESS( rhs )      ) THEN
      msg = 'Input IRLSE_NPOESS arguments are NOT allocated'
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Check Release/Version info
    IF ( ( lhs%Release /= rhs%Release ) .OR. &
         ( lhs%Version /= rhs%Version )      ) THEN
      WRITE( msg,'("Release/Version numbers are different : ",&
                 &i2,".",i2.2," vs. ",i2,".",i2.2)' ) &
                 lhs%Release, lhs%Version, &
                 rhs%Release, rhs%Version
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Check dimensions
    IF ( lhs%n_Frequencies /= rhs%n_Frequencies ) THEN
      WRITE( msg,'("n_Frequencies dimensions are different : ",i0," vs. ",i0)' ) &
                 lhs%n_Frequencies, rhs%n_Frequencies
      CALL Equal_CleanUp(); RETURN
    END IF
    IF ( lhs%n_Types /= rhs%n_Types ) THEN
      WRITE( msg,'("n_Types dimensions are different : ",i0," vs. ",i0)' ) &
                 lhs%n_Types, rhs%n_Types
      CALL Equal_CleanUp(); RETURN
    END IF


    ! Compare the values
    ! ...Frequency
    DO i = 1, lhs%n_Frequencies
      IF ( .NOT. Compare_Float( lhs%Frequency(i),rhs%Frequency(i),ULP=ULP ) ) THEN
        WRITE( msg,'("Frequency ",i0," values are different, ",es13.6," vs. ",es13.6)' ) &
                   i, lhs%Frequency(i),rhs%Frequency(i)
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...Type_Name
    DO i = 1, lhs%n_Types
      IF ( TRIM(lhs%Type_Name(i)) /= TRIM(rhs%Type_Name(i)) ) THEN
        WRITE( msg,'("Type_Name ",i0," values are different, ",a," vs. ",a)' ) &
                   i, TRIM(lhs%Type_Name(i)),TRIM(rhs%Type_Name(i))
        CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
      END IF
    END DO
    ! ...Reflectance
    DO j = 1, lhs%n_Types
      DO i = 1, lhs%n_Frequencies
        IF ( .NOT. Compare_Float( lhs%Reflectance(i,j),rhs%Reflectance(i,j),ULP=ULP ) ) THEN
          WRITE( msg,'("Reflectance (",i0,",",i0,") values are different, ",es13.6," vs. ",es13.6)' ) &
                     i,j, lhs%Reflectance(i,j),rhs%Reflectance(i,j)
          CALL Equal_CleanUp(); IF ( Check_Once ) RETURN
        END IF
      END DO
    END DO
  
  CONTAINS
  
    SUBROUTINE Equal_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Equal_CleanUp
    
  END FUNCTION Equal_IRLSE_NPOESS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Set_Property_IRLSE_NPOESS
!
! PURPOSE:
!       Function to sets the value of a property or a group of properties for
!       an IRLSE_NPOESS structure.
!
! CALLING SEQUENCE:
!       Error_Status = Set_Property_IRLSE_NPOESS( &
!         IRLSE_NPOESS             , &  ! Output
!         Frequency   = Frequency  , &  ! Optional input
!         Type_Name   = Type_Name  , &  ! Optional input
!         Reflectance = Reflectance)    ! Optional input
!
! OBJECT:
!       IRLSE_NPOESS:  Structure that is to have it properties modified.
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN OUT)
!
!
! OPTIONAL INPUT ARGUMENTS:
!       Frequency:     The frequency dimension of the structure.
!                      UNITS:      Inverse centimetres (cm^-1)
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1 (n_Frequencies)
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Type_Name:     The type name dimension of the structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Rank-1 (n_Types)
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
!
!       Reflectance:   The reflectance data.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-2 (n_Frequencies x n_Types)
!                      ATTRIBUTES: INTENT(IN), OPTIONAL
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

  FUNCTION Set_Property_IRLSE_NPOESS( &
    self       , &  ! In/output
    Frequency  , &  ! Optional input
    Type_Name  , &  ! Optional input
    Reflectance) &  ! Optional input
  RESULT( err_status )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN OUT) :: self
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Frequency(:)
    CHARACTER(*),  OPTIONAL, INTENT(IN)     :: Type_Name(:)
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Reflectance(:,:)
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Set_Property_IRLSE_NPOESS'
    ! Local variables
    CHARACTER(ML) :: msg
    
    ! Set up
    err_status = SUCCESS
    
    
    ! Set frequency data
    IF ( PRESENT(Frequency) ) THEN
      IF ( SIZE(Frequency) /= self%n_Frequencies ) THEN
        WRITE( msg,'("Frequency dimension mismatch: ",i0," vs ",i0)' ) &
                   SIZE(Frequency), self%n_Frequencies
        CALL Set_Property_CleanUp(); RETURN
      END IF
      self%Frequency = Frequency
    END IF
    
    
    ! Set Type_Name data
    IF ( PRESENT(Type_Name) ) THEN
      IF ( SIZE(Type_Name) /= self%n_Types ) THEN
        WRITE( msg,'("Type_Name dimension mismatch: ",i0," vs ",i0)' ) &
                   SIZE(Type_Name), self%n_Types
        CALL Set_Property_CleanUp(); RETURN
      END IF
      self%Type_Name = Type_Name
    END IF
    
    
    ! Set Reflectance data
    IF ( PRESENT(Reflectance) ) THEN
      IF ( SIZE(Reflectance,DIM=1) /= self%n_Frequencies .AND. &
           SIZE(Reflectance,DIM=2) /= self%n_Types             ) THEN
        WRITE( msg,'("Reflectance dimension mismatch: (",i0,",",i0,") vs (",i0,",",i0,")")' ) &
                   SIZE(Reflectance,DIM=1), SIZE(Reflectance,DIM=2), &
                   self%n_Frequencies, self%n_Types
        CALL Set_Property_CleanUp(); RETURN
      END IF
      self%Reflectance = Reflectance
    END IF
    
  CONTAINS
  
    SUBROUTINE Set_Property_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Set_Property_CleanUp
    
    
  END FUNCTION Set_Property_IRLSE_NPOESS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Get_Property_IRLSE_NPOESS
!
! PURPOSE:
!       Function to get the value of a property or a group of properties for
!       an IRLSE_NPOESS structure.
!
! CALLING SEQUENCE:
!       Error_Status = Get_Property_IRLSE_NPOESS( &
!         IRLSE_NPOESS                 , &  ! Output
!         Version       = Version      , &  ! Optional input
!         Algorithm     = Algorithm    , &  ! Optional output
!         n_Frequencies = n_Frequencies, &  ! Optional output
!         n_Types       = n_Types      , &  ! Optional output
!         Frequency     = Frequency    , &  ! Optional output
!         Type_Name     = Type_Name    , &  ! Optional output
!         Reflectance   = Reflectance  )    ! Optional output
!
! OBJECT:
!       IRLSE_NPOESS:  Structure that is to have it properties obtained.
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Version:       The version number of the NPOESS data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Algorithm:     The algorithm identifier for using the NPOESS data.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Frequencies: The number of frequencies dimension.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       n_Types:       The number of surface types dimension.
!                      UNITS:      N/A
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Frequency:     The frequency dimension of the structure.
!                      UNITS:      Inverse centimetres (cm^-1)
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-1 (n_Frequencies)
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Type_Name:     The type name dimension of the structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Rank-1 (n_Types)
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!       Reflectance:   The reflectance data.
!                      UNITS:      N/A
!                      TYPE:       REAL(fp)
!                      DIMENSION:  Rank-2 (n_Frequencies x n_Types)
!                      ATTRIBUTES: INTENT(OUT), OPTIONAL
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

  FUNCTION Get_Property_IRLSE_NPOESS( &
    self         , &  ! Input
    Version      , &  ! Optional input
    Algorithm    , &  ! Optional output
    n_Frequencies, &  ! Optional output
    n_Types      , &  ! Optional output
    Frequency    , &  ! Optional output
    Type_Name    , &  ! Optional output
    Reflectance  ) &  ! Optional output
  RESULT( err_status )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN)  :: self
    INTEGER,       OPTIONAL, INTENT(OUT) :: Version          
    INTEGER,       OPTIONAL, INTENT(OUT) :: Algorithm        
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Frequencies    
    INTEGER,       OPTIONAL, INTENT(OUT) :: n_Types          
    REAL(fp),      OPTIONAL, INTENT(OUT) :: Frequency(:)        
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Type_Name(:)        
    REAL(fp),      OPTIONAL, INTENT(OUT) :: Reflectance(:,:)      
    ! Function result
    INTEGER :: err_status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_Property_IRLSE_NPOESS'
    ! Local variables
    CHARACTER(ML) :: msg
    
    ! Set up
    err_status = SUCCESS
    
    
    ! Get data with defined sizes
    IF ( PRESENT(Version      ) ) Version       = self%Version      
    IF ( PRESENT(Algorithm    ) ) Algorithm     = self%Algorithm    
    IF ( PRESENT(n_Frequencies) ) n_Frequencies = self%n_Frequencies
    IF ( PRESENT(n_Types      ) ) n_Types       = self%n_Types      


    ! Get frequency data
    IF ( PRESENT(Frequency) ) THEN
      IF ( SIZE(Frequency) /= self%n_Frequencies ) THEN
        WRITE( msg,'("Frequency dimension mismatch: ",i0," vs ",i0)' ) &
                   SIZE(Frequency), self%n_Frequencies
        CALL Get_Property_CleanUp(); RETURN
      END IF
      Frequency = self%Frequency
    END IF
    
    
    ! Get Type_Name data
    IF ( PRESENT(Type_Name) ) THEN
      IF ( SIZE(Type_Name) /= self%n_Types ) THEN
        WRITE( msg,'("Type_Name dimension mismatch: ",i0," vs ",i0)' ) &
                   SIZE(Type_Name), self%n_Types
        CALL Get_Property_CleanUp(); RETURN
      END IF
      Type_Name = self%Type_Name
    END IF
    
    
    ! Get Reflectance data
    IF ( PRESENT(Reflectance) ) THEN
      IF ( SIZE(Reflectance,DIM=1) /= self%n_Frequencies .AND. &
           SIZE(Reflectance,DIM=2) /= self%n_Types             ) THEN
        WRITE( msg,'("Reflectance dimension mismatch: (",i0," vs ",i0)' ) &
                   SIZE(Reflectance), self%n_Frequencies
        CALL Get_Property_CleanUp(); RETURN
      END IF
      Reflectance = self%Reflectance
    END IF
    
  CONTAINS
  
    SUBROUTINE Get_Property_CleanUp()
      err_status = FAILURE
      CALL Display_Message( ROUTINE_NAME,TRIM(msg),err_status )
    END SUBROUTINE Get_Property_CleanUp
    
  END FUNCTION Get_Property_IRLSE_NPOESS
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Inspect_IRLSE_NPOESS
!
! PURPOSE:
!       Function to view the contents of an IRLSE_NPOESS structure.
!
! CALLING SEQUENCE:
!       CALL Inspect_IRLSE_NPOESS( IRLSE_NPOESS )
!
! OBJECT:
!       IRLSE_NPOESS:          IRLSE_NPOESS structure to inspect.
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Inspect_IRLSE_NPOESS( self )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN) :: self
    ! Output the IRLSE_NPOESS components     
    WRITE( *,'(/2x,"IRLSE_NPOESS INSPECT")' )
    WRITE( *,'( 2x,"====================")' )
    WRITE( *,'(2x,"Release       : ", i0)' ) self%Release
    WRITE( *,'(2x,"Version       : ", i0)' ) self%Version  
    WRITE( *,'(2x,"Algorithm_Id  : ", i0)' ) self%Algorithm
    WRITE( *,'(2x,"n_Frequencies : ", i0)' ) self%n_Frequencies
    WRITE( *,'(2x,"n_Types       : ", i0)' ) self%n_Types
    IF ( self%n_Frequencies > 0 ) THEN     
      WRITE( *,'(/2x,"FREQUENCIES:")' )
      WRITE( *,'(5es13.6)' ) self%Frequency
    END IF
    IF ( self%n_Types > 0 ) THEN     
      WRITE( *,'(/2x,"TYPE NAMES:")' )
      WRITE( *,'(4(1x,a))' ) self%Type_Name
    END IF
    IF ( self%n_Frequencies > 0 .AND. self%n_Types > 0 ) THEN
      WRITE( *,'(/2x,"REFLECTANCES:")' )
      WRITE( *,'(5es13.6)' ) self%Reflectance
    END IF
  END SUBROUTINE Inspect_IRLSE_NPOESS


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Info_IRLSE_NPOESS
!
! PURPOSE:
!       Subroutine to return a string containing version and dimension
!       information about the IRLSE_NPOESS data structure.
!
! CALLING SEQUENCE:
!       CALL Info_IRLSE_NPOESS( IRLSE_NPOESS, Info )
!
! OBJECT:
!       IRLSE_NPOESS:  IRLSE_NPOESS structure.
!                      UNITS:      N/A
!                      TYPE:       TYPE(IRLSE_NPOESS_type)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Info:          String containing version and dimension information
!                      about the passed IRLSE_NPOESS data structure.
!                      UNITS:      N/A
!                      TYPE:       CHARACTER(*)
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Info_IRLSE_NPOESS( self, Info )
    ! Arguments
    TYPE(IRLSE_NPOESS_type), INTENT(IN)  :: self
    CHARACTER(*)           , INTENT(OUT) :: Info
    ! Local variables
    CHARACTER(2000) :: LongString

    ! Setup
    Info = ' '
    IF ( .NOT. Allocated_IRLSE_NPOESS(self) ) RETURN
    
    
    ! Write the required data to the local string
    WRITE( LongString,'(a,1x,"IRLSE_NPOESS RELEASE.VERSION: ",i0,".",i2.2,1x,&
                       &"N_FREQUENCIES=",i0,2x,&
                       &"N_TYPES=",i0)' ) &
                       ACHAR(CARRIAGE_RETURN)//ACHAR(LINEFEED), &
                       self%Release, self%Version, &
                       self%n_Frequencies, &
                       self%n_Types


    ! Trim the output based on the
    ! dummy argument string length
    Info = LongString(1:MIN( LEN(Info), LEN_TRIM(LongString) ))

  END SUBROUTINE Info_IRLSE_NPOESS

END MODULE IRLSE_NPOESS_Define

 
