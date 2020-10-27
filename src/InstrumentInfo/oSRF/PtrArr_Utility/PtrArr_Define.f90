!
! PtrArr_Define
!
! Module defining the PtrArr object.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 22-Jul-2009
!                       paul.vandelst@noaa.gov

MODULE PtrArr_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  USE Type_Kinds           , ONLY: fp
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  IMPLICIT NONE


  ! -----------------  
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  INTEGER , PARAMETER :: ML = 256
  INTEGER , PARAMETER :: NAME_LENGTH = 80
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  
    
  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Operators
  PUBLIC :: OPERATOR(==)
  ! Derived type definitions
  PUBLIC :: PtrArr_type
  ! Procedures
  PUBLIC :: PtrArr_Associated
  PUBLIC :: PtrArr_Destroy
  PUBLIC :: PtrArr_Create
  PUBLIC :: PtrArr_SetValue
  PUBLIC :: PtrArr_GetValue
  PUBLIC :: PtrArr_FromVector
  PUBLIC :: PtrArr_ToVector
  PUBLIC :: PtrArr_Inspect
  PUBLIC :: PtrArr_DefineVersion
  PUBLIC :: PtrArr_ErrMsg


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE PtrArr_Equal
  END INTERFACE OPERATOR(==)


  ! ---------------
  ! Type definition
  ! ---------------
  TYPE :: PtrArr_type
    LOGICAL :: Is_Allocated = .FALSE.
    INTEGER                :: n = 0
    CHARACTER(NAME_LENGTH) :: Name = ''
    REAL(fp), ALLOCATABLE  :: Arr(:)
    ! For allocation debug purposes
    CHARACTER(ML) :: ErrMsg = ''
  END TYPE PtrArr_type


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  ! Elemental function to test the status of the allocatable components
  ! of a PtrArr object.
  
  ELEMENTAL FUNCTION PtrArr_Associated(self) RESULT(alloc_stat)
    TYPE(PtrArr_type), INTENT(IN) :: self
    LOGICAL :: alloc_stat
    alloc_stat = self%Is_Allocated
  END FUNCTION PtrArr_Associated



  ! Elemental subroutine to re-initialize PtrArr objects.
  
  ELEMENTAL SUBROUTINE PtrArr_Destroy(self)
    TYPE(PtrArr_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE PtrArr_Destroy



  ! Elemental subroutine to create an instance of a PtrArr object.
  
  ELEMENTAL SUBROUTINE PtrArr_Create(self, n)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: self
    INTEGER,           INTENT(IN)  :: n
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n < 1 ) RETURN
    
    ! Allocate
    ALLOCATE( self%Arr(n), STAT=alloc_stat, ERRMSG=self%ErrMsg )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    self%n = n
    self%Arr = ZERO
    
    ! Set allocation indicator
    self%Is_Allocated = .TRUE.
  END SUBROUTINE PtrArr_Create



  ! Function to set the value of PtrArr object components.
  
  FUNCTION PtrArr_SetValue(self,Name,Arr) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type),      INTENT(IN OUT) :: self
    CHARACTER(*), OPTIONAL, INTENT(IN)     :: Name
    REAL(fp)    , OPTIONAL, INTENT(IN)     :: Arr(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_SetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    
    ! Set up
    err_stat = SUCCESS
    
    ! Set array data
    IF ( PRESENT(Arr) ) THEN
      CALL PtrArr_Create(self,SIZE(Arr))
      IF ( .NOT. PtrArr_Associated(self) ) THEN
        err_stat = FAILURE
        msg = 'Error allocating array'
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      ! ...Assign data
      self%Arr = Arr
    END IF

    ! Set scalars
    IF ( PRESENT(Name) ) self%Name = Name
    
  END FUNCTION PtrArr_SetValue
  
  
  
  ! Function to get the value of PtrArr object components.
  
  FUNCTION PtrArr_GetValue(self,n_Points,Name,Arr) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type),                   INTENT(IN)  :: self
    INTEGER     , OPTIONAL,              INTENT(OUT) :: n_Points
    CHARACTER(*), OPTIONAL,              INTENT(OUT) :: Name
    REAL(fp)    , OPTIONAL, ALLOCATABLE, INTENT(OUT) :: Arr(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_GetValue'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat
    
    ! Set up
    err_stat = SUCCESS
    
    ! Get dimensions and scalars
    IF ( PRESENT(n_Points) ) n_Points = self%n
    IF ( PRESENT(Name    ) ) Name     = self%Name
    
    ! Get array data
    IF ( PRESENT(Arr) ) THEN
      ! ...Allocate output array
      ALLOCATE(Arr(self%n), STAT=alloc_stat, ERRMSG=alloc_msg)
      IF ( alloc_stat /= 0 ) THEN
        err_stat = FAILURE
        msg = 'Error allocating output array - '//TRIM(alloc_msg)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      ! ...Assign data
      Arr = self%Arr
    END IF
  END FUNCTION PtrArr_GetValue
  
  
  
  ! Function to convert a vector of data into a PtrArr object array.
  
  FUNCTION PtrArr_FromVector(self,n_Points,Vector) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type), INTENT(OUT) :: self(:)
    INTEGER          , INTENT(IN)  :: n_Points(:)
    REAL(fp)         , INTENT(IN)  :: Vector(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_FromVector'
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: n_elements
    INTEGER :: n, i1, i2
        
    ! Set up
    err_stat = SUCCESS
    n_elements = SIZE(self)
    ! ...Check inputs
    IF ( SIZE(n_Points) /= n_elements ) THEN
      err_stat = FAILURE
      msg = 'Size of output object and input n_Points arrays are different'
      CALL Display_Message(ROUTINE_NAME, msg, err_stat)
      RETURN
    END IF
    IF ( SUM(n_Points) /= SIZE(Vector) ) THEN
      err_stat = FAILURE
      msg = 'Sum of input n_Points and size of input Vector are different'
      CALL Display_Message(ROUTINE_NAME, msg, err_stat)
      RETURN
    END IF
    
    
    ! Transfer data from vector to object array
    i1 = 1
    DO n = 1, n_elements
      i2 = i1 + n_Points(n) - 1
      err_stat = PtrArr_SetValue( self(n), Arr=Vector(i1:i2) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error setting value for object #",i0)' ) n
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      i1 = i2 + 1
    END DO
    
  END FUNCTION PtrArr_FromVector
  
  
  
  ! Function to convert a PtrArr object array into a vector of data.
  
  FUNCTION PtrArr_ToVector(self,Vector) RESULT(err_stat)
    ! Arguments
    TYPE(PtrArr_type)    , INTENT(IN)  :: self(:)
    REAL(fp), ALLOCATABLE, INTENT(OUT) :: Vector(:)
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_ToVector'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: alloc_msg
    INTEGER :: alloc_stat
    INTEGER :: n_elements
    INTEGER :: n, i1, i2
    INTEGER :: n_points(SIZE(self))
    REAL(fp), ALLOCATABLE :: arr(:)
        
    ! Set up
    err_stat = SUCCESS
    n_elements = SIZE(self)

    ! Get point numbering and allocate
    DO n = 1, n_elements    
      err_stat = PtrArr_GetValue( self(n), n_Points = n_points(n) )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error getting n_Points for object #",i0)' ) n
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
    END DO
    ALLOCATE(Vector(SUM(n_points)), STAT=alloc_stat, ERRMSG=alloc_msg)
    IF ( alloc_stat /= 0 ) THEN
      err_stat = FAILURE
      msg = 'Error allocating output vector - '//TRIM(alloc_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat)
      RETURN
    END IF

    ! Transfer data from object array to vector
    i1 = 1
    DO n = 1, n_elements
      i2 = i1 + n_points(n) - 1
      err_stat = PtrArr_GetValue( self(n), Arr=arr )
      IF ( err_stat /= SUCCESS ) THEN
        WRITE( msg,'("Error getting value from object #",i0)' ) n
        CALL Display_Message(ROUTINE_NAME, msg, err_stat)
        RETURN
      END IF
      Vector(i1:i2) = arr
      i1 = i2 + 1
    END DO
    
  END FUNCTION PtrArr_ToVector
  
  
  
  ! Subroutine to view the contents of a PtrArr object.
  
  SUBROUTINE PtrArr_Inspect(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    ! Output the PtrArr components     
    WRITE(*,'(1x,"PtrArr OBJECT")')
    ! Dimensions
    WRITE(*,'(3x,"n : ",i0)') self%n
    IF ( self%Is_Allocated ) THEN
      WRITE(*,'(3x,">",a,"< data : ")') TRIM(self%Name)
      WRITE(*,'(5(1x,es13.6))' ) self%Arr
    END IF
  END SUBROUTINE PtrArr_Inspect  



  ! Subroutine to return the PtrArr definition module version information.
  
  SUBROUTINE PtrArr_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE PtrArr_DefineVersion



  ! Subroutine to display the PtrArr object error message (if any).
  
  SUBROUTINE PtrArr_ErrMsg(self)
    TYPE(PtrArr_type), INTENT(IN) :: self
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'PtrArr_ErrMsg'
    CALL Display_Message(ROUTINE_NAME, self%ErrMsg, INFORMATION)
  END SUBROUTINE PtrArr_ErrMsg



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  ! Elemental function to test the equality of two PtrArr objects.
  ! Used in OPERATOR(==) interface block.
  
  ELEMENTAL FUNCTION PtrArr_Equal( x, y ) RESULT( is_equal )
    TYPE(PtrArr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    
    ! Set up
    is_equal = .FALSE.
    
    ! Check the structure association status
    IF ( (.NOT. PtrArr_Associated(x)) .OR. &
         (.NOT. PtrArr_Associated(y)) ) RETURN
    
    ! Check the contents
    IF ( (x%n    == y%n   ) .AND. &
         (x%Name == y%Name) .AND. &
         ALL(x%Arr .EqualTo. y%Arr ) ) is_Equal = .TRUE.
    
  END FUNCTION PtrArr_Equal
  
END MODULE PtrArr_Define
