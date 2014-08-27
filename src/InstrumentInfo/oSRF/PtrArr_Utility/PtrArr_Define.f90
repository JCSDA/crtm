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
    '$Id$'
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

  ELEMENTAL FUNCTION PtrArr_Associated(self) RESULT(alloc_stat)
    TYPE(PtrArr_type), INTENT(IN) :: self
    LOGICAL :: alloc_stat
    alloc_stat = self%Is_Allocated
  END FUNCTION PtrArr_Associated



  ELEMENTAL SUBROUTINE PtrArr_Destroy(self)
    TYPE(PtrArr_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
  END SUBROUTINE PtrArr_Destroy



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



  SUBROUTINE PtrArr_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE PtrArr_DefineVersion



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
