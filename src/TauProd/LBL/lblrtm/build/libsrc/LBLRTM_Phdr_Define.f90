!
! LBLRTM_Phdr_Define
!
! Module containing the definition of the LBLRTM Panel header object, as
! well as procedures to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Phdr_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Phdr_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  ! Procedures
  PUBLIC :: LBLRTM_Phdr_SetValid
  PUBLIC :: LBLRTM_Phdr_IsValid
  PUBLIC :: LBLRTM_Phdr_Destroy
  PUBLIC :: LBLRTM_Phdr_Inspect
 ! PUBLIC :: LBLRTM_Phdr_DefineVersion
  PUBLIC :: LBLRTM_Phdr_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LBLRTM_Phdr_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE LBLRTM_Phdr_NotEqual
  END INTERFACE OPERATOR(/=)


  ! -----------------
  ! Module parameters
  ! -----------------
  !CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  !:tdoc+:
  TYPE :: LBLRTM_Phdr_type
    LOGICAL     :: Is_Valid            = .FALSE.
    REAL(DP)    :: Begin_Frequency     = 0.0_DP
    REAL(DP)    :: End_Frequency       = 0.0_DP
    REAL(FP)    :: Frequency_Interval  = 0.0_FP
    INTEGER(IP) :: n_Points            = 0_IP
  END TYPE LBLRTM_Phdr_type
  !:tdoc-:


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
!       LBLRTM_Phdr_SetValid
!
! PURPOSE:
!       Elemental subroutine to mark an instance of an LBLRTM_Phdr object
!       as containing valid data.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Phdr_SetValid( LBLRTM_Phdr )
!
! OBJECTS:
!       LBLRTM_Phdr:   Instance which is to have its validity set.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Phdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Phdr_SetValid(self)
    TYPE(LBLRTM_Phdr_type), INTENT(IN OUT) :: self
    self%Is_Valid = .TRUE.
  END SUBROUTINE LBLRTM_Phdr_SetValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_IsValid
!
! PURPOSE:
!       Elemental function to test if the LBLRTM_Phdr object contains
!       valid data.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Phdr_IsValid( LBLRTM_Phdr )
!
! OBJECTS:
!       LBLRTM_Phdr:   Instance which is to have its status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Phdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating
!                      if the object contains valid data.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Phdr_IsValid( self ) RESULT( Status )
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Valid
  END FUNCTION LBLRTM_Phdr_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LBLRTM_Phdr objects.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Phdr_Destroy( LBLRTM_Phdr )
!
! OBJECTS:
!       LBLRTM_Phdr:  Re-initialized LBLRTM_Phdr instance.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Phdr_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Phdr_Destroy( self )
    TYPE(LBLRTM_Phdr_type), INTENT(OUT) :: self
    self%Is_Valid = .FALSE.
  END SUBROUTINE LBLRTM_Phdr_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an instance of an LBLRTM_Phdr
!       object to stdout.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Phdr_Inspect( LBLRTM_Phdr )
!
! OBJECTS:
!       LBLRTM_Phdr:   LBLRTM_Phdr object to display.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Phdr_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Phdr_Inspect( self, offset )
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: self
    INTEGER,      OPTIONAL, INTENT(IN) :: offset
    CHARACTER(*), PARAMETER :: FMT_STRING = 'es22.15'
    INTEGER      :: n_spaces(3)
    CHARACTER(3) :: sp(3), cr
    IF ( .NOT. LBLRTM_Phdr_IsValid(self) ) RETURN
    ! Compute indent for stand-alone, or embedded, object
    n_spaces = [1,3,5]
    cr       = '/'
    IF ( PRESENT(offset) ) THEN
      n_spaces = n_spaces + ABS(offset)
      cr       = ''
    END IF
    WRITE(sp,'(i0,"x")') n_spaces
    ! Output data
    WRITE(*,'('//cr//sp(1)//',"LBLRTM_Phdr OBJECT")')
    WRITE(*,'('//sp(2)//',"Kind types")')
    WRITE(*,'('//sp(3)//',"Default REAL    : ",i0)') FP
    WRITE(*,'('//sp(3)//',"Default INTEGER : ",i0)') IP
    WRITE(*,'('//sp(2)//',"Data")')
    WRITE(*,'('//sp(3)//',"Begin_Frequency    : ",'//FMT_STRING//')') self%Begin_Frequency
    WRITE(*,'('//sp(3)//',"End_Frequency      : ",'//FMT_STRING//')') self%End_Frequency
    WRITE(*,'('//sp(3)//',"Frequency_Interval : ",'//FMT_STRING//')') self%Frequency_Interval
    WRITE(*,'('//sp(3)//',"n_Points           : ",i0,'//cr//')') self%n_Points
  END SUBROUTINE LBLRTM_Phdr_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Phdr_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Phdr_DefineVersion( Id )
!
! OUTPUTS:
!       Id:     Character string containing the version Id information for the
!               definition module(s).
!               UNITS:      N/A
!               TYPE:       CHARACTER(*)
!               DIMENSION:  Scalar
!               ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

!  SUBROUTINE LBLRTM_Phdr_DefineVersion( Id )
!    CHARACTER(*), INTENT(OUT) :: Id
!    Id = MODULE_VERSION_ID
!  END SUBROUTINE LBLRTM_Phdr_DefineVersion



!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Phdr_Compare
!
! PURPOSE:
!       Function to test the equality of two LBLRTM_Phdr objects.
!
!       This procedure is basically a copy of the LBLRTM_Phdr_Equal function
!       but non-elemental to allow for informational output when a difference
!       is found between the two structures.
!
!       Used for debugging only.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Phdr_Equal( x, y )
!
! OBJECTS:
!       x, y:      Two LBLRTM_Phdr objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Phdr_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:  Logical value indicating whether the inputs are equal.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  FUNCTION LBLRTM_Phdr_Compare( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Phdr_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

    ! Set up
    is_equal = .FALSE.


    ! Check the object association status
    IF ( LBLRTM_Phdr_IsValid(x) .NEQV. LBLRTM_Phdr_IsValid(y) ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF


    ! Check the contents
    IF ( .NOT. (x%Begin_Frequency .EqualTo. y%Begin_Frequency) ) THEN
      msg = 'Begin_Frequency components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%End_Frequency .EqualTo. y%End_Frequency) ) THEN
      msg = 'End_Frequency components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( .NOT. (x%Frequency_Interval .EqualTo. y%Frequency_Interval) ) THEN
      msg = 'Frequency_Interval components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( x%n_Points /= y%n_Points ) THEN
      msg = 'n_Points components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Phdr_Compare



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
!       LBLRTM_Phdr_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LBLRTM_Phdr objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Phdr_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:      Two LBLRTM_Phdr objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Phdr_type
!                  DIMENSION:  Scalar or any rank
!                  ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       is_equal:  Logical value indicating whether the inputs are equal.
!                  UNITS:      N/A
!                  TYPE:       LOGICAL
!                  DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Phdr_Equal( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Phdr_IsValid(x) .NEQV. LBLRTM_Phdr_IsValid(y) ) RETURN

    ! Check contents
    IF ( .NOT.((x%Begin_Frequency    .EqualTo. y%Begin_Frequency   ) .AND. &
               (x%End_Frequency      .EqualTo. y%End_Frequency     ) .AND. &
               (x%Frequency_Interval .EqualTo. y%Frequency_Interval) .AND. &
               (x%n_Points               ==    y%n_Points          )) ) RETURN

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Phdr_Equal


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Phdr_NotEqual
!
! PURPOSE:
!       Elemental function to test the inequality of two LBLRTM_Phdr objects.
!       Used in OPERATOR(/=) interface block.
!
!       This function is syntactic sugar.
!
! CALLING SEQUENCE:
!       not_equal = LBLRTM_Phdr_NotEqual( x, y )
!
!         or
!
!       IF ( x /= y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LBLRTM_Phdr objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Phdr_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       not_equal:     Logical value indicating whether the inputs are not equal.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Phdr_NotEqual( x, y ) RESULT( not_equal )
    TYPE(LBLRTM_Phdr_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION LBLRTM_Phdr_NotEqual

END MODULE LBLRTM_Phdr_Define
