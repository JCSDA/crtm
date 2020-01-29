!
! LBLRTM_Panel_Define
!
! Module containing the definition of the LBLRTM Panel data object, as
! well as procedures to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Panel_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE LBLRTM_Phdr_Define   , ONLY: OPERATOR(/=), &
                                   LBLRTM_Phdr_type   , &
                                   LBLRTM_Phdr_IsValid, &
                                   LBLRTM_Phdr_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Panel_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  ! Procedures
  PUBLIC :: LBLRTM_Panel_Associated
  PUBLIC :: LBLRTM_Panel_SetValid
  PUBLIC :: LBLRTM_Panel_IsValid
  PUBLIC :: LBLRTM_Panel_Destroy
  PUBLIC :: LBLRTM_Panel_Create
  PUBLIC :: LBLRTM_Panel_Inspect
  PUBLIC :: LBLRTM_Panel_DefineVersion
  PUBLIC :: LBLRTM_Panel_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LBLRTM_Panel_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE LBLRTM_Panel_NotEqual
  END INTERFACE OPERATOR(/=)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTM_Panel_Define.f90 35139 2013-12-26 18:14:43Z paul.vandelst@noaa.gov $'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  !:tdoc+:
  TYPE :: LBLRTM_Panel_type
    ! Allocation and valid data indicator
    LOGICAL :: Is_Allocated = .FALSE.
    LOGICAL :: Is_Valid     = .FALSE.
    ! The panel header
    TYPE(LBLRTM_Phdr_type) :: Header
    ! Dimensions
    INTEGER :: n_Points  = 0 ! L
    INTEGER :: n_Spectra = 0 ! N
    ! Data
    REAL(FP), ALLOCATABLE  :: Spectrum(:,:)  ! L x N
  END TYPE LBLRTM_Panel_type
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
!       LBLRTM_Panel_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the LBLRTM_Panel object.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Panel_Associated( LBLRTM_Panel )
!
! OBJECTS:
!       LBLRTM_Panel:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!       Status:        The return value is a logical value indicating if the
!                      object has been allocated.
!                      UNITS:      N/A
!                      TYPE:       LOGICAL
!                      DIMENSION:  Same as input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION LBLRTM_Panel_Associated( self ) RESULT( Status )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION LBLRTM_Panel_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_SetValid
!
! PURPOSE:
!       Elemental subroutine to mark an instance of an LBLRTM_Panel object
!       as containing valid data.
!
!       Valid flag is set only if the LBLRTM_Panel object is allocated AND
!       if the embedded LBLRTM_Phdr object is also valid.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_SetValid( LBLRTM_Panel )
!
! OBJECTS:
!       LBLRTM_Panel:  Instance which is to have its validity set.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Panel_SetValid(self)
    TYPE(LBLRTM_Panel_type), INTENT(IN OUT) :: self
    self%Is_Valid = LBLRTM_Panel_Associated(self) .AND. &
                    LBLRTM_Phdr_IsValid(self%Header)
  END SUBROUTINE LBLRTM_Panel_SetValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_IsValid
!
! PURPOSE:
!       Elemental function to test if the LBLRTM_Panel object contains
!       valid data.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Panel_IsValid( LBLRTM_Panel )
!
! OBJECTS:
!       LBLRTM_Panel:  Instance which is to have its status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
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

  ELEMENTAL FUNCTION LBLRTM_Panel_IsValid( self ) RESULT( Status )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Valid .AND. LBLRTM_Phdr_IsValid(self%Header)
  END FUNCTION LBLRTM_Panel_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LBLRTM_Panel objects.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_Destroy( LBLRTM_Panel )
!
! OBJECTS:
!       LBLRTM_Panel: Re-initialized LBLRTM_Panel instance.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Panel_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Panel_Destroy( self )
    TYPE(LBLRTM_Panel_type), INTENT(OUT) :: self
    self%Is_Allocated = .FALSE.
    self%Is_Valid     = .FALSE.
  END SUBROUTINE LBLRTM_Panel_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an LBLRTM_Panel object.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_Create( LBLRTM_Panel, &
!                                 LBLRTM_PHdr , &
!                                 n_Spectra   , &
!                                 Err_Msg = Err_Msg )
!
! OBJECTS:
!       LBLRTM_Panel:       LBLRTM_Panel object structure.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Panel_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       LBLRTM_Phdr:        LBLRTM_Phdr object for the current panel.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Phdr_type
!                           DIMENSION:  Conformable with LBLRTM_Panel
!                           ATTRIBUTES: INTENT(IN)
!
!       n_Spectra:          Number of spectra (or "panels").
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with LBLRTM_Panel
!                           ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!       Err_Msg:            String containing error message text if allocation
!                           failde.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Conformable with LBLRTM_Panel
!                           ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Panel_Create( &
    self     , &  ! Output
    Header   , &  ! Input
    n_Spectra, &  ! Input
    Err_Msg    )  ! Optional output
    ! Arguments
    TYPE(LBLRTM_Panel_type), INTENT(OUT) :: self
    TYPE(LBLRTM_Phdr_type) , INTENT(IN)  :: Header
    INTEGER                , INTENT(IN)  :: n_Spectra
    CHARACTER(*),  OPTIONAL, INTENT(OUT) :: Err_Msg
    ! Local variables
    CHARACTER(ML) :: msg
    INTEGER :: alloc_stat


    ! Setup
    IF ( PRESENT(Err_Msg) ) Err_Msg = ''
    ! ...Check input
    IF ( .NOT. LBLRTM_Phdr_IsValid(Header) ) THEN
      IF ( PRESENT(Err_Msg) ) Err_Msg = 'Panel header component is invalid'
      RETURN
    END IF
    IF ( Header%n_Points > 1000000 ) THEN
      WRITE(msg,'("Panel header n_Points is unreasonably large: ",i0)') Header%n_Points
      IF ( PRESENT(Err_Msg) ) Err_Msg = msg
      RETURN
    END IF
    IF ( n_Spectra < 1 ) THEN
      IF ( PRESENT(Err_Msg) ) Err_Msg = 'Input n_Spectra must be > 0'
      RETURN
    END IF


    ! Perform the allocation
    ALLOCATE( self%Spectrum( Header%n_Points, n_Spectra ), &
              STAT = alloc_stat, ERRMSG = msg )
    IF ( alloc_stat /= 0 ) THEN
      IF ( PRESENT(Err_Msg) ) Err_Msg = msg
      RETURN
    END IF


    ! Initialise
    ! ...Dimensions
    self%n_Points  = INT(Header%n_Points)
    self%n_Spectra = n_Spectra
    ! ...Header
    self%Header    = Header
    ! ...Arrays
    self%Spectrum  = 0.0_FP


    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE LBLRTM_Panel_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an instance of an LBLRTM_Panel
!       object to stdout.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_Inspect( LBLRTM_Panel )
!
! OBJECTS:
!       LBLRTM_Panel:  LBLRTM_Panel object to display.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Panel_Inspect( self, offset )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: self
    INTEGER,       OPTIONAL, INTENT(IN) :: offset
    CHARACTER(*), PARAMETER :: FMT_STRING = 'es22.15'
    INTEGER      :: i
    INTEGER      :: n_spaces(3), phdr_offset
    CHARACTER(3) :: sp(3), cr
    IF ( .NOT. LBLRTM_Panel_IsValid(self) ) RETURN
    ! Compute indent for stand-alone, or embedded, object
    n_spaces    = [1,3,5]
    phdr_offset = 2
    cr          = '/'
    IF ( PRESENT(offset) ) THEN
      n_spaces    = n_spaces    + ABS(offset)
      phdr_offset = phdr_offset + ABS(offset)
      cr          = ''
    END IF
    WRITE(sp,'(i0,"x")') n_spaces
    ! Output data
    WRITE(*,'('//cr//sp(1)//',"LBLRTM_Panel OBJECT")')
    CALL LBLRTM_Phdr_Inspect( self%Header, offset=phdr_offset )
    WRITE(*,'('//sp(2)//',"Kind types")')
    WRITE(*,'('//sp(3)//',"Default REAL    : ",i0)') FP
    WRITE(*,'('//sp(3)//',"Default INTEGER : ",i0)') IP
    WRITE(*,'('//sp(2)//',"Dimensions")')
    WRITE(*,'('//sp(3)//',"n_Points        : ",i0)') self%n_Points
    WRITE(*,'('//sp(3)//',"n_Spectra       : ",i0)') self%n_Spectra
    IF ( .NOT. LBLRTM_Panel_Associated(self) ) RETURN
    WRITE(*,'('//sp(2)//',"Data")')
    DO i = 1, self%n_Spectra
      WRITE(*,'('//sp(3)//',"Panel #",i0," spectrum :")') i
      WRITE(*,'(5(1x,'//FMT_STRING//'))') self%Spectrum(:,i)
    END DO
  END SUBROUTINE LBLRTM_Panel_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Panel_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Panel_DefineVersion( Id )
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

  SUBROUTINE LBLRTM_Panel_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Panel_DefineVersion


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Panel_Compare
!
! PURPOSE:
!       Function to test the equality of two LBLRTM_Panel objects.
!
!       This procedure is basically a copy of the LBLRTM_Panel_Equal function
!       but non-elemental to allow for informational output when a difference
!       is found between the two structures.
!
!       Used for debugging only.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Panel_Compare( x, y )
!
! OBJECTS:
!       x, y:      Two LBLRTM_Panel objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Panel_type
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

  FUNCTION LBLRTM_Panel_Compare( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Panel_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

    ! Set up
    is_equal = .FALSE.


    ! Check the object association status
    IF ( LBLRTM_Panel_Associated(x) .NEQV. LBLRTM_Panel_Associated(y) ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( LBLRTM_Panel_IsValid(x) .NEQV. LBLRTM_Panel_IsValid(y) ) THEN
      msg = 'Object validity statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF


    ! Check the contents
    ! ...Panel header
    IF ( x%Header /= y%Header ) THEN
      msg = 'Object header components are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Dimensions
    IF ( x%n_Points  /= y%n_Points .OR. &
         x%n_Spectra /= y%n_Spectra ) THEN
      msg = 'Object dimensions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Data array
    IF ( LBLRTM_Panel_Associated(x) .AND. LBLRTM_Panel_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Spectrum .EqualTo. y%Spectrum)) ) THEN
        msg = 'Object Spectrum data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
      END IF
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Panel_Compare



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
!       LBLRTM_Panel_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LBLRTM_Panel objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Panel_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:      Two LBLRTM_Panel objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Panel_type
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

  ELEMENTAL FUNCTION LBLRTM_Panel_Equal( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Panel_Associated(x) .NEQV. LBLRTM_Panel_Associated(y) ) RETURN
    IF ( LBLRTM_Panel_IsValid(x) .NEQV. LBLRTM_Panel_IsValid(y) ) RETURN

    ! Check contents
    ! ...Panel header
    IF ( x%Header /= y%Header ) RETURN
    ! ...Dimensions
    IF ( x%n_Points  /= y%n_Points .OR. &
         x%n_Spectra /= y%n_Spectra ) RETURN
    ! ...Arrays
    IF ( LBLRTM_Panel_Associated(x) .AND. LBLRTM_Panel_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Spectrum .EqualTo. y%Spectrum)) ) RETURN
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Panel_Equal


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Panel_NotEqual
!
! PURPOSE:
!       Elemental function to test the inequality of two LBLRTM_Panel objects.
!       Used in OPERATOR(/=) interface block.
!
!       This function is syntactic sugar.
!
! CALLING SEQUENCE:
!       not_equal = LBLRTM_Panel_NotEqual( x, y )
!
!         or
!
!       IF ( x /= y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LBLRTM_Panel objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Panel_type
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

  ELEMENTAL FUNCTION LBLRTM_Panel_NotEqual( x, y ) RESULT( not_equal )
    TYPE(LBLRTM_Panel_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION LBLRTM_Panel_NotEqual

END MODULE LBLRTM_Panel_Define
