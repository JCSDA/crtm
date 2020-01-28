!
! LBLRTM_Layer_Define
!
! Module containing the definition of the LBLRTM Layer data object, as
! well as procedures to manipulate it.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 23-Jan-2000
!                     paul.vandelst@ssec.wisc.edu
!

MODULE LBLRTM_Layer_Define

  ! ------------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds           , ONLY: FP, IP, DP => Double
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  USE LBLRTM_Utility       , ONLY: LBLRTM_n_Points
  USE LBLRTM_Fhdr_Define   , ONLY: OPERATOR(/=), &
                                   LBLRTM_Fhdr_type   , &
                                   LBLRTM_Fhdr_IsValid, &
                                   LBLRTM_Fhdr_Inspect
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Layer_type
  ! Operators
  PUBLIC :: OPERATOR(==)
  PUBLIC :: OPERATOR(/=)
  ! Procedures
  PUBLIC :: LBLRTM_Layer_Associated
  PUBLIC :: LBLRTM_Layer_SetValid
  PUBLIC :: LBLRTM_Layer_IsValid
  PUBLIC :: LBLRTM_Layer_Destroy
  PUBLIC :: LBLRTM_Layer_Create
  PUBLIC :: LBLRTM_Layer_Inspect
  PUBLIC :: LBLRTM_Layer_Frequency
  PUBLIC :: LBLRTM_Layer_DefineVersion
  PUBLIC :: LBLRTM_Layer_Compare


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE LBLRTM_Layer_Equal
  END INTERFACE OPERATOR(==)

  INTERFACE OPERATOR(/=)
    MODULE PROCEDURE LBLRTM_Layer_NotEqual
  END INTERFACE OPERATOR(/=)


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  '$Id: LBLRTM_Layer_Define.f90 35401 2014-01-07 19:59:22Z paul.vandelst@noaa.gov $'
  ! Default message string length
  INTEGER, PARAMETER :: ML = 512


  ! ------------------------
  ! Derived type definitions
  ! ------------------------
  !:tdoc+:
  TYPE :: LBLRTM_Layer_type
    ! Allocation and valid data indicator
    LOGICAL :: Is_Allocated = .FALSE.
    LOGICAL :: Is_Valid     = .FALSE.
    ! The panel header
    TYPE(LBLRTM_Fhdr_type) :: Header
    ! Dimensions
    INTEGER  :: n_Points  = 0 ! L
    INTEGER  :: n_Spectra = 0 ! N
    ! Frequency data
    REAL(DP) :: Begin_Frequency    = 0.0_DP
    REAL(DP) :: End_Frequency      = 0.0_DP
    REAL(FP) :: Frequency_Interval = 0.0_FP
    ! Spectral data
    REAL(FP), ALLOCATABLE  :: Spectrum(:,:)  ! L x N
  END TYPE LBLRTM_Layer_type
  !:tdoc-:



CONTAINS


!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Associated
!
! PURPOSE:
!       Elemental function to test the status of the allocatable components
!       of the LBLRTM_Layer object.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Layer_Associated( LBLRTM_Layer )
!
! OBJECTS:
!       LBLRTM_Layer:  Structure which is to have its member's
!                      status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
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

  ELEMENTAL FUNCTION LBLRTM_Layer_Associated( self ) RESULT( Status )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Allocated
  END FUNCTION LBLRTM_Layer_Associated


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_SetValid
!
! PURPOSE:
!       Elemental subroutine to mark an instance of an LBLRTM_Layer object
!       as containing valid data.
!
!       Valid flag is set only if the LBLRTM_Layer object is allocated AND
!       if the embedded LBLRTM_Fhdr object is also valid.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_SetValid( LBLRTM_Layer )
!
! OBJECTS:
!       LBLRTM_Layer:  Instance which is to have its validity set.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
!                      DIMENSION:  Scalar or any rank
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Layer_SetValid(self)
    TYPE(LBLRTM_Layer_type), INTENT(IN OUT) :: self
    self%Is_Valid = LBLRTM_Layer_Associated(self) .AND. &
                    LBLRTM_Fhdr_IsValid(self%Header)
  END SUBROUTINE LBLRTM_Layer_SetValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_IsValid
!
! PURPOSE:
!       Elemental function to test if the LBLRTM_Layer object contains
!       valid data.
!
! CALLING SEQUENCE:
!       Status = LBLRTM_Layer_IsValid( LBLRTM_Layer )
!
! OBJECTS:
!       LBLRTM_Layer:  Instance which is to have its status tested.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
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

  ELEMENTAL FUNCTION LBLRTM_Layer_IsValid( self ) RESULT( Status )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: self
    LOGICAL :: Status
    Status = self%Is_Valid .AND. LBLRTM_Fhdr_IsValid(self%Header)
  END FUNCTION LBLRTM_Layer_IsValid


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Destroy
!
! PURPOSE:
!       Elemental subroutine to re-initialize LBLRTM_Layer objects.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_Destroy( LBLRTM_Layer )
!
! OBJECTS:
!       LBLRTM_Layer: Re-initialized LBLRTM_Layer instance.
!                     UNITS:      N/A
!                     TYPE:       LBLRTM_Layer_type
!                     DIMENSION:  Scalar or any rank
!                     ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Layer_Destroy( self )
    TYPE(LBLRTM_Layer_type), INTENT(OUT) :: self
    self%Is_Valid = .FALSE.
  END SUBROUTINE LBLRTM_Layer_Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Create
!
! PURPOSE:
!       Elemental subroutine to create an instance of an LBLRTM_Layer object.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_Create( LBLRTM_Layer, &
!                                 LBLRTM_Fhdr , &
!                                 n_Spectra     )
!
! OBJECTS:
!       LBLRTM_Layer:       LBLRTM_Layer object structure.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Layer_type
!                           DIMENSION:  Scalar or any rank
!                           ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!       LBLRTM_Fhdr:        LBLRTM_Fhdr object for the current Layer.
!                           UNITS:      N/A
!                           TYPE:       LBLRTM_Fhdr_type
!                           DIMENSION:  Conformable with LBLRTM_Layer
!                           ATTRIBUTES: INTENT(OUT)
!
!       n_Spectra:          Number of spectra (or "panels").
!                           Must be > 0.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Conformable with LBLRTM_Layer
!                           ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE LBLRTM_Layer_Create( &
    self     , &  ! Output
    Header   , &  ! Input
    n_Spectra  )  ! Input
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(OUT) :: self
    TYPE(LBLRTM_Fhdr_type) , INTENT(IN)  :: Header
    INTEGER                , INTENT(IN)  :: n_Spectra
    ! Local variables
    INTEGER :: alloc_stat
    INTEGER :: n_points

    ! Check input
    IF ( .NOT. LBLRTM_Fhdr_IsValid(Header) ) RETURN
    IF ( n_Spectra < 1 ) RETURN


    ! Determine the number of spectral points
    n_points = LBLRTM_n_Points( Header%Begin_Frequency   , &
                                Header%End_Frequency     , &
                                Header%Frequency_Interval  )
    ! ...Add an extra point for calculation slop
    n_points = n_points + 1

    ! Perform the allocation
    ALLOCATE( self%Spectrum( n_points, n_Spectra ), &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN


    ! Initialise
    ! ...Dimensions
    self%n_Points  = n_points
    self%n_Spectra = n_Spectra
    ! ...Header
    self%Header    = Header
    ! ...Frequency data
    self%Begin_Frequency    = self%Header%Begin_Frequency
    self%End_Frequency      = self%Header%End_Frequency
    self%Frequency_Interval = self%Header%Frequency_Interval
    ! ...Arrays
    self%Spectrum  = 0.0_FP


    ! Set allocation indicator
    self%Is_Allocated = .TRUE.

  END SUBROUTINE LBLRTM_Layer_Create


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Inspect
!
! PURPOSE:
!       Subroutine to print the contents of an instance of an LBLRTM_Layer
!       object to stdout.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_Inspect( LBLRTM_Layer )
!
! OBJECTS:
!       LBLRTM_Layer:  LBLRTM_Layer object to display.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Layer_Inspect( self, offset )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: self
    INTEGER,       OPTIONAL, INTENT(IN) :: offset
    CHARACTER(*), PARAMETER :: FMT_STRING = 'es22.15'
    INTEGER      :: i
    INTEGER      :: n_spaces(3), fhdr_offset
    CHARACTER(3) :: sp(3), cr
    IF ( .NOT. LBLRTM_Layer_IsValid(self) ) RETURN
    ! Compute indent for stand-alone, or embedded, object
    n_spaces    = [1,3,5]
    fhdr_offset = 2
    cr          = '/'
    IF ( PRESENT(offset) ) THEN
      n_spaces    = n_spaces    + ABS(offset)
      fhdr_offset = fhdr_offset + ABS(offset)
      cr          = ''
    END IF
    WRITE(sp,'(i0,"x")') n_spaces
    ! Output data
    WRITE(*,'('//cr//sp(1)//',"LBLRTM_Layer OBJECT")')
    CALL LBLRTM_Fhdr_Inspect( self%Header, offset=fhdr_offset )
    WRITE(*,'('//sp(2)//',"Kind types")')
    WRITE(*,'('//sp(3)//',"Default REAL       : ",i0)') FP
    WRITE(*,'('//sp(3)//',"Default INTEGER    : ",i0)') IP
    WRITE(*,'('//sp(2)//',"Dimensions")')
    WRITE(*,'('//sp(3)//',"n_Points           : ",i0)') self%n_Points
    WRITE(*,'('//sp(3)//',"n_Spectra          : ",i0)') self%n_Spectra
    WRITE(*,'('//sp(2)//',"Scalars")')
    WRITE(*,'('//sp(3)//',"Begin_Frequency    : ",'//FMT_STRING//')') self%Begin_Frequency
    WRITE(*,'('//sp(3)//',"End_Frequency      : ",'//FMT_STRING//')') self%End_Frequency
    WRITE(*,'('//sp(3)//',"Frequency_Interval : ",'//FMT_STRING//')') self%Frequency_Interval
    IF ( .NOT. LBLRTM_Layer_Associated(self) ) RETURN
    WRITE(*,'('//sp(2)//',"Data")')
    DO i = 1, self%n_Spectra
      WRITE(*,'('//sp(3)//',"Panel #",i0," spectrum :")') i
      WRITE(*,'(5(1x,'//FMT_STRING//'))') self%Spectrum(:,i)
    END DO
  END SUBROUTINE LBLRTM_Layer_Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_Frequency
!
! PURPOSE:
!       Subroutine to compute the frequency grid for a valid LBLRTM Layer object.
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_Frequency( LBLRTM_Layer, Frequency )
!
! OBJECTS:
!       LBLRTM_Layer:  LBLRTM_Layer object to display.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT(IN)
!
! OUTPUTS:
!       Frequency:     Frequency grid for the current LBLRTM Layer object.
!                      UNITS:      Inverse centimetres (cm^-1)
!                      TYPE:       REAL(DP)
!                      DIMENSION:  Rank-1
!                      ATTRIBUTES: INTENT(OUT), ALLOCATABLE
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE LBLRTM_Layer_Frequency( self, frequency )
    ! Arguments
    TYPE(LBLRTM_Layer_type), INTENT(IN)  :: self
    REAL(DP),   ALLOCATABLE, INTENT(OUT) :: frequency(:)
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_Define::Frequency'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: err_msg
    INTEGER  :: alloc_stat
    INTEGER  :: i, n_points
    REAL(DP) :: f1, f2

    ! Setup
    IF ( .NOT. LBLRTM_Layer_IsValid(self) ) RETURN


    ! Determine the number of spectral points
    n_points = LBLRTM_n_Points( self%Begin_Frequency   , &
                                self%End_Frequency     , &
                                self%Frequency_Interval  )


    ! Allocate the frequency array
    ALLOCATE( frequency(n_points), STAT=alloc_stat, ERRMSG=err_msg )
    IF ( alloc_stat /= 0 ) THEN
      msg = 'Error allocating frequency array - '//TRIM(err_msg)
      CALL Display_Message( ROUTINE_NAME, msg, FAILURE ); RETURN
    END IF
    
    
    ! Construct the unit grid
    f1 = self%Begin_Frequency
    f2 = self%End_Frequency
    frequency = REAL([(i,i=0,n_points-1)],DP)/REAL(n_points-1,DP)
    frequency = frequency*(f2-f1) + f1
    
  END SUBROUTINE LBLRTM_Layer_Frequency


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       LBLRTM_Layer_DefineVersion
!
! PURPOSE:
!       Subroutine to return the version information for the
!       definition module(s).
!
! CALLING SEQUENCE:
!       CALL LBLRTM_Layer_DefineVersion( Id )
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

  SUBROUTINE LBLRTM_Layer_DefineVersion( Id )
    CHARACTER(*), INTENT(OUT) :: Id
    Id = MODULE_VERSION_ID
  END SUBROUTINE LBLRTM_Layer_DefineVersion


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Layer_Compare
!
! PURPOSE:
!       Function to test the equality of two LBLRTM_Layer objects.
!
!       This procedure is basically a copy of the LBLRTM_Compare_Equal function
!       but non-elemental to allow for informational output when a difference
!       is found between the two structures.
!
!       Used for debugging only.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Layer_Compare( x, y )
!
! OBJECTS:
!       x, y:      Two LBLRTM_Layer objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Layer_type
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

  FUNCTION LBLRTM_Layer_Compare( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Layer_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Layer_Associated(x) .NEQV. LBLRTM_Layer_Associated(y) ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    IF ( LBLRTM_Layer_IsValid(x) .NEQV. LBLRTM_Layer_IsValid(y) ) THEN
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
    ! ...Scalars
    IF ( .NOT. ((x%Begin_Frequency    .EqualTo. y%Begin_Frequency   ) .AND. &
                (x%End_Frequency      .EqualTo. y%End_Frequency     ) .AND. &
                (x%Frequency_Interval .EqualTo. y%Frequency_Interval)) ) THEN
      msg = 'Object Frequency data are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
    END IF
    ! ...Data array
    IF ( LBLRTM_Layer_Associated(x) .AND. LBLRTM_Layer_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Spectrum .EqualTo. y%Spectrum)) ) THEN
        msg = 'Object Spectrum data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE); RETURN
      END IF
    END IF


    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Layer_Compare



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
!       LBLRTM_Layer_Equal
!
! PURPOSE:
!       Elemental function to test the equality of two LBLRTM_Layer objects.
!       Used in OPERATOR(==) interface block.
!
! CALLING SEQUENCE:
!       is_equal = LBLRTM_Layer_Equal( x, y )
!
!         or
!
!       IF ( x == y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:      Two LBLRTM_Layer objects to be compared.
!                  UNITS:      N/A
!                  TYPE:       LBLRTM_Layer_type
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

  ELEMENTAL FUNCTION LBLRTM_Layer_Equal( x, y ) RESULT( is_equal )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.

    ! Check the object association status
    IF ( LBLRTM_Layer_Associated(x) .NEQV. LBLRTM_Layer_Associated(y) ) RETURN
    IF ( LBLRTM_Layer_IsValid(x)    .NEQV. LBLRTM_Layer_IsValid(y)    ) RETURN

    ! Check contents
    ! ...File header
    IF ( x%Header /= y%Header ) RETURN
    ! ...Dimensions
    IF ( x%n_Points  /= y%n_Points .OR. &
         x%n_Spectra /= y%n_Spectra ) RETURN
    ! ...Scalars
    IF ( .NOT. ((x%Begin_Frequency    .EqualTo. y%Begin_Frequency   ) .AND. &
                (x%End_Frequency      .EqualTo. y%End_Frequency     ) .AND. &
                (x%Frequency_Interval .EqualTo. y%Frequency_Interval)) ) RETURN
    ! ...Arrays
    IF ( LBLRTM_Layer_Associated(x) .AND. LBLRTM_Layer_Associated(y) ) THEN
      IF ( .NOT. (ALL(x%Spectrum .EqualTo. y%Spectrum)) ) RETURN
    END IF

    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION LBLRTM_Layer_Equal


!------------------------------------------------------------------------------
!
! NAME:
!       LBLRTM_Layer_NotEqual
!
! PURPOSE:
!       Elemental function to test the inequality of two LBLRTM_Layer objects.
!       Used in OPERATOR(/=) interface block.
!
!       This function is syntactic sugar.
!
! CALLING SEQUENCE:
!       not_equal = LBLRTM_Layer_NotEqual( x, y )
!
!         or
!
!       IF ( x /= y ) THEN
!         ...
!       END IF
!
! OBJECTS:
!       x, y:          Two LBLRTM_Layer objects to be compared.
!                      UNITS:      N/A
!                      TYPE:       LBLRTM_Layer_type
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

  ELEMENTAL FUNCTION LBLRTM_Layer_NotEqual( x, y ) RESULT( not_equal )
    TYPE(LBLRTM_Layer_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION LBLRTM_Layer_NotEqual

END MODULE LBLRTM_Layer_Define
