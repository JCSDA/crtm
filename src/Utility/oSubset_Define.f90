!
! oSubset_Define
!
! Module containing the subset object definition.
!
!
! CREATION HISTORY:
!   Written by:     Paul van Delst, 08-Apr-2016
!                   paul.vandelst@noaa.gov
!

MODULE oSubset_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Intrinsic modules
  USE ISO_Fortran_Env, ONLY: OUTPUT_UNIT
  ! Module usage
  USE File_Utility   , ONLY: File_Open
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE Sort_Utility   , ONLY: InsertionSort
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Datatypes
  PUBLIC :: oSubset_type


  ! -----------------
  ! Module Parameters
  ! -----------------
  ! Message string length
  INTEGER, PARAMETER :: ML = 256


  ! ------------------------
  ! Subset object definition
  ! ------------------------
  TYPE :: oSubset_type
    PRIVATE
    ! Housekeeping
    LOGICAL :: usable = .FALSE.
    INTEGER :: n_values = 0
    ! Data
    INTEGER, ALLOCATABLE :: number(:)
    INTEGER, ALLOCATABLE :: index(:) 
  CONTAINS
    PRIVATE
    ! Public methods
    PROCEDURE, PUBLIC, PASS(this) :: Is_Usable
    PROCEDURE, PUBLIC, PASS(this) :: Destroy
    PROCEDURE, PUBLIC, PASS(this) :: Inspect
    PROCEDURE, PUBLIC, PASS(this) :: Get_Value
    PROCEDURE, PUBLIC, PASS(this) :: Generate
    PROCEDURE :: Equal
    PROCEDURE :: NotEqual
    PROCEDURE :: Compare_
    GENERIC, PUBLIC :: OPERATOR(==) => Equal
    GENERIC, PUBLIC :: OPERATOR(/=) => NotEqual
    GENERIC, PUBLIC :: OPERATOR(.Compare.) => Compare_
    ! Private methods
    PROCEDURE, PASS(this) :: Create
  END TYPE oSubset_type


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
!   Is_Usable
!
! PURPOSE:
!   Elemental function method to test the status of oSubset objects to
!   determine if they are usable.
!
! CALLING SEQUENCE:
!   status = s_obj%Is_Usable()
!
! OBJECTS:
!   s_obj:   Object which is to have its usability tested.
!            UNITS:      N/A
!            CLASS:      oSubset_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   status:  The return value is a logical value indicating the
!            usable status of the object.
!              .TRUE.  - if the object is usable.
!              .FALSE. - if the object is NOT usable.
!            UNITS:      N/A
!            TYPE:       LOGICAL
!            DIMENSION:  Same as object
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Is_Usable( this )
    CLASS(oSubset_type), INTENT(IN) :: this
    LOGICAL :: Is_Usable
    Is_Usable = this%usable
  END FUNCTION Is_Usable


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Destroy
!
! PURPOSE:
!   Elemental subroutine method to re-initialize oSubset objects.
!
! CALLING SEQUENCE:
!   CALL s_obj%Destroy()
!
! OBJECTS:
!   s_obj:   Re-initialized oSubset object(s).
!            UNITS:      N/A
!            CLASS:      oSubset_type
!            DIMENSION:  Scalar or any rank
!            ATTRIBUTES: INTENT(OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Destroy( this )
    CLASS(oSubset_type), INTENT(OUT) :: this
    this%usable = .FALSE.
  END SUBROUTINE Destroy


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Inspect
!
! PURPOSE:
!   Subroutine method to display the contents of a oSubset object.
!
! CALLING SEQUENCE:
!   CALL s_obj%Inspect( unit = unit )
!
! OBJECTS:
!   s_obj:   Subset object
!            UNITS:      N/A
!            CLASS:      oSubset_type
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN)
!
! OPTIONAL INPUTS:
!   unit:    Unit number for an already open file to which the output
!            will be written.
!            If the argument is specified and the file unit is not
!            connected, the output goes to stdout.
!            UNITS:      N/A
!            TYPE:       INTEGER
!            DIMENSION:  Scalar
!            ATTRIBUTES: INTENT(IN), OPTIONAL
!
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Inspect( this, unit )
    ! Arguments
    CLASS(oSubset_type), INTENT(IN) :: this
    INTEGER,   OPTIONAL, INTENT(IN) :: unit
    ! Local variables
    INTEGER :: fid

    ! Setup
    fid = OUTPUT_UNIT
    IF ( PRESENT(unit) ) THEN
      IF ( File_Open(unit) ) fid = unit
    END IF

    ! The inspection output
    WRITE(fid,'(1x,"oSubset OBJECT")')
    WRITE(fid,'(3x,"n_Values:",1x,i0)') this%n_values
    IF ( .NOT. this%Is_Usable() ) RETURN
    WRITE(fid,'(3x,"Number  :")')
    WRITE(fid,'(10(1x,i5,:))') this%number
    WRITE(fid,'(3x,"Index  :")')
    WRITE(fid,'(10(1x,i5,:))') this%index

  END SUBROUTINE Inspect


!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Get_Value
!
! PURPOSE:
!   Subroutine method to get the contents of an oSubset object.
!
! CALLING SEQUENCE:
!   CALL s_obj%Get_Value( n_values = n_values, &
!                         number   = number  , &
!                         index    = index     )
!
! OBJECTS:
!   s_obj:     Subset object from which values are to be retrieved.
!              UNITS:      N/A
!              CLASS:      oSubset_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
! OPTIONAL OUTPUTS:
!   n_values:  The dimension of the components of the object.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(OUT), OPTIONAL
!
!   number:    Integer array to which the values of the Number
!              component of the object are to be assigned.
!              The actual argument must be defined as allocatable.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Rank-1
!              ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!
!   index:     Integer array to which the values of the Index
!              component of the object are to be assigned.
!              The actual argument must be defined as allocatable.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Rank-1
!              ATTRIBUTES: INTENT(OUT), OPTIONAL, ALLOCATABLE
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Get_Value( &
    this    , &  ! Input
    n_values, &  ! Optional output
    number  , &  ! Optional output
    index     )  ! Optional output
    ! Arguments
    CLASS(oSubset_type)           , INTENT(IN)  :: this
    INTEGER,              OPTIONAL, INTENT(OUT) :: n_values
    INTEGER, ALLOCATABLE, OPTIONAL, INTENT(OUT) :: number(:)
    INTEGER, ALLOCATABLE, OPTIONAL, INTENT(OUT) :: index(:)
    ! Local variables
    INTEGER :: n
    
    ! Check input
    IF ( .NOT. this%Is_Usable() ) RETURN

    ! Assign values
    n = this%n_values
    IF ( PRESENT(n_values) ) n_values = n
    
    IF ( PRESENT(number) ) THEN
      ALLOCATE(number(n))
      number = this%number
    END IF
    
    IF ( PRESENT(index) ) THEN
      ALLOCATE(index(n))
      index = this%index
    END IF

  END SUBROUTINE Get_Value
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   Generate
!
! PURPOSE:
!   Subroutine method to generate the subset indexing and populate the
!   the oSubset object.
!
!   Note: This is the *only* way to create/allocate and populate an
!         oSubset object.
!
! CALLING SEQUENCE:
!   CALL s_obj%Generate( list, subset_list )
!
! OBJECTS:
!   s_obj:       Subset object to populate with the generated subset
!                index information.
!                UNITS:      N/A
!                CLASS:      oSubset_type
!                DIMENSION:  Scalar
!                ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   list:        Array of values from which a subset is to be extracted.
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Rank-1
!                ATTRIBUTES: INTENT(IN)
!                
!   subset_list: Array of values defining the subset.
!                UNITS:      N/A
!                TYPE:       INTEGER
!                DIMENSION:  Rank-1
!                ATTRIBUTES: INTENT(IN)
!:sdoc-:
!--------------------------------------------------------------------------------

  SUBROUTINE Generate( &
    this       , &  ! Output
    list       , &  ! input
    subset_list  )  ! Input
    ! Arguments
    CLASS(oSubset_type), INTENT(OUT) :: this
    INTEGER            , INTENT(IN)  :: list(:)
    INTEGER            , INTENT(IN)  :: subset_list(:)
    ! Local variables
    INTEGER :: sorted_list(SIZE(list))
    INTEGER :: sorted_subset_list(SIZE(subset_list))
    INTEGER :: i, n_list
    INTEGER :: n_subset_list
    INTEGER :: n_values
    INTEGER :: isubset, iextract

    ! Set up
    ! ...No list data?
    n_list        = SIZE(list)
    n_subset_list = SIZE(subset_list)
    IF ( n_list < 1 .OR. n_subset_list < 1 ) RETURN


    ! Sort the lists
    sorted_list = list
    CALL InsertionSort( sorted_list )
    sorted_subset_list = subset_list
    CALL InsertionSort( sorted_subset_list )
    
    
    ! Count the elements to subset
    n_values = COUNT( sorted_subset_list >= sorted_list(1) .AND. &
                        sorted_subset_list <= sorted_list(n_list)  )
    IF ( n_values == 0 ) RETURN


    ! Allocate the object
    CALL this%Create( n_values )
    IF ( .NOT. this%Is_Usable() ) RETURN


    ! Define the start points for the search
    ! ...Determine the starting index in the SUBSET list array
    isubset = MINLOC( sorted_subset_list - sorted_list(1), &
                      MASK = ( (sorted_subset_list - sorted_list(1)) >= 0 ), &
                      DIM  = 1 )
    ! ...Set the starting index in the output. This is always 1.
    iextract = 1


    ! Loop over the number of MAIN list elements
    List_Loop: DO i = 1, n_list
      IF ( sorted_list(i) == sorted_subset_list(isubset) ) THEN  ! Is the list element in the subset?
        this%index(  iextract ) = i                              ! Save the index...
        this%number( iextract ) = sorted_list(i)                 ! ...and number
        iextract = iextract + 1                                  ! Increment the extract...
        isubset  = isubset  + 1                                  ! ...and subset indices
        IF ( isubset > n_subset_list ) EXIT List_Loop            ! Exit loop if last element found
      END IF
    END DO List_Loop

  END SUBROUTINE Generate



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                            ## OPERATOR METHODS ##                            ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   ==
!
! PURPOSE:
!   Operator method to test the equality of two oSubset objects.
!
! CALLING SEQUENCE:
!   IF ( x == y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:      Two subset objects to be compared.
!              UNITS:      N/A
!              CLASS:      oSubset_type
!              DIMENSION:  Scalar or any rank
!              ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_equal:  Logical value indicating whether the inputs are equal.
!              UNITS:      N/A
!              TYPE:       LOGICAL
!              DIMENSION:  Same as inputs.
!
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION Equal( x, y ) RESULT( is_equal )
    CLASS(oSubset_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal

    ! Set up
    is_equal = .FALSE.
   
    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) RETURN

    ! Check contents
    ! ...Dimensions
    IF ( x%n_values /= y%n_values ) RETURN
    ! ...Arrays
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. (ALL(x%Number == y%Number) .AND. &
                  ALL(x%Index  == y%Index )) ) RETURN
    END IF
    
    ! If we get here, then...
    is_equal = .TRUE.

  END FUNCTION Equal


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   /=
!
! PURPOSE:
!   Operator method to test the inequality of two oSubset objects.
!
! CALLING SEQUENCE:
!   IF ( x /= y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:  Two cloud cover objects to be compared.
!          UNITS:      N/A
!          CLASS:      oSubset_type
!          DIMENSION:  Scalar or any rank
!          ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  ELEMENTAL FUNCTION NotEqual( x, y ) RESULT( not_equal )
    CLASS(oSubset_type), INTENT(IN) :: x, y
    LOGICAL :: not_equal
    not_equal = .NOT. (x == y)
  END FUNCTION NotEqual
  
  
!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   .Compare.
!
! PURPOSE:
!   Operator method to compare two oSubset objects.
!
!   This procedure performs similarly to the == operator, but is non-elemental
!   to allow for informational output when a difference is found between the
!   two objects being compared.
!
!   Mostly used for debugging.
!
! CALLING SEQUENCE:
!   IF ( x .Compare. y ) THEN
!     ...
!   END IF
!
! OBJECTS:
!   x, y:      The cloud cover objects to compare.
!              UNITS:      N/A
!              CLASS:      oSubset_type
!              DIMENSION:  Scalar
!              ATTRIBUTES: INTENT(IN)
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Compare_( x, y ) RESULT( is_equal )
    CLASS(oSubset_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'oSubset_Define::Compare'
    ! Local variable
    CHARACTER(ML) :: msg

     ! Set up
    is_equal = .TRUE.

    ! Check the object association status
    IF ( x%Is_Usable() .NEQV. y%Is_Usable() ) THEN
      msg = 'Object association statuses are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF

    ! Check contents
    ! ...Dimensions
    IF ( x%n_values /= y%n_values ) THEN
      msg = 'Object dimensions are different'
      CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
      is_equal = .FALSE.
    END IF
    ! ...Arrays and sub-objects
    IF ( x%Is_Usable() .AND. y%Is_Usable() ) THEN
      IF ( .NOT. ALL(x%number == y%number) ) THEN
        msg = 'Object number component data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
      IF ( .NOT. ALL(x%index == y%index) ) THEN
        msg = 'Object index component data are different'
        CALL Display_Message(ROUTINE_NAME, msg, FAILURE)
        is_equal = .FALSE.
      END IF
    END IF

  END FUNCTION Compare_



!##################################################################################
!##################################################################################
!##                                                                              ##
!##                            ## PRIVATE METHODS ##                             ##
!##                                                                              ##
!##################################################################################
!##################################################################################

!--------------------------------------------------------------------------------
!
! NAME:
!   Create
!
! PURPOSE:
!   Private elemental subroutine method to create instances of oSubset objects.
!
! CALLING SEQUENCE:
!   CALL s_obj%Create( n_values )
!
! OBJECTS:
!   s_obj:     oSubset object
!              UNITS:      N/A
!              CLASS:      oSubset_type
!              DIMENSION:  Scalar or any rank
!              ATTRIBUTES: INTENT(OUT)
!
! INPUTS:
!   n_values:  Number of values in the subset.
!              Must be > 0.
!              UNITS:      N/A
!              TYPE:       INTEGER
!              DIMENSION:  Conformable with object.
!              ATTRIBUTES: INTENT(IN)
!
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE Create( this, n_values )
    ! Arguments
    CLASS(oSubset_type), INTENT(OUT) :: this
    INTEGER            , INTENT(IN)  :: n_values
    ! Local variables
    INTEGER :: alloc_stat

    ! Check input
    IF ( n_values < 1 ) RETURN

    ! Perform the allocations
    ALLOCATE( this%number( n_values ), &
              this%index( n_values ),  &
              STAT = alloc_stat )
    IF ( alloc_stat /= 0 ) RETURN

    ! Initialise
    this%n_values = n_values
    this%number   = 0
    this%index    = 0

    ! Set usability
    this%usable = .TRUE.

  END SUBROUTINE Create
  
END MODULE oSubset_Define
