
!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Interpolated
!
! PURPOSE:
!   Elemental function to test if the Interpolated bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Interpolated(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, INTERPOLATED_FLAG)
  END FUNCTION oSRF_Is_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Integrated
!
! PURPOSE:
!   Elemental function to test if the Integrated bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Integrated(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, INTEGRATED_FLAG)
  END FUNCTION oSRF_Is_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_F0_Computed
!
! PURPOSE:
!   Elemental function to test if the F0_Computed bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_F0_Computed( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_F0_Computed(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, F0_COMPUTED_FLAG)
  END FUNCTION oSRF_Is_F0_Computed
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Linearly_Interpolated
!
! PURPOSE:
!   Elemental function to test if the Linearly_Interpolated bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Linearly_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Linearly_Interpolated(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, LINEARLY_INTERPOLATED_FLAG)
  END FUNCTION oSRF_Is_Linearly_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Gaussian_Integrated
!
! PURPOSE:
!   Elemental function to test if the Gaussian_Integrated bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Gaussian_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Gaussian_Integrated(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, GAUSSIAN_INTEGRATED_FLAG)
  END FUNCTION oSRF_Is_Gaussian_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Is_Difference
!
! PURPOSE:
!   Elemental function to test if the Difference bit flag in an
!   oSRF object is set.
!
! CALLING SEQUENCE:
!   is_set = oSRF_Is_Difference( oSRF )
!
! OBJECTS:
!   oSRF: Object to be tested.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN)
!
! FUNCTION RESULT:
!   is_set: The return value is a logical value.
!            .TRUE.  - The flag is set.
!            .FALSE. - The flag is NOT set.
!           UNITS:      N/A
!           TYPE:       LOGICAL
!           DIMENSION:  Same as oSRF input
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL FUNCTION oSRF_Is_Difference(oSRF) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: oSRF
    LOGICAL :: is_set
    is_set = .FALSE.
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    is_set = oSRF_Is_Flag_Set(oSRF, DIFFERENCE_FLAG)
  END FUNCTION oSRF_Is_Difference
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Interpolated
!
! PURPOSE:
!   Elemental subroutine to set the Interpolated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Interpolated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, INTERPOLATED_FLAG)
  END SUBROUTINE oSRF_Set_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Integrated
!
! PURPOSE:
!   Elemental subroutine to set the Integrated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Integrated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, INTEGRATED_FLAG)
  END SUBROUTINE oSRF_Set_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_F0_Computed
!
! PURPOSE:
!   Elemental subroutine to set the F0_Computed bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_F0_Computed( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_F0_Computed( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, F0_COMPUTED_FLAG)
  END SUBROUTINE oSRF_Set_F0_Computed
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Linearly_Interpolated
!
! PURPOSE:
!   Elemental subroutine to set the Linearly_Interpolated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Linearly_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Linearly_Interpolated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, LINEARLY_INTERPOLATED_FLAG)
  END SUBROUTINE oSRF_Set_Linearly_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Gaussian_Integrated
!
! PURPOSE:
!   Elemental subroutine to set the Gaussian_Integrated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Gaussian_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Gaussian_Integrated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, GAUSSIAN_INTEGRATED_FLAG)
  END SUBROUTINE oSRF_Set_Gaussian_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Set_Difference
!
! PURPOSE:
!   Elemental subroutine to set the Difference bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Set_Difference( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Set_Difference( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Set_Flag(oSRF, DIFFERENCE_FLAG)
  END SUBROUTINE oSRF_Set_Difference
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Interpolated
!
! PURPOSE:
!   Elemental subroutine to clear the Interpolated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Interpolated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, INTERPOLATED_FLAG )
  END SUBROUTINE oSRF_Clear_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Integrated
!
! PURPOSE:
!   Elemental subroutine to clear the Integrated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Integrated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, INTEGRATED_FLAG )
  END SUBROUTINE oSRF_Clear_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_F0_Computed
!
! PURPOSE:
!   Elemental subroutine to clear the F0_Computed bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_F0_Computed( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_F0_Computed( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, F0_COMPUTED_FLAG )
  END SUBROUTINE oSRF_Clear_F0_Computed
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Linearly_Interpolated
!
! PURPOSE:
!   Elemental subroutine to clear the Linearly_Interpolated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Linearly_Interpolated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Linearly_Interpolated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, LINEARLY_INTERPOLATED_FLAG )
  END SUBROUTINE oSRF_Clear_Linearly_Interpolated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Gaussian_Integrated
!
! PURPOSE:
!   Elemental subroutine to clear the Gaussian_Integrated bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Gaussian_Integrated( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Gaussian_Integrated( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, GAUSSIAN_INTEGRATED_FLAG )
  END SUBROUTINE oSRF_Clear_Gaussian_Integrated
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Difference
!
! PURPOSE:
!   Elemental subroutine to clear the Difference bit flag in an 
!   oSRF object.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Difference( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Difference( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    IF ( .NOT. oSRF_Associated(oSRF) ) RETURN
    CALL oSRF_Clear_Flag( oSRF, DIFFERENCE_FLAG )
  END SUBROUTINE oSRF_Clear_Difference
  

!--------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!   oSRF_Clear_Flags
!
! PURPOSE:
!   Elemental subroutine to clear ALL oSRF flags.
!
! CALLING SEQUENCE:
!   CALL oSRF_Clear_Flags( oSRF )
!
! OBJECTS:
!   oSRF: Object to be altered.
!         UNITS:      N/A
!         TYPE:       oSRF_type
!         DIMENSION:  Scalar or any rank
!         ATTRIBUTES: INTENT(IN OUT)
!
!:sdoc-:
!--------------------------------------------------------------------------------

  ELEMENTAL SUBROUTINE oSRF_Clear_Flags( oSRF )
    TYPE(oSRF_type), INTENT(IN OUT) :: oSRF
    INTEGER :: n
    DO n = BEGIN_FLAG_POSITION, END_FLAG_POSITION
      CALL oSRF_Clear_Flag( oSRF, n )
    END DO
  END SUBROUTINE oSRF_Clear_Flags


  ! ---------------------
  ! Generic flag routines
  ! ---------------------

  ELEMENTAL FUNCTION oSRF_Is_Flag_Set(self, flag) RESULT(is_set)
    TYPE(oSRF_type), INTENT(IN) :: self
    INTEGER        , INTENT(IN) :: flag
    LOGICAL :: is_set
    is_set = BTEST(self%Flags,flag)
  END FUNCTION oSRF_Is_Flag_Set


  ELEMENTAL SUBROUTINE oSRF_Set_Flag(self, flag)
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    INTEGER        , INTENT(IN)     :: flag
    self%Flags = IBSET(self%Flags,flag)
  END SUBROUTINE oSRF_Set_Flag


  ELEMENTAL SUBROUTINE oSRF_Clear_Flag(self, flag)
    TYPE(oSRF_type), INTENT(IN OUT) :: self
    INTEGER        , INTENT(IN)     :: flag
    self%Flags = IBCLR(self%Flags,flag)
  END SUBROUTINE oSRF_Clear_Flag
  
