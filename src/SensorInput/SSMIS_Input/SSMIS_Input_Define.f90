!
! SSMIS_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to SSMIS
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE SSMIS_Input_Define

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp
  USE Compare_Float_Numbers, ONLY: OPERATOR(.EqualTo.)
  
  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: SSMIS_Input_type
  PUBLIC :: SSMIS_Input_Get_Property
  PUBLIC :: SSMIS_Input_Set_Property
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: SSMIS_Input_Inspect


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE SSMIS_Input_Assign
  END INTERFACE ASSIGNMENT(=)
  
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE SSMIS_Input_Equal
  END INTERFACE OPERATOR(==)


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'
  ! Initialisation constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: DEFAULT_MAGENTIC_FIELD = 0.3_fp
  

  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: SSMIS_Input_type
    PRIVATE
    ! Earth magnetic field strength in Gauss
    REAL(fp) :: Be = DEFAULT_MAGENTIC_FIELD
    ! Cosine of the angle between the Earth
    ! magnetic field and wave propagation direction                     
    REAL(fp) :: Cos_ThetaB = ZERO
    ! Doppler frequency shift caused by Earth-rotation in KHz
    ! (positive towards sensor). A zero value means no frequency shift.
    REAL(fp) :: Doppler_Shift = ZERO 
  END TYPE SSMIS_Input_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE SSMIS_Input_Get_Property( &
    SSMIS_Input   , &
    Field_Strength, &
    Cos_ThetaB    , &
    Doppler_Shift   )
    ! Arguments
    TYPE(SSMIS_Input_type), INTENT(IN)  :: SSMIS_Input
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Field_Strength
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_ThetaB    
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Doppler_Shift 
    ! Get components
    IF ( PRESENT(Field_Strength) ) Field_Strength = SSMIS_Input%Be
    IF ( PRESENT(Cos_ThetaB    ) ) Cos_ThetaB     = SSMIS_Input%Cos_ThetaB    
    IF ( PRESENT(Doppler_Shift ) ) Doppler_Shift  = SSMIS_Input%Doppler_Shift 
  END SUBROUTINE SSMIS_Input_Get_Property  

  SUBROUTINE SSMIS_Input_Set_Property ( &
    SSMIS_Input   , &
    Field_Strength, &
    Cos_ThetaB    , &
    Doppler_Shift   )
    ! Arguments
    TYPE(SSMIS_Input_type), INTENT(IN OUT) :: SSMIS_Input
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Field_Strength
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Cos_ThetaB    
    REAL(fp),     OPTIONAL, INTENT(IN)     :: Doppler_Shift 
    ! Set components
    IF ( PRESENT(Field_Strength) ) SSMIS_Input%Be            = Field_Strength
    IF ( PRESENT(Cos_ThetaB    ) ) SSMIS_Input%Cos_ThetaB    = Cos_ThetaB    
    IF ( PRESENT(Doppler_Shift ) ) SSMIS_Input%Doppler_Shift = Doppler_Shift 
  END SUBROUTINE SSMIS_Input_Set_Property   


  SUBROUTINE SSMIS_Input_Inspect(x)
    TYPE(SSMIS_Input_type), INTENT(IN) :: x
    ! Display components
    WRITE(*, '(5x,"SSMIS_Input field strength (gauss):",1x,es22.15)') x%Be           
    WRITE(*, '(5x,"SSMIS_Input COS(ThetaB)           :",1x,es22.15)') x%Cos_ThetaB   
    WRITE(*, '(5x,"SSMIS_Input Doppler shift (KHz)   :",1x,es22.15)') x%Doppler_Shift
  END SUBROUTINE SSMIS_Input_Inspect


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE SSMIS_Input_Assign(lhs, rhs)
    TYPE(SSMIS_Input_type), INTENT(OUT) :: lhs
    TYPE(SSMIS_Input_type), INTENT(IN)  :: rhs
    lhs%Be            = rhs%Be           
    lhs%Cos_ThetaB    = rhs%Cos_ThetaB   
    lhs%Doppler_Shift = rhs%Doppler_Shift
  END SUBROUTINE SSMIS_Input_Assign
  

  ELEMENTAL FUNCTION SSMIS_Input_Equal(x, y) RESULT(is_equal)
    TYPE(SSMIS_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = .FALSE.
    IF ( (x%Be            .EqualTo. y%Be           ) .AND. &
         (x%Cos_ThetaB    .EqualTo. y%Cos_ThetaB   ) .AND. &
         (x%Doppler_Shift .EqualTo. y%Doppler_Shift)       ) is_equal = .TRUE.
  END FUNCTION SSMIS_Input_Equal
  
END MODULE SSMIS_Input_Define
