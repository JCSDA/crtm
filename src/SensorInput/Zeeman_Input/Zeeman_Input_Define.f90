!
! Zeeman_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to Zeeman
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 26-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE Zeeman_Input_Define

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
  PUBLIC :: Zeeman_Input_type
  PUBLIC :: Zeeman_Input_Get_Property
  PUBLIC :: Zeeman_Input_Set_Property
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: Zeeman_Input_Inspect


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE Zeeman_Input_Assign
  END INTERFACE ASSIGNMENT(=)
  
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE Zeeman_Input_Equal
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
  TYPE :: Zeeman_Input_type
    PRIVATE
    ! Earth magnetic field strength in Gauss
    REAL(fp) :: Be = DEFAULT_MAGENTIC_FIELD
    ! Cosine of the angle between the Earth
    ! magnetic field and wave propagation direction                     
    REAL(fp) :: Cos_ThetaB = ZERO
    ! cosine of the azimuth angle of the Be vector in the
    ! (v, h, k) coordinates system, where v, h and k comprise                       
    ! a right-hand orthogonal system, similar to the (x, y, z)                      
    ! Catesian coordinates. The h vector is normal to the                           
    ! plane containing the k and z vectors, where k points                          
    ! to the wave propagation direction and z points                                
    ! to the zenith. h = (z cross k)/|z cross k|. The                               
    ! azimuth angle is the angle on the (v, h) plane                                
    ! from the positive v axis to the projected line of the                         
    ! Be vector on this plane, positive counterclockwise.                           
    REAL(fp) :: Cos_PhiB   = ZERO
    ! Doppler frequency shift caused by Earth-rotation in KHz
    ! (positive towards sensor). A zero value means no frequency shift.
    REAL(fp) :: Doppler_Shift = ZERO 
  END TYPE Zeeman_Input_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE Zeeman_Input_Get_Property( &
    Zeeman_Input   , &
    Field_Strength , &
    Cos_ThetaB     , &
    Cos_PhiB       , & 
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type),INTENT(IN)  :: Zeeman_Input
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Field_Strength
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_ThetaB    
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Cos_PhiB    
    REAL(fp),     OPTIONAL, INTENT(OUT) :: Doppler_Shift 
    ! Get components
    IF ( PRESENT(Field_Strength) ) Field_Strength = Zeeman_Input%Be
    IF ( PRESENT(Cos_ThetaB    ) ) Cos_ThetaB     = Zeeman_Input%Cos_ThetaB    
    IF ( PRESENT(Cos_PhiB      ) ) Cos_PhiB       = Zeeman_Input%Cos_PhiB    
    IF ( PRESENT(Doppler_Shift ) ) Doppler_Shift  = Zeeman_Input%Doppler_Shift 
  END SUBROUTINE Zeeman_Input_Get_Property  

  SUBROUTINE Zeeman_Input_Set_Property ( &
    Zeeman_Input   , &
    Field_Strength , &
    Cos_ThetaB     , &
    Cos_PhiB       , & 
    Doppler_Shift   )
    ! Arguments
    TYPE(Zeeman_Input_type), INTENT(IN OUT) :: Zeeman_Input
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Field_Strength
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_ThetaB    
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Cos_PhiB    
    REAL(fp),      OPTIONAL, INTENT(IN)     :: Doppler_Shift 
    ! Set components
    IF ( PRESENT(Field_Strength) ) Zeeman_Input%Be            = Field_Strength
    IF ( PRESENT(Cos_ThetaB    ) ) Zeeman_Input%Cos_ThetaB    = Cos_ThetaB    
    IF ( PRESENT(Cos_PhiB      ) ) Zeeman_Input%Cos_PhiB      = Cos_PhiB    
    IF ( PRESENT(Doppler_Shift ) ) Zeeman_Input%Doppler_Shift = Doppler_Shift 
  END SUBROUTINE Zeeman_Input_Set_Property   


  SUBROUTINE Zeeman_Input_Inspect(x)
    TYPE(Zeeman_Input_type), INTENT(IN) :: x
    ! Display components
    WRITE(*, '(5x,"Zeeman_Input field strength (gauss):",1x,es22.15)') x%Be           
    WRITE(*, '(5x,"Zeeman_Input COS(ThetaB)           :",1x,es22.15)') x%Cos_ThetaB   
    WRITE(*, '(5x,"Zeeman_Input COS(PhiB)             :",1x,es22.15)') x%Cos_PhiB   
    WRITE(*, '(5x,"Zeeman_Input Doppler shift (KHz)   :",1x,es22.15)') x%Doppler_Shift
  END SUBROUTINE Zeeman_Input_Inspect


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE Zeeman_Input_Assign(lhs, rhs)
    TYPE(Zeeman_Input_type), INTENT(OUT) :: lhs
    TYPE(Zeeman_Input_type), INTENT(IN)  :: rhs
    lhs%Be            = rhs%Be           
    lhs%Cos_ThetaB    = rhs%Cos_ThetaB   
    lhs%Cos_PhiB      = rhs%Cos_PhiB   
    lhs%Doppler_Shift = rhs%Doppler_Shift
  END SUBROUTINE Zeeman_Input_Assign
  

  ELEMENTAL FUNCTION Zeeman_Input_Equal(x, y) RESULT(is_equal)
    TYPE(Zeeman_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = .FALSE.
    IF ( (x%Be            .EqualTo. y%Be           ) .AND. &
         (x%Cos_ThetaB    .EqualTo. y%Cos_ThetaB   ) .AND. &
         (x%Cos_PhiB      .EqualTo. y%Cos_PhiB     ) .AND. &
         (x%Doppler_Shift .EqualTo. y%Doppler_Shift)       ) is_equal = .TRUE.
  END FUNCTION Zeeman_Input_Equal
  
END MODULE Zeeman_Input_Define
