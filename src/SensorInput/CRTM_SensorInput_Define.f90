!
! CRTM_SensorInput_Define
!
! Module defining the CRTM SensorInput container structure to hold
! all the various sensor specific input data.
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct. 15, 2009
!                       yong.han@noaa.gov
!
!                       Paul van Delst, 27-Oct-2009
!                       paul.vandelst@noaa.gov

MODULE CRTM_SensorInput_Define

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module use
  USE SSU_Input_Define,   ONLY: SSU_Input_type, &
                                ASSIGNMENT(=), OPERATOR(==), &
                                SSU_Input_Get_Property, &
                                SSU_Input_Set_Property, &
                                SSU_Input_Inspect
  USE Zeeman_Input_Define,ONLY: Zeeman_Input_type, &
                                ASSIGNMENT(=), OPERATOR(==), &
                                Zeeman_Input_Get_Property, &
                                Zeeman_Input_Set_Property, &
                                Zeeman_Input_Inspect

  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: CRTM_SensorInput_type
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: CRTM_SensorInput_Get
  PUBLIC :: CRTM_SensorInput_Set
  PUBLIC :: CRTM_SensorInput_Inspect
  PUBLIC :: CRTM_SensorInput_Get_Property
  PUBLIC :: CRTM_SensorInput_Set_Property
  ! Inherited types
  PUBLIC :: SSU_Input_type
  PUBLIC :: Zeeman_Input_type


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE CRTM_SensorInput_Assign
  END INTERFACE ASSIGNMENT(=)
  
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE CRTM_SensorInput_Equal
  END INTERFACE OPERATOR(==)
  
  INTERFACE CRTM_SensorInput_Get_Property
    MODULE PROCEDURE SSU_Input_Get_Property
    MODULE PROCEDURE Zeeman_Input_Get_Property
  END INTERFACE CRTM_SensorInput_Get_Property
  
  INTERFACE CRTM_SensorInput_Set_Property
    MODULE PROCEDURE SSU_Input_Set_Property
    MODULE PROCEDURE Zeeman_Input_Set_Property
  END INTERFACE CRTM_SensorInput_Set_Property
  
  
  ! --------------------
  ! Container definition
  ! --------------------
  TYPE :: CRTM_SensorInput_type
    TYPE(SSU_Input_type)   :: SSU
    TYPE(Zeeman_Input_type) :: Zeeman
  END TYPE CRTM_SensorInput_type


CONTAINS


  SUBROUTINE CRTM_SensorInput_Get( &
    SensorInput, &
    SSU  , &
    Zeeman  )
    ! Arguments
    TYPE(CRTM_SensorInput_type),      INTENT(IN)     :: SensorInput
    TYPE(SSU_Input_type),   OPTIONAL, INTENT(IN OUT) :: SSU
    TYPE(Zeeman_Input_type),OPTIONAL, INTENT(IN OUT) :: Zeeman
    ! Get components
    IF ( PRESENT(SSU)   ) SSU   = SensorInput%SSU
    IF ( PRESENT(Zeeman) ) Zeeman = SensorInput%Zeeman
  END SUBROUTINE CRTM_SensorInput_Get


  SUBROUTINE CRTM_SensorInput_Set( &
    SensorInput, &
    SSU  , &
    Zeeman  )
    ! Arguments
    TYPE(CRTM_SensorInput_type),      INTENT(IN OUT) :: SensorInput
    TYPE(SSU_Input_type),   OPTIONAL, INTENT(IN)     :: SSU
    TYPE(Zeeman_Input_type),OPTIONAL, INTENT(IN)     :: Zeeman
    ! Get components
    IF ( PRESENT(SSU)   ) SensorInput%SSU   = SSU
    IF ( PRESENT(Zeeman) ) SensorInput%Zeeman = Zeeman
  END SUBROUTINE CRTM_SensorInput_Set


  SUBROUTINE CRTM_SensorInput_Inspect(SensorInput)
    TYPE(CRTM_SensorInput_type), INTENT(IN) :: SensorInput
    WRITE(*, '(3x,"CRTM_SensorInput components:")')
    CALL SSU_Input_Inspect(SensorInput%SSU)
    CALL Zeeman_Input_Inspect(SensorInput%Zeeman)
  END SUBROUTINE CRTM_SensorInput_Inspect


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE CRTM_SensorInput_Assign(lhs, rhs)
    TYPE(CRTM_SensorInput_type), INTENT(OUT) :: lhs
    TYPE(CRTM_SensorInput_type), INTENT(IN)  :: rhs
    lhs%SSU    = rhs%SSU
    lhs%Zeeman  = rhs%Zeeman
  END SUBROUTINE CRTM_SensorInput_Assign
  

  ELEMENTAL FUNCTION CRTM_SensorInput_Equal(x, y) RESULT(is_equal)
    TYPE(CRTM_SensorInput_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = .FALSE.
    IF ( (x%SSU == y%SSU) .AND. &
         (x%Zeeman == y%Zeeman) ) is_equal = .TRUE.
  END FUNCTION CRTM_SensorInput_Equal
         
END MODULE CRTM_SensorInput_Define
