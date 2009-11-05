!
! SSU_Input_Define
!
! Module containing the structure definition and associated routines
! for CRTM inputs specific to SSU
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Oct 6, 2009
!                       yong.han@noaa.gov
!
!                       Paul van Delst, 20-Oct-2009
!                       paul.vandelst@noaa.gov
!

MODULE SSU_Input_Define

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
  PUBLIC :: SSU_Input_type
  PUBLIC :: SSU_Input_Get_Property
  PUBLIC :: SSU_Input_Set_Property
  PUBLIC :: SSU_Input_Cell_Pressure_Set
  PUBLIC :: ASSIGNMENT(=)
  PUBLIC :: OPERATOR(==)
  PUBLIC :: SSU_Input_Inspect


  ! -------------------
  ! Procedure overloads
  ! -------------------
  INTERFACE ASSIGNMENT(=)
    MODULE PROCEDURE SSU_Input_Assign
  END INTERFACE ASSIGNMENT(=)
  
  INTERFACE OPERATOR(==)
    MODULE PROCEDURE SSU_Input_Equal
  END INTERFACE OPERATOR(==)

  
  ! -----------------
  ! Module parameters
  ! -----------------
  INTEGER,  PARAMETER :: MAX_N_CHANNELS = 3
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  
  
  !--------------------
  ! Structure defintion
  !--------------------
  !:tdoc+:
  TYPE :: SSU_Input_type
    PRIVATE
    ! Time in decimal year (e.g. 2009.08892694 corresponds to 11:00 Feb. 2, 2009)
    REAL(fp) :: Time = ZERO
    ! SSU CO2 cell pressures (hPa)
    REAL(fp) :: Cell_Pressure(MAX_N_CHANNELS) = ZERO
  END TYPE SSU_Input_type
  !:tdoc-:


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE SSU_Input_Get_Property( &
    SSU_Input     , &
    ChannelIndex  , &
    Time          , &
    Cell_Pressures, &
    Cell_Pressure , &
    n_Channels      )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN)  :: SSU_Input
    INTEGER,    OPTIONAL, INTENT(IN)  :: ChannelIndex
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Time
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Cell_Pressures(:)
    REAL(fp),   OPTIONAL, INTENT(OUT) :: Cell_Pressure
    INTEGER,    OPTIONAL, INTENT(OUT) :: n_Channels
    ! Variables
    INTEGER :: n
    ! Get components
    ! ...The mission time
    IF ( PRESENT(Time) ) Time = SSU_Input%Time
    ! ...ALL the channel cell pressures
    IF ( PRESENT(Cell_Pressures) ) THEN
      Cell_Pressures = ZERO
      n = SIZE(Cell_Pressures)
      Cell_Pressures(1:MIN(n,MAX_N_CHANNELS)) = SSU_Input%Cell_Pressure(1:MIN(n,MAX_N_CHANNELS))
    END IF
    ! ...Channel-specific cell pressure
    IF ( PRESENT(Cell_Pressure) ) THEN
      n = 1
      IF ( PRESENT(ChannelIndex) ) THEN
        IF ( ChannelIndex >= 1 .AND. ChannelIndex <= MAX_N_CHANNELS ) n = ChannelIndex
      END IF
      Cell_Pressure = SSU_Input%Cell_Pressure(n)
    END IF
    ! ...The array component dimension
    IF ( PRESENT(n_Channels) ) n_Channels = MAX_N_CHANNELS
  END SUBROUTINE SSU_Input_Get_Property  

  SUBROUTINE SSU_Input_Set_Property ( &
    SSU_Input     , &
    ChannelIndex  , &
    Time          , &
    Cell_Pressures, &
    Cell_Pressure   )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN OUT) :: SSU_Input
    INTEGER,    OPTIONAL, INTENT(IN)     :: ChannelIndex
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Time
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Cell_Pressures(:)
    REAL(fp),   OPTIONAL, INTENT(IN)     :: Cell_Pressure
    ! Variables
    INTEGER :: n
    ! Set components
    ! ...The mission time
    IF ( PRESENT(Time) ) SSU_Input%Time = Time 
    ! ...ALL the channel cell pressures
    IF ( PRESENT(Cell_Pressures) ) THEN
      SSU_Input%Cell_Pressure = ZERO
      n = SIZE(Cell_Pressures)
      SSU_Input%Cell_Pressure(1:MIN(n,MAX_N_CHANNELS)) = Cell_Pressures(1:MIN(n,MAX_N_CHANNELS))
    END IF
    ! ...Channel-specific cell pressure
    IF ( PRESENT(Cell_Pressure) ) THEN
      n = 1
      IF ( PRESENT(ChannelIndex) ) THEN
        IF ( ChannelIndex >= 1 .AND. ChannelIndex <= MAX_N_CHANNELS ) n = ChannelIndex
      END IF
      SSU_Input%Cell_Pressure(n) = Cell_Pressure
    END IF
  END SUBROUTINE SSU_Input_Set_Property   

  FUNCTION SSU_Input_Cell_Pressure_Set( &
    SSU_Input ) &
  RESULT( Are_Set )
    ! Arguments
    TYPE(SSU_Input_type), INTENT(IN) :: SSU_Input
    ! Function result
    LOGICAL :: Are_Set
    ! Test cell pressures
    IF ( ALL(SSU_Input%Cell_Pressure > ZERO) ) THEN
      Are_Set = .TRUE.
    ELSE
      Are_Set = .FALSE.
    END IF
  END FUNCTION SSU_Input_Cell_Pressure_Set


  SUBROUTINE SSU_Input_Inspect(x)
    TYPE(SSU_Input_type), INTENT(IN) :: x
    ! Display components
    WRITE(*, '(5x,"SSU_Input time:",1x,es22.15)') x%Time
    WRITE(*, '(5x,"SSU_Input cell pressures:",10(1x,es22.15,:))') x%Cell_Pressure
  END SUBROUTINE SSU_Input_Inspect


!################################################################################
!################################################################################
!##                                                                            ##
!##                        ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

  SUBROUTINE SSU_Input_Assign(lhs, rhs)
    TYPE(SSU_Input_type), INTENT(OUT) :: lhs
    TYPE(SSU_Input_type), INTENT(IN)  :: rhs
    lhs%Time          = rhs%Time
    lhs%Cell_Pressure = rhs%Cell_Pressure
  END SUBROUTINE SSU_Input_Assign
  

  ELEMENTAL FUNCTION SSU_Input_Equal(x, y) RESULT(is_equal)
    TYPE(SSU_Input_type), INTENT(IN) :: x, y
    LOGICAL :: is_equal
    is_equal = .FALSE.
    IF ( (x%Time .EqualTo. y%Time) .AND. &
         ALL(x%Cell_Pressure .EqualTo. y%Cell_Pressure) ) is_equal = .TRUE.
  END FUNCTION SSU_Input_Equal
  
END MODULE SSU_Input_Define
