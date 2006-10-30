!====================================================================
!
! MODULE: LeastSquareProblem
!
!   SUBROUTINE: Add_LsmSample()
!   SUBROUTINE: Solve_Lsm()
!
!
! Created by Y.Tahara in Aug,02
!
!====================================================================


module LeastSquareProblem

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef


  !--- implicit

  implicit none


  !--- private & public

  private
  public Add_LsmSample
  public Solve_Lsm


  !--- module valiables in private

  integer                  ,save :: Nelem_lsm
  integer                  ,save :: Nsample_lsm
  real(fp_kind),allocatable,save :: y(:)     ! Predictand vector
  real(fp_kind),allocatable,save :: x(:,:)   ! Predictors matrix


 contains

  !====================================================================
  ! 
  ! SUBROUTINE: Alloc_LsmHeap()
  ! SUBROUTINE: Dealloc_LsmHeap()
  ! 
  !   
  ! 
  ! 
  ! 
  !====================================================================

  subroutine Alloc_LsmHeap( Nelem_in, Nsample_max )

    integer,intent(in) :: Nelem_in
    integer,intent(in) :: Nsample_max

    Nelem_lsm   = Nelem_in
    Nsample_lsm = 0
    
    allocate( y(          Nsample_max) )
    allocate( x(Nelem_lsm,Nsample_max) )
  
  end subroutine Alloc_LsmHeap
   
   
  subroutine Dealloc_LsmHeap()

    deallocate( y )
    deallocate( x )
  
  end subroutine Dealloc_LsmHeap



  !====================================================================
  ! 
  ! SUBROUTINE: Add_LsmSample()
  ! 
  !   Add_LsmSample() adds a set of a predictand and predictors to
  !   module variables, which will be used in Solve_Lsm() to
  !   generate regression coefficients.
  ! 
  !====================================================================

  subroutine Add_LsmSample( yy, xx )

    !--- interface

    real(fp_kind),intent(in)    :: yy              ! a predictand
    real(fp_kind),intent(in)    :: xx(Nelem_lsm)   ! predictors

    !--- add a set to module variables

    Nsample_lsm = Nsample_lsm + 1

    y(            Nsample_lsm) = yy       
    x(1:Nelem_lsm,Nsample_lsm) = xx(1:Nelem_lsm)

  end subroutine Add_LsmSample



  !====================================================================
  ! 
  ! SUBROUTINE: Solve_Lsm()
  ! 
  !   Solve_Lsm() calculates regression coefficients by solving 
  !   a least square problem.
  !   Sets of predictand and predictor must have been stored in
  !   module variables by calling Add_LsmSample().
  !   Mathmatical library is used for the calculation.   
  ! 
  !====================================================================

  subroutine Solve_Lsm( c, rn, Nda )

    !--- interface

    real(fp_kind),intent(out) :: c(Nel)   ! coefficients
    real(fp_kind),intent(out) :: rn       ! residual of LSM
    integer      ,intent(out) :: Nda      ! total # of sets in module variables

    !--- local variables

    real(fp_kind),parameter   :: tau = ZERO

    real(fp_kind),allocatable :: x1(:,:)
    real(fp_kind)             :: aux
    integer                   :: k


    !--- allocate
    
    allocate( x1(Nsample_lsm,Nelem_lsm) )


    !--- transpose the predictor matrix

    call DGETMO( x(1,1), Nelem_lsm, Nelem_lsm, Nsample_lsm, x1(1,1), Nsampel_lsm ) 


    !--- generate regression coefficients

    CALL DGELLS( 1, x1, Nsample_lsm, y, Nsample_lsm, c, Nelem_lsm, rn, tau, Nsample_lsm, Nelem_lsm, 1, k, aux, 0 )


    !--- deallocate
    
    deallocate( x1 )


  end subroutine Solve_Lsm

end module LeastSquareProblem
