!*****************************************************************************
!
! MODULE: Utilities_Statistics
!
!   SUBROUTINE: InitStat()
!   SUBROUTINE: AddStat() 
!   SUBROUTINE: CalcStat()
!
!
! Usage)
!
!   1) InitStat() initializes private working variables in this module.
!   2) AddStat() accumulates sample data to the working variables.
!   3) CalcStat() calculates statistics using the accumulated data.
!
!
! Created by Y.Tahara in Aug,02
!
!*****************************************************************************



module Utilities_Statistics

  !--- Modules

  USE Type_Kinds, only : fp_kind
  USE Parameters, only : ZERO, INFINITE


  !--- implicit

  implicit none


  !--- Private & Public

  private
  public InitStat
  public AddStat
  public CalcStat
  public StatResults


  !--- structure definition of a vaiable to keep statistical results

  type StatResults
    real(fp_kind) :: max1
    real(fp_kind) :: max2
    real(fp_kind) :: min1
    real(fp_kind) :: min2
    real(fp_kind) :: mean1
    real(fp_kind) :: mean2
    real(fp_kind) :: sd1
    real(fp_kind) :: sd2
    real(fp_kind) :: corr
    real(fp_kind) :: rmse
    real(fp_kind) :: sderr
    real(fp_kind) :: meanerr
    real(fp_kind) :: covar
  end type StatResults


  !--- global variables

  integer      ,allocatable,save :: numdt(:)
  real(fp_kind),allocatable,save :: sum1(:),     sum2(:)
  real(fp_kind),allocatable,save :: sum_dbl1(:), sum_dbl2(:)
  real(fp_kind),allocatable,save :: sum_mul12(:)
  real(fp_kind),allocatable,save :: mx1(:),      mx2(:)
  real(fp_kind),allocatable,save :: mn1(:),      mn2(:)


  !--- parameters
  
  real(fp_kind),parameter :: RMISS_STAT = -99999._fp_kind


  contains


  !===========================================================================
  !
  ! SUBROUTINE: InitStat()
  !
  !   InitStat() initializes private working variables defined in this
  !   module. The variables are used to accumulate statistical sample
  !   data information.
  !
  !===========================================================================

  subroutine InitStat( num_idx )

    !--- interface

    integer,intent(in) :: num_idx       ! # of statistical categories

    !--- local valiables

    integer,save :: num_idx_last  = -1


    !--- allocate

    if( num_idx  /= num_idx_last )then

      if( num_idx_last > 0 )then
        deallocate( numdt              )
        deallocate( sum1,     sum2     )
        deallocate( sum_dbl1, sum_dbl2 )
        deallocate( sum_mul12          )
        deallocate( mx1,      mx2      )
        deallocate( mn1,      mn2      )
      endif

      allocate( numdt    (num_idx) )
      allocate( sum1     (num_idx) )
      allocate( sum2     (num_idx) )
      allocate( sum_dbl1 (num_idx) )
      allocate( sum_dbl2 (num_idx) )
      allocate( sum_mul12(num_idx) )
      allocate( mx1      (num_idx) )
      allocate( mx2      (num_idx) )
      allocate( mn1      (num_idx) )
      allocate( mn2      (num_idx) )

      num_idx_last  = num_idx

    endif


    !--- Initialize

    numdt    (:) = 0
    sum1     (:) = ZERO
    sum2     (:) = ZERO
    sum_dbl1 (:) = ZERO
    sum_dbl2 (:) = ZERO
    sum_mul12(:) = ZERO
    mx1      (:) = -INFINITE
    mx2      (:) = -INFINITE
    mn1      (:) =  INFINITE
    mn2      (:) =  INFINITE

  end subroutine InitStat



  !===========================================================================
  !
  ! SUBROUTINE: AddStat()
  !
  !   AddStat() accumulates a set of statistical data to working variables.
  !   Before calling it, InitStat() must have been called.
  !
  !===========================================================================

  subroutine AddStat( idx, dx1, dx2 )

    !--- interface

    integer      ,intent(in) :: idx       ! statistical categoly number 
    real(fp_kind),intent(in) :: dx1, dx2  ! data 1 and 2 to be accumulated


    !--- # of samples

    numdt(idx) = numdt(idx) + 1


    !--- accumulate

    sum1     (idx) = sum1     (idx) + dx1
    sum2     (idx) = sum2     (idx) + dx2
    sum_dbl1 (idx) = sum_dbl1 (idx) + dx1*dx1
    sum_dbl2 (idx) = sum_dbl2 (idx) + dx2*dx2
    sum_mul12(idx) = sum_mul12(idx) + dx1*dx2


    !--- check max,min

    mx1(idx) = max( mx1(idx), dx1 )
    mx2(idx) = max( mx2(idx), dx2 )
    mn1(idx) = min( mn1(idx), dx1 )
    mn2(idx) = min( mn2(idx), dx2 )

  end subroutine AddStat



  !===========================================================================
  !
  ! SUBROUTINE: CalcStat()
  !
  !   CalcStat() calculates statistics. The results are stored in
  !   a structural variable, whose structure is defined in this module.
  !
  !===========================================================================

  subroutine CalcStat( idx, num, stat)

    !--- interface    

    integer          ,intent(in)  :: idx        ! statistical categoly number
    integer          ,intent(out) :: num        ! number of sample data
    type(StatResults),intent(out) :: stat       ! results

    !--- local variables

    real(fp_kind) :: rnum
    real(fp_kind) :: mse


    !--- check then number of data in the index    

    num = numdt(idx)

    if( num <= 0 )then
      stat%max1    = RMISS_STAT
      stat%max2    = RMISS_STAT
      stat%min1    = RMISS_STAT
      stat%min2    = RMISS_STAT
      stat%mean1   = RMISS_STAT
      stat%mean2   = RMISS_STAT
      stat%sd1     = RMISS_STAT
      stat%sd2     = RMISS_STAT
      stat%meanerr = RMISS_STAT
      stat%rmse    = RMISS_STAT
      stat%sderr   = RMISS_STAT
      stat%corr    = RMISS_STAT
      stat%covar   = RMISS_STAT
      return
    endif


    !--- # of samples in real
    
    rnum = real( num, fp_kind )
    

    !--- max and min for each

    stat%max1 = mx1(idx)
    stat%max2 = mx2(idx)
    stat%min1 = mn1(idx)
    stat%min2 = mn2(idx)


    !--- mean for each

    stat%mean1 = sum1(idx) / rnum
    stat%mean2 = sum2(idx) / rnum


    !--- mean difference 

    stat%meanerr = stat%mean1 - stat%mean2


    !--- standard deviation for each

    stat%sd1 = sqrt( max( (sum_dbl1(idx)/rnum - stat%mean1**2), ZERO ) )
    stat%sd2 = sqrt( max( (sum_dbl2(idx)/rnum - stat%mean2**2), ZERO ) )


    !--- RMS and SD difference

    mse = (sum_dbl1(idx) - 2*sum_mul12(idx) + sum_dbl2(idx)) / rnum

    stat%rmse  = sqrt( max( (mse                               ), ZERO ) )
    stat%sderr = sqrt( max( (mse - (stat%mean1 - stat%mean2)**2), ZERO ) )


    !--- covariance    

    stat%covar = sum_mul12(idx)/rnum - stat%mean1*stat%mean2


    !--- correlation    

    if( stat%sd1 == ZERO  .or.  stat%sd2 == ZERO )then
      stat%corr = ZERO
    else
      stat%corr = stat%covar / (stat%sd1 * stat%sd2)
    endif


  end subroutine CalcStat

end module Utilities_Statistics
