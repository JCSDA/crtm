!====================================================================
!
! MODULE: CalcRegCoef
!
!   SUBROUTINE: Calc_RegCoef()
!
!
! Created by Y.Tahara in Aug,02
!
!====================================================================


module CalcRegCoef

  !--- module

  use type_kinds, only : fp_kind
  use ParametersGenCoef
  use PredictandPredictor


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public Calc_RegCoef


 contains


  !====================================================================
  !
  ! SUBROUTINE: Calc_RegCoef()
  !
  !   Calc_RegCoef() generates regression coefficients, which are
  !   called OPTRAN transmittance coefficients.
  !
  !====================================================================

  subroutine Calc_RegCoef( Natmpred,       &
                           predcomb,       &
                           Nsample,        &
                           atmpred_lay,    &
                           predictand_lay, &
                           abslev_lay,     &
                           sqrtwgt_lay,    &
                           Npolyorder,     &
                           Ncoef,          &
                           coef, residual  )

    !--- interface

    integer      ,intent(in)  :: Natmpred                                       ! # of atmos predictors
    integer      ,intent(in)  :: predcomb   (Natmpred_maxused)                  ! an atmos predictor index set
    integer      ,intent(in)  :: Nsample                                        ! # of regression samples
    real(fp_kind),intent(in)  :: atmpred_lay(Natmpred_max,Nlay,Nangle,Natm)     ! atmos predictors
    real(fp_kind),intent(in)  :: predictand_lay(Nlay,Nangle,Natm)               ! predictands
    real(fp_kind),intent(in)  :: abslev_lay    (Nlay,Nangle,Natm)               ! absorber amount levels on layers
    real(fp_kind),intent(in)  :: sqrtwgt_lay   (Nlay,Nangle,Natm)       ! regression weights
    integer      ,intent(in)  :: Npolyorder    
    integer      ,intent(in)  :: Ncoef   
    real(fp_kind),intent(out) :: coef(Ncoef)                    ! regression coefficients
    real(fp_kind),intent(out) :: residual                       ! residual of LSM

    !--- local variables
 
    real(fp_kind),allocatable,save :: x(:,:)     ! Predictors matrix
    real(fp_kind),allocatable,save :: y(:)       ! Predictand vector
    real(fp_kind)                  :: x1(Ncoef)
    real(fp_kind)                  :: y1

    integer                 :: Nel
    integer,save            :: Nel_last  = -1
    integer                 :: Isam
    integer,save            :: Nsam_last = -1

    real(fp_kind),parameter :: tau = ZERO
    real(fp_kind)           :: aux
    integer                 :: k

    integer                 :: Iatm, Iang, Ilay


    !--- check # of predictors
    
    if( Natmpred < 0 )then
      coef(:)  = ZERO
      coef(1)  = - LOG_INF
      residual = - ONE
      return
    endif


    !--- # of regression predictors
    
    Nel = ( Natmpred + 1 ) * (Npolyorder + 1)

    
    !--- allocate if necessary
    
    if( Nel /= Nel_last .or. Nsample /= Nsam_last )then

      if( Nel_last > 0 )then
        deallocate( x )
        deallocate( y )
      endif

      Nel_last  = Nel
      Nsam_last = Nsample

      allocate( x(Nsample,Nel) )
      allocate( y(Nsample)     )

    endif


    !--- create matrix

    Isam = 0

    do Iatm = 1, Natm
    do Iang = 1, Nangle
    do Ilay = 1, Nlay


      !--- predictand

      y1 = predictand_lay(Ilay,Iang,Iatm)

      if( y1 < HMISS )cycle


      !--- increment # of samples

      Isam = Isam + 1


      !--- regression predictors

      call Get_RegPredictor( Natmpred, predcomb, &
                             atmpred_lay(1,Ilay,Iang,Iatm) , &
                             abslev_lay   (Ilay,Iang,Iatm) , &
                             Npolyorder, &
                             x1 )


      !--- apply weight

      y(Isam)       = y1        * sqrtwgt_lay(Ilay,Iang,Iatm)
      x(Isam,1:Nel) = x1(1:Nel) * sqrtwgt_lay(Ilay,Iang,Iatm)


    enddo
    enddo
    enddo
     

    !--- check if the number of samples is consistent

    if( Isam /= Nsample )then
      print *, '### ERROR ###'
      print *, '# of regression samples is not consistent'
      print *, 'Assigned number :', Nsample
      print *, 'Counted number  :', Isam
      stop 90
    endif


    !--- generate coefficients by least square method

!    call DGELLS( 1, x, Nsample, y, Nsample, coef, Nel, residual, tau, Nsample, Nel, 1, k, aux, 0 )

    !--- wraper for LAPACK's DGELS routine.  Yong Han March 2003.

    call DGELS_LAPACK(Nsample, Nel, x, y, coef, residual)
 
CONTAINS

!================================================================
!  A wrapper for calling LAPACK DGELS, which calculates real linear
!  system.
!    
!    Yong Han, March 2003
!================================================================  
 
  subroutine DGELS_LAPACK(Nsample, Nel, x, y, coef, residual)
    
    integer, intent(in) :: Nsample   ! number of samples
    integer, intent(in) :: Nel       ! number of predictors
    real(fp_kind), intent(in) :: x(Nsample,Nel)  ! Predictors matrix
    real(fp_kind), intent(in) :: y(Nsample)      ! Predictand vector
    real(fp_kind), intent(inout) :: coef(Nel)    ! regression coefficients
    real(fp_kind), intent(inout) :: residual  ! residual of LSM
        
    
    !--- local variables
    real(fp_kind)  :: xwork(Nsample, Nel)
    real(fp_kind)  :: ywork(Nsample, 1)
    real(fp_kind)  :: work(2*Nsample)
    real(fp_kind)  :: diff
    integer        :: i, Info
    
    
    if (Nsample < 1 .or. Nel < 1) then
      print *, 'DGELS_LAPACK either Nsample or Nel has a value less than 1'
      stop 99
    end if
    
    !--- copy data
    xwork(:, :) = x(:, :)
    ywork(:, 1) = y(:)
    
    call DGELS('N', Nsample, Nel, 1, xwork, Nsample, ywork, &
                Nsample, work, 2*Nsample, Info)

    if(Info /= 0)then
      print *, 'DGELS_LAPACK: error from DGELS'
      stop 99
    endif

    coef(:) = ywork(:, 1)

    residual = 0.
    do i = 1, Nsample
        diff = y(i) - SUM(coef(:) * x(i, :))
        residual = residual + diff*diff
    end do
    residual = sqrt(residual / Nsample)
    
  end subroutine DGELS_LAPACK

  end subroutine Calc_RegCoef

end module CalcRegCoef
