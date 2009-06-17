!====================================================================
!
! MODULE: PredAbsCoefTransTemp
!
!   SUBROUTINE: Pred_AbsCoefTransTemp()
!   SUBROUTINE: Pred_AbsCoef()
!   SUBROUTINE: Pred_Trans()
!   FUNCTION:   Pred_BrightTemp()
!
!
! Created by Y.Tahara in Aug,02
! Modified by Y.Tahara in Oct,02 to add Pred_AbsCoefTransTemp()
!
!====================================================================

module PredAbsCoefTransTemp


  !--- modules

  use type_kinds, only : fp_kind, Quad
  use ParametersGenCoef
  use PredictandPredictor
  use PlanckFunc


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public Pred_AbsCoefTransTemp
  Public Pred_AbsCoef
  Public Pred_Trans
  Public Pred_BrightTemp


 contains


  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: Pred_AbsCoefTransTemp
  !
  !   Pred_AbsCoefTransTemp predicts absorption coefficnets,
  !   transmittances and brightness temperatures.
  !
  !--------------------------------------------------------------------------

  subroutine Pred_AbsCoefTransTemp( &
                Ichan,           &
                Natmpred,        &
                predcomb,        &
                atmpred_lay,     &
                absamount_lev,   &
                abslev_lay,      &
                alltrans_lev,    &
                efctrans_lev,    &
                t_lay,           &
                tb_lbl,          &
                Npolyorder,      &
                Ncoef,           &
                coef,            &
                calabscoef_lay,  &
                caltrans_lev,    &
                calalltrans_lev, &
                tb_cal,          &
                max_atmpred_term )

    !--- interface

    integer        ,intent(in)  :: Ichan                                        ! channel seq #
    integer        ,intent(in)  :: Natmpred                                     ! # of atmos predictors used in prediction
    integer        ,intent(in)  :: predcomb   (Natmpred_maxused)                ! an atmos predictor index set
    real(fp_kind)  ,intent(in)  :: atmpred_lay(Natmpred_max,Nlay,Nangle,Natm)   ! atmos predictors
    real(fp_kind)  ,intent(in)  :: absamount_lev(0:Nlay,Nangle,Natm)            ! absorber amounts on levels
    real(fp_kind)  ,intent(in)  :: abslev_lay   (  Nlay,Nangle,Natm)            ! absorber amount levels on layers
    real(fp_kind)  ,intent(in)  :: alltrans_lev (0:Nlay,Nangle,Natm)            ! LBL total trans
    real(fp_kind)  ,intent(in)  :: efctrans_lev (0:Nlay,Nangle,Natm)            ! effective trans for other two gases
    real(fp_kind)  ,intent(in)  :: t_lay (Nlay,Natm)                            ! temperature (K)
    real(fp_kind)  ,intent(in)  :: tb_lbl(Nangle,Natm)                          ! LBL Tb (K)
    integer        ,intent(in)  :: Npolyorder
    integer        ,intent(in)  :: Ncoef
    real(fp_kind)  ,intent(in)  :: coef(Ncoef)                                  ! regression coefficients

    real(fp_kind)  ,intent(out) :: calabscoef_lay (  Nlay,Nangle,Natm)          ! predicted absorption coef
    real(fp_kind)  ,intent(out) :: caltrans_lev   (0:Nlay,Nangle,Natm)          ! predicted specified gas trans
    real(fp_kind)  ,intent(out) :: calalltrans_lev(0:Nlay,Nangle,Natm)          ! predicted total trans
    real(fp_kind)  ,intent(out) :: tb_cal(Nangle,Natm)                          ! predicted Tb (K)
    real(fp_kind)  ,intent(out) :: max_atmpred_term                             ! max of atmos pred term to watch sensitivity

    !--- local variables

    real(fp_kind)               :: max_atmpred_term_lay(Nlay)
    integer                     :: Iang, Iatm, Ilay


    !--- predict absorption coef, trans, Tb

    max_atmpred_term = ZERO
    
    do Iatm = 1, Natm
    do Iang = 1, Nangle


      !--- predict absorption coef

      call Pred_AbsCoef( Natmpred, predcomb,          &
                         atmpred_lay (1,1,Iang,Iatm), &
                         abslev_lay    (1,Iang,Iatm), &
                         Npolyorder,                  &
                         coef,                        &
                         calabscoef_lay(1,Iang,Iatm), &
                         max_atmpred_term_lay         )
                         

      !--- predict gas trans

      call Pred_Trans( absamount_lev (0,Iang,Iatm), &
                       calabscoef_lay(1,Iang,Iatm), &
                       caltrans_lev  (0,Iang,Iatm)  )


      !--- predict total transmittances

      calalltrans_lev(0:,Iang,Iatm) &
                = caltrans_lev(0:,Iang,Iatm) * efctrans_lev(0:,Iang,Iatm)


      !--- predict brightness temperature

      tb_cal(Iang,Iatm) = Pred_BrightTemp( Ichan,                        &
                                           calalltrans_lev(0,Iang,Iatm), &
                                           t_lay(1   ,Iatm),             &
                                           t_lay(Nlay,Iatm)              )

      !--- update max atmos pred term

      max_atmpred_term = ZERO

      do Ilay = 1, Nlay
        if( alltrans_lev(Ilay,Iang,Iatm) < MinTrans_SenseCheck ) exit
        
        if( max_atmpred_term_lay(Ilay) > max_atmpred_term )then
          max_atmpred_term = max_atmpred_term_lay(Ilay)
        endif
      enddo

    enddo
    enddo

  end subroutine Pred_AbsCoefTransTemp



  !====================================================================
  ! 
  ! SUBROUTINE: Pred_AbsCoef()
  !
  !   Pred_AbsCoef() predicts absorption coefficients by
  !
  !     k(l) = exp( sum sum RegCoef(i,p) * AtmosPred(i,l) * AbsAmountLev(l) ** p )
  !                  i   p
  !
  !   where
  !
  !     l : index for atmosphere profile layers
  !     i : index for atmospheric predictors
  !     p : order of a polynomial in terms of absorber amount level
  !
  !====================================================================

  subroutine Pred_AbsCoef &
                ( Natmpred, predcomb, atmpred_lay, abslev_lay, Npolyorder, &
                  coef, abscoef_lay, max_atmpred_term_lay )
                  
    !--- interface

    integer      ,intent(in)  :: Natmpred                       ! # of atmos predictors used in prediction
    integer      ,intent(in)  :: predcomb(Natmpred)             ! an atmos predictor index set
    real(fp_kind),intent(in)  :: atmpred_lay(Natmpred_max,Nlay) ! atmos predicictors
    real(fp_kind),intent(in)  :: abslev_lay(Nlay)               ! absorber amout levels on layers
    integer      ,intent(in)  :: Npolyorder
    real(fp_kind),intent(in)  :: coef(0:Npolyorder,0:Natmpred)  ! regression coef
    real(fp_kind),intent(out) :: abscoef_lay(Nlay)              ! absorption coef
    real(fp_kind),intent(out) :: max_atmpred_term_lay(Nlay)     ! max of atmos pred term

    !--- local variables

    real(fp_kind) :: pred(0:Npolyorder,0:Natmpred)
    real(fp_kind) :: log_k
    real(Quad)    :: log_k_quad, log_k_pred_quad(0:Natmpred)
    integer       :: Ilay, Ipred, Ipoly


    !--- Natmpred < 0

    if( Natmpred < 0 )then
      abscoef_lay(:)          = ZERO
      max_atmpred_term_lay(:) = ZERO
      return
    endif


    !--- initialize

    abscoef_lay(:)   = RMISS


    !--- calc coef

    do Ilay = 1, Nlay


      !--- predictors

      call Get_RegPredictor( Natmpred, predcomb, &
                             atmpred_lay(1,Ilay), abslev_lay(Ilay), &
                             Npolyorder, &
                             pred )


      !--- log_k for each atmospheric predictor term

      log_k_pred_quad(:) = 0._Quad
      
      do Ipred = 0, Natmpred
      do Ipoly = 0, Npolyorder
        log_k_pred_quad(Ipred) = log_k_pred_quad(Ipred) &
                             + real( coef(Ipoly,Ipred), Quad ) * real( pred(Ipoly,Ipred), Quad )
      enddo
      enddo


      !--- predict log k

      log_k_quad = 0._Quad

      do Ipred = 0, Natmpred
        log_k_quad = log_k_quad + real( log_k_pred_quad(Ipred), Quad )
      enddo

      log_k = real( log_k_quad, fp_kind )
      
      !--- convert to k

      if( log_k > HALF_LOG_INF )then
        abscoef_lay(Ilay) = EXP_HALF_LOG_INF
      else if( log_k < -HALF_LOG_INF )then
        abscoef_lay(Ilay) = ZERO
      else
        abscoef_lay(Ilay) = exp( log_k )
      endif


      !--- maximun of atmos pred term

      max_atmpred_term_lay(Ilay) = maxval( abs( real( log_k_pred_quad(:), fp_kind ) ) )

    enddo

  end subroutine Pred_AbsCoef



  !====================================================================
  !
  ! SUBROUTINE: Pred_Trans()
  !
  !   Pred_Trans() predicts transmittances from a profile of
  !   absorption coefficients by
  !
  !                        l
  !     Trans(l) = exp( - sum k(l) * (A(i) - A(i-1)) )
  !                       i=1
  !
  !====================================================================

  subroutine Pred_Trans( absamount_lev, abscoef_lay, trans_lev )

    !--- interface

    real(fp_kind),intent(in)    :: absamount_lev(0:Nlay)   ! absorber amounts on levels
    real(fp_kind),intent(inout) :: abscoef_lay  (  Nlay)   ! absorption coefficients
    real(fp_kind),intent(out)   :: trans_lev    (0:Nlay)   ! predicted gas transmittances

    !--- local variables

    real(fp_kind) :: total_od
    integer       :: Ilay


    !--- QC for absorption coef

    abscoef_lay(:) = max( abscoef_lay(:), ZERO )


    !--- initalize

    trans_lev(0) = ONE
    total_od     = ZERO

    !--- calc transmittances from absorption coef

    do Ilay = 1, Nlay

      total_od = total_od &
                  + abscoef_lay(Ilay) * ( absamount_lev(Ilay) - absamount_lev(Ilay-1) )

      if( -total_od > HALF_LOG_INF )then

        trans_lev(Ilay) = EXP_HALF_LOG_INF

      else if( -total_od < -HALF_LOG_INF )then

        trans_lev(Ilay) = ZERO

      else

        trans_lev(Ilay) = exp( - total_od )

      endif

    enddo

  end subroutine Pred_Trans



  !====================================================================
  !
  ! FUNCTION: Pred_BrightTemp()
  !
  !   Pred_BrightTemp() calculates a brightness temperature
  !   from transmittance and temperature profiles.
  !   Only up-welling radiance is considered as
  !
  !     Iup =   sum    B(l) * ( TransAll(l-1) - TransAll(l) )
  !           l=layers
  !
  !====================================================================

  function Pred_BrightTemp( Ichan, alltrans_lev, t_lay, Tskin ) result( Tb )

    !--- interface

    integer      ,intent(in) :: Ichan                   ! channel seq #
    real(fp_kind),intent(in) :: alltrans_lev(0:Nlay)    ! total transmittances
    real(fp_kind),intent(in) :: t_lay(Nlay)             ! temperatures (K)  
    real(fp_kind),intent(in) :: Tskin                   ! skin temperature (K)
    real(fp_kind)            :: Tb                      ! brightness temperature (K)

    !--- local variables

    real(fp_kind) :: rad
    real(fp_kind) :: tau, tau1
    integer       :: Ilev

    !--- surface

    if( alltrans_lev(Nlay) > TOLERANCE )then
      rad = VirtEmiss * Calc_PlanckTempToRad(Ichan,Tskin) * alltrans_lev(Nlay)
    else
      rad = ZERO
    endif


    !--- upwelling radiance

    do Ilev = 1, Nlay

      tau1 = alltrans_lev(Ilev-1)
      tau  = alltrans_lev(Ilev  )

! by Ychen 10/4/07
!      if( tau1 >= ZERO .and. tau >= ZERO )then

        !--- check if trans decrease as descending from TOA

!        if( tau1 < tau )then
!          print *, "### ERROR ###"
!          print *, " Total trans be decrease as descending from TOA in Tb calculation"
!          print *, " Levs :", Ilev-1, Ilev
!          print *, " Trans:", alltrans_lev
!          stop 99
!        endif

        !--- accumulate radiance

        rad = rad + Calc_PlanckTempToRad(Ichan,t_lay(Ilev)) * ( tau1 - tau )

!      endif

    enddo


    !--- Tb
    IF(rad > ZERO)THEN
      Tb = Calc_PlanckRadToTemp(Ichan,rad)
    ELSE
      Tb = 500.0_fp_kind  ! in this bad case, set the TB to a large value so that in the 
                          ! evaluation stage (compared to LBL), this set of predictors 
                          ! will be rejected.
    END IF

 
  end function Pred_BrightTemp

end module PredAbsCoefTransTemp
