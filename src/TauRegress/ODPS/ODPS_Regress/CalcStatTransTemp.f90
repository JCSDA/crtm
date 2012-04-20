!==============================================================================
!
! MODULE: CalcStatTransTemp
!
!   SUBROUTINE: Calc_StatTransTemp()
!
!
! Created by Y.Tahara in Oct,02
! Modified by Yong Han, Oct, 03
! 
! Modified by Yong Chen, Jan, 08
!==============================================================================

module CalcStatTransTemp

  !--- modules

  USE Type_Kinds, only : fp_kind
  USE Parameters
  USE Utilities_Statistics

  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Calc_StatTransTemp
  Public  StatTbTau
  
  real(fp_kind), parameter :: Threshold_TransFitTest = 0.0001_fp_kind


  !--- type of statistics variable

  type StatTbTau
    real(fp_kind) :: index              ! index for atmos predictor set selection
    real(fp_kind) :: residual           ! regression residual

    real(fp_kind) :: tbmean             ! Tb mean error against LBL over profiles and angles
    real(fp_kind) :: tbsderr            ! Tb S.D. error
    real(fp_kind) :: tbrmse             ! Tb RMS error 
    real(fp_kind) :: tbmaxerr           ! max Tb error of all profiles and angles

    real(fp_kind) :: taumean            ! total trans mean error against LBL over profiles, angles and layers
    real(fp_kind) :: tausderr           ! total trans S.D. error
    real(fp_kind) :: taurmse            ! total trans RMS error
    real(fp_kind) :: taumaxerr          ! max total trans error of all profiles and angles

    real(fp_kind) :: taumaxmean         ! max total trans mean error against LBL over layers of all profiles and angles 
    real(fp_kind) :: taumaxsderr        ! max total trans S.D. error
    real(fp_kind) :: taumaxrmse         ! max total trans RMS error

    real(fp_kind) :: maxpredterm        ! max predictor term
  end type StatTbTau


 contains


  !--------------------------------------------------------------------------
  !
  ! SUBROUTINE: Calc_StatTransTemp()
  !
  !   Calc_StatTransTemp() calculates the statistics of predicted
  !   transmittances and brightness temperatures against LBL. 
  !
  !--------------------------------------------------------------------------

  subroutine Calc_StatTransTemp( &
                        alltrans_lev,    tb_lbl, &
                        calalltrans_lev, tb_cal, &
                        stat                     )

    !--- interface

    real(fp_kind)  ,intent(in)  :: alltrans_lev (0:,:,:)        ! LBL total trans
    real(fp_kind)  ,intent(in)  :: tb_lbl(:,:)                  ! LBL Tb
    real(fp_kind)  ,intent(in)  :: calalltrans_lev(0:,:,:)      ! predicted total trans
    real(fp_kind)  ,intent(in)  :: tb_cal(:,:)                  ! predicted Tb
    type(StatTbTau),intent(out) :: stat                         ! statistics

    !--- local variables

    real(fp_kind)     :: taumaxmean, taumaxabsmean
    real(fp_kind)     :: taumaxsderr
    real(fp_kind)     :: taumaxrmse 
    
    real(fp_kind)     :: erralltrans_lev
    type(StatResults) :: stat0

    integer           :: Iang, Iatm, Ilev, Nlay, Nangle, Natm, Ndata


    Nlay   = SIZE(alltrans_lev, DIM=1) - 1
    Nangle = SIZE(tb_lbl, DIM=1)
    Natm   = SIZE(tb_lbl, DIM=2)

    !--- init stat heep

    call InitStat( 2 )


    !--- calc stat for Tb

    do Iatm = 1, Natm
    do Iang = 1, Nangle
      call AddStat( 1, (tb_cal(Iang,Iatm) - tb_lbl(Iang,Iatm)), ZERO )
    enddo
    enddo

    call CalcStat( 1, Ndata, stat0 )
    
    stat%tbmean   = stat0%meanerr
    stat%tbsderr  = stat0%sderr
    stat%tbrmse   = stat0%rmse
    stat%tbmaxerr = max( abs( stat0%max1 ), abs( stat0%min1 ) )

    !--- calc stat for trans

    taumaxabsmean = ZERO
    taumaxsderr   = ZERO
    taumaxrmse    = ZERO

    do Iatm = 1, Natm
    do Iang = 1, Nangle
      
      do Ilev = 1, Nlay

        if( alltrans_lev(Ilev,Iang,Iatm) < Threshold_TransFitTest )then
         cycle
        endif

        erralltrans_lev = calalltrans_lev(Ilev,Iang,Iatm) - alltrans_lev(Ilev,Iang,Iatm)

        call AddStat( 2,  erralltrans_lev, ZERO )

      enddo

    enddo
    enddo

    stat%taumaxmean  = taumaxmean
    stat%taumaxsderr = taumaxsderr
    stat%taumaxrmse  = taumaxrmse


    call CalcStat( 2, Ndata, stat0 )

    stat%taumean   = stat0%meanerr
    stat%tausderr  = stat0%sderr
    stat%taurmse   = stat0%rmse
    stat%taumaxerr = max( abs( stat0%max1 ), abs( stat0%min1 ) )

    !--- set index for predictor set selection
    
    if(Tb_OPTIMAL)then

      stat%index = stat%tbrmse
   
    else
     
      stat%index = stat%taurmse

    endif
  end subroutine Calc_StatTransTemp

end module CalcStatTransTemp
