!====================================================================
! 
! MODULE: CalcRegWeight 
! 
!   SUBROUTINE Calc_RegWeight()
! 
! 
! Created by Y.Tahara in Aug,02
!
!====================================================================

module CalcRegWeight

  !--- modules

  use type_kinds, only : fp_kind
  use ParametersGenCoef
  use PlanckFunc


  !--- implicit

  implicit none


  !--- public & private

  Private

  Public  Calc_RegWeight


 contains


  !====================================================================
  !
  ! SUBROUTINE Calc_RegWeight()
  !
  !   Calc_RegWeight() calculates regression weight, which
  !   is used in generating transmittance coefficients.
  !   The weight is calculated by 
  !
  !               Tau(k-1)                               
  !     W(k) = ln -------- ( | B(N) - r B(N) | TauAll(N) 
  !                Tau(k)                                
  !                            N-1
  !			     + sum | B(i) - B(i+1) | TauAll(i) )
  !			       i=k
  !            + OFFSET
  !
  !====================================================================

  subroutine Calc_RegWeight( Ichan, t_lay, alltrans_lev, trans_lev, &
                             p_lev, sense_lay, &
                             sqrtwgt_lay )

    !--- interface

    integer      ,intent(in)  :: Ichan                                  ! ch seq #
    real(fp_kind),intent(in)  :: t_lay(Nlay,Natm)                       ! temperature (K)
    real(fp_kind),intent(in)  :: alltrans_lev(0:Nlay,Nangle,Natm)       ! total trans
    real(fp_kind),intent(in)  :: trans_lev   (0:Nlay,Nangle,Natm)       ! particular gas trans
    real(fp_kind),intent(in)  :: p_lev(0:Nlay)                          ! temperature (K)
    real(fp_kind),intent(out) :: sense_lay   (  Nlay,Nangle,Natm)       ! sensetivity
    real(fp_kind),intent(out) :: sqrtwgt_lay (  Nlay,Nangle,Natm)       ! square root of weight

    !--- local variables

    integer :: Iatm, Iang


    !-- weight without offset
 
    do Iatm = 1, Natm
    do Iang = 1, Nangle

      call Calc_RegWeight_Profile( Ichan, &
                                   t_lay(1,Iatm), &
                                   alltrans_lev(0,Iang,Iatm), &
                                   trans_lev   (0,Iang,Iatm), &
                                   p_lev,                     &
                                   sense_lay   (1,Iang,Iatm), &
                                   sqrtwgt_lay (1,Iang,Iatm) )

    enddo
    enddo


 
    sqrtwgt_lay(:,:,:) = sqrtwgt_lay(:,:,:) &
                                + maxval( sqrtwgt_lay(:,:,:) ) * Weight_base_list(Iabsorber)

    !--- square root
    sqrtwgt_lay(:,:,:) = sqrt( sqrtwgt_lay(:,:,:) )

 
!###EXPERIMENT-NO WEIGHT###
!   sqrtwgt_lay(:,:,:) = ONE
!###EXPERIMENT-NO WEIGHT###

  end subroutine Calc_RegWeight


   
  !====================================================================
  ! Regression weight for one profile
  !====================================================================

  subroutine Calc_RegWeight_Profile( Ichan, &
                                    t_lay, alltrans_lev, trans_lev, &
                                    p_lev, sense_lay, &
                                    sqrtwgt_lay )

    !--- interface
 
    integer      ,intent(in)  :: Ichan                  ! ch seq #
    real(fp_kind),intent(in)  :: t_lay(Nlay)            ! temperature (K)
    real(fp_kind),intent(in)  :: alltrans_lev(0:Nlay)   ! total trans
    real(fp_kind),intent(in)  :: trans_lev   (0:Nlay)   ! particular gas trans
    real(fp_kind),intent(in)  :: p_lev       (0:Nlay)   ! temperature (K)
    real(fp_kind),intent(out) :: sense_lay   (  Nlay)   ! sensetivity
    real(fp_kind),intent(out) :: sqrtwgt_lay (  Nlay)   ! square root of weight

    !--- local variables
    
    real(fp_kind) :: PlanckRad(Nlay_max)
    real(fp_kind) :: Contr(Nlay_max)
    real(fp_kind) :: Contr_sense(Nlay_max)
    real(fp_kind) :: trans_temp
    integer       :: Ilay



!###EXPERIMENT-W=dTauAll###
!   sqrtwgt_lay(1:Nlay) = (alltrans_lev(0:Nlay-1) - alltrans_lev(1:Nlay)) / &
!                         log( p_lev(1:Nlay) / p_lev(0:Nlay-1) )
!   sense_lay(1:Nlay)   = sqrtwgt_lay(1:Nlay)
!   return
!###EXPERIMENT-W=dTauAll###
!   sqrtwgt_lay(:) = ZERO
!   do Ilay = 1, Nlay
!     if( trans_lev(Ilay-1) < TOLERANCE  .or. &
!         trans_lev(Ilay  ) < TOLERANCE  )cycle
!     sqrtwgt_lay(Ilay) = alltrans_lev(Ilay) * log(   trans_lev(Ilay-1) &
!                                                   / trans_lev(Ilay  ) ) 
!   enddo
!   return
!###EXPERIMENT-W=dTauAll###


    !--- calculate Planck radiance

    do Ilay = 1, Nlay

      PlanckRad(Ilay) = Calc_PlanckTempToRad( Ichan, t_lay(Ilay) )

    enddo


!###METHOD1-ORIGINAL###
    !--- contribution from lower layers (method 1 - original)

!Ychen 10/05/07
    IF( alltrans_lev(Nlay) < ZERO) THEN
     Contr(Nlay) = ZERO
     Contr_sense(Nlay) = ZERO 
    else 
     Contr(Nlay) = (ONE - VirtEmiss) * PlanckRad(Nlay) * alltrans_lev(Nlay)
     Contr_sense(Nlay) = Contr(Nlay)
    endif
    do Ilay = Nlay-1, 1, -1
      
     trans_temp = alltrans_lev(Ilay)
     
     IF( alltrans_lev(Ilay)  < ZERO) trans_temp = ZERO
       
      Contr(Ilay) = Contr(Ilay+1) &
                      + abs( PlanckRad(Ilay) - PlanckRad(Ilay+1) ) &
                         * trans_temp
      Contr_sense(Ilay) = Contr_sense(Ilay+1) &
                      +    ( PlanckRad(Ilay) - PlanckRad(Ilay+1) ) &
                         * trans_temp
     
    enddo
!###METHOD1-ORIGINAL###


!###METHOD2###
!   !--- contribution from lower layers (method 2 - experiment)
!
!!  Contr_sense(Nlay) =                     PlanckRad(Nlay) * alltrans_lev(Nlay)
!   Contr_sense(Nlay) = (ONE - VirtEmiss) * PlanckRad(Nlay) * alltrans_lev(Nlay)
!   Contr(Nlay)       = (ONE + VirtEmiss) * PlanckRad(Nlay) * alltrans_lev(Nlay)
!
!   do Ilay = Nlay-1, 1, -1
!
!     Contr_sense(Ilay) = Contr_sense(Ilay+1) &
!                         + alltrans_lev(Ilay) * ( PlanckRad(Ilay) - PlanckRad(Ilay+1) )
!     Contr(Ilay)       = - Contr_sense(Ilay) &
!                         + TWO * PlanckRad(Ilay) * alltrans_lev(Ilay)
!   
!   enddo
!   
!   Contr(:) = max( Contr(:), ZERO )
!###METHOD2###


    !--- calculate weight

    sqrtwgt_lay(:) = ZERO

    do Ilay = 1, Nlay

      if( trans_lev(Ilay-1) < TOLERANCE  .or. &
          trans_lev(Ilay  ) < TOLERANCE  )cycle

      sqrtwgt_lay(Ilay) = Contr(Ilay)       * log(   trans_lev(Ilay-1) &
                                                   / trans_lev(Ilay  ) ) 
      sense_lay(Ilay)   = Contr_sense(Ilay) * log(   trans_lev(Ilay-1) &
                                                   / trans_lev(Ilay  ) ) 

    enddo


  end subroutine Calc_RegWeight_Profile

end module CalcRegWeight
