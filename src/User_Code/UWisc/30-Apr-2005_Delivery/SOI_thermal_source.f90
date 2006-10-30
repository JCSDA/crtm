MODULE SOI_thermal_source

    USE type_kinds, only : fpk => fp_kind

    IMPLICIT NONE

    REAL(fpk), PRIVATE, PARAMETER    :: ONE = 1.0_fpk
    REAL(fpk), PRIVATE, PARAMETER    :: TWO = 2.0_fpk
    REAL(fpk), PRIVATE, PARAMETER    :: HALF = 0.5_fpk
    REAL(fpk), PRIVATE, PARAMETER    :: ZERO = 0.0_fpk
    REAL(fpk), PRIVATE, PARAMETER    :: TWELVE = 12.0_fpk
    REAL(fpk), PRIVATE, PARAMETER    :: cut_opd = 0.2_fpk

    PUBLIC :: thermal_source, thermal_source_TL, thermal_source_AD

CONTAINS


    subroutine    thermal_source( slopd, &    ! Slant optical depth of layer (opd/mu)
                                ssa,     &    ! ssa of layer
                                tau,     &    ! slant transmittance of layer (exp(-opd/mu))
                                Ttop,    &    ! Top Temp (or BB radiance) of layer
                                Tbot,    &    ! Bot Temp (or BB radiance) of layer
                                Sp,      &    ! Upwelling Source
                                Sm )          ! Downwelling Source


        REAL(fpk), intent(in)        ::    slopd
        REAL(fpk), intent(in)        ::    ssa
        REAL(fpk), intent(in)        ::    tau
        REAL(fpk), intent(in)        ::    Ttop
        REAL(fpk), intent(in)        ::    Tbot
        REAL(fpk), intent(out)       ::    Sp, Sm

        REAL(fpk)                    ::    A, B

        A = HALF * (Tbot + Ttop) * (ONE-tau)
        if (slopd < cut_opd) then
            B = (Ttop-Tbot)*(slopd/TWELVE)*(ONE-tau)
        else
            B = (Ttop-Tbot)*( tau + (ONE - tau)*(HALF - ONE/slopd) )
        endif

        sp = (A + B) * (ONE - ssa)
        sm = (A - B) * (ONE - ssa)
    
    end subroutine thermal_source


    subroutine thermal_source_TL( slopd, ssa, tau, Ttop, Tbot, &
                                  slopd_TL, ssa_TL, tau_TL, Ttop_TL, Tbot_TL, &
                                  Sp, Sm, Sp_TL, Sm_TL )

        REAL(fpk), intent(in)        ::    slopd, ssa, tau, Ttop, Tbot  ! regular input variables
        REAL(fpk), intent(in)        ::    slopd_TL, ssa_TL, tau_TL, Ttop_TL, Tbot_TL ! TL inputs
        REAL(fpk), intent(out)       ::    Sp, Sm, Sp_TL, Sm_TL    ! TL outputs

        REAL(fpk)                    ::    A, B, A_TL, B_TL

        A_TL = HALF * (Tbot_TL + Ttop_TL) * (ONE-tau) - HALF * (Tbot+Ttop)*tau_TL
        A = HALF * (Tbot + Ttop) * (ONE-tau)
        if (slopd < cut_opd) then
            B_TL = (Ttop_TL - Tbot_TL) * (slopd/TWELVE)*(ONE-tau) + &
                    (Ttop-Tbot)*( slopd_TL*(ONE-tau) - slopd*tau_TL)/TWELVE
               B = (Ttop-Tbot)*(slopd/TWELVE)*(ONE-tau)
        else
            B_TL = (Ttop_TL - Tbot_TL) * (tau + (ONE-tau)*(HALF-ONE/slopd) ) + &
                    (Ttop-Tbot) * ( tau_TL + (ONE-tau)*(slopd_TL/(slopd*slopd)) - &
                    tau_TL*(HALF - ONE/slopd) )
               B = (Ttop-Tbot)*( tau + (ONE - tau)*(HALF - ONE/slopd) )
        endif

        sp = (A + B) * (ONE - ssa)
        sm = (A - B) * (ONE - ssa)
        sp_TL = (A_TL + B_TL) * (ONE - ssa) - (A+B) * ssa_TL
        sm_TL = (A_TL - B_TL) * (ONE - ssa) - (A-B) * ssa_TL

    end subroutine thermal_source_TL

    subroutine thermal_source_AD( slopd, ssa, tau, Ttop, Tbot, &  ! input variables
                                  slopd_AD, ssa_AD, tau_AD, Ttop_AD, Tbot_AD, & !output AD variables
                                  sp_AD, sm_AD )     ! input AD variables


        REAL(fpk), intent(in)    :: slopd, ssa, tau, Ttop, Tbot
        REAL(fpk), intent(inout) :: slopd_AD, ssa_AD, tau_AD, Ttop_AD, Tbot_AD
        REAL(fpk), intent(in)    :: sp_AD, sm_AD
    
        REAL(fpk)            ::    A, B, A_AD, B_AD, dT, tmp
        ! forward re-computations
        dT = Ttop - Tbot
        A = HALF * (Tbot + Ttop) * (ONE - tau)
        if (slopd < cut_opd) then
            B = dT*(slopd/TWELVE)*(ONE - tau)
        else
            B = dT*( tau + (ONE - tau)*(HALF - ONE/slopd) )
        endif

        ! adjoint computations
        A_AD = (ONE - ssa) * (sp_AD + sm_AD)
        B_AD = (ONE - ssa) * (sp_AD - sm_AD)
        ssa_AD = -(A+B) * sp_AD - (A-B)*sm_AD + ssa_AD

        if (slopd < cut_opd) then
            tmp = slopd/TWELVE*(ONE - tau)
            Ttop_AD = tmp * B_AD + Ttop_AD
            Tbot_AD = -tmp * B_AD + Tbot_AD
            slopd_AD = dT/TWELVE * (ONE - tau)*B_AD + slopd_AD
            tau_AD = -dT*slopd/TWELVE * B_AD + tau_AD
        else
            tmp = tau + (ONE - tau)*(HALF - ONE/slopd)
            Ttop_AD =  tmp * B_AD + TTop_AD
            Tbot_AD = -tmp * B_AD + Tbot_AD
            slopd_AD = dT * (ONE - tau)/(slopd*slopd) * B_AD + slopd_AD
            tau_AD = dT * (HALF + ONE/slopd) * B_AD + tau_AD
        endif
    
        tau_AD = -HALF * (Tbot+Ttop) * A_AD + tau_AD
        tmp = HALF * (ONE - tau) * A_AD
        Ttop_AD = tmp + Ttop_AD
        Tbot_AD = tmp + Tbot_AD
        
    end subroutine thermal_source_AD    

end module SOI_thermal_source
