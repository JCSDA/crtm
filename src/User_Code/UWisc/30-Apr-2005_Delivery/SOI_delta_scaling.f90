
MODULE SOI_delta_scaling

	USE Type_Kinds, only : fpk => fp_kind

	IMPLICIT NONE

	REAL(fpk), PRIVATE, PARAMETER	:: ONE = 1.0_fpk
	REAL(fpk), PRIVATE, PARAMETER	:: TWO = 2.0_fpk
	REAL(fpk), PRIVATE, PARAMETER	:: ZERO = 0.0_fpk


PUBLIC  :: deltascaling, deltascaling_TL, deltascaling_AD

CONTAINS


	SUBROUTINE deltascaling( opd,  &  ! input
                              ssa,  &  ! input
                              asy,  &  ! input
                                f,  &  ! input
                             opd2,  &  ! output
                             ssa2,  &  ! output
                             asy2)     ! output
									
		REAL(fpk), intent(in), dimension(:)		::	opd, ssa, asy, f
		REAL(fpk), intent(out), dimension(:)	::	opd2, ssa2, asy2
	
		opd2 = opd * (ONE - ssa * f)
		ssa2 = (ONE-f)*ssa/(ONE - f*ssa)
		asy2 = (asy-f)/(ONE-f)

	END SUBROUTINE deltascaling

	SUBROUTINE deltascaling_TL( opd,   &  ! input
                                ssa,    &  ! input
                                asy,    &  ! input
                                  f,    &  ! input
						       opd_TL,  &  ! input
	                           ssa_TL,  &  ! input
                               asy_TL,  &  ! input
                                 f_TL,  &  ! input
                               opd2_TL, &  ! output
                               ssa2_TL, &  ! output
                               asy2_TL  )  ! output
  
									
		REAL(fpk), intent(in), dimension(:)		::	opd, ssa, asy, f
		REAL(fpk), intent(in), dimension(:)		::	opd_TL, ssa_TL, asy_TL, f_TL
		REAL(fpk), intent(out), dimension(:)	::	opd2_TL, ssa2_TL, asy2_TL
		
		opd2_TL = (ONE-ssa*f)*opd_TL - opd*(ssa*f_TL + f*ssa_TL)
		ssa2_TL = (ssa*(ssa-ONE)*f_TL + (ONE-f)*ssa_TL)/(ONE-ssa*f)**2		
		asy2_TL = asy_TL / (ONE-f) - (ONE-asy)*f_TL/(ONE-f)**2

	END SUBROUTINE deltascaling_TL


	SUBROUTINE deltascaling_AD( opd,   &  ! input
                                ssa,    &  ! input
                                asy,    &  ! input
                                  f,    &  ! input
						       opd_AD,  &  ! inout
	                           ssa_AD,  &  ! inout
                               asy_AD,  &  ! inout
                                 f_AD,  &  ! inout
                               opd2_AD, &  ! inout
                               ssa2_AD, &  ! inout
                               asy2_AD  )  ! inout
  									
		REAL(fpk), intent(in), dimension(:)     ::	opd, ssa, asy, f
		REAL(fpk), intent(inout), dimension(:)  ::	opd_AD, ssa_AD, asy_AD, f_AD
		REAL(fpk), intent(inout), dimension(:)  ::	opd2_AD, ssa2_AD, asy2_AD
		REAL(fpk), dimension(size(opd))         ::	denom1, denom2

		! useful constants
		denom1 = (ONE-ssa*f)
		denom1 = ONE/(denom1*denom1)
		denom2 = ONE/(ONE-f)

		! adjoint statements
		opd_AD = opd_AD + (ONE-ssa*f)*opd2_AD
		ssa_AD = ssa_AD + (ONE-f)*denom1 * ssa2_AD - f*opd*opd2_AD
		asy_AD = asy_AD + asy2_AD*denom2
		f_AD = f_AD - (ONE-asy)*denom2*denom2*asy2_AD + ssa*(ssa-ONE)*denom1*ssa2_AD - ssa*opd*opd2_AD
		opd2_AD = ZERO
		ssa2_AD = ZERO
		asy2_AD = ZERO

	END SUBROUTINE deltascaling_AD

END MODULE SOI_delta_scaling
