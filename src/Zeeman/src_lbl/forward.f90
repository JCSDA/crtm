
! O2 transition lines with no external manetic field (no Zeeman effect)
!
! N-     Freq (GHz)             N+      Freq (GHz)
!
! 1-      118.7503,             1+       56.2648, 
! 3-      62.4863,              3+       58.4466, 
! 5-      60.3061,              5+       59.5910,
! 7-      59.1642,              7+       60.4348, 
! 9-      58.3239,              9+       61.1506, 
! 11-     57.6125,              11+      61.8002,
! 13-     56.9682,              13+      62.4112, 
! 15-     56.3634,              15+      62.9980, 
! 17-     55.7838,              17+      63.5685,
! 19-     55.2214,              19+      64.1278, 
! 21-     54.6712,              21+      64.6789, 
! 23-     54.1300,              23+      65.2241,
! 25-     53.5958,              25+      65.7648, 
! 27-     53.0670,              27+      66.3021, 
! 29-     52.5424,              29+      66.8368,
! 31-     52.0215,              31+      67.3695, 
! 33-     51.5034,              33+      67.9008

    !------------------------------------------------------------
    ! N - quantum number of rotational angular momentum
    !     N takes odd values.
    ! J - quantum number of angular momentum, which is a vectorial
    !     sum of the total electron spin anglular momentum and the
    !     rotational angular momentum.
    !     J takes the values 
    !        J = N-1, N, N+1
    !     The selection rules permit transtions between
    !        J = N and N + 1  (called N+ lines)
    !        J = N and N-1    (called N- lines)
    !     The state with J = N-1 and J = N+1 are lower in energy
    !
    !------------------------------------------------------------

MODULE rt_zeeman_lbl

  USE type_kinds, only : fp_kind
  USE zeeman_rosen
  USE TBmx_rosen

  IMPLICIT NONE
      

  Private
  Public rt_lbl

  CONTAINS
  
  subroutine rt_lbl(JU, freq_offset, secant_view_angle, z_level, p_lay, t_lay, &
                    Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im, tau_rc, tau_lc, tau_od_Re, tau_od_Im)

    INTEGER      , INTENT(IN) :: JU ! JU = N+1, an even number, for N+ branch; JU = N, an odd number, for N- branch
    REAL(fp_kind), INTENT(IN) :: freq_offset
    REAL(fp_kind), INTENT(IN) :: secant_view_angle
    REAL(fp_kind), INTENT(IN) :: z_level(0:)
    REAL(fp_kind), INTENT(IN) :: p_lay(:), t_lay(:)
    REAL(fp_kind), INTENT(OUT) :: Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im
    REAL(fp_kind), INTENT(OUT) :: tau_rc(:), tau_lc(:), tau_od_Re(:), tau_od_Im(:)

    ! local

    REAL :: del_frq,Bfield
    COMPLEX, DIMENSION(SIZE(p_lay)) :: GM,     &    ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=+1)
                                       GPI,    &    ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=-1)
                                       GP           ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=0)
    COMPLEX                         :: GM_coeff,  & ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = -1
	                                   GPI_coeff, & ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M =  0
									   GP_coeff     ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = +1

    REAL, DIMENSION(SIZE(p_lay))      :: AL,     &    ! POWER ABSORPTION (OPACITY) FROM OTHER LINES
                                         CBTH         ! COS(ANGLES BETWEEN MAGNETIC FIELD 
                                                      ! AND PROPAGATION DIRECTION)
	REAL, DIMENSION(0:SIZE(p_lay))    :: TRNRC,  &    ! TRANSMITTANCE ALONG SECANT FROM TOP OF ATMOSPHERE 
                                                    ! TO BOTTOM OF LAYER I, IN RIGHT-CIRCULAR POL.
								         TRNLC        ! TRANSMITTANCE IN LEFT-CIRCULAR POL.
    COMPLEX, DIMENSION(0:SIZE(p_lay)) :: TRNLIN       ! OFF-DIAGONAL ELEMENT OF TRANSMITTANCE
                                                    ! COHERENCY MATRIX; IN TERMS OF STOKES 
                                                    ! PARAMETERS, Q=2*REAL(TRNLIN), U=2*AIMAG(TRNLIN)

    REAL                            :: secant_zenith_angle  ! secant of zenith angle

	INTEGER                         :: Nlay, Ilay
	REAL                            :: dz
 
    Nlay = SIZE(p_lay)
	secant_zenith_angle = real(secant_view_angle)

    !--- Frequency offset from line center (N- or N+ line center)
    del_frq =real(freq_offset)

    !--- 1 Tesla = 10000 Gauss, 1 Tesla = 1.e9 nano Tesla
    !--- The Earth Bfield is in the order of 0.5 Gauss (60 micro Tesla)
    Bfield = 0.5  ! Gauss

    CBTH(:) = cos(135.0*3.14156/180.)

	AL(:) = 0.0

    do Ilay = 1, Nlay

      call ZEEMAN(JU, real(t_lay(ilay)), real(p_lay(ilay)), del_frq, Bfield, GM_coeff, GPI_coeff, GP_coeff)

      dz = real(z_level(Ilay-1) - z_level(Ilay))
      GP(Ilay)  = GP_coeff * dz
	  GM(Ilay)  = GM_coeff * dz
	  GPI(Ilay) = GPI_coeff * dz

    enddo

    TRNRC(0) = 1.0
    TRNLC(0) = 1.0
	TRNLIN(0) = (0.,0.)
    call  TRANX2(secant_zenith_angle, Nlay, AL, GP, GM, GPI, CBTH, TRNRC(1:), TRNLC(1:),TRNLIN(1:))
	tau_rc(:) = real(TRNRC(1:),fp_kind)
	tau_lc(:) = real(TRNLC(1:),fp_kind)
	tau_od_Re(:) = real(TRNLIN(1:),fp_kind)
	tau_od_Im(:) = real(imag(TRNLIN(1:)),fp_kind)

    call radiance(TRNRC,TRNLC,TRNLIN, t_lay, Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im)

  end subroutine rt_lbl

  subroutine radiance(TRNRC,TRNLC,TRNLIN, t_lay, Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im)

    REAL, INTENT(IN)    :: TRNRC(0:),  &    ! TRANSMITTANCE ALONG SECANT FROM TOP OF ATMOSPHERE 
                                           ! TO BOTTOM OF LAYER I, IN RIGHT-CIRCULAR POL.
					       TRNLC(0:)        ! TRANSMITTANCE IN LEFT-CIRCULAR POL.
    COMPLEX, INTENT(IN) :: TRNLIN(0:)       ! OFF-DIAGONAL ELEMENT OF TRANSMITTANCE
                                           ! COHERENCY MATRIX; IN TERMS OF STOKES 
                                           ! PARAMETERS, Q=2*REAL(TRNLIN), U=2*AIMAG(TRNLIN)
    
    REAL(fp_kind), INTENT(IN) :: t_lay(:)

	REAL(fp_kind), INTENT(OUT) :: Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im

	INTEGER :: Nlay, Ilay

    Nlay = SIZE(t_lay)
	Tb_od_Re = 0.0_fp_kind
	Tb_od_Im = 0.0_fp_kind

    do Ilay = 1, Nlay

	  Tb_rc = Tb_rc + real(TRNRC(Ilay-1) - TRNRC(Ilay), fp_kind) * t_lay(Ilay)
	  Tb_lc = Tb_lc + real(TRNLC(Ilay-1) - TRNLC(Ilay), fp_kind) * t_lay(Ilay)
	  Tb_od_Re = Tb_od_Re + real(TRNLIN(Ilay-1) - TRNLIN(Ilay), fp_kind) * t_lay(Ilay)
	  Tb_od_Im = Tb_od_Im + real(imag(TRNLIN(Ilay-1) - TRNLIN(Ilay)), fp_kind) * t_lay(Ilay)

	enddo

  end subroutine radiance

end MODULE rt_zeeman_lbl
