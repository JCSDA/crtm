!--------------------------------------------------------------------------------
!M+
! NAME:
!       O2_zeeman_lbl
!
! PURPOSE:
!       Module containing routines to compute line-by-line transmittances, 
!       taking the Zeeman effect into account.
!
!
! MODULES:
!       type_kinds:             Module containing definitions for kinds of
!                               variable types
!
! CONTAINS:
!       tau_zeeman_lbl:   subroutine to compute line-by-line transmittance
!
!       radiance_zeeman:  subroutine to compute line-by-line brightness temperature
!                         radiance
!
!       search_line:    subroutine to find the O2 unsplit line JU, Given a 
!                       frequency (GHz).
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Nov. 2006
!
!  Copyright (C) 2006 Yong Han
!
!M-
!--------------------------------------------------------------------------------

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
! JU - line index for the transition lines                        
!        = N+1, an even number, for N+ branch;                    
!        = N, an odd number, for N- branch                        
!      For example: when JU = 7, it represents the 7- line        
!                   when JU = 8, it represents the 7+ line        
!------------------------------------------------------------     


MODULE O2_zeeman_lbl

!----------------------------------------------------------------------
! Unsplit O2 transition lines (no external magnetic field)
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
!
! Note that (1) the lines are generally separated by about 500 MHz. Since line
! widths above 30 km are 20 MHz or less, each line will be well isolated; (2)
! the lines between 1+ and 15-, 9- and 3+, 5- and 7+, and 13+ and 3- are close 
! (separated by 75 MHz or more)
!-----------------------------------------------------------------------------

  USE type_kinds, only : fp_kind
  USE Tran_matrix_linearBase

  IMPLICIT NONE


  Private
  Public tau_zeeman_lbl
  Public radiance_zeeman
  Public search_line

!  External ABSN2

  !--- O2 microwave unsplit transtion frequencies (N- and N+ lines)
  integer, parameter :: N_O2_FREQUENCIES = 17*2
  real(fp_kind), public, parameter :: O2_FREQUENCIES(N_O2_FREQUENCIES) = &
                              (/ 118.7503,   56.2648, &  !  1-,  1+
                                  62.4863,   58.4466, &  !  3-,  3+
                                  60.3061,   59.5910, &  !  5-,  5+
                                  59.1642,   60.4348, &  !  7-,  7+
                                  58.3239,   61.1506, &  !  9-,  9+
                                  57.6125,   61.8002, &  !  11-, 11+
                                  56.9682,   62.4112, &  !  13-, 13+
                                  56.3634,   62.9980, &  !  15-, 15+
                                  55.7838,   63.5685, &  !  17-, 17+
                                  55.2214,   64.1278, &  !  19-, 19+
                                  54.6712,   64.6789, &  !  21-, 21+
                                  54.1300,   65.2241, &  !  23-, 23+
                                  53.5958,   65.7648, &  !  25-, 25+
                                  53.0670,   66.3021, &  !  27-, 27+
                                  52.5424,   66.8368, &  !  29-, 29+
                                  52.0215,   67.3695, &  !  31-, 31+
                                  51.5034,   67.9008 /)  !  33-, 33+

  ! --- O2 microwave unsplit transtion frequencies listed according to their values.
  real(fp_kind), public, parameter :: O2_FREQUENCIES_SORTED(N_O2_FREQUENCIES) = &
     (/51.5034, 52.0215, 52.5424, 53.0670, 53.5958, 54.1300, &
       54.6712, 55.2214, 55.7838, 56.2648, 56.3634, 56.9682, &
       57.6125, 58.3239, 58.4466, 59.1642, 59.5910, 60.3061, &
       60.4348, 61.1506, 61.8002, 62.4112, 62.4863, 62.9980, &
       63.5685, 64.1278, 64.6789, 65.2241, 65.7648, 66.3021, &
       66.8368, 67.3695, 67.9008, 118.7503/)
  integer, public, parameter :: O2_FREQUENCIES_INDEX(N_O2_FREQUENCIES) = &
     (/33,31,29,27,25,23,21,19,17, 2,15,13,11, &
        9, 4, 7, 6, 5, 8,10,12,14, 3,16,18,20, &
       22,24,26,28,30,32,34, 1/)

  ! Zeeman effect is safely ignored if the computing frequency is displaced from 
  ! a line (N+ or N- line) center by more than DELTA_ZEEMAN_F
!  real(fp_kind), parameter :: DELTA_ZEEMAN_F = 6.0_fp_kind*0.001_fp_kind ! GHz

  real(fp_kind), parameter :: ZERO = 0.0_fp_kind

!  real(fp_kind), parameter :: FREQ_RANGE_MAX = 1.0_fp_kind  ! GHz
  real(fp_kind), parameter :: FREQ_RANGE_MAX = 3.0_fp_kind  ! GHz

  CONTAINS

!--------------------------------------------------------
!  subroutine to compute line-by-line transmittance
!
!  Inputs:
!    frequency:  frequency in GHz
!                TYPE: REAL( fp_kind )
!                DIMENSION: scalar
!    
! secant_angle:  secant of zenith angle
!                TYPE: REAL( fp_kind )
!                DIMENSION: scalar
!
!    z_level  :  altitude profile in km
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (0:n_layers)
!
!    p_lay    :  layer pressure profile in hPa (z_level provides its boundaries)
!                usually computed as p_lay(k) = (p_lev(k)-p_lev(k-1))/Log(p_lev(k)/p_lev(k-1)) 
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
! 
!    t_lay    :  layer mean temperature profile in K
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!    Bfield   :  earth magnetic field strength profile in Gauss
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!    
!    CBTH     :  profile of the cosine of the angle between the earth magnetic
!                field and wave propogation direction
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
! Outputs
!
!     tau11   :  The right circularly polarized transmittance profile
!                if the optional parameter linearBaseor is not present, or
!                the v' linearly polarized transmittance profile if 
!                linearBaseor is present.
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!                
!     tau22   :  The left circularly polarized transmittance profile
!                if the optional parameter linearBaseor is not present, or
!                the h' linearly polarized transmittance profile if 
!                linearBaseor is present.
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
! 
!    tau12_Re :  The real part of the coherent component 
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!    tau12_Im :  The imaginary part of the coherent component
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!  Optional inputs
!
!     JU      :  line index for the transition lines (see the definition in the
!                beginning of this file.)
!                If present, only a single unsplit transition line, represented
!                by the number JU will be included in the calculations; 
!                if not present, all lines are included.
!                TYPE: Integer
!                DIMENSION: scalar
!
!   linearBase : if present, the calculation uses linear polarization basis
!                if not present, it uses circular basis. 
!   
!-----------------------------------------------------------------------------

  subroutine tau_zeeman_lbl(frequency, secant_angle, &
                            z_level, p_lay, t_lay, &
                            Bfield, CBTH, &
                            tau11, tau22, tau12_Re, tau12_Im, &
                            JU, linearBase)

    REAL(fp_kind), INTENT(IN) :: frequency
    REAL(fp_kind), INTENT(IN) :: secant_angle
    REAL(fp_kind), INTENT(IN) :: z_level(0:)
    REAL(fp_kind), INTENT(IN) :: p_lay(:), t_lay(:)
    REAL(fp_kind), INTENT(IN) :: Bfield(:)  ! MAGNETIC FIELD (Gauss)
                                   	    ! 1 Tesla = 10000 Gauss, 1 Tesla = 1.e9 nano Tesla
                                            ! The Earth Bfield is in the order of 0.5 Gauss (60 micro Tesla)

    REAL(fp_kind), INTENT(IN)  :: CBTH(:)   ! COS(ANGLES BETWEEN MAGNETIC FIELD 

    REAL(fp_kind), INTENT(OUT) :: tau11(:), tau22(:), tau12_Re(:), tau12_Im(:)

    ! JU = N+1, an even number, for N+ branch; JU = N, an odd number, for N- branch
    INTEGER, OPTIONAL, INTENT(IN) :: JU
    ! if present(linearBase), the calculations are for linear-polarization base
    !    with tau11 = tau_vertical, tau22 = tau_horizontal, and tau_12 = off-diagonal element ;
    ! if not present(linearBase), the calculations are for circular-polarization base
    !    with tau11 = tau_rc, tau22 = tau_lc, tau12 = off-diagonal element
    INTEGER, OPTIONAL, INTENT(IN) :: linearBase 

    ! local

    COMPLEX, DIMENSION(SIZE(p_lay)) :: GM,     &    ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=+1)
                                       GPI,    &    ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=-1)
                                       GP           ! COMPLEX AMPLITUDE PROPAG.OPACITY  (DELTA M=0)
    COMPLEX                         :: GM_coeff,  & ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = -1
	                               GPI_coeff, & ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M =  0
				       GP_coeff     ! 1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = +1

    REAL, DIMENSION(SIZE(p_lay))    :: AL           ! POWER ABSORPTION (OPACITY) FROM OTHER LINES
                                                    ! AND PROPAGATION DIRECTION)
    REAL, DIMENSION(SIZE(p_lay))    :: TRN_11,  TRN_22                                                 
    COMPLEX, DIMENSION(SIZE(p_lay)) :: TRN_12       
                                                    
                                                    

    REAL                            :: frq_offset, dz
    INTEGER                         :: Nlay, Ilay
 
    Nlay = SIZE(p_lay)


    do Ilay = 1, Nlay

      if(present(JU))then

        frq_offset = frequency - O2_FREQUENCIES(JU)
        call ZEEMAN(JU, real(t_lay(ilay)), real(p_lay(ilay)), frq_offset, &
		    real(Bfield(ilay)), GM_coeff, GPI_coeff, GP_coeff)

      else
	    
	call zeeman_propagCoeff_allLines(frequency, t_lay(ilay), p_lay(ilay), Bfield(ilay), &
                                         GM_coeff, GPI_coeff, GP_coeff)

      endif

      dz = z_level(Ilay-1) - z_level(Ilay)
      GP(Ilay)  = GP_coeff * dz
      GM(Ilay)  = GM_coeff * dz
      GPI(Ilay) = GPI_coeff * dz

      ! N2 continuum is very small, so neglected here
      ! AL(Ilay) = ABSN2(real(t_lay(Ilay)),real(p_lay(Ilay)),real(frequency))*dz

      AL(Ilay) = ZERO

    enddo

!    IF(PRESENT(linearBase))THEN
!      call tran_matrix(real(secant_angle), AL, GP, GM, GPI, &
!	         real(CBTH), TRN_11, TRN_22,TRN_12)
!    ELSE
      call  TRANX2(real(secant_angle), Nlay, AL, GP, GM, GPI, &
                   real(CBTH), TRN_11, TRN_22,TRN_12)
!    ENDIF

    IF(PRESENT(linearBase))THEN
      ! transform to linear based polarization   
      tau11    = REAL(0.5*(TRN_11 + TRN_22 + 2.0*real(TRN_12)), fp_kind)             
      tau22    = REAL(0.5*(TRN_11 + TRN_22 - 2.0*real(TRN_12)), fp_kind)     
      tau12_Re = REAL(aimag(TRN_12), fp_kind)         
      tau12_Im = REAL(0.5*(TRN_11-TRN_22), fp_kind)   
    ELSE
      ! Circular polarization base
      tau11    = REAL(TRN_11,fp_kind)           
      tau22    = REAL(TRN_22,fp_kind)           
      tau12_Re = REAL(TRN_12,fp_kind)       
      tau12_Im = REAL(aimag(TRN_12),fp_kind) 
    ENDIF

  end subroutine tau_zeeman_lbl
    
  !========================================================
  !  Given a frequency (GHz) this routine find the 
  !  O2 unsplit line JU 
  !  inputs:
  !     frequency : frequency (GHz)
  !                 scalar
  !
  !  outputs:
  !     JU        : line index
  !                 integer
  !========================================================
                    
  subroutine search_line(frequency, JU)

    real(fp_kind), intent(in)  :: frequency
    integer      , intent(out) :: JU

    ! local
    integer :: i
    real(fp_kind) :: df, df_min 

    df_min = 10000.0_fp_kind
    do i = 1,N_O2_FREQUENCIES
    
      df = abs(frequency - O2_FREQUENCIES(i))
      if(df <= df_min)then
		  
        df_min = df
	JU = i
		 	 
      endif

    enddo
	  	
  end subroutine search_line

  subroutine zeeman_propagCoeff_allLines(frequency, t_lay, p_lay, Bfield, &
                                         GM_coeff, GPI_coeff, GP_coeff)

    real(fp_kind), intent(in)     :: frequency
    real(fp_kind), intent(in)     :: t_lay
    real(fp_kind), intent(in)     :: p_lay
    real(fp_kind), intent(in)     :: Bfield
    COMPLEX,       intent(out)    :: GM_coeff, GPI_coeff, GP_coeff

    
    ! local
	complex :: GM_coeff0, GPI_coeff0, GP_coeff0
	real :: frq_offset
	integer :: JU

    GM_coeff  = ZERO
    GPI_coeff = ZERO
    GP_coeff  = ZERO
    do JU = 1, N_O2_FREQUENCIES

      frq_offset = real(frequency - O2_FREQUENCIES(JU))

      ! lines in the range FREQ_RANGE_MAX are counted
      if(abs(frq_offset) <= FREQ_RANGE_MAX)then

        call ZEEMAN(JU, real(t_lay), real(p_lay), frq_offset, real(Bfield), &
	                GM_coeff0, GPI_coeff0, GP_coeff0)

        GM_coeff  = GM_coeff  + GM_coeff0
        GPI_coeff = GPI_coeff + GPI_coeff0
        GP_coeff  = GP_coeff  + GP_coeff0

      endif

     enddo

  end subroutine zeeman_propagCoeff_allLines


  subroutine radiance_zeeman(tau_rc, tau_lc, tau_od_Re, tau_od_Im, &
                             t_lay, Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im)


    REAL(fp_kind), INTENT(IN) :: tau_rc(:), tau_lc(:), tau_od_Re(:), tau_od_Im(:)
    REAL(fp_kind), INTENT(IN) :: t_lay(:)

    
    REAL(fp_kind), INTENT(OUT) :: Tb_rc, Tb_lc, Tb_od_Re, Tb_od_Im

    ! local 
    INTEGER :: Nlay, Ilay

    Nlay = SIZE(t_lay)

    Tb_rc = ZERO
    Tb_lc = ZERO
    Tb_od_Re = ZERO
    Tb_od_Im = ZERO

    do Ilay = 1, Nlay

      Tb_rc = Tb_rc + (tau_rc(Ilay-1) - tau_rc(Ilay))*t_lay(Ilay)
      Tb_lc = Tb_lc + (tau_lc(Ilay-1) - tau_lc(Ilay))*t_lay(Ilay)
      Tb_od_Re = Tb_od_Re + (tau_od_Re(Ilay-1) - tau_od_Re(Ilay))*t_lay(Ilay)
      Tb_od_Im = Tb_od_Im + (tau_od_Im(Ilay-1) - tau_od_Im(Ilay))*t_lay(Ilay)

    enddo

  end subroutine radiance_zeeman

end MODULE O2_zeeman_lbl
