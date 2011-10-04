!--------------------------------------------------------------------------------
!M+
! NAME:
!       transmittance_microwave_lbl
!
! PURPOSE:
!       Module containing routines to compute line-by-line O2, N2 and H2O 
!       transmittances, with the inclusion of the Zeeman effect in O2
!       transmittance calculation.
!
!
! MODULES:
!       O2_zeeman_lbl: Module containing routines to compute line-by-line transmittances, 
!                      taking the Zeeman effect into account.
!       type_kinds:    Module containing definitions for kinds of
!                      variable types
! External routines:
!
!  real, External :
!    O2ABS, ABSN2, ABH2O : routines for calculations of O2 (without Zeeman), N2 and H2O
!                          absorption coefficients.
!
!
! CONTAINS:
!       tauO2N2_zeeman_lbl: subroutine to compute line-by-line transmittance (O2+N2) with the
!                           inclusion of Zeeman effect.
!
!              tauO2N2_lbl: subroutine to compute line-by-line transmittance 
!                           (O2 + N2) without the inclusion of Zeeman effect.
!
!              tau_H2O_lbl: subroutine to compute the water vapor transmittance 
!
!
! CREATION HISTORY:
!       Written by:     Yong Han, NOAA/NESDIS, Nov. 2006
!
!  Copyright (C) 2006 Yong Han
!
!M-
!--------------------------------------------------------------------------------
MODULE transmittance_microwave_lbl

  USE type_kinds, only : fp_kind
  USE O2_zeeman_lbl

  IMPLICIT NONE

  Private
  Public :: tauO2N2_zeeman_lbl
  Public :: tauO2N2_lbl
  Public :: tau_H2O_lbl
  Public :: radiance_lbl

  real, External :: O2ABS, ABSN2, ABH2O
  
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_H2O    = 18.01528_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: MW_DRYAIR = 28.9648_fp_kind
  REAL( fp_kind ), PUBLIC, PARAMETER :: EPS       = MW_H2O / MW_DRYAIR
  REAL( fp_kind ), PUBLIC, PARAMETER :: MOLAR_GAS_CONSTANT = 8.314472_fp_kind

  real(fp_kind), private, parameter :: ONE = 1.0_fp_kind
  real(fp_kind), private, parameter :: ZERO = 0.0_fp_kind
  real(fp_kind), private, parameter :: THOUSAND = 1000.0_fp_kind

!  real(fp_kind), private, parameter :: MAX_PRESSURE_ZEEMAN = 9000.0_fp_kind !3.0_fp_kind ! when pressure > MAX_PRESSURE_ZEEMAN, zeeman effect
!                                                                ! considered not significant.

  real(fp_kind), private, parameter :: MAX_PRESSURE_ZEEMAN = 3.0_fp_kind ! when pressure > MAX_PRESSURE_ZEEMAN, zeeman effect
                                                                ! considered not significant.
                   
CONTAINS

!---------------------------------------------------------------------------------
!  subroutine to compute line-by-line transmittance (O2 + N2) without the
!                inclusion of Zeeman effect.
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
!    p_lay    :  layer pressure profile in hPa (z_level gives its boundaries)
!                usually computed as p_lay(k) = (p_lev(k)-p_lev(k-1))/Log(p_lev(k)/p_lev(k-1)) 
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
! 
!    t_lay    :  layer mean temperature profile in K
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!    w_lay    :  water vapor mixing ratio in g/kg
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
! Outputs
!
!     tau     :  transmittance profile (O2 + N2)
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!   
!-----------------------------------------------------------------------------
  subroutine tauO2N2_lbl(frequency, secant_angle, &
                         z_level, p_lay, t_lay, w_lay, tau)

    real(fp_kind), intent(in)  :: frequency         ! GHz                                
    real(fp_kind), intent(in)  :: secant_angle                                      
    real(fp_kind), intent(in)  :: z_level(0:)       ! km                                 
    real(fp_kind), intent(in)  :: p_lay(:)          ! hPa (=mb)                          
    real(fp_kind), intent(in)  :: t_lay(:)          ! K                                  
    real(fp_kind), intent(in)  :: w_lay(:)          ! water vapor mixing ratio (g/kg)    
    real(fp_kind), intent(out) :: tau(:)            ! transmittance                      

    ! local                                
    integer  :: Nlay, Ilay                 
    real(fp_kind) :: rho_vapor, coef, od   

    Nlay = SIZE(p_lay)

    ! Compute the transmittance with the Zeeman effects are ignored.
    ! Note that N2 continuum is added.
	 
    do Ilay = 1, Nlay

      rho_vapor = mixingRatio2density(w_lay(Ilay), p_lay(Ilay), t_lay(Ilay))
      ! absorption coefficient in nepers/km
	  coef = real(O2ABS(real(t_lay(Ilay)), real(p_lay(Ilay)), &
	              real(rho_vapor), real(frequency)), fp_kind)
      coef = coef + real(ABSN2(real(t_lay(Ilay)),real(p_lay(Ilay)),real(frequency)), fp_kind)
      od = coef*(z_level(Ilay-1) - z_level(Ilay))*secant_angle
      if(Ilay == 1)then
	 tau(Ilay) = exp(-od)
      else
        tau(Ilay) = tau(Ilay-1)*exp(-od)
      endif
      
    enddo

  end subroutine tauO2N2_lbl

!---------------------------------------------------------------------------------
!  subroutine to compute line-by-line transmittance (O2+N2) with the
!                inclusion of Zeeman effect above the MAX_PRESSURE_ZEEMAN height. 
!                Below that level Zeeman effect is negligible and therefore is not included.
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
!    p_lay    :  layer pressure profile in hPa (z_level gives its boundaries)
!                usually computed as p_lay(k) = (p_lev(k)-p_lev(k-1))/Log(p_lev(k)/p_lev(k-1)) 
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
! 
!    t_lay    :  layer mean temperature profile in K
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!    w_lay    :  water vapor mixing ratio in g/kg
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
!
!  Optional outputs
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
!   linearBase : if present, the calculation uses linear polarization basis
!                if not present, it uses circular basis. 
!   
!-----------------------------------------------------------------------------

  subroutine tauO2N2_zeeman_lbl(frequency, secant_angle, &
                                z_level, p_lay, t_lay, w_lay, &
                                tau_11, tau_22, &
                                tau12_re, tau12_im, &
				Bfield, CBTH, linearBase)

    real(fp_kind), intent(in)  :: frequency         ! GHz
    real(fp_kind), intent(in)  :: secant_angle 
    real(fp_kind), intent(in)  :: z_level(0:)       ! km
    real(fp_kind), intent(in)  :: p_lay(:)          ! hPa (=mb)
    real(fp_kind), intent(in)  :: t_lay(:)          ! K
    real(fp_kind), intent(in)  :: w_lay(:)          ! water vapor mixing ratio (g/kg)
    real(fp_kind), intent(out) :: tau_11(:), &  ! right_circlar or vertical polarization transmittance to space 	                                             
                                                ! (tau(1) == transmittance at second level of the first layer)
	                          tau_22(:)     ! left_circlar or horizontal polarization transmittance to space
    real(fp_kind), optional, intent(out)  &
	                       :: tau12_Re(:), tau12_Im(:) ! real and imaginary parts of off-diagonal 
								                       ! elements of transmittance matrix
    real(fp_kind), intent(in)  :: Bfield(:)     ! MAGNETIC FIELD (Gauss)
                                                ! 1 Tesla = 10000 Gauss, 1 Tesla = 1.e9 nano Tesla
                                                ! The Earth Bfield is in the order of 0.5 Gauss (50 micro Tesla)
    real(fp_kind), intent(in)  :: CBTH(:)      ! COS(ANGLES BETWEEN MAGNETIC FIELD 
    integer, optional, intent(in)  :: linearBase  ! if present, the calculations are linear-pol base; if not, circular-pol base



    !local                                                                                                                          
    real(fp_kind) :: coef, od, rho_vapor                                                                                            
    integer       :: Nlay, Ilay, n                                                                                                  
    real(fp_kind), dimension(SIZE(p_lay))  :: tau_12_Re, &  ! real part of off-diagonal elements of transmittance matrix            
                                              tau_12_Im     ! imaginary part of off-diagonal elements

    integer  :: JU   ! index for unsplit O2 transition lines                                                                        

    Nlay = SIZE(p_lay)

    !-------------------------------------------------------------------------------
    ! Include the zeeman contributions when pressure <= MAX_PRESSURE_ZEEMAN
    ! and ignore zeeman effects when pressure > MAX_PRESSURE_ZEEMAN
    !-------------------------------------------------------------------------------
	  
    n = 0
    do Ilay = 1, Nlay
      if(p_lay(Ilay) > MAX_PRESSURE_ZEEMAN)then
        n = Ilay
        exit
      endif
    enddo

    if(n > 0)then
    
      call search_line(frequency, JU) 

      if(PRESENT(linearBase))then

        call tau_zeeman_lbl(frequency, secant_angle, &
                            z_level(0:n), p_lay(1:n), t_lay(1:n), &
                            Bfield(1:n), CBTH(1:n), &
                            tau_11(1:n), tau_22(1:n), &
                            tau_12_Re(1:n), tau_12_Im(1:n), & 
                            JU = JU, linearBase = linearBase)

      else

        call tau_zeeman_lbl(frequency, secant_angle, &
                            z_level(0:n), p_lay(1:n), t_lay(1:n), &
                            Bfield(1:n), CBTH(1:n), &
                            tau_11(1:n), tau_22(1:n), &
                            tau_12_Re(1:n), tau_12_Im(1:n), & 
                            JU = JU)
      endif

    endif

    ! Compute the portion of transmittance with the Zeeman effects ignored.
    ! Note that N2 continuum is added.

    do Ilay = n+1, Nlay

      rho_vapor = mixingRatio2density(w_lay(Ilay), p_lay(Ilay), t_lay(Ilay))
      ! absorption coefficient in nepers/km
      coef = real(O2ABS(real(t_lay(Ilay)), real(p_lay(Ilay)), &
	          real(rho_vapor), real(frequency)), fp_kind)
      coef = coef + real(ABSN2(real(t_lay(Ilay)),real(p_lay(Ilay)),real(frequency)), fp_kind)
      od = coef*(z_level(Ilay-1) - z_level(Ilay))*secant_angle

      if(Ilay == 1)then
        tau_11(Ilay) = exp(-od)
        tau_22(Ilay) = tau_11(Ilay)
        tau_12_Re(Ilay) = ZERO
        tau_12_Im(Ilay) = ZERO
      else
        tau_11(Ilay) = tau_11(Ilay-1)*exp(-od)
        tau_22(Ilay) = tau_22(Ilay-1)*exp(-od)
        tau_12_Re(Ilay) = tau_12_Re(Ilay-1)
        tau_12_Im(Ilay) = tau_12_Im(Ilay-1)
      endif
      
    enddo

    if(PRESENT(tau12_Re) .and. PRESENT(tau12_Im))then   
      tau12_Re = tau_12_Re                              
      tau12_Im = tau_12_Im                              
    endif                                               

  end subroutine tauO2N2_zeeman_lbl

!---------------------------------------------------------------------------------
!  subroutine to compute line-by-line water vapor transmittance
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
!    p_lay    :  layer pressure profile in hPa (z_level gives its boundaries)
!                usually computed as p_lay(k) = (p_lev(k)-p_lev(k-1))/Log(p_lev(k)/p_lev(k-1)) 
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
! 
!    t_lay    :  layer mean temperature profile in K
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
!    w_lay    :  water vapor mixing ratio in g/kg
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!
! Outputs
!
!     tau     :  Water vapor transmittance profile
!                TYPE: REAL( fp_kind )
!                DIMENSION: Rank-1 array (1:n_layers)
!   
!-----------------------------------------------------------------------------

  subroutine tau_H2O_lbl(frequency, secant_angle, &
                        z_level, p_lay, t_lay, w_lay, tau)

    real(fp_kind), intent(in)  :: frequency         ! GHz
    real(fp_kind), intent(in)  :: secant_angle 
    real(fp_kind), intent(in)  :: z_level(0:)       ! km
    real(fp_kind), intent(in)  :: p_lay(:)          ! hPa (=mb)
    real(fp_kind), intent(in)  :: t_lay(:)          ! K
    real(fp_kind), intent(in)  :: w_lay(:)          ! water vapor mixing ratio (g/kg)


    real(fp_kind), intent(out)  :: tau(:)           ! transmittance to space 
                                                    ! (tau(1) == transmittance at second level of the first layer)

    ! local
    integer       :: Nlay, Ilay
    real(fp_kind), dimension(SIZE(p_lay))  :: rho_vapor, ds
    real(fp_kind)                          :: od


    Nlay = SIZE(p_lay)
    do Ilay = 1, Nlay
      rho_vapor(Ilay) = mixingRatio2density(w_lay(Ilay), p_lay(Ilay), t_lay(Ilay))
    enddo

    ds = (z_level(0:Nlay-1) - z_level(1:Nlay)) * secant_angle

    ! first layer
    od = ds(1) * real( ABH2O(real(t_lay(1)), real(p_lay(1)), &
                       real(rho_vapor(1)), real(frequency)), fp_kind)
    tau(1) = exp(-od)

    ! loop over remaining layers
    do Ilay = 2, Nlay
      od = ds(Ilay)*real(ABH2O(real(t_lay(Ilay)),real(p_lay(Ilay)),&
                       real(rho_vapor(Ilay)),real(frequency)), fp_kind)
      tau(Ilay) = tau(Ilay-1) * exp(-od)

    enddo

  end subroutine tau_H2O_lbl   

  function mixingRatio2density(vapor_mixingRatio, p, t) RESULT(rho_vapor)
    
    real(fp_kind), intent(in) :: vapor_mixingRatio, p, t

    real(fp_kind)             :: rho_vapor

    ! -- Molecular weights
    REAL( fp_kind ), PARAMETER :: SCALE_FACTOR = 1.0e+02_fp_kind

    real(fp_kind) :: vapor_pp, w

    ! convert mixing ratio (g/kg) to partial pressure

    w = (vapor_mixingRatio/THOUSAND)/EPS
    vapor_pp = p * w / (ONE + w)
!    vapor_pp = p * (ONE - EPS/(vapor_mixingRatio/THOUSAND + EPS))  ! hPa (=mb)
    ! convert vapor partial pressure to density
    rho_vapor = SCALE_FACTOR * vapor_pp * MW_H2O / &     ! g /m^3
    !                    ---------------------------
                         ( MOLAR_GAS_CONSTANT * t ) 

  end function mixingRatio2density

  function radiance_lbl(tau, t_lay) RESULT(rad)

    REAL(fp_kind), INTENT(IN) :: tau(:)
    REAL(fp_kind), INTENT(IN) :: t_lay(:)
    
    REAL(fp_kind) :: rad

    ! local 
    INTEGER :: Nlay, Ilay

    Nlay = SIZE(t_lay)

    rad = (ONE - tau(1))*t_lay(1)

    do Ilay = 2, Nlay

        rad = rad + (tau(Ilay-1) - tau(Ilay))*t_lay(Ilay)

    enddo

  end function radiance_lbl

end MODULE transmittance_microwave_lbl
