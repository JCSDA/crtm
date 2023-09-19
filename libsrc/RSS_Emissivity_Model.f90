! this is just Specular emissivity part of FORTRAN code for wind emissivity model in RSS SMAP V3.0 salinity release
! Thomas Meissner
! RSS
! October 15, 2018 

! References: 
! 1.	Meissner, T.; F. Wentz and D. Le Vine, Aquarius Salinity Retrieval Algorithm Theoretical Basis Document (ATBD), 
!       End of Mission Version; RSS Technical Report 120117; December 1, 2017; 
!       Available online at ftp://podaac-ftp.jpl.nasa.gov/allData/aquarius/docs/v5/AQ-014-PS-0017_Aquarius_ATBD-EndOfMission.pdf.
!
! 2.    Meissner, T, F. Wentz, and D, Le Vine, 2018, 
!       The Salinity Retrieval Algorithms for the NASA Aquarius Version 5 and SMAP Version 3 Releases, 
!       Remote Sensing 10, 1121, doi:10.3390/rs10071121. 
!
! 3.    Meissner, T. and F. Wentz, The complex dielectric constant of pure and sea water from microwave satellite observations, 
!       IEEE TGRS, 2004, 42(9), 1836 – 1849, doi:10.1109/TGRS.2004.831888.
!
! 4. 	Meissner, T. and F. Wentz, The emissivity of the ocean surface between 6 and 90 GHz 
!       over a large range of wind speeds and Earth incidence angles, 
!       IEEE TGRS, 2012, 50(8), 3004 – 3026, doi: 10.1109/TGRS.2011.2179662. 
!
! 5. 	Meissner, T., F. Wentz, F. and L. Ricciardulli, The emission and scattering of L-band microwave radiation 
!       from rough ocean surfaces and wind speed measurements from Aquarius, 
!       J. Geophys. Res. Oceans, 2014, 119, doi:10.1002/2014JC009837.
!



module RSS_Emissivity_Model

USE Type_Kinds        , ONLY: fp
implicit none
private
save
                     
! Declare local constant Pi
REAL, PARAMETER :: Pi = 3.14159265358979323846
REAL(fp), parameter  :: f0=17.97510

public ::  fdem0_meissner_wentz, &
 FDEM0_MEISSNER_WENTZ_TL, &
 FDEM0_MEISSNER_WENTZ_AD

contains


subroutine fdem0_meissner_wentz(freq,tht,sst,salinity, em0) 
!    input:
!    name   parameter		unit		range
!     
!    freq     frequency 	[GHz]		>0
!    tht      EIA               [deg]       [0, 90[
!    sst      SST		[C]		-25 c to 40 c for pure water
!						-2  c to 34 c for saline water
!    salinity salinity		[ppt]		0 to 40
!
!    output:
!    EM0     specular emissivity        [0,1]
!	         2-dimesnional vector, 1=v-pol, 2=h-pol 		 

implicit none

real(fp), intent(in)			  :: freq
real(fp), intent(in)			  :: tht,sst,salinity
complex(fp), dimension(2), intent(out)    :: em0 
real(fp), parameter			  :: f0=17.97510
real(fp)				  :: costht,sinsqtht,tht_r
real(fp)		         	  :: e0s,e1s,e2s,n1s,n2s,sig
complex(fp)				  :: permit,esqrt,rh,rv
complex(fp), parameter			  :: j=(0.,1.)

!convert phir to radian                                                                                             
tht_r =tht *pi/180.0_fp
call dielectric_meissner_wentz(sst,salinity,  e0s,e1s,e2s,n1s,n2s,sig)

costht=cos(tht_r)
sinsqtht=1.-costht*costht

!     debye law (2 relaxation wavelengths)
permit = (e0s - e1s)/(1.0 - j*(freq/n1s)) + (e1s - e2s)/(1.0 - j*(freq/n2s)) + e2s + j*sig*f0/freq
permit = conjg(permit)
	
esqrt  = sqrt(permit-sinsqtht)
rh     = (costht-esqrt)/(costht+esqrt)
rv     = (permit*costht-esqrt)/(permit*costht+esqrt)
em0(1) = 1.-rv*conjg(rv)
em0(2) = 1.-rh*conjg(rh)

return
end subroutine fdem0_meissner_wentz


subroutine dielectric_meissner_wentz(sst_in,s,   e0s,e1s,e2s,n1s,n2s,sig)
!
!     complex dielectric constant: eps
!     [MW 2004, MW 2012].
!     References:
!     [MW 2004]:   T. Meissner and F. J. Wentz, 
!              "The complex dielectric constant of pure and sea water from microwave satellite observations," 
!              IEEE Trans. Geosci. Remote Sens., vol. 42, no.9, pp 1836 – 1849, 2004. 
!
!     [MW 2012]:   T. Meissner and F. J. Wentz, 
!              "The Emissivity of the Ocean Surface between 6 – 90 GHz over a Large Range of Wind Speeds and Earth Incidence Angles,"
!              IEEE Trans. Geosci. Remote Sens., vol. 50, no.8, pp 3004 - 3026, 2012.
!   
!     Changes from [MW 2012]:
!     1. Typo (sign) in the printed version of coefficient d3 in Table 7. Its value should be -0.35594E-06.
!     2. Changed SST behavior of coefficient b2 from:
!     b2 = 1.0 + s*(z(10) + z(11)*sst) to
!     b2 = 1.0 + s*(z(10) + 0.5*z(11)*(sst + 30)) 
!
!     input:
!     name   parameter  unit  range
!     sst      sst      [c]   -25 c to 40 c for pure water
!                             -2  c to 34 c for saline water
!     s      salinity   [ppt]  0 to 40
!
!     output:
!     Debye pparameters: e0s,e1s,e2s,n1s,n2s,sig
!     The permittivity can be calculated in the subroutine: fdpermit_meissner_wentz 

implicit none

real(fp), intent(in)  :: sst_in,s
real(fp), intent(out) :: e0s,e1s,e2s,n1s,n2s,sig
real(fp), dimension(11), parameter :: &
      x=(/ 5.7230e+00, 2.2379e-02, -7.1237e-04, 5.0478e+00, -7.0315e-02, 6.0059e-04, 3.6143e+00, &
           2.8841e-02, 1.3652e-01,  1.4825e-03, 2.4166e-04 /)
real(fp), dimension(13), parameter :: &
      z=(/ -3.56417e-03,  4.74868e-06,  1.15574e-05,  2.39357e-03, -3.13530e-05, &
            2.52477e-07, -6.28908e-03,  1.76032e-04, -9.22144e-05, -1.99723e-02, &
            1.81176e-04, -2.04265e-03,  1.57883e-04  /)  ! 2004
real(fp), dimension(3), parameter :: a0coef=(/ -0.33330E-02,  4.74868e-06,  0.0e+00/)
real(fp), dimension(5), parameter :: b1coef=(/0.23232E-02, -0.79208E-04, 0.36764E-05, -0.35594E-06, 0.89795E-08/)
real(fp)                          :: e0,e1,e2,n1,n2
real(fp)                          :: a0,a1,a2,b1,b2
real(fp)                          :: sig35,r15,rtr15,alpha0,alpha1
real(fp)                          :: sst,sst2,sst3,sst4,s2
	 
sst=sst_in
if(sst.lt.-30.16) sst=-30.16  !protects against n1 and n2 going zero for very cold water

sst2=sst*sst
sst3=sst2*sst
sst4=sst3*sst
s2=s*s

!     pure water
      e0    = (3.70886e4 - 8.2168e1*sst)/(4.21854e2 + sst) ! stogryn et al.
      e1    = x(1) + x(2)*sst + x(3)*sst2
      n1    = (45.00 + sst)/(x(4) + x(5)*sst + x(6)*sst2)
      e2    = x(7) + x(8)*sst
      n2    = (45.00 + sst)/(x(9) + x(10)*sst + x(11)*sst2)

!     saline water
!     conductivity [s/m] taken from stogryn et al.
      sig35 = 2.903602 + 8.60700e-2*sst + 4.738817e-4*sst2 - 2.9910e-6*sst3 + 4.3047e-9*sst4
      r15   = s*(37.5109+5.45216*s+1.4409e-2*s2)/(1004.75+182.283*s+s2)
 
      alpha0 = (6.9431+3.2841*s-9.9486e-2*s2)/(84.850+69.024*s+s2)
      alpha1 = 49.843 - 0.2276*s + 0.198e-2*s2
      rtr15 = 1.0 + (sst-15.0)*alpha0/(alpha1+sst)
 
      sig = sig35*r15*rtr15
 
!     permittivity
      a0 = exp(a0coef(1)*s + a0coef(2)*s2 + a0coef(3)*s*sst)
      e0s = a0*e0

      if(sst.le.30) then
         b1 = 1.0 + s*(b1coef(1) + b1coef(2)*sst + b1coef(3)*sst2 + b1coef(4)*sst3 + b1coef(5)*sst4)
      else
	 b1 = 1.0 + s*(9.1873715e-04 + 1.5012396e-04*(sst-30))
      endif
      n1s = n1*b1

      a1  = exp(z(7)*s + z(8)*s2 + z(9)*s*sst)
      e1s = e1*a1

!     b2 = 1.0 + s*(z(10) + z(11)*sst)
      b2 = 1.0 + s*(z(10) + 0.5*z(11)*(sst + 30))
      n2s = n2*b2


      a2 = 1.0  + s*(z(12) + z(13)*sst)
      e2s = e2*a2
 
return
end subroutine  dielectric_meissner_wentz

!!!Tangent part
!        Generated by TAPENADE     (INRIA, Ecuador team)
!  Tapenade 3.15 (master) - 15 Apr 2020 11:54
!
!        Generated by TAPENADE     (INRIA, Ecuador team)
!  Tapenade 3.15 (master) - 15 Apr 2020 11:54
!
!  Differentiation of fdem0_meissner_wentz in forward (tangent) mode:
!   variations   of useful results: em0
!   with respect to varying inputs: sst
SUBROUTINE FDEM0_MEISSNER_WENTZ_TL(freq, tht, sst, sst_TL, salinity,salinity_TL, em0, &
& em0_TL)
  IMPLICIT NONE
  REAL(FP), INTENT(IN)                   :: freq
  REAL(FP), INTENT(IN)                   :: tht, sst, salinity
  REAL(FP), INTENT(IN)                   :: sst_TL,salinity_TL
  COMPLEX(FP), DIMENSION(2), INTENT(OUT) :: em0
  COMPLEX(FP), DIMENSION(2), INTENT(OUT) :: em0_TL
  REAL(FP)                               :: costht, sinsqtht, costht_TL, sinsqtht_TL, freq_TL,tht_TL
  REAL(FP)                               :: e0s, e1s, e2s, n1s, n2s, sig
  REAL(FP)                               :: e0s_TL, e1s_TL, e2s_TL, n1s_TL, n2s_TL, sig_TL
  COMPLEX(FP)                            :: permit, esqrt, rh, rv
  COMPLEX(FP)                            :: permit_TL, esqrt_TL, rh_TL, rv_TL
  COMPLEX, PARAMETER                     :: j=(0.,1.)
  REAL(FP)                               :: tht_r, tht_r_TL
  INTRINSIC COS
  INTRINSIC CONJG
  COMPLEX(FP)  :: temp
  COMPLEX(FP)  :: temp0
  COMPLEX(FP)  :: temp1
  COMPLEX(FP)  :: temp2
  COMPLEX(FP)  :: temp3
  
  freq_TL  = 0.0_fp
  tht_TL   = 0.0_fp
  tht_r_TL = pi*tht_TL/180.0_fp
  tht_r = tht*pi/180.0_fp
  CALL DIELECTRIC_MEISSNER_WENTZ_TL(sst, sst_TL, salinity, salinity_TL, e0s, e0s_TL, e1s, &
&                            e1s_TL, e2s, e2s_TL, n1s, n1s_TL, n2s, n2s_TL, sig&
&                            , sig_TL)
  costht_TL = -(SIN(tht_r)*tht_r_TL)
  costht = COS(tht_r)
  sinsqtht_TL = -(2*costht*costht_TL)
  sinsqtht = 1. - costht*costht
!     debye law (2 relaxation wavelengths)                                                                                                   
  temp = -(j*freq/n1s) + 1.0
  temp0 = (e0s-e1s)/temp
  temp1 = -(j*freq/n2s) + 1.0
  temp2 = (e1s-e2s)/temp1
  permit_TL = (e0s_TL-e1s_TL+temp0*j*(freq_TL-freq*n1s_TL/n1s)/n1s)/temp + (e1s_TL-&
&   e2s_TL+temp2*j*(freq_TL-freq*n2s_TL/n2s)/n2s)/temp1 + e2s_TL + j*f0*(sig_TL-&
&   sig*freq_TL/freq)/freq
  permit = temp0 + temp2 + e2s + j*f0*(sig/freq) 
  permit_TL = CONJG(permit_TL)
  permit = CONJG(permit)
  temp3 = SQRT(permit - sinsqtht)
  IF (permit - sinsqtht .EQ. 0.0) THEN
    esqrt_TL = 0.0
  ELSE
    esqrt_TL = (permit_TL-sinsqtht_TL)/(2.0*temp2)
  END IF
  esqrt = temp3
  temp2 = (costht-esqrt)/(costht+esqrt)
  rh_TL = (costht_TL-esqrt_TL-temp2*(costht_TL+esqrt_TL))/(costht+esqrt)
  rh = temp2
  temp2 = (permit*costht-esqrt)/(permit*costht+esqrt)
  rv_TL = (costht*permit_TL+permit*costht_TL-esqrt_TL-temp2*(costht*permit_TL+&
&   permit*costht_TL+esqrt_TL))/(permit*costht+esqrt)
  rv = temp2
  temp3 = CONJG(rv)
  em0_TL(1) = -(temp3*rv_TL+rv*CONJG(rv_TL))
  em0(1) = 1. - rv*temp3
  temp3 = CONJG(rh)
  em0_TL(2) = -(temp3*rh_TL+rh*CONJG(rh_TL))
  em0(2) = 1. - rh*temp3

  RETURN

END SUBROUTINE FDEM0_MEISSNER_WENTZ_TL

!  Differentiation of dielectric_meissner_wentz in forward (tangent) mode:
!   variations   of useful results: e2s n1s e0s sig n2s e1s
!   with respect to varying inputs: sst_in
SUBROUTINE DIELECTRIC_MEISSNER_WENTZ_TL(sst_in, sst_in_TL, salinity, salinity_TL, e0s, e0s_TL&
& , e1s, e1s_TL, e2s, e2s_TL, n1s, n1s_TL, n2s, n2s_TL, sig, sig_TL)
  IMPLICIT NONE
  REAL(FP), INTENT(IN)               :: sst_in, salinity
  REAL(FP), INTENT(IN)               :: sst_in_TL, salinity_TL
  REAL(FP), INTENT(OUT)              :: e0s, e1s, e2s, n1s, n2s, sig
  REAL(FP), INTENT(OUT)              :: e0s_TL, e1s_TL, e2s_TL, n1s_TL, n2s_TL, sig_TL
  REAL(FP), DIMENSION(11), PARAMETER :: x=(/5.7230e+00, 2.2379e-02, -&
&   7.1237e-04, 5.0478e+00, -7.0315e-02, 6.0059e-04, 3.6143e+00, &
&   2.8841e-02, 1.3652e-01, 1.4825e-03, 2.4166e-04/)                                                                                                
  REAL(FP), DIMENSION(13), PARAMETER :: z=(/-3.56417e-03, 4.74868e-06, &
&   1.15574e-05, 2.39357e-03, -3.13530e-05, 2.52477e-07, -6.28908e-03, &
&   1.76032e-04, -9.22144e-05, -1.99723e-02, 1.81176e-04, -2.04265e-03, &
&   1.57883e-04/)
  REAL(FP), DIMENSION(3), PARAMETER  :: a0coef=(/-0.33330e-02, 4.74868e-06&
&   , 0.0e+00/)
  REAL(FP), DIMENSION(5), PARAMETER  :: b1coef=(/0.23232e-02, -0.79208e-04&
&   , 0.36764e-05, -0.35594e-06, 0.89795e-08/)
  REAL(FP)                           :: e0, e1, e2, n1, n2
  REAL(FP)                           :: e0_TL, e1_TL, e2_TL, n1_TL, n2_TL
  REAL(FP)                           :: a0, a1, a2, b1, b2
  REAL(FP)                           :: a0_TL, a1_TL, a2_TL, b1_TL, b2_TL
  REAL(FP)                           :: sig35, r15, rtr15, alpha0, alpha1
  REAL(FP)                           :: sig35_TL, r15_TL, rtr15_TL, alpha0_TL, alpha1_TL
  REAL(FP)                           :: s,sst, sst2, sst3, sst4, s2
  REAL(FP)                           :: sst_TL, s_TL,sst2_TL, sst3_TL, sst4_TL, s2_TL
  INTRINSIC EXP
  REAL(FP) :: temp
  REAL(FP) :: temp0
  sst    = sst_in
  sst_TL = sst_in_TL
  s   = salinity
  s_TL= salinity_TL
!protects against n1 and n2 going zero for very cold water                                            
  IF (sst .LT. -30.16) THEN
    sst    = -30.16
    sst_TL = 0.0_4
  END IF
  sst2_TL = 2*sst*sst_TL
  sst2    = sst*sst
  sst3_TL = sst*sst2_TL + sst2*sst_TL
  sst3    = sst2*sst
  sst4_TL = sst*sst3_TL + sst3*sst_TL
  sst4    = sst3*sst
  s2_TL   = 2 * s * s_TL
  s2      = s * s
!     pure water                                                                                      
! stogryn et al.                                                                                      
  temp  = (-(8.2168e1*sst)+3.70886e4)/(sst+4.21854e2)
  e0_TL = -((temp+8.2168e1)*sst_TL/(sst+4.21854e2))
  e0    = temp
  e1_TL = x(2)*sst_TL + x(3)*sst2_TL
  e1    = x(1) + x(2)*sst + x(3)*sst2
  temp  = x(4) + x(5)*sst + x(6)*sst2
  temp0 = (sst+45.00)/temp
  n1_TL = (sst_TL-temp0*(x(5)*sst_TL+x(6)*sst2_TL))/temp
  n1    = temp0
  e2_TL = x(8)*sst_TL
  e2    = x(7) + x(8)*sst
  temp0 = x(9) + x(10)*sst + x(11)*sst2
  temp  = (sst+45.00)/temp0
  n2_TL = (sst_TL-temp*(x(10)*sst_TL+x(11)*sst2_TL))/temp0
  n2    = temp
!     saline water                                                                                    
!     conductivity [s/m] taken from stogryn et al.                                                    
  sig35_TL = 8.60700e-2*sst_TL + 4.738817e-4*sst2_TL + 4.3047e-9*sst4_TL - &
&   2.9910e-6*sst3_TL
  sig35    = 2.903602 + 8.60700e-2*sst + 4.738817e-4*sst2 - 2.9910e-6*sst3 &
&   + 4.3047e-9*sst4
  temp0    = s/(182.283*s+s2+1004.75)
  temp     = 5.45216*s + 1.4409e-2*s2 + 37.5109
  r15_TL   = temp0*(5.45216*s_TL+1.4409e-2*s2_TL) + temp*(s_TL-temp0*(182.283*s_TL+&
&   s2_TL))/(182.283*s+s2+1004.75)
  r15       = temp*temp0
  temp0     = (3.2841*s-9.9486e-2*s2+6.9431)/(69.024*s+s2+84.850)
  alpha0_TL = (3.2841*s_TL-9.9486e-2*s2_TL-temp0*(69.024*s_TL+s2_TL))/(69.024*s+s2&
&   +84.850)
  alpha0    = temp0
  alpha1_TL = 0.198e-2*s2_TL - 0.2276*s_TL
  alpha1    = 49.843 - 0.2276*s + 0.198e-2*s2
  temp0     = (sst-15.0)*alpha0/(alpha1+sst)
  rtr15_TL  = (alpha0*sst_TL+(sst-15.0)*alpha0_TL-temp0*(alpha1_TL+sst_TL))/(alpha1&
&   +sst)
  rtr15     = temp0 + 1.0
  sig_TL    = rtr15*(r15*sig35_TL+sig35*r15_TL) + sig35*r15*rtr15_TL
  sig       = sig35*r15*rtr15
!     permittivity                                                                                    
  temp0 = a0coef(1)*s + a0coef(2)*s2 + a0coef(3)*s*sst
  a0_TL = EXP(temp0)*(a0coef(1)*s_TL+a0coef(2)*s2_TL+a0coef(3)*(sst*s_TL+s*sst_TL)&
&   )
  a0     = EXP(temp0)
  e0s_TL = e0*a0_TL + a0*e0_TL
  e0s    = a0*e0
  IF (sst .LE. 30) THEN
    temp0 = b1coef(1) + b1coef(2)*sst + b1coef(3)*sst2 + b1coef(4)*sst3 &
&     + b1coef(5)*sst4
    b1_TL = temp0*s_TL + s*(b1coef(2)*sst_TL+b1coef(3)*sst2_TL+b1coef(4)*sst3_TL+&
&     b1coef(5)*sst4_TL)
    b1 = s*temp0 + 1.0
  ELSE
    b1_TL = (1.5012396e-04*(sst-30)+9.1873715e-04)*s_TL + s*1.5012396e-04*&
&     sst_TL
    b1 = 1.0 + s*(9.1873715e-04+1.5012396e-04*(sst-30))
  END IF
  n1s_TL = b1*n1_TL + n1*b1_TL
  n1s    = n1*b1
  temp0  = z(7)*s + z(8)*s2 + z(9)*s*sst
  a1_TL  = EXP(temp0)*(z(7)*s_TL+z(8)*s2_TL+z(9)*(sst*s_TL+s*sst_TL))
  a1     = EXP(temp0)
  e1s_TL = a1*e1_TL + e1*a1_TL
  e1s    = e1*a1
!     b2 = 1.0 + s*(z(10) + z(11)*sst)                                                                
  temp0  = z(10) + 0.5*z(11)*(sst+30)
  b2_TL  = temp0*s_TL + s*z(11)*0.5*sst_TL
  b2     = s*temp0 + 1.0
  n2s_TL = b2*n2_TL + n2*b2_TL
  n2s    = n2*b2
  temp0  = z(12) + z(13)*sst
  a2_TL  = temp0*s_TL + s*z(13)*sst_TL
  a2     = s*temp0 + 1.0
  e2s_TL = a2*e2_TL + e2*a2_TL
  e2s    = e2*a2
  RETURN
END SUBROUTINE DIELECTRIC_MEISSNER_WENTZ_TL


! Adjoint part
! derived analytically
!"x.adjoint = x.adjoint +f(x) * J "
 
SUBROUTINE FDEM0_MEISSNER_WENTZ_AD(freq, tht, sst, sst_AD, &
& salinity,salinity_AD, em0, em0_AD)
  IMPLICIT NONE
  REAL(FP), INTENT(IN)                     :: freq, tht
  REAL(FP), INTENT(IN)                     ::  sst, salinity
  REAL(FP), INTENT(INOUT)                  :: salinity_AD, sst_AD
  REAL(FP), DIMENSION(2), INTENT(INOUT) :: em0, em0_AD
  COMPLEX(FP), DIMENSION(2)                :: d_em0_dsst, d_em0_ds
  
  REAL(FP) :: costht, sinsqtht
  REAL(FP) :: e0s, e1s, e2s, n1s, n2s, sig
  REAL(FP) :: d_e0s_dsst,d_e0s_ds,d_e1s_dsst,d_e1s_ds,d_e2s_dsst,d_e2s_ds
  REAL(FP) :: d_n1s_dsst,d_n1s_ds, d_n2s_dsst,d_n2s_ds, d_sig_dsst,d_sig_ds 

  COMPLEX(FP):: permit, esqrt, rh, rv
  COMPLEX(FP):: d_permit_dsst, d_permit_ds, d_esqrt_dsst
  COMPLEX(FP):: d_esqrt_ds,d_rh_dsst ,d_rh_ds ,d_rv_dsst ,d_rv_ds 
  COMPLEX(FP), PARAMETER :: j=(0.,1.)
  REAL(FP) :: tht_r

  
tht_r =tht *pi/180.0_fp
call dielectric_meissner_wentz(sst,salinity,  e0s,e1s,e2s,n1s,n2s,sig)
!call dielectric_meissner_wentz(sst,salinity,  e0s,e1s,e2s,n1s,n2s,sig)
call DIELECTRIC_MEISSNER_WENTZ_K(sst, salinity, d_e0s_dsst, d_e0s_ds &
& , d_e1s_dsst, d_e1s_ds, d_e2s_dsst, d_e2s_ds, d_n1s_dsst, &
d_n1s_ds, d_n2s_dsst, d_n2s_ds, d_sig_dsst, d_sig_ds)
!print *, "F1"                                                                                                                            
costht=cos(tht_r)
sinsqtht=1.-costht*costht

!     debye law (2 relaxation wavelengths)                                                                                                
permit = (e0s - e1s)/(1.0 - j*(freq/n1s)) + (e1s - e2s)/(1.0 - j*(freq/n2s)) + e2s + j*sig*f0/freq
d_permit_dsst = (d_e0s_dsst-d_e1s_dsst) /(1.0 - j*(freq/n1s)) - (e0s - e1s) *(j*freq)/(n1s-j*freq)**2 * d_n1s_dsst &
+ (d_e1s_dsst-d_e2s_dsst) /(1.0 - j*(freq/n2s)) - (e1s - e2s) *(j*freq)/(n2s-j*freq)**2 * d_n2s_dsst &
+ d_e2s_dsst + j* d_sig_dsst *f0/freq

d_permit_ds = (d_e0s_ds-d_e1s_ds)/(1.0 - j*(freq/n1s)) - (e0s - e1s) *(j*freq)/(n1s-j*freq)**2 * d_n1s_ds &
+ (d_e1s_ds-d_e2s_ds) /(1.0 - j*(freq/n2s)) - (e1s - e2s) *(j*freq)/(n2s-j*freq)**2 * d_n2s_ds &
+ d_e2s_ds + j* d_sig_ds *f0/freq

permit        = CONJG(permit)
d_permit_dsst = CONJG(d_permit_dsst)
d_permit_ds   = CONJG(d_permit_ds)
!print *, "permit", permit                                                                                                                
esqrt  = sqrt(permit-sinsqtht)

d_esqrt_dsst  = d_permit_dsst /(2*sqrt(permit-sinsqtht))
d_esqrt_ds    = d_permit_ds /(2*sqrt(permit-sinsqtht))

rh     = (costht-esqrt)/(costht+esqrt)

d_rh_dsst = -d_esqrt_dsst * (2*costht)/(costht+esqrt)**2
d_rh_ds   = -d_esqrt_ds * (2*costht)/(costht+esqrt)**2

rv        = (permit*costht-esqrt)/(permit*costht+esqrt)
d_rv_dsst = (d_permit_dsst*costht-d_esqrt_dsst)/(permit*costht+esqrt) -(permit*costht-esqrt)/(permit*costht+esqrt)**2 *(d_permit_dsst*costht + d_esqrt_dsst)
d_rv_ds   = (d_permit_ds*costht-d_esqrt_ds)/(permit*costht+esqrt) -(permit*costht-esqrt)/(permit*costht+esqrt)**2 *(d_permit_ds*costht + d_esqrt_ds)

em0(1) = 1.-rv*CONJG(rv)
em0(2) = 1.-rh*CONJG(rh)

d_em0_dsst(1) = -d_rv_dsst*CONJG(rv) -rv*CONJG(d_rv_dsst) 
d_em0_dsst(2) = -d_rh_dsst*CONJG(rh) -rh*CONJG(d_rh_dsst)
d_em0_ds(1)   = -d_rv_ds*CONJG(rv) -rv*CONJG(d_rv_ds)
d_em0_ds(2)   = -d_rh_ds*CONJG(rh) -rh*CONJG(d_rh_ds)

sst_AD        = sst_AD + d_em0_dsst(1)* em0_AD(1)
sst_AD        = sst_AD + d_em0_dsst(2)* em0_AD(2)
salinity_AD   = salinity_AD  + d_em0_ds(1)* em0_AD(1)
salinity_AD   = salinity_AD  + d_em0_ds(2)* em0_AD(2)

END SUBROUTINE FDEM0_MEISSNER_WENTZ_AD

! jacobians of outputs of DIELECTRIC_MEISSNER_WENTZ for salinity and SST
SUBROUTINE DIELECTRIC_MEISSNER_WENTZ_K(sst_in, salinity, d_e0s_dsst, d_e0s_ds &
& , d_e1s_dsst, d_e1s_ds, d_e2s_dsst, d_e2s_ds, d_n1s_dsst, & 
d_n1s_ds, d_n2s_dsst, d_n2s_ds, d_sig_dsst, d_sig_ds)
  IMPLICIT NONE
  REAL(FP), INTENT(IN)               :: sst_in, salinity
  REAL(FP)                           :: d_e0s_dsst,d_e0s_ds,d_e1s_dsst
  REAL(FP)                           :: d_e1s_ds,d_e2s_dsst,d_e2s_ds
  REAL(FP)                           :: d_n1s_dsst,d_n1s_ds, d_n2s_dsst
  REAL(FP)                           :: d_n2s_ds, d_sig_dsst,d_sig_ds 
  REAL(FP)                           :: e0s, e1s, e2s, n1s, n2s, sig
  REAL(FP), DIMENSION(11), PARAMETER :: x=(/5.7230e+00, 2.2379e-02, -&
&   7.1237e-04, 5.0478e+00, -7.0315e-02, 6.0059e-04, 3.6143e+00, &
&   2.8841e-02, 1.3652e-01, 1.4825e-03, 2.4166e-04/)
  REAL(FP), DIMENSION(13), PARAMETER :: z=(/-3.56417e-03, 4.74868e-06, &
&   1.15574e-05, 2.39357e-03, -3.13530e-05, 2.52477e-07, -6.28908e-03, &
&   1.76032e-04, -9.22144e-05, -1.99723e-02, 1.81176e-04, -2.04265e-03, &
&   1.57883e-04/)
  REAL(FP), DIMENSION(3), PARAMETER  :: a0coef=(/-0.33330e-02, 4.74868e-06&
&   , 0.0e+00/)
  REAL(FP), DIMENSION(5), PARAMETER  :: b1coef=(/0.23232e-02, -0.79208e-04&
&   , 0.36764e-05, -0.35594e-06, 0.89795e-08/)
  REAL(FP) :: e0, e1, e2, n1, n2
  REAL(FP) :: sst, s,a0, a1, a2, b1, b2
  REAL(FP) :: sig35, r15, rtr15, alpha0, alpha1
  REAL(FP) :: d_e0_dsst,d_e1_dsst,d_n1_dsst,d_e2_dsst
  REAL(FP) :: d_n2_dsst, d_sig35_dsst,d_r15_ds, d_alpha0_ds
  REAL(FP) :: d_alpha1_ds,d_rtr15_dsst,d_rtr15_ds
  REAL(FP) :: d_a0_dsst,d_a0_ds,d_a1_dsst,d_a1_ds,d_a2_dsst
  REAL(FP) :: d_a2_ds,d_b1_dsst,d_b1_ds,d_b2_dsst,d_b2_ds
  sst = sst_in
  s = salinity
  if(sst.lt.-30.16) sst=-30.16  !protects against n1 and n2 going zero for very cold water 
  call dielectric_meissner_wentz(sst,salinity,  e0s,e1s,e2s,n1s,n2s,sig)
!     pure water                                                                                                                               
      e0    = (3.70886e4 - 8.2168e1*sst)/(4.21854e2 + sst) ! stogryn et al.                                                                    
      d_e0_dsst=(-7.1751499e4)/(4.21854e2 + sst)**2                                                                                                       
      
      e1    = x(1) + x(2)*sst + x(3)*(sst)**2
      d_e1_dsst = x(2)+2*x(3)*sst                                                                                                                    
      
      n1    = (45.00 + sst)/( x(4) + x(5)*sst + x(6)*(sst**2) )
      d_n1_dsst=-(x(6)* sst**2 +90*x(6)*sst+45*x(5)-x(4))/(x(6)*sst**2+x(5)*sst+x(4))**2                                                                 
      
      e2    = x(7) + x(8)*sst
      d_e2_dsst= x(8)
                                                                                                                               
      n2    = (45.00 + sst)/(x(9) + x(10)*sst  + x(11)*(sst**2))
      d_n2_dsst=-(x(11)* sst**2+90*x(11)*sst+45*x(10)-x(9))/(x(11)* sst**2 +x(10)*sst+x(9))**2                                                            

!     saline water                                                                                                                             
!     conductivity [s/m] taken from stogryn et al.                                                                                             
      sig35 = 2.903602 + 8.60700e-2*sst + 4.738817e-4*sst**2 - 2.9910e-6* sst**3 + 4.3047e-9*sst**4
      d_sig35_dsst=8.60700e-2 + 4.738817e-4*2*sst - 2.9910e-6*3*sst**2 + 4.3047e-9*4*sst**3                                                           

      r15   = s*(37.5109+5.45216*s+1.4409e-2*s**2)/(1004.75+182.283*s+s**2)
      d_r15_ds    =(0.014409*s**4+5.25302*s**3+999.756*s**2+10956.114*s+37689.076)/(s**2+182.283*s+1004.750)**2                              

      alpha0 = (6.9431+3.2841*s-9.9486e-2*s**2)/(84.850+69.024*s+s**2)
      d_alpha0_ds  = -(10.15096*s**2+30.76896*s+200.584)/(s**2+69.024*s+84.85)**2

      alpha1 = 49.843 - 0.2276*s + 0.198e-2*s**2
      d_alpha1_ds  = 0.396e-2*s-0.2276
                                                                                                                             
      rtr15 = 1.0 + (sst-15.0)*alpha0/(alpha1+sst)
      d_rtr15_dsst = (alpha0*(alpha1+15))/(sst+alpha1)**2                                                                                       
      d_rtr15_ds = d_alpha0_ds * (sst-15.0)/(alpha1+sst) - alpha0 *(sst-15.0) * d_alpha1_ds /(sst+alpha1)**2 

      !sig = sig35*r15*rtr15
      d_sig_dsst= r15* (d_sig35_dsst *rtr15 + sig35 * d_rtr15_dsst)                                                                            
      d_sig_ds  = sig35 *(d_r15_ds *rtr15 + r15*d_rtr15_ds)                                                                                        

!     permittivity                                                                                                                             
      a0 = exp(a0coef(1)*s + a0coef(2)*s**2 + a0coef(3)*s*sst)
      d_a0_dsst =(a0coef(3)*s) * exp(a0coef(1)*s + a0coef(2)*s**2 + a0coef(3)*s*sst)                                                                
      d_a0_ds   =(2*a0coef(2)*s+a0coef(1)+a0coef(3)*sst)* exp(s*(a0coef(2)*s+a0coef(1)+a0coef(3)*sst))                                              

      !e0s = a0*e0
      d_e0s_dsst= d_a0_dsst * e0 +a0*d_e0_dsst                                                                                                      
      d_e0s_ds   = e0*d_a0_ds                                                                                                                       

      if(sst.le.30) then
         b1  = 1.0 + s*(b1coef(1) + b1coef(2)*sst + b1coef(3)*sst**2 + b1coef(4)*sst**3 + b1coef(5)*sst**4)
         d_b1_dsst = s*(b1coef(2)+2*b1coef(3)*sst + 3*b1coef(4)*sst**2 + 4*b1coef(5)*sst**3)                                                             
         d_b1_ds =b1coef(1) + b1coef(2)*sst + b1coef(3)*sst**2 + b1coef(4)*sst**3 + b1coef(5)*sst**4                                                      
      else
         b1  = 1.0 + s*(9.1873715e-04 + 1.5012396e-04*(sst-30))
         d_b1_dsst = s*(1.5012396e-04)                                                                                                                 
         d_b1_ds   =  9.1873715e-04 + 1.5012396e-04*(sst-30)                                                                                           
      endif
      !n1s = n1*b1
      d_n1s_dsst= d_n1_dsst *b1 + n1 * d_b1_dsst                                                                                                   
      d_n1s_ds  = n1 * d_b1_ds
                                                               
      a1        = exp(z(7)*s + z(8)*s**2 + z(9)*s*sst)
      d_a1_dsst = exp(z(7)*s + z(8)*s**2) *z(9)*s*exp(z(9)*s*sst)                                                                                     
      d_a1_ds   = (2*z(8)*s+z(7)+z(9)*sst) * exp(s*(z(8)*s+z(7)+z(9)*sst)) 

      !e1s = e1*a1
      d_e1s_dsst = d_e1_dsst*a1 +e1* d_a1_dsst                                                                                                       
      d_e1s_ds   = e1*d_a1_ds                                                                                                                       

      b2  = 1.0 + s*(z(10) + 0.5*z(11)*(sst + 30))
      d_b2_dsst = s*0.5*z(11)                                                                                                                       
      d_b2_ds   = z(10) + 0.5*z(11)*(sst + 30)                                                                                                        
      
      !n2s = n2*b2
      d_n2s_dsst = d_n2_dsst *b2 +n2* d_b2_dsst                                                                                                      
      d_n2s_ds   = n2*d_b2_ds                                                                                                                       

      a2 = 1.0  + s*(z(12) + z(13)*sst)
      d_a2_dsst = s*z(13)                                                                                                                     
      d_a2_ds   = z(12) + z(13)*sst

      !e2s = e2*a2
      d_e2s_dsst = d_e2_dsst *a2 +e2*d_a2_dsst                                                                                                       
      d_e2s_ds   = e2*d_a2_ds                                                                                                                        

END SUBROUTINE DIELECTRIC_MEISSNER_WENTZ_K

end module RSS_Emissivity_Model


