!
! CRTM_BRDF_SfcOptics
!
! Module to compute the surface optical properties for BRDF model
! contribution to the radiative transfer.
!
!
! CREATION HISTORY:
!       Written by:     Quanhua Liu, JCSDA
!                       quanhua.liu@noaa.gov
!

MODULE CRTM_BRDF_SfcOptics

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds,                 ONLY: fp
  USE Message_Handler,            ONLY: SUCCESS
  USE Spectral_Units_Conversion,  ONLY: Inverse_cm_to_Micron
  USE CRTM_Parameters,            ONLY: ZERO, ONE, TWO, FOUR, PI, MAX_N_ANGLES
  USE CRTM_SpcCoeff,              ONLY: SC
  USE CRTM_Surface_Define,        ONLY: CRTM_Surface_type
  USE CRTM_GeometryInfo_Define,   ONLY: CRTM_GeometryInfo_type
  USE CRTM_SfcOptics_Define,      ONLY: CRTM_SfcOptics_type
  USE CRTM_Surface_IR_Emissivity, ONLY: Surface_IR_Emissivity
  ! Disable implicit typing
  IMPLICIT NONE

  ! ------------
  ! Visibilities
  ! ------------
  ! Everything private by default
  PRIVATE
  ! Data types
  ! Science routines
  PUBLIC :: Compute_Ocean_BRDF_SfcOptics
  PUBLIC :: HAPKE_VFUNCTION


  ! -----------------
  ! Module parameters
  ! -----------------
  ! RCS Id for the module
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &


CONTAINS
!
  FUNCTION Compute_Ocean_BRDF_SfcOptics(Wavelength,  &  ! Input
                                           Surface,  &  ! Input
                                      GeometryInfo,  &  ! Input
                                      SensorIndex ,  &  ! Input
                                      ChannelIndex,  &  ! Input
                                      SfcOptics,     &  ! Output
                                      Message_Log )  &  ! Error messaging
                                    RESULT ( Error_Status )
    ! Arguments
    TYPE(CRTM_Surface_type),      INTENT(IN)     :: Surface
    TYPE(CRTM_GeometryInfo_type), INTENT(IN)     :: GeometryInfo
    INTEGER,                      INTENT(IN)     :: ChannelIndex,SensorIndex
    TYPE(CRTM_SfcOptics_type),    INTENT(IN OUT) :: SfcOptics
    CHARACTER(*), OPTIONAL,       INTENT(IN)     :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Compute_Ocean_BRDF_SfcOptics'
    ! Local variables
    INTEGER :: i,j,k,mu,np
    REAL(fp) :: Azi, d_Azi, Source_Sensor_Azi, Wavelength,paw,rm0
    REAL(fp) :: rm(SfcOptics%n_Angles),rp(SfcOptics%n_Azimuth)
    REAL(fp) :: brdf(SfcOptics%n_Angles,SfcOptics%n_Azimuth)
    IF( SfcOptics%n_Azimuth <= 1 ) THEN
      print *,' n_Azimuth is too small to BRDF model ',n_Azimuth
      RETURN
      d_Azi = TWO * PI/( SfcOptics%n_Azimuth-1 )
    END IF
    ! ------
    ! Set up
    ! ------
    Error_Status = SUCCESS

    Source_Sensor_Azi = GeometryInfo%Source_Azimuth_Radian-GeometryInfo%Sensor_Azimuth_Radian
    IF( Source_Sensor_Azi < ZERO ) Source_Sensor_Azi = Source_Sensor_Azi + TWO*PI
      paw = GeometryInfo%Source_Azimuth_Radian/DEGREES_TO_RADIANS-Surface%Wind_Direction
      ! sun and satellite direct reflectance
      np = 1
      mu = 1
      rm0 = cos(GeometryInfo%Source_Zenith_Radian)
      rp(1) = Source_Sensor_Azi
      rm0 = cos(GeometryInfo%Source_Zenith_Radian)
      rm(1) = ONE / GeometryInfo%Secant_Sensor_Zenith

      call oceabrdf(Surface%Wind_Speed,paw,Surface%Salinity,Surface%Pigment_Concentration, &
           Wavelength,rm0,mu,np,rm(1:mu),rp(1:np),brdf)
      SfcOptics%Direct_Direct_Reflectivity(1) = brdf(1,1)
      
      ! direct reflectance
      np = SfcOptics%n_Azimuth
      mu = SfcOptics%n_Angles
      rm0 = cos(GeometryInfo%Source_Zenith_Radian)
      
      DO k = 1, SfcOptics%n_Azimuth
           rp(k) = (k-1)*d_Azi
      END DO
      rm(1:mu) = SfcOptics%Angle(1:mu)
      call oceabrdf(Surface%Wind_Speed,paw,Surface%Salinity,Surface%Pigment_Concentration, &
           Wavelength,rm0,mu,np,rm(1:mu),rp(1:np),brdf)
      SfcOptics%Direct_Reflectivity_save(1:mu,1,1:np) = brdf(1:mu,1:np)      
      
      ! reflectance
      np = SfcOptics%n_Azimuth
      mu = SfcOptics%n_Angles
      
      DO i = 1, SfcOptics%n_Angles
        rm0 = SfcOptics%Angle(i)
      
        DO k = 1, SfcOptics%n_Azimuth
           rp(k) = (k-1)*d_Azi
        END DO
        rm(1:mu) = SfcOptics%Angle(1:mu)
        call oceabrdf(Surface%Wind_Speed,paw,Surface%Salinity,Surface%Pigment_Concentration, &
           Wavelength,rm0,mu,np,rm(1:mu),rp(1:np),brdf)
        SfcOptics%Reflectivity_save(1:mu,1,i,1,1:np) = brdf(1:mu,1:np) 
      END DO  

  END FUNCTION Compute_Ocean_BRDF_SfcOptics
!
!
      SUBROUTINE oceabrdf(pws,paw,xsal,pcl,pwl,rm0,mu,np,rm,rp, &
                 brdfint)
!
! INPUT:  pws=wind speed (in m/s)
!         paw= azim. of sun - azim. of wind (in deg.)
!	  xsal=salinity (in ppt)
!	  pcl=pigment concentration (in mg.m-3)
!         pwl=wavelength of the computation (in micrometer)
!         mu=number of zenith angle
!         np=number of azimuth
!         rm=cosine of Gauss's angles for angles between -PI/2 and PI/2 deg
!         rp=Gauss's angles for angles between 0 and 2*PI
! OUTPUT: brdfint(j,k)=the total reflectance of the sea water
!
      parameter (nta=24,nfa=48)
      integer np,mu,k,j,m,n,iwl
      real(FP) rm(mu),rp(np),brdfint(mu,np),rm0
      real(FP) teta1,teta2,phi1,phi2,ta(nta),fa(nfa),wta(nta),wfa(nfa)
      real(FP) Ref(39)
      real(FP) pwl,paw,pcl,pws,wl,wspd,C,azw,xsal
      real(FP) pi,fac,nr,ni,n12
      real(FP) tetas,w,wlp,ref_i,rwc,rw,tds,summ,tp,fip
      real(FP) rogp,pond,tetav,tw,tdv,fi,rog,a,rwb
! effective reflectance of the whitecaps (Koepke, 1984)
      data Ref/ &
      0.220,0.220,0.220,0.220,0.220,0.220,0.215,0.210,0.200,0.190, &
      0.175,0.155,0.130,0.080,0.100,0.105,0.100,0.080,0.045,0.055, &
      0.065,0.060,0.055,0.040,0.000,0.000,0.000,0.000,0.000,0.000, &
      0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000/
! conversion of parameter
      C=pcl
      wspd=pws
      azw=paw
      wl=pwl
      pi=atan(1.)*4.
      fac=pi/180.
      tetas=acos(rm0)/fac
! COMPUTE INDEX OF WATER
      call indwat(wl,xsal,nr,ni)
      n12=sqrt(nr*nr+ni*ni)
! COMPUTE WHITECAPS REFLECTANCE (LAMBERTIAN)
      W=2.95e-06*(wspd**3.52)
      iwl=1+int((wl-0.2)/0.1)
      wlp=0.5+(iwl-1)*0.1
      Ref_i=ref(iwl+1)+(wl-wlp)/0.1*(ref(iwl)-ref(iwl+1))
      Rwc=W*Ref_i
! COMPUTE BACKSCATTERED REFLECTANCE FROM THE SEA WATER (LAMBERTIAN)
!  water reflectance below the sea surface
      call MORCASIWAT(wl,C,Rw)
! call gauss quadrature
      tds=1.0
      tdv=1.0
      if (Rw.gt.0.0001) then
        teta1=0.
        teta2=pi/2.
        call gauss(teta1,teta2,ta,wta,nta)
        phi1=0.
        phi2=2.*pi
        call gauss(phi1,phi2,fa,wfa,nfa)
! COMPUTE DOWNWARD TRANSMISSION FUNCTION
        tds=0.
        summ=0.
        do k=1,nfa
        do j=1,nta
          tp=ta(j)/fac
          fip=fa(k)/fac
          call sunglint(wspd,nr,ni,azw,tetas,tp,fip,rogp)
          pond=cos(ta(j))*sin(ta(j))*wfa(k)*wta(j)
          summ=summ+pond
          tds=tds+rogp*pond
        enddo
        enddo
        tds=1.-tds/summ
      endif
! Compute glint contribution,transmission terms and total reflectance
      Do 1 j=1,mu
        tetav=acos(rm(j))/fac
! compute upward transmission 
        if (Rw.gt.0.0001) then
          tw=asin(sin(tetav*fac)/nr)/fac
          tdv=0.
          summ=0.
          do n=1,nfa
          do m=1,nta
            tp=ta(m)/fac
            fip=fa(n)/fac
! here we assume no aborption
            call sunglint(wspd,1./nr,0.0_FP,azw,tw,tp,fip,rogp)
            pond=cos(ta(m))*sin(ta(m))*wfa(n)*wta(m)
            summ=summ+pond
            tdv=tdv+rogp*pond
          enddo
          enddo
          tdv=1.-tdv/summ
        endif
        Do 2 k=1,np
!!           if (j.eq.mu) then
!!             fi=rm(-mu)
!!           else
!!             fi=(rp(k)+rm(-mu))
!!           endif

   ! added by qliu
           fi=rp(k)
           
           if (fi.lt.0.) fi=fi+2.*pi
           if (fi.gt.(2.*pi)) fi=fi-2.*pi
           fi=fi/fac
! SUNGLINT REFLECTANCE
           call sunglint(wspd,nr,ni,azw,tetas,tetav,fi,rog)
!  water reflectance above the sea surface
! for explanation on value of a see OCEAALBE.f
           a=0.485
! add change in solid angle from under to above to surface
! that account for 1/(n12*n12) decrease in sea water directional
! reflectance
           Rwb=(1/(n12*n12))*tds*tdv*Rw/(1-a*Rw)
! TOTAL REFLECTANCE OF SEA WATER
           brdfint(j,k)=Rwc+(1-W)*Rog+(1-Rwc)*Rwb
 2      continue
 1    continue
      return
   end subroutine oceabrdf
!
  !------------------------------------------------------
  ! routines taken from OCEAROOLS.f of the 6Smodel
  !------------------------------------------------------

      subroutine morcasiwat(wl,C,R2)
! Spectral diffuse attenuation coefficient of Case I Waters as Predicted 
! by MOREL within the spectral range 400-700nm (1988, Journal of Geophysical 
! Research, Vol.93, No C9, pp 10749-10768)
!
! input parameters:	wl wavelength (IN MICROMETERS)
!			C  pigment concentration
! output parameter:	R2  reflectance of water
!
! According Morel,1988, we use:
!
! Kd	spectral value of the attenuation coefficient for 
!	 downwelling irradiance
!	 with: Kd=Kw+Xc*C**e
! Kw	spectral value of the diffuse attenuation coefficient 
!	 for pure oceanic water
! Xc, e	spectral coefficients to compute the diffuse attenuation 
!	 coefficient for pigment
! bb	total backscattering coefficient
!	 with: bb=0.5*bw+bbt*b
! bw	spectral value of the molecular scattering coefficient of water
! bbt,b	parameters to compute the scattering coefficients of pigments
!
! R2	reflectance of water below the surface
!	 with: R2=(0.33/u)*(bb/Kd)	where u is depending of R2
!
      real(FP) Kw,Kd
      real(FP) tKw(61),tXc(61),te(61),tbw(61)
      real(FP) wl,c,r2,xc,e,bw,bb,b,bbt,u1,r1,u2,err
      integer iwl

      data tKw/0.0209,0.0200,0.0196,0.0189,0.0183, &
       0.0182,0.0171,0.0170,0.0168,0.0166, &
       0.0168,0.0170,0.0173,0.0174,0.0175, &
       0.0184,0.0194,0.0203,0.0217,0.0240, &
       0.0271,0.0320,0.0384,0.0445,0.0490, &
       0.0505,0.0518,0.0543,0.0568,0.0615, &
       0.0640,0.0640,0.0717,0.0762,0.0807, &
       0.0940,0.1070,0.1280,0.1570,0.2000, &
       0.2530,0.2790,0.2960,0.3030,0.3100, &
       0.3150,0.3200,0.3250,0.3300,0.3400, &
       0.3500,0.3700,0.4050,0.4180,0.4300, &
       0.4400,0.4500,0.4700,0.5000,0.5500, &
       0.6500/
      data tXc/0.1100,0.1110,0.1125,0.1135,0.1126, &
       0.1104,0.1078,0.1065,0.1041,0.0996, &
       0.0971,0.0939,0.0896,0.0859,0.0823, &
       0.0788,0.0746,0.0726,0.0690,0.0660, &
       0.0636,0.0600,0.0578,0.0540,0.0498, &
       0.0475,0.0467,0.0450,0.0440,0.0426, &
       0.0410,0.0400,0.0390,0.0375,0.0360, &
       0.0340,0.0330,0.0328,0.0325,0.0330, &
       0.0340,0.0350,0.0360,0.0375,0.0385, &
       0.0400,0.0420,0.0430,0.0440,0.0445, &
       0.0450,0.0460,0.0475,0.0490,0.0515, &
       0.0520,0.0505,0.0440,0.0390,0.0340, &
       0.0300/
      data te/0.668,0.672,0.680,0.687,0.693, &
       0.701,0.707,0.708,0.707,0.704, &
       0.701,0.699,0.700,0.703,0.703, &
       0.703,0.703,0.704,0.702,0.700, &
       0.700,0.695,0.690,0.685,0.680, &
       0.675,0.670,0.665,0.660,0.655, &
       0.650,0.645,0.640,0.630,0.623, &
       0.615,0.610,0.614,0.618,0.622, &
       0.626,0.630,0.634,0.638,0.642, &
       0.647,0.653,0.658,0.663,0.667, &
       0.672,0.677,0.682,0.687,0.695, &
       0.697,0.693,0.665,0.640,0.620, &
       0.600/
      data tbw/0.0076,0.0072,0.0068,0.0064,0.0061, &
       0.0058,0.0055,0.0052,0.0049,0.0047, &
       0.0045,0.0043,0.0041,0.0039,0.0037, &
       0.0036,0.0034,0.0033,0.0031,0.0030, &
       0.0029,0.0027,0.0026,0.0025,0.0024, &
       0.0023,0.0022,0.0022,0.0021,0.0020, &
       0.0019,0.0018,0.0018,0.0017,0.0017, &
       0.0016,0.0016,0.0015,0.0015,0.0014, &
       0.0014,0.0013,0.0013,0.0012,0.0012, &
       0.0011,0.0011,0.0010,0.0010,0.0010, &
       0.0010,0.0009,0.0008,0.0008,0.0008, &
       0.0007,0.0007,0.0007,0.0007,0.0007, &
       0.0007/
      if (wl.lt.0.400.or.wl.gt.0.700)then
        R2=0.000
        goto 60
      endif

      iwl=1+nint((wl-0.400)/0.005)
      Kw=tKw(iwl)
      Xc=tXc(iwl)
      e=te(iwl)
      bw=tbw(iwl)
!
      if (abs(C).lt.0.0001)then
         bb=0.5*bw
         Kd=Kw
      else
         b=0.30*C**0.62
         bbt=0.002+0.02*(0.5-0.25*log10(C))*0.550/wl
         bb=0.5*bw+bbt*b
         Kd=Kw+Xc*C**e
      endif

      u1=0.75
      R1=0.33*bb/u1/Kd

 50   u2=0.90*(1.-R1)/(1.+2.25*R1)
      R2=0.33*bb/u2/Kd
      err=abs((R2-R1)/R2)
      if (err.lt.0.0001)goto 60
      R1=R2
      goto 50
 60   return
   end subroutine morcasiwat
!
       subroutine indwat(wl,xsal,nr,ni)
!
! input parameters:  wl=wavelength (in micrometers)
!                    xsal=salinity (in ppt), if xsal<0 then 34.3ppt by default
! output parameters: nr=index of refraction of sea water
!                    ni=extinction coefficient of sea water
!
       INTEGER, PARAMETER :: nw = 113
       real(FP) twl(nw),tnr(nw),tni(nw)
       real(FP) nr,ni,wl,xwl,yr,yi,nrc,nic,xsal
       integer i
! Indices of refraction for pure water from Hale and Querry, 
! Applied Optique, March 1973, Vol. 12,  No. 3, pp. 555-563
       data twl/ &
        0.250,0.275,0.300,0.325,0.345,0.375,0.400,0.425,0.445,0.475, &
        0.500,0.525,0.550,0.575,0.600,0.625,0.650,0.675,0.700,0.725, &
        0.750,0.775,0.800,0.825,0.850,0.875,0.900,0.925,0.950,0.975, &
        1.000,1.200,1.400,1.600,1.800,2.000,2.200,2.400,2.600,2.650, &
        2.700,2.750,2.800,2.850,2.900,2.950,3.000,3.050,3.100,3.150, &
        3.200,3.250,3.300,3.350,3.400,3.450,3.500,3.600,3.700,3.800, &
        3.900,4.000, &
          4.260,     4.490,     4.630, & 
          4.710,     5.000,     5.020,     5.270,     5.640, & 
          5.880,     6.170,     6.340,     6.660,     7.140, & 
          7.400,     7.960,     8.010,     8.130,     8.380, & 
          8.620,     8.880,     9.170,     9.470,     9.800, & 
         10.150,    10.550,    10.990,    11.460,    11.760, & 
         12.300,    12.900,    12.980,    13.600,    14.330, & 
         14.920,    15.740,    16.660,    17.460,    18.690, & 
         20.100,    20.610,    21.270,    23.250,    25.970, & 
         29.410,    33.890,    40.000,    48.780,    62.500, & 
         86.950,   142.800,   400.000/
        data tnr/ &
        1.362,1.354,1.349,1.346,1.343,1.341,1.339,1.338,1.337,1.336, &
        1.335,1.334,1.333,1.333,1.332,1.332,1.331,1.331,1.331,1.330, &
        1.330,1.330,1.329,1.329,1.329,1.328,1.328,1.328,1.327,1.327, &
        1.327,1.324,1.321,1.317,1.312,1.306,1.296,1.279,1.242,1.219, &
        1.188,1.157,1.142,1.149,1.201,1.292,1.371,1.426,1.467,1.483, &
        1.478,1.467,1.450,1.432,1.420,1.410,1.400,1.385,1.374,1.364, &
        1.357,1.351, &
          1.338,     1.332,     1.330, & 
          1.330,     1.325,     1.325,     1.312,     1.289, & 
          1.248,     1.363,     1.357,     1.334,     1.314, & 
          1.307,     1.291,     1.291,     1.286,     1.281, & 
          1.275,     1.268,     1.255,     1.247,     1.229, & 
          1.218,     1.185,     1.153,     1.126,     1.126, & 
          1.123,     1.146,     1.146,     1.177,     1.241, & 
          1.270,     1.297,     1.351,     1.401,     1.443, & 
          1.480,     1.487,     1.487,     1.511,     1.539, & 
          1.551,     1.536,     1.519,     1.555,     1.703, & 
          1.924,     2.056,     2.130/
        data tni/ &
        3.35E-08,2.35E-08,1.60E-08,1.08E-08,6.50E-09, &
        3.50E-09,1.86E-09,1.30E-09,1.02E-09,9.35E-10, &
        1.00E-09,1.32E-09,1.96E-09,3.60E-09,1.09E-08, &
        1.39E-08,1.64E-08,2.23E-08,3.35E-08,9.15E-08, &
        1.56E-07,1.48E-07,1.25E-07,1.82E-07,2.93E-07, &
        3.91E-07,4.86E-07,1.06E-06,2.93E-06,3.48E-06, &
        2.89E-06,9.89E-06,1.38E-04,8.55E-05,1.15E-04, &
        1.10E-03,2.89E-04,9.56E-04,3.17E-03,6.70E-03, &
        1.90E-02,5.90E-02,1.15E-01,1.85E-01,2.68E-01, &
        2.98E-01,2.72E-01,2.40E-01,1.92E-01,1.35E-01, &
        9.24E-02,6.10E-02,3.68E-02,2.61E-02,1.95E-02, &
        1.32E-02,9.40E-03,5.15E-03,3.60E-03,3.40E-03, &
        3.80E-03,4.60E-03, &
        0.8450E-02,0.1340E-01,0.1470E-01, & 
        0.1570E-01,0.1240E-01,0.1240E-01,0.9800E-02,0.1420E-01, & 
        0.6220E-01,0.8800E-01,0.5700E-01,0.3560E-01,0.3200E-01, & 
        0.3240E-01,0.3410E-01,0.3410E-01,0.3510E-01,0.3610E-01, & 
        0.3720E-01,0.3850E-01,0.4150E-01,0.4330E-01,0.4790E-01, & 
        0.5080E-01,0.6620E-01,0.9680E-01,0.1420E+00,0.1420E+00, & 
        0.2590E+00,0.3050E+00,0.3050E+00,0.3430E+00,0.3880E+00, & 
        0.4020E+00,0.4140E+00,0.4280E+00,0.4290E+00,0.4210E+00, & 
        0.3930E+00,0.3820E+00,0.3820E+00,0.3670E+00,0.3500E+00, & 
        0.3330E+00,0.3290E+00,0.3850E+00,0.4880E+00,0.5870E+00, & 
        0.5360E+00,0.5000E+00,0.5040E+00/
        i=2
 10     if (wl.lt.twl(i)) goto 20
        if (i.lt.nw) then
           i=i+1
           goto 10
           endif
 20     xwl=twl(i)-twl(i-1)        
        yr=tnr(i)-tnr(i-1)        
        yi=tni(i)-tni(i-1)        
        nr=tnr(i-1)+(wl-twl(i-1))*yr/xwl
        ni=tni(i-1)+(wl-twl(i-1))*yi/xwl
! 
! Correction to be applied to the index of refraction and to the extinction 
! coefficients of the pure water to obtain the ocean water one (see for 
! example Friedman). By default, a typical sea water is assumed 
! (Salinity=34.3ppt, Chlorinity=19ppt) as reported by Sverdrup. 
! In that case there is no correction for the extinction coefficient between 
! 0.25 and 4 microns. For the index of refraction, a correction of +0.006 
! has to be applied (McLellan). For a chlorinity of 19.0ppt the correction 
! is a linear function of the salt concentration. Then, in 6S users are able 
! to enter the salt concentration (in ppt).
! REFERENCES:
! Friedman D., Applied Optics, 1969, Vol.8, No.10, pp.2073-2078.
! McLellan H.J., Elements of physical Oceanography, Pergamon Press, Inc.,
!        New-York, 1965, p 129.
! Sverdrup H.V. et al., The Oceans (Prentice-Hall, Inc., Englewood Cliffs,
!        N.J., 1942, p 173.

        nrc=0.006
        nic=0.000
        nr=nr+nrc*(xsal/34.3)
        ni=ni+nic*(xsal/34.3)
        return
    end subroutine indwat
!
      subroutine sunglint(wspd,nr,ni,azw,ts,tv,fi,rog)
! input parameters:   wspd=speed of the wind (in m/s)
!                     nr=index of refraction of the sea water
!                     ni=extinction coefficient of the sea water
!                     azw=azim. of the sun - azim. of the wind (in deg.)
!                     ts=solar zenith angle (in deg.)
!                     tv=view zenith angle (in deg.)
!                     fi=relative azimuth (sun-satellite)
! output parameters:  rog=reflectance of the sun glint
!
      real(FP) pi,fac
      real(FP) wspd,nr,ni,ts,tv,fi,rog,azw,phw
      real(FP) cs,cv,ss,sv,phi,zx,zy,tantilt,tilt,proba,xe,xn,xe2,xn2
      real(FP) coef,cos2chi,coschi,sinchi
      real(FP) r1,sigmaC,sigmaU,C21,C03,C40,C04,C22
      pi=atan(1.)*4.
      fac=pi/180.
      phw=azw*fac
      cs=cos(ts*fac)
      cv=cos(tv*fac)
      ss=sin(ts*fac)
      sv=sin(tv*fac)
      phi=fi*fac
      Zx=-sv*sin(phi)/(cs+cv)
      Zy=(ss+sv*cos(phi))/(cs+cv)
      tantilt=sqrt(zx*zx+zy*zy)
      tilt=atan(tantilt)
!  Anisotropic Gaussian distribution
!    phw=phi_sun-phi_wind
      sigmaC=0.003+0.00192*wspd
      sigmaU=0.00316*wspd
      C21=0.01-0.0086*wspd
      C03=0.04-0.033*wspd
      C40=0.40
      C22=0.12
      C04=0.23
      xe=(cos(phw)*Zx+sin(phw)*Zy)/sqrt(SigmaC)
      xn=(-sin(phw)*Zx+cos(phw)*Zy)/sqrt(SigmaU)
      xe2=xe*xe
      xn2=xn*xn
      coef=1-C21/2.*(xe2-1)*xn-C03/6.*(xn2-3)*xn
      coef=coef+c40/24.*(xe2*xe2-6*xe2+3)
      coef=coef+C04/24.*(xn2*xn2-6*xn2+3)
      coef=coef+C22/4.*(xe2-1)*(xn2-1)
      proba=coef/2./pi/sqrt(sigmaU)/sqrt(sigmaC)*exp(-(xe2+xn2)/2.)
! Compute Fresnel's coefficient R1
      cos2chi=cv*cs+sv*ss*cos(phi)
      if (cos2chi.gt.1.0)cos2chi=0.99999999999
      if (cos2chi.lt.-1.0)cos2chi=-0.99999999999
      coschi=sqrt(0.5*(1+cos2chi))
      sinchi=sqrt(0.5*(1-cos2chi))
      Call Fresnel(nr,ni,coschi,sinchi,R1)
! Compute Reflectance of the sun glint
      Rog=pi*R1*proba/4./cs/cv/(cos(tilt)**4)
      return
   end subroutine sunglint
!
!
      Subroutine Fresnel(nr,ni,coschi,sinchi,R1)
!
! to compute the Fresnel's coefficient of reflection (see for
! example M. Born and E. Wolf, Principles of Optics, Pergamon Press, fifth
! edition, 1975, pp 628
! input parameters: nr=index of refraction of the sea water
!                   ni=extinction coefficient of the sea water
!                   coschi & sinchi=cosine and sine of the incident radiation 
!                                   with respect of the wave facet normal.
! output parameter: R1=Fresnel's coefficient for reflection
!
      real(FP) nr,ni,a1,a2,u,v,Rr2,Rl2,b1,b2,R1,coschi,sinchi
! absolute value for a1 to get v=0 when ni=0
      a1=abs(nr*nr-ni*ni-sinchi*sinchi)
      a2=sqrt((nr*nr-ni*ni-sinchi*sinchi)**2.+4*nr*nr*ni*ni)
      u=sqrt(0.5*(a1+a2))
      v=sqrt(0.5*(-a1+a2))
      Rr2=((coschi-u)**2+v*v)/((coschi+u)**2+v*v)
      b1=(nr*nr-ni*ni)*coschi
      b2=2*nr*ni*coschi
      Rl2=((b1-u)**2+(b2+v)**2)/((b1+u)**2+(b2-v)**2)
      R1=(Rr2+Rl2)/2.
      return
   end Subroutine Fresnel
!
!
      subroutine glitalbe(wspd,nr,ni,azw,rge)
!
! To compute the spherical albedo of the sea water. See for example
! Masuda et al., Remote Sens. Environ., 24, 313-329, 1988.
! 
! input parameters: wsp=wind of speed
!                   nr=index of refraction of the sea water
!                   ni=extinction coefficient of the sea water
!                   azw=azim. of sun - azim. of wind (in deg.)
! output parameter: rge=spherical albedo of the sun glint
!
      real(FP) nr,ni,azw,phw,rge,q,wspd,prefl,proba,pr,pp,pi,fac
      real(FP) sigma,sigmaC,sigmaU,C21,C03,C40,C04,C22
      real(FP) costt,hta,htb,hfa,cotb,cota,cofa,diff,coef
      real(FP) phin,cosphin,sinphin,costet,tet,sintet
      real(FP) costetn,sintetn,tantetn,coschi,sinchi
      real(FP) zx,zy,xe,xn,xe2,xn2,fonc0,pond,r1
      integer nta,nfa,ntb,km,i,j

      pi=atan(1.)*4.
      fac=pi/180.
      sigma=0.003+0.00512*wspd
      sigmaC=0.003+0.00192*wspd
      sigmaU=0.00316*wspd
      C21=0.01-0.0086*wspd
      C03=0.04-0.033*wspd
      C40=0.40
      C22=0.12
      C04=0.23
! costt to minimize the time of the computation
!     integration between 1 and costt instead of 1 and 0
      q=50
      costt=1./sqrt(1+q*sigma/4.)
      phw=azw*fac

      prefl=0.
      proba=0.

      ntb=31
      htb=1./float(ntb-1)
! loops on the zenith angle of the emitted radiation
      do km=1,ntb
        costet=(km-1)*htb
        tet=acos(costet)
        sintet=sin(tet)
        tet=tet/fac
! Simpson's rules for the angle of the emitted radiation teta
        cotb=2.
        diff=abs(km/2-km/2.)
        if (diff.lt.0.00001)cotb=4.
        if (km.eq.1.or.km.eq.ntb)cotb=1.0
!  loops step for phiN and tetaN (N is the facet unit normal vector)
        if (tet.lt.91)nta=801
        if (tet.lt.81)nta=301
        if (tet.lt.75)nta=101
        if (tet.lt.65)nta=31
        nfa=nta
        hta=(1.-costt)/float(nta-1)
        hfa=pi/float(nfa-1)
! loops on phiN (azimuth angle of the facet normal vector)
        pr=0.
        pp=0.
        do i=1,nfa
         phin=(i-1)*hfa
         cosphin=cos(phin)
         sinphin=sin(phin)
!  Simpson's rules for phin
         cofa=2.
         diff=abs(i/2-i/2.)
         if (diff.lt.0.00001)cofa=4.
         if (i.eq.1.or.i.eq.nfa)cofa=1.0
! loops on tetaN (zenith angle of the facet normal vector)
         do j=1,nta
          costetn=costt+(j-1)*hta
          sintetn=sqrt(1-costetn*costetn)
          tantetn=sintetn/costetn
!  Simpson's rules for tetaN
          cota=2.
          diff=abs(j/2-j/2.)
          if (diff.lt.0.00001)cota=4.
          if (j.eq.1.or.j.eq.nta)cota=1.0
! Fresnel's reflection coefficient R1
          coschi=costet*costetn+sintet*sintetn*cosphin
          if (coschi*coschi.gt.1.0)coschi=0.99999999999
          sinchi=sqrt(1-coschi*coschi)
          if (coschi.lt.0.0)then
            r1=0.
            cota=0.
          else
            Call Fresnel(nr,ni,coschi,sinchi,r1)
          endif
!  Anisotropic Gaussian distribution for wave facets slopes
          Zx=-tantetn*cosphin
          Zy=-tantetn*sinphin
          xe=(cos(phw)*Zx+sin(phw)*Zy)/sqrt(SigmaC)
          xn=(-sin(phw)*Zx+cos(phw)*Zy)/sqrt(SigmaU)
          xe2=xe*xe
          xn2=xn*xn
          coef=1-C21/2.*(xe2-1)*xn-C03/6.*(xn2-3)*xn
          coef=coef+c40/24.*(xe2*xe2-6*xe2+3)
          coef=coef+C04/24.*(xn2*xn2-6*xn2+3)
          coef=coef+C22/4.*(xe2-1)*(xn2-1)
          fonc0=0.5*coschi*coef*exp(-(xe2+xn2)/2.)/(costetn**4)
          pr=pr+r1*fonc0*cofa*cota*cotb
          pp=pp+fonc0*cofa*cota*cotb
         enddo
        enddo
!
        pond=2.*hta*hfa*htb/pi/sqrt(sigmaC)/sqrt(sigmaU)/3./3./3.
        prefl=prefl+pr*pond
        proba=proba+pp*pond
      enddo
      rge=prefl/proba
      return
   end subroutine glitalbe
!
      subroutine gauss(x1,x2,x,w,n)
      integer n
      real(FP) :: x1,x2,x(n),w(n)
      real(FP) :: xm,xl,z,p1,p2,p3,pp,z1
      integer m,i,j
      parameter (eps=3.E-14)
      m=(n+1)/2
      xm=0.5_FP*(x2+x1)
      xl=0.5_FP*(x2-x1)
      do 12 i=1,m
        z=cos(3.141592654_FP*(i-.25_FP)/(n+.5_FP))
1       continue
          p1=1.0_FP
          p2=0.0_FP
          do 11 j=1,n
            p3=p2
            p2=p1
            p1=((2.0_FP*j-1.0_FP)*z*p2-(j-1.0_FP)*p3)/j
11        continue
          pp=n*(z*p1-p2)/(z*z-1.0_FP)
          z1=z
          z=z1-p1/pp
        if(abs(z-z1).gt.eps)go to 1
        if (abs(z).lt.eps) z=0.0_FP
        x(i)=xm-xl*z
        x(n+1-i)=xm+xl*z
        w(i)=2.0_FP*xl/((1.0_FP-z*z)*pp*pp)
        w(n+1-i)=w(i)
12    continue
      return
      end subroutine gauss
!
! Land part
      SUBROUTINE HAPKE_VFUNCTION( MAXPARS, N_BRDF_STOKESSQ, NPARS, PARS, &
        XJ, SXJ, XI, SXI, PHI, CPHI, SKPHI, HAPKE_VKERNEL )
! -----------------------------------------------------------------------------
!  This subroutine is adopted from 6S code.
! -----------------------------------------------------------------------------
      INTEGER          MAXPARS, NPARS, N_BRDF_STOKESSQ
      DOUBLE PRECISION PARS ( MAXPARS )
      DOUBLE PRECISION XI, SXI, XJ, SXJ, PHI, CPHI, SKPHI
      DOUBLE PRECISION HAPKE_VKERNEL(MAXSTOKES_SQ)

!  Hapke Kernel function.
!  input variables:
!    XI, SXI  : Cosine/Sine of angle of reflection (positive)
!    XJ, SXJ  : Cosine/Sine of angle of incidence (positive)
!    XPHI     : Difference of azimuth angles of incidence and reflection
!    PARS(1)  : single scattering albedo in Hapke's BDR model
!    PARS(2)  : angular width parameter of opposition effect in Hapke's model
!    PARS(3)  : Empirical hot spot multiplier

!  local variables
!    B0_EMPIR : empirical factor to account for the finite size of
!               particles in Hapke's BDR model
!    B_HOT    : term that accounts for the opposition effect
!               (retroreflectance, hot spot) in Hapke's BDR model
!    CTHETA   : cosine of phase angle in Hapke's BDR model
!    GAMMA    : albedo factor in Hapke's BDR model
!    PHASE    : scattering phase function in Hapke's BDR model
!    THETA  : phase angle (radians); the angle between incidence and
!             reflection directions in Hapke's BDR model

!  local variables

      REAL(FP) :: CTHETA, THETA, PHASE
      REAL(FP) :: HOTSPOT, B0_EMPIR, HELP_HOT, B_HOT
      REAL(FP) :: SSALBEDO, GAMMA, REFLEC, f
      REAL(FP) :: HELP_J, TERM_J, HELP_I, TERM_I
      REAL(FP) :: XPHI, CKPHI

      HAPKE_VKERNEL = ZERO
      XPHI  = PIE - PHI
      CKPHI = - CPHI

!  geometrical part

!  This is the code that is in DISORT - not right, I think.
!       CTHETA = XI * XJ + DABS(SXI) *  DABS(SXJ) * CKPHI

      CTHETA = XI * XJ + SXI * SXJ * CKPHI
      IF ( CTHETA .GT. ONE ) CTHETA = ONE
      THETA  = DACOS( CTHETA )
      PHASE  = ONE + HALF * CTHETA

!  hot spot parameterization
      HOTSPOT  = PARS(2)
      B0_EMPIR = PARS(3)
      HELP_HOT = HOTSPOT + DTAN ( HALF * THETA )
      B_HOT    = B0_EMPIR * HOTSPOT / HELP_HOT

!  Albedo parameterization
      SSALBEDO = PARS(1)
      GAMMA    = DSQRT ( ONE - SSALBEDO )
      HELP_J   = TWO * XJ
      TERM_J   = ( ONE + HELP_J ) / ( ONE + HELP_J * GAMMA )
      HELP_I   = TWO * XI
      TERM_I   = ( ONE + HELP_I ) / ( ONE + HELP_I * GAMMA )

      REFLEC       = SSALBEDO * QUARTER / ( XI + XJ )
      f     = ( ONE + B_HOT ) * PHASE + TERM_J * TERM_I - ONE
      HAPKE_VKERNEL(1) = REFLEC * f

      RETURN
      END SUBROUTINE HAPKE_VFUNCTION
!
!
 END MODULE CRTM_BRDF_SfcOptics
!
