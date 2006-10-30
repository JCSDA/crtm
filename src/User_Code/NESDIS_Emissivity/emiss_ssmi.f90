subroutine emiss_ssmi(ntype_index,theta,frequency,mv,veg_frac, veg_tp, soil_tp, &
     t_soil, t_skin, snow_depth, tbb, esh, esv)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis SSM/I emissivity model over snow/ice
!
!   prgmmr: Banghua Yan      org: nesdis              date: 2004-02-12
!           Fuzhong Weng
!
! abstract:
!    Please refer to the following paper for details
!    Yan, B., F. Weng and K.Okamoto,2004: "A microwave snow emissivity model, submitted to TGRS
!
!   version: 4
!
! program history log:
!   2004-01-01  yan,b -  implement the algorithm for snow/ice emissivity
!   2004-02-01  yan,b -  modify the code for SSI
!   2004-07-23  okamoto - modify the code for GSI
!
! input argument list:
!      ntype_index    -  surface type
!              1 : ocean
!              2 : sea ice
!              3 : snow over land
!              4 : non-snow land
!      theta       -  local zenith angle in radian
!      frequency   -  frequency in GHz
!      veg_frac    -  Vegetation fraction (0 - 1.0)   (GDAS)
!      veg_tp      -  Vegetation type              (GDAS, not used)
!                        1: Broadleave Evergreen Trees
!                        2: Broadleave Deciduous Trees
!                        3: Broad & Needle Mixed Forest
!                        4: Needleleave Evergreen Trees
!                        5: Needleleave Deciduous Trees
!                        6: Broadleave Tree with Groundcover (Savana)
!                        7: Groundcover Only (Perenial Groundcover)
!                        8: Broadleave Shrubs with Perenial Groundcover
!                        9: Broadleave Shrubs with Bare Soil
!                        10: Dwarf Trees & Shrubs with Bare Soil
!                        11: Bare Soil'
!                        12: Cultivations (use paramater 7)
!                        13: Glacial
!      soil_tp     -  Soil type        (GDAS, not used)
!                        1: Loamy Sand (coarse)
!                        2: Silty Clayloam (medium)
!                        3: Light Clay (fine)
!                        4: Sand Loam (coarse-medium)
!                        5: Sandy Clay (coarse-fine)
!                        6: Clay Loam (medium-fine)
!                        7: Sandy Clay loam (coarse-med-fine)
!                        8: Loam (organic)
!                        9: Ice (use loamy sand property)
!      t_soil      -  soil temperature (K)    (GDAS)
!      t_skin      -  scattering layer temperature (K)   (GDAS)
!      mv          -  volumetric moisture content in soil (0.0 - 1.0) (GDAS)
!      mg          -  gravimetric water content (0.0 - 1.0) (GDAS,not used)
!      snow_depth  -  scatter medium depth (mm?)        (GDAS)
!      tbb[1] ~ tbb[7]: brightness temperature at four SSM/I
!                 tbb[1] :  at 19.35 GHz  v-polarization
!                 tbb[2] :  at 19.35 GHz  h-polarization
!                 tbb[3] :  at 22.235 GHz v-polarization
!                 tbb[4] :  at 37 GHz     v-polarization
!                 tbb[5] :  at 37 GHz     h-polarization
!                 tbb[6] :  at 85 GHz     v-polarization
!                 tbb[7] :  at 85 GHz     h-polarization
!       When tbb[ ]  = -999.9, it means a missing value (no available data)
!
! output argument list:
!   em_vector : esv, esh
!      esv    : emissivity at vertical polarization
!      esh    : emissivity at horizontal polarization
!
! important internal variables:
!
!   rhob    -  bulk volume density of the soil (1.18-1.12)
!   rhos    -  density of the solids (2.65 g.cm^3 for solid soil material)
!   sand    -  sand fraction (sand + clay = 1.0)
!   clay    -  clay fraction
!   lai     -  leaf area index (eg. lai = 4.0 for corn leaves)
!   sigma   -  surface roughness formed between medium 1 and 2,
!                  expressed as he standard deviation of roughtness height (mm)
!   leaf_thick  -  leaf thickness (mm)
!   rad     -  radius of dense medium scatterers (mm)
!   va      -  fraction volume of dense medium scatterers(0.0-1.0)
!   slnt    -  salinity (per throusand)
!   ep      -  dielectric constant of ice or sand particles, complex value
!                               (e.g, 3.0+i0.0)
!
! remarks:
!
!  Questions/comments: Please send to Fuzhong.Weng@noaa.gov and Banghua.Yan@noaa.gov
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind
  use constants, only: zero, half, one
  implicit none
  
  real(r_kind):: rhob,rhos,sand,clay
  integer      :: nw,nwv,nwh
  parameter(rhob=1.18_r_kind,rhos=2.65_r_kind,sand=0.8_r_kind,clay=0.2_r_kind)
  parameter(nw=7,nwv=4,nwh=3)
  
  real(r_kind) theta,frequency,mv, mg, mge, veg_frac, veg_tp, soil_tp, &
       t_soil, t_skin, snow_depth, em_vector(2)
  real(r_kind) tbb(nw),tv(nwv),th(nwh)
  real(r_kind) t_scat, b, theta_i,theta_t, mu,  &
       r12_h, r12_v, r21_h, r21_v, r23_h, r23_v,t21_v, t21_h, t12_v, t12_h, &
       gv, gh, ssalb_h,ssalb_v,tau_h,tau_v, esh, esv,      &
       lai, leaf_thick, slnty, rad, sigma, va
  real(r_kind) ev_default,eh_default
  complex(r_kind) ep, esoil, eveg, eair, esand
  integer ntype_index
  
  data ev_default/0.9_r_kind/
  data eh_default/0.88_r_kind/
  
  esh  =  eh_default  ;  esv  =  ev_default
  eair = dcmplx(one, -one)
  t_scat = t_skin
  b = t_scat
!        write(6,'(a,i3,a,7f8.2)') 'emiss_ssmi: type=',ntype_index,' tbb=',tbb
  if (ntype_index .ne. 4) then
     tv(1) = tbb(1);  tv(2) = tbb(3);  tv(3) = tbb(4);  tv(4) = tbb(6)
     th(1) = tbb(2); th(2) = tbb(5);  th(3) = tbb(7)
     call ossmem(ntype_index, theta,frequency,t_skin,tv,th,em_vector)
     esh = em_vector(1) ;  esv = em_vector(2)
  else
     sigma = half
     lai = 3.0_r_kind*veg_frac + half
     mge = half*veg_frac
     leaf_thick = 0.07_r_kind
     slnty = 3.0_r_kind
     mu  = cos(theta)
     r12_h = zero;  r12_v = zero;  r21_h = zero; r21_v = zero
     t21_h = one; t21_v = one; t12_v = one; t12_h = one
     theta_i  = theta    ! in radian
     call soil_diel(frequency, t_soil, mv, rhob, rhos, sand, clay, esoil)
     theta_t = dasin(dreal(sin(theta_i)*zsqrt(eair)/zsqrt(esoil)))
     call reflectance(eair, esoil, theta_i, theta_t, r23_v, r23_h)
     call rough_reflectance(frequency, theta_i, sigma, r23_v, r23_h)
     call canopy_diel(frequency,mge, eveg)
     call canopy_optic(lai, frequency, theta_i, eveg, leaf_thick, &
          gv, gh, ssalb_v, ssalb_h, tau_v, tau_h)
     call two_stream_solution( b, mu, gv, gh, ssalb_h,  ssalb_v,tau_h, tau_v, &
          r12_h, r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h,  &
          esv, esh)
  end if
  
  if(esh.gt.one) esh = one
  if(esh.lt.0.35_r_kind) esh = 0.35_r_kind
  if(esv.gt.one) esv = one
  if(esv.lt.0.35_r_kind) esv = 0.35_r_kind
  
  return
end subroutine emiss_ssmi


subroutine ossmem(ntype_index, theta,frequency,ts,tv,th,em_vector)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis SSM/I emissivity model over snow/ice
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2004-02-12
!
! abstract: Simulate microwave emissivity over ocean, sea ice and snow
!           using SSM/I  measurements and surface temperature
!
! program history log:
!
!      01/2004   : Implement the algorithm for snow/ice emissivity to F90 code by Banghua Yan
!      02/2004   : Modify the code for SSI subsystem                           by Banghua Yan
!      07/2004   : Modify the code for GSI subsystem                           by Kozo Okamoto
!
! input argument list:
!
!      ntype_index  : surface type
!                 1 : ocean
!                 2 : sea ice
!                 3 : snow over land
!      theta    : local zenith angle in radian  (not used here)
!      frequency: frequency in GHz
!      ts       :  scattering layer temperature (K)
!      tv[1] ~ tv[4]: brightness temperature at four SSM/I vertical polarization
!          tv[1] : 19.35  GHz
!          tv[2] : 22.235 GHz
!          tv[3] : 37     GHz
!          tv[4] : 85     GHz
!
!      th[1] ~ th[3]: brightness temperature at three SSM/I horizontal polarization
!          th[1] : 19.35  GHz
!          th[2] : 37     GHz
!          th[3] : 85     GHz
! output argument list:
!
!      em_vector     :  emissivity at two polarizations
!           em_vector[1] = eh
!           em_vector[2] = ev
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind
  use constants, only: one
  implicit none
  
  integer,parameter :: ntype = 3, nv=4, nh=3,ncoev=5,ncoeh=4
  integer ntype_index,ich,k,lp,nch
  real(r_kind) theta,frequency,ts,tv(*),th(*),em_vector(*)
  real(r_kind) ev(nv),eh(nh),freq_v(nv),freq_h(nh),ev_22
  real(r_kind) coe_v(ntype,nv,ncoev),coe_h(ntype,nh,ncoeh),pe , ev_cor,eh_cor
  logical data_invalid
  
  data freq_v/19.35_r_kind, 22.235_r_kind, 37.0_r_kind, 85.0_r_kind/
  data freq_h/19.35_r_kind, 37.0_r_kind, 85.0_r_kind/
  
! ocean
  data (coe_v(1,1,k),k=1,5)/ &
       -5.650765e-002_r_kind,  2.796378e-003_r_kind,  4.603629e-004_r_kind ,  &
       1.163488e-003_r_kind, -6.402050e-004_r_kind/
  data (coe_v(1,2,k),k=1,5)/ &
       -7.773900e-002_r_kind, -1.390087e-003_r_kind,  4.374652e-003_r_kind , &
       1.893395e-003_r_kind, -1.053837e-003_r_kind/
  data (coe_v(1,3,k),k=1,5)/ &
       -1.774548e-001_r_kind, -1.280647e-003_r_kind,  7.487299e-004_r_kind , &
       5.565533e-003_r_kind, -7.623489e-004_r_kind/
  data (coe_v(1,4,k),k=1,5)/ &
       -2.941845e-001_r_kind, -1.522888e-003_r_kind,  6.942110e-004_r_kind , &
       1.798103e-003_r_kind,  3.735965e-003_r_kind/
  data (coe_h(1,1,k),k=1,4)/ &
       -7.468897e-002_r_kind,  3.759362e-003_r_kind,  1.529964e-004_r_kind , &
       -4.894926e-005_r_kind/
  data (coe_h(1,2,k),k=1,4)/ &
       -1.989357e-001_r_kind,  1.534271e-004_r_kind,  4.117263e-003_r_kind , &
       1.201543e-004_r_kind/
  data (coe_h(1,3,k),k=1,4)/ &
       -3.180339e-001_r_kind, -4.772533e-005_r_kind,  1.822194e-004_r_kind , &
       4.691318e-003_r_kind/
  
! ice
  data (coe_v(2,1,k),k=1,5)/ -8.722723e-002_r_kind,  1.064573e-002_r_kind, &
       -5.333843e-003_r_kind, -1.394910e-003_r_kind,  4.007640e-004_r_kind/
  data (coe_v(2,2,k),k=1,5)/-1.373924e-001_r_kind,  6.580569e-003_r_kind, &
       -9.991220e-004_r_kind, -1.476022e-003_r_kind,  4.131816e-004_r_kind/
  data (coe_v(2,3,k),k=1,5)/ -2.329867e-001_r_kind,  6.419856e-003_r_kind, &
       -5.260987e-003_r_kind, 3.342582e-003_r_kind,  4.139272e-004_r_kind/
  data (coe_v(2,4,k),k=1,5)/ -3.528638e-001_r_kind,  6.342649e-003_r_kind, &
       -5.002575e-003_r_kind, -1.469298e-003_r_kind,  5.529711e-003_r_kind/
  data (coe_h(2,1,k),k=1,4)/ &
       -1.338736e-001_r_kind,  6.229798e-003_r_kind, -2.169491e-003_r_kind,  &
       5.706367e-004_r_kind/
  data (coe_h(2,2,k),k=1,4)/ &
       -2.747500e-001_r_kind,  2.041477e-003_r_kind,  2.581898e-003_r_kind,  &
       5.924890e-004_r_kind/
  data (coe_h(2,3,k),k=1,4)/ &
       -3.889575e-001_r_kind,  2.188889e-003_r_kind, -2.253243e-003_r_kind,  &
       5.750499e-003_r_kind/
  
!snow
  data (coe_v(3,1,k),k=1,5)/  1.109066e-001_r_kind,  5.449409e-003_r_kind,  &
       1.835799e-004_r_kind, -1.765248e-003_r_kind, -2.996101e-004_r_kind/
  data (coe_v(3,2,k),k=1,5)/ 9.356505e-002_r_kind,  1.320617e-003_r_kind,  &
       4.449195e-003_r_kind, -1.786960e-003_r_kind, -3.479687e-004_r_kind/
  data (coe_v(3,3,k),k=1,5)/ 6.387097e-002_r_kind,  1.252447e-003_r_kind,  &
       1.998846e-004_r_kind, 2.680219e-003_r_kind, -3.740141e-004_r_kind/
  data (coe_v(3,4,k),k=1,5)/ 4.150689e-002_r_kind,  1.420274e-003_r_kind,  &
       1.223339e-004_r_kind, -1.948946e-003_r_kind,  4.248289e-003_r_kind/
  
  data (coe_h(3,1,k),k=1,4)/ &
       8.503807e-002_r_kind,  5.357374e-003_r_kind, -1.361660e-003_r_kind, &
       -3.319696e-004_r_kind/
  data (coe_h(3,2,k),k=1,4)/ &
       4.200333e-002_r_kind,  1.278894e-003_r_kind,  2.963129e-003_r_kind, &
       -4.087036e-004_r_kind/
  data (coe_h(3,3,k),k=1,4)/ &
       2.082461e-002_r_kind,  1.438480e-003_r_kind, -1.723992e-003_r_kind,  &
       4.194914e-003_r_kind/
  
  save  coe_v,coe_h
  

!*** Quality check
  if(ntype_index < 1) ntype_index = 1
  if(ntype_index > 3) ntype_index = 3
  

! Initialize
  if(ntype_index == 1) then
     em_vector(1) = 0.4_r_kind
     em_vector(2) = 0.6_r_kind
  else if(ntype_index == 2) then
     em_vector(1) = 0.7_r_kind
     em_vector(2) = 0.8_r_kind
  else if(ntype_index == 3) then
     em_vector(1) = 0.75_r_kind
     em_vector(2) = 0.8_r_kind
  end if

! Data status check
  data_invalid = .False.
  if ( (ts <= 140.0_r_kind) .or. (ts >= 330.0_r_kind) ) data_invalid = .True.
  do ich = 1, nv
     if ( (tv(ich) .le. 50.0_r_kind) .or. (tv(ich) .ge. 330.0_r_kind) )  then
        data_invalid = .True.
        exit
     end if
  end do
  do ich = 1, nh
     if ( (th(ich) <= 50.0_r_kind) .or. (th(ich) >= 330.0_r_kind) )  then
        data_invalid = .True.
        exit
     end if
  end do
  if (data_invalid) RETURN
  

!*** Get intial emissivity for each frequency

! v components
  do ich=1,nv
     ev(ich) =  coe_v(ntype_index,ich,1) + coe_v(ntype_index,ich,2)*tv(1)  &
          + coe_v(ntype_index,ich,3)*tv(2) + coe_v(ntype_index,ich,4)*tv(3)  &
          + coe_v(ntype_index,ich,5)*tv(4)
  end do
  
! h components
  do ich=1,nh
     eh(ich) =  coe_h(ntype_index,ich,1)
     do lp =2,4
        eh(ich) =  eh(ich) + coe_h(ntype_index,ich,lp)*th(lp-1)
     end do
  end do


! *** Emissivity bias value

  if (ntype_index == 1) then         ! ocean
     pe= 0.001_r_kind + 3.885776e-003_r_kind*(tv(1) - th(1)) +  &
          3.727114e-005_r_kind*(tv(3) - th(2)) -  &
          1.141903e-004_r_kind*(tv(4) - th(3))
  else if (ntype_index == 2) then       ! seaice
     pe= 0.011_r_kind + 3.786080e-003_r_kind*(tv(1) - th(1)) -  &
          7.217788e-005_r_kind*(tv(3) - th(2)) +  &
          1.018791e-004_r_kind*(tv(4) - th(3))
  else            ! snow
     pe=     -0.002_r_kind + 4.470142e-003_r_kind*(tv(1) - th(1)) -  &
          1.991876e-004_r_kind*(tv(3) - th(2)) -  &
          1.704354e-005_r_kind*(tv(4) - th(3))
  end if
  
  ev_cor = one - pe*(ts-tv(1))/(tv(1)-th(1))
  if (ev_cor >  one)         ev_cor = one
  if (ev_cor <= 0.2_r_kind) ev_cor = 0.2_r_kind
  eh_cor = ev_cor - pe
  ev_cor = ev(1) - ev_cor
  eh_cor = eh(1) - eh_cor

!*** Calculate emissivity
  do ich=1, nv
     ev(ich) = ev(ich) - ev_cor
     if(ich <= 3) eh(ich) = eh(ich) - eh_cor
  end do


!*** Quality control at 22.235 GHz
  ev_22 = ev(1) + (ev(3)-ev(1))*(22.235_r_kind-19.35_r_kind)/(37.0_r_kind-19.35_r_kind)
!/\ type
  if( (ev(2) .gt. ev(1)) .and. (ev(2) .gt. ev(3)) ) ev(2) = ev_22
!\/ type
  if( (ev(2) .lt. ev(1)) .and. (ev(2) .lt. ev(3)) ) ev(2) = ev_22


!*** Interpolate emissivity at a certain frequency

! v-component
  nch = 4
  do ich=1,nv
     if(frequency <= freq_v(ich)) then
        nch = ich
        exit
     end if
  end do
  
  if (nch == 1) then
     em_vector(2) = ev(1)
  else
     if (nch == 4) then
        em_vector(2) = ev(4)
     else
        em_vector(2) = ev(nch-1) + (ev(nch) - ev(nch-1))* &
             (frequency - freq_v(nch-1))/(freq_v(nch) - freq_v(nch-1))
     end if
  end if
  
! h-component
  nch = 3
  do ich=1,nh
     if(frequency <= freq_h(ich)) then
        nch = ich
        exit
     end if
  end do
  if (nch == 1) then
     em_vector(1) = eh(1)
  else
     if (nch == 3) then
        em_vector(1) = eh(3)
     else
        em_vector(1) = eh(nch-1) + (eh(nch) - eh(nch-1))* &
             (frequency - freq_h(nch-1))/(freq_h(nch) - freq_h(nch-1))
     end if
     
  end if
  
end subroutine ossmem
