subroutine  iceem_amsu(theta,frequency,depth,ts,tba,tbb,esv,esh)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram: iceem_amsua  noaa/nesdis emissivity model over ice for  AMSU-A/B
!
!   prgmmr: Banghua Yan      org: nesdis              date: 2004-03-01
!           Fuzhong Weng
!
! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
!       ice for AMSU-A/B
!
!    reference:
!    Yan, B., F. Weng and K.Okamoto,2004:
!       "A microwave snow emissivity model, submitted to TGRS
!
!   version: beta (sea ice type is to be determined)
!
! program history log:
!   2004-01-01  yan,b   - implement the algorithm for the ice emissivity
!   2004-03-01  yan,b   - modify the code for SSI
!   2004-07-23  okamoto - modify the code for GSI
!
! input argument list:
!      theta            -  local zenith angle in radian
!      frequency        -  frequency in GHz
!      ts               -  surface temperature (K)    (GDAS)
!      depth            -  scatter medium depth (mm)  (not used here) (GDAS) !
!      tba[1] ~ tba[4]  -  brightness temperature at four AMSU-A window channels
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89 GHz
!      tbb[1] ~ tbb[2]  -  brightness temperature at two AMSU-B window channels:
!                              tbb[1] : 89 GHz
!                              tbb[2] : 150 GHz
!                          When tba[ ] or tbb[ ] = -999.9, it means a missing value (no available data)
!
! output argument list:
!   em_vector        -  esv, esh
!       esv       : emissivity at vertical polarization
!       esh       : emissivity at horizontal polarization
!       sea ice_type (to be determined)
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
  use constants, only: zero, one
  implicit none
  
  integer      :: nch,nwcha,nwchb,nwch,nalg
  Parameter(nwcha = 4, nwchb = 2, nwch = 5,nalg = 7)
  real(r_kind)    :: theta,frequency,depth,ts
  real(r_kind)    :: em_vector(2),esv,esh
  real(r_kind)    :: tb(nwch),tba(nwcha),tbb(nwchb)
  logical :: INDATA(nalg),AMSUAB,AMSUA,AMSUB,ABTs,ATs,BTs,MODL
  integer :: seaice_type,input_type,i,ich,np,k
  
  Equivalence(INDATA(1), ABTs)
  Equivalence(INDATA(2), ATs)
  Equivalence(INDATA(3), AMSUAB)
  Equivalence(INDATA(4), AMSUA)
  Equivalence(INDATA(5), BTs)
  Equivalence(INDATA(6), AMSUB)
  Equivalence(INDATA(7), MODL)

!  Initialization

  em_vector(1) = 0.85_r_kind
  em_vector(2) = 0.82_r_kind
  seaice_type  = -999
  input_type = -999
  do k = 1, nalg
     INDATA(k) = .TRUE.
  end do
  
! Read AMSU & Ts data and set available option
! Get five AMSU-A/B window measurements
  tb(1) = tba(1); tb(2) = tba(2);  tb(3) = tba(3)
  tb(4) = tba(4); tb(5) = tbb(2)

! Check available data
  if((ts <= 100.0_r_kind) .or. (ts >= 320.0_r_kind) ) then
     ABTs = .false.;   ATs = .false.;   BTs = .false.;  MODL = .false.
  end if
  do i=1,nwcha
     if((tba(i) <= 100.0_r_kind) .or. (tba(i) >= 320.0_r_kind) ) then
        ABTs = .false.;  ATs = .false.;   AMSUAB = .false.;  AMSUA = .false.
        exit
     end if
  end do
  do i=1,nwchb
     if((tbb(i) <= 100.0_r_kind) .or. (tbb(i) >= 320.0_r_kind) ) then
        ABTs = .false.;  AMSUAB = .false.;  BTs  = .false.;  AMSUB  = .false.
        exit
     end if
  end do
  if((depth <  zero) .or. (depth >= 3000.0_r_kind)) MODL = .false.
  if((frequency >= 80.0_r_kind) .and. (BTs)) then
     ATs = .false.;   AMSUAB = .false.
  end if
  
! Check input type and call a specific Option/subroutine
  DO np = 1, nalg
     if (INDATA(np)) then
        input_type = np
        exit
     end if
  ENDDO
  
  GET_option: SELECT CASE (input_type)
  CASE (1)
!        call siem_ABTs(theta,frequency,tb,ts,seaice_type,em_vector)
  CASE (2)
     call siem_ATs(theta,frequency,tba,ts,seaice_type,em_vector)
  CASE (3)
!        call siem_AB(theta,frequency,tb,seaice_type,em_vector)
  CASE (4)
!        call siem_amsua(theta,frequency,tba,seaice_type,em_vector)
  CASE(5)
     call siem_BTs(theta,frequency,tbb,ts,seaice_type,em_vector)
  CASE(6)
!        call siem_amsub(theta,frequency,tbb,seaice_type,em_vector)
  CASE(7)
!        call siem_default(theta,frequency,depth,ts,seaice_type,em_vector)
  END SELECT GET_option
  
  if (em_vector(1) > one)         em_vector(1) = one
  if (em_vector(2) > one)         em_vector(2) = one
  if (em_vector(1) < 0.6_r_kind) em_vector(1) = 0.6_r_kind
  if (em_vector(2) < 0.6_r_kind) em_vector(2) = 0.6_r_kind
  esv = em_vector(1)
  esh = em_vector(2)
  
end subroutine iceem_amsu


subroutine siem_ATs(theta,frequency,tba,ts,seaice_type,em_vector)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr: Banghua Yan                 org: nesdis              date: 2004-03-01
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery AMSUA & Ts
!
! program history log:
!   2004-10-28  treadon - correct out of bound problems for array coe
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle in radian
!      ts               -  surface temperature
!      tba[1] ~ tba[4]  -  brightness temperature at five AMSU-A window channels:
!                              tba[1] : 23.8 GHz
!                              tba[2] : 31.4 GHz
!                              tba[3] : 50.3 GHz
!                              tba[4] : 89   GHz
!
! output argument list:
!
!   em_vector[1] and [2]  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!       seaice_type        -     to be determined
!
! important internal variables:
!
!   coe23   - fitting coefficients to estimate discriminator at 23.8 GHz
!   coe31   - fitting coefficients to estimate discriminator at 31.4 GHz
!   coe50   - fitting coefficients to estimate discriminator at 50.3 GHz
!   coe89   - fitting coefficients to estimate discriminator at 89   GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind
  implicit none
  
  integer,parameter:: nch =10,nwch = 5,ncoe = 4
  real(r_kind)    :: tba(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer :: seaice_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)

! Fitting Coefficients Using Tb1, Tb2, Tb4 and Ts
  data coe23/ 9.815214e-001_r_kind,  3.783815e-003_r_kind,  &
       6.391155e-004_r_kind, -9.106375e-005_r_kind, -4.263206e-003_r_kind/
  data coe31/ 9.047181e-001_r_kind, -2.782826e-004_r_kind,  &
       4.664207e-003_r_kind, -3.121744e-005_r_kind, -3.976189e-003_r_kind/
  data coe50/ 1.163853e+000_r_kind, -1.419205e-003_r_kind,  &
       5.505238e-003_r_kind,  1.506867e-003_r_kind, -6.157735e-003_r_kind/
  data coe89/  1.020753e+000_r_kind, -8.666064e-004_r_kind,  &
       9.624331e-004_r_kind,  4.878773e-003_r_kind, -5.055044e-003_r_kind/
  data coe150/ 1.438246e+000_r_kind,  5.667756e-004_r_kind, &
       -2.621972e-003_r_kind,  5.928146e-003_r_kind, -5.856687e-003_r_kind/
  save coe23,coe31,coe50,coe89,coe150
  

! Calculate emissivity discriminators at five AMSU window channels
  
  do ich = 1, nwch
     discriminator(ich) = coe(1+(ich-1)*10)
     discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2)*tba(1)  &
          + coe((ich-1)*10 + 3)*tba(2)  &
          + coe((ich-1)*10 + 4)*tba(4)  &
          + coe( (ich-1)*10 + 5 )*ts
  end do
  
  call siem_interpolate(frequency,discriminator,emissivity,seaice_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
  
end subroutine siem_ATs



subroutine siem_BTs(theta,frequency,tbb,ts,seaice_type,em_vector)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2004-03-01
!
! abstract:
!         Calculate the emissivity discriminators and interpolate/extrapolate
!  emissivity at required frequency with respect to secenery BTs
!
! program history log:
!
! input argument list:
!
!      frequency        -  frequency in GHz
!      theta            -  local zenith angle (not used here)
!      ts               -  surface temperature in degree
!      tbb[1] ~ tbb[2]  -  brightness temperature at five AMSU-B window channels:
!                              tbb[1] : 89  GHz
!                              tbb[2] : 150 GHz
!
! output argument list:
!
!   em_vector(1) and (2)  -  emissivity at two polarizations.
!                              set esv = esh here and will be updated
!       seaice_type        -  ?
!
! important internal variables:
!
!   coe31   - fitting coefficients to estimate discriminator at 31.4 GHz
!   coe89   - fitting coefficients to estimate discriminator at 89   GHz
!   coe150  - fitting coefficients to estimate discriminator at 150  GHz
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind
  implicit none
  
  integer,parameter:: nch =10,nwch = 5,ncoe = 6
  real(r_kind)    :: tbb(*),theta
  real(r_kind)    :: em_vector(*),emissivity,ts,frequency,discriminator(nwch)
  integer :: seaice_type,i,k,ich,nvalid_ch
  real(r_kind)  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe-3),coe150(0:ncoe-3)
  real(r_kind)  :: coe(nch*(ncoe+1))
  
  Equivalence (coe(1),coe23)
  Equivalence (coe(11),coe31)
  Equivalence (coe(21),coe50)
  Equivalence (coe(31),coe89)
  Equivalence (coe(41),coe150)

! Fitting Coefficients at 31.4 GHz
  data coe23/ 2.239429e+000_r_kind, -2.153967e-002_r_kind,  &
       5.785736e-005_r_kind,  1.366728e-002_r_kind,    &
       -3.749251e-005_r_kind, -5.128486e-002_r_kind, -2.184161e-003_r_kind/
  data coe31/ 1.768085e+000_r_kind, -1.643430e-002_r_kind,  &
       4.850989e-005_r_kind,  1.288753e-002_r_kind,   &
       -3.628051e-005_r_kind, -4.751277e-002_r_kind, -2.580649e-003_r_kind/
  data coe50/ 8.910227e-001_r_kind,  6.170706e-003_r_kind, &
       -3.772921e-006_r_kind, -4.146567e-004_r_kind,   &
       -2.208121e-006_r_kind, -3.163193e-002_r_kind, -3.863217e-003_r_kind/
  save coe23,coe31,coe50,coe89,coe150

! Calculate emissivity discriminators at five AMSU window channels
  do ich = 1, nwch-2
     discriminator(ich) = coe(1+(ich-1)*10)
     nvalid_ch = 2
     do i=1,nvalid_ch
        discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*tbb(i) + &
             coe((ich-1)*10 + 2*i+1)*tbb(i)*tbb(i)
     end do
     discriminator(ich) = discriminator(ich) +           &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 )*cos(theta)  +   &
          coe( (ich-1)*10 + (nvalid_ch+1)*2 + 1 )*ts
  end do
  discriminator(4) = 9.278287e-001_r_kind +  5.549908e-003_r_kind*tbb(1) &
       - 5.728596e-004_r_kind*tbb(2) -  4.701641e-003_r_kind*ts
  discriminator(5) = 1.520531e+000_r_kind + 1.119648e-003_r_kind*tbb(1) &
       +  4.518667e-003_r_kind*tbb(2) - 7.744607e-003_r_kind*ts
  
  call siem_interpolate(frequency,discriminator,emissivity,seaice_type)
  
  em_vector(1) = emissivity
  em_vector(2) = emissivity
  
end subroutine siem_BTs


subroutine  siem_interpolate(frequency,discriminator,emissivity,seaice_type)
!
!$$$  subprogram documentation block
!                .      .    .                                       .
! subprogram:
!
!   prgmmr:Banghua Yan                  org: nesdis              date: 2004-03-01
!
! abstract:
!        (1) Find one snow emissivity spectrum to mimic the emission property of the
! realistic snow condition using a set of discrminators
!        (2) Interpolate/extrapolate emissivity at a required frequency
!
! program history log:
!
! input argument list:
!
!      frequency       - frequency in GHz
!      discriminators  - emissivity discriminators at five AMSU-A & B window channels
!            discriminator[1]   :  emissivity discriminator at 23.8 GHz
!            discriminator[2]   :  emissivity discriminator at 31.4 GHz
!            discriminator[3]   :  emissivity discriminator at 50.3 GHz
!            discriminator[4]   :  emissivity discriminator at 89   GHz
!            discriminator[5]   :  emissivity discriminator at 150  GHz
!
!       Note: discriminator(1) and discriminator(3) are missing value in
!            'AMSU-B & Ts','AMUS-B' and 'MODL' options., which are defined to as -999.9,
! output argument list:
!
!   em_vector[1] and [2]  -  emissivity at two polarizations.
!       seaice_type             -  snow type (reference [2])
!
! remarks:
!
! attributes:
!   language: f90
!   machine:  ibm rs/6000 sp
!
!$$$

  use kinds, only: r_kind
  implicit none
  
  integer,parameter:: ncand = 16,nch =5
  integer:: ich,ichmin,ichmax,i,j,k,s,seaice_type
  real(r_kind)   :: dem,demmin0
  real(r_kind)   :: em(ncand,nch)
  real(r_kind)   :: frequency,freq(nch),emissivity,discriminator(*)
  real(r_kind)   :: cor_factor,adjust_check,kratio, bconst
  data  freq/23.8_r_kind, 31.4_r_kind, 50.3_r_kind,89.0_r_kind, 150.0_r_kind/
  
! Estimate sea ice emissivity at a required frequency
  seaice_type = -999   ! temporal assumption
  do i = 2, nch
     if(frequency < freq(1))   exit
     if(frequency >= freq(nch)) exit
     if(frequency < freq(i)) then
        emissivity = discriminator(i-1) + (discriminator(i)-discriminator(i-1))* &
             (frequency - freq(i-1))/(freq(i) - freq(i-1))
        exit
     end if
     
  end do
  
  
  if(frequency < freq(1))    emissivity = discriminator(1)
  
! Assume emissivity = constant at frequencies >= 150 GHz
  if (frequency >= freq(nch)) emissivity = discriminator(nch)
  
end subroutine siem_interpolate

