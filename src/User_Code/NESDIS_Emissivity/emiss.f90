subroutine emiss(knchpf,kprof,kchan,kochan,knchan,indexn,wsp10,zasat,zlsat,&
     isflg,lndsea,emissav,pems5,ts5,soiltype5,soilt5,soilm5, &
     vegtype5,vegf5,snow5,itype,nsdata,nchan,kidsat,   &
     btm,amsua,amsub,ssmi )
!                .      .    .                                       .
! subprogram:    emiss       compute emissivity for IR and microwave 
!   prgmmr: treadon          org: np23                date: 1998-02-20
!
! abstract: compute surface emissivity for IR and microwave channels.
!
! program history log:
!   1998-02-20  treadon - gather all emissivity calculations from 
!                         setuprad and move into this subroutine
!   2004-07-23  weng,yan,okamoto - incorporate MW land and snow/ice emissivity 
!                         models for AMSU-A/B and SSM/I
!   2004-08-02  treadon - add only to module use, add intent in/out
!
! usage: call emiss(knchpf,kprof,kchan,kochan,wsp10,zasat,
!                   isflg,lndsea,emissav,nsdata)
!
!   input argument list:
!     knchpf   - total number of profiles (obs) times number of 
!                channels for each profile.
!     kprof    - profile number array
!     kchan    - channel number array
!     kochan   - old channel number array
!     knchan   - number of channels for each profile
!     indexn   - index pointing from channel,profile to location in array
!     wsp10    - 10 meter wind speed at obs location
!     zasat    - local satellite zenith angle in radians
!     zlsat    - satellite look angle in radians
!     isflg    - snow_ice (=1) /no snow_ice (=0) flag at obs
!                location
!     lndsea   - land (=1) /sea (=0) flag at obs location
!     ts5      - skin temperature
!     soiltype5- soil type
!     soilt5   - soil temperature
!     soilm5   - soil moisture   
!     vegtype5 - vegetation type 
!     vegf5    - vegetation fraction
!     snow5    - snow depth            
!     itype    - ir/microwave instrument type    
!     nsdata   - number of profiles              
!     nchan    - maximum number of channels for this satellite/instrument
!     kidsat   - satellite id
!     btm      - observation tb
!     amsua    - logical true if amsua is processed 
!     amsub    - logical true if amsub is processed 
!     ssmi     - logical true if ssmi  is processed 
!
!   output argument list:
!     emissav  - same as pems5 but for diagnostic array
!     pems5    - surface emissivity at obs location
!
!     NOTE:  pems5 is passed through include file "prfvark3.h"
!   other important variables
!     polar: channel polarization (0=vertical, 1=horizontal, or
!                                  0 to 1=mix of V and H)
!
!  
! ...............................................................
!             land snow ice sea
! landem()     o   o    x   x  : for all MW but f<80GHz,lower accuracy over snow
! snwem_amsu() x   o    x   x  : for AMSU-A/B
! iceem_amsu() x   x    o   x  : for AMSU-A/B
! emiss_ssmi() x   o    o   x  : for SSM/I
!
! if(snow)
!   if(amsua .or. amsub) call snwem_amsu()
!   else if(ssmi)        call emiss_ssmi()
!   else                 call landem()
! if(land)               call landem()
! if(ice)
!   if(amsua .or. amsub) call iceem_amsu()
!   else if(ssmi)        call emiss_ssmi()
!   else                 emissivity=0.92
! if(ocean) calculate
! ..............................................................
!
! attributes:
!   language: f90
!   machine:  IBM sp
!
!$$$
  use kinds, only: r_kind,r_single
  use error_handler, only: failure
  use irsse_model, only: forward_irsse
  use radinfo, only: polar,jppf,newchn_ir
  use spectral_coefficients, only: sc
  use constants, only: zero,one,rad2deg,two
  implicit none

! Declare passed variables.
  integer,intent(in):: knchpf,nsdata,nchan,kidsat,itype
  integer,dimension(nchan*nsdata),intent(in):: kprof,kchan,kochan
  integer,dimension(nchan,nsdata),intent(in):: indexn
  integer,dimension(nsdata),intent(in):: isflg,lndsea,knchan

  real(r_kind),dimension(nsdata),intent(in):: ts5,snow5,soiltype5,&
       soilt5,soilm5,vegtype5,vegf5
  real(r_kind),dimension(nsdata),intent(in):: wsp10,zasat,zlsat
  real(r_single),dimension(nchan,nsdata),intent(out):: emissav
  real(r_kind),dimension(nsdata*nchan),intent(out):: pems5

! Declare local variables
  integer kcho,n,kch,nn,nnp,i
  integer error_status
  integer quiet
  integer,dimension(nchan)::indx

  real(r_kind) zch4,xcorr2v,evertr,ehorzr,xcorr2h,ffoam,zcv2,zcv3
  real(r_kind) xcorr1,zcv1,zcv4,zch1,zch2,zcv5,zcv6,tau2,degre
  real(r_kind) wind,ehorz,evert,pcl2,sec,sec2,freqghz2
  real(r_kind) u10mps2,usec,tccub,tau1,tc,tcsq,term2,freqghz,psl2
  real(r_kind) term1,u10mps,ps2,pc2,pcc,pss,rvertsi,rverts,rvertsr
  real(r_kind) perm_real,perm_imag,rhorzsr,zch5,zch6,zch3,rhorzsi
  real(r_kind) rhorzs,perm_imag2,einf,fen,del2,del1,fen2,perm_real2
  real(r_kind) perm_imag1,perm_real1,den1,den2
  real(r_kind),dimension(1):: emissir

! -------- ice/snow-MW em  ---------
  integer     :: surf_type 
  real(r_kind),dimension(nchan,jppf):: btm
  real(r_kind):: tbasnw(4),tbbsnw(2),tbbmi(nchan)
  logical      :: amsuab 
  logical      :: amsua,amsub,ssmi
  real(r_kind):: ice_depth
  
  
! Explanation for emc :
! FreqGHz: Observation frequency in GHz
! Angdeg: local zenith angle
! Ci: cosine of local zenith angle
! CiCi: cosine squared of local zenith angle
! SiSi: sine squared of local zenith angle
! emc(38): Emissivity model data
! Permittivity model data (Lamkaouchi model)
!   [1-3]: Temperature polynomial coefficients for Tau1 - Lamkaouchi (1996)
!   [4-7]: Temperature polynomial coefficients for Tau2 - Lamkaouchi (1996)
!  [8-11]: Temperature polynomial coefficients for Del1 - Lamkaouchi (1996)
! [12-15]: Temperature polynomial coefficients for Del2 - Lamkaouchi (1996)
! [16-17]: Temperature polynomial coefficients for static permittivity - Lamkaouchi (1996)
! [18-19]: Temperature polynomial coefficients for infinite freq. permittivity - Lamkaouchi (1996)
! Pi is stored for good measure
!    [20]: Stored value of Pi  - temporary, use RTTOV pi when available.
! Large scale correction model version 1: This does *NOT* correct for
! hemispherical scattering and is *NO LONGER USED*
!    [21]: Angle coefficient for large scale correction - see English (1997)
!    [22]: Windspeed coefficient for large scale correction - see English (1997)
!    [23]: Constant for large scale correction - see English (1997)
!    [24]: Reference frequency for large scale correction - see English (1997)
!    [25]: Normalisation frequency for large scale correction - see English (1997)
!    [26]: Scaling factor for large scale correction - see English (1997)
! Bragg scattering correction coefficients
!    [27]: Scaling factor for small scale correction - see English (1997)
! Foam model coefficients for Monahan model
!    [28]: First coefficient in Monahan foam model (neutral stability)  - see English (1997)
!    [29]: Second coefficient in Monahan foam model (neutral stability) - see English (1997)
! Alternative permittivity model (Liebe)
!    [30]: a1 in Liebe's dielectric model - see Liebe (1989)
!    [31]: b1 in Liebe's dielectric model - see Liebe (1989)
!    [32]: c1 in Liebe's dielectric model - see Liebe (1989)
!    [33]: c2 in Liebe's dielectric model - see Liebe (1989)
!    [34]: d1 in Liebe's dielectric model - see Liebe (1989)
!    [35]: d2 in Liebe's dielectric model - see Liebe (1989)
!    [36]: d3 in Liebe's dielectric model - see Liebe (1989)
!    [37]: e1 in Liebe's dielectric model - see Liebe (1989)
!    [38]: e2 in Liebe's dielectric model - see Liebe (1989)
! Version 2 of large scale correction which *DOES»* take account of
! hemispherical scattering.
! 1.) Mixed polarisation mode (nominal V at nadir)
!    [39]: Term a00 in mixed pol of large scale correction model
!    [40]: Term a01 in mixed pol mode of large scale correction model
!    [41]: Term a02 in mixed pol mode of large scale correction model
!    [42]: Term a10 in mixed pol mode of large scale correction model
!    [43]: Term a11 in mixed pol mode of large scale correction model
!    [44]: Term a12 in mixed pol mode of large scale correction model
!    [45]: Term a20 in mixed pol mode of large scale correction model
!    [46]: Term a21 in mixed pol mode of large scale correction model
!    [47]: Term a22 in mixed pol mode of large scale correction model
!    [48]: Term a30 in mixed pol mode of large scale correction model
!    [49]: Term a31 in mixed pol mode of large scale correction model
!    [50]: Term a32 in mixed pol mode of large scale correction model
!    [51]: Term a40 in mixed pol mode of large scale correction model
!    [52]: Term a41 in mixed pol mode of large scale correction model
!    [53]: Term a42 in mixed pol mode of large scale correction model
! 2.) Vertical polarisation mode
!    [54]: Term a00 in vertical pol mode of large scale correction model
!    [55]: Term a01 in vertical pol mode of large scale correction model
!    [56]: Term a02 in vertical pol mode of large scale correction model
!    [57]: Term a10 in vertical pol mode of large scale correction model
!    [58]: Term a11 in vertical pol mode of large scale correction model
!    [59]: Term a12 in vertical pol mode of large scale correction model
!    [60]: Term a20 in vertical pol mode of large scale correction model
!    [61]: Term a21 in vertical pol mode of large scale correction model
!    [62]: Term a22 in vertical pol mode of large scale correction model
!    [63]: Term a30 in vertical pol mode of large scale correction model
!    [64]: Term a31 in vertical pol mode of large scale correction model
!    [65]: Term a32 in vertical pol mode of large scale correction model
!    [66]: Term a40 in vertical pol mode of large scale correction model
!    [67]: Term a41 in vertical pol mode of large scale correction model
!    [68]: Term a42 in vertical pol mode of large scale correction model
! 3. ) Horizontal polarisation mode
!    [69]: Term a00 in horizontal pol mode of large scale correction model
!    [70]: Term a01 in horizontal pol mode of large scale correction model
!    [71]: Term a02 in horizontal pol mode of large scale correction model
!    [72]: Term a10 in horizontal pol mode of large scale correction model
!    [73]: Term a11 in horizontal pol mode of large scale correction model
!    [74]: Term a12 in horizontal pol mode of large scale correction model
!    [75]: Term a20 in horizontal pol mode of large scale correction model
!    [76]: Term a21 in horizontal pol mode of large scale correction model
!    [77]: Term a22 in horizontal pol mode of large scale correction model
!    [78]: Term a30 in horizontal pol mode of large scale correction model
!    [79]: Term a31 in horizontal pol mode of large scale correction model
!    [80]: Term a32 in horizontal pol mode of large scale correction model
!    [81]: Term a40 in horizontal pol mode of large scale correction model
!    [82]: Term a41 in horizontal pol mode of large scale correction model
!    [83]: Term a42 in horizontal pol mode of large scale correction model
!    [84]: Windspeed coefficient in mixed polarisation high U, theta correction
!    [85]: View angle coefficient in mixed polarisation high U, theta correction
!    [86]: Constant coefficient in mixed polarisation high U, theta correction
!    [87]: Windspeed coefficient in vertical polarisation high U, theta correction
!    [88]: View angle coefficient in vertical polarisation high U, theta correction
!    [89]: Constant coefficient in vertical polarisation high U, theta correction
!    [90]: Windspeed coefficient in horizontal polarisation high U, theta correction
!    [91]: View angle coefficient in horizontal polarisation high U, theta correction
!    [92]: Constant coefficient in horizontal polarisation high U, theta correction

  real(r_kind),parameter,dimension(59):: emc = (/&
       0.175350E+02_r_kind, -.617670E+00_r_kind,  .894800E-02_r_kind,  .318420E+01_r_kind,&
       0.191890E-01_r_kind, -.108730E-01_r_kind,  .258180E-03_r_kind,  .683960E+02_r_kind,&
       -.406430E+00_r_kind,  .228320E-01_r_kind, -.530610E-03_r_kind,  .476290E+01_r_kind,&
       0.154100E+00_r_kind, -.337170E-01_r_kind,  .844280E-03_r_kind,  .782870E+02_r_kind,&
       -.434630E-02_r_kind,  .531250E+01_r_kind, -.114770E-01_r_kind,  .314159E+01_r_kind,&
       -.100000E+01_r_kind,  .195000E-04_r_kind,  .255000E+01_r_kind, -.637182E+01_r_kind,&
       0.253918E-01_r_kind,  .357569E-04_r_kind,  .942928E+01_r_kind, -.332839E-01_r_kind,&
       -.647724E-04_r_kind, -.329282E+01_r_kind,  .965450E-02_r_kind,  .281588E-04_r_kind,&
       0.252676E+00_r_kind,  .343867E-02_r_kind, -.156362E-04_r_kind, -.156669E-03_r_kind,&
       0.139485E-04_r_kind, -.407633E-07_r_kind, -.141316E+00_r_kind, -.356556E-02_r_kind,&
       0.142869E-04_r_kind, -.240701E+01_r_kind, -.563888E-01_r_kind,  .325227E-03_r_kind,&
       0.296005E+01_r_kind,  .704675E-01_r_kind, -.426440E-03_r_kind, -.751252E+00_r_kind,&
       -.191934E-01_r_kind,  .125937E-03_r_kind, -.288253E+00_r_kind, -.102655E-02_r_kind,&
       0.226701E-05_r_kind, -.119072E-02_r_kind, -.263165E-04_r_kind,  .114597E-06_r_kind,&
       0.406300E+00_r_kind,  .200031E-02_r_kind, -.781635E-05_r_kind/)
  
! local variables
!
  complex(r_kind) perm1,perm2,rvth,rhth,xperm
!
!  Start emiss here
!
!

!  ////////////  IR emissivity //////////////////
  if(itype /=0 .and. itype /=1) then
     write(6,*)'EMISS:  ILLEGAL surface emissivity type',itype
     call stop2(44)
  end if
     
! Compute or set the emissivity for IR channels.
  if(itype == 0)then
     do n = 1,nsdata
        nn   = indexn(1,n)
        nnp  = nn+knchan(n)-1
        
        if (lndsea(n)==0 .and. isflg(n)==0) then

!      Use Paul vanDelst's IR model over ice free ocean points.
!      NOTES:  
!         1) Paul's model interface requires indx and emissir
!            (see function call below) to be arrays.  
!         2) IR model has upper bound of 15 m/s for wind speeds
!         3) IR model has upper bound of 65 degrees for zenith angle
!         4) indx is a channel index array which contains the location
!            in the IR model channel list of the given satellite and 
!            channel.  Function newchn_ir returns this value
!
           wind    = wsp10(n)               ! wind speed in m/s
           degre   = zasat(n)*rad2deg       ! zenith angle in degrees
           do i = 1, knchan(n)
              kch  = kchan(indexn(i,n))
              indx(i) = newchn_ir(kidsat,kch) 
           end do
           
           error_status = forward_irsse(degre,wind,indx(1:knchan(n)), &
                pems5(nn:nnp),quiet=1)
           
           if (error_status == FAILURE) then
              write(6,*)'STOP FORWARD_IRSSE error'
              call stop2(45)
           endif
           
!       Use fixed IR emissivity values over land, snow, and ice.
        else
           if(lndsea(n)==1)then
              if(isflg(n)==1)then
                 !                land/snow
                 pems5(nn:nnp) = one
              else
!                land/no snow
                 pems5(nn:nnp) = 0.97_r_kind
              end if
           else
              if(isflg(n)==1)then
!                ocean/sea ice
                 pems5(nn:nnp) = 0.98_r_kind
              end if
           end if
        end if
        do i=1,knchan(n)
           emissav(i,n) = pems5(nn+i-1)
        end do
     end do
     

!  ////////////  MW emissivity //////////////////
  else

! Set emissivity for microwave channels 
!
     surf_type=0
     amsuab = .false.
     if(amsua.or.amsub) amsuab = .true. 
     
     do nn = 1,knchpf
        n    = kprof(nn)
        kch  = kchan(nn)
        kcho = kochan(nn)
        
        pems5(nn) = -one  !default value kozo

!    if(ssmi.or.amsua) write(6,*) 'obstype,isat,kch,kcho,n,btm=',obstype,isat,kch,kcho,n, btm(kcho,n)
        if(amsua) then
           tbasnw(1:3) = btm(1:3,n)
           tbasnw(4)   = btm(15,n)
           tbbsnw(1:2) = -999.9_r_kind
        else if(amsub) then
           tbasnw(1:4) = -999.9_r_kind
           tbbsnw(1:2) = btm(1:2,n)
        else
           tbasnw(1:4) = -999.9_r_kind
           tbbsnw(1:2) = -999.9_r_kind
        end if
        
        if(ssmi) tbbmi(1:knchan(n)) = btm(1:knchan(n),n)
        

        if(lndsea(n)==1)then
!          land points

!      ----- snow MW -------
           if(isflg(n)==1.and.snow5(n)>0.1_r_kind)then
!             land/snow points 

              if(amsuab) then 
                 call snwem_amsu(zasat(n),sc%frequency(kch), &
                      snow5(n),ts5(n),tbasnw,tbbsnw,ehorz,evert )
                 
!                  call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
                 pems5(nn) = evert !because esv=esh
              else if(ssmi) then 
                 surf_type=3
                 call emiss_ssmi(   &
                      surf_type,zasat(n),sc%frequency(kch), &
                      soilm5(n),vegf5(n),vegtype5(n),soiltype5(n), &
                      soilt5(n),ts5(n),snow5(n),tbbmi,ehorz,evert )
                 pems5(nn) = (one-polar(kch))*evert + polar(kch)*ehorz
              else
                 if(sc%frequency(kch)<80._r_kind)then  !snow & f<80GHz
!                  currently landem only works for frequencies < 80 Ghz
                    call landem(zasat(n),sc%frequency(kch),  &
                         soilm5(n),vegf5(n),vegtype5(n),soiltype5(n),soilt5(n), &
                         ts5(n),snow5(n),ehorz,evert)
                    call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
                 else  !snow & f>=80GHz
                    pems5(nn) = 0.90_r_kind
                 end if
              end if  !snow & amsuab/ssmi/othermw
              
!      ----- land (snow-free) MW -------
           else
              if(sc%frequency(kch)<80._r_kind)then  !land & f<80GHz
!                currently landem only works for frequencies < 80 Ghz
                 call landem(zasat(n),sc%frequency(kch),  &
                      soilm5(n),vegf5(n),vegtype5(n),soiltype5(n),soilt5(n), &
                      ts5(n),snow5(n),ehorz,evert  )
                 call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
              else  !land & f>=80GHz
                 pems5(nn) = 0.95_r_kind
              end if  !land & if f><80
           end if     !lndsea=1 & if land or snow
           

        else   !          sea or sea ice points 

!      ----- sea ice MW  -------
           if(isflg(n)==1)then
!          ocean/sea ice
              if(amsuab) then 
                 ice_depth=zero
                 call iceem_amsu(  &
                      zasat(n),sc%frequency(kch),ice_depth,ts5(n),&
                      tbasnw,tbbsnw,ehorz,evert )
!                   call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
                 pems5(nn) = evert  !because esv=esh
              else if(ssmi) then 
                 surf_type=2
                 call emiss_ssmi(   &
                      surf_type,zasat(n),sc%frequency(kch), &
                      soilm5(n),vegf5(n),vegtype5(n),soiltype5(n), &
                      soilt5(n),ts5(n),snow5(n),tbbmi,ehorz,evert )
                 pems5(nn) = (one-polar(kch))*evert + polar(kch)*ehorz
              else
                 pems5(nn) = 0.92_r_kind

              end if
!      ----- sea (ice-free) MW  -------
           else
           
!             Open ocean points
!
!             First set constants.  Then perform the calculation.
              freqghz = sc%frequency(kch)
              u10mps = wsp10(n)
              pcc=cos(zasat(n))
              pss=sin(abs(zasat(n)))
!             pcl2=cos(zlsat(n))**2
!             psl2=sin(zlsat(n))**2
              ps2=pss*pss
              pc2=pcc*pcc
              freqghz2=freqghz*freqghz
              u10mps2=u10mps*u10mps
              sec=one/pcc
              sec2=sec*sec
              usec=u10mps*sec
             
!             calculate piom (ellison et al.) xperm
!             to calculate xperm of saline water based on piom model.
!             convert from kelvin to centigrate and define quadratic and
!             cubic functions for later polynomials
              tc=ts5(n)-273.15_r_kind
              tcsq=tc*tc
              tccub=tcsq*tc
              
!             define two relaxation frequencies, tau1 and tau2
              tau1=emc(1)+emc(2)*tc+emc(3)*tcsq
              tau2=emc(4)+emc(5)*tc+emc(6)*tcsq+emc(7)*tccub
              
!             static xperm estatic=del1+del2+einf
              del1=emc(8)+emc(9)*tc+emc(10)*tcsq+emc(11)*tccub
              del2=emc(12)+emc(13)*tc+emc(14)*tcsq+emc(15)*tccub
              einf=emc(18)+emc(19)*tc
              
!             calculate xperm using double-debye formula
              fen=two*emc(20)*freqghz*0.001_r_kind
              fen2=fen**two
              den1=one+fen2*tau1*tau1
              den2=one+fen2*tau2*tau2
              perm_real1=del1/den1
              perm_real2=del2/den2
              perm_imag1=del1*fen*tau1/den1
              perm_imag2=del2*fen*tau2/den2
              perm_real=perm_real1+perm_real2+einf
              perm_imag=perm_imag1+perm_imag2
              xperm=dcmplx(perm_real,perm_imag)

!             calculate complex fresnel reflection coefficients
!             to calculate vertical and horizontal polarised reflectivities
!             given xperm at local incidencence angle for all channels
!             and profiles
              perm1 = cdsqrt(xperm - dcmplx(ps2,zero))
              perm2  = xperm*pcc
              rhth = (pcc - perm1)/(pcc + perm1)                     
              rvth = (perm2 - perm1)/(perm2 + perm1)
              rvertsr=dreal(rvth)
              rvertsi=dimag(rvth)
              rverts=rvertsr*rvertsr+rvertsi*rvertsi
              rhorzsr=dreal(rhth)
              rhorzsi=dimag(rhth)
              rhorzs=rhorzsr*rhorzsr+rhorzsi*rhorzsi

!             calculate small scale xcorr to reflection coefficients
!             the following lines are commented out because a warning will 
!             be printed from dcalmkaouchi if freqghz<10.
              xcorr1=exp(emc(21)*u10mps*pc2/freqghz2)

!             calculate large scale geometric correction
!             to calculate a correction to the fresnel reflection coefficients
!             allowing for the presence of large scale roughness      
!
!             jc: six coefficients (constant, u, u^2, sec, sec^2, u*sec)	
              zcv1=emc(24)+emc(25)*freqghz+emc(26)*freqghz2
              zcv2=(emc(27)+emc(28)*freqghz+emc(29)*freqghz2)*sec
              zcv3=(emc(30)+emc(31)*freqghz+emc(32)*freqghz2)*sec2
              zcv4=(emc(33)+emc(34)*freqghz+emc(35)*freqghz2)*u10mps
              zcv5=(emc(36)+emc(37)*freqghz+emc(38)*freqghz2)*u10mps2
              zcv6=(emc(39)+emc(40)*freqghz+emc(41)*freqghz2)*usec
              zch1=emc(42)+emc(43)*freqghz+emc(44)*freqghz2
              zch2=(emc(45)+emc(46)*freqghz+emc(47)*freqghz2)*sec
              zch3=(emc(48)+emc(49)*freqghz+emc(50)*freqghz2)*sec2
              zch4=(emc(51)+emc(52)*freqghz+emc(53)*freqghz2)*u10mps
              zch5=(emc(54)+emc(55)*freqghz+emc(56)*freqghz2)*u10mps2
              zch6=(emc(57)+emc(58)*freqghz+emc(59)*freqghz2)*usec

!             calculate correction for this polarisation
              xcorr2v=.01_r_kind*(zcv1+zcv2+zcv3+zcv4+zcv5+zcv6)
              xcorr2h=.01_r_kind*(zch1+zch2+zch3+zch4+zch5+zch6)
              
!             calculate foam emissivity correction
              ffoam=emc(22)*(u10mps**emc(23))
              evertr=one-rverts*xcorr1+xcorr2v
              ehorzr=one-rhorzs*xcorr1+xcorr2h
              evert=evertr - ffoam*evertr+ ffoam
              ehorz=ehorzr - ffoam*ehorzr + ffoam

!             Combine horizontal and vertical polarizations.
              call ehv2pem( ehorz,evert,zlsat(n),polar(kch), pems5(nn) )
           end if  !isflg=1/others
        end if     !lndsea=1/others
        

!        Load emissivity into array for radiative transfer model
!       (pems5) and diagnostic output file (emissav).
        
        pems5(nn)       = max(zero,min(pems5(nn),one))
        emissav(kcho,n) = pems5(nn)

     end do
  end if   !IR or MW
  

! End of routine.
  return

  
contains
  
  subroutine ehv2pem( ehorz0,evert0,zlsat0,polar0, pems0)
    implicit none
    real(r_kind):: ehorz0,evert0,zlsat0,polar0, pems0
    real(r_kind):: pcl2,psl2,term1,term2
    pcl2=cos(zlsat0)**2
    psl2=sin(zlsat0)**2
    term1 = evert0*pcl2 + ehorz0*psl2
    term2 = evert0*psl2 + ehorz0*pcl2
    pems0 = (one-polar0)*term1 + polar0*term2
    return
  end subroutine ehv2pem
  
end subroutine emiss
