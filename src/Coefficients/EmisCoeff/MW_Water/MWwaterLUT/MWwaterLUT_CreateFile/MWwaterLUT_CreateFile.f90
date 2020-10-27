!
  PROGRAM Regression

    USE Type_Kinds
    USE Message_Handler
    USE MWwaterLUT_Define

! ------------------------------------------------------------------------------------
!    This code generates the azimuth coefficients for v polarization.
! ------------------------------------------------------------------------------------

  IMPLICIT NONE

  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &


  INTEGER :: N_point_used, N_variables_used,i,j,k,N1,i1,N2

!  Data
  REAL(fp), PARAMETER :: ZERO = 0.0_fp, ONE = 1.0_fp

  INTEGER, PARAMETER :: N_outgoing_Azi = 1
  INTEGER, PARAMETER :: N_outgoing_Zenith = 15
  INTEGER, PARAMETER :: N_SST = 12
  INTEGER, PARAMETER :: N_Wind_S = 16
  INTEGER, PARAMETER :: N_Frequency = 18

  REAL(fp), DIMENSION(:), Allocatable :: Frequency_array,Wind_Speed_array
  REAL(fp), DIMENSION(:), Allocatable :: Outgoing_Azimuth,SST_temperature
  REAL(fp), DIMENSION(:), Allocatable :: Outgoing_Zenith, Outgoing_Zenith_W

  REAL(fp),DIMENSION(:,:,:,:,:), Allocatable :: eh2,ev2,stoke3,stoke4
  REAL(fp),DIMENSION(:,:,:), Allocatable :: fv,fh
  REAL(fp),DIMENSION(:,:,:,:), Allocatable :: aeh2,aev2,aeh3,aev3



  INTEGER, PARAMETER :: N_Stoke = 4
  REAL(fp), DIMENSION( N_Stoke ) :: Iri,Irc,large_scale,Fresnel_Emi,Emissivity,Reflectivity

  INTEGER, PARAMETER :: N_predictor = 50, N_sample = 500000
  REAL(fp), DIMENSION(N_sample,4) :: Stokes
  REAL(fp), DIMENSION(N_sample) :: Stocwind,Stocfre,stoczen,stocazi

  REAL(fp), DIMENSION(N_sample) :: eh_diff, ev_diff, s3_diff, s4_diff
  REAL(fp), DIMENSION(N_predictor+1,N_sample) :: Predictor
  REAL(fp), DIMENSION(N_predictor+1) :: Reg_Coef
  REAL(fp), DIMENSION(4*(N_predictor+1) ) :: t_Coef


  REAL(fp), PARAMETER :: Transmittance = 1.0
  REAL(fp) :: Wind_Direction,azimuth,f,f2,PI,seczen,deg_to_rad,seczen_sq,wind10,wind10_sq
  INTEGER, PARAMETER :: Fastem_Version = 2
  ! Outgoing zenith angle in degree, Outgoing_Zenith_W is the integration weights.

  INTEGER :: l, iZ, iAz, iS, nline,kk,NNazi,ii

  REAL(fp) :: std, rms, bias, salinity, yyy

  REAL(fp) :: std2, rms2, bias2, yyy2

  REAL(fp) :: small_corr,phi1,phi2,phi3

  INTEGER :: Isst,Iwind,Izenith,Ifreq,Iazith

  CHARACTER(256) :: input_filename
  INTEGER :: err_stat
  TYPE(MWwaterLUT_type) :: MWwaterLUT


  ! read data
     PI=acos(-1.0_fp)
     deg_to_rad = PI/180.0_fp
    N1 = 0

  !  open data file, which contains two-scale modeled emissivity, emissivity for flat surface
  !  foam is NOT include and treated separately.
  !  Please also note that azimuth angle-dependent is treated independently. Therefore, emissivity
  ! in this file is independent from azimuth angles.
  !
  input_filename = 'two_July_26_2011.dat'
  OPEN(16, file=input_filename, status = 'old')

  Allocate( Frequency_array(N_Frequency) )
  Allocate( SST_temperature(N_SST) )
  Allocate( Wind_Speed_array(N_Wind_S) )
  Allocate( Outgoing_Zenith(N_outgoing_Zenith) )
  Allocate( Outgoing_Azimuth(N_outgoing_azi) )

  Allocate( fv(N_Frequency,N_SST,N_outgoing_Zenith) )
  Allocate( fh(N_Frequency,N_SST,N_outgoing_Zenith) )

  Allocate( aeh2(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith) )
  Allocate( aev2(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith) )

  Allocate( aeh3(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith) )
  Allocate( aev3(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith) )

  Allocate( eh2(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith,N_outgoing_azi) )
  Allocate( ev2(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith,N_outgoing_azi) )
  Allocate( stoke3(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith,N_outgoing_azi) )
  Allocate( stoke4(N_Frequency,N_SST,N_Wind_S,N_outgoing_Zenith,N_outgoing_azi) )


  ! -----
  ! Create the MWwaterLUT structure
  CALL MWwaterLUT_Create( MWwaterLUT, &
                          N_outgoing_Zenith, & ! n_Angles
                          N_Frequency      , & ! n_Frequencies
                          N_SST            , & ! n_Temperatures
                          N_Wind_S           ) ! n_Wind_Speeds
  IF ( .NOT. MWwaterLUT_Associated( MWwaterLUT ) ) THEN
    PRINT *, 'Error creating MWwaterLUT structure'
    STOP
  END IF
  ! -----


  DO 101 Iwind = 1, N_Wind_S
  DO 101 Isst = 1, N_SST
  DO 101 Ifreq = 1, N_Frequency
  DO 101 Izenith = 1, N_outgoing_Zenith
  DO 101 Iazith = 1, N_outgoing_azi
     N1 = N1 + 1
    READ(16,*) Outgoing_Zenith(Izenith),Outgoing_Azimuth(Iazith), &
      Frequency_array(Ifreq),Wind_Speed_array(Iwind),SST_temperature(Isst), &
      aeh2(Ifreq,Isst,Iwind,Izenith), &
      aev2(Ifreq,Isst,Iwind,Izenith), &
      stoke3(Ifreq,Isst,Iwind,Izenith,Iazith), &
      stoke4(Ifreq,Isst,Iwind,Izenith,Iazith), &
      Fh(Ifreq,Isst,Izenith), &
      Fv(Ifreq,Isst,Izenith)
      IF( Izenith == 1 ) THEN  ! nadir, eh = ev at nadir
        aeh2(Ifreq,Isst,Iwind,Izenith) = (aeh2(Ifreq,Isst,Iwind,Izenith)+ &
          aev2(Ifreq,Isst,Iwind,Izenith))/2.0_fp
        aev2(Ifreq,Isst,Iwind,Izenith) = aeh2(Ifreq,Isst,Iwind,Izenith)
      END IF
 101 CONTINUE

  close(16)

  OPEN(36,file='MW_Water_Large_Scale_LUT.dat')
  write(36,'(4I10)') N_Wind_S,N_SST,N_Frequency,N_outgoing_Zenith
  write(36,'(6f13.5)') Wind_Speed_array
  write(36,'(6f13.5)') SST_temperature
  write(36,'(6f13.5)') Frequency_array
  write(36,'(6f13.5)') Outgoing_Zenith

  OPEN(26,file='MW_Water_Large_Scale_LUT.bin',form='unformatted')
  write(26) N_Wind_S,N_SST,N_Frequency,N_outgoing_Zenith
  write(26) Wind_Speed_array
  write(26) SST_temperature
  write(26) Frequency_array
  write(26) Outgoing_Zenith


  ! -----
  ! Populate the MWwaterLUT dimension arrays
  MWwaterLUT%Angle       = Outgoing_Zenith
  MWwaterLUT%Frequency   = Frequency_array
  MWwaterLUT%Temperature = SST_temperature
  MWwaterLUT%Wind_Speed  = Wind_Speed_array
  ! -----


       N1 = 0

  print *,'max ',maxval(aeh2),minval(aeh2)
  print *,'max ',maxval(fh),minval(fh)
    nline = 0

    DO 222 k = 1, N_Frequency
    DO 222 j = 1, N_Wind_S
      DO 222 iS = 1, N_SST

      DO 222 iZ = 1, N_outgoing_Zenith
!
         call small_scale_correction2( Wind_Speed_array(j),  & ! surface wind at 10 m (m/s)
                            Frequency_array(k),  & ! microwave frequency (GHz)
                                    small_corr)
         small_corr = exp(-small_corr*cos(Outgoing_Zenith(iZ)*deg_to_rad)**2 )
         N1 = N1 + 1
! substract flat and small surface part (Fresnel + small correction),
!   eh_diff/ev_diff is large-scale part
         eh_diff(N1) =aeh2(k,is,j,iZ) -(1.0-(1.0-fh(k,is,iZ))*small_corr)
         ev_diff(N1) =aev2(k,is,j,iZ) -(1.0-(1.0-fv(k,is,iZ))*small_corr)

         aeh3(k,is,j,iZ) =aeh2(k,is,j,iZ) -(1.0-(1.0-fh(k,is,iZ))*small_corr)
         aev3(k,is,j,iZ) =aev2(k,is,j,iZ) -(1.0-(1.0-fv(k,is,iZ))*small_corr)
!
         Predictor(1,N1) = 1.0
         f = Frequency_array(k)
         f2 = f * f
         wind10 = Wind_Speed_array(j)
         wind10_sq = wind10 * wind10
         seczen = 1.0_fp/cos(Outgoing_Zenith(iZ)*deg_to_rad)
         seczen_sq = seczen * seczen

         Predictor(1,N1) = 1.0
         Predictor(2,N1) = f
         Predictor(3,N1) = f2
         Predictor(4,N1) = seczen
         Predictor(5,N1) = seczen*f
         Predictor(6,N1) = seczen*f2

         Predictor(7,N1) = seczen_sq
         Predictor(8,N1) = seczen_sq*f
         Predictor(9,N1) = seczen_sq*f2

         Predictor(10,N1) = wind10
         Predictor(11,N1) = wind10*f
         Predictor(12,N1) = wind10*f2
         Predictor(13,N1) = wind10_sq
         Predictor(14,N1) = wind10_sq*f
         Predictor(15,N1) = wind10_sq*f2
         Predictor(16,N1) = wind10*seczen
         Predictor(17,N1) = wind10*seczen*f
         Predictor(18,N1) = wind10*seczen*f2


  ! -----
  ! Populate the MWwaterLUT data arrays
  MWwaterLUT%ev(iZ,k,iS,j) = aev2(k,is,j,iZ)
  MWwaterLUT%eh(iZ,k,iS,j) = aeh2(k,is,j,iZ)
  ! -----


 222  CONTINUE
     close(11)

 ! output original, instead of large-scale part LUT
 write(36,'(8f9.5)') ((((aev2(k,is,j,iZ),j=1,N_Wind_S),iS=1,N_SST),k=1,N_Frequency),iZ=1,N_outgoing_Zenith)
 write(36,'(8f9.5)') ((((aeh2(k,is,j,iZ),j=1,N_Wind_S),iS=1,N_SST),k=1,N_Frequency),iZ=1,N_outgoing_Zenith)

 write(26) ((((aev2(k,is,j,iZ),j=1,N_Wind_S),iS=1,N_SST),k=1,N_Frequency),iZ=1,N_outgoing_Zenith)
 write(26) ((((aeh2(k,is,j,iZ),j=1,N_Wind_S),iS=1,N_SST),k=1,N_Frequency),iZ=1,N_outgoing_Zenith)


  !-----
  ! Write the LUT to file
  err_stat = MWwaterLUT_WriteFile( MWwaterLUT, &
                                   'MWwaterLUT.bin', &
                                   Title   = 'Emissivity look-up table data for FASTEM5', &
                                   History = PROGRAM_VERSION_ID, &
                                   Comment = 'Input two-scale model datafile: '//TRIM(input_filename) )
  IF ( err_stat /= SUCCESS ) THEN
    PRINT *, 'Error writing MWwaterLUT.bin'
    STOP
  END IF
  !-----


      N_variables_used = 18
      print *,' N_point_used = ',N1
  ! call fitting code without intercept, horizontally polarized part
      CALL fit_reg(N1, N_predictor+1, Predictor, eh_diff, Reg_Coef, N1, N_variables_used)
      print *,' coefficients for h-polarization '
      Reg_Coef = Reg_Coef
      t_coef(19:36) = Reg_Coef(1:18)
      write(6,'(6E15.7)') (Reg_Coef(k),k=1,N_variables_used)
!
!  test fitting accuracy
     std=0.0
     bias=0.0
     rms=0.0
     DO i = 1, N1
       yyy = 0.0
       DO kk = 1, N_variables_used
         yyy = yyy + Reg_Coef(kk)*Predictor(kk,i)
       END DO
        bias=bias + eh_diff(i) - yyy
        rms = rms + (eh_diff(i) - yyy)**2
     END DO
     bias = bias/N1
     rms = sqrt(rms/N1)
     print *,' h-large fitting ',N1,bias,rms
!
! test accuracy
     std=0.0
     bias=0.0
     rms=0.0
     N1 = 0

     DO 301 k = 1, N_Frequency
     DO 301 j = 1, N_Wind_S
      DO 301 iS = 1, N_SST
      DO 301 iZ = 1, N_outgoing_Zenith
        call small_scale_correction2( Wind_Speed_array(j),  & ! surface wind at 10 m (m/s)
                            Frequency_array(k),  & ! microwave frequency (GHz)
                                    small_corr)
        small_corr = exp(-small_corr*cos(Outgoing_Zenith(iZ)*deg_to_rad)**2 )
        N1 = N1 + 1
         Predictor(1,N1) = 1.0
         f = Frequency_array(k)
         f2 = f * f
         wind10 = Wind_Speed_array(j)
         wind10_sq = wind10 * wind10
         seczen = 1.0_fp/cos(Outgoing_Zenith(iZ)*deg_to_rad)
         seczen_sq = seczen * seczen

         Predictor(1,N1) = 1.0
         Predictor(2,N1) = f
         Predictor(3,N1) = f2
         Predictor(4,N1) = seczen
         Predictor(5,N1) = seczen*f
         Predictor(6,N1) = seczen*f2
         Predictor(7,N1) = seczen_sq
         Predictor(8,N1) = seczen_sq*f
         Predictor(9,N1) = seczen_sq*f2
         Predictor(10,N1) = wind10
         Predictor(11,N1) = wind10*f
         Predictor(12,N1) = wind10*f2
         Predictor(13,N1) = wind10_sq
         Predictor(14,N1) = wind10_sq*f
         Predictor(15,N1) = wind10_sq*f2
         Predictor(16,N1) = wind10*seczen
         Predictor(17,N1) = wind10*seczen*f
         Predictor(18,N1) = wind10*seczen*f2

 301  CONTINUE


      print *,' coefficients for v-polarization '

      CALL fit_reg(N1, N_predictor+1, Predictor, ev_diff, Reg_Coef, N1, N_variables_used)
      t_coef(1:18) = Reg_Coef(1:18)
      write(6,502) (t_Coef(k),k=1,36)

      ! output regression fitting coef for large scale part by using from Steve selected predictors.
      write(36,502) (t_Coef(k),k=1,36)
 501  FORMAT(51x,',',2(ES13.6,'_fp,') )
 502  FORMAT(2x,5(ES13.6,'_fp,'),' &' )

!  check fitting accuracy

     ii = 0
     DO 403 k = 1, N_Frequency
          std=0.0
     bias=0.0
     rms=0.0
     std2=0.0
     bias2=0.0
     rms2=0.0

     N1 = 0
     DO 401 j = 1, N_Wind_S
      DO 401 iS = 1, N_SST
      DO 401 iZ = 1, N_outgoing_Zenith
     ii = ii + 1
      N1 = N1 + 1
       yyy = 0.0
       yyy2 = 0.0
       DO kk = 1, N_variables_used
         yyy = yyy + t_Coef(kk)*Predictor(kk,ii)
         yyy2 = yyy2 + t_Coef(kk+18)*Predictor(kk,ii)
       END DO
        bias=bias + ev_diff(ii) - yyy
        rms = rms + (ev_diff(ii) - yyy)**2
        bias2=bias2 + eh_diff(ii) - yyy2
        rms2 = rms2 + (eh_diff(ii) - yyy2)**2
 401 CONTINUE
     bias = bias/N1
     rms = sqrt(rms/N1)
          bias2 = bias2/N1
     rms2 = sqrt(rms2/N1)
     write(6,'(I10,5f12.5)') k,Frequency_array(k),bias,rms,bias2,rms2

 403 CONTINUE


     CLOSE(26)
     CLOSE(36)
     STOP

  END PROGRAM Regression
!
!
      subroutine fit_reg(NSET,NVAR,f,y,s,mset,mvar)
!------------------------------------------------------------------
      use type_kinds
      implicit real(double)(a-h,o-z)
      implicit integer(i-n)
      integer, parameter :: nd=100
!      implicit double precision(a-h,o-z)
      integer :: p,q

      DIMENSION f(NVAR,NSET),y(NSET),s(NVAR)
      dimension a(nd,nd),g(nd,nd),b(nd),m(nd)

      do 101 l=1,mvar
         b(l)=0.0
         do 102 k=1,mvar
         g(l,k)=0.0
            do 102 i=1,mset
            g(l,k)=g(l,k)+f(k,i)*f(l,i)
 102     continue
       do 101 i=1,mset
               b(l)=b(l)+y(i)*f(l,i)
 101    continue

      do 104 l=1,mvar
           do 103 k=1,mvar
             a(l,k)=g(l,k)
 103        continue
            a(l,mvar+1)=b(l)
 104    continue

      do 2660 i=1,mvar
        p=i
        q=1
        e=a(i,1)
        do 2622 j=i,mvar
          do 2620 k=1,mvar
            if(abs(e).ge.abs(a(j,k))) goto 2620
            e=a(j,k)
            q=k
            p=j
 2620     continue
 2622   continue

        if(abs(e).gt.1.000e-12) goto 2630
        print *,' No unique solution in fit_reg'
        stop

 2630   if(p.eq.i) goto 2642
        do 2640 k=1,mvar+1
          s(k)=a(i,k)
          a(i,k)=a(p,k)
          a(p,k)=s(k)
 2640   continue

 2642   do 2656 j=1,mvar
          if(j.eq.i) goto 2656
          if(abs(a(j,q)).lt.1.0e-10) goto 2656
          r=a(j,q)/a(i,q)
          do 2654 k=1,mvar+1
            a(j,k)=a(j,k)-a(i,k)*r
 2654     continue
 2656   continue

        m(i)=q
 2660 continue

      do 2668 i=1,mvar
        q=m(i)
        s(q)=a(i,mvar+1)/a(i,q)
 2668 continue
         return
       end subroutine fit_reg
!
!
    SUBROUTINE small_scale_correction2( wind10,  & ! surface wind at 10 m (m/s)
                                            f,  & ! microwave frequency (GHz)
                                       corr_f)  ! correction factor
! ----------------------------------------------------------------------------------
!  This subroutine compute small scale correction to Fresnel reflection coefficient.
!  Quanhua (Mark) Liu, JCSDA, Quanhua.Liu@noaa.gov
! -----------------------------------------------------------------------------------
    INTEGER, PARAMETER :: FP = SELECTED_REAL_KIND(15) ! double precision
    REAL(fp), PARAMETER :: min_f = 1.4_fp, max_f = 200.0_fp
    REAL(fp), PARAMETER ::  min_wind = 0.3_fp, max_wind = 35.0_fp
    REAL(fp), INTENT(IN) :: wind10, f
    REAL(fp), INTENT(OUT) :: corr_f
    REAL(fp) :: windspeed, frequency

    frequency = f
    windspeed = wind10

    IF( frequency < min_f ) frequency = min_f
    IF( frequency > max_f ) frequency = max_f
    IF( windspeed < min_wind ) windspeed = min_wind
    IF( windspeed > max_wind ) windspeed = max_wind

    corr_f =-5.0208480E-06_fp *windspeed*frequency +2.3297951E-08_fp *windspeed*frequency**2 &
            +4.6625726E-08_fp *windspeed**2* frequency -1.9765665E-09_fp *windspeed**2* frequency**2 &
            -7.0469823E-04_fp *windspeed**2 /frequency +7.5061193E-04_fp *windspeed**2 /frequency**2 &
            +9.8103876E-04_fp *windspeed + 1.5489504E-04_fp *windspeed**2

    RETURN
    END SUBROUTINE small_scale_correction2
!
