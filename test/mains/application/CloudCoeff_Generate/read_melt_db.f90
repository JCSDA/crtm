
!!$program testqtable
!!$  implicit none
!!$  real(8) :: x,qext,qsca,qback,asym,aeff,wave,tmp
!!$
!!$  aeff = 10
!!$  wave = 8421
!!$
!!$  call qtable(aeff,wave,qext,qsca,qback,asym,tmp,tmp)  
!!$  print '(A4,5G12.4)', 'x1:',wave,qext,qsca,qback,asym
!!$
!!$  wave = 22370
!!$  call qtable(aeff,wave,qext,qsca,qback,asym)  
!!$  print '(A4,5G12.4)', 'x2:',wave,qext,qsca,qback,asym
!!$
!!$end program testqtable


subroutine readmeltdb(aeff,wave,qext,qscat,qback,qxbck,asym,melt_fraction,numaeff,shape_number)
  !** fortran 90 program to read qtable files produced by ddscat
  !** designed to behave similar to bhmie
  !** output: qext,qscat,qback,asym (extinction,scattering,backscatter effic. 
  !** and asymmetry parameter) 
  !** new version:
  !** call once in Dmiesub, rather than calling each iteration
  !** pass entire dataset into Dmiesub
  !** qtable input file will be decided by wavel and nshp
  
  implicit none
  integer :: FID1,i,nsca
  integer, intent(out) :: numaeff
  real(8) :: qabs,asym2,PI,frac,wavemin, aeffmin,tmp
  real(8), dimension(15000,2), intent(out) :: qext,qscat,asym,qback,qxbck
  real(8), dimension(15000), intent(out) :: aeff, wave, melt_fraction, shape_number
  logical :: extrap, interp
  integer :: fnk, shpn
  REAL(8) :: aeff1, wave1, beta, theta, phi, qext1, qabs1, qsca1, qasy1, qbck1, qext2, qabs2, qsca2, &
       qasy2, qbck2, meltfrac
  real(8) :: S11, S12, S21, S22, S1X, S2X, S3X, S4X, qbcktmp

  PI = 4.0d0*ATAN(1.0d0)

  aeff(:) = -99.0d0
  wave(:) = -99.0d0
  qext = -99.0d0
  qscat = -99.0d0
  asym = -99.0d0
  qback = -99.0d0
  qxbck = -99.0d0
  melt_fraction(:) = -99.0d0
  shape_number(:) = -99

  FID1 = 11
  open(unit=FID1,file='shape_frequency.in',status='old')  !** hard code db filename, for now

!!$  !** read through the header (16 lines)
!!$
!!$  do i=1,15
!!$     read(FID1,*)
!!$  end do

  i = 0
  do

!    printf("%5d %5d %10.4E %10.4E %4d %4d %4d %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E %10.4E\n",$fnk, $shpn, $aeff, $wavel, $beta, $theta, $phi, $qext1,$qabs1, $qsca1, $qasy1, $qbck1, $qext2, $qabs2, $qsca2, $qasy2, $qbck2, $meltfrac);
     READ(FID1,*,END=888)  fnk, shpn, aeff1, wave1, beta, theta, phi, qext1, qabs1, qsca1, qasy1, qbck1, qext2, qabs2, &
          qsca2, qasy2, qbck2, meltfrac, S11, S12, S21, S22
!** in the general sense, 1 line for each aeff, each melt fraction, each rotation.   
     if (fnk == 999) then
        !** just read the means, for now
        i = i + 1
        aeff(i) = aeff1
        wave(i) = wave1
        qext(i,1) = qext1
        qscat(i,1) = qsca1
        asym(i,1)  = qasy1
        qback(i,1) = qbck1
        qext(i,2) = qext2
        qscat(i,2) = qsca2
        asym(i,2)  = qasy2
        qback(i,2) = qbck2
        melt_fraction(i) = meltfrac
        shape_number(i) = fnk 


        !*************************************************************************************
        !**  XPOL BLOCK 
        !** compute the cross-polarized backscattering.  
        !** first step is to determine the coefficient relationship between qback1 S2X  |S_2|^2 
        
        S1X = S11-S12-S21-S22
        S2X = S11+S12+S21+S22
        S3X = S11-S12+S21-S22
        S4X = S11+S12-S21-S22
        
        tmp = qbck1/S2X
        
        qxbck(i,1) = tmp*S4X

        tmp = qbck2/S2X
        qxbck(i,2) = tmp*S4X
        

!!$        if (fnk < 998) then 
!!$           i = i + 1
!!$           aeff(i) = aeff1
!!$           wave(i) = wave1
!!$           qext(i) = qext2  !** 1 is "VPOL"  2 is "Hpol"
!!$           qscat(i) = qsca2
!!$           asym(i)  = qasy2
!!$           qback(i) = qbck2
!!$           melt_fraction(i) = meltfrac
!!$           shape_number(i) = -1*fnk
!!$        end if
     end if

     !read(FID1,*,END=888) aeff(i),wave(i),qext(i),qabs,qscat(i),asym(i),asym2,qback(i),nsca,melt_fraction(i),shape_number(i)
  end do
888 continue

!  print *, 'testAEFF in read_melt_db:', i,minval(aeff(1:i))
!  stop 'breakpoint in read_melt_db'
  if (i > 30000) then 
     print *, 'adjust array size in readmeltdb to ', i
     stop
  end if

  close(FID1)
  numaeff = i



end subroutine readmeltdb

  
