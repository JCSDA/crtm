! f90 -o test4 test4.o -L$HOMES/lib -lpran -lpfft
!============================================================================
program test4
!============================================================================
! Check that the hermitian fast Fourier synthesis routine (hfft) and the
! corresponding slow Fourier evaluation routine (hsfe) give results that
! are mutually consistent by performing the evaluations in hsfe at the
! points of the standard transform grid. The data are randomly generated.
!============================================================================
use pfft
use pran
implicit none
integer,parameter      :: n=128,nh=n/2,nm=n-1,nhm=nh-1
real(8),dimension(0:nm):: a,b,c,dadx,dcdx
real(8)                :: da,pi,pi2,x,xran,ang,r,q
integer                :: i,im,ip,is
pi=4*atan(1.d0)
da=pi/nh

print '(" input seed for random sequence:")'; read(*,*)is
call plant(is)
a=0
b=0
do i=0,nm
   call gauss(xran)
   a(i)=xran
enddo
b=a

dadx(0)=0
dadx(nh)=0
do i=1,nhm
   r=a(i); q=a(n-i)
   dadx(i)=-q*i
   dadx(n-i)=r*i
enddo
call hfft(n,a)

call hfft(n,dadx)

do i=0,nm
   x=da*i
   call hsfe(n,b,x,c(i),dcdx(i))
enddo
do i=0,nm
   write(6,64)i,a(i),c(i),dadx(i),dcdx(i)
enddo

64 format(1x,i4,4(1x,e12.6))
end program test4







