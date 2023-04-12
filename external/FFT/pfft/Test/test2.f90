! f90 -o test2 test2.o -L$HOMES/lib -lpfft
!============================================================================
program test2
!============================================================================
! Test and demonstrate the standard idioms for Fourier differentiation and
! integration of real data assumed to have a period of 2pi
!============================================================================
use pfft
implicit none
integer,parameter   :: n=2**10,nm=n-1,nh=n/2,nhm=nh-1, ntest=4
real,dimension(0:nm):: a,b,dbdx
real                :: pi2,dx,x,r,q
integer             :: i,ip,im,itest
character(len=18)   :: f64='i10,3x,5(e12.6,1x)'
!============================================================================
pi2=8*atan(1.)
dx=pi2/n

do itest=1,ntest
   do i=0,nm
      x=dx*i
      select case(itest)
      case(1)
         a(i)=cos(x)
      case(2)
         a(i)=sin(x)
      case(3)
         a(i)=cos(2*x)
      case(4)
         a(i)=sin(2*x)
      end select
   enddo

   b=a
   call rfft(n,a)
   do i=1,nhm
      r=a(i)
      q=a(n-i)
      a(i)=-q*i
      a(n-i)=r*i
   enddo
   a(0)=0
   a(nh)=0
   call hfft(n,a)
   do i=0,n
      im=mod(i+nm,n)
      ip=mod(i+1,n)
      dbdx(i)=(b(ip)-b(im))/(2*dx)
   enddo
   do i=0,n,64
      write(*,*)i,a(i),dbdx(i)
   enddo
   read(*,*)
   call rfft(n,b)
   call rfft(n,dbdx)
   print '(" first and last few components of fft of b, dbdx:")'
   write(*,*)0,b(0),0.,dbdx(0)
   do i=1,2
      write(*,*)i,b(i),b(n-i),dbdx(i),dbdx(n-i)
   enddo
   read(*,*)
enddo

print '(" Test integration")'
do itest=1,ntest
   do i=0,nm
      x=i*dx
      select case(itest)
      case(1)
         a(i)=cos(x)
      case(2)
         a(i)=sin(x)
      case(3)
         a(i)=cos(2*x)
      case(4)
         a(i)=sin(2*x)
      end select
   enddo
   dbdx=a
   call rfft(n,a)
   do i=1,nhm
      r=a(i)
      q=a(n-i)
      a(i)=q/i
      a(n-i)=-r/i
   enddo
   a(0)=0
   a(nh)=0
   call hfft(n,a)
   r=a(0)
   a=a-r
   
   b(0)=0
   do i=1,nm
      b(i)=b(i-1)+dx*(dbdx(i-1)+dbdx(i))/2
   enddo
   do i=0,n,64
      write(*,f64)i,a(i),b(i)
   enddo
   read(*,*)
enddo

end program test2

