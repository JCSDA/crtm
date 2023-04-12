!=============================================================================
program test5
!=============================================================================
! Compare fast and slow transform results. Also verify that the Fourier
! single point evaluation routine gives results consistent with the
! Fourier inverse transform routines and conforms to the Cauchy-Riemann
! equations.
!=============================================================================:
use pran
use pfft
implicit none
real,parameter       :: d=1.e-3,d2=d*2
integer,parameter    :: n=32,m=7
real,dimension(0:n-1):: xa,ya,xb,yb,xc,yc,xd,yd
real                 :: xran,pi2,pi2on,x,y,x1,x2,x3,x4,y1,y2,y3,y4, &
                        f,g,f1,f2,f3,f4,g1,g2,g3,g4,dfdx,dfdy,dgdx,dgdy,&
                        df,dg,t1,t2
integer              :: i,ic
!=============================================================================
pi2=8*atan(1.)
pi2on=pi2/n
call promptseed
xa=0
ya=0
do i=0,m
   ic=mod(n-i,n)
   call gauss(xran)
   xa(i)=xran
   call gauss(xran)
   ya(i)=xran
   call gauss(xran)
   xa(ic)=xran
   call gauss(xran)
   ya(ic)=xran
enddo
xb=xa
xc=xa
yb=ya
yc=ya
call dfft(n,xa,ya)
call dsft(n,xb,yb)
do i=0,n-1
   x=pi2on*i
   y=0
   call dsfe(n,xc,yc,x,y,xd(i),yd(i),df,dg)
enddo

do i=0,n-1
   write(6,64)i,xa(i),xb(i),xd(i),ya(i),yb(i),yd(i)
enddo

! Check Cauchy Riemann relations:
call gauss(xran)
x=xran
call gauss(xran)
y=xran*.2
x1=x+d; x2=x; x3=x-d; x4=x
y1=y; y2=y+d; y3=y; y4=y-d
call dsfe(n,xc,yc,x,y,f,g,df,dg)
call dsfe(n,xc,yc,x1,y1,f1,g1,t1,t2)
call dsfe(n,xc,yc,x2,y2,f2,g2,t1,t2)
call dsfe(n,xc,yc,x3,y3,f3,g3,t1,t2)
call dsfe(n,xc,yc,x4,y4,f4,g4,t1,t2)
dfdx=(f1-f3)/d2
dfdy=(f2-f4)/d2
dgdx=(g1-g3)/d2
dgdy=(g2-g4)/d2
print'('' dfdx,dgdy,dfdy,dgdx'')'
write(6,64)0,dfdx,dgdy,dfdy,dgdx
write(6,64)1,df,dg

64 format(1x,i4,6(1x,e11.5))
end program test5

