program test3
use pfft
implicit none
integer,parameter:: n=15,nm=n-1,nh=n/2
real(8),dimension(0:nm):: a,b,c,d,at,bt,ct,dt
integer             :: i,j
print'('' input i,j'')'; read(*,*)i,j
a=0.
b=0.
c=0
a(i)=1.
b(j)=1.
c(mod(i+j,n))=1.
call fftcnv(n,a,b,d)
do i=0,nm
   write(6,64)i,a(i),b(i),c(i),d(i)
enddo
64 format(1x,i3,4(1x,e12.6))
end program test3

!=============================================================================
subroutine fftcnv(n,a,b,d)
!=============================================================================
use pfft
implicit none
integer,intent(IN   )               :: n
real(8),dimension(0:n-1),intent(IN ):: a,b
real(8),dimension(0:n-1),intent(OUT):: d
integer                             :: i,im,nh
real(8),dimension(0:n-1)            :: at,bt
!=============================================================================
nh=n/2
at=a
bt=b
call rfft(n,at)
call rfft(n,bt)
do i=0,nh,n-nh
   d(i)=at(i)*bt(i)
enddo
do i=1,n-nh-1
   im=n-i
   d(i) =at(i)*bt(i) -at(im)*bt(im)
   d(im)=at(i)*bt(im)+at(im)*bt(i)
enddo
call hfft(n,d)
end subroutine fftcnv

