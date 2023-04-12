!=============================================================================
MODULE pfft2
!=============================================================================
use pkind
use pfft
implicit none
private
public :: xcfft,xdfft,ycfft,ydfft,zcfft,zdfft, &
          xrfft,xhfft,yrfft,yhfft,zrfft,zhfft, &
          xyrfft,xyhfft

INTERFACE xcfft; MODULE PROCEDURE sxcfft, dxcfft,sx3cfft,dx3cfft; END INTERFACE
INTERFACE xdfft; MODULE PROCEDURE sxdfft, dxdfft,sx3dfft,dx3dfft; END INTERFACE
INTERFACE ycfft; MODULE PROCEDURE sycfft, dycfft,sy3cfft,dy3cfft; END INTERFACE
INTERFACE ydfft; MODULE PROCEDURE sydfft, dydfft,sy3dfft,dy3dfft; END INTERFACE
INTERFACE zcfft; MODULE PROCEDURE szcfft, dzcfft;                 END INTERFACE
INTERFACE zdfft; MODULE PROCEDURE szdfft, dzdfft;                 END INTERFACE
INTERFACE xrfft; MODULE PROCEDURE sxrfft, dxrfft,sx3rfft,dx3rfft; END INTERFACE
INTERFACE xhfft; MODULE PROCEDURE sxhfft, dxhfft,sx3hfft,dx3hfft; END INTERFACE
INTERFACE yrfft; MODULE PROCEDURE syrfft, dyrfft,sy3rfft,dy3rfft; END INTERFACE
INTERFACE yhfft; MODULE PROCEDURE syhfft, dyhfft,sy3hfft,dy3hfft; END INTERFACE
interface zrfft; module procedure szrfft, dzrfft;                 end interface
interface zhfft; module procedure szhfft, dzhfft;                 end interface
INTERFACE xyrfft;MODULE PROCEDURE sxyrfft,dxyrfft;                END INTERFACE
INTERFACE xyhfft;MODULE PROCEDURE sxyhfft,dxyhfft;                END INTERFACE

CONTAINS

!=============================================================================
SUBROUTINE sxcfft(n,ny,rb,qb) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!                SUBROUTINE  SXCFFT
! X-array version of CFFT (fft along dimension of faster-changing index)
!
! --> n:    period (x-dimension)
! --> ny:   y-dimension of array
! <-> rb:   real part of data (-->) and its transform (<--)
! <-> qb:   imaginary part of data (-->) and its transform (<--)
!============================================================================= 
INTEGER,                     INTENT(IN   ):: n,ny
REAL(SP),DIMENSION(0:n-1,ny),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
          nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(ny)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:); rb(i,:)=rb(j,:); rb(j,:)=t
      t=qb(i,:); qb(i,:)=qb(j,:); qb(j,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
  ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(k3,:)*rf3-qb(k3,:)*qf3    ! q13
         qb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3    ! r13
         rb(k3,:)=rb(k2,:)*rf1-qb(k2,:)*qf1    ! r12
         rb(k2,:)=rb(k2,:)*qf1+qb(k2,:)*rf1    ! q12
         qb(k2,:)=qb(k3,:)    -rb(k2,:)        ! r23
         rb(k2,:)=qb(k3,:)    +rb(k2,:)        ! q22
         qb(k3,:)=rb(k3,:)    +t         ! r22
         t  =rb(k3,:)    -t        ! q23
         rb(k3,:)=rb(k1,:)*rf2-qb(k1,:)*qf2    ! r11
         qb(k1,:)=rb(k1,:)*qf2+qb(k1,:)*rf2    ! q11
         rb(k1,:)=rb(k0,:)    -rb(k3,:)        ! r21
         rb(k0,:)=rb(k0,:)    +rb(k3,:)        ! r20
         rb(k3,:)=rb(k1,:)    -qb(k2,:)        ! r3
         rb(k1,:)=rb(k1,:)    +qb(k2,:)        ! r1
         qb(k2,:)=qb(k0,:)    +qb(k1,:)        ! q20
         qb(k1,:)=qb(k0,:)    -qb(k1,:)        ! q21
         qb(k0,:)=qb(k2,:)    +rb(k2,:)        ! q0
         qb(k2,:)=qb(k2,:)    -rb(k2,:)        ! q2
         rb(k2,:)=rb(k0,:)    -qb(k3,:)        ! r2
         rb(k0,:)=rb(k0,:)    +qb(k3,:)        ! r0
         qb(k3,:)=qb(k1,:)    -t         ! q3
         qb(k1,:)=qb(k1,:)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;      k1=k0+ma
         rf1=w(jmb);  qf1=w(nh+jmb)
         t  =rb(k1,:)*qf1+qb(k1,:)*rf1     ! q11
         qb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1     ! r11
         rb(k1,:)=rb(k0,:)    -qb(k1,:)         ! r1
         rb(k0,:)=rb(k0,:)    +qb(k1,:)         ! r0
         qb(k1,:)=qb(k0,:)    -t         ! q1
         qb(k0,:)=qb(k0,:)    +t         ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_SP;  rec=1.5_SP;  qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t       = rb(k2,:)*qf2+qb(k2,:) *rf2      ! r12
         qb(k2,:)= rb(k2,:)*rf2-qb(k2,:) *qf2      ! q12
         rb(k2,:)= rb(k1,:)*qf1+qb(k1,:) *rf1      ! q11
         rb(k1,:)= rb(k1,:)*rf1-qb(k1,:) *qf1      ! r11
         qb(k1,:)= rb(k2,:)    +t             ! q21
         rb(k2,:)=(rb(k2,:)    -t       )*qep    ! r22
         t  = rb(k1,:)    +qb(k2,:)           ! r21
         rb(k1,:)=(rb(k1,:)    -qb(k2,:))*qep      ! q22
         rb(k0,:)= rb(k0,:)    +t             ! r0
         qb(k0,:)= qb(k0,:)    +qb(k1,:)           ! q0
         t  = rb(k0,:)    -t*rec     ! r21
         qb(k1,:)= qb(k0,:)    -qb(k1,:) *rec      ! q21
         qb(k2,:)= qb(k1,:)    -rb(k1,:)           ! q2
         qb(k1,:)= qb(k1,:)    +rb(k1,:)           ! q1
         rb(k1,:)= t           -rb(k2,:)           ! r1
         rb(k2,:)= t           +rb(k2,:)           ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     = rb(k1,:)*qf1+qb(k1,:)*rf1                  ! q11
            rb(k1,:)= rb(k1,:)*rf1-qb(k1,:)*qf1           ! r11
            qb(k1,:)= rb(k4,:)*rf4-qb(k4,:)*qf4           ! q14
            rb(k4,:)= rb(k4,:)*qf4+qb(k4,:)*rf4           ! r14
            qb(k4,:)= rb(k1,:)    -qb(k1,:)            ! q24
            rb(k1,:)= rb(k1,:)    +qb(k1,:)            ! r21
            qb(k1,:)= t           +rb(k4,:)                  ! q21
            rb(k4,:)= t           -rb(k4,:)           ! r24
            t     = rb(k3,:)*rf3-qb(k3,:)*qf3           ! q13
            rb(k3,:)= rb(k3,:)*qf3+qb(k3,:)*rf3           ! r13
            qb(k3,:)= rb(k2,:)*qf2+qb(k2,:)*rf2           ! q12
            rb(k2,:)= rb(k2,:)*rf2-qb(k2,:)*qf2           ! r12
            qb(k2,:)= qb(k3,:)    +rb(k3,:)            ! q22
            rb(k3,:)= qb(k3,:)    -rb(k3,:)            ! r23
            qb(k3,:)= rb(k2,:)    -t                           ! q23
            rb(k2,:)= rb(k2,:)    +t                    ! r22
            rb(k0,:)= rb(k0,:)    +rb(k1,:)+rb(k2,:)             ! r0
            qb(k0,:)= qb(k0,:)    +qb(k1,:)+qb(k2,:)             ! q0
            t     = rb(k4,:)*qze+rb(k3,:)*qet                  ! r34
            rb(k3,:)= rb(k3,:)*qze-rb(k4,:)*qet           ! r33
            rb(k4,:)= rb(k0,:)    -rb(k2,:)*rzc-rb(k1,:)*rec     ! r32
            rb(k1,:)= rb(k0,:)    -rb(k1,:)*rzc-rb(k2,:)*rec     ! r31
            rb(k2,:)= rb(k4,:)    +rb(k3,:)            ! r2
            rb(k3,:)= rb(k4,:)    -rb(k3,:)            ! r3
            rb(k4,:)= rb(k1,:)    +t             ! r4
            rb(k1,:)= rb(k1,:)    -t             ! r1
            t     = qb(k0,:)    -qb(k1,:)*rzc-qb(k2,:)*rec     ! q31
            qb(k2,:)= qb(k0,:)    -qb(k2,:)*rzc-qb(k1,:)*rec     ! q32
            qb(k1,:)= qb(k3,:)*qze-qb(k4,:)*qet           ! q33
            qb(k4,:)= qb(k4,:)*qze+qb(k3,:)*qet           ! q34
            qb(k3,:)= qb(k2,:)    +qb(k1,:)            ! q3
            qb(k2,:)= qb(k2,:)    -qb(k1,:)            ! q2
            qb(k1,:)= t           +qb(k4,:)    ! q1
            qb(k4,:)= t           -qb(k4,:)    ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sxcfft 

!=============================================================================
SUBROUTINE dxcfft(n,ny,rb,qb)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!                SUBROUTINE  DXCFFT
! Double precision version of xcfft
!============================================================================= 
INTEGER,                     INTENT(IN   ):: n,ny
REAL(DP),DIMENSION(0:n-1,ny),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
REAL(DP),DIMENSION(ny)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:); rb(i,:)=rb(j,:); rb(j,:)=t
      t=qb(i,:); qb(i,:)=qb(j,:); qb(j,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(k3,:)*rf3-qb(k3,:)*qf3            ! q13
         qb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3            ! r13
         rb(k3,:)=rb(k2,:)*rf1-qb(k2,:)*qf1            ! r12
         rb(k2,:)=rb(k2,:)*qf1+qb(k2,:)*rf1            ! q12
         qb(k2,:)=qb(k3,:)    -rb(k2,:)         ! r23
         rb(k2,:)=qb(k3,:)    +rb(k2,:)         ! q22
         qb(k3,:)=rb(k3,:)    +t          ! r22
         t  =rb(k3,:)    -t          ! q23
         rb(k3,:)=rb(k1,:)*rf2-qb(k1,:)*qf2            ! r11
         qb(k1,:)=rb(k1,:)*qf2+qb(k1,:)*rf2            ! q11
         rb(k1,:)=rb(k0,:)    -rb(k3,:)         ! r21
         rb(k0,:)=rb(k0,:)    +rb(k3,:)         ! r20
         rb(k3,:)=rb(k1,:)    -qb(k2,:)         ! r3
         rb(k1,:)=rb(k1,:)    +qb(k2,:)         ! r1
         qb(k2,:)=qb(k0,:)    +qb(k1,:)         ! q20
         qb(k1,:)=qb(k0,:)    -qb(k1,:)         ! q21
         qb(k0,:)=qb(k2,:)    +rb(k2,:)         ! q0
         qb(k2,:)=qb(k2,:)    -rb(k2,:)         ! q2
         rb(k2,:)=rb(k0,:)    -qb(k3,:)         ! r2
         rb(k0,:)=rb(k0,:)    +qb(k3,:)         ! r0
         qb(k3,:)=qb(k1,:)    -t          ! q3
         qb(k1,:)=qb(k1,:)    +t          ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;      k1=k0+ma
         rf1=w(jmb);  qf1=w(nh+jmb)
         t  =rb(k1,:)*qf1+qb(k1,:)*rf1    ! q11
         qb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1    ! r11
         rb(k1,:)=rb(k0,:)    -qb(k1,:)        ! r1
         rb(k0,:)=rb(k0,:)    +qb(k1,:)        ! r0
         qb(k1,:)=qb(k0,:)    -t        ! q1
         qb(k0,:)=qb(k0,:)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_DP;  rec=1.5_DP;  qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t       = rb(k2,:)*qf2+qb(k2,:) *rf2             ! r12
         qb(k2,:)= rb(k2,:)*rf2-qb(k2,:) *qf2             ! q12
         rb(k2,:)= rb(k1,:)*qf1+qb(k1,:) *rf1             ! q11
         rb(k1,:)= rb(k1,:)*rf1-qb(k1,:) *qf1             ! r11
         qb(k1,:)= rb(k2,:)    +t             ! q21
         rb(k2,:)=(rb(k2,:)    -t       )*qep    ! r22
         t  = rb(k1,:)    +qb(k2,:)           ! r21
         rb(k1,:)=(rb(k1,:)    -qb(k2,:))*qep      ! q22
         rb(k0,:)= rb(k0,:)    +t             ! r0
         qb(k0,:)= qb(k0,:)    +qb(k1,:)            ! q0
         t  = rb(k0,:)    -t*rec            ! r21
         qb(k1,:)= qb(k0,:)    -qb(k1,:) *rec           ! q21
         qb(k2,:)= qb(k1,:)    -rb(k1,:)            ! q2
         qb(k1,:)= qb(k1,:)    +rb(k1,:)            ! q1
         rb(k1,:)= t           -rb(k2,:)           ! r1
         rb(k2,:)= t           +rb(k2,:)           ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(k1,:)*qf1+qb(k1,:)*rf1                   ! q11
            rb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1                   ! r11
            qb(k1,:)=rb(k4,:)*rf4-qb(k4,:)*qf4                   ! q14
            rb(k4,:)=rb(k4,:)*qf4+qb(k4,:)*rf4                   ! r14
            qb(k4,:)=rb(k1,:)    -qb(k1,:)            ! q24
            rb(k1,:)=rb(k1,:)    +qb(k1,:)            ! r21
            qb(k1,:)=t           +rb(k4,:)                  ! q21
            rb(k4,:)=t           -rb(k4,:)           ! r24
            t     =rb(k3,:)*rf3-qb(k3,:)*qf3                  ! q13
            rb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3                   ! r13
            qb(k3,:)=rb(k2,:)*qf2+qb(k2,:)*rf2                   ! q12
            rb(k2,:)=rb(k2,:)*rf2-qb(k2,:)*qf2                   ! r12
            qb(k2,:)=qb(k3,:)    +rb(k3,:)            ! q22
            rb(k3,:)=qb(k3,:)    -rb(k3,:)            ! r23
            qb(k3,:)=rb(k2,:)    -t             ! q23
            rb(k2,:)=rb(k2,:)    +t             ! r22
            rb(k0,:)=rb(k0,:)    +rb(k1,:)    +rb(k2,:)          ! r0
            qb(k0,:)=qb(k0,:)    +qb(k1,:)    +qb(k2,:)          ! q0
            t     =rb(k4,:)*qze+rb(k3,:)*qet                  ! r34
            rb(k3,:)=rb(k3,:)*qze-rb(k4,:)*qet                   ! r33
            rb(k4,:)=rb(k0,:)    -rb(k2,:)*rzc-rb(k1,:)*rec      ! r32
            rb(k1,:)=rb(k0,:)    -rb(k1,:)*rzc-rb(k2,:)*rec      ! r31
            rb(k2,:)=rb(k4,:)    +rb(k3,:)            ! r2
            rb(k3,:)=rb(k4,:)    -rb(k3,:)            ! r3
            rb(k4,:)=rb(k1,:)    +t             ! r4
            rb(k1,:)=rb(k1,:)    -t             ! r1
            t     =qb(k0,:)    -qb(k1,:)*rzc-qb(k2,:)*rec      ! q31
            qb(k2,:)=qb(k0,:)    -qb(k2,:)*rzc-qb(k1,:)*rec      ! q32
            qb(k1,:)=qb(k3,:)*qze-qb(k4,:)*qet                   ! q33
            qb(k4,:)=qb(k4,:)*qze+qb(k3,:)*qet                   ! q34
            qb(k3,:)=qb(k2,:)    +qb(k1,:)            ! q3
            qb(k2,:)=qb(k2,:)    -qb(k1,:)            ! q2
            qb(k1,:)=t           +qb(k4,:)    ! q1
            qb(k4,:)=t           -qb(k4,:)    ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dxcfft

!=============================================================================
SUBROUTINE sxdfft(n,ny,rb,qb) 
!=============================================================================
!                SUBROUTINE  SXDFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! X-array version of DFFT (fft along dimension of faster-changing index)
!
! --> n:    period (x-dimension)
! --> ny:   y-dimension of array
! <-> rb:   real part of transform (-->) and the Fourier synthesis (<--)
! <-> qb:   imaginary part of transform (-->) and the Fourier synthesis (<--)
!============================================================================= 
INTEGER,                     INTENT(IN   ):: n,ny
REAL(SP),DIMENSION(0:n-1,ny),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(ny)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._SP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0,:)=rb(0,:)*rfac; rb(1:nm,:)=rb(nm:1:-1,:)*rfac
qb(0,:)=qb(0,:)*rfac; qb(1:nm,:)=qb(nm:1:-1,:)*rfac

! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:); rb(i,:)=rb(j,:); rb(j,:)=t
      t=qb(i,:); qb(i,:)=qb(j,:); qb(j,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1;  mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(k3,:)*rf3-qb(k3,:)*qf3            ! q13
         qb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3            ! r13
         rb(k3,:)=rb(k2,:)*rf1-qb(k2,:)*qf1            ! r12
         rb(k2,:)=rb(k2,:)*qf1+qb(k2,:)*rf1            ! q12
         qb(k2,:)=qb(k3,:)    -rb(k2,:)         ! r23
         rb(k2,:)=qb(k3,:)    +rb(k2,:)         ! q22
         qb(k3,:)=rb(k3,:)    +t          ! r22
         t  =rb(k3,:)    -t          ! q23
         rb(k3,:)=rb(k1,:)*rf2-qb(k1,:)*qf2            ! r11
         qb(k1,:)=rb(k1,:)*qf2+qb(k1,:)*rf2            ! q11
         rb(k1,:)=rb(k0,:)    -rb(k3,:)         ! r21
         rb(k0,:)=rb(k0,:)    +rb(k3,:)         ! r20
         rb(k3,:)=rb(k1,:)    -qb(k2,:)         ! r3
         rb(k1,:)=rb(k1,:)    +qb(k2,:)         ! r1
         qb(k2,:)=qb(k0,:)    +qb(k1,:)         ! q20
         qb(k1,:)=qb(k0,:)    -qb(k1,:)         ! q21
         qb(k0,:)=qb(k2,:)    +rb(k2,:)         ! q0
         qb(k2,:)=qb(k2,:)    -rb(k2,:)         ! q2
         rb(k2,:)=rb(k0,:)    -qb(k3,:)         ! r2
         rb(k0,:)=rb(k0,:)    +qb(k3,:)         ! r0
         qb(k3,:)=qb(k1,:)    -t          ! q3
         qb(k1,:)=qb(k1,:)    +t          ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t       =rb(k1,:)*qf1+qb(k1,:)*rf1    ! q11
         qb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1    ! r11
         rb(k1,:)=rb(k0,:)    -qb(k1,:)        ! r1
         rb(k0,:)=rb(k0,:)    +qb(k1,:)        ! r0
         qb(k1,:)=qb(k0,:)    -t        ! q1
         qb(k0,:)=qb(k0,:)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(k2,:)*qf2+qb(k2,:) *rf2 ! r12
         qb(k2,:)= rb(k2,:)*rf2-qb(k2,:) *qf2   ! q12
         rb(k2,:)= rb(k1,:)*qf1+qb(k1,:) *rf1   ! q11
         rb(k1,:)= rb(k1,:)*rf1-qb(k1,:) *qf1   ! r11
         qb(k1,:)= rb(k2,:)    +t          ! q21
         rb(k2,:)=(rb(k2,:)    -t       )*qep ! r22
         t  = rb(k1,:)    +qb(k2,:) ! r21
         rb(k1,:)=(rb(k1,:)    -qb(k2,:))*qep ! q22
         rb(k0,:)= rb(k0,:)    +t          ! r0
         qb(k0,:)= qb(k0,:)    +qb(k1,:)        ! q0
         t  = rb(k0,:)    -t*rec  ! r21
         qb(k1,:)= qb(k0,:)    -qb(k1,:) *rec   ! q21
         qb(k2,:)= qb(k1,:)    -rb(k1,:)        ! q2
         qb(k1,:)= qb(k1,:)    +rb(k1,:)        ! q1
         rb(k1,:)= t        -rb(k2,:) ! r1
         rb(k2,:)= t        +rb(k2,:) ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze; 
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(k1,:)*qf1+qb(k1,:)*rf1             ! q11
            rb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1              ! r11
            qb(k1,:)=rb(k4,:)*rf4-qb(k4,:)*qf4              ! q14
            rb(k4,:)=rb(k4,:)*qf4+qb(k4,:)*rf4              ! r14
            qb(k4,:)=rb(k1,:)    -qb(k1,:)                  ! q24
            rb(k1,:)=rb(k1,:)    +qb(k1,:)              ! r21
            qb(k1,:)=t          +rb(k4,:)             ! q21
            rb(k4,:)=t          -rb(k4,:)             ! r24
            t     =rb(k3,:)*rf3-qb(k3,:)*qf3             ! q13
            rb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3              ! r13
            qb(k3,:)=rb(k2,:)*qf2+qb(k2,:)*rf2              ! q12
            rb(k2,:)=rb(k2,:)*rf2-qb(k2,:)*qf2              ! r12
            qb(k2,:)=qb(k3,:)    +rb(k3,:)              ! q22
            rb(k3,:)=qb(k3,:)    -rb(k3,:)              ! r23
            qb(k3,:)=rb(k2,:)    -t               ! q23
            rb(k2,:)=rb(k2,:)    +t               ! r22
            rb(k0,:)=rb(k0,:)    +rb(k1,:)    +rb(k2,:)     ! r0
            qb(k0,:)=qb(k0,:)    +qb(k1,:)    +qb(k2,:)     ! q0
            t     =rb(k4,:)*qze+rb(k3,:)*qet       ! r34
            rb(k3,:)=rb(k3,:)*qze-rb(k4,:)*qet       ! r33
            rb(k4,:)=rb(k0,:)    -rb(k2,:)*rzc-rb(k1,:)*rec ! r32
            rb(k1,:)=rb(k0,:)    -rb(k1,:)*rzc-rb(k2,:)*rec ! r31
            rb(k2,:)=rb(k4,:)    +rb(k3,:)       ! r2
            rb(k3,:)=rb(k4,:)    -rb(k3,:)       ! r3
            rb(k4,:)=rb(k1,:)    +t               ! r4
            rb(k1,:)=rb(k1,:)    -t               ! r1
            t     =qb(k0,:)    -qb(k1,:)*rzc-qb(k2,:)*rec ! q31
            qb(k2,:)=qb(k0,:)    -qb(k2,:)*rzc-qb(k1,:)*rec ! q32
            qb(k1,:)=qb(k3,:)*qze-qb(k4,:)*qet              ! q33
            qb(k4,:)=qb(k4,:)*qze+qb(k3,:)*qet              ! q34
            qb(k3,:)=qb(k2,:)    +qb(k1,:)       ! q3
            qb(k2,:)=qb(k2,:)    -qb(k1,:)       ! q2
            qb(k1,:)=t          +qb(k4,:)      ! q1
            qb(k4,:)=t          -qb(k4,:)             ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sxdfft 

!=============================================================================
SUBROUTINE dxdfft(n,ny,rb,qb) 
!=============================================================================
!                SUBROUTINE  DXDFFT: Double precision version of xdfft
!============================================================================= 
INTEGER,                     INTENT(IN   ):: n,ny
REAL(DP),DIMENSION(0:n-1,ny),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
REAL(DP),DIMENSION(ny)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._DP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0,:)=rb(0,:)*rfac; rb(1:nm,:)=rb(nm:1:-1,:)*rfac
qb(0,:)=qb(0,:)*rfac; qb(1:nm,:)=qb(nm:1:-1,:)*rfac

! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:); rb(i,:)=rb(j,:); rb(j,:)=t
      t=qb(i,:); qb(i,:)=qb(j,:); qb(j,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1;  mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(k3,:)*rf3-qb(k3,:)*qf3            ! q13
         qb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3            ! r13
         rb(k3,:)=rb(k2,:)*rf1-qb(k2,:)*qf1            ! r12
         rb(k2,:)=rb(k2,:)*qf1+qb(k2,:)*rf1            ! q12
         qb(k2,:)=qb(k3,:)    -rb(k2,:)         ! r23
         rb(k2,:)=qb(k3,:)    +rb(k2,:)         ! q22
         qb(k3,:)=rb(k3,:)    +t          ! r22
         t  =rb(k3,:)    -t          ! q23
         rb(k3,:)=rb(k1,:)*rf2-qb(k1,:)*qf2            ! r11
         qb(k1,:)=rb(k1,:)*qf2+qb(k1,:)*rf2            ! q11
         rb(k1,:)=rb(k0,:)    -rb(k3,:)         ! r21
         rb(k0,:)=rb(k0,:)    +rb(k3,:)         ! r20
         rb(k3,:)=rb(k1,:)    -qb(k2,:)         ! r3
         rb(k1,:)=rb(k1,:)    +qb(k2,:)         ! r1
         qb(k2,:)=qb(k0,:)    +qb(k1,:)         ! q20
         qb(k1,:)=qb(k0,:)    -qb(k1,:)         ! q21
         qb(k0,:)=qb(k2,:)    +rb(k2,:)         ! q0
         qb(k2,:)=qb(k2,:)    -rb(k2,:)         ! q2
         rb(k2,:)=rb(k0,:)    -qb(k3,:)         ! r2
         rb(k0,:)=rb(k0,:)    +qb(k3,:)         ! r0
         qb(k3,:)=qb(k1,:)    -t          ! q3
         qb(k1,:)=qb(k1,:)    +t          ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t       =rb(k1,:)*qf1+qb(k1,:)*rf1    ! q11
         qb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1    ! r11
         rb(k1,:)=rb(k0,:)    -qb(k1,:)        ! r1
         rb(k0,:)=rb(k0,:)    +qb(k1,:)        ! r0
         qb(k1,:)=qb(k0,:)    -t        ! q1
         qb(k0,:)=qb(k0,:)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(k2,:)*qf2+qb(k2,:) *rf2     ! r12
         qb(k2,:)= rb(k2,:)*rf2-qb(k2,:) *qf2     ! q12
         rb(k2,:)= rb(k1,:)*qf1+qb(k1,:) *rf1     ! q11
         rb(k1,:)= rb(k1,:)*rf1-qb(k1,:) *qf1     ! r11
         qb(k1,:)= rb(k2,:)    +t            ! q21
         rb(k2,:)=(rb(k2,:)    -t       )*qep   ! r22
         t  = rb(k1,:)    +qb(k2,:)   ! r21
         rb(k1,:)=(rb(k1,:)    -qb(k2,:))*qep   ! q22
         rb(k0,:)= rb(k0,:)    +t             ! r0
         qb(k0,:)= qb(k0,:)    +qb(k1,:)          ! q0
         t  = rb(k0,:)    -t        *rec   ! r21
         qb(k1,:)= qb(k0,:)    -qb(k1,:) *rec     ! q21
         qb(k2,:)= qb(k1,:)    -rb(k1,:)          ! q2
         qb(k1,:)= qb(k1,:)    +rb(k1,:)          ! q1
         rb(k1,:)= t        -rb(k2,:)          ! r1
         rb(k2,:)= t        +rb(k2,:)   ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze; 
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(k1,:)*qf1+qb(k1,:)*rf1                   ! q11
            rb(k1,:)=rb(k1,:)*rf1-qb(k1,:)*qf1                    ! r11
            qb(k1,:)=rb(k4,:)*rf4-qb(k4,:)*qf4                    ! q14
            rb(k4,:)=rb(k4,:)*qf4+qb(k4,:)*rf4                    ! r14
            qb(k4,:)=rb(k1,:)    -qb(k1,:)             ! q24
            rb(k1,:)=rb(k1,:)    +qb(k1,:)             ! r21
            qb(k1,:)=t          +rb(k4,:)                   ! q21
            rb(k4,:)=t          -rb(k4,:)                   ! r24
            t     =rb(k3,:)*rf3-qb(k3,:)*qf3                   ! q13
            rb(k3,:)=rb(k3,:)*qf3+qb(k3,:)*rf3                    ! r13
            qb(k3,:)=rb(k2,:)*qf2+qb(k2,:)*rf2                    ! q12
            rb(k2,:)=rb(k2,:)*rf2-qb(k2,:)*qf2                    ! r12
            qb(k2,:)=qb(k3,:)    +rb(k3,:)             ! q22
            rb(k3,:)=qb(k3,:)    -rb(k3,:)             ! r23
            qb(k3,:)=rb(k2,:)    -t              ! q23
            rb(k2,:)=rb(k2,:)    +t              ! r22
            rb(k0,:)=rb(k0,:)    +rb(k1,:)    +rb(k2,:)           ! r0
            qb(k0,:)=qb(k0,:)    +qb(k1,:)    +qb(k2,:)           ! q0
            t     =rb(k4,:)*qze+rb(k3,:)*qet                   ! r34
            rb(k3,:)=rb(k3,:)*qze-rb(k4,:)*qet                    ! r33
            rb(k4,:)=rb(k0,:)    -rb(k2,:)*rzc-rb(k1,:)*rec       ! r32
            rb(k1,:)=rb(k0,:)    -rb(k1,:)*rzc-rb(k2,:)*rec       ! r31
            rb(k2,:)=rb(k4,:)    +rb(k3,:)             ! r2
            rb(k3,:)=rb(k4,:)    -rb(k3,:)             ! r3
            rb(k4,:)=rb(k1,:)    +t              ! r4
            rb(k1,:)=rb(k1,:)    -t              ! r1
            t     =qb(k0,:)    -qb(k1,:)*rzc-qb(k2,:)*rec       ! q31
            qb(k2,:)=qb(k0,:)    -qb(k2,:)*rzc-qb(k1,:)*rec       ! q32
            qb(k1,:)=qb(k3,:)*qze-qb(k4,:)*qet                    ! q33
            qb(k4,:)=qb(k4,:)*qze+qb(k3,:)*qet                    ! q34
            qb(k3,:)=qb(k2,:)    +qb(k1,:)             ! q3
            qb(k2,:)=qb(k2,:)    -qb(k1,:)             ! q2
            qb(k1,:)=t          +qb(k4,:)            ! q1
            qb(k4,:)=t          -qb(k4,:)                   ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dxdfft

!=============================================================================
SUBROUTINE sycfft(nx,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  YCFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! Y-array version of CFFT (fft along dimension of slower-changing index)
!
! --> nx:   x-dimension of array
! --> n:    period  and y-dimension of array
! <-> rb:   real part of data (-->) and its transform (<--)
! <-> qb:   imaginary part of data (-->) and its transform (<--)
!============================================================================= 
INTEGER,                     INTENT(IN   ):: nx,n
REAL(SP),DIMENSION(nx,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER    nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i); rb(:,i)=rb(:,j); rb(:,j)=t
      t=qb(:,i); qb(:,i)=qb(:,j); qb(:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(:,k3)*rf3-qb(:,k3)*qf3           ! q13
         qb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3           ! r13
         rb(:,k3)=rb(:,k2)*rf1-qb(:,k2)*qf1           ! r12
         rb(:,k2)=rb(:,k2)*qf1+qb(:,k2)*rf1           ! q12
         qb(:,k2)=qb(:,k3)    -rb(:,k2)        ! r23
         rb(:,k2)=qb(:,k3)    +rb(:,k2)        ! q22
         qb(:,k3)=rb(:,k3)    +t         ! r22
         t  =rb(:,k3)    -t         ! q23
         rb(:,k3)=rb(:,k1)*rf2-qb(:,k1)*qf2           ! r11
         qb(:,k1)=rb(:,k1)*qf2+qb(:,k1)*rf2           ! q11
         rb(:,k1)=rb(:,k0)    -rb(:,k3)        ! r21
         rb(:,k0)=rb(:,k0)    +rb(:,k3)        ! r20
         rb(:,k3)=rb(:,k1)    -qb(:,k2)        ! r3
         rb(:,k1)=rb(:,k1)    +qb(:,k2)        ! r1
         qb(:,k2)=qb(:,k0)    +qb(:,k1)        ! q20
         qb(:,k1)=qb(:,k0)    -qb(:,k1)        ! q21
         qb(:,k0)=qb(:,k2)    +rb(:,k2)        ! q0
         qb(:,k2)=qb(:,k2)    -rb(:,k2)        ! q2
         rb(:,k2)=rb(:,k0)    -qb(:,k3)        ! r2
         rb(:,k0)=rb(:,k0)    +qb(:,k3)        ! r0
         qb(:,k3)=qb(:,k1)    -t         ! q3
         qb(:,k1)=qb(:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t  =rb(:,k1)*qf1+qb(:,k1)*rf1    ! q11
         qb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1    ! r11
         rb(:,k1)=rb(:,k0)    -qb(:,k1)        ! r1
         rb(:,k0)=rb(:,k0)    +qb(:,k1)        ! r0
         qb(:,k1)=qb(:,k0)    -t        ! q1
         qb(:,k0)=qb(:,k0)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(:,k2)*qf2+qb(:,k2) *rf2       ! r12
         qb(:,k2)= rb(:,k2)*rf2-qb(:,k2) *qf2       ! q12
         rb(:,k2)= rb(:,k1)*qf1+qb(:,k1) *rf1       ! q11
         rb(:,k1)= rb(:,k1)*rf1-qb(:,k1) *qf1       ! r11
         qb(:,k1)= rb(:,k2)    +t       ! q21
         rb(:,k2)=(rb(:,k2)    -t )*qep     ! r22
         t  = rb(:,k1)    +qb(:,k2)      ! r21
         rb(:,k1)=(rb(:,k1)    -qb(:,k2))*qep       ! q22
         rb(:,k0)= rb(:,k0)    +t       ! r0
         qb(:,k0)= qb(:,k0)    +qb(:,k1)      ! q0
         t  = rb(:,k0)    -t        *rec       ! r21
         qb(:,k1)= qb(:,k0)    -qb(:,k1) *rec       ! q21
         qb(:,k2)= qb(:,k1)    -rb(:,k1)      ! q2
         qb(:,k1)= qb(:,k1)    +rb(:,k1)      ! q1
         rb(:,k1)= t        -rb(:,k2)            ! r1
         rb(:,k2)= t        +rb(:,k2)     ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(:,k1)*qf1+qb(:,k1)*rf1                  ! q11
            rb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1                  ! r11
            qb(:,k1)=rb(:,k4)*rf4-qb(:,k4)*qf4                  ! q14
            rb(:,k4)=rb(:,k4)*qf4+qb(:,k4)*rf4                  ! r14
            qb(:,k4)=rb(:,k1)    -qb(:,k1)           ! q24
            rb(:,k1)=rb(:,k1)    +qb(:,k1)                 ! r21
            qb(:,k1)=t           +rb(:,k4)           ! q21
            rb(:,k4)=t           -rb(:,k4)           ! r24
            t     =rb(:,k3)*rf3-qb(:,k3)*qf3                  ! q13
            rb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3                  ! r13
            qb(:,k3)=rb(:,k2)*qf2+qb(:,k2)*rf2                  ! q12
            rb(:,k2)=rb(:,k2)*rf2-qb(:,k2)*qf2                  ! r12
            qb(:,k2)=qb(:,k3)    +rb(:,k3)           ! q22
            rb(:,k3)=qb(:,k3)    -rb(:,k3)           ! r23
            qb(:,k3)=rb(:,k2)    -t                   ! q23
            rb(:,k2)=rb(:,k2)    +t                   ! r22
            rb(:,k0)=rb(:,k0)    +rb(:,k1)    +rb(:,k2)         ! r0
            qb(:,k0)=qb(:,k0)    +qb(:,k1)    +qb(:,k2)         ! q0
            t     =rb(:,k4)*qze+rb(:,k3)*qet                  ! r34
            rb(:,k3)=rb(:,k3)*qze-rb(:,k4)*qet                  ! r33
            rb(:,k4)=rb(:,k0)    -rb(:,k2)*rzc-rb(:,k1)*rec     ! r32
            rb(:,k1)=rb(:,k0)    -rb(:,k1)*rzc-rb(:,k2)*rec     ! r31
            rb(:,k2)=rb(:,k4)    +rb(:,k3)           ! r2
            rb(:,k3)=rb(:,k4)    -rb(:,k3)           ! r3
            rb(:,k4)=rb(:,k1)    +t                   ! r4
            rb(:,k1)=rb(:,k1)    -t                   ! r1
            t     =qb(:,k0)    -qb(:,k1)*rzc-qb(:,k2)*rec     ! q31
            qb(:,k2)=qb(:,k0)    -qb(:,k2)*rzc-qb(:,k1)*rec     ! q32
            qb(:,k1)=qb(:,k3)*qze-qb(:,k4)*qet                  ! q33
            qb(:,k4)=qb(:,k4)*qze+qb(:,k3)*qet                  ! q34
            qb(:,k3)=qb(:,k2)    +qb(:,k1)           ! q3
            qb(:,k2)=qb(:,k2)    -qb(:,k1)           ! q2
            qb(:,k1)=t           +qb(:,k4)           ! q1
            qb(:,k4)=t           -qb(:,k4)           ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sycfft 

!=============================================================================
SUBROUTINE dycfft(nx,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  DYCFFT: Double precision version of ycfft
!============================================================================= 
INTEGER,                     INTENT(IN   ):: nx,n
REAL(DP),DIMENSION(nx,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i); rb(:,i)=rb(:,j); rb(:,j)=t
      t=qb(:,i); qb(:,i)=qb(:,j); qb(:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(:,k3)*rf3-qb(:,k3)*qf3     ! q13
         qb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3     ! r13
         rb(:,k3)=rb(:,k2)*rf1-qb(:,k2)*qf1     ! r12
         rb(:,k2)=rb(:,k2)*qf1+qb(:,k2)*rf1     ! q12
         qb(:,k2)=qb(:,k3)    -rb(:,k2)         ! r23
         rb(:,k2)=qb(:,k3)    +rb(:,k2)         ! q22
         qb(:,k3)=rb(:,k3)    +t          ! r22
         t  =rb(:,k3)    -t          ! q23
         rb(:,k3)=rb(:,k1)*rf2-qb(:,k1)*qf2     ! r11
         qb(:,k1)=rb(:,k1)*qf2+qb(:,k1)*rf2     ! q11
         rb(:,k1)=rb(:,k0)    -rb(:,k3)         ! r21
         rb(:,k0)=rb(:,k0)    +rb(:,k3)         ! r20
         rb(:,k3)=rb(:,k1)    -qb(:,k2)         ! r3
         rb(:,k1)=rb(:,k1)    +qb(:,k2)         ! r1
         qb(:,k2)=qb(:,k0)    +qb(:,k1)         ! q20
         qb(:,k1)=qb(:,k0)    -qb(:,k1)         ! q21
         qb(:,k0)=qb(:,k2)    +rb(:,k2)         ! q0
         qb(:,k2)=qb(:,k2)    -rb(:,k2)         ! q2
         rb(:,k2)=rb(:,k0)    -qb(:,k3)         ! r2
         rb(:,k0)=rb(:,k0)    +qb(:,k3)         ! r0
         qb(:,k3)=qb(:,k1)    -t          ! q3
         qb(:,k1)=qb(:,k1)    +t          ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t  =rb(:,k1)*qf1+qb(:,k1)*rf1    ! q11
         qb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1    ! r11
         rb(:,k1)=rb(:,k0)    -qb(:,k1)        ! r1
         rb(:,k0)=rb(:,k0)    +qb(:,k1)        ! r0
         qb(:,k1)=qb(:,k0)    -t        ! q1
         qb(:,k0)=qb(:,k0)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(:,k2)*qf2+qb(:,k2) *rf2   ! r12
         qb(:,k2)= rb(:,k2)*rf2-qb(:,k2) *qf2   ! q12
         rb(:,k2)= rb(:,k1)*qf1+qb(:,k1) *rf1   ! q11
         rb(:,k1)= rb(:,k1)*rf1-qb(:,k1) *qf1   ! r11
         qb(:,k1)= rb(:,k2)    +t          ! q21
         rb(:,k2)=(rb(:,k2)    -t )*qep   ! r22
         t  = rb(:,k1)    +qb(:,k2)        ! r21
         rb(:,k1)=(rb(:,k1)    -qb(:,k2))*qep   ! q22
         rb(:,k0)= rb(:,k0)    +t          ! r0
         qb(:,k0)= qb(:,k0)    +qb(:,k1)        ! q0
         t  = rb(:,k0)    -t        *rec   ! r21
         qb(:,k1)= qb(:,k0)    -qb(:,k1) *rec   ! q21
         qb(:,k2)= qb(:,k1)    -rb(:,k1)        ! q2
         qb(:,k1)= qb(:,k1)    +rb(:,k1)        ! q1
         rb(:,k1)= t        -rb(:,k2)        ! r1
         rb(:,k2)= t        +rb(:,k2)        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(:,k1)*qf1+qb(:,k1)*rf1              ! q11
            rb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1              ! r11
            qb(:,k1)=rb(:,k4)*rf4-qb(:,k4)*qf4              ! q14
            rb(:,k4)=rb(:,k4)*qf4+qb(:,k4)*rf4              ! r14
            qb(:,k4)=rb(:,k1)    -qb(:,k1)              ! q24
            rb(:,k1)=rb(:,k1)    +qb(:,k1)              ! r21
            qb(:,k1)=t           +rb(:,k4)              ! q21
            rb(:,k4)=t           -rb(:,k4)       ! r24
            t     =rb(:,k3)*rf3-qb(:,k3)*qf3              ! q13
            rb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3              !  r13
            qb(:,k3)=rb(:,k2)*qf2+qb(:,k2)*rf2              ! q12
            rb(:,k2)=rb(:,k2)*rf2-qb(:,k2)*qf2              ! r12
            qb(:,k2)=qb(:,k3)    +rb(:,k3)       ! q22
            rb(:,k3)=qb(:,k3)    -rb(:,k3)              ! r23
            qb(:,k3)=rb(:,k2)    -t        ! q23
            rb(:,k2)=rb(:,k2)    +t        ! r22
            rb(:,k0)=rb(:,k0)    +rb(:,k1)    +rb(:,k2)     ! r0
            qb(:,k0)=qb(:,k0)    +qb(:,k1)    +qb(:,k2)     ! q0
            t     =rb(:,k4)*qze+rb(:,k3)*qet              ! r34
            rb(:,k3)=rb(:,k3)*qze-rb(:,k4)*qet              ! r33
            rb(:,k4)=rb(:,k0)    -rb(:,k2)*rzc-rb(:,k1)*rec ! r32
            rb(:,k1)=rb(:,k0)    -rb(:,k1)*rzc-rb(:,k2)*rec ! r31
            rb(:,k2)=rb(:,k4)    +rb(:,k3)                  ! r2
            rb(:,k3)=rb(:,k4)    -rb(:,k3)              ! r3
            rb(:,k4)=rb(:,k1)    +t        ! r4
            rb(:,k1)=rb(:,k1)    -t        ! r1
            t       =qb(:,k0)    -qb(:,k1)*rzc-qb(:,k2)*rec ! q31
            qb(:,k2)=qb(:,k0)    -qb(:,k2)*rzc-qb(:,k1)*rec ! q32
            qb(:,k1)=qb(:,k3)*qze-qb(:,k4)*qet              ! q33
            qb(:,k4)=qb(:,k4)*qze+qb(:,k3)*qet              ! q34
            qb(:,k3)=qb(:,k2)    +qb(:,k1)       ! q3
            qb(:,k2)=qb(:,k2)    -qb(:,k1)       ! q2
            qb(:,k1)=t           +qb(:,k4)              ! q1
            qb(:,k4)=t           -qb(:,k4)              ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dycfft 

!=============================================================================
SUBROUTINE sydfft(nx,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  SYDFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! Y-array version of DFFT (fft along dimension of slower-changing index)
!
! --> nx:   x-dimension of array
! --> n:    period  and y-dimension of array
! <-> rb:   real part of transform (-->) and syntesized data (<--)
! <-> qb:   imaginary part of transform (-->) and synthesized data (<--)
!============================================================================= 
INTEGER,                     INTENT(IN   ):: nx,n
REAL(SP),DIMENSION(nx,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._SP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,0)=rb(:,0)*rfac; rb(:,1:nm)=rb(:,nm:1:-1)*rfac
qb(:,0)=qb(:,0)*rfac; qb(:,1:nm)=qb(:,nm:1:-1)*rfac

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i); rb(:,i)=rb(:,j); rb(:,j)=t
      t=qb(:,i); qb(:,i)=qb(:,j); qb(:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(:,k3)*rf3-qb(:,k3)*qf3           ! q13
         qb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3           ! r13
         rb(:,k3)=rb(:,k2)*rf1-qb(:,k2)*qf1           ! r12
         rb(:,k2)=rb(:,k2)*qf1+qb(:,k2)*rf1           ! q12
         qb(:,k2)=qb(:,k3)    -rb(:,k2)        ! r23
         rb(:,k2)=qb(:,k3)    +rb(:,k2)        ! q22
         qb(:,k3)=rb(:,k3)    +t         ! r22
         t  =rb(:,k3)    -t         ! q23
         rb(:,k3)=rb(:,k1)*rf2-qb(:,k1)*qf2           ! r11
         qb(:,k1)=rb(:,k1)*qf2+qb(:,k1)*rf2           ! q11
         rb(:,k1)=rb(:,k0)    -rb(:,k3)        ! r21
         rb(:,k0)=rb(:,k0)    +rb(:,k3)        ! r20
         rb(:,k3)=rb(:,k1)    -qb(:,k2)        ! r3
         rb(:,k1)=rb(:,k1)    +qb(:,k2)        ! r1
         qb(:,k2)=qb(:,k0)    +qb(:,k1)        ! q20
         qb(:,k1)=qb(:,k0)    -qb(:,k1)        ! q21
         qb(:,k0)=qb(:,k2)    +rb(:,k2)        ! q0
         qb(:,k2)=qb(:,k2)    -rb(:,k2)        ! q2
         rb(:,k2)=rb(:,k0)    -qb(:,k3)        ! r2
         rb(:,k0)=rb(:,k0)    +qb(:,k3)        ! r0
         qb(:,k3)=qb(:,k1)    -t         ! q3
         qb(:,k1)=qb(:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t  =rb(:,k1)*qf1+qb(:,k1)*rf1    ! q11
         qb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1    ! r11
         rb(:,k1)=rb(:,k0)    -qb(:,k1)        ! r1
         rb(:,k0)=rb(:,k0)    +qb(:,k1)        ! r0
         qb(:,k1)=qb(:,k0)    -t        ! q1
         qb(:,k0)=qb(:,k0)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(:,k2)*qf2+qb(:,k2) *rf2         ! r12
         qb(:,k2)= rb(:,k2)*rf2-qb(:,k2) *qf2         ! q12
         rb(:,k2)= rb(:,k1)*qf1+qb(:,k1) *rf1         ! q11
         rb(:,k1)= rb(:,k1)*rf1-qb(:,k1) *qf1         ! r11
         qb(:,k1)= rb(:,k2)    +t         ! q21
         rb(:,k2)=(rb(:,k2)    -t )*qep       ! r22
         t  = rb(:,k1)    +qb(:,k2)        ! r21
         rb(:,k1)=(rb(:,k1)    -qb(:,k2))*qep         ! q22
         rb(:,k0)= rb(:,k0)    +t         ! r0
         qb(:,k0)= qb(:,k0)    +qb(:,k1)        ! q0
         t  = rb(:,k0)    -t        *rec         ! r21
         qb(:,k1)= qb(:,k0)    -qb(:,k1) *rec         ! q21
         qb(:,k2)= qb(:,k1)    -rb(:,k1)        ! q2
         qb(:,k1)= qb(:,k1)    +rb(:,k1)        ! q1
         rb(:,k1)= t        -rb(:,k2)              ! r1
         rb(:,k2)= t        +rb(:,k2)       ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(:,k1)*qf1+qb(:,k1)*rf1              ! q11
            rb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1              ! r11
            qb(:,k1)=rb(:,k4)*rf4-qb(:,k4)*qf4              ! q14
            rb(:,k4)=rb(:,k4)*qf4+qb(:,k4)*rf4              ! r14
            qb(:,k4)=rb(:,k1)    -qb(:,k1)       ! q24
            rb(:,k1)=rb(:,k1)    +qb(:,k1)       ! r21
            qb(:,k1)=t           +rb(:,k4)       ! q21
            rb(:,k4)=t           -rb(:,k4)       ! r24
            t     =rb(:,k3)*rf3-qb(:,k3)*qf3              ! q13
            rb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3              ! r13
            qb(:,k3)=rb(:,k2)*qf2+qb(:,k2)*rf2              ! q12
            rb(:,k2)=rb(:,k2)*rf2-qb(:,k2)*qf2              ! r12
            qb(:,k2)=qb(:,k3)    +rb(:,k3)       ! q22
            rb(:,k3)=qb(:,k3)    -rb(:,k3)       ! r23
            qb(:,k3)=rb(:,k2)    -t                      ! q23
            rb(:,k2)=rb(:,k2)    +t               ! r22
            rb(:,k0)=rb(:,k0)    +rb(:,k1)    +rb(:,k2)     ! r0
            qb(:,k0)=qb(:,k0)    +qb(:,k1)    +qb(:,k2)     ! q0
            t     =rb(:,k4)*qze+rb(:,k3)*qet              ! r34
            rb(:,k3)=rb(:,k3)*qze-rb(:,k4)*qet              ! r33
            rb(:,k4)=rb(:,k0)    -rb(:,k2)*rzc-rb(:,k1)*rec ! r32
            rb(:,k1)=rb(:,k0)    -rb(:,k1)*rzc-rb(:,k2)*rec ! r31
            rb(:,k2)=rb(:,k4)    +rb(:,k3)       ! r2
            rb(:,k3)=rb(:,k4)    -rb(:,k3)       ! r3
            rb(:,k4)=rb(:,k1)    +t                      ! r4
            rb(:,k1)=rb(:,k1)    -t                      ! r1
            t     =qb(:,k0)    -qb(:,k1)*rzc-qb(:,k2)*rec ! q31
            qb(:,k2)=qb(:,k0)    -qb(:,k2)*rzc-qb(:,k1)*rec ! q32
            qb(:,k1)=qb(:,k3)*qze-qb(:,k4)*qet              ! q33
            qb(:,k4)=qb(:,k4)*qze+qb(:,k3)*qet              ! q34
            qb(:,k3)=qb(:,k2)    +qb(:,k1)       ! q3
            qb(:,k2)=qb(:,k2)    -qb(:,k1)       ! q2
            qb(:,k1)=t           +qb(:,k4)       ! q1
            qb(:,k4)=t           -qb(:,k4)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sydfft

!=============================================================================
SUBROUTINE dydfft(nx,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  DYDFFT: Double precision version of ydfft
!============================================================================= 
INTEGER,                     INTENT(IN   ):: nx,n
REAL(DP),DIMENSION(nx,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx)    :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._DP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,0)=rb(:,0)*rfac; rb(:,1:nm)=rb(:,nm:1:-1)*rfac
qb(:,0)=qb(:,0)*rfac; qb(:,1:nm)=qb(:,nm:1:-1)*rfac

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i); rb(:,i)=rb(:,j); rb(:,j)=t
      t=qb(:,i); qb(:,i)=qb(:,j); qb(:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t  =rb(:,k3)*rf3-qb(:,k3)*qf3           ! q13
         qb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3           ! r13
         rb(:,k3)=rb(:,k2)*rf1-qb(:,k2)*qf1           ! r12
         rb(:,k2)=rb(:,k2)*qf1+qb(:,k2)*rf1           ! q12
         qb(:,k2)=qb(:,k3)    -rb(:,k2)        ! r23
         rb(:,k2)=qb(:,k3)    +rb(:,k2)        ! q22
         qb(:,k3)=rb(:,k3)    +t         ! r22
         t  =rb(:,k3)    -t         ! q23
         rb(:,k3)=rb(:,k1)*rf2-qb(:,k1)*qf2           ! r11
         qb(:,k1)=rb(:,k1)*qf2+qb(:,k1)*rf2           ! q11
         rb(:,k1)=rb(:,k0)    -rb(:,k3)        ! r21
         rb(:,k0)=rb(:,k0)    +rb(:,k3)        ! r20
         rb(:,k3)=rb(:,k1)    -qb(:,k2)        ! r3
         rb(:,k1)=rb(:,k1)    +qb(:,k2)        ! r1
         qb(:,k2)=qb(:,k0)    +qb(:,k1)        ! q20
         qb(:,k1)=qb(:,k0)    -qb(:,k1)        ! q21
         qb(:,k0)=qb(:,k2)    +rb(:,k2)        ! q0
         qb(:,k2)=qb(:,k2)    -rb(:,k2)        ! q2
         rb(:,k2)=rb(:,k0)    -qb(:,k3)        ! r2
         rb(:,k0)=rb(:,k0)    +qb(:,k3)        ! r0
         qb(:,k3)=qb(:,k1)    -t         ! q3
         qb(:,k1)=qb(:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t  =rb(:,k1)*qf1+qb(:,k1)*rf1    ! q11
         qb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1    ! r11
         rb(:,k1)=rb(:,k0)    -qb(:,k1)        ! r1
         rb(:,k0)=rb(:,k0)    +qb(:,k1)        ! r0
         qb(:,k1)=qb(:,k0)    -t        ! q1
         qb(:,k0)=qb(:,k0)    +t        ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t  = rb(:,k2)*qf2+qb(:,k2) *rf2       ! r12
         qb(:,k2)= rb(:,k2)*rf2-qb(:,k2) *qf2       ! q12
         rb(:,k2)= rb(:,k1)*qf1+qb(:,k1) *rf1       ! q11
         rb(:,k1)= rb(:,k1)*rf1-qb(:,k1) *qf1       ! r11
         qb(:,k1)= rb(:,k2)    +t       ! q21
         rb(:,k2)=(rb(:,k2)    -t )*qep     ! r22
         t  = rb(:,k1)    +qb(:,k2)      ! r21
         rb(:,k1)=(rb(:,k1)    -qb(:,k2))*qep       ! q22
         rb(:,k0)= rb(:,k0)    +t       ! r0
         qb(:,k0)= qb(:,k0)    +qb(:,k1)      ! q0
         t  = rb(:,k0)    -t        *rec       ! r21
         qb(:,k1)= qb(:,k0)    -qb(:,k1) *rec       ! q21
         qb(:,k2)= qb(:,k1)    -rb(:,k1)      ! q2
         qb(:,k1)= qb(:,k1)    +rb(:,k1)      ! q1
         rb(:,k1)= t        -rb(:,k2)            ! r1
         rb(:,k2)= t        +rb(:,k2)     ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t     =rb(:,k1)*qf1+qb(:,k1)*rf1              ! q11
            rb(:,k1)=rb(:,k1)*rf1-qb(:,k1)*qf1              ! r11
            qb(:,k1)=rb(:,k4)*rf4-qb(:,k4)*qf4              ! q14
            rb(:,k4)=rb(:,k4)*qf4+qb(:,k4)*rf4              ! r14
            qb(:,k4)=rb(:,k1)    -qb(:,k1)       ! q24
            rb(:,k1)=rb(:,k1)    +qb(:,k1)       ! r21
            qb(:,k1)=t           +rb(:,k4)       ! q21
            rb(:,k4)=t           -rb(:,k4)       ! r24
            t     =rb(:,k3)*rf3-qb(:,k3)*qf3              ! q13
            rb(:,k3)=rb(:,k3)*qf3+qb(:,k3)*rf3              ! r13
            qb(:,k3)=rb(:,k2)*qf2+qb(:,k2)*rf2              ! q12
            rb(:,k2)=rb(:,k2)*rf2-qb(:,k2)*qf2              ! r12
            qb(:,k2)=qb(:,k3)    +rb(:,k3)       ! q22
            rb(:,k3)=qb(:,k3)    -rb(:,k3)       ! r23
            qb(:,k3)=rb(:,k2)    -t               ! q23
            rb(:,k2)=rb(:,k2)    +t               ! r22
            rb(:,k0)=rb(:,k0)    +rb(:,k1)    +rb(:,k2)     ! r0
            qb(:,k0)=qb(:,k0)    +qb(:,k1)    +qb(:,k2)     ! q0
            t     =rb(:,k4)*qze+rb(:,k3)*qet              ! r34
            rb(:,k3)=rb(:,k3)*qze-rb(:,k4)*qet              ! r33
            rb(:,k4)=rb(:,k0)    -rb(:,k2)*rzc-rb(:,k1)*rec ! r32
            rb(:,k1)=rb(:,k0)    -rb(:,k1)*rzc-rb(:,k2)*rec ! r31
            rb(:,k2)=rb(:,k4)    +rb(:,k3)       ! r2
            rb(:,k3)=rb(:,k4)    -rb(:,k3)       ! r3
            rb(:,k4)=rb(:,k1)    +t               ! r4
            rb(:,k1)=rb(:,k1)    -t               ! r1
            t     =qb(:,k0)    -qb(:,k1)*rzc-qb(:,k2)*rec ! q31
            qb(:,k2)=qb(:,k0)    -qb(:,k2)*rzc-qb(:,k1)*rec ! q32
            qb(:,k1)=qb(:,k3)*qze-qb(:,k4)*qet              ! q33
            qb(:,k4)=qb(:,k4)*qze+qb(:,k3)*qet              ! q34
            qb(:,k3)=qb(:,k2)    +qb(:,k1)       ! q3
            qb(:,k2)=qb(:,k2)    -qb(:,k1)       ! q2
            qb(:,k1)=t           +qb(:,k4)       ! q1
            qb(:,k4)=t           -qb(:,k4)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dydfft 

!=============================================================================
SUBROUTINE sx3cfft(n,ny,nz,rb,qb) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!                SUBROUTINE  SXCFFT
! X-array version of CFFT (fft along dimension of faster-changing index) in 3D
!
! --> n:    period (x-dimension)
! --> ny:   y-dimension of array
! --> nz:   z-dimension of array
! <-> rb:   real part of data (-->) and its transform (<--)
! <-> qb:   imaginary part of data (-->) and its transform (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: n,ny,nz
REAL(SP),DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
          nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(ny,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:,:); rb(i,:,:)=rb(j,:,:); rb(j,:,:)=t
      t=qb(i,:,:); qb(i,:,:)=qb(j,:,:); qb(j,:,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
  ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3    ! q13
         qb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3    ! r13
         rb(k3,:,:)=rb(k2,:,:)*rf1-qb(k2,:,:)*qf1    ! r12
         rb(k2,:,:)=rb(k2,:,:)*qf1+qb(k2,:,:)*rf1    ! q12
         qb(k2,:,:)=qb(k3,:,:)    -rb(k2,:,:)        ! r23
         rb(k2,:,:)=qb(k3,:,:)    +rb(k2,:,:)      ! q22
         qb(k3,:,:)=rb(k3,:,:)    +t               ! r22
         t    =rb(k3,:,:)    -t              ! q23
         rb(k3,:,:)=rb(k1,:,:)*rf2-qb(k1,:,:)*qf2    ! r11
         qb(k1,:,:)=rb(k1,:,:)*qf2+qb(k1,:,:)*rf2    ! q11
         rb(k1,:,:)=rb(k0,:,:)    -rb(k3,:,:)        ! r21
         rb(k0,:,:)=rb(k0,:,:)    +rb(k3,:,:)        ! r20
         rb(k3,:,:)=rb(k1,:,:)    -qb(k2,:,:)      ! r3
         rb(k1,:,:)=rb(k1,:,:)    +qb(k2,:,:)      ! r1
         qb(k2,:,:)=qb(k0,:,:)    +qb(k1,:,:)        ! q20
         qb(k1,:,:)=qb(k0,:,:)    -qb(k1,:,:)      ! q21
         qb(k0,:,:)=qb(k2,:,:)    +rb(k2,:,:)      ! q0
         qb(k2,:,:)=qb(k2,:,:)    -rb(k2,:,:)      ! q2
         rb(k2,:,:)=rb(k0,:,:)    -qb(k3,:,:)        ! r2
         rb(k0,:,:)=rb(k0,:,:)    +qb(k3,:,:)        ! r0
         qb(k3,:,:)=qb(k1,:,:)    -t               ! q3
         qb(k1,:,:)=qb(k1,:,:)    +t               ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;      k1=k0+ma
         rf1=w(jmb);  qf1=w(nh+jmb)
         t    =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1     ! q11
         qb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1     ! r11
         rb(k1,:,:)=rb(k0,:,:)    -qb(k1,:,:)       ! r1
         rb(k0,:,:)=rb(k0,:,:)    +qb(k1,:,:)       ! r0
         qb(k1,:,:)=qb(k0,:,:)    -t               ! q1
         qb(k0,:,:)=qb(k0,:,:)    +t               ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_SP;  rec=1.5_SP;  qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t         = rb(k2,:,:)*qf2+qb(k2,:,:) *rf2      ! r12
         qb(k2,:,:)= rb(k2,:,:)*rf2-qb(k2,:,:) *qf2      ! q12
         rb(k2,:,:)= rb(k1,:,:)*qf1+qb(k1,:,:) *rf1      ! q11
         rb(k1,:,:)= rb(k1,:,:)*rf1-qb(k1,:,:) *qf1      ! r11
         qb(k1,:,:)= rb(k2,:,:)    +t                   ! q21
         rb(k2,:,:)=(rb(k2,:,:)    -t        ) *qep  ! r22
         t    = rb(k1,:,:)    +qb(k2,:,:)           ! r21
         rb(k1,:,:)=(rb(k1,:,:)    -qb(k2,:,:))*qep      ! q22
         rb(k0,:,:)= rb(k0,:,:)    +t                   ! r0
         qb(k0,:,:)= qb(k0,:,:)    +qb(k1,:,:)           ! q0
         t    = rb(k0,:,:)    -t          *rec  ! r21
         qb(k1,:,:)= qb(k0,:,:)    -qb(k1,:,:) *rec      ! q21
         qb(k2,:,:)= qb(k1,:,:)    -rb(k1,:,:)           ! q2
         qb(k1,:,:)= qb(k1,:,:)    +rb(k1,:,:)           ! q1
         rb(k1,:,:)= t             -rb(k2,:,:)           ! r1
         rb(k2,:,:)= t             +rb(k2,:,:)           ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       = rb(k1,:,:)*qf1+qb(k1,:,:)*rf1                    ! q11
            rb(k1,:,:)= rb(k1,:,:)*rf1-qb(k1,:,:)*qf1                   ! r11
            qb(k1,:,:)= rb(k4,:,:)*rf4-qb(k4,:,:)*qf4                   ! q14
            rb(k4,:,:)= rb(k4,:,:)*qf4+qb(k4,:,:)*rf4                   ! r14
            qb(k4,:,:)= rb(k1,:,:)    -qb(k1,:,:)            ! q24
            rb(k1,:,:)= rb(k1,:,:)    +qb(k1,:,:)            ! r21
            qb(k1,:,:)= t             +rb(k4,:,:)                  ! q21
            rb(k4,:,:)= t             -rb(k4,:,:)           ! r24
            t       = rb(k3,:,:)*rf3-qb(k3,:,:)*qf3                   ! q13
            rb(k3,:,:)= rb(k3,:,:)*qf3+qb(k3,:,:)*rf3                   ! r13
            qb(k3,:,:)= rb(k2,:,:)*qf2+qb(k2,:,:)*rf2                   ! q12
            rb(k2,:,:)= rb(k2,:,:)*rf2-qb(k2,:,:)*qf2                   ! r12
            qb(k2,:,:)= qb(k3,:,:)    +rb(k3,:,:)            ! q22
            rb(k3,:,:)= qb(k3,:,:)    -rb(k3,:,:)            ! r23
            qb(k3,:,:)= rb(k2,:,:)    -t                           ! q23
            rb(k2,:,:)= rb(k2,:,:)    +t                    ! r22
            rb(k0,:,:)= rb(k0,:,:)    +rb(k1,:,:)    +rb(k2,:,:)         ! r0
            qb(k0,:,:)= qb(k0,:,:)    +qb(k1,:,:)    +qb(k2,:,:)         ! q0
            t       = rb(k4,:,:)*qze+rb(k3,:,:)*qet                    ! r34
            rb(k3,:,:)= rb(k3,:,:)*qze-rb(k4,:,:)*qet                   ! r33
            rb(k4,:,:)= rb(k0,:,:)    -rb(k2,:,:)*rzc-rb(k1,:,:)*rec     ! r32
            rb(k1,:,:)= rb(k0,:,:)    -rb(k1,:,:)*rzc-rb(k2,:,:)*rec     ! r31
            rb(k2,:,:)= rb(k4,:,:)    +rb(k3,:,:)            ! r2
            rb(k3,:,:)= rb(k4,:,:)    -rb(k3,:,:)            ! r3
            rb(k4,:,:)= rb(k1,:,:)    +t             ! r4
            rb(k1,:,:)= rb(k1,:,:)    -t             ! r1
            t       = qb(k0,:,:)    -qb(k1,:,:)*rzc-qb(k2,:,:)*rec     ! q31
            qb(k2,:,:)= qb(k0,:,:)    -qb(k2,:,:)*rzc-qb(k1,:,:)*rec     ! q32
            qb(k1,:,:)= qb(k3,:,:)*qze-qb(k4,:,:)*qet                   ! q33
            qb(k4,:,:)= qb(k4,:,:)*qze+qb(k3,:,:)*qet                   ! q34
            qb(k3,:,:)= qb(k2,:,:)    +qb(k1,:,:)            ! q3
            qb(k2,:,:)= qb(k2,:,:)    -qb(k1,:,:)            ! q2
            qb(k1,:,:)= t             +qb(k4,:,:)    ! q1
            qb(k4,:,:)= t             -qb(k4,:,:)    ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sx3cfft 

!=============================================================================
SUBROUTINE dx3cfft(n,ny,nz,rb,qb)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!                SUBROUTINE  DXCFFT
! Double precision version of xc3fft
!============================================================================= 
INTEGER,                        INTENT(IN   ):: n,ny,nz
REAL(DP),DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
REAL(DP),DIMENSION(ny,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:,:); rb(i,:,:)=rb(j,:,:); rb(j,:,:)=t
      t=qb(i,:,:); qb(i,:,:)=qb(j,:,:); qb(j,:,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3            ! q13
         qb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3            ! r13
         rb(k3,:,:)=rb(k2,:,:)*rf1-qb(k2,:,:)*qf1            ! r12
         rb(k2,:,:)=rb(k2,:,:)*qf1+qb(k2,:,:)*rf1            ! q12
         qb(k2,:,:)=qb(k3,:,:)    -rb(k2,:,:)               ! r23
         rb(k2,:,:)=qb(k3,:,:)    +rb(k2,:,:)               ! q22
         qb(k3,:,:)=rb(k3,:,:)    +t                ! r22
         t    =rb(k3,:,:)    -t                ! q23
         rb(k3,:,:)=rb(k1,:,:)*rf2-qb(k1,:,:)*qf2            ! r11
         qb(k1,:,:)=rb(k1,:,:)*qf2+qb(k1,:,:)*rf2            ! q11
         rb(k1,:,:)=rb(k0,:,:)    -rb(k3,:,:)               ! r21
         rb(k0,:,:)=rb(k0,:,:)    +rb(k3,:,:)               ! r20
         rb(k3,:,:)=rb(k1,:,:)    -qb(k2,:,:)               ! r3
         rb(k1,:,:)=rb(k1,:,:)    +qb(k2,:,:)               ! r1
         qb(k2,:,:)=qb(k0,:,:)    +qb(k1,:,:)               ! q20
         qb(k1,:,:)=qb(k0,:,:)    -qb(k1,:,:)               ! q21
         qb(k0,:,:)=qb(k2,:,:)    +rb(k2,:,:)               ! q0
         qb(k2,:,:)=qb(k2,:,:)    -rb(k2,:,:)               ! q2
         rb(k2,:,:)=rb(k0,:,:)    -qb(k3,:,:)               ! r2
         rb(k0,:,:)=rb(k0,:,:)    +qb(k3,:,:)               ! r0
         qb(k3,:,:)=qb(k1,:,:)    -t                ! q3
         qb(k1,:,:)=qb(k1,:,:)    +t                ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;      k1=k0+ma
         rf1=w(jmb);  qf1=w(nh+jmb)
         t    =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1      ! q11
         qb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1      ! r11
         rb(k1,:,:)=rb(k0,:,:)    -qb(k1,:,:)        ! r1
         rb(k0,:,:)=rb(k0,:,:)    +qb(k1,:,:)        ! r0
         qb(k1,:,:)=qb(k0,:,:)    -t                ! q1
         qb(k0,:,:)=qb(k0,:,:)    +t                ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_DP;  rec=1.5_DP;  qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t         = rb(k2,:,:)*qf2+qb(k2,:,:) *rf2             ! r12
         qb(k2,:,:)= rb(k2,:,:)*rf2-qb(k2,:,:) *qf2             ! q12
         rb(k2,:,:)= rb(k1,:,:)*qf1+qb(k1,:,:) *rf1             ! q11
         rb(k1,:,:)= rb(k1,:,:)*rf1-qb(k1,:,:) *qf1             ! r11
         qb(k1,:,:)= rb(k2,:,:)    +t                   ! q21
         rb(k2,:,:)=(rb(k2,:,:)    -t         )*qep  ! r22
         t    = rb(k1,:,:)    +qb(k2,:,:)                 ! r21
         rb(k1,:,:)=(rb(k1,:,:)    -qb(k2,:,:))*qep            ! q22
         rb(k0,:,:)= rb(k0,:,:)    +t                   ! r0
         qb(k0,:,:)= qb(k0,:,:)    +qb(k1,:,:)                  ! q0
         t    = rb(k0,:,:)    -t*rec          ! r21
         qb(k1,:,:)= qb(k0,:,:)    -qb(k1,:,:) *rec         ! q21
         qb(k2,:,:)= qb(k1,:,:)    -rb(k1,:,:)                  ! q2
         qb(k1,:,:)= qb(k1,:,:)    +rb(k1,:,:)                  ! q1
         rb(k1,:,:)= t             -rb(k2,:,:)                 ! r1
         rb(k2,:,:)= t             +rb(k2,:,:)                 ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1                     ! q11
            rb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1                   ! r11
            qb(k1,:,:)=rb(k4,:,:)*rf4-qb(k4,:,:)*qf4                   ! q14
            rb(k4,:,:)=rb(k4,:,:)*qf4+qb(k4,:,:)*rf4                   ! r14
            qb(k4,:,:)=rb(k1,:,:)    -qb(k1,:,:)            ! q24
            rb(k1,:,:)=rb(k1,:,:)    +qb(k1,:,:)            ! r21
            qb(k1,:,:)=t             +rb(k4,:,:)                  ! q21
            rb(k4,:,:)=t             -rb(k4,:,:)           ! r24
            t       =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3                  ! q13
            rb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3                   ! r13
            qb(k3,:,:)=rb(k2,:,:)*qf2+qb(k2,:,:)*rf2                   ! q12
            rb(k2,:,:)=rb(k2,:,:)*rf2-qb(k2,:,:)*qf2                   ! r12
            qb(k2,:,:)=qb(k3,:,:)    +rb(k3,:,:)            ! q22
            rb(k3,:,:)=qb(k3,:,:)    -rb(k3,:,:)            ! r23
            qb(k3,:,:)=rb(k2,:,:)    -t             ! q23
            rb(k2,:,:)=rb(k2,:,:)    +t             ! r22
            rb(k0,:,:)=rb(k0,:,:)    +rb(k1,:,:)    +rb(k2,:,:)          ! r0
            qb(k0,:,:)=qb(k0,:,:)    +qb(k1,:,:)    +qb(k2,:,:)          ! q0
            t       =rb(k4,:,:)*qze+rb(k3,:,:)*qet                  ! r34
            rb(k3,:,:)=rb(k3,:,:)*qze-rb(k4,:,:)*qet                   ! r33
            rb(k4,:,:)=rb(k0,:,:)    -rb(k2,:,:)*rzc-rb(k1,:,:)*rec      ! r32
            rb(k1,:,:)=rb(k0,:,:)    -rb(k1,:,:)*rzc-rb(k2,:,:)*rec      ! r31
            rb(k2,:,:)=rb(k4,:,:)    +rb(k3,:,:)            ! r2
            rb(k3,:,:)=rb(k4,:,:)    -rb(k3,:,:)            ! r3
            rb(k4,:,:)=rb(k1,:,:)    +t             ! r4
            rb(k1,:,:)=rb(k1,:,:)    -t             ! r1
            t       =qb(k0,:,:)    -qb(k1,:,:)*rzc-qb(k2,:,:)*rec      ! q31
            qb(k2,:,:)=qb(k0,:,:)    -qb(k2,:,:)*rzc-qb(k1,:,:)*rec      ! q32
            qb(k1,:,:)=qb(k3,:,:)*qze-qb(k4,:,:)*qet                   ! q33
            qb(k4,:,:)=qb(k4,:,:)*qze+qb(k3,:,:)*qet                   ! q34
            qb(k3,:,:)=qb(k2,:,:)    +qb(k1,:,:)            ! q3
            qb(k2,:,:)=qb(k2,:,:)    -qb(k1,:,:)            ! q2
            qb(k1,:,:)=t             +qb(k4,:,:)    ! q1
            qb(k4,:,:)=t             -qb(k4,:,:)    ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dx3cfft

!=============================================================================
SUBROUTINE sx3dfft(n,ny,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  SX3DFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! X-array version of DFFT (fft along dimension of faster-changing index) in 3D
!
! --> n:    period (x-dimension)
! --> ny:   y-dimension of array
! --> nz:   z-direction of array
! <-> rb:   real part of transform (-->) and the Fourier synthesis (<--)
! <-> qb:   imaginary part of transform (-->) and the Fourier synthesis (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: n,ny,nz
REAL(SP),DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(ny,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._SP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0,:,:)=rb(0,:,:)*rfac; rb(1:nm,:,:)=rb(nm:1:-1,:,:)*rfac
qb(0,:,:)=qb(0,:,:)*rfac; qb(1:nm,:,:)=qb(nm:1:-1,:,:)*rfac

! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:,:); rb(i,:,:)=rb(j,:,:); rb(j,:,:)=t
      t=qb(i,:,:); qb(i,:,:)=qb(j,:,:); qb(j,:,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1;  mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3            ! q13
         qb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3            ! r13
         rb(k3,:,:)=rb(k2,:,:)*rf1-qb(k2,:,:)*qf1            ! r12
         rb(k2,:,:)=rb(k2,:,:)*qf1+qb(k2,:,:)*rf1            ! q12
         qb(k2,:,:)=qb(k3,:,:)    -rb(k2,:,:)               ! r23
         rb(k2,:,:)=qb(k3,:,:)    +rb(k2,:,:)               ! q22
         qb(k3,:,:)=rb(k3,:,:)    +t                ! r22
         t    =rb(k3,:,:)    -t                ! q23
         rb(k3,:,:)=rb(k1,:,:)*rf2-qb(k1,:,:)*qf2            ! r11
         qb(k1,:,:)=rb(k1,:,:)*qf2+qb(k1,:,:)*rf2            ! q11
         rb(k1,:,:)=rb(k0,:,:)    -rb(k3,:,:)               ! r21
         rb(k0,:,:)=rb(k0,:,:)    +rb(k3,:,:)               ! r20
         rb(k3,:,:)=rb(k1,:,:)    -qb(k2,:,:)               ! r3
         rb(k1,:,:)=rb(k1,:,:)    +qb(k2,:,:)               ! r1
         qb(k2,:,:)=qb(k0,:,:)    +qb(k1,:,:)               ! q20
         qb(k1,:,:)=qb(k0,:,:)    -qb(k1,:,:)               ! q21
         qb(k0,:,:)=qb(k2,:,:)    +rb(k2,:,:)               ! q0
         qb(k2,:,:)=qb(k2,:,:)    -rb(k2,:,:)               ! q2
         rb(k2,:,:)=rb(k0,:,:)    -qb(k3,:,:)               ! r2
         rb(k0,:,:)=rb(k0,:,:)    +qb(k3,:,:)               ! r0
         qb(k3,:,:)=qb(k1,:,:)    -t                ! q3
         qb(k1,:,:)=qb(k1,:,:)    +t                ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t         =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1      ! q11
         qb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1      ! r11
         rb(k1,:,:)=rb(k0,:,:)    -qb(k1,:,:)        ! r1
         rb(k0,:,:)=rb(k0,:,:)    +qb(k1,:,:)        ! r0
         qb(k1,:,:)=qb(k0,:,:)    -t                ! q1
         qb(k0,:,:)=qb(k0,:,:)    +t                ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(k2,:,:)*qf2+qb(k2,:,:) *rf2 ! r12
         qb(k2,:,:)= rb(k2,:,:)*rf2-qb(k2,:,:) *qf2     ! q12
         rb(k2,:,:)= rb(k1,:,:)*qf1+qb(k1,:,:) *rf1     ! q11
         rb(k1,:,:)= rb(k1,:,:)*rf1-qb(k1,:,:) *qf1     ! r11
         qb(k1,:,:)= rb(k2,:,:)    +t                  ! q21
         rb(k2,:,:)=(rb(k2,:,:)    -t         )*qep ! r22
         t    = rb(k1,:,:)    +qb(k2,:,:)         ! r21
         rb(k1,:,:)=(rb(k1,:,:)    -qb(k2,:,:))*qep ! q22
         rb(k0,:,:)= rb(k0,:,:)    +t                  ! r0
         qb(k0,:,:)= qb(k0,:,:)    +qb(k1,:,:)          ! q0
         t    = rb(k0,:,:)    -t*rec  ! r21
         qb(k1,:,:)= qb(k0,:,:)    -qb(k1,:,:) *rec     ! q21
         qb(k2,:,:)= qb(k1,:,:)    -rb(k1,:,:)          ! q2
         qb(k1,:,:)= qb(k1,:,:)    +rb(k1,:,:)          ! q1
         rb(k1,:,:)= t            -rb(k2,:,:)         ! r1
         rb(k2,:,:)= t            +rb(k2,:,:)         ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze; 
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1             ! q11
            rb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1              ! r11
            qb(k1,:,:)=rb(k4,:,:)*rf4-qb(k4,:,:)*qf4              ! q14
            rb(k4,:,:)=rb(k4,:,:)*qf4+qb(k4,:,:)*rf4              ! r14
            qb(k4,:,:)=rb(k1,:,:)    -qb(k1,:,:)                    ! q24
            rb(k1,:,:)=rb(k1,:,:)    +qb(k1,:,:)              ! r21
            qb(k1,:,:)=t      +rb(k4,:,:)             ! q21
            rb(k4,:,:)=t      -rb(k4,:,:)             ! r24
            t       =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3             ! q13
            rb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3              ! r13
            qb(k3,:,:)=rb(k2,:,:)*qf2+qb(k2,:,:)*rf2              ! q12
            rb(k2,:,:)=rb(k2,:,:)*rf2-qb(k2,:,:)*qf2              ! r12
            qb(k2,:,:)=qb(k3,:,:)    +rb(k3,:,:)              ! q22
            rb(k3,:,:)=qb(k3,:,:)    -rb(k3,:,:)              ! r23
            qb(k3,:,:)=rb(k2,:,:)    -t               ! q23
            rb(k2,:,:)=rb(k2,:,:)    +t               ! r22
            rb(k0,:,:)=rb(k0,:,:)    +rb(k1,:,:)    +rb(k2,:,:)     ! r0
            qb(k0,:,:)=qb(k0,:,:)    +qb(k1,:,:)    +qb(k2,:,:)     ! q0
            t       =rb(k4,:,:)*qze+rb(k3,:,:)*qet               ! r34
            rb(k3,:,:)=rb(k3,:,:)*qze-rb(k4,:,:)*qet               ! r33
            rb(k4,:,:)=rb(k0,:,:)    -rb(k2,:,:)*rzc-rb(k1,:,:)*rec ! r32
            rb(k1,:,:)=rb(k0,:,:)    -rb(k1,:,:)*rzc-rb(k2,:,:)*rec ! r31
            rb(k2,:,:)=rb(k4,:,:)    +rb(k3,:,:)       ! r2
            rb(k3,:,:)=rb(k4,:,:)    -rb(k3,:,:)       ! r3
            rb(k4,:,:)=rb(k1,:,:)    +t               ! r4
            rb(k1,:,:)=rb(k1,:,:)    -t               ! r1
            t       =qb(k0,:,:)    -qb(k1,:,:)*rzc-qb(k2,:,:)*rec ! q31
            qb(k2,:,:)=qb(k0,:,:)    -qb(k2,:,:)*rzc-qb(k1,:,:)*rec ! q32
            qb(k1,:,:)=qb(k3,:,:)*qze-qb(k4,:,:)*qet              ! q33
            qb(k4,:,:)=qb(k4,:,:)*qze+qb(k3,:,:)*qet              ! q34
            qb(k3,:,:)=qb(k2,:,:)    +qb(k1,:,:)       ! q3
            qb(k2,:,:)=qb(k2,:,:)    -qb(k1,:,:)       ! q2
            qb(k1,:,:)=t      +qb(k4,:,:)      ! q1
            qb(k4,:,:)=t      -qb(k4,:,:)             ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sx3dfft 

!=============================================================================
SUBROUTINE dx3dfft(n,ny,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  DX3DFFT: Double precision version of x3dfft
!============================================================================= 
INTEGER,                        INTENT(IN   ):: n,ny,nz
REAL(DP),DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
REAL(DP),DIMENSION(ny,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._DP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0,:,:)=rb(0,:,:)*rfac; rb(1:nm,:,:)=rb(nm:1:-1,:,:)*rfac
qb(0,:,:)=qb(0,:,:)*rfac; qb(1:nm,:,:)=qb(nm:1:-1,:,:)*rfac

! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(i,:,:); rb(i,:,:)=rb(j,:,:); rb(j,:,:)=t
      t=qb(i,:,:); qb(i,:,:)=qb(j,:,:); qb(j,:,:)=t
   ENDIF
ENDDO
!  TRANSFORM THE DATA:
ma=1;  mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;            jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3            ! q13
         qb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3            ! r13
         rb(k3,:,:)=rb(k2,:,:)*rf1-qb(k2,:,:)*qf1            ! r12
         rb(k2,:,:)=rb(k2,:,:)*qf1+qb(k2,:,:)*rf1            ! q12
         qb(k2,:,:)=qb(k3,:,:)    -rb(k2,:,:)               ! r23
         rb(k2,:,:)=qb(k3,:,:)    +rb(k2,:,:)               ! q22
         qb(k3,:,:)=rb(k3,:,:)    +t                ! r22
         t    =rb(k3,:,:)    -t                ! q23
         rb(k3,:,:)=rb(k1,:,:)*rf2-qb(k1,:,:)*qf2            ! r11
         qb(k1,:,:)=rb(k1,:,:)*qf2+qb(k1,:,:)*rf2            ! q11
         rb(k1,:,:)=rb(k0,:,:)    -rb(k3,:,:)               ! r21
         rb(k0,:,:)=rb(k0,:,:)    +rb(k3,:,:)               ! r20
         rb(k3,:,:)=rb(k1,:,:)    -qb(k2,:,:)               ! r3
         rb(k1,:,:)=rb(k1,:,:)    +qb(k2,:,:)               ! r1
         qb(k2,:,:)=qb(k0,:,:)    +qb(k1,:,:)               ! q20
         qb(k1,:,:)=qb(k0,:,:)    -qb(k1,:,:)               ! q21
         qb(k0,:,:)=qb(k2,:,:)    +rb(k2,:,:)               ! q0
         qb(k2,:,:)=qb(k2,:,:)    -rb(k2,:,:)               ! q2
         rb(k2,:,:)=rb(k0,:,:)    -qb(k3,:,:)               ! r2
         rb(k0,:,:)=rb(k0,:,:)    +qb(k3,:,:)               ! r0
         qb(k3,:,:)=qb(k1,:,:)    -t                ! q3
         qb(k1,:,:)=qb(k1,:,:)    +t                ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO

IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t         =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1      ! q11
         qb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1      ! r11
         rb(k1,:,:)=rb(k0,:,:)    -qb(k1,:,:)        ! r1
         rb(k0,:,:)=rb(k0,:,:)    +qb(k1,:,:)        ! r0
         qb(k1,:,:)=qb(k0,:,:)    -t            ! q1
         qb(k0,:,:)=qb(k0,:,:)    +t            ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(k2,:,:)*qf2+qb(k2,:,:) *rf2       ! r12
         qb(k2,:,:)= rb(k2,:,:)*rf2-qb(k2,:,:) *qf2       ! q12
         rb(k2,:,:)= rb(k1,:,:)*qf1+qb(k1,:,:) *rf1       ! q11
         rb(k1,:,:)= rb(k1,:,:)*rf1-qb(k1,:,:) *qf1       ! r11
         qb(k1,:,:)= rb(k2,:,:)    +t                    ! q21
         rb(k2,:,:)=(rb(k2,:,:)    -t         )*qep   ! r22
         t    = rb(k1,:,:)    +qb(k2,:,:)           ! r21
         rb(k1,:,:)=(rb(k1,:,:)    -qb(k2,:,:))*qep   ! q22
         rb(k0,:,:)= rb(k0,:,:)    +t                     ! r0
         qb(k0,:,:)= qb(k0,:,:)    +qb(k1,:,:)            ! q0
         t    = rb(k0,:,:)    -t          *rec   ! r21
         qb(k1,:,:)= qb(k0,:,:)    -qb(k1,:,:) *rec       ! q21
         qb(k2,:,:)= qb(k1,:,:)    -rb(k1,:,:)            ! q2
         qb(k1,:,:)= qb(k1,:,:)    +rb(k1,:,:)            ! q1
         rb(k1,:,:)= t            -rb(k2,:,:)          ! r1
         rb(k2,:,:)= t            +rb(k2,:,:)     ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze; 
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(k1,:,:)*qf1+qb(k1,:,:)*rf1                   ! q11
            rb(k1,:,:)=rb(k1,:,:)*rf1-qb(k1,:,:)*qf1                    ! r11
            qb(k1,:,:)=rb(k4,:,:)*rf4-qb(k4,:,:)*qf4                    ! q14
            rb(k4,:,:)=rb(k4,:,:)*qf4+qb(k4,:,:)*rf4                    ! r14
            qb(k4,:,:)=rb(k1,:,:)    -qb(k1,:,:)             ! q24
            rb(k1,:,:)=rb(k1,:,:)    +qb(k1,:,:)             ! r21
            qb(k1,:,:)=t      +rb(k4,:,:)                   ! q21
            rb(k4,:,:)=t      -rb(k4,:,:)                   ! r24
            t       =rb(k3,:,:)*rf3-qb(k3,:,:)*qf3                   ! q13
            rb(k3,:,:)=rb(k3,:,:)*qf3+qb(k3,:,:)*rf3                    ! r13
            qb(k3,:,:)=rb(k2,:,:)*qf2+qb(k2,:,:)*rf2                    ! q12
            rb(k2,:,:)=rb(k2,:,:)*rf2-qb(k2,:,:)*qf2                    ! r12
            qb(k2,:,:)=qb(k3,:,:)    +rb(k3,:,:)             ! q22
            rb(k3,:,:)=qb(k3,:,:)    -rb(k3,:,:)             ! r23
            qb(k3,:,:)=rb(k2,:,:)    -t              ! q23
            rb(k2,:,:)=rb(k2,:,:)    +t              ! r22
            rb(k0,:,:)=rb(k0,:,:)    +rb(k1,:,:)    +rb(k2,:,:)           ! r0
            qb(k0,:,:)=qb(k0,:,:)    +qb(k1,:,:)    +qb(k2,:,:)           ! q0
            t       =rb(k4,:,:)*qze+rb(k3,:,:)*qet                   ! r34
            rb(k3,:,:)=rb(k3,:,:)*qze-rb(k4,:,:)*qet                    ! r33
            rb(k4,:,:)=rb(k0,:,:)    -rb(k2,:,:)*rzc-rb(k1,:,:)*rec       ! r32
            rb(k1,:,:)=rb(k0,:,:)    -rb(k1,:,:)*rzc-rb(k2,:,:)*rec       ! r31
            rb(k2,:,:)=rb(k4,:,:)    +rb(k3,:,:)             ! r2
            rb(k3,:,:)=rb(k4,:,:)    -rb(k3,:,:)             ! r3
            rb(k4,:,:)=rb(k1,:,:)    +t              ! r4
            rb(k1,:,:)=rb(k1,:,:)    -t              ! r1
            t       =qb(k0,:,:)    -qb(k1,:,:)*rzc-qb(k2,:,:)*rec       ! q31
            qb(k2,:,:)=qb(k0,:,:)    -qb(k2,:,:)*rzc-qb(k1,:,:)*rec       ! q32
            qb(k1,:,:)=qb(k3,:,:)*qze-qb(k4,:,:)*qet                    ! q33
            qb(k4,:,:)=qb(k4,:,:)*qze+qb(k3,:,:)*qet                    ! q34
            qb(k3,:,:)=qb(k2,:,:)    +qb(k1,:,:)             ! q3
            qb(k2,:,:)=qb(k2,:,:)    -qb(k1,:,:)             ! q2
            qb(k1,:,:)=t      +qb(k4,:,:)            ! q1
            qb(k4,:,:)=t      -qb(k4,:,:)                   ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dx3dfft

!=============================================================================
SUBROUTINE sy3cfft(nx,n,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  Y3CFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! Y-array version of CFFT (fft along dimension of slower-changing index) in 3D
!
! --> nx:   x-dimension of array
! --> n:    period  and y-dimension of array
! --> nz:   z-dimension of array
! <-> rb:   real part of data (-->) and its transform (<--)
! <-> qb:   imaginary part of data (-->) and its transform (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,n,nz
REAL(SP),DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER    nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i,:); rb(:,i,:)=rb(:,j,:); rb(:,j,:)=t
      t=qb(:,i,:); qb(:,i,:)=qb(:,j,:); qb(:,j,:)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3           ! q13
         qb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3           ! r13
         rb(:,k3,:)=rb(:,k2,:)*rf1-qb(:,k2,:)*qf1           ! r12
         rb(:,k2,:)=rb(:,k2,:)*qf1+qb(:,k2,:)*rf1           ! q12
         qb(:,k2,:)=qb(:,k3,:)    -rb(:,k2,:)              ! r23
         rb(:,k2,:)=qb(:,k3,:)    +rb(:,k2,:)              ! q22
         qb(:,k3,:)=rb(:,k3,:)    +t               ! r22
         t    =rb(:,k3,:)    -t               ! q23
         rb(:,k3,:)=rb(:,k1,:)*rf2-qb(:,k1,:)*qf2           ! r11
         qb(:,k1,:)=rb(:,k1,:)*qf2+qb(:,k1,:)*rf2           ! q11
         rb(:,k1,:)=rb(:,k0,:)    -rb(:,k3,:)              ! r21
         rb(:,k0,:)=rb(:,k0,:)    +rb(:,k3,:)              ! r20
         rb(:,k3,:)=rb(:,k1,:)    -qb(:,k2,:)              ! r3
         rb(:,k1,:)=rb(:,k1,:)    +qb(:,k2,:)              ! r1
         qb(:,k2,:)=qb(:,k0,:)    +qb(:,k1,:)              ! q20
         qb(:,k1,:)=qb(:,k0,:)    -qb(:,k1,:)              ! q21
         qb(:,k0,:)=qb(:,k2,:)    +rb(:,k2,:)              ! q0
         qb(:,k2,:)=qb(:,k2,:)    -rb(:,k2,:)              ! q2
         rb(:,k2,:)=rb(:,k0,:)    -qb(:,k3,:)              ! r2
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k3,:)              ! r0
         qb(:,k3,:)=qb(:,k1,:)    -t               ! q3
         qb(:,k1,:)=qb(:,k1,:)    +t               ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1      ! q11
         qb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1      ! r11
         rb(:,k1,:)=rb(:,k0,:)    -qb(:,k1,:)        ! r1
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k1,:)        ! r0
         qb(:,k1,:)=qb(:,k0,:)    -t                ! q1
         qb(:,k0,:)=qb(:,k0,:)    +t                ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,k2,:)*qf2+qb(:,k2,:) *rf2       ! r12
         qb(:,k2,:)= rb(:,k2,:)*rf2-qb(:,k2,:) *qf2       ! q12
         rb(:,k2,:)= rb(:,k1,:)*qf1+qb(:,k1,:) *rf1       ! q11
         rb(:,k1,:)= rb(:,k1,:)*rf1-qb(:,k1,:) *qf1       ! r11
         qb(:,k1,:)= rb(:,k2,:)    +t             ! q21
         rb(:,k2,:)=(rb(:,k2,:)    -t       )*qep   ! r22
         t    = rb(:,k1,:)    +qb(:,k2,:)            ! r21
         rb(:,k1,:)=(rb(:,k1,:)    -qb(:,k2,:))*qep       ! q22
         rb(:,k0,:)= rb(:,k0,:)    +t             ! r0
         qb(:,k0,:)= qb(:,k0,:)    +qb(:,k1,:)            ! q0
         t    = rb(:,k0,:)    -t          *rec       ! r21
         qb(:,k1,:)= qb(:,k0,:)    -qb(:,k1,:) *rec       ! q21
         qb(:,k2,:)= qb(:,k1,:)    -rb(:,k1,:)            ! q2
         qb(:,k1,:)= qb(:,k1,:)    +rb(:,k1,:)            ! q1
         rb(:,k1,:)= t            -rb(:,k2,:)            ! r1
         rb(:,k2,:)= t            +rb(:,k2,:)           ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1                  ! q11
            rb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1                  ! r11
            qb(:,k1,:)=rb(:,k4,:)*rf4-qb(:,k4,:)*qf4                  ! q14
            rb(:,k4,:)=rb(:,k4,:)*qf4+qb(:,k4,:)*rf4                  ! r14
            qb(:,k4,:)=rb(:,k1,:)    -qb(:,k1,:)           ! q24
            rb(:,k1,:)=rb(:,k1,:)    +qb(:,k1,:)                 ! r21
            qb(:,k1,:)=t             +rb(:,k4,:)           ! q21
            rb(:,k4,:)=t             -rb(:,k4,:)           ! r24
            t       =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3                  ! q13
            rb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3                  ! r13
            qb(:,k3,:)=rb(:,k2,:)*qf2+qb(:,k2,:)*rf2                  ! q12
            rb(:,k2,:)=rb(:,k2,:)*rf2-qb(:,k2,:)*qf2                  ! r12
            qb(:,k2,:)=qb(:,k3,:)    +rb(:,k3,:)           ! q22
            rb(:,k3,:)=qb(:,k3,:)    -rb(:,k3,:)           ! r23
            qb(:,k3,:)=rb(:,k2,:)    -t                   ! q23
            rb(:,k2,:)=rb(:,k2,:)    +t                   ! r22
            rb(:,k0,:)=rb(:,k0,:)    +rb(:,k1,:)    +rb(:,k2,:)         ! r0
            qb(:,k0,:)=qb(:,k0,:)    +qb(:,k1,:)    +qb(:,k2,:)         ! q0
            t       =rb(:,k4,:)*qze+rb(:,k3,:)*qet                  ! r34
            rb(:,k3,:)=rb(:,k3,:)*qze-rb(:,k4,:)*qet                  ! r33
            rb(:,k4,:)=rb(:,k0,:)    -rb(:,k2,:)*rzc-rb(:,k1,:)*rec     ! r32
            rb(:,k1,:)=rb(:,k0,:)    -rb(:,k1,:)*rzc-rb(:,k2,:)*rec     ! r31
            rb(:,k2,:)=rb(:,k4,:)    +rb(:,k3,:)           ! r2
            rb(:,k3,:)=rb(:,k4,:)    -rb(:,k3,:)           ! r3
            rb(:,k4,:)=rb(:,k1,:)    +t                   ! r4
            rb(:,k1,:)=rb(:,k1,:)    -t                   ! r1
            t       =qb(:,k0,:)    -qb(:,k1,:)*rzc-qb(:,k2,:)*rec     ! q31
            qb(:,k2,:)=qb(:,k0,:)    -qb(:,k2,:)*rzc-qb(:,k1,:)*rec     ! q32
            qb(:,k1,:)=qb(:,k3,:)*qze-qb(:,k4,:)*qet                  ! q33
            qb(:,k4,:)=qb(:,k4,:)*qze+qb(:,k3,:)*qet                  ! q34
            qb(:,k3,:)=qb(:,k2,:)    +qb(:,k1,:)           ! q3
            qb(:,k2,:)=qb(:,k2,:)    -qb(:,k1,:)           ! q2
            qb(:,k1,:)=t             +qb(:,k4,:)           ! q1
            qb(:,k4,:)=t             -qb(:,k4,:)           ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sy3cfft 

!=============================================================================
SUBROUTINE dy3cfft(nx,n,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  DY3CFFT: Double precision version of ycfft in 3D
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,n,nz
REAL(DP),DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i,:); rb(:,i,:)=rb(:,j,:); rb(:,j,:)=t
      t=qb(:,i,:); qb(:,i,:)=qb(:,j,:); qb(:,j,:)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3     ! q13
         qb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3     ! r13
         rb(:,k3,:)=rb(:,k2,:)*rf1-qb(:,k2,:)*qf1     ! r12
         rb(:,k2,:)=rb(:,k2,:)*qf1+qb(:,k2,:)*rf1     ! q12
         qb(:,k2,:)=qb(:,k3,:)    -rb(:,k2,:)         ! r23
         rb(:,k2,:)=qb(:,k3,:)    +rb(:,k2,:)         ! q22
         qb(:,k3,:)=rb(:,k3,:)    +t                ! r22
         t    =rb(:,k3,:)    -t                ! q23
         rb(:,k3,:)=rb(:,k1,:)*rf2-qb(:,k1,:)*qf2     ! r11
         qb(:,k1,:)=rb(:,k1,:)*qf2+qb(:,k1,:)*rf2     ! q11
         rb(:,k1,:)=rb(:,k0,:)    -rb(:,k3,:)         ! r21
         rb(:,k0,:)=rb(:,k0,:)    +rb(:,k3,:)         ! r20
         rb(:,k3,:)=rb(:,k1,:)    -qb(:,k2,:)         ! r3
         rb(:,k1,:)=rb(:,k1,:)    +qb(:,k2,:)         ! r1
         qb(:,k2,:)=qb(:,k0,:)    +qb(:,k1,:)         ! q20
         qb(:,k1,:)=qb(:,k0,:)    -qb(:,k1,:)         ! q21
         qb(:,k0,:)=qb(:,k2,:)    +rb(:,k2,:)         ! q0
         qb(:,k2,:)=qb(:,k2,:)    -rb(:,k2,:)         ! q2
         rb(:,k2,:)=rb(:,k0,:)    -qb(:,k3,:)         ! r2
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k3,:)         ! r0
         qb(:,k3,:)=qb(:,k1,:)    -t                ! q3
         qb(:,k1,:)=qb(:,k1,:)    +t                ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1    ! q11
         qb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1    ! r11
         rb(:,k1,:)=rb(:,k0,:)    -qb(:,k1,:)      ! r1
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k1,:)      ! r0
         qb(:,k1,:)=qb(:,k0,:)    -t              ! q1
         qb(:,k0,:)=qb(:,k0,:)    +t              ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,k2,:)*qf2+qb(:,k2,:) *rf2   ! r12
         qb(:,k2,:)= rb(:,k2,:)*rf2-qb(:,k2,:) *qf2   ! q12
         rb(:,k2,:)= rb(:,k1,:)*qf1+qb(:,k1,:) *rf1   ! q11
         rb(:,k1,:)= rb(:,k1,:)*rf1-qb(:,k1,:) *qf1   ! r11
         qb(:,k1,:)= rb(:,k2,:)    +t                ! q21
         rb(:,k2,:)=(rb(:,k2,:)    -t       )*qep   ! r22
         t    = rb(:,k1,:)    +qb(:,k2,:)        ! r21
         rb(:,k1,:)=(rb(:,k1,:)    -qb(:,k2,:))*qep   ! q22
         rb(:,k0,:)= rb(:,k0,:)    +t                ! r0
         qb(:,k0,:)= qb(:,k0,:)    +qb(:,k1,:)        ! q0
         t    = rb(:,k0,:)    -t          *rec   ! r21
         qb(:,k1,:)= qb(:,k0,:)    -qb(:,k1,:) *rec   ! q21
         qb(:,k2,:)= qb(:,k1,:)    -rb(:,k1,:)        ! q2
         qb(:,k1,:)= qb(:,k1,:)    +rb(:,k1,:)        ! q1
         rb(:,k1,:)= t            -rb(:,k2,:)        ! r1
         rb(:,k2,:)= t            +rb(:,k2,:)        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1              ! q11
            rb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1              ! r11
            qb(:,k1,:)=rb(:,k4,:)*rf4-qb(:,k4,:)*qf4              ! q14
            rb(:,k4,:)=rb(:,k4,:)*qf4+qb(:,k4,:)*rf4              ! r14
            qb(:,k4,:)=rb(:,k1,:)    -qb(:,k1,:)              ! q24
            rb(:,k1,:)=rb(:,k1,:)    +qb(:,k1,:)              ! r21
            qb(:,k1,:)=t             +rb(:,k4,:)              ! q21
            rb(:,k4,:)=t             -rb(:,k4,:)       ! r24
            t       =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3              ! q13
            rb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3              ! r13
            qb(:,k3,:)=rb(:,k2,:)*qf2+qb(:,k2,:)*rf2              ! q12
            rb(:,k2,:)=rb(:,k2,:)*rf2-qb(:,k2,:)*qf2              ! r12
            qb(:,k2,:)=qb(:,k3,:)    +rb(:,k3,:)       ! q22
            rb(:,k3,:)=qb(:,k3,:)    -rb(:,k3,:)              ! r23
            qb(:,k3,:)=rb(:,k2,:)    -t        ! q23
            rb(:,k2,:)=rb(:,k2,:)    +t        ! r22
            rb(:,k0,:)=rb(:,k0,:)    +rb(:,k1,:)    +rb(:,k2,:)     ! r0
            qb(:,k0,:)=qb(:,k0,:)    +qb(:,k1,:)    +qb(:,k2,:)     ! q0
            t       =rb(:,k4,:)*qze+rb(:,k3,:)*qet              ! r34
            rb(:,k3,:)=rb(:,k3,:)*qze-rb(:,k4,:)*qet              ! r33
            rb(:,k4,:)=rb(:,k0,:)    -rb(:,k2,:)*rzc-rb(:,k1,:)*rec ! r32
            rb(:,k1,:)=rb(:,k0,:)    -rb(:,k1,:)*rzc-rb(:,k2,:)*rec ! r31
            rb(:,k2,:)=rb(:,k4,:)    +rb(:,k3,:)                  ! r2
            rb(:,k3,:)=rb(:,k4,:)    -rb(:,k3,:)              ! r3
            rb(:,k4,:)=rb(:,k1,:)    +t        ! r4
            rb(:,k1,:)=rb(:,k1,:)    -t        ! r1
            t         =qb(:,k0,:)    -qb(:,k1,:)*rzc-qb(:,k2,:)*rec ! q31
            qb(:,k2,:)=qb(:,k0,:)    -qb(:,k2,:)*rzc-qb(:,k1,:)*rec ! q32
            qb(:,k1,:)=qb(:,k3,:)*qze-qb(:,k4,:)*qet              ! q33
            qb(:,k4,:)=qb(:,k4,:)*qze+qb(:,k3,:)*qet              ! q34
            qb(:,k3,:)=qb(:,k2,:)    +qb(:,k1,:)       ! q3
            qb(:,k2,:)=qb(:,k2,:)    -qb(:,k1,:)       ! q2
            qb(:,k1,:)=t             +qb(:,k4,:)              ! q1
            qb(:,k4,:)=t             -qb(:,k4,:)              ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dy3cfft 

!=============================================================================
SUBROUTINE sy3dfft(nx,n,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  SYDFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! Y-array version of DFFT (fft along dimension of slower-changing index) in 3D
!
! --> nx:   x-dimension of array
! --> n:    period  and y-dimension of array
! --> nz:   z-dimension of array
! <-> rb:   real part of transform (-->) and syntesized data (<--)
! <-> qb:   imaginary part of transform (-->) and synthesized data (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,n,nz
REAL(SP),DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._SP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,0,:)=rb(:,0,:)*rfac; rb(:,1:nm,:)=rb(:,nm:1:-1,:)*rfac
qb(:,0,:)=qb(:,0,:)*rfac; qb(:,1:nm,:)=qb(:,nm:1:-1,:)*rfac

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i,:); rb(:,i,:)=rb(:,j,:); rb(:,j,:)=t
      t=qb(:,i,:); qb(:,i,:)=qb(:,j,:); qb(:,j,:)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3           ! q13
         qb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3           ! r13
         rb(:,k3,:)=rb(:,k2,:)*rf1-qb(:,k2,:)*qf1           ! r12
         rb(:,k2,:)=rb(:,k2,:)*qf1+qb(:,k2,:)*rf1           ! q12
         qb(:,k2,:)=qb(:,k3,:)    -rb(:,k2,:)              ! r23
         rb(:,k2,:)=qb(:,k3,:)    +rb(:,k2,:)              ! q22
         qb(:,k3,:)=rb(:,k3,:)    +t               ! r22
         t    =rb(:,k3,:)    -t               ! q23
         rb(:,k3,:)=rb(:,k1,:)*rf2-qb(:,k1,:)*qf2           ! r11
         qb(:,k1,:)=rb(:,k1,:)*qf2+qb(:,k1,:)*rf2           ! q11
         rb(:,k1,:)=rb(:,k0,:)    -rb(:,k3,:)              ! r21
         rb(:,k0,:)=rb(:,k0,:)    +rb(:,k3,:)              ! r20
         rb(:,k3,:)=rb(:,k1,:)    -qb(:,k2,:)              ! r3
         rb(:,k1,:)=rb(:,k1,:)    +qb(:,k2,:)              ! r1
         qb(:,k2,:)=qb(:,k0,:)    +qb(:,k1,:)              ! q20
         qb(:,k1,:)=qb(:,k0,:)    -qb(:,k1,:)              ! q21
         qb(:,k0,:)=qb(:,k2,:)    +rb(:,k2,:)              ! q0
         qb(:,k2,:)=qb(:,k2,:)    -rb(:,k2,:)              ! q2
         rb(:,k2,:)=rb(:,k0,:)    -qb(:,k3,:)              ! r2
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k3,:)              ! r0
         qb(:,k3,:)=qb(:,k1,:)    -t               ! q3
         qb(:,k1,:)=qb(:,k1,:)    +t               ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1      ! q11
         qb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1      ! r11
         rb(:,k1,:)=rb(:,k0,:)    -qb(:,k1,:)        ! r1
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k1,:)        ! r0
         qb(:,k1,:)=qb(:,k0,:)    -t                ! q1
         qb(:,k0,:)=qb(:,k0,:)    +t                ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,k2,:)*qf2+qb(:,k2,:) *rf2         ! r12
         qb(:,k2,:)= rb(:,k2,:)*rf2-qb(:,k2,:) *qf2         ! q12
         rb(:,k2,:)= rb(:,k1,:)*qf1+qb(:,k1,:) *rf1         ! q11
         rb(:,k1,:)= rb(:,k1,:)*rf1-qb(:,k1,:) *qf1         ! r11
         qb(:,k1,:)= rb(:,k2,:)    +t               ! q21
         rb(:,k2,:)=(rb(:,k2,:)    -t       )*qep     ! r22
         t    = rb(:,k1,:)    +qb(:,k2,:)              ! r21
         rb(:,k1,:)=(rb(:,k1,:)    -qb(:,k2,:))*qep         ! q22
         rb(:,k0,:)= rb(:,k0,:)    +t               ! r0
         qb(:,k0,:)= qb(:,k0,:)    +qb(:,k1,:)              ! q0
         t    = rb(:,k0,:)    -t          *rec         ! r21
         qb(:,k1,:)= qb(:,k0,:)    -qb(:,k1,:) *rec         ! q21
         qb(:,k2,:)= qb(:,k1,:)    -rb(:,k1,:)              ! q2
         qb(:,k1,:)= qb(:,k1,:)    +rb(:,k1,:)              ! q1
         rb(:,k1,:)= t            -rb(:,k2,:)              ! r1
         rb(:,k2,:)= t            +rb(:,k2,:)             ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1              ! q11
            rb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1              ! r11
            qb(:,k1,:)=rb(:,k4,:)*rf4-qb(:,k4,:)*qf4              ! q14
            rb(:,k4,:)=rb(:,k4,:)*qf4+qb(:,k4,:)*rf4              ! r14
            qb(:,k4,:)=rb(:,k1,:)    -qb(:,k1,:)       ! q24
            rb(:,k1,:)=rb(:,k1,:)    +qb(:,k1,:)       ! r21
            qb(:,k1,:)=t             +rb(:,k4,:)       ! q21
            rb(:,k4,:)=t             -rb(:,k4,:)       ! r24
            t       =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3              ! q13
            rb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3              ! r13
            qb(:,k3,:)=rb(:,k2,:)*qf2+qb(:,k2,:)*rf2              ! q12
            rb(:,k2,:)=rb(:,k2,:)*rf2-qb(:,k2,:)*qf2              ! r12
            qb(:,k2,:)=qb(:,k3,:)    +rb(:,k3,:)       ! q22
            rb(:,k3,:)=qb(:,k3,:)    -rb(:,k3,:)       ! r23
            qb(:,k3,:)=rb(:,k2,:)    -t                      ! q23
            rb(:,k2,:)=rb(:,k2,:)    +t               ! r22
            rb(:,k0,:)=rb(:,k0,:)    +rb(:,k1,:)    +rb(:,k2,:)     ! r0
            qb(:,k0,:)=qb(:,k0,:)    +qb(:,k1,:)    +qb(:,k2,:)     ! q0
            t       =rb(:,k4,:)*qze+rb(:,k3,:)*qet              ! r34
            rb(:,k3,:)=rb(:,k3,:)*qze-rb(:,k4,:)*qet              ! r33
            rb(:,k4,:)=rb(:,k0,:)    -rb(:,k2,:)*rzc-rb(:,k1,:)*rec ! r32
            rb(:,k1,:)=rb(:,k0,:)    -rb(:,k1,:)*rzc-rb(:,k2,:)*rec ! r31
            rb(:,k2,:)=rb(:,k4,:)    +rb(:,k3,:)       ! r2
            rb(:,k3,:)=rb(:,k4,:)    -rb(:,k3,:)       ! r3
            rb(:,k4,:)=rb(:,k1,:)    +t                      ! r4
            rb(:,k1,:)=rb(:,k1,:)    -t                      ! r1
            t       =qb(:,k0,:)    -qb(:,k1,:)*rzc-qb(:,k2,:)*rec ! q31
            qb(:,k2,:)=qb(:,k0,:)    -qb(:,k2,:)*rzc-qb(:,k1,:)*rec ! q32
            qb(:,k1,:)=qb(:,k3,:)*qze-qb(:,k4,:)*qet              ! q33
            qb(:,k4,:)=qb(:,k4,:)*qze+qb(:,k3,:)*qet              ! q34
            qb(:,k3,:)=qb(:,k2,:)    +qb(:,k1,:)       ! q3
            qb(:,k2,:)=qb(:,k2,:)    -qb(:,k1,:)       ! q2
            qb(:,k1,:)=t             +qb(:,k4,:)       ! q1
            qb(:,k4,:)=t             -qb(:,k4,:)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE sy3dfft

!=============================================================================
SUBROUTINE dy3dfft(nx,n,nz,rb,qb) 
!=============================================================================
!                SUBROUTINE  DY3DFFT: Double precision version of ydfft in 3D
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,n,nz
REAL(DP),DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx,nz) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._DP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,0,:)=rb(:,0,:)*rfac; rb(:,1:nm,:)=rb(:,nm:1:-1,:)*rfac
qb(:,0,:)=qb(:,0,:)*rfac; qb(:,1:nm,:)=qb(:,nm:1:-1,:)*rfac

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,i,:); rb(:,i,:)=rb(:,j,:); rb(:,j,:)=t
      t=qb(:,i,:); qb(:,i,:)=qb(:,j,:); qb(:,j,:)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3           ! q13
         qb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3           ! r13
         rb(:,k3,:)=rb(:,k2,:)*rf1-qb(:,k2,:)*qf1           ! r12
         rb(:,k2,:)=rb(:,k2,:)*qf1+qb(:,k2,:)*rf1           ! q12
         qb(:,k2,:)=qb(:,k3,:)    -rb(:,k2,:)              ! r23
         rb(:,k2,:)=qb(:,k3,:)    +rb(:,k2,:)              ! q22
         qb(:,k3,:)=rb(:,k3,:)    +t               ! r22
         t    =rb(:,k3,:)    -t               ! q23
         rb(:,k3,:)=rb(:,k1,:)*rf2-qb(:,k1,:)*qf2           ! r11
         qb(:,k1,:)=rb(:,k1,:)*qf2+qb(:,k1,:)*rf2           ! q11
         rb(:,k1,:)=rb(:,k0,:)    -rb(:,k3,:)              ! r21
         rb(:,k0,:)=rb(:,k0,:)    +rb(:,k3,:)              ! r20
         rb(:,k3,:)=rb(:,k1,:)    -qb(:,k2,:)              ! r3
         rb(:,k1,:)=rb(:,k1,:)    +qb(:,k2,:)              ! r1
         qb(:,k2,:)=qb(:,k0,:)    +qb(:,k1,:)              ! q20
         qb(:,k1,:)=qb(:,k0,:)    -qb(:,k1,:)              ! q21
         qb(:,k0,:)=qb(:,k2,:)    +rb(:,k2,:)              ! q0
         qb(:,k2,:)=qb(:,k2,:)    -rb(:,k2,:)              ! q2
         rb(:,k2,:)=rb(:,k0,:)    -qb(:,k3,:)              ! r2
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k3,:)              ! r0
         qb(:,k3,:)=qb(:,k1,:)    -t               ! q3
         qb(:,k1,:)=qb(:,k1,:)    +t               ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1      ! q11
         qb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1      ! r11
         rb(:,k1,:)=rb(:,k0,:)    -qb(:,k1,:)        ! r1
         rb(:,k0,:)=rb(:,k0,:)    +qb(:,k1,:)        ! r0
         qb(:,k1,:)=qb(:,k0,:)    -t                ! q1
         qb(:,k0,:)=qb(:,k0,:)    +t                ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,k2,:)*qf2+qb(:,k2,:) *rf2       ! r12
         qb(:,k2,:)= rb(:,k2,:)*rf2-qb(:,k2,:) *qf2       ! q12
         rb(:,k2,:)= rb(:,k1,:)*qf1+qb(:,k1,:) *rf1       ! q11
         rb(:,k1,:)= rb(:,k1,:)*rf1-qb(:,k1,:) *qf1       ! r11
         qb(:,k1,:)= rb(:,k2,:)    +t             ! q21
         rb(:,k2,:)=(rb(:,k2,:)    -t       )*qep   ! r22
         t    = rb(:,k1,:)    +qb(:,k2,:)            ! r21
         rb(:,k1,:)=(rb(:,k1,:)    -qb(:,k2,:))*qep       ! q22
         rb(:,k0,:)= rb(:,k0,:)    +t             ! r0
         qb(:,k0,:)= qb(:,k0,:)    +qb(:,k1,:)            ! q0
         t    = rb(:,k0,:)    -t          *rec       ! r21
         qb(:,k1,:)= qb(:,k0,:)    -qb(:,k1,:) *rec       ! q21
         qb(:,k2,:)= qb(:,k1,:)    -rb(:,k1,:)            ! q2
         qb(:,k1,:)= qb(:,k1,:)    +rb(:,k1,:)            ! q1
         rb(:,k1,:)= t            -rb(:,k2,:)            ! r1
         rb(:,k2,:)= t            +rb(:,k2,:)           ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,k1,:)*qf1+qb(:,k1,:)*rf1              ! q11
            rb(:,k1,:)=rb(:,k1,:)*rf1-qb(:,k1,:)*qf1              ! r11
            qb(:,k1,:)=rb(:,k4,:)*rf4-qb(:,k4,:)*qf4              ! q14
            rb(:,k4,:)=rb(:,k4,:)*qf4+qb(:,k4,:)*rf4              ! r14
            qb(:,k4,:)=rb(:,k1,:)    -qb(:,k1,:)       ! q24
            rb(:,k1,:)=rb(:,k1,:)    +qb(:,k1,:)       ! r21
            qb(:,k1,:)=t             +rb(:,k4,:)       ! q21
            rb(:,k4,:)=t             -rb(:,k4,:)       ! r24
            t       =rb(:,k3,:)*rf3-qb(:,k3,:)*qf3              ! q13
            rb(:,k3,:)=rb(:,k3,:)*qf3+qb(:,k3,:)*rf3              ! r13
            qb(:,k3,:)=rb(:,k2,:)*qf2+qb(:,k2,:)*rf2              ! q12
            rb(:,k2,:)=rb(:,k2,:)*rf2-qb(:,k2,:)*qf2              ! r12
            qb(:,k2,:)=qb(:,k3,:)    +rb(:,k3,:)       ! q22
            rb(:,k3,:)=qb(:,k3,:)    -rb(:,k3,:)       ! r23
            qb(:,k3,:)=rb(:,k2,:)    -t               ! q23
            rb(:,k2,:)=rb(:,k2,:)    +t               ! r22
            rb(:,k0,:)=rb(:,k0,:)    +rb(:,k1,:)    +rb(:,k2,:)     ! r0
            qb(:,k0,:)=qb(:,k0,:)    +qb(:,k1,:)    +qb(:,k2,:)     ! q0
            t       =rb(:,k4,:)*qze+rb(:,k3,:)*qet              ! r34
            rb(:,k3,:)=rb(:,k3,:)*qze-rb(:,k4,:)*qet              ! r33
            rb(:,k4,:)=rb(:,k0,:)    -rb(:,k2,:)*rzc-rb(:,k1,:)*rec ! r32
            rb(:,k1,:)=rb(:,k0,:)    -rb(:,k1,:)*rzc-rb(:,k2,:)*rec ! r31
            rb(:,k2,:)=rb(:,k4,:)    +rb(:,k3,:)       ! r2
            rb(:,k3,:)=rb(:,k4,:)    -rb(:,k3,:)       ! r3
            rb(:,k4,:)=rb(:,k1,:)    +t               ! r4
            rb(:,k1,:)=rb(:,k1,:)    -t               ! r1
            t       =qb(:,k0,:)    -qb(:,k1,:)*rzc-qb(:,k2,:)*rec ! q31
            qb(:,k2,:)=qb(:,k0,:)    -qb(:,k2,:)*rzc-qb(:,k1,:)*rec ! q32
            qb(:,k1,:)=qb(:,k3,:)*qze-qb(:,k4,:)*qet              ! q33
            qb(:,k4,:)=qb(:,k4,:)*qze+qb(:,k3,:)*qet              ! q34
            qb(:,k3,:)=qb(:,k2,:)    +qb(:,k1,:)       ! q3
            qb(:,k2,:)=qb(:,k2,:)    -qb(:,k1,:)       ! q2
            qb(:,k1,:)=t             +qb(:,k4,:)       ! q1
            qb(:,k4,:)=t           -  qb(:,k4,:)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dy3dfft 

!=============================================================================
SUBROUTINE szcfft(nx,ny,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  ZCFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999, z-version, 2006)
! Z-array version of CFFT (fft along dimension of slower-changing index)
!
! --> nx:   x-dimension of array
! --> ny:   y-dimension of array
! --> n:    period and z-dimension of array
! <-> rb:   real part of data (-->) and its transform (<--)
! <-> qb:   imaginary part of data (-->) and its transform (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,ny,n
REAL(SP),DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER    nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx,ny) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,:,i); rb(:,:,i)=rb(:,:,j); rb(:,:,j)=t
      t=qb(:,:,i); qb(:,:,i)=qb(:,:,j); qb(:,:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3     ! q13
         qb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3     ! r13
         rb(:,:,k3)=rb(:,:,k2)*rf1-qb(:,:,k2)*qf1     ! r12
         rb(:,:,k2)=rb(:,:,k2)*qf1+qb(:,:,k2)*rf1     ! q12
         qb(:,:,k2)=qb(:,:,k3)    -rb(:,:,k2)        ! r23
         rb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k2)        ! q22
         qb(:,:,k3)=rb(:,:,k3)    +t         ! r22
         t    =rb(:,:,k3)    -t         ! q23
         rb(:,:,k3)=rb(:,:,k1)*rf2-qb(:,:,k1)*qf2     ! r11
         qb(:,:,k1)=rb(:,:,k1)*qf2+qb(:,:,k1)*rf2     ! q11
         rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k3)        ! r21
         rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k3)        ! r20
         rb(:,:,k3)=rb(:,:,k1)    -qb(:,:,k2)        ! r3
         rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k2)        ! r1
         qb(:,:,k2)=qb(:,:,k0)    +qb(:,:,k1)        ! q20
         qb(:,:,k1)=qb(:,:,k0)    -qb(:,:,k1)        ! q21
         qb(:,:,k0)=qb(:,:,k2)    +rb(:,:,k2)        ! q0
         qb(:,:,k2)=qb(:,:,k2)    -rb(:,:,k2)        ! q2
         rb(:,:,k2)=rb(:,:,k0)    -qb(:,:,k3)        ! r2
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k3)        ! r0
         qb(:,:,k3)=qb(:,:,k1)    -t         ! q3
         qb(:,:,k1)=qb(:,:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb) 
         t    =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1      ! q11
         qb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1      ! r11
         rb(:,:,k1)=rb(:,:,k0)    -qb(:,:,k1)        ! r1
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k1)        ! r0
         qb(:,:,k1)=qb(:,:,k0)    -t         ! q1
         qb(:,:,k0)=qb(:,:,k0)    +t         ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,:,k2)*qf2+qb(:,:,k2) *rf2   ! r12
         qb(:,:,k2)= rb(:,:,k2)*rf2-qb(:,:,k2) *qf2   ! q12
         rb(:,:,k2)= rb(:,:,k1)*qf1+qb(:,:,k1) *rf1   ! q11
         rb(:,:,k1)= rb(:,:,k1)*rf1-qb(:,:,k1) *qf1   ! r11
         qb(:,:,k1)= rb(:,:,k2)    +t         ! q21
         rb(:,:,k2)=(rb(:,:,k2)    -t       )*qep   ! r22
         t    = rb(:,:,k1)    +qb(:,:,k2)        ! r21
         rb(:,:,k1)=(rb(:,:,k1)    -qb(:,:,k2))*qep   ! q22
         rb(:,:,k0)= rb(:,:,k0)    +t         ! r0
         qb(:,:,k0)= qb(:,:,k0)    +qb(:,:,k1)        ! q0
         t    = rb(:,:,k0)    -t          *rec   ! r21
         qb(:,:,k1)= qb(:,:,k0)    -qb(:,:,k1) *rec   ! q21
         qb(:,:,k2)= qb(:,:,k1)    -rb(:,:,k1)        ! q2
         qb(:,:,k1)= qb(:,:,k1)    +rb(:,:,k1)        ! q1
         rb(:,:,k1)= t            -rb(:,:,k2)        ! r1
         rb(:,:,k2)= t            +rb(:,:,k2)       ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1              ! q11
            rb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1              ! r11
            qb(:,:,k1)=rb(:,:,k4)*rf4-qb(:,:,k4)*qf4              ! q14
            rb(:,:,k4)=rb(:,:,k4)*qf4+qb(:,:,k4)*rf4              ! r14
            qb(:,:,k4)=rb(:,:,k1)    -qb(:,:,k1)       ! q24
            rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k1)             ! r21
            qb(:,:,k1)=t             +rb(:,:,k4)       ! q21
            rb(:,:,k4)=t             -rb(:,:,k4)       ! r24
            t       =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3              ! q13
            rb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3              ! r13
            qb(:,:,k3)=rb(:,:,k2)*qf2+qb(:,:,k2)*rf2              ! q12
            rb(:,:,k2)=rb(:,:,k2)*rf2-qb(:,:,k2)*qf2              ! r12
            qb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k3)       ! q22
            rb(:,:,k3)=qb(:,:,k3)    -rb(:,:,k3)       ! r23
            qb(:,:,k3)=rb(:,:,k2)    -t               ! q23
            rb(:,:,k2)=rb(:,:,k2)    +t               ! r22
            rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k1)    +rb(:,:,k2)     ! r0
            qb(:,:,k0)=qb(:,:,k0)    +qb(:,:,k1)    +qb(:,:,k2)     ! q0
            t       =rb(:,:,k4)*qze+rb(:,:,k3)*qet              ! r34
            rb(:,:,k3)=rb(:,:,k3)*qze-rb(:,:,k4)*qet              ! r33
            rb(:,:,k4)=rb(:,:,k0)    -rb(:,:,k2)*rzc-rb(:,:,k1)*rec ! r32
            rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k1)*rzc-rb(:,:,k2)*rec ! r31
            rb(:,:,k2)=rb(:,:,k4)    +rb(:,:,k3)       ! r2
            rb(:,:,k3)=rb(:,:,k4)    -rb(:,:,k3)       ! r3
            rb(:,:,k4)=rb(:,:,k1)    +t               ! r4
            rb(:,:,k1)=rb(:,:,k1)    -t               ! r1
            t       =qb(:,:,k0)    -qb(:,:,k1)*rzc-qb(:,:,k2)*rec ! q31
            qb(:,:,k2)=qb(:,:,k0)    -qb(:,:,k2)*rzc-qb(:,:,k1)*rec ! q32
            qb(:,:,k1)=qb(:,:,k3)*qze-qb(:,:,k4)*qet              ! q33
            qb(:,:,k4)=qb(:,:,k4)*qze+qb(:,:,k3)*qet              ! q34
            qb(:,:,k3)=qb(:,:,k2)    +qb(:,:,k1)       ! q3
            qb(:,:,k2)=qb(:,:,k2)    -qb(:,:,k1)       ! q2
            qb(:,:,k1)=t             +qb(:,:,k4)       ! q1
            qb(:,:,k4)=t             -qb(:,:,k4)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE szcfft 

!=============================================================================
SUBROUTINE dzcfft(nx,ny,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  DZCFFT: Double precision version of zcfft
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,ny,n
REAL(DP),DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx,ny) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,:,i); rb(:,:,i)=rb(:,:,j); rb(:,:,j)=t
      t=qb(:,:,i); qb(:,:,i)=qb(:,:,j); qb(:,:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:,:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3   ! q13
         qb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3   ! r13
         rb(:,:,k3)=rb(:,:,k2)*rf1-qb(:,:,k2)*qf1   ! r12
         rb(:,:,k2)=rb(:,:,k2)*qf1+qb(:,:,k2)*rf1   ! q12
         qb(:,:,k2)=qb(:,:,k3)    -rb(:,:,k2)       ! r23
         rb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k2)       ! q22
         qb(:,:,k3)=rb(:,:,k3)    +t              ! r22
         t    =rb(:,:,k3)    -t              ! q23
         rb(:,:,k3)=rb(:,:,k1)*rf2-qb(:,:,k1)*qf2   ! r11
         qb(:,:,k1)=rb(:,:,k1)*qf2+qb(:,:,k1)*rf2   ! q11
         rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k3)       ! r21
         rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k3)       ! r20
         rb(:,:,k3)=rb(:,:,k1)    -qb(:,:,k2)       ! r3
         rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k2)       ! r1
         qb(:,:,k2)=qb(:,:,k0)    +qb(:,:,k1)       ! q20
         qb(:,:,k1)=qb(:,:,k0)    -qb(:,:,k1)      ! q21
         qb(:,:,k0)=qb(:,:,k2)    +rb(:,:,k2)      ! q0
         qb(:,:,k2)=qb(:,:,k2)    -rb(:,:,k2)      ! q2
         rb(:,:,k2)=rb(:,:,k0)    -qb(:,:,k3)      ! r2
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k3)      ! r0
         qb(:,:,k3)=qb(:,:,k1)    -t       ! q3
         qb(:,:,k1)=qb(:,:,k1)    +t       ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1      ! q11
         qb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1      ! r11
         rb(:,:,k1)=rb(:,:,k0)    -qb(:,:,k1)        ! r1
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k1)        ! r0
         qb(:,:,k1)=qb(:,:,k0)    -t         ! q1
         qb(:,:,k0)=qb(:,:,k0)    +t         ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    = rb(:,:,k2)*qf2+qb(:,:,k2) *rf2     ! r12
         qb(:,:,k2)= rb(:,:,k2)*rf2-qb(:,:,k2) *qf2     ! q12
         rb(:,:,k2)= rb(:,:,k1)*qf1+qb(:,:,k1) *rf1     ! q11
         rb(:,:,k1)= rb(:,:,k1)*rf1-qb(:,:,k1) *qf1     ! r11
         qb(:,:,k1)= rb(:,:,k2)    +t           ! q21
         rb(:,:,k2)=(rb(:,:,k2)    -t       )*qep     ! r22
         t    = rb(:,:,k1)    +qb(:,:,k2)          ! r21
         rb(:,:,k1)=(rb(:,:,k1)    -qb(:,:,k2))*qep     ! q22
         rb(:,:,k0)= rb(:,:,k0)    +t           ! r0
         qb(:,:,k0)= qb(:,:,k0)    +qb(:,:,k1)          ! q0
         t    = rb(:,:,k0)    -t          *rec     ! r21
         qb(:,:,k1)= qb(:,:,k0)    -qb(:,:,k1) *rec     ! q21
         qb(:,:,k2)= qb(:,:,k1)    -rb(:,:,k1)          ! q2
         qb(:,:,k1)= qb(:,:,k1)    +rb(:,:,k1)          ! q1
         rb(:,:,k1)= t            -rb(:,:,k2)          ! r1
         rb(:,:,k2)= t            +rb(:,:,k2)         ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1                 ! q11
            rb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1                 ! r11
            qb(:,:,k1)=rb(:,:,k4)*rf4-qb(:,:,k4)*qf4                 ! q14
            rb(:,:,k4)=rb(:,:,k4)*qf4+qb(:,:,k4)*rf4                 ! r14
            qb(:,:,k4)=rb(:,:,k1)    -qb(:,:,k1)          ! q24
            rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k1)          ! r21
            qb(:,:,k1)=t             +rb(:,:,k4)          ! q21
            rb(:,:,k4)=t             -rb(:,:,k4)          ! r24
            t       =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3                 ! q13
            rb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3                 ! r13
            qb(:,:,k3)=rb(:,:,k2)*qf2+qb(:,:,k2)*rf2                 ! q12
            rb(:,:,k2)=rb(:,:,k2)*rf2-qb(:,:,k2)*qf2                 ! r12
            qb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k3)          ! q22
            rb(:,:,k3)=qb(:,:,k3)    -rb(:,:,k3)          ! r23
            qb(:,:,k3)=rb(:,:,k2)    -t           ! q23
            rb(:,:,k2)=rb(:,:,k2)    +t           ! r22
            rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k1)    +rb(:,:,k2)        ! r0
            qb(:,:,k0)=qb(:,:,k0)    +qb(:,:,k1)    +qb(:,:,k2)        ! q0
            t       =rb(:,:,k4)*qze+rb(:,:,k3)*qet                 ! r34
            rb(:,:,k3)=rb(:,:,k3)*qze-rb(:,:,k4)*qet                 ! r33
            rb(:,:,k4)=rb(:,:,k0)    -rb(:,:,k2)*rzc-rb(:,:,k1)*rec    ! r32
            rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k1)*rzc-rb(:,:,k2)*rec    ! r31
            rb(:,:,k2)=rb(:,:,k4)    +rb(:,:,k3)          ! r2
            rb(:,:,k3)=rb(:,:,k4)    -rb(:,:,k3)          ! r3
            rb(:,:,k4)=rb(:,:,k1)    +t           ! r4
            rb(:,:,k1)=rb(:,:,k1)    -t           ! r1
            t         =qb(:,:,k0)    -qb(:,:,k1)*rzc-qb(:,:,k2)*rec    ! q31
            qb(:,:,k2)=qb(:,:,k0)    -qb(:,:,k2)*rzc-qb(:,:,k1)*rec    ! q32
            qb(:,:,k1)=qb(:,:,k3)*qze-qb(:,:,k4)*qet                 ! q33
            qb(:,:,k4)=qb(:,:,k4)*qze+qb(:,:,k3)*qet                 ! q34
            qb(:,:,k3)=qb(:,:,k2)    +qb(:,:,k1)          ! q3
            qb(:,:,k2)=qb(:,:,k2)    -qb(:,:,k1)          ! q2
            qb(:,:,k1)=t             +qb(:,:,k4)          ! q1
            qb(:,:,k4)=t             -qb(:,:,k4)          ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dzcfft

!=============================================================================
SUBROUTINE szdfft(nx,ny,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  SZDFFT
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
! Z-array version of DFFT (fft along dimension of slowest-changing index)
!
! --> nx:   x-dimension of array
! --> ny:   y-dimension of array
! --> n:    period  and z-dimension of array
! <-> rb:   real part of transform (-->) and synthesized data (<--)
! <-> qb:   imaginary part of transform (-->) and synthesized data (<--)
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,ny,n
REAL(SP),DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
REAL(SP),DIMENSION(nx,ny) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._SP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,:,0)=rb(:,:,0)*rfac; rb(:,:,1:nm)=rb(:,:,nm:1:-1)*rfac
qb(:,:,0)=qb(:,:,0)*rfac; qb(:,:,1:nm)=qb(:,:,nm:1:-1)*rfac

!  PERMUTE THE DATA:,:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,:,i); rb(:,:,i)=rb(:,:,j); rb(:,:,j)=t
      t=qb(:,:,i); qb(:,:,i)=qb(:,:,j); qb(:,:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:,:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3     ! q13
         qb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3     ! r13
         rb(:,:,k3)=rb(:,:,k2)*rf1-qb(:,:,k2)*qf1     ! r12
         rb(:,:,k2)=rb(:,:,k2)*qf1+qb(:,:,k2)*rf1     ! q12
         qb(:,:,k2)=qb(:,:,k3)    -rb(:,:,k2)        ! r23
         rb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k2)        ! q22
         qb(:,:,k3)=rb(:,:,k3)    +t         ! r22
         t    =rb(:,:,k3)    -t         ! q23
         rb(:,:,k3)=rb(:,:,k1)*rf2-qb(:,:,k1)*qf2     ! r11
         qb(:,:,k1)=rb(:,:,k1)*qf2+qb(:,:,k1)*rf2     ! q11
         rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k3)        ! r21
         rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k3)        ! r20
         rb(:,:,k3)=rb(:,:,k1)    -qb(:,:,k2)        ! r3
         rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k2)        ! r1
         qb(:,:,k2)=qb(:,:,k0)    +qb(:,:,k1)        ! q20
         qb(:,:,k1)=qb(:,:,k0)    -qb(:,:,k1)        ! q21
         qb(:,:,k0)=qb(:,:,k2)    +rb(:,:,k2)        ! q0
         qb(:,:,k2)=qb(:,:,k2)    -rb(:,:,k2)        ! q2
         rb(:,:,k2)=rb(:,:,k0)    -qb(:,:,k3)        ! r2
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k3)        ! r0
         qb(:,:,k3)=qb(:,:,k1)    -t         ! q3
         qb(:,:,k1)=qb(:,:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1      ! q11
         qb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1      ! r11
         rb(:,:,k1)=rb(:,:,k0)    -qb(:,:,k1)        ! r1
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k1)        ! r0
         qb(:,:,k1)=qb(:,:,k0)    -t         ! q1
         qb(:,:,k0)=qb(:,:,k0)    +t         ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_SP; rec=1.5_SP; qep=.5_SP*SQRT(3._SP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t    =rb(:,:,k2)*qf2+qb(:,:,k2) *rf2     ! r12
         qb(:,:,k2)=rb(:,:,k2)*rf2-qb(:,:,k2) *qf2     ! q12
         rb(:,:,k2)=rb(:,:,k1)*qf1+qb(:,:,k1) *rf1     ! q11
         rb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1) *qf1     ! r11
         qb(:,:,k1)=rb(:,:,k2)    +t          ! q21
         rb(:,:,k2)=(rb(:,:,k2)   -t      )*qep     ! r22
         t    = rb(:,:,k1)   +qb(:,:,k2)         ! r21
         rb(:,:,k1)=(rb(:,:,k1)   -qb(:,:,k2))*qep     ! q22
         rb(:,:,k0)=rb(:,:,k0)    +t          ! r0
         qb(:,:,k0)=qb(:,:,k0)    +qb(:,:,k1)         ! q0
         t    =rb(:,:,k0)    -t          *rec     ! r21
         qb(:,:,k1)=qb(:,:,k0)    -qb(:,:,k1) *rec     ! q21
         qb(:,:,k2)=qb(:,:,k1)    -rb(:,:,k1)         ! q2
         qb(:,:,k1)=qb(:,:,k1)    +rb(:,:,k1)         ! q1
         rb(:,:,k1)=t           -rb(:,:,k2)          ! r1
         rb(:,:,k2)=t           +rb(:,:,k2)        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1              ! q11
            rb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1              ! r11
            qb(:,:,k1)=rb(:,:,k4)*rf4-qb(:,:,k4)*qf4              ! q14
            rb(:,:,k4)=rb(:,:,k4)*qf4+qb(:,:,k4)*rf4              ! r14
            qb(:,:,k4)=rb(:,:,k1)    -qb(:,:,k1)       ! q24
            rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k1)       ! r21
            qb(:,:,k1)=t             +rb(:,:,k4)       ! q21
            rb(:,:,k4)=t             -rb(:,:,k4)       ! r24
            t       =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3              ! q13
            rb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3              ! r13
            qb(:,:,k3)=rb(:,:,k2)*qf2+qb(:,:,k2)*rf2              ! q12
            rb(:,:,k2)=rb(:,:,k2)*rf2-qb(:,:,k2)*qf2              ! r12
            qb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k3)       ! q22
            rb(:,:,k3)=qb(:,:,k3)    -rb(:,:,k3)       ! r23
            qb(:,:,k3)=rb(:,:,k2)    -t                      ! q23
            rb(:,:,k2)=rb(:,:,k2)    +t               ! r22
            rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k1)    +rb(:,:,k2)     ! r0
            qb(:,:,k0)=qb(:,:,k0)    +qb(:,:,k1)    +qb(:,:,k2)     ! q0
            t       =rb(:,:,k4)*qze+rb(:,:,k3)*qet              ! r34
            rb(:,:,k3)=rb(:,:,k3)*qze-rb(:,:,k4)*qet              ! r33
            rb(:,:,k4)=rb(:,:,k0)    -rb(:,:,k2)*rzc-rb(:,:,k1)*rec ! r32
            rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k1)*rzc-rb(:,:,k2)*rec ! r31
            rb(:,:,k2)=rb(:,:,k4)    +rb(:,:,k3)       ! r2
            rb(:,:,k3)=rb(:,:,k4)    -rb(:,:,k3)       ! r3
            rb(:,:,k4)=rb(:,:,k1)    +t                      ! r4
            rb(:,:,k1)=rb(:,:,k1)    -t                      ! r1
            t       =qb(:,:,k0)    -qb(:,:,k1)*rzc-qb(:,:,k2)*rec ! q31
            qb(:,:,k2)=qb(:,:,k0)    -qb(:,:,k2)*rzc-qb(:,:,k1)*rec ! q32
            qb(:,:,k1)=qb(:,:,k3)*qze-qb(:,:,k4)*qet              ! q33
            qb(:,:,k4)=qb(:,:,k4)*qze+qb(:,:,k3)*qet              ! q34
            qb(:,:,k3)=qb(:,:,k2)    +qb(:,:,k1)       ! q3
            qb(:,:,k2)=qb(:,:,k2)    -qb(:,:,k1)       ! q2
            qb(:,:,k1)=t             +qb(:,:,k4)       ! q1
            qb(:,:,k4)=t             -qb(:,:,k4)       ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE szdfft

!=============================================================================
SUBROUTINE dzdfft(nx,ny,n,rb,qb) 
!=============================================================================
!                SUBROUTINE  DZDFFT: Double precision version of zdfft
!============================================================================= 
INTEGER,                        INTENT(IN   ):: nx,ny,n
REAL(DP),DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,rfac
INTEGER,     DIMENSION(0:n-1) :: jumble
REAL(DP),    DIMENSION(0:n-1) :: w
REAL(DP),    DIMENSION(nx,ny) :: t
!=============================================================================
nm=n-1; nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
rfac=1._DP/n

!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(:,:,0)=rb(:,:,0)*rfac; rb(:,:,1:nm)=rb(:,:,nm:1:-1)*rfac
qb(:,:,0)=qb(:,:,0)*rfac; qb(:,:,1:nm)=qb(:,:,nm:1:-1)*rfac

!  PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t=rb(:,:,i); rb(:,:,i)=rb(:,:,j); rb(:,:,j)=t
      t=qb(:,:,i); qb(:,:,i)=qb(:,:,j); qb(:,:,j)=t
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb;              jmb2=j*mb2
      rf1=w(jmb);            qf1=w(nh+jmb)
      rf2=w(jmb2);           qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2;   qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma
         t    =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3     ! q13
         qb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3     ! r13
         rb(:,:,k3)=rb(:,:,k2)*rf1-qb(:,:,k2)*qf1     ! r12
         rb(:,:,k2)=rb(:,:,k2)*qf1+qb(:,:,k2)*rf1     ! q12
         qb(:,:,k2)=qb(:,:,k3)    -rb(:,:,k2)        ! r23
         rb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k2)        ! q22
         qb(:,:,k3)=rb(:,:,k3)    +t         ! r22
         t         =rb(:,:,k3)    -t         ! q23
         rb(:,:,k3)=rb(:,:,k1)*rf2-qb(:,:,k1)*qf2     ! r11
         qb(:,:,k1)=rb(:,:,k1)*qf2+qb(:,:,k1)*rf2     ! q11
         rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k3)        ! r21
         rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k3)        ! r20
         rb(:,:,k3)=rb(:,:,k1)    -qb(:,:,k2)        ! r3
         rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k2)        ! r1
         qb(:,:,k2)=qb(:,:,k0)    +qb(:,:,k1)        ! q20
         qb(:,:,k1)=qb(:,:,k0)    -qb(:,:,k1)        ! q21
         qb(:,:,k0)=qb(:,:,k2)    +rb(:,:,k2)        ! q0
         qb(:,:,k2)=qb(:,:,k2)    -rb(:,:,k2)        ! q2
         rb(:,:,k2)=rb(:,:,k0)    -qb(:,:,k3)        ! r2
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k3)        ! r0
         qb(:,:,k3)=qb(:,:,k1)    -t         ! q3
         qb(:,:,k1)=qb(:,:,k1)    +t         ! q1
      ENDDO
   ENDDO
   ma=ma4
ENDDO
IF(ln2==1)THEN
!  RADIX 2
   mb=mb/2; ma2=ma*2
   DO j=0,ma-1
      jmb=j*mb
      DO i=0,nm,ma2
         k0=j+i;     k1=k0+ma
         rf1=w(jmb); qf1=w(nh+jmb)
         t    =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1      ! q11
         qb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1      ! r11
         rb(:,:,k1)=rb(:,:,k0)    -qb(:,:,k1)        ! r1
         rb(:,:,k0)=rb(:,:,k0)    +qb(:,:,k1)        ! r0
         qb(:,:,k1)=qb(:,:,k0)    -t         ! q1
         qb(:,:,k0)=qb(:,:,k0)    +t         ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF

!  RADIX 3
rep=-.5_DP; rec=1.5_DP; qep=.5_DP*SQRT(3._DP)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j; k1=k0+ma; k2=k1+ma
         t     = rb(:,:,k2)*qf2+qb(:,:,k2) *rf2   ! r12
         qb(:,:,k2)= rb(:,:,k2)*rf2-qb(:,:,k2) *qf2   ! q12
         rb(:,:,k2)= rb(:,:,k1)*qf1+qb(:,:,k1) *rf1   ! q11
         rb(:,:,k1)= rb(:,:,k1)*rf1-qb(:,:,k1) *qf1   ! r11
         qb(:,:,k1)= rb(:,:,k2)    +t         ! q21
         rb(:,:,k2)=(rb(:,:,k2)    -t       )*qep   ! r22
         t    = rb(:,:,k1)    +qb(:,:,k2)        ! r21
         rb(:,:,k1)=(rb(:,:,k1)    -qb(:,:,k2))*qep   ! q22
         rb(:,:,k0)= rb(:,:,k0)    +t         ! r0
         qb(:,:,k0)= qb(:,:,k0)    +qb(:,:,k1)        ! q0
         t    = rb(:,:,k0)    -t          *rec   ! r21
         qb(:,:,k1)= qb(:,:,k0)    -qb(:,:,k1) *rec   ! q21
         qb(:,:,k2)= qb(:,:,k1)    -rb(:,:,k1)        ! q2
         qb(:,:,k1)= qb(:,:,k1)    +rb(:,:,k1)        ! q1
         rb(:,:,k1)= t             -rb(:,:,k2)        ! r1
         rb(:,:,k2)= t            +rb(:,:,k2)       ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO
IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5; rze=w(nze); qze=w(nh+nze); rzc=1.-rze
   ret=rze*rze-qze*qze; qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;            jmb2=jmb*2
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j; k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma
            t       =rb(:,:,k1)*qf1+qb(:,:,k1)*rf1              ! q11
            rb(:,:,k1)=rb(:,:,k1)*rf1-qb(:,:,k1)*qf1              ! r11
            qb(:,:,k1)=rb(:,:,k4)*rf4-qb(:,:,k4)*qf4              ! q14
            rb(:,:,k4)=rb(:,:,k4)*qf4+qb(:,:,k4)*rf4              ! r14
            qb(:,:,k4)=rb(:,:,k1)    -qb(:,:,k1)              ! q24
            rb(:,:,k1)=rb(:,:,k1)    +qb(:,:,k1)              ! r21
            qb(:,:,k1)=t             +rb(:,:,k4)              ! q21
            rb(:,:,k4)=t             -rb(:,:,k4)              ! r24
            t       =rb(:,:,k3)*rf3-qb(:,:,k3)*qf3              ! q13
            rb(:,:,k3)=rb(:,:,k3)*qf3+qb(:,:,k3)*rf3              ! r13
            qb(:,:,k3)=rb(:,:,k2)*qf2+qb(:,:,k2)*rf2              ! q12
            rb(:,:,k2)=rb(:,:,k2)*rf2-qb(:,:,k2)*qf2              ! r12
            qb(:,:,k2)=qb(:,:,k3)    +rb(:,:,k3)              ! q22
            rb(:,:,k3)=qb(:,:,k3)    -rb(:,:,k3)              ! r23
            qb(:,:,k3)=rb(:,:,k2)    -t                      ! q23
            rb(:,:,k2)=rb(:,:,k2)    +t                      ! r22
            rb(:,:,k0)=rb(:,:,k0)    +rb(:,:,k1)    +rb(:,:,k2)     ! r0
            qb(:,:,k0)=qb(:,:,k0)    +qb(:,:,k1)    +qb(:,:,k2)     ! q0
            t       =rb(:,:,k4)*qze+rb(:,:,k3)*qet              ! r34
            rb(:,:,k3)=rb(:,:,k3)*qze-rb(:,:,k4)*qet              ! r33
            rb(:,:,k4)=rb(:,:,k0)    -rb(:,:,k2)*rzc-rb(:,:,k1)*rec ! r32
            rb(:,:,k1)=rb(:,:,k0)    -rb(:,:,k1)*rzc-rb(:,:,k2)*rec ! r31
            rb(:,:,k2)=rb(:,:,k4)    +rb(:,:,k3)              ! r2
            rb(:,:,k3)=rb(:,:,k4)    -rb(:,:,k3)              ! r3
            rb(:,:,k4)=rb(:,:,k1)    +t                      ! r4
            rb(:,:,k1)=rb(:,:,k1)    -t                      ! r1
            t       =qb(:,:,k0)    -qb(:,:,k1)*rzc-qb(:,:,k2)*rec ! q31
            qb(:,:,k2)=qb(:,:,k0)    -qb(:,:,k2)*rzc-qb(:,:,k1)*rec ! q32
            qb(:,:,k1)=qb(:,:,k3)*qze-qb(:,:,k4)*qet              ! q33
            qb(:,:,k4)=qb(:,:,k4)*qze+qb(:,:,k3)*qet              ! q34
            qb(:,:,k3)=qb(:,:,k2)    +qb(:,:,k1)              ! q3
            qb(:,:,k2)=qb(:,:,k2)    -qb(:,:,k1)              ! q2
            qb(:,:,k1)=t             +qb(:,:,k4)              ! q1
            qb(:,:,k4)=t             -qb(:,:,k4)              ! q4
  ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dzdfft 

!=============================================================================
SUBROUTINE sxrfft(n,ny,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SXRFFT
!   X-array version of RFFT
!
! --> N       x-dimension of data array and period in direction of transform
! --> ny      y-dimension of array
! <-> R       Data and transform
!=============================================================================
INTEGER,                      INTENT(IN   ):: n,ny
REAL(SP), DIMENSION(0:n-1,ny),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        ::nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(SP), DIMENSION(0:n-1,ny/2):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
DO iyh=1,nyh
   iy=iyh*2
   rb(0:nm,iyh)=r(0:nm,iy-1)*.5_SP; qb(0:nm,iyh)=r(0:nm,iy)*.5_SP
ENDDO
CALL xcfft(n,nyh,rb,qb)
r(0,1:nyh2-1:2)=rb(0,1:nyh)*2; r(0,2:nyh2:2)=qb(0,1:nyh)*2
IF(.NOT.oddx)THEN
   r(nh,1:nyh2-1:2)=rb(nh,1:nyh)*2; r(nh,2:nyh2:2)=qb(nh,1:nyh)*2
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   r(1:nmh,iym) = rb(1:nmh,iyh)   +rb(nm:nhp:-1,iyh)
   r(nhp:nm,iym)=-qb(nmh:1:-1,iyh)+qb(nhp:nm,iyh)
   r(1:nmh,iy)  = qb(1:nmh,iyh)   +qb(nm:nhp:-1,iyh)
   r(nhp:nm,iy) = rb(nmh:1:-1,iyh)-rb(nhp:nm,iyh)
ENDDO
IF(oddy)CALL rfft(n,r(:,ny))
END SUBROUTINE sxrfft 

!=============================================================================
SUBROUTINE dxrfft(n,ny,r) 
!=============================================================================
!      SUBROUTINES DXRFFT: Double precision version of xrfft
!=============================================================================
INTEGER,                      INTENT(IN   ):: n,ny
REAL(DP), DIMENSION(0:n-1,ny),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(DP), DIMENSION(0:n-1,ny/2):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
DO iyh=1,nyh
   iy=iyh*2
   rb(0:nm,iyh)=r(0:nm,iy-1)*.5_DP; qb(0:nm,iyh)=r(0:nm,iy)*.5_DP
ENDDO
CALL xcfft(n,nyh,rb,qb)
r(0,1:nyh2-1:2)=rb(0,1:nyh)*2; r(0,2:nyh2:2)=qb(0,1:nyh)*2
IF(.NOT.oddx)THEN
   r(nh,1:nyh2-1:2)=rb(nh,1:nyh)*2; r(nh,2:nyh2:2)=qb(nh,1:nyh)*2
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   r(1:nmh,iym) = rb(1:nmh,iyh)   +rb(nm:nhp:-1,iyh)
   r(nhp:nm,iym)=-qb(nmh:1:-1,iyh)+qb(nhp:nm,iyh)
   r(1:nmh,iy)  = qb(1:nmh,iyh)   +qb(nm:nhp:-1,iyh)
   r(nhp:nm,iy) = rb(nmh:1:-1,iyh)-rb(nhp:nm,iyh)
ENDDO
IF(oddy)CALL rfft(n,r(:,ny))
END SUBROUTINE dxrfft 

!=============================================================================
SUBROUTINE sxhfft(n,ny,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SXHFFT
!   X-array version of HFFT
!
! --> N       x-dimension of data array and period in direction of transform
! --> ny      y-dimension of array
! <-> R       Data and transform
!=============================================================================
INTEGER,                      INTENT(IN   ):: n,ny
REAL(SP), DIMENSION(0:n-1,ny),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(SP), DIMENSION(0:n-1,ny/2):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
rb(0,1:nyh)=r(0,1:nyh2-1:2); qb(0,1:nyh)=r(0,2:nyh2:2)
IF(.NOT.oddx)THEN
   rb(nh,1:nyh)=r(nh,1:nyh2-1:2); qb(nh,1:nyh)=r(nh,2:nyh2:2)
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   rb(1:nmh,iyh) =r(1:nmh,iym)   +r(nm:nhp:-1,iy)
   rb(nhp:nm,iyh)=r(nmh:1:-1,iym)-r(nhp:nm,iy)
   qb(1:nmh,iyh) =r(1:nmh,iy)    -r(nm:nhp:-1,iym)
   qb(nhp:nm,iyh)=r(nhp:nm,iym)  +r(nmh:1:-1,iy)
ENDDO
CALL xdfft(n,nyh,rb,qb)
DO iyh=1,nyh
   iy=iyh*2
   r(0:nm,iy-1)=rb(0:nm,iyh); r(0:nm,iy)=qb(0:nm,iyh)
ENDDO
IF(oddy)CALL hfft(n,r(:,ny))
END SUBROUTINE sxhfft 

!=============================================================================
SUBROUTINE dxhfft(n,ny,r) 
!=============================================================================
!      SUBROUTINES DXHFFT: Double precision version of xhfft
!=============================================================================
INTEGER,                      INTENT(IN   ):: n,ny
REAL(DP), DIMENSION(0:n-1,ny),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(DP), DIMENSION(0:n-1,ny/2):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
rb(0,1:nyh)=r(0,1:nyh2-1:2); qb(0,1:nyh)=r(0,2:nyh2:2)
IF(.NOT.oddx)THEN
   rb(nh,1:nyh)=r(nh,1:nyh2-1:2); qb(nh,1:nyh)=r(nh,2:nyh2:2)
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   rb(1:nmh,iyh) =r(1:nmh,iym)   +r(nm:nhp:-1,iy)
   rb(nhp:nm,iyh)=r(nmh:1:-1,iym)-r(nhp:nm,iy)
   qb(1:nmh,iyh) =r(1:nmh,iy)    -r(nm:nhp:-1,iym)
   qb(nhp:nm,iyh)=r(nhp:nm,iym)  +r(nmh:1:-1,iy)
ENDDO
CALL xdfft(n,nyh,rb,qb)
DO iyh=1,nyh
   iy=iyh*2
   r(0:nm,iy-1)=rb(0:nm,iyh); r(0:nm,iy)=qb(0:nm,iyh)
ENDDO
IF(oddy)CALL hfft(n,r(:,ny))
END SUBROUTINE dxhfft 

!=============================================================================
SUBROUTINE syrfft(nx,n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SYRFFT
!   Y-array version of RFFT
!
! --> nx      x-dimension of data array 
! --> n       y-dimension of array and period of Fourier transform
! <-> R       Data and transform
!=============================================================================
INTEGER,                      INTENT(IN   ):: nx,n
REAL(SP), DIMENSION(nx,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,0:n-1):: rb,qb
REAL(SP), DIMENSION(0:n-1):: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,0:nm)=r(ix-1,0:nm)*.5_SP; qb(ixh,0:nm)=r(ix,0:nm)*.5_SP
ENDDO
CALL ycfft(nxh,n,rb,qb)
r(1:nxh2-1:2,0)=rb(1:nxh,0)*2; r(2:nxh2:2,0)=qb(1:nxh,0)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,nh)=rb(1:nxh,nh)*2; r(2:nxh2:2,nh)=qb(1:nxh,nh)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,1:nmh) = rb(ixh,1:nmh)   +rb(ixh,nm:nhp:-1)
   r(ixm,nhp:nm)=-qb(ixh,nmh:1:-1)+qb(ixh,nhp:nm)
   r(ix,1:nmh)  = qb(ixh,1:nmh)   +qb(ixh,nm:nhp:-1)
   r(ix,nhp:nm) = rb(ixh,nmh:1:-1)-rb(ixh,nhp:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm)
   CALL rfft(n,t)
   r(nx,0:nm)=t
ENDIF
END SUBROUTINE syrfft 

!=============================================================================
SUBROUTINE dyrfft(nx,n,r) 
!=============================================================================
!      SUBROUTINES DYRFFT: Double precision version of yrfft
!=============================================================================
INTEGER,                      INTENT(IN   ):: nx,n
REAL(DP), DIMENSION(nx,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,0:n-1):: rb,qb
REAL(DP), DIMENSION(0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,0:nm)=r(ix-1,0:nm)*.5_DP; qb(ixh,0:nm)=r(ix,0:nm)*.5_DP
ENDDO
CALL ycfft(nxh,n,rb,qb)
r(1:nxh2-1:2,0)=rb(1:nxh,0)*2; r(2:nxh2:2,0)=qb(1:nxh,0)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,nh)=rb(1:nxh,nh)*2; r(2:nxh2:2,nh)=qb(1:nxh,nh)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,1:nmh) = rb(ixh,1:nmh)   +rb(ixh,nm:nhp:-1)
   r(ixm,nhp:nm)=-qb(ixh,nmh:1:-1)+qb(ixh,nhp:nm)
   r(ix,1:nmh)  = qb(ixh,1:nmh)   +qb(ixh,nm:nhp:-1)
   r(ix,nhp:nm) = rb(ixh,nmh:1:-1)-rb(ixh,nhp:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm)
   CALL rfft(n,t)
   r(nx,0:nm)=t
ENDIF
END SUBROUTINE dyrfft 

!=============================================================================
SUBROUTINE syhfft(nx,n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SYHFFT
!   Y-array version of HFFT
!
! --> nx      x-dimension of data
! --> n       y-dimension of data array and period in direction of transform
! <-> r       Data and transform
!=============================================================================
INTEGER,                      INTENT(IN   ):: nx,n
REAL(SP), DIMENSION(nx,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,0:n-1):: rb,qb
REAL(SP), DIMENSION(0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,0)=r(1:nxh2-1:2,0); qb(1:nxh,0)=r(2:nxh2:2,0)
IF(.NOT.oddy)THEN
   rb(1:nxh,nh)=r(1:nxh2-1:2,nh); qb(1:nxh,nh)=r(2:nxh2:2,nh)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,1:nmh) =r(ixm,1:nmh)   +r(ix,nm:nhp:-1)
   rb(ixh,nhp:nm)=r(ixm,nmh:1:-1)-r(ix,nhp:nm)
   qb(ixh,1:nmh) =r(ix,1:nmh)    -r(ixm,nm:nhp:-1)
   qb(ixh,nhp:nm)=r(ixm,nhp:nm)  +r(ix,nmh:1:-1)
ENDDO
CALL ydfft(nxh,n,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,0:nm)=rb(ixh,0:nm); r(ix,0:nm)=qb(ixh,0:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm)
   CALL hfft(n,t)
   r(nx,0:nm)=t
ENDIF
END SUBROUTINE syhfft 

!=============================================================================
SUBROUTINE dyhfft(nx,n,r) 
!=============================================================================
!      SUBROUTINES DYHFFT
!=============================================================================
INTEGER,                      INTENT(IN   ):: nx,n
REAL(DP), DIMENSION(nx,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                        :: oddx,oddy
INTEGER                        :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,0:n-1):: rb,qb
REAL(DP), DIMENSION(0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,0)=r(1:nxh2-1:2,0); qb(1:nxh,0)=r(2:nxh2:2,0)
IF(.NOT.oddy)THEN
   rb(1:nxh,nh)=r(1:nxh2-1:2,nh); qb(1:nxh,nh)=r(2:nxh2:2,nh)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,1:nmh) =r(ixm,1:nmh)   +r(ix,nm:nhp:-1)
   rb(ixh,nhp:nm)=r(ixm,nmh:1:-1)-r(ix,nhp:nm)
   qb(ixh,1:nmh) =r(ix,1:nmh)    -r(ixm,nm:nhp:-1)
   qb(ixh,nhp:nm)=r(ixm,nhp:nm)  +r(ix,nmh:1:-1)
ENDDO
CALL ydfft(nxh,n,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,0:nm)=rb(ixh,0:nm); r(ix,0:nm)=qb(ixh,0:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm)
   CALL hfft(n,t)
   r(nx,0:nm)=t
ENDIF
END SUBROUTINE dyhfft 

!=============================================================================
SUBROUTINE sx3rfft(n,ny,nz,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SX3RFFT
!   X-array version of RFFT in 3D
!
! --> N       x-dimension of data array and period in direction of transform
! --> ny      y-dimension of array
! --> nz
! <-> R       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: n,ny,nz
REAL(SP), DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           ::nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(SP), DIMENSION(0:n-1,ny/2,nz):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
DO iyh=1,nyh
   iy=iyh*2
   rb(0:nm,iyh,:)=r(0:nm,iy-1,:)*.5_SP; qb(0:nm,iyh,:)=r(0:nm,iy,:)*.5_SP
ENDDO
CALL xcfft(n,nyh,nz,rb,qb)
r(0,1:nyh2-1:2,:)=rb(0,1:nyh,:)*2; r(0,2:nyh2:2,:)=qb(0,1:nyh,:)*2
IF(.NOT.oddx)THEN
   r(nh,1:nyh2-1:2,:)=rb(nh,1:nyh,:)*2; r(nh,2:nyh2:2,:)=qb(nh,1:nyh,:)*2
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   r(1:nmh,iym,:) = rb(1:nmh,iyh,:)   +rb(nm:nhp:-1,iyh,:)
   r(nhp:nm,iym,:)=-qb(nmh:1:-1,iyh,:)+qb(nhp:nm,iyh,:)
   r(1:nmh,iy,:)  = qb(1:nmh,iyh,:)   +qb(nm:nhp:-1,iyh,:)
   r(nhp:nm,iy,:) = rb(nmh:1:-1,iyh,:)-rb(nhp:nm,iyh,:)
ENDDO
IF(oddy)CALL xrfft(n,nz,r(:,ny,:))
END SUBROUTINE sx3rfft 

!=============================================================================
SUBROUTINE dx3rfft(n,ny,nz,r) 
!=============================================================================
!   SUBROUTINES DX3RFFT: Double precision version of xrfft in 3D
!=============================================================================
INTEGER,                         INTENT(IN   ):: n,ny,nz
REAL(DP), DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(DP), DIMENSION(0:n-1,ny/2,nz):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
DO iyh=1,nyh
   iy=iyh*2
   rb(0:nm,iyh,:)=r(0:nm,iy-1,:)*.5_DP; qb(0:nm,iyh,:)=r(0:nm,iy,:)*.5_DP
ENDDO
CALL xcfft(n,nyh,nz,rb,qb)
r(0,1:nyh2-1:2,:)=rb(0,1:nyh,:)*2; r(0,2:nyh2:2,:)=qb(0,1:nyh,:)*2
IF(.NOT.oddx)THEN
   r(nh,1:nyh2-1:2,:)=rb(nh,1:nyh,:)*2; r(nh,2:nyh2:2,:)=qb(nh,1:nyh,:)*2
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   r(1:nmh,iym,:) = rb(1:nmh,iyh,:)   +rb(nm:nhp:-1,iyh,:)
   r(nhp:nm,iym,:)=-qb(nmh:1:-1,iyh,:)+qb(nhp:nm,iyh,:)
   r(1:nmh,iy,:)  = qb(1:nmh,iyh,:)   +qb(nm:nhp:-1,iyh,:)
   r(nhp:nm,iy,:) = rb(nmh:1:-1,iyh,:)-rb(nhp:nm,iyh,:)
ENDDO
IF(oddy)CALL xrfft(n,nz,r(:,ny,:))
END SUBROUTINE dx3rfft 

!=============================================================================
SUBROUTINE sx3hfft(n,ny,nz,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SX3HFFT
!   X-array version of HFFT in 3D
!
! --> N       x-dimension of data array and period in direction of transform
! --> ny      y-dimension of array
! --> nz      z-dimension of array
! <-> R       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: n,ny,nz
REAL(SP), DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(SP), DIMENSION(0:n-1,ny/2,nz):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
rb(0,1:nyh,:)=r(0,1:nyh2-1:2,:); qb(0,1:nyh,:)=r(0,2:nyh2:2,:)
IF(.NOT.oddx)THEN
   rb(nh,1:nyh,:)=r(nh,1:nyh2-1:2,:); qb(nh,1:nyh,:)=r(nh,2:nyh2:2,:)
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   rb(1:nmh,iyh,:) =r(1:nmh,iym,:)   +r(nm:nhp:-1,iy,:)
   rb(nhp:nm,iyh,:)=r(nmh:1:-1,iym,:)-r(nhp:nm,iy,:)
   qb(1:nmh,iyh,:) =r(1:nmh,iy,:)    -r(nm:nhp:-1,iym,:)
   qb(nhp:nm,iyh,:)=r(nhp:nm,iym,:)  +r(nmh:1:-1,iy,:)
ENDDO
CALL xdfft(n,nyh,nz,rb,qb)
DO iyh=1,nyh
   iy=iyh*2
   r(0:nm,iy-1,:)=rb(0:nm,iyh,:); r(0:nm,iy,:)=qb(0:nm,iyh,:)
ENDDO
IF(oddy)CALL xhfft(n,nz,r(:,ny,:))
END SUBROUTINE sx3hfft 

!=============================================================================
SUBROUTINE dx3hfft(n,ny,nz,r) 
!=============================================================================
! SUBROUTINES DX3HFFT: Double precision version of xhfft in 3D
!=============================================================================
INTEGER,                         INTENT(IN   ):: n,ny,nz
REAL(DP), DIMENSION(0:n-1,ny,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nyh,nyh2,i,iyh,iy,iym
REAL(DP), DIMENSION(0:n-1,ny/2,nz):: rb,qb
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddx=(nh == nmh)  
nyh=ny/2; nyh2=nyh*2; oddy=(nyh2 /= ny)
rb(0,1:nyh,:)=r(0,1:nyh2-1:2,:); qb(0,1:nyh,:)=r(0,2:nyh2:2,:)
IF(.NOT.oddx)THEN
   rb(nh,1:nyh,:)=r(nh,1:nyh2-1:2,:); qb(nh,1:nyh,:)=r(nh,2:nyh2:2,:)
ENDIF
DO iyh=1,nyh
   iy=iyh*2; iym=iy-1
   rb(1:nmh,iyh,:) =r(1:nmh,iym,:)   +r(nm:nhp:-1,iy,:)
   rb(nhp:nm,iyh,:)=r(nmh:1:-1,iym,:)-r(nhp:nm,iy,:)
   qb(1:nmh,iyh,:) =r(1:nmh,iy,:)    -r(nm:nhp:-1,iym,:)
   qb(nhp:nm,iyh,:)=r(nhp:nm,iym,:)  +r(nmh:1:-1,iy,:)
ENDDO
CALL xdfft(n,nyh,nz,rb,qb)
DO iyh=1,nyh
   iy=iyh*2
   r(0:nm,iy-1,:)=rb(0:nm,iyh,:); r(0:nm,iy,:)=qb(0:nm,iyh,:)
ENDDO
IF(oddy)CALL xhfft(n,nz,r(:,ny,:))
END SUBROUTINE dx3hfft 

!=============================================================================
SUBROUTINE sy3rfft(nx,n,nz,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SY3RFFT
!   Y-array version of RFFT in 3D
!
! --> nx      x-dimension of data array 
! --> n       y-dimension of array and period of Fourier transform
! --> nz      z-dimension of array
! <-> R       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,n,nz
REAL(SP), DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,0:n-1,nz):: rb,qb
REAL(SP), DIMENSION(0:n-1,nz)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,0:nm,:)=r(ix-1,0:nm,:)*.5_SP; qb(ixh,0:nm,:)=r(ix,0:nm,:)*.5_SP
ENDDO
CALL ycfft(nxh,n,nz,rb,qb)
r(1:nxh2-1:2,0,:)=rb(1:nxh,0,:)*2; r(2:nxh2:2,0,:)=qb(1:nxh,0,:)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,nh,:)=rb(1:nxh,nh,:)*2; r(2:nxh2:2,nh,:)=qb(1:nxh,nh,:)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,1:nmh,:) = rb(ixh,1:nmh,:)   +rb(ixh,nm:nhp:-1,:)
   r(ixm,nhp:nm,:)=-qb(ixh,nmh:1:-1,:)+qb(ixh,nhp:nm,:)
   r(ix,1:nmh,:)  = qb(ixh,1:nmh,:)   +qb(ixh,nm:nhp:-1,:)
   r(ix,nhp:nm,:) = rb(ixh,nmh:1:-1,:)-rb(ixh,nhp:nm,:)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm,:)
   CALL xrfft(n,nz,t)
   r(nx,0:nm,:)=t
ENDIF
END SUBROUTINE sy3rfft 

!=============================================================================
SUBROUTINE dy3rfft(nx,n,nz,r) 
!=============================================================================
! SUBROUTINES DY3RFFT: Double precision version of yrfft in 3D
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,n,nz
REAL(DP), DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,0:n-1,nz):: rb,qb
REAL(DP), DIMENSION(0:n-1,nz)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,0:nm,:)=r(ix-1,0:nm,:)*.5_DP; qb(ixh,0:nm,:)=r(ix,0:nm,:)*.5_DP
ENDDO
CALL ycfft(nxh,n,nz,rb,qb)
r(1:nxh2-1:2,0,:)=rb(1:nxh,0,:)*2; r(2:nxh2:2,0,:)=qb(1:nxh,0,:)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,nh,:)=rb(1:nxh,nh,:)*2; r(2:nxh2:2,nh,:)=qb(1:nxh,nh,:)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,1:nmh,:) = rb(ixh,1:nmh,:)   +rb(ixh,nm:nhp:-1,:)
   r(ixm,nhp:nm,:)=-qb(ixh,nmh:1:-1,:)+qb(ixh,nhp:nm,:)
   r(ix,1:nmh,:)  = qb(ixh,1:nmh,:)   +qb(ixh,nm:nhp:-1,:)
   r(ix,nhp:nm,:) = rb(ixh,nmh:1:-1,:)-rb(ixh,nhp:nm,:)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm,:)
   CALL xrfft(n,nz,t)
   r(nx,0:nm,:)=t
ENDIF
END SUBROUTINE dy3rfft 

!=============================================================================
SUBROUTINE sy3hfft(nx,n,nz,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SY3HFFT
!   Y-array version of HFFT in 3D
!
! --> nx      x-dimension of data
! --> n       y-dimension of data array and period in direction of transform
! --> nx      z-dimension of data array
! <-> r       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,n,nz
REAL(SP), DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,0:n-1,nz):: rb,qb
REAL(SP), DIMENSION(0:n-1,nz)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,0,:)=r(1:nxh2-1:2,0,:); qb(1:nxh,0,:)=r(2:nxh2:2,0,:)
IF(.NOT.oddy)THEN
   rb(1:nxh,nh,:)=r(1:nxh2-1:2,nh,:); qb(1:nxh,nh,:)=r(2:nxh2:2,nh,:)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,1:nmh,:) =r(ixm,1:nmh,:)   +r(ix,nm:nhp:-1,:)
   rb(ixh,nhp:nm,:)=r(ixm,nmh:1:-1,:)-r(ix,nhp:nm,:)
   qb(ixh,1:nmh,:) =r(ix,1:nmh,:)    -r(ixm,nm:nhp:-1,:)
   qb(ixh,nhp:nm,:)=r(ixm,nhp:nm,:)  +r(ix,nmh:1:-1,:)
ENDDO
CALL ydfft(nxh,n,nz,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,0:nm,:)=rb(ixh,0:nm,:); r(ix,0:nm,:)=qb(ixh,0:nm,:)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm,:)
   CALL xhfft(n,nz,t)
   r(nx,0:nm,:)=t
ENDIF
END SUBROUTINE sy3hfft 

!=============================================================================
SUBROUTINE dy3hfft(nx,n,nz,r) 
!=============================================================================
!      SUBROUTINES DY3HFFT
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,n,nz
REAL(DP), DIMENSION(nx,0:n-1,nz),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,0:n-1,nz):: rb,qb
REAL(DP), DIMENSION(0:n-1,nz)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,0,:)=r(1:nxh2-1:2,0,:); qb(1:nxh,0,:)=r(2:nxh2:2,0,:)
IF(.NOT.oddy)THEN
   rb(1:nxh,nh,:)=r(1:nxh2-1:2,nh,:); qb(1:nxh,nh,:)=r(2:nxh2:2,nh,:)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,1:nmh,:) =r(ixm,1:nmh,:)   +r(ix,nm:nhp:-1,:)
   rb(ixh,nhp:nm,:)=r(ixm,nmh:1:-1,:)-r(ix,nhp:nm,:)
   qb(ixh,1:nmh,:) =r(ix,1:nmh,:)    -r(ixm,nm:nhp:-1,:)
   qb(ixh,nhp:nm,:)=r(ixm,nhp:nm,:)  +r(ix,nmh:1:-1,:)
ENDDO
CALL ydfft(nxh,n,nz,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,0:nm,:)=rb(ixh,0:nm,:); r(ix,0:nm,:)=qb(ixh,0:nm,:)
ENDDO
IF(oddx)THEN
   t=r(nx,0:nm,:)
   CALL xhfft(n,nz,t)
   r(nx,0:nm,:)=t
ENDIF
END SUBROUTINE dy3hfft 

!=============================================================================
SUBROUTINE szrfft(nx,ny,n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SZRFFT
!   Y-array version of RFFT
!
! --> nx      x-dimension of data array 
! --> ny      y-dimension of data array
! --> n       z-dimension of array and period of Fourier transform
! <-> R       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,ny,n
REAL(SP), DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,ny,0:n-1):: rb,qb
REAL(SP), DIMENSION(ny,0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,:,0:nm)=r(ix-1,:,0:nm)*.5_SP; qb(ixh,:,0:nm)=r(ix,:,0:nm)*.5_SP
ENDDO
CALL zcfft(nxh,ny,n,rb,qb)
r(1:nxh2-1:2,:,0)=rb(1:nxh,:,0)*2; r(2:nxh2:2,:,0)=qb(1:nxh,:,0)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,:,nh)=rb(1:nxh,:,nh)*2; r(2:nxh2:2,:,nh)=qb(1:nxh,:,nh)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,:,1:nmh) = rb(ixh,:,1:nmh)   +rb(ixh,:,nm:nhp:-1)
   r(ixm,:,nhp:nm)=-qb(ixh,:,nmh:1:-1)+qb(ixh,:,nhp:nm)
   r(ix,:,1:nmh)  = qb(ixh,:,1:nmh)   +qb(ixh,:,nm:nhp:-1)
   r(ix,:,nhp:nm) = rb(ixh,:,nmh:1:-1)-rb(ixh,:,nhp:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,:,0:nm)
   CALL yrfft(ny,n,t)
   r(nx,:,0:nm)=t
ENDIF
END SUBROUTINE szrfft 

!=============================================================================
SUBROUTINE dzrfft(nx,ny,n,r) 
!=============================================================================
!      SUBROUTINES DZRFFT: Double precision version of yrfft
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,ny,n
REAL(DP), DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,ny,0:n-1):: rb,qb
REAL(DP), DIMENSION(ny,0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
DO ixh=1,nxh
   ix=ixh*2
   rb(ixh,:,0:nm)=r(ix-1,:,0:nm)*.5_DP; qb(ixh,:,0:nm)=r(ix,:,0:nm)*.5_DP
ENDDO
CALL zcfft(nxh,ny,n,rb,qb)
r(1:nxh2-1:2,:,0)=rb(1:nxh,:,0)*2; r(2:nxh2:2,:,0)=qb(1:nxh,:,0)*2
IF(.NOT.oddy)THEN
   r(1:nxh2:2,:,nh)=rb(1:nxh,:,nh)*2; r(2:nxh2:2,:,nh)=qb(1:nxh,:,nh)*2
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   r(ixm,:,1:nmh) = rb(ixh,:,1:nmh)   +rb(ixh,:,nm:nhp:-1)
   r(ixm,:,nhp:nm)=-qb(ixh,:,nmh:1:-1)+qb(ixh,:,nhp:nm)
   r(ix,:,1:nmh)  = qb(ixh,:,1:nmh)   +qb(ixh,:,nm:nhp:-1)
   r(ix,:,nhp:nm) = rb(ixh,:,nmh:1:-1)-rb(ixh,:,nhp:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,:,0:nm)
   CALL yrfft(ny,n,t)
   r(nx,:,0:nm)=t
ENDIF
END SUBROUTINE dzrfft 

!=============================================================================
SUBROUTINE szhfft(nx,ny,n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!      SUBROUTINES SZHFFT
!   Y-array version of HFFT
!
! --> nx      x-dimension of data
! --> ny      y-dimension of data
! --> n       z-dimension of data array and period in direction of transform
! <-> r       Data and transform
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,ny,n
REAL(SP), DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(SP), DIMENSION(nx/2,ny,0:n-1):: rb,qb
REAL(SP), DIMENSION(ny,0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,:,0)=r(1:nxh2-1:2,:,0); qb(1:nxh,:,0)=r(2:nxh2:2,:,0)
IF(.NOT.oddy)THEN
   rb(1:nxh,:,nh)=r(1:nxh2-1:2,:,nh); qb(1:nxh,:,nh)=r(2:nxh2:2,:,nh)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,:,1:nmh) =r(ixm,:,1:nmh)   +r(ix,:,nm:nhp:-1)
   rb(ixh,:,nhp:nm)=r(ixm,:,nmh:1:-1)-r(ix,:,nhp:nm)
   qb(ixh,:,1:nmh) =r(ix,:,1:nmh)    -r(ixm,:,nm:nhp:-1)
   qb(ixh,:,nhp:nm)=r(ixm,:,nhp:nm)  +r(ix,:,nmh:1:-1)
ENDDO
CALL zdfft(nxh,ny,n,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,:,0:nm)=rb(ixh,:,0:nm); r(ix,:,0:nm)=qb(ixh,:,0:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,:,0:nm)
   CALL yhfft(ny,n,t)
   r(nx,:,0:nm)=t
ENDIF
END SUBROUTINE szhfft 

!=============================================================================
SUBROUTINE dzhfft(nx,ny,n,r) 
!=============================================================================
!      SUBROUTINES DZHFFT
!=============================================================================
INTEGER,                         INTENT(IN   ):: nx,ny,n
REAL(DP), DIMENSION(nx,ny,0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
LOGICAL                           :: oddx,oddy
INTEGER                           :: nm,nh,nmh,nhp,nxh,nxh2,i,ixh,ix,ixm
REAL(DP), DIMENSION(nx/2,ny,0:n-1):: rb,qb
REAL(DP), DIMENSION(ny,0:n-1)     :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1; oddy=(nh == nmh)  
nxh=nx/2; nxh2=nxh*2; oddx=(nxh2 /= nx)
rb(1:nxh,:,0)=r(1:nxh2-1:2,:,0); qb(1:nxh,:,0)=r(2:nxh2:2,:,0)
IF(.NOT.oddy)THEN
   rb(1:nxh,:,nh)=r(1:nxh2-1:2,:,nh); qb(1:nxh,:,nh)=r(2:nxh2:2,:,nh)
ENDIF
DO ixh=1,nxh
   ix=ixh*2; ixm=ix-1
   rb(ixh,:,1:nmh) =r(ixm,:,1:nmh)   +r(ix,:,nm:nhp:-1)
   rb(ixh,:,nhp:nm)=r(ixm,:,nmh:1:-1)-r(ix,:,nhp:nm)
   qb(ixh,:,1:nmh) =r(ix,:,1:nmh)    -r(ixm,:,nm:nhp:-1)
   qb(ixh,:,nhp:nm)=r(ixm,:,nhp:nm)  +r(ix,:,nmh:1:-1)
ENDDO
CALL zdfft(nxh,ny,n,rb,qb)
DO ixh=1,nxh
   ix=ixh*2
   r(ix-1,:,0:nm)=rb(ixh,:,0:nm); r(ix,:,0:nm)=qb(ixh,:,0:nm)
ENDDO
IF(oddx)THEN
   t=r(nx,:,0:nm)
   CALL yhfft(ny,n,t)
   r(nx,:,0:nm)=t
ENDIF
END SUBROUTINE dzhfft 

!=============================================================================
SUBROUTINE sxyrfft(nx,ny,a)
!=============================================================================
!   SUBROUTINE sxyrfft
!  Double Fourier transform of two-dimensional real data. Storage convention
! for the transformed data is now defined. Let k and l be x and y
! wavenumber indices and define zones K0,K1,K2,K3 of k such that:
!   K0={k=0}, K1={k>0 and k<nx/2.}, K2={k=nx/2.}, K3={k>nx/2.} and
! similarly define L0,L1,L2,L3 for zones of l. Note that K2 is null for 
! nx an odd number; likewise L2 is null for odd ny. In the table below, we
! show for each Cartesian product zone, whether the output field, a, is the
! real part (R) or the imaginary part, (I), of the corresponding complex
! transform.  
!                 L3| I  I  I  I
!                 L2| R  R  R  I
!                 L1| R  R  R  R   <--- N.B.
!                 L0| R  R  R  I
!                    ------------
!                    K0 K1 K2 K3
!
! Usual symmetries then apply with conjugacy pairings, (K1<-->K3), (L1<-->L3)
! and other zones being self-conjugate. 
!
!=============================================================================
INTEGER,                          INTENT(IN   ):: nx,ny
REAL(SP),DIMENSION(0:nx-1,0:ny-1),INTENT(INOUT):: a
!-----------------------------------------------------------------------------
INTEGER:: nxh,nxhp,nxhm,nxm,nyh,nyhp,nyhm,nym
REAL(SP),DIMENSION(nx-1-nx/2,ny-1-ny/2):: t
!=============================================================================
nxh=nx/2; nxhp=nxh+1; nxhm=nx-nxhp; nxm=nx-1
nyh=ny/2; nyhp=nyh+1; nyhm=ny-nyhp; nym=ny-1
CALL xrfft(nx,ny,a); CALL yrfft(nx,ny,a)
t                         =a(1:nxhm,1:nyhm)     +a(nxm:nxhp:-1,nym:nyhp:-1)
a(1:nxhm,1:nyhm)          =a(1:nxhm,1:nyhm)     -a(nxm:nxhp:-1,nym:nyhp:-1)
a(nxm:nxhp:-1,nym:nyhp:-1)=a(1:nxhm,nym:nyhp:-1)+a(nxm:nxhp:-1,1:nyhm)
a(1:nxhm,nym:nyhp:-1)     =a(1:nxhm,nym:nyhp:-1)-a(nxm:nxhp:-1,1:nyhm)
a(nxm:nxhp:-1,1:nyhm)     =t
END SUBROUTINE sxyrfft

!=============================================================================
SUBROUTINE dxyrfft(nx,ny,a)
!=============================================================================
!    SUBROUTINE DXYRFFT     
! Double precision version of xyrfft
!=============================================================================
INTEGER,                          INTENT(IN   ):: nx,ny
REAL(DP),DIMENSION(0:nx-1,0:ny-1),INTENT(INOUT):: a
!-----------------------------------------------------------------------------
INTEGER:: nxh,nxhp,nxhm,nxm,nyh,nyhp,nyhm,nym
REAL(DP),DIMENSION(nx-1-nx/2,ny-1-ny/2):: t
!=============================================================================
nxh=nx/2; nxhp=nxh+1; nxhm=nx-nxhp; nxm=nx-1
nyh=ny/2; nyhp=nyh+1; nyhm=ny-nyhp; nym=ny-1
CALL xrfft(nx,ny,a); CALL yrfft(nx,ny,a)
t                         =a(1:nxhm,1:nyhm)     +a(nxm:nxhp:-1,nym:nyhp:-1)
a(1:nxhm,1:nyhm)          =a(1:nxhm,1:nyhm)     -a(nxm:nxhp:-1,nym:nyhp:-1)
a(nxm:nxhp:-1,nym:nyhp:-1)=a(1:nxhm,nym:nyhp:-1)+a(nxm:nxhp:-1,1:nyhm)
a(1:nxhm,nym:nyhp:-1)     =a(1:nxhm,nym:nyhp:-1)-a(nxm:nxhp:-1,1:nyhm)
a(nxm:nxhp:-1,1:nyhm)     =t
END SUBROUTINE dxyrfft

!=============================================================================
SUBROUTINE sxyhfft(nx,ny,a)
!=============================================================================
!   SUBROUTINE sxyhfft
! Inverse operator to xyrfft
!=============================================================================
INTEGER,                          INTENT(IN   ):: nx,ny
REAL(SP),DIMENSION(0:nx-1,0:ny-1),INTENT(INOUT):: a
!-----------------------------------------------------------------------------
INTEGER:: nxh,nxhp,nxhm,nxm,nyh,nyhp,nyhm,nym
REAL(SP),DIMENSION(nx-1-nx/2,ny-1-ny/2):: t
!=============================================================================
nxh=nx/2; nxhp=nxh+1; nxhm=nx-nxhp; nxm=nx-1
nyh=ny/2; nyhp=nyh+1; nyhm=ny-nyhp; nym=ny-1
t                    =.5_SP*(a(nxm:nxhp:-1,1:nyhm)-a(1:nxhm,1:nyhm))
a(1:nxhm,1:nyhm)     =t+a(1:nxhm,1:nyhm)
a(nxm:nxhp:-1,1:nyhm)=.5_SP*(a(nxm:nxhp:-1,nym:nyhp:-1)-a(1:nxhm,nym:nyhp:-1))
a(1:nxhm,nym:nyhp:-1)=a(nxm:nxhp:-1,1:nyhm)+a(1:nxhm,nym:nyhp:-1)
a(nxm:nxhp:-1,nym:nyhp:-1)=t
CALL yhfft(nx,ny,a); CALL xhfft(nx,ny,a)
END SUBROUTINE sxyhfft

!=============================================================================
SUBROUTINE dxyhfft(nx,ny,a)
!=============================================================================
!   SUBROUTINE DXYHFFT
! Double precision version of xyhfft
!=============================================================================
INTEGER,                          INTENT(IN   ):: nx,ny
REAL(DP),DIMENSION(0:nx-1,0:ny-1),INTENT(INOUT):: a
!-----------------------------------------------------------------------------
INTEGER:: nxh,nxhp,nxhm,nxm,nyh,nyhp,nyhm,nym
REAL(DP),DIMENSION(nx-1-nx/2,ny-1-ny/2):: t
!=============================================================================
nxh=nx/2; nxhp=nxh+1; nxhm=nx-nxhp; nxm=nx-1
nyh=ny/2; nyhp=nyh+1; nyhm=ny-nyhp; nym=ny-1
t                    =.5_DP*(a(nxm:nxhp:-1,1:nyhm)-a(1:nxhm,1:nyhm))
a(1:nxhm,1:nyhm)     =t+a(1:nxhm,1:nyhm)
a(nxm:nxhp:-1,1:nyhm)=.5_DP*(a(nxm:nxhp:-1,nym:nyhp:-1)-a(1:nxhm,nym:nyhp:-1))
a(1:nxhm,nym:nyhp:-1)=a(nxm:nxhp:-1,1:nyhm)+a(1:nxhm,nym:nyhp:-1)
a(nxm:nxhp:-1,nym:nyhp:-1)=t
CALL yhfft(nx,ny,a); CALL xhfft(nx,ny,a)
END SUBROUTINE dxyhfft

end module pfft2
