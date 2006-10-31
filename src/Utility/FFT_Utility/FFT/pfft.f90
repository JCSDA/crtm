!                                                         ******************
!                                                         *   pfft.f90     *
!                                                         *  Purser 1994   *
!                                                         ******************
! Latest revisions: April 2003
! 
! NOAA/NCEP/Environmental Modeling Center.              jim.purser@noaa.gov
! Suite of Fast Fourier Transform routines based on factors 2, 3, 5.
! Data assumed from 0 to n-1 and transforms performed in place.
! For complex transforms of real data, real components of wavenumber k
! are found at positions k [0,..,n/2], imag components of wavenumber -k
! are found at positions n-k [n/2+1,..n-1]. 
!=============================================================================
MODULE fft2345 
!=============================================================================
IMPLICIT NONE
INTEGER ln2,ln3,ln4,ln5,ln,n,nm,nh
END MODULE fft2345 

!=============================================================================
MODULE pfft
!=============================================================================
use pkind
implicit none
private
public :: cfft,dfft,csft,dsft,dsfe,hsfe,rfft,hfft

INTERFACE snfftln; MODULE PROCEDURE infftln;                     END INTERFACE
INTERFACE sget235; MODULE PROCEDURE iget235;                     END INTERFACE
INTERFACE get2345; MODULE PROCEDURE iget2345;                    END INTERFACE
INTERFACE rumble;  MODULE PROCEDURE srumble,drumble;             END INTERFACE
INTERFACE fftco;   MODULE PROCEDURE sfftco, dfftco;              END INTERFACE
INTERFACE cfft;    MODULE PROCEDURE scfft,  dcfft;               END INTERFACE
INTERFACE dfft;    MODULE PROCEDURE sdfft,  ddfft;               END INTERFACE
INTERFACE csft;    MODULE PROCEDURE scsft,  dcsft;               END INTERFACE
INTERFACE dsft;    MODULE PROCEDURE sdsft,  ddsft;               END INTERFACE
INTERFACE dsfe;    MODULE PROCEDURE sdsfe,  ddsfe;               END INTERFACE
interface hsfe;    module procedure shsfe,  dhsfe;               end interface
INTERFACE rfft;    MODULE PROCEDURE srfft,  drfft;               END INTERFACE
INTERFACE hfft;    MODULE PROCEDURE shfft,  dhfft;               END INTERFACE

CONTAINS

!=============================================================================
SUBROUTINE infftln(n1,m,nfftln)
!=============================================================================
!        SUBROUTINE SNFFTLN
! Divide out as many factors, m, of given n1 as is possible, returning
! this number of factors as nfftln, and returning n1 as the quotient.
!=============================================================================
INTEGER,INTENT(INOUT):: n1
INTEGER,INTENT(IN   ):: m
INTEGER,INTENT(OUT  ):: nfftln
!-----------------------------------------------------------------------------
INTEGER              :: i,n2
!=============================================================================
nfftln=0
DO i=1,30
   n2=n1/m
   IF(n2*m /= n1)RETURN
   n1=n2
   nfftln=i
ENDDO
END SUBROUTINE infftln 

!=============================================================================
SUBROUTINE iget235(nin,get235) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE SGET235
!
!   Factorize NIN in terms of 2's, 3's, 4's and 5's and verify that 
!   these include the only prime factors. Set LN2, LN3, LN4, LN5 
!   respectively to the number of powers of 2, 3, 4, 5, set LN to be
!   the sum, LN2+LN3+LN4*2+LN5, and initialize n, nm, nh, all in fft2345.
!
! --> NIN      Number of data along the line of Fourier transformation
!=============================================================================
USE fft2345
LOGICAL,INTENT(OUT):: get235
INTEGER,INTENT(IN ):: nin
!-----------------------------------------------------------------------------
INTEGER k
!=============================================================================
k=nin
CALL snfftln(k,5,ln5)
CALL snfftln(k,4,ln4)
CALL snfftln(k,3,ln3)
CALL snfftln(k,2,ln2)
ln=ln2+ln4*2+ln3+ln5
n=(2**ln2)*(3**ln3)*(4**ln4)*(5**ln5); nm=n-1; nh=n/2
IF(n /= nin)STOP 'prime factors of fft period are not only 2, 3, 5'
get235=(k==1)
END SUBROUTINE iget235 

!=============================================================================
SUBROUTINE iget2345(pl2,pl3,pl4,pl5)
!=============================================================================
USE fft2345
INTEGER, INTENT(OUT):: pl2,pl3,pl4,pl5
pl2=ln2; pl3=ln3; pl4=ln4; pl5=ln5
END SUBROUTINE iget2345 

!=============================================================================
SUBROUTINE srumble(jumble,tumble) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE SRUMBLE
!
!   Initialize coefficient arrays JUMBLE and TUMBLE for use with the
!   fast-Fourier-transform routines when the number N of data has the prime-
!   factorization 2**LN2*3**LN3*5**LN5
!
! <-- JUMBLE:  Permutation of N data encoded as a sequence of transpositions
! <-- TUMBLE:  Trigonometric coefficients for use in FFT. The first half are
!	       the cosines, the second half the sines, of uniformly increasing
!	       relevant angles.
!=============================================================================
USE fft2345
INTEGER, DIMENSION(0:*),INTENT(OUT):: jumble
REAL(SP),DIMENSION(0:*),INTENT(OUT):: tumble
!-----------------------------------------------------------------------------
INTEGER, PARAMETER                 :: ml=20
INTEGER                            :: i,j,l,id,is,ir,kd
INTEGER, DIMENSION(ml)             :: nd,md
REAL(SP)                           :: ang,pi2on
!=============================================================================
pi2on=8._SP*ATAN(1._SP)/n
DO i=0,nh-1
   ang=pi2on*i
   tumble(i)   =COS(ang); tumble(i+nh)=SIN(ang)
ENDDO
id=1;  is=0
DO i=1,ln5;       is=is+1; md(is)=id; id=id*5; ENDDO
DO i=1,ln3;       is=is+1; md(is)=id; id=id*3; ENDDO
DO i=1,ln2+ln4*2; is=is+1; md(is)=id; id=id*2; ENDDO
id=1
DO i=1,ln2+ln4*2; nd(is)=id; id=id*2; is=is-1; ENDDO
DO i=1,ln3;       nd(is)=id; id=id*3; is=is-1; ENDDO
DO i=1,ln5;       nd(is)=id; id=id*5; is=is-1; ENDDO
jumble(0)=n
DO i=1,nm
   ir=i; j=0
   DO l=1,ln; kd=ir/nd(l); ir=ir-kd*nd(l); j=j+kd*md(l); ENDDO
   jumble(i)=j
ENDDO
DO i=1,nm
   j=jumble(i)
   IF(j < i)THEN
400   j=jumble(j); IF(j < i)GOTO 400
      jumble(i)=j
   ENDIF
ENDDO
END SUBROUTINE srumble 

!=============================================================================
SUBROUTINE drumble(jumble,tumble) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!		    SUBROUTINE DRUMBLE: double precision version of rumble
!=============================================================================
USE fft2345
INTEGER, DIMENSION(0:*),INTENT(OUT):: jumble
REAL(DP),DIMENSION(0:*),INTENT(OUT):: tumble
!-----------------------------------------------------------------------------
INTEGER, PARAMETER                 :: ml=20
INTEGER                            :: i,j,l,id,is,ir,kd
INTEGER, DIMENSION(ml)             :: nd,md
REAL(DP)                           :: ang,pi2on
!=============================================================================
pi2on=8._DP*ATAN(1._DP)/n
DO i=0,nh-1
   ang=pi2on*i
   tumble(i)   =COS(ang); tumble(i+nh)=SIN(ang)
ENDDO
id=1;  is=0
DO i=1,ln5;       is=is+1; md(is)=id; id=id*5; ENDDO
DO i=1,ln3;       is=is+1; md(is)=id; id=id*3; ENDDO
DO i=1,ln2+ln4*2; is=is+1; md(is)=id; id=id*2; ENDDO
id=1
DO i=1,ln2+ln4*2; nd(is)=id; id=id*2; is=is-1; ENDDO
DO i=1,ln3;       nd(is)=id; id=id*3; is=is-1; ENDDO
DO i=1,ln5;       nd(is)=id; id=id*5; is=is-1; ENDDO
jumble(0)=n
DO i=1,nm
   ir=i; j=0
   DO l=1,ln; kd=ir/nd(l); ir=ir-kd*nd(l); j=j+kd*md(l); ENDDO
   jumble(i)=j
ENDDO
DO i=1,nm
   j=jumble(i)
   IF(j < i)THEN
400   j=jumble(j); IF(j < i)GOTO 400
      jumble(i)=j
   ENDIF
ENDDO
END SUBROUTINE drumble

!=============================================================================
SUBROUTINE sfftco(n,j,w,ln2,ln3,ln4,ln5) 
!=============================================================================
!   R. J. Purser, NCEP, Washington D.C. 1999. jpurser@ncep.noaa.gov
!                   SUBROUTINE SFFTCO
!   Provide the FFT integer coefficients, j, and real coefficients, w,
! corresponding to a period, n, which factors into powers of 2, 3 and 5
! only. For short period transforms, retain (in js, ws) up to two previously 
! used sets of these coefficients to avoid the cost of recalculating them
! in repeated applications. Then the first element of the relevant
! column of js is always the period, n, making it easy to recognize
! whether or not the new requirements for j and w are met in previously
! recorded pairs, js and ws. 
!   For long period transforms, for which the storage burden is 
! intolerable, the coefficients are computed anew every time they are 
! used, and are not retained between applications.
!
! --> n:      period of data
! --> j:      array of n permutation indices to unscramble the fft output.
! --> w:      array of n real trigonometric coefficients for the fft.
!=============================================================================
INTEGER, PARAMETER :: bsize=2048,bsizem=bsize-1,bsize2=bsize*2
INTEGER,                  INTENT(IN) :: n
INTEGER, DIMENSION(0:n-1),INTENT(OUT):: j
REAL(SP),DIMENSION(0:n-1),INTENT(OUT):: w
INTEGER,                  INTENT(OUT):: ln2,ln3,ln4,ln5
!-----------------------------------------------------------------------------
LOGICAL                              :: get235
INTEGER                              :: ult,nm
INTEGER,        DIMENSION(0:bsizem,2):: js
REAL(SP),       DIMENSION(0:bsizem,2):: ws
DATA ult/1/ ! Column index of js and ws for latest fft coefficients used.
DATA js/bsize2*0/,ws/bsize2*0._SP/
!=============================================================================
nm=n-1
CALL sget235(n,get235)
IF(.NOT. get235)STOP 'prime factors are not only 2, 3, 5'
CALL get2345(ln2,ln3,ln4,ln5)
IF(n <= bsize)THEN
   IF(n == js(0,ult))THEN
      j(0:nm)=js(0:nm,ult); w(0:nm)=ws(0:nm,ult) ! copy existing values
      RETURN
   ENDIF
   ult=3-ult                             ! Try the alternative location
   IF(n == js(0,ult))THEN
      j(0:nm)=js(0:nm,ult); w(0:nm)=ws(0:nm,ult) ! copy existing values
      RETURN
   ENDIF
   CALL rumble(j,w)
   js(0:nm,ult)=j(0:nm); ws(0:nm,ult)=w(0:nm)
ELSE
   CALL rumble(j,w) ! Too big to record in js and ws
ENDIF
END SUBROUTINE sfftco

!=============================================================================
SUBROUTINE dfftco(n,j,w,ln2,ln3,ln4,ln5) 
!=============================================================================
!   R. J. Purser, NCEP, Washington D.C. 1999. jpurser@ncep.noaa.gov
!                   SUBROUTINE DFFTCO: double precision version of fftco
!=============================================================================
INTEGER, PARAMETER:: bsize=2048,bsizem=bsize-1,bsize2=bsize*2
INTEGER,                  INTENT(IN) :: n
INTEGER, DIMENSION(0:n-1),INTENT(OUT):: j
REAL(DP),DIMENSION(0:n-1),INTENT(OUT):: w
INTEGER,                  INTENT(OUT):: ln2,ln3,ln4,ln5
!-----------------------------------------------------------------------------
LOGICAL                              :: get235
INTEGER                              :: ult,nm
INTEGER, DIMENSION(0:bsizem,2)       :: js
REAL(DP),DIMENSION(0:bsizem,2)       :: ws
DATA ult/1/ ! Column index of js and ws for latest fft coefficients used.
DATA js/bsize2*0/,ws/bsize2*0._DP/
!=============================================================================
nm=n-1
CALL sget235(n,get235)
IF(.NOT. get235)STOP 'prime factors are not only 2, 3, 5'
CALL get2345(ln2,ln3,ln4,ln5)
IF(n <= bsize)THEN
   IF(n == js(0,ult))THEN
      j(0:nm)=js(0:nm,ult); w(0:nm)=ws(0:nm,ult) ! copy existing values
      RETURN
   ENDIF
   ult=3-ult                             ! Try the alternative location
   IF(n == js(0,ult))THEN
      j(0:nm)=js(0:nm,ult); w(0:nm)=ws(0:nm,ult) ! copy existing values
      RETURN
   ENDIF
   CALL rumble(j,w)
   js(0:nm,ult)=j(0:nm); ws(0:nm,ult)=w(0:nm)
ELSE
   CALL rumble(j,w) ! Too big to record in js and ws
ENDIF
END SUBROUTINE dfftco


!=============================================================================
SUBROUTINE scfft(n,rb,qb)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!		    SUBROUTINES SCFFT, SDFFT
!
!   Fourier analyze (CFFT) or synthesize (DFFT)	a line of complex data
! --> n	      Number of complex data of series (product of 2's,3's,5's)
! <-> rb      Real part of data and transform
! <-> qb      Imaginary part of data and transform
!
!   For related routines, see subroutine RUMBLE (in which the
!   coefficient arrays are initialized), XCFFT, YCFFT (matrix versions
!   of CFFT), XDFFT, YDFFT (matrix versions of DFFT), RFFT (Fourier
!   analysis of real data), HFFT (Fourier synthesis of real data), and
!   matrix counterparts, XRFFT, YRFFT, XHFFT, YHFFT.
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(SP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
           nze
REAL(SP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(SP),DIMENSION(0:n-1) :: w
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 sdfft(n,rb,qb)
rfac=1._SP/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
  
! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t1=rb(i); rb(i)=rb(j); rb(j)=t1
      t1=qb(i); qb(i)=qb(j); qb(j)=t1
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb; jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j;    k1=k0+ma;   k2=k1+ma;   k3=k2+ma
         r0=rb(k0); r1=rb(k1);  r2=rb(k2);  r3=rb(k3)
         q0=qb(k0); q1=qb(k1);  q2=qb(k2);  q3=qb(k3)
         t1    =r3*rf3-q3*qf3	        ! q13
         q3    =r3*qf3+q3*rf3	        ! r13
         r3    =r2*rf1-q2*qf1	        ! r12
         r2    =r2*qf1+q2*rf1	        ! q12
         q2    =q3    -r2		! r23
         r2    =q3    +r2		! q22
         q3    =r3    +t1		! r22
         t1    =r3    -t1		! q23
         r3    =r1*rf2-q1*qf2	        ! r11
         q1    =r1*qf2+q1*rf2	        ! q11
         r1    =r0    -r3		! r21
         r0    =r0    +r3		! r20
         rb(k3)=r1    -q2	        ! r3
         rb(k1)=r1    +q2	        ! r1
         q2    =q0    +q1		! q20
         q1    =q0    -q1		! q21
         qb(k0)=q2    +r2	        ! q0
         qb(k2)=q2    -r2	        ! q2
         rb(k2)=r0    -q3	        ! r2
         rb(k0)=r0    +q3	        ! r0
         qb(k3)=q1    -t1	        ! q3
         qb(k1)=q1    +t1	        ! q1
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
         k0=j+i;        k1=k0+ma
         rf1=w(jmb);    qf1=w(nh+jmb)
         r0    =rb(k0);     q0=qb(k0)
         r1    =rb(k1);     q1=qb(k1)
         t1    =r1*qf1+q1*rf1 ! q11
         q1    =r1*rf1-q1*qf1 ! r11
         rb(k1)=r0    -q1     ! r1
         rb(k0)=r0    +q1     ! r0
         qb(k1)=q0    -t1     ! q1
         qb(k0)=q0    +t1     ! q0
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
         k0=i+j;     k1=k0+ma;    k2=k1+ma
         r1=rb(k1);  q1=qb(k1)
         r2=rb(k2);  q2=qb(k2)
         t1    = r2*qf2+q2 *rf2	        ! r12
         q2    = r2*rf2-q2 *qf2	        ! q12
         r2    = r1*qf1+q1 *rf1	        ! q11
         r1    = r1*rf1-q1 *qf1	        ! r11
         q1    = r2    +t1		! q21
         r2    =(r2    -t1)*qep	        ! r22
         t1    = r1    +q2		! r21
         r1    =(r1    -q2)*qep	        ! q22
         rb(k0)= rb(k0)+t1	        ! r0
         qb(k0)= qb(k0)+q1	        ! q0
         t1    = rb(k0)-t1 *rec	        ! r21
         q1    = qb(k0)-q1 *rec	        ! q21
         qb(k2)= q1    -r1	        ! q2
         qb(k1)= q1    +r1	        ! q1
         rb(k1)= t1    -r2	        ! r1
         rb(k2)= t1    +r2	        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5;  rze=w(nze); qze=w(nh+nze); rzc=1.-rze; ret=rze*rze-qze*qze
   qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;             jmb2=jmb*2
         rf1=w(jmb);           qf1=w(nh+jmb)
         rf2=w(jmb2);          qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2;  qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2;  qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j;   k1=k0+ma;  k2=k1+ma;  k3=k2+ma;  k4=k3+ma
            r1=rb(k1); r2=rb(k2); r3=rb(k3); r4=rb(k4)
            q1=qb(k1); q2=qb(k2); q3=qb(k3); q4=qb(k4)
            t1    =r1*qf1+q1*rf1	      ! q11
            r1    =r1*rf1-q1*qf1	      ! r11
            q1    =r4*rf4-q4*qf4	      ! q14
            r4    =r4*qf4+q4*rf4	      ! r14
            q4    =r1    -q1		      ! q24
            r1    =r1    +q1		      ! r21
            q1    =t1    +r4		      ! q21
            r4    =t1    -r4		      ! r24
            t1    =r3*rf3-q3*qf3	      ! q13
            r3    =r3*qf3+q3*rf3	      ! r13
            q3    =r2*qf2+q2*rf2	      ! q12
            r2    =r2*rf2-q2*qf2	      ! r12
            q2    =q3    +r3		      ! q22
            r3    =q3    -r3		      ! r23
            q3    =r2    -t1		      ! q23
            r2    =r2    +t1		      ! r22
            rb(k0)=rb(k0)+r1    +r2           ! r0
            qb(k0)=qb(k0)+q1    +q2           ! q0
            t1    =r4*qze+r3*qet              ! r34
            r3    =r3*qze-r4*qet              ! r33
            r4    =rb(k0)-r2*rzc-r1*rec       ! r32
            r1    =rb(k0)-r1*rzc-r2*rec       ! r31
            rb(k2)=r4    +r3	              ! r2
            rb(k3)=r4    -r3	              ! r3
            rb(k4)=r1    +t1	              ! r4
            rb(k1)=r1    -t1	              ! r1
            t1    =qb(k0)-q1*rzc-q2*rec       ! q31
            q2    =qb(k0)-q2*rzc-q1*rec       ! q32
            q1    =q3*qze-q4*qet              ! q33
            q4    =q4*qze+q3*qet              ! q34
            qb(k3)=q2    +q1	              ! q3
            qb(k2)=q2    -q1	              ! q2
            qb(k1)=t1    +q4	              ! q1
            qb(k4)=t1    -q4	              ! q4
         ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE scfft 

!=============================================================================
SUBROUTINE dcfft(n,rb,qb)
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999)
!		    SUBROUTINES DCFFT, DDFFT
! Double precision versions of cfft, dfft
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
INTEGER :: ln2,ln3,ln4,ln5
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,ma,ma2,ma3,ma4,ma5,mb,mb2,jmb,jmb2, &
          nze
REAL(DP):: r0,r1,r2,r3,r4,q0,q1,q2,q3,q4,rf1,rf2,rf3,rf4,qf1,qf2,qf3,qf4, &
           rep,rec,rze,rzc,ret,qep,qze,qet,t1,rfac
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 ddfft(n,rb,qb)
rfac=1._DP/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 nh=n/2
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5)
  
! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t1=rb(i); rb(i)=rb(j); rb(j)=t1
      t1=qb(i); qb(i)=qb(j); qb(j)=t1
   ENDIF
ENDDO

!  TRANSFORM THE DATA:
ma=1; mb=n
!  RADIX 4
DO l=1,ln4
   mb=mb/4; ma4=ma*4; mb2=mb*2
   DO j=0,ma-1
      jmb=j*mb; jmb2=j*mb2
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=w(jmb2);         qf2=w(nh+jmb2)
      rf3=rf1*rf2-qf1*qf2; qf3=rf1*qf2+qf1*rf2
      DO i=0,nm,ma4
         k0=i+j;    k1=k0+ma;   k2=k1+ma;   k3=k2+ma
         r0=rb(k0); r1=rb(k1);  r2=rb(k2);  r3=rb(k3)
         q0=qb(k0); q1=qb(k1);  q2=qb(k2);  q3=qb(k3)
         t1    =r3*rf3-q3*qf3	        ! q13
         q3    =r3*qf3+q3*rf3	        ! r13
         r3    =r2*rf1-q2*qf1	        ! r12
         r2    =r2*qf1+q2*rf1	        ! q12
         q2    =q3    -r2		! r23
         r2    =q3    +r2		! q22
         q3    =r3    +t1		! r22
         t1    =r3    -t1		! q23
         r3    =r1*rf2-q1*qf2	        ! r11
         q1    =r1*qf2+q1*rf2	        ! q11
         r1    =r0    -r3		! r21
         r0    =r0    +r3		! r20
         rb(k3)=r1    -q2	        ! r3
         rb(k1)=r1    +q2	        ! r1
         q2    =q0    +q1		! q20
         q1    =q0    -q1		! q21
         qb(k0)=q2    +r2	        ! q0
         qb(k2)=q2    -r2	        ! q2
         rb(k2)=r0    -q3	        ! r2
         rb(k0)=r0    +q3	        ! r0
         qb(k3)=q1    -t1	        ! q3
         qb(k1)=q1    +t1	        ! q1
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
         k0=j+i;        k1=k0+ma
         rf1=w(jmb);    qf1=w(nh+jmb)
         r0=rb(k0);     q0=qb(k0)
         r1=rb(k1);     q1=qb(k1)
         t1    =r1*qf1+q1*rf1 ! q11
         q1    =r1*rf1-q1*qf1 ! r11
         rb(k1)=r0    -q1     ! r1
         rb(k0)=r0    +q1     ! r0
         qb(k1)=q0    -t1     ! q1
         qb(k0)=q0    +t1     ! q0
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
         k0=i+j;     k1=k0+ma;    k2=k1+ma
         r1=rb(k1);  q1=qb(k1)
         r2=rb(k2);  q2=qb(k2)
         t1    = r2*qf2+q2 *rf2	        ! r12
         q2    = r2*rf2-q2 *qf2	        ! q12
         r2    = r1*qf1+q1 *rf1	        ! q11
         r1    = r1*rf1-q1 *qf1	        ! r11
         q1    = r2    +t1		! q21
         r2    =(r2    -t1)*qep	        ! r22
         t1    = r1    +q2		! r21
         r1    =(r1    -q2)*qep	        ! q22
         rb(k0)= rb(k0)+t1	        ! r0
         qb(k0)= qb(k0)+q1	        ! q0
         t1    = rb(k0)-t1 *rec	        ! r21
         q1    = qb(k0)-q1 *rec	        ! q21
         qb(k2)= q1    -r1	        ! q2
         qb(k1)= q1    +r1	        ! q1
         rb(k1)= t1    -r2	        ! r1
         rb(k2)= t1    +r2	        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5;  rze=w(nze); qze=w(nh+nze); rzc=1.-rze; ret=rze*rze-qze*qze
   qet=2*rze*qze; rec=1.-ret
   DO l=1,ln5
      mb=mb/5; ma5=ma*5
      DO j=0,ma-1
         jmb=j*mb;             jmb2=jmb*2
         rf1=w(jmb);           qf1=w(nh+jmb)
         rf2=w(jmb2);          qf2=w(nh+jmb2)
         rf3=rf1*rf2-qf1*qf2;  qf3=rf1*qf2+qf1*rf2
         rf4=rf2*rf2-qf2*qf2;  qf4=2*rf2*qf2
         DO i=0,nm,ma5
            k0=i+j;   k1=k0+ma;  k2=k1+ma;  k3=k2+ma;  k4=k3+ma
            r1=rb(k1); r2=rb(k2); r3=rb(k3); r4=rb(k4)
            q1=qb(k1); q2=qb(k2); q3=qb(k3); q4=qb(k4)
            t1    =r1*qf1+q1*rf1	      ! q11
            r1    =r1*rf1-q1*qf1	      ! r11
            q1    =r4*rf4-q4*qf4	      ! q14
            r4    =r4*qf4+q4*rf4	      ! r14
            q4    =r1    -q1		      ! q24
            r1    =r1    +q1		      ! r21
            q1    =t1    +r4		      ! q21
            r4    =t1    -r4		      ! r24
            t1    =r3*rf3-q3*qf3	      ! q13
            r3    =r3*qf3+q3*rf3	      ! r13
            q3    =r2*qf2+q2*rf2	      ! q12
            r2    =r2*rf2-q2*qf2	      ! r12
            q2    =q3    +r3		      ! q22
            r3    =q3    -r3		      ! r23
            q3    =r2    -t1		      ! q23
            r2    =r2    +t1		      ! r22
            rb(k0)=rb(k0)+r1    +r2           ! r0
            qb(k0)=qb(k0)+q1    +q2           ! q0
            t1    =r4*qze+r3*qet              ! r34
            r3    =r3*qze-r4*qet              ! r33
            r4    =rb(k0)-r2*rzc-r1*rec       ! r32
            r1    =rb(k0)-r1*rzc-r2*rec       ! r31
            rb(k2)=r4    +r3	              ! r2
            rb(k3)=r4    -r3	              ! r3
            rb(k4)=r1    +t1	              ! r4
            rb(k1)=r1    -t1	              ! r1
            t1    =qb(k0)-q1*rzc-q2*rec       ! q31
            q2    =qb(k0)-q2*rzc-q1*rec       ! q32
            q1    =q3*qze-q4*qet              ! q33
            q4    =q4*qze+q3*qet              ! q34
            qb(k3)=q2    +q1	              ! q3
            qb(k2)=q2    -q1	              ! q2
            qb(k1)=t1    +q4	              ! q1
            qb(k4)=t1    -q4	              ! q4
         ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF
END SUBROUTINE dcfft

!=============================================================================
SUBROUTINE scsft(n,rb,qb) 
!=============================================================================
!           SUBROUTINE SCSFT
! Complex slow Fourier transform. For inverse, see SDSFT.
! --> n:     Period
! <-> rb,qb: Real and imaginary parts of data and transform.
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(SP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER                  :: nm,i,k,ik
REAL(SP)                 :: dang,ang,rfac
REAL(SP),DIMENSION(0:n-1):: ra,qa,wc,ws
!=============================================================================
nm=n-1
rfac=1._SP/n
dang=8._SP*ATAN(1._SP)*rfac
DO k=0,nm; ang=k*dang; wc(k)=COS(ang); ws(k)=SIN(ang); ENDDO
ra=0; qa=0
DO k=0,nm
   DO i=0,nm
      ik=MOD(i*k,n)
      ra(k)=ra(k)+wc(ik)*rb(i)-ws(ik)*qb(i)
      qa(k)=qa(k)+wc(ik)*qb(i)+ws(ik)*rb(i)
   ENDDO
ENDDO
rb=ra; qb=qa
END SUBROUTINE scsft 

!=============================================================================
SUBROUTINE dcsft(n,rb,qb) 
!=============================================================================
!           SUBROUTINE DCSFT: Double precision version of csft
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER                                :: nm,i,k,ik
REAL(DP)                               :: dang,ang,rfac
REAL(DP),DIMENSION(0:n-1)              :: ra,qa,wc,ws
!=============================================================================
nm=n-1
rfac=1._DP/n
dang=8._DP*ATAN(1._DP)*rfac
DO k=0,nm; ang=k*dang; wc(k)=COS(ang); ws(k)=SIN(ang); ENDDO
ra=0; qa=0
DO k=0,nm
   DO i=0,nm
      ik=MOD(i*k,n)
      ra(k)=ra(k)+wc(ik)*rb(i)-ws(ik)*qb(i)
      qa(k)=qa(k)+wc(ik)*qb(i)+ws(ik)*rb(i)
   ENDDO
ENDDO
rb=ra; qb=qa
END SUBROUTINE dcsft 

!=============================================================================
SUBROUTINE sdsft(n,rb,qb) 
!=============================================================================
!           SUBROUTINE SDSFT
! Complex slow inverse Fourier transform. For direct transform, see CSFT.
! --> n:     Period
! <-> rb,qb: Real and imaginary parts of transform and synthesis.
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(SP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER                                :: nm,i,k,ik
REAL(SP)                               :: dang,ang,rfac
REAL(SP),DIMENSION(0:n-1)              :: ra,qa,wc,ws
!=============================================================================
nm=n-1
rfac=1._SP/n
dang=8._SP*ATAN(1._SP)*rfac
DO k=0,nm; ang=k*dang; wc(k)=COS(ang); ws(k)=SIN(ang); ENDDO
ra=0; qa=0
DO k=0,nm
   DO i=0,nm
      ik=MOD(i*k,n)
      ra(k)=ra(k)+wc(ik)*rb(i)+ws(ik)*qb(i)
      qa(k)=qa(k)+wc(ik)*qb(i)-ws(ik)*rb(i)
   ENDDO
ENDDO
rb=ra*rfac; qb=qa*rfac
END SUBROUTINE sdsft 

!=============================================================================
SUBROUTINE ddsft(n,rb,qb) 
!=============================================================================
!           SUBROUTINE DDSFT: Double precision version of dsft
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!-----------------------------------------------------------------------------
INTEGER                                :: nm,i,k,ik
REAL(DP)                               :: dang,ang,rfac
REAL(DP),DIMENSION(0:n-1)              :: ra,qa,wc,ws
!=============================================================================
nm=n-1
rfac=1._DP/n
dang=8._DP*ATAN(1._DP)*rfac
DO k=0,nm; ang=k*dang; wc(k)=COS(ang); ws(k)=SIN(ang); ENDDO
ra=0; qa=0
DO k=0,nm
   DO i=0,nm
      ik=MOD(i*k,n)
      ra(k)=ra(k)+wc(ik)*rb(i)+ws(ik)*qb(i)
      qa(k)=qa(k)+wc(ik)*qb(i)-ws(ik)*rb(i)
   ENDDO
ENDDO
rb=ra*rfac; qb=qa*rfac
END SUBROUTINE ddsft 

!=============================================================================
subroutine sdsfe(n,rb,qb,x,y,r,q,dr,dq)
!=============================================================================
! Single precision complex "slow" Fourier evaluation.
! Evaluate the (complex) Fourier transform at a single point, together with
! its complex derivative there. Assume the period of the series is 2*pi.
!
! --> n     : number of imaginary Fourier coefficients of a complex function
! --> rb,qb : real and imaginary Fourier coefficients
! --> x,y   : real and imaginary components of complex representation of point.
! <-- r,q   : real and imaginary components of the function at the point.
! <-- dr,dq : real and imaginary components of derivative of function there.
!=============================================================================
integer,                  intent(IN ):: n
real(SP),dimension(0:n-1),intent(IN ):: rb,qb
real(SP),                 intent(IN ):: x,y
real(SP),                 intent(OUT):: r,q,dr,dq
!-----------------------------------------------------------------------------
real(SP)                             :: rfac,angx,angy,cangx,sangx,eangy,&
                                        rbk,qbk,rbl,qbl,rterm,qterm
integer                              :: k,nm,nh,nhm,L
!============================================================================
nm=n-1;  nh=(n+1)/2;  nhm=nh-1
r=0; q=0; dr=0; dq=0
rfac=1._SP/n
do k=0,nhm
   L=mod(n-k,n); rbk=rb(k); qbk=qb(k); rbl=rb(L); qbl=qb(L)
   angx=k*x; cangx=cos(angx); sangx=sin(angx); angy=k*y
   if(rbk==0 .and. qbk==0)goto 3
   eangy=exp(angy)
   rterm=(rbk*cangx+qbk*sangx)*eangy; qterm=(-rbk*sangx+qbk*cangx)*eangy
   r =r +rterm;    q =q +qterm;  if(k==0)cycle
   dr=dr+qterm*k;  dq=dq-rterm*k
3  if(rbl==0 .and. qbl==0)cycle
   eangy=exp(-angy)
   rterm=(rbl*cangx-qbl*sangx)*eangy; qterm=( rbl*sangx+qbl*cangx)*eangy
   r=r  +rterm;    q =q +qterm
   dr=dr-qterm*k;  dq=dq+rterm*k
enddo
r=r*rfac; q=q*rfac; dr=dr*rfac; dq=dq*rfac
end subroutine sdsfe

!=============================================================================
subroutine ddsfe(n,rb,qb,x,y,r,q,dr,dq)
!=============================================================================
! Double precision complex "slow" Fourier evaluation.
! Evaluate the (complex) Fourier transform at a single point, together with
! its complex derivative there. Assume the period of the series is 2*pi.
!
! --> n     : number of imaginary Fourier coefficients of a complex function
! --> rb,qb : real and imaginary Fourier coefficients
! --> x,y   : real and imaginary components of complex representation of point.
! <-- r,q   : real and imaginary components of the function at the point.
! <-- dr,dq : real and imaginary components of derivative of function there.
!=============================================================================
integer,                  intent(IN ):: n
real(DP),dimension(0:n-1),intent(IN ):: rb,qb
real(DP),                 intent(IN ):: x,y
real(DP),                 intent(OUT):: r,q,dr,dq
!-----------------------------------------------------------------------------
real(DP)                             :: rfac,angx,angy,cangx,sangx,eangy,&
                                        rbk,qbk,rbl,qbl,rterm,qterm
integer                              :: k,nm,nh,nhm,L
!============================================================================
nm=n-1;  nh=(n+1)/2;  nhm=nh-1
r=0; q=0; dr=0; dq=0
rfac=1._DP/n
do k=0,nhm
   L=mod(n-k,n); rbk=rb(k); qbk=qb(k); rbl=rb(L); qbl=qb(L)
   angx=k*x; cangx=cos(angx); sangx=sin(angx); angy=k*y
   if(rbk==0 .and. qbk==0)goto 3
   eangy=exp(angy)
   rterm=(rbk*cangx+qbk*sangx)*eangy; qterm=(-rbk*sangx+qbk*cangx)*eangy
   r =r +rterm;    q =q +qterm;  if(k==0)cycle
   dr=dr+qterm*k;  dq=dq-rterm*k
3  if(rbl==0 .and. qbl==0)cycle
   eangy=exp(-angy)
   rterm=(rbl*cangx-qbl*sangx)*eangy; qterm=( rbl*sangx+qbl*cangx)*eangy
   r=r  +rterm;    q =q +qterm
   dr=dr-qterm*k;  dq=dq+rterm*k
enddo
r=r*rfac; q=q*rfac; dr=dr*rfac; dq=dq*rfac
end subroutine ddsfe

!============================================================================
subroutine shsfe(n,b,x,r,dr)
!============================================================================
! Slow Hermitian Fourier synthesis evaluation and derivative
!============================================================================
integer,                  intent(IN ):: n
real(sp),dimension(0:n-1),intent(IN ):: b
real(sp),                 intent(IN ):: x
real(sp),                 intent(OUT):: r,dr
!----------------------------------------------------------------------------
integer                              :: nm,nh,nhm,k,l
real(sp)                             :: ang,cang,sang,bk,bl,rfac
!============================================================================
nm=n-1;  nh=(n+1)/2;  nhm=nh-1
rfac=(2._sp)/n
r=(b(0)+b(nh)*cos(x*nh))/2
dr=0
do k=1,nhm
   l=n-k
   bk=b(k); bl=b(l)
   ang=x*k; cang=cos(ang); sang=sin(ang)
   r=r+bk*cang-bl*sang
   dr=dr-(bk*sang+bl*cang)*k
enddo
r=r*rfac; dr=dr*rfac
end subroutine shsfe
!
!============================================================================
subroutine dhsfe(n,b,x,r,dr)
!============================================================================
integer,                  intent(IN ):: n
real(dp),dimension(0:n-1),intent(IN ):: b
real(dp),                 intent(IN ):: x
real(dp),                 intent(OUT):: r,dr
!----------------------------------------------------------------------------
integer                              :: nm,nh,nhm,k,l
real(dp)                             :: ang,cang,sang,bk,bl,rfac
!============================================================================
nm=n-1;  nh=(n+1)/2;  nhm=nh-1
rfac=(2._dp)/n
r=(b(0)+b(nh)*cos(x*nh))/2
dr=0
do k=1,nhm
   l=n-k
   bk=b(k); bl=b(l)
   ang=x*k; cang=cos(ang); sang=sin(ang)
   r=r+bk*cang-bl*sang
   dr=dr-(bk*sang+bl*cang)*k
enddo
r=r*rfac; dr=dr*rfac
end subroutine dhsfe

!=============================================================================
SUBROUTINE srfft(n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!		    SUBROUTINES SRFFT
!   Fourier analyze (RFFT) a line of real data (for inverse, see HFFT)
!   The real coefficients of the transforms are stored in direct order
!   of wavenumber in elements 0 through n/2. The imaginary coefficients
!   of the transform are stored in reverse order of wavenumber in
!   the remaining elements. In this way, the structure of the transform 
!   of real data is made to conform more immediately to that of the 
!   complex data transform (provided in cfft).
!
! --> N	      Number of data of series (product of 2's,3's,5's)
! <-> R	      Data and transform
!=============================================================================
INTEGER,                   INTENT(IN   ):: n
REAL(SP), DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nhp,i
REAL(SP), DIMENSION(0:n-1)              :: t
!=============================================================================
nm=n-1; nhp=n/2+1
t=0
CALL cfft(n,r,t)
r(nhp:nm)=t(nhp:nm)
END SUBROUTINE srfft 

!=============================================================================
SUBROUTINE drfft(n,r) 
!=============================================================================
!		    SUBROUTINES DRFFT: Double precision version of rfft
!=============================================================================
INTEGER,                   INTENT(IN   ):: n
REAL(DP), DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nhp,i
REAL(DP), DIMENSION(0:n-1)              :: t
!=============================================================================
nm=n-1; nhp=n/2+1
t=0
CALL cfft(n,r,t)
r(nhp:nm)=t(nhp:nm)
END SUBROUTINE drfft 

!=============================================================================
SUBROUTINE shfft(n,r) 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!		    SUBROUTINES SHFFT
!   Fourier synthesize (HFFT) a line of real data
!
! --> N	      Number of data of series (product of 2's,3's,5's)
! <-> R	      Data and transform
!=============================================================================
INTEGER,                   INTENT(IN   ):: n
REAL(SP), DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nh,nmh,nhp,i
REAL(SP), DIMENSION(0:n-1)              :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1
t(0)=0; t(nh)=0
t(nhp:nm)=r(nhp:nm); t(1:nmh)=-t(nm:nhp:-1); r(nhp:nm)=r(nmh:1:-1)
CALL dfft(n,r,t)
END SUBROUTINE shfft 

!=============================================================================
SUBROUTINE dhfft(n,r) 
!=============================================================================
!		    SUBROUTINES DHFFT: Double precision version of hfft
!=============================================================================
INTEGER,                   INTENT(IN   ):: n
REAL(DP), DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nh,nmh,nhp,i
REAL(DP),DIMENSION(0:n-1)               :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1
t(0)=0; t(nh)=0
t(nhp:nm)=r(nhp:nm); t(1:nmh)=-t(nm:nhp:-1); r(nhp:nm)=r(nmh:1:-1)
CALL dfft(n,r,t)
END SUBROUTINE dhfft 

END MODULE pfft

