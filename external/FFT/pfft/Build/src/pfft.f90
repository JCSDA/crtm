!                                                         ******************
!                                                         *   pfft.f90     *
!                                                         *  Purser 1994   *
!                                                         ******************
! Latest revisions: April 2003
!                   April 2011 (include radix-7, 11, 13)
! 
! NOAA/NCEP/Environmental Modeling Center.              jim.purser@noaa.gov
! Suite of Fast Fourier Transform routines based on factors 2, 3, 5, 7,
! 11 ("B") and 13 ("D")..
! Data assumed from 0 to n-1 and transforms performed in place.
! For complex transforms of real data, real components of wavenumber k
! are found at positions k [0,..,n/2], imag components of wavenumber -k
! are found at positions n-k [n/2+1,..n-1]. 
!=============================================================================
MODULE fft23457bd
!=============================================================================
IMPLICIT NONE
INTEGER ln2,ln3,ln4,ln5,ln7,lnb,lnd,ln,n,nm,nh
END MODULE fft23457bd

!=============================================================================
MODULE pfft
!=============================================================================
use pkind, only: sp,dp
implicit none
private
public :: fftco,fftcop,cfft,dfft,csft,dsft,dsfe,hsfe,rfft,hfft,fftcnv,fconv

INTERFACE snfftln;    MODULE PROCEDURE infftln;                  END INTERFACE
INTERFACE sget2357bd; MODULE PROCEDURE iget2357bd;               END INTERFACE
INTERFACE get23457bd; MODULE PROCEDURE iget23457bd;              END INTERFACE
INTERFACE rumble;   MODULE PROCEDURE srumble,drumble;            END INTERFACE
INTERFACE fftco;    MODULE PROCEDURE sfftco, dfftco;             end interface
interface fftcop;   module procedure sfftcop, dfftcop;           END INTERFACE
INTERFACE cfft
   MODULE PROCEDURE scfft,  dcfft, scfftp, dcfftp;               END INTERFACE
INTERFACE dfft
   MODULE PROCEDURE sdfft,  ddfft, sdfftp, ddfftp;               END INTERFACE
interface gfft;     module procedure sgfft,  dgfft;              end interface
INTERFACE csft;     MODULE PROCEDURE scsft,  dcsft;              END INTERFACE
INTERFACE dsft;     MODULE PROCEDURE sdsft,  ddsft;              END INTERFACE
INTERFACE dsfe;     MODULE PROCEDURE sdsfe,  ddsfe;              END INTERFACE
interface hsfe;     module procedure shsfe,  dhsfe;              end interface
INTERFACE rfft     
   MODULE PROCEDURE srfft,  drfft, srfftp, drfftp;               END INTERFACE
INTERFACE hfft
   MODULE PROCEDURE shfft,  dhfft, shfftp, dhfftp;               END INTERFACE
interface fftcnv
   module procedure sfftcnv,dfftcnv,sfftcnvp, dfftcnvp;          end interface
interface fconv;    module procedure sfconv, dfconv;             end interface


CONTAINS

!=============================================================================
SUBROUTINE infftln(n1,m,nfftln)!                                     [snfftln]
!=============================================================================
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
SUBROUTINE iget2357bd(nin,get2357bd)!                             [sget2357bd]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
!   Factorize NIN in terms of 2's, 3's, 4's, 5's, 7's, 11's and 13's  and 
!   verify that these include the only prime factors. Set LN2, LN3, LN4, LN5, 
!   LN7, LNB, LND respectively to the number of powers of: 2, 3, 4, 5, 7, 
!   11, 13; set LN to be the sum, 
!   LN2+LN3+LN4*2+LN5+LN7+LNB+LND, and initialize n, nm, nh, 
!   all in fft23457bd.
!
! --> NIN      Number of data along the line of Fourier transformation
!=============================================================================
USE fft23457bd
LOGICAL,INTENT(OUT):: get2357bd
INTEGER,INTENT(IN ):: nin
!-----------------------------------------------------------------------------
INTEGER k
!=============================================================================
k=nin
call snfftln(k,13,lnd)
call snfftln(k,11,lnb)
call snfftln(k,7,ln7)
CALL snfftln(k,5,ln5)
CALL snfftln(k,4,ln4)
CALL snfftln(k,3,ln3)
CALL snfftln(k,2,ln2)
ln=ln2+ln4*2+ln3+ln5+ln7+lnb+lnd
n=(2**ln2)*(3**ln3)*(4**ln4)*(5**ln5)*(7**ln7)*(11**lnb)*(13**lnd)
nm=n-1; nh=n/2
IF(n /= nin)STOP 'prime factors of fft period are not only 2, 3, 5, 7, 11, 13'
get2357bd=(k==1)
END SUBROUTINE iget2357bd

!=============================================================================
SUBROUTINE iget23457bd(pl2,pl3,pl4,pl5,pl7,plb,pld)!              [get23457bd]
!=============================================================================
USE fft23457bd
INTEGER, INTENT(OUT):: pl2,pl3,pl4,pl5,pl7,plb,pld
pl2=ln2; pl3=ln3; pl4=ln4; pl5=ln5; pl7=ln7; plb=lnb; pld=lnd
END SUBROUTINE iget23457bd

!=============================================================================
SUBROUTINE srumble(jumble,tumble)!                                    [rumble] 
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!
!   Initialize coefficient arrays JUMBLE and TUMBLE for use with the
!   fast-Fourier-transform routines when the number N of data has the prime-
!   factorization 2**LN2*3**LN3*5**LN5*7**LN7*11**LNB*13**LND
!
! <-- JUMBLE:  Permutation of N data encoded as a sequence of transpositions
! <-- TUMBLE:  Trigonometric coefficients for use in FFT. The first half are
!	       the cosines, the second half the sines, of uniformly increasing
!	       relevant angles.
!=============================================================================
USE fft23457bd
INTEGER, DIMENSION(0:*),INTENT(OUT):: jumble
REAL(SP),DIMENSION(0:*),INTENT(OUT):: tumble
!-----------------------------------------------------------------------------
real(sp),parameter                 :: one=1
INTEGER, PARAMETER                 :: ml=30
INTEGER                            :: i,j,l,id,is,ir,kd
INTEGER, DIMENSION(ml)             :: nd,md
REAL(SP)                           :: ang,pi2on
!=============================================================================
pi2on=8*ATAN(one)/n
DO i=0,nh-1
   ang=pi2on*i
   tumble(i)   =COS(ang); tumble(i+nh)=SIN(ang)
ENDDO
id=1;  is=0
DO i=1,lnd;       is=is+1; md(is)=id; id=id*13; ENDDO
DO i=1,lnb;       is=is+1; md(is)=id; id=id*11; ENDDO
DO i=1,ln7;       is=is+1; md(is)=id; id=id*7;  ENDDO
DO i=1,ln5;       is=is+1; md(is)=id; id=id*5;  ENDDO
DO i=1,ln3;       is=is+1; md(is)=id; id=id*3;  ENDDO
DO i=1,ln2+ln4*2; is=is+1; md(is)=id; id=id*2;  ENDDO
id=1
DO i=1,ln2+ln4*2; nd(is)=id; id=id*2;  is=is-1; ENDDO
DO i=1,ln3;       nd(is)=id; id=id*3;  is=is-1; ENDDO
DO i=1,ln5;       nd(is)=id; id=id*5;  is=is-1; ENDDO
DO i=1,ln7;       nd(is)=id; id=id*7;  is=is-1; ENDDO
DO i=1,lnb;       nd(is)=id; id=id*11; is=is-1; ENDDO
DO i=1,lnd;       nd(is)=id; id=id*13; is=is-1; ENDDO
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
SUBROUTINE drumble(jumble,tumble)!                                    [rumble]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!=============================================================================
USE fft23457bd
INTEGER, DIMENSION(0:*),INTENT(OUT):: jumble
REAL(DP),DIMENSION(0:*),INTENT(OUT):: tumble
!-----------------------------------------------------------------------------
real(dp),parameter                 :: one=1
INTEGER, PARAMETER                 :: ml=30
INTEGER                            :: i,j,l,id,is,ir,kd
INTEGER, DIMENSION(ml)             :: nd,md
REAL(DP)                           :: ang,pi2on
!=============================================================================
pi2on=8*ATAN(one)/n
DO i=0,nh-1
   ang=pi2on*i
   tumble(i)   =COS(ang); tumble(i+nh)=SIN(ang)
ENDDO
id=1;  is=0
DO i=1,lnd;       is=is+1; md(is)=id; id=id*13; ENDDO
DO i=1,lnb;       is=is+1; md(is)=id; id=id*11; ENDDO
DO i=1,ln7;       is=is+1; md(is)=id; id=id*7;  ENDDO
DO i=1,ln5;       is=is+1; md(is)=id; id=id*5;  ENDDO
DO i=1,ln3;       is=is+1; md(is)=id; id=id*3;  ENDDO
DO i=1,ln2+ln4*2; is=is+1; md(is)=id; id=id*2;  ENDDO
id=1
DO i=1,ln2+ln4*2; nd(is)=id; id=id*2;  is=is-1; ENDDO
DO i=1,ln3;       nd(is)=id; id=id*3;  is=is-1; ENDDO
DO i=1,ln5;       nd(is)=id; id=id*5;  is=is-1; ENDDO
DO i=1,ln7;       nd(is)=id; id=id*7;  is=is-1; ENDDO
DO i=1,lnb;       nd(is)=id; id=id*11; is=is-1; ENDDO
DO i=1,lnd;       nd(is)=id; id=id*13; is=is-1; ENDDO
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
SUBROUTINE sfftco(n,j,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd)!                  [fftco] 
!=============================================================================
!   R. J. Purser, NCEP, Washington D.C. 1999. jpurser@ncep.noaa.gov
!                   SUBROUTINE SFFTCO
!   Provide the FFT integer coefficients, j, and real coefficients, w,
! corresponding to a period, n, which factors into powers of 2, 3, 5, 7, 11, 13
! only. For short period transforms, retain (in js, ws) up to two previously 
! used sets of these coefficients to avoid the cost of recalculating them
! in repeated applications. Then the first element of the relevant
! column of js is always the period, n, making it easy to recognize
! whether or not the new requirements for j and w are met in previously
! recorded pairs, js and ws. 
!   For long period transforms, it is worth supplying an integer array, js,
! and a real array, ws, both of size n, to store the transform parameters
! between applications so that they do not have to be recomputed each time.
! This is found to save significant time in large scale repeated applications.
! In that case, the relevant version of this routine is SFFTCOP.
!
! --> n:      period of data
! --> j:      array of n permutation indices to unscramble the fft output.
! --> w:      array of n real trigonometric coefficients for the fft.
!=============================================================================
INTEGER, PARAMETER :: bsize=2048,bsizem=bsize-1,bsize2=bsize*2
INTEGER,                  INTENT(IN) :: n
INTEGER, DIMENSION(0:n-1),INTENT(OUT):: j
REAL(SP),DIMENSION(0:n-1),INTENT(OUT):: w
INTEGER,                  INTENT(OUT):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
!-----------------------------------------------------------------------------
LOGICAL                              :: get2357bd
INTEGER                              :: ult,nm
INTEGER,        DIMENSION(0:bsizem,2):: js
REAL(SP),       DIMENSION(0:bsizem,2):: ws
DATA ult/1/ ! Column index of js and ws for latest fft coefficients used.
DATA js/bsize2*0/,ws/bsize2*0._SP/
!=============================================================================
nm=n-1
CALL sget2357bd(n,get2357bd)
IF(.NOT. get2357bd)STOP 'prime factors are not only 2, 3, 5, 7, 11, 13'
CALL get23457bd(ln2,ln3,ln4,ln5,ln7,lnb,lnd)
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
SUBROUTINE dfftco(n,j,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd)!                  [fftco]
!=============================================================================
!   R. J. Purser, NCEP, Washington D.C. 1999. jpurser@ncep.noaa.gov
!                   SUBROUTINE DFFTCO: double precision version of fftco
!=============================================================================
INTEGER, PARAMETER:: bsize=2048,bsizem=bsize-1,bsize2=bsize*2
INTEGER,                  INTENT(IN) :: n
INTEGER, DIMENSION(0:n-1),INTENT(OUT):: j
REAL(DP),DIMENSION(0:n-1),INTENT(OUT):: w
INTEGER,                  INTENT(OUT):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
!-----------------------------------------------------------------------------
LOGICAL                              :: get2357bd
INTEGER                              :: ult,nm
INTEGER, DIMENSION(0:bsizem,2)       :: js
REAL(DP),DIMENSION(0:bsizem,2)       :: ws
DATA ult/1/ ! Column index of js and ws for latest fft coefficients used.
DATA js/bsize2*0/,ws/bsize2*0._DP/
!=============================================================================
nm=n-1
CALL sget2357bd(n,get2357bd)
IF(.NOT. get2357bd)STOP 'prime factors are not only 2, 3, 5, 7, 11, 13'
CALL get23457bd(ln2,ln3,ln4,ln5,ln7,lnb,lnd)
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
SUBROUTINE sfftcop(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd)!              [fftcop]
!=============================================================================
INTEGER,                    INTENT(IN   ):: n
integer, dimension(0:n-1),  intent(INOUT):: js
real(sp),dimension(0:n-1),  intent(INOUT):: ws
INTEGER,                    INTENT(  OUT):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
!-----------------------------------------------------------------------------
LOGICAL                                  :: get2357bd
!=============================================================================
CALL sget2357bd(n,get2357bd)
IF(.NOT. get2357bd)STOP 'prime factors are not only 2, 3, 5, 7, 11, 13'
CALL get23457bd(ln2,ln3,ln4,ln5,ln7,lnb,lnd)
IF(n /= js(0))CALL rumble(js,ws)
END SUBROUTINE sfftcop
!=============================================================================
SUBROUTINE dfftcop(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd)!               [fftcop]
!=============================================================================
!   R. J. Purser, NCEP, Washington D.C. 1999. jpurser@ncep.noaa.gov
!  double precision version of fftcop with the saved parameter arrays
!  provided through the parameter list. This allows the user to provide
!  these parameter arrays of the appropriate size for the duration of the
!  fft tasks and does not require permanent storage to be set aside for them.
! This is advantageous when the FFT problem size might be very large, or
! where many different-sized FFT tasks are alternated repetitively (which
! would necessitate redundant recalculations of these parameters is only
! a fixed size pair of work arrays were reserved).
!=============================================================================
INTEGER,                    INTENT(IN   ):: n
integer, dimension(0:n-1),  intent(INOUT):: js
real(dp),dimension(0:n-1),  intent(INOUT):: ws
INTEGER,                    INTENT(  OUT):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
!-----------------------------------------------------------------------------
LOGICAL                                  :: get2357bd
!=============================================================================
CALL sget2357bd(n,get2357bd)
IF(.NOT. get2357bd)STOP 'prime factors are not only 2, 3, 5, 7, 11, 13'
CALL get23457bd(ln2,ln3,ln4,ln5,ln7,lnb,lnd)
IF(n /= js(0))CALL rumble(js,ws)
END SUBROUTINE dfftcop

!=============================================================================
SUBROUTINE scfft(n,rb,qb)!                                              [cfft]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999; radix-7, 11, 13 included, 2011)
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(sp),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
real(sp),parameter        :: one=1
INTEGER                   :: ln2,ln3,ln4,ln5,ln7,lnb,lnd
INTEGER                   :: nm
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(sp),DIMENSION(0:n-1) :: w
real(sp)                  :: rfac
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 sdfft(n,rb,qb)!                                                [dfft]
rfac=one/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 continue
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd)
call  gfft(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd,rb,qb)
end SUBROUTINE scfft

!=============================================================================
SUBROUTINE dcfft(n,rb,qb)!                                              [cfft]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999; radix-7, 11, 13 included, 2011)
!		    SUBROUTINES DCFFT, DDFFT
! Double precision versions of cfft, dfft
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
real(dp),parameter        :: one=1
INTEGER                   :: ln2,ln3,ln4,ln5,ln7,lnb,lnd
INTEGER                   :: nm
INTEGER, DIMENSION(0:n-1) :: jumble
REAL(DP),DIMENSION(0:n-1) :: w
real(dp)                  :: rfac
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 ddfft(n,rb,qb)!                                                [dfft]
rfac=one/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 continue
CALL fftco(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd)
call  gfft(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd,rb,qb)
end SUBROUTINE dcfft

!=============================================================================
SUBROUTINE scfftp(n,js,ws,rb,qb)!                                        [cfft]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999; radix-7, 11, 13, included and user-provided parameter
!  arrays an option, 2011)
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(sp),dimension(0:n-1),intent(INOUT):: ws
REAL(sP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
real(sp),parameter        :: one=1
INTEGER                   :: ln2,ln3,ln4,ln5,ln7,lnb,lnd
INTEGER                   :: nm
real(sp)                  :: rfac
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 sdfftp(n,js,ws,rb,qb)!                                         [dfft]
rfac=one/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 continue
CALL fftcop(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd)
call   gfft(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd,rb,qb)
end SUBROUTINE scfftp
!=============================================================================
SUBROUTINE dcfftp(n,js,ws,rb,qb)!                                        [cfft]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
! (Revised for f90, 1999; radix-7, 11, 13, included and user-provided parameter
!  arrays an option, 2011)
! Double precision versions of cfft, dfft
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(dp),dimension(0:n-1),intent(INOUT):: ws
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: rb,qb
!------------------------------------------------------------------------------
real(dp),parameter        :: one=1
INTEGER                   :: ln2,ln3,ln4,ln5,ln7,lnb,lnd
INTEGER                   :: nm
real(dp)                  :: rfac
!=============================================================================
nm=n-1
GOTO 300
ENTRY	 ddfftp(n,js,ws,rb,qb)!                                         [dfft]
rfac=one/n
nm=n-1
!  FOR FOURIER SYNTHESIS, SCALE, AND REVERSE THE ORDER OF WAVENUMBERS:
rb(0)=rb(0)*rfac; rb(1:nm)=rb(nm:1:-1)*rfac
qb(0)=qb(0)*rfac; qb(1:nm)=qb(nm:1:-1)*rfac
300 continue
CALL fftcop(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd)
call   gfft(n,js,ws,ln2,ln3,ln4,ln5,ln7,lnb,lnd,rb,qb)
end SUBROUTINE dcfftp

!=============================================================================
subroutine sgfft(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd,r,q)!           [gfft]
!=============================================================================
integer,                  intent(IN   ):: n
integer, dimension(0:n-1),intent(IN   ):: jumble
real(sp),dimension(0:n-1),intent(IN   ):: w
integer,                  intent(IN   ):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
real(sp),dimension(0:n-1),intent(INOUT):: r,q
!-----------------------------------------------------------------------------
real(sp),parameter:: one=1,half=one/2
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,k5,k6,k7,k8,k9,ka,kb,kc, &
           ma,ma2,ma3,ma4,ma5,ma7,mab,mad,mb,mb2, &
           jmb,jmb2,jmb3,jmb4,jmb5,jmb6, &
           nze
REAL(sp):: r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,ra,rb,rc, &
           q0,q1,q2,q3,q4,q5,q6,q7,q8,q9,qa,qb,qc,  &
           rf1,rf2,rf3,rf4,rf5,rf6,rf7,rf8,rf9,rfa,rfb,rfc,&
           qf1,qf2,qf3,qf4,qf5,qf6,qf7,qf8,qf9,qfa,qfb,qfc, &
           rep,rze,rzc,ret,rec,rth,rtc,rio,ric,rka,rkc,rla,rlc, &
           qep,qze,qet,qth,qio,qka,qla, &
           t1,t2,t3,t4,t5,t6
!=============================================================================
nm=n-1
nh=n/2  
! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t1=r(i); r(i)=r(j); r(j)=t1
      t1=q(i); q(i)=q(j); q(j)=t1
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
         r0=r(k0); r1=r(k1);  r2=r(k2);  r3=r(k3)
         q0=q(k0); q1=q(k1);  q2=q(k2);  q3=q(k3)
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
         r(k3) =r1    -q2	        ! r3
         r(k1) =r1    +q2	        ! r1
         q2    =q0    +q1		! q20
         q1    =q0    -q1		! q21
         q(k0) =q2    +r2	        ! q0
         q(k2) =q2    -r2	        ! q2
         r(k2) =r0    -q3	        ! r2
         r(k0) =r0    +q3	        ! r0
         q(k3) =q1    -t1	        ! q3
         q(k1) =q1    +t1	        ! q1
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
         r0=r(k0);     q0=q(k0)
         r1=r(k1);     q1=q(k1)
         t1   =r1*qf1+q1*rf1 ! q11
         q1   =r1*rf1-q1*qf1 ! r11
         r(k1)=r0    -q1     ! r1
         r(k0)=r0    +q1     ! r0
         q(k1)=q0    -t1     ! q1
         q(k0)=q0    +t1     ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-half;  rec=3*half;  qep=half*SQRT(3*one)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j;     k1=k0+ma;    k2=k1+ma
         r1=r(k1);  q1=q(k1)
         r2=r(k2);  q2=q(k2)
         t1    = r2*qf2+q2 *rf2	        ! r12
         q2    = r2*rf2-q2 *qf2	        ! q12
         r2    = r1*qf1+q1 *rf1	        ! q11
         r1    = r1*rf1-q1 *qf1	        ! r11
         q1    = r2    +t1		! q21
         r2    =(r2    -t1)*qep	        ! r22
         t1    = r1    +q2		! r21
         r1    =(r1    -q2)*qep	        ! q22
         r(k0) = r(k0)+t1	        ! r0
         q(k0) = q(k0)+q1	        ! q0
         t1    = r(k0)-t1 *rec	        ! r21
         q1    = q(k0)-q1 *rec	        ! q21
         q(k2) = q1    -r1	        ! q2
         q(k1) = q1    +r1	        ! q1
         r(k1) = t1    -r2	        ! r1
         r(k2) = t1    +r2	        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5;  rze=w(nze); qze=w(nh+nze); rzc=one-rze
   ret=rze*rze-qze*qze;  qet=2*rze*qze; rec=one-ret
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
            r1=r(k1); r2=r(k2); r3=r(k3); r4=r(k4)
            q1=q(k1); q2=q(k2); q3=q(k3); q4=q(k4)
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
            r(k0) =r(k0)+r1    +r2            ! r0
            q(k0) =q(k0)+q1    +q2            ! q0
            t1    =r4*qze+r3*qet              ! r34
            r3    =r3*qze-r4*qet              ! r33
            r4    =r(k0)-r2*rzc-r1*rec        ! r32
            r1    =r(k0)-r1*rzc-r2*rec        ! r31
            r(k2) =r4    +r3	              ! r2
            r(k3) =r4    -r3	              ! r3
            r(k4) =r1    +t1	              ! r4
            r(k1) =r1    -t1	              ! r1
            t1    =q(k0)-q1*rzc-q2*rec        ! q31
            q2    =q(k0)-q2*rzc-q1*rec        ! q32
            q1    =q3*qze-q4*qet              ! q33
            q4    =q4*qze+q3*qet              ! q34
            q(k3) =q2    +q1	              ! q3
            q(k2) =q2    -q1	              ! q2
            q(k1) =t1    +q4	              ! q1
            q(k4) =t1    -q4	              ! q4
         ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF

if(ln7 > 0)then
!  RADIX 7
   nze=n/7
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta etc
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta etc
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta etc.
   do L=1,Ln7
      mb=mb/7; ma7=ma*7
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         rf5=rf2*rf3-qf2*qf3; qf5=rf2*qf3+qf2*rf3
         rf6=rf3*rf3-qf3*qf3; qf6=2*rf3*qf3
         do i=0,nm,ma7
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=r6*rf6-q6*qf6           ! r16
            q6=r6*qf6+q6*rf6           ! q16
            r6=r1-q1                   ! r26
            r1=r1+q1                   ! r21
            q1=t1+q6                   ! q21
            q6=t1-q6                   ! q26
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=r5*rf5-q5*qf5           ! r15
            q5=r5*qf5+q5*rf5           ! q15
            r5=r2-q2                   ! r25
            r2=r2+q2                   ! r22
            q2=t2+q5                   ! q22
            q5=t2-q5                   ! q25
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=r4*rf4-q4*qf4           ! r14
            q4=r4*qf4+q4*rf4           ! q14
            r4=r3-q3                   ! r24
            r3=r3+q3                   ! r23
            q3=t3+q4                   ! q23
            q4=t3-q4                   ! q24
            r0=r0+r1+r2+r3             ! r0
            q0=q0+q1+q2+q3             ! q0
            t1=   r6*qze+r5*qet+r4*qth ! r36
            t2=   r6*qet-r5*qth-r4*qze ! r35
            t3=   r6*qth-r5*qze+r4*qet ! r34
            r4=r0-r1*rtc-r2*rzc-r3*rec ! r33
            r5=r0-r1*rec-r2*rtc-r3*rzc ! r32
            r6=r0-r1*rzc-r2*rec-r3*rtc ! r31
            r1=   q6*qze+q5*qet+q4*qth ! q36
            r2=   q6*qet-q5*qth-q4*qze ! q35
            r3=   q6*qth-q5*qze+q4*qet ! q34
            q4=q0-q1*rtc-q2*rzc-q3*rec ! q33
            q5=q0-q1*rec-q2*rtc-q3*rzc ! q32
            q6=q0-q1*rzc-q2*rec-q3*rtc ! q31
            r(k0)=r0                   ! r0
            r(k1)=r6-r1                ! r1
            r(k6)=r6+r1                ! r6
            r(k2)=r5-r2                ! r2
            r(k5)=r5+r2                ! r5
            r(k3)=r4-r3                ! r3
            r(k4)=r4+r3                ! r4
            q(k0)=q0                   ! q0
            q(k1)=q6+t1                ! q1
            q(k6)=q6-t1                ! q6
            q(k2)=q5+t2                ! q2
            q(k5)=q5-t2                ! q5
            q(k3)=q4+t3                ! q3
            q(k4)=q4-t3                ! q4
         enddo
      enddo
      ma=ma7
   enddo
endif
if(lnb > 0)then
!  RADIX 11
   nze=n/11
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta 
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta
   rio=ret*ret-qet*qet; qio=2*ret*qet;       ric=one-rio ! <- iota
   rka=ret*rth-qet*qth; qka=ret*qth+qet*rth; rkc=one-rka ! <- kappa
   do L=1,Lnb
      mb=mb/11; mab=ma*11
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3; jmb4=jmb*4; jmb5=jmb*5
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=w(jmb4);         qf4=w(nh+jmb4)
         rf5=w(jmb5);         qf5=w(nh+jmb5)
         rf6=rf3*rf3-qf3*qf3; qf6=2*rf3*qf3
         rf7=rf3*rf4-qf3*qf4; qf7=rf3*qf4+qf3*rf4
         rf8=rf4*rf4-qf4*qf4; qf8=2*rf4*qf4
         rf9=rf4*rf5-qf4*qf5; qf9=rf4*qf5+qf4*rf5
         rfa=rf5*rf5-qf5*qf5; qfa=2*rf5*qf5
         do i=0,nm,mab
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            k7=k6+ma; k8=k7+ma; k9=k8+ma; ka=k9+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            r7=r(k7);r8=r(k8);r9=r(k9);ra=r(ka)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            q7=q(k7);q8=q(k8);q9=q(k9);qa=q(ka)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=ra*rfa-qa*qfa           ! r1a
            qa=ra*qfa+qa*rfa           ! q1a
            ra=r1-q1                   ! r2a
            r1=r1+q1                   ! r21
            q1=t1+qa                   ! q21
            qa=t1-qa                   ! q2a
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=r9*rf9-q9*qf9           ! r19
            q9=r9*qf9+q9*rf9           ! q19
            r9=r2-q2                   ! r29
            r2=r2+q2                   ! r22
            q2=t2+q9                   ! q22
            q9=t2-q9                   ! q29
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=r8*rf8-q8*qf8           ! r18
            q8=r8*qf8+q8*rf8           ! q18
            r8=r3-q3                   ! r28
            r3=r3+q3                   ! r23
            q3=t3+q8                   ! q23
            q8=t3-q8                   ! q28
            t4=r4*qf4+q4*rf4           ! q14
            r4=r4*rf4-q4*qf4           ! r14
            q4=r7*rf7-q7*qf7           ! r17
            q7=r7*qf7+q7*rf7           ! q17
            r7=r4-q4                   ! r27
            r4=r4+q4                   ! r24
            q4=t4+q7                   ! q24
            q7=t4-q7                   ! q27
            t5=r5*qf5+q5*rf5           ! q15
            r5=r5*rf5-q5*qf5           ! r15
            q5=r6*rf6-q6*qf6           ! r16
            q6=r6*qf6+q6*rf6           ! q16
            r6=r5-q5                   ! r26
            r5=r5+q5                   ! r25
            q5=t5+q6                   ! q25
            q6=t5-q6                   ! q26
            r0=r0+r1+r2+r3+r4+r5       ! r0
            q0=q0+q1+q2+q3+q4+q5       ! q0
            t1=   ra*qze+r9*qet+r8*qth+r7*qio+r6*qka
            t2=   ra*qet+r9*qio-r8*qka-r7*qth-r6*qze
            t3=   ra*qth-r9*qka-r8*qet+r7*qze+r6*qio
            t4=   ra*qio-r9*qth+r8*qze+r7*qka-r6*qet
            t5=   ra*qka-r9*qze+r8*qio-r7*qet+r6*qth
            r6=r0-r1*rkc-r2*rzc-r3*ric-r4*rec-r5*rtc
            r7=r0-r1*ric-r2*rtc-r3*rzc-r4*rkc-r5*rec
            r8=r0-r1*rtc-r2*rkc-r3*rec-r4*rzc-r5*ric
            r9=r0-r1*rec-r2*ric-r3*rkc-r4*rtc-r5*rzc
            ra=r0-r1*rzc-r2*rec-r3*rtc-r4*ric-r5*rkc
            r1=   qa*qze+q9*qet+q8*qth+q7*qio+q6*qka
            r2=   qa*qet+q9*qio-q8*qka-q7*qth-q6*qze
            r3=   qa*qth-q9*qka-q8*qet+q7*qze+q6*qio
            r4=   qa*qio-q9*qth+q8*qze+q7*qka-q6*qet
            r5=   qa*qka-q9*qze+q8*qio-q7*qet+q6*qth
            q6=q0-q1*rkc-q2*rzc-q3*ric-q4*rec-q5*rtc
            q7=q0-q1*ric-q2*rtc-q3*rzc-q4*rkc-q5*rec
            q8=q0-q1*rtc-q2*rkc-q3*rec-q4*rzc-q5*ric
            q9=q0-q1*rec-q2*ric-q3*rkc-q4*rtc-q5*rzc
            qa=q0-q1*rzc-q2*rec-q3*rtc-q4*ric-q5*rkc
            r(k0)=r0                   ! r0
            r(k1)=ra-r1                ! r1
            r(ka)=ra+r1                ! ra
            r(k2)=r9-r2                ! r2
            r(k9)=r9+r2                ! r9
            r(k3)=r8-r3                ! r3
            r(k8)=r8+r3                ! r8
            r(k4)=r7-r4                ! r4
            r(k7)=r7+r4                ! r7
            r(k5)=r6-r5                ! r5
            r(k6)=r6+r5                ! r6
            q(k0)=q0                   ! q0
            q(k1)=qa+t1                ! q1
            q(ka)=qa-t1                ! qa
            q(k2)=q9+t2                ! q2
            q(k9)=q9-t2                ! q9
            q(k3)=q8+t3                ! q3
            q(k8)=q8-t3                ! q8
            q(k4)=q7+t4                ! q4
            q(k7)=q7-t4                ! q7
            q(k5)=q6+t5                ! q5
            q(k6)=q6-t5                ! q6
         enddo
      enddo
      ma=mab
   enddo
endif
if(lnd > 0)then
!  RADIX 13
   nze=n/13
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta 
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta
   rio=ret*ret-qet*qet; qio=2*ret*qet;       ric=one-rio ! <- iota
   rka=ret*rth-qet*qth; qka=ret*qth+qet*rth; rkc=one-rka ! <- kappa
   rla=rth*rth-qth*qth; qla=2*rth*qth;       rlc=one-rla ! <- lambda
   do L=1,Lnd
      mb=mb/13; mad=ma*13
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3; jmb4=jmb*4; jmb5=jmb*5; jmb6=jmb*6
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=w(jmb4);         qf4=w(nh+jmb4)
         rf5=w(jmb5);         qf5=w(nh+jmb5)
         rf6=w(jmb6);         qf6=w(nh+jmb6)
         rf7=rf3*rf4-qf3*qf4; qf7=rf3*qf4+qf3*rf4
         rf8=rf4*rf4-qf4*qf4; qf8=2*rf4*qf4
         rf9=rf4*rf5-qf4*qf5; qf9=rf4*qf5+qf4*rf5
         rfa=rf5*rf5-qf5*qf5; qfa=2*rf5*qf5
         rfb=rf5*rf6-qf5*qf6; qfb=rf5*qf6+qf5*rf6
         rfc=rf6*rf6-qf6*qf6; qfc=2*rf6*qf6
         do i=0,nm,mad
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            k7=k6+ma; k8=k7+ma; k9=k8+ma; ka=k9+ma; kb=ka+ma; kc=kb+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            r7=r(k7);r8=r(k8);r9=r(k9);ra=r(ka);rb=r(kb);rc=r(kc)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            q7=q(k7);q8=q(k8);q9=q(k9);qa=q(ka);qb=q(kb);qc=q(kc)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=rc*rfc-qc*qfc           ! r1c
            qc=rc*qfc+qc*rfc           ! q1c
            rc=r1-q1                   ! r2c
            r1=r1+q1                   ! r21
            q1=t1+qc                   ! q21
            qc=t1-qc                   ! q2c
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=rb*rfb-qb*qfb           ! r1b
            qb=rb*qfb+qb*rfb           ! q1b
            rb=r2-q2                   ! r2b
            r2=r2+q2                   ! r22
            q2=t2+qb                   ! q22
            qb=t2-qb                   ! q2b
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=ra*rfa-qa*qfa           ! r1a
            qa=ra*qfa+qa*rfa           ! q1a
            ra=r3-q3                   ! r2a
            r3=r3+q3                   ! r23
            q3=t3+qa                   ! q23
            qa=t3-qa                   ! q2a
            t4=r4*qf4+q4*rf4           ! q14
            r4=r4*rf4-q4*qf4           ! r14
            q4=r9*rf9-q9*qf9           ! r19
            q9=r9*qf9+q9*rf9           ! q19
            r9=r4-q4                   ! r29
            r4=r4+q4                   ! r24
            q4=t4+q9                   ! q24
            q9=t4-q9                   ! q29
            t5=r5*qf5+q5*rf5           ! q15
            r5=r5*rf5-q5*qf5           ! r15
            q5=r8*rf8-q8*qf8           ! r18
            q8=r8*qf8+q8*rf8           ! q18
            r8=r5-q5                   ! r28
            r5=r5+q5                   ! r25
            q5=t5+q8                   ! q25
            q8=t5-q8                   ! q28
            t6=r6*qf6+q6*rf6           ! q16
            r6=r6*rf6-q6*qf6           ! r16
            q6=r7*rf7-q7*qf7           ! r17
            q7=r7*qf7+q7*rf7           ! q17
            r7=r6-q6                   ! r27
            r6=r6+q6                   ! r26
            q6=t6+q7                   ! q26
            q7=t6-q7                   ! q27
            r0=r0+r1+r2+r3+r4+r5+r6       ! r0
            q0=q0+q1+q2+q3+q4+q5+q6       ! q0
            t1=   rc*qze+rb*qet+ra*qth+r9*qio+r8*qka+r7*qla
            t2=   rc*qet+rb*qio+ra*qla-r9*qka-r8*qth-r7*qze
            t3=   rc*qth+rb*qla-ra*qio-r9*qze+r8*qet+r7*qka
            t4=   rc*qio-rb*qka-ra*qze+r9*qth-r8*qla-r7*qet
            t5=   rc*qka-rb*qth+ra*qet-r9*qla-r8*qze+r7*qio
            t6=   rc*qla-rb*qze+ra*qka-r9*qet+r8*qio-r7*qth
            r7=r0-r1*rlc-r2*rzc-r3*rkc-r4*rec-r5*ric-r6*rtc
            r8=r0-r1*rkc-r2*rtc-r3*rec-r4*rlc-r5*rzc-r6*ric
            r9=r0-r1*ric-r2*rkc-r3*rzc-r4*rtc-r5*rlc-r6*rec
            ra=r0-r1*rtc-r2*rlc-r3*ric-r4*rzc-r5*rec-r6*rkc
            rb=r0-r1*rec-r2*ric-r3*rlc-r4*rkc-r5*rtc-r6*rzc
            rc=r0-r1*rzc-r2*rec-r3*rtc-r4*ric-r5*rkc-r6*rlc
            r1=   qc*qze+qb*qet+qa*qth+q9*qio+q8*qka+q7*qla
            r2=   qc*qet+qb*qio+qa*qla-q9*qka-q8*qth-q7*qze
            r3=   qc*qth+qb*qla-qa*qio-q9*qze+q8*qet+q7*qka
            r4=   qc*qio-qb*qka-qa*qze+q9*qth-q8*qla-q7*qet
            r5=   qc*qka-qb*qth+qa*qet-q9*qla-q8*qze+q7*qio
            r6=   qc*qla-qb*qze+qa*qka-q9*qet+q8*qio-q7*qth
            q7=q0-q1*rlc-q2*rzc-q3*rkc-q4*rec-q5*ric-q6*rtc
            q8=q0-q1*rkc-q2*rtc-q3*rec-q4*rlc-q5*rzc-q6*ric
            q9=q0-q1*ric-q2*rkc-q3*rzc-q4*rtc-q5*rlc-q6*rec
            qa=q0-q1*rtc-q2*rlc-q3*ric-q4*rzc-q5*rec-q6*rkc
            qb=q0-q1*rec-q2*ric-q3*rlc-q4*rkc-q5*rtc-q6*rzc
            qc=q0-q1*rzc-q2*rec-q3*rtc-q4*ric-q5*rkc-q6*rlc
            r(k0)=r0                   ! r0
            r(k1)=rc-r1                ! r1
            r(kc)=rc+r1                ! rc
            r(k2)=rb-r2                ! r2
            r(kb)=rb+r2                ! rb
            r(k3)=ra-r3                ! r3
            r(ka)=ra+r3                ! ra
            r(k4)=r9-r4                ! r4
            r(k9)=r9+r4                ! r9
            r(k5)=r8-r5                ! r5
            r(k8)=r8+r5                ! r8
            r(k6)=r7-r6                ! r6
            r(k7)=r7+r6                ! r7
            q(k0)=q0                   ! q0
            q(k1)=qc+t1                ! q1
            q(kc)=qc-t1                ! qc
            q(k2)=qb+t2                ! q2
            q(kb)=qb-t2                ! qb
            q(k3)=qa+t3                ! q3
            q(ka)=qa-t3                ! qa
            q(k4)=q9+t4                ! q4
            q(k9)=q9-t4                ! q9
            q(k5)=q8+t5                ! q5
            q(k8)=q8-t5                ! q8
            q(k6)=q7+t6                ! q6
            q(k7)=q7-t6                ! q7
         enddo
      enddo
      ma=mad
   enddo
endif
END SUBROUTINE sgfft

!=============================================================================
subroutine dgfft(n,jumble,w,ln2,ln3,ln4,ln5,ln7,lnb,lnd,r,q)!           [gfft]
!=============================================================================
integer,                  intent(IN   ):: n
integer, dimension(0:n-1),intent(IN   ):: jumble
real(dp),dimension(0:n-1),intent(IN   ):: w
integer,                  intent(IN   ):: ln2,ln3,ln4,ln5,ln7,lnb,lnd
real(dp),dimension(0:n-1),intent(INOUT):: r,q
!-----------------------------------------------------------------------------
real(dp),parameter:: one=1,half=one/2
INTEGER :: nm,nh,i,j,l,k0,k1,k2,k3,k4,k5,k6,k7,k8,k9,ka,kb,kc, &
           ma,ma2,ma3,ma4,ma5,ma7,mab,mad,mb,mb2, &
           jmb,jmb2,jmb3,jmb4,jmb5,jmb6, &
           nze
REAL(dp):: r0,r1,r2,r3,r4,r5,r6,r7,r8,r9,ra,rb,rc, &
           q0,q1,q2,q3,q4,q5,q6,q7,q8,q9,qa,qb,qc,  &
           rf1,rf2,rf3,rf4,rf5,rf6,rf7,rf8,rf9,rfa,rfb,rfc,&
           qf1,qf2,qf3,qf4,qf5,qf6,qf7,qf8,qf9,qfa,qfb,qfc, &
           rep,rze,rzc,ret,rec,rth,rtc,rio,ric,rka,rkc,rla,rlc, &
           qep,qze,qet,qth,qio,qka,qla, &
           t1,t2,t3,t4,t5,t6
!=============================================================================
nm=n-1
nh=n/2  
! PERMUTE THE DATA:
DO i=1,nm
   j=jumble(i)
   IF(j > i)THEN
      t1=r(i); r(i)=r(j); r(j)=t1
      t1=q(i); q(i)=q(j); q(j)=t1
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
         r0=r(k0); r1=r(k1);  r2=r(k2);  r3=r(k3)
         q0=q(k0); q1=q(k1);  q2=q(k2);  q3=q(k3)
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
         r(k3) =r1    -q2	        ! r3
         r(k1) =r1    +q2	        ! r1
         q2    =q0    +q1		! q20
         q1    =q0    -q1		! q21
         q(k0) =q2    +r2	        ! q0
         q(k2) =q2    -r2	        ! q2
         r(k2) =r0    -q3	        ! r2
         r(k0) =r0    +q3	        ! r0
         q(k3) =q1    -t1	        ! q3
         q(k1) =q1    +t1	        ! q1
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
         r0=r(k0);     q0=q(k0)
         r1=r(k1);     q1=q(k1)
         t1   =r1*qf1+q1*rf1 ! q11
         q1   =r1*rf1-q1*qf1 ! r11
         r(k1)=r0    -q1     ! r1
         r(k0)=r0    +q1     ! r0
         q(k1)=q0    -t1     ! q1
         q(k0)=q0    +t1     ! q0
      ENDDO
   ENDDO
   ma=ma2
ENDIF
!  RADIX 3
rep=-half;  rec=3*half;  qep=half*SQRT(3*one)
DO l=1,ln3
   mb=mb/3; ma3=ma*3
   DO j=0,ma-1
      jmb=j*mb
      rf1=w(jmb);          qf1=w(nh+jmb)
      rf2=rf1*rf1-qf1*qf1; qf2=2*rf1*qf1
      DO i=0,nm,ma3
         k0=i+j;     k1=k0+ma;    k2=k1+ma
         r1=r(k1);  q1=q(k1)
         r2=r(k2);  q2=q(k2)
         t1    = r2*qf2+q2 *rf2	        ! r12
         q2    = r2*rf2-q2 *qf2	        ! q12
         r2    = r1*qf1+q1 *rf1	        ! q11
         r1    = r1*rf1-q1 *qf1	        ! r11
         q1    = r2    +t1		! q21
         r2    =(r2    -t1)*qep	        ! r22
         t1    = r1    +q2		! r21
         r1    =(r1    -q2)*qep	        ! q22
         r(k0) = r(k0)+t1	        ! r0
         q(k0) = q(k0)+q1	        ! q0
         t1    = r(k0)-t1 *rec	        ! r21
         q1    = q(k0)-q1 *rec	        ! q21
         q(k2) = q1    -r1	        ! q2
         q(k1) = q1    +r1	        ! q1
         r(k1) = t1    -r2	        ! r1
         r(k2) = t1    +r2	        ! r2
      ENDDO
   ENDDO
   ma=ma3
ENDDO

IF(ln5 > 0)THEN
!  RADIX 5
   nze=n/5;  rze=w(nze); qze=w(nh+nze); rzc=one-rze
   ret=rze*rze-qze*qze;  qet=2*rze*qze; rec=one-ret
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
            r1=r(k1); r2=r(k2); r3=r(k3); r4=r(k4)
            q1=q(k1); q2=q(k2); q3=q(k3); q4=q(k4)
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
            r(k0) =r(k0)+r1    +r2            ! r0
            q(k0) =q(k0)+q1    +q2            ! q0
            t1    =r4*qze+r3*qet              ! r34
            r3    =r3*qze-r4*qet              ! r33
            r4    =r(k0)-r2*rzc-r1*rec        ! r32
            r1    =r(k0)-r1*rzc-r2*rec        ! r31
            r(k2) =r4    +r3	              ! r2
            r(k3) =r4    -r3	              ! r3
            r(k4) =r1    +t1	              ! r4
            r(k1) =r1    -t1	              ! r1
            t1    =q(k0)-q1*rzc-q2*rec        ! q31
            q2    =q(k0)-q2*rzc-q1*rec        ! q32
            q1    =q3*qze-q4*qet              ! q33
            q4    =q4*qze+q3*qet              ! q34
            q(k3) =q2    +q1	              ! q3
            q(k2) =q2    -q1	              ! q2
            q(k1) =t1    +q4	              ! q1
            q(k4) =t1    -q4	              ! q4
         ENDDO
      ENDDO
      ma=ma5
   ENDDO
ENDIF

if(ln7 > 0)then
!  RADIX 7
   nze=n/7
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta etc
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta etc
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta etc.
   do L=1,Ln7
      mb=mb/7; ma7=ma*7
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=rf2*rf2-qf2*qf2; qf4=2*rf2*qf2
         rf5=rf2*rf3-qf2*qf3; qf5=rf2*qf3+qf2*rf3
         rf6=rf3*rf3-qf3*qf3; qf6=2*rf3*qf3
         do i=0,nm,ma7
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=r6*rf6-q6*qf6           ! r16
            q6=r6*qf6+q6*rf6           ! q16
            r6=r1-q1                   ! r26
            r1=r1+q1                   ! r21
            q1=t1+q6                   ! q21
            q6=t1-q6                   ! q26
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=r5*rf5-q5*qf5           ! r15
            q5=r5*qf5+q5*rf5           ! q15
            r5=r2-q2                   ! r25
            r2=r2+q2                   ! r22
            q2=t2+q5                   ! q22
            q5=t2-q5                   ! q25
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=r4*rf4-q4*qf4           ! r14
            q4=r4*qf4+q4*rf4           ! q14
            r4=r3-q3                   ! r24
            r3=r3+q3                   ! r23
            q3=t3+q4                   ! q23
            q4=t3-q4                   ! q24
            r0=r0+r1+r2+r3             ! r0
            q0=q0+q1+q2+q3             ! q0
            t1=   r6*qze+r5*qet+r4*qth ! r36
            t2=   r6*qet-r5*qth-r4*qze ! r35
            t3=   r6*qth-r5*qze+r4*qet ! r34
            r4=r0-r1*rtc-r2*rzc-r3*rec ! r33
            r5=r0-r1*rec-r2*rtc-r3*rzc ! r32
            r6=r0-r1*rzc-r2*rec-r3*rtc ! r31
            r1=   q6*qze+q5*qet+q4*qth ! q36
            r2=   q6*qet-q5*qth-q4*qze ! q35
            r3=   q6*qth-q5*qze+q4*qet ! q34
            q4=q0-q1*rtc-q2*rzc-q3*rec ! q33
            q5=q0-q1*rec-q2*rtc-q3*rzc ! q32
            q6=q0-q1*rzc-q2*rec-q3*rtc ! q31
            r(k0)=r0                   ! r0
            r(k1)=r6-r1                ! r1
            r(k6)=r6+r1                ! r6
            r(k2)=r5-r2                ! r2
            r(k5)=r5+r2                ! r5
            r(k3)=r4-r3                ! r3
            r(k4)=r4+r3                ! r4
            q(k0)=q0                   ! q0
            q(k1)=q6+t1                ! q1
            q(k6)=q6-t1                ! q6
            q(k2)=q5+t2                ! q2
            q(k5)=q5-t2                ! q5
            q(k3)=q4+t3                ! q3
            q(k4)=q4-t3                ! q4
         enddo
      enddo
      ma=ma7
   enddo
endif
if(lnb > 0)then
!  RADIX 11
   nze=n/11
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta 
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta
   rio=ret*ret-qet*qet; qio=2*ret*qet;       ric=one-rio ! <- iota
   rka=ret*rth-qet*qth; qka=ret*qth+qet*rth; rkc=one-rka ! <- kappa
   do L=1,Lnb
      mb=mb/11; mab=ma*11
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3; jmb4=jmb*4; jmb5=jmb*5
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=w(jmb4);         qf4=w(nh+jmb4)
         rf5=w(jmb5);         qf5=w(nh+jmb5)
         rf6=rf3*rf3-qf3*qf3; qf6=2*rf3*qf3
         rf7=rf3*rf4-qf3*qf4; qf7=rf3*qf4+qf3*rf4
         rf8=rf4*rf4-qf4*qf4; qf8=2*rf4*qf4
         rf9=rf4*rf5-qf4*qf5; qf9=rf4*qf5+qf4*rf5
         rfa=rf5*rf5-qf5*qf5; qfa=2*rf5*qf5
         do i=0,nm,mab
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            k7=k6+ma; k8=k7+ma; k9=k8+ma; ka=k9+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            r7=r(k7);r8=r(k8);r9=r(k9);ra=r(ka)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            q7=q(k7);q8=q(k8);q9=q(k9);qa=q(ka)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=ra*rfa-qa*qfa           ! r1a
            qa=ra*qfa+qa*rfa           ! q1a
            ra=r1-q1                   ! r2a
            r1=r1+q1                   ! r21
            q1=t1+qa                   ! q21
            qa=t1-qa                   ! q2a
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=r9*rf9-q9*qf9           ! r19
            q9=r9*qf9+q9*rf9           ! q19
            r9=r2-q2                   ! r29
            r2=r2+q2                   ! r22
            q2=t2+q9                   ! q22
            q9=t2-q9                   ! q29
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=r8*rf8-q8*qf8           ! r18
            q8=r8*qf8+q8*rf8           ! q18
            r8=r3-q3                   ! r28
            r3=r3+q3                   ! r23
            q3=t3+q8                   ! q23
            q8=t3-q8                   ! q28
            t4=r4*qf4+q4*rf4           ! q14
            r4=r4*rf4-q4*qf4           ! r14
            q4=r7*rf7-q7*qf7           ! r17
            q7=r7*qf7+q7*rf7           ! q17
            r7=r4-q4                   ! r27
            r4=r4+q4                   ! r24
            q4=t4+q7                   ! q24
            q7=t4-q7                   ! q27
            t5=r5*qf5+q5*rf5           ! q15
            r5=r5*rf5-q5*qf5           ! r15
            q5=r6*rf6-q6*qf6           ! r16
            q6=r6*qf6+q6*rf6           ! q16
            r6=r5-q5                   ! r26
            r5=r5+q5                   ! r25
            q5=t5+q6                   ! q25
            q6=t5-q6                   ! q26
            r0=r0+r1+r2+r3+r4+r5       ! r0
            q0=q0+q1+q2+q3+q4+q5       ! q0
            t1=   ra*qze+r9*qet+r8*qth+r7*qio+r6*qka
            t2=   ra*qet+r9*qio-r8*qka-r7*qth-r6*qze
            t3=   ra*qth-r9*qka-r8*qet+r7*qze+r6*qio
            t4=   ra*qio-r9*qth+r8*qze+r7*qka-r6*qet
            t5=   ra*qka-r9*qze+r8*qio-r7*qet+r6*qth
            r6=r0-r1*rkc-r2*rzc-r3*ric-r4*rec-r5*rtc
            r7=r0-r1*ric-r2*rtc-r3*rzc-r4*rkc-r5*rec
            r8=r0-r1*rtc-r2*rkc-r3*rec-r4*rzc-r5*ric
            r9=r0-r1*rec-r2*ric-r3*rkc-r4*rtc-r5*rzc
            ra=r0-r1*rzc-r2*rec-r3*rtc-r4*ric-r5*rkc
            r1=   qa*qze+q9*qet+q8*qth+q7*qio+q6*qka
            r2=   qa*qet+q9*qio-q8*qka-q7*qth-q6*qze
            r3=   qa*qth-q9*qka-q8*qet+q7*qze+q6*qio
            r4=   qa*qio-q9*qth+q8*qze+q7*qka-q6*qet
            r5=   qa*qka-q9*qze+q8*qio-q7*qet+q6*qth
            q6=q0-q1*rkc-q2*rzc-q3*ric-q4*rec-q5*rtc
            q7=q0-q1*ric-q2*rtc-q3*rzc-q4*rkc-q5*rec
            q8=q0-q1*rtc-q2*rkc-q3*rec-q4*rzc-q5*ric
            q9=q0-q1*rec-q2*ric-q3*rkc-q4*rtc-q5*rzc
            qa=q0-q1*rzc-q2*rec-q3*rtc-q4*ric-q5*rkc
            r(k0)=r0                   ! r0
            r(k1)=ra-r1                ! r1
            r(ka)=ra+r1                ! ra
            r(k2)=r9-r2                ! r2
            r(k9)=r9+r2                ! r9
            r(k3)=r8-r3                ! r3
            r(k8)=r8+r3                ! r8
            r(k4)=r7-r4                ! r4
            r(k7)=r7+r4                ! r7
            r(k5)=r6-r5                ! r5
            r(k6)=r6+r5                ! r6
            q(k0)=q0                   ! q0
            q(k1)=qa+t1                ! q1
            q(ka)=qa-t1                ! qa
            q(k2)=q9+t2                ! q2
            q(k9)=q9-t2                ! q9
            q(k3)=q8+t3                ! q3
            q(k8)=q8-t3                ! q8
            q(k4)=q7+t4                ! q4
            q(k7)=q7-t4                ! q7
            q(k5)=q6+t5                ! q5
            q(k6)=q6-t5                ! q6
         enddo
      enddo
      ma=mab
   enddo
endif
if(lnd > 0)then
!  RADIX 13
   nze=n/13
   rze=w(nze);          qze=w(nh+nze);       rzc=one-rze ! <- zeta
   ret=rze*rze-qze*qze; qet=2*rze*qze;       rec=one-ret ! <- eta 
   rth=rze*ret-qze*qet; qth=rze*qet+qze*ret; rtc=one-rth ! <- theta
   rio=ret*ret-qet*qet; qio=2*ret*qet;       ric=one-rio ! <- iota
   rka=ret*rth-qet*qth; qka=ret*qth+qet*rth; rkc=one-rka ! <- kappa
   rla=rth*rth-qth*qth; qla=2*rth*qth;       rlc=one-rla ! <- lambda
   do L=1,Lnd
      mb=mb/13; mad=ma*13
      do j=0,ma-1
         jmb=j*mb; jmb2=jmb*2; jmb3=jmb*3; jmb4=jmb*4; jmb5=jmb*5; jmb6=jmb*6
         rf1=w(jmb);          qf1=w(nh+jmb)
         rf2=w(jmb2);         qf2=w(nh+jmb2)
         rf3=w(jmb3);         qf3=w(nh+jmb3)
         rf4=w(jmb4);         qf4=w(nh+jmb4)
         rf5=w(jmb5);         qf5=w(nh+jmb5)
         rf6=w(jmb6);         qf6=w(nh+jmb6)
         rf7=rf3*rf4-qf3*qf4; qf7=rf3*qf4+qf3*rf4
         rf8=rf4*rf4-qf4*qf4; qf8=2*rf4*qf4
         rf9=rf4*rf5-qf4*qf5; qf9=rf4*qf5+qf4*rf5
         rfa=rf5*rf5-qf5*qf5; qfa=2*rf5*qf5
         rfb=rf5*rf6-qf5*qf6; qfb=rf5*qf6+qf5*rf6
         rfc=rf6*rf6-qf6*qf6; qfc=2*rf6*qf6
         do i=0,nm,mad
            k0=i+j
            k1=k0+ma; k2=k1+ma; k3=k2+ma; k4=k3+ma; k5=k4+ma; k6=k5+ma
            k7=k6+ma; k8=k7+ma; k9=k8+ma; ka=k9+ma; kb=ka+ma; kc=kb+ma
            r0=r(k0)
            r1=r(k1);r2=r(k2);r3=r(k3);r4=r(k4);r5=r(k5);r6=r(k6)
            r7=r(k7);r8=r(k8);r9=r(k9);ra=r(ka);rb=r(kb);rc=r(kc)
            q0=q(k0)
            q1=q(k1);q2=q(k2);q3=q(k3);q4=q(k4);q5=q(k5);q6=q(k6)
            q7=q(k7);q8=q(k8);q9=q(k9);qa=q(ka);qb=q(kb);qc=q(kc)
            t1=r1*qf1+q1*rf1           ! q11
            r1=r1*rf1-q1*qf1           ! r11
            q1=rc*rfc-qc*qfc           ! r1c
            qc=rc*qfc+qc*rfc           ! q1c
            rc=r1-q1                   ! r2c
            r1=r1+q1                   ! r21
            q1=t1+qc                   ! q21
            qc=t1-qc                   ! q2c
            t2=r2*qf2+q2*rf2           ! q12
            r2=r2*rf2-q2*qf2           ! r12
            q2=rb*rfb-qb*qfb           ! r1b
            qb=rb*qfb+qb*rfb           ! q1b
            rb=r2-q2                   ! r2b
            r2=r2+q2                   ! r22
            q2=t2+qb                   ! q22
            qb=t2-qb                   ! q2b
            t3=r3*qf3+q3*rf3           ! q13
            r3=r3*rf3-q3*qf3           ! r13
            q3=ra*rfa-qa*qfa           ! r1a
            qa=ra*qfa+qa*rfa           ! q1a
            ra=r3-q3                   ! r2a
            r3=r3+q3                   ! r23
            q3=t3+qa                   ! q23
            qa=t3-qa                   ! q2a
            t4=r4*qf4+q4*rf4           ! q14
            r4=r4*rf4-q4*qf4           ! r14
            q4=r9*rf9-q9*qf9           ! r19
            q9=r9*qf9+q9*rf9           ! q19
            r9=r4-q4                   ! r29
            r4=r4+q4                   ! r24
            q4=t4+q9                   ! q24
            q9=t4-q9                   ! q29
            t5=r5*qf5+q5*rf5           ! q15
            r5=r5*rf5-q5*qf5           ! r15
            q5=r8*rf8-q8*qf8           ! r18
            q8=r8*qf8+q8*rf8           ! q18
            r8=r5-q5                   ! r28
            r5=r5+q5                   ! r25
            q5=t5+q8                   ! q25
            q8=t5-q8                   ! q28
            t6=r6*qf6+q6*rf6           ! q16
            r6=r6*rf6-q6*qf6           ! r16
            q6=r7*rf7-q7*qf7           ! r17
            q7=r7*qf7+q7*rf7           ! q17
            r7=r6-q6                   ! r27
            r6=r6+q6                   ! r26
            q6=t6+q7                   ! q26
            q7=t6-q7                   ! q27
            r0=r0+r1+r2+r3+r4+r5+r6       ! r0
            q0=q0+q1+q2+q3+q4+q5+q6       ! q0
            t1=   rc*qze+rb*qet+ra*qth+r9*qio+r8*qka+r7*qla
            t2=   rc*qet+rb*qio+ra*qla-r9*qka-r8*qth-r7*qze
            t3=   rc*qth+rb*qla-ra*qio-r9*qze+r8*qet+r7*qka
            t4=   rc*qio-rb*qka-ra*qze+r9*qth-r8*qla-r7*qet
            t5=   rc*qka-rb*qth+ra*qet-r9*qla-r8*qze+r7*qio
            t6=   rc*qla-rb*qze+ra*qka-r9*qet+r8*qio-r7*qth
            r7=r0-r1*rlc-r2*rzc-r3*rkc-r4*rec-r5*ric-r6*rtc
            r8=r0-r1*rkc-r2*rtc-r3*rec-r4*rlc-r5*rzc-r6*ric
            r9=r0-r1*ric-r2*rkc-r3*rzc-r4*rtc-r5*rlc-r6*rec
            ra=r0-r1*rtc-r2*rlc-r3*ric-r4*rzc-r5*rec-r6*rkc
            rb=r0-r1*rec-r2*ric-r3*rlc-r4*rkc-r5*rtc-r6*rzc
            rc=r0-r1*rzc-r2*rec-r3*rtc-r4*ric-r5*rkc-r6*rlc
            r1=   qc*qze+qb*qet+qa*qth+q9*qio+q8*qka+q7*qla
            r2=   qc*qet+qb*qio+qa*qla-q9*qka-q8*qth-q7*qze
            r3=   qc*qth+qb*qla-qa*qio-q9*qze+q8*qet+q7*qka
            r4=   qc*qio-qb*qka-qa*qze+q9*qth-q8*qla-q7*qet
            r5=   qc*qka-qb*qth+qa*qet-q9*qla-q8*qze+q7*qio
            r6=   qc*qla-qb*qze+qa*qka-q9*qet+q8*qio-q7*qth
            q7=q0-q1*rlc-q2*rzc-q3*rkc-q4*rec-q5*ric-q6*rtc
            q8=q0-q1*rkc-q2*rtc-q3*rec-q4*rlc-q5*rzc-q6*ric
            q9=q0-q1*ric-q2*rkc-q3*rzc-q4*rtc-q5*rlc-q6*rec
            qa=q0-q1*rtc-q2*rlc-q3*ric-q4*rzc-q5*rec-q6*rkc
            qb=q0-q1*rec-q2*ric-q3*rlc-q4*rkc-q5*rtc-q6*rzc
            qc=q0-q1*rzc-q2*rec-q3*rtc-q4*ric-q5*rkc-q6*rlc
            r(k0)=r0                   ! r0
            r(k1)=rc-r1                ! r1
            r(kc)=rc+r1                ! rc
            r(k2)=rb-r2                ! r2
            r(kb)=rb+r2                ! rb
            r(k3)=ra-r3                ! r3
            r(ka)=ra+r3                ! ra
            r(k4)=r9-r4                ! r4
            r(k9)=r9+r4                ! r9
            r(k5)=r8-r5                ! r5
            r(k8)=r8+r5                ! r8
            r(k6)=r7-r6                ! r6
            r(k7)=r7+r6                ! r7
            q(k0)=q0                   ! q0
            q(k1)=qc+t1                ! q1
            q(kc)=qc-t1                ! qc
            q(k2)=qb+t2                ! q2
            q(kb)=qb-t2                ! qb
            q(k3)=qa+t3                ! q3
            q(ka)=qa-t3                ! qa
            q(k4)=q9+t4                ! q4
            q(k9)=q9-t4                ! q9
            q(k5)=q8+t5                ! q5
            q(k8)=q8-t5                ! q8
            q(k6)=q7+t6                ! q6
            q(k7)=q7-t6                ! q7
         enddo
      enddo
      ma=mad
   enddo
endif
END SUBROUTINE dgfft

!=============================================================================
SUBROUTINE scsft(n,rb,qb)!                                              [csft]
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
SUBROUTINE dcsft(n,rb,qb)!                                              [csft]
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
SUBROUTINE sdsft(n,rb,qb)!                                              [dsft]
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
SUBROUTINE ddsft(n,rb,qb)!                                              [dsft]
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
subroutine sdsfe(n,rb,qb,x,y,r,q,dr,dq)!                                [dsfe]
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
subroutine ddsfe(n,rb,qb,x,y,r,q,dr,dq)!                                [dsfe]
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
subroutine shsfe(n,b,x,r,dr)!                                          [hsfe]
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
subroutine dhsfe(n,b,x,r,dr)!                                          [hsfe]
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
SUBROUTINE srfft(n,r)!                                                  [rfft] 
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
! --> N	      Number of data of series (product of 2s,3s,5s,7s,11s,13s)
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
SUBROUTINE drfft(n,r)!                                                  [rfft] 
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
SUBROUTINE srfftp(n,js,ws,r)!                                           [rfft] 
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(sp),dimension(0:n-1),intent(INOUT):: ws
REAL(sp),DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nhp,i
REAL(sp), DIMENSION(0:n-1)              :: t
!=============================================================================
nm=n-1; nhp=n/2+1
t=0
CALL cfft(n,js,ws,r,t)
r(nhp:nm)=t(nhp:nm)
END SUBROUTINE srfftp
!=============================================================================
SUBROUTINE drfftp(n,js,ws,r)!                                           [rfft] 
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(dp),dimension(0:n-1),intent(INOUT):: ws
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nhp,i
REAL(DP), DIMENSION(0:n-1)              :: t
!=============================================================================
nm=n-1; nhp=n/2+1
t=0
CALL cfft(n,js,ws,r,t)
r(nhp:nm)=t(nhp:nm)
END SUBROUTINE drfftp 

!=============================================================================
SUBROUTINE shfft(n,r)!                                                  [hfft]
!=============================================================================
!   R.J.Purser, National Meteorological Center, Washington D.C.  1994
!   (Revised for f90, 1999)
!		    SUBROUTINES SHFFT
!   Fourier synthesize (HFFT) a line of real data
!
! --> N	      Number of data of series (product of 2s,3s,5s,7s,11s,13s)
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
SUBROUTINE dhfft(n,r)!                                                  [hfft] 
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

!=============================================================================
SUBROUTINE shfftp(n,js,ws,r)!                                           [hfft] 
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(sp),dimension(0:n-1),intent(INOUT):: ws
REAL(sp),DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nh,nmh,nhp,i
REAL(sp),DIMENSION(0:n-1)               :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1
t(0)=0; t(nh)=0
t(nhp:nm)=r(nhp:nm); t(1:nmh)=-t(nm:nhp:-1); r(nhp:nm)=r(nmh:1:-1)
CALL dfft(n,js,ws,r,t)
END SUBROUTINE shfftp
!=============================================================================
SUBROUTINE dhfftp(n,js,ws,r)!                                           [hfft] 
!=============================================================================
INTEGER,                  INTENT(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(dp),dimension(0:n-1),intent(INOUT):: ws
REAL(DP),DIMENSION(0:n-1),INTENT(INOUT):: r
!-----------------------------------------------------------------------------
INTEGER                                 :: nm,nh,nmh,nhp,i
REAL(DP),DIMENSION(0:n-1)               :: t
!=============================================================================
nm=n-1; nh=n/2; nmh=nm/2; nhp=nh+1
t(0)=0; t(nh)=0
t(nhp:nm)=r(nhp:nm); t(1:nmh)=-t(nm:nhp:-1); r(nhp:nm)=r(nmh:1:-1)
CALL dfft(n,js,ws,r,t)
END SUBROUTINE dhfftp

!=============================================================================
subroutine sfftcnv(n,a,b,d)!                                          [fftcnv]
!=============================================================================
integer,                  intent(IN ):: n
real(sp),dimension(0:n-1),intent(IN ):: a,b
real(sp),dimension(0:n-1),intent(OUT):: d
real(sp),dimension(0:n-1)            :: ws
integer, dimension(0:n-1)            :: js
!=============================================================================
js(0)=0
call fftcnv(n,js,ws,a,b,d)
end subroutine sfftcnv
!=============================================================================
subroutine dfftcnv(n,a,b,d)!                                          [fftcnv]
!=============================================================================
integer,                  intent(IN ):: n
real(dp),dimension(0:n-1),intent(IN ):: a,b
real(dp),dimension(0:n-1),intent(OUT):: d
real(dp),dimension(0:n-1)            :: ws
integer, dimension(0:n-1)            :: js
!=============================================================================
js(0)=0
call fftcnv(n,js,ws,a,b,d)
end subroutine dfftcnv
!=============================================================================
subroutine sfftcnvp(n,js,ws,a,b,d)!                                   [fftcnv]
!=============================================================================
integer,                  intent(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(sp),dimension(0:n-1),intent(INOUT):: ws
real(sp),dimension(0:n-1),intent(IN   ):: a,b
real(sp),dimension(0:n-1),intent(  OUT):: d
real(sp),dimension(0:n-1)              :: at,bt
!=============================================================================
at=a
bt=b
call rfft(n,js,ws,at)
call rfft(n,js,ws,bt)
call fconv(n,at,bt,d)
call hfft(n,js,ws,d)
end subroutine sfftcnvp
!=============================================================================
subroutine dfftcnvp(n,js,ws,a,b,d)!                                   [fftcnv]
!=============================================================================
integer,                  intent(IN   ):: n
integer, dimension(0:n-1),intent(INOUT):: js
real(dp),dimension(0:n-1),intent(INOUT):: ws
real(dp),dimension(0:n-1),intent(IN   ):: a,b
real(dp),dimension(0:n-1),intent(  OUT):: d
real(dp),dimension(0:n-1)              :: at,bt
!=============================================================================
at=a
bt=b
call rfft(n,js,ws,at)
call rfft(n,js,ws,bt)
call fconv(n,at,bt,d)
call hfft(n,js,ws,d)
end subroutine dfftcnvp

!=============================================================================
subroutine sfconv(n,a,b,c)!                                            [fconv]
!=============================================================================
! Assuming Hermtian representations, A and B, of period n, form their
! product, C, that represents the transform of the convolution of the a and b
! whose transforms are aforementioned A and B.
!=============================================================================
integer,                  intent(IN ):: n
real(sp),dimension(0:n-1),intent(IN ):: a,b
real(sp),dimension(0:n-1),intent(OUT):: c
!-----------------------------------------------------------------------------
integer                              :: i,ic,nmnh,nh
!=============================================================================
nh=n/2
nmnh=n-nh ! =nh for n even; =nh+1 for n odd
do i=0,nh,nmnh ! <- trick index stride and limit allows for no 2-wave at n odd
   c(i)=a(i)*b(i)
enddo
do i=1,nmnh-1; ic=n-i
   c(i )=a(i)*b(i )-a(ic)*b(ic)
   c(ic)=a(i)*b(ic)+a(ic)*b(i )
enddo
end subroutine sfconv
!=============================================================================
subroutine dfconv(n,a,b,c)!                                            [fconv]
!=============================================================================
integer,                  intent(IN ):: n
real(dp),dimension(0:n-1),intent(IN ):: a,b
real(dp),dimension(0:n-1),intent(OUT):: c
!-----------------------------------------------------------------------------
integer                              :: i,ic,nmnh,nh
!=============================================================================
nh=n/2
nmnh=n-nh ! =nh for n even; =nh+1 for n odd
do i=0,nh,nmnh ! <- trick index stride and limit allows for no 2-wave at n odd
   c(i)=a(i)*b(i)
enddo
do i=1,nmnh-1; ic=n-i
   c(i )=a(i)*b(i )-a(ic)*b(ic)
   c(ic)=a(i)*b(ic)+a(ic)*b(i )
enddo
end subroutine dfconv

END MODULE pfft

