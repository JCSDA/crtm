C  %Z%SCCS: %G%  FILE: %M% (%I%)
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
!ROUTINE NAME: shval3
!CALL INTERFACE:
        SUBROUTINE shval3 (flat, flon, elevkm,
     1                     nmax, gh, iext, ext, x, y, z)
c ================================================================
!F77  LANGUAGE-  FORTRAN 77 
c
!ABSTRACT: calculates field components from spherical harmonic (sh)
c       models.
c
!ROUTINE HISTORY:
c       version 1.03
c       based on subroutine 'igrf' by d. r. barraclough and
c       s. r. c. malin
c
c       norman w. peddie, u.s. geological survey, mail stop 964,
c       federal center, box 25046, denver, colorado 80225
c
c       minor changes to argument list- p. rosenkranz, 5/21/93
c       fixed sqrt neg argument trap -   " 11/9/93
c
C  ! Log for /home/jsg/airs_level2_f77/src/mit/shval3.F[1.0]:
!   Initial version
!  Apr. 25, 1997 PWR - internal double precision version
c
c
!ARGUMENTS:
c specifications-
      IMPLICIT NONE
      real flat,flon,elevkm,gh(*),ext(3),x,y,z
      integer nmax,iext
c       input:
c           flat  - north latitude, in degrees (- for south)
c           flon  - east longitude, in degrees (- for west)
c           elevkm  - elevation in km
c                   if elevkm < 6000., flat,flon and elevkm
c                   are interpreted as geodetic coordinates
c                   (i.e., measured from mean sea level). 
c                   if elevkm > 6000., they are interpreted
c                   as geocentric coordinates (i.e., from 
c                   earth's center).
c           nmax  - maximum degree and order of coefficients
c           gh    - schmidt quasi-normal internal spherical
c                   harmonic coefficients (nanotesla).
c                   number of coeff = nmax*(nmax+2)
c           iext  - external field coefficients flag (= 0 if none)
c           ext   - the three 1st-degree external coefficients
c                   (not used if iext = 0)
c
c       output:
c           x     -  northward component (nanotesla)
c           y     -  eastward component (nanotesla)
c           z     -  vertically-downward component (nanotesla)
c
!ROUTINES CALLED: none
!PARENT: general purpose
!RETURN VALUES:
!FILES ACCESSED:
!DESCRIPTION: d. r. barraclough and s. r. c. malin, report no. 71/1, 
c  institute of geological sciences, u.k.
c
!KNOWN BUGS AND LIMITATIONS:
c       The required sizes of the local arrays used in this subroutine
c       depend on the value of nmax. This version is dimensioned for 
c       nmax of 14 or less. The minimum dimensions
c       needed are indicated below.
c
c --------------------------
c      minimum dimension nmax
        double precision  sl(14), cl(14)
c      minimum dimension (nmax * (nmax + 3)) / 2
        double precision  p(119), q(119)
!END HEADER*************************************************************
      double precision erad,a2,b2,dtr,r,slat,aa,clat,sd,cd,bb,cc,dd,
     & ratio,rr,fn,fm,argb
      integer n,l,m,npq,j,k,i
c**** CONSTANTS
      parameter (erad = 6371.2d0)
      parameter (a2 = 40680925.d0)
      parameter (b2 = 40408588.d0)
c           erad  - value of earth's radius associated with the sh
c                   coefficients, in km
c           a2,b2 - squares of semi-major and semi-minor axes of
c                   the reference spheroid used for transforming
c                   between geodetic and geocentric coordinates or
c                   components
c
c ================================================================
        dtr = 3.1415926536D0/180.D0
        r = elevkm
        if (90. - flat .lt. .001) then
            aa = 89.999d0
c           300 ft from n. pole
        else if (90. + flat .lt. .001) then
            aa = -89.999d0
c           300 ft from s. pole
        else
            aa = flat
        endif
        slat = dsin (aa * dtr)
        clat = dcos (aa * dtr)
        sl(1) = dsin (dble(flon) * dtr)
        cl(1) = dcos (dble(flon) * dtr)
        x = 0.
        y = 0.
        z = 0.
        sd = 0.d0
        cd = 1.d0
        n = 0
        l = 1
        m = 1
        npq = (nmax * (nmax + 3)) / 2
        if (elevkm.lt.6000.) then
            aa = a2 * clat * clat
            bb = b2 * slat * slat
            cc = aa + bb
            dd = sqrt (cc)
            r = dsqrt (dble(elevkm) * (dble(elevkm) + 2.d0 * dd)
     1                   + (a2 * aa + b2 * bb) / cc)
            cd = (dble(elevkm) + dd) / r
            sd = (a2 - b2) / dd * slat * clat / r
            aa = slat
            slat = slat * cd - clat * sd
            clat = clat * cd + aa * sd
        endif
        ratio = erad / r
        aa = dsqrt (3.d0)
        p(1) = 2.d0 * slat
        p(2) = 2.d0 * clat
        p(3) = 4.5d0 * slat * slat - 1.5d0
        p(4) = 3.d0 * aa * clat * slat
        q(1) = -clat
        q(2) = slat
        q(3) = -3.d0 * clat * slat
        q(4) = aa * (slat * slat - clat * clat)
        do 10  k = 1, npq
            if (n .lt. m) then
                m = 0
                n = n + 1
                rr = ratio**(n + 2)
                fn = n
            endif
            fm = m
            if (k .ge. 5) then
                if (m .eq. n) then
                    aa = dsqrt (1.d0 - .5d0 / fm)
                    j = k - n - 1
                    p(k) = (1.d0 + 1.d0 / fm) * aa * clat * p(j)
                    q(k) = aa * (clat * q(j) + slat / fm * p(j))
                    sl(m) = sl(m-1) * cl(1) + cl(m-1) * sl(1)
                    cl(m) = cl(m-1) * cl(1) - sl(m-1) * sl(1)
                else
                    aa = dsqrt (fn * fn - fm * fm)
                    ARGB = (fn - 1.d0)**2 - fm * fm
                    bb = dsqrt (dmax1(ARGB,0.d0)) / aa
                    cc = (2.d0 * fn - 1.d0) / aa
                    i = k - n
                    j = k - 2 * n + 1
                    p(k) = (fn + 1.d0) * (cc * slat / fn * p(i) - bb
     1                     / (fn - 1.d0) * p(j))
                    q(k) = cc * (slat * q(i) - clat / fn * p(i))
     1                     - bb * q(j)
                endif
            endif
            aa = rr * gh(l)
            if (m .eq. 0) then
                x = x + aa * q(k)
                z = z - aa * p(k)
                l = l + 1
            else
                bb = rr * gh(l+1)
                cc = aa * cl(m) + bb * sl(m)
                x = x + cc * q(k)
                z = z - cc * p(k)
                if (clat .gt. 0.d0) then
                    y = y + (aa * sl(m) - bb * cl(m)) * fm * p(k)
     1                  / ((fn + 1.d0) * clat)
                else
                    y = y + (aa * sl(m) - bb * cl(m)) * q(k)
     1              * slat
                endif
                l = l + 2
            endif
            m = m + 1
   10   continue
        if (iext .ne. 0) then
            aa = dble(ext(2)) * cl(1) + dble(ext(3)) * sl(1)
            x = dble(x) - dble(ext(1)) * clat + aa * slat
            y = dble(y) + dble(ext(2)) * sl(1) - dble(ext(3)) * cl(1)
            z = dble(z) + dble(ext(1)) * slat + aa * clat
        endif
        aa = x
        x = dble(x) * cd + dble(z) * sd
        z = dble(z) * cd - aa * sd
        return
        end
