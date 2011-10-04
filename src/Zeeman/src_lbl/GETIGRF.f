        SUBROUTINE getigrf (iu, date, nmax, gh, ios)
C ===============================================================
C
C    Language- Fortran 77     Version 1.0
C
C    Purpose-
C       Reads spherical harmonic coefficients for the IGRF
C       model into an array and extrapolates to date.
C
C       Inputs:
C           iu    - Logical unit number
C           date  - date in decimal year of desired model
C
C       Outputs:
C           nmax  - Maximum degree and order of model
C           gh    - Schmidt quasi-normal internal spherical
C                   harmonic coefficients (nanotesla)
C           ios   - Error number: =  0, no error
C                                 = -2, records out of order
C                                 =  FORTRAN run-time error number
C
C    Combined from parts of NGDC programs GEOMAG and GETSHC.
C    P.Rosenkranz, 5/21/93
C ===============================================================
      real  gh1(170), gh2(170), gh(170)
      character*80 inarray
      character*8 model
      integer maxn(3)
c
***** Read record from open file into inarray and check for
***** IGRF 
***** which means its a header. If its a header then the 
***** header information is stored in its appropriate array.
***** As programmed, DGRF's are ignored.
c
20    continue
      read (iu,21, iostat=ios, err=55) inarray
21    format(a80)
      if (inarray(6:9) .ne. 'IGRF') goto 20
c
***** Model header record; read and store information:
***** model name, base epoch, degree & order for g&h, secular
***** change & acceleration coefficients, valid date & elevation
***** ranges.
      read (inarray, 22) model, epoch, maxn,
     +   yrmin, yrmax, altmin, altmax
22    format (4x,a8,f8.2,3i3,2f8.2,2f7.1)
      if (date.lt.yrmin .or. date.gt.yrmax) print 23, date,model
23    format(' The date of',f8.1,' is outside the recommended ',
     + ' range of model ',a8/' The model should be updated.')
C
C ---------------------------------------------------------------
C       Read the coefficient file, arranged as follows:
C
C M     N     G     H
C ----------------------
C                                   /   0     1    GH(1)  -
C                                  /    1     1    GH(2) GH(3)
C                                 /     0     2    GH(4)  -
C                                /      1     2    GH(5) GH(6)
C           NMAX*(NMAX+3)/2     /       2     2    GH(7) GH(8)
C              records          \       0     3    GH(9)  -
C                                \      .     .     .     .
C                                 \     .     .     .     .
C           NMAX*(NMAX+2)          \    .     .     .     .
C           elements in GH          \  NMAX  NMAX   .     .
C
C       N and M are, respectively, the degree and order of the
C       coefficient.
C ---------------------------------------------------------------
        i = 0
        nmax = maxn(1)
        do 30 nn = 1, nmax
            do 35 mm = 0, nn
                read (iu, 40, iostat=ios, err=55) m,n,g,h,g2,h2
 40      format (2i2, 2f8.1, 2f8.2)
                if (nn .ne. n .or. mm .ne. m) then
                    ios = -2
                    goto 55
                endif
                i = i + 1
                gh1(i) = g
                gh2(i) = g2
                if (m .ne. 0) then
                    i = i + 1
                    gh1(i) = h
                    gh2(i) = h2
                endif
 35        continue
 30     continue
c
c       extrapolate to the desired date using the base coefficients
c       and secular change coefficients
        call extrapsh(date,epoch,maxn(1),gh1,maxn(2),gh2,nmax,gh)
c
 55     return
        end
