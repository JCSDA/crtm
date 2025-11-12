! This subroutine calculates the extinction and scattering coefficients
! and the single scattering albedo for a distribution of spherical
! scatterers using Mie scattering.  The Legendre coefficients for
! the phase (scattering) matrix are also computed.  These coefficients
! are in terms of the (I,Q,U,V) Stokes vector and are normalized (i.e. 
! the first I-I term is 1).
!
! The distribution of particle sizes is a modified gamma distribution
! which is specified by four parameters  a, b, alpha, and gamma:
!
!    N(r) = a * r^alpha * exp(-b * r^gamma)     .
!
! The Mie results are integrated across the radius range using the
! trapezoidal rule (endpoints have half weight).
!
! The wavelength, radii, and size distribution parameters should
! all be input in the same units (e.g. microns or cm).  The complex
! index of refraction is specified with a negative imaginary part.

SUBROUTINE Dmie(nshp, wavelength, rhoavx, mspherex, mcore, corefrac, &
     numDx, MAXLEG, ad, bd, alpha, gamma, IWCout, volext, mext, volscat, &
     albedo, volback, mback, volasym, nlegen, coef1, coef2, coef3, coef4)
  USE Type_Kinds
  IMPLICIT NONE
  
  
  REAL(fp),    INTENT (in)  :: wavelength, corefrac
  REAL(fp),    INTENT (in)  :: ad, bd, alpha, gamma, rhoavx
  COMPLEX(fp), INTENT (in)  :: mspherex, mcore  
  INTEGER,     INTENT (in)  :: MAXLEG, numDx, nshp
  INTEGER,     INTENT (out) :: nlegen
  REAL(fp),    INTENT (out) :: volext, volscat, albedo, volback, volasym, mext, mback, IWCout
  REAL(fp),    INTENT (out) :: coef1(MAXLEG+1), coef2(MAXLEG+1), coef3(MAXLEG+1), coef4(MAXLEG+1)
  
  INTEGER, PARAMETER :: maxn = 4096
  
  INTEGER :: nterms, nquad, nmie,stopir
  INTEGER :: i, k, l, M, ir, numD,tcnumD, cnumD,freqx,dmf, cnt, itmp, mshpn
  LOGICAL :: coated = .FALSE.
  REAL(fp) :: x, xcore, delD, diameter, ndens, tmp, reff,ypval
  REAL(fp) :: qext, qscat, qback, qxbck, k2,temp_K, asym, freq_GHz
  REAL(fp) :: Rqext, Rqscat, Rqback, volcross
  REAL(fp) :: minqext, minqscat, minqback, minasym
  REAL(fp) :: maxqext, maxqscat, maxqback, maxasym
  REAL(fp) :: N, Dmin, Dmax, rhoav, ra, rb
  REAL(fp) :: Ddistribution, ratio,vol, vasym, ak2,f1,f2,swf
  REAL(fp) :: mu(maxn), wts(maxn)
  REAL(fp) :: p1, p2, p3, p4, PL, PL1, PL2, PI, mass
  REAL(fp) :: sumqe, sumqs, sumqb, sumqx, Zray,Zmie, sumas, melt_mass
  REAL(fp) :: sump1(maxn), sump2(maxn)  
  REAL(fp) :: sump3(maxn), sump4(maxn)
  REAL(fp) :: s1(maxn),s2(maxn), RR, aV, bV
  REAL(fp), DIMENSION(20)      :: cext,cabs,cscat,cback,casym,creff_um
!  REAL(fp), DIMENSION(20,37)   :: cphase
  REAL(fp), DIMENSION(15000,2) :: tQqext,tQqscat,tQqback,tQqxbck,tQasym !** :,1 = "V-pol" :,2 = "H-pol" (see read_melt_db.f90)
  REAL(fp), DIMENSION(15000)   :: tQaeff,tQwavel, tmelt_fraction, tshape_number
  REAL(fp), DIMENSION(1000)    :: Qqext,Qqscat,Qqback,Qqxbck,Qasym,Qaeff,Qwavel,melt_fraction
  INTEGER,  DIMENSION(1000)    :: qq, shape_number

  COMPLEX(fp) :: a(maxn), b(maxn)  
  COMPLEX(fp) :: msphere, ck2
  LOGICAL     :: DDA,  MIE
  CHARACTER(len=128) :: shpfn,systr
  CHARACTER(len=3)   :: flag,POL
  
  DDA    = .FALSE.
  MIE    = .FALSE.
  coated = .FALSE.  
  rhoav  = rhoavx !** kludge, because intent(in) variables aren't allowed to be changed
  numD   = numDx  !** kludge 


  !** this entire block needs to become a single scattering table
  IF ((ABS(nshp) >= 1 .AND. nshp < 5) .OR. nshp == 88 ) THEN !** includes special case of -1
     flag = 'BMI'   
  ELSEIF ((nshp >= 5 .AND. nshp <= 9)) THEN
     !** we're in DDA mode 
     !** the read routines are generalized to read in concatenated
     !** information from DDA calculations, which may include multiple
     !** shape types, melted particles, etc. 
     !** in this block we can specify some arbitrary predefined properties,
     !** such as limiting to a particular shape or ony "dry" particles, etc.

     flag = 'DDA'
  ELSEIF (nshp == 99) THEN
     flag = 'DDA'
     shpfn = 'qtable.in'
     !** accept any qtable.in that is currently linked
     !** read in 

     !** this will be the spot where Kuo scattering table qfiles get linked in,
     !** since these files are a concatenation of all scattering properties for 
     !** all shapes at a given frequency, we cannot use "interpolation" to find
     !** the appropriate size for matching
     !** instead, we'll call a routine to come up with an "average" property
     !** given the size, which will be based on the values within the radius 
     !** step.  So if radius = 500, qtable file has 475, 480, 500, 550, and step size is 25
     !** we pick 475, 480, 500, but not 550. (not implemented yet)
  END IF
  
  !** need to have transparent method for getting DDA table information
  !** probably need to database it somehow
  
  IF (flag == 'DDA') THEN
     !     print *, 'Qtable: reading from:', shpfn
     IF (nshp /= 99) THEN
        WRITE(systr,'(''ln -fs '',A35,'' ./qtable.in'')') shpfn
        !     print *, 'systr:', systr
        CALL system(systr)
        !     print *, 'Qtable: reading from:', shpfn
     END IF
  END IF
  
  IF (flag == 'DDA') THEN 
     DDA = .TRUE.
     rhoav = 917d0
  elseif (flag == 'BMI') then
     MIE = .TRUE.
  ELSE
     STOP 'A type flag needs to be set in Dmiesub.f90'
  END IF

  ratio = (1.0d3/rhoav)**(1./3.)  !** initial density of water / density of particle

  PI = 4.0d0*ATAN(1.0d0)
  !  if (corefrac .gt. 0.0 .and. corefrac .lt. 1.0) coated = .true.  

  !** intialize arrays, set Dmin/Dmax
  CALL glimits(alpha,bd,gamma,Dmin,Dmax)
  IF (DDA) THEN
     Qaeff(:)   = -99.0d0
     Qwavel(:)  = -99.0d0
     Qqext(:)   = -99.0d0
     Qqscat(:)  = -99.0d0
     Qqback(:)  = -99.0d0
     Qqxbck(:)  = -99.0d0
     Qasym(:)   = -99.0d0
     melt_fraction(:) = -99.0d0
     shape_number(:)  = -99.0d0

     tQaeff(:)  = -99.0d0
     tQwavel(:) = -99.0d0
     tQqext     = -99.0d0
     tQqscat    = -99.0d0
     tQqback    = -99.0d0
     tQqxbck    = -99.0d0
     tQasym     = -99.0d0
     tmelt_fraction(:) = -99.0d0
     tshape_number(:)  = -99.0d0

     !** get whatever qtable.in is linked to (this means that the right file needs to be linked)
     !** eventually generalize qtable2 to be more like scat_db, so that provided wavelength and shape, it will return the arrays
     !** returns vectors of the contents of the given qfile, inclunding shape numbers
     !** important note: qtable.in needs to be "preselected" prior to this point to ensure 
     !** the properties of interest are integrated over.  Eventually a preselection routine could occur here,
     !** but the variables will have to be separated between the "read-in" values and the PSD-values
     !** right now I assume that the vector  Qaeff will be monotonic increasing, and the remainging quantities
     !** have been previously sorted.  Qwavel must be constant here -- there's no mixing of 
     !** wavelengths in this code.   Otherwise, and combination of melt_fractions or shape_numbers can 
     !** occur here.  
     !** eventually need to find a way to modify the PSD parameters (N0, Lambda) based on the melted state


     !** cnumD determines the number of valid entries

     !** current shape number list for the main database:
     !** 1.  1_flake
     !** 2.  5_flake aggregate
     !** 3,  20_flake aggregate
     !** 4.  graupel
     !** 5.  1% DDA ice sphere (not created yet)
     !** 6.  10% DDA ice sphere (not created yet)
     !** 7.  50% DDA ice sphere (not created yet)
     !** 8.  100% DDA ice sphere (not created yet)

     !$$$     call qtable2(tQaeff,tQwavel,tQqext,tQqscat,tQqback,tQasym,tmelt_fraction,tcnumD,tshape_number)
     CALL readmeltdb(tQaeff,tQwavel,tQqext,tQqscat,tQqback,tQqxbck,tQasym,tmelt_fraction,tcnumD,tshape_number)
     !** these scalings cause -99 values to become even more negative.  Any checks shouldn't rely on the empty value being -99
     tQqback = tQqback*(4.0d0*PI)  !** it is times 4*pi, not divide for DDA vs. Mie  (note: in the DDA source, the 4pi term might be there already...
     tQqxbck = tQqxbck*(4.0d0*PI)  !** it is times 4*pi, not divide for DDA vs. Mie  (note: in the DDA source, the 4pi term might be there already...

     !** readmeltdb has read in the vectors, which may contain the same AEFF values for different 
     !** melt_fractions. 
     !** current verison assumes that swf is same for all sizes 

     !** find the value of melt_fraction nearest to swf
     swf = corefrac

     IF (swf < 0) swf = 0.0d0

     dmf = MINVAL(MINLOC(ABS(tmelt_fraction(:) - swf))) ! isolate target WF from db (nearest matching...)

     IF (ABS(swf-tmelt_fraction(dmf)) > 0.15) THEN 
        PRINT *, 'warning: SWF not close enough to DMF:', swf,tmelt_fraction(dmf)
        STOP 'SWF stop'
     END IF
     
     POL = "M" !** V, H or M
     IF (POL == "V") THEN 
        cnt = COUNT(ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. (tshape_number(:) < 998) )  ! count matching and oriented shapes
        IF (cnt > 1000) THEN
           PRINT *, 'cnt:', cnt
           STOP 'increase cnt in Dmiesub'
        END IF
        qq(1:cnt) = PACK((/(k,k=1,tcnumD)/),ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. &
             (tshape_number(:) < 998) )
        
        Qqext(1:cnt)  = tQqext(qq(1:cnt),1)
        Qqscat(1:cnt) = tQqscat(qq(1:cnt),1)
        Qqback(1:cnt) = tQqback(qq(1:cnt),1)
        Qqxbck(1:cnt) = tQqxbck(qq(1:cnt),1)
        Qasym(1:cnt)  = tQasym(qq(1:cnt),1)
     ELSEIF (POL == "H") THEN
        cnt = COUNT(ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. (tshape_number(:) < 998) )  ! count matching and oriented shapes
        IF (cnt > 1000) THEN
           PRINT *, 'cnt:', cnt
           STOP 'increase cnt in Dmiesub'
        END IF
        qq(1:cnt) = PACK((/(k,k=1,tcnumD)/),ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. &
             (tshape_number(:) < 998) )
        
        Qqext(1:cnt)  = tQqext(qq(1:cnt),2)
        Qqscat(1:cnt) = tQqscat(qq(1:cnt),2)
        Qqback(1:cnt) = tQqback(qq(1:cnt),2)
        Qqxbck(1:cnt) = tQqxbck(qq(1:cnt),2)
        Qasym(1:cnt)  = tQasym(qq(1:cnt),2)
     ELSE !** means only (testing, if 999 only doesn't work, use (1 + 2) / 2 
        cnt = COUNT(ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. (tshape_number(:) == 999) )  ! count matching and orientation-averaged
        IF (cnt > 1000) THEN
           PRINT *, 'cnt:', cnt
           STOP 'increase cnt in Dmiesub'
        END IF
        qq(1:cnt) = PACK((/(k,k=1,tcnumD)/),ABS(tmelt_fraction(:)-swf) == ABS(tmelt_fraction(dmf)-swf) .AND. &
             (tshape_number(:) == 999) )
        
        Qqext(1:cnt)  = tQqext(qq(1:cnt),1) 
        Qqscat(1:cnt) = tQqscat(qq(1:cnt),1)
        Qqback(1:cnt) = tQqback(qq(1:cnt),1)
        Qqxbck(1:cnt) = tQqxbck(qq(1:cnt),1)
        Qasym(1:cnt)  = tQasym(qq(1:cnt),1)
     END IF
     Qaeff(1:cnt)  = tQaeff(qq(1:cnt))
     Qwavel(1:cnt) = tQwavel(qq(1:cnt))
     melt_fraction(1:cnt) = tmelt_fraction(qq(1:cnt))
     shape_number(1:cnt)  = tshape_number(qq(1:cnt))
     cnumD = cnt
  END IF

  ! Find the maximum number of terms required in the Mie series
  x = PI * ratio * Dmax / wavelength  !** physical / geometric maximum diameter 
  nterms  = 0
  msphere = mspherex
  CALL Dmiecalc(nterms, x, msphere, a, b)  
  
  nlegen = 2 * nterms
  nlegen = MIN(MAXLEG, nlegen)
  nterms = MIN(nlegen, nterms)  
  nquad  = INT((nlegen + 2 * nterms + 2) / 2.0d0)
  nquad  = MIN(nquad, maxn)
  
  ! Get the Gauss-Legendre quadrature abscissas and weights
  CALL Dgausquad(nquad, mu, wts)

  !** zero summed quantities
  sumqe = 0.0d0
  sumqs = 0.0d0
  sumqb = 0.0d0
  sumqx = 0.0d0
  sumas = 0.0d0
  sump1 = 0.0d0  
  sump2 = 0.0d0  
  sump3 = 0.0d0  
  sump4 = 0.0d0  
  vol   = 0.0d0
  mass  = 0.0d0
  RR    = 0.0d0
  N     = 0.0d0

  ! Integration loop over radius of spheres
  !** PSD is always specified in terms of liquid equivalent diameters, to ensure consistent
  !** mass comparisons.  Physical units, however, require the proper ratio modification of size.
  
  IF (numD .GT. 0) delD = (Dmax - Dmin) / (numD-1) 
  
  stopir = 1000
  DO ir = 1, numD
     IF (MIE .OR. DDA) THEN
        diameter = Dmin + (ir-1) * delD  
        
        ratio = (1.0d3/rhoav)**(1.0d0/3.0d0)  !** initial density of water / density of particle       
        reff = diameter/2.0d0*1.0d4*ratio  !** water diameter cm to ice effective radius microns
     END IF
     
     IF (DDA) THEN
        !** To-do: need N0 adjustment here to ensure correct IWC due to truncation / definite integration
        
        qext  = 0.0d0
        qscat = 0.0d0
        qback = 0.0d0
        qxbck = 0.0d0
        asym  = 0.0d0
        
        !** pre-process the dataset a little  
        !** melt_fraction should be pre-selected, as is frequency.
        !** that leaves all of the AEFFs, 75 rotations, and 2 means. 
        
        !** quality check for goodness of fit for reff 
        
        cnt       = COUNT(ABS(Qaeff(1:cnumD)-reff) == MINVAL(ABS(Qaeff(1:cnumD)-reff)))  !** 152 (75 rotations * 2 + 2 means)
        qq(1:cnt) = PACK((/(k,k=1,cnumD)/),ABS(Qaeff(1:cnumD)-reff) == MINVAL(ABS(Qaeff(1:cnumD)-reff)))  ! pack them up
        !** qq contains the indices of Qaeff that are the closest to reff.
        
        itmp  = COUNT(ABS(shape_number(qq(1:cnt))-999) == MINVAL(ABS(shape_number(qq(1:cnt))-999)))
        mshpn = MINVAL(PACK((/(k,k=1,cnt)/),ABS(shape_number(qq(1:cnt))-999) == &
                MINVAL(ABS(shape_number(qq(1:cnt))-999))))
        
        
        qext     = Qqext(qq(mshpn))       
        minqext  = MINVAL(Qqext(qq(1:cnt)))   !** min of all Qqext values for both polarizations
        maxqext  = MAXVAL(Qqext(qq(1:cnt)))   !** max of all Qqext values for both polarizations
        qscat    = Qqscat(qq(mshpn))
        minqscat = MINVAL(Qqscat(qq(1:cnt)))  !** min of all Qqscat values for both polarizations
        maxqscat = MAXVAL(Qqscat(qq(1:cnt)))  !** max of all Qqscat values for both polarizations
        qback    = Qqback(qq(mshpn))
        qxbck    = Qqxbck(qq(mshpn))
        minqback = MINVAL(Qqback(qq(1:cnt)))  !** min of all Qqback values for both polarizations
        maxqback = MAXVAL(Qqback(qq(1:cnt)))  !** max of all Qqback values for both polarizations
        asym     = Qasym(qq(mshpn))
        minasym  = MINVAL(Qasym(qq(1:cnt)))   !** min of all Qasym values for both polarizations
        maxasym  = MAXVAL(Qasym(qq(1:cnt)))   !** max of all Qasym values for both polarizations
        f1       = melt_fraction(qq(mshpn))
        
     END IF
     x = PI * diameter*ratio / wavelength  !** physical size parameter
     
     IF (MIE) THEN       
        nmie = 0   !** autocompute nmie inside Dmiecalc
        IF (coated) THEN  
           xcore = corefrac * x  
           CALL Dmiecoat(nmie, xcore, mcore, x, msphere, a, b)  
        ELSE  
           CALL Dmiecalc(nmie, x, msphere, a, b)  
        END IF
        CALL Dmiecross(nmie, x, a, b, qext, qscat, qback, asym)  
        
        ck2    = (msphere**2.0d0 - 1.0d0)/(msphere**2.0d0 + 2.0d0)
        Rqext  = 4.0d0*x*ABS(AIMAG(ck2*(1.0d0+x**2/15.0d0*ck2*((msphere**4.0d0 + 27.0d0*msphere**2.0d0 + 38.0d0)/ &
                 (2.0d0*msphere**2.0d0 + 3.0d0))))) + 8.0d0/3.0d0 * x**4.0d0*REAL(ck2**2.0d0)
        Rqscat = 8.0d0/3.0d0 * x**4.0d0*ck2**2.0d0
        Rqback = 4.0d0*x**4.0d0*ck2**2.0d0
     END IF
     
     ndens = Ddistribution(ad, bd, alpha, gamma, diameter)   !** L^{-4}
     
     IF (((ir .EQ. 1) .OR. (ir .EQ. numD)) .AND. (numD .GT. 0)) &
          ndens = 0.5d0 * ndens
     
     IF (MIE .OR. DDA) THEN
        !** In DDSCAT, Qext = Cext/(PI*Reff**2), the efficiencies are *incorrect*, the cross-sections
        !** are, presumably, correct.  PI/4*(ratio*diameter) recovers the "effective diameter" (i.e.,
        !** the diameter of a compact sphere of ice with the same mass as the particle.  The
        !** integration here assumes liquid equivalent diameter, so ratio is required to convert it to
        !** effective diameter.
        
        sumqe = sumqe +  qext * ndens * PI/4.0d0*(ratio*diameter)**2 * delD  !** L^{-4}*L^2*L = L^{-1}
        sumqs = sumqs + qscat * ndens * PI/4.0d0*(ratio*diameter)**2 * delD
        sumqb = sumqb + qback * ndens * PI/4.0d0*(ratio*diameter)**2 * delD
        sumqx = sumqx + qxbck * ndens * PI/4.0d0*(ratio*diameter)**2 * delD 
        sumas = sumas + asym  * qscat * ndens * PI/4.0d0*(ratio*diameter)**2 * delD
     END IF
     
     nmie = MIN(nmie, nterms)
     DO i = 1, nquad  
        CALL Dmieangle(nmie, a, b, mu (i), p1, p2, p3, p4)
        sump1(i) = sump1(i) + p1 * ndens  
        sump2(i) = sump2(i) + p2 * ndens  
        sump3(i) = sump3(i) + p3 * ndens  
        sump4(i) = sump4(i) + p4 * ndens  
     END DO
     
     vol  = vol  + ndens * (ratio*diameter)**3 * delD        !** L^{-4}*L^3*L = 1
     mass = mass + ndens * 1.0d3*PI/6.0d0*diameter**3 * delD !** L^{-4} * M L^{-3} * L^{3} * L  = M / L^{-3}  | cm^{-4} * kg/m^3 * cm^3 * cm = kg/m^3 

     melt_mass = melt_mass + f1 * ndens * 1.0d3*PI/6.0d0*diameter**3 * delD   !** L^{-4} * M L^{-3} * L^{3} * L  = M / L^{-3}  | cm^{-4} * kg/m^3 * cm^3 * cm = kg/m^3 
     aV = 8.83d0
     bV = 0.36d0
     RR = RR + ratio*(ndens*1.0d8)*PI/6.0d0*(diameter/100.0d0)**3 * aV * (diameter/100.0d0)**bV * (delD/100.0d0) !** m^{-4} m^3 m
     N  = N + ndens*delD !** L^{-4}*L = L^{-3} 
     
  END DO !** diameter loop
  IWCout = mass
  
  ! Multiply the sums by the integration delta and other constants
  ! Put quadrature weights in angular array for later
  volext   = sumqe  !**L^{-1} aka extinction coefficient
  volscat  = sumqs  
  volback  = sumqb
  volcross = sumqx
  volasym  = sumas/volscat  
  
  IF (volasym > 1.0d0) THEN
     PRINT '(A,2G12.4)', 'Warning, volasym > 1.0', volasym,volscat
  END IF
  
  mext   =  volext/(rhoav*PI/6.0d0*vol) !** L^{-1} * L^3/M = L^2/M  -> m^{-1}/(kg/m^3) = m^2/kg mass extinction coeff.
  mback  = volback/(rhoav*PI/6.0d0*vol) !** L^{-1} * L^3/M = L^2/M  -> m^{-1}/(kg/m^3) = m^2/kg mass backscattering coeff.
  
  IF ((volscat .EQ. 0.0) .OR. (volext .EQ. 0.0)) THEN  
     albedo = 0.0d0
  ELSE  
     albedo = volscat / volext
  END IF
  !** QC albedo
  IF (albedo < 0.0d0) THEN
     PRINT '(A,3G12.4)', 'Warning, albedo < 0.0: ', volasym,volscat, albedo
  END IF
  IF (albedo > 1.0d0) THEN
     PRINT '(A,3G12.4)', 'Warning, albedo > 0.0: ', volasym,volscat, albedo
  END IF
  
!** Radar calculations  
!!$  k2   = (abs((msphere*msphere-1.0d0)/(msphere*msphere+2.0d0)))**2.0d0
!!$  Zray = 1.0d12*(ad/ratio)*720.0d0/(bd/ratio)**7.0d0   !** Rayleigh
!!$  Zmie = 1.0d12*wavelength**4.0d0/PI**5.0d0/k2*volback !** Mie
!!$  Zray = 1.0d12*PI**5.0d0/wavelength**4.0d0*k2*(ad/ratio)*720.0d0/(bd/ratio)**7.0d0   !** alternative Rayleigh 

  
  !** okay, volume extinction, mass extinction match 7.82 and 7.84 in Petty, pg 195 for qext = 2.0, cross-sections are dimensionally consistent. All are invariant to changes in the number of diameters in the integration (correctly so).  volext is sensitive to changes in N0, whereas extcs and mext are not, appropriately.  We care mostly about volback, which is the integrated backscattering cross-section.  
  !** verified that volback is, indeed, the backscattering coefficient (L^{-1}) by comparison with equation 7.6.15 in Liao.  
  !** produces Mie reflectivities (Z) consistent with Rayleigh theory for x<<1
  !** verified for ice and water, using modified K factor (rather than 0.93). 
  !** rayleigh compares only when using modified K factor and ratios on ad and bd
  !** Still need to decide which is correct: modified K or K = 0.93?  Liu and Illingworth 2000 use:
  !** Z = sum_i |K(rho)|^2 N(D) D^6 f(D,rho)/0.93 
  !** this reduces to Z = sum_i N(D) D^6 (the typical rayleigh reflection)
  !** "For consistency in this paper we shall use the definition of Z referred to a 1 mm raindrop per cubic meter with a centimeter-wavelength radar"

  IF (MIE) THEN
     IF (volscat .EQ. 0.0) THEN  
        tmp = 0.0 
     ELSE  
        tmp = (wavelength**2 / (pi * volscat) ) * delD  
     END IF
     sump1(1:nquad) = tmp * sump1(1:nquad) * wts(1:nquad)
     sump2(1:nquad) = tmp * sump2(1:nquad) * wts(1:nquad)
     sump3(1:nquad) = tmp * sump3(1:nquad) * wts(1:nquad)
     sump4(1:nquad) = tmp * sump4(1:nquad) * wts(1:nquad)
     
     ! Integrate the angular scattering functions times Legendre
     ! polynomials to find the Legendre coefficients
     coef1 = 0.0d0
     coef2 = 0.0d0
     coef3 = 0.0d0
     coef4 = 0.0d0
     
     ! Use upward recurrence to find Legendre polynomials
     DO i = 1, nquad
        PL1 = 1.0d0  
        PL  = 1.0d0
        DO L = 0, nlegen
           M = L + 1  
           IF (L .GT. 0) PL = (2.0d0*DBLE(L)-1.0d0) * mu(i) * PL1/DBLE(L) - (DBLE(L)-1.0d0) * PL2/DBLE(L)
           coef1(M) = coef1(M) + sump1(i) * PL  
           coef2(M) = coef2(M) + sump2(i) * PL  
           coef3(M) = coef3(M) + sump3(i) * PL  
           coef4(M) = coef4(M) + sump4(i) * PL  
           PL2 = PL1  
           PL1 = PL  
        END DO
     END DO

     !** COMMENTS BELOW (these appear being commented to fix the issue with CRTM)
!!$     DO L = 0, nlegen
!!$        M = L + 1  
!!$        IF ((coef1(M) .NE. 0) .OR. (coef2(M) .NE. 0) .OR. &
!!$             (coef3(M) .NE. 0) .OR. (coef4(M) .NE. 0)) THEN
!!$           coef1(M) = (2.0d0 * DBLE(L) + 1.0d0) / 2.0d0 * coef1(M)
!!$           coef2(M) = (2.0d0 * DBLE(L) + 1.0d0) / 2.0d0 * coef2(M)  
!!$           coef3(M) = (2.0d0 * DBLE(L) + 1.0d0) / 2.0d0 * coef3(M)
!!$           coef4(M) = (2.0d0 * DBLE(L) + 1.0d0) / 2.0d0 * coef4(M)  
!!$        ELSE
!!$           coef1(M)=0.0d0
!!$           coef2(M)=0.0d0
!!$           coef3(M)=0.0d0
!!$           coef4(M)=0.0d0
!!$        END IF
!!$     END DO
     !** END COMMENTS
  END IF
  
  RETURN
END SUBROUTINE Dmie


SUBROUTINE Dmiecalc(nterms, x, mn, a, b)  
  ! MIECALC calculates the complex Mie coefficients An and Bn
  ! given the dimensionless size parameter X and the complex
  ! index of refraction (Mre,Mim).  The number of terms calculated
  ! is given by NTERMS unless NTERMS <= 0 or in which case the
  ! appropriate number is calculated and returned in NTERMS.
  
  USE Type_Kinds
  IMPLICIT NONE
  
  
  INTEGER,     INTENT (inout) :: nterms
  REAL(fp),    INTENT (in)    :: x
  COMPLEX(fp), INTENT (in)    :: mn 
  COMPLEX(fp), INTENT (out)   :: a(*), b(*)
  
  INTEGER, PARAMETER :: maxterms = 100000
  INTEGER     :: nstop, n, nn  
  REAL(fp)    :: psin, psim, chin, chim, tmp  
  COMPLEX(fp) :: m, y, D(maxterms+15), xin, xim, ctmp  
  
  ! If NTERMS is not specified calculate it
  nstop = x + 4.0 * x**0.3334 + 2
  IF (nterms .LE. 0) nterms = nstop
  IF (nterms .GT. maxterms) &
       CALL Derror_msg('Mie calculation requires more terms than available.')
  
  ! Generate the Dn's by down recurrence  D = d(log(PSI(y)))/dy
  m     = CONJG(mn)
  y     = m * x
  nn    = nterms + 15
  D(nn) = CMPLX(0.0d0, 0.0d0)
  DO n = nn, 2, - 1
     D(n - 1) = n / y - 1.0 / (D(n) + n / y)  
  END DO
  
  ! Generate the PSIn's and XIn'S by upward recurrence
  ! and calculate the An's and Bn's from them.
  ! (PSIN = PSI(n), PSIM = PSI(n-1), same for CHI)
  psim = COS(x) 
  psin = SIN(x)  
  chim = - SIN(x)  
  chin = COS(x)  
  DO n = 1, nterms  
     tmp  = psin  
     psin = DBLE(2 * n - 1) / x * psin - psim  
     psim = tmp
     tmp  = chin  
     chin = DBLE(2 * n - 1) / x * chin - chim  
     chim = tmp  
     xin  = CMPLX(psin, - chin)  
     xim  = CMPLX(psim, - chim)  
     ctmp = D(n) / m + n / x  
     a(n) = (ctmp * psin - psim) / (ctmp * xin - xim)  
     ctmp = m * D(n) + n / x  
     b(n) = (ctmp * psin - psim) / (ctmp * xin - xim)  
  END DO
  
  RETURN
END SUBROUTINE Dmiecalc


SUBROUTINE Dmiecoat(nterms, x, m1n, y, m2n, a, b)  
  
  ! MIECOAT calculates the complex Mie coefficients An and Bn
  ! for a coated sphere given the dimensionless size parameters
  ! X and Y and the complex indices of refraction M1 and M2.
  ! This subroutine is copied from Bohren and Huffman.
  !
  ! The number of terms calculated is given by NTERMS unless
  ! NTERMS <= 0 or in which case the appropriate number is calculated
  ! and returned in NTERMS.
  
  USE Type_Kinds
  IMPLICIT NONE
  
  INTEGER,     INTENT (inout) :: nterms  
  REAL(fp),    INTENT (in)    :: x, y  
  COMPLEX(fp), INTENT (in)    :: m1n, m2n  
  COMPLEX(fp), INTENT (out)   :: a(*), b(*)
  
  INTEGER, PARAMETER :: maxterms = 100000
  INTEGER     :: nstop, n  
  LOGICAL     :: indone  
  REAL(fp)    :: eps, crit1, crit2, crit3, crit4, comp  
  REAL(fp)    :: psi0y, psi1y, psiy  
  REAL(fp)    :: chi0y, chi1y, chiy  
  COMPLEX(fp) :: m1, m2  
  COMPLEX(fp) :: x1, x2, y2, m12  
  COMPLEX(fp) :: chi0x2, chi1x2, chix2, chipx2  
  COMPLEX(fp) :: chi0y2, chi1y2, chiy2, chipy2  
  COMPLEX(fp) :: xi0y, xi1y, xiy  
  COMPLEX(fp) :: d0x1, d1x1, d0x2, d1x2, d0y2, d1y2  
  COMPLEX(fp) :: ancap, bncap, brack, crack, dnbar, gnbar  
  
  ! If NTERMS is not specified calculate it
  nstop = y + 4.0 * y**0.3334 + 2   !** this is ugly. nstop is integer -- this is how Bohren & Huffman coded it
  IF (nterms .LE. 0) nterms = nstop  
  IF (nterms .GT. maxterms) &
       CALL Derror_msg('Mie calculation requires more terms than available.')
  
  eps    = 1.0d-8
  m1     = CONJG(m1n)  
  m2     = CONJG(m2n)  
  x1     = m1 * x  
  x2     = m2 * x  
  y2     = m2 * y  
  m12    = m2 / m1
  d0x1   = COS(x1) / SIN(x1)  
  d0x2   = COS(x2) / SIN(x2)  
  d0y2   = COS(y2) / SIN(y2)  
  psi0y  = COS(y)  
  psi1y  = SIN(y)  
  chi0y  = - SIN(y)  
  chi1y  = COS(y)  
  xi0y   = CMPLX(psi0y, - chi0y)  
  xi1y   = CMPLX(psi1y, - chi1y)  
  chi0y2 = - SIN(y2)  
  chi1y2 = COS(y2)  
  chi0x2 = - SIN(x2)  
  chi1x2 = COS(x2)  
  indone = .FALSE.
  
  DO n = 1, nterms  
     psiy = DBLE(2 * n - 1) * psi1y / y - psi0y  
     chiy = DBLE(2 * n - 1) * chi1y / y - chi0y  
     xiy  = CMPLX(psiy, - chiy)  
     d1y2 = 1.0 / (DBLE(n) / y2 - d0y2) - DBLE(n) / y2  
     IF (.NOT. indone) THEN  
        d1x1   = 1.0 / (DBLE(n) / x1 - d0x1) - DBLE(n) / x1  
        d1x2   = 1.0 / (DBLE(n) / x2 - d0x2) - DBLE(n) / x2  
        chix2  = DBLE(2 * n - 1) * chi1x2 / x2 - chi0x2  
        chiy2  = DBLE(2 * n - 1) * chi1y2 / y2 - chi0y2  
        chipx2 = chi1x2 - DBLE(n) * chix2 / x2  
        chipy2 = chi1y2 - DBLE(n) * chiy2 / y2  
        ancap  = (m12 * d1x1 - d1x2) &
                 / ((m12*d1x1*chix2 - chipx2) * (chix2*d1x2 - chipx2))
        bncap  = (m12 * d1x2 - d1x1) &
                 / ((m12*chipx2 - d1x1*chix2) * (chix2*d1x2 - chipx2))
        brack  = ancap * (chiy2 * d1y2 - chipy2)  
        crack  = bncap * (chiy2 * d1y2 - chipy2)  
        crit1  = ABS(brack * chipy2)  
        crit2  = ABS(brack * chiy2)  
        crit3  = ABS(crack * chipy2)  
        crit4  = ABS(crack * chiy2)  
        comp   = eps * ABS(d1y2)  
        IF ((crit1 .LE. comp) .AND. (crit2 .LE. eps) .AND. &
             (crit3 .LE. comp) .AND. (crit4 .LT. eps)) THEN
           indone = .TRUE.
           brack  = CMPLX(0.0d0, 0.0d0)  
           crack  = brack  
        END IF
     END IF
     dnbar  = (d1y2 - brack * chipy2) / (1.0 - brack * chiy2)  
     gnbar  = (d1y2 - crack * chipy2) / (1.0 - crack * chiy2)  
     a(n)   = ((dnbar / m2 + DBLE(n) / y) * psiy - psi1y) &
              / ((dnbar / m2 + DBLE(n) / y) * xiy - xi1y)
     b(n)   = ((m2 * gnbar + DBLE(n) / y) * psiy - psi1y) &
              / ((m2 * gnbar + DBLE(n) / y) * xiy - xi1y)
     psi0y  = psi1y
     psi1y  = psiy  
     chi0y  = chi1y  
     chi1y  = chiy  
     xi1y   = CMPLX(psi1y, - chi1y)  
     chi0x2 = chi1x2  
     chi1x2 = chix2  
     chi0y2 = chi1y2  
     chi1y2 = chiy2  
     d0x1   = d1x1  
     d0x2   = d1x2  
     d0y2   = d1y2  
  END DO
  
  RETURN  
END SUBROUTINE Dmiecoat


SUBROUTINE Dmiecross(nterms, x, a, b, qext, qscat, qback,asym)
  
  ! MIECROSS calculates the extinction, scattering, and
  ! backscatter efficiencies given the Mie coefficients An and Bn
  ! and the size parameter X.
  USE Type_Kinds
  IMPLICIT NONE
  
  
  INTEGER,     INTENT (in)  :: nterms  
  REAL(fp),    INTENT (in)  :: x
  COMPLEX(fp), INTENT (in)  :: a(*), b(*)  
  REAL(fp),    INTENT (out) :: qext, qscat, qback,asym
  
  INTEGER     :: n
  REAL(fp)    :: sum1, sum2, sum4
  COMPLEX(fp) :: sum3
  
  sum1 = 0.0d0
  sum2 = 0.0d0  
  sum3 = CMPLX(0.0d0, 0.0d0) 
  sum4 = 0.0d0
  
  
  DO n = 1, nterms  
     sum1 = sum1 + DBLE(2.0d0*n + 1.0d0) * (DBLE(a(n)) + DBLE(b(n)))  
     sum2 = sum2 + DBLE(2.0d0*n + 1.0d0) * (DBLE(a(n) * CONJG(a(n))) &
            + DBLE(b(n) * CONJG(b(n))))
     sum3 = sum3 + (2.0d0*n+1.0d0)*(-1.0d0)**n * (a(n) - b(n))
     
     sum4 = sum4 + DBLE((2.0d0*N + 1.0d0)/(N*(N + 1.0d0))) * &
          (DBLE(A(N))*DBLE(B(N)) + AIMAG(A(N))*AIMAG(B(N)))
     IF (N .GT. 1) THEN
        sum4 = sum4 + DBLE((N - 1.0d0) * (N + 1.0d0) / N) * &
             (DBLE(A(N-1))*DBLE(A(N)) + AIMAG(A(N-1))*AIMAG(A(N)) + &
             DBLE(B(N-1))*DBLE(B(N)) + AIMAG(B(N-1))*AIMAG(B(N)))
     END IF
  END DO
  qext  = 2.0d0 / x**2 * sum1
  qscat = 2.0d0 / x**2 * sum2  
  qback = ABS(sum3)**2 / x**2
  asym  = 2.0d0 /sum2 *  sum4 !** temporary for asym
  RETURN
END SUBROUTINE Dmiecross

SUBROUTINE Dmieangle(nterms, a, b, mu, p1, p2, p3, p4)
  
  ! MIEANGLE calculates the intensity scattering matrix elements
  ! (P1,P2,P3,P4) for a particular value of MU (cos(theta)) from the
  ! Mie coefficients An's and Bn's.  The matrix elements are for the
  ! stokes intensity vector (I,Q,U,V) and are calculated from the
  ! complex scattering amplitudes S1 and S2.
  USE Type_Kinds
  IMPLICIT NONE
  
  
  INTEGER,     INTENT (in)  :: nterms
  REAL(fp),    INTENT (in)  :: mu
  COMPLEX(fp), INTENT (in)  :: a(*), b(*)  
  REAL(fp),    INTENT (out) :: p1, p2, p3, p4  
  INTEGER     :: n
  REAL(fp)    :: tmp, pin, pim, taun, c
  COMPLEX(fp) :: s1, s2  
  
  s1 = CMPLX (0.0d0, 0.0d0)  
  s2 = CMPLX (0.0d0, 0.0d0)  
  
  ! Sum up the series using the An's and Bn's
  pin = 1.0d0  
  pim = 0.0d0  
  DO n = 1, nterms
     taun = DBLE(n) * mu * pin - DBLE(n + 1) * pim  
     !    Calculate the scattering functions at +mu and -mu 
     !    using the PIn's and the TAUn's.
     c   = DBLE(2 * n + 1) / DBLE(n * (n + 1))  
     s1  = s1 + c * (a(n) * pin + b(n) * taun)  
     s2  = s2 + c * (b(n) * pin + a(n) * taun)  
     !    Calculate the angular function PIn by up recurrence
     tmp = pin  
     pin = (DBLE(2 * n + 1) * mu * pin - DBLE(n + 1) * pim) / DBLE(n)  
     pim = tmp
  END DO
  ! Calculate the Stokes parameter scattering matrix elements
  p1 = 0.5 * (ABS(s2)**2 + ABS(s1)**2)  !** I 
  p2 = 0.5 * (ABS(s2)**2 - ABS(s1)**2)  !** Q
  ! P1 = CDABS(S1)**2     Il Ir  system for Stokes parameters
  ! P2 = CDABS(S2)**2
  p3 = DBLE(CONJG(s1) * s2)     !** U
  p4 = - AIMAG(CONJG(s1) * s2)  !** V
  
  RETURN  
END SUBROUTINE Dmieangle


FUNCTION Ddistribution(a, b, alpha, gamma, D) RESULT(density)
  ! DISTRIBUTION returns the particle density for a given diameter D
  ! for a modified gamma distribution specified by A, B, ALPHA, GAMMA
  !
  !     N(r) = a * D^alpha * exp(-b * D^gamma)    .
  USE Type_Kinds
  IMPLICIT NONE
  
  REAL(fp), INTENT (in) :: a, b, alpha, gamma, D
  REAL(fp) :: density
  
  ! Modified gamma distibution
  density = a * D**alpha * EXP( - b * D**gamma)  !** L^{-4} * L^{alpha} 
  
  RETURN
END FUNCTION Ddistribution


SUBROUTINE Dgausquad(N, XA, WT)  
  
  ! N = nquad 
  ! XA = cos(angle), presumably
  ! WT = weights
  ! Generates the abscissas (X) and weights (W) for an N point
  ! Gauss-Legendre quadrature.
  USE Type_Kinds
  IMPLICIT NONE
  
  
  INTEGER,  INTENT (in) :: N  
  REAL(fp), INTENT (out) :: XA(*), WT(*)
  
  REAL(fp), PARAMETER :: teeny = 3.0d-13
  REAL(fp) :: X, XP, PL, PL1, PL2, DPL
  INTEGER  :: K, I, J, L
  
  K = (N + 1) / 2  !** also ugly...
  Jloop: DO J = 1, K
     X = COS(3.141592654 * (J - 0.25) / (N + 0.5))  
     Iloop: DO I = 1, 10
        PL1 = 1  
        PL  = X  
        Lloop: DO L = 2, N
           PL2 = PL1  
           PL1 = PL  
           PL  = (DBLE(2*L - 1) * X * PL1 - DBLE(L - 1) * PL2) / DBLE(L)
        END DO Lloop
        DPL = DBLE(N) * (X * PL - PL1) / (X * X - 1.0d0)
        XP  = X
        X   = XP - PL / DPL
        IF (ABS(X - XP) .LE. teeny) EXIT Iloop
     END DO Iloop
     XA(J) = - X  
     XA(N - J + 1) = X  
     WT(J) = 2.0d0 / ((1.0d0 - X * X) * DPL * DPL)  
     WT(N - J + 1) = WT(J)  
  END DO Jloop
  
  RETURN
END SUBROUTINE Dgausquad

SUBROUTINE Derror_msg(message)
  IMPLICIT NONE
  CHARACTER(*) :: message
  
  PRINT *, message
END SUBROUTINE Derror_msg
!--------------------------------------------------------------------------

SUBROUTINE glimits(al,flam,gam,d1,d2)
  
  ! subroutine to choose limits of integration for
  ! modified gamma distribution 
  ! flam is calculated assuming diameters, so this returns diameters
  ! d1, d2.  
  USE Type_Kinds
  IMPLICIT NONE
  
  REAL(fp), INTENT(IN)  :: al,flam,gam
  REAL(fp), INTENT(OUT) :: d1,d2
  REAL(fp) :: vmode,vmax,vtarg,factor,v1,v2,gamma_mod
  INTEGER  :: it
  
  !     find value of diameter corresponding to volume mode
  vmode = ((al+3.0d0)/(flam*gam))**(1.0d0/gam)
  
  !     find (relative) value of distribution at volume mode
  vmax = gamma_mod(al,flam,gam,vmode)*(vmode**3.0d0)
  
  !    find lower and upper limits of integration so that kernel value
  !    falls below specified threshold.
  vtarg = 1.0d-4*vmax

  ! for lower limit, start at peak and then decrease by increments 
  ! of 20% until we get below specified threshold
  
  it     = 0
  factor = 0.8d0
  d1     = vmode
  v1     = vmax
  DO WHILE(v1 > vtarg .AND. it <= 1000)
     d1 = factor*d1
     v1 = gamma_mod(al,flam,gam,d1)*(d1**3.0d0)
     it = it + 1
  END DO
  
  IF(it >= 1000)STOP 'unable to find d1.'
  
  ! for upper limit, start at peak and then inrease by increments 
  ! of 20% until we get below specified threshold
  
  it     = 0
  factor = 1.2d0
  d2     = vmode
  v2     = vmax
  DO WHILE(v2 > vtarg .AND. it <= 1000)
     d2 = factor*d2
     v2 = gamma_mod(al,flam,gam,d2)*(d2**3.0d0)
     it = it + 1
  END DO
  
  
  IF(it >= 1000)STOP 'unable to find d2.'
  
END SUBROUTINE glimits

!-------------------------------------------------------------------------
! modified gamma distribution (sans n_0)

FUNCTION gamma_mod(al,flam,gam,d)
  USE Type_Kinds
  IMPLICIT NONE
  
  REAL(fp) :: al,flam,gam,d,gamma_mod
  
  ! d is the liquid equiv. diameter of the particle [meters]
  IF (d < 0.0) THEN
     STOP 'd should non-negative in gamma_mod()'
  ELSE IF (d == 0.0 .AND. al < 0.0) THEN
     STOP 'invalid alpha or d in gamma_mod()'
  END IF
  
  gamma_mod = (d**al)*EXP(-1.0*flam*(d**gam))      
  
END FUNCTION gamma_mod


SUBROUTINE compute_backscatter( nlegen, coef, bk2_m )
  USE Type_Kinds
  IMPLICIT NONE
  
  INTEGER,  PARAMETER   :: MAXLEG = 128
  INTEGER,  INTENT(in)  :: nlegen
  REAL(fp), INTENT(in), DIMENSION(6,MAXLEG+1) :: coef   !** (1,2,3,4,1,3) 
  REAL(fp), INTENT(out) :: bk2_m
  
  REAL(fp) :: pm(4,4), x, sum, pl1, pl, pl2, S1, S2
  INTEGER  :: i, n, m, row(6), col(6)
  
  row(1:6) = (/1,1,3,3,2,4/)
  col(1:6) = (/1,2,3,4,2,4/)
  
  !** X is the cosine(angle), and in this case X = -1 for backscattering
  !** following sum_legendre() in scatconvert.f, and assuming 6 coefficients (for now)
  x = -1.0d0
  
  DO i = 1,6
     sum = 0.0d0
     pl1 = 1.0d0
     pl  = 1.0d0
     DO n = 0, nlegen
        m = n + 1

        !** pl(n==0) = 1.0d0 (yes)
        !** pl(n==1) = 2-1*-1*1/1 - (1-1)*1/1 = -1.0d0  (yes
        !** pl(n==2) = (2*2-1)*-1*-1/2 - (2-1)*1/2 = 3/2 - 1/2 = 1.0 (yes)
        !** pl(n==3) = (2*3-1)*-1*1/3 - (3-1)*-1/3 = -5/3 + 2/3 = -3/3 = -1.0 (yes) 
        
        IF (n .GT. 0)  pl = (2.0d0*REAL(n)-1.0d0)*x*pl1/REAL(n) - (REAL(n)-1.0d0)*pl2/REAL(n) !** new
        
        !**            p(n+1) = (2*n+1)*cos(angle)*p(n-1)/n
        sum = sum + coef(i,m)*pl
        pl2 = pl1
        pl1 = pl
     END DO
     IF (i == 1) S1 = sum  !** not 100% certain
     IF (i == 2) S2 = sum  !** not 100% certain
     pm( row(i), col(i) ) = sum
  END DO
  
  bk2_m = S1
  
  !** there's a constant factor of sbk_m / bk2_m = 0.1257E-01 bk2_m / sbk_m = 79.58
  !** seems to persist for snow at all wavelengths. nummu doesn't change the ratio
  !** BTJ note: this is almost equal to (rho_snow/rho_ice)**(1/3) ...
  
!!$      phase_matrix(2,1) = phase_matrix(1,2)  !** not valid in the most general case
!!$      phase_matrix(4,3) = -phase_matrix(3,4) !** not valid in the most general case
  
END SUBROUTINE compute_backscatter

