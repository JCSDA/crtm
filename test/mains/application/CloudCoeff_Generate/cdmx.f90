SUBROUTINE cdmx(f1,f2,tc,nu,mflag,Kfactor,nav)

  ! variables:
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: mflag
  REAL(8), INTENT(IN) :: f1,f2,tc,nu
  REAL(8), INTENT(OUT) :: Kfactor
  COMPLEX(8), INTENT(OUT) :: nav
  INTEGER :: iunit
  REAL(8) :: f3,xlam,null
  REAL(8) :: nr1,nr2,nr3,ni1,ni2,ni3,er1,er2,er3,ei1,ei2,ei3,eavr,eavi
  REAL(8) :: navr,navi,cc,ermix,eimix,K2,K02
  COMPLEX(8) :: K,K0,nav0
  COMPLEX(8) :: a3,a2,a1,a0,p,q,w,r1
  COMPLEX(8) :: e1,e2,e3,eav
  LOGICAL,parameter :: W84 = .true.
  !************************************************************************
  !
  ! Version 4.0
  !
  ! This subroutine calls BRUGG or MAXGAR, EPSREFWAT, REFICE 
  ! and NDX2DIE (index of refraction to dielectric constant)
  !
  ! It also returns the average dielectric constant, and the |K|^2 
  ! parameter.  It also has error checking to insure that proper values are
  ! being used before they are passed into functions and subroutines.
  ! DIE2NDX and NDX2DIE are subroutines used to convert back and forth
  ! from dielectric constants to indices of refraction.
  ! 
  ! Aug. 2005 : B.T. Johnson, University of Wisconsin
  ! Atmospheric and Oceanic Sciences. jbenjam@rain.aos.wisc.edu
  !
  ! Revision History
  ! Version 4.0  Aug. 8, 2005 [BTJ]
  !   Finally added general solution to symmetric 3-component 
  !   form of Bruggeman method.
  ! Version 3.01 Aug. 4, 2004 [BTJ]
  !   Instead of returning K2, return Kfactor = |K|^2/|K0|^2 (K2/K0)
  !   K0 = (eps_0 - 1)/(eps_0 + 2) where eps_0 = dielectric constant of 
  !   pure water.
  ! Version 3.0 Jan. 1, 2003 [BTJ]
  !   Converted to Fortran 90 style for use with Monte Carlo model
  !   No significant code changes, added INTENT keyword to inputs
  !   removed RETURN.
  !
  ! Version 2.0 Sept. 5, 2000 [BTJ]
  !   Changed calling parameters: Included MFLAG to allow selection of 
  !   mixture formula to be used.  0 = Bruggeman formula, ???0 and ???1
  !   refer to Maxwell Garnett formulas, where ???0 is the average of
  !   the first two components in the third component's matrix, and ???1
  !   is the 1st component in the average of the 2nd and 3rd matrix.
  !   e.g. 1230 = [WI]A and 1231 = W[IA].  [WI]A is Water included in
  !   Ice Matrix, which the average is then included in the Air matrix. 
  !   W[IA] is Water included in the average of Ice included in Air.
  !   The general trend is inclusion -> matrix from left to right.
  ! 
  ! NOTE: This change modifies CALLING ROUTINES because of the 
  ! changes in the arguments. 
  !
  ! Version 1.4 August 30, 2000 [BTJ]
  ! Changed NAVR, NAVI in parameter list to complex NAV.  NOTE: This
  ! modifies CALLING ROUTINES because of the changes in the arguments.
  ! 
  ! Version 1.3 August 23, 2000 [BTJ]
  ! Added 6 additional cases to MAXGAR to account for one component
  ! inclusions in a averaged matrix e.g. A[WI] = Air in Water/Ice Matrix
  ! 
  ! Version 1.2 July 26, 2000 [BTJ]
  ! Moved subroutine into "beta" mode, corrected comments.
  ! Version 1.1 February 1, 2000 [BTJ]
  ! Added Bruggeman and Maxwell Garnett subroutines.
  ! Version 1.0e November 1999 [BTJ]
  ! Removed RHOAV from the passing parameters and made modificiations
  ! to the notes.  Preparing for final packaging.
  ! Version 1.0d August 1998 [BTJ]
  ! Included a check for the minimum and maximum allowable limits for
  ! wavelength: [0.200 - 1.0e5] microns
  ! Note: this limit is only valid when water is incorporated,
  ! otherwise, the limit is larger for ice: [0.045 - 8.6e6] microns.
  ! The limitations are based on information from the index of
  ! refraction subroutines, and do not necessarily constitute the range
  ! of *physically* acceptable parameters.
  ! Version 1.0c August 1998 [BTJ]
  ! Uses revised version of REFWAT to incorporate Liebe (1991) 
  ! [EPSW] microwave index of refraction calculations.
  ! Now valid for most wavelengths again (see Version 1.0d).
  ! Version 1.0b July 1998 [BTJ]
  ! Replaced REFWAT with EPSW : Note this makes water valid only
  ! for microwave wavelengths!
  ! Fixed a bug in a loop: "DO I=2,3" changed to "DO I=1,3"
  ! Version 1.0a July 1998 [BTJ]
  ! Original 
  ! 
  ! The subroutine will:
  ! 1. Accept inputs of F1, F2, TC, NU, MFLAG. 
  ! 2. Convert NU (microwave frequency) to XLAM (wavelength in 
  ! in microns). Since XLAM is in microns, IUNIT=0.  See
  ! refwat.f or refice.f for more info on IUNIT and XLAM.
  ! 3. Send IUNIT, XLAM, and (TC+273.15) to EPSREFWAT and REFICE to 
  ! return the proper real and complex portions of the index of 
  ! refraction; NR and NI.
  ! 4. Convert index of refraction into the respective dielectric
  ! functions using equation 9.1 on page 227 of Bohren and Huffman
  ! "Absorption and Scattering of Light by Small Particles" (1983).
  ! (NR --> ER, NI --> EI) as coded in the subroutines NDX2DIE
  ! and DIE2NDX.
  ! 5. Send F1, F2, ERi, EIi (i = components 1 to 3 ) into BRUGG/MAXGAR,
  ! and return EAVR, and EAVI.  EAVR = avg. real dielectric
  ! EAVI = avg. imaginary dielectric constant.
  ! 6. Convert EAVR, and EAVI into their respective indexes of refraction;
  ! NAVR, and NAVI.  Combine NAVR and NAVI into one complex variable NAV,
  ! yielding the complex index of refraction.
  ! 7. Calculate |K|^2 (K2) using the equation K=(NAV**2-1)/(NAV**2+2) 
  ! from Rogers and Yau "A Short Course in Cloud Physics". 
  ! Where NAV is the average complex index of refraction of the entity. 
  ! 8. Return NAV, Kfactor = |K|^2/|K_0|^2, to the calling procedure.
  !
  ! Notes:
  ! > TC is degrees celcius, adding 273.15 to get Kelvins.
  ! > IUNIT, XLAM see refwat.f by Eric A. Smith or refice.f by Stephen
  ! G. Warren.  XLAM is the wavelength.
  ! > When compiling, be sure to include DIE2NDX, NDX2DIE, EPSREFWAT,
  ! REFICE, BRUGG, and/or MAXGAR (see diemix.f)
  ! > BRUGG only needs to be called once, because it mixes all three
  ! components (if they exist) in one pass. However, MAXGAR, requires
  ! two calls (as is done in this routine) in order to mix the 
  ! components.  The first argument supplied to MAXGAR is the inclusion 
  ! volume fraction, the second argument is the matrix volume fraction.
  ! 
  !************************************************************************

  cc=2.99792d8
  iunit=0
  ! NU is in GHz, so we convert to Hz and complete the division.  However,
  ! XLAM needs to be in microns (1.0E-6 meters),
  xlam=(cc/(nu*1.0d9))*1.0d6
  ! write(*,*) "xlam:",xlam,"nu:",nu
  IF(f1 > 0.0.AND.(xlam > 1.0d6.OR.xlam <= 0.200)) & 
       WRITE(*,*)'wavelength out of range:water [0.200 um-1.0e+6 um]'
  IF(f2 > 0.0.AND.(xlam > 8.6d6.OR.xlam <= 0.045)) &
       WRITE(*,*)'wavelength out of range: ice [0.045 um-8.6e+6 um]'

  CALL epsrefwat(iunit,xlam,(tc+273.15),nr1,ni1,null,null)
  if (W84) then 
    CALL refice(iunit,xlam,(tc+273.15),nr2,ni2,null,null)
  else
    call WB2008ice(xlam,nr2,ni2)
  end if
  ! dielectric constant of air... assumed to be 1.0 since the surrounding
  ! medium is also air, and is assumed to be 1.0 in Mie calculations.
  er3 = 1.0d0
  ei3 = 0.0d0
  eavr = 1.0d0 ! initialize these to the vacuum/air value
  eavi = 0.0d0
  ermix = 1.0d0
  eimix = 0.0d0

  f3 = (1.0d0-f1-f2) ! volume fraction of air, only used internally
  CALL ndx2die(nr1,ni1,er1,ei1)
  CALL ndx2die(nr2,ni2,er2,ei2)
  ! use bruggeman formula [wi]a, 0 retains compatibility with prev. versions
  IF (mflag == 30 .OR. mflag == 0) THEN
     CALL brugg(f1,f2,er1,ei1,er2,ei2,er3,ei3,eavr,eavi)
  ELSE IF (mflag == 20) THEN
     ! brug [wa]i
     CALL brugg(f1,f3,er1,ei1,er3,ei3,er2,ei2,eavr,eavi)
  ELSE IF (mflag == 10) THEN
     ! brug [ia]w
     CALL brugg(f2,f3,er2,ei2,er3,ei3,er1,ei1,eavr,eavi)
  ELSE IF (mflag == 12321) THEN
     !** 3C symmetrical Bruggeman solution
     !** Verified 8/20/2005 BTJ
     !** uses modified Cardano's formula with Vieta Substitution
     e1 = CMPLX(er1,ei1)
     e2 = CMPLX(er2,ei2)
     e3 = CMPLX(er3,ei3)
     a3 = -4.0d0
     a2 = (-2.0d0*(e1+e2-2*e3)-2.0d0*(-3.0d0*e1+3.0d0*e3)*f1-2.0d0*(-3.0d0*e2+3*e3)*f2)/a3
     a1 = (-e1*e2 + 2.0d0*(e1*e3 + e2*e3) + 3.0d0*(e1*e2-e2*e3)*f1 + &
          3.0d0*(e1*e2-e1*e3)*f2)/a3
     a0 = (e1*e2*e3)/a3
     p = (3.0d0*a1-a2**2.0d0)/3.0d0
     q = (9.0d0*a1*a2-27.0d0*a0-2.0d0*a2**3.0d0)/27.0d0

     w = (0.5d0*(q+SQRT(q**2.0d0+4.0d0/27.0d0*p**3)))**(1.0d0/3.0d0)

     r1 = w - p/(3.0d0*w)-1.0d0/3.0d0*a2

     eavr = DBLE(r1)
     eavi = AIMAG(r1)
  ELSE IF (mflag == 1230) THEN
     ! maxwell garnett [wi]a
     IF (f1 > 0.0d0 .OR. f2 > 0.0d0) THEN  
        CALL maxgar(f1,f2,er1,ei1,er2,ei2,eavr,eavi)
     END IF
     IF ((1-f3) < 1.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((1-f3),f3,ermix,eimix,er3,ei3,eavr,eavi)
     END IF
  ELSE IF (mflag == 2130) THEN
     ! [iw]a
     IF (f1 > 0.0d0 .OR. f2 > 0.0d0) THEN 
        CALL maxgar(f2,f1,er2,ei2,er1,ei1,eavr,eavi)
     END IF
     IF ((1-f3) < 1.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((1-f3),f3,ermix,eimix,er3,ei3,eavr,eavi)
     END IF
  ELSE IF (mflag == 3210) THEN
     ! [ai]w
     IF (f3 > 0.0d0 .OR. f2 > 0.0d0) THEN
        CALL maxgar(f3,f2,er3,ei3,er2,ei2,eavr,eavi)
     END IF
     IF (f1 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((f2+f3),f1,ermix,eimix,er1,ei1,eavr,eavi)
     END IF
  ELSE IF (mflag == 2310) THEN  
     ! [ia]w
     IF (f3 > 0.0d0 .OR. f2 > 0.0d0) THEN 
        CALL maxgar(f2,f3,er2,ei2,er3,ei3,eavr,eavi)
     END IF
     IF (f1 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((f2+f3),f1,ermix,eimix,er1,ei1,eavr,eavi)
     END IF
  ELSE IF (mflag == 1320) THEN
     ! [wa]i
     IF (f1 > 0.0d0 .OR. f3 > 0.0d0) THEN
        CALL maxgar(f1,f3,er1,ei1,er3,ei3,eavr,eavi)
     END IF
     IF (f2 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((f1+f3),f2,ermix,eimix,er2,ei2,eavr,eavi)
     END IF
  ELSE IF (mflag == 3120) THEN
     ! [aw]i
     IF (f3 > 0.0d0 .OR. f1 > 0.0d0) THEN
        CALL maxgar(f3,f1,er3,ei3,er1,ei1,eavr,eavi)
     END IF
     IF (f2 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar((f1+f3),f2,ermix,eimix,er2,ei2,eavr,eavi)
     END IF
     ! =====================================================================
  ELSE IF (mflag == 1231) THEN
     ! w[ia]
     IF (f2 > 0.0d0 .OR. f3 > 0.0d0) THEN
        CALL maxgar(f2,f3,er2,ei2,er3,ei3,eavr,eavi)
     END IF
     IF (f1 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f1,(f2+f3),er1,ei1,ermix,eimix,eavr,eavi)
     END IF
  ELSE IF (mflag == 2131) THEN
     ! i[wa]
     IF (f3 > 0.0d0 .OR. f1 > 0.0d0) THEN
        CALL maxgar(f1,f3,er1,ei1,er3,ei3,eavr,eavi) 
     END IF
     IF (f2 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f2,(f1+f3),er2,ei2,ermix,eimix,eavr,eavi)
     END IF
  ELSE IF (mflag == 3211) THEN
     ! a[iw]
     IF (f2 > 0.0d0 .OR. f1 > 0.0d0) THEN
        CALL maxgar(f2,f1,er2,ei2,er1,ei1,eavr,eavi)
     END IF
     IF ((1.0d0-f3) < 1.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f3,1-f3,er3,ei3,ermix,eimix,eavr,eavi)
     END IF
  ELSE IF (mflag == 2311) THEN
     ! i[aw]
     IF (f3 > 0.0d0 .OR. f1 > 0.0d0) THEN
        CALL maxgar(f3,f1,er3,ei3,er1,ei1,eavr,eavi)
     END IF
     IF (f2 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f2,(f1+f3),er2,ei2,ermix,eimix,eavr,eavi)
     END IF
  ELSE IF (mflag == 1321) THEN
     ! w[ai]
     IF (f3 > 0.0d0 .OR. f2 > 0.0d0) THEN
        CALL maxgar(f3,f2,er3,ei3,er2,ei2,eavr,eavi)
     END IF
     IF (f1 > 0.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f1,(f2+f3),er1,ei1,ermix,eimix,eavr,eavi)
     END IF
  ELSE IF (mflag == 3121) THEN  
     ! a[wi]
     IF (f2 > 0.0d0 .OR. f1 > 0.0d0) THEN
        CALL maxgar(f1,f2,er1,ei1,er2,ei2,eavr,eavi)
     END IF
     IF ((1.0d0-f3) < 1.0d0) THEN
        ermix = eavr
        eimix = eavi
        CALL maxgar(f3,1-f3,er3,ei3,ermix,eimix,eavr,eavi)
     END IF
  ELSE 
     print *, 'error in cdmx - mixture mflag setting incorrect:', mflag
     STOP 
  END IF


  CALL die2ndx(eavr,eavi,navr,navi)

  nav  = CMPLX(navr,navi) !** average

  nav0 = CMPLX(nr1,ni1)   !** pure water
  K    = (nav*nav-1.0d0)/(nav*nav+2.0d0)
  K0   = (nav0*nav0-1.0d0)/(nav0*nav0+2.0d0)

  K2   = (abs(K))**2
  K02  = (abs(K0))**2
!  Kfactor = K2/K02
  Kfactor = K2

contains
  SUBROUTINE brugg(f1,f2,er1,ei1,er2,ei2,er3,ei3,eavr,eavi)
    !  variables:
    IMPLICIT NONE
    REAL(8), INTENT(in)  :: f1,f2,er1,er2,er3,ei1,ei2,ei3
    REAL(8), INTENT(out) :: eavr,eavi
    REAL(8) :: f
    COMPLEX(8) :: e1,e2,e3,eav,b,eav0
    !*************************************************************************
    !  Subroutine calculates the average dielectric function for up to 3 
    !  number of constituents.  It uses a modified version of the Bruggeman 
    !  dielectric mixing function. (modified to use 3 materials instead of 2)
    !  Called by CDMX subroutine.
    !
    !  July 2000 : Benjamin T. Johnson, Atmospheric and Oceanic Sciences Dept.
    !              University of Wisconsin - Madison 
    !              jbenjam@aos.wisc.edu
    ! 
    !  The subroutine will:
    !  1. Accept the parameters F1, F2, ER1,EI1,ER2,EI2,ER3,EI3.   
    !
    !  2. Calculate the real and complex portions of the average or mixed
    !     dielectric constant, and return it back to the calling program.
    !
    !  Variable and parameter descriptions:  
    !     F1 = volume fraction of component 1
    !     F2 = volume fraction of component 2
    !     1-F1-F2 = volume fraction of component 3 
    !     F  = volume fraction of inclusion.
    !     
    !     ERx, EIx are the real and complex parts of the dielectric  
    !     constant for each of the three materials (x=1,2,3)
    ! 
    !     EAVR, EAVI are average or mixed real and imaginary parts of the 
    !     dielectric constant.
    !*************************************************************************
    IF (f1 == 0.0d0 .AND. f2 == 0.0d0) THEN 
       f = 0.0d0
    ELSE 
       f=f1/(f1+f2)
    END IF

    ! verified both multiply 0.5 and divide by 2.0d0 are correct

    e1=CMPLX(er1,ei1)
    e2=CMPLX(er2,ei2)
    e3=CMPLX(er3,ei3)
    b=((1.0d0-3.0d0*(1.0d0-f))*e2+(1.0d0-3.0d0*f)*e1)/2.0d0
    eav0=-0.5d0*b+0.5d0*(b*b+2.0d0*e2*e1)**0.5d0     
    f=(f1+f2)/1.0d0
    b=((1.0d0-3.0d0*(1.0d0-f))*e3+(1.0d0-3.0d0*f)*eav0)/2.0d0
    eav=-0.5d0*b+0.5d0*(b*b+2.0d0*e3*eav0)**0.5d0
    ! converts back to real and imaginary components
    eavr=DBLE(eav)
    eavi=AIMAG(eav)

  END SUBROUTINE brugg

! ========================================================================
! ========================================================================

  SUBROUTINE maxgar(fi,fm,eri,eii,erm,eim,eavr,eavi)
    !  variables:
    IMPLICIT NONE
    REAL(8), INTENT(in)  :: fi,fm,eri,erm,eii,eim
    REAL(8), INTENT(out) :: eavr,eavi
    REAL(8) :: f
    COMPLEX(8) :: ei,em, eav,b

    !*************************************************************************
    !  subroutine calculates the average dielectric function for TWO 
    !  number of constituents.  It is based on the Maxwell Garnett 
    !  dielectric mixing function. >>>ONLY MIXES TWO MATERIALS<<<<
    !
    !  Note: When mixing materials, considerations must be made as to which
    !        component will be the inclusion or the matrix.  Significant 
    !        differences appear with respect to the average dielectric
    !        if the two materials are simply interchanged.  By this same
    !        token, care must be taken when generalizing to more than two
    !        components as to which materals will be matrix and inclusion.
    !        See Bohren and Huffman, 1983: Absorption and Scattering of Light
    !        by small particles.  The Bruggeman routine (BRUGG) does not have
    !        the distinction of inclusion and matrix. 
    !
    !  July 2000 : Benjamin T. Johnson [jbenjam@aos.wisc.edu] 
    !  
    !     INPUTS: FI, FM, ERI,EII,ERM,EIM
    !     OUTPUT: EAVR, EAVI
    !
    !  The subroutine will:
    !  1. Accept the parameters FI,FM, ERI,ERM,EII,EIM  
    !
    !  2. Calculate the real and complex portions of the average or mixed
    !     dielectric constant, and return it back to the calling program.
    !
    !  Variable and parameter descriptions:  
    !     FI is the fractional part of component that is the inclusion
    !     with respect to component FM. FM is the fraction of components
    !     that make up the "matrix" of which FI is included.
    !     
    !     ERI, EII are the real and complex parts of the dielectric  
    !     constant for the included material, and ERM, EIM are for the
    !     matrix material.
    ! 
    !     EAVR, EAVI are average or mixed real and imaginary parts of the 
    !     dielectric constant.
    ! 
    !*************************************************************************
    ! F is the volume fraction of the inclusions, thus, FI is the inclusion.
    IF (fi == 0 .AND. fm == 0) THEN
       f = 0
    ELSE   
       f    = fi/(fi+fm)
    END IF
    ei   = CMPLX(eri,eii)
    em   = CMPLX(erm,eim)
    b    = ((ei-em)/(ei+2.0d0*em))
    eav  = em*(1.0d0+(3.0d0*f*b)/(1.0d0-f*b))
    eavr = DBLE(eav)
    eavi = AIMAG(eav)

  END SUBROUTINE maxgar

SUBROUTINE ndx2die(nr,ni,er,ei)
  !************************************************************************
  !
  !     purpose : Convert index of refraction to dielectric constant.
  !
  !     Real and imaginary components of the index of refraction (NR,NI)
  !     are converted into the real and imaginary components
  !     of the dielectric function.  The equation used is 9.1 from Bohren
  !     and Huffman "Absorption and Scattering of Light by Small Particles"
  !     (1983)
  !
  !     Inputs   NR = real part of index of refraction
  !              NI = imaginary part of index of refraction
  !
  !     Outputs  ER = real part of dielectric function
  !              EI = imaginary part of dielectric function
  !
  !     July 2000 : Benjamin T. Johnson - University of Wisconsin
  !                 [jbenjam@aos.wisc.edu]
  !***********************************************************************
  IMPLICIT NONE
  REAL(8),INTENT(IN)  :: nr,ni
  REAL(8),INTENT(OUT) :: er,ei

  er = nr**2-ni**2
  ei = 2*nr*ni

END SUBROUTINE ndx2die

SUBROUTINE die2ndx(er,ei,nr,ni)
  !*********************************************************************
  !
  !     Purpose: Convert dielectric constant to index of refraction.
  !
  !     Real and imaginary compenents of the dielectric constant (ER,EI)
  !     are converted into the real and imaginary compenents of the
  !     index of refraction.   The equation used is 9.2 from Bohren and
  !     Huffman "Absorption and Scattering of Light by Small Particles"
  !     (1983)
  !
  !     Inputs   ER = real part of dielectric function
  !              EI = imaginary part of dielectric function
  !
  !     Outputs  NR = real part of index of refraction
  !              NI = imaginary part of index of refraction
  !
  !     July 2000 : Benjamin T. Johnson - University of Wisconsin
  !                 [jbenjam@aos.wisc.edu]
  !*********************************************************************
  IMPLICIT NONE
  REAL(8), INTENT(IN) :: er,ei
  REAL(8), INTENT(OUT) :: nr,ni
  COMPLEX(8) ::  e,n

  e  = CMPLX(er,ei)
  n  = SQRT(e)
  nr = REAL(n)
  ni = AIMAG(n)
END SUBROUTINE die2ndx

END SUBROUTINE cdmx







