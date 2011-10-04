      REAL FUNCTION O2ABS(TEMP,PRES,VAPDEN,FREQ)
C  Copyright (c) 2003 Massachusetts Institute of Technology
C
C     PURPOSE: RETURNS ABSORPTION COEFFICIENT DUE TO OXYGEN IN AIR,
C              IN NEPERS/KM
C
C      5/1/95  P. Rosenkranz 
C      11/5/97  P. Rosenkranz - 1- line modification.
c      12/16/98 pwr - updated submm freq's and intensities from HITRAN96
c      8/21/02  pwr - revised width at 425
c      3/20/03  pwr - 1- line mixing and width revised
C
c     IMPLICIT NONE
C
C     ARGUMENTS:
      REAL TEMP,PRES,VAPDEN,FREQ
C
C     NAME    UNITS    DESCRIPTION        VALID RANGE
C
C     TEMP    KELVIN   TEMPERATURE        UNCERTAIN, but believed to be
c                                          valid for atmosphere
C     PRES   MILLIBARS PRESSURE           3 TO 1000
C     VAPDEN  G/M**3   WATER VAPOR DENSITY  (ENTERS LINEWIDTH CALCULATION
C                      DUE TO GREATER BROADENING EFFICIENCY OF H2O)
C     FREQ    GHZ      FREQUENCY          0 TO 900
C
C     REFERENCES FOR EQUATIONS AND COEFFICIENTS:
C     P.W. Rosenkranz, CHAP. 2 and appendix, in ATMOSPHERIC REMOTE SENSING
C      BY MICROWAVE RADIOMETRY (M.A. Janssen, ed., 1993).
C     H.J. Liebe et al, JQSRT V.48, pp.629-643 (1992).
c     M.J. Schwartz, Ph.D. thesis, M.I.T. (1998).
c     A.F. Krupnov et al, J. Mol. Spect. v.215, pp.309-311 (2002).
C     M.Yu. Tretyakov et al, J. Mol. Spect. (2003 preprint).
C     SUBMILLIMETER LINE INTENSITIES FROM HITRAN96.
c
c     This version differs from Liebe's MPM92 in these significant respects:
c     1. The 1- line has the width and mixing coefficient measured by 
c      Tretyakov et al. 
c     2. It modifies the 1- line width temperature dependence to (1/T)**0.9
c     3. It uses the same temperature dependence (X) for submillimeter 
c      line widths as in the 60 GHz band: (1/T)**0.8 
c     4. The 425 GHz line width is from Krupnov et al.
C
c     Local variables:
      REAL TH,TH1,B,PRESWV,PRESDA,DEN,DENS,DFNR,SUM,STR,Y,SF1,SF2,FCEN
      INTEGER K
      REAL X,WB300,W300(40),F(40),Y300(40),S300(40),V(40),BE(40)
      COMMON /O2COM/ X,WB300,W300,F,Y300,S300,V,BE
C      LINES ARE ARRANGED 1-,1+,3-,3+,ETC. IN SPIN-ROTATION SPECTRUM
      DATA F/118.7503, 56.2648, 62.4863, 58.4466, 60.3061, 59.5910,
     2  59.1642, 60.4348, 58.3239, 61.1506, 57.6125, 61.8002,
     3  56.9682, 62.4112, 56.3634, 62.9980, 55.7838, 63.5685,
     4  55.2214, 64.1278, 54.6712, 64.6789, 54.1300, 65.2241,
     5  53.5957, 65.7648, 53.0669, 66.3021, 52.5424, 66.8368,
     6  52.0214, 67.3696, 51.5034, 67.9009, 368.4984, 424.7632,
     7  487.2494, 715.3931, 773.8397, 834.1458/
        DATA S300/.2936E-14,.8079E-15, .2480E-14,.2228E-14,
     &  .3351E-14,.3292E-14, .3721E-14,.3891E-14,
     &  .3640E-14,.4005E-14, .3227E-14,.3715E-14,
     &  .2627E-14,.3156E-14, .1982E-14,.2477E-14,
     &  .1391E-14,.1808E-14, .9124E-15,.1230E-14,
     &  .5603E-15,.7842E-15, .3228E-15,.4689E-15,
     &  .1748E-15,.2632E-15, .8898E-16,.1389E-15,
     &  .4264E-16,.6899E-16, .1924E-16,.3229E-16,
     &  .8191E-17,.1423E-16, .6494E-15, .7083E-14, .3025E-14,
     &  .1835E-14, .1158E-13, .3993E-14/
      DATA BE/.009,.015, .083,.084, 2*.212, 2*.391, 2*.626,
     & 2*.915, 2*1.260, 1.660,1.665, 2.119,2.115, 2.624,2.625,
     & 2*3.194, 2*3.814, 2*4.484, 2*5.224, 2*6.004, 2*6.844,
     & 2*7.744, .048, .044, .049, .145, .141, .145/
C      WIDTHS IN MHZ/MB
      DATA WB300/.56/, X/.8/
      DATA W300/1.67, 1.646, 1.468, 1.449, 1.382, 1.360,
     & 1.319, 1.297, 1.266, 1.248, 1.221, 1.207, 1.181, 1.171,
     & 1.144, 1.139, 1.110, 1.108, 1.079, 1.078, 2*1.05,
     & 2*1.02,2*1.00,2*.97,2*.94,2*.92,2*.89, 3*1.64, 3*1.81/
      DATA Y300/  -0.036,  0.2408, -0.3486,  0.5227,
     & -0.5430,  0.5877, -0.3970,  0.3237, -0.1348,  0.0311,
     &  0.0725, -0.1663,  0.2832, -0.3629,  0.3970, -0.4599,
     &  0.4695, -0.5199,  0.5187, -0.5597,  0.5903, -0.6246,
     &  0.6656, -0.6942,  0.7086, -0.7325,  0.7348, -0.7546,
     &  0.7702, -0.7864,  0.8083, -0.8210,  0.8439, -0.8529, 6*0./
      DATA V/  0.0079, -0.0978,  0.0844, -0.1273,
     &  0.0699, -0.0776,  0.2309, -0.2825,  0.0436, -0.0584,
     &  0.6056, -0.6619,  0.6451, -0.6759,  0.6547, -0.6675,
     &  0.6135, -0.6139,  0.2952, -0.2895,  0.2654, -0.2590,
     &  0.3750, -0.3680,  0.5085, -0.5002,  0.6206, -0.6091,
     &  0.6526, -0.6393,  0.6640, -0.6475,  0.6729, -0.6545, 6*0./
C
      TH = 300./TEMP
      TH1 = TH-1.
      B = TH**X
      PRESWV = VAPDEN*TEMP/217.
      PRESDA = PRES -PRESWV
      DEN = .001*(PRESDA*B + 1.1*PRESWV*TH)
      DENS = .001*(PRESDA*TH**.9 + 1.1*PRESWV*TH)
      DFNR = WB300*DEN
      SUM = 1.6E-17*FREQ*FREQ*DFNR/(TH*(FREQ*FREQ + DFNR*DFNR))
      DO 32 K=1,40
      IF(K.EQ.1) THEN !exception for 1- line
        DF = W300(1)*DENS
      ELSE
        DF = W300(K)*DEN
      ENDIF
      FCEN = F(K)
      Y = .001*PRES*B*(Y300(K)+V(K)*TH1)
      STR = S300(K)*EXP(-BE(K)*TH1)
      SF1 = (DF + (FREQ-FCEN)*Y)/((FREQ-FCEN)**2 + DF*DF)
      SF2 = (DF - (FREQ+FCEN)*Y)/((FREQ+FCEN)**2 + DF*DF)
32    SUM = SUM + STR*(SF1+SF2)*(FREQ/F(K))**2
      O2ABS = .5034E12*SUM*PRESDA*TH**3/3.14159
      O2ABS = AMAX1(O2ABS,0.)
      RETURN
      END
