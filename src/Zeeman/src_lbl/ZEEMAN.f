      SUBROUTINE ZEEMAN(JU,T,P,DEL,BFIELD,GM,GPI,GP)
C
C   LANGUAGE - FORTRAN 77
C   DATE-  10/22/93  PROGRAMMER- P. ROSENKRANZ
C          12/16/98  pwr - update intensities and freq's from HITRAN96
c           3/20/03  pwr - 1- line width revised
C
C   PURPOSE-
C   COMPUTE PROPAGATION COEFFICIENT(1/KM) FOR ONE ZEEMAN-SPLIT OXYGEN
C   LINE, SPECIFIED BY JU.
C   COMPLEX ERROR FUNCTION IS USED FOR LINE SHAPE.
C
C  ARGUMENT SPECIFICATIONS-
      INTEGER JU
      REAL T,P,DEL,BFIELD
      COMPLEX GM,GPI,GP
C
C  NAME    IN/OUT  UNITS    DESCRIPTON
C
C  JU        I              LARGER OF THE TWO ANGULAR MOMENTUM QUANTUM 
C                           NUMBERS:  N+1 FOR + BRANCH, N FOR - BRANCH.
C  T         I     KELVIN   ATMOSPHERIC TEMPERATURE
C  P         I     MILLIBAR ATMOSPHERIC PRESSURE
C  DEL       I     GHZ      FREQUENCY OFFSET FROM LINE CENTER
C  BFIELD    I     GAUSS    MAGNETIC FIELD MAGNITUDE
C  GM        O     1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = -1
C  GPI       O     1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M =  0
C  GP        O     1/KM     AMPLITUDE PROPAGATION COEFF FOR DELTA M = +1
C
C Known limitations: Zeeman splitting is computed for Hund's case(b),
c  which may be inaccurate for JU < 6.  However, JU=1 should be OK.
C
C*******************************************************************
      COMMON /ZCOM/ ALPHAC,ABC,ROT
C  LOCAL VARIABLES
      REAL U,W300(34),F(34),S300(34)
      COMPLEX CERROR
      DATA ALPHAC/.760E-7/,ABC/2.8026E-3/,ROT/6.89526E-3/
      DATA F/
     1 118.7503, 56.2648, 62.4863, 58.4466, 60.3061, 59.5910,
     2  59.1642, 60.4348, 58.3239, 61.1506, 57.6125, 61.8002,
     3  56.9682, 62.4112, 56.3634, 62.9980, 55.7838, 63.5685,
     4  55.2214, 64.1278, 54.6712, 64.6789, 54.1300, 65.2241,
     5  53.5958, 65.7648, 53.0670, 66.3021, 52.5424, 66.8368,
     6  52.0215, 67.3695, 51.5034, 67.9008/
        DATA S300/.2920E-14,.7995E-15, .2456E-14,.2204E-14,
     &  .3317E-14,.3258E-14, .3682E-14,.3853E-14,
     &  .3605E-14,.3965E-14, .3194E-14,.3678E-14,
     &  .2602E-14,.3126E-14, .1963E-14,.2455E-14,
     &  .1379E-14,.1792E-14, .9056E-15,.1222E-14,
     &  .5571E-15,.7803E-15, .3217E-15,.4674E-15,
     &  .1746E-15,.2632E-15, .8922E-16,.1394E-15,
     &  .4292E-16,.6956E-16, .1948E-16,.3270E-16,
     &  .8341E-17,.1451E-16/
      DATA U/.8/
C  WIDTHS IN MHZ/MB
      DATA W300/1.67, 1.646, 1.468, 1.449, 1.382, 1.360,
     & 1.319, 1.297, 1.266, 1.248, 1.221, 1.207, 1.181, 1.171,
     & 1.144, 1.139, 1.110, 1.108, 1.079, 1.078, 2*1.05,
     & 2*1.02,2*1.00,2*.97,2*.94,2*.92,2*.89/
C
      AB = ABC*BFIELD
      TH = 300./T
      DC = .001*P*TH**U
      DFC = W300(JU)*DC
      ALPHA = ALPHAC*F(JU)*SQRT(T)
      RCD = DFC/ALPHA
      JL = JU-1
      N = (JL/2)*2+1
      AN = N
      ANN = N*(N+1)
      BFAC = EXP(ROT*ANN*(1.-TH))
      FAC = .5*.5034E12*P*S300(JU)*BFAC*TH**3/(1.77245*ALPHA)
      GPI = CMPLX(0.,0.)
      GP = GPI
      GM = GPI
      IF(JL.EQ.N) GOTO 20
C
C     LINE IN N- BRANCH
      DF0 = AB*(AN+2.)/ANN
      DN = AN*(2.*AN-1.)*(2.*AN+1.)
      N1 = N-1
      MN1 = -N1
      DO 10 MP=MN1,N1
      AM = MP
      F0 = DF0*AM
      S0 = FAC*3.*(AN+AM)*(AN-AM)/DN
      GPI = GPI + S0*CERROR((F0-DEL)/ALPHA,RCD)
      AM = MP-1
      FP = AB*(AM*(AN+2.)+(AN+1.))/ANN
      SP = FAC*.75*(AN-AM)*(AN-AM-1.)/DN
      GP = GP + SP*CERROR((FP-DEL)/ALPHA,RCD)
      AM = MP+1
      FM = AB*(AM*(AN+2.)-(AN+1.))/ANN
      SM = FAC*.75*(AN+AM)*(AN+AM-1.)/DN
10    GM = GM + SM*CERROR((FM-DEL)/ALPHA,RCD)
      RETURN
C
C     LINE IN N+ BRANCH
20    DF0 = -AB*(AN-1.)/ANN
      DN = (AN+1.)*(2.*AN+1.)*(2.*AN+3.)
      MN = -N
      DO 30 M=MN,N
      AM = M
      F0 = DF0*AM
      S0 = FAC*3.*(AN+1.+AM)*(AN+1.-AM)/DN
      GPI = GPI + S0*CERROR((F0-DEL)/ALPHA,RCD)
      FP = -AB*(AM*(AN-1.)+AN)/ANN
      SP = FAC*.75*(AN+AM+1.)*(AN+AM+2.)/DN
      GP = GP + SP*CERROR((FP-DEL)/ALPHA,RCD)
      FM = -AB*(AM*(AN-1.)-AN)/ANN
      SM = FAC*.75*(AN-AM+1.)*(AN-AM+2.)/DN
30    GM = GM + SM*CERROR((FM-DEL)/ALPHA,RCD)
      RETURN
      END
