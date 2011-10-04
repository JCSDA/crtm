      FUNCTION ABLIQ(WATER,FREQ,TEMP)
C     COMPUTES ABSORPTION IN NEPERS/KM BY SUSPENDED WATER DROPLETS
c     ARGUMENTS (INPUT):
C     WATER IN G/M**3
C     FREQ IN GHZ     (VALID FROM 0 TO 1000 GHZ)
C     TEMP IN KELVIN
C
C     REFERENCES:
C     LIEBE, HUFFORD AND MANABE, INT. J. IR & MM WAVES V.12, pp.659-675
C      (1991);  Liebe et al, AGARD Conf. Proc. 542, May 1993.
c
C     REVISION HISTORY:
C        PWR 8/3/92   original version
c        PWR 12/14/98 temp. dependence of EPS2 eliminated to agree 
c                     with MPM93 
c        pwr 2/27/02  use exponential dep. on T, eq. 2b instead of eq. 4a 
C
      COMPLEX EPS,RE
      IF(WATER.LE.0.) THEN
       ABLIQ = 0.
       RETURN
      ENDIF
      THETA1 = 1.-300./TEMP
      EPS0 = 77.66 - 103.3*THETA1
      EPS1 = .0671*EPS0
      EPS2 = 3.52                 ! from MPM93
      FP = 20.1*EXP(7.88*THETA1)  ! from eq. 2b
      FS = 39.8*FP
      EPS = (EPS0-EPS1)/CMPLX(1.,FREQ/FP) +
     & (EPS1-EPS2)/CMPLX(1.,FREQ/FS) +EPS2
      RE = (EPS-1.)/(EPS+2.)
      ABLIQ = -.06286*AIMAG(RE)*FREQ*WATER
      RETURN
      END



