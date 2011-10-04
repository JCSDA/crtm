      FUNCTION ABSN2(T,P,F)
C  Copyright (c) 2002 Massachusetts Institute of Technology
C     ABSN2 = COLLISION-INDUCED ABSORPTION COEFFICIENT (NEPER/KM)
C     IN AIR
C     T = TEMPERATURE (K)
C     P = PRESSURE (MB)
C     F = FREQUENCY (GHZ)(valid 0-1000 GHz)
C
c     5/22/02 P.Rosenkranz
c
C     Equations based on:
C      Borysow, A, and L. Frommhold, 
C      Astrophysical Journal, v.311, pp.1043-1057 (1986)
C     with modification of 1.29 to account for O2-O2 and O2-N2
c     collisions, as suggested by
C      J.R. Pardo, E.Serabyn, J.Cernicharo, J. Quant. Spectros.
c      Radiat. Trans. v.68, pp.419-433 (2001).
c
      TH = 300./T
      FDEPEN = .5 + .5/(1.+(F/450.)**2)
      BF = 6.5E-14*FDEPEN*P*P*F*F*TH**3.6
      ABSN2 = 1.29*BF
      RETURN
      END
