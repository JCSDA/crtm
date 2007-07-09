C     author:    $Author: paulv $
C     revision:  $Revision: 2.1 $
C     created:   $Date: 2005/09/13 15:30:25 $
      PROGRAM LNFL                                                       LN00010
C
C  --------------------------------------------------------------------------
C |                                                                          |
C |  Copyright 2002, 2003, Atmospheric & Environmental Research, Inc. (AER). |
C |  This software may be used, copied, or redistributed as long as it is    |
C |  not sold and this copyright notice is reproduced on each copy made.     |
C |  This model is provided as is without any express or implied warranties. |
C |                       (http://www.rtweb.aer.com/)                        |
C |                                                                          |
C  --------------------------------------------------------------------------
C
C                                                                        LN00020
C**********************************************************************  LN00030
C                                                                        LN00040
C                            LNFL                    21 OCTOBER 1991     LN00050
C                                                                        LN00060
C    THIS PROGRAM CREATES A LINE DATA FILE FOR LBLRTM.                   LN00070
C                                                                        LN00080
C                                                                        LN00090
C    FOR USE WITH 1986 TAPE FORMAT FOR TAPE1 AND SUPPLEMENTAL OR         LN00100
C    REPLACEMENT LINE DATA ON TAPE2.                                     LN00110
C                                                                        LN00120
C                                                                        LN00130
C    DEFAULT IMPLEMENTATION OF THIS PROGRAM CAUSES LINE FILE DATA        LN00140
C    INCLUDING LINE COUPLING INFORMATION, INTERNALLY STORED IN THIS      LN00150
C    PROGRAM, TO BE WRITTEN TO TAPE2 AND INCORPORATED INTO THE LBLRTM    LN00160
C    LINE FILE, TAPE3. THE INTERNALLY STORED LINE DATA REPLACES LINE     LN00170
C    DATA FROM TAPE1 FOR IDENTICAL TRANSITIONS.                          LN00180
C                                                                        LN00190
C    INTERNAL LINE COUPLING DATA IS INCLUDED FOR OXYGEN IN THE           LN00200
C     0., 2. (60GHZ) AND  4. (120GHZ) CM-1 SPECTRAL REGION AND FOR THE   LN00210
C    CARBON DIOXIDE Q-BRANCHES AT 618., 667., 720., 721.  AND 791. CM-1  LN00220
C    THE DATA IS PROVIDED FOR THE MAIN ISOTOPES ONLY. THE LINE COUPLING  LN00230
C    COEFFICIENTS ARE DUE TO HOKE AND CLOUGH, 1988.                      LN00240
C
C    These coefficients have been updated to provide consistency with
C    HITRAN96 oxygen and carbon dioxide line parameters (June 1999).
C    ADDITIONAL LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES
C    AT 1932 cm-1, 2076 cm-1, 2093 cm-1, 2193 cm-1 (April 2001)
C     - coupling coefficients for these branches are from 
C       Strow et al. 1994
c
c    ******************************************************************* 
c
c     October 2004
c
c     This version of lnfl includes the capability of utilizing the
c     HITRAN2004 line parameter database.  HITRAN2004 uses
c     a new 160 character format; the suffix _160 has been used
c     for the necessary variable and module designations in  LNFL.
c
c     The HITRAN2004 frequencies for carbon dioxide have been modified
c     from those previously used for the line coupling coefficients 
c     included in LNFL.  This has necessitated modification of LNFL
c     to include a revised line coupling module.  Note that at this stage
c     the coupling coefficients have NOT been modified to ensure the
c     proper constraint requirements.  This will be done in the future.
c
c     HITRAN2004 reference:
c     Rothman,  L.S., D. Jacquemart, A. Barbe, D.C. Benner, M. Birk, 
c     L.R. Brown, M.R. Carleer, C. Chackerian, Jr, K. Chance, V. Dana, 
c     V.M. Devi, J.-M. Flaud, R.R. Gamache, A. Goldman J.-M. Hartmann, 
c     K.W. Jucks, A.G. Maki, J.Y. Mandin, S. Massie, J. Orphal, A. Perrin, 
c     C.P. Rinsland, M.A.H. Smith, R.A. Toth, J. Vander Auwera, 
c     P. Varanasi, G. Wagner,  The HITRAN 2004 Molecular Spectroscopic 
c     Database, J. Quant. Spectrosc. Radiat .Transfer, in press, 2005.
c
c    ******************************************************************* 
C                                                                        LN00250
C    LNFL ALSO TRANSFERS TO TAPE3 THE ISOTOPE NUMBER FOR EACH LINE.      LN00260
C                                                                        LN00270
C    THE FILES TAPE1 AND TAPE2 MUST INCLUDE THE LINE PARAMETERS          LN00280
C    REQUIRED FOR THE MOLECULAR TYPES AND WAVENUMBER RANGE SELECTED.     LN00290
C                                                                        LN00300
C______________________________________________________________________  LN00310
C                                                                        LN00320
C                             TAPE1                                      LN00330
C                                                                        LN00340
C    THIS FILE CONTAINS THE AFGL OR EQUIVALENT LINE DATA IN CODED        LN00350
C        FORMAT FOR THE MAIN AND TRACE ATMOSPHERIC MOLECULES.            LN00360
C                                                                        LN00370
C    THE REFERENCE FOR THE 1986 AFGL LINE DATA IS                        LN00380
C                                                                        LN00390
C       L.S. ROTHMAN, R. R. GAMACHE, A. GOLDMAN, L. R. BROWN,            LN00400
C       R. A. TOTH, H. M. PICKETT, R. L. POYNTER, J.-M. FLAUD,           LN00410
C       C. CAMY-PEYRET, A. BARBE, N. HUSSON, C. P. RINSLAND,             LN00420
C       AND M. A. H. SMITH                                               LN00430
C                                                                        LN00440
C                  'THE HITRAN DATABASE: 1986 EDITION,'                  LN00450
C                     APPLIED OPTICS 26,4058 (1987).                     LN00460
C                                                                        LN00470
C                                                                        LN00480
C    TAPE1 and TAPE2 ARE ASSUMED TO BE in UNBLOCKED FORMAT.
C    IF THE BLK1 OPTION IS SELECTED ON INPUT RECORD 1 or the BLK2
C    OPTION ON TAPE2, THE RESPECTIVE FILES ARE ASSUMED TO BE
C    BLOCKED WITH 51 TRANSITIONS PER BLOCK.
C                                                                        LN00520
C    FOR F100 FORMAT THERE ARE 100 CHARACTERS FOR EACH TRANSITION. 
C    FOR F160 FORMAT THERE ARE 160 CHARACTERS FOR EACH TRANSITION. 
C    THE DATA FOR EACH TRANSITION INCLUDE MOLECULE IDENTIFICATION,       LN00540
C    LINE FREQUENCY, INTENSITY, AIR HALF-WIDTH, SELF HALFWIDTH,          LN00550
C    LOWER STATE ENERGY, TEMPERATUE DEPENDENCE OF THE AIR HALFWIDTH,     LN00560
C    PRESSURE SHIFT, UPPER AND LOWER STATE VIBRATIONAL AND ROTATIONAL    LN00570
C    IDENTIFICATIONS, REFERENCE PARAMETERS AND A FLAG FOR LINE           LN00580
C    COUPLING.                                                           LN00590
C                                                                        LN00600
C    MOLECULE NUMBERS 1 THROUGH 38 MAY BE SELECTED.                      LN00610
C                                                                        LN00620
C                                                                        LN00630
C    THE VARIABLES AND THE FORMAT FOR THE TRANSITIONS ON TAPE1 ARE       LN00640
C                                                                        LN00650
C ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,TDEP,SHIFT,IVUP,IVLO,CUP,CLO,IFLG  LN00660
C                                                                        LN00670
C 2X,I1,F12.6,2E10.3, 2F5.4,    F10.4,F4.2, F8.6,   2I3,     2A9,7X,I2   LN00680
C                                                                        LN00690
C                                                                        LN00700
C______________________________________________________________________  LN00710
C                                                                        LN00720
C                                                                        LN00730
C              TAPE2                                                     LN00740
C                                                                        LN00750
C    TAPE2 IS AVAILABLE FOR THE USER TO PROVIDE ALTERNATE LINE DATA      LN00760
C               (REPLACEMENT OR SUPPLEMENTAL)                            LN00770
C                                                                        LN00780
C    MOLECULE NUMBERS 1 THROUGH 38 MAY BE SELECTED.                      LN00790
C                                                                        LN00800
C    TWO FORMAT OPTIONS ARE AVAILABLE: OPTION SELECTED ON RECORD 2.      LN00810
C                                                                        LN00820
C                                                                        LN00830
C    F80        1982 AFGL FORMAT                                         LN00840
C                                                                        LN00850
C    F100       1986 AFGL FORMAT (USE WITH HITRAN 1991)                  LN00860
C                                                                        LN00870
C               LINE COUPLING COEFFICIENTS MAY BE PROVIDED ON THIS FILE  LN00880
C                                                                        LN00890
c    F160       160 column format-  HITRAN 2004
c
C                                                                        LN00900
C    IF FORMAT F100 IS SELECTED, A BLOCKING OPTION IS AVAILABLE:         LN00910
C                                                                        LN00920
C        NBLK2       ONE (1) TRANSITION PER RECORD (NO BLOCKING)         LN00930
C                                                                        LN00940
C                    DEFAULT IS 51 TRANSITIONS PER RECORD                LN00950
C                                                                        LN00960
C______________________________________________________________________  LN00970
C                                                                        LN00980
C                                                                        LN00990
C              TAPE3                                                     LN01000
C                                                                        LN01010
C    LINE DATA FILE FOR LBLRTM; UNFORMATTED                              LN01020
C    TAPE3 NOT COMPATIBLE WITH FASCOD2 CODE                              LN01030
C                                                                        LN01040
C______________________________________________________________________  LN01050
C                                                                        LN01060
C                                                                        LN01070
C              TAPE7                                                     LN01080
C                                                                        LN01090
C    FORMATTED REPRESENTATION OF TAPE3 SELECTED BY OPTION 'LNOUT' ON     LN01100
C    RECORD 2.                                                           LN01110
C                                                                        LN01120
C    ONE TRANSITION PER RECORD IN SAME REPRESENTATION AS DATA ON         LN01130
C    ON TAPE1 AND TAPE2.                                                 LN01140
C                                                                        LN01150
C______________________________________________________________________  LN01160
C                                                                        LN01170
C                                                                        LN01180
C              TAPE10                                                    LN01190
C                                                                        LN01200
C    THIS FILE IS AN INTERMEDIATE UNFORMATTED FILE                       LN01210
C                                                                        LN01220
C**********************************************************************  LN01230
C                                                                        LN01240
C NOTE : IN LINE COUPLING MODE (DEFAULT), THE CORRESPONDING LINES ON     LN01250
C        THE TAPE1 LINE DATA FILE ARE REPLACED BY LINE DATA              LN01260
C        INTERNALLY STORED IN BLOCK DATA IN THIS PROGRAM CONTAINING      LN01270
C        THE Y'S AND THE G'S.                                            LN01280
C                                                                        LN01290
C**********************************************************************  LN01300
C-                                                                       LN01310
C-                      STATEMENT FLAGS                                  LN01320
C-                                                                       LN01330
C-    LNFL   HAS BEEN STRUCTURED TO HAVE ENHANCED PORTABILITY UNDER      LN01340
C-    FORTRAN 77.  FOUR FLAGS (COLUMN73) HAVE BEEN USED TO FACILITATE    LN01350
C-    PROGRAM CONVERSION.                                                LN01360
C-                                                                       LN01370
C-   &    IDENTIFIES STATEMENTS REQUIRED FOR WORD SIZE LESS THAN 8 CHAR  LN01380
C-               ALL STATEMENTS FLAGGED WITH & IN COLUMN 73 HAVE         LN01390
C-            C& STARTING IN COLUMN 1. THESE TWO CHARACTERS MUST         LN01400
C-               BE CHANGED TO BLANKS FOR COMPUTERS WITH WORD SIZE       LN01410
C-               LESS THAN 8 CHARACTERS.                                 LN01420
C-                                                                       LN01430
C-   !    IDENTIFIES STATEMENTS REQUIRED TO DOUBLE PRECISION THE         LN01440
C-               VARIABLES NEEDED FOR CALCULATIONS WHICH NEED MORE       LN01450
C-               THAN 32 BITS TO OBTAIN SUFFICIENT ACCURACY (I.E.        LN01460
C-               THE FREQUENCIES). STATEMENTS FLAGGED WITH ! HAVE        LN01470
C-            C! STARTING IN COLUMN 1. THESE TWO CHARACTERS SHOULD BE    LN01480
C-               CHANGED TO BLANKS FOR COMPUTERS HAVING SINGLE           LN01490
C-               PRECISION LESS THAN 10 SIGNIFICANT DIGITS.              LN01500
C-                                                                       LN01510
C-   #    IDENTIFIES STATEMENTS THAT MAY BE USEFUL FOR ACCELERATED       LN01520
C-               FILE DATA TRANSFER UNDER CDC AND OTHER OPERATING        LN01530
C-               SYSTEMS ALLOWING BUFFERED I/0.                          LN01540
C-                                                                       LN01550
C-   >    IDENTIFIES STATEMENTS THAT MAY BE USEFUL FOR CONVERSION,       LN01560
C-               TYPICALLY SYSTEM SPECIFIC CALLS (I.E. DATE, TIME,       LN01570
C-               CPU TIME, RANDOM NUMBER, ETC.).                         LN01580
C-                                                                       LN01590
C----------------------------------------------------------------------  LN01600
C                                                                        LN01610
      IMPLICIT REAL*8           (V)                                     !LN01620
C                                                                        LN01630
      character*8 HID,HTIME,HDATE,HID1,HMOL  
C                                                                        LN01650
      CHARACTER*8 GREJ,GNLTE,GREJNL,GNEGEPP                              LN01660
C                                                                        LN01670
      CHARACTER*18 hnamlnfl,hvrlnfl,hnamutl,hvrutl
      CHARACTER*5 rev_num
      CHARACTER HNOCPL*5,HREJ*3,HNLTE*4,HMRG2*4,HF160*4,HOLIND1*40 
      CHARACTER HBLK1*4,HBLK2*4,         HF80*3,HF100*4,HOLIND2*40   
      CHARACTER*5 HNBLK1,HNBLK2,HLNOUT,HH86T1,HH86T2                     LN01690
      CHARACTER*27 QUANT1,QUANTC                                         LN01700
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN                               LN01710
c
      character*15 h_vib
C                                                                        LN01720
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALIN(250)             LN01730
C                                                                        LN01740 
      common /vib_map/ ncl_v,nclass_v(64),n_lvl_v(32),h_vib(32,256)
      common /rot_map/ ncl_r,nclass_r(64),n_lvl_r(32)
c
      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN01750
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN01760
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN01770
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN01780
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN01790
C                                                                        LN01800
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN01810
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN01820
     *               IREC,IRECTL,HID1(2),LSTWD                           LN01830
      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2
C                                                                        LN01840
      EQUIVALENCE (HID1(1),HDATE) , (HID1(2),HTIME)                      LN01850
C                                                                        LN01860
      COMMON /BUFIDC/ CMOL(64),CHID10,CHID08                             LN01870
      CHARACTER CMOL*6,CHID10*8,CHID08*8,CFORM*11                        LN01880
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN01890
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN01900
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN01910
     *              MIND1(64),IOUT(51)                                   LN01920
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN01930
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN01940
     *               MINDC(64),IOUTC(51)                                 LN01950
      COMMON /TRAC/ VNU2(40),STR2(40),ALF2(40),EPP2(40),MOL2(40),        LN01960
     *              HWHM2(40),TMPAL2(40),PSHIF2(40),IFG2(40),            LN01970
     *              MIND2(64)                                            LN01980
      COMMON /CPLMOL/ MOLCPL(38),NCPL                                    LN01990
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN02000
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN02010
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN02020
      COMMON /CVRLBL/ HNAMLNFL,HVRLNFL
      COMMON /CVRUTL/ HNAMUTL,HVRUTL
      common /eppinfo/ negflag
C                                                                        LN02030
      DIMENSION MOLCNT(64),IID(10),RCDHDR(5)                             LN02040
      dimension iid2(10)
      DIMENSION IWD1(2),IWD2(2500),AMOL1(51),AMOL2(40),AMOLC(51)         LN02050
C                                                                        LN02060
      EQUIVALENCE (IWD1(1),VLO) , (IWD2(1),VNU3(1))                      LN02070
      EQUIVALENCE (IID(1),HID(1)) , (RCDHDR(1),VLO)                      LN02080
      equivalence (iid2(1),n_negepp(1))
      EQUIVALENCE (MOL1(1),AMOL1(1)) , (MOL2(1),AMOL2(1))                LN02090
      EQUIVALENCE (MOLC(1),AMOLC(1))                                     LN02100
C                                                                        LN02110
      DATA HF80 / 'F80'/,HF100 / 'F100'/,HF160 / 'F160'/,
     *     HNOCPL / 'NOCPL'/, HLNOUT / 'LNOUT'/, HMRG2 / 'MRG2'/
      DATA HREJ / 'REJ'/,HNLTE / 'NLTE'/,HNBLK1 / 'NBLK1'/,              LN02140
     *     HNBLK2 / 'NBLK2'/, HBLK1 /'BLK1'/, HBLK2 /'BLK2'/
      DATA HH86T1 / 'H86T1'/,HH86T2 / 'H86T2'/
      DATA GREJ / ' REJ    '/,GNLTE / ' NLTE   '/,GREJNL / 'NLTE REJ'/   LN02160
      DATA GNEGEPP / '       ^'/
      DATA MOLCNT / 64*0 /                                               LN02170
      DATA VLST1 / -1. /,VLST2 / -2. /                                   LN02180
c
      data mol_max /38/
C                                                                        LN02190
C#    DATA CFORM/'BUFFERED   '/                                          LN02200
      DATA CFORM / 'UNFORMATTED'/                                        LN02210
C                                                                        LN02220
      HNAMLNFL= '           lnfl.f:'     
      HVRLNFL = '$Revision: 2.1 $'
c
      read(hvrlnfl,900) rev_num
      write(chid08,901) rev_num
c
      CALL CPUTIM (TIME0)                                                LN02230
C                                                                        LN02240
      lstwd = -654321
      NWDLIN = NWDL(IID,LSTWD)                                           LN02250
      lstwd2 = -654321
      NWDLIN2 = NWDL(IID2,LSTWD2)
      NBLOCK = 0                                                         LN02260
      lstw1 = -654321
      LRC = NWDL(IWD1,LSTW1)                                             LN02270
      lstw2 = -654321
      ILNGTH = NWDL(IWD2,LSTW2)                                          LN02280
      RADCN2 = PLANCK*CLIGHT/BOLTZ                                       LN02290

      do 5 m=1,64
         n_negepp(m) = 0
         n_resetepp(m) = 0
 5    continue
C                                                                        LN02295
      call set_vib_map
      call set_rot_map
c
      IFIL1 = 1                                                          LN02300
      IFIL2 = 2                                                          LN02310
      LINFIL = 3                                                         LN02320
c
      OPEN (LINFIL,FILE='TAPE3',STATUS='NEW',FORM=CFORM)                 LN02330
      OPEN (IRD,FILE='TAPE5',STATUS='UNKNOWN',FORM='FORMATTED')          LN02340
      OPEN (IPR,FILE='TAPE6',STATUS='UNKNOWN',FORM='FORMATTED')          LN02350
      LINMRG = 10                                                        LN02360
      OPEN (LINMRG,FILE='TAPE10',STATUS='UNKNOWN',FORM=CFORM)            LN02370
      ILIN = 0                                                           LN02380
      NWDS = 0                                                           LN02390
C                                                                        LN02400
      READ (IRD,902) (HID(I),I=1,9)                                      LN02410
C                                                                        LN02420
C     HID CONTAINS 72 CHARACTERS OF HEADER IDENTIFICATION.               LN02430
C                                                                        LN02440
C     CHID08 CONTAINS THE CVS VERSION NUMBER OF LNFL.F, AND
C 
C     CHID10 CONTAINS THE FLAG "I" FOR THE ISOTOPE INCLUSION
C     IN TAPE3, TESTED IN LBLRTM
C
      READ (CHID08,902) HID(8)                                           LN02450
      READ (CHID10,902) HID(10)                                          LN02450
      CALL LBLDAT (HDATE)                                                LN02460
      CALL FTIME (HTIME)                                                 LN02470
      WRITE (IPR,905) HID,HID1                                           LN02480
C                                                                        LN02490
      READ (IRD,910) VMIN,VMAX                                           LN02500
      WRITE (IPR,915) VMIN,VMAX                                          LN02510
C                                                                        LN02520
C     VMIN IS LOW WAVENUMBER LIMIT; VMAX IS HIGH WAVENUMBER LIMIT.       LN02530
C                                                                        LN02540
c----------------------------------------------------------------
c  For TAPE1
c
      READ (IRD,920) (MIND1(I),I=1,mol_max),HOLIND1                            LN02550
C                                                                        LN02560
C     INBLK1 AND INBLK2 ARE FLAGS TO DETERMINE THE BLOCKING FOR          LN02570
C     INPUT FILES TAPE1 AND TAPE2                                        LN02580
C                                                                        LN02590
C     FOR INBLCK  = 0, BLOCKING = 51 TRANSITIONS/BLOCK   100 CHAR./TRAN  LN02600
C     FOR INBLCK  = 1, BLOCKING =  1 TRANSITIONS/BLOCK   100 CHAR./TRAN  LN02610
C     FOR IPUOUT = 1, THE MERGED HITRAN TAPE IS OUTPUT TO TAPE7          LN02620
C                                                                        LN02630
c     new defaults

      INBLK1 = 1                                                         LN02640
      INBLK2 = 1                                                         LN02650
      IPUOUT = 0                                                         LN02660
c
      CALL HOLRT (5,HLNOUT,IPUOUT,HOLIND1,40)                             LN02710
      IF (IPUOUT.EQ.1) WRITE (IPR,925) HLNOUT                            LN02720

      CALL HOLRT (5,HNOCPL,INCPST,HOLIND1,40)                             LN02790
      IF (INCPST.EQ.1) WRITE (IPR,925) HNOCPL                            LN02680
C                                                                        LN02730
      CALL HOLRT (3,HREJ,IREJ,HOLIND1,40)                                 LN03250
      IF (IREJ.EQ.1) WRITE (IPR,930) HREJ                                LN03260

      CALL HOLRT (4,HMRG2,IMRG2,HOLIND1,40)                                 LN03250
      IF (IMRG2.EQ.1) WRITE (IPR,930) HMRG2                              LN03260

      CALL HOLRT (4,HNLTE,INLTE,HOLIND1,40)                               LN03200
      IF (INLTE.EQ.1) WRITE (IPR,930) HNLTE   

      CALL HOLRT (5,HNBLK1,j_NBLK1,HOLIND1,40)                             LN02670
      IF (j_NBLK1.EQ.1) WRITE (*,991) 
 991  format('******   NBLK1 selected:  ignored   ******')

      CALL HOLRT (4,HBLK1,IBLK1,HOLIND1,40)                             LN02670
      IF (IBLK1.EQ.1 .and. j_nblk1.ne.1) then
         INBLK1 = 0
         WRITE (IPR,930) HBLK1 
      endif
 
      CALL HOLRT (4,HF160,IF160_1,HOLIND1,40)   
      IF (IF160_1.EQ.1) WRITE (IPR,930) HF160  

      CALL HOLRT (5,HH86T1,I86T1,HOLIND1,40)
      IF (I86T1.EQ.1) WRITE (IPR,925) HH86T1

      IF (IPUOUT.EQ.1)                                                   LN02740
     *     OPEN (IPU,FILE='TAPE7',STATUS='UNKNOWN',FORM='FORMATTED')     LN02750
C                                                                        LN02800
      OPEN (IFIL1,FILE='TAPE1',STATUS='OLD',FORM='FORMATTED')            LN02810
c
c     ----------------------------------------------------------------
c     the following may be required by the Absoft f90 compiler:
c
c%%      OPEN (IFIL1,FILE='TAPE1',STATUS='OLD',action='read',
c%%     *                                             FORM='FORMATTED')        LN02810
c
c     ----------------------------------------------------------------
C FOR TAPE2
C
      If (imrg2.eq. 1)  then
c
         READ (IRD,920) (MIND2(I),I=1,mol_max),HOLIND2                            LN02550
c
c        Default format for TAPE2 is now 100 character (formerly F100)
c
         CALL HOLRT (5,HNBLK2,j_NBLK2,HOLIND2,40)                             LN02670
         IF (j_NBLK2.EQ.1) WRITE (*,992) 
 992     format('******   NBLK2 selected:  ignored   ******')

         CALL HOLRT (4,HBLK2,IBLK2,HOLIND2,40)                             LN02670
         IF (IBLK2.EQ.1 .and. j_nblk2.ne.1) then
            INBLK2 = 0
            WRITE (IPR,930) HBLK2 
         endif

         CALL HOLRT (3,HF80,IF80,HOLIND2,40)                                 LN02780
         IF (IF80.EQ.1) WRITE (IPR,925) HF80
C
C        FOR HITRAN86
C
c%%%         CALL HOLRT (5,HH86T2,I86T2,HOLIND2,40)
c%%%         IF (I86T2.EQ.1) WRITE (IPR,925) HH86T2
C                                                                        LN02820
      endif
c
      IF (imrg2.eq.0 .and. INCPST.eq.0) THEN               
         OPEN (IFIL2,FILE='TAPE2',STATUS='NEW',FORM='FORMATTED')         LN02870
      ELSE                                                               LN02880
         IF (imrg2.EQ.1) then 
            OPEN (IFIL2,FILE='TAPE2',STATUS='OLD',FORM='FORMATTED')      LN02900
c
c           ----------------------------------------------------------------
c           the following may be required by the Absoft f90 compiler:
c
c%%           OPEN (IFIL2,FILE='TAPE2',STATUS='OLD',action='read',
c%%     *                                              FORM='FORMATTED')       LN02900
c           ----------------------------------------------------------------
         endif
      ENDIF                                                              LN02910
C                                                                        LN02920
c     check to see if line coupling parameters stored in LNFL 
c                                           should be written to TAPE2
c
      IF (imrg2.eq.0 .and. INCPST.EQ.0) THEN 
c        set imrg2 to indicate coupling parameters written to TAPE2
         IMRG2 = -1                                                      LN02940
         if (if160_1 .eq. 1) then
            CALL CPSTOR_160 (molcpl,npl)   
            INBLK2 = 1    
         else
            CALL CPSTOR_100 (molcpl,npl)   
            INBLK2 = 1   
         endif
      ENDIF                                                              LN03010

c
      IF (IMRG2.EQ.1) THEN
c
         DO 8 I = 1, mol_max                                             LN03130
            MINDC(I) = MIND2(I)  
 8       CONTINUE                                           
c
      ELSE if (imrg2.eq.-1) THEN                                         LN03090

         DO 10 I = 1, mol_max                                            LN03130
            MINDC(I) = MOLCPL(I)                            
            IF (MIND1(I).EQ.0) MINDC(I) = 0                 
 10      CONTINUE                                           
      ENDIF                                                              LN03190
c
c     change imrg2 to indicate that coupled lines on TAPE2
c     are to be merged
c
      if (imrg2.eq.1 .and. incpst.eq.0) then
         imrg2 = -1
      endif

      IF (INLTE.EQ.1) THEN                                               LN03210
         READ (GNLTE,902) HID(9)                                         LN03220
         WRITE (IPR,930) HNLTE                                           LN03230
      ENDIF                                                              LN03240
C                                                                        LN03270
C     MOLIND IS AN ARRAY TO SELECT MOLECULES (=1 YES, =0 NO), FORMAT     LN03280
C     (38I1)        -- PUT 1 IN COLUMN CORRESPONDING TO MOLECULE ID.     LN03290
C                                                                        LN03300
      DO 20 I = 1, mol_max                                                    LN03310
         READ (CMOL(I),935) HMOL(I)                                      LN03320
         IF (MIND1(I).GT.0) MOLIND(I) = 1                                LN03330
         IF (MIND2(I).GT.0) MOLIND(I) = MOLIND(I)+2                      LN03340
c
         IF (MOLIND(I).GE.3) PRINT 940,I                                 LN03360
         IF (MOLIND(I).NE.0) NMOL = I                                    LN03370
C                                                                        LN03380
   20 CONTINUE                                                           LN03390
C                                                                        LN03400
      NMOL = MAX(NMOL,7)                                                 LN03410
      WRITE (IPR,945) (HMOL(I),MOLIND(I),I=1,NMOL)                       LN03420
      WRITE (IPR,945)                                                    LN03430
      IF (IREJ.EQ.1) THEN                                                LN03440
         READ (GREJ,902) HID(9)                                          LN03450
         IF (INLTE.EQ.1) READ (GREJNL,902) HID(9)                        LN03460
         READ (IRD,950) (SR(I),I=1,NMOL)                                 LN03470
      ENDIF                                                              LN03480
C                                                                        LN03490
C     IF STRENGTH REJECTION READ IN IS NEGATIVE, SET TO DEFAULT VALUE    LN03500
C                                                                        LN03510
      DO 30 I = 1, NMOL                                                  LN03520
         IF (IREJ.EQ.1.AND.SR(I).LT.0) SR(I) = SRD(I)                    LN03530
   30 CONTINUE                                                           LN03540
C                                                                        LN03550
C     IF HITRAN TAPE, SKIP OVER RECORDS NOT NEEDED                       LN03560
C                                                                        LN03570
      IEOF1 = 0                                                          LN03580
      IEOF2 = 2                                                          LN03590
      IF (INBLK1.EQ.0) CALL SKIPT (IFIL1,VMIN,IEOF1)                     LN03600
      IF (INBLK2.EQ.0.AND.IMRG2.EQ.-1) CALL SKIPT (IFIL2,VMIN,IEOF2)     LN03610
C                                                                        LN03620
C     IF VMIN .LE. .007 AND O2 IS INCLUDED, SET FLAG TO REPLACE          LN03630
C     O2 LINES WITH SPECIAL BAND FOR LINE COUPLING CASE                  LN03640
C                                                                        LN03650
      IO2BND = 0                                                         LN03660
      IF (VMIN.LE..007.AND.IMRG2.EQ.-1.AND.MINDC(7).NE.0) IO2BND = 1     LN03670
c
      if (imrg2.eq.1 .and. if80.eq.1 ) imrg2 = 2
c
c     at this stage imrg2 has three possible values:
c         2   merge with tape2 in  80 column format
c         1   merge with tape2 in 100 column format
c         0   no merge
c        -1   merge inclusive of line coupling coefficients

C                                                                        LN03680
      IF (imrg2.eq.2)   CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)  
c
      IF (IMRG2 .EQ. 1) CALL RDFIL2 (IFIL2,I2,N2,IEOF2)                   LN03700
c
      IF (IMRG2 .EQ.-1) CALL RDFIL2 (IFIL2,I2,N2,IEOF2)                   LN03700
      IF (IO2BND.EQ.-1) WRITE (IPR,955)                                  LN03710
C                                                                        LN03720
   40 IF (IEOF1.EQ.0) THEN                                               LN03730
         If (IF160_1 .eq. 1) then
            CALL RDFIL1_f160 (IFIL1,I1,N1,IEOF1)          
         else
            CALL RDFIL1 (IFIL1,I1,N1,IEOF1)                                 LN03740
         endif
      ELSE                                                               LN03750
         N1 = 0                                                          LN03760
      ENDIF                                                              LN03770
      IF (negflag.EQ.1) THEN
         READ (GNEGEPP,902) HID(7)
      ENDIF                                                              LN03240
      IF (N1.LT.1) GO TO 90                                              LN03780
C                                                                        LN03790
      DO 80 J = I1, N1                                                   LN03800
         IREPLN = 0                                                      LN03810
         I = IOUT(J)                                                     LN03820
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN03830
         IF (MOL1(I).EQ.0) GO TO 80                                      LN03840
   50    IF (N2.LT.1) GO TO 70                                           LN03850
         IF (IMRG2.EQ.1) THEN                                            LN03860
            IF (MOL2(I2).EQ.0) GO TO 60                                  LN03870
            IF (VNU1(I).LE.VNU2(I2)) GO TO 70                            LN03880
            CALL MOVE (LINMRG,I2,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,   LN03890
     *                 TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                  LN03900
         ENDIF                                                           LN03910
         IF (IMRG2.EQ.-1) THEN                                           LN03920
            IF (MOLC(IC).EQ.0) GO TO 60                                  LN03930
C                                                                        LN03940
C     CHECK FOR O2 MOLECULE IF VNU LT .007 TO REPLACE                    LN03950
C     LINE WITH A BAND CENTERED AT .000010 CM-1                          LN03960
C                                                                        LN03970
            IF (IO2BND.EQ.-1.AND.VNU1(I).LT..007) THEN                   LN03980
               MMOL1 = MOD(MOL1(I),100)                                  LN03990
               IF (MMOL1.EQ.7.AND.IFG1(I).NE.3) GO TO 80                 LN04000
            ENDIF                                                        LN04010
            VNUTST = VNUC(IC)-VNU1(I)                                    LN04020
C                                                                        LN04030
C    CHECK TO SEE IF WAVENUMBERS ARE WITHIN  1.0 WAVENUMBERS             LN04040
C    IF SO, CHECK FOR MATCH OF QUANTUM NUMBERS - IF MATCH                LN04050
C    REPLACE LINE IN FILE1 WITH LINE IN FILE2                            LN04060
C                                                                        LN04070
            IF (ABS(VNUTST).LE.1.) THEN                                  LN04080
               IF (QUANTC(IC).EQ.QUANT1(I)) THEN                         LN04090
                  CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,   LN04100
     *                       HWHMC,TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)      LN04110
                  IREPLN = 1                                             LN04120
                  GO TO 60                                               LN04130
               ENDIF                                                     LN04140
            ENDIF                                                        LN04150
            IF (VNU1(I).LE.VNUC(IC)) GO TO 70                            LN04160
            CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,HWHMC,   LN04170
     *                 TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)                  LN04180
         ENDIF                                                           LN04190
   60    I2 = I2+1                                                       LN04200
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN04210
         IF (I2.LE.N2) THEN                                              LN04220
            IF (IREPLN.EQ.1) GO TO 80                                    LN04230
            GO TO 50                                                     LN04240
         ENDIF                                                           LN04250
         IF (IEOF2.EQ.0 .AND. IMRG2.NE.0) THEN                    
            IF (IMRG2.EQ. 2) CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)
            IF (IMRG2.EQ. 1 .or. imrg2.eq.-1) 
     *                       CALL RDFIL2 (IFIL2,I2,N2,IEOF2)          
         ELSE                                                            LN04290
            N2 = 0                                                       LN04300
         ENDIF                                                           LN04310
         IF (IMRG2.EQ.0) IEOF2 = 1                                       LN04320
         IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                     LN04330
         IF (IREPLN.EQ.1) GO TO 80                                       LN04340
         GO TO 50                                                        LN04350
   70    CONTINUE                                                        LN04360
         CALL MOVE (LINMRG,I,VNU1,STR1,ALF1,EPP1,MOL1,AMOL1,HWHM1,       LN04370
     *              TMPAL1,PSHIF1,IFG1,MOLCNT,ALIN1)                     LN04380
   80 CONTINUE                                                           LN04390
      GO TO 40                                                           LN04400
   90 IF (N2.LT.0) GO TO 110                                             LN04410
      IF (I2.GT.N2) GO TO 110                                            LN04420
      IF (I2.EQ.0) GO TO 100                                             LN04430
      IF (IMRG2.EQ.1) THEN                                               LN04440
         CALL MOVE (LINMRG,I2,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,      LN04450
     *              TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                     LN04460
      ELSEIF (IMRG2.EQ.-1) THEN                                          LN04470
         IC = IOUTC(I2)                                                  LN04480
         CALL MOVE (LINMRG,IC,VNUC,STRC,ALFC,EPPC,MOLC,AMOLC,HWHMC,      LN04490
     *              TMPALC,PSHIFC,IFGC,MOLCNT,ALINC)                     LN04500
      ENDIF                                                              LN04510
  100 I2 = I2+1                                                          LN04520
      IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                        LN04530
      IF (I2.LE.N2) GO TO 90                                             LN04540
      IF (IEOF2.EQ.0.AND.IMRG2.NE.0) THEN                                LN04550
            IF (IMRG2.EQ. 2) CALL RDFL82 (IFIL2,I2,N2,IEOF2,VLST2)
            IF (IMRG2.EQ. 1 .or. imrg2.eq.-1) 
     *                       CALL RDFIL2 (IFIL2,I2,N2,IEOF2)
      ELSE                                                               LN04580
         N2 = 0                                                          LN04590
      ENDIF                                                              LN04600
      IF (IMRG2.EQ.-1.AND.I2.GE.1) IC = IOUTC(I2)                        LN04610
      IF (IMRG2.EQ.0) IEOF2 = 1                                          LN04620
      GO TO 90                                                           LN04630
C                                                                        LN04640
  110 CALL BLKOUT (LINMRG)                                               LN04650
      REWIND LINMRG                                                      LN04660
      REWIND LINFIL                                                      LN04670
      DO 120 M = 1, NMOL                                                 LN04680
         MOLIND(M) = MOLCNT(M)                                           LN04690
  120 CONTINUE                                                           LN04700
      WRITE (IPR,960) LINFIL,FLINLO,FLINHI,ILIN                          LN04710
      WRITE (IPR,965) (HMOL(I),MOLCNT(I),MCNTLC(I),MCNTNL(I),
     *     N_NEGEPP(I),N_RESETEPP(I),SUMSTR(I),                          LN04720
     *                 SR(I),I=1,NMOL)                                   LN04730
      CALL BUFOUT (LINFIL,HID(1),NWDLIN)                                 LN04740
      if (negflag.eq.1) then
         CALL BUFOUT (LINFIL,n_negepp(1),NWDLIN2)
      endif
      WRITE (IPR,970) NBLOCK                                             LN04750
C                                                                        LN04760
      DO 130 I = 1, NBLOCK                                               LN04770
         CALL BUFIN (LINMRG,IEOF,RCDHDR(1),LRC)                          LN04780
         NWDS = ILNGTH                                                   LN04790
         CALL BUFOUT (LINFIL,RCDHDR(1),LRC)                              LN04800
         CALL BUFIN (LINMRG,IEOF,VNU3(1),ILNGTH)                         LN04810
         CALL CKFL (VLO,VHI,LINES,VNU3,IFLG)                             LN04820
         CALL BUFOUT (LINFIL,VNU3(1),ILNGTH)                             LN04830
  130 CONTINUE                                                           LN04840
C                                                                        LN04850
      CALL CPUTIM (TIME1)                                                LN04860
      TIME = TIME1-TIME0                                                 LN04870
      WRITE (IPR,975) TIME,TIME0,TIME1                                   LN04880
      WRITE(IPR,980) hnamlnfl,hvrlnfl,hnamutl,hvrutl
C                                                                        LN04890
      STOP ' LINFIL COMPLETE '                                           LN04900
C                                                                        LN04910
 900  format(10x,a5)
 901  format('V: ',a5)
 902  FORMAT (10A8)                                                      LN04920
 905  FORMAT ('1',10A8,2(1X,A8,1X))                                      LN04930
 910  FORMAT (2F10.3)                                                    LN04940
 915  FORMAT ('0',20X,'VMIN =',F12.6,' CM-1,      VMAX =',F12.6,         LN04950
     *        ' CM-1')                                                   LN04960
c            mol_max
 920  FORMAT (38I1,4X,A40)                                               LN04970
 925  FORMAT ('0',' ** NOTE IFLG SET - ',A5,' *****',/)                  LN04980
 930  FORMAT ('0',' ** NOTE IFLG SET - ',A4,' *****',/)                  LN04990
 935  FORMAT (A6)                                                        LN05000
 940  FORMAT (' YOU HAVE ASKED FOR THE SAME MOLECULE ON  TWO TAPES',/,   LN05010
     *        '      DUPLICATE LINES MAY ARISE.  MOLECULE = ',I3)        LN05020
 945  FORMAT (' ',40X,A6,' = ',I1)                                       LN05030
 950  FORMAT (8E10.3)                                                    LN05040
 955  FORMAT ('0',' O2 LINES < .007 CM-1 HAVE BEEN REPLACED BY A O2',    LN05050
     *        1X,'BAND CENTERED AT .000010 CM-1 ',/)                     LN05060
 960  FORMAT ('0',9X,'TAPE NO. =',I2/10X,'LOWEST LINE =',F12.6,          LN05070
     *        ' CM-1,  ','HIGHEST LINE =',F12.6,                         LN05080
     *        ' CM-1,  TOTAL NUMBER OF LINES =',I7)                      LN05090
 965  FORMAT ('0',/,23X,'COUPLED',4X,'NLTE',3X,'NEGATIVE',3X,
     *        'RESET',4X,'SUM LBLRTM',6X,                                LN05100
     *        'STRENGTH',/,7X,'MOL',5X,'LINES',4X,'LINES',4X,'LINES',
     *        6X,'EPP',6X,'EPP',   
     *        6X,'STRENGTHS',6X,'REJECTION',2(/),(' ',4X,A6,' = ',I6,    LN05120
     *        3X,I6,3X,I6,3X,I6,3X,i6,3X,1PE12.4,5X,E10.3,0P))           LN05130
 970  FORMAT ('0',20X,'NUMBER OF BLOCKS =',I4)                           LN05140
 975  FORMAT ('0',10X,' TOTAL TIME =',F10.3,' TIME IN =',F10.3,          LN05150
     *        ' TIME OUT =',F10.3)                                       LN05160
 980  FORMAT (//'0 Modules and versions used in this calculation:',/,/,
     *         5X,a18,2X,A18,10X,a18,2X,A18,/)
C                                                                        LN05170
      END                                                                LN05180
      FUNCTION NWDL (IWD,ILAST)                                          LN06490
C                                                                        LN06500
      DIMENSION IWD(*)                                                   LN06510
C                                                                        LN06520
c      ILAST = -654321                                                    LN06530
      DO 10 I = 1, 9000                                                  LN06540
         IF (IWD(I).EQ.ILAST) THEN                                       LN06550
            NWDL = I-1                                                   LN06560
            RETURN                                                       LN06570
         ENDIF                                                           LN06580
   10 CONTINUE                                                           LN06590
C                                                                        LN06600
      STOP ' NWDL - IWD,ILAST '                                          LN06610
C                                                                        LN06620
      END                                                                LN06630
      SUBROUTINE HOLRT (N,HOL,IHOL,HOLIND,NMAX)                          LN06640
C                                                                        LN06650
      CHARACTER HOL*(*),HOLIND*40,BLNK*1                                 LN06660
C                                                                        LN06670
      DATA BLNK / ' '/                                                   LN06680
C                                                                        LN06690
      IHOL = 0                                                           LN06700
      J = N-1                                                            LN06710
      IMAX = NMAX-J                                                      LN06720
      DO 10 I = 1, IMAX                                                  LN06730
         IF (HOLIND(I:I+J).EQ.HOL(1:N)) THEN                             LN06740
            IHOL = 1                                                     LN06750
            RETURN                                                       LN06760
         ENDIF                                                           LN06770
   10 CONTINUE                                                           LN06780
C                                                                        LN06790
      RETURN                                                             LN06800
C                                                                        LN06810
      END                                                                LN06820
      SUBROUTINE CKFL (VLO,VHI,LINES,VNU3,IFLG)                          LN06830
C                                                                        LN06840
      IMPLICIT REAL*8           (V)                                     !LN06850
C                                                                        LN06860
      DIMENSION VNU3(250),IFLG(250)                                      LN06870
C                                                                        LN06880
      VST = VLO                                                          LN06890
      DO 10 I = 1, LINES                                                 LN06900
         IF (IFLG(I).GE.0) THEN                                          LN06910
            IF (VNU3(I).LT.VST) PRINT 900,I,VST,VNU3(I)                  LN06920
            VST = VNU3(I)                                                LN06930
            IF (VNU3(I).GT.VHI) PRINT 905,I,VHI,VNU3(I)                  LN06940
         ENDIF                                                           LN06950
   10 CONTINUE                                                           LN06960
C                                                                        LN06970
      RETURN                                                             LN06980
C                                                                        LN06990
  900 FORMAT (' * CKFL * VNU LT VST: I = ',I5,' VST = ',F15.5,           LN07000
     *        ' VNU(I) = ',F15.5)                                        LN07010
  905 FORMAT (' * CKFL * VNU GT VHI: I = ',I5,' VHI = ',F15.5,           LN07020
     *        ' VNU(I) = ',F15.5)                                        LN07030
C                                                                        LN07040
      END                                                                LN07050
      SUBROUTINE SKIPT (IFIL,VNU,IEOF)                                   LN07060
C                                                                        LN07070
      IMPLICIT REAL*8           (V)                                     !LN07080
C                                                                        LN07090
   10 READ (IFIL,900,END=20) VA                                          LN07100
C                                                                        LN07110
C#    IF (UNIT(IFIL) .EQ. 0.) IEOF=0                                     LN07120
C                                                                        LN07130
      IEOF = 0                                                           LN07140
      IF (VA.LT.VNU) GO TO 10                                            LN07150
      BACKSPACE IFIL                                                     LN07160
      BACKSPACE IFIL                                                     LN07170
C                                                                        LN07180
      RETURN                                                             LN07190
C                                                                        LN07200
   20 PRINT 905,IFIL,VNU                                                 LN07210
      IEOF = 1                                                           LN07220
C                                                                        LN07230
      RETURN                                                             LN07240
C                                                                        LN07250
  900 FORMAT (3X,F12.6)                                                  LN07260
  905 FORMAT (' ERROR HIT EOF ON TAPE ',I2,' BEFORE ',F10.2)             LN07270
C                                                                        LN07280
      END                                                                LN07290
c  ****************************************
      BLOCK DATA Isotop
c  ****************************************
c
      PARAMETER (NMOL=38)
      COMMON /ISVECT/ ISO_MAX(NMOL)
      common /iso_id/ iso_82(97)
c
c    The number of isotopes for a particular molecule:
      DATA (ISO_MAX(I),I=1,NMOL)/
c     H2O, CO2, O3, N2O, CO, CH4, O2,
     +  6,   9,  9,   5,  6,   3,  3,
c      NO, SO2, NO2, NH3, HNO3, OH, HF, HCl, HBr, HI,
     +  3,   2,   1,   2,    1,  3,  1,   2,   2,  1,
c     ClO, OCS, H2CO, HOCl, N2, HCN, CH3Cl, H2O2, C2H2, C2H6, PH3
     +  2,   5,    3,    2,  1,   3,     2,    1,    2,    1,   1,
c     COF2, SF6, H2S, HCOOH, HO2, O, ClONO2, NO+, HOBr, C2H4
     +  1,   1,   3,     1,   1,  1,     2,    1,    2,    2/
c
      DATA ISO_82/
c       H2O
     +   161,181,171,162,182,172,
c       CO2
     +  626,636,628,627,638,637,828,728,727,
c       O3
     +  666,668,686,667,676,886,868,678,768,
c       N2O
     +  446,456,546,448,447,
c       CO,                 CH4
     +  26,36,28,27,38,37,  211,311,212,
c       O2,        NO,        SO2
     +  66,68,67,  46,56,48  ,626,646,
c       NO2,   NH3,        HNO3
     +  646,   4111,5111,  146,
c       OH,        HF,  HCl,    HBr,    HI
     +  61,81,62,  19,  15,17,  19,11,  17,
c       ClO,    OCS,                 H2CO
     +  56,76,  622,624,632,623,822,  126,136,128,
c       HOCl,     N2,  HCN
     +  165,167,  44,  124,134,125
c      CH3Cl,    H2O2,  C2H2,       C2H6,  PH3
     +, 215,217,  1661,  1221,1231,  1221,  1111,
c       COF2, SF6, H2S,           HCOOH,  HO2, O,   ClONO2      NO+
     +  269,  29,  121,141,131,   126,    166, 6,   5646,7646,  46,
c       HOBr,      C2H4
     +  169,161,   221,231/  
c
C
      END
c*******************************************************************************
c
      SUBROUTINE MOVE (LINMRG,I,VNU2,STR2,ALF2,EPP2,MOL2,AMOL2,HWHM2,    LN08580
     *                 TMPAL2,PSHIF2,IFG2,MOLCNT,ALIN2)                  LN08590
C                                                                        LN08600
      IMPLICIT REAL*8           (V)                                     !LN08610
C                                                                        LN08620
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN08630
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN08640
     *               IREC,IRECTL,HID1(2),LSTWD                           LN08650

      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2
      common /eppinfo/ negflag

      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN08660
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN08670
C                                                                        LN08680
      CHARACTER*8      HID,HID1,HMOL                                    &LN08690
C                                                                        LN08700
      DIMENSION VNU2(*),STR2(*),ALF2(*),EPP2(*),MOL2(*),AMOL2(*),        LN08710
     *          HWHM2(*),TMPAL2(*),PSHIF2(*),IFG2(*),ALIN2(*)            LN08720
      DIMENSION MOLCNT(*)                                                LN08730
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN08740
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN08750
      COMMON /LCHAR/ ALIN1(51),ALINE(40),ALINC(51),ALIN(250)             LN08760
C                                                                        LN08770
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN,ALINE                         LN08780
      DIMENSION AMOL3(250)                                               LN08790
C                                                                        LN08800
      EQUIVALENCE (AMOL3(1),MOL3(1))                                     LN08810
C                                                                        LN08820
      M = MOD(MOL2(I),100)                                               LN08830
      ILIN3 = ILIN3+1                                                    LN08840
      NWDS = NWDS+9                                                      LN08850
      VNU3(ILIN3) = VNU2(I)                                              LN08860
      STR3(ILIN3) = STR2(I)                                              LN08870
      ALF3(ILIN3) = ALF2(I)                                              LN08880
      EPP3(ILIN3) = EPP2(I)                                              LN08890
      MOL3(ILIN3) = MOL2(I)                                              LN08900
      HWHMS(ILIN3) = HWHM2(I)                                            LN08910
      TMPALF(ILIN3) = TMPAL2(I)                                          LN08920
      PSHIFT(ILIN3) = PSHIF2(I)                                          LN08930
      IFLG(ILIN3) = IFG2(I)                                              LN08940
      ALIN(ILIN3) = ALIN2(I)                                             LN08950
      ILIN = ILIN+1                                                      LN08960
      SUMSTR(M) = SUMSTR(M)+STR2(I)                                      LN08970
      MOLCNT(M) = MOLCNT(M)+1                                            LN08980

c     Check for negative ENERGY values
c     If ENERGY = -1., then set ENERGY to 300.
c     If ENERGY = -n, then set ENERGY to n

      if (iflg(ilin3).eq.0 .and. epp3(ilin3) .lt. -0.99) then
         negflag = 1
         n_negepp(m) = n_negepp(m)+1
      endif

      IF (IFG2(I).GE.1) THEN                                             LN08990
         NWDS = NWDS+9                                                   LN09000
         ILIN3 = ILIN3+1                                                 LN09010
         VNU3(ILIN3) = VNU2(I+1)                                         LN09020
         STR3(ILIN3) = STR2(I+1)                                         LN09030
         ALF3(ILIN3) = ALF2(I+1)                                         LN09040
         EPP3(ILIN3) = EPP2(I+1)                                         LN09050
         AMOL3(ILIN3) = AMOL2(I+1)                                       LN09060
         HWHMS(ILIN3) = HWHM2(I+1)                                       LN09070
         TMPALF(ILIN3) = TMPAL2(I+1)                                     LN09080
         PSHIFT(ILIN3) = PSHIF2(I+1)                                     LN09090
         IFLG(ILIN3) = IFG2(I+1)                                         LN09100
         ALIN(ILIN3) = ALIN2(I+1)                                        LN09110
         MCNTLC(M) = MCNTLC(M)+1                                         LN09120
         ILINLC = ILINLC+1                                               LN09130
      ENDIF                                                              LN09140
c
      MIS = MOD(MOL2(I),1000)                                            LN09143
      IF (MIS.NE.MOL2(I)) THEN                                           LN09146
         MCNTNL(M) = MCNTNL(M)+1                                         LN09150
         ILINNL = ILINNL+1                                               LN09160
      ENDIF                                                              LN09165
      IF (ILIN3.GE.NMAX-1) CALL BLKOUT (LINMRG)                          LN09170
C                                                                        LN09180
      RETURN                                                             LN09190
C                                                                        LN09200
      END                                                                LN09210
      SUBROUTINE BLKOUT (LINMRG)                                         LN09220
C                                                                        LN09230
      IMPLICIT REAL*8           (V)                                     !LN09240
C                                                                        LN09250
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN09260
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN09270
     *               IREC,IRECTL,HID1(2),LSTWD                           LN09280

      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2

C                                                                        LN09290
      CHARACTER*8      HID,HID1,HMOL                                    &LN09300
C                                                                        LN09310
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN09320
      COMMON VNU3(250),STR3(250),ALF3(250),EPP3(250),MOL3(250),          LN09330
     *       HWHMS(250),TMPALF(250),PSHIFT(250),IFLG(250),LSTW2          LN09340
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALIN(250)             LN09350
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALIN,ALINE                         LN09360
      COMMON /ICN/ I3,NMAX,NBLOCK                                        LN09370
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN09380
      DIMENSION RCDHDR(5)                                                LN09390
C                                                                        LN09400
      EQUIVALENCE (RCDHDR(1),VLO)                                        LN09410
      CHARACTER*50 ZEROFL                                                LN09420
C                                                                        LN09430
      DATA ZEROFL /                                                      LN09440
     *           '00000000000000000000000000000000000000000000000000'/   LN09450
C                                                                        LN09460
      NBLOCK = NBLOCK+1                                                  LN09470
      WRITE (ALINE,900) ZEROFL,ZEROFL                                    LN09480
      IF (I3.EQ.NMAX) GO TO 20                                           LN09490
      I1 = I3+1                                                          LN09500
      IF (I1.GT.NMAX) GO TO 20                                           LN09510
      DO 10 I = I1, NMAX                                                 LN09520
         NWDS = NWDS+9                                                   LN09530
         VNU3(I) = 0.                                                    LN09540
         STR3(I) = 0.                                                    LN09550
         ALF3(I) = 0.                                                    LN09560
         EPP3(I) = 0.                                                    LN09570
         MOL3(I) = 0.                                                    LN09580
         HWHMS(I) = 0.                                                   LN09590
         TMPALF(I) = 0.                                                  LN09600
         IFLG(I) = 0                                                     LN09610
   10 CONTINUE                                                           LN09620
   20 VLO = VNU3(1)                                                      LN09630
      ITST = I3                                                          LN09640
   30 IF (IFLG(ITST).GE.0) THEN                                          LN09650
         VHI = VNU3(ITST)                                                LN09660
      ELSE                                                               LN09670
         ITST = ITST-1                                                   LN09680
         GO TO 30                                                        LN09690
      ENDIF                                                              LN09700
      IF (NBLOCK.EQ.1) FLINLO = VLO                                      LN09710
      FLINHI = VHI                                                       LN09720
      LINES = I3                                                         LN09730
      CALL BUFOUT (LINMRG,RCDHDR(1),LRC)                                 LN09740
      CALL BUFOUT (LINMRG,VNU3(1),ILNGTH)                                LN09750
      IF (IPUOUT.NE.1) GO TO 50                                          LN09760
      DO 40 IOUT = 1, I3                                                 LN09770
         IF (ALIN(IOUT).EQ.ALINE) GO TO 40                               LN09780
         WRITE (IPU,905) ALIN(IOUT)                                      LN09790
   40 CONTINUE                                                           LN09800
   50 NWDS = 0                                                           LN09810
      LINES = 0                                                          LN09820
      I3 = 0                                                             LN09830
C                                                                        LN09840
      RETURN                                                             LN09850
C                                                                        LN09860
  900 FORMAT (2A50)                                                      LN09870
  905 FORMAT (A100)                                                      LN09880
C                                                                        LN09890
      END                                                                LN09900
c-------------------------------------------------------------------------------
      SUBROUTINE RDFIL1_f160 (LINBCD,I1,N1,IEOF)                              LN09910
C                                                                        LN09920
      IMPLICIT REAL*8           (V)                                     !LN09930
C                                                                        LN09940
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN09950
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN09960
     *               IREC,IRECTL,HID1(2),LSTWD                           LN09970
c
      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2
C                                                                        LN09980
      CHARACTER*8      HID,HID1,HMOL                                    &LN09990
      REAL*8           STRSV
C                                                                        LN10000
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN10010
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN10020
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN10030
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN10040
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN10050
      COMMON /IC1/ ILIN3                                                 LN10060
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN10070
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN10080
     *              MIND1(64),IOUT(51)                                   LN10090
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN10100
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALINE(250)            LN10110
c 
      common /vib_map/ ncl_v,nclass_v(64),n_lvl_v(32),h_vib(32,256)
      common /rot_map/ ncl_r,nclass_r(64),n_lvl_r(32)
c
      parameter (ntmol=38)
c
      COMMON /ISVECT/ ISO_MAX(NTMOL)
      common /eppinfo/ negflag
c
      character*8 h_rdlin1
c
      data h_rdlin1/' tape1 '/
C                                                                        LN10120
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALINE
      CHARACTER*160 ALIN(51)  
      character*15 h_vib,hvib_u,hvib_l
      character*15 h_cup,h_clo
      CHARACTER*50 ZEROFL                                                LN10140
      CHARACTER*27 QUANT1,QUANTC                                         LN10150
      CHARACTER*9 CUP,CLO
      character*8 clo_rd
      CHARACTER*7 HOL                                                    LN10160
      CHARACTER*1 CFLAG,CBLNK,CMINUS,hr_1,h_1,h_2

      character*1 a_15
      character*2 a_11,a_12,a_13,a_14
      character*1 b_15
      character*2 b_11,b_12,b_13,b_14

      character*1 b_21,b_23
      character*3 b_22
      character*5 b_24
 
      character*2 a_31,a_32,a_33
      character*3 a_34
      character*2 b_31,b_32,b_33
      character*3 b_34

      character*1 a_45
      character*2 a_41,a_42,a_43,a_44
      character*4 a_46
      character*1 b_45
      character*2 b_41,b_42,b_43,b_44
      character*4 b_46

      character*1 b_51,b_53,b_56
      character*2 b_52,b_54
      character*5 b_55

      character*1 b_61,b_63
      character*3 b_62
      character*5 b_64
 
      DIMENSION AMOL1(51)                                                LN10180
C                                                                        LN10190
      EQUIVALENCE (MOL1(1),AMOL1(1))                                     LN10200
C                                                                        LN10210
      DATA TEMP0 / 296. /                                                LN10220
      DATA ZEROFL /                                                      LN10230
     *           '00000000000000000000000000000000000000000000000000'/   LN10240
      DATA CBLNK / ' '/,CMINUS / '-'/, n_hdr/0/, h_1/'>'/, h_2/'%'/
C                                                                        LN10260
      BETA0 = RADCN2/TEMP0                                               LN10270
      IER = 0                                                            LN10280
      IEOF = 0                                                           LN10290
      I1 = 1                                                             LN10300
      N1 = 0                                                             LN10310
      ILIN3 = 0                                                          LN10320
C                                                                        LN10330
   10 CONTINUE                                                           LN10340
      IF (IEOF.EQ.1) GO TO 60                                            LN10350
      IEOF = 1                                                           LN10360
      IBLK = 51                                                          LN10370
      IF (INBLK1.EQ.0) THEN                                              LN10380
         READ (LINBCD,900,END=60) ALIN                                   LN10390
      ELSE                                                               LN10400
         IBLK = 50                                                       LN10410
         DO 20 I = 1, IBLK                                               LN10420
            READ (LINBCD,900,END=30) ALIN(I)                             LN10430
 20      CONTINUE                                                        LN10440
         READ (ALIN(IBLK),905) CFLAG,IFLAG                               LN10450
         IF (IFLAG.LT.0.AND.CFLAG.NE.CBLNK.AND.CFLAG.NE.CMINUS) THEN     LN10460
            IBLK = 51                                                    LN10470
            READ (LINBCD,900,END=30) ALIN(IBLK)                          LN10480
         ENDIF                                                           LN10490
      ENDIF                                                              LN10500
C                                                                        LN10510
      IEOF = 0                                                           LN10520
      GO TO 40                                                           LN10530
   30 IBLK = I-1                                                         LN10540
      IF (IBLK.LE.0) GO TO 60                                            LN10550
   40 IFLGM1 = 0                                                         LN10560

      DO 50 I = 1, 51                                                    LN10570
         IF (I.GT.IBLK) WRITE (ALIN(I),910) ZEROFL,ZEROFL                LN10580
c
c     skip over header records:

         READ (ALIN(I),913) hr_1
         if (hr_1.eq.h_1 .or. hr_1.eq.h_2) then
            n_hdr = n_hdr+1
            if (n_hdr.eq. 1) Write (*,914)     
            go to 50
         endif

c     test for invalid molecule number:

         READ (ALIN(I),915) MOL                                          LN10590
         IF (MOL.le.0) GO TO 50                                          LN10600
         M = MOL                                                         LN10610
         IF (MIND1(M).EQ.0) GO TO 50                                     LN10620
         IF (IFLGM1.LE.0) THEN                                          

c           not a record with line coupling

c_______________________________________________________________________
c           READ (ALIN(I),920) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,   LN10640
c    *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN10650

c            write (*,*) m,nclass_r(m)
c            write (*,924) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,
c     *                    TDEP,SHIFT,hvib_u,hvib_l,CUP,CLO,HOL,IFLGSV 
c_old *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN10650

 924  FORMAT (2X,I1,F12.6,1p,D10.3,E10.3,0p,2F5.4,F10.4,F4.2,F8.6,
     *    2A15,A9,6x,a9,6x,A7,5x,I1) 


c     New Format:

c       mol,iso, freq,   inten,ein_a,   al_f,al_s,     E_l,   al_T,shft,
c        I2, I1,F12.6,1P,E10.3,E10.3,0P,F5.4,F5.4,1P,F10.4,0P,F4.2,F8.6,
c         2,  3,   15,      25,   35,     40,  45,      55,     59,  67,
c      hv_u,hv_l,  r_u,r_l,  err,ref, mx, g_u, g_l
c       A15, A15,  A15,A15,  6I1,6I2, A1,F7.1,F7.1
c        82,  97,  112,127,  133,145,146, 153, 160
c_______________________________________________________________________

            if (nclass_r(m) .eq. 0) then

               READ (ALIN(I),950) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l,  CUP,CLO,  HOL,IFLGSV

 950           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15,  A9,6x,a9,6x,A7,5x,I1)

c_______________________________________________________________________

            else if (nclass_r(m) .eq. 1) then

c              H2O, O3, SO2, NO2, HNO3, H2CO,HOCl, H2O2,COF2, H2S, HO2,
c              HCOOH, ClONO2, HOBr, C2H4

c              tested with H2O

               READ (ALIN(I),951) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l, 
     *              a_11,a_12,a_13,a_14,a_15,
     *              b_11,b_12,b_13,b_14,b_15,               HOL,IFLGSV

 951           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15, 
     *           1x,a2,1x,a2,1x,a2,3x,a2,a1,
     *           1x,a2,1x,a2,1x,a2,3x,a2,a1,      A7,5x,I1)

c     hitran upper:
c                i3,i3,i3,a5,a1
c     hitran lower:
c                i3,i3,i3,a5,a1

               write (cup,961)  a_11,a_12,a_13,a_14,a_15
               write (clo,961)  b_11,b_12,b_13,b_14,b_15
 961           format (a2,a2,a2,a2,a1)

c_______________________________________________________________________

            else if (nclass_r(m) .eq. 2) then

c              CO2, N2O, CO, HF, HCl, HBr, HI,OCS, N2, HCN, C2H2, NO+

c              tested with CO2

               READ (ALIN(I),952) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l,  CUP,
     *              b_21,b_22,b_23,b_24,                    HOL,IFLGSV

 952           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15, 10x,a5,
c%%% *           5x,a1,1x,a2, a1,a5,      A7,5x,I1)

     *           5x,a1,   a3, a1,a5,      A7,5x,I1)

c     hitran upper:
c               10x,a5
c     hitran lower:
c                5x,a1,i3,a1,a5


c              write (clo,962)  b_21,  b_22, b_23
c%%% 962           format     (4x,a1, 1X,a2,   a1)
 962           format         (4x,a1,    a3,   a1)

c     The symmetry has been dropped here!
               write (clo,962)  b_21,b_22

c_______________________________________________________________________

            else if (nclass_r(m) .eq. 3) then

c              SF6, CH4

c              tested with CH4

               READ (ALIN(I),953) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l, 
     *              a_31,a_32,a_33,a_34,
     *              b_31,b_32,b_33,b_34,                    HOL,IFLGSV

 953           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15, 
     *           2x,1x,a2,a2,1x,a2,3x,a2,
     *           2x,1x,a2,a2,1x,a2,3x,a2,      A7,5x,I1)

c     hitran upper:
c                2x,i3,a2,i3,a5
c     hitran lower:
c                2x,i3,a2,i3,a5

               write (cup,963)  a_31,a_32,a_33,a_34
               write (clo,963)  b_31,b_32,b_33,b_34
 963           format (a2,a2,1x,a2,a2)

c               write (*,*) m, nclass_v(m), nclass_r(m)

c               write (*,*) cup,a_31,a_32,a_33,A_34
c               write (*,*) clo,b_31,b_32,b_33,b_34

c_______________________________________________________________________

            else if (nclass_r(m) .eq. 4) then

c              CH3D, CH3Cl, C2H6, NH3, PH3,CH3OH

c              tested with NH3

c              ************  how is CH3D differentiated from CH4 

               READ (ALIN(I),954) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l, 
     *              a_41,a_42,a_43,a_44,a_45,a_46,
     *              b_41,b_42,b_43,b_44,b_45,b_46,          HOL,IFLGSV

 954           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15, 
     *           1x,a2,1x,a2,a2,a2,a1,a4,
     *           1x,a2,1x,a2,a2,a2,a1,a4,      A7,5x,I1)

c     hitran upper:
c                i3,i3,i2,a2,a1,a4
c     hitran lower:
c                i3,i3,i2,a2,a1,a4

               write (cup,964)  a_41,a_42,a_43,a_44,a_45
               write (clo,964)  b_41,b_42,b_43,b_44,b_45
 964           format (a2,a2,a2,a1,1x,a1)

c               write (*,*) m, nclass_v(m), nclass_r(m)

c               write (*,*) cup,a_41,a_42,a_43,A_44
c               write (*,*) clo,b_41,b_42,b_43,b_44

c_______________________________________________________________________

           else if (nclass_r(m) .eq. 5) then

c              O2         

c              tested with O2

               READ (ALIN(I),955) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l,  CUP,
     *              b_51,b_52,b_53,b_54,b_55,b_56,  HOL,IFLGSV

 955           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *              2A15, 10x,a5,
     *           1x,a1,1x,a2,a1,1x,a2,3x,a2,a1,      A7,5x,I1)

c     hitran upper:
c               10x,a5
c     hitran lower:
c                1x,a1,i3,a1,i3,a5,a1


               write (clo,965)  b_51,b_52,b_53,b_54,b_55,b_56
 965           format (a1,a2,a1,a2,a2,a1)

c_______________________________________________________________________

            else if (nclass_r(m) .eq. 6) then

c              NO, OH, ClO

c              tested with NO  symmetry not converted to +/-

               READ (ALIN(I),956) 
     *              ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,TDEP,SHIFT,
     *              hvib_u,hvib_l,  CUP,
     *              b_61,b_62,b_63,b_64,                    HOL,IFLGSV

 956           format(2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *                      2A15, 10x,a5,
     *                      3x,a1,1x,a4, a1,a5,      A7,5x,I1)

c              hitran upper:
c                   10x,a5
c              hitran lower:
c                    3x,a1,f5.1,a1,a5
 
c              write (clo,966)  b_61,b_62,b_63,b_64
 966           format (3x,a1,a4,a1,a3)

c              F"  has been dropped here!
               write (clo,966)  b_61,b_62,b_63

c_______________________________________________________________________
            endif

            IFLAG = IFLGSV                                               LN10660
         
c
c     check that molecule and isotope are within proper range
c
            if (m   .lt. 1) then
                write(*,*) ALIN(I) 
                stop ' molecule value is less than 1 '
            endif
            if (iso .lt. 1) then
                write(*,*) ALIN(I) 
                stop '  isotope value is less than 1 '
            endif
c
c   the TIPS program in lblrtm is currently limited to molecules up to 38
c
            if (m .gt. nmol) then
               call line_exception (1,ipr,h_rdlin1,m,nmol,iso,iso_max)
               go to 50
            else if (iso .gt. iso_max(m)) then
               call line_exception (2,ipr,h_rdlin1,m,nmol,iso,iso_max)
               go to 50
            endif
c
            MOL = MOL+100*ISO                                            LN10670
            IF (IFLAG.LT.0) THEN                                         LN10680
               IFLAG = IABS(IFLAG)                                       LN10690
            ELSE                                                         LN10700
               IFLAG = 0                                                 LN10710
            ENDIF                                                        LN10720
            IFLGM1 = IFLAG
            VNUS = VNU                                                   LN10730
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50                                   LN12380
            IF (VNUS.GT.VMAX) RETURN                                     LN12390
C
c     at this stage, map vibrational designations to numeric form

            m_cl = nclass_v(m)

            ivup = -99
            ivlo = -99

            do lvl=1,n_lvl_v(m_cl)
               if (hvib_u .eq. h_vib(m_cl,lvl)) ivup = lvl
               if (hvib_l .eq. h_vib(m_cl,lvl)) ivlo = lvl
c              write (*,*) lvl,ivup,ivlo,hvib_u,hvib_l,h_vib(m_cl,lvl)
            end do

c     test for valid vibrational states

            if (ivup.eq.-99 .or. ivlo.eq.-99) then
               write (*,*) 
     *   '****   CHECK THAT TAPE1 IS 160 CHARACTER FORMAT   ****'
                STOP 
     *   '****   INVALID VIBRATIONAL STATE ENCOUNTERED ****'

            endif

            STR = STRSV/(VNU*(1.0-EXP(-BETA0*VNU)))                      LN10760
            IF (STR.LT.SR(M)) GO TO 50                                   LN10770
            IF (INLTE.EQ.1) CALL VIBQ1 (MOL,IVUP,IVLO)                   LN10780
            ILIN3 = ILIN3+1                                              LN10790
            ILINS = ILIN3                                                LN10795
            WRITE (QUANT1(ILIN3),930) M,ISO,IVUP,IVLO,CUP,CLO            LN10800
C                                                                        LN10810
C           NOTE: SHIFT VARIABLE MAY CONTAIN COUPLING INFORMATION        LN10820
C            - NO PRESSURE SHIFT INFORMATION CURRENTLY PROVIDED,         LN10830
C                  AND COUPLING INFORMATION DELETED -                    LN10840
C                                                                        LN10850
            IF (I86T1.EQ.1) SHIFT = 0.                                   LN10860
C                                                                        LN10870
C           HWHMS = 0. IN HITRAN 86.  CHECK FOR ZERO; IF ZERO            LN10880
C           AND NOT WATER, SET TO HWHMF.  IF WATER, SET TO 5. * HWHMF    LN10890
C                                                                        LN10900
            IF (HWHMS.EQ.0.) THEN                                        LN10910
               HWHMS = HWHMF                                             LN10920
               IF (M.EQ.1) HWHMS = 5.*HWHMF                              LN10930
            ENDIF                                                        LN10940
c
            if (hwhms.le.0.9999)  then
               WRITE (ALIN1(ILIN3),921) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN10950
     *                              HWHMS,ENERGY,TDEP,SHIFT,IVUP,        LN10960
     *                              IVLO,CUP,CLO,HOL,IFLGSV              LN10970
            else
               WRITE (ALIN1(ILIN3),922) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN10950
     *                              HWHMS,ENERGY,TDEP,SHIFT,IVUP,        LN10960
     *                              IVLO,CUP,CLO,HOL,IFLGSV              LN10970
            endif
c
            VNU1(ILIN3)   = VNU                                          LN10980
            STR1(ILIN3)   = STR                                          LN10990
            ALF1(ILIN3)   = HWHMF                                        LN11000
            EPP1(ILIN3)   = ENERGY                                       LN11010
            MOL1(ILIN3)   = MOL                                          LN11020
            HWHM1(ILIN3)  = HWHMS                                        LN11025
            TMPAL1(ILIN3) = 1.-TDEP                                      LN11030
            PSHIF1(ILIN3) = SHIFT                                        LN11040
            IFG1(ILIN3)   = IFLAG                                        LN11060
            N1 = N1+1                                                    LN11070
C
C           SET FLAG FOR O2 0.0 CM-1 BAND CASE
C
            IF (IO2BND.EQ.1.AND.IFLAG.EQ.3) IO2BND = -1                  LN11080
            IOUT(N1) = ILINS                                             LN11090

         ELSE                                                            LN11100
C
C           LINE COUPLING INFORMATION READ IN
C
            READ (ALIN(I),925) Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG             LN11110
C
C           SET IFLGM1 TO ZERO TO ENSURE THE NEXT PASS USES 
C           REGULAR FORMAT FOR READING LINE INFORMATION
C
            IFLGM1 = 0                                                   LN11120
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50
            ILIN3 = ILIN3 + 1                                            LN11130
            ALIN1(ILIN3)  = ALIN(I)                                      LN11140
            VNU1(ILIN3)   = Y1                                           LN11150
            STR1(ILIN3)   = G1                                           LN11160
            ALF1(ILIN3)   = Y2                                           LN11170
            EPP1(ILIN3)   = G2                                           LN11180
            AMOL1(ILIN3)  = Y3                                           LN11190
            HWHM1(ILIN3)  = G3                                           LN11200
            TMPAL1(ILIN3) = Y4                                           LN11210
            PSHIF1(ILIN3) = G4                                           LN11220
            IFG1(ILIN3)   = IFLAG                                        LN11230
         ENDIF                                                           LN11240
C
   50 CONTINUE                                                           LN11250
C                                                                        LN11260
      IF (ILIN3.LT.1.AND.IBLK.LT.50) GO TO 60                            LN11290
      IF (ILIN3.LT.1) GO TO 10                                           LN11300
C                                                                        LN11310
      RETURN                                                             LN11320
C                                                                        LN11330
   60 CONTINUE                                                           LN11340
C                                                                        LN11350
      PRINT 940                                                          LN11360

c      ENDIF                                                              LN11390
C                                                                        LN11400
      RETURN                                                             LN11410
C                                                                        LN11420
c900   FORMAT (51(A100))                                                  LN11430
 900  FORMAT (51(A160))                                                  LN11430
 905  FORMAT (2X,A1,95X,I2)                                              LN11440
 910  FORMAT (2A50)                                                      LN11450
 913  format (a1)
 914  format ( ' skipping over header records on TAPE1')
 915  FORMAT (I2)                                                        LN11460
      
      
c920   FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,
c    *        A7,I2)                                                     LN11470
 
 920  FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,
     *    2A15,A9,7x,a8,6x,A7,5x,I1) 
 
 921  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 922  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.3,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 925  FORMAT (2X,4(E13.6,E11.4),I2)                                      LN11480
 930  FORMAT (I2,I1,2I3,2A9)                                             LN11490
 940  FORMAT (' TAPE1 IS AT A EOF ')
c940   FORMAT (' TAPE1 IS AT A EOF ',/,                                   LN11510
c     *        ' THE TAPE2 HAS ONE EOF AT EOT  ')
 945  FORMAT (' THE EOF ON TAPE2 IS BEFORE',/,
     *        ' VMAX = ',F12.5,' YOU MAY HAVE A BAD TAPE ')              LN11540
C                                                                        LN11550
      END                                                                LN11560
c-------------------------------------------------------------------------------
      SUBROUTINE RDFIL1 (LINBCD,I1,N1,IEOF)                              LN09910
C                                                                        LN09920
      IMPLICIT REAL*8           (V)                                     !LN09930
C                                                                        LN09940
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN09950
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN09960
     *               IREC,IRECTL,HID1(2),LSTWD                           LN09970
c
      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2
C                                                                        LN09980
      CHARACTER*8      HID,HID1,HMOL                                    &LN09990
      REAL*8           STRSV
C                                                                        LN10000
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN10010
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN10020
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN10030
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN10040
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN10050
      COMMON /IC1/ ILIN3                                                 LN10060
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN10070
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN10080
     *              MIND1(64),IOUT(51)                                   LN10090
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN10100
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALINE(250)            LN10110
c 
      parameter (ntmol=38)
c
      COMMON /ISVECT/ ISO_MAX(NTMOL)
      common /eppinfo/ negflag
c
      character*8 h_rdlin1
      character*1 h_alin_70,h_alin_73,h_blnk
c
      data h_rdlin1/' tape1 '/, h_blnk/' '/,
     *     h_alin_70/' '/, h_alin_73/' '/
C                                                                        LN10120
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALINE,ALIN(51)                     LN10130
      CHARACTER*50 ZEROFL                                                LN10140
      CHARACTER*27 QUANT1,QUANTC                                         LN10150
      CHARACTER*9 CUP,CLO
      CHARACTER*7 HOL                                                    LN10160
      CHARACTER*1 CFLAG,CBLNK,CMINUS,a_1,h_1,h_2
      DIMENSION AMOL1(51)                                                LN10180
C                                                                        LN10190
      EQUIVALENCE (MOL1(1),AMOL1(1))                                     LN10200
C                                                                        LN10210
      DATA TEMP0 / 296. /                                                LN10220
      DATA ZEROFL /                                                      LN10230
     *           '00000000000000000000000000000000000000000000000000'/   LN10240
      DATA CBLNK / ' '/,CMINUS / '-'/, n_hdr/0/, h_1/'>'/, h_2/'%'/
C                                                                        LN10260
      BETA0 = RADCN2/TEMP0                                               LN10270
      IER = 0                                                            LN10280
      IEOF = 0                                                           LN10290
      I1 = 1                                                             LN10300
      N1 = 0                                                             LN10310
      ILIN3 = 0                                                          LN10320
C                                                                        LN10330
   10 CONTINUE                                                           LN10340
      IF (IEOF.EQ.1) GO TO 60                                            LN10350
      IEOF = 1                                                           LN10360
      IBLK = 51                                                          LN10370
      IF (INBLK1.EQ.0) THEN                                              LN10380
         READ (LINBCD,900,END=60) ALIN                                   LN10390
      ELSE                                                               LN10400
         IBLK = 50                                                       LN10410
         DO 20 I = 1, IBLK                                               LN10420
            READ (LINBCD,900,END=30) ALIN(I)                             LN10430
   20    CONTINUE                                                        LN10440
         READ (ALIN(IBLK),905) CFLAG,IFLAG                               LN10450
         IF (IFLAG.LT.0.AND.CFLAG.NE.CBLNK.AND.CFLAG.NE.CMINUS) THEN     LN10460
            IBLK = 51                                                    LN10470
            READ (LINBCD,900,END=30) ALIN(IBLK)                          LN10480
         ENDIF                                                           LN10490
      ENDIF                                                              LN10500
C                                                                        LN10510
      IEOF = 0                                                           LN10520
      GO TO 40                                                           LN10530
   30 IBLK = I-1                                                         LN10540

      IF (IBLK.LE.0) GO TO 60                                            LN10550
   40 IFLGM1 = 0                                                         LN10560
      DO 50 I = 1, 51                                                    LN10570
         IF (I.GT.IBLK) WRITE (ALIN(I),910) ZEROFL,ZEROFL                LN10580
c
c     skip over header records:
         READ (ALIN(I),913) a_1
         if (a_1.eq.h_1 .or. a_1.eq.h_2) then
            n_hdr = n_hdr+1
            if (n_hdr.eq. 1) Write (*,914)     
            go to 50
         endif
c
c     test for invalid molecule number:
         READ (ALIN(I),915) MOL                                          LN10590
         IF (MOL.le.0) GO TO 50                                          LN10600
         M = MOL                                                         LN10610
         IF (MIND1(M).EQ.0) GO TO 50                                     LN10620
         IF (IFLGM1.LE.0) THEN                                           LN10630
c
c     check vibrational data 


            read (alin(i),918) h_alin_70, h_alin_73   
 918        format(69x,a1,2x,a1)
            if (h_alin_70.eq.h_blnk .or. h_alin_73.eq.h_blnk) then
               write (*,919) 
 919           format
     *             ('***  vibrational quantum numbers invalid  ***',/,
     *              '***  check format specification of TAPE1  ***')
               stop '***        invalid TAPE1                  ***'
            endif

            READ (ALIN(I),920) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,   LN10640
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN10650
            IFLAG = IFLGSV                                               LN10660
c
c     check that molecule and isotope are within proper range
c
            if (m   .lt. 1) then
                write(*,*) ALIN(I) 
                stop ' molecule value is less than 1 '
            endif
            if (iso .lt. 1) then
                write(*,*) ALIN(I) 
                stop '  isotope value is less than 1 '
            endif
c
c   the TIPS program in lblrtm is currently limited to molecules up to 38
c
            if (m .gt. nmol) then
               call line_exception (1,ipr,h_rdlin1,m,nmol,iso,iso_max)
               go to 50
            else if (iso .gt. iso_max(m)) then
               call line_exception (2,ipr,h_rdlin1,m,nmol,iso,iso_max)
               go to 50
            endif
c
            MOL = MOL+100*ISO                                            LN10670
            IF (IFLAG.LT.0) THEN                                         LN10680
               IFLAG = IABS(IFLAG)                                       LN10690
            ELSE                                                         LN10700
               IFLAG = 0                                                 LN10710
            ENDIF                                                        LN10720
            IFLGM1 = IFLAG
            VNUS = VNU                                                   LN10730
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50                                   LN12380
            IF (VNUS.GT.VMAX) RETURN                                     LN12390
C
            STR = STRSV/(VNU*(1.0-EXP(-BETA0*VNU)))                      LN10760
            IF (STR.LT.SR(M)) GO TO 50                                   LN10770
            IF (INLTE.EQ.1) CALL VIBQ1 (MOL,IVUP,IVLO)                   LN10780
            ILIN3 = ILIN3+1                                              LN10790
            ILINS = ILIN3                                                LN10795
            WRITE (QUANT1(ILIN3),930) M,ISO,IVUP,IVLO,CUP,CLO            LN10800
C                                                                        LN10810
C           NOTE: SHIFT VARIABLE MAY CONTAIN COUPLING INFORMATION        LN10820
C            - NO PRESSURE SHIFT INFORMATION CURRENTLY PROVIDED,         LN10830
C                  AND COUPLING INFORMATION DELETED -                    LN10840
C                                                                        LN10850
            IF (I86T1.EQ.1) SHIFT = 0.                                   LN10860
C                                                                        LN10870
C           HWHMS = 0. IN HITRAN 86.  CHECK FOR ZERO; IF ZERO            LN10880
C           AND NOT WATER, SET TO HWHMF.  IF WATER, SET TO 5. * HWHMF    LN10890
C                                                                        LN10900
            IF (HWHMS.EQ.0.) THEN                                        LN10910
               HWHMS = HWHMF                                             LN10920
               IF (M.EQ.1) HWHMS = 5.*HWHMF                              LN10930
            ENDIF                                                        LN10940
c
            if (hwhms.le.0.9999)  then
               WRITE (ALIN1(ILIN3),921) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN10950
     *                              HWHMS,ENERGY,TDEP,SHIFT,IVUP,        LN10960
     *                              IVLO,CUP,CLO,HOL,IFLGSV              LN10970
            else
               WRITE (ALIN1(ILIN3),922) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN10950
     *                              HWHMS,ENERGY,TDEP,SHIFT,IVUP,        LN10960
     *                              IVLO,CUP,CLO,HOL,IFLGSV              LN10970
            endif
c
            VNU1(ILIN3)   = VNU                                          LN10980
            STR1(ILIN3)   = STR                                          LN10990
            ALF1(ILIN3)   = HWHMF                                        LN11000
            EPP1(ILIN3)   = ENERGY                                       LN11010
            MOL1(ILIN3)   = MOL                                          LN11020
            HWHM1(ILIN3)  = HWHMS                                        LN11025
            TMPAL1(ILIN3) = 1.-TDEP                                      LN11030
            PSHIF1(ILIN3) = SHIFT                                        LN11040
            IFG1(ILIN3)   = IFLAG                                        LN11060
            N1 = N1+1                                                    LN11070
C
C           SET FLAG FOR O2 0.0 CM-1 BAND CASE
C
            IF (IO2BND.EQ.1.AND.IFLAG.EQ.3) IO2BND = -1                  LN11080
            IOUT(N1) = ILINS                                             LN11090
         ELSE                                                            LN11100
C
C           LINE COUPLING INFORMATION READ IN
C
            READ (ALIN(I),925) Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG             LN11110
C
C           SET IFLGM1 TO ZERO TO ENSURE THE NEXT PASS USES 
C           REGULAR FORMAT FOR READING LINE INFORMATION
C
            IFLGM1 = 0                                                   LN11120
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50
            ILIN3 = ILIN3 + 1                                            LN11130
            ALIN1(ILIN3)  = ALIN(I)                                      LN11140
            VNU1(ILIN3)   = Y1                                           LN11150
            STR1(ILIN3)   = G1                                           LN11160
            ALF1(ILIN3)   = Y2                                           LN11170
            EPP1(ILIN3)   = G2                                           LN11180
            AMOL1(ILIN3)  = Y3                                           LN11190
            HWHM1(ILIN3)  = G3                                           LN11200
            TMPAL1(ILIN3) = Y4                                           LN11210
            PSHIF1(ILIN3) = G4                                           LN11220
            IFG1(ILIN3)   = IFLAG                                        LN11230
         ENDIF                                                           LN11240
C
   50 CONTINUE                                                           LN11250
C                                                                        LN11260
      IF (ILIN3.LT.1.AND.IBLK.LT.50) GO TO 60                            LN11290
      IF (ILIN3.LT.1) GO TO 10                                           LN11300
C                                                                        LN11310
      RETURN                                                             LN11320
C                                                                        LN11330
   60 CONTINUE                                                           LN11340
C                                                                        LN11350
      PRINT 940                                                          LN11360

      RETURN                                                             LN11410
C                                                                        LN11420
 900  FORMAT (51(A100))                                                  LN11430
 905  FORMAT (2X,A1,95X,I2)                                              LN11440
 910  FORMAT (2A50)                                                      LN11450
 913  format (a1)
 914  format ( ' skipping over header records on TAPE1')
 915  FORMAT (I2)                                                        LN11460
 920  FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,
     *        A7,I2)                                                     LN11470
 921  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 922  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.3,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 925  FORMAT (2X,4(E13.6,E11.4),I2)                                      LN11480
 930  FORMAT (I2,I1,2I3,2A9)                                             LN11490
 940  FORMAT (' TAPE1 IS AT A EOF ')
C                                                                        LN11550
      END                                                                LN11560
      SUBROUTINE RDFIL2 (LINBCD,I2,N2,IEOF)                              LN11570
C                                                                        LN11580
      IMPLICIT REAL*8           (V)                                     !LN11590
C                                                                        LN11600
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN11610
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN11620
     *               IREC,IRECTL,HID1(2),LSTWD                           LN11630

      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2
C                                                                        LN11640
      CHARACTER*8      HID,HID1,HMOL                                    &LN11650
      REAL*8           STRSV
C                                                                        LN11660
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN11670
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN11680
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN11690
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN11700
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN11710
      COMMON /IC2/ ILIN3                                                 LN11720
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN11730
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN11740
     *               MINDC(64),IOUTC(51)                                 LN11750
      COMMON /QUANT/ QUANT1(51),QUANTC(51)                               LN11760
      COMMON /LCHAR/ ALIN1(51),ALIN2(40),ALINC(51),ALINE(250)            LN11770
c 
      parameter (ntmol=38)
c
      COMMON /ISVECT/ ISO_MAX(NTMOL) 
c
      character*8 h_rdlin2
c
      data h_rdlin2/' tape2 '/
C                                                                        LN11780
      CHARACTER*100 ALIN1,ALIN2,ALINC,ALINE,ALIN(51)                     LN11790
      CHARACTER*50 ZEROFL                                                LN11800
      CHARACTER*27 QUANT1,QUANTC                                         LN11810
      CHARACTER*9 CUP,CLO                                                LN11820
      CHARACTER*7 HOL
      CHARACTER*1 CFLAG,CBLNK,CMINUS,hr_1,h_1,h_2
      DIMENSION AMOLC(51)                                                LN11840
C                                                                        LN11850
      EQUIVALENCE (MOLC(1),AMOLC(1))                                     LN11860
C                                                                        LN11870
      DATA TEMP0 / 296. /                                                LN11880
      DATA ZEROFL /                                                      LN11890
     *           '00000000000000000000000000000000000000000000000000'/   LN11900
      DATA CBLNK / ' '/,CMINUS / '-'/,n_hdr/0/, h_1/'>'/, h_2/'%'/
C                                                                        LN11920
      BETA0 = RADCN2/TEMP0                                               LN11930
      IER = 0                                                            LN11940
      IEOF = 0                                                           LN11950
      I2 = 1                                                             LN11960
      N2 = 0                                                             LN11970
      ILIN3 = 0                                                          LN11980
C                                                                        LN11990
   10 CONTINUE                                                           LN12000
      IF (IEOF.EQ.1) GO TO 60                                            LN12010
      IEOF = 1                                                           LN12020
      IBLK = 51                                                          LN12030
      IF (INBLK2.EQ.0) THEN                                              LN12040
         READ (LINBCD,900,END=60) ALIN                                   LN12050
      ELSE                                                               LN12060
         IBLK = 50                                                       LN12070
         DO 20 I = 1, IBLK                                               LN12080
            READ (LINBCD,900,END=30) ALIN(I)                             LN12090
   20    CONTINUE                                                        LN12100
         READ (ALIN(IBLK),905) CFLAG,IFLAG                               LN12110
         IF (IFLAG.LT.0.AND.CFLAG.NE.CBLNK.AND.CFLAG.NE.CMINUS) THEN     LN12120
            IBLK = 51                                                    LN12130
            READ (LINBCD,900,END=30) ALIN(IBLK)                          LN12140
         ENDIF                                                           LN12150
      ENDIF                                                              LN12160
C                                                                        LN12170
      IEOF = 0                                                           LN12180
      GO TO 40                                                           LN12190
   30 IBLK = I-1                                                         LN12200
      PRINT 940
      IF (IBLK.LE.0) GO TO 60                                            LN12210
   40 IFLGM1 = 0                                                         LN12220
c
      DO 50 I = 1, 51                                                    LN12230
         IF (I.GT.IBLK) WRITE (ALIN(I),910) ZEROFL,ZEROFL                LN12240
c
c        skip over header records:
         READ (ALIN(I),913) hr_1
         if (hr_1.eq.h_1 .or. hr_1.eq.h_2) then
            n_hdr = n_hdr+1
            if (n_hdr.eq. 1) Write (*,914)     
            go to 50
         endif
c
c     test for invalid molecule number:
         READ (ALIN(I),915) MOL                                          LN12250
         IF (MOL.EQ.0) GO TO 50                                          LN12260
         M = MOL                                                         LN12270
         IF (MINDC(M).EQ.0) GO TO 50                                     LN12280
         IF (IFLGM1.LE.0) THEN                                           LN12290
            READ (ALIN(I),920) ISO,VNU,STRSV,TRANS,HWHMF,HWHMS,ENERGY,   LN12300
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HOL,IFLGSV   LN12310
            IFLAG = IFLGSV
c
c     check that molecule and isotope are within proper range
c
            if (m   .lt. 1) then
                write(*,*) ALIN(I) 
                stop ' molecule value is less than 1 '
            endif
            if (iso .lt. 1) then
                write(*,*) ALIN(I) 
                stop '  isotope value is less than 1 '
            endif
c
c   the TIPS program in lblrtm is currently limited to molecules up to 38
c
            if (m .gt. nmol) then
               call line_exception (1,ipr,h_rdlin2,m,nmol,iso,iso_max)
               go to 50
            else if (iso .gt. iso_max(mol)) then
               call line_exception (2,ipr,h_rdlin2,m,nmol,iso,iso_max)
               go to 50
            endif
c
            MOL = MOL+100*ISO
            IF (IFLAG.LT.0) THEN                                         LN12320
               IFLAG = IABS(IFLAG)                                       LN12330
            ELSE                                                         LN12340
               IFLAG = 0                                                 LN12350
            ENDIF                                                        LN12360
            IFLGM1 = IFLAG
            VNUS = VNU                                                   LN12370
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50                                   LN12380
            IF (VNUS.GT.VMAX) RETURN                                     LN12390
C
            STR = STRSV/(VNU*(1.0-EXP(-BETA0*VNU)))                      LN12400
            IF (STR.LT.SR(M)) GO TO 50                                   LN12410
            IF (INLTE.EQ.1) CALL VIBQ1 (MOL,IVUP,IVLO)                   LN12420
            ILIN3 = ILIN3+1                                              LN12430
            ILINS = ILIN3
            WRITE (QUANTC(ILIN3),930) M,ISO,IVUP,IVLO,CUP,CLO            LN12440
C
C           NOTE: SHIFT VARIABLE MAY CONTAIN COUPLING INFORMATION        LN12450
C            - NO PRESSURE SHIFT INFORMATION CURRENTLY PROVIDED,         LN12460
C                     AND COUPLING INFORMATION DELETED -                 LN12470
C
            IF (I86T2.EQ.1) SHIFT = 0.                                   LN12480
C
C           HWHMS = 0. IN HITRAN 86.  CHECK FOR ZERO; IF ZERO
C           AND NOT WATER, SET TO HWHMF.  IF WATER, SET TO 5. * HWHMF
C
            IF (HWHMS.EQ.0.) THEN                                        LN12490
               HWHMS = HWHMF                                             LN12500
               IF (M.EQ.1) HWHMS = 5.*HWHMF                              LN12510
            ENDIF                                                        LN12520
c
            if (hwhms.le.0.9999) then
               WRITE (ALINC(ILIN3),921) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN12530
     *                               HWHMS,ENERGY,TDEP,SHIFT,IVUP,       LN12540
     *                               IVLO,CUP,CLO,HOL,IFLGSV             LN12550
            else
               WRITE (ALINC(ILIN3),922) M,ISO,VNU,STRSV,TRANS,HWHMF,        LN12530
     *                               HWHMS,ENERGY,TDEP,SHIFT,IVUP,       LN12540
     *                               IVLO,CUP,CLO,HOL,IFLGSV             LN12550
            endif
c
            VNUC(ILIN3)   = VNU                                          LN12560
            STRC(ILIN3)   = STR                                          LN12570
            ALFC(ILIN3)   = HWHMF                                        LN12580
            EPPC(ILIN3)   = ENERGY                                       LN12590
            MOLC(ILIN3)   = MOL                                          LN12600
            HWHMC(ILIN3)  = HWHMS                                        LN12610
            TMPALC(ILIN3) = 1.-TDEP                                      LN12620
            PSHIFC(ILIN3) = SHIFT                                        LN12630
            IFGC(ILIN3)   = IFLAG                                        LN12640
            N2 = N2+1                                                    LN12650
C                                                                        LN12660
C           SET FLAG FOR O2 0.0 CM-1 BAND CASE                           LN12670
C                                                                        LN12680
            IF (IO2BND.EQ.1.AND.IFLAG.EQ.3) IO2BND = -1                  LN12690
            IOUTC(N2) = ILINS                                            LN12700
         ELSE                                                            LN12710
C                                                                        LN12720
C           LINE COUPLING INFORMATION READ IN                            LN12730
C                                                                        LN12740
            READ (ALIN(I),925) Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG             LN12750
C
C           SET IFLGM1 TO ZERO TO ENSURE THE NEXT PASS USES 
C           REGULAR FORMAT FOR READING LINE INFORMATION
C
            IFLGM1 = 0                                                   LN12760
C
C           SKIP OVER UNEEDED WAVENUMBERS
C
            IF (VNUS.LT.VMIN) GO TO 50
            ILIN3 = ILIN3 + 1                                            LN12770
            ALINC(ILIN3)  = ALIN(I)                                      LN12780
            VNUC(ILIN3)   = Y1                                           LN12790
            STRC(ILIN3)   = G1                                           LN12800
            ALFC(ILIN3)   = Y2                                           LN12810
            EPPC(ILIN3)   = G2                                           LN12820
            AMOLC(ILIN3)  = Y3                                           LN12830
            HWHMC(ILIN3)  = G3                                           LN12835
            TMPALC(ILIN3) = Y4                                           LN12840
            PSHIFC(ILIN3) = G4                                           LN12850
            IFGC(ILIN3)   = IFLAG                                        LN12870
         ENDIF                                                           LN12890
C                                                                        LN12900
   50 CONTINUE                                                           LN12910
C                                                                        LN12920
      IF (ILIN3.LT.1.AND.IBLK.LT.50) GO TO 60                            LN12950
      IF (ILIN3.LT.1) GO TO 10                                           LN12960
C                                                                        LN12970
      RETURN                                                             LN12980
C                                                                        LN12990
   60 CONTINUE                                                           LN13000
C                                                                        LN13020
      RETURN                                                             LN13030
C                                                                        LN13040
 900  FORMAT (51(A100))                                                  LN13050
 905  FORMAT (2X,A1,95X,I2)                                              LN13060
 910  FORMAT (2A50)                                                      LN13070
 913  format (a1)
 914  format ( ' skipping over header records on TAPE2')
 915  FORMAT (I2)                                                        LN13080
 920  FORMAT (2X,I1,F12.6,D10.3,E10.3,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,
     *        A7,I2)                                                     LN13090
 921  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 922  FORMAT (I2,I1,F12.6,1P,D10.3,E10.3,0P,2F5.3,F10.4,F4.2,F8.6,2I3,
     *        2A9,A7,I2)
 925  FORMAT (2X,4(E13.6,E11.4),I2)                                      LN13100
 930  FORMAT (I2,I1,2I3,2A9)                                             LN13110
 940  FORMAT (' TAPE2 IS AT A EOF ')                                     LN13130
C                                                                        LN13140
      END                                                                LN13150
c-----------------------------------------------------------------------
c
      subroutine line_exception(ind,ipr,h_sub,mol,nmol,iso,iso_max)

      character*8 h_sub
      dimension iso_max(*)

      data  mol_max_pr_1/-99/, iso_max_pr_1/-99/
      
      if ((ind.eq.1 .and. mol_max_pr_1.lt.0) .or.
     *    (ind.eq.2 .and. iso_max_pr_1.lt.0)) then
         write (*,*)
         write (*,*) 'Line file exception encountered in', h_sub
         write (*,*) 'This message only written for first exception',
     *               ' for molecule and isotope cases'
         write (*,*) 'Other exceptions may exist'

         write (ipr,*) '****************************************'
         write (ipr,*) 'Line file exception encountered'
         write (ipr,*) 'This message only written for first exception'
         write (ipr,*) 'Other exceptions may exist'
         write (ipr,*) '****************************************'
         write (ipr,*)
      endif
c
      if (ind .eq. 1) then
          if (mol_max_pr_1 .lt. 0) then
             mol_max_pr_1 = 11
             write (*,*)
             write (*,*)   ' tape3: molecule number ', mol,
     *             ' greater than ', nmol,' encountered and skipped'
             write (ipr,*) ' tape3: molecule number ', mol,
     *             ' greater than ', nmol,' encountered and skipped'
               write (*,*)
            endif
            go to 25
c
         else if (ind .eq. 2) then
            if (iso_max_pr_1 .lt. 0) then
               iso_max_pr_1 = 11
               write (*,*)
               write (*,*)   ' tape3: molecule number ', mol
               write (ipr,*) ' tape3: molecule number ', mol

               write (*,*)   ' tape3: isotope number ', iso,
     *                       ' greater than ', iso_max(mol),
     *                       ' encountered and skipped'
               write (ipr,*) ' tape3: isotope number ', iso,
     *                       ' greater than ', iso_max(mol),
     *                       ' encountered and skipped'
               write (*,*)
            endif
            go to 25
         endif
C
 25      continue

         return
         end
c-----------------------------------------------------------------------

      Subroutine set_vib_map

      character*15 h_vib

      common /vib_map/ ncl_v,nclass_v(64),n_lvl_v(32),h_vib(32,256)

c     molecules:       64
c     classes:         32
c     vib_ids/class:  256
      
      data ncl_v /10/,  nmol /39/

      data (nclass_v(m),m=1,39)  /

c    *       1,       2,       3,       4,       5,       6,       7,
c    *     H2O,     CO2,      O3,     N2O,      CO,     CH4,      O2,
     *       6,       5,       6,       4,       1,      10,       2,
c
c    *       8,       9,      10,      11,      12,      13,      14,
c    *      NO,     SO2,     NO2,     NH3,    HNO3,      OH,      HF,
     *       3,       6,       6,       8,      10,       3,       1,
c
c    *      15,      16,      17,      18,      19,      20,      21,
c    *     HCl,     HBr,      HI,     ClO,     OCS,    H2CO,    HOCl,
     *       1,       1,       1,       3,       4,       9,       6,
c
c    *      22,      23,      24,      25,      26,      27,      28,
c    *      N2,     HCN,   CH3Cl,    H2O2,    C2H2,    C2H6,     PH3,
     *       1,       4,      10,       9,       7,      10,       8,
c
c    *      29,      30,      31,      32,      33,      34,      35,
c    *    COF2,     SF6,     H2S,   HCOOH,     HO2,       O,  ClONO2,
     *       9,      10,       6,      10,       6,       1,      10,
c
c    *      36,      37,      38,      39,
c    *     NO+,    HOBr,    C2H4,   CH3OH,
     *       1,       6,      10,      10 /
c
      do mol=1,nmol
c         write (*,925) mol,h_dum, nclass_v(mol)
 925     format(i5,a8,i5)
      end do

      data (n_lvl_v(j),j=1,10)    /
c    *    1,    2,    3,    4,    5,    6,    7,
     *   24,   29,   38,   91,  132,  116,   36,
c    *    8,    9,   10,   11,   12,   13,   14,
     *   47,   39,   91/

      do j_cl=1,ncl_v
c         write      (*,935) n_lvl_v(j_cl)
 935     format(i5)
         
      end do

      data ( h_vib(1,lvl),lvl=1,24 ) /
     1 '              0', '              1', '              2',
     1 '              3', '              4', '              5',
     1 '              6', '              7', '              8',
     1 '              9', '             10', '             11',
     1 '             12', '             13', '             14',
     1 '             15', '             16', '             17',
     1 '             18', '             19', '             20',
     1 '             21', '             22', '             23'/

      data ( h_vib(2,lvl),lvl=1,29) /
     2 '            X 0', '            X 1', '            a 0',
     2 '            a 1', '            b 0', '            b 1',
     2 '            b 2', '               ', '            X 2',
     2 '            B 0', '            B 1', '            B 2',
     2 '            B 3', '            B 4', '            B 5',
     2 '            B 6', '            B 7', '            B 8',
     2 '            B 9', '            B10', '            B11',
     2 '            B12', '            B13', '            B14',
     2 '            B15', '            B16', '            B17',
     2 '            B18', '            B19'/

      data ( h_vib(3,lvl),lvl=1,38) /
     3 '       X3/2   0', '       X3/2   1', '       X3/2   2',
     3 '       X3/2   3', '       X3/2   4', '       X3/2   5',
     3 '       X3/2   6', '       X3/2   7', '       X3/2   8',
     3 '       X3/2   9', '       X1/2   0', '       X1/2   1',
     3 '       X1/2   2', '       X1/2   3', '       X1/2   4',
     3 '       X1/2   5', '       X1/2   6', '       X1/2   7',
     3 '       X1/2   8', '       X1/2   9', '       X1/2  10',
     3 '       X1/2  11', '       X1/2  12', '       X3/2  10',
     3 '       X3/2  11', '       X3/2  12', '       A1     0',
     3 '       A1     1', '       A1     2', '       A1     3',
     3 '       A2     0', '       A2     1', '       A2     2',
     3 '       A2     3', '       X3/2  13', '       X3/2  14',
     3 '       X1/2  13', '       X1/2  14'/

      data ( h_vib(4,lvl),lvl=1,91 ) /
     4 '        0 0 0 0', '        0 1 1 0', '        0 2 0 0',
     4 '        0 2 2 0', '        1 0 0 0', '        0 3 1 0',
     4 '        0 3 3 0', '        1 1 1 0', '        0 4 0 0',
     4 '        0 4 2 0', '        1 2 0 0', '        1 2 2 0',
     4 '        2 0 0 0', '        0 0 0 1', '        0 5 1 0',
     4 '        1 3 1 0', '        1 3 3 0', '        2 1 1 0',
     4 '        0 1 1 1', '        1 4 0 0', '        1 4 2 0',
     4 '        2 2 0 0', '        2 2 2 0', '        3 0 0 0',
     4 '        0 2 0 1', '        0 2 2 1', '        1 0 0 1',
     4 '        2 3 1 0', '        3 1 1 0', '        0 3 1 1',
     4 '        0 3 3 1', '        1 1 1 1', '        4 0 0 0',
     4 '        3 2 0 0', '        2 0 0 1', '        1 2 0 1',
     4 '        1 2 2 1', '        0 0 0 2', '        2 1 1 1',
     4 '        0 1 1 2', '               ', '        0 6 0 0',
     4 '        0 6 2 0', '        0 4 4 0', '        0 5 3 0',
     4 '        0 4 4 1', '        0 4 2 1', '        0 4 0 1',
     4 '        1 5 1 0', '        1 5 3 0', '        2 3 3 0',
     4 '        0 5 3 1', '        0 5 1 1', '        1 0 0 2',
     4 '        1 3 1 1', '        1 3 3 1', '        0 7 3 0',
     4 '        0 7 1 0', '        1 6 0 0', '        2 4 0 0',
     4 '        2 4 2 0', '        4 1 1 0', '        3 2 2 0',
     4 '        0 2 2 2', '        0 2 0 2', '        1 4 0 1',
     4 '        1 4 2 1', '        2 2 0 1', '        3 0 0 1',
     4 '        2 5 1 0', '        4 2 0 0', '        3 3 1 0',
     4 '        0 6 2 1', '        1 1 1 2', '        2 3 1 1',
     4 '        3 1 1 1', '        3 4 0 0', '        5 0 0 0',
     4 '        0 1 1 3', '        0 0 0 3', '        2 0 0 2',
     4 '        3 2 0 1', '        4 0 0 1', '        1 0 0 3',
     4 '        2 2 2 1', '        0 9 1 0', '        0 7 1 1',
     4 '        0 2 0 2', '        0 5 1 1', '        1 1 1 2',
     4 '        3 4 2 0'/

      data ( h_vib(5,lvl),lvl=1,132 ) /
     5 '       0 0 0 01', '       0 1 1 01', '       1 0 0 02',
     5 '       0 2 2 01', '       1 0 0 01', '       1 1 1 02',
     5 '       0 3 3 01', '       1 1 1 01', '       0 0 0 11',
     5 '       2 0 0 03', '       1 2 2 02', '       2 0 0 02',
     5 '       0 4 4 01', '       1 2 2 01', '       2 0 0 01',
     5 '       0 1 1 11', '       2 1 1 03', '       1 3 3 02',
     5 '       2 1 1 02', '       0 5 5 01', '       1 3 3 01',
     5 '       2 1 1 01', '       1 0 0 12', '       0 2 2 11',
     5 '       1 0 0 11', '       3 0 0 04', '       2 2 2 03',
     5 '       1 4 4 02', '       3 0 0 03', '       2 2 2 02',
     5 '       0 6 6 01', '       3 0 0 02', '       1 4 4 01',
     5 '       2 2 2 01', '       3 0 0 01', '       1 1 1 12',
     5 '       0 3 3 11', '       1 1 1 11', '       0 0 0 21',
     5 '       3 1 1 04', '       3 1 1 03', '       3 1 1 02',
     5 '       2 0 0 13', '       1 2 2 12', '       2 3 3 01',
     5 '       3 1 1 01', '       0 4 4 11', '       2 0 0 12',
     5 '       1 2 2 11', '       2 0 0 11', '       0 1 1 21',
     5 '       4 0 0 04', '       3 2 2 03', '       2 1 1 13',
     5 '       4 0 0 02', '       1 3 3 12', '       0 5 5 11',
     5 '       2 1 1 12', '       1 3 3 11', '       2 1 1 11',
     5 '       1 0 0 22', '       0 2 2 21', '       1 0 0 21',
     5 '       3 0 0 14', '       2 2 2 13', '       1 4 4 12',
     5 '       4 1 1 02', '       3 0 0 13', '       0 6 6 11',
     5 '       2 2 2 12', '       3 0 0 12', '       4 1 1 01',
     5 '       1 4 4 11', '       2 2 2 11', '       3 0 0 11',
     5 '       1 1 1 22', '       0 3 3 21', '       1 1 1 21',
     5 '       0 0 0 31', '       3 1 1 14', '       2 3 3 13',
     5 '       3 1 1 13', '       2 3 3 12', '       3 1 1 12',
     5 '       1 5 5 11', '       2 0 0 23', '       2 3 3 11',
     5 '       1 2 2 22', '       3 1 1 11', '       2 0 0 22',
     5 '       1 2 2 21', '       2 0 0 21', '       0 1 1 31',
     5 '       4 0 0 15', '       3 2 2 14', '       4 0 0 14',
     5 '       3 2 2 13', '       4 0 0 13', '       5 1 1 02',
     5 '       3 2 2 12', '       4 0 0 12', '       2 1 1 23',
     5 '       3 2 2 11', '       2 1 1 22', '       4 0 0 11',
     5 '       2 1 1 21', '       1 0 0 32', '       0 2 2 31',
     5 '       1 0 0 31', '       4 1 1 14', '       4 1 1 13',
     5 '       4 1 1 12', '       1 1 1 32', '       0 3 3 31',
     5 '       1 1 1 31', '       2 0 0 33', '       1 2 2 32',
     5 '       2 0 0 32', '       1 2 2 31', '       2 0 0 31',
     5 '       2 1 1 33', '       2 1 1 32', '       2 1 1 31',
     5 '       2 3 3 03', '       1 5 5 02', '       2 3 3 02',
     5 '       0 7 7 01', '               ', '       1 0 0 41',
     5 '       1 0 0 51', '       1 0 0 52', '       0 0 0 51'/

      data ( h_vib(6,lvl),lvl=1,116 ) /
c******
c**
c** There is a deuterated water vapor line in HITRAN 2004 at 12657.367357 cm-1
c** with upper state quantum number 2 3 0, a state not in the Vib.dat file.
c**
c** As a temporary measure to address this problem '2 3 0' has been put into
c** h_vib(6,116) and n_lnvl_v(6) has been increased to 116.  sac 08 Feb 2005
c**
c******
     6 '          0 0 0', '          0 1 0', '          0 2 0',
     6 '          1 0 0', '          0 0 1', '          0 3 0',
     6 '          1 1 0', '          0 1 1', '          0 4 0',
     6 '          1 2 0', '          0 2 1', '          2 0 0',
     6 '          1 0 1', '          0 0 2', '          1 3 0',
     6 '          0 3 1', '          2 1 0', '          1 1 1',
     6 '          0 1 2', '          0 4 1', '          2 2 0',
     6 '          1 2 1', '          0 2 2', '          3 0 0',
     6 '          2 0 1', '          1 0 2', '          0 0 3',
     6 '          1 3 1', '          3 1 0', '          2 1 1',
     6 '          1 1 2', '          0 1 3', '          1 4 1',
     6 '          0 4 2', '          3 2 0', '          2 2 1',
     6 '          3 0 1', '          2 0 2', '          1 2 2',
     6 '          0 2 3', '          4 0 0', '          1 0 3',
     6 '          0 0 4', '          1 5 1', '          3 3 0',
     6 '          2 3 1', '          2 1 2', '          3 1 1',
     6 '          4 1 0', '          1 1 3', '          3 2 1',
     6 '          2 2 2', '          3 0 2', '          4 0 1',
     6 '          4 2 0', '          1 2 3', '          5 0 0',
     6 '          2 0 3', '          1 0 4', '               ',
     6 '          3 3 1', '          2 1 3', '          3 1 2',
     6 '          4 1 1', '          3 0 3', '          4 0 2',
     6 '          4 0 3', '          4 2 1', '          5 0 1',
     6 '          3 1 3', '          4 1 2', '          2 3 2',
     6 '          0 5 0', '          0 6 0', '          0 7 0',
     6 '          0 3 2', '          0 5 1', '          0 6 1',
     6 '          0 8 0', '          1 4 0', '          1 5 0',
     6 '          0 3 3', '          0 3 4', '          0 4 3',
     6 '          0 5 3', '          0 6 1', '          0 6 3',
     6 '          0 7 1', '          1 1 5', '          1 3 2',
     6 '          1 3 3', '          1 4 2', '          1 6 0',
     6 '          1 7 0', '          2 2 3', '          2 4 0',
     6 '          2 4 1', '          3 2 2', '          3 4 0',
     6 '          3 4 1', '          4 3 0', '          4 3 1',
     6 '          5 1 0', '          5 1 1', '          5 2 0',
     6 '          6 0 0', '          6 0 1', '          6 1 0',
     6 '          6 1 1', '          6 2 0', '          7 0 0',
     6 '          7 0 1', '          8 0 0', '          0 5 0',
     6 '          0 6 0', '          2 3 0'/

      data ( h_vib(7,lvl),lvl=1,36 ) /
     7 ' 0 0 0 0 1 1   ', ' 0 0 0 0 0 0+  ', ' 0 0 1 0 0 0+  ',
     7 ' 1 0 1 0 0 0+  ', ' 0 0 0 0 1 1  u', ' 0 0 0 0 0 0+ g',
     7 ' 0 0 0 0 3 1  u', ' 0 0 0 1 1 0+ u', ' 0 0 0 2 1 1 1u',
     7 ' 0 0 0 2 1 1 2u', ' 0 0 1 0 0 0+ u', ' 0 1 0 1 1 0+ u',
     7 ' 1 0 1 0 0 0+ u', ' 1 1 0 1 1 0+ u', ' 0 0 0 0 2 0+ g',
     7 ' 0 0 0 0 2 2  g', ' 0 0 0 0 4 0+ g', ' 0 0 0 0 4 2  g',
     7 ' 0 0 0 2 2 0+2g', ' 0 0 0 2 2 0- g', ' 0 0 0 2 2 2 2g',
     7 ' 0 1 0 1 0 1  g', ' 1 0 1 0 1 1  g', ' 0 0 0 1 0 1  g',
     7 ' 0 0 0 1 1 0- u', ' 0 0 0 1 1 2  u', ' 0 0 0 1 3 0+ u',
     7 ' 0 0 0 1 3 0- u', ' 0 0 0 1 3 2 1u', ' 0 0 0 1 3 2 2u',
     7 ' 0 0 0 3 1 0+ u', ' 0 0 0 3 1 0- u', ' 0 0 0 3 1 2 1u',
     7 ' 0 0 0 3 1 2 2u', ' 0 1 0 0 1 1  u', ' 1 0 1 1 0 1  u'/

      data ( h_vib(8,lvl),lvl=1,47 ) /
     8 '      0 0 0 0  ', '      0 1 0 0  ', '      0 2 0 0  ',
     8 '      0 0 0 1  ', '      0 1 0 1  ', '      0 0 0 2  ',
     8 '      0 0 1 0  ', '      1 0 0 0  ', '      0 0 0 0 a',
     8 '      0 1 0 0 a', '      0 2 0 0 a', '      0 0 0 1 a',
     8 '      0 0 0 0 s', '      0 1 0 0 s', '      0 2 0 0 s',
     8 '      0 0 0 1 s', '               ', '      0 3 0 0 s',
     8 '      0 1 0 1 s', '      0 1 0 1 a', '      0 3 0 0 a',
     8 '      0 2 0 1 s', '      0 0 0 2As', '      0 0 0 2Aa',
     8 '      0 0 0 2Es', '      0 0 0 2Ea', '      1 0 0 0 s',
     8 '      1 0 0 0 a', '      0 0 1 0 s', '      0 0 1 0 a',
     8 '      0 4 0 0 s', '      0 2 0 1 a', '      0 3 0 1 s',
     8 '      0 4 0 0 a', '      0 1 0 2As', '      0 1 0 2Es',
     8 '      0 1 0 2Aa', '      0 1 0 2Ea', '      1 1 0 0 s',
     8 '      1 1 0 0 a', '      0 1 1 0 s', '      0 1 1 0 a',
     8 '      0 3 0 1 a', '      1 0 0 1 s', '      1 0 0 1 a',
     8 '      0 0 1 1 s', '      0 0 1 1 a'/

       data ( h_vib(9,lvl),lvl=1,39 ) /
     9 '    0 0 0 0 0 0', '    0 0 0 0 0 2', '    0 0 1 1 0 0',
     9 '    0 0 1 0 0 1', '    1 0 0 0 0 0', '    0 0 0 0 1 0',
     9 '    0 1 0 1 0 0', '    0 1 0 0 0 1', '    0 0 0 0 0 1',
     9 '    0 1 0 0 0 0', '    0 0 0 1 0 0', '    0 2 0 0 0 0',
     9 '               ', '    0 0 2 0 0 1', '    0 0 0 0 2 0',
     9 '    0 0 000 0 0', '    0 0 000 0 1', '    0 0 001 0 0',
     9 '    0 0 002 0 0', '    0 0 011 0 0', '    0 0 012 0 0',
     9 '    0 0 021 0 0', '    0 0 022 0 0', '    0 0 031 0 0',
     9 '    0 0 032 0 0', '    0 0 101 0 0', '    0 0 102 0 0',
     9 '    0 0 111 0 0', '    0 0 112 0 0', '    0 0 003 0 0',
     9 '    0 0 004 0 0', '    0 0 013 0 0', '    0 0 014 0 0',
     9 '    0 0 023 0 0', '    0 0 024 0 0', '    0 0 033 0 0',
     9 '    0 0 034 0 0', '    0 0 103 0 0', '    0 0 104 0 0'/

      data ( h_vib(10,lvl),lvl=1,91 ) /
     * '         GROUND', '             V1', '             V2',
     * '             V4', '             V5', '             V9',
     * '            2V5', '            2V9', '            3V6',
     * '            3V9', '          V5+V9', '               ',
     * '             V6', '             V3', '            2V6',
     * '             V7', '             V8', '          V8+V9',
     * '          V3+V6', '            2V3', '          V5+V6',
     * '          V3+V5', '          V4+V9', '            V10',
     * '            V11', '         V2+V12', '       2V10+V12',
     * '         V9+V10', '            V12', '           2V12',
     * '         V6+V12', '         V7+V12', '         V8+V12',
     * '        V8+2V12', '           3V12', '           4V12',
     * '    0 0 0 0    ', '    0 0 0 0 1A1', '    0 0 0 1 1F2',
     * '    0 0 0 2 1 E', '    0 0 0 2 1A1', '    0 0 0 2 1F2',
     * '    0 0 0 3 1A1', '    0 0 0 3 1F1', '    0 0 0 3 1F2',
     * '    0 0 0 3 2F2', '    0 0 0 4    ', '    0 0 1 0 1F2',
     * '    0 0 1 1 1 E', '    0 0 1 1 1A1', '    0 0 1 1 1F1',
     * '    0 0 1 1 1F2', '    0 0 1 2    ', '    0 0 1 2 1F2',
     * '    0 0 2 0 1F2', '    0 0 3 0    ', '    0 1 0 0 1 E',
     * '    0 1 0 1 1F1', '    0 1 0 1 1F2', '    0 1 0 2 1 E',
     * '    0 1 0 2 1A1', '    0 1 0 2 1A2', '    0 1 0 2 1F1',
     * '    0 1 0 2 1F2', '    0 1 0 2 2 E', '    0 1 1 0 1F1',
     * '    0 1 1 0 1F2', '    0 1 2 0    ', '    0 2 0 0 1 E',
     * '    0 2 0 0 1A1', '    0 2 0 1 1F1', '    0 2 0 1 1F2',
     * '    0 2 0 1 2F2', '    0 3 0 0    ', '    0 3 0 0 1 E',
     * '    0 3 0 0 1A1', '    0 3 0 0 1A2', '    1 0 0 0 1A1',
     * '    1 0 0 1 1F2', '    1 1 0 0 1 E', '    0 0 0 2 1 E',
     * '    0 0 0 2 1A1', '    0 0 0 2 1F2', '    0 0 0 3 1A1',
     * '    0 0 0 3 1F1', '    0 0 0 3 1F2', '    0 0 0 3 2F2',
     * '    0 0 1 1 1 E', '    0 0 1 1 1A1', '    0 0 1 1 1F1',
     * '    0 0 1 1 1F2'/


      do j_cl=1,ncl_v
         do lvl=1,n_lvl_v(j_cl)
c            write      (*,940) h_vib(j_cl,lvl)
 940        format(a15)
         end do
      end do

      return

      end
c__________________________________________________________________________

      Subroutine set_rot_map

      common /rot_map/ ncl_r,nclass_r(64),n_lvl_r(32)

c     molecules:       64
c     classes:         32
c     rot_ids/class:  256
      
      data ncl_r / 6/,  nmol /38/

      data (nclass_r(m),m=1,38)  /

c    *       1,       2,       3,       4,       5,       6,       7,
c    *     H2O,     CO2,      O3,     N2O,      CO,     CH4,      O2,
     *       1,       2,       1,       2,       2,       3,       5,
c
c    *       8,       9,      10,      11,      12,      13,      14,
c    *      NO,     SO2,     NO2,     NH3,    HNO3,      OH,      HF,
     *       6,       1,       1,       4,       1,       6,       2,
c
c    *      15,      16,      17,      18,      19,      20,      21,
c    *     HCl,     HBr,      HI,     ClO,     OCS,    H2CO,    HOCl,
     *       2,       2,       2,       6,       2,       1,       1,
c
c    *      22,      23,      24,      25,      26,      27,      28,
c    *      N2,     HCN,   CH3Cl,    H2O2,    C2H2,    C2H6,     PH3,
     *       2,       2,       4,       1,       2,       4,       4,
c
c    *      29,      30,      31,      32,      33,      34,      35,
c    *    COF2,     SF6,     H2S,   HCOOH,     HO2,       O,  ClONO2,
     *       1,       3,       1,       1,       1,       0,       1,
c
c    *      36,      37,      38,
c    *     NO+,    HOBr,    C2H4,
     *       2,       1,       1/
c
      do mol=1,nmol
c        write (*,925) mol,h_mol_nam(mol), nclass_r(mol)
 925     format(i5,a8,i5)

      end do
  
      return

      end
c-----------------------------------------------------------------------
      SUBROUTINE VIBQ1 (MOL,IVUP,IVLO)                                   LN13160
C                                                                        LN13170
      IMPLICIT REAL*8           (V)                                     !LN13180
C                                                                        LN13190
C                                                                        LN13200
C     THIS SUBROUTINE MAPS THE TWO VIBRATIONAL LEVEL                     LN13210
C     ID'S OF SELECTED MOLECULES AGAINST STORED VIB STATE                LN13220
C     ID'S.  MOL IS ADJUSTED TO INCLUDE ADDITIONAL IDENTIFIERS.          LN13230
C          IJ ----- UPPER STATE ID                                       LN13240
C          KL ----- LOWER STATE ID                                       LN13250
C          MN ----- ORDINARY MOLECULAR ID                                LN13260
C                                                                        LN13270
      DIMENSION MH2O(8),MCO2(26),MO3(18),MCO(3),MNO(3),MNOA(3)           LN13280
C                                                                        LN13290
      DATA MH2O / 1,2,3,4,5,6,7,8 /                                      LN13300
      DATA MCO2 / 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,23,24,25,36,    LN13310
     *            37,38,43,47,48,50 /                                    LN13315
      DATA MO3 / 1,2,5,4,3,8,7,14,13,12,18,27,43,100,101,102,103,104 /   LN13320
      DATA MCO / 1,2,3 /                                                 LN13330
      DATA MNO / 1,2,3 /,MNOA / 11,12,13 /                               LN13340
                                                                         LN13350
C                                                                        LN13360
      DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO / 8,26,18,3,3 /               LN13370
      MOL2 = MOL                                                         LN13380
      MOL1 = MOD(MOL2,100)                                               LN13400
      IF (MOL1.GT.8) GO TO 210                                           LN13390
C                                                                        LN13420
C     MOL1 IS THE ORIGINAL MOLECULAR ID                                  LN13430
C     MOL2 IS THE NEW TAGGED MOL ID                                      LN13440
C                                                                        LN13460
      IF (MOL1.NE.1) GO TO 10                                            LN13470
C                                                                        LN13480
C     WATER VAPOR LINE                                                   LN13490
C                                                                        LN13500
C     DATA MH2O / 1,2,3,4,5,6,7,8/                                       LN13510
C                                                                        LN13520
      NUPP = IVUP                                                        LN13530
      NLOW = IVLO                                                        LN13540
      IF (NLOW.GT.NUMH2O) RETURN                                         LN13550
      IF (NUPP.GT.NUMH2O) RETURN                                         LN13560
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN13570
C                                                                        LN13580
      RETURN                                                             LN13590
C                                                                        LN13600
C     CO2 LINE                                                           LN13610
C                                                                        LN13620
   10 IF (MOL1.NE.2) GO TO 60                                            LN13630
C                                                                        LN13640
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN13650
C                                                                        LN13660
      DO 20 I = 1, NUMCO2                                                LN13670
         IMAP = I                                                        LN13680
         IF (IVUP.EQ.MCO2(I)) GO TO 30                                   LN13690
   20 CONTINUE                                                           LN13700
C                                                                        LN13710
      RETURN                                                             LN13720
C                                                                        LN13730
   30 NUPP = IMAP                                                        LN13740
      DO 40 I = 1, NUMCO2                                                LN13750
         IMAP = I                                                        LN13760
         IF (IVLO.EQ.MCO2(I)) GO TO 50                                   LN13770
   40 CONTINUE                                                           LN13780
C                                                                        LN13790
      RETURN                                                             LN13800
C                                                                        LN13810
   50 NLOW = IMAP                                                        LN13820
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN13830
C                                                                        LN13840
      RETURN                                                             LN13850
C                                                                        LN13860
   60 CONTINUE                                                           LN13870
C                                                                        LN13880
      IF (MOL1.NE.3) GO TO 110                                           LN13890
C                                                                        LN13900
C     OZONE LINE                                                         LN13910
C                                                                        LN13920
C     DATA MO3 / 1,2,5,4,3,8,7,14,13,12,18,27,43,100,101,102,103,104 /   LN13930
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN13940
C                                                                        LN13950
      DO 70 I = 1, NUMO3                                                 LN13960
         IMAP = I                                                        LN13970
         IF (IVUP.EQ.MO3(I)) GO TO 80                                    LN13980
   70 CONTINUE                                                           LN13990
C                                                                        LN14000
      RETURN                                                             LN14010
C                                                                        LN14020
   80 NUPP = IMAP                                                        LN14030
      DO 90 I = 1, NUMO3                                                 LN14040
         IMAP = I                                                        LN14050
         IF (IVLO.EQ.MO3(I)) GO TO 100                                   LN14060
   90 CONTINUE                                                           LN14070
C                                                                        LN14080
      RETURN                                                             LN14090
C                                                                        LN14100
  100 NLOW = IMAP                                                        LN14110
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14120
C                                                                        LN14130
      RETURN                                                             LN14140
C                                                                        LN14150
  110 CONTINUE                                                           LN14160
      IF (MOL1.NE.5) GO TO 160                                           LN14170
C                                                                        LN14180
C     CARBON MONOXIDE LINE                                               LN14190
C                                                                        LN14200
C     DATA MCO / 1,2,3 /                                                 LN14210
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN14220
C                                                                        LN14230
      DO 120 I = 1, NUMCO                                                LN14240
         IMAP = I                                                        LN14250
         IF (IVUP.EQ.MCO(I)) GO TO 130                                   LN14260
  120 CONTINUE                                                           LN14270
C                                                                        LN14280
      RETURN                                                             LN14290
C                                                                        LN14300
  130 NUPP = IMAP                                                        LN14310
      DO 140 I = 1, NUMCO                                                LN14320
         IMAP = I                                                        LN14330
         IF (IVLO.EQ.MCO(I)) GO TO 150                                   LN14340
  140 CONTINUE                                                           LN14350
C                                                                        LN14360
      RETURN                                                             LN14370
C                                                                        LN14380
  150 NLOW = IMAP                                                        LN14390
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14400
C                                                                        LN14410
      RETURN                                                             LN14420
C                                                                        LN14430
  160 CONTINUE                                                           LN14440
      IF (MOL1.NE.8) GO TO 210                                           LN14450
C                                                                        LN14460
C     NITRIC OXIDE LINE                                                  LN14470
C                                                                        LN14480
C     DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN14490
C     DATA MNO / 1,2,3 /,MNOA /11,12,13/                                 LN14500
C                                                                        LN14510
      DO 170 I = 1, NUMNO                                                LN14520
         IMAP = I                                                        LN14530
         IF (IVUP.EQ.MNO(I)) GO TO 180                                   LN14540
         IF (IVUP.EQ.MNOA(I)) GO TO 180                                  LN14550
  170 CONTINUE                                                           LN14560
C                                                                        LN14570
      RETURN                                                             LN14580
C                                                                        LN14590
  180 NUPP = IMAP                                                        LN14600
      DO 190 I = 1, NUMNO                                                LN14610
         IMAP = I                                                        LN14620
         IF (IVLO.EQ.MNO(I)) GO TO 200                                   LN14630
         IF (IVLO.EQ.MNOA(I)) GO TO 200                                  LN14640
  190 CONTINUE                                                           LN14650
C                                                                        LN14660
      RETURN                                                             LN14670
C                                                                        LN14680
  200 NLOW = IMAP                                                        LN14690
      MOL = MOL2+1000*NLOW+100000*NUPP                                   LN14700
C                                                                        LN14710
  210 RETURN                                                             LN14720
C                                                                        LN14730
      END                                                                LN14740
      SUBROUTINE RDFL82 (LINBCD,ILO,NMAX,IEOF,VLST)                      LN14750
C                                                                        LN14760
      IMPLICIT REAL*8           (V)                                     !LN14770
C                                                                        LN14780
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN14790
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN14800
     *               IREC,IRECTL,HID1(2),LSTWD                           LN14810


      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2

C                                                                        LN14820
      CHARACTER*8      HID,HID1,HMOL                                    &LN14830
C                                                                        LN14840
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN14850
      COMMON /HOL/ HOL82(40)                                             LN14860
      CHARACTER HOL82*35                                                 LN14870
      COMMON /TRAC/ VNU(40),STR(40),ALF(40),EPP(40),MOL(40),HWHMF(40),   LN14880
     *              TMPALF(40),PSHIFT(40),IFLG(40),MIND2(64)             LN14890
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN14900
      COMMON /HBLOCK/ INBLK1,INBLK2,IO2BND,I86T1,I86T2                   LN14910
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN14920
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN14930
      COMMON /LCHAR/ ALIN1(51),ALIN(40),ALINC(51),ALINE(250)             LN14940
      DIMENSION IDATE(40),ISO(40)                                        LN14950
C                                                                        LN14960
      CHARACTER*100 ALIN1,ALIN,ALINC,ALINE                               LN14970
      CHARACTER*50 ZEROFL                                                LN14980
C                                                                        LN14990
      DATA ZEROFL /                                                      LN15000
     *           '00000000000000000000000000000000000000000000000000'/   LN15010
      DATA TEMP0 / 296. /                                                LN15020
C                                                                        LN15030
C     HALF WIDTH TEMPERATURE DEPENDANCE (TALF(M))                        LN15040
C     STORED IN DATA STATEMENTS FROM MOLEC FOR DENSITY FORMALISM         LN15050
C                                                                        LN15060
      BETA0 = RADCN2/TEMP0                                               LN15070
      ILO = 0                                                            LN15080
      NMAX = 0                                                           LN15090
      IER = 0                                                            LN15100
      IEOF = 0                                                           LN15110
C                                                                        LN15120
      NLIN = 1                                                           LN15130
      KEOF = 0                                                           LN15140
C                                                                        LN15150
   10 CONTINUE                                                           LN15160
      IF (KEOF.EQ.1) GO TO 80                                            LN15170
      IF (INBLK2.EQ.0) THEN                                              LN15180
         READ (LINBCD,900,END=80) NLIN,                                  LN15190
     *                           (VNU(K),STR(K),ALF(K),EPP(K),HOL82(K),  LN15200
     *                            IDATE(K),ISO(K),MOL(K),K=1,NLIN)       LN15210
      ELSE                                                               LN15220
         NLIN = 40                                                       LN15230
         KEOF = 1                                                        LN15240
         DO 20 I = 1, NLIN                                               LN15250
            READ (LINBCD,905,END=30) VNU(I),STR(I),ALF(I),EPP(I),        LN15260
     *                               HOL82(I),IDATE(I),ISO(I),MOL(I)     LN15270
   20    CONTINUE                                                        LN15280
         KEOF = 0                                                        LN15290
      ENDIF                                                              LN15300
      IEOF = 0                                                           LN15310
      GO TO 40                                                           LN15320
   30 NLIN = I-1                                                         LN15330
      IF (NLIN.LE.0) GO TO 80                                            LN15340
C                                                                        LN15350
   40 INUM = 0                                                           LN15360
      IF (VNU(NLIN).LT.VMIN) GO TO 10                                    LN15370
      DO 50 I = 1, NLIN                                                  LN15380
         WRITE (ALIN(I),910) VNU(I),STR(I),ALF(I),EPP(I),HOL82(I),       LN15390
     *                       IDATE(I),ISO(I),MOL(I)                      LN15400
         IF (VNU(I).LT.VLST) THEN                                        LN15410
            IF (LINBCD.EQ.1) IER = 1                                     LN15420
            IF (LINBCD.EQ.2) IER = 2                                     LN15430
            NLIN = I-1                                                   LN15440
            WRITE (IPR,915) VNU(I),VLST,IER                              LN15450
            IEOF = 999                                                   LN15460
C                                                                        LN15470
            RETURN                                                       LN15480
C                                                                        LN15490
         ENDIF                                                           LN15500
C                                                                        LN15510
         VLST = VNU(I)                                                   LN15520
         IF (VNU(I).GT.VMAX) GO TO 70                                    LN15530
         IF (VNU(I).LT.VMIN) GO TO 50                                    LN15540
         M = MOL(I)                                                      LN15550
         IF (MIND2(M).NE.1) MOL(I) = 0                                   LN15560
         IF (MOL(I).EQ.0) GO TO 50                                       LN15570
         STR(I) = STR(I)/(VNU(I)*(1.0-EXP(-BETA0*VNU(I))))               LN15580
         HWHMF(I) = ALF(I)                                               LN15590
         IF (M.EQ.1) HWHMF(I) = ALF(I)*5.                                LN15600
         TMPALF(I) = TALF(M)                                             LN15610
         PSHIFT(I) = 0.                                                  LN15620
         IFLG(I) = 0                                                     LN15630
         IF (STR(I).LT.SR(M)) MOL(I) = 0                                 LN15640
         IF (MOL(I).EQ.0) GO TO 50                                       LN15650
         NMAX = I                                                        LN15660
         INUM = INUM+1                                                   LN15670
         IF (INUM.EQ.1) ILO = I                                          LN15680
   50 CONTINUE                                                           LN15690
C                                                                        LN15700
      IF (NLIN.LT.40) THEN                                               LN15710
         DO 60 I = NLIN+1, 40                                            LN15720
            WRITE (ALIN(I),920) ZEROFL,ZEROFL                            LN15730
   60    CONTINUE                                                        LN15740
      ENDIF                                                              LN15750
C                                                                        LN15760
      IF (INUM.LE.0) GO TO 10                                            LN15770
      IF (INLTE.EQ.1)                                                    LN15780
     *     CALL VIBQU (ILO,NMAX,NLIN,VNU,STR,ALF,EPP,MOL,ISO)            LN15790
      IF (KEOF.EQ.1) IEOF = 1                                            LN15800
C                                                                        LN15810
      RETURN                                                             LN15820
C                                                                        LN15830
   70 IF (NMAX.LT.1) IEOF = 99                                           LN15840
C                                                                        LN15850
      RETURN                                                             LN15860
C                                                                        LN15870
   80 CONTINUE                                                           LN15880
      CLOSE (LINBCD)                                                     LN15890
      IF (LINBCD.EQ.1)                                                   LN15900
     *     OPEN (1,FILE='TAPE1',STATUS='OLD',FORM='FORMATTED')           LN15910
c
c          ----------------------------------------------------------------
c          the following may be required by the Absoft f90 compiler:
c
c%%     *     OPEN (1,FILE='TAPE1',STATUS='OLD',action='read',
c%%     *                                              FORM='FORMATTED')       LN02900
c          ----------------------------------------------------------------
      IF (LINBCD.EQ.2)                                                   LN15920
     *     OPEN (2,FILE='TAPE2',STATUS='OLD',FORM='FORMATTED')           LN15930
c
c          ----------------------------------------------------------------
c          the following may be required by the Absoft f90 compiler:
c
c%%     *     OPEN (2,FILE='TAPE2',STATUS='OLD',action='read',
c%%     *                                              FORM='FORMATTED')       LN02900
c          ----------------------------------------------------------------
      IEOF = IEOF+1                                                      LN15940
C                                                                        LN15950
      LSTFIL = VMAX/100.+1.                                              LN15960
C                                                                        LN15970
      IF (IEOF.GE.LSTFIL) RETURN                                         LN15980
C                                                                        LN15990
      GO TO 10                                                           LN16000
C                                                                        LN16010
  900 FORMAT (I10,40(F10.3,E10.3,F5.3,F10.3,A35,I3,I4,I3))               LN16020
  905 FORMAT (F10.3,E10.3,F5.3,F10.3,A35,I3,I4,I3)                       LN16030
  910 FORMAT (F10.3,1P,E10.3,0P,F5.3,F10.3,A35,I3,I4,I3)                 LN16040
  915 FORMAT ('  LINE OUT OF ORDER  BAD LINE =',F10.3,/,                 LN16050
     *        ' LAST GOOD LINE =',F10.3,' ON UNIT',I3,///)               LN16060
  920 FORMAT (2A50)                                                      LN16070
C                                                                        LN16080
      END                                                                LN16090
      SUBROUTINE VIBQU (ILO,NMAX,NLIN,VNU,STR,ALF,EPP,MOL,NSO82)         LN16100
C                                                                        LN16110
      IMPLICIT REAL*8           (V)                                     !LN16120
C                                                                        LN16130
C                                                                        LN16140
C    THIS SUBROUTINE COMPARES THE TWO VIBRATIONAL LEVEL                  LN16150
C    ID'S OF SELECTED MOLECULES AGAINST STORED VIB STATE                 LN16160
C    ID'S.  MOL IS ADJUSTED TO INCLUDE ADDITIONAL IDENTIFIERS.           LN16170
C          IJ ----- UPPER STATE ID                                       LN16180
C          KL ----- LOWER STATE ID                                       LN16190
C          MN ----- ORDINARY MOLECULAR ID                                LN16200
C                                                                        LN16210
C                                                                        LN16220
      DIMENSION VNU(*),STR(*),ALF(*),EPP(*),MOL(*),NSO82(*)              LN16230
      COMMON /HOL/ HOL82(40)                                             LN16240
      CHARACTER*35 HOL82,TI                                              LN16250
      CHARACTER*10 AQH2O(8),AQCO2(26),AQO3(18),AQCO(3),AQNO(3),          LN16260
     *             BLNK,HTEST,QIB1,QIB2                                  LN16270
C                                                                        LN16280
      DATA NUMH2O,NUMCO2,NUMO3,NUMCO,NUMNO/ 8,26,18,3,3 /                LN16290
C                                                                        LN16300
      DATA BLNK /'          '/                                           LN16310
C                                                                        LN16320
      DATA AQH2O/                                                        LN16330
     *       '000' ,                                                     LN16340
     *       '010' ,                                                     LN16350
     *       '020' ,                                                     LN16360
     *       '100' ,                                                     LN16370
     *       '001' ,                                                     LN16380
     *       '030' ,                                                     LN16390
     *       '110' ,                                                     LN16400
     *       '011' /                                                     LN16410
C                                                                        LN16420
      DATA AQCO2/                                                        LN16430
     *      '00001' , '01101' , '10002' , '02201' , '10001' , '11102' ,  LN16440
     *      '03301' , '11101' , '00011' , '20003' , '12202' , '20002' ,  LN16450
     *      '04401' , '12201' , '20001' , '01111' , '10012' , '02211' ,  LN16460
     *      '10011' , '11112' , '03311' , '11111' , '20013' , '04411' ,  LN16470
     *      '20012' , '20011' /                                          LN16490
C                                                                        LN16600
      DATA AQO3/                                                         LN16610
     *       '000' ,                                                     LN16620
     *       '010' ,                                                     LN16630
     *       '001' ,                                                     LN16640
     *       '100' ,                                                     LN16650
     *       '020' ,                                                     LN16660
     *       '011' ,                                                     LN16670
     *       '110' ,                                                     LN16680
     *       '002' ,                                                     LN16690
     *       '101' ,                                                     LN16700
     *       '200' ,                                                     LN16710
     *       '111' ,                                                     LN16720
     *       '003' ,                                                     LN16730
     *       '004' ,                                                     LN16740
     *       '005' ,                                                     LN16750
     *       '006' ,                                                     LN16760
     *       '007' ,                                                     LN16770
     *       '008' ,                                                     LN16780
     *       '009' /                                                     LN16790
C                                                                        LN16800
      DATA AQCO/                                                         LN16810
     *       '0' ,                                                       LN16820
     *       '1' ,                                                       LN16830
     *       '2' /                                                       LN16840
C                                                                        LN16850
      DATA AQNO/                                                         LN16860
     *       '0' ,                                                       LN16870
     *       '1' ,                                                       LN16880
     *       '2' /                                                       LN16890
C                                                                        LN16900
      IF (ILO.LT.1) RETURN                                               LN16910
C                                                                        LN16920
      DO 260 I = ILO, NMAX                                               LN16930
         mol1 = mol(i)/10
         iso  = mol(i)-mol1*10
         MOL2 = mol(i)+100*ISO
         IF (MOL(I).EQ.0) GO TO 260                                      LN16970
         QIB1 = BLNK                                                     LN16980
         QIB2 = BLNK                                                     LN16990
         TI = HOL82(I)                                                   LN17000
c
c     for the first 8 molecules, attach vibrational information
c
         IF (MOL1.GT.8) GO TO 250                                        LN17020
C                                                                        LN17030
C     MOL1 IS THE ORIGINAL MOLECULAR ID                                  LN17040
C     MOL2 IS THE NEW TAGGED MOL ID                                      LN17050
C                                                                        LN17060
         IF (MOL1.NE.1) GO TO 50                                         LN17070
C                                                                        LN17080
C     WATER VAPOR LINE                                                   LN17090
C                                                                        LN17100
         QIB1(1:1) = TI(23:23)                                           LN17110
         QIB1(2:2) = TI(25:25)                                           LN17120
         QIB1(3:3) = TI(27:27)                                           LN17130
         QIB2(1:1) = TI(30:30)                                           LN17140
         QIB2(2:2) = TI(32:32)                                           LN17150
         QIB2(3:3) = TI(34:34)                                           LN17160
         NUPP = 0                                                        LN17170
         DO 10 NAQ = 1, NUMH2O                                           LN17180
            HTEST = AQH2O(NAQ)                                           LN17190
            IF (QIB1.NE.HTEST) GO TO 10                                  LN17200
            NUPP = NAQ                                                   LN17210
            GO TO 20                                                     LN17220
   10    CONTINUE                                                        LN17230
         GO TO 250                                                       LN17240
   20    NLOW = 0                                                        LN17250
         DO 30 NAQ = 1, NUMH2O                                           LN17260
            HTEST = AQH2O(NAQ)                                           LN17270
            IF (QIB2.NE.HTEST) GO TO 30                                  LN17280
            NLOW = NAQ                                                   LN17290
            GO TO 40                                                     LN17300
   30    CONTINUE                                                        LN17310
         GO TO 250                                                       LN17320
   40    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN17330
C                                                                        LN17340
CPRT  PRINT 900,MOL2,TI                                                  LN17350
C                                                                        LN17360
         GO TO 250                                                       LN17370
   50    CONTINUE                                                        LN17380
         IF (MOL1.NE.2) GO TO 100                                        LN17390
C                                                                        LN17400
C     CO2 LINE                                                           LN17410
C                                                                        LN17420
         QIB1(1:1) = TI(4:4)                                             LN17430
         QIB1(2:2) = TI(6:6)                                             LN17440
         QIB1(3:3) = TI(8:8)                                             LN17450
         QIB1(4:4) = TI(10:10)                                           LN17460
         QIB1(5:5) = TI(12:12)                                           LN17470
         QIB2(1:1) = TI(19:19)                                           LN17480
         QIB2(2:2) = TI(21:21)                                           LN17490
         QIB2(3:3) = TI(23:23)                                           LN17500
         QIB2(4:4) = TI(25:25)                                           LN17510
         QIB2(5:5) = TI(27:27)                                           LN17520
         NUPP = 0                                                        LN17530
         DO 60 NAQ = 1, NUMCO2                                           LN17540
            HTEST = AQCO2(NAQ)                                           LN17550
            IF (QIB1.NE.HTEST) GO TO 60                                  LN17560
            NUPP = NAQ                                                   LN17570
            GO TO 70                                                     LN17580
   60    CONTINUE                                                        LN17590
         GO TO 250                                                       LN17600
   70    NLOW = 0                                                        LN17610
         DO 80 NAQ = 1, NUMCO2                                           LN17620
            HTEST = AQCO2(NAQ)                                           LN17630
            IF (QIB2.NE.HTEST) GO TO 80                                  LN17640
            NLOW = NAQ                                                   LN17650
            GO TO 90                                                     LN17660
   80    CONTINUE                                                        LN17670
         GO TO 250                                                       LN17680
   90    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN17690
C                                                                        LN17700
CPRT  PRINT 900,MOL2,TI                                                  LN17710
C                                                                        LN17720
         GO TO 250                                                       LN17730
  100    CONTINUE                                                        LN17740
C                                                                        LN17750
         IF (MOL1.NE.3) GO TO 150                                        LN17760
C                                                                        LN17770
C     OZONE LINE                                                         LN17780
C                                                                        LN17790
         QIB1(1:3) = TI( 1: 6)                                           LN17800
         QIB2(1:3) = TI(14:16)                                           LN17830
         NUPP = 0                                                        LN17860
         DO 110 NAQ = 1, NUMO3                                           LN17870
            HTEST = AQO3(NAQ)                                            LN17880
c$$         IF (QIB1.NE.HTEST) GO TO 110                                 LN17890
            IF (QIB1.eq.HTEST) then
               NUPP = NAQ             
               GO TO 120                 
            endif
  110    CONTINUE                                                        LN17920
         GO TO 250                                                       LN17930
  120    NLOW = 0                                                        LN17940
         DO 130 NAQ = 1, NUMO3                                           LN17950
            HTEST = AQO3(NAQ)                                            LN17960
            IF (QIB2.NE.HTEST) GO TO 130                                 LN17970
            NLOW = NAQ                                                   LN17980
            GO TO 140                                                    LN17990
  130    CONTINUE                                                        LN18000
         GO TO 250                                                       LN18010
  140    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18020
         GO TO 250                                                       LN18030
C                                                                        LN18040
  150    CONTINUE                                                        LN18050
         IF (MOL1.NE.5) GO TO 200                                        LN18060
C                                                                        LN18070
C     CARBON MONOXIDE LINE                                               LN18080
C                                                                        LN18090
         QIB1 = TI(8:8)                                                  LN18100
         QIB2 = TI(16:16)                                                LN18110
         NUPP = 0                                                        LN18120
         DO 160 NAQ = 1, NUMCO                                           LN18130
            HTEST = AQCO(NAQ)                                            LN18140
            IF (QIB1.NE.HTEST) GO TO 160                                 LN18150
            NUPP = NAQ                                                   LN18160
            GO TO 170                                                    LN18170
  160    CONTINUE                                                        LN18180
         GO TO 250                                                       LN18190
  170    NLOW = 0                                                        LN18200
         DO 180 NAQ = 1, NUMCO                                           LN18210
            HTEST = AQCO(NAQ)                                            LN18220
            IF (QIB2.NE.HTEST) GO TO 180                                 LN18230
            NLOW = NAQ                                                   LN18240
            GO TO 190                                                    LN18250
  180    CONTINUE                                                        LN18260
         GO TO 250                                                       LN18270
  190    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18280
C                                                                        LN18290
CPRT  PRINT 900,MOL2,TI                                                  LN18300
C                                                                        LN18310
         GO TO 250                                                       LN18320
C                                                                        LN18330
  200    CONTINUE                                                        LN18340
         IF (MOL1.NE.8) GO TO 250                                        LN18350
C                                                                        LN18360
C     NITRIC OXIDE LINE                                                  LN18370
C                                                                        LN18380
         QIB1 = TI(10:10)                                                LN18390
         QIB2 = TI(20:20)                                                LN18400
         NUPP = 0                                                        LN18410
         DO 210 NAQ = 1, NUMNO                                           LN18420
            HTEST = AQNO(NAQ)                                            LN18430
            IF (QIB1.NE.HTEST) GO TO 210                                 LN18440
            NUPP = NAQ                                                   LN18450
            GO TO 220                                                    LN18460
  210    CONTINUE                                                        LN18470
         GO TO 250                                                       LN18480
  220    NLOW = 0                                                        LN18490
         DO 230 NAQ = 1, NUMNO                                           LN18500
            HTEST = AQNO(NAQ)                                            LN18510
            IF (QIB2.NE.HTEST) GO TO 230                                 LN18520
            NLOW = NAQ                                                   LN18530
            GO TO 240                                                    LN18540
  230    CONTINUE                                                        LN18550
         GO TO 250                                                       LN18560
  240    MOL2 = MOL2+1000*NLOW+100000*NUPP                               LN18570
  250    CONTINUE                                                        LN18580
C                                                                        LN18590
         MOL(I) = MOL2                                                   LN18600
  260    continue
C                                                                        LN18610
      RETURN                                                             LN18620
C                                                                        LN18630
C  900 FORMAT (' MOL2 = ',I10,' TI = ',A35)                              LN18640
C                                                                        LN18650
      END                                                                LN18660
      SUBROUTINE CPSTOR_100 (molcpl,ncpl)
C                                                                        LN18680
C     THIS SUBROUTINE WRITES OUT THE LINES AND THEIR COUPLING            LN18690
C     COEFFICIENTS TO TAPE2 FOR USE BY LNFL                              LN18700
C                                                                        LN18710
      COMMON /CLINES_100/ CPLINS(886)                                        LN18720
      CHARACTER CPLINS*100, HQ*7                                         LN18730
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN18740
      COMMON /CPLMOL_100/ MOLCPL_100(38),NCPL_100 

      dimension molcpl(38)

      CHARACTER CUP*9, CLO*9
C                                                                        LN18760
c      set up molcpl and ncpl based on particular line coupling file used
c
      ncpl =ncpl_100

      do m=1,38
         molcpl(m) = molcpl_100(m)
      enddo

C                                                                        LN18760
c______
c
c     -zero frequency oxygen line is not modified  based on
c       Cimini et al 2003 (kcp and sac)

      i=1

         READ (CPLINS(I),920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
         WRITE (2,920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
c 
         READ (CPLINS(I+1),925) ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG

         WRITE (2,925)          ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C
c     modify the line coupling for the oxygen lines
C
      DO 10 I = 3,82,2                                                   LN18770
c
         READ (CPLINS(I),920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
         WRITE (2,920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
C 
         READ (CPLINS(I+1),925) ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C**********************
c
C     These coefficients have been updated to provide consistency with
C     HITRAN96 oxygen line parameters (June 1999).
c
c     -modify the oxygen line coupling coefficients based on the data of 
c      Cimini et al. 2003 (kcp and sac)
c
         data y_fac/0.87/, g_fac/0./
c
         y1 = y_fac*y1
         y2 = y_fac*y2
         y3 = y_fac*y3
         y4 = y_fac*y4
c
         g1 = g_fac*g1
         g2 = g_fac*g2
         g3 = g_fac*g3
         g4 = g_fac*g4
c
         WRITE (2,925)          ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C
 10   CONTINUE                                                           LN18790
C                                                                        LN18760
C   REMEMBER TO CHECK AND FIX SHIFT = 0. PROBLEM
C                                                                        LN11550
      DO 40 I = 83,NCPL                                                  LN18770
         WRITE (2,900) CPLINS(I)                                         LN18780
   40 CONTINUE                                                           LN18790
C                                                                        LN18800
      REWIND 2                                                           LN18810
C                                                                        LN18820
      RETURN                                                             LN18830
C                                                                        LN18840
  900 FORMAT (A100)                                                      LN18850
  920 FORMAT (I3,F12.6,1P,2E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,A7,I2) LN11470
  925 FORMAT (I2,1P,4(E13.6,E11.4),0P,I2)                                LN11480
C                                                                        LN18860
      END                                                                LN18870
      BLOCK DATA CPLINS_100                                                  LN19940
C                                                                        LN19950
C**********************************************************************  LN19960
C                                                                        LN19970
C     THIS BLOCK DATA CONTAINS TRANSITION DATA INCLUDING LINE COUPLING   LN19980
C     PARAMETERS FOR REPLACEMENT OF SELECTED TRANSITIONS ON THE HITRAN   LN19990
C     DATABASE AFFECTED BY LINE COUPLING.                                LN20000
C                                                                        LN20010
C     THE FORMULATION IS DESCRIBED IN HOKE ET AL, 1988: PROC. OF THE     LN20020
C     INTERNATIONAL RADIATION  SYMPOSIUM, J. LENOBLE AND J.F. GELEYN,    LN20030
C     ED., A. DEEPAK PUB., 368-371.                                      LN20040
C                                                                        LN20050
C     THE LINE COUPLING DATA GENERALLY FOLLOWS THE DEVELOPMENT OF SMITH, LN20060
C            S' = S * ( 1. + G * (P/P0)**2 )           P0=1013 MB        LN20070
C            S''= S * (      Y * (P/P0)  )                               LN20080
C     VALUES FOR Y AND G ARE PROVIDED AT FOUR TEMPERATURES:              LN20090
C     200 K, 250 K, 296 K, AND 340 K                                     LN20100
C                                                                        LN20110
C**********************************************************************  LN20120
C                                                                        LN20130
C                    OXYGEN                                              LN20140
C                                                                        LN20150
C     A TRANSITION AT 0.000010 CM-1 HAS BEEN PROVIDED IN ORDER TO        LN20160
C     TREAT THE 'ZERO FREQUENCY' (NON-RESONANT) O2 BAND AS A SINGLE      LN20170
C     LINE WITH A PRESSURE DEPENDENT REFERENCE HALFWIDTH, REPLACING      LN20180
C     THE 'ZERO FREQUENCY' TRANSITIONS ON THE HITRAN DATA BASE FOR       LN20190
C     THE MAIN ISOTOPE.                                                  LN20200
C                                                                        LN20210
C     THE COUPLING COEFFICIENTS FOR THE 2 CM-1 (60 GHZ) FOR THE MAIN     LN20220
C     ISOTOPE ARE INCLUDED. THESE HAVE BEEN CALCULATED BY CLOUGH AND     LN20230
C     HOKE.                                                              LN20240
C                                                                        LN20250
C            THE O2 LINE COUPLING COEFFICIENTS WERE PROVIDED BY          LN20260
C            CLOUGH (1987). Y'S AND G'S WERE CALCULATED FOR OXYGEN       LN20270
C            USING A RELAXATION MATRIX WHICH WAS FIT TO SMITH'S          LN20280
C            HALFWIDTHS.                                                 LN20290
C                                                                        LN20300
C              NT        TEMP         X       A1            A2           LN20310
C                                                                        LN20320
C               1     200.0000     .7500  7.328597E-03  7.303265E-01     LN20330
C               2     250.0000     .7500  5.467282E-03  7.535262E-01     LN20340
C               3     296.0000     .7500  4.371108E-03  7.712720E-01     LN20350
C               4     340.0000     .7500  3.632896E-03  7.855163E-01     LN20360
C                                                                        LN20370
C         ALPHA(T) = ALPHA(TO)*(TO/T)**X                                 LN20380
C                                                                        LN20390
C         W(J,K) = A1*SQRT(RHO(J)/RHO(K))*EXP(-A2*BETA*ABS(E(J)-E(K)))   LN20400
C
C    These coefficients have been updated to provide consistency with
C    HITRAN96 oxygen line parameters (June 1999).
C                                                                        LN20410
C**********************************************************************  LN20600
C                                                                        LN20610
C                   CARBON DIOXIDE                                       LN20620
C                                                                        LN20630
C     LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES AT        LN20640
C     618 CM-1, 667 CM-1, 720 CM-1, 721 CM-1, 742 CM-1 AND 792 CM-1.     LN20650
C     (for additional Q-branches see below)                              LN20660
C   ...................................................................  LN20670
C   .                                                                 .  LN20680
C   .   THE LINE COUPLING COEFFICIENTS FOR CARBON DIOXIDE HAVE BEEN   .  LN20690
C   .   MULTIPLIED BY A FACTOR OF 1.3 TO PROVIDE AGREEMENT WITH THE   .  LN20700
C   .   THE HIS DOWNLOOKING DATA OF 14 APRIL 1986 AND WITH THE HIS    .  LN20710
C   .   UPLOOKING GAPEX DATA OF 1 NOV. 1988. (CLOUGH ET AL 1991).     .  LN20720
C   .                                                                 .  LN20730
C   ...................................................................  LN20740
C                                                                        LN20750
C                                                                        LN20760
C     WITH THE EXCEPTION OF THE TRANSITION WAVENUMBER VALUES,
C     THE CO2 PARAMETERS, INCLUDING THE LINE COUPLING COEFFICIENTS,      LN20770
C     ARE FROM THE 1996 HITRAN LINE PARAMETERS. THE WAVENUMBER VALUES
C     ARE FROM HITRAN96.                                                 LN20800
C                                                                        LN20810
C     COUPLING COEFFICIENTS ARE FROM M.L. HOKE (1991).                   LN20820
C                                                                        LN20830
C    These coefficients have been updated to provide consistency with
C    HITRAN96 carbon dioxide line parameters (June 1999).
C
C    
C    ADDITIONAL LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES
C    AT 1932 cm-1, 2076 cm-1, 2093 cm-1, 2193 cm-1 (April 2001)
C     - coupling coefficients for these branches are from 
C       Strow et al. 1994
C**********************************************************************  LN20840
C                                                                        LN20850
      IMPLICIT CHARACTER*50 (C)                                          LN20860
      COMMON /CPLMOL_100/ MOLCPL_100(38),NCPL_100
      COMMON /CLINES_100/ CPL001(16),CPL005(16),CPL009(16),CPL013(16),       LN20880
     +     CPL017(16),CPL021(16),CPL025(16),CPL029(16),CPL033(16),       LN20890
     +     CPL037(16),CPL041(16),CPL045(16),CPL049(16),CPL053(16),       LN20900
     +     CPL057(16),CPL061(16),CPL065(16),CPL069(16),CPL073(16),       LN20910
     +     CPL077(16),CPL081(16),CPL085(16),CPL089(16),CPL093(16),       LN20920
     +     CPL097(16),CPL101(16),CPL105(16),CPL109(16),CPL113(16),       LN20930
     +     CPL117(16),CPL121(16),CPL125(16),CPL129(16),CPL133(16),       LN20940
     +     CPL137(16),CPL141(16),CPL145(16),CPL149(16),CPL153(16),       LN20950
     +     CPL157(16),CPL161(16),CPL165(16),CPL169(16),CPL173(16),       LN20960
     +     CPL177(16),CPL181(16),CPL185(16),CPL189(16),CPL193(16),       LN20970
     +     CPL197(16),CPL201(16),CPL205(16),CPL209(16),CPL213(16),       LN20980
     +     CPL217(16),CPL221(16),CPL225(16),CPL229(16),CPL233(16),       LN20990
     +     CPL237(16),CPL241(16),CPL245(16),CPL249(16),CPL253(16),       LN21000
     +     CPL257(16),CPL261(16),CPL265(16),CPL269(16),CPL273(16),       LN21010
     +     CPL277(16),CPL281(16),CPL285(16),CPL289(16),CPL293(16),       LN21020
     +     CPL297(16),CPL301(16),CPL305(16),CPL309(16),CPL313(16),       LN21030
     +     CPL317(16),CPL321(16),CPL325(16),CPL329(16),CPL333(16), 
     +     CPL337(16),CPL341(16),CPL345(16),CPL349(16),CPL353(16), 
     +     CPL357(16),CPL361(16),CPL365(16),CPL369(16),CPL373(16), 
     +     CPL377(16),CPL381(16),CPL385(16),CPL389(16),CPL393(16), 
     +     CPL397(16),CPL401(16),CPL405(16),CPL409(16),CPL413(16), 
     +     CPL417(16),CPL421(16),CPL425(16),CPL429(16),CPL433(16), 
     +     CPL437(16),CPL441(12) 
C                                                                        LN21040
C     MOLCPL CONTAINS THE FLAGS FOR MOLECULE 2 AND 7                     LN21050
C                                                                        LN21060
      DATA MOLCPL_100/0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,                         LN21070
     1                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                         LN21080
     2                0,0,0,0,0,0,0,0/                                       LN21090
C                                                                        LN21100

C     Previously NCPL was 598 ([([(293-1)/4]+1) * 16] + 12) / 2 = 598
C     Adding Line coupling changed it to 886:
C     ([([(437-1)/4]+1) * 16] + 12) / 2 = 886
C     Note: need to also change CPLINS  LN18720
C
      DATA NCPL_100/886/
C
      DATA CPL001/
     1     ' 71    0.000010 2.401E-35 0.000E+00.0340.0000   -1',
     1     '.00000.000.000000  0  0 O2 BAND<.007 CM-1000 0 0-3',
     1     ' 7 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00',
     1     ' 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00-3',
     2     ' 71    1.666572 1.370E-29 3.521E-04.0320.0314 2230',
     2     '.42240.71 0.00000  1  1         Q39R38  d404 3 1-1',
     2     ' 7 1.083685E+00-3.5147E-01 8.378760E-01-2.0751E-01',
     2     ' 6.860124E-01-1.3722E-01 5.810179E-01-9.7055E-02-1',
     3     ' 71    1.683641 3.851E-29 3.525E-04.0320.0314 2011',
     3     '.21190.71 0.00000  1  1         Q37R36  d404 3 1-1',
     3     ' 7 1.095630E+00-3.6449E-01 8.356707E-01-2.1345E-01',
     3     ' 6.750125E-01-1.4019E-01 5.640647E-01-9.8456E-02-1',
     4     ' 71    1.700770 1.023E-28 3.532E-04.0320.0314 1803',
     4     '.17650.71 0.00000  1  1         Q35R34  d404 3 1-1',
     4     ' 7 1.115808E+00-3.7857E-01 8.450911E-01-2.2006E-01',
     4     ' 6.781475E-01-1.4372E-01 5.629458E-01-1.0045E-01-1'/
      DATA CPL005/
     5     ' 71    1.717968 2.563E-28 3.536E-04.0320.0314 1606',
     5     '.35000.71 0.00000  1  1         Q33R32  d404 3 1-1',
     5     ' 7 1.132280E+00-3.9224E-01 8.518667E-01-2.2596E-01',
     5     ' 6.797590E-01-1.4650E-01 5.613293E-01-1.0171E-01-1',
     6     ' 71    1.735249 6.058E-28 3.542E-04.0320.0314 1420',
     6     '.76420.71 0.00000  1  1         Q31R30  d404 3 1-1',
     6     ' 7 1.145445E+00-4.0537E-01 8.556679E-01-2.3106E-01',
     6     ' 6.788533E-01-1.4844E-01 5.576693E-01-1.0220E-01-1',
     7     ' 71    1.752627 1.350E-27 3.549E-04.0320.0314 1246',
     7     '.44930.71 0.00000  1  1         Q29R28  d404 3 1-1',
     7     ' 7 1.153709E+00-4.1654E-01 8.550279E-01-2.3434E-01',
     7     ' 6.739851E-01-1.4884E-01 5.504953E-01-1.0138E-01-1',
     8     ' 71    1.770123 2.836E-27 3.559E-04.0320.0314 1083',
     8     '.43350.71 0.00000  1  1         Q27R26  d404 3 1-1',
     8     ' 7 1.152147E+00-4.2378E-01 8.461281E-01-2.3449E-01',
     8     ' 6.620204E-01-1.4677E-01 5.371368E-01-9.8547E-02-1'/
      DATA CPL009/
     9     ' 71    1.787763 5.605E-27 3.568E-04.0320.0314  931',
     9     '.74330.71 0.00000  1  1         Q25R24  d404 3 1-1',
     9     ' 7 1.142295E+00-4.2478E-01 8.301889E-01-2.3010E-01',
     9     ' 6.439650E-01-1.4127E-01 5.184343E-01-9.3064E-02-1',
     *     ' 71    1.805583 1.042E-26 3.580E-04.0380.0373  791',
     *     '.40320.71 0.00000  1  1         Q23R22  d404 3 1-1',
     *     ' 7 1.118166E+00-4.1498E-01 8.026411E-01-2.1854E-01',
     *     ' 6.161239E-01-1.3066E-01 4.912944E-01-8.3784E-02-1',
     1     ' 71    1.823634 1.820E-26 3.595E-04.0350.0343  662',
     1     '.43590.71 0.00000  1  1         Q21R20  d404 3 1-1',
     1     ' 7 1.071800E+00-3.8937E-01 7.577658E-01-1.9716E-01',
     1     ' 5.741029E-01-1.1339E-01 4.522096E-01-6.9746E-02-1',
     2     ' 71    1.841987 2.981E-26 3.611E-04.0370.0363  544',
     2     '.86220.71 0.00000  1  1         Q19R18  d404 3 1-1',
     2     ' 7 1.003603E+00-3.4386E-01 6.961964E-01-1.6409E-01',
     2     ' 5.185847E-01-8.8501E-02 4.018506E-01-5.0455E-02-1'/
      DATA CPL013/
     3     ' 71    1.860747 4.572E-26 3.632E-04.0380.0373  438',
     3     '.70100.71 0.00000  1  1         Q17R16  d404 3 1-1',
     3     ' 7 9.018632E-01-2.7140E-01 6.093928E-01-1.1616E-01',
     3     ' 4.428841E-01-5.4466E-02 3.347702E-01-2.5202E-02-1',
     4     ' 71    1.876791 2.726E-26 1.683E-04.0450.0441    2',
     4     '.08430.71 0.00000  1  1         Q 1P 2  d404 3 1-1',
     4     ' 7 9.550456E-01-3.9580E-01 8.042110E-01-2.8801E-01',
     4     ' 7.025955E-01-2.2387E-01 6.269926E-01-1.8088E-01-1',
     5     ' 71    1.880080 6.547E-26 3.659E-04.0380.0373  343',
     5     '.96940.71 0.00000  1  1         Q15R14  d404 3 1-1',
     5     ' 7 7.601213E-01-1.7046E-01 4.932302E-01-5.4191E-02',
     5     ' 3.441107E-01-1.2776E-02 2.488348E-01 4.3720E-03-1',
     6     ' 71    1.900255 8.724E-26 3.693E-04.0390.0382  260',
     6     '.68250.71 0.00000  1  1         Q13R12  d404 3 1-1',
     6     ' 7 5.722225E-01-4.7104E-02 3.437666E-01 1.5909E-02',
     6     ' 2.194682E-01 3.1475E-02 1.419426E-01 3.3980E-02-1'/
      DATA CPL017/
     7     ' 71    1.921745 1.076E-25 3.740E-04.0410.0402  188',
     7     '.85310.71 0.00000  1  1         Q11R10  d404 3 1-1',
     7     ' 7 3.297889E-01 7.6482E-02 1.557628E-01 7.8251E-02',
     7     ' 6.534860E-02 6.6489E-02 1.147462E-02 5.4611E-02-1',
     8     ' 71    1.945475 1.219E-25 3.808E-04.0430.0422  128',
     8     '.49210.71 0.00000  1  1         Q 9R 8  d404 3 1-1',
     8     ' 7 3.918693E-02 1.5052E-01-6.463015E-02 1.0263E-01',
     8     '-1.125612E-01 7.2165E-02-1.373579E-01 5.2122E-02-1',
     9     ' 71    1.949568 7.509E-26 2.559E-04.0440.0431   16',
     9     '.38760.71 0.00000  1  1         Q 3P 4  d404 3 1-1',
     9     ' 7 7.890647E-01-1.6462E-01 6.803579E-01-1.4306E-01',
     9     ' 6.022381E-01-1.2225E-01 5.418589E-01-1.0496E-01-1',
     *     ' 71    1.973505 1.249E-25 3.910E-04.0440.0431   79',
     *     '.60700.71 0.00000  1  1         Q 7R 6  d404 3 1-1',
     *     ' 7-2.779495E-01 1.1300E-01-3.002727E-01 5.6920E-02',
     *     '-3.000740E-01 2.9693E-02-2.924966E-01 1.4739E-02-1'/
      DATA CPL021/
     1     ' 71    1.987741 1.108E-25 2.851E-04.0420.0412   42',
     1     '.22400.71 0.00000  1  1         Q 5P 6  d404 3 1-1',
     1     ' 7 4.794608E-01 1.0262E-01 4.536001E-01 3.5299E-02',
     1     ' 4.231822E-01 6.8059E-03 3.943356E-01-6.8794E-03-1',
     2     ' 71    2.011594 1.128E-25 4.094E-04.0440.0431   42',
     2     '.20010.71 0.00000  1  1         Q 5R 4  d404 3 1-1',
     2     ' 7-5.327848E-01-3.5579E-02-4.816093E-01-4.6390E-02',
     2     '-4.396182E-01-4.6499E-02-4.047589E-01-4.3716E-02-1',
     3     ' 71    2.015887 1.307E-25 2.999E-04.0410.0402   79',
     3     '.56460.71 0.00000  1  1         Q 7P 8  d404 3 1-1',
     3     ' 7 1.235438E-01 2.1328E-01 1.883597E-01 1.2978E-01',
     3     ' 2.114051E-01 8.4445E-02 2.185150E-01 5.7132E-02-1',
     4     ' 71    2.039763 1.341E-25 3.084E-04.0400.0392  128',
     4     '.39780.71 0.00000  1  1         Q 9P10  d404 3 1-1',
     4     ' 7-2.063466E-01 1.6357E-01-6.143573E-02 1.2604E-01',
     4     ' 9.821956E-03 9.5538E-02 4.983153E-02 7.3278E-02-1'/
      DATA CPL025/
     5     ' 71    2.061431 1.239E-25 3.142E-04.0390.0382  188',
     5     '.71340.71 0.00000  1  1         Q11P12  d404 3 1-1',
     5     ' 7-4.831046E-01 3.2946E-02-2.749348E-01 6.4624E-02',
     5     '-1.646512E-01 6.3743E-02-9.755671E-02 5.6468E-02-1',
     6     ' 71    2.081814 1.048E-25 3.185E-04.0380.0373  260',
     6     '.50090.71 0.00000  1  1         Q13P14  d404 3 1-1',
     6     ' 7-6.968355E-01-1.1040E-01-4.437926E-01-1.4286E-02',
     6     '-3.048795E-01 1.5332E-02-2.174644E-01 2.4972E-02-1',
     7     ' 71    2.084317 8.366E-26 4.488E-04.0470.0461   16',
     7     '.25290.71 0.00000  1  1         Q 3R 2  d404 3 1-1',
     7     ' 7-5.651932E-01-1.1675E-01-4.878898E-01-9.0282E-02',
     7     '-4.338773E-01-7.2866E-02-3.927341E-01-6.0462E-02-1',
     8     ' 71    2.101386 8.186E-26 3.216E-04.0340.0333  343',
     8     '.74810.71 0.00000  1  1         Q15P16  d404 3 1-1',
     8     ' 7-8.583280E-01-2.3058E-01-5.745448E-01-8.6245E-02',
     8     '-4.151896E-01-3.2102E-02-3.128858E-01-8.0634E-03-1'/
      DATA CPL029/
     9     ' 71    2.120417 5.941E-26 3.240E-04.0360.0353  438',
     9     '.44140.71 0.00000  1  1         Q17P18  d404 3 1-1',
     9     ' 7-9.781035E-01-3.2006E-01-6.749418E-01-1.4386E-01',
     9     '-5.017825E-01-7.2235E-02-3.890086E-01-3.7363E-02-1',
     *     ' 71    2.139072 4.024E-26 3.260E-04.0350.0343  544',
     *     '.56510.71 0.00000  1  1         Q19P20  d404 3 1-1',
     *     ' 7-1.060925E+00-3.7897E-01-7.478667E-01-1.8521E-01',
     *     '-5.665768E-01-1.0269E-01-4.471765E-01-6.0590E-02-1',
     1     ' 71    2.157456 2.549E-26 3.275E-04.0350.0343  662',
     1     '.10210.71 0.00000  1  1         Q21P22  d404 3 1-1',
     1     ' 7-1.114748E+00-4.1189E-01-7.986852E-01-2.1158E-01',
     1     '-6.135178E-01-1.2353E-01-4.904359E-01-7.7288E-02-1',
     2     ' 71    2.175640 1.514E-26 3.287E-04.0320.0314  791',
     2     '.03320.71 0.00000  1  1         Q23P24  d404 3 1-1',
     2     ' 7-1.148377E+00-4.2691E-01-8.338457E-01-2.2700E-01',
     2     '-6.476877E-01-1.3703E-01-5.229612E-01-8.8783E-02-1'/
      DATA CPL033/
     3     ' 71    2.193676 8.448E-27 3.299E-04.0320.0314  931',
     3     '.33740.71 0.00000  1  1         Q25P26  d404 3 1-1',
     3     ' 7-1.164290E+00-4.2786E-01-8.549446E-01-2.3337E-01',
     3     '-6.701965E-01-1.4423E-01-5.455564E-01-9.5669E-02-1',
     4     ' 71    2.211599 4.431E-27 3.309E-04.0320.0314 1082',
     4     '.99210.71 0.00000  1  1         Q27P28  d404 3 1-1',
     4     ' 7-1.167919E+00-4.2068E-01-8.659866E-01-2.3402E-01',
     4     '-6.842068E-01-1.4721E-01-5.608208E-01-9.9336E-02-1',
     5     ' 71    2.229435 2.188E-27 3.318E-04.0320.0314 1245',
     5     '.97250.71 0.00000  1  1         Q29P30  d404 3 1-1',
     5     ' 7-1.163218E+00-4.0865E-01-8.700014E-01-2.3093E-01',
     5     '-6.921621E-01-1.4729E-01-5.707883E-01-1.0072E-01-1',
     6     ' 71    2.247206 1.017E-27 3.323E-04.0320.0314 1420',
     6     '.25230.71 0.00000  1  1         Q31P32  d404 3 1-1',
     6     ' 7-1.149577E+00-3.9362E-01-8.662894E-01-2.2521E-01',
     6     '-6.933521E-01-1.4521E-01-5.747715E-01-1.0031E-01-1'/
      DATA CPL037/
     7     ' 71    2.264927 4.460E-28 3.331E-04.0320.0314 1605',
     7     '.80300.71 0.00000  1  1         Q33P34  d404 3 1-1',
     7     ' 7-1.132430E+00-3.7744E-01-8.592104E-01-2.1813E-01',
     7     '-6.914504E-01-1.4184E-01-5.759681E-01-9.8767E-02-1',
     8     ' 71    2.282611 1.844E-28 3.336E-04.0320.0314 1802',
     8     '.59470.71 0.00000  1  1         Q35P36  d404 3 1-1',
     8     ' 7-1.111641E+00-3.6136E-01-8.487706E-01-2.1060E-01',
     8     '-6.866307E-01-1.3788E-01-5.747203E-01-9.6604E-02-1',
     9     ' 71    2.300266 7.199E-29 3.342E-04.0320.0314 2010',
     9     '.59530.71 0.00000  1  1         Q37P38  d404 3 1-1',
     9     ' 7-1.090479E+00-3.4557E-01-8.383655E-01-2.0283E-01',
     9     '-6.825245E-01-1.3348E-01-5.748819E-01-9.3942E-02-1',
     *     ' 71    2.317902 2.653E-29 3.346E-04.0320.0314 2229',
     *     '.77100.71 0.00000  1  1         Q39P40  d404 3 1-1',
     *     ' 7-1.076503E+00-3.3148E-01-8.385238E-01-1.9607E-01',
     *     '-6.914820E-01-1.2989E-01-5.898091E-01-9.2027E-02-1'/
      DATA CPL041/
     1     ' 71    3.961085 9.956E-26 6.864E-04.0500.0490    0',
     1     '.00000.71 0.00000  1  1         Q 1R 0  d404 3 1-1',
     1     ' 7-6.874010E-02-1.5962E-03-6.168349E-02-1.2712E-03',
     1     '-5.666008E-02-1.0585E-03-5.276715E-02-9.0542E-04-1',
     2     ' 21  612.196012 1.306E-25 5.472E-03.0589.0584 3196',
     2     '.99980.630.000000  3  2             Q 80 464 2 2-1',
     2     ' 2 8.954269E-02-3.2300E-03 7.303400E-02-2.1646E-03',
     2     ' 6.252350E-02-1.5965E-03 5.478330E-02-1.2325E-03-1',
     3     ' 21  612.518377 2.384E-25 5.606E-03.0594.0590 3073',
     3     '.12720.640.000000  3  2             Q 78 464 2 2-1',
     3     ' 2 7.751120E-02-2.3153E-03 6.083089E-02-1.4004E-03',
     3     ' 5.065710E-02-9.5481E-04 4.343170E-02-6.9090E-04-1',
     4     ' 21  612.829271 4.283E-25 5.742E-03.0598.0597 2952',
     4     '.34450.650.000000  3  2             Q 76 464 2 2-1',
     4     ' 2 7.815340E-02-2.4218E-03 6.037200E-02-1.4342E-03',
     4     ' 4.959500E-02-9.5802E-04 4.201600E-02-6.7971E-04-1'/
      DATA CPL045/
     5     ' 21  613.128917 7.568E-25 5.876E-03.0603.0605 2834',
     5     '.65380.660.000000  3  2             Q 74 464 2 2-1',
     5     ' 2 8.068450E-02-2.5909E-03 6.190730E-02-1.5231E-03',
     5     ' 5.052580E-02-1.0101E-03 4.254120E-02-7.1157E-04-1',
     6     ' 21  613.417536 1.315E-24 6.008E-03.0608.0612 2720',
     6     '.05690.660.000000  3  2             Q 72 464 2 2-1',
     6     ' 2 8.379930E-02-2.7565E-03 6.407180E-02-1.6098E-03',
     6     ' 5.210920E-02-1.0611E-03 4.372420E-02-7.4321E-04-1',
     7     ' 21  613.695347 2.247E-24 6.137E-03.0613.0620 2608',
     7     '.55590.670.000000  3  2             Q 70 464 2 2-1',
     7     ' 2 8.704670E-02-2.7787E-03 6.640010E-02-1.5972E-03',
     7     ' 5.388240E-02-1.0376E-03 4.511390E-02-7.1620E-04-1',
     8     ' 21  613.962567 3.777E-24 6.265E-03.0618.0629 2500',
     8     '.15230.680.000000  3  2             Q 68 464 2 2-1',
     8     ' 2 9.070879E-02-3.0975E-03 6.905990E-02-1.7927E-03',
     8     ' 5.594290E-02-1.1729E-03 4.676360E-02-8.1602E-04-1'/
      DATA CPL049/
     9     ' 21  614.219411 6.242E-24 6.390E-03.0623.0638 2394',
     9     '.84840.690.000000  3  2             Q 66 464 2 2-1',
     9     ' 2 9.441249E-02-3.3709E-03 7.175349E-02-1.9478E-03',
     9     ' 5.803980E-02-1.2734E-03 4.845490E-02-8.8577E-04-1',
     *     ' 21  614.466091 1.014E-23 6.509E-03.0628.0647 2292',
     *     '.64530.700.000000  3  2             Q 64 464 2 2-1',
     *     ' 2 9.832940E-02-3.6683E-03 7.460440E-02-2.1139E-03',
     *     ' 6.026150E-02-1.3793E-03 5.025020E-02-9.5811E-04-1',
     1     ' 21  614.702818 1.621E-23 6.630E-03.0633.0657 2193',
     1     '.54540.710.000000  3  2             Q 62 464 2 2-1',
     1     ' 2 1.024543E-01-3.9944E-03 7.759180E-02-2.2942E-03',
     1     ' 6.258460E-02-1.4932E-03 5.212609E-02-1.0353E-03-1',
     2     ' 21  614.929797 2.548E-23 6.749E-03.0638.0667 2097',
     2     '.54980.720.000000  3  2             Q 60 464 2 2-1',
     2     ' 2 1.067625E-01-4.3524E-03 8.070010E-02-2.4913E-03',
     2     ' 6.499350E-02-1.6171E-03 5.406830E-02-1.1189E-03-1'/
      DATA CPL053/
     3     ' 21  615.147231 3.936E-23 6.862E-03.0643.0678 2004',
     3     '.66040.730.000000  3  2             Q 58 464 2 2-1',
     3     ' 2 1.115075E-01-4.7602E-03 8.413339E-02-2.7156E-03',
     3     ' 6.766370E-02-1.7580E-03 5.622759E-02-1.2139E-03-1',
     4     ' 21  615.355319 5.979E-23 6.973E-03.0647.0689 1914',
     4     '.87870.740.000000  3  2             Q 56 464 2 2-1',
     4     ' 2 1.163968E-01-5.2006E-03 8.765250E-02-2.9557E-03',
     4     ' 7.038720E-02-1.9076E-03 5.842200E-02-1.3140E-03-1',
     5     ' 21  615.554256 8.929E-23 7.081E-03.0652.0700 1828',
     5     '.20630.750.000000  3  2             Q 54 464 2 2-1',
     5     ' 2 1.216514E-01-5.6904E-03 9.142900E-02-3.2209E-03',
     5     ' 7.330959E-02-2.0721E-03 6.077630E-02-1.4236E-03-1',
     6     ' 21  615.744233 1.311E-22 7.188E-03.0656.0712 1744',
     6     '.64450.750.000000  3  2             Q 52 464 2 2-1',
     6     ' 2 1.266785E-01-6.1987E-03 9.499230E-02-3.4927E-03',
     6     ' 7.603310E-02-2.2386E-03 6.294730E-02-1.5333E-03-1'/
      DATA CPL057/
     7     ' 21  615.925436 1.891E-22 7.287E-03.0660.0725 1664',
     7     '.19470.760.000000  3  2             Q 50 464 2 2-1',
     7     ' 2 1.324180E-01-6.7996E-03 9.905870E-02-3.8133E-03',
     7     ' 7.914010E-02-2.4348E-03 6.542380E-02-1.6623E-03-1',
     8     ' 21  616.098045 2.682E-22 7.387E-03.0663.0738 1586',
     8     '.85830.770.000000  3  2             Q 48 464 2 2-1',
     8     ' 2 1.386060E-01-7.4669E-03 1.034462E-01-4.1668E-03',
     8     ' 8.249540E-02-2.6493E-03 6.810180E-02-1.8023E-03-1',
     9     ' 21  616.262237 3.737E-22 7.482E-03.0666.0751 1512',
     9     '.63660.770.000000  3  2             Q 46 464 2 2-1',
     9     ' 2 1.450670E-01-8.1895E-03 1.080053E-01-4.5438E-03',
     9     ' 8.597030E-02-2.8748E-03 7.086560E-02-1.9475E-03-1',
     *     ' 21  616.418182 5.116E-22 7.573E-03.0668.0765 1441',
     *     '.53080.780.000000  3  2             Q 44 465 2 2-1',
     *     ' 2 1.515800E-01-8.9517E-03 1.125384E-01-4.9325E-03',
     *     ' 8.938409E-02-3.1019E-03 7.355530E-02-2.0901E-03-1'/
      DATA CPL061/
     1     ' 21  616.566046 6.881E-22 7.662E-03.0671.0780 1373',
     1     '.54210.780.000000  3  2             Q 42 465 2 2-1',
     1     ' 2 1.587170E-01-9.7834E-03 1.175005E-01-5.3453E-03',
     1     ' 9.311770E-02-3.3361E-03 7.649330E-02-2.2322E-03-1',
     2     ' 21  616.705989 9.089E-22 7.746E-03.0673.0795 1308',
     2     '.67180.780.000000  3  2             Q 40 465 2 2-1',
     2     ' 2 1.661140E-01-1.0442E-02 1.225822E-01-5.6177E-03',
     2     ' 9.690069E-02-3.4550E-03 7.944300E-02-2.2782E-03-1',
     3     ' 21  616.838162 1.179E-21 7.827E-03.0675.0810 1246',
     3     '.92050.780.000000  3  2             Q 38 465 2 2-1',
     3     ' 2 1.746810E-01-1.1858E-02 1.285011E-01-6.3814E-03',
     3     ' 1.013363E-01-3.9295E-03 8.292440E-02-2.5977E-03-1',
     4     ' 21  616.962715 1.501E-21 7.903E-03.0677.0826 1188',
     4     '.28980.780.000000  3  2             Q 36 465 2 2-1',
     4     ' 2 1.823640E-01-1.2615E-02 1.336530E-01-6.6617E-03',
     4     ' 1.050985E-01-4.0273E-03 8.580520E-02-2.6133E-03-1'/
      DATA CPL065/
     5     ' 21  617.079787 1.876E-21 7.977E-03.0679.0843 1132',
     5     '.78030.780.000000  3  2             Q 34 465 2 2-1',
     5     ' 2 1.916330E-01-1.3779E-02 1.399060E-01-7.1695E-03',
     5     ' 1.096680E-01-4.2719E-03 8.931649E-02-2.7320E-03-1',
     6     ' 21  617.189514 2.300E-21 8.046E-03.0682.0860 1080',
     6     '.39320.780.000000  3  2             Q 32 465 2 2-1',
     6     ' 2 2.006550E-01-1.5210E-02 1.458600E-01-7.8220E-03',
     6     ' 1.139437E-01-4.6086E-03 9.254440E-02-2.9156E-03-1',
     7     ' 21  617.292023 2.766E-21 8.113E-03.0686.0878 1031',
     7     '.12920.780.000000  3  2             Q 30 465 2 2-1',
     7     ' 2 2.104830E-01-1.6540E-02 1.522950E-01-8.3248E-03',
     7     ' 1.185314E-01-4.7971E-03 9.598680E-02-2.9644E-03-1',
     8     ' 21  617.387434 3.260E-21 8.176E-03.0690.0897  984',
     8     '.98900.780.000000  3  2             Q 28 465 2 2-1',
     8     ' 2 2.208050E-01-1.7129E-02 1.588860E-01-8.2408E-03',
     8     ' 1.231139E-01-4.5128E-03 9.934080E-02-2.6257E-03-1'/
      DATA CPL069/
     9     ' 21  617.475863 3.763E-21 8.233E-03.0695.0916  941',
     9     '.97370.780.000000  3  2             Q 26 465 2 2-1',
     9     ' 2 2.310230E-01-1.8751E-02 1.652040E-01-8.7888E-03',
     9     ' 1.273727E-01-4.6726E-03 1.023555E-01-2.6247E-03-1',
     *     ' 21  617.557416 4.251E-21 8.286E-03.0701.0935  902',
     *     '.08380.780.000000  3  2             Q 24 465 2 2-1',
     *     ' 2 2.423590E-01-1.9111E-02 1.721460E-01-8.3274E-03',
     *     ' 1.319890E-01-4.0173E-03 1.055834E-01-1.9557E-03-1',
     1     ' 21  617.632192 4.697E-21 8.336E-03.0708.0956  865',
     1     '.32000.770.000000  3  2             Q 22 465 2 2-1',
     1     ' 2 2.521350E-01-1.8187E-02 1.774890E-01-6.8917E-03',
     1     ' 1.350830E-01-2.6047E-03 1.073800E-01-6.8557E-04-1',
     2     ' 21  617.700283 5.068E-21 8.382E-03.0717.0977  831',
     2     '.68290.760.000000  3  2             Q 20 465 2 2-1',
     2     ' 2 2.624700E-01-1.7969E-02 1.828840E-01-5.7630E-03',
     2     ' 1.379820E-01-1.3615E-03 1.088906E-01 4.7098E-04-1'/
      DATA CPL073/
     3     ' 21  617.761776 5.331E-21 8.423E-03.0728.0998  801',
     3     '.17320.740.000000  3  2             Q 18 465 2 2-1',
     3     ' 2 2.716090E-01-1.2322E-02 1.868230E-01-7.7059E-04',
     3     ' 1.393860E-01 2.8036E-03 1.089348E-01 3.9222E-03-1',
     4     ' 21  617.816745 5.457E-21 8.461E-03.0740.1020  773',
     4     '.79120.710.000000  3  2             Q 16 465 2 2-1',
     4     ' 2 2.785250E-01-4.9414E-03 1.883700E-01 5.5132E-03',
     4     ' 1.384500E-01 7.9239E-03 1.067625E-01 8.0865E-03-1',
     5     ' 21  617.865262 5.417E-21 8.494E-03.0754.1043  749',
     5     '.53750.690.000000  3  2             Q 14 465 2 2-1',
     5     ' 2 2.786550E-01 1.6385E-02 1.836640E-01 2.1152E-02',
     5     ' 1.317940E-01 1.9850E-02 9.937979E-02 1.7450E-02-1',
     6     ' 21  617.907388 5.193E-21 8.523E-03.0771.1067  728',
     6     '.41240.690.000000  3  2             Q 12 465 2 2-1',
     6     ' 2 2.673190E-01 5.5714E-02 1.687270E-01 4.8269E-02',
     6     ' 1.159626E-01 3.9797E-02 8.371739E-02 3.2739E-02-1'/
      DATA CPL077/
     7     ' 21  617.943177 4.775E-21 8.548E-03.0790.1091  710',
     7     '.41630.700.000000  3  2             Q 10 465 2 2-1',
     7     ' 2 2.343900E-01 1.6074E-01 1.352000E-01 1.1706E-01',
     7     ' 8.376029E-02 8.9060E-02 5.347550E-02 6.9972E-02-1',
     8     ' 21  617.972675 4.165E-21 8.568E-03.0811.1116  695',
     8     '.54960.710.000000  3  2             Q  8 465 2 2-1',
     8     ' 2 1.443000E-01 4.0498E-01 5.435950E-02 2.6988E-01',
     8     ' 1.086917E-02 1.9534E-01-1.250730E-02 1.4877E-01-1',
     9     ' 21  617.995920 3.379E-21 8.585E-03.0836.1141  683',
     9     '.81240.730.000000  3  2             Q  6 465 2 2-1',
     9     ' 2-8.047130E-02 1.1181E+00-1.357070E-01 7.0037E-01',
     9     '-1.547650E-01 4.8794E-01-1.593150E-01 3.6276E-01-1',
     *     ' 21  618.012943 2.442E-21 8.595E-03.0863.1190  675',
     *     '.20500.740.000000  3  2             Q  4 465 2 2-1',
     *     ' 2-8.516560E-01 3.6551E+00-7.574060E-01 2.1692E+00',
     *     '-6.807320E-01 1.4582E+00-6.170970E-01 1.0603E+00-1'/
      DATA CPL081/
     1     ' 21  618.023765 1.395E-21 8.605E-03.0893.1228  669',
     1     '.72750.750.000000  3  2             Q  2 465 2 2-1',
     1     ' 2-5.640050E+00-1.0555E+01-4.470570E+00-6.7582E+00',
     1     '-3.743090E+00-4.8014E+00-3.241810E+00-3.6357E+00-1',
     2     ' 21  667.386195 7.476E-20 1.647E-02.0893.1228    2',
     2     '.34130.750.000000  2  1             Q  2 465 2 2-1',
     2     ' 2 4.205760E+00-5.8766E+00 3.334370E+00-3.7640E+00',
     2     ' 2.792270E+00-2.6750E+00 2.418650E+00-2.0261E+00-1',
     3     ' 21  667.400695 1.311E-19 1.648E-02.0863.1190    7',
     3     '.80430.740.000000  2  1             Q  4 465 2 2-1',
     3     ' 2 6.460220E-01 2.0171E+00 5.739110E-01 1.1961E+00',
     3     ' 5.155800E-01 8.0354E-01 4.672850E-01 5.8395E-01-1',
     4     ' 21  667.423478 1.816E-19 1.648E-02.0836.1141   16',
     4     '.38900.730.000000  2  1             Q  6 465 2 2-1',
     4     ' 2 7.248670E-02 6.1975E-01 1.115361E-01 3.8769E-01',
     4     ' 1.243710E-01 2.6980E-01 1.267903E-01 2.0041E-01-1'/
      DATA CPL085/
     5     ' 21  667.454542 2.243E-19 1.647E-02.0811.1116   28',
     5     '.09510.710.000000  2  1             Q  8 465 2 2-1',
     5     ' 2-1.004549E-01 2.2759E-01-3.422380E-02 1.5140E-01',
     5     '-2.396290E-03 1.0944E-01 1.456260E-02 8.3265E-02-1',
     6     ' 21  667.493883 2.578E-19 1.647E-02.0790.1091   42',
     6     '.92250.700.000000  2  1             Q 10 465 2 2-1',
     6     ' 2-1.684020E-01 9.1130E-02-9.516520E-02 6.6109E-02',
     6     '-5.732610E-02 5.0180E-02-3.515200E-02 3.9360E-02-1',
     7     ' 21  667.541495 2.813E-19 1.647E-02.0771.1067   60',
     7     '.87090.690.000000  2  1             Q 12 465 2 2-1',
     7     ' 2-1.944020E-01 3.2209E-02-1.212549E-01 2.7599E-02',
     7     '-8.221719E-02 2.2641E-02-5.843890E-02 1.8568E-02-1',
     8     ' 21  667.597373 2.946E-19 1.648E-02.0754.1043   81',
     8     '.94010.690.000000  2  1             Q 14 465 2 2-1',
     8     ' 2-2.023580E-01 1.0175E-02-1.319890E-01 1.2425E-02',
     8     '-9.368060E-02 1.1489E-02-6.981650E-02 1.0026E-02-1'/
      DATA CPL089/
     9     ' 21  667.661509 2.981E-19 1.648E-02.0740.1020  106',
     9     '.12970.710.000000  2  1             Q 16 465 2 2-1',
     9     ' 2-2.020850E-01-1.5421E-03-1.352780E-01 3.8432E-03',
     9     '-9.840609E-02 4.9509E-03-7.508410E-02 4.8970E-03-1',
     *     ' 21  667.733896 2.927E-19 1.648E-02.0728.0998  133',
     *     '.43930.740.000000  2  1             Q 18 465 2 2-1',
     *     ' 2-1.994460E-01-5.9959E-03-1.361360E-01 1.7673E-04',
     *     '-1.008046E-01 2.0098E-03-7.817810E-02 2.5282E-03-1',
     1     ' 21  667.814523 2.798E-19 1.648E-02.0717.0977  163',
     1     '.86840.760.000000  2  1             Q 20 465 2 2-1',
     1     ' 2-1.928420E-01-9.3341E-03-1.333150E-01-2.7214E-03',
     1     '-9.982570E-02-3.8948E-04-7.818460E-02 5.4867E-04-1',
     2     ' 21  667.903382 2.609E-19 1.647E-02.0708.0956  197',
     2     '.41660.770.000000  2  1             Q 22 465 2 2-1',
     2     ' 2-1.866150E-01-9.6885E-03-1.304550E-01-3.4759E-03',
     2     '-9.861150E-02-1.1582E-03-7.787260E-02-1.4468E-04-1'/
      DATA CPL093/
     3     ' 21  668.000459 2.378E-19 1.648E-02.0701.0935  234',
     3     '.08330.780.000000  2  1             Q 24 465 2 2-1',
     3     ' 2-1.791920E-01-1.0275E-02-1.263314E-01-4.3234E-03',
     3     '-9.618180E-02-1.9794E-03-7.641270E-02-8.7781E-04-1',
     4     ' 21  668.105744 2.120E-19 1.648E-02.0695.0916  273',
     4     '.86800.780.000000  2  1             Q 26 465 2 2-1',
     4     ' 2-1.720940E-01-1.0276E-02-1.222013E-01-4.6981E-03',
     4     '-9.359610E-02-2.4214E-03-7.473180E-02-1.3043E-03-1',
     5     ' 21  668.219223 1.851E-19 1.647E-02.0690.0897  316',
     5     '.76980.780.000000  2  1             Q 28 465 2 2-1',
     5     ' 2-1.647230E-01-9.4160E-03-1.176617E-01-4.4179E-03',
     5     '-9.055280E-02-2.3481E-03-7.258680E-02-1.3149E-03-1',
     6     ' 21  668.340880 1.584E-19 1.647E-02.0686.0878  362',
     6     '.78820.780.000000  2  1             Q 30 465 2 2-1',
     6     ' 2-1.579110E-01-9.2258E-03-1.134523E-01-4.5527E-03',
     6     '-8.772660E-02-2.5681E-03-7.059649E-02-1.5493E-03-1'/
      DATA CPL097/
     7     ' 21  668.470702 1.330E-19 1.648E-02.0682.0860  411',
     7     '.92250.780.000000  2  1             Q 32 465 2 2-1',
     7     ' 2-1.514890E-01-8.5960E-03-1.093404E-01-4.3399E-03',
     7     '-8.486010E-02-2.5089E-03-6.849179E-02-1.5548E-03-1',
     8     ' 21  668.608671 1.095E-19 1.647E-02.0679.0843  464',
     8     '.17170.780.000000  2  1             Q 34 465 2 2-1',
     8     ' 2-1.452100E-01-7.8357E-03-1.053052E-01-4.0033E-03',
     8     '-8.203260E-02-2.3413E-03-6.641180E-02-1.4677E-03-1',
     9     ' 21  668.754770 8.856E-20 1.648E-02.0677.0826  519',
     9     '.53500.780.000000  2  1             Q 36 465 2 2-1',
     9     ' 2-1.397630E-01-7.3280E-03-1.017640E-01-3.8054E-03',
     9     '-7.953010E-02-2.2624E-03-6.455150E-02-1.4427E-03-1',
     *     ' 21  668.908980 7.032E-20 1.648E-02.0675.0810  578',
     *     '.01160.780.000000  2  1             Q 38 465 2 2-1',
     *     ' 2-1.340560E-01-6.9488E-03-9.794330E-02-3.6825E-03',
     *     '-7.675070E-02-2.2344E-03-6.242990E-02-1.4555E-03-1'/
      DATA CPL101/
     1     ' 21  669.071282 5.485E-20 1.648E-02.0673.0795  639',
     1     '.60040.780.000000  2  1             Q 40 465 2 2-1',
     1     ' 2-1.286259E-01-6.1932E-03-9.428120E-02-3.2761E-03',
     1     '-7.407010E-02-1.9820E-03-6.036940E-02-1.2854E-03-1',
     2     ' 21  669.241656 4.204E-20 1.648E-02.0671.0780  704',
     2     '.30050.780.000000  2  1             Q 42 465 2 2-1',
     2     ' 2-1.236196E-01-5.8899E-03-9.088949E-02-3.1684E-03',
     2     '-7.157540E-02-1.9486E-03-5.844670E-02-1.2851E-03-1',
     3     ' 21  669.420080 3.167E-20 1.648E-02.0668.0765  772',
     3     '.11070.780.000000  2  1             Q 44 465 2 2-1',
     3     ' 2-1.190072E-01-5.4778E-03-8.775389E-02-2.9731E-03',
     3     '-6.926270E-02-1.8435E-03-5.665920E-02-1.2253E-03-1',
     4     ' 21  669.606531 2.345E-20 1.648E-02.0666.0751  843',
     4     '.03010.770.000000  2  1             Q 46 464 2 2-1',
     4     ' 2-1.144598E-01-5.0631E-03-8.462349E-02-2.7664E-03',
     4     '-6.692920E-02-1.7255E-03-5.483790E-02-1.1530E-03-1'/
      DATA CPL105/
     5     ' 21  669.800987 1.707E-20 1.647E-02.0663.0738  917',
     5     '.05730.770.000000  2  1             Q 48 464 2 2-1',
     5     ' 2-1.104857E-01-4.7008E-03-8.190909E-02-2.5843E-03',
     5     '-6.492069E-02-1.6206E-03-5.328180E-02-1.0880E-03-1',
     6     ' 21  670.003422 1.222E-20 1.647E-02.0660.0725  994',
     6     '.19130.760.000000  2  1             Q 50 464 2 2-1',
     6     ' 2-1.065636E-01-4.3611E-03-7.917260E-02-2.4094E-03',
     6     '-6.285500E-02-1.5172E-03-5.165160E-02-1.0223E-03-1',
     7     ' 21  670.213812 8.606E-21 1.648E-02.0656.0712 1074',
     7     '.43070.750.000000  2  1             Q 52 464 2 2-1',
     7     ' 2-1.026506E-01-4.0382E-03-7.643610E-02-2.2412E-03',
     7     '-6.078669E-02-1.4166E-03-5.001880E-02-9.5759E-04-1',
     8     ' 21  670.432130 5.960E-21 1.648E-02.0652.0700 1157',
     8     '.77420.750.000000  2  1             Q 54 464 2 2-1',
     8     ' 2-9.913930E-02-3.7514E-03-7.397909E-02-2.0909E-03',
     8     '-5.892770E-02-1.3261E-03-4.854980E-02-8.9899E-04-1'/
      DATA CPL109/
     9     ' 21  670.658348 4.061E-21 1.648E-02.0647.0689 1244',
     9     '.22050.740.000000  2  1             Q 56 464 2 2-1',
     9     ' 2-9.576450E-02-3.4891E-03-7.160140E-02-1.9523E-03',
     9     '-5.712070E-02-1.2422E-03-4.711590E-02-8.4425E-04-1',
     *     ' 21  670.892440 2.722E-21 1.648E-02.0643.0678 1333',
     *     '.76790.730.000000  2  1             Q 58 464 2 2-1',
     *     ' 2-9.251189E-02-3.2469E-03-6.929129E-02-1.8230E-03',
     *     '-5.535140E-02-1.1631E-03-4.570280E-02-7.9215E-04-1',
     1     ' 21  671.134374 1.795E-21 1.648E-02.0638.0667 1426',
     1     '.41540.720.000000  2  1             Q 60 464 2 2-1',
     1     ' 2-8.929700E-02-3.0166E-03-6.700329E-02-1.6991E-03',
     1     '-5.359640E-02-1.0866E-03-4.430010E-02-7.4142E-04-1',
     2     ' 21  671.384123 1.165E-21 1.648E-02.0633.0657 1522',
     2     '.16110.710.000000  2  1             Q 62 464 2 2-1',
     2     ' 2-8.654879E-02-2.8198E-03-6.507410E-02-1.5938E-03',
     2     '-5.213390E-02-1.0220E-03-4.314570E-02-6.9880E-04-1'/
      DATA CPL113/
     3     ' 21  671.641654 7.438E-22 1.648E-02.0628.0647 1621',
     3     '.00370.700.000000  2  1             Q 64 464 2 2-1',
     3     ' 2-8.402549E-02-2.6467E-03-6.328270E-02-1.5006E-03',
     3     '-5.076370E-02-9.6453E-04-4.205240E-02-6.6063E-04-1',
     4     ' 21  671.906936 4.674E-22 1.648E-02.0623.0638 1722',
     4     '.94130.690.000000  2  1             Q 66 464 2 2-1',
     4     ' 2-8.132670E-02-2.4743E-03-6.134830E-02-1.4065E-03',
     4     '-4.927130E-02-9.0559E-04-4.085380E-02-6.2094E-04-1',
     5     ' 21  672.179936 2.891E-22 1.648E-02.0618.0629 1827',
     5     '.97240.680.000000  2  1             Q 68 464 2 2-1',
     5     ' 2-7.890870E-02-2.3162E-03-5.961410E-02-1.3179E-03',
     5     '-4.793360E-02-8.4871E-04-3.978000E-02-5.8159E-04-1',
     6     ' 21  672.460621 1.760E-22 1.648E-02.0613.0620 1936',
     6     '.09530.670.000000  2  1             Q 70 464 2 2-1',
     6     ' 2-7.644130E-02-2.1063E-03-5.782660E-02-1.1859E-03',
     6     '-4.654260E-02-7.5527E-04-3.865420E-02-5.1119E-04-1'/
      DATA CPL117/
     7     ' 21  672.748956 1.055E-22 1.648E-02.0608.0612 2047',
     7     '.30810.660.000000  2  1             Q 72 464 2 2-1',
     7     ' 2-7.405320E-02-2.1436E-03-5.611060E-02-1.2308E-03',
     7     '-4.521530E-02-7.9847E-04-3.758820E-02-5.5078E-04-1',
     8     ' 21  673.044906 6.221E-23 1.648E-02.0603.0605 2161',
     8     '.60890.660.000000  2  1             Q 74 464 2 2-1',
     8     ' 2-7.223839E-02-2.0723E-03-5.481840E-02-1.1982E-03',
     8     '-4.422730E-02-7.8214E-04-3.680040E-02-5.4265E-04-1',
     9     ' 21  673.348434 3.612E-23 1.648E-02.0598.0597 2278',
     9     '.99610.650.000000  2  1             Q 76 464 2 2-1',
     9     ' 2-7.007780E-02-1.9686E-03-5.324280E-02-1.1430E-03',
     9     '-4.299360E-02-7.4871E-04-3.579810E-02-5.2107E-04-1',
     *     ' 21  673.659504 2.065E-23 1.648E-02.0594.0590 2399',
     *     '.46780.640.000000  2  1             Q 78 464 2 2-1',
     *     ' 2-6.803810E-02-1.8629E-03-5.176730E-02-1.0855E-03',
     *     '-4.184830E-02-7.1309E-04-3.487510E-02-4.9747E-04-1'/
      DATA CPL121/
     1     ' 21  673.978079 1.162E-23 1.648E-02.0589.0584 2523',
     1     '.02150.630.000000  2  1             Q 80 464 2 2-1',
     1     ' 2-6.634810E-02-1.7720E-03-5.054660E-02-1.0356E-03',
     1     '-4.090320E-02-6.8199E-04-3.411590E-02-4.7671E-04-1',
     2     ' 21  674.304119 6.438E-24 1.647E-02.0585.0578 2649',
     2     '.65580.620.000000  2  1             Q 82 464 2 2-1',
     2     ' 2-6.442020E-02-1.6744E-03-4.913870E-02-9.8114E-04',
     2     '-3.980340E-02-6.4739E-04-3.322800E-02-4.5323E-04-1',
     3     ' 21  674.637585 3.512E-24 1.647E-02.0581.0572 2779',
     3     '.36820.610.000000  2  1             Q 84 464 2 2-1',
     3     ' 2-6.275750E-02-1.5882E-03-4.793620E-02-9.3287E-04',
     3     '-3.887650E-02-6.1665E-04-3.249090E-02-4.3230E-04-1',
     4     ' 21  674.978437 1.887E-24 1.648E-02.0578.0566 2912',
     4     '.15650.600.000000  2  1             Q 86 464 2 2-1',
     4     ' 2-6.104929E-02-1.5025E-03-4.671160E-02-8.8455E-04',
     4     '-3.794830E-02-5.8568E-04-3.177070E-02-4.1110E-04-1'/
      DATA CPL125/
     5     ' 21  675.326634 9.979E-25 1.648E-02.0574.0561 3048',
     5     '.01900.590.000000  2  1             Q 88 464 2 2-1',
     5     ' 2-5.964400E-02-1.4288E-03-4.576129E-02-8.4310E-04',
     5     '-3.728400E-02-5.5930E-04-3.130790E-02-3.9322E-04-1',
     6     ' 21  675.682135 5.197E-25 1.648E-02.0571.0556 3186',
     6     '.95310.580.000000  2  1             Q 90 464 2 2-1',
     6     ' 2-5.837780E-02-1.3575E-03-4.504630E-02-8.0400E-04',
     6     '-3.691740E-02-5.3535E-04-3.118050E-02-3.7779E-04-1',
     7     ' 21  676.044898 2.665E-25 1.647E-02.0568.0551 3328',
     7     '.95650.570.000000  2  1             Q 92 464 2 2-1',
     7     ' 2-5.827900E-02-1.3118E-03-4.566510E-02-7.8963E-04',
     7     '-3.794960E-02-5.3468E-04-3.246360E-02-3.8376E-04-1',
     8     ' 21  676.414878 1.346E-25 1.648E-02.0565.0547 3474',
     8     '.02710.560.000000  2  1             Q 94 464 2 2-1',
     8     ' 2-6.767929E-02-1.8326E-03-5.537999E-02-1.2352E-03',
     8     '-4.752150E-02-9.1491E-04-4.170660E-02-7.0849E-04-1'/
      DATA CPL129/
     9     ' 21  714.584932 1.563E-25 9.937E-03.0585.0578 3323',
     9     '.96000.620.000000  5  2             Q 82 464 2 2-1',
     9     ' 2 1.043419E-01-4.3304E-03 8.500960E-02-2.8955E-03',
     9     ' 7.270119E-02-2.1316E-03 6.365320E-02-1.6435E-03-1',
     *     ' 21  714.833340 2.794E-25 9.818E-03.0589.0584 3196',
     *     '.99980.630.000000  5  2             Q 80 464 2 2-1',
     *     ' 2 8.547109E-02-2.7180E-03 6.647290E-02-1.5905E-03',
     *     ' 5.495750E-02-1.0532E-03 4.684420E-02-7.4217E-04-1',
     1     ' 21  715.080664 4.916E-25 9.698E-03.0594.0590 3073',
     1     '.12720.640.000000  5  2             Q 78 464 2 2-1',
     1     ' 2 8.333519E-02-2.6967E-03 6.357520E-02-1.5434E-03',
     1     ' 5.167760E-02-9.9856E-04 4.338360E-02-6.8722E-04-1',
     2     ' 21  715.326500 8.517E-25 9.581E-03.0598.0597 2952',
     2     '.34450.650.000000  5  2             Q 76 464 2 2-1',
     2     ' 2 8.376680E-02-2.7561E-03 6.339970E-02-1.5692E-03',
     2     ' 5.112770E-02-1.0101E-03 4.259060E-02-6.9147E-04-1'/
      DATA CPL133/
     3     ' 22  715.521048 1.019E-25 4.503E-03.0628.0647 2273',
     3     '.70190.700.000000  5  2             Q 64 464 2 2-1',
     3     ' 2 8.441290E-02-2.7048E-03 6.850740E-02-1.7945E-03',
     3     ' 5.840900E-02-1.3125E-03 5.103280E-02-1.0071E-03-1',
     4     ' 21  715.570456 1.453E-24 9.469E-03.0603.0605 2834',
     4     '.65380.660.000000  5  2             Q 74 464 2 2-1',
     4     ' 2 8.474569E-02-2.8145E-03 6.390150E-02-1.5963E-03',
     4     ' 5.132920E-02-1.0240E-03 4.258410E-02-6.9883E-04-1',
     5     ' 21  715.812157 2.441E-24 9.361E-03.0608.0612 2720',
     5     '.05690.660.000000  5  2             Q 72 464 2 2-1',
     5     ' 2 8.603010E-02-2.8502E-03 6.472050E-02-1.6059E-03',
     5     ' 5.186350E-02-1.0241E-03 4.292340E-02-6.9491E-04-1',
     6     ' 22  715.873962 1.599E-25 4.502E-03.0633.0657 2174',
     6     '.60420.710.000000  5  2             Q 62 464 2 2-1',
     6     ' 2 6.773780E-02-1.5636E-03 5.272540E-02-9.0671E-04',
     6     ' 4.365530E-02-5.9804E-04 3.728530E-02-4.2094E-04-1'/
      DATA CPL137/
     7     ' 21  716.051237 4.037E-24 9.256E-03.0613.0620 2608',
     7     '.55590.670.000000  5  2             Q 70 464 2 2-1',
     7     ' 2 8.750819E-02-2.7021E-03 6.572280E-02-1.4888E-03',
     7     ' 5.258370E-02-9.2888E-04 4.345120E-02-6.1595E-04-1',
     8     ' 22  716.217992 2.470E-25 4.503E-03.0638.0667 2078',
     8     '.61110.720.000000  5  2             Q 60 464 2 2-1',
     8     ' 2 6.627010E-02-1.5855E-03 5.046990E-02-8.9114E-04',
     8     ' 4.104360E-02-5.6980E-04 3.452020E-02-3.8905E-04-1',
     9     ' 21  716.287347 6.573E-24 9.154E-03.0618.0629 2500',
     9     '.15230.680.000000  5  2             Q 68 464 2 2-1',
     9     ' 2 8.909940E-02-2.9068E-03 6.682130E-02-1.6236E-03',
     9     ' 5.339490E-02-1.0283E-03 4.407130E-02-6.9359E-04-1',
     *     ' 21  716.520149 1.054E-23 9.060E-03.0623.0638 2394',
     *     '.84840.690.000000  5  2             Q 66 464 2 2-1',
     *     ' 2 9.066200E-02-3.0329E-03 6.788860E-02-1.6936E-03',
     *     ' 5.418010E-02-1.0735E-03 4.467190E-02-7.2532E-04-1'/
      DATA CPL141/
     1     ' 22  716.552908 3.753E-25 4.503E-03.0643.0678 1985',
     1     '.72400.730.000000  5  2             Q 58 464 2 2-1',
     1     ' 2 6.720480E-02-1.6767E-03 5.063760E-02-9.3556E-04',
     1     ' 4.077580E-02-5.9370E-04 3.398590E-02-4.0228E-04-1',
     2     ' 21  716.749319 1.663E-23 8.965E-03.0628.0647 2292',
     2     '.64530.700.000000  5  2             Q 64 464 2 2-1',
     2     ' 2 9.255220E-02-3.1756E-03 6.919380E-02-1.7701E-03',
     2     ' 5.515380E-02-1.1207E-03 4.542850E-02-7.5690E-04-1',
     3     ' 22  716.878488 5.611E-25 4.503E-03.0647.0689 1895',
     3     '.94450.740.000000  5  2             Q 56 464 2 2-1',
     3     ' 2 6.896890E-02-1.7884E-03 5.166329E-02-9.9438E-04',
     3     ' 4.136470E-02-6.2911E-04 3.428880E-02-4.2514E-04-1',
     4     ' 21  716.974545 2.582E-23 8.870E-03.0633.0657 2193',
     4     '.54540.710.000000  5  2             Q 62 464 2 2-1',
     4     ' 2 9.464389E-02-3.3325E-03 7.064850E-02-1.8530E-03',
     4     ' 5.624710E-02-1.1713E-03 4.628520E-02-7.9024E-04-1'/
      DATA CPL145/
     5     ' 22  717.194520 8.252E-25 4.503E-03.0652.0700 1809',
     5     '.27430.750.000000  5  2             Q 54 464 2 2-1',
     5     ' 2 7.103980E-02-1.9092E-03 5.301270E-02-1.0579E-03',
     5     ' 4.228770E-02-6.6755E-04 3.492840E-02-4.5020E-04-1',
     6     ' 21  717.195526 3.948E-23 8.784E-03.0638.0667 2097',
     6     '.54980.720.000000  5  2             Q 60 464 2 2-1',
     6     ' 2 9.685650E-02-3.5025E-03 7.219029E-02-1.9423E-03',
     6     ' 5.740930E-02-1.2254E-03 4.719910E-02-8.2566E-04-1',
     7     ' 21  717.411975 5.940E-23 8.699E-03.0643.0678 2004',
     7     '.66040.730.000000  5  2             Q 58 464 2 2-1',
     7     ' 2 9.922510E-02-3.6889E-03 7.383480E-02-2.0397E-03',
     7     ' 5.864430E-02-1.2839E-03 4.816890E-02-8.6360E-04-1',
     8     ' 22  717.500804 1.194E-24 4.504E-03.0656.0712 1725',
     8     '.71460.750.000000  5  2             Q 52 464 2 2-1',
     8     ' 2 7.318740E-02-2.0359E-03 5.445180E-02-1.1238E-03',
     8     ' 4.331470E-02-7.0691E-04 3.568110E-02-4.7559E-04-1'/
      DATA CPL149/
     9     ' 21  717.623616 8.796E-23 8.617E-03.0647.0689 1914',
     9     '.87870.740.000000  5  2             Q 56 464 2 2-1',
     9     ' 2 1.017588E-01-3.8921E-03 7.558980E-02-2.1450E-03',
     9     ' 5.995990E-02-1.3468E-03 4.919980E-02-9.0405E-04-1',
     *     ' 22  717.797147 1.699E-24 4.504E-03.0660.0725 1645',
     *     '.26680.760.000000  5  2             Q 50 464 2 2-1',
     *     ' 2 7.544030E-02-2.1740E-03 5.597410E-02-1.1947E-03',
     *     ' 4.441710E-02-7.4884E-04 3.650790E-02-5.0233E-04-1',
     1     ' 21  717.830183 1.282E-22 8.541E-03.0652.0700 1828',
     1     '.20630.750.000000  5  2             Q 54 464 2 2-1',
     1     ' 2 1.043289E-01-4.1088E-03 7.735520E-02-2.2563E-03',
     1     ' 6.127290E-02-1.4124E-03 5.022420E-02-9.4589E-04-1',
     2     ' 21  718.031423 1.838E-22 8.466E-03.0656.0712 1744',
     2     '.64450.750.000000  5  2             Q 52 464 2 2-1',
     2     ' 2 1.070121E-01-4.3477E-03 7.916740E-02-2.3776E-03',
     2     ' 6.260280E-02-1.4834E-03 5.124340E-02-9.9060E-04-1'/
      DATA CPL153/
     3     ' 22  718.083367 2.377E-24 4.503E-03.0663.0738 1567',
     3     '.93240.770.000000  5  2             Q 48 464 2 2-1',
     3     ' 2 7.788170E-02-2.3286E-03 5.762640E-02-1.2733E-03',
     3     ' 4.562220E-02-7.9487E-04 3.742050E-02-5.3140E-04-1',
     4     ' 21  718.227094 2.592E-22 8.392E-03.0660.0725 1664',
     4     '.19470.760.000000  5  2             Q 50 464 2 2-1',
     4     ' 2 1.102465E-01-4.6324E-03 8.138260E-02-2.5228E-03',
     4     ' 6.424470E-02-1.5687E-03 5.251870E-02-1.0445E-03-1',
     5     ' 22  718.359291 3.270E-24 4.503E-03.0666.0751 1493',
     5     '.71260.770.000000  5  2             Q 46 464 2 2-1',
     5     ' 2 8.071569E-02-2.5067E-03 5.957510E-02-1.3636E-03',
     5     ' 4.706650E-02-8.4744E-04 3.853850E-02-5.6436E-04-1',
     6     ' 21  718.416963 3.597E-22 8.325E-03.0663.0738 1586',
     6     '.85830.770.000000  5  2             Q 48 464 2 2-1',
     6     ' 2 1.136668E-01-4.9378E-03 8.373820E-02-2.6772E-03',
     6     ' 6.600230E-02-1.6584E-03 5.389279E-02-1.1007E-03-1'/
      DATA CPL157/
     7     ' 21  718.600811 4.909E-22 8.258E-03.0666.0751 1512',
     7     '.63660.770.000000  5  2             Q 46 464 2 2-1',
     7     ' 2 1.170429E-01-5.2550E-03 8.601059E-02-2.8332E-03',
     7     ' 6.766370E-02-1.7463E-03 5.516550E-02-1.1539E-03-1',
     8     ' 22  718.624753 4.423E-24 4.503E-03.0668.0765 1422',
     8     '.60860.780.000000  5  2             Q 44 465 2 2-1',
     8     ' 2 8.358610E-02-2.6898E-03 6.153550E-02-1.4537E-03',
     8     ' 4.851600E-02-8.9822E-04 3.965780E-02-5.9505E-04-1',
     9     ' 21  718.778426 6.588E-22 8.195E-03.0668.0765 1441',
     9     '.53080.780.000000  5  2             Q 44 465 2 2-1',
     9     ' 2 1.210235E-01-5.6217E-03 8.872240E-02-3.0117E-03',
     9     ' 6.966700E-02-1.8457E-03 5.671770E-02-1.2132E-03-1',
     *     ' 22  718.879598 5.881E-24 4.503E-03.0671.0780 1354',
     *     '.62180.780.000000  5  2             Q 42 465 2 2-1',
     *     ' 2 8.661120E-02-2.8773E-03 6.358039E-02-1.5414E-03',
     *     ' 5.001230E-02-9.4474E-04 4.080440E-02-6.2109E-04-1'/
      DATA CPL161/
     1     ' 21  718.949607 8.695E-22 8.135E-03.0671.0780 1373',
     1     '.54210.780.000000  5  2             Q 42 465 2 2-1',
     1     ' 2 1.248195E-01-5.9652E-03 9.123790E-02-3.1668E-03',
     1     ' 7.147790E-02-1.9244E-03 5.808530E-02-1.2545E-03-1',
     2     ' 21  719.114166 1.128E-21 8.078E-03.0673.0795 1308',
     2     '.67180.780.000000  5  2             Q 40 465 2 2-1',
     2     ' 2 1.294020E-01-6.2158E-03 9.431890E-02-3.2432E-03',
     2     ' 7.372950E-02-1.9370E-03 5.981170E-02-1.2400E-03-1',
     3     ' 22  719.123678 7.684E-24 4.502E-03.0673.0795 1289',
     3     '.75310.780.000000  5  2             Q 40 465 2 2-1',
     3     ' 2 8.986770E-02-3.0069E-03 6.577610E-02-1.5840E-03',
     3     ' 5.161910E-02-9.5497E-04 4.203550E-02-6.1724E-04-1',
     4     ' 21  719.271920 1.438E-21 8.022E-03.0675.0810 1246',
     4     '.92050.780.000000  5  2             Q 38 465 2 2-1',
     4     ' 2 1.341990E-01-6.9127E-03 9.751039E-02-3.6185E-03',
     4     ' 7.604089E-02-2.1706E-03 6.156930E-02-1.3983E-03-1'/
      DATA CPL165/
     5     ' 22  719.356853 9.866E-24 4.503E-03.0675.0810 1228',
     5     '.00350.780.000000  5  2             Q 38 465 2 2-1',
     5     ' 2 9.332569E-02-3.3510E-03 6.808750E-02-1.7690E-03',
     5     ' 5.329740E-02-1.0700E-03 4.331600E-02-6.9491E-04-1',
     6     ' 21  719.422701 1.802E-21 7.972E-03.0677.0826 1188',
     6     '.28980.780.000000  5  2             Q 36 465 2 2-1',
     6     ' 2 1.389830E-01-7.1973E-03 1.006525E-01-3.6884E-03',
     6     ' 7.828210E-02-2.1650E-03 6.325150E-02-1.3627E-03-1',
     7     ' 21  719.566345 2.218E-21 7.924E-03.0679.0843 1132',
     7     '.78030.780.000000  5  2             Q 34 465 2 2-1',
     7     ' 2 1.440660E-01-7.6545E-03 1.039181E-01-3.8590E-03',
     7     ' 8.057009E-02-2.2272E-03 6.493630E-02-1.3767E-03-1',
     8     ' 22  719.578992 1.244E-23 4.502E-03.0677.0826 1169',
     8     '.37430.780.000000  5  2             Q 36 465 2 2-1',
     8     ' 2 9.698519E-02-3.5144E-03 7.051460E-02-1.8178E-03',
     8     ' 5.504720E-02-1.0770E-03 4.463940E-02-6.8451E-04-1'/
      DATA CPL169/
     9     ' 21  719.702703 2.681E-21 7.881E-03.0682.0860 1080',
     9     '.39320.780.000000  5  2             Q 32 465 2 2-1',
     9     ' 2 1.494610E-01-8.3243E-03 1.073228E-01-4.1488E-03',
     9     ' 8.290230E-02-2.3673E-03 6.661460E-02-1.4468E-03-1',
     *     ' 22  719.789971 1.541E-23 4.504E-03.0679.0843 1113',
     *     '.86630.780.000000  5  2             Q 34 465 2 2-1',
     *     ' 2 1.005537E-01-3.7426E-03 7.280520E-02-1.9045E-03',
     *     ' 5.664620E-02-1.1098E-03 4.581200E-02-6.9332E-04-1',
     1     ' 21  719.831631 3.180E-21 7.838E-03.0686.0878 1031',
     1     '.12920.780.000000  5  2             Q 30 465 2 2-1',
     1     ' 2 1.558050E-01-8.9053E-03 1.113892E-01-4.3364E-03',
     1     ' 8.574410E-02-2.4121E-03 6.870370E-02-1.4322E-03-1',
     2     ' 21  719.952997 3.700E-21 7.797E-03.0690.0897  984',
     2     '.98900.780.000000  5  2             Q 28 465 2 2-1',
     2     ' 2 1.614730E-01-8.9419E-03 1.147835E-01-4.1240E-03',
     2     ' 8.794499E-02-2.1473E-03 7.019740E-02-1.1704E-03-1'/
      DATA CPL173/
     3     ' 22  719.989673 1.873E-23 4.503E-03.0682.0860 1061',
     3     '.48050.780.000000  5  2             Q 32 465 2 2-1',
     3     ' 2 1.046682E-01-4.0936E-03 7.545330E-02-2.0601E-03',
     3     ' 5.850000E-02-1.1875E-03 4.717700E-02-7.3389E-04-1',
     4     ' 21  720.066676 4.222E-21 7.762E-03.0695.0916  941',
     4     '.97370.780.000000  5  2             Q 26 465 2 2-1',
     4     ' 2 1.678820E-01-9.6879E-03 1.185977E-01-4.3486E-03',
     4     ' 9.040070E-02-2.1901E-03 7.184710E-02-1.1418E-03-1',
     5     ' 21  720.172553 4.718E-21 7.727E-03.0701.0935  902',
     5     '.08380.780.000000  5  2             Q 24 465 2 2-1',
     5     ' 2 1.743560E-01-9.6145E-03 1.222780E-01-3.9500E-03',
     5     ' 9.264190E-02-1.7423E-03 7.325239E-02-7.1746E-04-1',
     6     ' 22  720.177991 2.234E-23 4.503E-03.0686.0878 1012',
     6     '.21780.780.000000  5  2             Q 30 465 2 2-1',
     6     ' 2 1.090856E-01-4.3859E-03 7.827040E-02-2.1572E-03',
     6     ' 6.045650E-02-1.2133E-03 4.860570E-02-7.2974E-04-1'/
      DATA CPL177/
     7     ' 21  720.270521 5.160E-21 7.695E-03.0708.0956  865',
     7     '.32000.770.000000  5  2             Q 22 465 2 2-1',
     7     ' 2 1.811810E-01-8.9716E-03 1.259908E-01-3.0953E-03',
     7     ' 9.477130E-02-9.3032E-04 7.447830E-02 7.9618E-07-1',
     8     ' 22  720.354822 2.613E-23 4.503E-03.0690.0897  966',
     8     '.07890.780.000000  5  2             Q 28 465 2 2-1',
     8     ' 2 1.136772E-01-4.4613E-03 8.113560E-02-2.0879E-03',
     8     ' 6.240389E-02-1.1070E-03 4.999670E-02-6.1834E-04-1',
     9     ' 21  720.360484 5.517E-21 7.667E-03.0717.0977  831',
     9     '.68290.760.000000  5  2             Q 20 465 2 2-1',
     9     ' 2 1.868100E-01-8.5502E-03 1.284634E-01-2.3335E-03',
     9     ' 9.570209E-02-1.7355E-04 7.457450E-02 6.7579E-04-1',
     *     ' 21  720.442352 5.756E-21 7.641E-03.0728.0998  801',
     *     '.17320.740.000000  5  2             Q 18 465 2 2-1',
     *     ' 2 1.922960E-01-5.2611E-03 1.304680E-01 4.7493E-04',
     *     ' 9.604270E-02 2.1339E-03 7.404929E-02 2.5707E-03-1'/
      DATA CPL181/
     1     ' 21  720.516044 5.847E-21 7.618E-03.0740.1020  773',
     1     '.79120.710.000000  5  2             Q 16 465 2 2-1',
     1     ' 2 1.957410E-01-1.0498E-03 1.303770E-01 3.9495E-03',
     1     ' 9.436959E-02 4.9235E-03 7.163000E-02 4.8188E-03-1',
     2     ' 22  720.520072 2.995E-23 4.503E-03.0695.0916  923',
     2     '.06470.780.000000  5  2             Q 26 465 2 2-1',
     2     ' 2 1.183806E-01-4.8312E-03 8.399169E-02-2.2001E-03',
     2     ' 6.428760E-02-1.1289E-03 5.130060E-02-6.0466E-04-1',
     3     ' 21  720.581489 5.766E-21 7.597E-03.0754.1043  749',
     3     '.53750.690.000000  5  2             Q 14 465 2 2-1',
     3     ' 2 1.939860E-01 1.0416E-02 1.255592E-01 1.2266E-02',
     3     ' 8.840390E-02 1.1224E-02 6.531590E-02 9.7422E-03-1',
     4     ' 21  720.638622 5.496E-21 7.579E-03.0771.1067  728',
     4     '.41240.690.000000  5  2             Q 12 465 2 2-1',
     4     ' 2 1.870180E-01 3.1290E-02 1.158144E-01 2.6628E-02',
     4     ' 7.788300E-02 2.1772E-02 5.482620E-02 1.7819E-02-1'/
      DATA CPL185/
     5     ' 22  720.673654 3.362E-23 4.502E-03.0701.0935  883',
     5     '.17590.780.000000  5  2             Q 24 465 2 2-1',
     5     ' 2 1.227538E-01-4.7939E-03 8.643050E-02-2.0046E-03',
     5     ' 6.573319E-02-9.0973E-04 5.217160E-02-3.9702E-04-1',
     6     ' 21  720.687389 5.029E-21 7.564E-03.0790.1091  710',
     6     '.41630.700.000000  5  2             Q 10 465 2 2-1',
     6     ' 2 1.585090E-01 8.7192E-02 8.787480E-02 6.3050E-02',
     6     ' 5.152030E-02 4.7761E-02 3.030950E-02 3.7406E-02-1',
     7     ' 21  720.727743 4.369E-21 7.551E-03.0811.1116  695',
     7     '.54960.710.000000  5  2             Q  8 465 2 2-1',
     7     ' 2 9.608299E-02 2.1607E-01 3.160300E-02 1.4365E-01',
     7     ' 6.649499E-04 1.0379E-01-1.578460E-02 7.8936E-02-1',
     8     ' 21  720.759644 3.533E-21 7.542E-03.0836.1141  683',
     8     '.81240.730.000000  5  2             Q  6 465 2 2-1',
     8     ' 2-7.450300E-02 5.8986E-01-1.121445E-01 3.6877E-01',
     8     '-1.243398E-01 2.5652E-01-1.264484E-01 1.9046E-01-1'/
      DATA CPL189/
     9     ' 21  720.783063 2.548E-21 7.535E-03.0863.1190  675',
     9     '.20500.740.000000  5  2             Q  4 465 2 2-1',
     9     ' 2-6.337760E-01 1.9115E+00-5.628870E-01 1.1329E+00',
     9     '-5.056220E-01 7.6077E-01-4.582500E-01 5.5268E-01-1',
     *     ' 21  720.797976 1.453E-21 7.531E-03.0893.1228  669',
     *     '.72750.750.000000  5  2             Q  2 465 2 2-1',
     *     ' 2-4.101890E+00-5.5910E+00-3.252210E+00-3.5814E+00',
     *     '-2.723500E+00-2.5454E+00-2.359110E+00-1.9280E+00-1',
     1     ' 22  720.815488 3.693E-23 4.503E-03.0708.0956  846',
     1     '.41300.770.000000  5  2             Q 22 465 2 2-1',
     1     ' 2 1.277770E-01-4.5231E-03 8.922550E-02-1.6129E-03',
     1     ' 6.738810E-02-5.3026E-04 5.317260E-02-5.8263E-05-1',
     2     ' 22  720.945500 3.963E-23 4.503E-03.0717.0977  812',
     2     '.77690.760.000000  5  2             Q 20 465 2 2-1',
     2     ' 2 1.319890E-01-4.3334E-03 9.115729E-02-1.2470E-03',
     2     ' 6.820190E-02-1.6272E-04 5.337540E-02 2.7130E-04-1'/
      DATA CPL193/
     3     ' 22  721.063622 4.149E-23 4.503E-03.0728.0998  782',
     3     '.26790.740.000000  5  2             Q 18 465 2 2-1',
     3     ' 2 1.356030E-01-2.7440E-03 9.238839E-02 1.1248E-04',
     3     ' 6.829810E-02 9.5528E-04 5.288530E-02 1.1899E-03-1',
     4     ' 22  721.169795 4.228E-23 4.503E-03.0740.1020  754',
     4     '.88670.710.000000  5  2             Q 16 465 2 2-1',
     4     ' 2 1.386190E-01-7.6398E-04 9.278880E-02 1.7699E-03',
     4     ' 6.750510E-02 2.2948E-03 5.151250E-02 2.2737E-03-1',
     5     ' 22  721.263964 4.181E-23 4.503E-03.0754.1043  730',
     5     '.63360.690.000000  5  2             Q 14 465 2 2-1',
     5     ' 2 1.378780E-01 4.8391E-03 8.982220E-02 5.8427E-03',
     5     ' 6.367660E-02 5.3847E-03 4.739800E-02 4.6910E-03-1',
     6     ' 22  721.346082 3.994E-23 4.502E-03.0771.1067  709',
     6     '.50910.690.000000  5  2             Q 12 465 2 2-1',
     6     ' 2 1.319890E-01 1.5053E-02 8.217950E-02 1.2858E-02',
     6     ' 5.561530E-02 1.0533E-02 3.944590E-02 8.6300E-03-1'/
      DATA CPL197/
     7     ' 22  721.416107 3.662E-23 4.502E-03.0790.1091  691',
     7     '.51350.700.000000  5  2             Q 10 465 2 2-1',
     7     ' 2 1.136798E-01 4.2236E-02 6.395220E-02 3.0605E-02',
     7     ' 3.828890E-02 2.3214E-02 2.327000E-02 1.8199E-02-1',
     8     ' 22  721.474005 3.187E-23 4.502E-03.0811.1116  676',
     8     '.64720.710.000000  5  2             Q  8 465 2 2-1',
     8     ' 2 6.788730E-02 1.0503E-01 2.289690E-02 6.9849E-02',
     8     ' 1.291992E-03 5.0482E-02-1.020968E-02 3.8401E-02-1',
     9     ' 22  721.519745 2.581E-23 4.503E-03.0836.1141  664',
     9     '.91030.730.000000  5  2             Q  6 465 2 2-1',
     9     ' 2-4.760730E-02 2.8782E-01-7.463690E-02 1.8005E-01',
     9     '-8.363289E-02 1.2531E-01-8.544900E-02 9.3084E-02-1',
     *     ' 22  721.553307 1.863E-23 4.503E-03.0863.1190  656',
     *     '.30310.740.000000  5  2             Q  4 465 2 2-1',
     *     ' 2-4.419350E-01 9.2862E-01-3.923530E-01 5.5039E-01',
     *     '-3.523260E-01 3.6960E-01-3.192540E-01 2.6851E-01-1'/
      DATA CPL201/
     1     ' 22  721.574672 1.063E-23 4.503E-03.0893.1228  650',
     1     '.82580.750.000000  5  2             Q  2 465 2 2-1',
     1     ' 2-2.852330E+00-2.7054E+00-2.261350E+00-1.7328E+00',
     1     '-1.893580E+00-1.2315E+00-1.640210E+00-9.3270E-01-1',
     2     ' 21  735.944778 1.115E-25 5.002E-03.0615.0625 3223',
     2     '.66380.680.000000  8  4             Q 69 464 2 2-1',
     2     ' 2 9.022520E-02-3.0728E-03 7.330310E-02-2.0419E-03',
     2     ' 6.255080E-02-1.4957E-03 5.468190E-02-1.1486E-03-1',
     3     ' 21  736.256550 1.793E-25 4.922E-03.0620.0633 3116',
     3     '.70190.690.000000  8  4             Q 67 464 2 2-1',
     3     ' 2 7.312370E-02-1.8191E-03 5.683080E-02-1.0497E-03',
     3     ' 4.699109E-02-6.8853E-04 4.008160E-02-4.8173E-04-1',
     4     ' 21  736.561118 2.840E-25 4.847E-03.0626.0642 3012',
     4     '.84300.700.000000  8  4             Q 65 464 2 2-1',
     4     ' 2 7.174440E-02-1.8531E-03 5.457140E-02-1.0399E-03',
     4     ' 4.430920E-02-6.6290E-04 3.720340E-02-4.5068E-04-1'/
      DATA CPL205/
     5     ' 21  736.858304 4.429E-25 4.776E-03.0631.0652 2912',
     5     '.08910.710.000000  8  4             Q 63 464 2 2-1',
     5     ' 2 7.287019E-02-1.9608E-03 5.487170E-02-1.0940E-03',
     5     ' 4.413110E-02-6.9325E-04 3.672370E-02-4.6849E-04-1',
     6     ' 21  737.147938 6.798E-25 4.707E-03.0636.0662 2814',
     6     '.44190.720.000000  8  4             Q 61 464 2 2-1',
     6     ' 2 7.470319E-02-2.0838E-03 5.594940E-02-1.1592E-03',
     6     ' 4.475510E-02-7.3287E-04 3.704740E-02-4.9433E-04-1',
     7     ' 21  737.429856 1.027E-24 4.640E-03.0640.0672 2719',
     7     '.90310.730.000000  8  4             Q 59 464 2 2-1',
     7     ' 2 7.679230E-02-2.2156E-03 5.731310E-02-1.2290E-03',
     7     ' 4.568850E-02-7.7538E-04 3.768830E-02-5.2225E-04-1',
     8     ' 21  737.703900 1.527E-24 4.576E-03.0645.0683 2628',
     8     '.47390.730.000000  8  4             Q 57 464 2 2-1',
     8     ' 2 7.896589E-02-2.3581E-03 5.877950E-02-1.3042E-03',
     8     ' 4.673890E-02-8.2098E-04 3.846180E-02-5.5212E-04-1'/
      DATA CPL209/
     9     ' 21  737.969920 2.235E-24 4.515E-03.0650.0694 2540',
     9     '.15620.740.000000  8  4             Q 55 464 2 2-1',
     9     ' 2 8.156589E-02-2.5251E-03 6.056960E-02-1.3916E-03',
     9     ' 4.805970E-02-8.7381E-04 3.946930E-02-5.8652E-04-1',
     *     ' 21  738.227770 3.219E-24 4.457E-03.0654.0706 2454',
     *     '.95170.750.000000  8  4             Q 53 464 2 2-1',
     *     ' 2 8.414769E-02-2.6997E-03 6.235450E-02-1.4826E-03',
     *     ' 4.938700E-02-9.2832E-04 4.049630E-02-6.2176E-04-1',
     1     ' 21  738.477311 4.561E-24 4.400E-03.0658.0718 2372',
     1     '.86130.760.000000  8  4             Q 51 464 2 2-1',
     1     ' 2 8.705840E-02-2.8991E-03 6.437080E-02-1.5856E-03',
     1     ' 5.089240E-02-9.8948E-04 4.166760E-02-6.6092E-04-1',
     2     ' 21  738.718409 6.359E-24 4.346E-03.0661.0731 2293',
     2     '.88700.760.000000  8  4             Q 49 464 2 2-1',
     2     ' 2 9.002500E-02-3.1102E-03 6.641570E-02-1.6932E-03',
     2     ' 5.241600E-02-1.0525E-03 4.285060E-02-7.0073E-04-1'/
      DATA CPL213/
     3     ' 21  738.950938 8.721E-24 4.294E-03.0664.0744 2218',
     3     '.02950.770.000000  8  4             Q 47 464 2 2-1',
     3     ' 2 9.320220E-02-3.3429E-03 6.859840E-02-1.8104E-03',
     3     ' 5.403710E-02-1.1203E-03 4.411029E-02-7.4292E-04-1',
     4     ' 21  739.174774 1.176E-23 4.244E-03.0667.0758 2145',
     4     '.29050.770.000000  8  4             Q 45 465 2 2-1',
     4     ' 2 9.656010E-02-3.5928E-03 7.089030E-02-1.9335E-03',
     4     ' 5.573229E-02-1.1898E-03 4.542200E-02-7.8503E-04-1',
     5     ' 21  739.389803 1.561E-23 4.199E-03.0670.0772 2075',
     5     '.67110.780.000000  8  4             Q 43 465 2 2-1',
     5     ' 2 1.000467E-01-3.8513E-03 7.324850E-02-2.0561E-03',
     5     ' 5.746260E-02-1.2560E-03 4.675060E-02-8.2291E-04-1',
     6     ' 21  739.595914 2.036E-23 4.154E-03.0672.0787 2009',
     6     '.17260.780.000000  8  4             Q 41 465 2 2-1',
     6     ' 2 1.038362E-01-4.0735E-03 7.580040E-02-2.1455E-03',
     6     ' 5.932679E-02-1.2934E-03 4.817929E-02-8.3608E-04-1'/
      DATA CPL217/
     7     ' 21  739.793001 2.611E-23 4.112E-03.0674.0802 1945',
     7     '.79600.780.000000  8  4             Q 39 465 2 2-1',
     7     ' 2 1.078103E-01-4.3689E-03 7.844719E-02-2.2786E-03',
     7     ' 6.124170E-02-1.3608E-03 4.963140E-02-8.7161E-04-1',
     8     ' 21  739.980966 3.291E-23 4.072E-03.0676.0818 1885',
     8     '.54240.780.000000  8  4             Q 37 465 2 2-1',
     8     ' 2 1.120626E-01-4.8374E-03 8.127079E-02-2.5187E-03',
     8     ' 6.328010E-02-1.5031E-03 5.117580E-02-9.6325E-04-1',
     9     ' 21  740.159714 4.075E-23 4.033E-03.0678.0835 1828',
     9     '.41270.780.000000  8  4             Q 35 465 2 2-1',
     9     ' 2 1.164813E-01-4.9880E-03 8.414640E-02-2.5233E-03',
     9     ' 6.531460E-02-1.4609E-03 5.268770E-02-9.0557E-04-1',
     *     ' 21  740.329156 4.957E-23 3.998E-03.0681.0852 1774',
     *     '.40800.780.000000  8  4             Q 33 465 2 2-1',
     *     ' 2 1.214226E-01-5.5925E-03 8.737169E-02-2.8259E-03',
     *     ' 6.760650E-02-1.6362E-03 5.439980E-02-1.0164E-03-1'/
      DATA CPL221/
     1     ' 21  740.489209 5.919E-23 3.964E-03.0684.0869 1723',
     1     '.52930.780.000000  8  4             Q 31 465 2 2-1',
     1     ' 2 1.261923E-01-5.6775E-03 9.037080E-02-2.7504E-03',
     1     ' 6.965660E-02-1.5188E-03 5.587140E-02-8.9220E-04-1',
     2     ' 21  740.564671 1.426E-25 4.961E-03.0618.0629 3169',
     2     '.76860.680.000000  8  4             Q 68 464 2 2-1',
     2     ' 2 6.196060E-01-1.4033E-01 5.030480E-01-9.3046E-02',
     2     ' 4.289610E-01-6.8019E-02 3.748290E-01-5.2164E-02-1',
     3     ' 21  740.607401 2.276E-25 4.886E-03.0623.0638 3064',
     3     '.36280.690.000000  8  4             Q 66 464 2 2-1',
     3     ' 2 4.570930E-01-6.4044E-02 3.525470E-01-3.4976E-02',
     3     ' 2.898220E-01-2.1792E-02 2.460770E-01-1.4501E-02-1',
     4     ' 21  740.639794 6.937E-23 3.932E-03.0687.0887 1675',
     4     '.77730.780.000000  8  4             Q 29 465 2 2-1',
     4     ' 2 1.312610E-01-6.2297E-03 9.350250E-02-2.9748E-03',
     4     ' 7.175609E-02-1.6179E-03 5.734820E-02-9.3471E-04-1'/
      DATA CPL225/
     5     ' 21  740.651907 3.575E-25 4.812E-03.0628.0647 2962',
     5     '.06050.700.000000  8  4             Q 64 464 2 2-1',
     5     ' 2 4.212650E-01-5.9257E-02 3.165500E-01-3.1416E-02',
     5     ' 2.544880E-01-1.8920E-02 2.118740E-01-1.2125E-02-1',
     6     ' 21  740.697871 5.529E-25 4.741E-03.0633.0657 2862',
     6     '.86330.710.000000  8  4             Q 62 464 2 2-1',
     6     ' 2 4.096170E-01-5.8493E-02 3.043300E-01-3.1112E-02',
     6     ' 2.419560E-01-1.8803E-02 1.993160E-01-1.2100E-02-1',
     7     ' 21  740.744994 8.416E-25 4.673E-03.0638.0667 2766',
     7     '.77290.720.000000  8  4             Q 60 464 2 2-1',
     7     ' 2 4.045990E-01-5.8140E-02 2.990130E-01-3.1027E-02',
     7     ' 2.363920E-01-1.8837E-02 1.936090E-01-1.2195E-02-1',
     8     ' 21  740.780837 7.974E-23 3.902E-03.0692.0906 1631',
     8     '.15280.780.000000  8  4             Q 27 465 2 2-1',
     8     ' 2 1.368510E-01-6.5692E-03 9.691110E-02-3.0143E-03',
     8     ' 7.401419E-02-1.5617E-03 5.891730E-02-8.4747E-04-1'/
      DATA CPL229/
     9     ' 21  740.792989 1.261E-24 4.608E-03.0643.0678 2673',
     9     '.79130.730.000000  8  4             Q 58 464 2 2-1',
     9     ' 2 4.009330E-01-5.7845E-02 2.953860E-01-3.0900E-02',
     9     ' 2.327520E-01-1.8803E-02 1.899560E-01-1.2216E-02-1',
     *     ' 21  740.841586 1.860E-24 4.546E-03.0647.0689 2583',
     *     '.91970.740.000000  8  4             Q 56 464 2 2-1',
     *     ' 2 4.000620E-01-5.8157E-02 2.939950E-01-3.1054E-02',
     *     ' 2.310750E-01-1.8914E-02 1.881230E-01-1.2312E-02-1',
     1     ' 21  740.890529 2.699E-24 4.486E-03.0652.0700 2497',
     1     '.15990.750.000000  8  4             Q 54 464 2 2-1',
     1     ' 2 4.015180E-01-5.9059E-02 2.946580E-01-3.1538E-02',
     1     ' 2.312960E-01-1.9226E-02 1.880580E-01-1.2539E-02-1',
     2     ' 21  740.912271 8.983E-23 3.874E-03.0698.0926 1589',
     2     '.65670.780.000000  8  4             Q 25 465 2 2-1',
     2     ' 2 1.419340E-01-6.3337E-03 9.978019E-02-2.6316E-03',
     2     ' 7.574320E-02-1.1795E-03 5.998070E-02-5.0011E-04-1'/
      DATA CPL233/
     3     ' 21  740.939576 3.854E-24 4.428E-03.0656.0712 2413',
     3     '.51370.750.000000  8  4             Q 52 464 2 2-1',
     3     ' 2 4.044560E-01-6.0316E-02 2.964130E-01-3.2161E-02',
     3     ' 2.324010E-01-1.9591E-02 1.887470E-01-1.2777E-02-1',
     4     ' 21  740.988499 5.415E-24 4.372E-03.0660.0725 2332',
     4     '.98220.760.000000  8  4             Q 50 464 2 2-1',
     4     ' 2 4.081740E-01-6.1781E-02 2.986100E-01-3.2844E-02',
     4     ' 2.337920E-01-1.9963E-02 1.896570E-01-1.2998E-02-1',
     5     ' 21  741.034032 9.909E-23 3.847E-03.0704.0946 1551',
     5     '.28960.770.000000  8  4             Q 23 465 2 2-1',
     5     ' 2 1.477450E-01-6.3688E-03 1.030185E-01-2.3889E-03',
     5     ' 7.766330E-02-8.8566E-04 6.114160E-02-2.1625E-04-1',
     6     ' 21  741.037082 7.486E-24 4.319E-03.0663.0738 2255',
     6     '.56690.770.000000  8  4             Q 48 464 2 2-1',
     6     ' 2 4.127890E-01-6.3565E-02 3.013790E-01-3.3673E-02',
     6     ' 2.355860E-01-2.0407E-02 1.908660E-01-1.3257E-02-1'/
      DATA CPL237/
     7     ' 21  741.085120 1.018E-23 4.269E-03.0666.0751 2181',
     7     '.26900.770.000000  8  4             Q 46 464 2 2-1',
     7     ' 2 4.196530E-01-6.5924E-02 3.058770E-01-3.4785E-02',
     7     ' 2.387970E-01-2.1011E-02 1.932840E-01-1.3608E-02-1',
     8     ' 21  741.132424 1.362E-23 4.223E-03.0668.0765 2110',
     8     '.09010.780.000000  8  4             Q 44 465 2 2-1',
     8     ' 2 4.255680E-01-6.8056E-02 3.094130E-01-3.5681E-02',
     8     ' 2.410980E-01-2.1424E-02 1.948440E-01-1.3799E-02-1',
     9     ' 21  741.146061 1.069E-22 3.822E-03.0713.0966 1516',
     9     '.05220.760.000000  8  4             Q 21 465 2 2-1',
     9     ' 2 1.527890E-01-5.7918E-03 1.054547E-01-1.6892E-03',
     9     ' 7.880340E-02-2.3972E-04 6.156410E-02 3.4557E-04-1',
     *     ' 21  741.178812 1.791E-23 4.176E-03.0671.0780 2042',
     *     '.03140.780.000000  8  4             Q 42 465 2 2-1',
     *     ' 2 4.339270E-01-7.0730E-02 3.146650E-01-3.6796E-02',
     *     ' 2.446990E-01-2.1931E-02 1.974310E-01-1.4022E-02-1'/
      DATA CPL241/
     1     ' 21  741.224115 2.316E-23 4.133E-03.0673.0795 1977',
     1     '.09380.780.000000  8  4             Q 40 465 2 2-1',
     1     ' 2 4.461210E-01-7.2097E-02 3.229070E-01-3.6841E-02',
     1     ' 2.507570E-01-2.1549E-02 2.021370E-01-1.3497E-02-1',
     2     ' 21  741.248305 1.127E-22 3.800E-03.0722.0987 1483',
     2     '.94510.750.000000  8  4             Q 19 465 2 2-1',
     2     ' 2 1.571440E-01-4.3365E-03 1.070810E-01-3.4252E-04',
     2     ' 7.911800E-02 9.0000E-04 6.118710E-02 1.2946E-03-1',
     3     ' 21  741.268176 2.944E-23 4.092E-03.0675.0810 1915',
     3     '.27870.780.000000  8  4             Q 38 465 2 2-1',
     3     ' 2 4.545970E-01-7.7984E-02 3.278600E-01-4.0030E-02',
     3     ' 2.538770E-01-2.3560E-02 2.041650E-01-1.4886E-02-1',
     4     ' 21  741.310845 3.677E-23 4.052E-03.0677.0826 1856',
     4     '.58700.780.000000  8  4             Q 36 465 2 2-1',
     4     ' 2 4.675320E-01-7.9943E-02 3.361670E-01-4.0136E-02',
     4     ' 2.597010E-01-2.3071E-02 2.084680E-01-1.4200E-02-1'/
      DATA CPL245/
     5     ' 21  741.340716 1.157E-22 3.775E-03.0734.1009 1454',
     5     '.96880.730.000000  8  4             Q 17 465 2 2-1',
     5     ' 2 1.596400E-01-1.1603E-03 1.068340E-01 2.1979E-03',
     5     ' 7.764380E-02 2.9204E-03 5.913960E-02 2.9193E-03-1',
     6     ' 21  741.351984 4.512E-23 4.015E-03.0679.0843 1801',
     6     '.01980.780.000000  8  4             Q 34 465 2 2-1',
     6     ' 2 4.798430E-01-8.3291E-02 3.436290E-01-4.1105E-02',
     6     ' 2.646020E-01-2.3200E-02 2.118480E-01-1.3991E-02-1',
     7     ' 21  741.391463 5.437E-23 3.980E-03.0682.0860 1748',
     7     '.57800.780.000000  8  4             Q 32 465 2 2-1',
     7     ' 2 4.948060E-01-8.9475E-02 3.529890E-01-4.3724E-02',
     7     ' 2.709850E-01-2.4432E-02 2.164370E-01-1.4585E-02-1',
     8     ' 21  741.423250 1.155E-22 3.752E-03.0747.1032 1429',
     8     '.12370.700.000000  8  4             Q 15 465 2 2-1',
     8     ' 2 1.579370E-01 4.8221E-03 1.028391E-01 6.5901E-03',
     8     ' 7.280130E-02 6.2633E-03 5.404880E-02 5.5350E-03-1'/
      DATA CPL249/
     9     ' 21  741.429164 6.432E-23 3.948E-03.0686.0878 1699',
     9     '.26250.780.000000  8  4             Q 30 465 2 2-1',
     9     ' 2 5.094830E-01-9.3395E-02 3.616990E-01-4.4444E-02',
     9     ' 2.765750E-01-2.4094E-02 2.201680E-01-1.3875E-02-1',
     *     ' 21  741.464974 7.464E-23 3.917E-03.0690.0897 1653',
     *     '.07420.780.000000  8  4             Q 28 465 2 2-1',
     *     ' 2 5.241340E-01-9.2297E-02 3.699150E-01-4.1397E-02',
     *     ' 2.814760E-01-2.0804E-02 2.231580E-01-1.0783E-02-1',
     1     ' 21  741.495867 1.116E-22 3.727E-03.0762.1055 1406',
     1     '.41040.690.000000  8  4             Q 13 465 2 2-1',
     1     ' 2 1.512420E-01 1.8996E-02 9.427210E-02 1.6325E-02',
     1     ' 6.379230E-02 1.3426E-02 4.516720E-02 1.1037E-02-1',
     2     ' 21  741.498792 8.493E-23 3.888E-03.0695.0916 1610',
     2     '.01380.780.000000  8  4             Q 26 465 2 2-1',
     2     ' 2 5.449990E-01-9.9716E-02 3.826550E-01-4.3577E-02',
     2     ' 2.899520E-01-2.1169E-02 2.290990E-01-1.0446E-02-1'/
      DATA CPL253/
     3     ' 21  741.530521 9.468E-23 3.860E-03.0701.0935 1570',
     3     '.08220.780.000000  8  4             Q 24 465 2 2-1',
     3     ' 2 5.596890E-01-9.5359E-02 3.897140E-01-3.7356E-02',
     3     ' 2.932280E-01-1.5161E-02 2.302560E-01-5.0940E-03-1',
     4     ' 21  741.558535 1.038E-22 3.700E-03.0780.1079 1386',
     4     '.82920.690.000000  8  4             Q 11 465 2 2-1',
     4     ' 2 1.279564E-01 4.7939E-02 7.173140E-02 3.5139E-02',
     4     ' 4.264130E-02 2.6809E-02 2.554240E-02 2.1086E-02-1',
     5     ' 21  741.560077 1.033E-22 3.835E-03.0708.0956 1533',
     5     '.27980.770.000000  8  4             Q 22 465 2 2-1',
     5     ' 2 5.755100E-01-8.6311E-02 3.971760E-01-2.7262E-02',
     5     ' 2.965560E-01-6.0146E-03 2.313350E-01 2.8170E-03-1',
     6     ' 21  741.587380 1.101E-22 3.809E-03.0717.0977 1499',
     6     '.60740.760.000000  8  4             Q 20 465 2 2-1',
     6     ' 2 5.945030E-01-7.9882E-02 4.056260E-01-1.7976E-02',
     6     ' 2.998580E-01 2.8337E-03 2.318420E-01 1.0573E-02-1'/
      DATA CPL257/
     7     ' 21  741.611223 9.198E-23 3.663E-03.0800.1103 1370',
     7     '.38050.700.000000  8  4             Q  9 465 2 2-1',
     7     ' 2 7.440159E-02 1.1693E-01 2.420210E-02 7.8256E-02',
     7     ' 2.723240E-05 5.6749E-02-1.292473E-02 4.3246E-02-1',
     8     ' 21  741.612358 1.146E-22 3.787E-03.0728.0998 1469',
     8     '.06560.740.000000  8  4             Q 18 465 2 2-1',
     8     ' 2 5.982470E-01-3.8120E-02 4.012710E-01 1.5179E-02',
     8     ' 2.920450E-01 2.9179E-02 2.225470E-01 3.1781E-02-1',
     9     ' 21  741.634949 1.161E-22 3.765E-03.0740.1020 1441',
     9     '.65480.710.000000  8  4             Q 16 465 2 2-1',
     9     ' 2 6.028100E-01 6.7136E-03 3.958890E-01 5.1272E-02',
     9     ' 2.824380E-01 5.7804E-02 2.111460E-01 5.4666E-02-1',
     *     ' 21  741.653906 7.624E-23 3.605E-03.0823.1128 1357',
     *     '.06450.720.000000  8  4             Q  7 465 2 2-1',
     *     ' 2-6.491030E-02 2.9518E-01-9.238189E-02 1.8525E-01',
     *     '-1.009684E-01 1.2909E-01-1.021657E-01 9.5889E-02-1'/
      DATA CPL261/
     1     ' 21  741.655094 1.141E-22 3.741E-03.0754.1043 1417',
     1     '.37560.690.000000  8  4             Q 14 465 2 2-1',
     1     ' 2 6.053840E-01 1.3380E-01 3.871400E-01 1.4271E-01',
     1     ' 2.690220E-01 1.2683E-01 1.959230E-01 1.0852E-01-1',
     2     ' 21  741.672743 1.082E-22 3.713E-03.0771.1067 1396',
     2     '.22830.690.000000  8  4             Q 12 465 2 2-1',
     2     ' 2 5.354050E-01 3.6899E-01 3.185520E-01 2.9945E-01',
     2     ' 2.042820E-01 2.3969E-01 1.356290E-01 1.9374E-01-1',
     3     ' 21  741.686566 5.676E-23 3.483E-03.0849.1154 1346',
     3     '.88150.740.000000  8  4             Q  5 465 2 2-1',
     3     ' 2-4.931420E-01 7.9608E-01-4.370860E-01 4.7161E-01',
     3     '-3.923790E-01 3.1615E-01-3.557580E-01 2.2901E-01-1',
     4     ' 21  741.687854 9.841E-23 3.683E-03.0790.1091 1378',
     4     '.21330.700.000000  8  4             Q 10 465 2 2-1',
     4     ' 2 4.498130E-01 9.6794E-01 2.335320E-01 6.8916E-01',
     4     ' 1.235390E-01 5.1762E-01 6.027710E-02 4.0323E-01-1'/
      DATA CPL265/
     5     ' 21  741.700389 8.460E-23 3.639E-03.0811.1116 1363',
     5     '.33090.710.000000  8  4             Q  8 465 2 2-1',
     5     ' 2 1.593280E-01 2.3809E+00-1.838850E-02 1.5630E+00',
     5     '-9.934209E-02 1.1203E+00-1.392430E-01 8.4738E-01-1',
     6     ' 21  741.709186 3.334E-23 3.106E-03.0877.1204 1339',
     6     '.83150.750.000000  8  4             Q  3 465 2 2-1',
     6     ' 2-2.662660E+00-2.4121E+00-2.129400E+00-1.5701E+00',
     6     '-1.793870E+00-1.1281E+00-1.560000E+00-8.6066E-01-1',
     7     ' 21  741.710319 6.696E-23 3.557E-03.0836.1141 1351',
     7     '.58140.730.000000  8  4             Q  6 465 2 2-1',
     7     ' 2-5.914870E-01 6.1892E+00-6.393920E-01 3.8290E+00',
     7     '-6.333990E-01 2.6437E+00-6.089720E-01 1.9523E+00-1',
     8     ' 21  741.717619 4.562E-23 3.357E-03.0863.1190 1342',
     8     '.96480.740.000000  8  4             Q  4 465 2 2-1',
     8     ' 2-3.056690E+00 1.6987E+01-2.607930E+00 9.9081E+00',
     8     '-2.288910E+00 6.5692E+00-2.044900E+00 4.7254E+00-1'/
      DATA CPL269/
     9     ' 21  741.722273 1.926E-23 2.484E-03.0893.1228 1337',
     9     '.48160.750.000000  8  4             Q  2 465 2 2-1',
     9     ' 2-1.531530E+01-7.5414E+01-1.215279E+01-4.8229E+01',
     9     '-1.018316E+01-3.4242E+01-8.824659E+00-2.5918E+01-1',
     *     ' 21  791.452602 7.009E-24 6.612E-04.0893.1228 1287',
     *     '.75130.750.000000  8  3             Q  2 465 2 2-1',
     *     ' 2 5.116410E+00-8.6767E+00 4.055350E+00-5.5549E+00',
     *     ' 3.395340E+00-3.9463E+00 2.940600E+00-2.9880E+00-1',
     1     ' 21  791.464539 1.227E-23 6.604E-04.0863.1190 1293',
     1     '.21800.740.000000  8  3             Q  4 465 2 2-1',
     1     ' 2 7.640489E-01 3.0051E+00 6.799909E-01 1.7842E+00',
     1     ' 6.114030E-01 1.2000E+00 5.543720E-01 8.7278E-01-1',
     2     ' 21  791.483327 1.698E-23 6.597E-04.0836.1141 1301',
     2     '.80830.730.000000  8  3             Q  6 465 2 2-1',
     2     ' 2 7.558720E-02 9.1463E-01 1.247753E-01 5.7294E-01',
     2     ' 1.415440E-01 3.9916E-01 1.453790E-01 2.9676E-01-1'/
      DATA CPL273/
     3     ' 21  791.509002 2.093E-23 6.582E-04.0811.1116 1313',
     3     '.52230.710.000000  8  3             Q  8 465 2 2-1',
     3     ' 2-1.342250E-01 3.3225E-01-5.237830E-02 2.2157E-01',
     3     '-1.268878E-02 1.6046E-01 8.724430E-03 1.2226E-01-1',
     4     ' 21  791.541616 2.399E-23 6.564E-04.0790.1091 1328',
     4     '.35950.700.000000  8  3             Q 10 465 2 2-1',
     4     ' 2-2.155530E-01 1.3105E-01-1.253746E-01 9.5594E-02',
     4     '-7.851090E-02 7.2808E-02-5.085989E-02 5.7247E-02-1',
     5     ' 21  791.581236 2.609E-23 6.543E-04.0771.1067 1346',
     5     '.31980.690.000000  8  3             Q 12 465 2 2-1',
     5     ' 2-2.453100E-01 4.4574E-02-1.557400E-01 3.8916E-02',
     5     '-1.077180E-01 3.2197E-02-7.831070E-02 2.6541E-02-1',
     6     ' 21  791.627939 2.722E-23 6.519E-04.0754.1043 1367',
     6     '.40270.690.000000  8  3             Q 14 465 2 2-1',
     6     ' 2-2.538250E-01 1.2476E-02-1.678820E-01 1.6705E-02',
     6     '-1.208935E-01 1.5821E-02-9.148619E-02 1.3968E-02-1'/
      DATA CPL277/
     7     ' 21  791.681820 2.741E-23 6.489E-04.0740.1020 1391',
     7     '.60790.710.000000  8  3             Q 16 465 2 2-1',
     7     ' 2-2.518360E-01-4.4882E-03-1.706770E-01 4.1707E-03',
     7     '-1.257048E-01 6.2223E-03-9.713080E-02 6.4126E-03-1',
     8     ' 21  791.742983 2.678E-23 6.457E-04.0728.0998 1418',
     8     '.93490.740.000000  8  3             Q 18 465 2 2-1',
     8     ' 2-2.451150E-01-1.0301E-02-1.689610E-01-8.4595E-04',
     8     '-1.263184E-01 2.1109E-03-9.891439E-02 3.0579E-03-1',
     9     ' 21  791.811549 2.546E-23 6.423E-04.0717.0977 1449',
     9     '.38320.760.000000  8  3             Q 20 465 2 2-1',
     9     ' 2-2.365090E-01-1.4752E-02-1.651520E-01-4.8327E-03',
     9     '-1.248611E-01-1.2333E-03-9.872329E-02 2.7820E-04-1',
     *     ' 21  791.887652 2.359E-23 6.383E-04.0708.0956 1482',
     *     '.95210.770.000000  8  3             Q 22 465 2 2-1',
     *     ' 2-2.271490E-01-1.4947E-02-1.602640E-01-5.7658E-03',
     *     '-1.222247E-01-2.2577E-03-9.735440E-02-6.7427E-04-1'/
      DATA CPL281/
     1     ' 21  791.971438 2.135E-23 6.340E-04.0701.0935 1519',
     1     '.64120.780.000000  8  3             Q 24 465 2 2-1',
     1     ' 2-2.166710E-01-1.5411E-02-1.541540E-01-6.7707E-03',
     1     '-1.183741E-01-3.3024E-03-9.482979E-02-1.6362E-03-1',
     2     ' 21  792.063067 1.890E-23 6.296E-04.0695.0916 1559',
     2     '.44960.780.000000  8  3             Q 26 465 2 2-1',
     2     ' 2-2.068820E-01-1.5115E-02-1.482520E-01-7.1338E-03',
     2     '-1.145092E-01-3.8225E-03-9.217779E-02-2.1683E-03-1',
     3     ' 21  792.162713 1.637E-23 6.246E-04.0690.0897 1602',
     3     '.37650.780.000000  8  3             Q 28 465 2 2-1',
     3     ' 2-1.968070E-01-1.3721E-02-1.419080E-01-6.6511E-03',
     3     '-1.101698E-01-3.6722E-03-8.905130E-02-2.1575E-03-1',
     4     ' 21  792.270563 1.389E-23 6.193E-04.0686.0878 1648',
     4     '.42110.780.000000  8  3             Q 30 465 2 2-1',
     4     ' 2-1.869140E-01-1.3118E-02-1.354730E-01-6.6366E-03',
     4     '-1.056094E-01-3.8434E-03-8.564790E-02-2.3876E-03-1'/
      DATA CPL285/
     5     ' 21  792.386815 1.155E-23 6.137E-04.0682.0860 1697',
     5     '.58260.780.000000  8  3             Q 32 465 2 2-1',
     5     ' 2-1.777750E-01-1.2004E-02-1.295203E-01-6.2085E-03',
     5     '-1.013831E-01-3.6774E-03-8.249800E-02-2.3388E-03-1',
     6     ' 21  792.511682 9.417E-24 6.076E-04.0679.0843 1749',
     6     '.86010.780.000000  8  3             Q 34 465 2 2-1',
     6     ' 2-1.693900E-01-1.0817E-02-1.239550E-01-5.6627E-03',
     6     '-9.736350E-02-3.3930E-03-7.944690E-02-2.1819E-03-1',
     7     ' 21  792.645390 7.533E-24 6.012E-04.0677.0826 1805',
     7     '.25240.780.000000  8  3             Q 36 465 2 2-1',
     7     ' 2-1.608230E-01-9.8674E-03-1.181492E-01-5.2426E-03',
     7     '-9.309039E-02-3.1869E-03-7.614879E-02-2.0790E-03-1',
     8     ' 21  792.788177 5.915E-24 5.947E-04.0675.0810 1863',
     8     '.75870.780.000000  8  3             Q 38 465 2 2-1',
     8     ' 2-1.528540E-01-9.1400E-03-1.127022E-01-4.9452E-03',
     8     '-8.905649E-02-3.0592E-03-7.301710E-02-2.0309E-03-1'/
      DATA CPL289/
     9     ' 21  792.940292 4.559E-24 5.878E-04.0673.0795 1925',
     9     '.37770.780.000000  8  3             Q 40 465 2 2-1',
     9     ' 2-1.454440E-01-8.0413E-03-1.076049E-01-4.3536E-03',
     9     '-8.526310E-02-2.6924E-03-7.006220E-02-1.7846E-03-1',
     *     ' 21  793.102000 3.451E-24 5.806E-04.0671.0780 1990',
     *     '.10820.780.000000  8  3             Q 42 465 2 2-1',
     *     ' 2-1.381120E-01-7.4499E-03-1.024881E-01-4.0929E-03',
     *     '-8.140209E-02-2.5665E-03-6.702019E-02-1.7244E-03-1',
     1     ' 21  793.273576 2.565E-24 5.730E-04.0668.0765 2057',
     1     '.94900.780.000000  8  3             Q 44 465 2 2-1',
     1     ' 2-1.314430E-01-6.7694E-03-9.786400E-02-3.7518E-03',
     1     '-7.793630E-02-2.3709E-03-6.430840E-02-1.6045E-03-1',
     2     ' 21  793.455306 1.873E-24 5.652E-04.0666.0751 2128',
     2     '.89870.770.000000  8  3             Q 46 464 2 2-1',
     2     ' 2-1.251744E-01-6.1331E-03-9.345569E-02-3.4215E-03',
     2     '-7.460050E-02-2.1744E-03-6.168110E-02-1.4788E-03-1'/
      DATA CPL293/
     3     ' 21  793.647490 1.344E-24 5.572E-04.0663.0738 2202',
     3     '.95630.770.000000  8  3             Q 48 464 2 2-1',
     3     ' 2-1.187953E-01-5.5149E-03-8.897330E-02-3.0939E-03',
     3     '-7.121530E-02-1.9756E-03-5.902260E-02-1.3490E-03-1',
     4     ' 21  793.850440 9.474E-25 5.487E-04.0660.0725 2280',
     4     '.12010.760.000000  8  3             Q 50 464 2 2-1',
     4     ' 2-1.136564E-01-5.0120E-03-8.542559E-02-2.8270E-03',
     4     '-6.859450E-02-1.8134E-03-5.701540E-02-1.2429E-03-1',
     5     ' 21  794.064478 6.564E-25 5.401E-04.0656.0712 2360',
     5     '.38870.750.000000  8  3             Q 52 464 2 2-1',
     5     ' 2-1.083940E-01-4.5349E-03-8.181550E-02-2.5709E-03',
     5     '-6.595290E-02-1.6562E-03-5.501990E-02-1.1396E-03-1',
     6     ' 21  794.289939 4.470E-25 5.311E-04.0652.0700 2443',
     6     '.76050.750.000000  8  3             Q 54 464 2 2-1',
     6     ' 2-1.037816E-01-4.1081E-03-7.880600E-02-2.3418E-03',
     6     '-6.388590E-02-1.5162E-03-5.357170E-02-1.0480E-03-1'/
      DATA CPL297/
     7     ' 21  794.527167 2.992E-25 5.219E-04.0647.0689 2530',
     7     '.23410.740.000000  8  3             Q 56 464 2 2-1',
     7     ' 2-1.003418E-01-3.7461E-03-7.698080E-02-2.1558E-03',
     7     '-6.297980E-02-1.4091E-03-5.324540E-02-9.8286E-04-1',
     8     ' 21  794.776519 1.969E-25 5.125E-04.0643.0678 2619',
     8     '.80760.730.000000  8  3             Q 58 464 2 2-1',
     8     ' 2-1.003717E-01-3.5602E-03-7.855120E-02-2.1121E-03',
     8     '-6.530420E-02-1.4208E-03-5.595850E-02-1.0181E-03-1',
     9     ' 21  795.038363 1.274E-25 5.030E-04.0638.0667 2712',
     9     '.47950.720.000000  8  3             Q 60 464 2 2-1',
     9     ' 2-1.210092E-01-5.6705E-03-9.830080E-02-3.7712E-03',
     9     '-8.385520E-02-2.7622E-03-7.329530E-02-2.1217E-03-1',
     *     ' 21 1932.478947 3.209E-24 2.347E-07.0893.1228    2',
     *     '.34130.750.000000  6  1             Q  2 465 2 2-1',
     *     ' 2 0.254104E+01 0.0000E+00 0.203732E+01 0.0000E+00',
     *     ' 0.172035E+01 0.0000E+00 0.149406E+01 0.0000E+00-1'/
      DATA CPL301/
     1     ' 21 1932.499537 5.633E-24 2.351E-07.0863.1190    7',
     1     '.80430.740.000000  6  1             Q  4 465 2 2-1',
     1     ' 2 0.432113E+00 0.0000E+00 0.375123E+00 0.0000E+00',
     1     ' 0.331785E+00 0.0000E+00 0.298441E+00 0.0000E+00-1',
     2     ' 21 1932.531875 7.823E-24 2.356E-07.0836.1141   16',
     2     '.38900.730.000000  6  1             Q  6 465 2 2-1',
     2     ' 2 0.707673E-01 0.0000E+00 0.865256E-01 0.0000E+00',
     2     ' 0.894494E-01 0.0000E+00 0.889436E-01 0.0000E+00-1',
     3     ' 21 1932.575936 9.692E-24 2.363E-07.0811.1116   28',
     3     '.09510.710.000000  6  1             Q  8 465 2 2-1',
     3     ' 2-0.455414E-01 0.0000E+00-0.881456E-02 0.0000E+00',
     3     ' 0.823076E-02 0.0000E+00 0.179171E-01 0.0000E+00-1',
     4     ' 21 1932.631688 1.118E-23 2.371E-07.0790.1091   42',
     4     '.92250.700.000000  6  1             Q 10 465 2 2-1',
     4     ' 2-0.925533E-01 0.0000E+00-0.490540E-01 0.0000E+00',
     4     '-0.269095E-01 0.0000E+00-0.134153E-01 0.0000E+00-1'/
      DATA CPL305/
     5     ' 21 1932.699087 1.225E-23 2.381E-07.0771.1067   60',
     5     '.87090.690.000000  6  1             Q 12 465 2 2-1',
     5     ' 2-0.112996E+00 0.0000E+00-0.678782E-01 0.0000E+00',
     5     '-0.440151E-01 0.0000E+00-0.291239E-01 0.0000E+00-1',
     6     ' 21 1932.778085 1.290E-23 2.395E-07.0754.1043   81',
     6     '.94010.690.000000  6  1             Q 14 465 2 2-1',
     6     ' 2-0.121438E+00 0.0000E+00-0.768583E-01 0.0000E+00',
     6     '-0.527515E-01 0.0000E+00-0.375267E-01 0.0000E+00-1',
     7     ' 21 1932.868622 1.313E-23 2.409E-07.0740.1020  106',
     7     '.12970.710.000000  6  1             Q 16 465 2 2-1',
     7     ' 2-0.124067E+00 0.0000E+00-0.809305E-01 0.0000E+00',
     7     '-0.572472E-01 0.0000E+00-0.421816E-01 0.0000E+00-1',
     8     ' 21 1932.970629 1.298E-23 2.426E-07.0728.0998  133',
     8     '.43930.740.000000  6  1             Q 18 465 2 2-1',
     8     ' 2-0.123615E+00 0.0000E+00-0.823094E-01 0.0000E+00',
     8     '-0.593723E-01 0.0000E+00-0.447116E-01 0.0000E+00-1'/
      DATA CPL309/
     9     ' 21 1933.084030 1.250E-23 2.444E-07.0717.0977  163',
     9     '.86840.760.000000  6  1             Q 20 465 2 2-1',
     9     ' 2-0.121483E+00 0.0000E+00-0.821391E-01 0.0000E+00',
     9     '-0.600942E-01 0.0000E+00-0.459586E-01 0.0000E+00-1',
     *     ' 21 1933.208741 1.175E-23 2.464E-07.0708.0956  197',
     *     '.41660.770.000000  6  1             Q 22 465 2 2-1',
     *     ' 2-0.118391E+00 0.0000E+00-0.810422E-01 0.0000E+00',
     *     '-0.599595E-01 0.0000E+00-0.464107E-01 0.0000E+00-1',
     1     ' 21 1933.344666 1.080E-23 2.485E-07.0701.0935  234',
     1     '.08330.780.000000  6  1             Q 24 465 2 2-1',
     1     ' 2-0.114989E+00 0.0000E+00-0.795425E-01 0.0000E+00',
     1     '-0.594078E-01 0.0000E+00-0.464480E-01 0.0000E+00-1',
     2     ' 21 1933.491703 9.727E-24 2.511E-07.0695.0916  273',
     2     '.86800.780.000000  6  1             Q 26 465 2 2-1',
     2     ' 2-0.111364E+00 0.0000E+00-0.777423E-01 0.0000E+00',
     2     '-0.585419E-01 0.0000E+00-0.461729E-01 0.0000E+00-1'/
      DATA CPL313/
     3     ' 21 1933.649743 8.582E-24 2.537E-07.0690.0897  316',
     3     '.76980.780.000000  6  1             Q 28 465 2 2-1',
     3     ' 2-0.107716E+00 0.0000E+00-0.758167E-01 0.0000E+00',
     3     '-0.575157E-01 0.0000E+00-0.457211E-01 0.0000E+00-1',
     4     ' 21 1933.818665 7.426E-24 2.565E-07.0686.0878  362',
     4     '.78820.780.000000  6  1             Q 30 465 2 2-1',
     4     ' 2-0.104129E+00 0.0000E+00-0.738507E-01 0.0000E+00',
     4     '-0.564116E-01 0.0000E+00-0.451740E-01 0.0000E+00-1',
     5     ' 21 1933.998342 6.307E-24 2.596E-07.0682.0860  411',
     5     '.92250.780.000000  6  1             Q 32 465 2 2-1',
     5     ' 2-0.100662E+00 0.0000E+00-0.719055E-01 0.0000E+00',
     5     '-0.552880E-01 0.0000E+00-0.445865E-01 0.0000E+00-1',
     6     ' 21 1934.188638 5.258E-24 2.628E-07.0679.0843  464',
     6     '.17170.780.000000  6  1             Q 34 465 2 2-1',
     6     ' 2-0.973473E-01 0.0000E+00-0.700220E-01 0.0000E+00',
     6     '-0.541890E-01 0.0000E+00-0.440037E-01 0.0000E+00-1'/
      DATA CPL317/
     7     ' 21 1934.389410 4.307E-24 2.663E-07.0677.0826  519',
     7     '.53500.780.000000  6  1             Q 36 465 2 2-1',
     7     ' 2-0.942093E-01 0.0000E+00-0.682345E-01 0.0000E+00',
     7     '-0.531528E-01 0.0000E+00-0.434653E-01 0.0000E+00-1',
     8     ' 21 1934.600504 3.467E-24 2.700E-07.0675.0810  578',
     8     '.01160.780.000000  6  1             Q 38 465 2 2-1',
     8     ' 2-0.912641E-01 0.0000E+00-0.665777E-01 0.0000E+00',
     8     '-0.522237E-01 0.0000E+00-0.430215E-01 0.0000E+00-1',
     9     ' 21 1934.821762 2.743E-24 2.739E-07.0673.0795  639',
     9     '.60040.780.000000  6  1             Q 40 465 2 2-1',
     9     ' 2-0.885857E-01 0.0000E+00-0.651347E-01 0.0000E+00',
     9     '-0.514864E-01 0.0000E+00-0.427546E-01 0.0000E+00-1',
     *     ' 21 1935.053015 2.134E-24 2.781E-07.0671.0780  704',
     *     '.30050.780.000000  6  1             Q 42 465 2 2-1',
     *     ' 2-0.862324E-01 0.0000E+00-0.639988E-01 0.0000E+00',
     *     '-0.510490E-01 0.0000E+00-0.427794E-01 0.0000E+00-1'/
      DATA CPL321/
     1     ' 21 1935.294087 1.632E-24 2.824E-07.0668.0765  772',
     1     '.11070.780.000000  6  1             Q 44 465 2 2-1',
     1     ' 2-0.845166E-01 0.0000E+00-0.634903E-01 0.0000E+00',
     1     '-0.512219E-01 0.0000E+00-0.433892E-01 0.0000E+00-1',
     2     ' 21 1935.544794 1.228E-24 2.870E-07.0666.0751  843',
     2     '.03010.770.000000  6  1             Q 46 464 2 2-1',
     2     ' 2-0.841809E-01 0.0000E+00-0.643444E-01 0.0000E+00',
     2     '-0.527002E-01 0.0000E+00-0.452366E-01 0.0000E+00-1',
     3     ' 21 1935.804946 9.094E-25 2.919E-07.0663.0738  917',
     3     '.05730.770.000000  6  1             Q 48 464 2 2-1',
     3     ' 2-0.881561E-01 0.0000E+00-0.691832E-01 0.0000E+00',
     3     '-0.578241E-01 0.0000E+00-0.504333E-01 0.0000E+00-1',
     4     ' 21 1936.074343 6.623E-25 2.970E-07.0660.0725  994',
     4     '.19130.760.000000  6  1             Q 50 464 2 2-1',
     4     ' 2-0.114074E+00 0.0000E+00-0.927106E-01 0.0000E+00',
     4     '-0.792209E-01 0.0000E+00-0.700866E-01 0.0000E+00-1'/
      DATA CPL325/
     5     ' 21 2076.862561 3.891E-23 2.648E-06.0893.1228    2',
     5     '.34130.750.000000  8  1             Q  2 465 2 2-1',
     5     ' 2 0.334269E+01 0.0000E+00 0.267950E+01 0.0000E+00',
     5     ' 0.226224E+01 0.0000E+00 0.196437E+01 0.0000E+00-1',
     6     ' 21 2076.878177 6.819E-23 2.648E-06.0863.1190    7',
     6     '.80430.740.000000  8  1             Q  4 465 2 2-1',
     6     ' 2 0.560214E+00 0.0000E+00 0.486422E+00 0.0000E+00',
     6     ' 0.430180E+00 0.0000E+00 0.386871E+00 0.0000E+00-1',
     7     ' 21 2076.902725 9.445E-23 2.647E-06.0836.1141   16',
     7     '.38900.730.000000  8  1             Q  6 465 2 2-1',
     7     ' 2 0.841097E-01 0.0000E+00 0.106151E+00 0.0000E+00',
     7     ' 0.110859E+00 0.0000E+00 0.110819E+00 0.0000E+00-1',
     8     ' 21 2076.936219 1.166E-22 2.645E-06.0811.1116   28',
     8     '.09510.710.000000  8  1             Q  8 465 2 2-1',
     8     ' 2-0.686817E-01 0.0000E+00-0.191307E-01 0.0000E+00',
     8     ' 0.411843E-02 0.0000E+00 0.174645E-01 0.0000E+00-1'/
      DATA CPL329/
     9     ' 21 2076.978677 1.340E-22 2.645E-06.0790.1091   42',
     9     '.92250.700.000000  8  1             Q 10 465 2 2-1',
     9     ' 2-0.129922E+00 0.0000E+00-0.716192E-01 0.0000E+00',
     9     '-0.417552E-01 0.0000E+00-0.234692E-01 0.0000E+00-1',
     *     ' 21 2077.030120 1.461E-22 2.643E-06.0771.1067   60',
     *     '.87090.690.000000  8  1             Q 12 465 2 2-1',
     *     ' 2-0.156139E+00 0.0000E+00-0.958815E-01 0.0000E+00',
     *     '-0.638563E-01 0.0000E+00-0.437977E-01 0.0000E+00-1',
     1     ' 21 2077.090575 1.529E-22 2.641E-06.0754.1043   81',
     1     '.94010.690.000000  8  1             Q 14 465 2 2-1',
     1     ' 2-0.166581E+00 0.0000E+00-0.107198E+00 0.0000E+00',
     1     '-0.749498E-01 0.0000E+00-0.545200E-01 0.0000E+00-1',
     2     ' 21 2077.160074 1.546E-22 2.640E-06.0740.1020  106',
     2     '.12970.710.000000  8  1             Q 16 465 2 2-1',
     2     ' 2-0.169266E+00 0.0000E+00-0.111976E+00 0.0000E+00',
     2     '-0.803987E-01 0.0000E+00-0.602547E-01 0.0000E+00-1'/
      DATA CPL333/
     3     ' 21 2077.238654 1.517E-22 2.638E-06.0728.0998  133',
     3     '.43930.740.000000  8  1             Q 18 465 2 2-1',
     3     ' 2-0.167956E+00 0.0000E+00-0.113256E+00 0.0000E+00',
     3     '-0.827669E-01 0.0000E+00-0.632275E-01 0.0000E+00-1',
     4     ' 21 2077.326356 1.448E-22 2.635E-06.0717.0977  163',
     4     '.86840.760.000000  8  1             Q 20 465 2 2-1',
     4     ' 2-0.164481E+00 0.0000E+00-0.112535E+00 0.0000E+00',
     4     '-0.833220E-01 0.0000E+00-0.645424E-01 0.0000E+00-1',
     5     ' 21 2077.423226 1.349E-22 2.632E-06.0708.0956  197',
     5     '.41660.770.000000  8  1             Q 22 465 2 2-1',
     5     ' 2-0.159776E+00 0.0000E+00-0.110610E+00 0.0000E+00',
     5     '-0.827556E-01 0.0000E+00-0.648113E-01 0.0000E+00-1',
     6     ' 21 2077.529313 1.228E-22 2.630E-06.0701.0935  234',
     6     '.08330.780.000000  8  1             Q 24 465 2 2-1',
     6     ' 2-0.154573E+00 0.0000E+00-0.108093E+00 0.0000E+00',
     6     '-0.815936E-01 0.0000E+00-0.644975E-01 0.0000E+00-1'/
      DATA CPL337/
     7     ' 21 2077.644675 1.094E-22 2.628E-06.0695.0916  273',
     7     '.86800.780.000000  8  1             Q 26 465 2 2-1',
     7     ' 2-0.149164E+00 0.0000E+00-0.105241E+00 0.0000E+00',
     7     '-0.800623E-01 0.0000E+00-0.638047E-01 0.0000E+00-1',
     8     ' 21 2077.769370 9.536E-23 2.624E-06.0690.0897  316',
     8     '.76980.780.000000  8  1             Q 28 465 2 2-1',
     8     ' 2-0.143664E+00 0.0000E+00-0.102176E+00 0.0000E+00',
     8     '-0.782826E-01 0.0000E+00-0.628497E-01 0.0000E+00-1',
     9     ' 21 2077.903466 8.149E-23 2.620E-06.0686.0878  362',
     9     '.78820.780.000000  8  1             Q 30 465 2 2-1',
     9     ' 2-0.138271E+00 0.0000E+00-0.990751E-01 0.0000E+00',
     9     '-0.764094E-01 0.0000E+00-0.617713E-01 0.0000E+00-1',
     *     ' 21 2078.047031 6.829E-23 2.616E-06.0682.0860  411',
     *     '.92250.780.000000  8  1             Q 32 465 2 2-1',
     *     ' 2-0.133024E+00 0.0000E+00-0.959899E-01 0.0000E+00',
     *     '-0.745005E-01 0.0000E+00-0.606318E-01 0.0000E+00-1'/
      DATA CPL341/
     1     ' 21 2078.200140 5.615E-23 2.612E-06.0679.0843  464',
     1     '.17170.780.000000  8  1             Q 34 465 2 2-1',
     1     ' 2-0.127984E+00 0.0000E+00-0.929855E-01 0.0000E+00',
     1     '-0.726188E-01 0.0000E+00-0.594894E-01 0.0000E+00-1',
     2     ' 21 2078.362875 4.532E-23 2.608E-06.0677.0826  519',
     2     '.53500.780.000000  8  1             Q 36 465 2 2-1',
     2     ' 2-0.123161E+00 0.0000E+00-0.900912E-01 0.0000E+00',
     2     '-0.708025E-01 0.0000E+00-0.583877E-01 0.0000E+00-1',
     3     ' 21 2078.535319 3.591E-23 2.603E-06.0675.0810  578',
     3     '.01160.780.000000  8  1             Q 38 465 2 2-1',
     3     ' 2-0.118611E+00 0.0000E+00-0.873739E-01 0.0000E+00',
     3     '-0.691230E-01 0.0000E+00-0.574006E-01 0.0000E+00-1',
     4     ' 21 2078.717563 2.795E-23 2.598E-06.0673.0795  639',
     4     '.60040.780.000000  8  1             Q 40 465 2 2-1',
     4     ' 2-0.114363E+00 0.0000E+00-0.848875E-01 0.0000E+00',
     4     '-0.676455E-01 0.0000E+00-0.565965E-01 0.0000E+00-1'/
      DATA CPL345/
     5     ' 21 2078.909703 2.138E-23 2.593E-06.0671.0780  704',
     5     '.30050.780.000000  8  1             Q 42 465 2 2-1',
     5     ' 2-0.110540E+00 0.0000E+00-0.827799E-01 0.0000E+00',
     5     '-0.665247E-01 0.0000E+00-0.561305E-01 0.0000E+00-1',
     6     ' 21 2079.111838 1.607E-23 2.588E-06.0668.0765  772',
     6     '.11070.780.000000  8  1             Q 44 465 2 2-1',
     6     ' 2-0.107423E+00 0.0000E+00-0.813610E-01 0.0000E+00',
     6     '-0.660710E-01 0.0000E+00-0.563028E-01 0.0000E+00-1',
     7     ' 21 2079.324074 1.187E-23 2.582E-06.0666.0751  843',
     7     '.03010.770.000000  8  1             Q 46 464 2 2-1',
     7     ' 2-0.105927E+00 0.0000E+00-0.815156E-01 0.0000E+00',
     7     '-0.671114E-01 0.0000E+00-0.578824E-01 0.0000E+00-1',
     8     ' 21 2079.546522 8.621E-24 2.576E-06.0663.0738  917',
     8     '.05730.770.000000  8  1             Q 48 464 2 2-1',
     8     ' 2-0.109302E+00 0.0000E+00-0.861602E-01 0.0000E+00',
     8     '-0.722533E-01 0.0000E+00-0.632271E-01 0.0000E+00-1'/
      DATA CPL349/
     9     ' 21 2079.779298 6.156E-24 2.570E-06.0660.0725  994',
     9     '.19130.760.000000  8  1             Q 50 464 2 2-1',
     9     ' 2-0.137504E+00 0.0000E+00-0.111937E+00 0.0000E+00',
     9     '-0.957878E-01 0.0000E+00-0.849051E-01 0.0000E+00-1',
     *     ' 21 2093.346577 1.252E-24 2.167E-06.0893.1228  669',
     *     '.72750.750.000000 14  2             Q  2 465 2 2-1',
     *     ' 2 0.550811E+01 0.0000E+00 0.559421E+01 0.0000E+00',
     *     ' 0.565187E+01 0.0000E+00 0.569583E+01 0.0000E+00-1',
     1     ' 21 2093.350674 2.962E-24 2.925E-06.0863.1190  675',
     1     '.20500.740.000000 14  2             Q  4 465 2 2-1',
     1     ' 2 0.502300E+01 0.0000E+00 0.508130E+01 0.0000E+00',
     1     ' 0.512090E+01 0.0000E+00 0.515159E+01 0.0000E+00-1',
     2     ' 21 2093.355723 2.166E-24 2.709E-06.0877.1204  672',
     2     '.06760.750.000000 14  2             Q  3 465 2 2-1',
     2     ' 2 0.587007E+01 0.0000E+00 0.615352E+01 0.0000E+00',
     2     ' 0.633967E+01 0.0000E+00 0.647732E+01 0.0000E+00-1'/
      DATA CPL353/
     3     ' 21 2093.357109 4.342E-24 3.095E-06.0836.1141  683',
     3     '.81240.730.000000 14  2             Q  6 465 2 2-1',
     3     ' 2-0.318873E+01 0.0000E+00-0.314904E+01 0.0000E+00',
     3     '-0.312127E+01 0.0000E+00-0.310060E+01 0.0000E+00-1',
     4     ' 21 2093.365874 5.475E-24 3.160E-06.0811.1116  695',
     4     '.54960.710.000000 14  2             Q  8 465 2 2-1',
     4     ' 2 0.182957E+01 0.0000E+00 0.186720E+01 0.0000E+00',
     4     ' 0.189339E+01 0.0000E+00 0.191374E+01 0.0000E+00-1',
     5     ' 21 2093.372081 3.684E-24 3.034E-06.0849.1154  679',
     5     '.09900.740.000000 14  2             Q  5 465 2 2-1',
     5     ' 2-0.170774E+01 0.0000E+00-0.159666E+01 0.0000E+00',
     5     '-0.151831E+01 0.0000E+00-0.145872E+01 0.0000E+00-1',
     6     ' 21 2093.376963 6.354E-24 3.191E-06.0790.1091  710',
     6     '.41630.700.000000 14  2             Q 10 465 2 2-1',
     6     ' 2-0.192337E+00 0.0000E+00-0.164584E+00 0.0000E+00',
     6     '-0.144764E+00 0.0000E+00-0.129397E+00 0.0000E+00-1'/
      DATA CPL357/
     7     ' 21 2093.390364 6.968E-24 3.208E-06.0771.1067  728',
     7     '.41240.690.000000 14  2             Q 12 465 2 2-1',
     7     ' 2 0.134226E+01 0.0000E+00 0.137092E+01 0.0000E+00',
     7     ' 0.139108E+01 0.0000E+00 0.140688E+01 0.0000E+00-1',
     8     ' 21 2093.395717 4.940E-24 3.134E-06.0823.1128  689',
     8     '.25530.720.000000 14  2             Q  7 465 2 2-1',
     8     ' 2-0.228838E+01 0.0000E+00-0.228712E+01 0.0000E+00',
     8     '-0.227952E+01 0.0000E+00-0.227095E+01 0.0000E+00-1',
     9     ' 21 2093.406065 7.319E-24 3.219E-06.0754.1043  749',
     9     '.53750.690.000000 14  2             Q 14 465 2 2-1',
     9     ' 2 0.129775E+00 0.0000E+00 0.150342E+00 0.0000E+00',
     9     ' 0.165280E+00 0.0000E+00 0.177065E+00 0.0000E+00-1',
     *     ' 21 2093.424049 7.418E-24 3.226E-06.0740.1020  773',
     *     '.79120.710.000000 14  2             Q 16 465 2 2-1',
     *     ' 2 0.188039E+01 0.0000E+00 0.190981E+01 0.0000E+00',
     *     ' 0.193010E+01 0.0000E+00 0.194593E+01 0.0000E+00-1'/
      DATA CPL361/
     1     ' 21 2093.426641 5.949E-24 3.178E-06.0800.1103  702',
     1     '.53640.700.000000 14  2             Q  9 465 2 2-1',
     1     ' 2-0.278532E+01 0.0000E+00-0.295669E+01 0.0000E+00',
     1     '-0.306712E+01 0.0000E+00-0.314606E+01 0.0000E+00-1',
     2     ' 21 2093.444299 7.292E-24 3.231E-06.0728.0998  801',
     2     '.17320.740.000000 14  2             Q 18 465 2 2-1',
     2     ' 2 0.183964E+00 0.0000E+00 0.200116E+00 0.0000E+00',
     2     ' 0.211974E+00 0.0000E+00 0.221433E+00 0.0000E+00-1',
     3     ' 21 2093.464864 6.697E-24 3.201E-06.0780.1079  718',
     3     '.94200.690.000000 14  2             Q 11 465 2 2-1',
     3     ' 2 0.549655E+00 0.0000E+00 0.815931E+00 0.0000E+00',
     3     ' 0.101176E+01 0.0000E+00 0.116395E+01 0.0000E+00-1',
     4     ' 21 2093.466793 6.975E-24 3.235E-06.0717.0977  831',
     4     '.68290.760.000000 14  2             Q 20 465 2 2-1',
     4     ' 2-0.163992E+01 0.0000E+00-0.164404E+01 0.0000E+00',
     4     '-0.164500E+01 0.0000E+00-0.164507E+01 0.0000E+00-1'/
      DATA CPL365/
     5     ' 21 2093.491505 6.506E-24 3.238E-06.0708.0956  865',
     5     '.32000.770.000000 14  2             Q 22 465 2 2-1',
     5     ' 2 0.196033E+00 0.0000E+00 0.209554E+00 0.0000E+00',
     5     ' 0.219510E+00 0.0000E+00 0.227495E+00 0.0000E+00-1',
     6     ' 21 2093.510401 7.180E-24 3.214E-06.0762.1055  738',
     6     '.47180.690.000000 14  2             Q 13 465 2 2-1',
     6     ' 2-0.516206E+00 0.0000E+00-0.484283E+00 0.0000E+00',
     6     '-0.454408E+00 0.0000E+00-0.427736E+00 0.0000E+00-1',
     7     ' 21 2093.518407 5.929E-24 3.240E-06.0701.0935  902',
     7     '.08380.780.000000 14  2             Q 24 465 2 2-1',
     7     ' 2-0.252674E+00 0.0000E+00-0.246618E+00 0.0000E+00',
     7     '-0.241457E+00 0.0000E+00-0.237025E+00 0.0000E+00-1',
     8     ' 21 2093.547466 5.285E-24 3.241E-06.0695.0916  941',
     8     '.97370.780.000000 14  2             Q 26 465 2 2-1',
     8     ' 2 0.209494E+00 0.0000E+00 0.221770E+00 0.0000E+00',
     8     ' 0.230746E+00 0.0000E+00 0.237935E+00 0.0000E+00-1'/
      DATA CPL369/
     9     ' 21 2093.563268 7.405E-24 3.223E-06.0747.1032  761',
     9     '.12550.700.000000 14  2             Q 15 465 2 2-1',
     9     ' 2-0.520714E+00 0.0000E+00-0.537589E+00 0.0000E+00',
     9     '-0.545325E+00 0.0000E+00-0.548400E+00 0.0000E+00-1',
     *     ' 21 2093.578645 4.613E-24 3.242E-06.0690.0897  984',
     *     '.98900.780.000000 14  2             Q 28 465 2 2-1',
     *     ' 2-0.705079E-01 0.0000E+00-0.641853E-01 0.0000E+00',
     *     '-0.590465E-01 0.0000E+00-0.546902E-01 0.0000E+00-1',
     1     ' 21 2093.611903 3.946E-24 3.243E-06.0686.0878 1031',
     1     '.12920.780.000000 14  2             Q 30 465 2 2-1',
     1     ' 2 0.246471E+00 0.0000E+00 0.259078E+00 0.0000E+00',
     1     ' 0.268114E+00 0.0000E+00 0.275276E+00 0.0000E+00-1',
     2     ' 21 2093.623487 7.389E-24 3.229E-06.0734.1009  786',
     2     '.90280.730.000000 14  2             Q 17 465 2 2-1',
     2     ' 2-0.445895E+00 0.0000E+00-0.484620E+00 0.0000E+00',
     2     '-0.512173E+00 0.0000E+00-0.532538E+00 0.0000E+00-1'/
      DATA CPL373/
     3     ' 21 2093.647193 3.311E-24 3.245E-06.0682.0860 1080',
     3     '.39320.780.000000 14  2             Q 32 465 2 2-1',
     3     ' 2-0.134499E-01 0.0000E+00-0.751778E-02 0.0000E+00',
     3     '-0.277403E-02 0.0000E+00 0.123292E-02 0.0000E+00-1',
     4     ' 21 2093.684462 2.725E-24 3.245E-06.0679.0843 1132',
     4     '.78030.780.000000 14  2             Q 34 465 2 2-1',
     4     ' 2 0.366267E+00 0.0000E+00 0.382820E+00 0.0000E+00',
     4     ' 0.394258E+00 0.0000E+00 0.403101E+00 0.0000E+00-1',
     5     ' 21 2093.691080 7.164E-24 3.233E-06.0722.0987  815',
     5     '.80330.750.000000 14  2             Q 19 465 2 2-1',
     5     ' 2-0.376982E+00 0.0000E+00-0.431750E+00 0.0000E+00',
     5     '-0.476995E+00 0.0000E+00-0.514822E+00 0.0000E+00-1',
     6     ' 21 2093.723653 2.202E-24 3.246E-06.0677.0826 1188',
     6     '.28980.780.000000 14  2             Q 36 465 2 2-1',
     6     ' 2 0.896224E-02 0.0000E+00 0.145087E-01 0.0000E+00',
     6     ' 0.188912E-01 0.0000E+00 0.225827E-01 0.0000E+00-1'/
      DATA CPL377/
     7     ' 21 2093.764700 1.747E-24 3.247E-06.0675.0810 1246',
     7     '.92050.780.000000 14  2             Q 38 465 2 2-1',
     7     ' 2 0.147797E+01 0.0000E+00 0.153764E+01 0.0000E+00',
     7     ' 0.157661E+01 0.0000E+00 0.160539E+01 0.0000E+00-1',
     8     ' 21 2093.766074 6.767E-24 3.236E-06.0713.0966  847',
     8     '.82630.760.000000 14  2             Q 21 465 2 2-1',
     8     ' 2-0.406611E+00 0.0000E+00-0.548721E+00 0.0000E+00',
     8     '-0.687176E+00 0.0000E+00-0.818311E+00 0.0000E+00-1',
     9     ' 21 2093.807531 1.361E-24 3.246E-06.0673.0795 1308',
     9     '.67180.780.000000 14  2             Q 40 465 2 2-1',
     9     ' 2 0.177138E-01 0.0000E+00 0.229598E-01 0.0000E+00',
     9     ' 0.270597E-01 0.0000E+00 0.304998E-01 0.0000E+00-1',
     *     ' 21 2093.848496 6.239E-24 3.238E-06.0704.0946  882',
     *     '.97150.770.000000 14  2             Q 23 465 2 2-1',
     *     ' 2-0.185583E+00 0.0000E+00-0.170725E+00 0.0000E+00',
     *     '-0.148667E+00 0.0000E+00-0.122321E+00 0.0000E+00-1'/
      DATA CPL381/
     1     ' 21 2093.852068 1.042E-24 3.246E-06.0671.0780 1373',
     1     '.54210.780.000000 14  2             Q 42 465 2 2-1',
     1     ' 2-0.499541E+00 0.0000E+00-0.518065E+00 0.0000E+00',
     1     '-0.529316E+00 0.0000E+00-0.537056E+00 0.0000E+00-1',
     2     ' 21 2093.898222 7.843E-25 3.247E-06.0668.0765 1441',
     2     '.53080.780.000000 14  2             Q 44 465 2 2-1',
     2     ' 2 0.194289E-01 0.0000E+00 0.244160E-01 0.0000E+00',
     2     ' 0.282790E-01 0.0000E+00 0.315083E-01 0.0000E+00-1',
     3     ' 21 2093.938379 5.625E-24 3.241E-06.0698.0926  921',
     3     '.23820.780.000000 14  2             Q 25 465 2 2-1',
     3     ' 2-0.172010E+00 0.0000E+00-0.176786E+00 0.0000E+00',
     3     '-0.177242E+00 0.0000E+00-0.174493E+00 0.0000E+00-1',
     4     ' 21 2093.945898 5.801E-25 3.247E-06.0666.0751 1512',
     4     '.63660.770.000000 14  2             Q 46 464 2 2-1',
     4     ' 2-0.207907E+00 0.0000E+00-0.215524E+00 0.0000E+00',
     4     '-0.219870E+00 0.0000E+00-0.222637E+00 0.0000E+00-1'/
      DATA CPL385/
     5     ' 21 2093.994990 4.218E-25 3.247E-06.0663.0738 1586',
     5     '.85830.770.000000 14  2             Q 48 464 2 2-1',
     5     ' 2 0.165425E-01 0.0000E+00 0.212260E-01 0.0000E+00',
     5     ' 0.248374E-01 0.0000E+00 0.278500E-01 0.0000E+00-1',
     6     ' 21 2094.035758 4.963E-24 3.242E-06.0692.0906  962',
     6     '.62570.780.000000 14  2             Q 27 465 2 2-1',
     6     ' 2-0.148625E+00 0.0000E+00-0.155540E+00 0.0000E+00',
     6     '-0.159654E+00 0.0000E+00-0.161494E+00 0.0000E+00-1',
     7     ' 21 2094.045381 3.016E-25 3.247E-06.0660.0725 1664',
     7     '.19470.760.000000 14  2             Q 50 464 2 2-1',
     7     ' 2-0.147075E+00 0.0000E+00-0.153143E+00 0.0000E+00',
     7     '-0.156589E+00 0.0000E+00-0.158746E+00 0.0000E+00-1',
     8     ' 21 2094.096945 2.121E-25 3.248E-06.0656.0712 1744',
     8     '.64450.750.000000 14  2             Q 52 464 2 2-1',
     8     ' 2 0.102460E-01 0.0000E+00 0.144881E-01 0.0000E+00',
     8     ' 0.177658E-01 0.0000E+00 0.205038E-01 0.0000E+00-1'/
      DATA CPL389/
     9     ' 21 2094.140671 4.290E-24 3.243E-06.0687.0887 1007',
     9     '.13350.780.000000 14  2             Q 29 465 2 2-1',
     9     ' 2-0.128505E+00 0.0000E+00-0.134705E+00 0.0000E+00',
     9     '-0.138850E+00 0.0000E+00-0.141231E+00 0.0000E+00-1',
     *     ' 21 2094.149544 1.467E-25 3.248E-06.0652.0700 1828',
     *     '.20630.750.000000 14  2             Q 54 464 2 2-1',
     *     ' 2-0.147568E+00 0.0000E+00-0.155506E+00 0.0000E+00',
     *     '-0.160291E+00 0.0000E+00-0.163475E+00 0.0000E+00-1',
     1     ' 21 2094.203028 9.980E-26 3.248E-06.0647.0689 1914',
     1     '.87870.740.000000 14  2             Q 56 464 2 2-1',
     1     ' 2 0.168911E-02 0.0000E+00 0.528872E-02 0.0000E+00',
     1     ' 0.809817E-02 0.0000E+00 0.104587E-01 0.0000E+00-1',
     2     ' 21 2094.253158 3.635E-24 3.244E-06.0684.0869 1054',
     2     '.76070.780.000000 14  2             Q 31 465 2 2-1',
     2     ' 2-0.111961E+00 0.0000E+00-0.116345E+00 0.0000E+00',
     2     '-0.118343E+00 0.0000E+00-0.118054E+00 0.0000E+00-1'/
      DATA CPL393/
     3     ' 21 2094.257233 6.679E-26 3.248E-06.0643.0678 2004',
     3     '.66040.730.000000 14  2             Q 58 464 2 2-1',
     3     ' 2-0.296171E+00 0.0000E+00-0.319425E+00 0.0000E+00',
     3     '-0.334544E+00 0.0000E+00-0.345393E+00 0.0000E+00-1',
     4     ' 21 2094.311982 4.398E-26 3.248E-06.0638.0667 2097',
     4     '.54980.720.000000 14  2             Q 60 464 2 2-1',
     4     ' 2-0.803024E-02 0.0000E+00-0.531853E-02 0.0000E+00',
     4     '-0.316156E-02 0.0000E+00-0.133010E-02 0.0000E+00-1',
     5     ' 21 2094.367083 2.850E-26 3.249E-06.0633.0657 2193',
     5     '.54540.710.000000 14  2             Q 62 464 2 2-1',
     5     ' 2 0.152109E+00 0.0000E+00 0.171513E+00 0.0000E+00',
     5     ' 0.184870E+00 0.0000E+00 0.194945E+00 0.0000E+00-1',
     6     ' 21 2094.373264 3.022E-24 3.245E-06.0681.0852 1105',
     6     '.50660.780.000000 14  2             Q 33 465 2 2-1',
     6     ' 2-0.992314E-01 0.0000E+00-0.104757E+00 0.0000E+00',
     6     '-0.110366E+00 0.0000E+00-0.116433E+00 0.0000E+00-1'/
      DATA CPL397/
     7     ' 21 2094.422328 1.817E-26 3.249E-06.0628.0647 2292',
     7     '.64530.700.000000 14  2             Q 64 464 2 2-1',
     7     ' 2-0.189671E-01 0.0000E+00-0.175942E-01 0.0000E+00',
     7     '-0.164500E-01 0.0000E+00-0.154455E-01 0.0000E+00-1',
     8     ' 21 2094.477492 1.140E-26 3.249E-06.0623.0638 2394',
     8     '.84840.690.000000 14  2             Q 66 464 2 2-1',
     8     ' 2 0.218180E-01 0.0000E+00 0.278284E-01 0.0000E+00',
     8     ' 0.320475E-01 0.0000E+00 0.352860E-01 0.0000E+00-1',
     9     ' 21 2094.501037 2.465E-24 3.245E-06.0678.0835 1159',
     9     '.37040.780.000000 14  2             Q 35 465 2 2-1',
     9     ' 2-0.881065E-01 0.0000E+00-0.922439E-01 0.0000E+00',
     9     '-0.959486E-01 0.0000E+00-0.994810E-01 0.0000E+00-1',
     *     ' 21 2094.532335 7.038E-27 3.249E-06.0618.0629 2500',
     *     '.15230.680.000000 14  2             Q 68 464 2 2-1',
     *     ' 2-0.371104E-01 0.0000E+00-0.387906E-01 0.0000E+00',
     *     '-0.399199E-01 0.0000E+00-0.407098E-01 0.0000E+00-1'/
      DATA CPL401/
     1     ' 21 2094.586596 4.277E-27 3.249E-06.0613.0620 2608',
     1     '.55590.670.000000 14  2             Q 70 464 2 2-1',
     1     ' 2-0.200521E-01 0.0000E+00-0.198152E-01 0.0000E+00',
     1     '-0.196703E-01 0.0000E+00-0.195280E-01 0.0000E+00-1',
     2     ' 21 2094.636526 1.974E-24 3.245E-06.0676.0818 1216',
     2     '.35100.780.000000 14  2             Q 37 465 2 2-1',
     2     ' 2-0.789190E-01 0.0000E+00-0.823537E-01 0.0000E+00',
     2     '-0.853472E-01 0.0000E+00-0.881427E-01 0.0000E+00-1',
     3     ' 21 2094.779786 1.553E-24 3.246E-06.0674.0802 1276',
     3     '.44760.780.000000 14  2             Q 39 465 2 2-1',
     3     ' 2-0.711470E-01 0.0000E+00-0.740625E-01 0.0000E+00',
     3     '-0.765227E-01 0.0000E+00-0.787385E-01 0.0000E+00-1',
     4     ' 21 2094.930874 1.200E-24 3.245E-06.0672.0787 1339',
     4     '.65920.780.000000 14  2             Q 41 465 2 2-1',
     4     ' 2-0.645127E-01 0.0000E+00-0.670426E-01 0.0000E+00',
     4     '-0.691302E-01 0.0000E+00-0.709688E-01 0.0000E+00-1'/
      DATA CPL405/
     5     ' 21 2095.089850 9.117E-25 3.247E-06.0670.0772 1405',
     5     '.98470.780.000000 14  2             Q 43 465 2 2-1',
     5     ' 2-0.587818E-01 0.0000E+00-0.610124E-01 0.0000E+00',
     5     '-0.628169E-01 0.0000E+00-0.643749E-01 0.0000E+00-1',
     6     ' 21 2095.256781 6.806E-25 3.247E-06.0667.0758 1475',
     6     '.42310.770.000000 14  2             Q 45 465 2 2-1',
     6     ' 2-0.537867E-01 0.0000E+00-0.557801E-01 0.0000E+00',
     6     '-0.573626E-01 0.0000E+00-0.587029E-01 0.0000E+00-1',
     7     ' 21 2095.431732 4.994E-25 3.247E-06.0664.0744 1547',
     7     '.97310.770.000000 14  2             Q 47 464 2 2-1',
     7     ' 2-0.494031E-01 0.0000E+00-0.512056E-01 0.0000E+00',
     7     '-0.526105E-01 0.0000E+00-0.537775E-01 0.0000E+00-1',
     8     ' 21 2095.614777 3.603E-25 3.247E-06.0661.0731 1623',
     8     '.63350.760.000000 14  2             Q 49 464 2 2-1',
     8     ' 2-0.455338E-01 0.0000E+00-0.471802E-01 0.0000E+00',
     8     '-0.484409E-01 0.0000E+00-0.494671E-01 0.0000E+00-1'/
      DATA CPL409/
     9     ' 21 2095.805991 2.557E-25 3.248E-06.0658.0718 1702',
     9     '.40330.760.000000 14  2             Q 51 464 2 2-1',
     9     ' 2-0.420894E-01 0.0000E+00-0.436063E-01 0.0000E+00',
     9     '-0.447474E-01 0.0000E+00-0.456571E-01 0.0000E+00-1',
     *     ' 21 2096.005452 1.784E-25 3.247E-06.0654.0706 1784',
     *     '.28080.750.000000 14  2             Q 53 464 2 2-1',
     *     ' 2-0.390122E-01 0.0000E+00-0.404201E-01 0.0000E+00',
     *     '-0.414606E-01 0.0000E+00-0.422721E-01 0.0000E+00-1',
     1     ' 21 2096.213245 1.225E-25 3.249E-06.0650.0694 1869',
     1     '.26490.740.000000 14  2             Q 55 464 2 2-1',
     1     ' 2-0.362495E-01 0.0000E+00-0.375636E-01 0.0000E+00',
     1     '-0.385180E-01 0.0000E+00-0.392445E-01 0.0000E+00-1',
     2     ' 21 2096.429457 8.271E-26 3.248E-06.0645.0683 1957',
     2     '.35420.730.000000 14  2             Q 57 464 2 2-1',
     2     ' 2-0.337447E-01 0.0000E+00-0.349768E-01 0.0000E+00',
     2     '-0.358555E-01 0.0000E+00-0.365086E-01 0.0000E+00-1'/
      DATA CPL413/
     3     ' 21 2096.654179 5.495E-26 3.248E-06.0640.0672 2048',
     3     '.54710.730.000000 14  2             Q 59 464 2 2-1',
     3     ' 2-0.314797E-01 0.0000E+00-0.326384E-01 0.0000E+00',
     3     '-0.334496E-01 0.0000E+00-0.340370E-01 0.0000E+00-1',
     4     ' 21 2096.887508 3.592E-26 3.249E-06.0636.0662 2142',
     4     '.84200.720.000000 14  2             Q 61 464 2 2-1',
     4     ' 2-0.294183E-01 0.0000E+00-0.305101E-01 0.0000E+00',
     4     '-0.312607E-01 0.0000E+00-0.317897E-01 0.0000E+00-1',
     5     ' 21 2097.129543 2.310E-26 3.248E-06.0631.0652 2240',
     5     '.23750.710.000000 14  2             Q 63 464 2 2-1',
     5     ' 2-0.275364E-01 0.0000E+00-0.285685E-01 0.0000E+00',
     5     '-0.292663E-01 0.0000E+00-0.297456E-01 0.0000E+00-1',
     6     ' 21 2097.380390 1.462E-26 3.248E-06.0626.0642 2340',
     6     '.73220.700.000000 14  2             Q 65 464 2 2-1',
     6     ' 2-0.258387E-01 0.0000E+00-0.268271E-01 0.0000E+00',
     6     '-0.274876E-01 0.0000E+00-0.279328E-01 0.0000E+00-1'/
      DATA CPL417/
     7     ' 21 2097.640158 9.110E-27 3.249E-06.0620.0633 2444',
     7     '.32370.690.000000 14  2             Q 67 464 2 2-1',
     7     ' 2-0.244277E-01 0.0000E+00-0.254228E-01 0.0000E+00',
     7     '-0.260875E-01 0.0000E+00-0.265334E-01 0.0000E+00-1',
     8     ' 21 2097.908961 5.585E-27 3.249E-06.0615.0625 2551',
     8     '.01120.680.000000 14  2             Q 69 464 2 2-1',
     8     ' 2-0.244783E-01 0.0000E+00-0.256690E-01 0.0000E+00',
     8     '-0.264679E-01 0.0000E+00-0.270131E-01 0.0000E+00-1',
     9     ' 21 2128.359397 3.522E-25 3.731E-06.0660.0725 1664',
     9     '.19470.760.000000 15  2             Q 50 464 2 2-1',
     9     ' 2 0.335885E+00 0.0000E+00 0.274557E+00 0.0000E+00',
     9     ' 0.235617E+00 0.0000E+00 0.208927E+00 0.0000E+00-1',
     *     ' 21 2128.448849 4.890E-25 3.704E-06.0663.0738 1586',
     *     '.85830.770.000000 15  2             Q 48 464 2 2-1',
     *     ' 2 0.248574E+00 0.0000E+00 0.196359E+00 0.0000E+00',
     *     ' 0.165060E+00 0.0000E+00 0.144512E+00 0.0000E+00-1'/
      DATA CPL421/
     1     ' 21 2128.537756 6.680E-25 3.679E-06.0666.0751 1512',
     1     '.63660.770.000000 15  2             Q 46 464 2 2-1',
     1     ' 2 0.231097E+00 0.0000E+00 0.177494E+00 0.0000E+00',
     1     ' 0.146034E+00 0.0000E+00 0.125741E+00 0.0000E+00-1',
     2     ' 21 2128.625721 8.972E-25 3.654E-06.0668.0765 1441',
     2     '.53080.780.000000 15  2             Q 44 465 2 2-1',
     2     ' 2 0.227107E+00 0.0000E+00 0.171176E+00 0.0000E+00',
     2     ' 0.138545E+00 0.0000E+00 0.117611E+00 0.0000E+00-1',
     3     ' 21 2128.712372 1.185E-24 3.631E-06.0671.0780 1373',
     3     '.54210.780.000000 15  2             Q 42 465 2 2-1',
     3     ' 2 0.227604E+00 0.0000E+00 0.169293E+00 0.0000E+00',
     3     ' 0.135321E+00 0.0000E+00 0.113538E+00 0.0000E+00-1',
     4     ' 21 2128.797352 1.538E-24 3.608E-06.0673.0795 1308',
     4     '.67180.780.000000 15  2             Q 40 465 2 2-1',
     4     ' 2 0.229981E+00 0.0000E+00 0.169319E+00 0.0000E+00',
     4     ' 0.133996E+00 0.0000E+00 0.111320E+00 0.0000E+00-1'/
      DATA CPL425/
     5     ' 21 2128.880325 1.963E-24 3.588E-06.0675.0810 1246',
     5     '.92050.780.000000 15  2             Q 38 465 2 2-1',
     5     ' 2 0.233459E+00 0.0000E+00 0.170385E+00 0.0000E+00',
     5     ' 0.133690E+00 0.0000E+00 0.110095E+00 0.0000E+00-1',
     6     ' 21 2128.960972 2.461E-24 3.568E-06.0677.0826 1188',
     6     '.28980.780.000000 15  2             Q 36 465 2 2-1',
     6     ' 2 0.237800E+00 0.0000E+00 0.172171E+00 0.0000E+00',
     6     ' 0.134048E+00 0.0000E+00 0.109499E+00 0.0000E+00-1',
     7     ' 21 2129.038995 3.031E-24 3.549E-06.0679.0843 1132',
     7     '.78030.780.000000 15  2             Q 34 465 2 2-1',
     7     ' 2 0.242811E+00 0.0000E+00 0.174444E+00 0.0000E+00',
     7     ' 0.134820E+00 0.0000E+00 0.109278E+00 0.0000E+00-1',
     8     ' 21 2129.114112 3.665E-24 3.532E-06.0682.0860 1080',
     8     '.39320.780.000000 15  2             Q 32 465 2 2-1',
     8     ' 2 0.248359E+00 0.0000E+00 0.177042E+00 0.0000E+00',
     8     ' 0.135831E+00 0.0000E+00 0.109249E+00 0.0000E+00-1'/
      DATA CPL429/
     9     ' 21 2129.186058 4.350E-24 3.515E-06.0686.0878 1031',
     9     '.12920.780.000000 15  2             Q 30 465 2 2-1',
     9     ' 2 0.254408E+00 0.0000E+00 0.179889E+00 0.0000E+00',
     9     ' 0.136985E+00 0.0000E+00 0.109304E+00 0.0000E+00-1',
     *     ' 21 2129.254587 5.064E-24 3.499E-06.0690.0897  984',
     *     '.98900.780.000000 15  2             Q 28 465 2 2-1',
     *     ' 2 0.260963E+00 0.0000E+00 0.182948E+00 0.0000E+00',
     *     ' 0.138224E+00 0.0000E+00 0.109373E+00 0.0000E+00-1',
     1     ' 21 2129.319469 5.780E-24 3.485E-06.0695.0916  941',
     1     '.97370.780.000000 15  2             Q 26 465 2 2-1',
     1     ' 2 0.267769E+00 0.0000E+00 0.185984E+00 0.0000E+00',
     1     ' 0.139328E+00 0.0000E+00 0.109250E+00 0.0000E+00-1',
     2     ' 21 2129.380490 6.462E-24 3.471E-06.0701.0935  902',
     2     '.08380.780.000000 15  2             Q 24 465 2 2-1',
     2     ' 2 0.274695E+00 0.0000E+00 0.188839E+00 0.0000E+00',
     2     ' 0.140139E+00 0.0000E+00 0.108776E+00 0.0000E+00-1'/
      DATA CPL433/
     3     ' 21 2129.437455 7.071E-24 3.459E-06.0708.0956  865',
     3     '.32000.770.000000 15  2             Q 22 465 2 2-1',
     3     ' 2 0.281375E+00 0.0000E+00 0.191176E+00 0.0000E+00',
     3     ' 0.140343E+00 0.0000E+00 0.107656E+00 0.0000E+00-1',
     4     ' 21 2129.490183 7.563E-24 3.448E-06.0717.0977  831',
     4     '.68290.760.000000 15  2             Q 20 465 2 2-1',
     4     ' 2 0.287340E+00 0.0000E+00 0.192559E+00 0.0000E+00',
     4     ' 0.139544E+00 0.0000E+00 0.105522E+00 0.0000E+00-1',
     5     ' 21 2129.538510 7.893E-24 3.438E-06.0728.0998  801',
     5     '.17320.740.000000 15  2             Q 18 465 2 2-1',
     5     ' 2 0.291524E+00 0.0000E+00 0.192077E+00 0.0000E+00',
     5     ' 0.136947E+00 0.0000E+00 0.101670E+00 0.0000E+00-1',
     6     ' 21 2129.582286 8.021E-24 3.429E-06.0740.1020  773',
     6     '.79120.710.000000 15  2             Q 16 465 2 2-1',
     6     ' 2 0.292180E+00 0.0000E+00 0.188251E+00 0.0000E+00',
     6     ' 0.131264E+00 0.0000E+00 0.949415E-01 0.0000E+00-1'/
      DATA CPL437/
     7     ' 21 2129.621381 7.912E-24 3.421E-06.0754.1043  749',
     7     '.53750.690.000000 15  2             Q 14 465 2 2-1',
     7     ' 2 0.286180E+00 0.0000E+00 0.178505E+00 0.0000E+00',
     7     ' 0.120305E+00 0.0000E+00 0.834352E-01 0.0000E+00-1',
     8     ' 21 2129.655676 7.543E-24 3.414E-06.0771.1067  728',
     8     '.41240.690.000000 15  2             Q 12 465 2 2-1',
     8     ' 2 0.267032E+00 0.0000E+00 0.157596E+00 0.0000E+00',
     8     ' 0.996385E-01 0.0000E+00 0.632776E-01 0.0000E+00-1',
     9     ' 21 2129.685071 6.903E-24 3.408E-06.0790.1091  710',
     9     '.41630.700.000000 15  2             Q 10 465 2 2-1',
     9     ' 2 0.220923E+00 0.0000E+00 0.114526E+00 0.0000E+00',
     9     ' 0.600854E-01 0.0000E+00 0.265908E-01 0.0000E+00-1',
     *     ' 21 2129.709477 5.999E-24 3.403E-06.0811.1116  695',
     *     '.54960.710.000000 15  2             Q  8 465 2 2-1',
     *     ' 2 0.114451E+00 0.0000E+00 0.230035E-01 0.0000E+00',
     *     '-0.201768E-01 0.0000E+00-0.453158E-01 0.0000E+00-1'/
      DATA CPL441/
     1     ' 21 2129.728825 4.851E-24 3.399E-06.0836.1141  683',
     1     '.81240.730.000000 15  2             Q  6 465 2 2-1',
     1     ' 2-0.151953E+00 0.0000E+00-0.194589E+00 0.0000E+00',
     1     '-0.205129E+00 0.0000E+00-0.206808E+00 0.0000E+00-1',
     2     ' 21 2129.743058 3.499E-24 3.396E-06.0863.1190  675',
     2     '.20500.740.000000 15  2             Q  4 465 2 2-1',
     2     ' 2-0.989306E+00 0.0000E+00-0.856316E+00 0.0000E+00',
     2     '-0.755677E+00 0.0000E+00-0.678356E+00 0.0000E+00-1',
     3     ' 21 2129.752135 1.995E-24 3.394E-06.0893.1228  669',
     3     '.72750.750.000000 15  2             Q  2 465 2 2-1',
     3     ' 2-0.595900E+01 0.0000E+00-0.470076E+01 0.0000E+00',
     3     '-0.390828E+01 0.0000E+00-0.333855E+01 0.0000E+00-1'/
C                                                                        LN33830
      END                                                                LN33840
      SUBROUTINE CPSTOR_160 (molcpl,ncpl)   
C                                                                        LN18680
C     THIS SUBROUTINE WRITES OUT THE LINES AND THEIR COUPLING            LN18690
C     COEFFICIENTS TO TAPE2 FOR USE BY LNFL                              LN18700
C                                                                        LN18710
      COMMON /CLINES_160/ CPLINS(886)                                        LN18720
      CHARACTER CPLINS*100, HQ*7                                         LN18730
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN18740
      COMMON /CPLMOL_160/ MOLCPL_160(38),NCPL_160

      dimension molcpl(38)

      CHARACTER CUP*9, CLO*9
C                                                                        LN18760
c      set up molcpl and ncpl based on particular line coupling file used
c
      ncpl =ncpl_160

      do m=1,38
         molcpl(m) = molcpl_160(m)
      enddo

c______
c
c     -zero frequency oxygen line is not modified  based on
c       Cimini et al 2003 (kcp and sac)

      i=1

         READ (CPLINS(I),920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
         WRITE (2,920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
c 
         READ (CPLINS(I+1),925) ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG

         WRITE (2,925)          ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C
c     modify the line coupling for the oxygen lines
C
      DO 10 I = 3,82,2                                                   LN18770
c
         READ (CPLINS(I),920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
         WRITE (2,920) ISO,VNU,STR,TRANS,HWHM,HWHMS,ENERGY,
     *                         TDEP,SHIFT,IVUP,IVLO,CUP,CLO,HQ,IFLAG 
C 
         READ (CPLINS(I+1),925) ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C**********************
c
C     These coefficients have been updated to provide consistency with
C     HITRAN96 oxygen line parameters (June 1999).
c
c     -modify the oxygen line coupling coefficients based on the data of 
c      Cimini et al. 2003 (kcp and sac)
c
         data y_fac/0.87/, g_fac/0./
c
         y1 = y_fac*y1
         y2 = y_fac*y2
         y3 = y_fac*y3
         y4 = y_fac*y4
c
         g1 = g_fac*g1
         g2 = g_fac*g2
         g3 = g_fac*g3
         g4 = g_fac*g4
c
         WRITE (2,925)          ISO,Y1,G1,Y2,G2,Y3,G3,Y4,G4,IFLAG
C
 10   CONTINUE                                                           LN18790
C                                                                        LN18760
C   REMEMBER TO CHECK AND FIX SHIFT = 0. PROBLEM
C                                                                        LN11550
      DO 40 I = 83,NCPL                                                  LN18770
         WRITE (2,900) CPLINS(I)                                         LN18780
   40 CONTINUE                                                           LN18790
C                                                                        LN18800
      REWIND 2                                                           LN18810
C                                                                        LN18820
      RETURN                                                             LN18830
C                                                                        LN18840
  900 FORMAT (A100)                                                      LN18850
  920 FORMAT (I3,F12.6,1P,2E10.3,0P,2F5.4,F10.4,F4.2,F8.6,2I3,2A9,A7,I2) LN11470
  925 FORMAT (I2,1P,4(E13.6,E11.4),0P,I2)                                LN11480
C                                                                        LN18860
      END                                                                LN18870
      BLOCK DATA CPLINS_160                                                  LN19940
C                                                                        LN19950
C**********************************************************************  LN19960
C                                                                        LN19970
C     THIS BLOCK DATA CONTAINS TRANSITION DATA INCLUDING LINE COUPLING   LN19980
C     PARAMETERS FOR REPLACEMENT OF SELECTED TRANSITIONS ON THE HITRAN   LN19990
C     DATABASE AFFECTED BY LINE COUPLING.                                LN20000
C                                                                        LN20010
C     THE FORMULATION IS DESCRIBED IN HOKE ET AL, 1988: PROC. OF THE     LN20020
C     INTERNATIONAL RADIATION  SYMPOSIUM, J. LENOBLE AND J.F. GELEYN,    LN20030
C     ED., A. DEEPAK PUB., 368-371.                                      LN20040
C                                                                        LN20050
C     THE LINE COUPLING DATA GENERALLY FOLLOWS THE DEVELOPMENT OF SMITH, LN20060
C            S' = S * ( 1. + G * (P/P0)**2 )           P0=1013 MB        LN20070
C            S''= S * (      Y * (P/P0)  )                               LN20080
C     VALUES FOR Y AND G ARE PROVIDED AT FOUR TEMPERATURES:              LN20090
C     200 K, 250 K, 296 K, AND 340 K                                     LN20100
C                                                                        LN20110
C**********************************************************************  LN20120
C                                                                        LN20130
C                    OXYGEN                                              LN20140
C                                                                        LN20150
C     A TRANSITION AT 0.000010 CM-1 HAS BEEN PROVIDED IN ORDER TO        LN20160
C     TREAT THE 'ZERO FREQUENCY' (NON-RESONANT) O2 BAND AS A SINGLE      LN20170
C     LINE WITH A PRESSURE DEPENDENT REFERENCE HALFWIDTH, REPLACING      LN20180
C     THE 'ZERO FREQUENCY' TRANSITIONS ON THE HITRAN DATA BASE FOR       LN20190
C     THE MAIN ISOTOPE.                                                  LN20200
C                                                                        LN20210
C     THE COUPLING COEFFICIENTS FOR THE 2 CM-1 (60 GHZ) FOR THE MAIN     LN20220
C     ISOTOPE ARE INCLUDED. THESE HAVE BEEN CALCULATED BY CLOUGH AND     LN20230
C     HOKE.                                                              LN20240
C                                                                        LN20250
C            THE O2 LINE COUPLING COEFFICIENTS WERE PROVIDED BY          LN20260
C            CLOUGH (1987). Y'S AND G'S WERE CALCULATED FOR OXYGEN       LN20270
C            USING A RELAXATION MATRIX WHICH WAS FIT TO SMITH'S          LN20280
C            HALFWIDTHS.                                                 LN20290
C                                                                        LN20300
C              NT        TEMP         X       A1            A2           LN20310
C                                                                        LN20320
C               1     200.0000     .7500  7.328597E-03  7.303265E-01     LN20330
C               2     250.0000     .7500  5.467282E-03  7.535262E-01     LN20340
C               3     296.0000     .7500  4.371108E-03  7.712720E-01     LN20350
C               4     340.0000     .7500  3.632896E-03  7.855163E-01     LN20360
C                                                                        LN20370
C         ALPHA(T) = ALPHA(TO)*(TO/T)**X                                 LN20380
C                                                                        LN20390
C         W(J,K) = A1*SQRT(RHO(J)/RHO(K))*EXP(-A2*BETA*ABS(E(J)-E(K)))   LN20400
C
C    These coefficients have been updated to provide consistency with
C    HITRAN96 oxygen line parameters (June 1999).
C                                                                        LN20410
C**********************************************************************  LN20600
C                                                                        LN20610
C                   CARBON DIOXIDE                                       LN20620
C                                                                        LN20630
C     LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES AT        LN20640
C     618 CM-1, 667 CM-1, 720 CM-1, 721 CM-1, 742 CM-1 AND 792 CM-1.     LN20650
C     (for additional Q-branches see below)                              LN20660
C   ...................................................................  LN20670
C   .                                                                 .  LN20680
C   .   THE LINE COUPLING COEFFICIENTS FOR CARBON DIOXIDE HAVE BEEN   .  LN20690
C   .   MULTIPLIED BY A FACTOR OF 1.3 TO PROVIDE AGREEMENT WITH THE   .  LN20700
C   .   THE HIS DOWNLOOKING DATA OF 14 APRIL 1986 AND WITH THE HIS    .  LN20710
C   .   UPLOOKING GAPEX DATA OF 1 NOV. 1988. (CLOUGH ET AL 1991).     .  LN20720
C   .                                                                 .  LN20730
C   ...................................................................  LN20740
C                                                                        LN20750
C                                                                        LN20760
C     WITH THE EXCEPTION OF THE TRANSITION WAVENUMBER VALUES,
C     THE CO2 PARAMETERS, INCLUDING THE LINE COUPLING COEFFICIENTS,      LN20770
C     ARE FROM THE 1996 HITRAN LINE PARAMETERS. THE WAVENUMBER VALUES
C     ARE FROM HITRAN96.                                                 LN20800
C                                                                        LN20810
C     COUPLING COEFFICIENTS ARE FROM M.L. HOKE (1991).                   LN20820
C                                                                        LN20830
C    These coefficients have been updated to provide consistency with
C    HITRAN96 carbon dioxide line parameters (June 1999).
C
C    
C    ADDITIONAL LINE COUPLING PARAMETERS ARE PROVIDED FOR CO2 Q BRANCHES
C    AT 1932 cm-1, 2076 cm-1, 2093 cm-1, 2193 cm-1 (April 2001)
C     - coupling coefficients for these branches are from 
C       Strow et al. 1994
C**********************************************************************  LN20840
C                                                                        LN20850
      IMPLICIT CHARACTER*50 (C)                                          LN20860
      COMMON /CPLMOL_160/ MOLCPL_160(38),NCPL_160
      COMMON /CLINES_160/ 
     +                CPL001(16),CPL005(16),CPL009(16),CPL013(16),       LN20880
     +     CPL017(16),CPL021(16),CPL025(16),CPL029(16),CPL033(16),       LN20890
     +     CPL037(16),CPL041(16),CPL045(16),CPL049(16),CPL053(16),       LN20900
     +     CPL057(16),CPL061(16),CPL065(16),CPL069(16),CPL073(16),       LN20910
     +     CPL077(16),CPL081(16),CPL085(16),CPL089(16),CPL093(16),       LN20920
     +     CPL097(16),CPL101(16),CPL105(16),CPL109(16),CPL113(16),       LN20930
     +     CPL117(16),CPL121(16),CPL125(16),CPL129(16),CPL133(16),       LN20940
     +     CPL137(16),CPL141(16),CPL145(16),CPL149(16),CPL153(16),       LN20950
     +     CPL157(16),CPL161(16),CPL165(16),CPL169(16),CPL173(16),       LN20960
     +     CPL177(16),CPL181(16),CPL185(16),CPL189(16),CPL193(16),       LN20970
     +     CPL197(16),CPL201(16),CPL205(16),CPL209(16),CPL213(16),       LN20980
     +     CPL217(16),CPL221(16),CPL225(16),CPL229(16),CPL233(16),       LN20990
     +     CPL237(16),CPL241(16),CPL245(16),CPL249(16),CPL253(16),       LN21000
     +     CPL257(16),CPL261(16),CPL265(16),CPL269(16),CPL273(16),       LN21010
     +     CPL277(16),CPL281(16),CPL285(16),CPL289(16),CPL293(16),       LN21020
     +     CPL297(16),CPL301(16),CPL305(16),CPL309(16),CPL313(16),       LN21030
     +     CPL317(16),CPL321(16),CPL325(16),CPL329(16),CPL333(16), 
     +     CPL337(16),CPL341(16),CPL345(16),CPL349(16),CPL353(16), 
     +     CPL357(16),CPL361(16),CPL365(16),CPL369(16),CPL373(16), 
     +     CPL377(16),CPL381(16),CPL385(16),CPL389(16),CPL393(16), 
     +     CPL397(16),CPL401(16),CPL405(16),CPL409(16),CPL413(16), 
     +     CPL417(16),CPL421(16),CPL425(16),CPL429(16),CPL433(16), 
     +     CPL437(16),CPL441(12) 
C                                                                        LN21040
C     MOLCPL CONTAINS THE FLAGS FOR MOLECULE 2 AND 7                     LN21050
C                                                                        LN21060
      DATA MOLCPL_160/0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,                         LN21070
     1                0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,                         LN21080
     2                0,0,0,0,0,0,0,0/                                       LN21090
C                                                                        LN21100

C     Previously NCPL was 598 ([([(293-1)/4]+1) * 16] + 12) / 2 = 598
C     Adding Line coupling changed it to 886:
C     ([([(437-1)/4]+1) * 16] + 12) / 2 = 886
C     Note: need to also change CPLINS  LN18720
C
      DATA NCPL_160/886/
C
      DATA CPL001/
     1     ' 71    0.000010 2.401E-35 0.000E+00.0340.0000   -1',
     1     '.00000.000.000000  0  0 O2 BAND<.007 CM-1000 0 0-3',
     1     ' 7 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00',
     1     ' 5.000000E-01 0.0000E+00 5.000000E-01 0.0000E+00-3',
     2     ' 71    1.666572 1.370E-29 3.521E-04.0320.0314 2230',
     2     '.42240.71 0.00000  1  1         Q39R38  d404 3 1-1',
     2     ' 7 1.083685E+00-3.5147E-01 8.378760E-01-2.0751E-01',
     2     ' 6.860124E-01-1.3722E-01 5.810179E-01-9.7055E-02-1',
     3     ' 71    1.683641 3.851E-29 3.525E-04.0320.0314 2011',
     3     '.21190.71 0.00000  1  1         Q37R36  d404 3 1-1',
     3     ' 7 1.095630E+00-3.6449E-01 8.356707E-01-2.1345E-01',
     3     ' 6.750125E-01-1.4019E-01 5.640647E-01-9.8456E-02-1',
     4     ' 71    1.700770 1.023E-28 3.532E-04.0320.0314 1803',
     4     '.17650.71 0.00000  1  1         Q35R34  d404 3 1-1',
     4     ' 7 1.115808E+00-3.7857E-01 8.450911E-01-2.2006E-01',
     4     ' 6.781475E-01-1.4372E-01 5.629458E-01-1.0045E-01-1'/
      DATA CPL005/
     5     ' 71    1.717968 2.563E-28 3.536E-04.0320.0314 1606',
     5     '.35000.71 0.00000  1  1         Q33R32  d404 3 1-1',
     5     ' 7 1.132280E+00-3.9224E-01 8.518667E-01-2.2596E-01',
     5     ' 6.797590E-01-1.4650E-01 5.613293E-01-1.0171E-01-1',
     6     ' 71    1.735249 6.058E-28 3.542E-04.0320.0314 1420',
     6     '.76420.71 0.00000  1  1         Q31R30  d404 3 1-1',
     6     ' 7 1.145445E+00-4.0537E-01 8.556679E-01-2.3106E-01',
     6     ' 6.788533E-01-1.4844E-01 5.576693E-01-1.0220E-01-1',
     7     ' 71    1.752627 1.350E-27 3.549E-04.0320.0314 1246',
     7     '.44930.71 0.00000  1  1         Q29R28  d404 3 1-1',
     7     ' 7 1.153709E+00-4.1654E-01 8.550279E-01-2.3434E-01',
     7     ' 6.739851E-01-1.4884E-01 5.504953E-01-1.0138E-01-1',
     8     ' 71    1.770123 2.836E-27 3.559E-04.0320.0314 1083',
     8     '.43350.71 0.00000  1  1         Q27R26  d404 3 1-1',
     8     ' 7 1.152147E+00-4.2378E-01 8.461281E-01-2.3449E-01',
     8     ' 6.620204E-01-1.4677E-01 5.371368E-01-9.8547E-02-1'/
      DATA CPL009/
     9     ' 71    1.787763 5.605E-27 3.568E-04.0320.0314  931',
     9     '.74330.71 0.00000  1  1         Q25R24  d404 3 1-1',
     9     ' 7 1.142295E+00-4.2478E-01 8.301889E-01-2.3010E-01',
     9     ' 6.439650E-01-1.4127E-01 5.184343E-01-9.3064E-02-1',
     *     ' 71    1.805583 1.042E-26 3.580E-04.0380.0373  791',
     *     '.40320.71 0.00000  1  1         Q23R22  d404 3 1-1',
     *     ' 7 1.118166E+00-4.1498E-01 8.026411E-01-2.1854E-01',
     *     ' 6.161239E-01-1.3066E-01 4.912944E-01-8.3784E-02-1',
     1     ' 71    1.823634 1.820E-26 3.595E-04.0350.0343  662',
     1     '.43590.71 0.00000  1  1         Q21R20  d404 3 1-1',
     1     ' 7 1.071800E+00-3.8937E-01 7.577658E-01-1.9716E-01',
     1     ' 5.741029E-01-1.1339E-01 4.522096E-01-6.9746E-02-1',
     2     ' 71    1.841987 2.981E-26 3.611E-04.0370.0363  544',
     2     '.86220.71 0.00000  1  1         Q19R18  d404 3 1-1',
     2     ' 7 1.003603E+00-3.4386E-01 6.961964E-01-1.6409E-01',
     2     ' 5.185847E-01-8.8501E-02 4.018506E-01-5.0455E-02-1'/
      DATA CPL013/
     3     ' 71    1.860747 4.572E-26 3.632E-04.0380.0373  438',
     3     '.70100.71 0.00000  1  1         Q17R16  d404 3 1-1',
     3     ' 7 9.018632E-01-2.7140E-01 6.093928E-01-1.1616E-01',
     3     ' 4.428841E-01-5.4466E-02 3.347702E-01-2.5202E-02-1',
     4     ' 71    1.876791 2.726E-26 1.683E-04.0450.0441    2',
     4     '.08430.71 0.00000  1  1         Q 1P 2  d404 3 1-1',
     4     ' 7 9.550456E-01-3.9580E-01 8.042110E-01-2.8801E-01',
     4     ' 7.025955E-01-2.2387E-01 6.269926E-01-1.8088E-01-1',
     5     ' 71    1.880080 6.547E-26 3.659E-04.0380.0373  343',
     5     '.96940.71 0.00000  1  1         Q15R14  d404 3 1-1',
     5     ' 7 7.601213E-01-1.7046E-01 4.932302E-01-5.4191E-02',
     5     ' 3.441107E-01-1.2776E-02 2.488348E-01 4.3720E-03-1',
     6     ' 71    1.900255 8.724E-26 3.693E-04.0390.0382  260',
     6     '.68250.71 0.00000  1  1         Q13R12  d404 3 1-1',
     6     ' 7 5.722225E-01-4.7104E-02 3.437666E-01 1.5909E-02',
     6     ' 2.194682E-01 3.1475E-02 1.419426E-01 3.3980E-02-1'/
      DATA CPL017/
     7     ' 71    1.921745 1.076E-25 3.740E-04.0410.0402  188',
     7     '.85310.71 0.00000  1  1         Q11R10  d404 3 1-1',
     7     ' 7 3.297889E-01 7.6482E-02 1.557628E-01 7.8251E-02',
     7     ' 6.534860E-02 6.6489E-02 1.147462E-02 5.4611E-02-1',
     8     ' 71    1.945475 1.219E-25 3.808E-04.0430.0422  128',
     8     '.49210.71 0.00000  1  1         Q 9R 8  d404 3 1-1',
     8     ' 7 3.918693E-02 1.5052E-01-6.463015E-02 1.0263E-01',
     8     '-1.125612E-01 7.2165E-02-1.373579E-01 5.2122E-02-1',
     9     ' 71    1.949568 7.509E-26 2.559E-04.0440.0431   16',
     9     '.38760.71 0.00000  1  1         Q 3P 4  d404 3 1-1',
     9     ' 7 7.890647E-01-1.6462E-01 6.803579E-01-1.4306E-01',
     9     ' 6.022381E-01-1.2225E-01 5.418589E-01-1.0496E-01-1',
     *     ' 71    1.973505 1.249E-25 3.910E-04.0440.0431   79',
     *     '.60700.71 0.00000  1  1         Q 7R 6  d404 3 1-1',
     *     ' 7-2.779495E-01 1.1300E-01-3.002727E-01 5.6920E-02',
     *     '-3.000740E-01 2.9693E-02-2.924966E-01 1.4739E-02-1'/
      DATA CPL021/
     1     ' 71    1.987741 1.108E-25 2.851E-04.0420.0412   42',
     1     '.22400.71 0.00000  1  1         Q 5P 6  d404 3 1-1',
     1     ' 7 4.794608E-01 1.0262E-01 4.536001E-01 3.5299E-02',
     1     ' 4.231822E-01 6.8059E-03 3.943356E-01-6.8794E-03-1',
     2     ' 71    2.011594 1.128E-25 4.094E-04.0440.0431   42',
     2     '.20010.71 0.00000  1  1         Q 5R 4  d404 3 1-1',
     2     ' 7-5.327848E-01-3.5579E-02-4.816093E-01-4.6390E-02',
     2     '-4.396182E-01-4.6499E-02-4.047589E-01-4.3716E-02-1',
     3     ' 71    2.015887 1.307E-25 2.999E-04.0410.0402   79',
     3     '.56460.71 0.00000  1  1         Q 7P 8  d404 3 1-1',
     3     ' 7 1.235438E-01 2.1328E-01 1.883597E-01 1.2978E-01',
     3     ' 2.114051E-01 8.4445E-02 2.185150E-01 5.7132E-02-1',
     4     ' 71    2.039763 1.341E-25 3.084E-04.0400.0392  128',
     4     '.39780.71 0.00000  1  1         Q 9P10  d404 3 1-1',
     4     ' 7-2.063466E-01 1.6357E-01-6.143573E-02 1.2604E-01',
     4     ' 9.821956E-03 9.5538E-02 4.983153E-02 7.3278E-02-1'/
      DATA CPL025/
     5     ' 71    2.061431 1.239E-25 3.142E-04.0390.0382  188',
     5     '.71340.71 0.00000  1  1         Q11P12  d404 3 1-1',
     5     ' 7-4.831046E-01 3.2946E-02-2.749348E-01 6.4624E-02',
     5     '-1.646512E-01 6.3743E-02-9.755671E-02 5.6468E-02-1',
     6     ' 71    2.081814 1.048E-25 3.185E-04.0380.0373  260',
     6     '.50090.71 0.00000  1  1         Q13P14  d404 3 1-1',
     6     ' 7-6.968355E-01-1.1040E-01-4.437926E-01-1.4286E-02',
     6     '-3.048795E-01 1.5332E-02-2.174644E-01 2.4972E-02-1',
     7     ' 71    2.084317 8.366E-26 4.488E-04.0470.0461   16',
     7     '.25290.71 0.00000  1  1         Q 3R 2  d404 3 1-1',
     7     ' 7-5.651932E-01-1.1675E-01-4.878898E-01-9.0282E-02',
     7     '-4.338773E-01-7.2866E-02-3.927341E-01-6.0462E-02-1',
     8     ' 71    2.101386 8.186E-26 3.216E-04.0340.0333  343',
     8     '.74810.71 0.00000  1  1         Q15P16  d404 3 1-1',
     8     ' 7-8.583280E-01-2.3058E-01-5.745448E-01-8.6245E-02',
     8     '-4.151896E-01-3.2102E-02-3.128858E-01-8.0634E-03-1'/
      DATA CPL029/
     9     ' 71    2.120417 5.941E-26 3.240E-04.0360.0353  438',
     9     '.44140.71 0.00000  1  1         Q17P18  d404 3 1-1',
     9     ' 7-9.781035E-01-3.2006E-01-6.749418E-01-1.4386E-01',
     9     '-5.017825E-01-7.2235E-02-3.890086E-01-3.7363E-02-1',
     *     ' 71    2.139072 4.024E-26 3.260E-04.0350.0343  544',
     *     '.56510.71 0.00000  1  1         Q19P20  d404 3 1-1',
     *     ' 7-1.060925E+00-3.7897E-01-7.478667E-01-1.8521E-01',
     *     '-5.665768E-01-1.0269E-01-4.471765E-01-6.0590E-02-1',
     1     ' 71    2.157456 2.549E-26 3.275E-04.0350.0343  662',
     1     '.10210.71 0.00000  1  1         Q21P22  d404 3 1-1',
     1     ' 7-1.114748E+00-4.1189E-01-7.986852E-01-2.1158E-01',
     1     '-6.135178E-01-1.2353E-01-4.904359E-01-7.7288E-02-1',
     2     ' 71    2.175640 1.514E-26 3.287E-04.0320.0314  791',
     2     '.03320.71 0.00000  1  1         Q23P24  d404 3 1-1',
     2     ' 7-1.148377E+00-4.2691E-01-8.338457E-01-2.2700E-01',
     2     '-6.476877E-01-1.3703E-01-5.229612E-01-8.8783E-02-1'/
      DATA CPL033/
     3     ' 71    2.193676 8.448E-27 3.299E-04.0320.0314  931',
     3     '.33740.71 0.00000  1  1         Q25P26  d404 3 1-1',
     3     ' 7-1.164290E+00-4.2786E-01-8.549446E-01-2.3337E-01',
     3     '-6.701965E-01-1.4423E-01-5.455564E-01-9.5669E-02-1',
     4     ' 71    2.211599 4.431E-27 3.309E-04.0320.0314 1082',
     4     '.99210.71 0.00000  1  1         Q27P28  d404 3 1-1',
     4     ' 7-1.167919E+00-4.2068E-01-8.659866E-01-2.3402E-01',
     4     '-6.842068E-01-1.4721E-01-5.608208E-01-9.9336E-02-1',
     5     ' 71    2.229435 2.188E-27 3.318E-04.0320.0314 1245',
     5     '.97250.71 0.00000  1  1         Q29P30  d404 3 1-1',
     5     ' 7-1.163218E+00-4.0865E-01-8.700014E-01-2.3093E-01',
     5     '-6.921621E-01-1.4729E-01-5.707883E-01-1.0072E-01-1',
     6     ' 71    2.247206 1.017E-27 3.323E-04.0320.0314 1420',
     6     '.25230.71 0.00000  1  1         Q31P32  d404 3 1-1',
     6     ' 7-1.149577E+00-3.9362E-01-8.662894E-01-2.2521E-01',
     6     '-6.933521E-01-1.4521E-01-5.747715E-01-1.0031E-01-1'/
      DATA CPL037/
     7     ' 71    2.264927 4.460E-28 3.331E-04.0320.0314 1605',
     7     '.80300.71 0.00000  1  1         Q33P34  d404 3 1-1',
     7     ' 7-1.132430E+00-3.7744E-01-8.592104E-01-2.1813E-01',
     7     '-6.914504E-01-1.4184E-01-5.759681E-01-9.8767E-02-1',
     8     ' 71    2.282611 1.844E-28 3.336E-04.0320.0314 1802',
     8     '.59470.71 0.00000  1  1         Q35P36  d404 3 1-1',
     8     ' 7-1.111641E+00-3.6136E-01-8.487706E-01-2.1060E-01',
     8     '-6.866307E-01-1.3788E-01-5.747203E-01-9.6604E-02-1',
     9     ' 71    2.300266 7.199E-29 3.342E-04.0320.0314 2010',
     9     '.59530.71 0.00000  1  1         Q37P38  d404 3 1-1',
     9     ' 7-1.090479E+00-3.4557E-01-8.383655E-01-2.0283E-01',
     9     '-6.825245E-01-1.3348E-01-5.748819E-01-9.3942E-02-1',
     *     ' 71    2.317902 2.653E-29 3.346E-04.0320.0314 2229',
     *     '.77100.71 0.00000  1  1         Q39P40  d404 3 1-1',
     *     ' 7-1.076503E+00-3.3148E-01-8.385238E-01-1.9607E-01',
     *     '-6.914820E-01-1.2989E-01-5.898091E-01-9.2027E-02-1'/
      DATA CPL041/
     1     ' 71    3.961085 9.956E-26 6.864E-04.0500.0490    0',
     1     '.00000.71 0.00000  1  1         Q 1R 0  d404 3 1-1',
     1     ' 7-6.874010E-02-1.5962E-03-6.168349E-02-1.2712E-03',
     1     '-5.666008E-02-1.0585E-03-5.276715E-02-9.0542E-04-1',
     2     ' 21  612.198431 1.306E-25 5.472E-03.0589.0584 3196',
     2     '.99980.630.000000  3  2             Q 80 464 2 2-1',
     2     ' 2 8.954269E-02-3.2300E-03 7.303400E-02-2.1646E-03',
     2     ' 6.252350E-02-1.5965E-03 5.478330E-02-1.2325E-03-1',
     3     ' 21  612.520406 2.384E-25 5.606E-03.0594.0590 3073',
     3     '.12720.640.000000  3  2             Q 78 464 2 2-1',
     3     ' 2 7.751120E-02-2.3153E-03 6.083089E-02-1.4004E-03',
     3     ' 5.065710E-02-9.5481E-04 4.343170E-02-6.9090E-04-1',
     4     ' 21  612.830969 4.283E-25 5.742E-03.0598.0597 2952',
     4     '.34450.650.000000  3  2             Q 76 464 2 2-1',
     4     ' 2 7.815340E-02-2.4218E-03 6.037200E-02-1.4342E-03',
     4     ' 4.959500E-02-9.5802E-04 4.201600E-02-6.7971E-04-1'/
      DATA CPL045/
     5     ' 21  613.130334 7.568E-25 5.876E-03.0603.0605 2834',
     5     '.65380.660.000000  3  2             Q 74 464 2 2-1',
     5     ' 2 8.068450E-02-2.5909E-03 6.190730E-02-1.5231E-03',
     5     ' 5.052580E-02-1.0101E-03 4.254120E-02-7.1157E-04-1',
     6     ' 21  613.418714 1.315E-24 6.008E-03.0608.0612 2720',
     6     '.05690.660.000000  3  2             Q 72 464 2 2-1',
     6     ' 2 8.379930E-02-2.7565E-03 6.407180E-02-1.6098E-03',
     6     ' 5.210920E-02-1.0611E-03 4.372420E-02-7.4321E-04-1',
     7     ' 21  613.696323 2.247E-24 6.137E-03.0613.0620 2608',
     7     '.55590.670.000000  3  2             Q 70 464 2 2-1',
     7     ' 2 8.704670E-02-2.7787E-03 6.640010E-02-1.5972E-03',
     7     ' 5.388240E-02-1.0376E-03 4.511390E-02-7.1620E-04-1',
     8     ' 21  613.963372 3.777E-24 6.265E-03.0618.0629 2500',
     8     '.15230.680.000000  3  2             Q 68 464 2 2-1',
     8     ' 2 9.070879E-02-3.0975E-03 6.905990E-02-1.7927E-03',
     8     ' 5.594290E-02-1.1729E-03 4.676360E-02-8.1602E-04-1'/
      DATA CPL049/
     9     ' 21  614.220072 6.242E-24 6.390E-03.0623.0638 2394',
     9     '.84840.690.000000  3  2             Q 66 464 2 2-1',
     9     ' 2 9.441249E-02-3.3709E-03 7.175349E-02-1.9478E-03',
     9     ' 5.803980E-02-1.2734E-03 4.845490E-02-8.8577E-04-1',
     *     ' 21  614.466632 1.014E-23 6.509E-03.0628.0647 2292',
     *     '.64530.700.000000  3  2             Q 64 464 2 2-1',
     *     ' 2 9.832940E-02-3.6683E-03 7.460440E-02-2.1139E-03',
     *     ' 6.026150E-02-1.3793E-03 5.025020E-02-9.5811E-04-1',
     1     ' 21  614.703257 1.621E-23 6.630E-03.0633.0657 2193',
     1     '.54540.710.000000  3  2             Q 62 464 2 2-1',
     1     ' 2 1.024543E-01-3.9944E-03 7.759180E-02-2.2942E-03',
     1     ' 6.258460E-02-1.4932E-03 5.212609E-02-1.0353E-03-1',
     2     ' 21  614.930150 2.548E-23 6.749E-03.0638.0667 2097',
     2     '.54980.720.000000  3  2             Q 60 464 2 2-1',
     2     ' 2 1.067625E-01-4.3524E-03 8.070010E-02-2.4913E-03',
     2     ' 6.499350E-02-1.6171E-03 5.406830E-02-1.1189E-03-1'/
      DATA CPL053/
     3     ' 21  615.147513 3.936E-23 6.862E-03.0643.0678 2004',
     3     '.66040.730.000000  3  2             Q 58 464 2 2-1',
     3     ' 2 1.115075E-01-4.7602E-03 8.413339E-02-2.7156E-03',
     3     ' 6.766370E-02-1.7580E-03 5.622759E-02-1.2139E-03-1',
     4     ' 21  615.355541 5.979E-23 6.973E-03.0647.0689 1914',
     4     '.87870.740.000000  3  2             Q 56 464 2 2-1',
     4     ' 2 1.163968E-01-5.2006E-03 8.765250E-02-2.9557E-03',
     4     ' 7.038720E-02-1.9076E-03 5.842200E-02-1.3140E-03-1',
     5     ' 21  615.554428 8.929E-23 7.081E-03.0652.0700 1828',
     5     '.20630.750.000000  3  2             Q 54 464 2 2-1',
     5     ' 2 1.216514E-01-5.6904E-03 9.142900E-02-3.2209E-03',
     5     ' 7.330959E-02-2.0721E-03 6.077630E-02-1.4236E-03-1',
     6     ' 21  615.744362 1.311E-22 7.188E-03.0656.0712 1744',
     6     '.64450.750.000000  3  2             Q 52 464 2 2-1',
     6     ' 2 1.266785E-01-6.1987E-03 9.499230E-02-3.4927E-03',
     6     ' 7.603310E-02-2.2386E-03 6.294730E-02-1.5333E-03-1'/
      DATA CPL057/
     7     ' 21  615.925529 1.891E-22 7.287E-03.0660.0725 1664',
     7     '.19470.760.000000  3  2             Q 50 464 2 2-1',
     7     ' 2 1.324180E-01-6.7996E-03 9.905870E-02-3.8133E-03',
     7     ' 7.914010E-02-2.4348E-03 6.542380E-02-1.6623E-03-1',
     8     ' 21  616.098108 2.682E-22 7.387E-03.0663.0738 1586',
     8     '.85830.770.000000  3  2             Q 48 464 2 2-1',
     8     ' 2 1.386060E-01-7.4669E-03 1.034462E-01-4.1668E-03',
     8     ' 8.249540E-02-2.6493E-03 6.810180E-02-1.8023E-03-1',
     9     ' 21  616.262275 3.737E-22 7.482E-03.0666.0751 1512',
     9     '.63660.770.000000  3  2             Q 46 464 2 2-1',
     9     ' 2 1.450670E-01-8.1895E-03 1.080053E-01-4.5438E-03',
     9     ' 8.597030E-02-2.8748E-03 7.086560E-02-1.9475E-03-1',
     *     ' 21  616.418198 5.116E-22 7.573E-03.0668.0765 1441',
     *     '.53080.780.000000  3  2             Q 44 465 2 2-1',
     *     ' 2 1.515800E-01-8.9517E-03 1.125384E-01-4.9325E-03',
     *     ' 8.938409E-02-3.1019E-03 7.355530E-02-2.0901E-03-1'/
      DATA CPL061/
     1     ' 21  616.566044 6.881E-22 7.662E-03.0671.0780 1373',
     1     '.54210.780.000000  3  2             Q 42 465 2 2-1',
     1     ' 2 1.587170E-01-9.7834E-03 1.175005E-01-5.3453E-03',
     1     ' 9.311770E-02-3.3361E-03 7.649330E-02-2.2322E-03-1',
     2     ' 21  616.705970 9.089E-22 7.746E-03.0673.0795 1308',
     2     '.67180.780.000000  3  2             Q 40 465 2 2-1',
     2     ' 2 1.661140E-01-1.0442E-02 1.225822E-01-5.6177E-03',
     2     ' 9.690069E-02-3.4550E-03 7.944300E-02-2.2782E-03-1',
     3     ' 21  616.838131 1.179E-21 7.827E-03.0675.0810 1246',
     3     '.92050.780.000000  3  2             Q 38 465 2 2-1',
     3     ' 2 1.746810E-01-1.1858E-02 1.285011E-01-6.3814E-03',
     3     ' 1.013363E-01-3.9295E-03 8.292440E-02-2.5977E-03-1',
     4     ' 21  616.962671 1.501E-21 7.903E-03.0677.0826 1188',
     4     '.28980.780.000000  3  2             Q 36 465 2 2-1',
     4     ' 2 1.823640E-01-1.2615E-02 1.336530E-01-6.6617E-03',
     4     ' 1.050985E-01-4.0273E-03 8.580520E-02-2.6133E-03-1'/
      DATA CPL065/
     5     ' 21  617.079734 1.876E-21 7.977E-03.0679.0843 1132',
     5     '.78030.780.000000  3  2             Q 34 465 2 2-1',
     5     ' 2 1.916330E-01-1.3779E-02 1.399060E-01-7.1695E-03',
     5     ' 1.096680E-01-4.2719E-03 8.931649E-02-2.7320E-03-1',
     6     ' 21  617.189451 2.300E-21 8.046E-03.0682.0860 1080',
     6     '.39320.780.000000  3  2             Q 32 465 2 2-1',
     6     ' 2 2.006550E-01-1.5210E-02 1.458600E-01-7.8220E-03',
     6     ' 1.139437E-01-4.6086E-03 9.254440E-02-2.9156E-03-1',
     7     ' 21  617.291952 2.766E-21 8.113E-03.0686.0878 1031',
     7     '.12920.780.000000  3  2             Q 30 465 2 2-1',
     7     ' 2 2.104830E-01-1.6540E-02 1.522950E-01-8.3248E-03',
     7     ' 1.185314E-01-4.7971E-03 9.598680E-02-2.9644E-03-1',
     8     ' 21  617.387357 3.260E-21 8.176E-03.0690.0897  984',
     8     '.98900.780.000000  3  2             Q 28 465 2 2-1',
     8     ' 2 2.208050E-01-1.7129E-02 1.588860E-01-8.2408E-03',
     8     ' 1.231139E-01-4.5128E-03 9.934080E-02-2.6257E-03-1'/
      DATA CPL069/
     9     ' 21  617.475780 3.763E-21 8.233E-03.0695.0916  941',
     9     '.97370.780.000000  3  2             Q 26 465 2 2-1',
     9     ' 2 2.310230E-01-1.8751E-02 1.652040E-01-8.7888E-03',
     9     ' 1.273727E-01-4.6726E-03 1.023555E-01-2.6247E-03-1',
     *     ' 21  617.557327 4.251E-21 8.286E-03.0701.0935  902',
     *     '.08380.780.000000  3  2             Q 24 465 2 2-1',
     *     ' 2 2.423590E-01-1.9111E-02 1.721460E-01-8.3274E-03',
     *     ' 1.319890E-01-4.0173E-03 1.055834E-01-1.9557E-03-1',
     1     ' 21  617.632099 4.697E-21 8.336E-03.0708.0956  865',
     1     '.32000.770.000000  3  2             Q 22 465 2 2-1',
     1     ' 2 2.521350E-01-1.8187E-02 1.774890E-01-6.8917E-03',
     1     ' 1.350830E-01-2.6047E-03 1.073800E-01-6.8557E-04-1',
     2     ' 21  617.700187 5.068E-21 8.382E-03.0717.0977  831',
     2     '.68290.760.000000  3  2             Q 20 465 2 2-1',
     2     ' 2 2.624700E-01-1.7969E-02 1.828840E-01-5.7630E-03',
     2     ' 1.379820E-01-1.3615E-03 1.088906E-01 4.7098E-04-1'/
      DATA CPL073/
     3     ' 21  617.761675 5.331E-21 8.423E-03.0728.0998  801',
     3     '.17320.740.000000  3  2             Q 18 465 2 2-1',
     3     ' 2 2.716090E-01-1.2322E-02 1.868230E-01-7.7059E-04',
     3     ' 1.393860E-01 2.8036E-03 1.089348E-01 3.9222E-03-1',
     4     ' 21  617.816642 5.457E-21 8.461E-03.0740.1020  773',
     4     '.79120.710.000000  3  2             Q 16 465 2 2-1',
     4     ' 2 2.785250E-01-4.9414E-03 1.883700E-01 5.5132E-03',
     4     ' 1.384500E-01 7.9239E-03 1.067625E-01 8.0865E-03-1',
     5     ' 21  617.865156 5.417E-21 8.494E-03.0754.1043  749',
     5     '.53750.690.000000  3  2             Q 14 465 2 2-1',
     5     ' 2 2.786550E-01 1.6385E-02 1.836640E-01 2.1152E-02',
     5     ' 1.317940E-01 1.9850E-02 9.937979E-02 1.7450E-02-1',
     6     ' 21  617.907280 5.193E-21 8.523E-03.0771.1067  728',
     6     '.41240.690.000000  3  2             Q 12 465 2 2-1',
     6     ' 2 2.673190E-01 5.5714E-02 1.687270E-01 4.8269E-02',
     6     ' 1.159626E-01 3.9797E-02 8.371739E-02 3.2739E-02-1'/
      DATA CPL077/
     7     ' 21  617.943067 4.775E-21 8.548E-03.0790.1091  710',
     7     '.41630.700.000000  3  2             Q 10 465 2 2-1',
     7     ' 2 2.343900E-01 1.6074E-01 1.352000E-01 1.1706E-01',
     7     ' 8.376029E-02 8.9060E-02 5.347550E-02 6.9972E-02-1',
     8     ' 21  617.972564 4.165E-21 8.568E-03.0811.1116  695',
     8     '.54960.710.000000  3  2             Q  8 465 2 2-1',
     8     ' 2 1.443000E-01 4.0498E-01 5.435950E-02 2.6988E-01',
     8     ' 1.086917E-02 1.9534E-01-1.250730E-02 1.4877E-01-1',
     9     ' 21  617.995808 3.379E-21 8.585E-03.0836.1141  683',
     9     '.81240.730.000000  3  2             Q  6 465 2 2-1',
     9     ' 2-8.047130E-02 1.1181E+00-1.357070E-01 7.0037E-01',
     9     '-1.547650E-01 4.8794E-01-1.593150E-01 3.6276E-01-1',
     *     ' 21  618.012829 2.442E-21 8.595E-03.0863.1190  675',
     *     '.20500.740.000000  3  2             Q  4 465 2 2-1',
     *     ' 2-8.516560E-01 3.6551E+00-7.574060E-01 2.1692E+00',
     *     '-6.807320E-01 1.4582E+00-6.170970E-01 1.0603E+00-1'/
      DATA CPL081/
     1     ' 21  618.023651 1.395E-21 8.605E-03.0893.1228  669',
     1     '.72750.750.000000  3  2             Q  2 465 2 2-1',
     1     ' 2-5.640050E+00-1.0555E+01-4.470570E+00-6.7582E+00',
     1     '-3.743090E+00-4.8014E+00-3.241810E+00-3.6357E+00-1',
     2     ' 21  667.386041 7.476E-20 1.647E-02.0893.1228    2',
     2     '.34130.750.000000  2  1             Q  2 465 2 2-1',
     2     ' 2 4.205760E+00-5.8766E+00 3.334370E+00-3.7640E+00',
     2     ' 2.792270E+00-2.6750E+00 2.418650E+00-2.0261E+00-1',
     3     ' 21  667.400540 1.311E-19 1.648E-02.0863.1190    7',
     3     '.80430.740.000000  2  1             Q  4 465 2 2-1',
     3     ' 2 6.460220E-01 2.0171E+00 5.739110E-01 1.1961E+00',
     3     ' 5.155800E-01 8.0354E-01 4.672850E-01 5.8395E-01-1',
     4     ' 21  667.423323 1.816E-19 1.648E-02.0836.1141   16',
     4     '.38900.730.000000  2  1             Q  6 465 2 2-1',
     4     ' 2 7.248670E-02 6.1975E-01 1.115361E-01 3.8769E-01',
     4     ' 1.243710E-01 2.6980E-01 1.267903E-01 2.0041E-01-1'/
      DATA CPL085/
     5     ' 21  667.454386 2.243E-19 1.647E-02.0811.1116   28',
     5     '.09510.710.000000  2  1             Q  8 465 2 2-1',
     5     ' 2-1.004549E-01 2.2759E-01-3.422380E-02 1.5140E-01',
     5     '-2.396290E-03 1.0944E-01 1.456260E-02 8.3265E-02-1',
     6     ' 21  667.493726 2.578E-19 1.647E-02.0790.1091   42',
     6     '.92250.700.000000  2  1             Q 10 465 2 2-1',
     6     ' 2-1.684020E-01 9.1130E-02-9.516520E-02 6.6109E-02',
     6     '-5.732610E-02 5.0180E-02-3.515200E-02 3.9360E-02-1',
     7     ' 21  667.541336 2.813E-19 1.647E-02.0771.1067   60',
     7     '.87090.690.000000  2  1             Q 12 465 2 2-1',
     7     ' 2-1.944020E-01 3.2209E-02-1.212549E-01 2.7599E-02',
     7     '-8.221719E-02 2.2641E-02-5.843890E-02 1.8568E-02-1',
     8     ' 21  667.597213 2.946E-19 1.648E-02.0754.1043   81',
     8     '.94010.690.000000  2  1             Q 14 465 2 2-1',
     8     ' 2-2.023580E-01 1.0175E-02-1.319890E-01 1.2425E-02',
     8     '-9.368060E-02 1.1489E-02-6.981650E-02 1.0026E-02-1'/
      DATA CPL089/
     9     ' 21  667.661347 2.981E-19 1.648E-02.0740.1020  106',
     9     '.12970.710.000000  2  1             Q 16 465 2 2-1',
     9     ' 2-2.020850E-01-1.5421E-03-1.352780E-01 3.8432E-03',
     9     '-9.840609E-02 4.9509E-03-7.508410E-02 4.8970E-03-1',
     *     ' 21  667.733731 2.927E-19 1.648E-02.0728.0998  133',
     *     '.43930.740.000000  2  1             Q 18 465 2 2-1',
     *     ' 2-1.994460E-01-5.9959E-03-1.361360E-01 1.7673E-04',
     *     '-1.008046E-01 2.0098E-03-7.817810E-02 2.5282E-03-1',
     1     ' 21  667.814356 2.798E-19 1.648E-02.0717.0977  163',
     1     '.86840.760.000000  2  1             Q 20 465 2 2-1',
     1     ' 2-1.928420E-01-9.3341E-03-1.333150E-01-2.7214E-03',
     1     '-9.982570E-02-3.8948E-04-7.818460E-02 5.4867E-04-1',
     2     ' 21  667.903212 2.609E-19 1.647E-02.0708.0956  197',
     2     '.41660.770.000000  2  1             Q 22 465 2 2-1',
     2     ' 2-1.866150E-01-9.6885E-03-1.304550E-01-3.4759E-03',
     2     '-9.861150E-02-1.1582E-03-7.787260E-02-1.4468E-04-1'/
      DATA CPL093/
     3     ' 21  668.000288 2.378E-19 1.648E-02.0701.0935  234',
     3     '.08330.780.000000  2  1             Q 24 465 2 2-1',
     3     ' 2-1.791920E-01-1.0275E-02-1.263314E-01-4.3234E-03',
     3     '-9.618180E-02-1.9794E-03-7.641270E-02-8.7781E-04-1',
     4     ' 21  668.105570 2.120E-19 1.648E-02.0695.0916  273',
     4     '.86800.780.000000  2  1             Q 26 465 2 2-1',
     4     ' 2-1.720940E-01-1.0276E-02-1.222013E-01-4.6981E-03',
     4     '-9.359610E-02-2.4214E-03-7.473180E-02-1.3043E-03-1',
     5     ' 21  668.219046 1.851E-19 1.647E-02.0690.0897  316',
     5     '.76980.780.000000  2  1             Q 28 465 2 2-1',
     5     ' 2-1.647230E-01-9.4160E-03-1.176617E-01-4.4179E-03',
     5     '-9.055280E-02-2.3481E-03-7.258680E-02-1.3149E-03-1',
     6     ' 21  668.340702 1.584E-19 1.647E-02.0686.0878  362',
     6     '.78820.780.000000  2  1             Q 30 465 2 2-1',
     6     ' 2-1.579110E-01-9.2258E-03-1.134523E-01-4.5527E-03',
     6     '-8.772660E-02-2.5681E-03-7.059649E-02-1.5493E-03-1'/
      DATA CPL097/
     7     ' 21  668.470522 1.330E-19 1.648E-02.0682.0860  411',
     7     '.92250.780.000000  2  1             Q 32 465 2 2-1',
     7     ' 2-1.514890E-01-8.5960E-03-1.093404E-01-4.3399E-03',
     7     '-8.486010E-02-2.5089E-03-6.849179E-02-1.5548E-03-1',
     8     ' 21  668.608490 1.095E-19 1.647E-02.0679.0843  464',
     8     '.17170.780.000000  2  1             Q 34 465 2 2-1',
     8     ' 2-1.452100E-01-7.8357E-03-1.053052E-01-4.0033E-03',
     8     '-8.203260E-02-2.3413E-03-6.641180E-02-1.4677E-03-1',
     9     ' 21  668.754588 8.856E-20 1.648E-02.0677.0826  519',
     9     '.53500.780.000000  2  1             Q 36 465 2 2-1',
     9     ' 2-1.397630E-01-7.3280E-03-1.017640E-01-3.8054E-03',
     9     '-7.953010E-02-2.2624E-03-6.455150E-02-1.4427E-03-1',
     *     ' 21  668.908799 7.032E-20 1.648E-02.0675.0810  578',
     *     '.01160.780.000000  2  1             Q 38 465 2 2-1',
     *     ' 2-1.340560E-01-6.9488E-03-9.794330E-02-3.6825E-03',
     *     '-7.675070E-02-2.2344E-03-6.242990E-02-1.4555E-03-1'/
      DATA CPL101/
     1     ' 21  669.071103 5.485E-20 1.648E-02.0673.0795  639',
     1     '.60040.780.000000  2  1             Q 40 465 2 2-1',
     1     ' 2-1.286259E-01-6.1932E-03-9.428120E-02-3.2761E-03',
     1     '-7.407010E-02-1.9820E-03-6.036940E-02-1.2854E-03-1',
     2     ' 21  669.241480 4.204E-20 1.648E-02.0671.0780  704',
     2     '.30050.780.000000  2  1             Q 42 465 2 2-1',
     2     ' 2-1.236196E-01-5.8899E-03-9.088949E-02-3.1684E-03',
     2     '-7.157540E-02-1.9486E-03-5.844670E-02-1.2851E-03-1',
     3     ' 21  669.419908 3.167E-20 1.648E-02.0668.0765  772',
     3     '.11070.780.000000  2  1             Q 44 465 2 2-1',
     3     ' 2-1.190072E-01-5.4778E-03-8.775389E-02-2.9731E-03',
     3     '-6.926270E-02-1.8435E-03-5.665920E-02-1.2253E-03-1',
     4     ' 21  669.606367 2.345E-20 1.648E-02.0666.0751  843',
     4     '.03010.770.000000  2  1             Q 46 464 2 2-1',
     4     ' 2-1.144598E-01-5.0631E-03-8.462349E-02-2.7664E-03',
     4     '-6.692920E-02-1.7255E-03-5.483790E-02-1.1530E-03-1'/
      DATA CPL105/
     5     ' 21  669.800831 1.707E-20 1.647E-02.0663.0738  917',
     5     '.05730.770.000000  2  1             Q 48 464 2 2-1',
     5     ' 2-1.104857E-01-4.7008E-03-8.190909E-02-2.5843E-03',
     5     '-6.492069E-02-1.6206E-03-5.328180E-02-1.0880E-03-1',
     6     ' 21  670.003279 1.222E-20 1.647E-02.0660.0725  994',
     6     '.19130.760.000000  2  1             Q 50 464 2 2-1',
     6     ' 2-1.065636E-01-4.3611E-03-7.917260E-02-2.4094E-03',
     6     '-6.285500E-02-1.5172E-03-5.165160E-02-1.0223E-03-1',
     7     ' 21  670.213684 8.606E-21 1.648E-02.0656.0712 1074',
     7     '.43070.750.000000  2  1             Q 52 464 2 2-1',
     7     ' 2-1.026506E-01-4.0382E-03-7.643610E-02-2.2412E-03',
     7     '-6.078669E-02-1.4166E-03-5.001880E-02-9.5759E-04-1',
     8     ' 21  670.432022 5.960E-21 1.648E-02.0652.0700 1157',
     8     '.77420.750.000000  2  1             Q 54 464 2 2-1',
     8     ' 2-9.913930E-02-3.7514E-03-7.397909E-02-2.0909E-03',
     8     '-5.892770E-02-1.3261E-03-4.854980E-02-8.9899E-04-1'/
      DATA CPL109/
     9     ' 21  670.658264 4.061E-21 1.648E-02.0647.0689 1244',
     9     '.22050.740.000000  2  1             Q 56 464 2 2-1',
     9     ' 2-9.576450E-02-3.4891E-03-7.160140E-02-1.9523E-03',
     9     '-5.712070E-02-1.2422E-03-4.711590E-02-8.4425E-04-1',
     *     ' 21  670.892385 2.722E-21 1.648E-02.0643.0678 1333',
     *     '.76790.730.000000  2  1             Q 58 464 2 2-1',
     *     ' 2-9.251189E-02-3.2469E-03-6.929129E-02-1.8230E-03',
     *     '-5.535140E-02-1.1631E-03-4.570280E-02-7.9215E-04-1',
     1     ' 21  671.134356 1.795E-21 1.648E-02.0638.0667 1426',
     1     '.41540.720.000000  2  1             Q 60 464 2 2-1',
     1     ' 2-8.929700E-02-3.0166E-03-6.700329E-02-1.6991E-03',
     1     '-5.359640E-02-1.0866E-03-4.430010E-02-7.4142E-04-1',
     2     ' 21  671.384147 1.165E-21 1.648E-02.0633.0657 1522',
     2     '.16110.710.000000  2  1             Q 62 464 2 2-1',
     2     ' 2-8.654879E-02-2.8198E-03-6.507410E-02-1.5938E-03',
     2     '-5.213390E-02-1.0220E-03-4.314570E-02-6.9880E-04-1'/
      DATA CPL113/
     3     ' 21  671.641729 7.438E-22 1.648E-02.0628.0647 1621',
     3     '.00370.700.000000  2  1             Q 64 464 2 2-1',
     3     ' 2-8.402549E-02-2.6467E-03-6.328270E-02-1.5006E-03',
     3     '-5.076370E-02-9.6453E-04-4.205240E-02-6.6063E-04-1',
     4     ' 21  671.907070 4.674E-22 1.648E-02.0623.0638 1722',
     4     '.94130.690.000000  2  1             Q 66 464 2 2-1',
     4     ' 2-8.132670E-02-2.4743E-03-6.134830E-02-1.4065E-03',
     4     '-4.927130E-02-9.0559E-04-4.085380E-02-6.2094E-04-1',
     5     ' 21  672.180139 2.891E-22 1.648E-02.0618.0629 1827',
     5     '.97240.680.000000  2  1             Q 68 464 2 2-1',
     5     ' 2-7.890870E-02-2.3162E-03-5.961410E-02-1.3179E-03',
     5     '-4.793360E-02-8.4871E-04-3.978000E-02-5.8159E-04-1',
     6     ' 21  672.460904 1.760E-22 1.648E-02.0613.0620 1936',
     6     '.09530.670.000000  2  1             Q 70 464 2 2-1',
     6     ' 2-7.644130E-02-2.1063E-03-5.782660E-02-1.1859E-03',
     6     '-4.654260E-02-7.5527E-04-3.865420E-02-5.1119E-04-1'/
      DATA CPL117/
     7     ' 21  672.749332 1.055E-22 1.648E-02.0608.0612 2047',
     7     '.30810.660.000000  2  1             Q 72 464 2 2-1',
     7     ' 2-7.405320E-02-2.1436E-03-5.611060E-02-1.2308E-03',
     7     '-4.521530E-02-7.9847E-04-3.758820E-02-5.5078E-04-1',
     8     ' 21  673.045389 6.221E-23 1.648E-02.0603.0605 2161',
     8     '.60890.660.000000  2  1             Q 74 464 2 2-1',
     8     ' 2-7.223839E-02-2.0723E-03-5.481840E-02-1.1982E-03',
     8     '-4.422730E-02-7.8214E-04-3.680040E-02-5.4265E-04-1',
     9     ' 21  673.349040 3.612E-23 1.648E-02.0598.0597 2278',
     9     '.99610.650.000000  2  1             Q 76 464 2 2-1',
     9     ' 2-7.007780E-02-1.9686E-03-5.324280E-02-1.1430E-03',
     9     '-4.299360E-02-7.4871E-04-3.579810E-02-5.2107E-04-1',
     *     ' 21  673.660250 2.065E-23 1.648E-02.0594.0590 2399',
     *     '.46780.640.000000  2  1             Q 78 464 2 2-1',
     *     ' 2-6.803810E-02-1.8629E-03-5.176730E-02-1.0855E-03',
     *     '-4.184830E-02-7.1309E-04-3.487510E-02-4.9747E-04-1'/
      DATA CPL121/
     1     ' 21  673.978984 1.162E-23 1.648E-02.0589.0584 2523',
     1     '.02150.630.000000  2  1             Q 80 464 2 2-1',
     1     ' 2-6.634810E-02-1.7720E-03-5.054660E-02-1.0356E-03',
     1     '-4.090320E-02-6.8199E-04-3.411590E-02-4.7671E-04-1',
     2     ' 21  674.305204 6.438E-24 1.647E-02.0585.0578 2649',
     2     '.65580.620.000000  2  1             Q 82 464 2 2-1',
     2     ' 2-6.442020E-02-1.6744E-03-4.913870E-02-9.8114E-04',
     2     '-3.980340E-02-6.4739E-04-3.322800E-02-4.5323E-04-1',
     3     ' 21  674.638873 3.512E-24 1.647E-02.0581.0572 2779',
     3     '.36820.610.000000  2  1             Q 84 464 2 2-1',
     3     ' 2-6.275750E-02-1.5882E-03-4.793620E-02-9.3287E-04',
     3     '-3.887650E-02-6.1665E-04-3.249090E-02-4.3230E-04-1',
     4     ' 21  674.979953 1.887E-24 1.648E-02.0578.0566 2912',
     4     '.15650.600.000000  2  1             Q 86 464 2 2-1',
     4     ' 2-6.104929E-02-1.5025E-03-4.671160E-02-8.8455E-04',
     4     '-3.794830E-02-5.8568E-04-3.177070E-02-4.1110E-04-1'/
      DATA CPL125/
     5     ' 21  675.328407 9.979E-25 1.648E-02.0574.0561 3048',
     5     '.01900.590.000000  2  1             Q 88 464 2 2-1',
     5     ' 2-5.964400E-02-1.4288E-03-4.576129E-02-8.4310E-04',
     5     '-3.728400E-02-5.5930E-04-3.130790E-02-3.9322E-04-1',
     6     ' 21  675.684195 5.197E-25 1.648E-02.0571.0556 3186',
     6     '.95310.580.000000  2  1             Q 90 464 2 2-1',
     6     ' 2-5.837780E-02-1.3575E-03-4.504630E-02-8.0400E-04',
     6     '-3.691740E-02-5.3535E-04-3.118050E-02-3.7779E-04-1',
     7     ' 21  676.047277 2.665E-25 1.647E-02.0568.0551 3328',
     7     '.95650.570.000000  2  1             Q 92 464 2 2-1',
     7     ' 2-5.827900E-02-1.3118E-03-4.566510E-02-7.8963E-04',
     7     '-3.794960E-02-5.3468E-04-3.246360E-02-3.8376E-04-1',
     8     ' 21  676.417613 1.346E-25 1.648E-02.0565.0547 3474',
     8     '.02710.560.000000  2  1             Q 94 464 2 2-1',
     8     ' 2-6.767929E-02-1.8326E-03-5.537999E-02-1.2352E-03',
     8     '-4.752150E-02-9.1491E-04-4.170660E-02-7.0849E-04-1'/
      DATA CPL129/
     9     ' 21  714.581428 1.563E-25 9.937E-03.0585.0578 3323',
     9     '.96000.620.000000  5  2             Q 82 464 2 2-1',
     9     ' 2 1.043419E-01-4.3304E-03 8.500960E-02-2.8955E-03',
     9     ' 7.270119E-02-2.1316E-03 6.365320E-02-1.6435E-03-1',
     *     ' 21  714.830448 2.794E-25 9.818E-03.0589.0584 3196',
     *     '.99980.630.000000  5  2             Q 80 464 2 2-1',
     *     ' 2 8.547109E-02-2.7180E-03 6.647290E-02-1.5905E-03',
     *     ' 5.495750E-02-1.0532E-03 4.684420E-02-7.4217E-04-1',
     1     ' 21  715.078294 4.916E-25 9.698E-03.0594.0590 3073',
     1     '.12720.640.000000  5  2             Q 78 464 2 2-1',
     1     ' 2 8.333519E-02-2.6967E-03 6.357520E-02-1.5434E-03',
     1     ' 5.167760E-02-9.9856E-04 4.338360E-02-6.8722E-04-1',
     2     ' 21  715.324573 8.517E-25 9.581E-03.0598.0597 2952',
     2     '.34450.650.000000  5  2             Q 76 464 2 2-1',
     2     ' 2 8.376680E-02-2.7561E-03 6.339970E-02-1.5692E-03',
     2     ' 5.112770E-02-1.0101E-03 4.259060E-02-6.9147E-04-1'/
      DATA CPL133/
     3     ' 22  715.521048 1.019E-25 4.503E-03.0628.0647 2273',
     3     '.70190.700.000000  5  2             Q 64 464 2 2-1',
     3     ' 2 8.441290E-02-2.7048E-03 6.850740E-02-1.7945E-03',
     3     ' 5.840900E-02-1.3125E-03 5.103280E-02-1.0071E-03-1',
     4     ' 21  715.568904 1.453E-24 9.469E-03.0603.0605 2834',
     4     '.65380.660.000000  5  2             Q 74 464 2 2-1',
     4     ' 2 8.474569E-02-2.8145E-03 6.390150E-02-1.5963E-03',
     4     ' 5.132920E-02-1.0240E-03 4.258410E-02-6.9883E-04-1',
     5     ' 21  715.810918 2.441E-24 9.361E-03.0608.0612 2720',
     5     '.05690.660.000000  5  2             Q 72 464 2 2-1',
     5     ' 2 8.603010E-02-2.8502E-03 6.472050E-02-1.6059E-03',
     5     ' 5.186350E-02-1.0241E-03 4.292340E-02-6.9491E-04-1',
     6     ' 22  715.873962 1.599E-25 4.502E-03.0633.0657 2174',
     6     '.60420.710.000000  5  2             Q 62 464 2 2-1',
     6     ' 2 6.773780E-02-1.5636E-03 5.272540E-02-9.0671E-04',
     6     ' 4.365530E-02-5.9804E-04 3.728530E-02-4.2094E-04-1'/
      DATA CPL137/
     7     ' 21  716.050259 4.037E-24 9.256E-03.0613.0620 2608',
     7     '.55590.670.000000  5  2             Q 70 464 2 2-1',
     7     ' 2 8.750819E-02-2.7021E-03 6.572280E-02-1.4888E-03',
     7     ' 5.258370E-02-9.2888E-04 4.345120E-02-6.1595E-04-1',
     8     ' 22  716.217992 2.470E-25 4.503E-03.0638.0667 2078',
     8     '.61110.720.000000  5  2             Q 60 464 2 2-1',
     8     ' 2 6.627010E-02-1.5855E-03 5.046990E-02-8.9114E-04',
     8     ' 4.104360E-02-5.6980E-04 3.452020E-02-3.8905E-04-1',
     9     ' 21  716.286585 6.573E-24 9.154E-03.0618.0629 2500',
     9     '.15230.680.000000  5  2             Q 68 464 2 2-1',
     9     ' 2 8.909940E-02-2.9068E-03 6.682130E-02-1.6236E-03',
     9     ' 5.339490E-02-1.0283E-03 4.407130E-02-6.9359E-04-1',
     *     ' 21  716.519563 1.054E-23 9.060E-03.0623.0638 2394',
     *     '.84840.690.000000  5  2             Q 66 464 2 2-1',
     *     ' 2 9.066200E-02-3.0329E-03 6.788860E-02-1.6936E-03',
     *     ' 5.418010E-02-1.0735E-03 4.467190E-02-7.2532E-04-1'/
      DATA CPL141/
     1     ' 22  716.552908 3.753E-25 4.503E-03.0643.0678 1985',
     1     '.72400.730.000000  5  2             Q 58 464 2 2-1',
     1     ' 2 6.720480E-02-1.6767E-03 5.063760E-02-9.3556E-04',
     1     ' 4.077580E-02-5.9370E-04 3.398590E-02-4.0228E-04-1',
     2     ' 21  716.748876 1.663E-23 8.965E-03.0628.0647 2292',
     2     '.64530.700.000000  5  2             Q 64 464 2 2-1',
     2     ' 2 9.255220E-02-3.1756E-03 6.919380E-02-1.7701E-03',
     2     ' 5.515380E-02-1.1207E-03 4.542850E-02-7.5690E-04-1',
     3     ' 22  716.878488 5.611E-25 4.503E-03.0647.0689 1895',
     3     '.94450.740.000000  5  2             Q 56 464 2 2-1',
     3     ' 2 6.896890E-02-1.7884E-03 5.166329E-02-9.9438E-04',
     3     ' 4.136470E-02-6.2911E-04 3.428880E-02-4.2514E-04-1',
     4     ' 21  716.974215 2.582E-23 8.870E-03.0633.0657 2193',
     4     '.54540.710.000000  5  2             Q 62 464 2 2-1',
     4     ' 2 9.464389E-02-3.3325E-03 7.064850E-02-1.8530E-03',
     4     ' 5.624710E-02-1.1713E-03 4.628520E-02-7.9024E-04-1'/
      DATA CPL145/
     5     ' 22  717.194520 8.252E-25 4.503E-03.0652.0700 1809',
     5     '.27430.750.000000  5  2             Q 54 464 2 2-1',
     5     ' 2 7.103980E-02-1.9092E-03 5.301270E-02-1.0579E-03',
     5     ' 4.228770E-02-6.6755E-04 3.492840E-02-4.5020E-04-1',
     6     ' 21  717.195286 3.948E-23 8.784E-03.0638.0667 2097',
     6     '.54980.720.000000  5  2             Q 60 464 2 2-1',
     6     ' 2 9.685650E-02-3.5025E-03 7.219029E-02-1.9423E-03',
     6     ' 5.740930E-02-1.2254E-03 4.719910E-02-8.2566E-04-1',
     7     ' 21  717.411804 5.940E-23 8.699E-03.0643.0678 2004',
     7     '.66040.730.000000  5  2             Q 58 464 2 2-1',
     7     ' 2 9.922510E-02-3.6889E-03 7.383480E-02-2.0397E-03',
     7     ' 5.864430E-02-1.2839E-03 4.816890E-02-8.6360E-04-1',
     8     ' 22  717.500804 1.194E-24 4.504E-03.0656.0712 1725',
     8     '.71460.750.000000  5  2             Q 52 464 2 2-1',
     8     ' 2 7.318740E-02-2.0359E-03 5.445180E-02-1.1238E-03',
     8     ' 4.331470E-02-7.0691E-04 3.568110E-02-4.7559E-04-1'/
      DATA CPL149/
     9     ' 21  717.623497 8.796E-23 8.617E-03.0647.0689 1914',
     9     '.87870.740.000000  5  2             Q 56 464 2 2-1',
     9     ' 2 1.017588E-01-3.8921E-03 7.558980E-02-2.1450E-03',
     9     ' 5.995990E-02-1.3468E-03 4.919980E-02-9.0405E-04-1',
     *     ' 22  717.797147 1.699E-24 4.504E-03.0660.0725 1645',
     *     '.26680.760.000000  5  2             Q 50 464 2 2-1',
     *     ' 2 7.544030E-02-2.1740E-03 5.597410E-02-1.1947E-03',
     *     ' 4.441710E-02-7.4884E-04 3.650790E-02-5.0233E-04-1',
     1     ' 21  717.830102 1.282E-22 8.541E-03.0652.0700 1828',
     1     '.20630.750.000000  5  2             Q 54 464 2 2-1',
     1     ' 2 1.043289E-01-4.1088E-03 7.735520E-02-2.2563E-03',
     1     ' 6.127290E-02-1.4124E-03 5.022420E-02-9.4589E-04-1',
     2     ' 21  718.031368 1.838E-22 8.466E-03.0656.0712 1744',
     2     '.64450.750.000000  5  2             Q 52 464 2 2-1',
     2     ' 2 1.070121E-01-4.3477E-03 7.916740E-02-2.3776E-03',
     2     ' 6.260280E-02-1.4834E-03 5.124340E-02-9.9060E-04-1'/
      DATA CPL153/
     3     ' 22  718.083367 2.377E-24 4.503E-03.0663.0738 1567',
     3     '.93240.770.000000  5  2             Q 48 464 2 2-1',
     3     ' 2 7.788170E-02-2.3286E-03 5.762640E-02-1.2733E-03',
     3     ' 4.562220E-02-7.9487E-04 3.742050E-02-5.3140E-04-1',
     4     ' 21  718.227057 2.592E-22 8.392E-03.0660.0725 1664',
     4     '.19470.760.000000  5  2             Q 50 464 2 2-1',
     4     ' 2 1.102465E-01-4.6324E-03 8.138260E-02-2.5228E-03',
     4     ' 6.424470E-02-1.5687E-03 5.251870E-02-1.0445E-03-1',
     5     ' 22  718.359291 3.270E-24 4.503E-03.0666.0751 1493',
     5     '.71260.770.000000  5  2             Q 46 464 2 2-1',
     5     ' 2 8.071569E-02-2.5067E-03 5.957510E-02-1.3636E-03',
     5     ' 4.706650E-02-8.4744E-04 3.853850E-02-5.6436E-04-1',
     6     ' 21  718.416936 3.597E-22 8.325E-03.0663.0738 1586',
     6     '.85830.770.000000  5  2             Q 48 464 2 2-1',
     6     ' 2 1.136668E-01-4.9378E-03 8.373820E-02-2.6772E-03',
     6     ' 6.600230E-02-1.6584E-03 5.389279E-02-1.1007E-03-1'/
      DATA CPL157/
     7     ' 21  718.600788 4.909E-22 8.258E-03.0666.0751 1512',
     7     '.63660.770.000000  5  2             Q 46 464 2 2-1',
     7     ' 2 1.170429E-01-5.2550E-03 8.601059E-02-2.8332E-03',
     7     ' 6.766370E-02-1.7463E-03 5.516550E-02-1.1539E-03-1',
     8     ' 22  718.624753 4.423E-24 4.503E-03.0668.0765 1422',
     8     '.60860.780.000000  5  2             Q 44 465 2 2-1',
     8     ' 2 8.358610E-02-2.6898E-03 6.153550E-02-1.4537E-03',
     8     ' 4.851600E-02-8.9822E-04 3.965780E-02-5.9505E-04-1',
     9     ' 21  718.778403 6.588E-22 8.195E-03.0668.0765 1441',
     9     '.53080.780.000000  5  2             Q 44 465 2 2-1',
     9     ' 2 1.210235E-01-5.6217E-03 8.872240E-02-3.0117E-03',
     9     ' 6.966700E-02-1.8457E-03 5.671770E-02-1.2132E-03-1',
     *     ' 22  718.879598 5.881E-24 4.503E-03.0671.0780 1354',
     *     '.62180.780.000000  5  2             Q 42 465 2 2-1',
     *     ' 2 8.661120E-02-2.8773E-03 6.358039E-02-1.5414E-03',
     *     ' 5.001230E-02-9.4474E-04 4.080440E-02-6.2109E-04-1'/
      DATA CPL161/
     1     ' 21  718.949582 8.695E-22 8.135E-03.0671.0780 1373',
     1     '.54210.780.000000  5  2             Q 42 465 2 2-1',
     1     ' 2 1.248195E-01-5.9652E-03 9.123790E-02-3.1668E-03',
     1     ' 7.147790E-02-1.9244E-03 5.808530E-02-1.2545E-03-1',
     2     ' 21  719.114135 1.128E-21 8.078E-03.0673.0795 1308',
     2     '.67180.780.000000  5  2             Q 40 465 2 2-1',
     2     ' 2 1.294020E-01-6.2158E-03 9.431890E-02-3.2432E-03',
     2     ' 7.372950E-02-1.9370E-03 5.981170E-02-1.2400E-03-1',
     3     ' 22  719.123678 7.684E-24 4.502E-03.0673.0795 1289',
     3     '.75310.780.000000  5  2             Q 40 465 2 2-1',
     3     ' 2 8.986770E-02-3.0069E-03 6.577610E-02-1.5840E-03',
     3     ' 5.161910E-02-9.5497E-04 4.203550E-02-6.1724E-04-1',
     4     ' 21  719.271883 1.438E-21 8.022E-03.0675.0810 1246',
     4     '.92050.780.000000  5  2             Q 38 465 2 2-1',
     4     ' 2 1.341990E-01-6.9127E-03 9.751039E-02-3.6185E-03',
     4     ' 7.604089E-02-2.1706E-03 6.156930E-02-1.3983E-03-1'/
      DATA CPL165/
     5     ' 22  719.356853 9.866E-24 4.503E-03.0675.0810 1228',
     5     '.00350.780.000000  5  2             Q 38 465 2 2-1',
     5     ' 2 9.332569E-02-3.3510E-03 6.808750E-02-1.7690E-03',
     5     ' 5.329740E-02-1.0700E-03 4.331600E-02-6.9491E-04-1',
     6     ' 21  719.422656 1.802E-21 7.972E-03.0677.0826 1188',
     6     '.28980.780.000000  5  2             Q 36 465 2 2-1',
     6     ' 2 1.389830E-01-7.1973E-03 1.006525E-01-3.6884E-03',
     6     ' 7.828210E-02-2.1650E-03 6.325150E-02-1.3627E-03-1',
     7     ' 21  719.566293 2.218E-21 7.924E-03.0679.0843 1132',
     7     '.78030.780.000000  5  2             Q 34 465 2 2-1',
     7     ' 2 1.440660E-01-7.6545E-03 1.039181E-01-3.8590E-03',
     7     ' 8.057009E-02-2.2272E-03 6.493630E-02-1.3767E-03-1',
     8     ' 22  719.578992 1.244E-23 4.502E-03.0677.0826 1169',
     8     '.37430.780.000000  5  2             Q 36 465 2 2-1',
     8     ' 2 9.698519E-02-3.5144E-03 7.051460E-02-1.8178E-03',
     8     ' 5.504720E-02-1.0770E-03 4.463940E-02-6.8451E-04-1'/
      DATA CPL169/
     9     ' 21  719.702644 2.681E-21 7.881E-03.0682.0860 1080',
     9     '.39320.780.000000  5  2             Q 32 465 2 2-1',
     9     ' 2 1.494610E-01-8.3243E-03 1.073228E-01-4.1488E-03',
     9     ' 8.290230E-02-2.3673E-03 6.661460E-02-1.4468E-03-1',
     *     ' 22  719.789971 1.541E-23 4.504E-03.0679.0843 1113',
     *     '.86630.780.000000  5  2             Q 34 465 2 2-1',
     *     ' 2 1.005537E-01-3.7426E-03 7.280520E-02-1.9045E-03',
     *     ' 5.664620E-02-1.1098E-03 4.581200E-02-6.9332E-04-1',
     1     ' 21  719.831565 3.180E-21 7.838E-03.0686.0878 1031',
     1     '.12920.780.000000  5  2             Q 30 465 2 2-1',
     1     ' 2 1.558050E-01-8.9053E-03 1.113892E-01-4.3364E-03',
     1     ' 8.574410E-02-2.4121E-03 6.870370E-02-1.4322E-03-1',
     2     ' 21  719.952924 3.700E-21 7.797E-03.0690.0897  984',
     2     '.98900.780.000000  5  2             Q 28 465 2 2-1',
     2     ' 2 1.614730E-01-8.9419E-03 1.147835E-01-4.1240E-03',
     2     ' 8.794499E-02-2.1473E-03 7.019740E-02-1.1704E-03-1'/
      DATA CPL173/
     3     ' 22  719.989673 1.873E-23 4.503E-03.0682.0860 1061',
     3     '.48050.780.000000  5  2             Q 32 465 2 2-1',
     3     ' 2 1.046682E-01-4.0936E-03 7.545330E-02-2.0601E-03',
     3     ' 5.850000E-02-1.1875E-03 4.717700E-02-7.3389E-04-1',
     4     ' 21  720.066596 4.222E-21 7.762E-03.0695.0916  941',
     4     '.97370.780.000000  5  2             Q 26 465 2 2-1',
     4     ' 2 1.678820E-01-9.6879E-03 1.185977E-01-4.3486E-03',
     4     ' 9.040070E-02-2.1901E-03 7.184710E-02-1.1418E-03-1',
     5     ' 21  720.172468 4.718E-21 7.727E-03.0701.0935  902',
     5     '.08380.780.000000  5  2             Q 24 465 2 2-1',
     5     ' 2 1.743560E-01-9.6145E-03 1.222780E-01-3.9500E-03',
     5     ' 9.264190E-02-1.7423E-03 7.325239E-02-7.1746E-04-1',
     6     ' 22  720.177991 2.234E-23 4.503E-03.0686.0878 1012',
     6     '.21780.780.000000  5  2             Q 30 465 2 2-1',
     6     ' 2 1.090856E-01-4.3859E-03 7.827040E-02-2.1572E-03',
     6     ' 6.045650E-02-1.2133E-03 4.860570E-02-7.2974E-04-1'/
      DATA CPL177/
     7     ' 21  720.270432 5.160E-21 7.695E-03.0708.0956  865',
     7     '.32000.770.000000  5  2             Q 22 465 2 2-1',
     7     ' 2 1.811810E-01-8.9716E-03 1.259908E-01-3.0953E-03',
     7     ' 9.477130E-02-9.3032E-04 7.447830E-02 7.9618E-07-1',
     8     ' 22  720.354822 2.613E-23 4.503E-03.0690.0897  966',
     8     '.07890.780.000000  5  2             Q 28 465 2 2-1',
     8     ' 2 1.136772E-01-4.4613E-03 8.113560E-02-2.0879E-03',
     8     ' 6.240389E-02-1.1070E-03 4.999670E-02-6.1834E-04-1',
     9     ' 21  720.360391 5.517E-21 7.667E-03.0717.0977  831',
     9     '.68290.760.000000  5  2             Q 20 465 2 2-1',
     9     ' 2 1.868100E-01-8.5502E-03 1.284634E-01-2.3335E-03',
     9     ' 9.570209E-02-1.7355E-04 7.457450E-02 6.7579E-04-1',
     *     ' 21  720.442256 5.756E-21 7.641E-03.0728.0998  801',
     *     '.17320.740.000000  5  2             Q 18 465 2 2-1',
     *     ' 2 1.922960E-01-5.2611E-03 1.304680E-01 4.7493E-04',
     *     ' 9.604270E-02 2.1339E-03 7.404929E-02 2.5707E-03-1'/
      DATA CPL181/
     1     ' 21  720.515946 5.847E-21 7.618E-03.0740.1020  773',
     1     '.79120.710.000000  5  2             Q 16 465 2 2-1',
     1     ' 2 1.957410E-01-1.0498E-03 1.303770E-01 3.9495E-03',
     1     ' 9.436959E-02 4.9235E-03 7.163000E-02 4.8188E-03-1',
     2     ' 22  720.520072 2.995E-23 4.503E-03.0695.0916  923',
     2     '.06470.780.000000  5  2             Q 26 465 2 2-1',
     2     ' 2 1.183806E-01-4.8312E-03 8.399169E-02-2.2001E-03',
     2     ' 6.428760E-02-1.1289E-03 5.130060E-02-6.0466E-04-1',
     3     ' 21  720.581388 5.766E-21 7.597E-03.0754.1043  749',
     3     '.53750.690.000000  5  2             Q 14 465 2 2-1',
     3     ' 2 1.939860E-01 1.0416E-02 1.255592E-01 1.2266E-02',
     3     ' 8.840390E-02 1.1224E-02 6.531590E-02 9.7422E-03-1',
     4     ' 21  720.638520 5.496E-21 7.579E-03.0771.1067  728',
     4     '.41240.690.000000  5  2             Q 12 465 2 2-1',
     4     ' 2 1.870180E-01 3.1290E-02 1.158144E-01 2.6628E-02',
     4     ' 7.788300E-02 2.1772E-02 5.482620E-02 1.7819E-02-1'/
      DATA CPL185/
     5     ' 22  720.673654 3.362E-23 4.502E-03.0701.0935  883',
     5     '.17590.780.000000  5  2             Q 24 465 2 2-1',
     5     ' 2 1.227538E-01-4.7939E-03 8.643050E-02-2.0046E-03',
     5     ' 6.573319E-02-9.0973E-04 5.217160E-02-3.9702E-04-1',
     6     ' 21  720.687286 5.029E-21 7.564E-03.0790.1091  710',
     6     '.41630.700.000000  5  2             Q 10 465 2 2-1',
     6     ' 2 1.585090E-01 8.7192E-02 8.787480E-02 6.3050E-02',
     6     ' 5.152030E-02 4.7761E-02 3.030950E-02 3.7406E-02-1',
     7     ' 21  720.727639 4.369E-21 7.551E-03.0811.1116  695',
     7     '.54960.710.000000  5  2             Q  8 465 2 2-1',
     7     ' 2 9.608299E-02 2.1607E-01 3.160300E-02 1.4365E-01',
     7     ' 6.649499E-04 1.0379E-01-1.578460E-02 7.8936E-02-1',
     8     ' 21  720.759540 3.533E-21 7.542E-03.0836.1141  683',
     8     '.81240.730.000000  5  2             Q  6 465 2 2-1',
     8     ' 2-7.450300E-02 5.8986E-01-1.121445E-01 3.6877E-01',
     8     '-1.243398E-01 2.5652E-01-1.264484E-01 1.9046E-01-1'/
      DATA CPL189/
     9     ' 21  720.782958 2.548E-21 7.535E-03.0863.1190  675',
     9     '.20500.740.000000  5  2             Q  4 465 2 2-1',
     9     ' 2-6.337760E-01 1.9115E+00-5.628870E-01 1.1329E+00',
     9     '-5.056220E-01 7.6077E-01-4.582500E-01 5.5268E-01-1',
     *     ' 21  720.797871 1.453E-21 7.531E-03.0893.1228  669',
     *     '.72750.750.000000  5  2             Q  2 465 2 2-1',
     *     ' 2-4.101890E+00-5.5910E+00-3.252210E+00-3.5814E+00',
     *     '-2.723500E+00-2.5454E+00-2.359110E+00-1.9280E+00-1',
     1     ' 22  720.815488 3.693E-23 4.503E-03.0708.0956  846',
     1     '.41300.770.000000  5  2             Q 22 465 2 2-1',
     1     ' 2 1.277770E-01-4.5231E-03 8.922550E-02-1.6129E-03',
     1     ' 6.738810E-02-5.3026E-04 5.317260E-02-5.8263E-05-1',
     2     ' 22  720.945500 3.963E-23 4.503E-03.0717.0977  812',
     2     '.77690.760.000000  5  2             Q 20 465 2 2-1',
     2     ' 2 1.319890E-01-4.3334E-03 9.115729E-02-1.2470E-03',
     2     ' 6.820190E-02-1.6272E-04 5.337540E-02 2.7130E-04-1'/
      DATA CPL193/
     3     ' 22  721.063622 4.149E-23 4.503E-03.0728.0998  782',
     3     '.26790.740.000000  5  2             Q 18 465 2 2-1',
     3     ' 2 1.356030E-01-2.7440E-03 9.238839E-02 1.1248E-04',
     3     ' 6.829810E-02 9.5528E-04 5.288530E-02 1.1899E-03-1',
     4     ' 22  721.169795 4.228E-23 4.503E-03.0740.1020  754',
     4     '.88670.710.000000  5  2             Q 16 465 2 2-1',
     4     ' 2 1.386190E-01-7.6398E-04 9.278880E-02 1.7699E-03',
     4     ' 6.750510E-02 2.2948E-03 5.151250E-02 2.2737E-03-1',
     5     ' 22  721.263964 4.181E-23 4.503E-03.0754.1043  730',
     5     '.63360.690.000000  5  2             Q 14 465 2 2-1',
     5     ' 2 1.378780E-01 4.8391E-03 8.982220E-02 5.8427E-03',
     5     ' 6.367660E-02 5.3847E-03 4.739800E-02 4.6910E-03-1',
     6     ' 22  721.346082 3.994E-23 4.502E-03.0771.1067  709',
     6     '.50910.690.000000  5  2             Q 12 465 2 2-1',
     6     ' 2 1.319890E-01 1.5053E-02 8.217950E-02 1.2858E-02',
     6     ' 5.561530E-02 1.0533E-02 3.944590E-02 8.6300E-03-1'/
      DATA CPL197/
     7     ' 22  721.416107 3.662E-23 4.502E-03.0790.1091  691',
     7     '.51350.700.000000  5  2             Q 10 465 2 2-1',
     7     ' 2 1.136798E-01 4.2236E-02 6.395220E-02 3.0605E-02',
     7     ' 3.828890E-02 2.3214E-02 2.327000E-02 1.8199E-02-1',
     8     ' 22  721.474005 3.187E-23 4.502E-03.0811.1116  676',
     8     '.64720.710.000000  5  2             Q  8 465 2 2-1',
     8     ' 2 6.788730E-02 1.0503E-01 2.289690E-02 6.9849E-02',
     8     ' 1.291992E-03 5.0482E-02-1.020968E-02 3.8401E-02-1',
     9     ' 22  721.519745 2.581E-23 4.503E-03.0836.1141  664',
     9     '.91030.730.000000  5  2             Q  6 465 2 2-1',
     9     ' 2-4.760730E-02 2.8782E-01-7.463690E-02 1.8005E-01',
     9     '-8.363289E-02 1.2531E-01-8.544900E-02 9.3084E-02-1',
     *     ' 22  721.553307 1.863E-23 4.503E-03.0863.1190  656',
     *     '.30310.740.000000  5  2             Q  4 465 2 2-1',
     *     ' 2-4.419350E-01 9.2862E-01-3.923530E-01 5.5039E-01',
     *     '-3.523260E-01 3.6960E-01-3.192540E-01 2.6851E-01-1'/
      DATA CPL201/
     1     ' 22  721.574672 1.063E-23 4.503E-03.0893.1228  650',
     1     '.82580.750.000000  5  2             Q  2 465 2 2-1',
     1     ' 2-2.852330E+00-2.7054E+00-2.261350E+00-1.7328E+00',
     1     '-1.893580E+00-1.2315E+00-1.640210E+00-9.3270E-01-1',
     2     ' 21  735.940173 1.115E-25 5.002E-03.0615.0625 3223',
     2     '.66380.680.000000  8  4             Q 69 464 2 2-1',
     2     ' 2 9.022520E-02-3.0728E-03 7.330310E-02-2.0419E-03',
     2     ' 6.255080E-02-1.4957E-03 5.468190E-02-1.1486E-03-1',
     3     ' 21  736.252851 1.793E-25 4.922E-03.0620.0633 3116',
     3     '.70190.690.000000  8  4             Q 67 464 2 2-1',
     3     ' 2 7.312370E-02-1.8191E-03 5.683080E-02-1.0497E-03',
     3     ' 4.699109E-02-6.8853E-04 4.008160E-02-4.8173E-04-1',
     4     ' 21  736.558189 2.840E-25 4.847E-03.0626.0642 3012',
     4     '.84300.700.000000  8  4             Q 65 464 2 2-1',
     4     ' 2 7.174440E-02-1.8531E-03 5.457140E-02-1.0399E-03',
     4     ' 4.430920E-02-6.6290E-04 3.720340E-02-4.5068E-04-1'/
      DATA CPL205/
     5     ' 21  736.856025 4.429E-25 4.776E-03.0631.0652 2912',
     5     '.08910.710.000000  8  4             Q 63 464 2 2-1',
     5     ' 2 7.287019E-02-1.9608E-03 5.487170E-02-1.0940E-03',
     5     ' 4.413110E-02-6.9325E-04 3.672370E-02-4.6849E-04-1',
     6     ' 21  737.146205 6.798E-25 4.707E-03.0636.0662 2814',
     6     '.44190.720.000000  8  4             Q 61 464 2 2-1',
     6     ' 2 7.470319E-02-2.0838E-03 5.594940E-02-1.1592E-03',
     6     ' 4.475510E-02-7.3287E-04 3.704740E-02-4.9433E-04-1',
     7     ' 21  737.428578 1.027E-24 4.640E-03.0640.0672 2719',
     7     '.90310.730.000000  8  4             Q 59 464 2 2-1',
     7     ' 2 7.679230E-02-2.2156E-03 5.731310E-02-1.2290E-03',
     7     ' 4.568850E-02-7.7538E-04 3.768830E-02-5.2225E-04-1',
     8     ' 21  737.702997 1.527E-24 4.576E-03.0645.0683 2628',
     8     '.47390.730.000000  8  4             Q 57 464 2 2-1',
     8     ' 2 7.896589E-02-2.3581E-03 5.877950E-02-1.3042E-03',
     8     ' 4.673890E-02-8.2098E-04 3.846180E-02-5.5212E-04-1'/
      DATA CPL209/
     9     ' 21  737.969324 2.235E-24 4.515E-03.0650.0694 2540',
     9     '.15620.740.000000  8  4             Q 55 464 2 2-1',
     9     ' 2 8.156589E-02-2.5251E-03 6.056960E-02-1.3916E-03',
     9     ' 4.805970E-02-8.7381E-04 3.946930E-02-5.8652E-04-1',
     *     ' 21  738.227421 3.219E-24 4.457E-03.0654.0706 2454',
     *     '.95170.750.000000  8  4             Q 53 464 2 2-1',
     *     ' 2 8.414769E-02-2.6997E-03 6.235450E-02-1.4826E-03',
     *     ' 4.938700E-02-9.2832E-04 4.049630E-02-6.2176E-04-1',
     1     ' 21  738.477158 4.561E-24 4.400E-03.0658.0718 2372',
     1     '.86130.760.000000  8  4             Q 51 464 2 2-1',
     1     ' 2 8.705840E-02-2.8991E-03 6.437080E-02-1.5856E-03',
     1     ' 5.089240E-02-9.8948E-04 4.166760E-02-6.6092E-04-1',
     2     ' 21  738.718411 6.359E-24 4.346E-03.0661.0731 2293',
     2     '.88700.760.000000  8  4             Q 49 464 2 2-1',
     2     ' 2 9.002500E-02-3.1102E-03 6.641570E-02-1.6932E-03',
     2     ' 5.241600E-02-1.0525E-03 4.285060E-02-7.0073E-04-1'/
      DATA CPL213/
     3     ' 21  738.951057 8.721E-24 4.294E-03.0664.0744 2218',
     3     '.02950.770.000000  8  4             Q 47 464 2 2-1',
     3     ' 2 9.320220E-02-3.3429E-03 6.859840E-02-1.8104E-03',
     3     ' 5.403710E-02-1.1203E-03 4.411029E-02-7.4292E-04-1',
     4     ' 21  739.174982 1.176E-23 4.244E-03.0667.0758 2145',
     4     '.29050.770.000000  8  4             Q 45 465 2 2-1',
     4     ' 2 9.656010E-02-3.5928E-03 7.089030E-02-1.9335E-03',
     4     ' 5.573229E-02-1.1898E-03 4.542200E-02-7.8503E-04-1',
     5     ' 21  739.390076 1.561E-23 4.199E-03.0670.0772 2075',
     5     '.67110.780.000000  8  4             Q 43 465 2 2-1',
     5     ' 2 1.000467E-01-3.8513E-03 7.324850E-02-2.0561E-03',
     5     ' 5.746260E-02-1.2560E-03 4.675060E-02-8.2291E-04-1',
     6     ' 21  739.596230 2.036E-23 4.154E-03.0672.0787 2009',
     6     '.17260.780.000000  8  4             Q 41 465 2 2-1',
     6     ' 2 1.038362E-01-4.0735E-03 7.580040E-02-2.1455E-03',
     6     ' 5.932679E-02-1.2934E-03 4.817929E-02-8.3608E-04-1'/
      DATA CPL217/
     7     ' 21  739.793346 2.611E-23 4.112E-03.0674.0802 1945',
     7     '.79600.780.000000  8  4             Q 39 465 2 2-1',
     7     ' 2 1.078103E-01-4.3689E-03 7.844719E-02-2.2786E-03',
     7     ' 6.124170E-02-1.3608E-03 4.963140E-02-8.7161E-04-1',
     8     ' 21  739.981327 3.291E-23 4.072E-03.0676.0818 1885',
     8     '.54240.780.000000  8  4             Q 37 465 2 2-1',
     8     ' 2 1.120626E-01-4.8374E-03 8.127079E-02-2.5187E-03',
     8     ' 6.328010E-02-1.5031E-03 5.117580E-02-9.6325E-04-1',
     9     ' 21  740.160081 4.075E-23 4.033E-03.0678.0835 1828',
     9     '.41270.780.000000  8  4             Q 35 465 2 2-1',
     9     ' 2 1.164813E-01-4.9880E-03 8.414640E-02-2.5233E-03',
     9     ' 6.531460E-02-1.4609E-03 5.268770E-02-9.0557E-04-1',
     *     ' 21  740.329522 4.957E-23 3.998E-03.0681.0852 1774',
     *     '.40800.780.000000  8  4             Q 33 465 2 2-1',
     *     ' 2 1.214226E-01-5.5925E-03 8.737169E-02-2.8259E-03',
     *     ' 6.760650E-02-1.6362E-03 5.439980E-02-1.0164E-03-1'/
      DATA CPL221/
     1     ' 21  740.489568 5.919E-23 3.964E-03.0684.0869 1723',
     1     '.52930.780.000000  8  4             Q 31 465 2 2-1',
     1     ' 2 1.261923E-01-5.6775E-03 9.037080E-02-2.7504E-03',
     1     ' 6.965660E-02-1.5188E-03 5.587140E-02-8.9220E-04-1',
     2     ' 21  740.565366 1.426E-25 4.961E-03.0618.0629 3169',
     2     '.76860.680.000000  8  4             Q 68 464 2 2-1',
     2     ' 2 6.196060E-01-1.4033E-01 5.030480E-01-9.3046E-02',
     2     ' 4.289610E-01-6.8019E-02 3.748290E-01-5.2164E-02-1',
     3     ' 21  740.607845 2.276E-25 4.886E-03.0623.0638 3064',
     3     '.36280.690.000000  8  4             Q 66 464 2 2-1',
     3     ' 2 4.570930E-01-6.4044E-02 3.525470E-01-3.4976E-02',
     3     ' 2.898220E-01-2.1792E-02 2.460770E-01-1.4501E-02-1',
     4     ' 21  740.640143 6.937E-23 3.932E-03.0687.0887 1675',
     4     '.77730.780.000000  8  4             Q 29 465 2 2-1',
     4     ' 2 1.312610E-01-6.2297E-03 9.350250E-02-2.9748E-03',
     4     ' 7.175609E-02-1.6179E-03 5.734820E-02-9.3471E-04-1'/
      DATA CPL225/
     5     ' 21  740.652160 3.575E-25 4.812E-03.0628.0647 2962',
     5     '.06050.700.000000  8  4             Q 64 464 2 2-1',
     5     ' 2 4.212650E-01-5.9257E-02 3.165500E-01-3.1416E-02',
     5     ' 2.544880E-01-1.8920E-02 2.118740E-01-1.2125E-02-1',
     6     ' 21  740.697985 5.529E-25 4.741E-03.0633.0657 2862',
     6     '.86330.710.000000  8  4             Q 62 464 2 2-1',
     6     ' 2 4.096170E-01-5.8493E-02 3.043300E-01-3.1112E-02',
     6     ' 2.419560E-01-1.8803E-02 1.993160E-01-1.2100E-02-1',
     7     ' 21  740.745010 8.416E-25 4.673E-03.0638.0667 2766',
     7     '.77290.720.000000  8  4             Q 60 464 2 2-1',
     7     ' 2 4.045990E-01-5.8140E-02 2.990130E-01-3.1027E-02',
     7     ' 2.363920E-01-1.8837E-02 1.936090E-01-1.2195E-02-1',
     8     ' 21  740.781174 7.974E-23 3.902E-03.0692.0906 1631',
     8     '.15280.780.000000  8  4             Q 27 465 2 2-1',
     8     ' 2 1.368510E-01-6.5692E-03 9.691110E-02-3.0143E-03',
     8     ' 7.401419E-02-1.5617E-03 5.891730E-02-8.4747E-04-1'/
      DATA CPL229/
     9     ' 21  740.792943 1.261E-24 4.608E-03.0643.0678 2673',
     9     '.79130.730.000000  8  4             Q 58 464 2 2-1',
     9     ' 2 4.009330E-01-5.7845E-02 2.953860E-01-3.0900E-02',
     9     ' 2.327520E-01-1.8803E-02 1.899560E-01-1.2216E-02-1',
     *     ' 21  740.841505 1.860E-24 4.546E-03.0647.0689 2583',
     *     '.91970.740.000000  8  4             Q 56 464 2 2-1',
     *     ' 2 4.000620E-01-5.8157E-02 2.939950E-01-3.1054E-02',
     *     ' 2.310750E-01-1.8914E-02 1.881230E-01-1.2312E-02-1',
     1     ' 21  740.890436 2.699E-24 4.486E-03.0652.0700 2497',
     1     '.15990.750.000000  8  4             Q 54 464 2 2-1',
     1     ' 2 4.015180E-01-5.9059E-02 2.946580E-01-3.1538E-02',
     1     ' 2.312960E-01-1.9226E-02 1.880580E-01-1.2539E-02-1',
     2     ' 21  740.912594 8.983E-23 3.874E-03.0698.0926 1589',
     2     '.65670.780.000000  8  4             Q 25 465 2 2-1',
     2     ' 2 1.419340E-01-6.3337E-03 9.978019E-02-2.6316E-03',
     2     ' 7.574320E-02-1.1795E-03 5.998070E-02-5.0011E-04-1'/
      DATA CPL233/
     3     ' 21  740.939489 3.854E-24 4.428E-03.0656.0712 2413',
     3     '.51370.750.000000  8  4             Q 52 464 2 2-1',
     3     ' 2 4.044560E-01-6.0316E-02 2.964130E-01-3.2161E-02',
     3     ' 2.324010E-01-1.9591E-02 1.887470E-01-1.2777E-02-1',
     4     ' 21  740.988429 5.415E-24 4.372E-03.0660.0725 2332',
     4     '.98220.760.000000  8  4             Q 50 464 2 2-1',
     4     ' 2 4.081740E-01-6.1781E-02 2.986100E-01-3.2844E-02',
     4     ' 2.337920E-01-1.9963E-02 1.896570E-01-1.2998E-02-1',
     5     ' 21  741.034341 9.909E-23 3.847E-03.0704.0946 1551',
     5     '.28960.770.000000  8  4             Q 23 465 2 2-1',
     5     ' 2 1.477450E-01-6.3688E-03 1.030185E-01-2.3889E-03',
     5     ' 7.766330E-02-8.8566E-04 6.114160E-02-2.1625E-04-1',
     6     ' 21  741.037038 7.486E-24 4.319E-03.0663.0738 2255',
     6     '.56690.770.000000  8  4             Q 48 464 2 2-1',
     6     ' 2 4.127890E-01-6.3565E-02 3.013790E-01-3.3673E-02',
     6     ' 2.355860E-01-2.0407E-02 1.908660E-01-1.3257E-02-1'/
      DATA CPL237/
     7     ' 21  741.085109 1.018E-23 4.269E-03.0666.0751 2181',
     7     '.26900.770.000000  8  4             Q 46 464 2 2-1',
     7     ' 2 4.196530E-01-6.5924E-02 3.058770E-01-3.4785E-02',
     7     ' 2.387970E-01-2.1011E-02 1.932840E-01-1.3608E-02-1',
     8     ' 21  741.132448 1.362E-23 4.223E-03.0668.0765 2110',
     8     '.09010.780.000000  8  4             Q 44 465 2 2-1',
     8     ' 2 4.255680E-01-6.8056E-02 3.094130E-01-3.5681E-02',
     8     ' 2.410980E-01-2.1424E-02 1.948440E-01-1.3799E-02-1',
     9     ' 21  741.146357 1.069E-22 3.822E-03.0713.0966 1516',
     9     '.05220.760.000000  8  4             Q 21 465 2 2-1',
     9     ' 2 1.527890E-01-5.7918E-03 1.054547E-01-1.6892E-03',
     9     ' 7.880340E-02-2.3972E-04 6.156410E-02 3.4557E-04-1',
     *     ' 21  741.178873 1.791E-23 4.176E-03.0671.0780 2042',
     *     '.03140.780.000000  8  4             Q 42 465 2 2-1',
     *     ' 2 4.339270E-01-7.0730E-02 3.146650E-01-3.6796E-02',
     *     ' 2.446990E-01-2.1931E-02 1.974310E-01-1.4022E-02-1'/
      DATA CPL241/
     1     ' 21  741.224212 2.316E-23 4.133E-03.0673.0795 1977',
     1     '.09380.780.000000  8  4             Q 40 465 2 2-1',
     1     ' 2 4.461210E-01-7.2097E-02 3.229070E-01-3.6841E-02',
     1     ' 2.507570E-01-2.1549E-02 2.021370E-01-1.3497E-02-1',
     2     ' 21  741.248588 1.127E-22 3.800E-03.0722.0987 1483',
     2     '.94510.750.000000  8  4             Q 19 465 2 2-1',
     2     ' 2 1.571440E-01-4.3365E-03 1.070810E-01-3.4252E-04',
     2     ' 7.911800E-02 9.0000E-04 6.118710E-02 1.2946E-03-1',
     3     ' 21  741.268306 2.944E-23 4.092E-03.0675.0810 1915',
     3     '.27870.780.000000  8  4             Q 38 465 2 2-1',
     3     ' 2 4.545970E-01-7.7984E-02 3.278600E-01-4.0030E-02',
     3     ' 2.538770E-01-2.3560E-02 2.041650E-01-1.4886E-02-1',
     4     ' 21  741.311005 3.677E-23 4.052E-03.0677.0826 1856',
     4     '.58700.780.000000  8  4             Q 36 465 2 2-1',
     4     ' 2 4.675320E-01-7.9943E-02 3.361670E-01-4.0136E-02',
     4     ' 2.597010E-01-2.3071E-02 2.084680E-01-1.4200E-02-1'/
      DATA CPL245/
     5     ' 21  741.340987 1.157E-22 3.775E-03.0734.1009 1454',
     5     '.96880.730.000000  8  4             Q 17 465 2 2-1',
     5     ' 2 1.596400E-01-1.1603E-03 1.068340E-01 2.1979E-03',
     5     ' 7.764380E-02 2.9204E-03 5.913960E-02 2.9193E-03-1',
     6     ' 21  741.352171 4.512E-23 4.015E-03.0679.0843 1801',
     6     '.01980.780.000000  8  4             Q 34 465 2 2-1',
     6     ' 2 4.798430E-01-8.3291E-02 3.436290E-01-4.1105E-02',
     6     ' 2.646020E-01-2.3200E-02 2.118480E-01-1.3991E-02-1',
     7     ' 21  741.391673 5.437E-23 3.980E-03.0682.0860 1748',
     7     '.57800.780.000000  8  4             Q 32 465 2 2-1',
     7     ' 2 4.948060E-01-8.9475E-02 3.529890E-01-4.3724E-02',
     7     ' 2.709850E-01-2.4432E-02 2.164370E-01-1.4585E-02-1',
     8     ' 21  741.423511 1.155E-22 3.752E-03.0747.1032 1429',
     8     '.12370.700.000000  8  4             Q 15 465 2 2-1',
     8     ' 2 1.579370E-01 4.8221E-03 1.028391E-01 6.5901E-03',
     8     ' 7.280130E-02 6.2633E-03 5.404880E-02 5.5350E-03-1'/
      DATA CPL249/
     9     ' 21  741.429392 6.432E-23 3.948E-03.0686.0878 1699',
     9     '.26250.780.000000  8  4             Q 30 465 2 2-1',
     9     ' 2 5.094830E-01-9.3395E-02 3.616990E-01-4.4444E-02',
     9     ' 2.765750E-01-2.4094E-02 2.201680E-01-1.3875E-02-1',
     *     ' 21  741.465217 7.464E-23 3.917E-03.0690.0897 1653',
     *     '.07420.780.000000  8  4             Q 28 465 2 2-1',
     *     ' 2 5.241340E-01-9.2297E-02 3.699150E-01-4.1397E-02',
     *     ' 2.814760E-01-2.0804E-02 2.231580E-01-1.0783E-02-1',
     1     ' 21  741.496119 1.116E-22 3.727E-03.0762.1055 1406',
     1     '.41040.690.000000  8  4             Q 13 465 2 2-1',
     1     ' 2 1.512420E-01 1.8996E-02 9.427210E-02 1.6325E-02',
     1     ' 6.379230E-02 1.3426E-02 4.516720E-02 1.1037E-02-1',
     2     ' 21  741.499044 8.493E-23 3.888E-03.0695.0916 1610',
     2     '.01380.780.000000  8  4             Q 26 465 2 2-1',
     2     ' 2 5.449990E-01-9.9716E-02 3.826550E-01-4.3577E-02',
     2     ' 2.899520E-01-2.1169E-02 2.290990E-01-1.0446E-02-1'/
      DATA CPL253/
     3     ' 21  741.530781 9.468E-23 3.860E-03.0701.0935 1570',
     3     '.08220.780.000000  8  4             Q 24 465 2 2-1',
     3     ' 2 5.596890E-01-9.5359E-02 3.897140E-01-3.7356E-02',
     3     ' 2.932280E-01-1.5161E-02 2.302560E-01-5.0940E-03-1',
     4     ' 21  741.558778 1.038E-22 3.700E-03.0780.1079 1386',
     4     '.82920.690.000000  8  4             Q 11 465 2 2-1',
     4     ' 2 1.279564E-01 4.7939E-02 7.173140E-02 3.5139E-02',
     4     ' 4.264130E-02 2.6809E-02 2.554240E-02 2.1086E-02-1',
     5     ' 21  741.560340 1.033E-22 3.835E-03.0708.0956 1533',
     5     '.27980.770.000000  8  4             Q 22 465 2 2-1',
     5     ' 2 5.755100E-01-8.6311E-02 3.971760E-01-2.7262E-02',
     5     ' 2.965560E-01-6.0146E-03 2.313350E-01 2.8170E-03-1',
     6     ' 21  741.587643 1.101E-22 3.809E-03.0717.0977 1499',
     6     '.60740.760.000000  8  4             Q 20 465 2 2-1',
     6     ' 2 5.945030E-01-7.9882E-02 4.056260E-01-1.7976E-02',
     6     ' 2.998580E-01 2.8337E-03 2.318420E-01 1.0573E-02-1'/
      DATA CPL257/
     7     ' 21  741.611460 9.198E-23 3.663E-03.0800.1103 1370',
     7     '.38050.700.000000  8  4             Q  9 465 2 2-1',
     7     ' 2 7.440159E-02 1.1693E-01 2.420210E-02 7.8256E-02',
     7     ' 2.723240E-05 5.6749E-02-1.292473E-02 4.3246E-02-1',
     8     ' 21  741.612619 1.146E-22 3.787E-03.0728.0998 1469',
     8     '.06560.740.000000  8  4             Q 18 465 2 2-1',
     8     ' 2 5.982470E-01-3.8120E-02 4.012710E-01 1.5179E-02',
     8     ' 2.920450E-01 2.9179E-02 2.225470E-01 3.1781E-02-1',
     9     ' 21  741.635206 1.161E-22 3.765E-03.0740.1020 1441',
     9     '.65480.710.000000  8  4             Q 16 465 2 2-1',
     9     ' 2 6.028100E-01 6.7136E-03 3.958890E-01 5.1272E-02',
     9     ' 2.824380E-01 5.7804E-02 2.111460E-01 5.4666E-02-1',
     *     ' 21  741.654138 7.624E-23 3.605E-03.0823.1128 1357',
     *     '.06450.720.000000  8  4             Q  7 465 2 2-1',
     *     ' 2-6.491030E-02 2.9518E-01-9.238189E-02 1.8525E-01',
     *     '-1.009684E-01 1.2909E-01-1.021657E-01 9.5889E-02-1'/
      DATA CPL261/
     1     ' 21  741.655346 1.141E-22 3.741E-03.0754.1043 1417',
     1     '.37560.690.000000  8  4             Q 14 465 2 2-1',
     1     ' 2 6.053840E-01 1.3380E-01 3.871400E-01 1.4271E-01',
     1     ' 2.690220E-01 1.2683E-01 1.959230E-01 1.0852E-01-1',
     2     ' 21  741.672990 1.082E-22 3.713E-03.0771.1067 1396',
     2     '.22830.690.000000  8  4             Q 12 465 2 2-1',
     2     ' 2 5.354050E-01 3.6899E-01 3.185520E-01 2.9945E-01',
     2     ' 2.042820E-01 2.3969E-01 1.356290E-01 1.9374E-01-1',
     3     ' 21  741.686793 5.676E-23 3.483E-03.0849.1154 1346',
     3     '.88150.740.000000  8  4             Q  5 465 2 2-1',
     3     ' 2-4.931420E-01 7.9608E-01-4.370860E-01 4.7161E-01',
     3     '-3.923790E-01 3.1615E-01-3.557580E-01 2.2901E-01-1',
     4     ' 21  741.688095 9.841E-23 3.683E-03.0790.1091 1378',
     4     '.21330.700.000000  8  4             Q 10 465 2 2-1',
     4     ' 2 4.498130E-01 9.6794E-01 2.335320E-01 6.8916E-01',
     4     ' 1.235390E-01 5.1762E-01 6.027710E-02 4.0323E-01-1'/
      DATA CPL265/
     5     ' 21  741.700625 8.460E-23 3.639E-03.0811.1116 1363',
     5     '.33090.710.000000  8  4             Q  8 465 2 2-1',
     5     ' 2 1.593280E-01 2.3809E+00-1.838850E-02 1.5630E+00',
     5     '-9.934209E-02 1.1203E+00-1.392430E-01 8.4738E-01-1',
     6     ' 21  741.709411 3.334E-23 3.106E-03.0877.1204 1339',
     6     '.83150.750.000000  8  4             Q  3 465 2 2-1',
     6     ' 2-2.662660E+00-2.4121E+00-2.129400E+00-1.5701E+00',
     6     '-1.793870E+00-1.1281E+00-1.560000E+00-8.6066E-01-1',
     7     ' 21  741.710550 6.696E-23 3.557E-03.0836.1141 1351',
     7     '.58140.730.000000  8  4             Q  6 465 2 2-1',
     7     ' 2-5.914870E-01 6.1892E+00-6.393920E-01 3.8290E+00',
     7     '-6.333990E-01 2.6437E+00-6.089720E-01 1.9523E+00-1',
     8     ' 21  741.717846 4.562E-23 3.357E-03.0863.1190 1342',
     8     '.96480.740.000000  8  4             Q  4 465 2 2-1',
     8     ' 2-3.056690E+00 1.6987E+01-2.607930E+00 9.9081E+00',
     8     '-2.288910E+00 6.5692E+00-2.044900E+00 4.7254E+00-1'/
      DATA CPL269/
     9     ' 21  741.722498 1.926E-23 2.484E-03.0893.1228 1337',
     9     '.48160.750.000000  8  4             Q  2 465 2 2-1',
     9     ' 2-1.531530E+01-7.5414E+01-1.215279E+01-4.8229E+01',
     9     '-1.018316E+01-3.4242E+01-8.824659E+00-2.5918E+01-1',
     *     ' 21  791.452892 7.009E-24 6.612E-04.0893.1228 1287',
     *     '.75130.750.000000  8  3             Q  2 465 2 2-1',
     *     ' 2 5.116410E+00-8.6767E+00 4.055350E+00-5.5549E+00',
     *     ' 3.395340E+00-3.9463E+00 2.940600E+00-2.9880E+00-1',
     1     ' 21  791.464831 1.227E-23 6.604E-04.0863.1190 1293',
     1     '.21800.740.000000  8  3             Q  4 465 2 2-1',
     1     ' 2 7.640489E-01 3.0051E+00 6.799909E-01 1.7842E+00',
     1     ' 6.114030E-01 1.2000E+00 5.543720E-01 8.7278E-01-1',
     2     ' 21  791.483619 1.698E-23 6.597E-04.0836.1141 1301',
     2     '.80830.730.000000  8  3             Q  6 465 2 2-1',
     2     ' 2 7.558720E-02 9.1463E-01 1.247753E-01 5.7294E-01',
     2     ' 1.415440E-01 3.9916E-01 1.453790E-01 2.9676E-01-1'/
      DATA CPL273/
     3     ' 21  791.509296 2.093E-23 6.582E-04.0811.1116 1313',
     3     '.52230.710.000000  8  3             Q  8 465 2 2-1',
     3     ' 2-1.342250E-01 3.3225E-01-5.237830E-02 2.2157E-01',
     3     '-1.268878E-02 1.6046E-01 8.724430E-03 1.2226E-01-1',
     4     ' 21  791.541912 2.399E-23 6.564E-04.0790.1091 1328',
     4     '.35950.700.000000  8  3             Q 10 465 2 2-1',
     4     ' 2-2.155530E-01 1.3105E-01-1.253746E-01 9.5594E-02',
     4     '-7.851090E-02 7.2808E-02-5.085989E-02 5.7247E-02-1',
     5     ' 21  791.581533 2.609E-23 6.543E-04.0771.1067 1346',
     5     '.31980.690.000000  8  3             Q 12 465 2 2-1',
     5     ' 2-2.453100E-01 4.4574E-02-1.557400E-01 3.8916E-02',
     5     '-1.077180E-01 3.2197E-02-7.831070E-02 2.6541E-02-1',
     6     ' 21  791.628239 2.722E-23 6.519E-04.0754.1043 1367',
     6     '.40270.690.000000  8  3             Q 14 465 2 2-1',
     6     ' 2-2.538250E-01 1.2476E-02-1.678820E-01 1.6705E-02',
     6     '-1.208935E-01 1.5821E-02-9.148619E-02 1.3968E-02-1'/
      DATA CPL277/
     7     ' 21  791.682121 2.741E-23 6.489E-04.0740.1020 1391',
     7     '.60790.710.000000  8  3             Q 16 465 2 2-1',
     7     ' 2-2.518360E-01-4.4882E-03-1.706770E-01 4.1707E-03',
     7     '-1.257048E-01 6.2223E-03-9.713080E-02 6.4126E-03-1',
     8     ' 21  791.743285 2.678E-23 6.457E-04.0728.0998 1418',
     8     '.93490.740.000000  8  3             Q 18 465 2 2-1',
     8     ' 2-2.451150E-01-1.0301E-02-1.689610E-01-8.4595E-04',
     8     '-1.263184E-01 2.1109E-03-9.891439E-02 3.0579E-03-1',
     9     ' 21  791.811853 2.546E-23 6.423E-04.0717.0977 1449',
     9     '.38320.760.000000  8  3             Q 20 465 2 2-1',
     9     ' 2-2.365090E-01-1.4752E-02-1.651520E-01-4.8327E-03',
     9     '-1.248611E-01-1.2333E-03-9.872329E-02 2.7820E-04-1',
     *     ' 21  791.887956 2.359E-23 6.383E-04.0708.0956 1482',
     *     '.95210.770.000000  8  3             Q 22 465 2 2-1',
     *     ' 2-2.271490E-01-1.4947E-02-1.602640E-01-5.7658E-03',
     *     '-1.222247E-01-2.2577E-03-9.735440E-02-6.7427E-04-1'/
      DATA CPL281/
     1     ' 21  791.971743 2.135E-23 6.340E-04.0701.0935 1519',
     1     '.64120.780.000000  8  3             Q 24 465 2 2-1',
     1     ' 2-2.166710E-01-1.5411E-02-1.541540E-01-6.7707E-03',
     1     '-1.183741E-01-3.3024E-03-9.482979E-02-1.6362E-03-1',
     2     ' 21  792.063371 1.890E-23 6.296E-04.0695.0916 1559',
     2     '.44960.780.000000  8  3             Q 26 465 2 2-1',
     2     ' 2-2.068820E-01-1.5115E-02-1.482520E-01-7.1338E-03',
     2     '-1.145092E-01-3.8225E-03-9.217779E-02-2.1683E-03-1',
     3     ' 21  792.163016 1.637E-23 6.246E-04.0690.0897 1602',
     3     '.37650.780.000000  8  3             Q 28 465 2 2-1',
     3     ' 2-1.968070E-01-1.3721E-02-1.419080E-01-6.6511E-03',
     3     '-1.101698E-01-3.6722E-03-8.905130E-02-2.1575E-03-1',
     4     ' 21  792.270862 1.389E-23 6.193E-04.0686.0878 1648',
     4     '.42110.780.000000  8  3             Q 30 465 2 2-1',
     4     ' 2-1.869140E-01-1.3118E-02-1.354730E-01-6.6366E-03',
     4     '-1.056094E-01-3.8434E-03-8.564790E-02-2.3876E-03-1'/
      DATA CPL285/
     5     ' 21  792.387111 1.155E-23 6.137E-04.0682.0860 1697',
     5     '.58260.780.000000  8  3             Q 32 465 2 2-1',
     5     ' 2-1.777750E-01-1.2004E-02-1.295203E-01-6.2085E-03',
     5     '-1.013831E-01-3.6774E-03-8.249800E-02-2.3388E-03-1',
     6     ' 21  792.511973 9.417E-24 6.076E-04.0679.0843 1749',
     6     '.86010.780.000000  8  3             Q 34 465 2 2-1',
     6     ' 2-1.693900E-01-1.0817E-02-1.239550E-01-5.6627E-03',
     6     '-9.736350E-02-3.3930E-03-7.944690E-02-2.1819E-03-1',
     7     ' 21  792.645675 7.533E-24 6.012E-04.0677.0826 1805',
     7     '.25240.780.000000  8  3             Q 36 465 2 2-1',
     7     ' 2-1.608230E-01-9.8674E-03-1.181492E-01-5.2426E-03',
     7     '-9.309039E-02-3.1869E-03-7.614879E-02-2.0790E-03-1',
     8     ' 21  792.788455 5.915E-24 5.947E-04.0675.0810 1863',
     8     '.75870.780.000000  8  3             Q 38 465 2 2-1',
     8     ' 2-1.528540E-01-9.1400E-03-1.127022E-01-4.9452E-03',
     8     '-8.905649E-02-3.0592E-03-7.301710E-02-2.0309E-03-1'/
      DATA CPL289/
     9     ' 21  792.940562 4.559E-24 5.878E-04.0673.0795 1925',
     9     '.37770.780.000000  8  3             Q 40 465 2 2-1',
     9     ' 2-1.454440E-01-8.0413E-03-1.076049E-01-4.3536E-03',
     9     '-8.526310E-02-2.6924E-03-7.006220E-02-1.7846E-03-1',
     *     ' 21  793.102261 3.451E-24 5.806E-04.0671.0780 1990',
     *     '.10820.780.000000  8  3             Q 42 465 2 2-1',
     *     ' 2-1.381120E-01-7.4499E-03-1.024881E-01-4.0929E-03',
     *     '-8.140209E-02-2.5665E-03-6.702019E-02-1.7244E-03-1',
     1     ' 21  793.273825 2.565E-24 5.730E-04.0668.0765 2057',
     1     '.94900.780.000000  8  3             Q 44 465 2 2-1',
     1     ' 2-1.314430E-01-6.7694E-03-9.786400E-02-3.7518E-03',
     1     '-7.793630E-02-2.3709E-03-6.430840E-02-1.6045E-03-1',
     2     ' 21  793.455544 1.873E-24 5.652E-04.0666.0751 2128',
     2     '.89870.770.000000  8  3             Q 46 464 2 2-1',
     2     ' 2-1.251744E-01-6.1331E-03-9.345569E-02-3.4215E-03',
     2     '-7.460050E-02-2.1744E-03-6.168110E-02-1.4788E-03-1'/
      DATA CPL293/
     3     ' 21  793.647715 1.344E-24 5.572E-04.0663.0738 2202',
     3     '.95630.770.000000  8  3             Q 48 464 2 2-1',
     3     ' 2-1.187953E-01-5.5149E-03-8.897330E-02-3.0939E-03',
     3     '-7.121530E-02-1.9756E-03-5.902260E-02-1.3490E-03-1',
     4     ' 21  793.850651 9.474E-25 5.487E-04.0660.0725 2280',
     4     '.12010.760.000000  8  3             Q 50 464 2 2-1',
     4     ' 2-1.136564E-01-5.0120E-03-8.542559E-02-2.8270E-03',
     4     '-6.859450E-02-1.8134E-03-5.701540E-02-1.2429E-03-1',
     5     ' 21  794.064674 6.564E-25 5.401E-04.0656.0712 2360',
     5     '.38870.750.000000  8  3             Q 52 464 2 2-1',
     5     ' 2-1.083940E-01-4.5349E-03-8.181550E-02-2.5709E-03',
     5     '-6.595290E-02-1.6562E-03-5.501990E-02-1.1396E-03-1',
     6     ' 21  794.290119 4.470E-25 5.311E-04.0652.0700 2443',
     6     '.76050.750.000000  8  3             Q 54 464 2 2-1',
     6     ' 2-1.037816E-01-4.1081E-03-7.880600E-02-2.3418E-03',
     6     '-6.388590E-02-1.5162E-03-5.357170E-02-1.0480E-03-1'/
      DATA CPL297/
     7     ' 21  794.527330 2.992E-25 5.219E-04.0647.0689 2530',
     7     '.23410.740.000000  8  3             Q 56 464 2 2-1',
     7     ' 2-1.003418E-01-3.7461E-03-7.698080E-02-2.1558E-03',
     7     '-6.297980E-02-1.4091E-03-5.324540E-02-9.8286E-04-1',
     8     ' 21  794.776665 1.969E-25 5.125E-04.0643.0678 2619',
     8     '.80760.730.000000  8  3             Q 58 464 2 2-1',
     8     ' 2-1.003717E-01-3.5602E-03-7.855120E-02-2.1121E-03',
     8     '-6.530420E-02-1.4208E-03-5.595850E-02-1.0181E-03-1',
     9     ' 21  795.038490 1.274E-25 5.030E-04.0638.0667 2712',
     9     '.47950.720.000000  8  3             Q 60 464 2 2-1',
     9     ' 2-1.210092E-01-5.6705E-03-9.830080E-02-3.7712E-03',
     9     '-8.385520E-02-2.7622E-03-7.329530E-02-2.1217E-03-1',
     *     ' 21 1932.478947 3.209E-24 2.347E-07.0893.1228    2',
     *     '.34130.750.000000  6  1             Q  2 465 2 2-1',
     *     ' 2 0.254104E+01 0.0000E+00 0.203732E+01 0.0000E+00',
     *     ' 0.172035E+01 0.0000E+00 0.149406E+01 0.0000E+00-1'/
      DATA CPL301/
     1     ' 21 1932.499537 5.633E-24 2.351E-07.0863.1190    7',
     1     '.80430.740.000000  6  1             Q  4 465 2 2-1',
     1     ' 2 0.432113E+00 0.0000E+00 0.375123E+00 0.0000E+00',
     1     ' 0.331785E+00 0.0000E+00 0.298441E+00 0.0000E+00-1',
     2     ' 21 1932.531875 7.823E-24 2.356E-07.0836.1141   16',
     2     '.38900.730.000000  6  1             Q  6 465 2 2-1',
     2     ' 2 0.707673E-01 0.0000E+00 0.865256E-01 0.0000E+00',
     2     ' 0.894494E-01 0.0000E+00 0.889436E-01 0.0000E+00-1',
     3     ' 21 1932.575936 9.692E-24 2.363E-07.0811.1116   28',
     3     '.09510.710.000000  6  1             Q  8 465 2 2-1',
     3     ' 2-0.455414E-01 0.0000E+00-0.881456E-02 0.0000E+00',
     3     ' 0.823076E-02 0.0000E+00 0.179171E-01 0.0000E+00-1',
     4     ' 21 1932.631688 1.118E-23 2.371E-07.0790.1091   42',
     4     '.92250.700.000000  6  1             Q 10 465 2 2-1',
     4     ' 2-0.925533E-01 0.0000E+00-0.490540E-01 0.0000E+00',
     4     '-0.269095E-01 0.0000E+00-0.134153E-01 0.0000E+00-1'/
      DATA CPL305/
     5     ' 21 1932.699087 1.225E-23 2.381E-07.0771.1067   60',
     5     '.87090.690.000000  6  1             Q 12 465 2 2-1',
     5     ' 2-0.112996E+00 0.0000E+00-0.678782E-01 0.0000E+00',
     5     '-0.440151E-01 0.0000E+00-0.291239E-01 0.0000E+00-1',
     6     ' 21 1932.778085 1.290E-23 2.395E-07.0754.1043   81',
     6     '.94010.690.000000  6  1             Q 14 465 2 2-1',
     6     ' 2-0.121438E+00 0.0000E+00-0.768583E-01 0.0000E+00',
     6     '-0.527515E-01 0.0000E+00-0.375267E-01 0.0000E+00-1',
     7     ' 21 1932.868622 1.313E-23 2.409E-07.0740.1020  106',
     7     '.12970.710.000000  6  1             Q 16 465 2 2-1',
     7     ' 2-0.124067E+00 0.0000E+00-0.809305E-01 0.0000E+00',
     7     '-0.572472E-01 0.0000E+00-0.421816E-01 0.0000E+00-1',
     8     ' 21 1932.970629 1.298E-23 2.426E-07.0728.0998  133',
     8     '.43930.740.000000  6  1             Q 18 465 2 2-1',
     8     ' 2-0.123615E+00 0.0000E+00-0.823094E-01 0.0000E+00',
     8     '-0.593723E-01 0.0000E+00-0.447116E-01 0.0000E+00-1'/
      DATA CPL309/
     9     ' 21 1933.084030 1.250E-23 2.444E-07.0717.0977  163',
     9     '.86840.760.000000  6  1             Q 20 465 2 2-1',
     9     ' 2-0.121483E+00 0.0000E+00-0.821391E-01 0.0000E+00',
     9     '-0.600942E-01 0.0000E+00-0.459586E-01 0.0000E+00-1',
     *     ' 21 1933.208741 1.175E-23 2.464E-07.0708.0956  197',
     *     '.41660.770.000000  6  1             Q 22 465 2 2-1',
     *     ' 2-0.118391E+00 0.0000E+00-0.810422E-01 0.0000E+00',
     *     '-0.599595E-01 0.0000E+00-0.464107E-01 0.0000E+00-1',
     1     ' 21 1933.344666 1.080E-23 2.485E-07.0701.0935  234',
     1     '.08330.780.000000  6  1             Q 24 465 2 2-1',
     1     ' 2-0.114989E+00 0.0000E+00-0.795425E-01 0.0000E+00',
     1     '-0.594078E-01 0.0000E+00-0.464480E-01 0.0000E+00-1',
     2     ' 21 1933.491703 9.727E-24 2.511E-07.0695.0916  273',
     2     '.86800.780.000000  6  1             Q 26 465 2 2-1',
     2     ' 2-0.111364E+00 0.0000E+00-0.777423E-01 0.0000E+00',
     2     '-0.585419E-01 0.0000E+00-0.461729E-01 0.0000E+00-1'/
      DATA CPL313/
     3     ' 21 1933.649743 8.582E-24 2.537E-07.0690.0897  316',
     3     '.76980.780.000000  6  1             Q 28 465 2 2-1',
     3     ' 2-0.107716E+00 0.0000E+00-0.758167E-01 0.0000E+00',
     3     '-0.575157E-01 0.0000E+00-0.457211E-01 0.0000E+00-1',
     4     ' 21 1933.818665 7.426E-24 2.565E-07.0686.0878  362',
     4     '.78820.780.000000  6  1             Q 30 465 2 2-1',
     4     ' 2-0.104129E+00 0.0000E+00-0.738507E-01 0.0000E+00',
     4     '-0.564116E-01 0.0000E+00-0.451740E-01 0.0000E+00-1',
     5     ' 21 1933.998342 6.307E-24 2.596E-07.0682.0860  411',
     5     '.92250.780.000000  6  1             Q 32 465 2 2-1',
     5     ' 2-0.100662E+00 0.0000E+00-0.719055E-01 0.0000E+00',
     5     '-0.552880E-01 0.0000E+00-0.445865E-01 0.0000E+00-1',
     6     ' 21 1934.188638 5.258E-24 2.628E-07.0679.0843  464',
     6     '.17170.780.000000  6  1             Q 34 465 2 2-1',
     6     ' 2-0.973473E-01 0.0000E+00-0.700220E-01 0.0000E+00',
     6     '-0.541890E-01 0.0000E+00-0.440037E-01 0.0000E+00-1'/
      DATA CPL317/
     7     ' 21 1934.389410 4.307E-24 2.663E-07.0677.0826  519',
     7     '.53500.780.000000  6  1             Q 36 465 2 2-1',
     7     ' 2-0.942093E-01 0.0000E+00-0.682345E-01 0.0000E+00',
     7     '-0.531528E-01 0.0000E+00-0.434653E-01 0.0000E+00-1',
     8     ' 21 1934.600504 3.467E-24 2.700E-07.0675.0810  578',
     8     '.01160.780.000000  6  1             Q 38 465 2 2-1',
     8     ' 2-0.912641E-01 0.0000E+00-0.665777E-01 0.0000E+00',
     8     '-0.522237E-01 0.0000E+00-0.430215E-01 0.0000E+00-1',
     9     ' 21 1934.821762 2.743E-24 2.739E-07.0673.0795  639',
     9     '.60040.780.000000  6  1             Q 40 465 2 2-1',
     9     ' 2-0.885857E-01 0.0000E+00-0.651347E-01 0.0000E+00',
     9     '-0.514864E-01 0.0000E+00-0.427546E-01 0.0000E+00-1',
     *     ' 21 1935.053015 2.134E-24 2.781E-07.0671.0780  704',
     *     '.30050.780.000000  6  1             Q 42 465 2 2-1',
     *     ' 2-0.862324E-01 0.0000E+00-0.639988E-01 0.0000E+00',
     *     '-0.510490E-01 0.0000E+00-0.427794E-01 0.0000E+00-1'/
      DATA CPL321/
     1     ' 21 1935.294087 1.632E-24 2.824E-07.0668.0765  772',
     1     '.11070.780.000000  6  1             Q 44 465 2 2-1',
     1     ' 2-0.845166E-01 0.0000E+00-0.634903E-01 0.0000E+00',
     1     '-0.512219E-01 0.0000E+00-0.433892E-01 0.0000E+00-1',
     2     ' 21 1935.544794 1.228E-24 2.870E-07.0666.0751  843',
     2     '.03010.770.000000  6  1             Q 46 464 2 2-1',
     2     ' 2-0.841809E-01 0.0000E+00-0.643444E-01 0.0000E+00',
     2     '-0.527002E-01 0.0000E+00-0.452366E-01 0.0000E+00-1',
     3     ' 21 1935.804946 9.094E-25 2.919E-07.0663.0738  917',
     3     '.05730.770.000000  6  1             Q 48 464 2 2-1',
     3     ' 2-0.881561E-01 0.0000E+00-0.691832E-01 0.0000E+00',
     3     '-0.578241E-01 0.0000E+00-0.504333E-01 0.0000E+00-1',
     4     ' 21 1936.074343 6.623E-25 2.970E-07.0660.0725  994',
     4     '.19130.760.000000  6  1             Q 50 464 2 2-1',
     4     ' 2-0.114074E+00 0.0000E+00-0.927106E-01 0.0000E+00',
     4     '-0.792209E-01 0.0000E+00-0.700866E-01 0.0000E+00-1'/
      DATA CPL325/
     5     ' 21 2076.862584 3.891E-23 2.648E-06.0893.1228    2',
     5     '.34130.750.000000  8  1             Q  2 465 2 2-1',
     5     ' 2 0.334269E+01 0.0000E+00 0.267950E+01 0.0000E+00',
     5     ' 0.226224E+01 0.0000E+00 0.196437E+01 0.0000E+00-1',
     6     ' 21 2076.878200 6.819E-23 2.648E-06.0863.1190    7',
     6     '.80430.740.000000  8  1             Q  4 465 2 2-1',
     6     ' 2 0.560214E+00 0.0000E+00 0.486422E+00 0.0000E+00',
     6     ' 0.430180E+00 0.0000E+00 0.386871E+00 0.0000E+00-1',
     7     ' 21 2076.902750 9.445E-23 2.647E-06.0836.1141   16',
     7     '.38900.730.000000  8  1             Q  6 465 2 2-1',
     7     ' 2 0.841097E-01 0.0000E+00 0.106151E+00 0.0000E+00',
     7     ' 0.110859E+00 0.0000E+00 0.110819E+00 0.0000E+00-1',
     8     ' 21 2076.936246 1.166E-22 2.645E-06.0811.1116   28',
     8     '.09510.710.000000  8  1             Q  8 465 2 2-1',
     8     ' 2-0.686817E-01 0.0000E+00-0.191307E-01 0.0000E+00',
     8     ' 0.411843E-02 0.0000E+00 0.174645E-01 0.0000E+00-1'/
      DATA CPL329/
     9     ' 21 2076.978705 1.340E-22 2.645E-06.0790.1091   42',
     9     '.92250.700.000000  8  1             Q 10 465 2 2-1',
     9     ' 2-0.129922E+00 0.0000E+00-0.716192E-01 0.0000E+00',
     9     '-0.417552E-01 0.0000E+00-0.234692E-01 0.0000E+00-1',
     *     ' 21 2077.030150 1.461E-22 2.643E-06.0771.1067   60',
     *     '.87090.690.000000  8  1             Q 12 465 2 2-1',
     *     ' 2-0.156139E+00 0.0000E+00-0.958815E-01 0.0000E+00',
     *     '-0.638563E-01 0.0000E+00-0.437977E-01 0.0000E+00-1',
     1     ' 21 2077.090608 1.529E-22 2.641E-06.0754.1043   81',
     1     '.94010.690.000000  8  1             Q 14 465 2 2-1',
     1     ' 2-0.166581E+00 0.0000E+00-0.107198E+00 0.0000E+00',
     1     '-0.749498E-01 0.0000E+00-0.545200E-01 0.0000E+00-1',
     2     ' 21 2077.160109 1.546E-22 2.640E-06.0740.1020  106',
     2     '.12970.710.000000  8  1             Q 16 465 2 2-1',
     2     ' 2-0.169266E+00 0.0000E+00-0.111976E+00 0.0000E+00',
     2     '-0.803987E-01 0.0000E+00-0.602547E-01 0.0000E+00-1'/
      DATA CPL333/
     3     ' 21 2077.238692 1.517E-22 2.638E-06.0728.0998  133',
     3     '.43930.740.000000  8  1             Q 18 465 2 2-1',
     3     ' 2-0.167956E+00 0.0000E+00-0.113256E+00 0.0000E+00',
     3     '-0.827669E-01 0.0000E+00-0.632275E-01 0.0000E+00-1',
     4     ' 21 2077.326396 1.448E-22 2.635E-06.0717.0977  163',
     4     '.86840.760.000000  8  1             Q 20 465 2 2-1',
     4     ' 2-0.164481E+00 0.0000E+00-0.112535E+00 0.0000E+00',
     4     '-0.833220E-01 0.0000E+00-0.645424E-01 0.0000E+00-1',
     5     ' 21 2077.423268 1.349E-22 2.632E-06.0708.0956  197',
     5     '.41660.770.000000  8  1             Q 22 465 2 2-1',
     5     ' 2-0.159776E+00 0.0000E+00-0.110610E+00 0.0000E+00',
     5     '-0.827556E-01 0.0000E+00-0.648113E-01 0.0000E+00-1',
     6     ' 21 2077.529358 1.228E-22 2.630E-06.0701.0935  234',
     6     '.08330.780.000000  8  1             Q 24 465 2 2-1',
     6     ' 2-0.154573E+00 0.0000E+00-0.108093E+00 0.0000E+00',
     6     '-0.815936E-01 0.0000E+00-0.644975E-01 0.0000E+00-1'/
      DATA CPL337/
     7     ' 21 2077.644721 1.094E-22 2.628E-06.0695.0916  273',
     7     '.86800.780.000000  8  1             Q 26 465 2 2-1',
     7     ' 2-0.149164E+00 0.0000E+00-0.105241E+00 0.0000E+00',
     7     '-0.800623E-01 0.0000E+00-0.638047E-01 0.0000E+00-1',
     8     ' 21 2077.769419 9.536E-23 2.624E-06.0690.0897  316',
     8     '.76980.780.000000  8  1             Q 28 465 2 2-1',
     8     ' 2-0.143664E+00 0.0000E+00-0.102176E+00 0.0000E+00',
     8     '-0.782826E-01 0.0000E+00-0.628497E-01 0.0000E+00-1',
     9     ' 21 2077.903517 8.149E-23 2.620E-06.0686.0878  362',
     9     '.78820.780.000000  8  1             Q 30 465 2 2-1',
     9     ' 2-0.138271E+00 0.0000E+00-0.990751E-01 0.0000E+00',
     9     '-0.764094E-01 0.0000E+00-0.617713E-01 0.0000E+00-1',
     *     ' 21 2078.047084 6.829E-23 2.616E-06.0682.0860  411',
     *     '.92250.780.000000  8  1             Q 32 465 2 2-1',
     *     ' 2-0.133024E+00 0.0000E+00-0.959899E-01 0.0000E+00',
     *     '-0.745005E-01 0.0000E+00-0.606318E-01 0.0000E+00-1'/
      DATA CPL341/
     1     ' 21 2078.200197 5.615E-23 2.612E-06.0679.0843  464',
     1     '.17170.780.000000  8  1             Q 34 465 2 2-1',
     1     ' 2-0.127984E+00 0.0000E+00-0.929855E-01 0.0000E+00',
     1     '-0.726188E-01 0.0000E+00-0.594894E-01 0.0000E+00-1',
     2     ' 21 2078.362935 4.532E-23 2.608E-06.0677.0826  519',
     2     '.53500.780.000000  8  1             Q 36 465 2 2-1',
     2     ' 2-0.123161E+00 0.0000E+00-0.900912E-01 0.0000E+00',
     2     '-0.708025E-01 0.0000E+00-0.583877E-01 0.0000E+00-1',
     3     ' 21 2078.535385 3.591E-23 2.603E-06.0675.0810  578',
     3     '.01160.780.000000  8  1             Q 38 465 2 2-1',
     3     ' 2-0.118611E+00 0.0000E+00-0.873739E-01 0.0000E+00',
     3     '-0.691230E-01 0.0000E+00-0.574006E-01 0.0000E+00-1',
     4     ' 21 2078.717636 2.795E-23 2.598E-06.0673.0795  639',
     4     '.60040.780.000000  8  1             Q 40 465 2 2-1',
     4     ' 2-0.114363E+00 0.0000E+00-0.848875E-01 0.0000E+00',
     4     '-0.676455E-01 0.0000E+00-0.565965E-01 0.0000E+00-1'/
      DATA CPL345/
     5     ' 21 2078.909785 2.138E-23 2.593E-06.0671.0780  704',
     5     '.30050.780.000000  8  1             Q 42 465 2 2-1',
     5     ' 2-0.110540E+00 0.0000E+00-0.827799E-01 0.0000E+00',
     5     '-0.665247E-01 0.0000E+00-0.561305E-01 0.0000E+00-1',
     6     ' 21 2079.111932 1.607E-23 2.588E-06.0668.0765  772',
     6     '.11070.780.000000  8  1             Q 44 465 2 2-1',
     6     ' 2-0.107423E+00 0.0000E+00-0.813610E-01 0.0000E+00',
     6     '-0.660710E-01 0.0000E+00-0.563028E-01 0.0000E+00-1',
     7     ' 21 2079.324185 1.187E-23 2.582E-06.0666.0751  843',
     7     '.03010.770.000000  8  1             Q 46 464 2 2-1',
     7     ' 2-0.105927E+00 0.0000E+00-0.815156E-01 0.0000E+00',
     7     '-0.671114E-01 0.0000E+00-0.578824E-01 0.0000E+00-1',
     8     ' 21 2079.546655 8.621E-24 2.576E-06.0663.0738  917',
     8     '.05730.770.000000  8  1             Q 48 464 2 2-1',
     8     ' 2-0.109302E+00 0.0000E+00-0.861602E-01 0.0000E+00',
     8     '-0.722533E-01 0.0000E+00-0.632271E-01 0.0000E+00-1'/
      DATA CPL349/
     9     ' 21 2079.779459 6.156E-24 2.570E-06.0660.0725  994',
     9     '.19130.760.000000  8  1             Q 50 464 2 2-1',
     9     ' 2-0.137504E+00 0.0000E+00-0.111937E+00 0.0000E+00',
     9     '-0.957878E-01 0.0000E+00-0.849051E-01 0.0000E+00-1',
     *     ' 21 2093.346577 1.252E-24 2.167E-06.0893.1228  669',
     *     '.72750.750.000000 14  2             Q  2 465 2 2-1',
     *     ' 2 0.550811E+01 0.0000E+00 0.559421E+01 0.0000E+00',
     *     ' 0.565187E+01 0.0000E+00 0.569583E+01 0.0000E+00-1',
     1     ' 21 2093.350674 2.962E-24 2.925E-06.0863.1190  675',
     1     '.20500.740.000000 14  2             Q  4 465 2 2-1',
     1     ' 2 0.502300E+01 0.0000E+00 0.508130E+01 0.0000E+00',
     1     ' 0.512090E+01 0.0000E+00 0.515159E+01 0.0000E+00-1',
     2     ' 21 2093.355723 2.166E-24 2.709E-06.0877.1204  672',
     2     '.06760.750.000000 14  2             Q  3 465 2 2-1',
     2     ' 2 0.587007E+01 0.0000E+00 0.615352E+01 0.0000E+00',
     2     ' 0.633967E+01 0.0000E+00 0.647732E+01 0.0000E+00-1'/
      DATA CPL353/
     3     ' 21 2093.357109 4.342E-24 3.095E-06.0836.1141  683',
     3     '.81240.730.000000 14  2             Q  6 465 2 2-1',
     3     ' 2-0.318873E+01 0.0000E+00-0.314904E+01 0.0000E+00',
     3     '-0.312127E+01 0.0000E+00-0.310060E+01 0.0000E+00-1',
     4     ' 21 2093.365874 5.475E-24 3.160E-06.0811.1116  695',
     4     '.54960.710.000000 14  2             Q  8 465 2 2-1',
     4     ' 2 0.182957E+01 0.0000E+00 0.186720E+01 0.0000E+00',
     4     ' 0.189339E+01 0.0000E+00 0.191374E+01 0.0000E+00-1',
     5     ' 21 2093.372081 3.684E-24 3.034E-06.0849.1154  679',
     5     '.09900.740.000000 14  2             Q  5 465 2 2-1',
     5     ' 2-0.170774E+01 0.0000E+00-0.159666E+01 0.0000E+00',
     5     '-0.151831E+01 0.0000E+00-0.145872E+01 0.0000E+00-1',
     6     ' 21 2093.376963 6.354E-24 3.191E-06.0790.1091  710',
     6     '.41630.700.000000 14  2             Q 10 465 2 2-1',
     6     ' 2-0.192337E+00 0.0000E+00-0.164584E+00 0.0000E+00',
     6     '-0.144764E+00 0.0000E+00-0.129397E+00 0.0000E+00-1'/
      DATA CPL357/
     7     ' 21 2093.390364 6.968E-24 3.208E-06.0771.1067  728',
     7     '.41240.690.000000 14  2             Q 12 465 2 2-1',
     7     ' 2 0.134226E+01 0.0000E+00 0.137092E+01 0.0000E+00',
     7     ' 0.139108E+01 0.0000E+00 0.140688E+01 0.0000E+00-1',
     8     ' 21 2093.395717 4.940E-24 3.134E-06.0823.1128  689',
     8     '.25530.720.000000 14  2             Q  7 465 2 2-1',
     8     ' 2-0.228838E+01 0.0000E+00-0.228712E+01 0.0000E+00',
     8     '-0.227952E+01 0.0000E+00-0.227095E+01 0.0000E+00-1',
     9     ' 21 2093.406065 7.319E-24 3.219E-06.0754.1043  749',
     9     '.53750.690.000000 14  2             Q 14 465 2 2-1',
     9     ' 2 0.129775E+00 0.0000E+00 0.150342E+00 0.0000E+00',
     9     ' 0.165280E+00 0.0000E+00 0.177065E+00 0.0000E+00-1',
     *     ' 21 2093.424049 7.418E-24 3.226E-06.0740.1020  773',
     *     '.79120.710.000000 14  2             Q 16 465 2 2-1',
     *     ' 2 0.188039E+01 0.0000E+00 0.190981E+01 0.0000E+00',
     *     ' 0.193010E+01 0.0000E+00 0.194593E+01 0.0000E+00-1'/
      DATA CPL361/
     1     ' 21 2093.426641 5.949E-24 3.178E-06.0800.1103  702',
     1     '.53640.700.000000 14  2             Q  9 465 2 2-1',
     1     ' 2-0.278532E+01 0.0000E+00-0.295669E+01 0.0000E+00',
     1     '-0.306712E+01 0.0000E+00-0.314606E+01 0.0000E+00-1',
     2     ' 21 2093.444299 7.292E-24 3.231E-06.0728.0998  801',
     2     '.17320.740.000000 14  2             Q 18 465 2 2-1',
     2     ' 2 0.183964E+00 0.0000E+00 0.200116E+00 0.0000E+00',
     2     ' 0.211974E+00 0.0000E+00 0.221433E+00 0.0000E+00-1',
     3     ' 21 2093.464864 6.697E-24 3.201E-06.0780.1079  718',
     3     '.94200.690.000000 14  2             Q 11 465 2 2-1',
     3     ' 2 0.549655E+00 0.0000E+00 0.815931E+00 0.0000E+00',
     3     ' 0.101176E+01 0.0000E+00 0.116395E+01 0.0000E+00-1',
     4     ' 21 2093.466793 6.975E-24 3.235E-06.0717.0977  831',
     4     '.68290.760.000000 14  2             Q 20 465 2 2-1',
     4     ' 2-0.163992E+01 0.0000E+00-0.164404E+01 0.0000E+00',
     4     '-0.164500E+01 0.0000E+00-0.164507E+01 0.0000E+00-1'/
      DATA CPL365/
     5     ' 21 2093.491505 6.506E-24 3.238E-06.0708.0956  865',
     5     '.32000.770.000000 14  2             Q 22 465 2 2-1',
     5     ' 2 0.196033E+00 0.0000E+00 0.209554E+00 0.0000E+00',
     5     ' 0.219510E+00 0.0000E+00 0.227495E+00 0.0000E+00-1',
     6     ' 21 2093.510401 7.180E-24 3.214E-06.0762.1055  738',
     6     '.47180.690.000000 14  2             Q 13 465 2 2-1',
     6     ' 2-0.516206E+00 0.0000E+00-0.484283E+00 0.0000E+00',
     6     '-0.454408E+00 0.0000E+00-0.427736E+00 0.0000E+00-1',
     7     ' 21 2093.518407 5.929E-24 3.240E-06.0701.0935  902',
     7     '.08380.780.000000 14  2             Q 24 465 2 2-1',
     7     ' 2-0.252674E+00 0.0000E+00-0.246618E+00 0.0000E+00',
     7     '-0.241457E+00 0.0000E+00-0.237025E+00 0.0000E+00-1',
     8     ' 21 2093.547466 5.285E-24 3.241E-06.0695.0916  941',
     8     '.97370.780.000000 14  2             Q 26 465 2 2-1',
     8     ' 2 0.209494E+00 0.0000E+00 0.221770E+00 0.0000E+00',
     8     ' 0.230746E+00 0.0000E+00 0.237935E+00 0.0000E+00-1'/
      DATA CPL369/
     9     ' 21 2093.563268 7.405E-24 3.223E-06.0747.1032  761',
     9     '.12550.700.000000 14  2             Q 15 465 2 2-1',
     9     ' 2-0.520714E+00 0.0000E+00-0.537589E+00 0.0000E+00',
     9     '-0.545325E+00 0.0000E+00-0.548400E+00 0.0000E+00-1',
     *     ' 21 2093.578645 4.613E-24 3.242E-06.0690.0897  984',
     *     '.98900.780.000000 14  2             Q 28 465 2 2-1',
     *     ' 2-0.705079E-01 0.0000E+00-0.641853E-01 0.0000E+00',
     *     '-0.590465E-01 0.0000E+00-0.546902E-01 0.0000E+00-1',
     1     ' 21 2093.611903 3.946E-24 3.243E-06.0686.0878 1031',
     1     '.12920.780.000000 14  2             Q 30 465 2 2-1',
     1     ' 2 0.246471E+00 0.0000E+00 0.259078E+00 0.0000E+00',
     1     ' 0.268114E+00 0.0000E+00 0.275276E+00 0.0000E+00-1',
     2     ' 21 2093.623487 7.389E-24 3.229E-06.0734.1009  786',
     2     '.90280.730.000000 14  2             Q 17 465 2 2-1',
     2     ' 2-0.445895E+00 0.0000E+00-0.484620E+00 0.0000E+00',
     2     '-0.512173E+00 0.0000E+00-0.532538E+00 0.0000E+00-1'/
      DATA CPL373/
     3     ' 21 2093.647193 3.311E-24 3.245E-06.0682.0860 1080',
     3     '.39320.780.000000 14  2             Q 32 465 2 2-1',
     3     ' 2-0.134499E-01 0.0000E+00-0.751778E-02 0.0000E+00',
     3     '-0.277403E-02 0.0000E+00 0.123292E-02 0.0000E+00-1',
     4     ' 21 2093.684462 2.725E-24 3.245E-06.0679.0843 1132',
     4     '.78030.780.000000 14  2             Q 34 465 2 2-1',
     4     ' 2 0.366267E+00 0.0000E+00 0.382820E+00 0.0000E+00',
     4     ' 0.394258E+00 0.0000E+00 0.403101E+00 0.0000E+00-1',
     5     ' 21 2093.691080 7.164E-24 3.233E-06.0722.0987  815',
     5     '.80330.750.000000 14  2             Q 19 465 2 2-1',
     5     ' 2-0.376982E+00 0.0000E+00-0.431750E+00 0.0000E+00',
     5     '-0.476995E+00 0.0000E+00-0.514822E+00 0.0000E+00-1',
     6     ' 21 2093.723653 2.202E-24 3.246E-06.0677.0826 1188',
     6     '.28980.780.000000 14  2             Q 36 465 2 2-1',
     6     ' 2 0.896224E-02 0.0000E+00 0.145087E-01 0.0000E+00',
     6     ' 0.188912E-01 0.0000E+00 0.225827E-01 0.0000E+00-1'/
      DATA CPL377/
     7     ' 21 2093.764700 1.747E-24 3.247E-06.0675.0810 1246',
     7     '.92050.780.000000 14  2             Q 38 465 2 2-1',
     7     ' 2 0.147797E+01 0.0000E+00 0.153764E+01 0.0000E+00',
     7     ' 0.157661E+01 0.0000E+00 0.160539E+01 0.0000E+00-1',
     8     ' 21 2093.766074 6.767E-24 3.236E-06.0713.0966  847',
     8     '.82630.760.000000 14  2             Q 21 465 2 2-1',
     8     ' 2-0.406611E+00 0.0000E+00-0.548721E+00 0.0000E+00',
     8     '-0.687176E+00 0.0000E+00-0.818311E+00 0.0000E+00-1',
     9     ' 21 2093.807531 1.361E-24 3.246E-06.0673.0795 1308',
     9     '.67180.780.000000 14  2             Q 40 465 2 2-1',
     9     ' 2 0.177138E-01 0.0000E+00 0.229598E-01 0.0000E+00',
     9     ' 0.270597E-01 0.0000E+00 0.304998E-01 0.0000E+00-1',
     *     ' 21 2093.848496 6.239E-24 3.238E-06.0704.0946  882',
     *     '.97150.770.000000 14  2             Q 23 465 2 2-1',
     *     ' 2-0.185583E+00 0.0000E+00-0.170725E+00 0.0000E+00',
     *     '-0.148667E+00 0.0000E+00-0.122321E+00 0.0000E+00-1'/
      DATA CPL381/
     1     ' 21 2093.852068 1.042E-24 3.246E-06.0671.0780 1373',
     1     '.54210.780.000000 14  2             Q 42 465 2 2-1',
     1     ' 2-0.499541E+00 0.0000E+00-0.518065E+00 0.0000E+00',
     1     '-0.529316E+00 0.0000E+00-0.537056E+00 0.0000E+00-1',
     2     ' 21 2093.898222 7.843E-25 3.247E-06.0668.0765 1441',
     2     '.53080.780.000000 14  2             Q 44 465 2 2-1',
     2     ' 2 0.194289E-01 0.0000E+00 0.244160E-01 0.0000E+00',
     2     ' 0.282790E-01 0.0000E+00 0.315083E-01 0.0000E+00-1',
     3     ' 21 2093.938379 5.625E-24 3.241E-06.0698.0926  921',
     3     '.23820.780.000000 14  2             Q 25 465 2 2-1',
     3     ' 2-0.172010E+00 0.0000E+00-0.176786E+00 0.0000E+00',
     3     '-0.177242E+00 0.0000E+00-0.174493E+00 0.0000E+00-1',
     4     ' 21 2093.945898 5.801E-25 3.247E-06.0666.0751 1512',
     4     '.63660.770.000000 14  2             Q 46 464 2 2-1',
     4     ' 2-0.207907E+00 0.0000E+00-0.215524E+00 0.0000E+00',
     4     '-0.219870E+00 0.0000E+00-0.222637E+00 0.0000E+00-1'/
      DATA CPL385/
     5     ' 21 2093.994990 4.218E-25 3.247E-06.0663.0738 1586',
     5     '.85830.770.000000 14  2             Q 48 464 2 2-1',
     5     ' 2 0.165425E-01 0.0000E+00 0.212260E-01 0.0000E+00',
     5     ' 0.248374E-01 0.0000E+00 0.278500E-01 0.0000E+00-1',
     6     ' 21 2094.035758 4.963E-24 3.242E-06.0692.0906  962',
     6     '.62570.780.000000 14  2             Q 27 465 2 2-1',
     6     ' 2-0.148625E+00 0.0000E+00-0.155540E+00 0.0000E+00',
     6     '-0.159654E+00 0.0000E+00-0.161494E+00 0.0000E+00-1',
     7     ' 21 2094.045381 3.016E-25 3.247E-06.0660.0725 1664',
     7     '.19470.760.000000 14  2             Q 50 464 2 2-1',
     7     ' 2-0.147075E+00 0.0000E+00-0.153143E+00 0.0000E+00',
     7     '-0.156589E+00 0.0000E+00-0.158746E+00 0.0000E+00-1',
     8     ' 21 2094.096945 2.121E-25 3.248E-06.0656.0712 1744',
     8     '.64450.750.000000 14  2             Q 52 464 2 2-1',
     8     ' 2 0.102460E-01 0.0000E+00 0.144881E-01 0.0000E+00',
     8     ' 0.177658E-01 0.0000E+00 0.205038E-01 0.0000E+00-1'/
      DATA CPL389/
     9     ' 21 2094.140671 4.290E-24 3.243E-06.0687.0887 1007',
     9     '.13350.780.000000 14  2             Q 29 465 2 2-1',
     9     ' 2-0.128505E+00 0.0000E+00-0.134705E+00 0.0000E+00',
     9     '-0.138850E+00 0.0000E+00-0.141231E+00 0.0000E+00-1',
     *     ' 21 2094.149544 1.467E-25 3.248E-06.0652.0700 1828',
     *     '.20630.750.000000 14  2             Q 54 464 2 2-1',
     *     ' 2-0.147568E+00 0.0000E+00-0.155506E+00 0.0000E+00',
     *     '-0.160291E+00 0.0000E+00-0.163475E+00 0.0000E+00-1',
     1     ' 21 2094.203028 9.980E-26 3.248E-06.0647.0689 1914',
     1     '.87870.740.000000 14  2             Q 56 464 2 2-1',
     1     ' 2 0.168911E-02 0.0000E+00 0.528872E-02 0.0000E+00',
     1     ' 0.809817E-02 0.0000E+00 0.104587E-01 0.0000E+00-1',
     2     ' 21 2094.253158 3.635E-24 3.244E-06.0684.0869 1054',
     2     '.76070.780.000000 14  2             Q 31 465 2 2-1',
     2     ' 2-0.111961E+00 0.0000E+00-0.116345E+00 0.0000E+00',
     2     '-0.118343E+00 0.0000E+00-0.118054E+00 0.0000E+00-1'/
      DATA CPL393/
     3     ' 21 2094.257233 6.679E-26 3.248E-06.0643.0678 2004',
     3     '.66040.730.000000 14  2             Q 58 464 2 2-1',
     3     ' 2-0.296171E+00 0.0000E+00-0.319425E+00 0.0000E+00',
     3     '-0.334544E+00 0.0000E+00-0.345393E+00 0.0000E+00-1',
     4     ' 21 2094.311982 4.398E-26 3.248E-06.0638.0667 2097',
     4     '.54980.720.000000 14  2             Q 60 464 2 2-1',
     4     ' 2-0.803024E-02 0.0000E+00-0.531853E-02 0.0000E+00',
     4     '-0.316156E-02 0.0000E+00-0.133010E-02 0.0000E+00-1',
     5     ' 21 2094.367083 2.850E-26 3.249E-06.0633.0657 2193',
     5     '.54540.710.000000 14  2             Q 62 464 2 2-1',
     5     ' 2 0.152109E+00 0.0000E+00 0.171513E+00 0.0000E+00',
     5     ' 0.184870E+00 0.0000E+00 0.194945E+00 0.0000E+00-1',
     6     ' 21 2094.373264 3.022E-24 3.245E-06.0681.0852 1105',
     6     '.50660.780.000000 14  2             Q 33 465 2 2-1',
     6     ' 2-0.992314E-01 0.0000E+00-0.104757E+00 0.0000E+00',
     6     '-0.110366E+00 0.0000E+00-0.116433E+00 0.0000E+00-1'/
      DATA CPL397/
     7     ' 21 2094.422328 1.817E-26 3.249E-06.0628.0647 2292',
     7     '.64530.700.000000 14  2             Q 64 464 2 2-1',
     7     ' 2-0.189671E-01 0.0000E+00-0.175942E-01 0.0000E+00',
     7     '-0.164500E-01 0.0000E+00-0.154455E-01 0.0000E+00-1',
     8     ' 21 2094.477492 1.140E-26 3.249E-06.0623.0638 2394',
     8     '.84840.690.000000 14  2             Q 66 464 2 2-1',
     8     ' 2 0.218180E-01 0.0000E+00 0.278284E-01 0.0000E+00',
     8     ' 0.320475E-01 0.0000E+00 0.352860E-01 0.0000E+00-1',
     9     ' 21 2094.501037 2.465E-24 3.245E-06.0678.0835 1159',
     9     '.37040.780.000000 14  2             Q 35 465 2 2-1',
     9     ' 2-0.881065E-01 0.0000E+00-0.922439E-01 0.0000E+00',
     9     '-0.959486E-01 0.0000E+00-0.994810E-01 0.0000E+00-1',
     *     ' 21 2094.532335 7.038E-27 3.249E-06.0618.0629 2500',
     *     '.15230.680.000000 14  2             Q 68 464 2 2-1',
     *     ' 2-0.371104E-01 0.0000E+00-0.387906E-01 0.0000E+00',
     *     '-0.399199E-01 0.0000E+00-0.407098E-01 0.0000E+00-1'/
      DATA CPL401/
     1     ' 21 2094.586596 4.277E-27 3.249E-06.0613.0620 2608',
     1     '.55590.670.000000 14  2             Q 70 464 2 2-1',
     1     ' 2-0.200521E-01 0.0000E+00-0.198152E-01 0.0000E+00',
     1     '-0.196703E-01 0.0000E+00-0.195280E-01 0.0000E+00-1',
     2     ' 21 2094.636526 1.974E-24 3.245E-06.0676.0818 1216',
     2     '.35100.780.000000 14  2             Q 37 465 2 2-1',
     2     ' 2-0.789190E-01 0.0000E+00-0.823537E-01 0.0000E+00',
     2     '-0.853472E-01 0.0000E+00-0.881427E-01 0.0000E+00-1',
     3     ' 21 2094.779786 1.553E-24 3.246E-06.0674.0802 1276',
     3     '.44760.780.000000 14  2             Q 39 465 2 2-1',
     3     ' 2-0.711470E-01 0.0000E+00-0.740625E-01 0.0000E+00',
     3     '-0.765227E-01 0.0000E+00-0.787385E-01 0.0000E+00-1',
     4     ' 21 2094.930874 1.200E-24 3.245E-06.0672.0787 1339',
     4     '.65920.780.000000 14  2             Q 41 465 2 2-1',
     4     ' 2-0.645127E-01 0.0000E+00-0.670426E-01 0.0000E+00',
     4     '-0.691302E-01 0.0000E+00-0.709688E-01 0.0000E+00-1'/
      DATA CPL405/
     5     ' 21 2095.089850 9.117E-25 3.247E-06.0670.0772 1405',
     5     '.98470.780.000000 14  2             Q 43 465 2 2-1',
     5     ' 2-0.587818E-01 0.0000E+00-0.610124E-01 0.0000E+00',
     5     '-0.628169E-01 0.0000E+00-0.643749E-01 0.0000E+00-1',
     6     ' 21 2095.256781 6.806E-25 3.247E-06.0667.0758 1475',
     6     '.42310.770.000000 14  2             Q 45 465 2 2-1',
     6     ' 2-0.537867E-01 0.0000E+00-0.557801E-01 0.0000E+00',
     6     '-0.573626E-01 0.0000E+00-0.587029E-01 0.0000E+00-1',
     7     ' 21 2095.431732 4.994E-25 3.247E-06.0664.0744 1547',
     7     '.97310.770.000000 14  2             Q 47 464 2 2-1',
     7     ' 2-0.494031E-01 0.0000E+00-0.512056E-01 0.0000E+00',
     7     '-0.526105E-01 0.0000E+00-0.537775E-01 0.0000E+00-1',
     8     ' 21 2095.614777 3.603E-25 3.247E-06.0661.0731 1623',
     8     '.63350.760.000000 14  2             Q 49 464 2 2-1',
     8     ' 2-0.455338E-01 0.0000E+00-0.471802E-01 0.0000E+00',
     8     '-0.484409E-01 0.0000E+00-0.494671E-01 0.0000E+00-1'/
      DATA CPL409/
     9     ' 21 2095.805991 2.557E-25 3.248E-06.0658.0718 1702',
     9     '.40330.760.000000 14  2             Q 51 464 2 2-1',
     9     ' 2-0.420894E-01 0.0000E+00-0.436063E-01 0.0000E+00',
     9     '-0.447474E-01 0.0000E+00-0.456571E-01 0.0000E+00-1',
     *     ' 21 2096.005452 1.784E-25 3.247E-06.0654.0706 1784',
     *     '.28080.750.000000 14  2             Q 53 464 2 2-1',
     *     ' 2-0.390122E-01 0.0000E+00-0.404201E-01 0.0000E+00',
     *     '-0.414606E-01 0.0000E+00-0.422721E-01 0.0000E+00-1',
     1     ' 21 2096.213245 1.225E-25 3.249E-06.0650.0694 1869',
     1     '.26490.740.000000 14  2             Q 55 464 2 2-1',
     1     ' 2-0.362495E-01 0.0000E+00-0.375636E-01 0.0000E+00',
     1     '-0.385180E-01 0.0000E+00-0.392445E-01 0.0000E+00-1',
     2     ' 21 2096.429457 8.271E-26 3.248E-06.0645.0683 1957',
     2     '.35420.730.000000 14  2             Q 57 464 2 2-1',
     2     ' 2-0.337447E-01 0.0000E+00-0.349768E-01 0.0000E+00',
     2     '-0.358555E-01 0.0000E+00-0.365086E-01 0.0000E+00-1'/
      DATA CPL413/
     3     ' 21 2096.654179 5.495E-26 3.248E-06.0640.0672 2048',
     3     '.54710.730.000000 14  2             Q 59 464 2 2-1',
     3     ' 2-0.314797E-01 0.0000E+00-0.326384E-01 0.0000E+00',
     3     '-0.334496E-01 0.0000E+00-0.340370E-01 0.0000E+00-1',
     4     ' 21 2096.887508 3.592E-26 3.249E-06.0636.0662 2142',
     4     '.84200.720.000000 14  2             Q 61 464 2 2-1',
     4     ' 2-0.294183E-01 0.0000E+00-0.305101E-01 0.0000E+00',
     4     '-0.312607E-01 0.0000E+00-0.317897E-01 0.0000E+00-1',
     5     ' 21 2097.129543 2.310E-26 3.248E-06.0631.0652 2240',
     5     '.23750.710.000000 14  2             Q 63 464 2 2-1',
     5     ' 2-0.275364E-01 0.0000E+00-0.285685E-01 0.0000E+00',
     5     '-0.292663E-01 0.0000E+00-0.297456E-01 0.0000E+00-1',
     6     ' 21 2097.380390 1.462E-26 3.248E-06.0626.0642 2340',
     6     '.73220.700.000000 14  2             Q 65 464 2 2-1',
     6     ' 2-0.258387E-01 0.0000E+00-0.268271E-01 0.0000E+00',
     6     '-0.274876E-01 0.0000E+00-0.279328E-01 0.0000E+00-1'/
      DATA CPL417/
     7     ' 21 2097.640158 9.110E-27 3.249E-06.0620.0633 2444',
     7     '.32370.690.000000 14  2             Q 67 464 2 2-1',
     7     ' 2-0.244277E-01 0.0000E+00-0.254228E-01 0.0000E+00',
     7     '-0.260875E-01 0.0000E+00-0.265334E-01 0.0000E+00-1',
     8     ' 21 2097.908961 5.585E-27 3.249E-06.0615.0625 2551',
     8     '.01120.680.000000 14  2             Q 69 464 2 2-1',
     8     ' 2-0.244783E-01 0.0000E+00-0.256690E-01 0.0000E+00',
     8     '-0.264679E-01 0.0000E+00-0.270131E-01 0.0000E+00-1',
     9     ' 21 2128.359397 3.522E-25 3.731E-06.0660.0725 1664',
     9     '.19470.760.000000 15  2             Q 50 464 2 2-1',
     9     ' 2 0.335885E+00 0.0000E+00 0.274557E+00 0.0000E+00',
     9     ' 0.235617E+00 0.0000E+00 0.208927E+00 0.0000E+00-1',
     *     ' 21 2128.448849 4.890E-25 3.704E-06.0663.0738 1586',
     *     '.85830.770.000000 15  2             Q 48 464 2 2-1',
     *     ' 2 0.248574E+00 0.0000E+00 0.196359E+00 0.0000E+00',
     *     ' 0.165060E+00 0.0000E+00 0.144512E+00 0.0000E+00-1'/
      DATA CPL421/
     1     ' 21 2128.537756 6.680E-25 3.679E-06.0666.0751 1512',
     1     '.63660.770.000000 15  2             Q 46 464 2 2-1',
     1     ' 2 0.231097E+00 0.0000E+00 0.177494E+00 0.0000E+00',
     1     ' 0.146034E+00 0.0000E+00 0.125741E+00 0.0000E+00-1',
     2     ' 21 2128.625721 8.972E-25 3.654E-06.0668.0765 1441',
     2     '.53080.780.000000 15  2             Q 44 465 2 2-1',
     2     ' 2 0.227107E+00 0.0000E+00 0.171176E+00 0.0000E+00',
     2     ' 0.138545E+00 0.0000E+00 0.117611E+00 0.0000E+00-1',
     3     ' 21 2128.712372 1.185E-24 3.631E-06.0671.0780 1373',
     3     '.54210.780.000000 15  2             Q 42 465 2 2-1',
     3     ' 2 0.227604E+00 0.0000E+00 0.169293E+00 0.0000E+00',
     3     ' 0.135321E+00 0.0000E+00 0.113538E+00 0.0000E+00-1',
     4     ' 21 2128.797352 1.538E-24 3.608E-06.0673.0795 1308',
     4     '.67180.780.000000 15  2             Q 40 465 2 2-1',
     4     ' 2 0.229981E+00 0.0000E+00 0.169319E+00 0.0000E+00',
     4     ' 0.133996E+00 0.0000E+00 0.111320E+00 0.0000E+00-1'/
      DATA CPL425/
     5     ' 21 2128.880325 1.963E-24 3.588E-06.0675.0810 1246',
     5     '.92050.780.000000 15  2             Q 38 465 2 2-1',
     5     ' 2 0.233459E+00 0.0000E+00 0.170385E+00 0.0000E+00',
     5     ' 0.133690E+00 0.0000E+00 0.110095E+00 0.0000E+00-1',
     6     ' 21 2128.960972 2.461E-24 3.568E-06.0677.0826 1188',
     6     '.28980.780.000000 15  2             Q 36 465 2 2-1',
     6     ' 2 0.237800E+00 0.0000E+00 0.172171E+00 0.0000E+00',
     6     ' 0.134048E+00 0.0000E+00 0.109499E+00 0.0000E+00-1',
     7     ' 21 2129.038995 3.031E-24 3.549E-06.0679.0843 1132',
     7     '.78030.780.000000 15  2             Q 34 465 2 2-1',
     7     ' 2 0.242811E+00 0.0000E+00 0.174444E+00 0.0000E+00',
     7     ' 0.134820E+00 0.0000E+00 0.109278E+00 0.0000E+00-1',
     8     ' 21 2129.114112 3.665E-24 3.532E-06.0682.0860 1080',
     8     '.39320.780.000000 15  2             Q 32 465 2 2-1',
     8     ' 2 0.248359E+00 0.0000E+00 0.177042E+00 0.0000E+00',
     8     ' 0.135831E+00 0.0000E+00 0.109249E+00 0.0000E+00-1'/
      DATA CPL429/
     9     ' 21 2129.186058 4.350E-24 3.515E-06.0686.0878 1031',
     9     '.12920.780.000000 15  2             Q 30 465 2 2-1',
     9     ' 2 0.254408E+00 0.0000E+00 0.179889E+00 0.0000E+00',
     9     ' 0.136985E+00 0.0000E+00 0.109304E+00 0.0000E+00-1',
     *     ' 21 2129.254587 5.064E-24 3.499E-06.0690.0897  984',
     *     '.98900.780.000000 15  2             Q 28 465 2 2-1',
     *     ' 2 0.260963E+00 0.0000E+00 0.182948E+00 0.0000E+00',
     *     ' 0.138224E+00 0.0000E+00 0.109373E+00 0.0000E+00-1',
     1     ' 21 2129.319469 5.780E-24 3.485E-06.0695.0916  941',
     1     '.97370.780.000000 15  2             Q 26 465 2 2-1',
     1     ' 2 0.267769E+00 0.0000E+00 0.185984E+00 0.0000E+00',
     1     ' 0.139328E+00 0.0000E+00 0.109250E+00 0.0000E+00-1',
     2     ' 21 2129.380490 6.462E-24 3.471E-06.0701.0935  902',
     2     '.08380.780.000000 15  2             Q 24 465 2 2-1',
     2     ' 2 0.274695E+00 0.0000E+00 0.188839E+00 0.0000E+00',
     2     ' 0.140139E+00 0.0000E+00 0.108776E+00 0.0000E+00-1'/
      DATA CPL433/
     3     ' 21 2129.437455 7.071E-24 3.459E-06.0708.0956  865',
     3     '.32000.770.000000 15  2             Q 22 465 2 2-1',
     3     ' 2 0.281375E+00 0.0000E+00 0.191176E+00 0.0000E+00',
     3     ' 0.140343E+00 0.0000E+00 0.107656E+00 0.0000E+00-1',
     4     ' 21 2129.490183 7.563E-24 3.448E-06.0717.0977  831',
     4     '.68290.760.000000 15  2             Q 20 465 2 2-1',
     4     ' 2 0.287340E+00 0.0000E+00 0.192559E+00 0.0000E+00',
     4     ' 0.139544E+00 0.0000E+00 0.105522E+00 0.0000E+00-1',
     5     ' 21 2129.538510 7.893E-24 3.438E-06.0728.0998  801',
     5     '.17320.740.000000 15  2             Q 18 465 2 2-1',
     5     ' 2 0.291524E+00 0.0000E+00 0.192077E+00 0.0000E+00',
     5     ' 0.136947E+00 0.0000E+00 0.101670E+00 0.0000E+00-1',
     6     ' 21 2129.582286 8.021E-24 3.429E-06.0740.1020  773',
     6     '.79120.710.000000 15  2             Q 16 465 2 2-1',
     6     ' 2 0.292180E+00 0.0000E+00 0.188251E+00 0.0000E+00',
     6     ' 0.131264E+00 0.0000E+00 0.949415E-01 0.0000E+00-1'/
      DATA CPL437/
     7     ' 21 2129.621381 7.912E-24 3.421E-06.0754.1043  749',
     7     '.53750.690.000000 15  2             Q 14 465 2 2-1',
     7     ' 2 0.286180E+00 0.0000E+00 0.178505E+00 0.0000E+00',
     7     ' 0.120305E+00 0.0000E+00 0.834352E-01 0.0000E+00-1',
     8     ' 21 2129.655676 7.543E-24 3.414E-06.0771.1067  728',
     8     '.41240.690.000000 15  2             Q 12 465 2 2-1',
     8     ' 2 0.267032E+00 0.0000E+00 0.157596E+00 0.0000E+00',
     8     ' 0.996385E-01 0.0000E+00 0.632776E-01 0.0000E+00-1',
     9     ' 21 2129.685071 6.903E-24 3.408E-06.0790.1091  710',
     9     '.41630.700.000000 15  2             Q 10 465 2 2-1',
     9     ' 2 0.220923E+00 0.0000E+00 0.114526E+00 0.0000E+00',
     9     ' 0.600854E-01 0.0000E+00 0.265908E-01 0.0000E+00-1',
     *     ' 21 2129.709477 5.999E-24 3.403E-06.0811.1116  695',
     *     '.54960.710.000000 15  2             Q  8 465 2 2-1',
     *     ' 2 0.114451E+00 0.0000E+00 0.230035E-01 0.0000E+00',
     *     '-0.201768E-01 0.0000E+00-0.453158E-01 0.0000E+00-1'/
      DATA CPL441/
     1     ' 21 2129.728825 4.851E-24 3.399E-06.0836.1141  683',
     1     '.81240.730.000000 15  2             Q  6 465 2 2-1',
     1     ' 2-0.151953E+00 0.0000E+00-0.194589E+00 0.0000E+00',
     1     '-0.205129E+00 0.0000E+00-0.206808E+00 0.0000E+00-1',
     2     ' 21 2129.743058 3.499E-24 3.396E-06.0863.1190  675',
     2     '.20500.740.000000 15  2             Q  4 465 2 2-1',
     2     ' 2-0.989306E+00 0.0000E+00-0.856316E+00 0.0000E+00',
     2     '-0.755677E+00 0.0000E+00-0.678356E+00 0.0000E+00-1',
     3     ' 21 2129.752135 1.995E-24 3.394E-06.0893.1228  669',
     3     '.72750.750.000000 15  2             Q  2 465 2 2-1',
     3     ' 2-0.595900E+01 0.0000E+00-0.470076E+01 0.0000E+00',
     3     '-0.390828E+01 0.0000E+00-0.333855E+01 0.0000E+00-1'/
C                                                                        LN33830
      END                                                                LN33840
      BLOCK DATA                                                         LN18880
C                                                                        LN18890
      IMPLICIT REAL*8           (V)                                     !LN18900
C                                                                        LN18910
      COMMON /IFIL/ IRD,IPR,IPU,NWDR,LRC,ILNGTH,INLTE,IER,IPUOUT         LN18920
      COMMON /CONTRL/ VMIN,VMAX,VLO,VHI,LINES,NWDS,LSTW1                 LN18930
C                                                                        LN18940
      CHARACTER*8      HID,HTIME,HDATE,HID1,HMOL                        &LN18950
C                                                                        LN18960
      COMMON /BUFID/ HID(10),HMOL(64),MOLIND(64),MCNTLC(64),MCNTNL(64),  LN18970
     *               SUMSTR(64),NMOL,FLINLO,FLINHI,ILIN,ILINLC,ILINNL,   LN18980
     *               IREC,IRECTL,HID1(2),LSTWD                           LN18990

      common /bufid2/ n_negepp(64),n_resetepp(64),xspace(4096),lstwd2

      COMMON /BUFIDC/ CMOL(64),CHID10,CHID08                             LN19000
      CHARACTER CMOL*6,CHID10*8,CHID08*8                                 LN19010
      COMMON /MANE/ VNU1(51),STR1(51),ALF1(51),EPP1(51),MOL1(51),        LN19020
     *              HWHM1(51),TMPAL1(51),PSHIF1(51),IFG1(51),            LN19030
     *              MIND1(64),IOUT(51)                                   LN19040
      COMMON /MAINC/ VNUC(51),STRC(51),ALFC(51),EPPC(51),MOLC(51),       LN19050
     *               HWHMC(51),TMPALC(51),PSHIFC(51),IFGC(51),           LN19060
     *               MINDC(64),IOUTC(51)                                 LN19070
      COMMON /TRAC/ VNU2(40),STR2(40),ALF2(40),EPP2(40),MOL2(40),        LN19080
     *              HWHM2(40),TMPAL2(40),PSHIF2(40),IFG2(40),MIND2(64)   LN19090
      COMMON /UNITS/ PI,PLANCK,BOLTZ,CLIGHT,AVOG,RADCN1,RADCN2           LN19100
      COMMON /SREJ/ SR(64),SRD(64),TALF(64)                              LN19110
      COMMON /ICN/ ILIN3,NMAX,NBLOCK                                     LN19120
      common /eppinfo/ negflag
C                                                                        LN19130
      EQUIVALENCE (HID1(1),HDATE) , (HID1(2),HTIME)                      LN19140
C                                                                        LN19150
C     THE FOLLOWING VALUES FOR TALF(M) ARE BASED ON TABLE II OF          LN19160
C     ROTHMAN ET AL, 1987.                                               LN19170
C                                                                        LN19180
      DATA  CMOL(1), CMOL(2), CMOL(3), CMOL(4), CMOL(5), CMOL(6) /       LN19190
     *     '  H2O ','  CO2 ','   O3 ','  N2O ','   CO ','  CH4 ' /,      LN19200
     *      TALF(1), TALF(2), TALF(3), TALF(4), TALF(5), TALF(6) /       LN19210
     *         0.64,    0.25,    0.24,    0.25,    0.21,    0.29 /       LN19220
      DATA  CMOL(7), CMOL(8), CMOL(9),CMOL(10),CMOL(11),CMOL(12) /       LN19230
     *     '   O2 ','   NO ','  SO2 ','  NO2 ','  NH3 ','  HNO3' /,      LN19240
     *      TALF(7), TALF(8), TALF(9),TALF(10),TALF(11),TALF(12) /       LN19250
     *          0.5,     0.5,     0.5,     0.5,     0.5,     0.5 /       LN19260
      DATA CMOL(13),CMOL(14),CMOL(15),CMOL(16),CMOL(17),CMOL(18) /       LN19270
     *     '    OH','    HF','  HCL ','  HBR ','   HI ','  CLO ' /,      LN19280
     *     TALF(13),TALF(14),TALF(15),TALF(16),TALF(17),TALF(18) /       LN19290
     *          0.5,     0.5,    0.46,     0.5,     0.5,     0.5 /       LN19300
      DATA CMOL(19),CMOL(20),CMOL(21),CMOL(22),CMOL(23),CMOL(24) /       LN19310
     *     '  OCS ',' H2CO ',' HOCL ','   N2 ','  HCN ','CH3CL ' /,      LN19320
     *     TALF(19),TALF(20),TALF(21),TALF(22),TALF(23),TALF(24) /       LN19330
     *          0.5,     0.5,     0.5,     0.5,     0.5,     0.5 /       LN19340
      DATA CMOL(25),CMOL(26),CMOL(27),CMOL(28),CMOL(29),CMOL(30) /       LN19350
     *     ' H2O2 ',' C2H2 ',' C2H6 ','  PH3 ',' COF2 ','  SF6 ' /,      LN19360
     *     TALF(25),TALF(26),TALF(27),TALF(28),TALF(29),TALF(30) /       LN19370
     *          0.5,    0.25,     0.5,     0.5,     0.5,     0.5 /       LN19380
      DATA CMOL(31),CMOL(32),CMOL(33),CMOL(34),CMOL(35),CMOL(36) /       LN19390
     *     '  H2S ','HCOOH ','  HO2 ','    O ','CLONO2','  NO+ ' /,      LN19400
     *     TALF(31),TALF(32),TALF(33),TALF(34),TALF(35),TALF(36) /       LN19410
     *          0.5,     0.5,     0.5,     0.5,     0.5,     0.5 /       LN19420
      DATA CMOL(37),CMOL(38)                                     /
     *     ' HOBr ',' C2H4 '                                     /,
     *     TALF(37),TALF(38)                                     /
     *          0.5,     0.5                                     /
C                                                                        LN19430
      DATA (CMOL(I),I=39,64) / 26*'      '/                              LN19440
      DATA (TALF(I),I=39,64) / 26*0.0 /                                  LN19450
C                                                                        LN19460
C     THE FOLLOWING DATA STATEMENTS CONTAIN THE DEFAULT REJECTION        LN19470
C     FOR EACH OF THE FIRST 32 POSSIBLE MOLECULES - THESE ARE BASED         LN19480
C     ON A LIMB VIEW OF A TROPICAL ATMOSPHERE GIVEN THE FOLLOWING           LN19490
C     EQUATION:                                                          LN19500
C                  S(M) = DPTMIN/(50.*W(M))                              LN19510
C                                      WHERE DPTMIN=0.0005               LN19520
C                                                                        LN19530
C                THIS GIVES:  S(M) = 1.E-5/W(M)                          LN19540
C                                                                        LN19550
C                NOTE: NO PROFILES ARE CURRENTLY AVAILABLE FOR           LN19560
C                      MOLECULES 29 THROUGH 38 SO DEFAULT                LN19570
C                      REJECTIONS ARE CURRENTLY SET TO ZERO.             LN19580
C                                                                        LN19590
C                    H2O        CO2         O3        N2O         CO     LN19600
      DATA SRD / 6.667E-31, 1.873E-29, 3.817E-26, 1.984E-26, 4.785E-26,  LN19610
C                                                                        LN19620
C                    CH4         O2         NO        SO2        NO2     LN19630
     *           3.745E-27, 2.959E-32, 1.898E-23, 3.077E-23, 6.098E-23,  LN19640
C                                                                        LN19650
C                    NH3       HNO3         OH         HF        HCL     LN19660
     *           1.799E-23, 2.950E-23, 6.944E-21, 1.541E-21, 1.100E-23,  LN19670
C                                                                        LN19680
C                    HBR         HI        CLO        OCS       H2CO     LN19690
     *           3.623E-21, 2.062E-21, 3.436E-21, 1.110E-23, 7.246E-24,  LN19700
C                                                                        LN19710
C                   HOCL         N2        HCN      CH3CL       H2O2     LN19720
     *           5.650E-22, 7.937E-33, 3.802E-23, 9.804E-24, 3.731E-23,  LN19730
C                                                                        LN19740
C                   C2H2       C2H6        PH3       COF2        SF6     LN19750
     *           4.717E-23, 3.401E-24, 6.173E-13, 0.000E+00, 0.000E+00,  LN19760
C                                                                        LN19770
C                    H2S      HCOOH     HO2, O, ClONO2, NO+,HOBr,C2H4
     *           0.000E+00, 0.000E+00, 32*0.0 /                          LN19790
C                                                                        LN19800
      DATA SR / 64*0.0 /                                                 LN19810
      DATA CHID10 / '       I'/                                          LN19820
      DATA IRD,IPR,IPU / 5,66,7 /                                        LN19830
      DATA SUMSTR / 64*0. /,MCNTLC / 64*0 /,ILINLC / 0 /,IREC / 0 /,     LN19840
     *     IRECTL / 0 /                                                  LN19850
      DATA MOLIND / 64*0 /,MIND1 / 64*0 /,MIND2 / 64*0 /                 LN19860
      DATA MINDC / 64*0 /,IOUT / 51*0 /,IOUTC / 51*0 /                   LN19870
      DATA MCNTNL / 64*0 /,ILINNL / 0 /                                  LN19880
      DATA PLANCK / 6.626176E-27 /,BOLTZ / 1.380662E-16 /,               LN19890
     *     CLIGHT / 2.99792458E10 /,AVOG / 6.022045E23 /                 LN19900
      DATA ILIN3 / 0 /,NMAX / 250 /,NBLOCK / 0 /                         LN19910
      data negflag / 0 /
C                                                                        LN19920
      END                                                                LN19930
c_______________________________________________________________________________
