C     path:      %P%
C     revision:  $Revision: 7.4 $
C     created:   $Date: 2005/08/16 22:31:18 $  
C     presently: %H%  %T%
c______________________________________________________________________________

      SUBROUTINE LBLDAT(HDATE)                                           LN05190
C                                                                        LN05200
      character*8 hdate
C                                                                        LN05220
      CHARACTER GDATE*10                                                 LN05230
C                                                                        LN05240
      INTEGER*4 IARRAY(8)                                               >LN05250

      COMMON /CVRUTL/ HANUTL,HVRUTL
C
      CHARACTER*18 HNAMUTL,HVRUTL
C

C
C     ASSIGN NAME and CVS VERSION NUMBER TO MODULE 
C
      HNAMUTL= '       util_dos.f:'
      HVRUTL = '$Revision: 7.4 $' 

C                                                                        LN05260
      CALL DATIM(IARRAY)                                                >LN05280
c     
      IARRAY(8)=iarray(8)-(int(iarray(8)/100))*100
      WRITE (GDATE,900) IARRAY(8),IARRAY(7),IARRAY(6)                   >LN05300
C                                                                        LN05310
      READ (GDATE,901) HDATE                                             LN05320
C                                                                        LN05330
C     GDATE AND FORMAT ARE FOR CYBER AND CRAY                            LN05340
C                                                                        LN05350
C       -- CYBER REQUIRES FORMAT (1X,A8)                                 LN05360
C       -- CRAY  REQUIRES FORMAT (A8)                                    LN05370
C                                                                        LN05380
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE DATE               LN05390
C                                                                        LN05400
      RETURN                                                             LN05410
C                                                                        LN05420
  900 FORMAT (   I2.2,2('/',I2.2))
C>901 FORMAT (1X,A8)                                                    >LN05440
  901 FORMAT (A8)                                                        LN05450
C                                                                        LN05460
      END                                                                LN05470
      SUBROUTINE FTIME (HTIME)                                           LN05480
C                                                                        LN05490
      DOUBLE PRECISION HTIME                                            &LN05500
C                                                                        LN05510
      CHARACTER GTIME*10                                                 LN05520
C                                                                        LN05530
      INTEGER*4 IARRAY(8)                                               >LN05540
C                                                                        LN05550
C>>   CALL CLOCK (GTIME)                                                 LN05560
C>VAX CALL TIME (GTIME)                                                 >LN05570
      CALL DATIM (IARRAY)                                               >LN05580
      WRITE (GTIME,900) IARRAY(5),IARRAY(4),IARRAY(3)                   >LN05590
C                                                                        LN05600
      READ (GTIME,901) HTIME                                             LN05610
C                                                                        LN05620
C     GTIME AND FORMAT ARE FOR CYBER AND CRAY                            LN05630
C                                                                        LN05640
C       -- CYBER REQUIRES FORMAT (1X,A8)                                 LN05650
C       -- CRAY  REQUIRES FORMAT (A8)                                    LN05660
C                                                                        LN05670
C     CHANGE THESE TO WORD SIZE AND FORMAT OF ROUTINE GTIME              LN05680
C                                                                        LN05690
      RETURN                                                             LN05700
C                                                                        LN05710
  900 FORMAT (   I2,2(':',I2.2))                                         LN05720
C>901 FORMAT (1X,A8)                                                    >LN05730
  901 FORMAT (A8)                                                        LN05740
C                                                                        LN05750
      END                                                                LN05760
      SUBROUTINE CPUTIM (TIME)                                           LN05770
C                                                                        LN05780
      COMMON /TIMIN/ A1                                                  LN05790
C                                                                        LN05800
      REAL*4 ETIME                                                      >LN05810
      INTEGER*4 ICODE                                                           
C                                                                        LN05820
C     THIS SUBROUTINE OBTAINS CPU TIME                                   LN05830
C                                                                        LN05840
      IF (A1.LT.0.) THEN                                                 LN05850
C>>      CALL SECOND (TIME)                                              LN05860
C>VAX    A1 = SECNDS(0.0)                                               >LN05870
         CALL CPUTIME(ETIME,ICODE)                                              
         A1=ETIME                                                               
         TIME=0                                                                 
      ELSE                                                               LN05890
C>>      CALL SECOND (TIME)                                              LN05900
C>VAX    TIME = SECNDS(A1)                                              >LN05910
         CALL CPUTIME(ETIME,ICODE)                                              
         STIME=ETIME-A1                                                         
         TIME=STIME*1.E-06                                                      
C        TIME = ETIME(TARRAY)                                           >LN05920
      ENDIF                                                              LN05930
C                                                                        LN05940
      RETURN                                                             LN05950
C                                                                        LN05960
      END                                                                LN05970
      BLOCK DATA BTIM                                                    LN05980
C                                                                        LN05990
      COMMON /TIMIN/ A1                                                  LN06000
C                                                                        LN06010
      DATA A1 /-1. /                                                     LN06020
C                                                                        LN06030
      END                                                                LN06040

      SUBROUTINE BUFIN (IFILE,IEOF,IARRAY,IWORDS)                         A10770
C                                                                         A10780
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING     A10790
C     AT LOCATION IARRAY                                                  A10800
C                                                                         A10810
C     IFILE IS THE FILE DESIGNATION                                       A10820
C                                                                         A10830
      DATA i_4 / 4 /    
C                                                                         A10850
      DIMENSION IARRAY(IWORDS)                                            A10840

      IEOF = 1                                                            A10860
C                                                                         A10880
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                       A10890
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10                                     A10900
C                                                                         A10910
      READ (IFILE,END=10) IARRAY                                          A10920
      ITEST = MIN(IWORDS,i_4)                                               A10930
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99                                A10940
C                                                                         A10950
      RETURN                                                              A10960
C                                                                         A10970
   10 IEOF = 0                                                            A10980
C                                                                         A10990
      RETURN                                                              A11000
C                                                                         A11010
      END                                                                 A11020
c >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

c         note the name change

      SUBROUTINE BUFIN_sgl (IFILE,IEOF,IARRAY,IWORDS)
C
C     THIS SUBROUTINE BUFFERS IN (READS) IWORDS INTO  IARRAY STARTING
C     AT LOCATION IARRAY
C
C     IFILE IS THE FILE DESIGNATION
C                                  

      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)

      DATA i_4 / 4 /    

      DIMENSION IARRAY(IWORDS)
C                                                                         A10830
      IEOF = 1             
C                          
C#    BUFFER IN (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) GO TO 10              
C                                               
      READ (IFILE,END=10) IARRAY
      ITEST = MIN(IWORDS,i_4)                 
      IF (IARRAY(ITEST).EQ.-99) IEOF = -99      
C                                               
      RETURN                                    
C                                               
   10 IEOF = 0                                  
C                                               
      RETURN                                    
C                                               
      END                                       
c______________________________________________________________________________

      SUBROUTINE BUFOUT (IFILE,IARRAY,IWORDS)                             A11030
C                                                                         A11040
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING    A11050
C     AT LOCATION IARRAY                                                  A11060
C                                                                         A11070
C     IFILE IS THE FILE DESIGNATION                                       A11080
C                                                                         A11090
      DIMENSION IARRAY(IWORDS)                                            A11100
C                                                                         A11120
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))                      A11130
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '                     A11140
C                                                                         A11150
      WRITE (IFILE) IARRAY                                                A11160
C                                                                         A11170
      RETURN                                                              A11180
C                                                                         A11190
      END                                                                 A11200
c______________________________________________________________________________

      SUBROUTINE BUFOUT_sgl (IFILE,IARRAY,IWORDS)
C                                                 
C     THIS SUBROUTINE BUFFERS OUT (WRITES) IWORDS FROM IARRAY STARTING
C     AT LOCATION IARRAY                                                 
C                                                                     
C     IFILE IS THE FILE DESIGNATION                                   
C                                                                     
c
      implicit integer*4 (i-n)
      implicit real*4    (a-h,o-z)
c
      DIMENSION IARRAY(IWORDS)
C                                                   
C#    BUFFER OUT (IFILE,1) (IARRAY(ILO),IARRAY(IHI))
C#    IF (UNIT(IFILE).EQ.0.) STOP ' ERROR IN BUFOUT '
C                                                    
      WRITE (IFILE) IARRAY
C                                                    
      RETURN                                         
C                                                    
      END                                            
