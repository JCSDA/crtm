PRO Diff_SPCtoIFGtoSPC

  SPCtoIFGfile = './Test_SPCtoIFG/Test_SPCtoIFG.bin'
  IFGtoSPCfile = './Test_IFGtoSPC/Test_IFGtoSPC.bin'

  Read_SPCIFG, SPCtoIFGfile, $
               f1, rSpc1, iSpc1, $
               x1, rIfg1, iIfg1
               
  Read_SPCIFG, IFGtoSPCfile, $
               f2, rSpc2, iSpc2, $
               x2, rIfg2, iIfg2

  drSpc=rSpc1-rSpc2
  diSpc=iSpc1-iSpc2
  yRange=[MIN(drspc)<MIN(dispc),MAX(drspc)>MAX(dispc)]
  WPLOT, f1, drspc, $
         YRANGE=yRange, $
         TITLE='Spectra differences after SPC->IFG->SPC FFT', $
         XTITLE = 'Frequency (cm!U-1!N)', $
         /NEW, /NODATA
  WOPLOT, f1, drspc, $
          COLOR=5, PSYM=PSym
  WOPLOT, f1, dispc, $
          COLOR=4, PSYM=PSym
   
  drIfg=rIfg1-rIfg2
  diIfg=iIfg1-iIfg2
  yRange=[MIN(drifg)<MIN(diifg),MAX(drifg)>MAX(diifg)]
  WPLOT, x1, drifg, $
         YRANGE=yRange, $
         XTITLE = 'Optical delay (cm)', $
         /NEW, /NODATA
  WOPLOT, x1, drifg, $
          COLOR=5, PSYM=PSym
  WOPLOT, x1, diifg, $
          COLOR=4, PSYM=PSym

END
