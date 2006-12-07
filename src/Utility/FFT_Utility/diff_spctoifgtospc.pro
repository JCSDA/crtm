PRO Diff_SPCtoIFGtoSPC

  SPCtoIFGfile = './Test_SPCtoIFG/Test_SPCtoIFG.bin'
  IFGtoSPCfile = './Test_IFGtoSPC/Test_IFGtoSPC.bin'

  Read_SPCIFG, SPCtoIFGfile, $
               Spc1, $
               d2=Ifg1
               
  Read_SPCIFG, IFGtoSPCfile, $
               Spc2, $
               d2=Ifg2

  drSpc=Spc1.ry-Spc2.ry
  diSpc=Spc1.iy-Spc2.iy
  yRange=[MIN(drspc)<MIN(dispc),MAX(drspc)>MAX(dispc)]
  WPLOT, Spc1.x, drspc, $
         YRANGE=yRange, $
         TITLE='Spectra differences after SPC->IFG->SPC FFT', $
         XTITLE = 'Frequency (cm!U-1!N)', $
         /NEW, /NODATA
  WOPLOT, Spc1.x, drspc, $
          COLOR=5, PSYM=PSym
  WOPLOT, Spc1.x, dispc, $
          COLOR=4, PSYM=PSym
   
  drIfg=Ifg1.ry-Ifg2.ry
  diIfg=Ifg1.iy-Ifg2.iy
  yRange=[MIN(drifg)<MIN(diifg),MAX(drifg)>MAX(diifg)]
  WPLOT, Ifg1.x, drifg, $
         YRANGE=yRange, $
         XTITLE = 'Optical delay (cm)', $
         /NEW, /NODATA
  WOPLOT, Ifg1.x, drifg, $
          COLOR=5, PSYM=PSym
  WOPLOT, Ifg1.x, diifg, $
          COLOR=4, PSYM=PSym

END
