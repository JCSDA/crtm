PRO Diff_Fourier_Interpolate, file1, file2

  Read_Fourier_Interpolate, file1, $ ; Input
                            fIn , spcIn , $ ; Output
                            f1Out, spc1Out    ; Output

  Read_Fourier_Interpolate, file2, $ ; Input
                            fIn , spcIn , $ ; Output
                            f2Out, spc2Out    ; Output

  WPLOT, fIn, spcIn, $
        /NODATA
  WOPLOT, fIn, spcIn, $
         COLOR=5, PSYM=-6
  WOPLOT, f1Out, spc1Out, $
         COLOR=4, PSYM=-4
  WOPLOT, f2Out, spc2Out, $
         COLOR=3, PSYM=-2
         
  idx1 = WHERE(f1Out GE MIN(fIn) AND f1Out LE MAX(fIn) )
  idx2 = WHERE(f2Out GE MIN(fIn) AND f2Out LE MAX(fIn) )

  WOPLOT, f1Out[idx1], spc1Out[idx1]-spc2Out[idx2], $
         COLOR=2, PSYM=-5
  WOPLOT, f1Out[idx1], 100.0d0*(spc1Out[idx1]-spc2Out[idx2])/spc1Out[idx1], $
         COLOR=1, PSYM=-1
  WOPLOT, !X.CRANGE,[0,0],LINESTYLE=2

END
