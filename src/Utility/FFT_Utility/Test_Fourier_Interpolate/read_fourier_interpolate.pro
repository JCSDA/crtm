PRO Read_Fourier_Interpolate, fileName    , $ ; Input
                              fIn , spcIn , $ ; Output
                              fOut, spcOut    ; Output

  OPENR, fileId, fileName, /GET_LUN

  n = 0L
  n2 = 0L
  READF, fileId, n, n2

  spcIn  = DBLARR(2,n)
  spcOut = DBLARR(2,n2)

  READF, fileId, spcIn
  READF, fileId, spcOut

  FREE_LUN, fileId

  fIn   = spcIn[0,*]
  spcIn = spcIn[1,*]
  fOut   = spcOut[0,*]
  spcOut = spcOut[1,*]

  loc=WHERE(fOut[1:n2-1] NE 0.0d0, count)
  IF ( count GT 0 ) THEN BEGIN
    fOut   = [ fOut[0],(fOut[1:n2-1])[loc] ]
    spcOut = [ spcOut[0],(spcOut[1:n2-1])[loc] ]
  ENDIF

END
