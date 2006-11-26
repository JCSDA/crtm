PRO Plot_DumpComplex, fileName, OVERPLOT=overPlot

  OPENR, fileId, fileName, /GET_LUN

  n = 0L
  READF, fileId, n

  data = DBLARR(3,n)
  READF, fileId, data

  FREE_LUN, fileId

  PRINT, FORMAT='(5x, "Read ",a," points from ",a)', STRTRIM(n,2), fileName
  x = data[0,*]
  ry = data[1,*]
  iy = TEMPORARY(data[2,*])

  IF ( KEYWORD_SET(overPlot) ) THEN BEGIN
    WOPLOT, x, ry, $
            COLOR=3, PSYM=-6
    WOPLOT, x, iy, $
            COLOR=2, PSYM=-6
  ENDIF ELSE BEGIN
    yRange=[MIN(ry)<MIN(iy),MAX(ry)>MAX(iy)]
    WPLOT, x, ry, $
           YRANGE=yRange, $
           /NODATA
    WOPLOT, x, ry, $
            COLOR=5, PSYM=-4
    WOPLOT, x, iy, $
            COLOR=4, PSYM=-4
  ENDELSE

END
