pro display_xy

  files = ['linint-rank1_xy.dat', $
           'linint-scalar_xy.dat', $
           'polyint-rank1_xy.dat', $
           'polyint-scalar_xy.dat', $
           'splineint-rank1_xy.dat', $
           'splineint-scalar_xy.dat' ]
  nfiles=N_ELEMENTS(files)

  PRINT, FORMAT='(/5x,"Select file to display")'
  FOR i = 0,nfiles-1 DO BEGIN
    PRINT, FORMAT='(10x,i1,") ", a )', i+1, files[i]
  ENDFOR
  READ, ifile, PROMPT='     Enter choice: '

  ifile=((LONG(ifile)-1)>0)<(nfiles-1)
  

  o = ddread('orig_xy.dat')
  ox = reform(o[0,*])
  oy = reform(o[1,*])

  i = ddread(files[ifile])
  i=TRANSPOSE(i)
  ix = reform(i[*,0])
  truey = reform(i[*,1])
  inty = reform(i[*,2:*])
  info=SIZE(inty,/STRUCTURE)
  IF ( info.N_DIMENSIONS EQ 1 ) THEN BEGIN
    dy = truey-inty
    n = 1
  ENDIF ELSE BEGIN
    n=info.DIMENSIONS(1)
    dy=inty
    FOR i=0, n-1 DO BEGIN
      dy[*,i] = truey-inty[*,i]
    ENDFOR
  ENDELSE

  wplot, ox, oy, psym=-2
  woplot, ix, truey, psym=-6
  FOR i=0, n-1 DO BEGIN
    woplot, ix, inty[*,i], color=i+1, psym=-4
  ENDFOR

  sumerror=dblarr(n)
  wplot, ix, 100.0*dy[*,0]/truey, psym=-6,ytitle='% error', /new, /nodata
  FOR i=0, n-1 DO BEGIN
    woplot, ix, 100.0*dy[*,i]/truey, color=i+1, psym=-4
    sumerror[i]=total(abs(dy[*,i]))
    PRINT, 'order '+strtrim(2*i+1,2)+' sumerror: ', sumerror[i]
  ENDFOR
  woplot, !X.CRANGE,[0,0],LINESTYLE=2

END
