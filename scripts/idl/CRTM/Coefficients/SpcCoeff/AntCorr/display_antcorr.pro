PRO Plot_Data, a, l, aTitle, Path

  ; Get data info
  Info = SIZE(a,/STRUCTURE)
  n_FOVs     = Info.DIMENSIONS(0)
  n_Channels = Info.DIMENSIONS(1)
  n_Paths    = Info.DIMENSIONS(2)
  
  ; Determine data range
  yRange = [MIN(a[*,l,*]),MAX(a[*,l,*])]
  
  ; Set plot parameters
  aColor = [5,4,2,1]
  pSym = [-4,-6,-5,0]
  charSize = 2.5
  
  ; Plot ze data
  PLOT, LINDGEN(n_FOVs)+1,a[*,l,0], $
        TITLE=aTitle, $
        XTITLE='FOV position', $
        YRANGE=yRange, $
        CHARSIZE=charSize, $
        /NODATA
  FOR i=0,n_Paths-1 DO BEGIN
    OPLOT, LINDGEN(n_FOVs)+1,a[*,l,i], $
           COLOR=aColor[i], $
           PSYM=pSym[i]
  ENDFOR
  mylegend, 1.05, 0.6, Path, $
            COLOR=aColor, $
            PSYM=pSym, $
            CHARSIZE=charSize*0.75
  
END

PRO Display_AntCorr, Sensor_Id

  ; Specify the current datafile
  File = STRTRIM(Sensor_Id) + '.AntCorr.nc'
  
  ; Specify datafile paths
  Path = ['NESDIS', $
          'AAPP/fdf', $
          'AAPP/fdf_halw', $
          'AAPP/fdf_v6.4' ]
  n_Paths = N_ELEMENTS(Path)

  ; Read data files
  FOR i = 0, n_Paths - 1 DO BEGIN
    result = Read_netCDF(Path[i]+'/'+File,a,/Quiet)
    IF ( i EQ 0 ) THEN $
      ac = a $
    ELSE $
      ac = [ ac, a ]
  ENDFOR

  ; Set up plot for earth, space, and platform
  psave = !P
  xsave = !X
  !P.MULTI = [0,1,3]
  !X.OMARGIN = [0,30]

  ; Loop over channels
  FOR l = 0, ac[0].n_Channels - 1 DO BEGIN
    aTag = ' for '+Sensor_Id+' ch.'+STRTRIM(ac[0].Sensor_Channel[l],2)
    Plot_Data, ac.a_earth, l, 'A_earth'+aTag, Path
    Plot_Data, ac.a_space, l, 'A_space'+aTag, Path
    Plot_Data, ac.a_platform, l, 'A_platform'+aTag, Path
    IF ( l LT ac[0].n_Channels - 1 ) THEN BEGIN
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDIF
  ENDFOR
  
  ; Restore system structures
  !P = psave
  !X = xsave

END

