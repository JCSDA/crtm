PRO Read_Tb_data, AtmProfile_Id, $  ; Input
                  Path         , $  ; Input
                  Tb                ; Output
  ; Get info
  info = SIZE(Tb,/STRUCT)
  ; Read the profile data results
  FOR m = 0, info.DIMENSIONS[0]-1 DO BEGIN
    filename = Path+"/"+AtmProfile_Id+'.Profile'+STRING(m+1,FORMAT='(i4.4)')+'.Tb.dat'
    read_atms_tb, filename, x
    Tb[m,*] = x
  ENDFOR
END



PRO calculate_int_water, AtmProfile,            $
                         m,                     $
                         Int_Water_Vapor
  @fundamental_constants
  RECIPROCAL_GRAVITY = (1.0d0/STANDARD_GRAVITY)
  ; Calculate Precipitable water for a profile
  FOR k = 0, AtmProfile.n_Layers - 1 DO BEGIN
    dPonG = (RECIPROCAL_GRAVITY * (AtmProfile.level_pressure[k,m] - AtmProfile.level_pressure[k+1,m]))*100.0d0
    Int_Water_Vapor[m] = Int_Water_Vapor[m] + (((dPonG * AtmProfile.layer_absorber[k,0,m])/1000000.0d0))*100.0d0
  ENDFOR 
END


;+
; Procedure to display the ATMS oSRF Tb sensitivity data.
;
PRO plot_atms_tb, $
  ref_id       = ref_id      , $  ; Input keyword. Default is 'boxcar' if not specified.
  srf_id       = srf_id      , $  ; Input keyword. Default is ['table12','ngas'] if not specified.
  path         = path        , $  ; Input keyword. Default is 'results/' if not specified.
  label        = label       , $  ; Input keyword. Default is to not label plots (a), (b), ...etc.
  scale_factor = scale_factor, $  ; Input keyword (scale PS output. Default = 1.0)
  ps           = ps               ; Input keyword
;-
 
  @color_db
  @srf_parameters
  @error_codes


  ; Parameters
  N_PROFILES=83
  N_CHANNELS=22
  PROFILE_ID="ECMWF"+STRTRIM(N_PROFILES,2)

 
  ; Data set as reference
  IF ( N_ELEMENTS(ref_id) EQ 0 ) THEN $
    ref_dataset = 'boxcar' $
  ELSE $
    ref_dataset = ref_id[0]


  ; Data sets to view
  IF ( N_ELEMENTS(SRF_Id) EQ 0 ) THEN $
    dataset = ['table12','ngas'] $
  ELSE $
    dataset = SRF_Id
  n_datasets = N_ELEMENTS(dataset)


  ; Path to results
  IF ( N_ELEMENTS(path) EQ 0 ) THEN $
    results_dir = "results/" $
  ELSE $
    results_dir = path[0]
    
    
  ; Read the MonoRTM output
  ; ...The reference data first
  Tb_ref = DBLARR(N_PROFILES,N_CHANNELS)
  Read_Tb_Data, PROFILE_ID, results_dir+ref_dataset, Tb_ref
  ; ...All the other data
  Tb = DBLARR(N_PROFILES,N_CHANNELS)
  FOR i = 0, n_datasets-1 DO BEGIN
    Read_Tb_Data, PROFILE_ID, results_dir+dataset[i], Tb
    IF ( i EQ 0 ) THEN BEGIN
      Tb_dataset = Tb
      dTb = Tb - Tb_ref
    ENDIF ELSE BEGIN
      Tb_dataset = [[[Tb_dataset]], [[Tb]]]
      dTb = [[[dTb]], [[Tb - Tb_ref]]]
    ENDELSE
  ENDFOR

    
  ; Compute integrated water vapour
  result = read_netcdf(PROFILE_ID+'.AtmProfile.nc', AtmProfile)
  Int_Water_Vapor = DBLARR(N_PROFILES)
  FOR m = 0, N_PROFILES - 1 DO BEGIN
    calculate_int_water, AtmProfile, m, Int_Water_Vapor
  ENDFOR


  ; Set plotting parameters
  psave = !P
  xsave = !X
  ysave = !Y
  Legend_Text = dataset
  bcolor      = (KEYWORD_SET(PS)) ? BLUE : CYAN
  Color       = [RED , GREEN  , bcolor, MAGENTA, AQUAMARINE]
  IF ( n_datasets EQ 1 ) THEN BEGIN
    Color[0] = (KEYWORD_SET(PS)) ? !P.BACKGROUND : !P.COLOR
  ENDIF
  Psym        = [1, 4, 5, 6, 7]
  Thick       = REPLICATE((KEYWORD_SET(PS)) ? 2 : 1, N_ELEMENTS(Color))
  Font        = (KEYWORD_SET(PS)) ? 1   : -1
  Charsize    = (KEYWORD_SET(PS)) ? 1.35 : 1.0
  xmargin = [10,10]
  xlabel = 0.92
  ylabel = 0.93
  charsizelabel = Charsize*1.2
  xlegend = 1.25
  ylegend = 0.92
  charsizelegend = Charsize*1.2
  factor = (N_ELEMENTS(scale_factor) GT 0) ? FLOAT(scale_factor) : 1.0


  ; Main channel loop
  FOR l = 0, N_CHANNELS - 1 DO BEGIN

    !P.MULTI = [0,2,2,0,1]
;    !X.OMARGIN = [3,0]
;    !X.MARGIN = [7,2]
    IF ( KEYWORD_SET(ps) ) THEN BEGIN
      pson, filename = 'atms_npp.ch'+STRTRIM(l+1,2)+'.dTb.ps'
      DEVICE, SCALE_FACTOR=factor
    ENDIF

    
    ; Plot difference
    ; ...Determine plot range
    yrange = [MIN(dTb[*,l,*]),MAX(dTb[*,l,*])]
    ; ...Setup plot
    PLOT, Tb_ref[*,l], Tb_ref[*,l]-Tb_ref[*,l], $
      /NODATA, $
      YRANGE=yrange, $
      XMARGIN=xmargin, $
      FONT=Font, $
      CHARSIZE=Charsize, $ 
      TITLE = 'Ch.'+STRTRIM(l+1,2)+' dT!DB!N as a function of T!DB,ref!N', $
      XTITLE = 'T!DB,ref!N (K)', YTITLE = 'Measured-Reference SRF dT!DB!N (K)'
    IF ( KEYWORD_SET(label) ) THEN BEGIN
      myxyouts, xlabel, ylabel,'(a)', $
        FONT=Font, $
        CHARSIZE=Charsizelabel
    ENDIF
    ; ...Plot zero line
    OPLOT, !X.CRANGE, [0,0], $
      LINESTYLE=2, $
      THICK=Thick[0]
    PRINT, 'Channel', l+1
    ; ...Now plot all the available data
    FOR i = 0, n_datasets-1 DO BEGIN
      OPLOT, Tb_ref[*,l], dTb[*,l,i], $
             PSYM=Psym[i], $
             THICK=Thick[i], $
             COLOR=Color[i]
    ENDFOR
    ; ...Display a legend
    mylegend, xlegend, ylegend, Legend_Text, $
      COLOR    = color[0:n_datasets-1], $
      PSYM     = psym[0:n_datasets-1], $
      THICK    = thick[0:n_datasets-1], $
      FONT     = font, $
      CHARSIZE = charsizelegend
                  



    ; Plot precipitable water
    ; ...Determine plot range
    yrange = [MIN(dTb[*,l,*]),MAX(dTb[*,l,*])]
    PLOT, Int_Water_Vapor, dTb[*,l,0], $
      /NODATA, $
      YRANGE=yrange, $
      XMARGIN=xmargin, $
      FONT=Font, $
      CHARSIZE=Charsize, $ 
      TITLE = 'Ch.'+STRTRIM(l+1,2)+' dT!DB!N as a function of precipitable water', $
      XTITLE = 'Integrated water (cm)', YTITLE = 'Measured-Reference SRF dT!DB!N (K)'
    IF ( KEYWORD_SET(label) ) THEN BEGIN
      myxyouts, xlabel, ylabel,'(b)', $
        FONT=Font, $
        CHARSIZE=Charsizelabel
    ENDIF
    ; ...Plot zero line
    OPLOT, !X.CRANGE, [0,0], $
      LINESTYLE=2, $
      THICK=Thick[0]
    ; ...Now plot all the available data
    FOR i = 0, n_datasets-1 DO BEGIN
      OPLOT, Int_Water_Vapor, dTb[*,l,i], $
             PSYM=Psym[i], $
             THICK=Thick[i], $
             COLOR=Color[i]
    ENDFOR


    
    IF ( KEYWORD_SET(ps) ) THEN BEGIN
      psoff
    ENDIF ELSE BEGIN      
      q=GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q') THEN BREAK
      IF ( STRUPCASE(q) EQ 'S') THEN STOP
    ENDELSE

    
  ENDFOR


  ; Restore sysvars
  !P = psave
  !X = xsave
  !Y = ysave
  
END ; PRO plot_atms_tb.pro
