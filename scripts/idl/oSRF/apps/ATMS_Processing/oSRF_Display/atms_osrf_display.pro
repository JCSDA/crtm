;+
; Driver script to display multiple ATMS oSRF datasets simultaneously
; along with the boxcar representation and a spectrum.
;
; Spectrum datafiles must reside in a "spectra/" subdirectory
;
PRO atms_osrf_display, $
  osrf_id = osrf_id          , $ ; Array of ATMS oSRF ids (Default = ['table12', 'ngas'])
  ps = ps                    , $ ; Input keyword
  o2 = o2                    , $ ; Input keyword (only plot oxygen channels)
  er = er                    , $ ; Input keyword (use non-unity emissivity spectrum)
  scale_factor = scale_factor, $ ; Input keyword (scale PS output. Default = 1.0)
  _extra=extra                   ; Extra keywords passed to Plot_Spectrum_and_oSRF.
;-

  @color_db

  ; Parameters
  SENSOR_ID = "atms_npp"
  N_CHANNELS = 22
  OSRF_EXTENSION = ".osrf.nc"
  
  ; ...PS dependent
  Thick    = (KEYWORD_SET(PS)) ? 2    :  1
  Font     = (KEYWORD_SET(PS)) ? 1    : -1
  Charsize = (KEYWORD_SET(PS)) ? 1.35 : 1.0
  Bcolor   = (KEYWORD_SET(PS)) ? BLUE : CYAN
  Color     = [RED, GREEN, bcolor, MAGENTA, AQUAMARINE, GOLD, YELLOW]
  Psym      = [0,0,0,0,0,0,0]
  Linestyle = [0,0,0,0,0,0,0]
  Factor = (N_ELEMENTS(scale_factor) GT 0) ? FLOAT(scale_factor) : 1.0
  
  ; Define the other oSRF ids to read
  IF ( N_ELEMENTS(osrf_id) GT 0 ) THEN $
    other_tag = osrf_id $
  ELSE $
    other_tag = ['table12', 'ngas']
    
  ; Specify channel frequency limits
  IF ( KEYWORD_SET(o2) ) THEN BEGIN
    ; ...for oxygen complex spectral region
    ch1 = 12
    ch2 = 15
    spectrum_filename = 'O2partial.spectrum'
    textra = ' (low-frequency bands only)'
    ; ...  1     2      3      4     5     6     7     8     9     10    11    12     13      14      15      16     17     18     19     20     21     22
    f1 = [23.6, 31.25, 50.15, 51.4, 52.4, 53.3, 54.0, 54.6, 55.1, 56.8, 56.8, 56.85, 56.92 , 56.95,  56.96 , 87.0, 163.5, 172.0, 176.0, 179.0, 180.0, 181.5 ]
    f2 = [24.0, 31.55, 50.45, 52.1, 53.2, 53.9, 54.8, 55.3, 55.9, 57.8, 57.8, 57.09, 57.015, 56.986, 56.976, 89.5, 167.5, 195.0, 190.0, 187.5, 186.5, 185.0 ]
  ENDIF ELSE BEGIN
    ; ...for entire ATMS channel coverage
    ch1 = 1
    ch2 = n_Channels
    spectrum_filename = 'sall.spectrum'
    textra = ''
    ; ...  1     2      3      4     5     6     7     8     9     10    11    12    13    14    15    16     17     18     19     20     21     22
    f1 = [23.6, 31.25, 50.15, 51.4, 52.4, 53.3, 54.0, 54.6, 55.1, 56.8, 56.8, 56.8, 56.8, 56.8, 56.8, 87.0, 163.5, 172.0, 176.0, 179.0, 180.0, 181.5 ]
    f2 = [24.0, 31.55, 50.45, 52.1, 53.2, 53.9, 54.8, 55.3, 55.9, 57.8, 57.8, 57.8, 57.8, 57.8, 57.8, 89.5, 167.5, 195.0, 190.0, 187.5, 186.5, 185.0 ]
  ENDELSE
  
  ; Read spectrum
  er_path = KEYWORD_SET(er) ? "e0.6_r0.4/" : ""
  spectrum_readfile, "spectra/"+er_path+spectrum_filename, spectrum
  
  ; Read oSRF data
  ; ...Boxcar data
  boxcar_tag = "boxcar"
  boxcar_filename = SENSOR_ID + "." + boxcar_tag + OSRF_EXTENSION
  boxcar = OBJ_NEW('OSRF_FILE',boxcar_filename)
  boxcar->Read
  bosrf=boxcar->Get(channel=lindgen(22)+1)
  ; ...All the others
  nother = N_ELEMENTS(other_tag)
  other = OBJARR(nother)
  oosrf = OBJARR(N_CHANNELS,nother)
  FOR j = 0, nother-1 DO BEGIN
    other_filename = SENSOR_ID + "." + other_tag[j] + OSRF_EXTENSION
    other[j] = OBJ_NEW('OSRF_FILE',other_filename)
    other[j]->Read
    x = other[j]->Get(channel=LINDGEN(N_CHANNELS)+1)
    oosrf[*,j] = x
  ENDFOR
  
  
  psave = !P
  FOR i = ch1-1, ch2-1 DO BEGIN
    IF ( KEYWORD_SET(ps) ) THEN BEGIN
      !P.MULTI = [0,2,2]
      psfile = 'atms_npp.ch'+STRTRIM(i+1,2)+'.osrf.ps'
      pson, file=psfile
      DEVICE, SCALE_FACTOR=Factor
    ENDIF

    title='ATMS NPP Spectral Response Functions for channel '+STRTRIM(i+1,2)+textra
    plot_spectrum_and_osrf,spectrum,bosrf[i],title=title,xrange=[f1[i],f2[i]], $
      ps=ps, /boxcar, sym=-4, linestyle=2, _extra=extra

    FOR j = 0, nother-1 DO $
      plot_spectrum_and_osrf,spectrum,oosrf[i,j],/overplot, ps=ps, $
        color=Color[j],linestyle=Linestyle[j]

    IF ( KEYWORD_SET(ps) ) THEN BEGIN
      ; Create legend for PS output only
      legend = [boxcar_tag, other_tag]
      n = N_ELEMENTS(legend)
      mylegend, 1.35, 0.95, $
                legend, $
                COLOR = ([!P.COLOR, Color])[0:n-1], $
                LINESTYLE= ([2, Linestyle])[0:n-1], $
                PSYM = ([-4, Psym])[0:n-1], $
                THICK = REPLICATE(thick,n), $
                FONT = Font, $
                CHARSIZE = Charsize
      psoff
    ENDIF ELSE BEGIN
      q=get_kbrd(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDELSE
  ENDFOR
  !P = psave
  
  OBJ_DESTROY, [other,boxcar,bosrf]
  OBJ_DESTROY, oosrf

END
