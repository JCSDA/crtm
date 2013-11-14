;+
;
; Application to overplot multiple oSRF datasets, e.g. oSRF data
; for the same sensor but measured under different conditions.
;

PRO oSRF_Plotter, $
  SRF_Files                        , $ ; Input. Number of SRF datasets to overplot (Max. 6)
  Path            = path           , $ ; Input keyword. Same size as SRF_Files. Default is "." 
  Label           = label          , $ ; Input keyword. Same size as SRF_Files. Default is SRF_Files
  Ylog            = ylog           , $ ; Input keyword.
  No_Pause        = no_pause       , $ ; Input keyword. 
  Close_As_You_Go = close_as_you_go, $ ; Input keyword.
  Debug           = debug              ; Input keyword.
;-

  ; Setup
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check keywords
  n_files = N_ELEMENTS(SRF_Files)
  path       = (N_ELEMENTS(path ) GT 0) ? path  : REPLICATE(".", n_files)
  label      = (N_ELEMENTS(label) GT 0) ? label : SRF_Files
  plot_pause = ~ KEYWORD_SET(No_Pause)
  ; ...Set parameters
  COLOR = ['black', 'green', 'red', 'blue', 'magenta', 'cyan']
  MAX_N_FILES = N_ELEMENTS(COLOR)


  ; Check arguments
  IF ( n_files GT MAX_N_FILES ) THEN $
    MESSAGE, "Too many SRF files! Maximum of "+STRTRIM(MAX_N_FILES,2)+" accepted", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( N_ELEMENTS(path)  NE n_files OR $
       N_ELEMENTS(label) NE n_files ) THEN $
    MESSAGE, "SRF path and label arrays do not match the number of files!", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Read the SRF datafiles
  MESSAGE, "Reading the oSRF datafiles...", /INFORMATIONAL
  osrf_file = HASH()
  FOR n = 0, n_files - 1 DO BEGIN
    ; ...Read all the data
    filename = path[n]+PATH_SEP()+SRF_Files[n]
    osrf_file[n] = OBJ_NEW('osrf_file', filename, Debug = debug)
    osrf_file[n].Read, Debug = debug
    ; ...Check the data
    IF ( n EQ 0 ) THEN BEGIN
      osrf_file[n].Get_Property, $
        Debug = debug, $
        Sensor_Id  = sensor_id, $
        n_Channels = n_channels
    ENDIF ELSE BEGIN
      osrf_file[n].Get_Property, $
        Debug = debug, $
        Sensor_Id  = sid, $
        n_Channels = nch
      IF ( (sid NE sensor_id ) OR $
           (nch NE n_channels)    ) THEN $
        MESSAGE, "Mismatch in sensor data for "+STRTRIM(SRF_Files[n],2), $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDELSE
  ENDFOR  ; File read loop


  ; Begin channel loop
  FOR l = 0, n_channels - 1 DO BEGIN

    ; Create hash for the channel SRFs from each file    
    osrf = HASH()
    pref = OBJARR(n_files) ; For legend

    ; Begin the file loop for display
    FOR n = 0, n_files - 1 DO BEGIN
      osrf[n] = osrf_file[n].Get(Position = l, Debug = debug)
      IF ( n EQ 0 ) THEN BEGIN
        osrf[n].Plot, $
          COLOR = COLOR[n], $
          NAME  = label[n], $
          YLOG  = ylog, $
          Debug = debug
        osrf[n].Get_Property, $
          pref = p, $
          Debug = debug
        pref[n] = p     ; For legend
      ENDIF ELSE BEGIN
        osrf[0].Oplot, osrf[n], $
          COLOR = COLOR[n], $
          NAME  = label[n], $
          pRef  = p
        pref[n] = p[1]  ; For legend
      ENDELSE
    ENDFOR  ; File plot loop

    ; Display the legend
    ; ...Determine legend position
    osrf[0].Get_Property, n_Bands = n_bands, Debug = debug
    CASE n_bands OF
      2: position = [0.325,0.2]
      4: position = [0.25,0.2]
      ELSE: position = [0.5,0.2]
    ENDCASE
    ; ...Display it
    legend = LEGEND(TARGET=pref, $
                    POSITION=position, $
                    HORIZONTAL_ALIGNMENT='CENTER', $
                    VERTICAL_ALIGNMENT='CENTER', $
                    FONT_SIZE=9, $
                    /NORMAL)


    ; Generate output plot files
    ; ...Generate a root filename
    osrf[0].Get_Property, Channel = channel, Debug=debug
    fileroot = sensor_id+'-'+STRTRIM(channel,2)
    ; ...Get the window reference    
    osrf[0].Get_Property, $
      wRef  = w, $
      Debug = Debug
    ; ...Output a PNG file
    w.Save, fileroot+'.png', HEIGHT=500, BORDER=10
    ; ...Output an EPS file
    ; ......Increase the font size for EPS files
    font_size = HASH()
    FOR band = 1, n_bands DO BEGIN
      osrf[0].Get_Property, band, pRef=p, Debug=debug
      font_size[band] = p.font_size
      p.font_size = p.font_size * 2.0
    ENDFOR
    ; ......Create the EPS file
    w.Save, fileroot+'.eps'
    ; ......Restore the font sizes
    FOR band = 1, n_bands DO BEGIN
      osrf[0].Get_Property, band, pRef=p, Debug=debug
      p.font_size = font_size[band]
    ENDFOR
    

    ; Only pause if not at last channel
    not_last_channel = ~ (l EQ n_channels-1)
    IF ( plot_pause AND not_last_channel ) THEN BEGIN
      PRINT, FORMAT='(/5x,"Press <ENTER> to continue, Q to quit")'
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDIF


    ; Close this window if required
    IF ( KEYWORD_SET(close_as_you_go) ) THEN w.Close
      
  ENDFOR  ; Channel loop  

END
