FUNCTION Get_Colours, n_colours

  COMPILE_OPT HIDDEN
  
  ; Sort the colours in order of RGB-ness
  colour_names = TAG_NAMES( !color )
  n = N_ELEMENTS(colour_names)
  integer_colour = LONARR(n)
  icolour = 0L
  FOR i = 0, n-1 DO BEGIN
    string_colour = STRING(!COLOR.(i),FORMAT='(3z2)')
    READS, string_colour, icolour, FORMAT='(z)'
    integer_colour[i] = icolour
  ENDFOR
  idx = SORT(integer_colour)
  colour_names = colour_names(idx[0:-20])
  
  ; Select the colours for the plots
  n = N_ELEMENTS(colour_names)
  skip = n/n_colours - 1L
  colour_list = []
  FOR i = 0L, n-1L, skip DO BEGIN
    colour_list = [ colour_list, colour_names[i] ]
  ENDFOR
  
  RETURN, colour_list
  
END


;+
;
; Application to overplot multiple oSRF datasets, e.g. oSRF data
; for the same sensor but measured under different conditions.
;

PRO oSRF_Plotter, $
  SRF_Files                        , $ ; Input. Number of SRF datasets to overplot
  Path            = path           , $ ; Input keyword. Same size as SRF_Files. Default is "."
  Label           = label          , $ ; Input keyword. Same size as SRF_Files. Default is SRF_Files
  Ylog            = ylog           , $ ; Input keyword. Plot SRFs with log y-axis. Default is linear.
  Frange          = frange         , $ ; Input keyword. Hash containing channel frequency range values.
  Channel_List    = channel_list   , $ ; Input keyword. Array containing list of channels to plot.
  Difference      = difference     , $ ; Input keyword. Plot SRF differences from first SRF file.
  No_Tfit         = no_tfit        , $ ; Input keyword. Do not plot Tfit data. Default is plot.
  No_Pause        = no_pause       , $ ; Input keyword. Do not pause between plots. Default is pause.
  No_Symbol       = no_symbol      , $ ; Input keyword. Do not plot initial SRF with symbol. Default is symbol.
  Close_As_You_Go = close_as_you_go, $ ; Input keyword. Close window plots. Default is not to.
  EPS             = eps            , $ ; Input keyword. Generate EPS output. Default is PNG output.
  Debug           = debug              ; Input keyword.
;-

  ; Setup
  COMPILE_OPT HIDDEN
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check simple keywords
  n_files = N_ELEMENTS(SRF_Files)
  n_plots = n_files
  _path           = (N_ELEMENTS(path ) GT 0) ? path  : REPLICATE(".", n_files)
  _label          = (N_ELEMENTS(label) GT 0) ? label : SRF_Files
  plot_difference = KEYWORD_SET(difference)
  plot_tfit       = ~ KEYWORD_SET(no_tfit)
  plot_pause      = (~ KEYWORD_SET(no_pause)) AND (~ KEYWORD_SET(eps))
  plot_symbol     = ~ KEYWORD_SET(no_symbol)
  ; ...Check complex keywords
  IF ( N_ELEMENTS(frange) GT 0 ) THEN BEGIN
    use_frange = TRUE
    IF ( ~ ISA(frange, 'HASH') ) THEN $
      MESSAGE, "FRANGE keyword specified, but not a hash.", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF ELSE BEGIN
    use_frange = FALSE
  ENDELSE
  ; ...Set plotting parameters
  ; ...Font sizes for plots
  font_size   = KEYWORD_SET(eps) ? EPS_FONT_SIZE : WIN_FONT_SIZE
  l_font_size = font_size ;*0.8 ;KEYWORD_SET(eps) ? font_size*0.6 : font_size*0.8
  thick       = 2 ;KEYWORD_SET(eps) ? 1 : 2
  IF ( n_files LE 6 ) THEN $
    color = ['red', 'blue', 'green','magenta', 'cyan', 'black'] $
  ELSE $
    color = Get_Colours(n_files)


  ; Check arguments
  IF ( N_ELEMENTS(_path)  NE n_files OR $
       N_ELEMENTS(_label) NE n_files ) THEN $
    MESSAGE, "SRF path and label arrays do not match the number of files!", $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Remove any directory and extension specifiers from label array
  _label = FILE_BASENAME(_label,'.osrf.nc')
  
  
  ; Reorder stuff for difference data
  IF ( KEYWORD_SET(difference) ) THEN BEGIN
    n_plots = n_files-1
    color   = color[1:-1]
    _label  = _label[1:-1]
  ENDIF
  
  
  ; Read the SRF datafiles
  MESSAGE, "Reading the oSRF datafiles...", /INFORMATIONAL
  osrf_file = HASH()
  FOR n = 0, n_files - 1 DO BEGIN
    ; ...Read all the data
    filename = _path[n]+PATH_SEP()+SRF_Files[n]
    osrf_file[n] = OBJ_NEW('osrf_file', filename, Debug = debug)
    osrf_file[n].Read, Debug = debug
    ; ...Check the data
    IF ( n EQ 0 ) THEN BEGIN
      osrf_file[n].Get_Property, $
        Debug = debug, $
        Sensor_Id      = sensor_id , $
        n_Channels     = n_channels, $
        Sensor_Channel = sensor_channel
    ENDIF ELSE BEGIN
      osrf_file[n].Get_Property, $
        Debug = debug, $
        Sensor_Id  = sid, $
        n_Channels = nch
      IF ( nch NE n_channels ) THEN $
        MESSAGE, "Channel number mismatch for "+STRTRIM(SRF_Files[n],2), $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDELSE
  ENDFOR  ; File read loop


  ; Process the channel list keyword
  _channel_list = (N_ELEMENTS(channel_list) GT 0) ? channel_list  : sensor_channel
  n_channels = N_ELEMENTS(_channel_list)


  ; Begin channel loop
  FOR l = 0, n_channels - 1 DO BEGIN

    ; The channel number to obtain
    channel = _channel_list[l]

    
    ; Create hash for the channel SRFs from each file
    osrf = HASH()
    pref = OBJARR(n_plots) ; For legend


    ; Retrieve the channel data and subtract if necessary
    IF ( plot_difference ) THEN BEGIN
      rsrf = osrf_file[0].Get(Channel = _channel_list[l], Debug = debug)
      FOR n = 1, n_files-1 DO BEGIN
        osrf[n-1] = osrf_file[n].Get(Channel = _channel_list[l], Debug = debug)
        osrf[n-1] = oSRF_Subtract(osrf[n-1], rsrf, Debug = debug)
      ENDFOR
    ENDIF ELSE BEGIN
      FOR n = 0, n_files - 1 DO BEGIN
        osrf[n] = osrf_file[n].Get(Channel = _channel_list[l], Debug = debug)
      ENDFOR
    ENDELSE


    ; Begin the plotting loop
    MESSAGE, "Plotting data for channel "+STRTRIM(channel,2), /INFORMATIONAL
    first_plot = TRUE
    FOR n = 0, n_plots - 1 DO BEGIN
    
      IF ( use_frange ) THEN BEGIN
        xrange = frange[channel]
        xstyle = 1
      ENDIF
      IF ( first_plot ) THEN BEGIN
        w = WINDOW( WINDOW_TITLE = sensor_id + ' Channel '+STRTRIM(channel,2)+' oSRF', $
                    DIMENSIONS = [800,600], $
                    BUFFER = KEYWORD_SET(eps) )
        osrf[n].Plot, $
          COLOR  = color[n], $
          THICK  = thick, $
          NAME   = _label[n], $
          YLOG   = ylog, $
          XRANGE = xrange, $
          XSTYLE = xstyle, $
          SYMBOL = plot_symbol ? 'diamond' : 'none', $
          Owin   = w, $
          /gTitle, $
          EPS    = eps, $
          Debug  = debug
        osrf[n].Get_Property, $
          pref = p, $
          Debug = debug
        pref[n] = p     ; For legend
        first_plot = FALSE
      ENDIF ELSE BEGIN
        osrf[0].Oplot, osrf[n], $
          COLOR = color[n], $
          THICK = thick, $
          NAME  = _label[n], $
          pRef  = p
        pref[n] = p[1]  ; For legend
      ENDELSE
    ENDFOR  ; File plot loop


    ; Get the number of bands and central frequency
    osrf[0].Get_Property, n_Bands = n_bands, f0 = f0, Sensor_Type = sensor_type, Debug = debug


    ; Display the legend for multi-plots
    IF ( n_plots GT 1 ) THEN BEGIN
      ; ...Determine legend position
      CASE n_bands OF
        2: position = [0.6,0.3]
        4: position = [0.5,0.4]
        ELSE: BEGIN
                f0 = sensor_type EQ MICROWAVE_SENSOR ? Inverse_cm_to_GHz(f0) : f0
                xf0 = pref[0].ConvertCoord(f0,0.3,/DATA,/TO_RELATIVE)
                position = [xf0[0], 0.3]
              END
      ENDCASE
      ; ...Display it
      legend = LEGEND(TARGET=pref, $
                      POSITION=position, $
                      /RELATIVE, $
                      HORIZONTAL_ALIGNMENT='CENTER', $
                      VERTICAL_ALIGNMENT  ='TOP', $
                      SAMPLE_WIDTH = 0.1, $
                      FONT_SIZE=l_font_size)
    ENDIF


    ; Generate output plot files
    ; ...Generate a root filename
    fileroot = sensor_id+'-'+STRTRIM(channel,2)
    IF ( plot_difference ) THEN fileroot = fileroot + '.difference'
    ; ...Output the desired format
    IF ( KEYWORD_SET(eps) ) THEN BEGIN
      w.Save, fileroot+'.eps'
    ENDIF ELSE BEGIN
      w.Save, fileroot+'.png', HEIGHT=600, BORDER=10
    ENDELSE


    ; ===============================
    ; Plot the tfit data if requested
    IF ( plot_tfit ) THEN BEGIN

      tpref = OBJARR(n_plots) ; For legend

      ; Begin the plotting loop
      FOR n = 0, n_plots - 1 DO BEGIN
        IF ( n EQ 0 ) THEN BEGIN
          tw = WINDOW( WINDOW_TITLE = 'Channel '+STRTRIM(channel,2)+' (Teff-Tfit) residuals', $
                       DIMENSIONS = [800,600], $
                       BUFFER = KEYWORD_SET(eps) )
          osrf[n].Tfit_Plot, $
            COLOR  = color[n], $
            THICK  = thick, $
            NAME   = _label[n], $
            SYMBOL = plot_symbol ? 'diamond' : 'none', $
            Owin   = tw, $
            /gTitle, $
            EPS    = eps, $
            Debug  = debug
          osrf[n].Get_Property, $
            tpref = tp, $
            Debug = debug
          tpref[n] = tp     ; For legend
        ENDIF ELSE BEGIN
          osrf[0].Tfit_Oplot, osrf[n], $
            COLOR = color[n], $
            THICK = thick, $
            NAME  = _label[n], $
            pRef  = tp
          tpref[n] = tp  ; For legend
        ENDELSE
      ENDFOR  ; File plot loop


      ; Display the legend for multi-plots
      IF ( n_plots GT 1 ) THEN BEGIN
        ; ...Set legend position
        position = [0.5,0.5]
        ; ...Display it
        legend = LEGEND(TARGET=tpref, $
                        POSITION=position, $
                        /RELATIVE, $
                        HORIZONTAL_ALIGNMENT='CENTER', $
                        VERTICAL_ALIGNMENT='TOP', $
                        FONT_SIZE=l_font_size, $
                        /NORMAL )
      ENDIF


      ; Generate output plot files
      ; ...Generate a root filename
      fileroot = sensor_id+'-'+STRTRIM(channel,2)+'.tfit'
      IF ( plot_difference ) THEN fileroot = fileroot + '.difference'
      ; ...Output the desired format
      IF ( KEYWORD_SET(eps) ) THEN BEGIN
        tw.Save, fileroot+'.eps'
      ENDIF ELSE BEGIN
        tw.Save, fileroot+'.png', HEIGHT=500, BORDER=10
      ENDELSE

    ENDIF  ; plot_tfit IF construct
    ; ===============================


    ; Only pause if not at last channel
    not_last_channel = ~ (l EQ n_channels-1)
    IF ( plot_pause AND not_last_channel ) THEN BEGIN
      PRINT, FORMAT='(/5x,"Press <ENTER> to continue, Q to quit")'
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    ENDIF


    ; Close the windows if required
    IF ( KEYWORD_SET(close_as_you_go) OR KEYWORD_SET(eps) ) THEN BEGIN
      w.Close
      IF ( plot_tfit ) THEN tw.Close
    ENDIF

  ENDFOR  ; Channel loop

END
