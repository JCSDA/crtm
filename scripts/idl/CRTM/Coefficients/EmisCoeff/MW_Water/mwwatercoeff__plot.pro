;+
PRO MWwaterCoeff::Plot, $
  Font         = Font        , $ ; Input keyword
  Charsize     = Charsize    , $ ; Input keyword
  Angle        = Angle       , $ ; Input keyword
  Frequency    = Frequency   , $ ; Input keyword
  Temperature  = Temperature , $ ; Input keyword
  Wind_Speed   = Wind_Speed  , $ ; Input keyword
  iAngle       = iAngle      , $ ; Input keyword
  iFrequency   = iFrequency  , $ ; Input keyword
  iTemperature = iTemperature, $ ; Input keyword
  iWind_Speed  = iWind_Speed , $ ; Input keyword
  Horizontal   = Horizontal  , $ ; Input keyword
  Debug        = Debug       , $ ; Input keyword
  _EXTRA = Extra
;-
  ; Set up
  @color_db
  ; ...Set up error handler
  @emiscoeff_pro_err_handler
  ; ...ALL *input* pointers must be associated
  IF ( NOT self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input MWwaterCoeff pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Save current plotting sysvars
  psave = !P
  xsave = !X
  ysave = !Y
  zsave = !Z


  ; Process keywords
  ; ...The plotting keywords  
  _font     = KEYWORD_SET(Font    ) ? Font     : -1
  _charsize = KEYWORD_SET(Charsize) ? Charsize : 3.0
  ; ...The data surface type keywords
  _angle       = KEYWORD_SET(Angle      )
  _frequency   = KEYWORD_SET(Frequency  )
  _temperature = KEYWORD_SET(Temperature)
  _wind_speed  = KEYWORD_SET(Wind_Speed )
  ; ...The data index keywords
  _iangle       = KEYWORD_SET(iAngle      ) ? iAngle       : 0
  _ifrequency   = KEYWORD_SET(iFrequency  ) ? iFrequency   : 0
  _itemperature = KEYWORD_SET(iTemperature) ? iTemperature : 0
  _iwind_speed  = KEYWORD_SET(iWind_Speed ) ? iWind_Speed  : 0
  ; ...The emissivity data selection
  IF ( KEYWORD_SET(Horizontal) ) THEN BEGIN
    data_name = 'e!Dh!N'
    self->Get_Property, eh = emissivity
  ENDIF ELSE BEGIN
    data_name = 'e!Dv!N'
    self->Get_Property, ev = emissivity
  ENDELSE


  ; Ensure the data indices are within bounds
  self->Get_Property, n_Angles=n_angles
  i = n_angles - 1
  _iangle = (_iangle < i) > 0

  self->Get_Property, n_Frequencies=n_frequencies
  i = n_frequencies - 1
  _ifrequency = (_ifrequency < i) > 0

  self->Get_Property, n_Temperatures=n_temperatures
  i = n_temperatures - 1
  _itemperature = (_itemperature < i) > 0

  self->Get_Property, n_Wind_Speeds=n_wind_speeds
  i = n_wind_speeds - 1
  _iwind_speed = (_iwind_speed < i) > 0


  ; Extract the data combinations and set plotting titles.
  CASE 1 OF
    ; Two-dimensional plots
    (_angle AND _frequency AND (NOT _temperature) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[*,*,_itemperature,_iwind_speed])
      self->Get_Property, Angle=x, Frequency=y, Temperature=t, Wind_Speed=w
      xtitle = 'Angle (!Uo!N)'
      ytitle = 'Frequency (GHz)'
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for T=' + tvalue + 'K' + ' and W=' + wvalue + 'm/s'
    END
    (_angle AND _temperature AND (NOT _frequency) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[*,_ifrequency,*,_iwind_speed])
      self->Get_Property, Angle=x, Temperature=y, Frequency=f, Wind_Speed=w
      xtitle = 'Angle (!Uo!N)'
      ytitle = 'Temperature (K)'
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for F=' + fvalue + 'GHz' + ' and W=' + wvalue + 'm/s'
    END
    (_angle AND _wind_speed AND (NOT _frequency) AND (NOT _temperature)): BEGIN
      data = REFORM(emissivity[*,_ifrequency,_itemperature,*])
      self->Get_Property, Angle=x, Wind_Speed=y, Frequency=f, Temperature=t
      xtitle = 'Angle (!Uo!N)'
      ytitle = 'Wind speed (m/s)'
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      title = data_name + ' for F=' + fvalue + 'GHz' + ' and T=' + tvalue + 'K'
    END
    (_frequency AND _temperature AND (NOT _angle) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[_iangle,*,*,_iwind_speed])
      self->Get_Property, Frequency=x, Temperature=y, Angle=a, Wind_Speed=w 
      xtitle = 'Frequency (GHz)'
      ytitle = 'Temperature (K)'
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N' + ' and W=' + wvalue + 'm/s'
    END
    (_frequency AND _wind_speed AND (NOT _angle) AND (NOT _temperature)): BEGIN
      data = REFORM(emissivity[_iangle,*,_itemperature,*])
      self->Get_Property, Frequency=x, Wind_Speed=y, Angle=a, Temperature=t
      xtitle = 'Frequency (GHz)'
      ytitle = 'Wind speed (m/s)'
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N' + ' and T=' + tvalue + 'K'
    END
    (_temperature AND _wind_speed AND (NOT _angle) AND (NOT _frequency)): BEGIN
      data = REFORM(emissivity[_iangle,_ifrequency,*,*])
      self->Get_Property, Temperature=x, Wind_Speed=y, Angle=a, Frequency=f
      xtitle = 'Temperature (K)'
      ytitle = 'Wind speed (m/s)'
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N' + ' and F=' + fvalue + 'GHz'
    END
    ; One-dimensional plots
    (_angle AND (NOT _frequency) AND (NOT _temperature) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[*,_ifrequency,_itemperature,_iwind_speed])
      self->Get_Property, Angle=x, Frequency=f, Temperature=t, Wind_Speed=w
      xtitle = 'Angle (!Uo!N)'
      ytitle = data_name
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for F=' + fvalue + 'GHz, T=' + tvalue + 'K, and W=' + wvalue + 'm/s'
    END
    (_frequency AND (NOT _angle) AND (NOT _temperature) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[_iangle,*,_itemperature,_iwind_speed])
      self->Get_Property, Angle=a, Frequency=x, Temperature=t, Wind_Speed=w
      xtitle = 'Frequency (GHz)'
      ytitle = data_name
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N, T=' + tvalue + 'K, and W=' + wvalue + 'm/s'
    END
    (_temperature AND (NOT _angle) AND (NOT _frequency) AND (NOT _wind_speed)): BEGIN
      data = REFORM(emissivity[_iangle,_ifrequency,*,_iwind_speed])
      self->Get_Property, Angle=a, Frequency=f, Temperature=x, Wind_Speed=w
      xtitle = 'Temperature (K)'
      ytitle = data_name
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      wvalue = STRTRIM(STRING(w[_iwind_speed] ,FORMAT='(f4.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N, F=' + fvalue + 'GHz, and W=' + wvalue + 'm/s'
    END
    (_wind_speed AND (NOT _angle) AND (NOT _frequency) AND (NOT _temperature)): BEGIN
      data = REFORM(emissivity[_iangle,_ifrequency,_itemperature,*])
      self->Get_Property, Angle=a, Frequency=f, Temperature=t, Wind_Speed=x
      xtitle = 'Wind speed (m/s)'
      ytitle = data_name
      avalue = STRTRIM(STRING(a[_iangle],FORMAT='(f4.1)'),2)
      fvalue = STRTRIM(STRING(f[_ifrequency],FORMAT='(f5.1)'),2)
      tvalue = STRTRIM(STRING(t[_itemperature],FORMAT='(f5.1)'),2)
      title = data_name + ' for A=' + avalue + '!Uo!N, F=' + fvalue + 'GHz, and T=' + tvalue + 'K'
    END
    ELSE: MESSAGE, 'Invalid combination of selected angle, frequency, temperature and wind speed.', $
                   NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDCASE
  
  
  ; Get dimension info
  data_info = SIZE(data, /STRUCTURE)

  ; Plot the data
  IF (data_info.N_DIMENSIONS EQ 2) THEN BEGIN
    SHADE_SURF, data, x, y, $
      XTITLE = xtitle, $
      YTITLE = ytitle, $
      FONT = _font, $
      CHARSIZE = _charsize, $
      PIXELS = 1000
    XYOUTS, 0.5, 0.96, title, $
      FONT = _font, $
      CHARSIZE = _charsize/1.5, $
      /NORMAL, $
      ALIGNMENT = 0.5
  ENDIF ELSE BEGIN
    PLOT, x, data, $
      TITLE  = title, $
      XTITLE = xtitle, $
      YTITLE = ytitle, $
      FONT = _font, $
      CHARSIZE = _charsize/2.0, $
      PSYM = -4, $
      /YNOZERO
  ENDELSE

  
  ; Restore plotting sysvars
  !P = psave
  !X = xsave
  !Y = ysave
  !Z = zsave

END
