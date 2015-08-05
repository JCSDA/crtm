FUNCTION f1_edge, f1, df
  COMPILE_OPT HIDDEN
  d = 1.0d0/df
  n_bands = f1.Count()
  f = HASH()
  FOR band = 1, n_bands DO BEGIN
    f[band] = DOUBLE(LONG(f1[band]*d))/d
    WHILE (f[band] LT f1[band]) DO f[band] = f[band] + df
  ENDFOR
  RETURN, f
END

FUNCTION f2_edge, f2, df
  COMPILE_OPT HIDDEN
  d = 1.0d0/df
  n_bands = f2.Count()
  f = HASH()
  FOR band = 1, n_bands DO BEGIN
    f[band] = DOUBLE(LONG(f2[band]*d)+1L)/d
    WHILE (f[band] GT f2[band]) DO  f[band] = f[band] - df
  ENDFOR
  RETURN, f
END

FUNCTION n_f_points, f1, f2, df
  COMPILE_OPT HIDDEN
  n_bands = f1.Count()
  n_points = []
  FOR band = 1, n_bands DO BEGIN
    n_points = [n_points, LONG((f2[band]-f1[band])/df + 1.5d0)]
  ENDFOR
  RETURN, n_points
END

;+
PRO OSRF::Compute_Interpolation_Frequency, $
  new          , $  ; Output
  Debug = Debug, $  ; Input keyword
  LoRes = LoRes, $  ; IR/VIS Keyword. DEFAULT. This keyword also has precendence over HiRes.
  HiRes = HiRes     ; IR/VIS Keyword
;-
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Process keywords
  df = DF_LORES
  hires_flag = FALSE
  IF ( KEYWORD_SET(HiRes)) THEN BEGIN
    df = DF_HIRES
    hires_flag = TRUE
  ENDIF
  IF ( KEYWORD_SET(LoRes)) THEN BEGIN
    df = DF_LORES
    hires_flag = FALSE
  ENDIF


  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'oSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check that the object argument is an oSRF object
  IF ( ~ OBJ_ISA(new,'oSRF') ) THEN $
    MESSAGE, 'Output object class is not oSRF.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; If not an IR or VIS sensor, just copy and return
  IF ( (self.Sensor_Type NE INFRARED_SENSOR) AND $
       (self.Sensor_Type NE VISIBLE_SENSOR ) ) THEN BEGIN
    MESSAGE, 'Just copying. No interpolation performed for this sensor type: ' + $
             SENSOR_TYPE_NAME[self.Sensor_Type], /INFORMATIONAL
    self.Assign, new, Debug = debug
    RETURN
  ENDIF


  ; Determine interpolation limit frequencies and n_Points
  f1 = f1_edge(self.f1,df)
  f2 = f2_edge(self.f2,df)
  n_Points = n_f_points(f1, f2, df)
  
  
  ; Allocate the new OSRF
  new.Allocate, n_Points, Debug=Debug


  ; Set other frequency-independent properties
  new.Set_Property, $
    Debug            = Debug                , $  ; Input keyword
    Version          = self.Version         , $  ; Input keyword
    Sensor_Id        = self.Sensor_Id       , $  ; Input keyword
    WMO_Satellite_Id = self.WMO_Satellite_Id, $  ; Input keyword
    WMO_Sensor_Id    = self.WMO_Sensor_Id   , $  ; Input keyword
    Sensor_Type      = self.Sensor_Type     , $  ; Input keyword
    Channel          = self.Channel              ; Input keyword


  ; Set the interpolation resolution flag
  new.Set_Flag, Is_HiRes = hires_flag
  
  
  ; Fill each band frequency array
  new.Get_Property, n_Bands=n_bands
  FOR i = 0L, n_bands-1L DO BEGIN
    band = i+1L
    f = DINDGEN(n_Points[i])/DOUBLE(n_Points[i]-1L)
    f = f*(f2[band]-f1[band]) + f1[band]
    new.Set_Property, band, Frequency=f, Debug=Debug
  ENDFOR

END
