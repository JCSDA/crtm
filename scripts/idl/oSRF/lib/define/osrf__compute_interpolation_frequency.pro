FUNCTION f1_edge, f1, df
  COMPILE_OPT HIDDEN
  d = 1.0d0/df
  n_bands = f1.Count()
  f = HASH()
  FOR band = 1, n_bands DO BEGIN
    f[band] = DOUBLE(LONG(f1[band]*d))/d
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
    remainder = f2[band] - LONG(f2[band]/df)*df  ; MOD doesn't always work.
    IF ( (remainder EQ 0.0d0) AND (f[band] GT f2[band]) ) THEN $
      f[band] = DOUBLE(LONG(f2[band]*d))/d
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
  new                                  , $  ; In/Output
  Debug             = Debug            , $  ; Input keyword
  n_Points_per_Band = n_Points_per_Band, $  ; MW Keyword
  LoRes             = LoRes            , $  ; IR/VIS Keyword
  HiRes             = HiRes                 ; IR/VIS Keyword
;-
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'oSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Check that the object argument is an oSRF object
  IF ( ~OBJ_ISA(new,'oSRF') ) THEN $
    MESSAGE, 'Output object class is not oSRF.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Process keywords based on sensor type
  CASE 1 OF
    ; Microwave instruments
    (self.Sensor_Type EQ MICROWAVE_SENSOR): $
      BEGIN
        ; ...Check keyword was set
        IF ( N_ELEMENTS(n_Points_per_Band) EQ 0 ) THEN $
          MESSAGE, 'Nothing to do for '+SENSOR_TYPE_NAME[self.Sensor_Type]+' sensor....', $
                   NONAME=MsgSwitch, NOPRINT=MsgSwitch
        ; ...Determine interpolation limit frequencies and n_Points
        f1 = self.f1
        f2 = self.f2
        n_Points = REPLICATE(LONG(n_Points_per_Band[0]), self.n_Bands)
      END
    
    ; Infrared or visible instruments
    (self.Sensor_Type EQ INFRARED_SENSOR) OR $
    (self.Sensor_Type EQ VISIBLE_SENSOR ): $
      BEGIN
        ; ...Process keywords
        CASE 1 OF
          KEYWORD_SET(HiRes): df = DF_HIRES
          KEYWORD_SET(LoRes): df = DF_LORES
          ELSE: MESSAGE, 'Nothing to do for '+SENSOR_TYPE_NAME[self.Sensor_Type]+' sensor....', $
                         NONAME=MsgSwitch, NOPRINT=MsgSwitch
        ENDCASE
        ; ...Determine interpolation limit frequencies and n_Points
        f1 = f1_edge(self.f1,df)
        f2 = f2_edge(self.f2,df)
        n_Points = n_f_points(f1, f2, df)
      END
    
    ; Unknown instruments
    ELSE: MESSAGE, 'Cannot process this sensor type, '+STRTRIM(self.Sensor_Type,2), $
                   NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDCASE
  
  
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


  ; Fill each band frequency array
  new.Get_Property, n_Bands=n_bands
  FOR i = 0L, n_bands-1L DO BEGIN
    band = i+1L
    f = DINDGEN(n_Points[i])/DOUBLE(n_Points[i]-1L)
    f = f*(f2[band]-f1[band]) + f1[band]
    new.Set_Property, band, Frequency=f, Debug=Debug
  ENDFOR

END
