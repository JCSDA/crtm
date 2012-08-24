FUNCTION f1_edge, f1, df
  d = 1.0d0/df
  RETURN, DOUBLE(LONG(f1*d))/d
END
FUNCTION f2_edge, f2, df
  d = 1.0d0/df
  RETURN, DOUBLE(LONG(f2*d)+1L)/d
END
FUNCTION n_f_points, f1, f2, df
  RETURN, LONG((f2-f1)/df + 1.5d0)
END

PRO OSRF::Compute_Interpolation_Frequency, $
  new, $  ; Output
  n_Points_per_Band=n_Points_per_Band, $  ; MW Keyword
  LoRes=LoRes, $  ; IR/VIS Keyword
  HiRes=HiRes, $  ; IR/VIS Keyword
  Debug=Debug

  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
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
      f1 = *self.f1
      f2 = *self.f2
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
      f1 = f1_edge(*self.f1,df)
      f2 = f2_edge(*self.f2,df)
      n_Points = n_f_points(f1, f2, df)
    END
    
    ; Unknown instruments
    ELSE: MESSAGE, 'Cannot process this sensor type, '+STRTRIM(self.Sensor_Type,2), $
                   NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDCASE
  
  
  ; Allocate the new OSRF                 
  new->Allocate, n_Points, Debug=Debug

;!!!! HERE COPY OVER THE OTHER, SCALAR, STUFF !!!!

  ; Fill each band frequency array
  FOR i = 0L, new.n_Bands-1L DO BEGIN
    f = DINDGEN(n_Points[i])/DOUBLE(n_Points[i]-1L)
    f = f*(f2[i]-f1[i]) + f1[i]
    new->Set_Property, i+1, Frequency=f, Debug=Debug
  ENDFOR

END ; PRO OSRF::Compute_Interpolation_Frequency
