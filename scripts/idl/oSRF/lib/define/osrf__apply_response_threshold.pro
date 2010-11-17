PRO OSRF::Apply_Response_Threshold, $
  new               , $ ; Output
  Response_Threshold, $ ; Input
  Detector = Detector   ; Keyword argument
  
  Debug = Debug
  
  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  Detector = KEYWORD_SET(Detector) ? Detector : 1L

  self->Assign, new, Debug=Debug
  new->Destroy, /No_Clear, Debug=Debug
  
  self->Get_Property, $
    n_Bands = n_Bands, $
    Sensor_Type = Sensor_Type, $
    Channel = Channel, $
    Sensor_Id = Sensor_Id
    
  min_inside_freq_idx  = INTARR(n_Bands)
  min_outside_freq_idx = INTARR(n_Bands)
  max_inside_freq_idx  = INTARR(n_Bands)
  max_outside_freq_idx = INTARR(n_Bands)
  n_Points = INTARR(n_Bands)
    
  FOR i = 0, n_Bands - 1 DO BEGIN
  
    Band = i + 1
  
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response = r

    n_Orig_Points = N_ELEMENTS(f)     

    maxval = MAX(r, maxidx)
    FOR n = maxidx, 0, -1 DO BEGIN  
      IF ( r[n] LT Response_Threshold ) THEN BEGIN
        min_inside_freq_idx[i] = n 
        BREAK
      ENDIF
    ENDFOR

    FOR n = maxidx, n_Orig_Points - 1 DO BEGIN
      IF ( r[n] LT Response_Threshold ) THEN BEGIN
        max_inside_freq_idx[i] = n 
        BREAK
      ENDIF
    ENDFOR

    FOR n = 0, n_Orig_Points - 1 DO BEGIN
      IF ( r[n] GE Response_Threshold ) THEN BEGIN
        min_outside_freq_idx[i] = n - 1
        BREAK
      ENDIF
    ENDFOR

    FOR n = n_Orig_Points - 1, 0, -1 DO BEGIN 
      IF ( r[n] GE Response_Threshold ) THEN BEGIN
        max_outside_freq_idx[i] = n + 1
        BREAK
      ENDIF
    ENDFOR
    
    f=f[min_outside_freq_idx[i]:max_outside_freq_idx[i]]
    r=r[min_outside_freq_idx[i]:max_outside_freq_idx[i]] 
    
    n_Points[i]=N_ELEMENTS(f)
    
  ENDFOR
        
  new->Allocate, n_Points, Debug=Debug
  
  FOR i = 0, n_Bands - 1 DO BEGIN
  
    Band = i + 1
  
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response = r
      
    f=f[min_outside_freq_idx[i]:max_outside_freq_idx[i]]
    r=r[min_outside_freq_idx[i]:max_outside_freq_idx[i]]
  
    IF ( Sensor_Type EQ MICROWAVE_SENSOR ) THEN BEGIN
      IF ( min_inside_freq_idx[i] GT min_outside_freq_idx[i] ) THEN $
        PRINT , 'For '+Sensor_Id+' the response cutoff '+strtrim(Response_Threshold,2)+ $
                ' may result in an incorrect minimum frequency for band # '+ $
                 strtrim(Band,2)+' Channel # '+strtrim(Channel,2)

      IF ( max_inside_freq_idx[i] LT max_outside_freq_idx[i] ) THEN $
        PRINT , 'For '+Sensor_Id+' the response cutoff '+strtrim(Response_Threshold,2)+ $
                ' may result in an incorrect maximum frequency for band # '+ $
                strtrim(Band,2)+' Channel # '+strtrim(Channel,2)
    ENDIF ELSE BEGIN
      IF ( min_inside_freq_idx[i] GT min_outside_freq_idx[i] ) THEN $
        PRINT , 'For '+Sensor_Id+' the response cutoff '+strtrim(Response_Threshold,2)+ $
                ' may result in an incorrect minimum frequency for channel #' + $
                strtrim(Channel,2)+' Detector # '+strtrim(Detector,2)
      
      IF ( max_inside_freq_idx[i] LT max_outside_freq_idx[i] ) THEN $
        PRINT , 'For '+Sensor_Id+' the response cutoff '+strtrim(Response_Threshold,2)+ $
                ' may result in an incorrect maximum frequency for channel #' + $
                strtrim(Channel,2)+' Detector # '+strtrim(Detector,2)
    ENDELSE

    ; Set the frequency and response
    new->Set_Property, $
      Band, $
      Frequency=f, $
      Response=r  
      
  ENDFOR  
    
  ; Done
  CATCH, /CANCEL
  
END ; PRO OSRF::Apply_Response_Threshold
      
       
