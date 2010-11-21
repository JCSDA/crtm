PRO OSRF::Apply_Response_Threshold, $
  new               ,  $ ; Output
  Response_Threshold,  $ ; Input
  Detector = Detector, $ ; Input keyword
  Debug = Debug          ; Input keyword
 
  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Color Definitions
  @color_db
  
  Detector = KEYWORD_SET(Detector) ? Detector : 1L
  
  SOLAR_TEMPERATURE       = 5700L
  TERRESTRIAL_TEMPERATURE = 285L

  self->Assign, new, Debug=Debug
  self->Assign, new_inside, Debug=Debug
  new->Destroy, /No_Clear, Debug=Debug
  new_inside->Destroy, /No_Clear, Debug=Debug
  
  self->Get_Property, $
    n_Bands = n_Bands, $
    Sensor_Type = Sensor_Type, $
    Channel = Channel, $
    Sensor_Id = Sensor_Id
    
  min_inside_freq_idx  = LONARR(n_Bands)
  min_outside_freq_idx = LONARR(n_Bands)
  max_inside_freq_idx  = LONARR(n_Bands)
  max_outside_freq_idx = LONARR(n_Bands)
  n_Points_Inside  = LONARR(n_Bands)
  n_Points_Outside = LONARR(n_Bands)
  f_Inside  = PTRARR(n_Bands)
  r_Inside  = PTRARR(n_Bands)
  f_Outside = PTRARR(n_Bands)
  r_Outside = PTRARR(n_Bands)
  Bounds_Different = 0L
  
  General_Information = ''

  FOR i = 0, n_Bands - 1 DO BEGIN
  
    IF ( Sensor_Type EQ VISIBLE_SENSOR ) THEN $
      T=SOLAR_TEMPERATURE $
    ELSE $
      T=TERRESTRIAL_TEMPERATURE
  
    Band = i + 1
  
    self->Get_Property, $
      Band, $
      Frequency = f, $
      Response = r
      
    min_inside_freq_idx[i]  = 0L
    min_outside_freq_idx[i] = 0L
    max_inside_freq_idx[i]  = N_ELEMENTS(f) - 1L
    max_outside_freq_idx[i] = N_ELEMENTS(f) - 1L

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

    FOR n = 0, maxidx DO BEGIN
      IF ( r[n] GE Response_Threshold ) THEN BEGIN
        CASE n OF
          0: min_outside_freq_idx[i] = 0
          ELSE: min_outside_freq_idx[i] = n - 1
        ENDCASE
        BREAK
      ENDIF
    ENDFOR

    FOR n = n_Orig_Points - 1, maxidx, -1 DO BEGIN 
      IF ( r[n] GE Response_Threshold ) THEN BEGIN
        CASE n OF
          n_Orig_Points - 1: max_outside_freq_idx[i] = n_Orig_Points - 1
          ELSE: max_outside_freq_idx[i] = n + 1
        ENDCASE
        BREAK
      ENDIF
    ENDFOR
   
    n_Points_Inside[i]  = (max_inside_freq_idx[i] - min_inside_freq_idx[i]) + 1L  
    n_Points_Outside[i] = (max_outside_freq_idx[i] - min_outside_freq_idx[i]) + 1L
    
    f_inside[i] = ptr_new(f[min_inside_freq_idx[i]:max_inside_freq_idx[i]])
    r_inside[i] = ptr_new(r[min_inside_freq_idx[i]:max_inside_freq_idx[i]])
    
    f_outside[i] = ptr_new(f[min_outside_freq_idx[i]:max_outside_freq_idx[i]])
    r_outside[i] = ptr_new(r[min_outside_freq_idx[i]:max_outside_freq_idx[i]])
    
    IF ( min_inside_freq_idx[i] GT min_outside_freq_idx[i] OR $
         max_inside_freq_idx[i] LT max_outside_freq_idx[i] ) THEN $
      Bounds_Different = 1L
      
    IF ( Sensor_Type EQ MICROWAVE_SENSOR ) THEN BEGIN      
      IF ( Bounds_Different ) THEN $
        General_Information = General_Information+'For '+Sensor_Id+$
        ' Channel # '+strtrim(Channel,2)+ $
        ' Band # '+strtrim(Band,2)+' and response' $
        +' cutoff of '+strtrim(Response_Threshold,2)+' the' $
        +' outside and/or inside frequencies are different'             
    ENDIF ELSE BEGIN    
      IF ( Bounds_Different ) THEN $          
        General_Information = 'For '+Sensor_Id+' Channel # '+strtrim(Channel,2)+ $
        ' Detector # '+strtrim(Detector,2)+' and response' $
        +' cutoff of '+strtrim(Response_Threshold,2)+' the' $ 
        +' outside and/or inside frequencies are different'     
    ENDELSE 

    
  ENDFOR

  new_inside->Allocate, n_Points_Inside, Debug=Debug
  new->Allocate, n_Points_Outside, Debug=Debug

  FOR i = 0, n_Bands - 1 DO BEGIN
  
    ; Set the frequency and responses
    ; for the inside and outside grids
    new->Set_Property, $
      Band, $
      Frequency=*(f_outside)[i], $
      Response=*(r_outside)[i]
      
    new->Integrate, Debug=Debug
    new->Compute_Central_Frequency, Debug=Debug
    new->Compute_Planck_Coefficients, Debug=Debug
    new->Compute_Polychromatic_Coefficients, Debug=Debug
    
    IF ( Bounds_Different ) THEN $ 
      new_inside->Set_Property, $
        Band, $
        Frequency=*(f_inside)[i], $
        Response=*(r_inside)[i]                      
  ENDFOR  
    
  IF ( Bounds_Different ) THEN BEGIN
    self->Compute_Planck_Radiance, T, Debug=Debug
    self.Convolved_R = self->Convolve(*self.Radiance, Debug=Debug)
    result = Planck_Temperature(self.f0, self.Convolved_R, Teff_Original)

    new->Compute_Central_Frequency, Debug=Debug      
    new->Compute_Planck_Radiance, T, Debug=Debug
    new.Convolved_R = new->Convolve(*new.Radiance, Debug=Debug)
    result = Planck_Temperature(new.f0, new.Convolved_R, Teff_outside)
    
    new_inside->Compute_Central_Frequency, Debug=Debug
    new_inside->Compute_Planck_Radiance, T, Debug=Debug
    new_inside.Convolved_R = new_inside->Convolve(*new_inside.Radiance, Debug=Debug)
    result = Planck_Temperature(new_inside.f0, new_inside.Convolved_R, Teff_inside)
    
    Teff_Difference = Teff_outside - Teff_inside
    
    MESSAGE, General_Information+': Effective Temperature of Outside SRF '+$
             strtrim(Teff_Outside,2)+'K: Effective Temperature of Inside SRF '+$
             strtrim(Teff_Inside,2)+'K: Teff_Outside - Teff_Inside = '+$
             strtrim(Teff_Difference,2)+'K', /INFORMATIONAL
  ENDIF 
  
  IF ( NOT (Sensor_Type EQ MICROWAVE_SENSOR) ) THEN BEGIN
    wplot, f, r, $
          TITLE=Sensor_Id+' ch.'+STRTRIM(Channel,2), $
          XTITLE='Frequency (cm!U-1!N)', $
          YTITLE='Relative Response', $
          /nodata
    woplot, f[0:min_outside_freq_idx[0]], r[0:min_outside_freq_idx[0]], color=cyan

    woplot, f[min_outside_freq_idx[0]:min_inside_freq_idx[0]], r[min_outside_freq_idx[0]:min_inside_freq_idx[0]], $
          color=red
    woplot, f[min_inside_freq_idx[0]:max_outside_freq_idx[0]], r[min_inside_freq_idx[0]:max_outside_freq_idx[0]], $
          color=green
    woplot, f[max_inside_freq_idx[0]:max_outside_freq_idx[0]], r[max_inside_freq_idx[0]:max_outside_freq_idx[0]], $
          color=red
    woplot, f[max_outside_freq_idx[0]:N_ELEMENTS(f)-1], r[max_outside_freq_idx[0]:N_ELEMENTS(f)-1], $
          color=cyan
    q=get_kbrd(1)
  ENDIF
  
  ptr_free, f_outside, r_outside, f_inside, r_inside
 
  ; Done
  CATCH, /CANCEL
  
END ; PRO OSRF::Apply_Response_Threshold
