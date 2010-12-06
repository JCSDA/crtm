;+
; oSRF method to apply a response threshold to an SRF.
;
; Plots of results are only generated if the threshold cutoffs
; occur at the same indices when applied from outer edges in,
; and from the SRF centre out.

PRO oSRF::Apply_Response_Threshold, $
  Response_Threshold , $  ; Input
  No_Plot  = No_Plot , $  ; Input keyword
  Debug    = Debug        ; Input keyword
;-
 
  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Check keywords
  Plot_Data  = NOT KEYWORD_SET(No_Plot)
  Plot_Pause = NOT KEYWORD_SET(No_Pause)


  ; Extract dimension and info from oSRF
  self->Get_Property, Debug=Debug, $
    n_Bands = n_bands, $
    Channel = channel, $
    Sensor_Id = sensor_id
  sensor_id = STRTRIM(sensor_id,2)
  
  
  ; Create structures for data
  n_points = LONARR(n_bands)
  frequency = LIST()
  response  = LIST()


  ; Begin band loop 
  FOR n = 0, n_Bands-1 DO BEGIN
    band = n+1
    
    ; Get the current band data
    self->Get_Property, Debug=Debug, $
      band, $
      n_Points  = np, $
      Frequency = f, $
      Response  = r
    
    ; Determine max value index for current band
    max_r = MAX(r, max_idx)


    ; Find inner cutoff points
    ; ...low-frequency side
    FOR i = max_idx, 0L, -1L DO $
      IF (r[i] LT Response_Threshold ) THEN BREAK
    inner_low_idx = i + 1L
    ; ...high-frequency side
    FOR i = max_idx, np-1L DO $
      IF (r[i] LT Response_Threshold ) THEN BREAK
    inner_high_idx = i - 1L


    ; Find outer cutoff points
    ; ...low-frequency side
    FOR i = 0, max_idx DO $
      IF (r[i] GT Response_Threshold ) THEN BREAK
    outer_low_idx = i
    ; ...high-frequency side
    FOR i = np-1L, max_idx, -1L DO $
      IF (r[i] GT Response_Threshold ) THEN BREAK
    outer_high_idx = i


    ; Issue warning if inner and outer indices differ
    IF ( inner_low_idx  NE outer_low_idx OR $
         inner_high_idx NE outer_high_idx ) THEN BEGIN
      channel_string = ' channel ' + STRTRIM(channel,2)
      band_string    = (n_bands GT 1) ? ', band ' + STRTRIM(band,2) : ''
      MESSAGE, 'Inner and outer cutoff points are different for ' + $
               sensor_id + channel_string + band_string, $
               /INFORMATIONAL
               
      ; Plot SRF data for inspection
      IF ( Plot_Data ) THEN BEGIN
        p = PLOT( f, r, $
                  TITLE=sensor_id + channel_string + ' threshold cutoff discrepancy', $
                  XTITLE='Frequency (cm!U-1!N)', $
                  YTITLE='Relative Response', $
                  SYMBOL='diamond' )
        p.Refresh, /DISABLE
        ; ...Plot response threshold
        !NULL = PLOT(p.Xrange, [Response_Threshold, Response_Threshold], $
                     OVERPLOT=p, LINESTYLE='dash')
        ; ...Plot maximum SRF value point
        max_f = [f[max_idx],f[max_idx]]
        !NULL = PLOT(max_f, p.Yrange, OVERPLOT=p, LINESTYLE='dash')
        ; ...Plot inner cutoff points
        inner_low_f = [f[inner_low_idx],f[inner_low_idx]]
        !NULL = PLOT(inner_low_f, p.Yrange, OVERPLOT=p, COLOR='red')
        inner_high_f = [f[inner_high_idx],f[inner_high_idx]]
        !NULL = PLOT(inner_high_f, p.Yrange, OVERPLOT=p, COLOR='red')
        ; ...Plot outer cutoff points
        outer_low_f = [f[outer_low_idx],f[outer_low_idx]]
        !NULL = PLOT(outer_low_f, p.Yrange, OVERPLOT=p, COLOR='green')
        outer_high_f = [f[outer_high_idx],f[outer_high_idx]]
        !NULL = PLOT(outer_high_f, p.Yrange, OVERPLOT=p, COLOR='green')
        p.Refresh

        ; Pause for reflection
        PRINT, FORMAT='(/5x,"Press <ENTER> to continue, Q to quit, S to stop.")'
        q = GET_KBRD(1)
        IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
        IF ( STRUPCASE(q) EQ 'S' ) THEN STOP
      ENDIF
      
    ENDIF

    ; Add truncated SRFs to the end of the list
    frequency.Add, f[inner_low_idx:inner_high_idx]
    response.Add,  r[inner_low_idx:inner_high_idx]
    n_points[n] = inner_high_idx - inner_low_idx + 1L
    

  ENDFOR


  ; Load the truncated SRF data into the oSRF object
  ; ...Reallocate the object to the new data size(s)
  self->Allocate, n_points, Debug=Debug
  ; ...Clear flags
  self->Clear_Flag, $
    Debug=Debug, $
    /All
  ; ...Set the data values
  FOR n = 0, n_bands - 1 DO BEGIN
    band = n+1
    f  = frequency.Remove(n)
    r  = response.Remove(n)
    self->Set_Property, $
      band, $
      Debug=Debug, $
      Frequency = f, $
      Response  = r
  ENDFOR
  
END ; PRO OSRF::Apply_Response_Threshold
