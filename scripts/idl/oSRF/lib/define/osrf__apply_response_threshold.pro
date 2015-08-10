;+
; oSRF method to apply a response threshold to an SRF.
;
; Note that the threshold is an ABSOLUTE, not relative value.
;

PRO oSRF::Apply_Response_Threshold, $
  Response_Threshold, $  ; Input
  Debug = Debug          ; Input keyword
;-

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Assign input to local variables
  _response_threshold = Response_Threshold


  ; Check if structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Extract dimension and info from oSRF
  self->Get_Property, Debug=Debug, $
    n_Bands          = n_bands          , $
    Channel          = channel          , $
    Sensor_Id        = sensor_id        , $
    Sensor_Type      = sensor_type      , $
    WMO_Satellite_Id = wmo_satellite_id , $
    f0               = original_f0
  sensor_id      = STRTRIM(sensor_id,2)
  channel_string = ' channel ' + STRTRIM(channel,2)


  ; Main loop for applying threshold
  FOR n_attempts = 0, F0_DIFFERENCE_MAXIMUM_ITERATION DO BEGIN

    ; Exit if too many attempts made
    IF ( n_attempts EQ F0_DIFFERENCE_MAXIMUM_ITERATION ) THEN BEGIN
      apply_threshold = FALSE
      MESSAGE, 'Maximum threshold application count reached for ' + $
             sensor_id + channel_string + $
             '. Zero threshold applied.', $
             /INFORMATIONAL
      _response_threshold = ZERO
    ENDIF


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
        IF (r[i] LT _response_threshold ) THEN BREAK
      inner_low_idx = i > 0L
      ; ...high-frequency side
      FOR i = max_idx, np - 1L DO $
        IF (r[i] LT _response_threshold ) THEN BREAK
      inner_high_idx = i < (np - 1L)


      ; Find outer cutoff points
      ; ...low-frequency side
      FOR i = 0L, max_idx DO $
        IF (r[i] GT _response_threshold ) THEN BREAK
      outer_low_idx = (i - 1L) > 0L
      ; ...high-frequency side
      FOR i = np - 1L, max_idx, -1L DO $
        IF (r[i] GT _response_threshold ) THEN BREAK
      outer_high_idx = (i + 1L) < (np - 1L)


      ; Issue warning if inner and outer indices differ
      IF ( (inner_low_idx  NE outer_low_idx OR inner_high_idx NE outer_high_idx) AND $
           KEYWORD_SET(debug) ) THEN BEGIN
        band_string = (n_bands GT 1) ? ', band ' + STRTRIM(band,2) : ''
        MESSAGE, 'Inner and outer cutoff points are different for ' + $
                 sensor_id + channel_string + band_string, $
                 /INFORMATIONAL

; *** REMOVE THIS AND (somehow) PUT IN Plot METHOD ***
;
;        ; Plot SRF data for inspection
;        IF ( Plot_Data ) THEN BEGIN
;          title = sensor_id + channel_string + ' threshold cutoff discrepancy'
;          p = PLOT( f, r, $
;                    TITLE=title, $
;                    XTITLE='Frequency (cm!U-1!N)', $
;                    YTITLE='Relative Response', $
;                    XTICKFONT_SIZE=9, $
;                    YTICKFONT_SIZE=9, $
;                    SYMBOL='diamond', $
;                    SYM_SIZE=0.6, $
;                    BUFFER=Buffer, $
;                    WINDOW_TITLE = title )
;          p.Refresh, /DISABLE
;          ; ...Plot response threshold
;          !NULL = PLOT(p.Xrange, [_response_threshold, _response_threshold], $
;                       OVERPLOT=p, LINESTYLE='dash')
;          ; ...Plot maximum SRF value point
;          max_f = [f[max_idx],f[max_idx]]
;          !NULL = PLOT(max_f, p.Yrange, OVERPLOT=p, LINESTYLE='dash')
;          ; ...Plot inner cutoff points
;          inner_low_f = [f[inner_low_idx],f[inner_low_idx]]
;          !NULL = PLOT(inner_low_f, p.Yrange, OVERPLOT=p, COLOR='red')
;          inner_high_f = [f[inner_high_idx],f[inner_high_idx]]
;          !NULL = PLOT(inner_high_f, p.Yrange, OVERPLOT=p, COLOR='red')
;          ; ...Plot outer cutoff points
;          outer_low_f = [f[outer_low_idx],f[outer_low_idx]]
;          !NULL = PLOT(outer_low_f, p.Yrange, OVERPLOT=p, COLOR='green')
;          outer_high_f = [f[outer_high_idx],f[outer_high_idx]]
;          !NULL = PLOT(outer_high_f, p.Yrange, OVERPLOT=p, COLOR='green')
;          p.Refresh
;          ; ...Save the object reference for this band
;          gRef[band] = p
;
;          ; Only pause if not at last band
;          IF ( Plot_Pause AND (band LT n_bands) ) THEN BEGIN
;            PRINT, FORMAT='(/5x,"Press <ENTER> to continue, Q to quit, S to stop.")'
;            q = GET_KBRD(1)
;            IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
;            IF ( STRUPCASE(q) EQ 'S' ) THEN STOP
;          ENDIF
;        ENDIF

      ENDIF

      ; Add truncated SRFs to the end of the list
      frequency.Add, f[outer_low_idx:outer_high_idx]
      response.Add,  r[outer_low_idx:outer_high_idx]
      n_points[n] = outer_high_idx - outer_low_idx + 1L


    ENDFOR  ; Band loop


    ; Load the truncated SRF data into a new oSRF object
    new = oSRF()
    ; ...Reallocate the object to the new data size(s)
    new->Allocate, n_points, Debug=Debug
    ; ...Clear flags
    new->Clear_Flag, $
      Debug=Debug, $
      /All
    new->Set_Property, $
         Debug = Debug, $
         Sensor_Id        = sensor_id       , $
         Sensor_Type      = sensor_type     , $
         Channel          = channel         , $
         WMO_Satellite_Id = wmo_satellite_id, $
         WMO_Sensor_Id    = wmo_sensor_id
    ; ...Set the data values
    FOR n = 0, n_bands - 1 DO BEGIN
      band = n+1
      f  = frequency[n]
      r  = response[n]
      new->Set_Property, $
        band, $
        Debug=Debug, $
        Frequency = f, $
        Response  = r
    ENDFOR


    ; Compute the various SRF parameters for the new object
    new->Integrate, Debug=Debug
    new->Compute_Central_Frequency, Debug=Debug
    new->Compute_Planck_Coefficients, Debug=Debug
    new->Compute_Polychromatic_Coefficients, Debug=Debug


    ; Compare central frequencies for indication of suitability of threshold
    new->Get_Property, Debug=Debug, f0=f0
    f0_difference_percentage = 100.0d0 * ABS(f0 - original_f0)/original_f0
    IF ( (f0_difference_percentage GT FO_DIFFERENCE_PERCENTAGE_CUTOFF) AND $
         n_attempts LT F0_DIFFERENCE_MAXIMUM_ITERATION ) THEN BEGIN
      MESSAGE, 'Pre- and post-threshold SRF central frequencies for ' + $
               sensor_id + channel_string + $
               ' differ by ' + $
               STRING(f0_difference_percentage,FORMAT='(e13.6,"%. ")') + $
               'Decreasing threshold and trying again....', $
               /INFORMATIONAL
    ENDIF ELSE BREAK


    ; Decrease the response threshold
    _response_threshold = _response_threshold / 2.0d0


  ENDFOR ; n_attempts Loop


  ; Copy new object to self
  new->Assign, self, Debug=Debug


END
