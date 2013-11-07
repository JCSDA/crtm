;+
; oSRF method to compute the polychromatic coefficients for an SRF.
;

PRO OSRF::Compute_Polychromatic_Coefficients, $
  Debug = Debug  ; Input keyword
;-

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler


  ; Check if object has been allocated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'OSRF object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the sensor type
  self.Get_Property, Sensor_Type=sensor_type, Debug=Debug


  ; Compute the central frequency if necessary
  IF ( ~ self.Flag_Is_Set(F0_COMPUTED_FLAG) ) THEN self.Compute_Central_Frequency, Debug=Debug

  
  ; Copy the SRF into a work object
  self.Assign, work, Debug=Debug

  
  ; Generate the "monochromatic" temperatures
  ; ...Set min and max temps based on Sensor_Type
  IF ( sensor_type EQ VISIBLE_SENSOR ) THEN BEGIN
    min_T        = VISIBLE_BC_MIN_T
    max_T        = VISIBLE_BC_MAX_T
    d_T          = VISIBLE_BC_D_T
    dT_threshold = VISIBLE_BC_DT_THRESHOLD
  ENDIF ELSE BEGIN 
    min_T        = BC_MIN_T
    max_T        = BC_MAX_T
    d_T          = BC_D_T
    dT_threshold = BC_DT_THRESHOLD
  ENDELSE
  ; ...Compute the set of temperatures
  T    = oSRF_Compute_Temperatures(min_T, max_T, d_T)
  n_T  = N_ELEMENTS(T)
  Teff = DBLARR(n_T)


  ; Get the central frequency
  work.Get_Property, f0=f0, Debug=Debug


  ; Generate the polychromatic temperatures
  FOR i = 0L, n_T-1L DO BEGIN
    ; ...Compute the monochromatic Planck radiances
    work.Compute_Planck_Radiance, T[i], Debug=Debug
    ; ...Convolve Planck radiance with SRF
    work.Convolved_R = work.Convolve(work.Radiance, Debug=Debug) 
    ; ...Convert convolved radiance back to temperature
    result = Planck_Temperature(f0, work.Convolved_R, x)
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error computing effective temperature at T='+STRING(T[i],FORMAT='(f5.1)'), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    Teff[i] = x
  ENDFOR
  
  
  ; Perform the polynomial fit
  Degree_of_Fit = N_POLYCHROMATIC_COEFFS-1L
  x = POLY_FIT( T, Teff, Degree_of_Fit, /DOUBLE, STATUS=status, YFIT=tfit, YERROR=terror )
  IF ( status NE 0 ) THEN $
    MESSAGE, 'Error performing polynomial fit', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Check result
  IF ( terror GT dT_threshold ) THEN BEGIN
    work.Get_Property, Sensor_Id = sensor_id, Channel = channel, Debug = debug
    filename = STRTRIM(sensor_id,2)+'-'+STRTRIM(channel,2)+'.tfit.dat'
    OPENW, lun, filename, /GET_LUN
    PRINTF, lun, $
      STRTRIM(sensor_id,2), STRTRIM(channel,2), terror, $
      FORMAT='("! ",/,"! Polychromatic coefficient fit for ",a," channel ",a,/2x,"StdError=",e17.10)'
    PRINTF, lun, $
      FORMAT='("! ",/,"! Index       T          Teff          Tfit         Teff-Tfit")'
    dT_fit = ABS(Teff - tfit)
    FOR i = 0L, n_T-1 DO BEGIN
      PRINTF, lun, $
        i, T[i], Teff[i], tfit[i], dT_fit[i], $
        FORMAT='(1x,i5,3(1x,f13.8),1x,e17.10)'
    ENDFOR
    FREE_LUN, lun
    MESSAGE, 'Fit of effective temperatures yielded (Teff-Tfit) differences > ' + $
             STRING(dT_threshold,FORMAT='(e13.6)') + ". See " + STRTRIM(filename,2), $
             /INFORMATIONAL
  ENDIF


; *** REMOVE THIS AND PUT IN Plot METHOD ***
;  ; Plot the fit data
;  IF ( Plot_Data ) THEN BEGIN
;    work.Get_Property, Sensor_Id=sensor_id, Channel=channel, Debug=Debug
;    title_fmt='(/2x,"Polychromatic coefficient fit for ",a," channel ",a,"!CStdError=",e17.10)'
;    gRef = PLOT(T,Teff - tfit, $
;                XTITLE='Temperature (K)', $
;                YTITLE='T!Deff!N - T!Dfit!N (K)', $
;                TITLE=STRING(STRTRIM(sensor_id,2), STRTRIM(channel,2), terror, FORMAT=title_fmt),$
;                FONT_SIZE = 9, $
;                WINDOW_TITLE = sensor_id+', channel '+STRTRIM(channel,2)+' (Teff-Tfit) differences')
;    !NULL = PLOT(gRef.Xrange,[0,0],LINESTYLE='dashed',/OVERPLOT)
;  ENDIF


  ; Save results in original object
  self.Polychromatic_Coeffs = x


  ; Cleanup
  OBJ_DESTROY, work, Debug=Debug
  
END
