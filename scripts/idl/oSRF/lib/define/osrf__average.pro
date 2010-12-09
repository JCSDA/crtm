;+
; NAME:
;       OSRF::Average
;
; PURPOSE:
;       The OSRF::Average procedure method averages an array of input
;       SRF objects (e.g. for different detectors)
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Average, $
;         iSRF         , $ ; Input
;         Debug = Debug    ; Input keyword
;
; INPUTS:
;       iSRF:        Array of oSRF objects containing the interpolated reponse data to
;                    be averaged.
;                    UNITS:      N/A
;                    TYPE:       OBJ(oSRF)
;                    DIMENSION:  Rank-1
;                    ATTRIBUTES: INTENT(IN)
;                    
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       Given an array of oSRF objects, isrf, their average can be computed
;       like so
;
;         IDL> avgsrf = OBJ_NEW('oSRF')
;         IDL> avgsrf->Average, isrf, Debug=Debug
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Nov-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Average, $
  iSRF           , $ ; Input data
  Debug       = Debug  
  
  ; Set up
  ; ...oSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  ; Tolerance below frequency minimum for the average grid
  Tolerance_Below_Min = DOUBLE(-1E-5)
  
  ; Tolerance above frequency maximum for the average grid
  Tolerance_Above_Max = DOUBLE(1E-5)
  
  ; ...Assign non-averaging dependent metadata
  isrf[0]->Get_Property, $
      Channel     = channel    , $
      Sensor_Type = sensor_type, $
      Debug=Debug      
  self->Set_Property, $
      Channel     = channel    , $
      Sensor_Type = sensor_type, $
      Debug=Debug
  
  ; The number of SRFs to average
  n_sets = N_ELEMENTS(iSRF)

  ; Determine frequency min/max for all bands
  ; ...Get the number of bands
  isrf[0]->Get_Property, n_Bands=n_bands, Debug=Debug

  ; ...Initialise min/max arrays
  fmin = MAKE_ARRAY(n_bands, VALUE=-1.0d+10)
  fmax = MAKE_ARRAY(n_bands, VALUE= 1.0d+10)

  ; ...Get the band frequency limits
  FOR n = 0, n_sets-1 DO BEGIN
    FOR i = 0, n_bands-1 DO BEGIN
      band = i+1
      isrf[n]->Get_Property, band, f1=f1, f2=f2, Debug=Debug
      fmin[i] = fmin[i] > f1
      fmax[i] = fmax[i] < f2
    ENDFOR
  ENDFOR

  ; Average the band data, looping in opposite order
  band_data = LIST(LENGTH=n_bands)
  n_points  = LONARR(n_bands)
  FOR i = 0, n_bands-1 DO BEGIN
    band = i+1
    FOR n = 0, n_sets-1 DO BEGIN

      ; Get the data
      isrf[n]->Get_Property, band, Frequency=f, Response=r, Debug=Debug

      ; Determine extraction indices
      idx = WHERE( (f - fmin[i]) GE Tolerance_Below_Min  AND $
                   (f - fmax[i]) LE Tolerance_Above_Max, count)
      IF ( count EQ 0 ) THEN $
        MESSAGE, 'No data within frequency limits for set#'+STRTRIM(n+1,2)+' band#'+STRTRIM(band,2), $
                 NODATA=MsgSwitch, NOPRINT=MsgSwitch

      ; Extract and sum the data
      IF ( n EQ 0 ) THEN BEGIN
        n_points[i] = count
        frequency   = f[idx]
        response    = r[idx]
      ENDIF ELSE BEGIN
        IF ( count NE n_points ) THEN $
          MESSAGE, 'No. of extracted data points for band#'+STRTRIM(band,2)+' is different!', $
                   NODATA=MsgSwitch, NOPRINT=MsgSwitch
        response = response + r[idx]
      ENDELSE
    ENDFOR ; iSRF set loop
    
    ; Save averaged data for this band
    band_data[i] = LIST(frequency, response/DOUBLE(n_sets))
    
  ENDFOR ; Band loop

  ; Store the averaged data back into the object
  ; ...Reallocate the object
  self->Allocate, n_points, Debug=Debug
  ; ...Set the SRF data
  FOR i = 0, n_bands-1 DO BEGIN
    band = i+1
    f = (band_data[i])[0]
    r = (band_data[i])[1]
    self->Set_Property, band, Frequency=f, Response=r, Debug=Debug
  ENDFOR

  ; Recompute the various SRF parameters
  self->Integrate, Debug=Debug
  self->Compute_Central_Frequency, Debug=Debug
  self->Compute_Planck_Coefficients, Debug=Debug
  self->Compute_Polychromatic_Coefficients, Debug=Debug

END

