;+
; Driver script to compute "deviation-from-boxcar-shape" scores for the
; various "low resolution" digitised ATMS oSRF datasets.
;
PRO atms_osrf_shape_discriminator, $
  sensor_id, $ ; Input. Sensor identifier string; e.g. 'atms_npp'.
  osrf_id  , $ ; Input. Array of RESlow ATMS oSRF ids; e.g. ['sdl.T50_Vnom_LOpri_RESlow',..]
  DEBUG=debug  ; Input keyword
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF
  
  
  ; Parameters
  ONE = 1.0d0
  OSRF_EXTENSION = ".osrf.nc"
  SHAPE_EXTENSION = ".shape.dat"
  
  
  ; Construct oSRF filenames
  osrf_file = sensor_id+"."+osrf_id+OSRF_EXTENSION
  n_osrf_files = N_ELEMENTS(osrf_file)
  ; ...Check their existence
  FOR i = 0, n_osrf_files - 1 DO $
    IF ( NOT (FILE_INFO(osrf_file[i])).EXISTS ) THEN $
      MESSAGE, osrf_file[i]+" not found", /NONAME, /NOPRINT


  ; Read oSRF data
  ; ...Boxcar data
  boxcar_id = "boxcar"
  boxcar_osrf_file = sensor_id+"."+boxcar_id+OSRF_EXTENSION
  boxcar = OBJ_NEW('OSRF_FILE',boxcar_osrf_file,DEBUG=debug)
  boxcar->Read, DEBUG=debug
  boxcar->Get_Property, n_channels=n_channels, DEBUG=debug
  channel = LINDGEN(n_channels)+1
  bosrf = boxcar->Get(channel=channel, DEBUG=debug)
  ; ...All the others
  n_osrfs = N_ELEMENTS(osrf_id)
  osrf  = OBJARR(n_osrfs)
  oosrf = OBJARR(n_channels,n_osrfs)
  FOR j = 0, n_osrfs-1 DO BEGIN
    osrf_file = sensor_id+"."+osrf_id[j]+OSRF_EXTENSION
    osrf[j] = OBJ_NEW('OSRF_FILE',osrf_file, DEBUG=debug)
    osrf[j]->Read, DEBUG=debug
    x = osrf[j]->Get(channel=channel, DEBUG=debug)
    oosrf[*,j] = x
  ENDFOR
  oosrf = TRANSPOSE(oosrf)


  ; Create array to hold shape data and counts
  shape = DBLARR(n_osrfs,n_channels)


  ; Compute the shape factors
  FOR j = 0, n_channels-1 DO BEGIN
    ; Determine the number of bands
    bosrf[j]->Get_Property, N_BANDS=n_bands, DEBUG=debug
    FOR band = 1, n_bands DO BEGIN
      ; Get the boxcar band edge frequencies
      bosrf[j]->Get_Property, band, F1=f1, F2=f2, DEBUG=debug
      ; Loop over the other osrf datasets
      FOR i = 0, n_osrfs-1 DO BEGIN
        ; Get the current band/osrf data
        oosrf[i,j]->Get_Property, band, FREQUENCY=f, RESPONSE=r, N_POINTS=n_points, DEBUG=debug
        r = r/MAX(r)
        ; Find the in-band and out-of-band locations
        idx_inband = WHERE(f GE f1 AND f LE f2, n_inband, $
                           COMPLEMENT=idx_outband, NCOMPLEMENT=n_outband)
        ; Accumulate the shape information
        diffsum = DBLARR(n_Points)
        IF ( n_inband  GT 0 ) THEN diffsum[idx_inband]  = ABS(r[idx_inband]-ONE)
        IF ( n_outband GT 0 ) THEN diffsum[idx_outband] = ABS(r[idx_outband])
        shape[i,j] = shape[i,j] + Integral(f,diffsum)/Integral(f,r)
      ENDFOR
    ENDFOR
  ENDFOR


  ; Write results to file
  FOR i = 0, n_osrfs - 1 DO BEGIN
    outfile = "shape/"+sensor_id+"."+osrf_id[i]+SHAPE_EXTENSION
    OPENW, fid, outfile, /GET_LUN, WIDTH=200
    PRINTF, fid, "! Ch#   Shape Factor"
    FOR l = 0, n_channels-1 DO $
      PRINTF, fid, l+1, shape[i,l], FORMAT='(2x,i3,3x,f10.8)'
    FREE_LUN, fid
  ENDFOR
  

  ; Clean up
  OBJ_DESTROY, [boxcar, osrf]
  OBJ_DESTROY, bosrf
  OBJ_DESTROY, oosrf
END
