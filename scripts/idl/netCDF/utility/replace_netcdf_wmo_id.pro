;+
;
; Utility procedure to replace the WMO Satellite and Sensor Id Global Attribute
; values in netCDF files.
; 
PRO Replace_netCDF_WMO_Id, $
  NCfile          , $  ; Input. Name of netCDF file in which to make the changes
  WMO_Satellite_Id, $  ; Input. New WMO satellite identifier
  WMO_Sensor_Id        ; Input. New WMO sensor identifier
;-

  ; Set up error handler
  @error_codes
  CATCH, err_stat
  IF ( err_stat NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  ; Replace the sensor attributes
  Replace_netCDF_GAtt, NCfile, 'WMO_Satellite_Id', LONG(WMO_Satellite_Id)
  Replace_netCDF_GAtt, NCfile, 'WMO_Sensor_Id'   , LONG(WMO_Sensor_Id)

END
