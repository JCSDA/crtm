;+
; Function to write the global attributes to a netCDF SRF data file.
;
FUNCTION WriteGAtts_SRF_netCDF, Filename                         , $  ; Input
                                FileId                           , $  ; Input
                                Version         =Version         , $  ; Optional input
                                Sensor_Id       =Sensor_Id       , $  ; Optional input
                                WMO_Satellite_Id=WMO_Satellite_Id, $  ; Optional input
                                WMO_Sensor_Id   =WMO_Sensor_Id   , $  ; Optional input
                                Title           =Title           , $  ; Optional input
                                History         =History         , $  ; Optional input
                                Comment         =Comment              ; Optional input
;-
  ; Set up
  ; ------
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    NCDF_CONTROL, FileId, /ABORT
    RETURN, FAILURE
  ENDIF
  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters
  ; Write only attribute names
  WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
  CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 

  ; Mandatory global attributes
  ; ---------------------------
  ; Software ID
  NCDF_ATTPUT, FileId, WRITE_MODULE_HISTORY_GATTNAME, 'IDL netCDF SRF Writer', /GLOBAL
  ; Creation date
  NCDF_ATTPUT, FileId, CREATION_DATE_AND_TIME_GATTNAME, SYSTIME(/UTC)+' (UTC)', /GLOBAL
  ; The Release
  NCDF_ATTPUT, FileId, RELEASE_GATTNAME, SRF_RELEASE, /GLOBAL
  
  ; Optional global attributes
  ; --------------------------
  ; The Version
  IF ( N_ELEMENTS(Version) EQ 0 ) THEN Version = SRF_VERSION
  NCDF_ATTPUT, FileId, VERSION_GATTNAME, LONG(Version), /GLOBAL
  ; The Sensor_Id
  IF ( N_ELEMENTS(Sensor_Id) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, SENSOR_ID_GATTNAME, STRING(Sensor_Id), /GLOBAL
  ; The WMO_Satellite_Id
  IF ( N_ELEMENTS(WMO_Satellite_Id) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, WMO_SATELLITE_ID_GATTNAME, LONG(WMO_Satellite_ID), /GLOBAL
  ; The WMO_Sensor_Id
  IF ( N_ELEMENTS(WMO_Sensor_Id) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, WMO_SENSOR_ID_GATTNAME, LONG(WMO_Sensor_ID), /GLOBAL
  ; The Title
  IF ( N_ELEMENTS(Title) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, TITLE_GATTNAME, STRING(Title), /GLOBAL
  ; The History
  IF ( N_ELEMENTS(History) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, HISTORY_GATTNAME, STRING(History), /GLOBAL
  ; The Comment
  IF ( N_ELEMENTS(Comment) GT 0 ) THEN $
    NCDF_ATTPUT, FileId, COMMENT_GATTNAME, STRING(Comment), /GLOBAL

  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION WriteGAtts_SRF_netCDF
