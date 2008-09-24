;+
; Function to read the global attributes from a netCDF SRF data file.
;
FUNCTION ReadGAtts_SRF_netCDF, Filename                         , $  ; Input
                               FileId                           , $  ; Input
                               Release         =Release         , $  ; Optional output
                               Version         =Version         , $  ; Optional output
                               Sensor_Id       =Sensor_Id       , $  ; Optional output
                               WMO_Satellite_Id=WMO_Satellite_Id, $  ; Optional output
                               WMO_Sensor_Id   =WMO_Sensor_Id   , $  ; Optional output
                               Title           =Title           , $  ; Optional output
                               History         =History         , $  ; Optional output
                               Comment         =Comment              ; Optional output
;-
  ; Set up
  ; ------
  ; Error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF
  ; Include the SRF netCDF parameters
  @srf_netcdf_parameters
  

  ; Get the global attributes
  ; -------------------------
  ; The Release
  AttInfo = NCDF_ATTINQ( FileID,/GLOBAL,RELEASE_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+RELEASE_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2), $
             /NONAME, /NOPRINT
  NCDF_ATTGET, FileID, /GLOBAL, RELEASE_GATTNAME, Release
  Release = LONG(Release)
  IF ( Release NE SRF_RELEASE ) THEN $
    MESSAGE, 'Invalid '+RELEASE_GATTNAME+' attribute value in '+STRTRIM(Filename,2), $
             /NONAME, /NOPRINT
  
  ; The Version
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, VERSION_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+VERSION_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    Version = SRF_VERSION
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, VERSION_GATTNAME, Version
    Version = LONG(Version)
  ENDELSE

  ; The Sensor_Id
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, SENSOR_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+SENSOR_ID_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    Sensor_Id = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, SENSOR_ID_GATTNAME, Sensor_Id
    Sensor_Id = STRING(Sensor_Id)
  ENDELSE

  ; The WMO_Satellite_Id
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, WMO_SATELLITE_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+WMO_SATELLITE_ID_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, WMO_SATELLITE_ID_GATTNAME, WMO_Satellite_Id
    WMO_Satellite_Id = LONG(WMO_Satellite_Id)
  ENDELSE

  ; The WMO_Sensor_Id
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, WMO_SENSOR_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+WMO_SENSOR_ID_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    WMO_Sensor_Id = INVALID_WMO_SENSOR_ID
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, WMO_SENSOR_ID_GATTNAME, WMO_Sensor_Id
    WMO_Sensor_Id = LONG(WMO_Sensor_Id)
  ENDELSE

  ; The Title
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, TITLE_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+TITLE_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    Title = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, TITLE_GATTNAME, Title
    Title = STRING(Title)
  ENDELSE

  ; The History
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, HISTORY_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+HISTORY_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    History = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, HISTORY_GATTNAME, History
    History = STRING(History)
  ENDELSE

  ; The Comment
  AttInfo = NCDF_ATTINQ( FileID, /GLOBAL, COMMENT_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN BEGIN
    MESSAGE, 'Global attribute '+COMMENT_GATTNAME+$
             ' not found in '+STRTRIM(Filename,2)+'. Skipping...', $
             /INFORMATIONAL
    Comment = ''
  ENDIF ELSE BEGIN
    NCDF_ATTGET, FileID, /GLOBAL, COMMENT_GATTNAME, Comment
    Comment = STRING(Comment)
  ENDELSE


  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION ReadGAtts_SRF_netCDF
