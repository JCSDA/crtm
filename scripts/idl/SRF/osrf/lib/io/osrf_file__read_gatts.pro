;+
; NAME:
;       OSRF_File::Read_GAtts
;
; PURPOSE:
;       The OSRF_File::Read_GAtts procedure method reads global attributes
;       from an OSRF_File
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF_File methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Read_GAtts, $
;         Debug = Debug  ;  Input keyword
;
; INPUT KEYWORDS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       osrf_file parameters: Include file containing OSRF_File specific
;                             parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF_File::Read_GAtts, $
  Debug = Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  
  
  ; Open the netCDF SRF file
  fid = NCDF_OPEN( self.filename, /NOWRITE )
  NCDF_CONTROL, fid, /VERBOSE

  
  ; Get the "mandatory" global attributes
  ; ...The Release
  AttInfo = NCDF_ATTINQ( fid,/GLOBAL,RELEASE_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+RELEASE_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, RELEASE_GATTNAME, Release
  self.Release = LONG(Release)
  IF ( self.Release NE OSRF_RELEASE ) THEN $
    MESSAGE, 'Invalid '+RELEASE_GATTNAME+' attribute value in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  ; ...The Version
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, VERSION_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+VERSION_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, VERSION_GATTNAME, Version
  self.Version = LONG(Version)

  ; ...The Sensor_Type
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, SENSOR_TYPE_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+SENSOR_TYPE_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, SENSOR_TYPE_GATTNAME, Sensor_Type
  self.Sensor_Type = LONG(Sensor_Type)

  ; ...The Sensor_Id
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, SENSOR_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+SENSOR_ID_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, SENSOR_ID_GATTNAME, Sensor_Id
  self.Sensor_Id = STRING(Sensor_Id)

  ; ...The WMO_Satellite_Id
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, WMO_SATELLITE_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+WMO_SATELLITE_ID_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, WMO_SATELLITE_ID_GATTNAME, WMO_Satellite_Id
  self.WMO_Satellite_Id = LONG(WMO_Satellite_Id)

  ; ...The WMO_Sensor_Id
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, WMO_SENSOR_ID_GATTNAME )
  IF ( AttInfo.DataType EQ 'UNKNOWN' ) THEN $
    MESSAGE, 'Global attribute '+WMO_SENSOR_ID_GATTNAME+$
             ' not found in '+STRTRIM(self.filename,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  NCDF_ATTGET, fid, /GLOBAL, WMO_SENSOR_ID_GATTNAME, WMO_Sensor_Id
  self.WMO_Sensor_Id = LONG(WMO_Sensor_Id)


  ; Get the "optional" global attributes
  ; ...The Title
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, TITLE_GATTNAME )
  IF ( AttInfo.DataType NE 'UNKNOWN' ) THEN BEGIN
    NCDF_ATTGET, fid, /GLOBAL, TITLE_GATTNAME, Title
    self.Title = STRING(Title)
  ENDIF

  ; ...The History
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, HISTORY_GATTNAME )
  IF ( AttInfo.DataType NE 'UNKNOWN' ) THEN BEGIN
    NCDF_ATTGET, fid, /GLOBAL, HISTORY_GATTNAME, History
    self.History = STRING(History)
  ENDIF

  ; ...The Comment
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, COMMENT_GATTNAME )
  IF ( AttInfo.DataType NE 'UNKNOWN' ) THEN BEGIN
    NCDF_ATTGET, fid, /GLOBAL, COMMENT_GATTNAME, Comment
    self.Comment = STRING(Comment)
  ENDIF
  

  ; Done
  NCDF_CLOSE, fid
  CATCH, /CANCEL
 
END ; PRO OSRF_File::Read_GAtts
