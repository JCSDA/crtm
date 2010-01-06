;+
; NAME:
;       OSRF_File::Write_GAtts
;
; PURPOSE:
;       The OSRF_File::Write_GAtts procedure method writes global attributes
;       to an OSRF_File
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside OSRF_File methods.
;
;
; CALLING SEQUENCE:
;       Obj->[OSRF_File::]Write_GAtts, $
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

PRO OSRF_File::Write_GAtts, $
  Debug = Debug  ; Input keyword
  
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @osrf_file_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
  ; ...Write only attribute names
  WRITE_MODULE_HISTORY_GATTNAME   = 'write_module_history' 
  CREATION_DATE_AND_TIME_GATTNAME = 'creation_date_and_time' 

  
  ; Open the netCDF SRF file
  fid = NCDF_OPEN( self.filename, /WRITE )
  NCDF_CONTROL, fid, /VERBOSE


  ; Put netCDF file into define mode
  NCDF_CONTROL, fid, /REDEF


  ; Write the "mandatory" global attributes
  NCDF_ATTPUT, fid, WRITE_MODULE_HISTORY_GATTNAME  , 'IDL OSRF_File Writer', /GLOBAL
  NCDF_ATTPUT, fid, CREATION_DATE_AND_TIME_GATTNAME, SYSTIME(/UTC)+' (UTC)', /GLOBAL
  NCDF_ATTPUT, fid, RELEASE_GATTNAME               , self.Release          , /GLOBAL
  NCDF_ATTPUT, fid, VERSION_GATTNAME               , self.Version          , /GLOBAL
  NCDF_ATTPUT, fid, SENSOR_ID_GATTNAME             , self.Sensor_Id        , /GLOBAL
  NCDF_ATTPUT, fid, WMO_SATELLITE_ID_GATTNAME      , self.WMO_Satellite_ID , /GLOBAL
  NCDF_ATTPUT, fid, WMO_SENSOR_ID_GATTNAME         , self.WMO_Sensor_ID    , /GLOBAL
  NCDF_ATTPUT, fid, SENSOR_TYPE_GATTNAME           , self.Sensor_Type      , /GLOBAL
  
  ; Write the "optional" global attributes
  IF ( STRLEN(self.Title) GT 0 ) THEN $
    NCDF_ATTPUT, fid, TITLE_GATTNAME  , self.Title  , /GLOBAL
  IF ( STRLEN(self.History) GT 0 ) THEN $
    NCDF_ATTPUT, fid, HISTORY_GATTNAME, self.History, /GLOBAL
  IF ( STRLEN(self.Comment) GT 0 ) THEN $
    NCDF_ATTPUT, fid, COMMENT_GATTNAME, self.Comment, /GLOBAL


  ; Done
  NCDF_CLOSE, fid
  CATCH, /CANCEL
 
END ; PRO OSRF_File::Write_GAtts
