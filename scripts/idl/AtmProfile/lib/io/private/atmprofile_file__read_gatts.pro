;+
; NAME:
;       AtmProfile_File::Read_GAtts
;
; PURPOSE:
;       The AtmProfile_File::Read_GAtts procedure method reads global attributes
;       from an AtmProfile_File
;
;       NOTE: This method should be considered PRIVATE to the class
;             and should not be invoked outside AtmProfile_File methods.
;
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Read_GAtts, $
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
;       atmprofile_file parameters: Include file containing AtmProfile_File specific
;                                   parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 06-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile_File::Read_GAtts, $
  Debug = Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...netCDF parameters
  @atmprofile_file_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler
  
  
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
  IF ( self.Release NE ATMPROFILE_RELEASE ) THEN $
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


  ; Get the "optional" global attributes
  ; ...The Profile_Set_Id
  AttInfo = NCDF_ATTINQ( fid, /GLOBAL, PROFILE_SET_ID_GATTNAME )
  IF ( AttInfo.DataType NE 'UNKNOWN' ) THEN BEGIN
    NCDF_ATTGET, fid, /GLOBAL, PROFILE_SET_ID_GATTNAME, Profile_Set_Id
    self.Profile_Set_Id = STRING(Profile_Set_Id)
  ENDIF

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
 
END ; PRO AtmProfile_File::Read_GAtts
