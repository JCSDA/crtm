;+
;
; Procedure to update a netCDF file "Version" variable (older files) or attribute.
;
PRO Update_netCDF_Version, NCfile , $  ; Input. File to modify.
                           Version  =Version  , $  ; Input. Integer value for updated version. Ignored if INCREMENT is set.
                           Increment=Increment, $  ; Input. If set, file version is just incremented.
                           Attribute=Attribute     ; Input. If set, version is a GAtt.
;-

  NAME = 'Version'
  
  ; Do nothing if no keywords set
  IF ( (NOT KEYWORD_SET(Version)) AND (NOT KEYWORD_SET(Increment)) ) THEN RETURN
  ; Open file for writing
  NCid = NCDF_OPEN(NCfile, /WRITE)
  
  ; Branch for attribute or variable version
  IF ( KEYWORD_SET(Attribute) ) THEN BEGIN
    ; Get the current version attribute
    NCDF_ATTGET, NCid, /GLOBAL, NAME, ver
    ; Replace the value
    IF ( KEYWORD_SET(Increment) ) THEN ver++ ELSE ver=LONG(Version)
    NCDF_ATTPUT, NCid, NAME, ver, /GLOBAL

  ENDIF ELSE BEGIN
    ; Get the current version variable
    varid = NCDF_VARID(NCid, NAME)
    NCDF_VARGET, NCid, varid, ver
    ; Replace the value
    IF ( KEYWORD_SET(Increment) ) THEN ver++ ELSE ver=LONG(Version)
    NCDF_VARPUT, NCid, varid, ver
  ENDELSE
  
  NCDF_CLOSE, ncid
END
