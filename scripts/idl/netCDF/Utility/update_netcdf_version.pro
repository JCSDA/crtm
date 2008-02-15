;+
;
; Procedure to update a netCDF file "Version" attribute.
;
PRO Update_netCDF_Version, File, Version, Attribute=Attribute
;-
  ncid = NCDF_OPEN(file, /WRITE)
  
  IF ( KEYWORD_SET(Attribute) ) THEN BEGIN
    NCDF_ATTPUT, ncid, 'Version', LONG(Version), /GLOBAL
  ENDIF ELSE BEGIN
    varid = NCDF_VARID(ncid, 'Version')
    NCDF_VARPUT, ncid, varid, version
  ENDELSE
  
  NCDF_CLOSE, ncid
END
