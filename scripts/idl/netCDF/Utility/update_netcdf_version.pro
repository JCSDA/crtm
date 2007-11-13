;+
;
; Procedure to update a netCDF file "Version" attribute.
;
PRO Update_netCDF_Version, File, Version
;-
  ncid = NCDF_OPEN(file, /WRITE)
  varid = NCDF_VARID(ncid, 'Version')
  NCDF_VARPUT, ncid, varid, version
  NCDF_CLOSE, ncid
END
