;+
;
; Procedure to update a netCDF file "comment" attribute.
;
PRO Update_netCDF_Comment, File, Update
;-
  ncid = NCDF_OPEN(file,/WRITE)
  NCDF_ATTGET, ncid, /GLOBAL, 'comment', comment
  comment = update+'; ' + STRING(comment)
  NCDF_CONTROL, ncid, /REDEF
  NCDF_ATTPUT, ncid, /GLOBAL, 'comment', comment
  NCDF_CLOSE, ncid
END
