;+
;
; Procedure to replace a netCDF global attribute.
;
PRO Replace_netCDF_GAtt, NCfile  , $  ; Input. File to modify.
                         GAttName, $  ; Input. Name of GAtt to replace
                         GAttData     ; Input. GAtt data to put in file
;-

  IF ( NOT Valid_String(GAttName) ) THEN RETURN

  NCid = NCDF_OPEN(NCfile,/WRITE)
  NCDF_CONTROL, NCid, /REDEF
  NCDF_ATTPUT, NCid, /GLOBAL, GAttName, GAttData
  NCDF_CLOSE, NCid
END
