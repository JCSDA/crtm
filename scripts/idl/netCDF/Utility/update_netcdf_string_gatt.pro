;+
;
; Procedure to update a netCDF file string global attribute.
;
PRO Update_netCDF_String_GAtt, NCfile, $  ; Input. File to modify.
                               Name  , $  ; Input. Name of GAtt to update
                               Update     ; Input. String to prepend to GAtt.
;-

  NCid = NCDF_OPEN(NCfile,/WRITE)
  ; Get the current attribute value
  NCDF_ATTGET, NCid, /GLOBAL, Name, GAtt
  ; Prepend the update
  GAtt = STRTRIM(Update,2) + '; ' + STRING(GAtt)
  ; Write the updated global attribute back to file
  NCDF_CONTROL, NCid, /REDEF
  NCDF_ATTPUT, NCid, /GLOBAL, NAME, GAtt
  NCDF_CLOSE, NCid
END
