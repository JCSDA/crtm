;+
;
; Procedure to update a netCDF file "comment" global attribute.
;
PRO Update_netCDF_Comment, NCfile, $  ; Input. File to modify.
                           Update     ; Input. String to prepend to comment GAtt.
;-
  Update_netCDF_String_GAtt, NCfile   , $
                             'comment', $
                             Update
END
