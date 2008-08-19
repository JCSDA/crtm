;+
;
; Procedure to update a netCDF file "history" global attribute.
;
PRO Update_netCDF_History, NCfile, $  ; Input. File to modify.
                           Update     ; Input. String to prepend to history GAtt.
;-
  Update_netCDF_String_GAtt, NCfile   , $
                             'history', $
                             Update
END
