;+
; Function to convert frequency in MegaHertz to GigaHertz.
;
FUNCTION MHz_to_GHz, f  ; Input frequency in MHz
;-
  RETURN, DOUBLE(f)/1000.0d0
END

