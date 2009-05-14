;+
; Function to convert frequency in GigaHertz to MegaHertz.
;
FUNCTION GHz_to_MHz, f  ; Input frequency in GHz
;-
  RETURN, DOUBLE(f)*1000.0d0
END

