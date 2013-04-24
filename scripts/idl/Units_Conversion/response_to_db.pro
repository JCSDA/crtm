;+
; Function to convert relative response to deciBels
;
FUNCTION Response_to_dB, Response  ; Input response
;-
  RETURN, 10.0d0*ALOG10(DOUBLE(Response))  ; Return deciBels
END
