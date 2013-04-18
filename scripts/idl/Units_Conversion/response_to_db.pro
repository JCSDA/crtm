;+
; Procedure to convert relative response to deciBels
;
PRO Response_to_dB, Response, Response_dB  ; Input response
;-
  Response_dB = 10.0d0*ALOG10(DOUBLE(Response))  ; Return deciBels
END
