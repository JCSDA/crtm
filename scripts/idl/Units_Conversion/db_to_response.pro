;+
; Procedure to convert deciBels to relative response.
;
PRO dB_to_Response, dB, B ; Input deciBels
;-
  B = 10.0d0^(0.1d0*DOUBLE(dB))  ; Convert decibels to relative response
END
