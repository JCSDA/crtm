;+
; Function to convert deciBels to relative response.
;
FUNCTION dB_to_Response, dB ; Input deciBels
;-
  B = 0.1d0*DOUBLE(dB)      ; Convert to Bels
  RETURN, 1.0d0/(10.0d0^B)  ; Return relative response
END
  
