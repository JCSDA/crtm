;+
;
; NAME:
;       error_codes
;
; PURPOSE:
;       Include file containing error handling codes.
;
; CALLING SEQUENCE:
;       @error_codes
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 21-Dec-2000
;                       paul.vandelst@noaa.gov
;
;-

  ; Define true/false explicitly
  FALSE = 0
  TRUE  = 1

  ; Define aliases for true/false
  ; ...Define function result codes
  FAILURE = FALSE
  SUCCESS = TRUE
  ; ...Define test result code
  FAIL = FALSE
  PASS = TRUE
