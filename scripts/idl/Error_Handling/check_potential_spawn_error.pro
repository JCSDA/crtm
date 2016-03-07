;+
;
; NAME:
;   Check_Potential_SPAWN_Error
;
; PURPOSE:
;   Function for checking the stderr output of a SPAWN command for potential
;   error keywords,
;
;     IDL> SPAWN, some-*nix-command, stdout, stderr
;
;   Only intended for use on *nix systems.
;
; CALLING SEQUENCE:
;   exit_stat = Check_Potential_SPAWN_Error( stderr )
;
; INPUTS:
;   stderr:     Error output stream from the SPAWN command via the
;               ErrResult argument.
;               UNITS:      N/A
;               TYPE:       CHARACTER
;               DIMENSION:  Scalar
;               ATTRIBUTES: INTENT(IN)
; 
; FUNCTION RESULT:
;   exit_stat:  Equivalent *nix command exit status.
;               If == 0, no error detected
;                  /= 0, potential error detected
;               UNITS:      N/A
;               TYPE:       INTEGER
;               DIMENSION:  Scalar
;
; CREATION HISTORY:
;   Written by:     Paul van Delst, 04-Mar-2016
;                   paul.vandelst@noaa.gov
;
;-

FUNCTION Check_Potential_SPAWN_Error, stderr

  ; Parameters
  ; ...Common indications of failure
  ERROR_STRING = ['sigseg', $
                  'segmentation fault']
  ; ...Unix status codes
  UNIX_SUCCESS =  0
  UNIX_FAILURE = -1  ; Or non-zero

  
  ; Define default success for Unix process
  exit_stat = UNIX_SUCCESS
  
  ; Only process if stderr not empty
  IF ( N_ELEMENTS(stderr) GT 1 ) THEN BEGIN
    lc_stderr = STRLOWCASE(stderr)
    ; Check for each error indicator
    FOR i = 0, N_ELEMENTS(ERROR_STRING)-1 DO BEGIN
      error_idx = WHERE(STRPOS(lc_stderr, ERROR_STRING[i]) NE -1, error_count)
      IF ( error_count GT 0 ) THEN BEGIN
        exit_stat = UNIX_FAILURE
        BREAK
      ENDIF
    ENDFOR
  ENDIF
  
  ; Done
  RETURN, exit_stat

END
