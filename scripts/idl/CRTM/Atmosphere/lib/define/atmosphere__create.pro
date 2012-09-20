;+
; NAME:
;       Atmosphere::Create
;
; PURPOSE:
;       The Atmosphere::Create procedure method creates an allocated
;       instance of an atmosphere object. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[Atmosphere::]Create, $
;         n_Layers     , $ ; Input
;         n_Absorbers  , $ ; Input
;         Debug = Debug    ; Input keyword
;
; INPUTS:
;       n_Layers:     The number of layers dimension of the
;                     Atmosphere data. Must be > 0.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN)
;
;       n_Absorbers:  The number of absorbers dimension of the
;                     Atmosphere data. Must be > 0.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN)
;                  
; INPUT KEYWORDS:
;       Debug:        Set this keyword for debugging. If set then:
;                     - the error handler for this function is disabled
;                       so that execution halts where the error occurs,
;                     - more verbose output is produced.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO Atmosphere::Create, $
  n_Layers     , $ ; Input
  n_Absorbers  , $ ; Input
  Debug = Debug    ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_parameters
  @atmosphere_pro_err_handler
 
 
  ; Check inputs
  IF ( N_PARAMS() NE 2 ) THEN $
    MESSAGE, 'Must supply n_Layers and n_Absorbers dimensions', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( n_Layers LT 1 ) THEN $
    MESSAGE, 'n_Layers dimension must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( (n_Absorbers LT N_MIN_ABSORBERS) OR $
       (n_Absorbers GT N_MAX_ABSORBERS) ) THEN $
    MESSAGE, 'n_Absorbers dimension must '+ $
             STRTRIM(N_MIN_ABSORBERS,2)+' <= n <= '+ $
             STRTRIM(N_MAX_ABSORBERS,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Assign the dimensions
  self.n_Layers    = n_Layers   
  self.n_Absorbers = n_Absorbers
 

  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
