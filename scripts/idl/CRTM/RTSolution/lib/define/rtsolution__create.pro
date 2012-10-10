;+
; NAME:
;       RTSolution::Create
;
; PURPOSE:
;       The RTSolution::Create procedure method creates an allocated
;       instance of an RTSolution object. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[RTSolution::]Create, $
;         n_Layers     , $ ; Input
;         n_Absorbers  , $ ; Input
;         Debug = Debug    ; Input keyword
;
; INPUTS:
;       n_Layers:     The number of layers dimension of the
;                     RTSolution data. Must be > 0.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;                     ATTRIBUTES: INTENT(IN)
;
;       n_Absorbers:  The number of absorbers dimension of the
;                     RTSolution data. Must be > 0.
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

PRO RTSolution::Create, $
  n_layers     , $  ; Input
  Debug = debug     ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_pro_err_handler


  ; Check inputs
  IF ( n_layers LT 1 ) THEN $
    MESSAGE, 'Input n_Layers must be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Destroy the input
  self->Destroy, Debug = debug


  ; Assign the dimensions
  self.n_Layers = n_layers   


  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
