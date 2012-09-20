;+
; NAME:
;       Cloud::Create
;
; PURPOSE:
;       The Cloud::Create procedure method creates an allocated
;       instance of a Cloud object. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Create, $
;         n_Layers     , $
;         Debug = Debug   
;
; INPUTS:
;       n_Layers:  The number of layers dimension of the
;                  Cloud data. Must be > 0.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN)
;                  
; INPUT KEYWORDS:
;       Debug:     Set this keyword for debugging.
;                  If NOT SET => Error handler is enabled. (DEFAULT)
;                     SET     => Error handler is disabled; Routine
;                                traceback output is enabled.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;-

PRO Cloud::Create, $
  n_Layers     , $ ; Input
  Debug = Debug    ; Input keyword

  ; Set up
  @cloud_pro_err_handler
 
 
  ; Check input
  IF ( n_Layers LT 1 ) THEN $
    MESSAGE, 'n_Layers dimensions must be > 0', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
   

  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Assign the dimension
  self.n_Layers = n_Layers
 

  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
