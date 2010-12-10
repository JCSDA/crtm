;+
; NAME:
;       Aerosol::Allocate
;
; PURPOSE:
;       The Aerosol::Allocate procedure method prepares the object 
;       for data assignment. If the object is already allocated,
;       it is reallocated and any contained data is lost.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Allocate, $
;         n_Layers     , $ ; Input
;         Debug = Debug    ; Input keyword
;
; INPUTS:
;       n_Layers:  The number of layers dimension of the
;                  Aerosol data. Must be > 0.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN)
;                  
; INPUT KEYWORDS:
;       Debug:     Set this keyword for debugging. If set then:
;                  - the error handler for this function is disabled
;                    so that execution halts where the error occurs,
;                  - more verbose output is produced.
;                  UNITS:      N/A
;                  TYPE:       INTEGER
;                  DIMENSION:  Scalar
;                  ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       aerosol_parameters: Include file for aerosol specific parameters.
;
;       aerosol_pro_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 10-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Aerosol::Allocate, $
  n_Layers     , $ ; Input
  Debug = Debug    ; Input keyword

  ; Set up
  @aerosol_pro_err_handler
 
 
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

END ; PRO Aerosol::Allocate
