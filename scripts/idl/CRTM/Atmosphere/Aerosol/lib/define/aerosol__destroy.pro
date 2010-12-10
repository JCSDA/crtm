
; NAME:
;       Aerosol::Destroy
;
; PURPOSE:
;       The Aerosol::Destroy procedure method reinitialises a Aerosol object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Obj->[Aerosol::]Destroy, $
;         Debug=Debug           ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
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

PRO Aerosol::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @aerosol_parameters
  @aerosol_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated     = FALSE
  self.n_Layers         = 0L
  self.Type             = INVALID_AEROSOL
  self.Effective_Radius = LIST()
  self.Concentration    = LIST()

 
END ; PRO Aerosol::Destroy
