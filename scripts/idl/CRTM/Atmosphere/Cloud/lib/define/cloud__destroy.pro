
; NAME:
;       Cloud::Destroy
;
; PURPOSE:
;       The Cloud::Destroy procedure method reinitialises a Cloud object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Obj->[Cloud::]Destroy, $
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
;       cloud_parameters: Include file for cloud specific parameters.
;
;       cloud_pro_err_handler: Include file for error handling.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 10-Dec-2010
;                       paul.vandelst@noaa.gov
;
;-

PRO Cloud::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  @cloud_parameters
  @cloud_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated       = FALSE
  self.n_Layers           = 0L
  self.Type               = INVALID_CLOUD
  self.Effective_Radius   = LIST()
  self.Effective_Variance = LIST()
  self.Water_Content      = LIST()

 
END ; PRO Cloud::Destroy
