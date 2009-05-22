;+
; NAME:
;       OSRF::Set_Flags
;
; PURPOSE:
;       The OSRF::Set_Flags procedure method sets the bit flag component.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Set_Flags, $
;         Debug          = Debug         , $  ; Input keyword
;         f_grid_regular = f_grid_regular     ; Input keyword
;         
; INPUT KEYWORD PARAMETERS:
;       Debug:              Set this keyword for debugging.
;                           If NOT SET => Error handler is enabled. (DEFAULT)
;                              SET     => Error handler is disabled; Routine
;                                         traceback output is enabled.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       f_grid_regular:     Set this keyword to set the corresponding flag.
;                           Setting this flag indicates the OSRF frequency
;                           data is on a regularly-spaced frequency grid.
;                           The default is an irregularly-spaced frequency
;                           grid.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       srf_parameters: Include file containing SRF specific
;                       parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       Given a valid, allocated, OSRF object, x, the various bit flags can
;       be set like so,
;
;         IDL> x->Set_Flags, /f_grid_regular
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Set_Flags, $
  Debug          = Debug         , $  ; Input keyword
  f_grid_regular = f_grid_regular     ; Input keyword


  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'OSRF structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Set flags
  IF ( KEYWORD_SET(f_grid_regular) ) THEN self.Flags = self.Flags OR F_GRID_REGULAR_FLAG

  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Set_Flags
