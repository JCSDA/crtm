;+
; NAME:
;       OSRF::Clear_Flags
;
; PURPOSE:
;       The OSRF::Clear_Flags procedure method clears the bit flag component.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Clear_Flags, $
;         Debug          = Debug         , $  ; Input keyword
;         f_grid_regular = f_grid_regular, $  ; Input keyword
;         all            = all                ; Input keyword
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
;       f_grid_regular:     Set this keyword to clear the corresponding flag.
;                           Clearing this flag indicates the OSRF frequency
;                           data is NOT on a regularly-spaced frequency grid,
;                           which is the default.
;                           grid.
;                           UNITS:      N/A
;                           TYPE:       INTEGER
;                           DIMENSION:  Scalar
;                           ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       all:                Set this keyword to clear ALL the OSRF bit flags.
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
;       be cleared like so,
;
;         IDL> x->Clear_Flags, /f_grid_regular
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 22-May-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Clear_Flags, $
  Debug          = Debug         , $  ; Input keyword
  f_grid_regular = f_grid_regular, $  ; Input keyword
  all            = all                ; Input keyword


  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_pro_err_handler
 
  
  ; Check if structure has been allocated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'OSRF structure has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  
  ; Clear flags
  IF ( KEYWORD_SET(f_grid_regular) ) THEN self.Flags = self.Flags AND ( NOT F_GRID_REGULAR_FLAG )
  IF ( KEYWORD_SET(all           ) ) THEN self.Flags = 0L
  
  ; Done
  CATCH, /CANCEL
 
END ; PRO OSRF::Clear_Flags
