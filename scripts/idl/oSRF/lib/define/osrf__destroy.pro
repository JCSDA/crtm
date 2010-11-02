;+
; NAME:
;       OSRF::Destroy
;
; PURPOSE:
;       The OSRF::Destroy procedure method deallocates and frees the
;       pointer components of an OSRF object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Destroy, $
;         No_Clear=No_Clear, $  ; Input keyword
;         Debug=Debug           ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       No_Clear:    Set this keyword to NOT reinitialise the scalar
;                    components to their default values. The default
;                    is to also clear the scalar values.
;                    If NOT SET => scalar components are reinitialised (DEFAULT)
;                       SET,    => scalar components are NOT reinitialised
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
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
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       After creating a OSRF object,
;
;         IDL> x = OBJ_NEW('OSRF')
;
;       and allocating it,
;
;         IDL> x->Allocate, 10
;
;       the object internals can be reinitialised like so,
;
;         IDL> x->Destroy
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Destroy, $
  No_Clear=No_Clear, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
 
  ; Initialise the non-pointer members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.Release              = OSRF_RELEASE
    self.Version              = OSRF_VERSION
    self.Sensor_Id            = ' '
    self.WMO_Satellite_Id     = INVALID_WMO_SATELLITE_ID
    self.WMO_Sensor_Id        = INVALID_WMO_SENSOR_ID
    self.Sensor_Type          = INVALID_SENSOR
    self.Channel              = INVALID
    self.Integral             = ZERO
    self.Flags                = 0L
    self.f0                   = ZERO
    self.Planck_Coeffs        = ZERO
    self.Polychromatic_Coeffs = ZERO
    self.Convolved_R          = ZERO
    self.Convolved_T          = ZERO
  ENDIF


  ; If ALL pointer members are NOT associated, do nothing
  IF ( NOT self->Associated(Debug=Debug) ) THEN RETURN


  ; Deallocate the pointer members and nullify
  FOR i = 0, self.n_Bands-1 DO BEGIN
    PTR_FREE, $
      (*self.Frequency)[i], $  
      (*self.Response)[i] , $
      (*self.Radiance)[i]
  ENDFOR
  PTR_FREE, $
    self.f1       , $
    self.f2       , $
    self.n_Points , $
    self.Frequency, $
    self.Response , $
    self.Radiance , $
    self.psysvar  , $
    self.xsysvar  , $
    self.ysysvar     
  self.f1        = PTR_NEW()
  self.f2        = PTR_NEW()
  self.n_Points  = PTR_NEW()
  self.Frequency = PTR_NEW()
  self.Response  = PTR_NEW()
  self.Radiance  = PTR_NEW()
  self.psysvar   = PTR_NEW()
  self.xsysvar   = PTR_NEW()
  self.ysysvar   = PTR_NEW()


  ; Reinitialise the dimensions
  self.n_Bands = 0


  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

END ; PRO OSRF::Destroy
