;+
; NAME:
;       OSRF::Destroy
;
; PURPOSE:
;       The OSRF::Destroy function method deallocates and frees the
;       pointer components of an OSRF object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Destroy( $
;                  No_Clear=No_Clear, $  ; Input keyword
;                  Debug=Debug        )  ; Input keyword
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
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the deallocations were sucessful
;                       == FAILURE an unrecoverable error occurred
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;
; INCLUDE FILES:
;       srf_parameters: Include file containing SRF specific
;                       parameter value definitions.
;
;       osrf_func_err_handler: Error handler code for OSRF functions.
;
; EXAMPLE:
;       After creating a OSRF object,
;
;         IDL> x = OBJ_NEW('OSRF')
;
;       and allocating it,
;
;         IDL> Result = x->Allocate(10)
;
;       the object internals can be reinitialised like so,
;
;         IDL> Result = x->Destroy()
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Destroy, $
  No_Clear=No_Clear, $  ; Input keyword
  Debug=Debug           ; Input keyword
 
  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_func_err_handler
 
 
  ; Initialise the scalar members
  IF ( NOT KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.Release          = SRF_RELEASE
    self.Version          = SRF_VERSION
    self.Sensor_Id        = ' '
    self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    self.Sensor_Type      = INVALID_SENSOR
    self.Channel          = INVALID
    self.Integral         = ZERO
  ENDIF


  ; If ALL pointer members are NOT associated, do nothing
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN GOTO, Done


  ; Deallocate the pointer members and nullify
  FOR i = 0, self.n_Bands-1 DO BEGIN
    PTR_FREE, $
      (*self.Frequency)[i], $  
      (*self.Response)[i]
  ENDFOR
  PTR_FREE, $
    self.f1       , $
    self.f2       , $
    self.n_Points , $
    self.Frequency, $
    self.Response 
  self.f1        = PTR_NEW()
  self.f2        = PTR_NEW()
  self.n_Points  = PTR_NEW()
  self.Frequency = PTR_NEW()
  self.Response  = PTR_NEW()


  ; Reinitialise the dimensions
  self.n_Bands = 0


  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             /NONAME, /NOPRINT

  ; Done
  Done:
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION OSRF::Destroy
