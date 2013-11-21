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
;       osrf_parameters: Include file containing OSRF specific
;                        parameter value definitions.
;
;       osrf_pro_err_handler: Error handler code for OSRF procedures.
;
; EXAMPLE:
;       An OSRF object, say, x, cna be reinitialised like so,
;
;         IDL> x.Destroy
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Destroy, $
  Debug=Debug           ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler
 
 
  ; Initialise the scalar and array data
  self.Release              = OSRF_RELEASE
  self.Version              = OSRF_VERSION
  self.Sensor_Id            = ''
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


  ; Empty out data
  ; ...Hashes
  IF ( OBJ_VALID(self.f1       ) ) THEN (self.f1       ).Remove, /ALL
  IF ( OBJ_VALID(self.f2       ) ) THEN (self.f2       ).Remove, /ALL
  IF ( OBJ_VALID(self.n_Points ) ) THEN (self.n_Points ).Remove, /ALL
  IF ( OBJ_VALID(self.Frequency) ) THEN (self.Frequency).Remove, /ALL
  IF ( OBJ_VALID(self.Response ) ) THEN (self.Response ).Remove, /ALL
  IF ( OBJ_VALID(self.Bandwidth) ) THEN (self.Bandwidth).Remove, /ALL
  IF ( OBJ_VALID(self.Radiance ) ) THEN (self.Radiance ).Remove, /ALL
  IF ( OBJ_VALID(self.pRef     ) ) THEN (self.pRef     ).Remove, /ALL
  ; ...Pointers
  IF ( PTR_VALID(self.T   ) ) THEN PTR_FREE, self.T   
  IF ( PTR_VALID(self.Teff) ) THEN PTR_FREE, self.Teff
  IF ( PTR_VALID(self.Tfit) ) THEN PTR_FREE, self.Tfit


  ; Create new items
  ; ...Hashes
  self.f1        = HASH()
  self.f2        = HASH()
  self.n_Points  = HASH()
  self.Frequency = HASH()
  self.Response  = HASH()
  self.Bandwidth = HASH()
  self.Radiance  = HASH()
  self.pRef      = HASH()
  ; ...Pointers
  self.T    = PTR_NEW(/ALLOCATE_HEAP)
  self.Teff = PTR_NEW(/ALLOCATE_HEAP)
  self.Tfit = PTR_NEW(/ALLOCATE_HEAP)
  ; ..."Regular" objects
  self.wRef  = OBJ_NEW()
  self.twRef = OBJ_NEW()
  self.tpRef = OBJ_NEW()


  ; Reinitialise the dimensions
  self.n_Bands = 0


  ; Set the allocation indicator
  self.Is_Allocated = FALSE

END
