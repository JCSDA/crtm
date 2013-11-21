;+
; NAME:
;       OSRF::Assign
;
; PURPOSE:
;       The OSRF::Assign procedure method copies a valid OSRF object.
;
; CALLING SEQUENCE:
;       Obj->[OSRF::]Assign, $
;         new        , $  ; Output
;         Debug=Debug     ; Input keyword
;
; OUTPUT:
;       new:         A deep copy of the OSRF object.
;                    UNITS:      N/A
;                    TYPE:       OSRF object
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
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
;       Given an instance of a OSRF object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(OSRF)>
;
;       a new instance of the data object is created by:
;
;         IDL> x.Assign,y
;         IDL> help, y
;         Y               OBJREF    = <ObjHeapVar12(OSRF)>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO OSRF::Assign, $
  new, $       ; Output
  Debug=Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...OSRF parameters
  @osrf_parameters
  ; ...Set up error handler
  @osrf_pro_err_handler

  ; ...ALL input data pointers must be associated
  IF ( ~ self.Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Some or all input OSRF members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; ...Return argument must be present
  IF ( ~ ARG_PRESENT(new) ) THEN $
    MESSAGE, 'No output object argument specified.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
             
  ; ...Check the return object
  type_name = SIZE(new,/TNAME)
  IF ( type_name EQ 'OBJREF' ) THEN BEGIN
    IF ( OBJ_VALID(new) ) THEN BEGIN
      IF ( OBJ_ISA(new,'oSRF') ) THEN BEGIN
        new->Destroy, Debug=Debug
      ENDIF ELSE BEGIN
        OBJ_DESTROY, new, Debug=Debug
        new = OBJ_NEW('oSRF', Debug=Debug)
      ENDELSE
    ENDIF ELSE BEGIN
      new = OBJ_NEW('oSRF', Debug=Debug)
    ENDELSE
  ENDIF ELSE BEGIN
    new = OBJ_NEW('oSRF', Debug=Debug)
  ENDELSE
    

  ; Allocate the output object
  self.Get_Property, n_Points=n_points, Debug=Debug
  new.Allocate, n_points, Debug=Debug


  ; Copy scalar/array components  
  new.Release              = self.Release
  new.Version              = self.Version
  new.Sensor_ID            = self.Sensor_ID  
  new.WMO_Satellite_Id     = self.WMO_Satellite_Id
  new.WMO_Sensor_Id        = self.WMO_Sensor_Id
  new.Sensor_Type          = self.Sensor_Type   
  new.Channel              = self.Channel
  new.Integral             = self.Integral
  new.Flags                = self.Flags
  new.f0                   = self.f0
  new.Planck_Coeffs        = self.Planck_Coeffs
  new.Polychromatic_Coeffs = self.Polychromatic_Coeffs
  new.Convolved_R          = self.Convolved_R
  new.Convolved_T          = self.Convolved_T


  ; Copy hash components
  new.f1        = (self.f1)[*]
  new.f2        = (self.f2)[*]
  new.Frequency = (self.Frequency)[*]
  new.Response  = (self.Response)[*] 
  new.Bandwidth = (self.Bandwidth)[*] 
  new.Radiance  = (self.Radiance)[*] 


  ; Copy pointer components
  *new.T    = *self.T
  *new.Teff = *self.Teff
  *new.Tfit = *self.Tfit
  
  
  ; Output debug info
  IF ( KEYWORD_SET(debug) ) THEN BEGIN
    new.Set_Flag, Debug = debug
  ENDIF
END
