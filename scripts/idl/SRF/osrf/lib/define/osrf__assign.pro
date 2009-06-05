;+
; NAME:
;       OSRF::Assign
;
; PURPOSE:
;       The OSRF::Assign function method copies a valid OSRF object.
;
; CALLING SEQUENCE:
;       Result = Obj->[OSRF::]Assign( $
;                  new        , $  ; Output
;                  Debug=Debug  )  ; Input keyword
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
; FUNCTION RESULT:
;       Result:      The return value is an integer defining the error
;                    status. The error codes are defined in the error_codes
;                    include file.
;                    If == SUCCESS the computation was sucessful
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
;       Given an instance of a OSRF object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(OSRF)>
;
;       a new instance of the data object is created by:
;
;         IDL> Result = x->Assign(y)
;         IDL> help, y
;         Y               OBJREF    = <ObjHeapVar12(OSRF)>
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 20-Apr-2009
;                       paul.vandelst@noaa.gov
;
;-

FUNCTION OSRF::Assign, $
  new, $       ; Output
  Debug=Debug  ; Input keyword

  ; Set up
  ; ...Generic SRF parameters
  @srf_parameters
  
  ; ...Set up error handler
  @osrf_func_err_handler

  ; ...ALL *input* pointers must be associated
  IF ( self->Associated(Debug=Debug) EQ FALSE ) THEN $
    MESSAGE, 'Some or all input OSRF pointer members are NOT associated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; ...Destroy output object if defined.
  IF ( N_ELEMENTS(new) GT 0 ) THEN $
    IF ( SIZE(new, /TNAME) EQ 'OBJREF' ) THEN OBJ_DESTROY, new, Debug=Debug
  
  ; ...Create a new object reference
  new = OBJ_NEW('OSRF',Debug=Debug)


  ; Allocate the output object
  Result = new->Allocate( *self.n_Points,Debug=Debug )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating output OSRF structure', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Assign data components
  new.Release = self.Release
  new.Version = self.Version
  
  new.Sensor_ID        = self.Sensor_ID  
  new.WMO_Satellite_Id = self.WMO_Satellite_Id
  new.WMO_Sensor_Id    = self.WMO_Sensor_Id
  new.Sensor_Type      = self.Sensor_Type   
  new.Channel          = self.Channel
  new.Integral         = self.Integral
  *new.f1              = *self.f1       
  *new.f2              = *self.f2       
  *new.n_Points        = *self.n_Points  
  FOR i = 0, self.n_Bands-1 DO BEGIN
    *(*new.Frequency)[i] = *(*self.Frequency)[i]
    *(*new.Response)[i]  = *(*self.Response)[i] 
  ENDFOR


  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION OSRF::Assign
