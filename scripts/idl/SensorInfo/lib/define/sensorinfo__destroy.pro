;+
; NAME:
;       SensorInfo::Destroy
;
; PURPOSE:
;       The SensorInfo::Destroy procedure method deallocates and frees the
;       pointer components of a SensorInfo object.
;
;       NOTE: This method is called by the Cleanup procedure method, but
;             this Destroy method *can* be called outside the context of
;             object creation and destruction. That is, this is *not*
;             a lifecycle method.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Destroy, $
;         No_Clear = No_Clear, $  ; Input keyword
;         Debug    = Debug        ; Input keyword
;
; KEYWORDS:
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
;       sensorinfo_parameters: Include file containing SensorInfo specific
;                              parameter value definitions.
;
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo procedures.
;
; EXAMPLE:
;       After creating a SensorInfo object,
;
;         IDL> x = OBJ_NEW('SensorInfo')
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
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Destroy, $
  Debug    = Debug   , $  ; Input keyword
  No_Clear = No_Clear     ; Input keyword
 
  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler

 
  ; Initialise the non-pointer members
  IF ( ~ KEYWORD_SET(No_Clear) ) THEN BEGIN
    self.Sensor_Name      = ' '
    self.Satellite_Name   = ' '
    self.Sensor_Id        = ' '
    self.WMO_Satellite_Id = INVALID_WMO_SATELLITE_ID
    self.WMO_Sensor_Id    = INVALID_WMO_SENSOR_ID
    self.Sensor_Type      = INVALID_SENSOR
  ENDIF


  ; If ALL pointer members are NOT associated, do nothing
  IF ( ~ self->Associated(Debug=Debug) ) THEN RETURN


  ; Deallocate the pointer members and nullify
  PTR_FREE, $
    self.Sensor_Channel, $
    self.Use_Flag      , $
    self.Noise         
  self.Sensor_Channel = PTR_NEW()
  self.Use_Flag       = PTR_NEW()
  self.Noise          = PTR_NEW()


  ; Reinitialise the dimensions
  self.n_Channels = 0
  

  ; Decrement and test allocation counter
  self.n_Allocates = self.n_Allocates - 1
  IF ( self.n_Allocates NE 0 ) THEN $
    MESSAGE, 'Allocation counter /= 0, Value = ' + STRTRIM(self.n_Allocates, 2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

END
