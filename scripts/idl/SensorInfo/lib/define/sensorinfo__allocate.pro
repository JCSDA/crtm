;+
; NAME:
;       SensorInfo::Allocate
;
; PURPOSE:
;       The SensorInfo::Allocate procedure method allocates the SensorInfo
;       object data arrays.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo::]Allocate, $
;         n_Channels, $  ; Input
;         Debug=Debug    ; Input keyword
;
; ARGUMENTS:
;       n_Channels:  The number of channels to which the SensorInfo
;                    data arrays are to be allocated.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN)
;                    
; KEYWORDS:
;       Debug:       Set this keyword for debugging. If set then:
;                    - the error handler for this function is disabled
;                      so that execution halts where the error occurs,
;                    - more verbose output is produced.
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
;       it can be allocated to the required number of channels, in this
;       example 25:
;
;         IDL> n_Channels = 25
;         IDL> x->Allocate, n_Channels
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Allocate, $
  n_Channels, $  ; Input
  Debug = Debug  ; Input keyword

  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler
  ; ...Check dimension input
  IF ( n_Channels LT 1 ) THEN $
    MESSAGE, 'Input N_POINTS must be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Check if ANY pointers are already associated
  ; ...If they are, deallocate them but leave scalars.
  IF ( self->Associated(/ANY_Test,Debug=Debug) ) THEN $
    self->Destroy, /No_Clear, Debug=Debug
 
 
  ; Perform the allocations 
  self.Sensor_Channel = PTR_NEW(LONARR(n_Channels))
  self.Use_Flag       = PTR_NEW(LONARR(n_Channels))
  self.Noise          = PTR_NEW(DBLARR(n_Channels))
 
 
  ; Assign the dimensions
  self.n_Channels = n_Channels
 
 
  ; Increment and test allocation counter
  self.n_Allocates = self.n_Allocates + 1
  IF ( self.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(self.n_Allocates,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

 
  ; Done
  CATCH, /CANCEL
 
END ; PRO SensorInfo::Allocate
