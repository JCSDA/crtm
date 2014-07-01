;+
; NAME:
;       SensorInfo_List::Add
;
; PURPOSE:
;       The SensorInfo_List::Add procedure method adds one or more
;       child objects to the container.
;
;       This method overrides the IDL_Container::Add procedure method.
;
; CALLING SEQUENCE:
;       Obj->[SensorInfo)List::]Add, $
;         Objects      , $  ; Input
;         Debug = Debug     ; Input keyword
;
; ARGUMENTS:
;       Objects:     An object instance or array of object instances to
;                    be added to the SensorInfo container object.
;                    UNITS:      N/A
;                    TYPE:       Object
;                    DIMENSION:  Scalar or any rank array
;                    ATTRIBUTES: INTENT(IN)
;
; KEYWORDS:
;       Any keywords to the IDL_Container::Add method can be used.
;       In addition, the following keywords are available:
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
;       sensorinfo_pro_err_handler: Error handler code for SensorInfo prcedures.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 26-Jun-2009
;                       paul.vandelst@noaa.gov
;
;-
PRO SensorInfo_List::Add, $
  objects, $
  Debug  = Debug, $  ; Input keyword
  _EXTRA = Extra     ; Keywords passed onto IDL_Container::Add
 
  ; Set up
  ; ...SensorInfo parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler


  ; Add object to the container
  self->IDL_Container::Add, objects, _EXTRA = Extra

  
  ; If the object added is a SensorInfo object, increment the sensor counter
  loc = WHERE( OBJ_ISA(objects, 'SensorInfo'), n_Sensors )
  IF ( n_Sensors GT 0 ) THEN self.n_Sensors = self.n_Sensors + n_Sensors

END
