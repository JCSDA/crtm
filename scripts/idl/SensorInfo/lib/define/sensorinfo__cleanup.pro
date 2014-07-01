;+
; NAME:
;       SensorInfo::Cleanup
;
; PURPOSE:
;       The SensorInfo::Cleanup procedure method performs cleanup
;       on a SensorInfo object when it is destroyed.
;
;       NOTE: Cleanup methods are special *lifecycle methods* and, as
;             such, cannot be called outside the context of object
;             creation and destruction. This means that in most cases
;             you cannot call the Cleanup method directly. There is one
;             exception to this rule: if you write your own subclass of
;             this class, you can call the Cleanup method from within the
;             Init or Cleanup method of the subclass.
;
; CALLING SEQUENCE:
;       OBJ_DESTROY, Obj
;
;         or
;
;       Obj->[SensorInfo::]Cleanup, Debug=Debug  (In a lifecycle method only)
;
; KEYWORDS:
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
;       After creating and allocating a SensorInfo object, e.g.
;
;         IDL> x = OBJ_NEW('SensorInfo')
;         IDL> x->Allocate,10
;
;       the Cleanup method is invoked when the object is destroyed,
;
;         IDL> OBJ_DESTROY, x
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 01-Oct-2008
;                       paul.vandelst@noaa.gov
;
;-

PRO SensorInfo::Cleanup, Debug=Debug  ; Input keyword
 
  ; Set up
  ; ...Parameters
  @sensorinfo_parameters
  ; ...Set up error handler
  @sensorinfo_pro_err_handler


  ; Deallocate pointers, and clear scalars
  self->Destroy, Debug=Debug

END
