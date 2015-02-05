;+
; NAME:
;       RTSolution::Destroy
;
; PURPOSE:
;       The RTSolution::Destroy procedure method reinitialises a RTSolution object.
;
; CALLING SEQUENCE:
;       Obj->[RTSolution::]Destroy, $
;         Debug=Debug
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
;-

PRO RTSolution::Destroy, $
  Debug = debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_parameters
  @rtsolution_pro_err_handler


  ; Reinitialise
  self.Is_Allocated            = FALSE
  self.n_Layers                = 0L
  self.Algorithm               = ''
  self.Sensor_Id               = ''
  self.WMO_Satellite_Id        = 0L
  self.WMO_Sensor_Id           = 0L
  self.Sensor_Channel          = 0L
  self.SOD                     = ZERO
  self.Surface_Emissivity      = ZERO
  self.Surface_Reflectivity    = ZERO
  self.Up_Radiance             = ZERO
  self.Down_Radiance           = ZERO
  self.Down_Solar_Radiance     = ZERO
  self.Surface_Planck_Radiance = ZERO
  self.Radiance                = ZERO
  self.Brightness_Temperature  = ZERO
  
  IF ( OBJ_VALID(self.Upwelling_Radiance ) ) THEN self.Upwelling_Radiance  -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Layer_Optical_Depth) ) THEN self.Layer_Optical_Depth -> REMOVE, /ALL

END
