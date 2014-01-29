;+
; NAME:
;       TauProfile::Destroy
;
; PURPOSE:
;       The TauProfile::Destroy procedure method reinitialises a TauProfile object.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]Destroy, $
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

PRO TauProfile::Destroy, $
  Debug = Debug  ; Input keyword
 
  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  @tauprofile_pro_err_handler
 
 
  ; Reinitialise
  self.Is_Allocated     = FALSE
  self.n_Layers         = 0L
  self.n_Channels       = 0L
  self.n_Angles         = 0L
  self.n_Profiles       = 0L
  self.n_Molecule_Sets  = 0L
  self.Sensor_ID        = ''
  self.WMO_Satellite_ID = 0L
  self.WMO_Sensor_ID    = 0L

  IF ( OBJ_VALID(self.Level_Pressure) ) THEN self.Level_Pressure -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Channel       ) ) THEN self.Channel        -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Angle         ) ) THEN self.Angle          -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Profile       ) ) THEN self.Profile        -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Molecule_Set  ) ) THEN self.Molecule_Set   -> REMOVE, /ALL
  IF ( OBJ_VALID(self.Tau           ) ) THEN self.Tau            -> REMOVE, /ALL

END
