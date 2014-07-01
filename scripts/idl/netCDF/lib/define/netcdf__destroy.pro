;+
; NAME:
;       netCDF::Destroy
;
; PURPOSE:
;       The netCDF::Destroy procedure method reinitialises a netCDF object.
;
; CALLING SEQUENCE:
;       Obj->[netCDF::]Destroy, $
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

PRO netCDF::Destroy, $
  Debug = Debug  ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @netcdf_pro_err_handler

  ; Reinitialise
  self.file = ''
  self.id   = -1
  IF ( OBJ_VALID(self.info) ) THEN self.info -> REMOVE, /ALL
  IF ( OBJ_VALID(self.dim ) ) THEN self.dim  -> REMOVE, /ALL
  IF ( OBJ_VALID(self.gatt) ) THEN self.gatt -> REMOVE, /ALL
  IF ( OBJ_VALID(self.data) ) THEN self.data -> REMOVE, /ALL

END
