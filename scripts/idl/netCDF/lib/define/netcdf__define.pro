;+
; netCDF object definition procedure

PRO netCDF__Define

  COMPILE_OPT HIDDEN

  void = { netCDF, $
           file: '', $
           id  : 0L, $
           info: OBJ_NEW(), $
           dim : OBJ_NEW(), $
           gatt: OBJ_NEW(), $
           data: OBJ_NEW(), $
           INHERITS IDL_Object  }
END

;-
