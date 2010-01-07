FUNCTION RTSfile::Init, Debug=Debug  ; Input keyword

  ; Set up
  ; ...Local parameters
  ZERO = 0.0d0
  ; ...Set up error handler
  @rts_func_err_handler
 

  ; Set default values
  self.Filename = ''
  self.FileId   = 0L

  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION RTSfile::Init
