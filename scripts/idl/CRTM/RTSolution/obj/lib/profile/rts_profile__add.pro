FUNCTION RTS_Profile::Add, $
  Objects, $
  Position = Position

  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler

  
  ; Add the required RTS object
  self->IDL_Container::Add( $
    Objects, $
    POSITION = Position )
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END ; FUNCTION RTS_Profile::Add
