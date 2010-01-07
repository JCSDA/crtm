FUNCTION RTS_Profile::Get, $
  Objects, $
  Position = Position

  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler

  
  ; Extract the required objects
  Objects = self->IDL_Container::Get( $
    ISA = "RTS_Channel", $
    POSITION = Position, $
    COUNT = Count )
  IF ( Count EQ 0 ) THEN $
    MESSAGE, 'Error getting RTS_Channel objects', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END ; FUNCTION RTS_Profile::Get
