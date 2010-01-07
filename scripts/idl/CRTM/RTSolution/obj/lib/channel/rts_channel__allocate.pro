FUNCTION RTS_Channel::Allocate, $
  n_Layers       , $  ; Input
  Debug=Debug         ; Input keyword

  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler
 
  ; ...Check dimension input
  IF ( n_Layers LT 1 ) THEN $
    MESSAGE, 'Input n_Layers must be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  IF ( N_ELEMENTS(n_Bands) EQ 0 ) THEN n_Bands=1
 
  ; Check if ANY pointers are already associated
  ; If they are, deallocate them but leave scalars.
  IF ( self->Associated(/ANY_Test,Debug=Debug) EQ TRUE ) THEN BEGIN
    Result = self->Destroy(/No_Clear,Debug=Debug)
    IF ( Result NE SUCCESS ) THEN $
      MESSAGE, 'Error destroying RTS object', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDIF
 
 
  ; Perform the allocations 
  self.Upwelling_Radiance  = PTR_NEW(DBLARR( n_Layers ))
  self.Layer_Optical_Depth = PTR_NEW(DBLARR( n_Layers ))
 
  ; Assign the dimensions
  self.n_Layers = n_Layers
 
  ; Increment and test allocation counter
  self.n_Allocates = self.n_Allocates + 1
  IF ( self.n_Allocates NE 1 ) THEN $
    MESSAGE, 'Allocation counter /= 1, Value = ' + STRTRIM(self.n_Allocates,2), $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
 
  ; Done
  CATCH, /CANCEL
  RETURN, SUCCESS
 
END ; FUNCTION RTS_Channel::Allocate
