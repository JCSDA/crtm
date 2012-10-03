;+
PRO RTS_Channel::Create, $
  n_Layers   , $  ; Input
  Debug=Debug     ; Input keyword
;-
  ; Set up
  @rts_pro_err_handler


  ; Check inputs
  IF ( n_Layers LT 1 ) THEN $
    MESSAGE, 'Input n_Layers must be > 0.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Destroy the input
  self->Destroy, Debug=Debug


  ; Allocate the pointer components
  self.Upwelling_Radiance  = PTR_NEW(DBLARR(n_Layers))
  self.Layer_Optical_Depth = PTR_NEW(DBLARR(n_Layers))


  ; Initialise dimensions
  self.n_Layers = n_Layers


  ; Set allocation indicator
  self.Is_Allocated = TRUE

END
