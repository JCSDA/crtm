PRO RTS_Channel::Cleanup, Debug=Debug  ; Input keyword
 
  ; Set up
  @rts_pro_err_handler


  ; Deallocate pointers, and clear scalars
  Result = self->Destroy(Debug=Debug)
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error destroying RTS structure', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch

  ; Done
  Done:
  CATCH, /CANCEL

END ; PRO RTS_Channel::Cleanup
