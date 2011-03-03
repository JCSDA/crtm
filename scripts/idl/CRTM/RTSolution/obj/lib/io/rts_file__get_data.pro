FUNCTION RTS_File::Get_Data, $
  Profile_Number, $ ; Input
  Channel_Tag   , $ ; Input
  Debug = Debug     ; Input keyword


  ; Set up
  ; ...Set up error handler
  @rts_func_err_handler
 
  ; Construct the channel Get_Property command
  cmd_string = "rts_channel[n]->Get_Property, "+Channel_Tag+"=x"
  
  ; Get the required profile
  rts_profile = self->Get(POSITION = Profile_Number-1)

  ; Now get the channel data
  rts_channel = rts_profile->Get(/ALL, ISA="RTS_Channel", COUNT=n_channels)
  data = DBLARR(n_channels)
  FOR n = 0L, n_channels-1L DO BEGIN
    result = EXECUTE(cmd_string)
    IF ( result NE TRUE ) THEN $
      MESSAGE, 'Error retrieving '+Channel_Tag+' data for channel position '+STRTRIM(n+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    data[n] = x
  ENDFOR

  ; Done
  CATCH, /CANCEL
  RETURN, data
 
END ; FUNCTION RTS_File::Get_Data
