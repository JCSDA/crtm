FUNCTION List::Get_Channel_Data, $
  data_name    , $ ; Input
  Debug = debug    ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @rtsolution_func_err_handler
  @rtsolution_parameters
  ; ...Check that all list members are RTSolution objects
  IF ( ~ self->IsA_RTSolution_List(Debug = debug) ) THEN $
    MESSAGE, 'Non-RTSolution object found in channel list.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Construct the data retrieval command
  cmd_string = "self[i]->RTSolution::Get_Property, " + data_name + "=x, Debug=debug"


  ; Define the return array
  n_channels = self.Count()
  data = MAKE_ARRAY(n_channels, VALUE = ZERO)
  
  
  ; Loop over list elements
  FOR i = 0L, n_channels-1 DO BEGIN
    ; ...Execute the command
    result = EXECUTE(cmd_string)
    IF ( result NE TRUE ) THEN $
      MESSAGE, "Error retrieving " + data_name + $
               " data for channel index " + STRTRIM(i+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Save the channel data
    data[i] = x
  ENDFOR


  ; Done
  RETURN, data

END
