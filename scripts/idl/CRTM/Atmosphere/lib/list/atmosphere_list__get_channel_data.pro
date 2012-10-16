FUNCTION Atmosphere_List::Get_Channel_Data, $
  data_name          , $ ; Input
  Layer    = layer   , $ ; Input keyword
  Absorber = absorber, $ ; Input keyword
  Debug    = debug       ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @atmosphere_func_err_handler
  @atmosphere_parameters


  ; Construct the data retrieval and assignment commands
  getdata_cmd = "self[i]->Atmosphere::Get_Property, " + data_name + "=x, Debug=debug"
  IF ( N_ELEMENTS(layer) GT 0 ) THEN BEGIN
    k = layer-1
    ; ...Array data
    CASE STRLOWCASE(data_name) OF
      'cloud': BEGIN
        print, '**** not implemented yet ****'
      END
      'aerosol': BEGIN
        print, '**** not implemented yet ****'
      END
      'absorber_amount': BEGIN
        j = N_ELEMENTS(absorber) GT 0 ? LONG(absorber[0])-1 : 0
        assigndata_cmd = "data[i] = x[k,j]"
      END
      ELSE: assigndata_cmd = "data[i] = x[k]"
    ENDCASE
  ENDIF ELSE BEGIN
    ; ...Scalar data
    assigndata_cmd = "data[i] = x"
  ENDELSE
    


  ; Define the return array
  n_channels = self.Count()
  data = MAKE_ARRAY(n_channels, VALUE = ZERO)
  
  
  ; Loop over list elements
  FOR i = 0L, n_channels-1 DO BEGIN
    ; ...Get the data
    result = EXECUTE(getdata_cmd)
    IF ( result NE TRUE ) THEN $
      MESSAGE, "Error retrieving " + data_name + $
               " data for channel index " + STRTRIM(i+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Save the channel data
    result = EXECUTE(assigndata_cmd)
    IF ( result NE TRUE ) THEN $
      MESSAGE, "Error assigning " + data_name + $
               " data for channel index " + STRTRIM(i+1,2), $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ENDFOR


  ; Done
  RETURN, data

END
