;+
FUNCTION RTS_File::Get_Data, $
  Channel_Property_Name, $ ; Input. Name of variable data to be returned.
  Profile = Profile    , $ ; Input keyword. If not specified, channel property for all profiles are returned.
  Debug = Debug            ; Input keyword
;-

  ; Set up
  @rts_func_err_handler


  ; Determine profile list
  ; ...Get the defaults
  self->Get_Property, n_Profiles = n_profiles
  _profile = LINDGEN(n_profiles) + 1L
  ; ...Alter defaults if necessary
  IF ( N_ELEMENTS(Profile) GT 0 ) THEN BEGIN
    _profile = Profile
    ; ...Check profile(s) is(are) present in object
    idx = WHERE( _profile GE 1 OR _profile LE n_profiles, n_profiles )
    IF ( n_profiles EQ 0 ) THEN $
      MESSAGE, "Requested profile(s) not present in object", $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ; ...Set profile list
    _profile = _profile[idx]
  ENDIF


  ; Construct the channel Get_Property command
  cmd_string = "rts_channel[n]->Get_Property, " + Channel_Property_Name + "=x, Debug=debug"


  ; Loop over profile
  FOR m = 0L, n_profiles-1 DO BEGIN

    ; Get the required profile
    rts_profile = self->Get(POSITION = _profile[m]-1)

    ; Now get the channel data
    rts_channel = rts_profile->Get(/ALL, ISA="RTS_Channel", COUNT=n_channels)

    ; Define the return data array
    IF ( m EQ 0 ) THEN data = DBLARR(n_channels,n_profiles)

    ; Loop over channel
    FOR n = 0L, n_channels-1L DO BEGIN
      result = EXECUTE(cmd_string)
      IF ( result NE TRUE ) THEN $
        MESSAGE, "Error retrieving " + Channel_Property_Name + " data for channel position " + $
                 STRTRIM(n+1,2) + " in profile #" + STRTRIM(_profile[m],2), $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
      data[n,m] = x
    ENDFOR
  ENDFOR

  RETURN, REFORM(data)

END
