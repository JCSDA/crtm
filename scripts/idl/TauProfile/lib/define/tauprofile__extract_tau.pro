PRO TauProfile::Extract_Tau, $
   channel      , $  ; Input
   angle        , $  ; Input
   profile      , $  ; Input
   molecule_set , $  ; Input
   tau          , $  ; Output
   Debug = debug     ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  @tauprofile_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the dimension lists
  self.Get_Property, $
    Channel      = channel_list     , $
    Angle        = angle_list       , $
    Profile      = profile_list     , $
    Molecule_Set = molecule_set_list, $
    Debug = debug


  ; Get the indices
  lidx = WHERE(channel_list      EQ channel     , lcount)
  iidx = WHERE(angle_list        EQ angle       , icount)
  midx = WHERE(profile_list      EQ profile     , mcount)
  jidx = WHERE(molecule_set_list EQ molecule_set, jcount)
  IF ( lcount EQ 0 OR $
       icount EQ 0 OR $
       mcount EQ 0 OR $
       jcount EQ 0 ) THEN $
    MESSAGE, 'Invalid data request', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
             
       
  ; Extract the transmittance profile
  tau = ((self.Tau)[0])[*,lidx[0],iidx[0],midx[0],jidx[0]]

END
