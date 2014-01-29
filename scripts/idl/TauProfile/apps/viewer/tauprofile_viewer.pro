PRO TauProfile_Viewer, $
  file, $
  Debug = debug


  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_pro_err_handler
  ; ...Check arguments
  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, 'Entered...', /INFORMATIONAL


  ; Create the widget display
  TauProfile_GUI, file, Debug = debug


  IF ( KEYWORD_SET(debug) ) THEN MESSAGE, '...Exiting', /INFORMATIONAL

END
