;+
; NAME:
;       TauProfile::Set_Property
;
; PURPOSE:
;       The TauProfile::Set_Property procedure method sets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]Set_Property, $
;         Debug           = Debug          , $
;         Sensor_ID       = Sensor_ID      , $
;         Level_Pressure  = Level_Pressure , $
;         Channel         = Channel        , $
;         Angle           = Angle          , $
;         Profile         = Profile        , $
;         Molecule_Set    = Molecule_Set   , $
;         Tau             = Tau
;
; INPUT KEYWORDS:
;       Debug:            Set this keyword for debugging.
;                         If NOT SET => Error handler is enabled. (DEFAULT)
;                            SET     => Error handler is disabled; Routine
;                                       traceback output is enabled.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Sensor_Id:        The sensor id of the instrument for the
;                         TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Level_Pressure:   Array defining the level pressures for which
;                         there is TauProfile data.
;                         UNITS:      hPa
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-1 (n_Layers+1)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Channel:          Array defining the sensor channels for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Channels)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Angle:            Array defining the secant of the zenith angles for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-1 (n_Agnles)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Profile:          Array defining the profile data set index for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Profiles)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Molecule_Set:     Array containing the identifiers of the molecular sets for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Molecule_Sets)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Tau:              Array of the instrument resolution transmittances.
;                         *** UNDEFINED upon exit ***
;                         UNITS:      N/A
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-5 (n_Layers x n_Channels x n_Angles x n_Profiles x n_Molecule_Sets)
;                         ATTRIBUTES: INTENT(IN), OPTIONAL
;
; SIDE EFFECTS:
;
;       If supplied, the Tau input keyword is copied into the object and the
;       keyword argument beconmes undefined upon exit.
;
;-

PRO TauProfile::Set_Property, $
   Debug          = debug         , $  ; Input keyword
   Sensor_ID      = sensor_id     , $  ; Input keyword
   Level_Pressure = level_pressure, $  ; Input keyword
   Channel        = channel       , $  ; Input keyword
   Angle          = angle         , $  ; Input keyword
   Profile        = profile       , $  ; Input keyword
   Molecule_Set   = molecule_set  , $  ; Input keyword
   Tau            = tau                ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  @tauprofile_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get the object dimensions for checking
  self->Get_Property, $
    n_Layers        = n_layers       , $
    n_Channels      = n_channels     , $
    n_Angles        = n_angles       , $
    n_Profiles      = n_profiles     , $
    n_Molecule_Sets = n_molecule_sets, $
    Debug = Debug


  ; Set scalar data
  IF ( N_ELEMENTS(sensor_id) GT 0 ) THEN self.Sensor_ID = STRING(sensor_id)


  ; Set the dimensional array data
  ; ...The level pressure data
  n = N_ELEMENTS(level_pressure)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE (n_layers+1) ) THEN $
      MESSAGE, 'Size of input Level_Pressure different from TauProfile allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Level_Pressure).Add, level_pressure
  ENDIF
  ; ...The sensor channel list
  n = N_ELEMENTS(channel)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE (n_channels) ) THEN $
      MESSAGE, 'Size of input sensor Channel list different from TauProfile allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Channel).Add, channel
  ENDIF
  ; ...The secant zenith angle list
  n = N_ELEMENTS(angle)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE (n_angles) ) THEN $
      MESSAGE, 'Size of input zenith Angle list different from TauProfile allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Angle).Add, angle
  ENDIF
  ; ...The profile data set list
  n = N_ELEMENTS(profile)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE (n_profiles) ) THEN $
      MESSAGE, 'Size of input Profile dataset list different from TauProfile allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Profile).Add, profile
  ENDIF
  ; ...The molecular data set list
  n = N_ELEMENTS(molecule_set)
  IF ( n GT 0 ) THEN BEGIN
    IF ( n NE (n_molecule_sets) ) THEN $
      MESSAGE, 'Size of input Molecule_Set list different from TauProfile allocation.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    (self.Molecule_Set).Add, molecule_set
  ENDIF


  ; Set the transmittance array data
  tau_info = SIZE(tau,/STRUCTURE)
  IF ( tau_info.N_ELEMENTS GT 0 ) THEN BEGIN
    IF ( tau_info.N_DIMENSIONS NE 5 ) THEN $
      MESSAGE, 'Transmittance data array must have 5-dimensions.', $
               NONAME=MsgSwitch, NOPRINT=MsgSwitch
    dimcheck = [n_layers, n_channels, n_angles, n_profiles, n_molecule_sets]
    dimname  = ['layer' , 'channel' , 'angle' , 'profile' , 'molecule set']
    FOR i = 0, tau_info.N_DIMENSIONS - 1 DO BEGIN
      IF ( tau_info.DIMENSIONS[i] NE dimcheck[i] ) THEN $
        MESSAGE, dimname[i] + $
                 ' dimension of transmittance data array different from TauProfile allocation.', $
                 NONAME=MsgSwitch, NOPRINT=MsgSwitch
    ENDFOR
    (self.Tau).Add, TEMPORARY(tau)
  ENDIF
  

END
