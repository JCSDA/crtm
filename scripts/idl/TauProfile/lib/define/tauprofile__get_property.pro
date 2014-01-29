;+
; NAME:
;       TauProfile::Get_Property
;
; PURPOSE:
;       The TauProfile::Get_Property procedure method gets the value of a property
;       or group of properties for this object.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]Get_Property, $
;         Debug           = Debug          , $
;         n_Layers        = n_Layers       , $
;         n_Channels      = n_Channels     , $
;         n_Angles        = n_Angles       , $
;         n_Profiles      = n_Profiles     , $
;         n_Molecule_Sets = n_Molecule_Sets, $
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
; OUTPUT KEYWORDS:
;       n_Layers:         The number of layers dimension of the
;                         TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Channels:       The number of sensor channels dimension
;                         of the TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Angles:         The number of view angles dimension of the
;                         TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Profiles:       The number of atmospheric profiles dimension
;                         of the TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       n_Molecule_Sets:  The number of molecular data sets dimension
;                         of the TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Sensor_Id:        The sensor id of the instrument for the
;                         TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       CHARACTER
;                         DIMENSION:  Scalar
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Level_Pressure:   Array defining the level pressures for which
;                         there is TauProfile data.
;                         UNITS:      hPa
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-1 (n_Layers+1)
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Channel:          Array defining the sensor channels for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Channels)
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Angle:            Array defining the secant of the zenith angles for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-1 (n_Agnles)
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Profile:          Array defining the profile data set index for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Profiles)
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Molecule_Set:     Array containing the identifiers of the molecular sets for which
;                         there is TauProfile data.
;                         UNITS:      N/A
;                         TYPE:       INTEGER
;                         DIMENSION:  Rank-1 (n_Molecule_Sets)
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;       Tau:              Array of the instrument resolution transmittances.
;                         UNITS:      N/A
;                         TYPE:       DOUBLE
;                         DIMENSION:  Rank-????
;                         ATTRIBUTES: INTENT(OUT), OPTIONAL
;
;-

PRO TauProfile::Get_Property, $
   Debug           = debug          , $  ; Input keyword
   n_Layers        = n_layers       , $  ; Output keyword
   n_Channels      = n_channels     , $  ; Output keyword
   n_Angles        = n_angles       , $  ; Output keyword
   n_Profiles      = n_profiles     , $  ; Output keyword
   n_Molecule_Sets = n_molecule_sets, $  ; Output keyword
   Sensor_ID       = sensor_id      , $  ; Output keyword
   Level_Pressure  = level_pressure , $  ; Output keyword
   Channel         = channel        , $  ; Output keyword
   Angle           = angle          , $  ; Output keyword
   Profile         = profile        , $  ; Output keyword
   Molecule_Set    = molecule_set   , $  ; Output keyword
   Tau             = tau                 ; Output keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_parameters
  @tauprofile_pro_err_handler
  ; ...Check the structure has been allocated
  IF ( ~ self->Associated(Debug=Debug) ) THEN $
    MESSAGE, 'Object has not been allocated.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch


  ; Get data
  ; ...Scalars
  IF ( ARG_PRESENT(n_layers       ) ) THEN n_layers        = self.n_Layers
  IF ( ARG_PRESENT(n_channels     ) ) THEN n_channels      = self.n_Channels
  IF ( ARG_PRESENT(n_angles       ) ) THEN n_angles        = self.n_Angles
  IF ( ARG_PRESENT(n_profiles     ) ) THEN n_profiles      = self.n_Profiles
  IF ( ARG_PRESENT(n_molecule_sets) ) THEN n_molecule_sets = self.n_Molecule_Sets
  IF ( ARG_PRESENT(sensor_id      ) ) THEN sensor_id       = self.Sensor_ID
  ; ...Arrays
  IF ( ARG_PRESENT(level_pressure) ) THEN level_pressure = (self.Level_Pressure)[0]
  IF ( ARG_PRESENT(channel       ) ) THEN channel        = (self.Channel       )[0]
  IF ( ARG_PRESENT(angle         ) ) THEN angle          = (self.Angle         )[0]
  IF ( ARG_PRESENT(profile       ) ) THEN profile        = (self.Profile       )[0]
  IF ( ARG_PRESENT(molecule_set  ) ) THEN molecule_set   = (self.Molecule_Set  )[0]
  IF ( ARG_PRESENT(tau           ) ) THEN tau            = (self.Tau           )[0]

END
