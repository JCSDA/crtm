;+
; NAME:
;       TauProfile::ReadFile
;
; PURPOSE:
;       The TauProfile::ReadFile procedure method reads a netCDF TauProfile
;       data file and populates the TauProfile object.
;
; CALLING SEQUENCE:
;       Obj->[TauProfile::]ReadFile, $
;         Filename, $
;         Debug             = Debug            , $
;         Channel_List      = Channel_List     , $
;         Angle_List        = Angle_List       , $
;         Profile_List      = Profile_List     , $
;         Molecule_Set_List = Molecule_Set_List   
;
; INPUTS:
;       Filename:          Character string specifying the name of the
;                          netCDF format TauProfile data file to read.
;                          UNITS:      N/A
;                          TYPE:       CHARACTER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT( IN )
;
; INPUT KEYWORDS:
;       Debug:             Set this keyword for debugging.
;                          If NOT SET => Error handler is enabled. (DEFAULT)
;                             SET     => Error handler is disabled; Routine
;                                        traceback output is enabled.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Channel_List:      List of channel numbers for which transmittance
;                          profile data is required. If not specified, all
;                          channels are read.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Angle_List:        List of zenith angle secant values for which
;                          transmittance profile data is required. If not
;                          specified, all angles are read.
;                          UNITS:      N/A
;                          TYPE:       DOUBLE
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Profile_List:      List of profile numbers for which transmittance
;                          profile data is required. If not specified, all
;                          profiles are read.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
;       Molecule_Set_List: List of molecule set numbers for which
;                          transmittance profile data is required. If not
;                          specified, all molecule sets are read.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT(IN), OPTIONAL
;
; FUNCTION RESULT:
;       Error_Status:      The return value is an integer defining the error status.
;                          The error codes are defined in the error_codes.pro file.
;                          If == SUCCESS the netCDF data read was successful.
;                             == FAILURE an unrecoverable error occurred.
;                          UNITS:      N/A
;                          TYPE:       INTEGER
;                          DIMENSION:  Scalar
;
;-

PRO Match_List, $
  list_name  , $  ; Input
  master_list, $  ; Input
  user_list  , $  ; Input
  match_index, $  ; Output
  fp = fp         ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  ; ...Keywords
  ip = ~(NOT KEYWORD_SET(fp))
  ; ...Define a tolerance value for floating point comparisons
  TOLERANCE = (MACHAR(/DOUBLE)).EPS

  ; Match up the elements
  n_user_elements = N_ELEMENTS(user_list)
  match_index = []
  FOR i = 0, n_user_elements-1 DO BEGIN
    IF ( ip ) THEN BEGIN
      loc = WHERE(master_list EQ user_list[i], count)
    ENDIF ELSE BEGIN
      loc = WHERE(ABS(master_list - user_list[i]) LE TOLERANCE, count)
    ENDELSE
    IF ( count GT 0 ) THEN match_index = [match_index, loc[0]]
  ENDFOR
  n_matches = N_ELEMENTS(match_index)
  IF ( n_matches NE n_user_elements ) THEN $
    MESSAGE, 'Only ' + $
             STRTRIM(n_matches,2) + $
             ' elements selected from the ' + list_name + ' input of ' + $
             STRTRIM(n_user_elements,2) + ' elements.', /INFORMATIONAL

END



PRO TauProfile::ReadFile, $
   filename                             , $  ; Input
   Quiet             = quiet            , $  ; Input keyword
   Debug             = debug            , $  ; Input keyword
   Channel_List      = channel_list     , $  ; Input keyword
   Angle_List        = angle_list       , $  ; Input keyword
   Profile_List      = profile_list     , $  ; Input keyword
   Molecule_Set_List = molecule_set_list     ; Input keyword

  ; Set up
  COMPILE_OPT HIDDEN
  @tauprofile_netcdf_parameters
  @tauprofile_pro_err_handler
  ; ...Check input
  IF ( ~ Valid_String(filename) ) THEN $
    MESSAGE, 'Must specify a filename', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  ; ...Process keywords
  noisy = ~(KEYWORD_SET(quiet)) || KEYWORD_SET(debug)
  ; ...Ensure the object is empty
  self.Destroy, Debug=debug


  ; Inquire the file
  self.InquireFile, $
    filename, $
    Quiet           = quiet         , $
    Debug           = debug         , $
    n_Layers        = n_layers      , $
    n_Channels      = l_max         , $
    n_Angles        = i_max         , $
    n_Profiles      = m_max         , $
    n_Molecule_Sets = j_max         , $
    Sensor_ID       = sensor_id     , $
    Level_Pressure  = level_pressure, $
    Channel         = l_list        , $
    Angle           = i_list        , $
    Profile         = m_list        , $
    Molecule_Set    = j_list      


  ; Match up the dimension lists
  ; ...The channel list
  IF ( N_ELEMENTS(channel_list) GT 0 ) THEN BEGIN
    Match_List, 'channel_list', l_list, channel_list, channel_index
  ENDIF ELSE BEGIN
    channel_list  = l_list
    channel_index = LINDGEN(l_max)
  ENDELSE
  n_channels = N_ELEMENTS(channel_index)
  
  ; ...The angle list
  IF ( N_ELEMENTS(angle_list) GT 0 ) THEN BEGIN
    Match_List, 'angle_list', i_list, angle_list, angle_index, /fp
  ENDIF ELSE BEGIN
    angle_list  = i_list
    angle_index = LINDGEN(i_max)
  ENDELSE
  n_angles = N_ELEMENTS(angle_index)

  ; ...The profile list
  IF ( N_ELEMENTS(profile_list) GT 0 ) THEN BEGIN
    Match_List, 'profile_list', m_list, profile_list, profile_index
  ENDIF ELSE BEGIN
    profile_list  = m_list
    profile_index = LINDGEN(m_max)
  ENDELSE
  n_profiles = N_ELEMENTS(profile_index)
  
  ; ...The molecule set list
  IF ( N_ELEMENTS(molecule_set_list) GT 0 ) THEN BEGIN
    Match_List, 'molecule_set_list', j_list, molecule_set_list, molecule_set_index
  ENDIF ELSE BEGIN
    molecule_set_list  = j_list
    molecule_set_index = LINDGEN(j_max)
  ENDELSE
  n_molecule_sets = N_ELEMENTS(molecule_set_index)


  ; Info output
  IF ( noisy ) THEN BEGIN
    MESSAGE, 'Reading ' + STRTRIM(n_channels,2)      + ' channels...'     , /INFORMATIONAL
    MESSAGE, 'Reading ' + STRTRIM(n_angles,2)        + ' angles...'       , /INFORMATIONAL
    MESSAGE, 'Reading ' + STRTRIM(n_profiles,2)      + ' profiles...'     , /INFORMATIONAL
    MESSAGE, 'Reading ' + STRTRIM(n_molecule_sets,2) + ' molecule sets...', /INFORMATIONAL
    IF ( KEYWORD_SET(debug) ) THEN BEGIN
      PRINT, FORMAT='(/2x,"DEBUG: channel_index = ")'
      PRINT, channel_index, FORMAT='(15i5)'
      PRINT, FORMAT='(/2x,"DEBUG: angle_index = ")'
      PRINT, angle_index, FORMAT='(15i5)'
      PRINT, FORMAT='(/2x,"DEBUG: profile_index = ")'
      PRINT, profile_index, FORMAT='(15i5)'
      PRINT, FORMAT='(/2x,"DEBUG: molecule_set_index = ")'
      PRINT, molecule_set_index, FORMAT='(15i5)'
    ENDIF
  ENDIF
  
   
  ; Determine the read type mask
  all_channels      = n_channels      EQ l_max ? 1B : 0B
  all_angles        = n_angles        EQ i_max ? 2B : 0B
  all_profiles      = n_profiles      EQ m_max ? 4B : 0B
  all_molecule_sets = n_molecule_sets EQ j_max ? 8B : 0B
  read_type =  all_channels + all_angles + all_profiles + all_molecule_sets
  IF ( KEYWORD_SET(debug) ) THEN PRINT, read_type, FORMAT='(/2x,"DEBUG: read_type = ",b4.4,/)'


  ; Create the output data structure
  self.Create, $
    n_layers       , $
    n_channels     , $
    n_angles       , $
    n_profiles     , $
    n_molecule_sets, $
    Debug = debug
  IF ( ~self->Associated(Debug = debug) ) THEN $
    MESSAGE, 'TauProfile object allocation failed.', $
             NONAME=MsgSwitch, NOPRINT=MsgSwitch
  

  ; Fill the structure ancillary data
  self.Set_Property, $
    Debug = debug, $
    Sensor_ID      = sensor_id        , $
    Level_Pressure = level_pressure   , $
    Channel        = channel_list     , $
    Angle          = angle_list       , $
    Profile        = profile_list     , $
    Molecule_Set   = molecule_set_list


  ; Create a local array to hold the transmittance data
  transmittance = DBLARR(n_layers, n_channels, n_angles, n_profiles, n_molecule_sets)

  
  ; Open the file
  fid = NCDF_OPEN( filename, /NOWRITE )
  ; ...and get the transmittance data variable ID
  varid = NCDF_VARID( fid, TRANSMITTANCE_VARNAME )



  ; Define the different types of block reads. These are
  ; based on how the data is stored to minimise file
  ; access. Data is stored like so:
  ;   Layer x Channel x Angle x Profile x Molecule Set
  ;    (K)      (L)      (I)      (M)         (J)
  ; All layers are always read.

  ; Read everything
  ;    LIMJ
  ;    1111
  ;    1248
  All_LIMJ = 15B

  ; Read data for selected molecule sets
  ;    LIMJ
  ;    1110
  ;    124
  All_LIM  =  7B

  ; Read data for selected molecule sets and profiles
  ;    LIMJ
  ;    1100
  ;    12
  All_LI   =  3B

  ; Read data for selected molecule sets, profiles, and angles.
  ;    LIMJ
  ;    1000
  ;    1
  All_L    =  1B


  ; Read data based on read_type mask
  CASE 1 OF

    ; Read everything
    ((Read_Type AND All_LIMJ) EQ All_LIMJ ): $
      BEGIN
        NCDF_VARGET, fid, varid, transmittance
        self.Set_Property, $
          Debug = debug, $
          tau = transmittance
      END

    ; Read all data for selected molecule sets
    ((Read_Type AND All_LIM) EQ All_LIM ): $
      BEGIN
        FOR j = 0L, n_molecule_sets - 1L DO BEGIN
          NCDF_VARGET, fid, varid, tau, $
                       COUNT  = [ n_layers   , $
                                  n_channels , $
                                  n_angles   , $
                                  n_profiles , $
                                  1         ], $
                       OFFSET = [ 0, 0, 0, 0, $
                                  molecule_set_index[j] ]
          transmittance[*,*,*,*,j] = TEMPORARY(Tau)
        ENDFOR
      END

    ; Read data for selected molecule sets and profiles
    ((Read_Type AND All_LI) EQ All_LI ): $
      BEGIN
        FOR j = 0L, n_molecule_sets - 1L DO BEGIN
          FOR m = 0L, n_profiles - 1L DO BEGIN
            NCDF_VARGET, fid, varid, tau, $
                         COUNT  = [ n_layers   , $
                                    n_channels , $
                                    n_angles   , $
                                    1, 1      ], $
                         OFFSET = [ 0, 0, 0, $
                                    profile_index[m]     , $
                                    molecule_set_index[j]  ]
            transmittance[*,*,*,m,j] = TEMPORARY(tau)
          ENDFOR
        ENDFOR
      END

    ; Read data for selected molecule sets, profiles, and angles.
    ((Read_Type AND All_L) EQ All_L ):$
      BEGIN
        FOR j = 0L, n_molecule_sets - 1L DO BEGIN
          FOR m = 0L, n_profiles - 1L DO BEGIN
            FOR i = 0L, n_angles - 1L DO BEGIN
              NCDF_VARGET, fid, varid, tau, $
                           COUNT  = [ n_Layers   , $
                                      n_Channels , $
                                      1, 1, 1   ], $
                           OFFSET = [ 0, 0, $
                                      angle_index[i]       , $
                                      profile_index[m]     , $
                                      molecule_set_index[j]  ]
              transmittance[*,*,i,m,j] = TEMPORARY(tau)
            ENDFOR
          ENDFOR
        ENDFOR
      END

    ; Read non-contiguous profiles
    ELSE: BEGIN
      FOR j = 0L, n_molecule_sets - 1L DO BEGIN
        FOR m = 0L, n_profiles - 1L DO BEGIN
          FOR i = 0L, n_angles - 1L DO BEGIN
            FOR l = 0L, n_channels - 1L DO BEGIN
              NCDF_VARGET, fid, varid, tau, $
                           COUNT  = [ n_layers    , $
                                      1, 1, 1, 1 ], $
                           OFFSET = [ 0, $
                                      channel_index[l]     , $
                                      angle_index[i]       , $
                                      profile_index[m]     , $
                                      molecule_set_index[j]  ]
              transmittance[*,l,i,m,j] = TEMPORARY(tau)
            ENDFOR
          ENDFOR
        ENDFOR
      ENDFOR
    END

  ENDCASE
  
  
  ; Put the data array into the object
  self.Set_Property, $
    Debug = debug, $
    Tau = transmittance
  

  ; Close the input data file
  NCDF_CLOSE, fid

END
