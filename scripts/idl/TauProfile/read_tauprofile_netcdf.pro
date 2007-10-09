;+
; NAME:
;       Read_TauProfile_netCDF
;
; PURPOSE:
;       Function to read data from a netCDF format TauProfile file.
;
; CALLING SEQUENCE:
;       Error_Status = Read_TauProfile_netCDF( Filename,   $  ; Input
;                                              TauProfile, $  ; Output
;                                              Channel_List      = Channel_List,      $  ; Optional input
;                                              Angle_List        = Angle_List,        $  ; Optional input
;                                              Profile_List      = Profile_List,      $  ; Optional input
;                                              Molecule_Set_List = Molecule_Set_List     ; Optional input
;
; INPUT ARGUMENTS:
;       Filename:          Character string specifying the name of the
;                          netCDF format TauProfile data file to read.
;                          UNITS:      None
;                          TYPE:       CHARACTER
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT( IN )
;
; OPTIONAL INPUT ARGUMENTS:
;       Channel_List:      List of channel numbers for which transmittance
;                          profile data is required. If not specified, all
;                          channels are read.
;                          UNITS:      None
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT( IN )
;
;       Angle_List:        List of zenith angle secant values for which
;                          transmittance profile data is required. If not
;                          specified, all angles are read.
;                          UNITS:      None
;                          TYPE:       REAL
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT( IN )
;
;       Profile_List:      List of profile numbers for which transmittance
;                          profile data is required. If not specified, all
;                          profiles are read.
;                          UNITS:      None
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT( IN )
;
;       Molecule_Set_List: List of molecule set numbers for which
;                          transmittance profile data is required. If not
;                          specified, all molecule sets are read.
;                          UNITS:      None
;                          TYPE:       INTEGER
;                          DIMENSION:  Rank-1
;                          ATTRIBUTES: INTENT( IN )
;
; OUTPUT ARGUMENTS:
;       TauProfile:        Structure containing the transmittance data read
;                          from file.
;                          UNITS:      N/A
;                          TYPE:       TauProfile structure
;                          DIMENSION:  Scalar
;                          ATTRIBUTES: INTENT( OUT )
;
; FUNCTION RESULT:
;       Error_Status: The return value is an integer defining the error status.
;                     The error codes are defined in the error_codes.pro file.
;                     If == SUCCESS the netCDF data read was successful.
;                        == FAILURE an unrecoverable error occurred.
;                     UNITS:      N/A
;                     TYPE:       INTEGER
;                     DIMENSION:  Scalar
;
; SIDE EFFECTS:
;       If the TauProfile structure argument contains anything, it is
;       destroyed prior to the data file read or upon exit if an error
;       occurred.
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC 05-Apr-2004
;                       paul.vandelst@ssec.wisc.edu
;-

PRO Integer_List_Match, UserList         , $ ; Input
                        FileList         , $ ; Input
                        FileMax          , $ ; Input
                        MatchList        , $ ; Output
                        MatchInFile_Index    ; Output

  IF ( N_ELEMENTS( UserList ) GT 0 ) THEN BEGIN
    ; Sort and unique-ify
    MatchList = UserList[ UNIQ( UserList, SORT( UserList ) ) ]
    nMatches = N_ELEMENTS( MatchList )
    MatchInFile_Index = LONARR( nMatches )
    ; Check that all specified values are valid
    FOR l = 0L, nMatches - 1L DO BEGIN
      Loc = WHERE( File_List EQ MatchList[l], Count )
      IF ( Count EQ 0 ) THEN $
        MESSAGE, 'CHANNEL_LIST input contains an invalid channel, ' + $
                 STRING( MatchList[l], FORMAT = '(i4)' ), $
                 /NONAME, /NOPRINT $
      ELSE $
        MatchInFile_Index[l] = Loc
    ENDFOR
  ENDIF ELSE BEGIN
    ; Default is to get all values
    MatchList = FileList
    MatchInFile_Index = LINDGEN( FileMax )
  ENDELSE

END 


; =============
; Main function
; =============
FUNCTION Read_TauProfile_netCDF, Filename,   $  ; Input
                                 TauProfile, $  ; Output
                                 Channel_List      = Channel_List,      $  ; Optional input
                                 Angle_List        = Angle_List,        $  ; Optional input
                                 Profile_List      = Profile_List,      $  ; Optional input
                                 Molecule_Set_List = Molecule_Set_List     ; Optional input

  ; Set up
  ; ------
  ; Set compilation options
  COMPILE_OPT STRICTARR

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    Undefine, TauProfile
    RETURN, FAILURE
  ENDIF    

  ; Include the TauProfile netCDF parameters
  @tauprofile_netcdf_parameters

  ; Define a tolerance value for fp comparisons
  TOLERANCE = ( MACHAR() ).EPS

  ; Check input
  n_Arguments = 2
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
  IF ( NOT Valid_String( Filename ) ) THEN $
    MESSAGE, 'Input Filename argument not defined!', $
             /NONAME, /NOPRINT


  ; Inquire the netCDF file
  ; -----------------------
  Result = Inquire_TauProfile_netCDF( Filename, $
                                      N_LAYERS          = n_Layers        , $  ; Optional output
                                      N_CHANNELS        = L_Max           , $  ; Optional output
                                      N_ANGLES          = I_Max           , $  ; Optional output
                                      N_PROFILES        = M_Max           , $  ; Optional output
                                      N_MOLECULE_SETS   = J_Max           , $  ; Optional output
                                      RELEASE           = Release         , $  ; Optional output
                                      VERSION           = Version         , $  ; Optional output
                                      SENSOR_ID         = Sensor_ID       , $  ; Optional output
                                      WMO_SATELLITE_ID  = WMO_Satellite_ID, $  ; Optional output
                                      WMO_SENSOR_ID     = WMO_Sensor_ID   , $  ; Optional output
                                      LEVEL_PRESSURE    = Level_Pressure  , $  ; Optional output
                                      CHANNEL_LIST      = L_List          , $  ; Optional output
                                      ANGLE_LIST        = I_List          , $  ; Optional output
                                      PROFILE_LIST      = M_List          , $  ; Optional output
                                      MOLECULE_SET_LIST = J_List            )  ; Optional output
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'TauProfile file inquiry failed.', $
             /NONAME, /NOPRINT

  ; Match up the channel list
  Integer_List_Match, Channel_List , $ ; Input
                      L_List       , $ ; Input
                      L_Max        , $ ; Input
                      Channel      , $ ; Output
                      Channel_Index    ; Output

  ; The number of channels to read
  n_Channels = N_ELEMENTS( Channel )

  ; Set flag if all channels to be read
  All_Channels = 0B
  IF ( n_Channels EQ L_Max ) THEN All_Channels = 1B


  ; The angle list
  ; --------------
  IF ( N_ELEMENTS( Angle_List ) GT 0 ) THEN BEGIN
    ; Sort and unique-ify
    Angle = Angle_List[ UNIQ( Angle_List, SORT( Angle_List ) ) ]
    n_Angles = N_ELEMENTS( Angle )
    Angle_Index = LONARR( n_Angles )
    ; Check that all specified angles are valid
    FOR i = 0L, n_Angles - 1L DO BEGIN
      Loc = WHERE( ABS( I_List - Angle[i] ) LT TOLERANCE, Count )
      IF ( Count EQ 0 ) THEN $
        MESSAGE, 'ANGLE_LIST input contains an invalid angle, ' + $
                 STRING( Angle[i], FORMAT = '(f4.2)' ), $
                 /NONAME, /NOPRINT $
      ELSE $
        Angle_Index[i] = Loc
    ENDFOR
  ENDIF ELSE BEGIN
    ; Default is to get all angles
    Angle = I_List
    Angle_Index = LINDGEN( I_Max )
  ENDELSE

  ; The number of angles to read
  n_Angles = N_ELEMENTS( Angle )

  ; Set flag if all angles to be read
  All_Angles = 0B
  IF ( n_Angles EQ I_Max ) THEN All_Angles = 2B


  ; The profile list
  ; ----------------
  IF ( N_ELEMENTS( Profile_List ) GT 0 ) THEN BEGIN
    ; Sort and unique-ify
    Profile = Profile_List[ UNIQ( Profile_List, SORT( Profile_List ) ) ]
    n_Profiles = N_ELEMENTS( Profile )
    Profile_Index = LONARR( n_Profiles )
    ; Check that all specified profiles are valid
    FOR m = 0L, n_Profiles - 1L DO BEGIN
      Loc = WHERE( M_List EQ Profile[m], Count )
      IF ( Count EQ 0 ) THEN $
        MESSAGE, 'PROFILE_List input contains an invalid profile, ' + $
                 STRING( Profile[m], FORMAT = '(i2)' ), $
                 /NONAME, /NOPRINT $
      ELSE $
        Profile_Index[m] = Loc
    ENDFOR
  ENDIF ELSE BEGIN
    ; Default is to get all profiles
    Profile = M_List
    Profile_Index = LINDGEN( M_Max )
  ENDELSE

  ; The number of profiles to read
  n_Profiles = N_ELEMENTS( Profile )

  ; Set flag if all profiles to be read
  All_Profiles = 0B
  IF ( n_Profiles EQ M_Max ) THEN All_Profiles = 4B


  ; The molecule set list
  ; ---------------------
  IF ( N_ELEMENTS( Molecule_Set_List ) GT 0 ) THEN BEGIN
    ; Sort and unique-ify
    Molecule_Set = Molecule_Set_List[ UNIQ( Molecule_Set_List, SORT( Molecule_Set_List ) ) ]
    n_Molecule_Sets = N_ELEMENTS( Molecule_Set_List )
    Molecule_Set_Index = LONARR( n_Molecule_Sets )
    ; Check that all specified molecule sets are valid
    FOR j = 0L, n_Molecule_Sets - 1L DO BEGIN
      Loc = WHERE( J_List EQ Molecule_Set[j], Count )
      IF ( Count EQ 0 ) THEN $
        MESSAGE, 'CHANNEL_LIST input contains an invalid channel, ' + $
                 STRING( Molecule_Set[j], FORMAT = '(i3)' ), $
                 /NONAME, /NOPRINT $
      ELSE $
        Molecule_Set_Index[j] = Loc
    ENDFOR
  ENDIF ELSE BEGIN
    ; Default is to get all molecule sets
    Molecule_Set = J_List
    Molecule_Set_Index = LINDGEN( J_Max )
  ENDELSE

  ; The number of molecule sets to read
  n_Molecule_Sets = N_ELEMENTS( Molecule_Set )

  ; Set flag if all molecule sets to be read
  All_Molecule_Sets = 0B
  IF ( n_Molecule_Sets EQ J_Max ) THEN All_Molecule_Sets = 8B


  ; Create the output data structure
  ; --------------------------------
  ; Clear the structure if required
  IF ( N_ELEMENTS( TauProfile ) GT 0 ) THEN $
    Result = Destroy_TauProfile( TauProfile )

  ; Define the structure
  TauProfile = { TauProfile }

  ; Allocate the structure
  Result = Allocate_TauProfile( n_Layers, $
                                n_Channels, $
                                n_Angles, $
                                n_Profiles, $
                                n_Molecule_Sets, $
                                TauProfile )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating TauProfile structure', $
             /NONAME, /NOPRINT


  ; Fill the structure ancillary data
  ; ---------------------------------
  TauProfile.Release          = Release
  TauProfile.Version          = Version
  TauProfile.Sensor_ID        = Sensor_ID
  TauProfile.WMO_Satellite_ID = WMO_Satellite_ID
  TauProfile.WMO_Sensor_ID    = WMO_Sensor_ID   

  *TauProfile.Level_Pressure = Level_Pressure
  *TauProfile.Channel        = Channel    
  *TauProfile.Angle          = Angle       
  *TauProfile.Profile        = Profile     
  *TauProfile.Molecule_Set   = Molecule_Set



  ; Read the required transmittance data
  ; ------------------------------------
  ; Open the file
  FileID = NCDF_OPEN( Filename, /NOWRITE )

  ; Get the transmittance data variable ID
  VarID = NCDF_VARID( FileID, TRANSMITTANCE_VARNAME )

  ; Determine the type of read to perform
  Read_Type =  All_Molecule_Sets + All_Profiles + All_Angles + All_Channels


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


  ; Read data based on read type
  CASE 1 OF

    ; Read everything
    ((Read_Type AND All_LIMJ ) EQ All_LIMJ ): $
      BEGIN
        NCDF_VARGET, FileID, VarID, *TauProfile.Tau
      END

    ; Read all data for selected molecule sets
    ((Read_Type AND All_LIM ) EQ All_LIM ): $
      BEGIN
        FOR j = 0L, TauProfile.n_Molecule_Sets - 1L DO BEGIN
          NCDF_VARGET, FileID, VarID, Tau, $
                       COUNT = [ TauProfile.n_Layers, $
                                 TauProfile.n_Channels, $
                                 TauProfile.n_Angles, $
                                 TauProfile.n_Profiles, $
                                 1 ], $
                       OFFSET = [ 0, 0, 0, 0, Molecule_Set_Index[j] ]
          (*TauProfile.Tau)[ *, *, *, *, j ] = TEMPORARY( Tau )
        ENDFOR
      END

    ; Read data for selected molecule sets and profiles
    ((Read_Type AND All_LI ) EQ All_LI ): $
      BEGIN
        FOR j = 0L, TauProfile.n_Molecule_Sets - 1L DO BEGIN
          FOR m = 0L, TauProfile.n_Profiles - 1L DO BEGIN
            NCDF_VARGET, FileID, VarID, Tau, $
                         COUNT = [ TauProfile.n_Layers, $
                                   TauProfile.n_Channels, $
                                   TauProfile.n_Angles, $
                                   1, $
                                   1 ], $
                         OFFSET = [ 0, $
                                    0, $
                                    0, $
                                    Profile_Index[m], $
                                    Molecule_Set_Index[j] ]
            (*TauProfile.Tau)[ *, *, *, m, j ] = TEMPORARY( Tau )
          ENDFOR
        ENDFOR
      END

    ; Read data for selected molecule sets, profiles, and angles.
    ((Read_Type AND All_L ) EQ All_L ):$
      BEGIN
        FOR j = 0L, TauProfile.n_Molecule_Sets - 1L DO BEGIN
          FOR m = 0L, TauProfile.n_Profiles - 1L DO BEGIN
            FOR i = 0L, TauProfile.n_Angles - 1L DO BEGIN
              NCDF_VARGET, FileID, VarID, Tau, $
                           COUNT = [ TauProfile.n_Layers, $
                                     TauProfile.n_Channels, $
                                     1, $
                                     1, $
                                     1 ], $
                            OFFSET = [ 0, $
                                       0, $
                                       Angle_Index[i], $
                                       Profile_Index[m], $
                                       Molecule_Set_Index[j] ]
              (*TauProfile.Tau)[ *, *, i, m, j ] = TEMPORARY( Tau )
            ENDFOR
          ENDFOR
        ENDFOR
      END

    ; Read non-contiguous profiles
    ELSE: BEGIN
      FOR j = 0L, TauProfile.n_Molecule_Sets - 1L DO BEGIN
        FOR m = 0L, TauProfile.n_Profiles - 1L DO BEGIN
          FOR i = 0L, TauProfile.n_Angles - 1L DO BEGIN
            FOR l = 0L, TauProfile.n_Channels - 1L DO BEGIN
              NCDF_VARGET, FileID, VarID, Tau, $
                           COUNT = [ TauProfile.n_Layers, $
                                     1, $
                                     1, $
                                     1, $
                                     1 ], $
                          OFFSET = [ 0, $
                                     Channel_Index[l], $
                                     Angle_Index[i], $
                                     Profile_Index[m], $
                                     Molecule_Set_Index[j] ]
              (*TauProfile.Tau)[ *, l, i, m, j ] = TEMPORARY( Tau )
            ENDFOR
          ENDFOR
        ENDFOR
      ENDFOR
    END

  ENDCASE

  ; Close the input data file
  NCDF_CLOSE, FileID


  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END ; FUNCTION Read_TauProfile_netCDF
