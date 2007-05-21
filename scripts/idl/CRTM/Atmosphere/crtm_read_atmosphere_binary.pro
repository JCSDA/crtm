PRO CRTM_Cloud__Define
  void = { CRTM_Cloud, $
           n_Layers           : 0L, $
           Type               : 0L, $
           Effective_Radius   : PTR_NEW(), $
           Effective_Variance : PTR_NEW(), $
           Water_Content      : PTR_NEW()  }
END

PRO CRTM_Aerosol__Define
  void = { CRTM_Aerosol, $
           n_Layers           : 0L, $
           Type               : 0L, $
           Effective_Radius   : PTR_NEW(), $
           Concentration      : PTR_NEW()  }
END

PRO CRTM_Atmosphere__Define
  void = { CRTM_Atmosphere, $
           n_Layers       : 0L, $
           n_Absorbers    : 0L, $
           n_Clouds       : 0L, $
           n_Aerosols     : 0L, $
           Climatology    : 0L, $
           Absorber_ID    : PTR_NEW(), $
           Absorber_Units : PTR_NEW(), $
           Level_Pressure : PTR_NEW(), $
           Pressure       : PTR_NEW(), $
           Temperature    : PTR_NEW(), $
           Absorber       : PTR_NEW(), $
           Cloud          : PTR_NEW(), $
           Aerosol        : PTR_NEW()  }
END


FUNCTION CRTM_Destroy_Cloud, Cloud

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Reinitialise the dimensions
  ; ---------------------------
  Cloud.n_Layers = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Cloud.Effective_Radius, $
            Cloud.Effective_Variance, $
            Cloud.Water_Content

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Destroy_Aerosol, Aerosol

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Reinitialise the dimensions
  ; ---------------------------
  Aerosol.n_Layers = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Aerosol.Effective_Radius, $
            Aerosol.Concentration

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Destroy_Atmosphere, Atm

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Reinitialise the dimensions
  ; ---------------------------
  Atm.n_Layers    = 0L
  Atm.n_Absorbers = 0L
  Atm.n_Clouds    = 0L
  Atm.n_Aerosols  = 0L
  
  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Atm.Absorber_ID   , $
            Atm.Absorber_Units, $
            Atm.Level_Pressure, $
            Atm.Pressure      , $
            Atm.Temperature   , $
            Atm.Absorber
  
  ; Free the Cloud structure array if required
  ; ------------------------------------------
  IF ( PTR_VALID( Atm.Cloud ) ) THEN BEGIN
    n_Clouds = N_ELEMENTS( *Atm.Cloud )
    FOR n = 0, n_Clouds - 1 DO BEGIN
      result = CRTM_Destroy_Cloud( (*Atm.Cloud)[n] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying Atmosphere Cloud element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF
  PTR_FREE, Atm.Cloud

  ; Free the Aerosol structure array if required
  ; --------------------------------------------
  IF ( PTR_VALID( Atm.Aerosol ) ) THEN BEGIN
    n_Aerosols = N_ELEMENTS( *Atm.Aerosol )
    FOR n = 0, n_Aerosols - 1 DO BEGIN
      result = CRTM_Destroy_Aerosol( (*Atm.Aerosol)[n] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying Atmosphere Aerosol element '+STRTRIM(n+1,2), $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDIF
  PTR_FREE, Atm.Aerosol

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Allocate_Cloud, n_Layers, $
                              Cloud
  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Destroy the structure first
  ; ---------------------------
  result = CRTM_Destroy_Cloud( Cloud )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Cloud structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Cloud.Effective_Radius   = PTR_NEW(DBLARR( n_Layers ))
  Cloud.Effective_Variance = PTR_NEW(DBLARR( n_Layers ))
  Cloud.Water_Content      = PTR_NEW(DBLARR( n_Layers ))

  ; Assign the dimension
  ; --------------------
  Cloud.n_Layers = n_Layers

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Allocate_Aerosol, n_Layers, $
                              Aerosol
  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Destroy the structure first
  ; ---------------------------
  result = CRTM_Destroy_Aerosol( Aerosol )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Aerosol structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Aerosol.Effective_Radius = PTR_NEW(DBLARR( n_Layers ))
  Aerosol.Concentration    = PTR_NEW(DBLARR( n_Layers ))

  ; Assign the dimension
  ; --------------------
  Aerosol.n_Layers = n_Layers

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Allocate_Atmosphere, n_Layers   , $
                                   n_Absorbers, $
                                   n_Clouds   , $
                                   n_Aerosols , $
                                   Atm
  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN, FAILURE
  ENDIF    

  ; Destroy the structure first
  ; ---------------------------
  result = CRTM_Destroy_Atmosphere( Atm )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying Atmosphere structure', $
              /NONAME, /NOPRINT

  ; Allocate the intrinsic type arrays
  ; ----------------------------------
  Atm.Absorber_ID    = PTR_NEW(LONARR( n_Absorbers ))
  Atm.Absorber_Units = PTR_NEW(LONARR( n_Absorbers ))
  Atm.Level_Pressure = PTR_NEW(DBLARR( n_Layers+1 ))
  Atm.Pressure       = PTR_NEW(DBLARR( n_Layers ))
  Atm.Temperature    = PTR_NEW(DBLARR( n_Layers ))
  Atm.Absorber       = PTR_NEW(DBLARR( n_Layers, n_Absorbers ))
  
  ; Allocate the cloud and aerosol structure arrays
  ; -----------------------------------------------
  IF ( n_Clouds   GT 0 ) THEN Atm.Cloud   = PTR_NEW(REPLICATE( {CRTM_Cloud}  , n_Clouds   ))
  IF ( n_Aerosols GT 0 ) THEN Atm.Aerosol = PTR_NEW(REPLICATE( {CRTM_Aerosol}, n_Aerosols ))
  
  ; Assign the dimensions
  ; ---------------------
  Atm.n_Layers    = n_Layers
  Atm.n_Absorbers = n_Absorbers
  Atm.n_Clouds    = n_Clouds
  Atm.n_Aerosols  = n_Aerosols
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION Read_Cloud_Record, FileID, $
                            Cloud

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF    

  ; Read the data dimensions
  ; ------------------------
  n_Layers = 0L
  READU, FileID, n_Layers
  
  ; Allocate the cloud structure
  ; -----------------------------
  result = CRTM_Allocate_Cloud( n_Layers, $
                                Cloud     )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating Cloud structure', $
             /NONAME, /NOPRINT

  ; Read the cloud data
  ; -------------------
  Type = 0L
  READ, FileID, Type, $
                *Cloud.Effective_Radius, $
                *Cloud.Effective_Variance, $
                *Cloud.Water_Content
  Cloud.Type = Type
                
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END

FUNCTION Read_Aerosol_Record, FileID, $
                              Aerosol

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF    

  ; Read the data dimensions
  ; ------------------------
  n_Layers = 0L
  READU, FileID, n_Layers
  
  ; Allocate the Aerosol structure
  ; ------------------------------
  result = CRTM_Allocate_Aerosol( n_Layers, $
                                  Aerosol   )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating Aerosol structure', $
             /NONAME, /NOPRINT

  ; Read the Aerosol data
  ; ---------------------
  Type = 0L
  READ, FileID, Type, $
                *Aerosol.Effective_Radius, $
                *Aerosol.Concentration
  Aerosol.Type = Type
                
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION Read_Atmosphere_Record, FileID, $
                                 Atm

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF    


  ; Read the data dimensions
  ; ------------------------
  n_Layers    = 0L
  n_Absorbers = 0L
  n_Clouds    = 0L
  n_Aerosols  = 0L
  READU, FileID, n_Layers   , $
                 n_Absorbers, $
                 n_Clouds   , $
                 n_Aerosols
  
  ; Allocate the atmosphere structure
  ; ---------------------------------
  result = CRTM_Allocate_Atmosphere( n_Layers   , $
                                     n_Absorbers, $
                                     n_Clouds   , $
                                     n_Aerosols , $
                                     Atm          )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating Atmosphere structure', $
             /NONAME, /NOPRINT
  
  ; Read the climatology model flag and absorber IDs
  ; ------------------------------------------------
  Climatology = 0L
  READU, FileID, Climatology, $
                 *Atm.Absorber_ID, $
                 *Atm.Absorber_Units
  Atm.Climatology = Climatology

  ; Read the atmospheric profile data
  ; ---------------------------------
  READU, FileID, *Atm.Level_Pressure, $
                 *Atm.Pressure      , $
                 *Atm.Temperature   , $
                 *Atm.Absorber      

  ; Read the cloud data
  ; -------------------
  FOR n = 0, n_Clouds-1 DO BEGIN
    result = Read_Cloud_Record( FileID, (*Atm.Cloud)[n] )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Cloud element '+STRTRIM(n+1,2), $
               /NONAME, /NOPRINT
  ENDFOR

  ; Read the Aerosol data
  ; -------------------
  FOR n = 0, n_Aerosols-1 DO BEGIN
    result = Read_Aerosol_Record( FileID, (*Atm.Aerosol)[n] )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error reading Atmosphere Aerosol element '+STRTRIM(n+1,2), $
               /NONAME, /NOPRINT
  ENDFOR

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Read_Atmosphere_Binary, Filename, $
                                      Atm

  ; Set up error handler
  ; --------------------
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    FREE_LUN, FileID
    RETURN, FAILURE
  ENDIF    

  ; Open the file
  ; -------------
  FileID = Open_Binary_File( filename )
  IF ( FileID < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             /NONAME, /NOPRINT

  ; Read the file dimensions
  ; ------------------------
  n_Channels = 0L
  n_Profiles = 0L
  READU, FileID, n_Channels, n_Profiles
  
  ; Create the output array
  ; -----------------------
  Atm = PTRARR( n_Channels, n_Profiles )
  FOR m = 0L, n_Profiles-1L DO BEGIN
    FOR l = 0L, n_Channels-1L DO BEGIN
      Atm[l,m] = PTR_NEW({CRTM_Atmosphere})
      result = Read_Atmosphere_Record( FileID, *Atm[l,m] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error reading Atmosphere element ('+STRTRIM(l+1,2)+','+ $
                 STRTRIM(m+1,2)+' from '+Filename, $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDFOR
  

  ; Done
  ; ----
  FREE_LUN, FileID
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
