PRO CRTM_RTSolution__Define
  void = { CRTM_RTSolution, $
           n_Layers                : 0L, $
           Surface_Emissivity      : 0.0d0, $
           Up_Radiance             : 0.0d0, $
           Down_Radiance           : 0.0d0, $
           Down_Solar_Radiance     : 0.0d0, $
           Surface_Planck_Radiance : 0.0d0, $
           Radiance                : 0.0d0, $
           Brightness_Temperature  : 0.0d0, $
           Layer_Optical_Depth     : PTR_NEW() }
END


FUNCTION CRTM_Destroy_RTSolution, Rts

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
  Rts.n_Layers = 0L

  ; Free the structure pointers
  ; ---------------------------
  PTR_FREE, Rts.Layer_Optical_Depth

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END


FUNCTION CRTM_Allocate_RTSolution, n_Layers, $
                                   Rts

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
  result = CRTM_Destroy_RTSolution( Rts )
  IF ( result NE SUCCESS ) THEN $
     MESSAGE, 'Error destroying RTSolution structure', $
              /NONAME, /NOPRINT
              
  ; Allocate the arrays
  ; -------------------
  Rts.Layer_Optical_Depth = PTR_NEW(DBLARR( n_Layers ))
  
  ; Assign the dimensions
  ; ---------------------
  Rts.n_Layers    = n_Layers
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS

END


FUNCTION Read_RTSolution_Record, FileID, $
                                 Rts

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
  n_Layers = Rts.n_Layers
  READU, FileID, n_Layers
  
  ; Allocate the RTSolution structure
  ; ---------------------------------
  result = CRTM_Allocate_RTSolution( n_Layers, $
                                     Rts       )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating RTSolution structure', $
             /NONAME, /NOPRINT
  
  ; Read the forward radiative transfer intermediate results
  ; --------------------------------------------------------
  Surface_Emissivity      = Rts.Surface_Emissivity     
  Up_Radiance             = Rts.Up_Radiance            
  Down_Radiance           = Rts.Down_Radiance          
  Down_Solar_Radiance     = Rts.Down_Solar_Radiance    
  Surface_Planck_Radiance = Rts.Surface_Planck_Radiance
  READU, FileID, Surface_Emissivity     , $
                 Up_Radiance            , $
                 Down_Radiance          , $
                 Down_Solar_Radiance    , $
                 Surface_Planck_Radiance, $
                 *Rts.Layer_Optical_Depth
  Rts.Surface_Emissivity      = Surface_Emissivity     
  Rts.Up_Radiance             = Up_Radiance            
  Rts.Down_Radiance           = Down_Radiance          
  Rts.Down_Solar_Radiance     = Down_Solar_Radiance    
  Rts.Surface_Planck_Radiance = Surface_Planck_Radiance
  
  ; Read the radiative transfer results
  ; -----------------------------------
  Radiance               = Rts.Radiance
  Brightness_Temperature = Rts.Brightness_Temperature
  READU, FileID, Radiance              , $
                 Brightness_Temperature
  Rts.Radiance               = Radiance
  Rts.Brightness_Temperature = Brightness_Temperature

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END

;+
FUNCTION CRTM_Read_RTSolution_Binary, Filename, $  ; Input
                                      Rts          ; Output
;-

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
  FileID = Open_Binary_File( Filename )
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
  Rts = PTRARR( n_Channels, n_Profiles )
  FOR m = 0L, n_Profiles-1L DO BEGIN
    FOR l = 0L, n_Channels-1L DO BEGIN
      Rts[l,m] = PTR_NEW({CRTM_RTSolution})
      result = Read_RTSolution_Record( FileID, *Rts[l,m] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error reading RTSolution element ('+STRTRIM(l+1,2)+','+ $
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


