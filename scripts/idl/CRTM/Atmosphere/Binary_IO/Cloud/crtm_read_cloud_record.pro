;+
FUNCTION CRTM_Read_Cloud_Record, FileID           , $  ; Input
                                 Cloud            , $  ; Output
                                 ALLOCATE=Allocate, $  ; Optional input
                                 DEBUG=Debug           ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Read the data dimensions
  ; ------------------------
  n_Layers = 0L
  READU, FileID, n_Layers
  
  ; Allocate the cloud structure if required
  ; ----------------------------------------
  IF ( KEYWORD_SET(Allocate) ) THEN BEGIN
    result = CRTM_Allocate_Cloud( n_Layers, $
                                  Cloud     )
    IF ( result NE SUCCESS ) THEN $
      MESSAGE, 'Error allocating Cloud structure', $
               /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    IF ( n_Layers NE Cloud.n_Layers ) THEN $
      MESSAGE, 'Cloud data dimensions, ' + $
               STRTRIM(n_Layers,2)+ $
               ', are inconsistent with structure definition, ' + $
               STRTRIM(Cloud.n_Layers,2), $
               /NONAME, /NOPRINT
  ENDELSE
  
  ; Read the cloud data
  ; -------------------
  Type = Cloud.Type
  READU, FileID, Type, $
                 *Cloud.Effective_Radius, $
                 *Cloud.Effective_Variance, $
                 *Cloud.Water_Content
  Cloud.Type = Type
                
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(n_Layers,2)+$
          '; Type='+STRTRIM(Type,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
