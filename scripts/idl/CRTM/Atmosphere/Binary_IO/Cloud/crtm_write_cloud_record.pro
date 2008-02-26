;+
FUNCTION CRTM_Write_Cloud_Record, FileID     , $  ; Input
                                  Cloud      , $  ; Input
                                  DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error

  ; Write the data dimensions
  ; -------------------------
  WRITEU, FileID, Cloud.n_Layers
  
  ; Write the cloud data
  ; -------------------
  WRITEU, FileID, Cloud.Type, $
                 *Cloud.Effective_Radius, $
                 *Cloud.Effective_Variance, $
                 *Cloud.Water_Content
                
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Layers='+STRTRIM(Cloud.n_Layers,2)+$
          '; Type='+STRTRIM(Cloud.Type,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF

  ; Done
  ; ----
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
