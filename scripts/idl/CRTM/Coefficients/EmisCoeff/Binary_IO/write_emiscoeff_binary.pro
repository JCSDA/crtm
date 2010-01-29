;+
FUNCTION write_emiscoeff_binary, Filename   , $ ; Input
                                 EmisC      , $ ; Input
                                 SWAP=Swap  , $ ; Optional input
                                 DEBUG=Debug    ; Optional input
;-
  ; Set up
  ; ------
  @crtm_binary_io_error
  @emiscoeff_parameters
  
  ; Open the file
  ; -------------
  FileID = Open_Binary_File( Filename, /Write, SWAP=Swap )
  IF ( FileID < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             /NONAME, /NOPRINT
            
  ; Write the file metadata
  ; -----------------------
  WRITEU, FileID, EmisC.Data_Type
  
  WRITEU, FileID, EmisC.Release, $
                  EmisC.Version
                  
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  Data_Type='+STRTRIM(EmisC.Data_Type,2)+$
          '; Release='+STRTRIM(EmisC.Release,2)+$
          '; Version='+STRTRIM(EmisC.Version,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF

  ; Write the file dimensions
  ; -------------------------
  WRITEU, FileID, EmisC.n_Angles     , $
                  EmisC.n_Frequencies, $
                  EmisC.n_Wind_Speeds
                  
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Angles='+STRTRIM(n_Angles,2)+$
          '; n_Frequencies='+STRTRIM(n_Frequencies,2)+$
          '; n_Wind_Speeds='+STRTRIM(n_Wind_Speeds,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Write the type and number of data fields
  ; ----------------------------------------
  WRITEU, FileID, N_EMISCOEFF_ITEMS
  WRITEU, FileID, EMISCOEFF_DATA_TYPES
  
  ; Write the data
  ; --------------
  WRITEU, FileID, *EmisC.Angle 
  WRITEU, FileID, *EmisC.Frequency
  WRITEU, FileID, *EmisC.Wind_Speed    
  WRITEU, FileID, *EmisC.Emissivity
                                
  ; Done
  ; ----
  FREE_LUN, FileID
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
