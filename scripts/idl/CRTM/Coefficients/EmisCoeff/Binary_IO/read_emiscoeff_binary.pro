;+
FUNCTION Read_EmisCoeff_Binary, Filename   , $ ; Input
                                EmisC      , $ ; Output
                                DEBUG=Debug    ; Optional input
;-
  ; Set up
  ; ------
  @crtm_binary_io_error
  
  EmisC={EmisCoeff}
  
  ; open file for reading
  ; ---------------------
  FileID = Open_Binary_File( Filename )
  IF ( FileID < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             /NONAME, /NOPRINT
      
  ; Read the metadata fields
  ; ------------------------
  Data_Type = 0L
  READU, FileID, Data_Type
  
  Release = 0L
  Version = 0L
  READU, FileID, Release, $
                 Version
                 
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  Data_Type='+STRTRIM(Data_Type,2)+$
          '; Release='+STRTRIM(Release,2)+$
          '; Version='+STRTRIM(Version,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Assign EmisC metadata
  EmisC.Data_Type = Data_Type
  EmisC.Release   = Release
  EmisC.Version   = Version
  
  ; Read the data dimensions
  ; ------------------------
  n_Angles      = 0L
  n_Frequencies = 0L
  n_Wind_Speeds = 0L                                
  READU, FileID, n_Angles     , $
                 n_Frequencies, $
                 n_Wind_Speeds
                   
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Angles='+STRTRIM(n_Angles,2)+$
          '; n_Frequencies='+STRTRIM(n_Frequencies,2)+$
          '; n_Wind_Speeds='+STRTRIM(n_Wind_Speeds,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF
  
  ; Get the number of data fields
  ; and the type of data
  READU, FileID, n_EmisCoeff_Items  
  READU, FileID, Emiscoeff_Data_Types

  ; Allocate the EmisC structure
  ; ----------------------------
  result = Allocate_EmisCoeff( n_Angles     , $
                               n_Frequencies, $
                               n_Wind_Speeds, $
                               EmisC          )
  IF ( result NE SUCCESS ) THEN $
    MESSAGE, 'Error allocating EmisC structure', $
             /NONAME, /NOPRINT
             
  ; Read into the structure's arrays
  ; --------------------------------
  READU, FileID, *EmisC.Angle     
  READU, FileID, *EmisC.Frequency
  READU, FileID, *EmisC.Wind_Speed
  READU, FileID, *EmisC.Emissivity
                 
  ; Done
  ; ----
  FREE_LUN, FileID
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
