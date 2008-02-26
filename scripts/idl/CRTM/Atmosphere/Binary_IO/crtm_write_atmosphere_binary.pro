;+
FUNCTION CRTM_Write_Atmosphere_Binary, Filename   , $  ; Input
                                       Atm        , $  ; Input
                                       DEBUG=Debug     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error
  
  ; Determine the dimensions
  ; ------------------------
  Atm_Info = SIZE(Atm, /STRUCTURE)
  CASE Atm_Info.N_DIMENSIONS OF
    1: BEGIN
         n_Channels = 0L
         n_Profiles = LONG(Atm_Info.DIMENSIONS[0])
       END
    2: BEGIN
         n_Channels = LONG(Atm_Info.DIMENSIONS[0])
         n_Profiles = LONG(Atm_Info.DIMENSIONS[1])
       END
    ELSE: MESSAGE, 'Invalid number of Atm dimensions', $
                   /NONAME, /NOPRINT
  ENDCASE
  
  ; Open the file
  ; -------------
  FileID = Open_Binary_File( Filename, /Write )
  IF ( FileID < 0 ) THEN $
    MESSAGE, 'Error opening file '+Filename, $
             /NONAME, /NOPRINT

  ; Write the file dimensions
  ; -------------------------
  WRITEU, FileID, n_Channels, n_Profiles
  
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Channels='+STRTRIM(n_Channels,2)+$
          '; n_Profiles='+STRTRIM(n_Profiles,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF

  ; Write the output data
  ; ---------------------
  IF ( n_Channels GT 0 ) THEN BEGIN
    ; Multiple channel data
    FOR m = 0L, n_Profiles-1L DO BEGIN

      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Writing profile #'+STRTRIM(m+1,2)
        PRINT
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      
      FOR l = 0L, n_Channels-1L DO BEGIN
        result = CRTM_Write_Atmosphere_Record( FileID, *Atm[l,m], DEBUG=Debug )
        IF ( result NE SUCCESS ) THEN $
          MESSAGE, 'Error writing Atmosphere element ('+STRTRIM(l+1,2)+','+ $
                   STRTRIM(m+1,2)+') from '+Filename, $
                   /NONAME, /NOPRINT
      ENDFOR
    ENDFOR
  ENDIF ELSE BEGIN
    ; No channel data
    FOR m = 0L, n_Profiles-1L DO BEGIN

      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Writing profile #'+STRTRIM(m+1,2)
        PRINT
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      
      result = CRTM_Write_Atmosphere_Record( FileID, *Atm[m], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error writing Atmosphere element ('+STRTRIM(m+1,2)+') from '+Filename, $
                 /NONAME, /NOPRINT
    ENDFOR
  ENDELSE    

  ; Done
  ; ----
  FREE_LUN, FileID
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
