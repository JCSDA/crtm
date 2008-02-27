;+
FUNCTION CRTM_Read_Atmosphere_Binary, Filename                         , $  ; Input
                                      Atm                              , $  ; Output, PTRARR
                                      DEBUG=Debug                      , $  ; Optional input
                                      SELECTED_PROFILE=Selected_Profile     ; Optional input
;-
  ; Set up error handler
  ; --------------------
  @crtm_binary_io_error
  
  ; Destroy the output if necessary
  ; -------------------------------
  Atm_Info = SIZE(Atm, /STRUCTURE)
  ; Are there any elements?
  IF ( Atm_Info.N_ELEMENTS GT 0 ) THEN BEGIN
    FOR n = 0, Atm_Info.N_ELEMENTS - 1 DO BEGIN
      result = CRTM_Destroy_Atmosphere( *Atm[n] )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error destroying Atmosphere upon input', $
                 /NONAME, /NOPRINT
      PTR_FREE, Atm[n]
    ENDFOR
  ENDIF
  
  ; Process keywords
  ; ----------------
  Read_All = TRUE
  ism = -1
  IF ( N_ELEMENTS(Selected_Profile) EQ 1 ) THEN BEGIN
    Read_All = FALSE
    ism = LONG(Selected_Profile)-1
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
  
  IF ( KEYWORD_SET(Debug) ) THEN BEGIN
    Msg = '  n_Channels='+STRTRIM(n_Channels,2)+$
          '; n_Profiles='+STRTRIM(n_Profiles,2)
    MESSAGE, Msg, /INFORMATIONAL
  ENDIF

  ; Create the output array
  ; -----------------------
  IF ( n_Channels GT 0 ) THEN BEGIN
    ; Multiple channel data
    Atm = PTRARR( n_Channels, n_Profiles )
    FOR m = 0L, n_Profiles-1L DO BEGIN

      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Reading profile #'+STRTRIM(m+1,2)
        PRINT
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      
      IF ( Read_All EQ TRUE ) THEN im = m ELSE im = 0
      FOR l = 0L, n_Channels-1L DO BEGIN
        IF ( PTR_VALID(Atm[l,im]) ) THEN BEGIN
          result = CRTM_Destroy_Atmosphere(*Atm[l,im])
          PTR_FREE, Atm[l,im]
        ENDIF
        Atm[l,im] = PTR_NEW({CRTM_Atmosphere})
        result = CRTM_Read_Atmosphere_Record( FileID, *Atm[l,im], DEBUG=Debug )
        IF ( result NE SUCCESS ) THEN $
          MESSAGE, 'Error reading Atmosphere element ('+STRTRIM(l+1,2)+','+ $
                   STRTRIM(m+1,2)+') from '+Filename, $
                   /NONAME, /NOPRINT
      ENDFOR
      ; If we have read the required profile, exit
      IF ( (Read_All EQ FALSE) AND (m EQ ism) ) THEN BEGIN
        Atm = Atm[*,0]
        BREAK
      ENDIF
    ENDFOR
  ENDIF ELSE BEGIN
    ; No channel data
    Atm = PTRARR( n_Profiles )
    FOR m = 0L, n_Profiles-1L DO BEGIN

      IF ( KEYWORD_SET(Debug) ) THEN BEGIN
        Msg = '  Reading profile #'+STRTRIM(m+1,2)
        PRINT
        MESSAGE, Msg, /INFORMATIONAL
      ENDIF
      
      IF ( Read_All EQ TRUE ) THEN im = m ELSE im = 0
      IF ( PTR_VALID(Atm[im]) ) THEN BEGIN
        result = CRTM_Destroy_Atmosphere(*Atm[im])
        PTR_FREE, Atm[im]
      ENDIF
      Atm[im] = PTR_NEW({CRTM_Atmosphere})
      result = CRTM_Read_Atmosphere_Record( FileID, *Atm[im], DEBUG=Debug )
      IF ( result NE SUCCESS ) THEN $
        MESSAGE, 'Error reading Atmosphere element ('+STRTRIM(m+1,2)+') from '+Filename, $
                 /NONAME, /NOPRINT
      ; If we have read the required profile, exit
      IF ( (Read_All EQ FALSE) AND (m EQ ism) ) THEN BEGIN
        Atm = Atm[0]
        BREAK
      ENDIF
    ENDFOR
  ENDELSE    

  ; Done
  ; ----
  FREE_LUN, FileID
  CATCH, /CANCEL
  RETURN, SUCCESS
  
END
