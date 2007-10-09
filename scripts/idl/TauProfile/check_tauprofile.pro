PRO Check_TauProfile, TauProfile_Filename, $
                      Microwave = Microwave, $
                      PS = PS

  COMPILE_OPT STRICTARR

  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, $
             /CONTINUE
    IF ( KEYWORD_SET( PS ) ) THEN PSOff
    IF ( N_ELEMENTS( TauProfile ) GT 0 ) THEN BEGIN
      IF ( TauProfile.n_Allocates EQ 1 ) THEN $
        Result = Destroy_TauProfile( TauProfile )
    ENDIF
    RETURN
  ENDIF    


  ALL_Index = 10
  IF ( KEYWORD_SET( Microwave ) ) THEN BEGIN
    Molecule_Set_Index = [ 15, 101, 113 ]
    Molecule_Set_Name  = [ 'ALL', 'WCO', 'EffWLO', 'EffDRY' ]
  ENDIF ELSE BEGIN
    Molecule_Set_Index = [ 15, 101, 113, 114 ]
    Molecule_Set_Name  = [ 'ALL', 'WCO', 'EffWLO', 'EffDRY', 'EffOZO' ]
  ENDELSE
  n_Molecule_Sets = N_ELEMENTS( Molecule_Set_Index )


  IF ( KEYWORD_SET( PS ) ) THEN BEGIN
    PSOn, Filename = TauProfile_Filename + '.TauALL-TauPROD_Check.ps'
    DEVICE, SCALE_FACTOR = 0.6
    Font = 1
  ENDIF ELSE BEGIN
    Font = -1
  ENDELSE




  ; ------------------------------
  ; Read the TauProfile data file
  ; ------------------------------

  Result = Read_TauProfile_netCDF( TauProfile_Filename, $
                                   TauProfile )

  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading TauProfile file ' + TauProfile_Filename, $
             /NONAME, /NOPRINT

  Pressure = REVERSE( (*TauProfile.Level_Pressure)[1:TauProfile.n_Layers] )
  Channel  = *TauProfile.Channel


  ; -----------------------------------------------------
  ; Extract TOTAL transmittance from TauProfile structure
  ; -----------------------------------------------------

  jALL = WHERE( *TauProfile.Molecule_Set EQ ALL_Index, Count )
  IF ( Count EQ 0 ) THEN $
    MESSAGE, 'Total transmittance not found in TauProfile from ' + TauProfile_Filename, $
             /NONAME, /NOPRINT

  TauALL = (*TauProfile.Tau)[*,*,*,*,jALL]


  ; ------------------------------------
  ; Create a transmittance product array
  ; ------------------------------------

  TauPROD = MAKE_ARRAY( DIMENSION = SIZE( TauALL, /DIMENSIONS ), $
                        VALUE     = 1.0d0 )


  ; --------------------------------
  ; Loop over required molecule sets
  ; --------------------------------

  FOR j = 0, n_Molecule_Sets - 1 DO BEGIN

    Molecule_Set = Molecule_Set_Index[ j ]

    jMatch = WHERE( *TauProfile.Molecule_Set EQ Molecule_Set, Count )
    IF ( Count EQ 0 ) THEN CONTINUE

    TauPROD = TauPROD * (*TauProfile.Tau)[*,*,*,*,jMatch]

  ENDFOR

  TauDiff = TOTAL( ( TOTAL( TauALL - TauPROD, 4 ) ), 3 )

  zRange = [ MIN( TauDIFF ), MAX( TauDIFF ) ]
  zTitle = 'SUM(dTau)'
  Axis_Scale, zRange, zTitle, $
              zScaleFactor, zScaleTitle

  SURFACE, zScaleFactor * TauDiff, $
           Pressure, Channel, $
           XTITLE = 'Pressure (hPa)', $
           XRANGE = [ MAX( Pressure ), 0 ], /XSTYLE, $
           YTITLE = 'Channel', $
           YRANGE = [ MIN( Channel ), MAX( Channel ) ], /YSTYLE, $
           ZTITLE = zScaleTitle, $
           ZRANGE = zScaleFactor * zRange, $
           CHARSIZE = 3.0, $
           FONT = Font

  XYOUTS, 0.5, 0.96, 'Cumulative Tau(ALL) - Tau(PROD) transmittance difference!Cfor ' + TauProfile_Filename, $
          /NORM, ALIGN = 0.5, $
          CHARSIZE = 2.0, $
          FONT = Font

  IF ( KEYWORD_SET( PS ) ) THEN PSOff


  ; ---------------------------
  ; Destroy the data structures
  ; ---------------------------

  Result = Destroy_TauProfile( TauProfile )

END
