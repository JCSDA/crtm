PRO Compare_TauProfile, File1, File2, SensorName, $
                        PS = PS

  COMPILE_OPT STRICTARR

  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, $
             /CONTINUE
    IF ( KEYWORD_SET( PS ) ) THEN PSOff
    IF ( N_ELEMENTS( TauProfile1 ) GT 0 ) THEN BEGIN
      IF ( TauProfile1.n_Allocates EQ 1 ) THEN $
        Result = Destroy_TauProfile( TauProfile1 )
    ENDIF
    IF ( N_ELEMENTS( TauProfile2 ) GT 0 ) THEN BEGIN
      IF ( TauProfile2.n_Allocates EQ 1 ) THEN $
        Result = Destroy_TauProfile( TauProfile2 )
    ENDIF
    RETURN
  ENDIF    


  IF ( KEYWORD_SET( PS ) ) THEN BEGIN
    PSOn, Filename = 'TauProfile_Diff.ps'
    DEVICE, SCALE_FACTOR = 0.6
    Font = 1
  ENDIF ELSE BEGIN
    Font = -1
  ENDELSE


  Molecule_Set_Index = [ 1, 10, 12, 13, 15, 101, 113 ]
  Molecule_Set_Name  = [ 'WLO', 'ALL', 'WET', 'DRY', 'WCO', 'EffWLO', 'EffDRY' ]


  ; ------------------------------
  ; Read the TauProfile data files
  ; ------------------------------

  Result = Read_TauProfile_netCDF( File1, $
                                   TauProfile1 )

  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading TauProfile file ' + File1, $
             /NONAME, /NOPRINT


  Result = Read_TauProfile_netCDF( File2, $
                                   TauProfile2 )

  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading TauProfile file ' + File2, $
             /NONAME, /NOPRINT

  Pressure = REVERSE( (*TauProfile1.Level_Pressure)[1:TauProfile1.n_Layers] )
  Channel  = *TauProfile1.Channel


  ; --------------------------------------
  ; Loop over molecule sets in TauProfile1
  ; --------------------------------------

  FOR j1 = 0, TauProfile1.n_Molecule_Sets - 1 DO BEGIN

    Molecule_Set1 = (*TauProfile1.Molecule_Set)[j1]

    j2 = WHERE( *TauProfile2.Molecule_Set EQ Molecule_Set1, Count )
    IF ( Count EQ 0 ) THEN CONTINUE

    jName = WHERE( Molecule_Set_Index EQ Molecule_Set1, Count )
    IF ( Count EQ 0 ) THEN $
      Name = 'UNKNOWN' $
    ELSE $
      Name = Molecule_Set_Name[ jName ]

    Tau1 = (*TauProfile1.Tau)[*,*,*,*,j1]
    Tau2 = (*TauProfile2.Tau)[*,*,*,*,j2]

    TauDiff = TOTAL( ( TOTAL( TEMPORARY( Tau1 ) - TEMPORARY( Tau2 ), 4 ) ), 3 )

    SURFACE, TauDiff, $
             Pressure, Channel, $
             XTITLE = 'Pressure (hPa)', $
             XRANGE = [ MAX( Pressure ), 0 ], /XSTYLE, $
             YTITLE = 'Channel', $
             YRANGE = [ MIN( Channel ), MAX( Channel ) ], /YSTYLE, $
             ZTITLE = 'dTau', $
             CHARSIZE = 3.0, $
             FONT = Font

    XYOUTS, 0.5, 0.96, 'Tau(' + Name + ') transmittance difference for ' + SensorName, $
            /NORM, ALIGN = 0.5, $
            CHARSIZE = 2.0, $
            FONT = Font

    IF ( NOT ( KEYWORD_SET( PS ) ) ) THEN BEGIN
      q = GET_KBRD( 1 )
      IF ( STRUPCASE( q ) EQ 'Q' ) THEN BREAK
    ENDIF

  ENDFOR

  IF ( KEYWORD_SET( PS ) ) THEN PSOff


  ; ---------------------------
  ; Destroy the data structures
  ; ---------------------------

  Result = Destroy_TauProfile( TauProfile1 )
  Result = Destroy_TauProfile( TauProfile2 )

END
