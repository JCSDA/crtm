PRO plot_bias, Sat_ID, nChannels, Max_FOV = Max_FOV

  BiasName = [ 'Constant', $
               'Angle',    $
               'CLW',      $
               'Lapse2',   $
               'Lapse',    $
               'Airmass',  $
               'Fixed',    $
               'Total',    $
               'NBC',      $
               'BC',       $
               'DNBC',     $
               'DBC',      $
               'NewAng',   $
               'NewFix',   $
               'update'    ]

  BiasDescriptor = [ 'Constant bias',          $
                     'Angle bias',             $
                     'CLW bias',               $
                     '(Lapse rate)!U2!N bias', $
                     'Lapse rate bias',        $
                     'Airmass bias',           $
                     'Fixed bias',             $
                     'Fixed+Airmass bias',     $
                     'O-G(nbc)',               $
                     'O-G(bc)',                $
                     'O-G(Dnbc)',              $
                     'O-G(Dbc)',               $
                     'NewAng',                 $
                     'NewFix',                 $
                     'Update'                  ]

  nBiases = N_ELEMENTS( BiasName )

  ; -- Indices of bias types into the arrays
  iBias_Constant = 0
  iBias_Angle    = 1
  iBias_CLW      = 2
  iBias_Lapse2   = 3
  iBias_Lapse    = 4
  iBias_Airmass  = 5
  iBias_Fixed    = 6
  iBias_Total    = 7
  iBias_NBC      = 8
  iBias_BC       = 9
  iBias_DNBC     = 10
  iBias_DBC      = 11
  iBias_NewAng   = 12
  iBias_NewFix   = 13
  iBias_Update   = 14

  ; -- Indices of biases used to determine min/max
  iBias_MinMax = [ iBias_Constant, $
                   iBias_Angle,    $
                   iBias_CLW,      $
                   iBias_Lapse2,   $
                   iBias_Lapse,    $
                   iBias_Airmass,  $
                   iBias_BC,       $
                   iBias_NewAng,   $
                   iBias_Update    ]


  ; -- Read the bias data
  FOR n = 0L, nBiases - 1L DO BEGIN

    FileName = BiasName[n] + '_' + Sat_ID
    Read_SatBias_Angle, FileName, nChannels, tmp

    IF ( n EQ 0 ) THEN $
      Component = TEMPORARY(tmp) $
    ELSE $
      Component = [ [ Component ], [ TEMPORARY(tmp) ] ]
  ENDFOR


  ; -- Read the sample size data
  Read_SatBias_Angle, 'sample_' + Sat_ID, nChannels, Sample, /High_Precision


  ; -- Determine the maximum number of FOVs to display
  IF ( N_ELEMENTS( Max_FOV ) NE 0 ) THEN $
    mz = Max_FOV-1L $
  ELSE $
    mz = N_ELEMENTS(Component[0,0].Bias)-1L


  x = findgen(mz+1)

  psave = !P

  ; -- The indices for plotting, overplotting
  IF ( !D.NAME EQ 'PS' ) THEN $
    Colors_AllAirMass = [ !P.COLOR, 5, 4, 6, 2 ] $
  ELSE $
    Colors_AllAirMass = [ !P.COLOR, 5, 4, 3, 2 ]

  iBias_AllAirMass = [ iBias_Constant, $
                       iBias_Angle,    $
                       iBias_Lapse2,   $
                       iBias_Lapse,    $
                       iBias_CLW       ]
  iBias_Others = [ iBias_Airmass, $
                   iBias_Fixed,   $
                   iBias_NBC,     $
                   iBias_BC,      $
                   iBias_NewAng,  $
                   iBias_NewFix   ]

  FitColor = 2
  AvgColor = 1
  MinMaxColor = 5
  CharSize = 2.0
  PSym = -4
  IF !D.NAME EQ 'PS' THEN CharSize = 3.5

  ; -- Plot all the bias components
  FOR l = 0, nChannels - 1L DO BEGIN
    !p.multi = [0,4,3]

    ; -- find the min and max bias values
    Min_Bias =  999.0
    Max_Bias = -999.0
    FOR n = 0L, N_ELEMENTS( iBias_MinMax ) - 1L DO BEGIN
      Min_Bias = Min_Bias < MIN( Component[l,iBias_MinMax[n]].Bias[0:mz] )
      Max_Bias = Max_Bias > MAX( Component[l,iBias_MinMax[n]].Bias[0:mz] )
    ENDFOR


    ; --------------------------------
    ; Loop over the Airmass bias types
    ; --------------------------------

    FOR n = 0L, N_ELEMENTS( iBias_AllAirMass ) - 1L DO BEGIN

      y = Component[l,iBias_AllAirMass[n]].Bias[0:mz]


      IF ( n EQ 0 ) THEN BEGIN

        Loc = WHERE( iBias_MinMax EQ iBias_AllAirMass[n], Count )
        IF ( Count GT 0 ) THEN $
          YRange = [ Min_Bias, Max_Bias ] $
        ELSE $
          YRange = [ MIN(y), MAX(y) ]

        PLOT, x, y, $
              TITLE = Sat_ID+': Air Mass Biases. Ch. ' + $
                      STRTRIM( Component[l,iBias_AllAirMass[n]].Instrument_Channel, 2 ), $
              XTITLE = 'FOV position', $
              YTITLE = 'Bias (K)', $
              YRANGE = YRange, $
              PSYM = PSym, $
              CHARSIZE = CharSize

        OPLOT, !X.CRANGE, [0,0], LINESTYLE = 2

      ENDIF ELSE BEGIN

        OPLOT, x, y, $
               COLOR = Colors_AllAirMass[n], $
               PSYM = PSym

      ENDELSE

    ENDFOR  ; AirMass bias components


    ; -- Plot a legend for the AirMass biases
    PLOT, [0,1], [0,1], $
          /NODATA, $
          XSTYLE = 4, YSTYLE = 4

    XYOUTS, 0.5, 0.9, 'Air Mass Biases', $
            ALIGN = 0.5, $
            /DATA, $
            CHARSIZE = CharSize

    MyLegend, 0.1, 0.85, $
              BiasDescriptor[ iBias_AllAirMass ], $
              COLOR = Colors_AllAirMass, $
              PSYM = REPLICATE( PSym, N_ELEMENTS( iBias_AllAirMass ) ), $
              CHARSIZE = 1.5


    ; -------------------------
    ; Plot the sample size data
    ; -------------------------

    y = Sample[l].Bias[0:mz]
    PLOT, x, y, $
          TITLE = Sat_ID+': Sample size. Ch. ' + STRTRIM( Sample[l].Instrument_Channel, 2 ), $
          XTITLE = 'FOV position', $
          YTITLE = '# of obs', $
          /YNOZERO, PSYM = PSym, CHARSIZE=CharSize


    ; -----------------------------------------------
    ; Plot the air mass angle and lapse rate bias sum
    ; -----------------------------------------------

    y = Component[l,iBias_Angle].Bias[0:mz] + $
        Component[l,iBias_Lapse2].Bias[0:mz] + $
        Component[l,iBias_Lapse].Bias[0:mz]

    PLOT, x, y, $
          TITLE = Sat_ID+': Angle+Lapse. Ch. ' + STRTRIM( Component[l,iBias_NBC].Instrument_Channel, 2 ), $
          XTITLE = 'FOV position', $
          YTITLE = 'Bias (K)', $
          YRANGE = [ Min_Bias, Max_Bias ], $
          /YNOZERO, PSYM = PSym, CHARSIZE=CharSize

    OPLOT, !X.CRANGE, [0,0], LINESTYLE = 2


    ; ------------------------------
    ; Loop over the other bias types
    ; ------------------------------

    FOR n = 0L, N_ELEMENTS( iBias_Others ) - 1L  DO BEGIN

      y = Component[l,iBias_Others[n]].Bias[0:mz]

      Loc = WHERE( iBias_MinMax EQ iBias_Others[n], Count )
      IF ( Count GT 0 ) THEN BEGIN
        YRange = [ Min_Bias, Max_Bias ]
        PlotColor = !P.COLOR
      ENDIF ELSE BEGIN
        YRange = [ MIN(y), MAX(y) ]
        PlotColor = MinMaxColor
      ENDELSE

      PLOT, x, y, $
            TITLE = Sat_ID+': '+BiasDescriptor[iBias_Others[n]]+'. Ch. ' + $
                    STRTRIM( Component[l,iBias_Others[n]].Instrument_Channel, 2 ), $
            XTITLE = 'FOV position', $
            YTITLE = 'Bias (K)', $
            YRANGE = YRange, $
            COLOR = PlotColor, $
            /YNOZERO, PSYM = PSym, CHARSIZE=CharSize

      yavg = MEAN( y )
      OPLOT, !X.CRANGE, [yavg, yavg],COLOR=AvgColor

      OPLOT, !X.CRANGE, [0,0], LINESTYLE = 2

      ; -- Over plot the diagnostic file nbc and bc
      ; -- data on the NBC and BC plots
      IF ( iBias_Others[n] EQ iBias_NBC OR $
           iBias_Others[n] EQ iBias_BC     ) THEN BEGIN

        CASE iBias_Others[n] OF
          iBias_NBC: y2 = Component[l,iBias_DNBC].Bias[0:mz]
          iBias_BC:  y2 = Component[l,iBias_DBC].Bias[0:mz]
        ENDCASE

        OPLOT, x, y2, COLOR = 4, PSYM = -6, SYMSIZE=1.5

      ENDIF

    ENDFOR  ; Other bias types


    ; ----------------------------------------------------
    ; Plot the obs-ges with just the fixed bias correction
    ; ----------------------------------------------------

    y = Component[l,iBias_NBC].Bias[0:mz] - Component[l,iBias_Fixed].Bias[0:mz]
    PLOT, x, y, $
          TITLE = Sat_ID+': O-G(Fixed BC Only). Ch. ' + STRTRIM( Component[l,iBias_NBC].Instrument_Channel, 2 ), $
          XTITLE = 'FOV position', $
          YTITLE = 'Bias (K)', $
          YRANGE = [ Min_Bias, Max_Bias ], $
          /YNOZERO, PSYM = PSym, CHARSIZE=CharSize

    yavg = MEAN( y )
    OPLOT, !X.CRANGE, [yavg, yavg],COLOR=AvgColor

    y2 = Component[l,iBias_NBC].Bias[0:mz] - Component[l,iBias_NewFix].Bias[0:mz]
    OPLOT, x, y2, COLOR = 4, PSYM = -6, SYMSIZE=1.5

    OPLOT, !X.CRANGE, [0,0], LINESTYLE = 2


    ; ------------------------------------------------------------
    ; Plot the BC obs-ges but with the new fix and ang corrections
    ; ------------------------------------------------------------

    y = Component[l,iBias_NBC].Bias[0:mz] - $
        Component[l,iBias_Constant].Bias[0:mz] - $
        Component[l,iBias_NewAng  ].Bias[0:mz] - $  ; <---***
        Component[l,iBias_Lapse2  ].Bias[0:mz] - $
        Component[l,iBias_Lapse   ].Bias[0:mz] - $
        Component[l,iBias_CLW     ].Bias[0:mz] - $
        Component[l,iBias_NewFix  ].Bias[0:mz]      ; <---***
        
    PLOT, x, y, $
          TITLE = Sat_ID+': O-G(New BC). Ch. ' + STRTRIM( Component[l,iBias_NBC].Instrument_Channel, 2 ), $
          XTITLE = 'FOV position', $
          YTITLE = 'Bias (K)', $
          YRANGE = [ Min_Bias, Max_Bias ], $
          /YNOZERO, PSYM = PSym, CHARSIZE=CharSize

    yavg = MEAN( y )
    OPLOT, !X.CRANGE, [yavg, yavg],COLOR=AvgColor

    OPLOT, !X.CRANGE, [0,0], LINESTYLE = 2



    IF ( !D.NAME NE 'PS' ) THEN BEGIN
      q = GET_KBRD(1)
      IF ( STRUPCASE(q) EQ 'Q' ) THEN GOTO, Done
    ENDIF

  ENDFOR

  DONE: 

  !P = psave

END
