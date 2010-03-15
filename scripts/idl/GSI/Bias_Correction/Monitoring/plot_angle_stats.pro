PRO Plot_Angle_Stats, Instrument_Tag, $
                      Path = Path, $
                      Region = Region, $
                      Component = Component, $
                      Channel_List = Channel_List, $
                      nXplots = nXplots, $
                      nYplots = nYplots, $
                      CharSize = CharSize, $
                      Xmargin = Xmargin, $
                      Ymargin = Ymargin, $
                      _EXTRA = Extra


  ;#------------------------------------------------------------------------------#
  ;#                         -- SET UP ERROR HANDLER --                           #  
  ;#------------------------------------------------------------------------------#

  @error_codes

  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
  ENDIF


  ;#------------------------------------------------------------------------------#
  ;#                             -- INCLUDE FILES --                              # 
  ;#------------------------------------------------------------------------------#

  @monitoring_parameters


  ;#------------------------------------------------------------------------------#
  ;#                              -- CHECK INPUT --                               #
  ;#------------------------------------------------------------------------------#
 
  n_Arguments = 1
  IF ( N_PARAMS() LT n_Arguments ) THEN $
    MESSAGE, 'Invalid number of arguments.', $
             /NONAME, /NOPRINT
 
  IF ( NOT Valid_String( Instrument_Tag ) ) THEN $
    MESSAGE, 'Input Instrument_Tag argument not defined!', $
             /NONAME, /NOPRINT

  IF ( N_ELEMENTS( Path ) NE 0 ) THEN BEGIN
    IF ( NOT Valid_String( Path ) ) THEN $
      MESSAGE, 'Input Path keyword argument not defined!', $
               /NONAME, /NOPRINT
  ENDIF ELSE BEGIN
    Path = './'
  ENDELSE

  IF ( N_ELEMENTS( Region ) EQ 0 ) THEN $
    Region = GLOBAL $
  ELSE BEGIN
    IF ( Region LT 0 OR Region GT (N_REGIONS-1L) ) THEN $
      Region = GLOBAL
  ENDELSE

  IF ( N_ELEMENTS( Component ) EQ 0 ) THEN $
    Component = COUNT_ANGLE $
  ELSE BEGIN
    IF ( Component LT 0 OR Component GT (N_ANGLE_COMPONENTS-1L) ) THEN $
      Component = COUNT_ANGLE
  ENDELSE

  IF ( N_ELEMENTS( nXplots ) EQ 0 ) THEN $
    nXplots = DEFAULT_NXPLOTS

  IF ( N_ELEMENTS( nYplots ) EQ 0 ) THEN $
    nYplots = DEFAULT_NYPLOTS

  IF ( N_ELEMENTS( Charsize ) NE 1 ) THEN $
    CharSize = !P.CHARSIZE
  IF ( CharSize EQ 0.0 ) THEN $
    CharSize = 1.0

  IF ( N_ELEMENTS( Xmargin ) NE 2 ) THEN $
    Xmargin = DEFAULT_XMARGIN

  IF ( N_ELEMENTS( Ymargin ) NE 2 ) THEN $
    Ymargin = DEFAULT_YMARGIN


  ;#------------------------------------------------------------------------------#
  ;#                         -- CHECK PLOTTING DEVICE --                          #
  ;#------------------------------------------------------------------------------#

  IF( !D.NAME EQ 'PS' ) THEN BEGIN
    Font  = 1
    Thick = 3
    CharSize_Scale = 1.1
    Thick_Scale = 1.0
  ENDIF ELSE BEGIN
    Font  = -1
    Thick = 1
    CharSize_Scale = 1.0
    Thick_Scale = 0.5
  ENDELSE


  ;#------------------------------------------------------------------------------#
  ;#           -- GET THE FILENAMES AND EXTRACT THE LAST DATE AND TIME --         #
  ;#------------------------------------------------------------------------------#

  Files = FINDFILE( Path+Instrument_Tag+'.*.ieee_d', COUNT = FileCount )

  DateTime = (STRSPLIT( Files[FileCount-1L], '.', /EXTRACT ))[2]
  Year  = DOUBLE( STRMID( DateTime, 0, 4 ) )
  Month = DOUBLE( STRMID( DateTime, 4, 2 ) )
  Day   = DOUBLE( STRMID( DateTime, 6, 2 ) )
  Hour  = DOUBLE( STRMID( DateTime, 8, 2 ) )
  DateTime = STRING( Hour, Day, MONTH_NAME[Month-1], Year, $
                     FORMAT = '(i2.2, "Z, ", i2, a, i4 )' )
 

  ;#------------------------------------------------------------------------------#
  ;#                              -- READ THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  ; -----------------------------------
  ; Get the number of FOVs and channels
  ; -----------------------------------

  Inst_Index = WHERE( INST_TAG EQ Instrument_Tag, Count )
  IF ( Count NE 1 ) THEN $
    MESSAGE, 'Single file for ' + Instrument_Tag + ' not found.', $
             /NONAME, /NOPRINT

  n_FOVs     = INST_N_FOVS[ Inst_Index[0] ]
  n_Channels = INST_N_CHANNELS[ Inst_Index[0] ]

  Result = Read_Angle_Stats( Path+Instrument_Tag+'.ieee_d', $
                             n_FOVS, n_Channels, $
                             Angle, _EXTRA = Extra )
  IF ( Result NE SUCCESS ) THEN $
    MESSAGE, 'Error reading angle bias data file ' + Path+Instrument_Tag+'.ieee_d', $
             /NONAME, /NOPRINT


  ; -----------------------------------------------------------------
  ; Check the Channel_List keyword now the channels have been defined
  ; -----------------------------------------------------------------

  IF ( N_ELEMENTS( Channel_List ) EQ 0 ) THEN $
    Channel_List = LINDGEN( n_Channels ) + 1L



  ;#------------------------------------------------------------------------------#
  ;#                              -- PLOT THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  ; -------------------
  ; Set up for plotting
  ; -------------------

  ; -- The plotting region widths in normal coords
  dXplots = (MAXX-MINX) / DOUBLE( nXplots )
  dYplots = (MAXY-MINY) / DOUBLE( nYplots )

  ; -- The character size in normal coords
  Xcharsize = Character_Size[0] * CharSize
  Ycharsize = Character_Size[1] * CharSize

  ; -- Initialise the plotting locations
  nX = 0
  nY = 0

  ; -- Set the plotting screen/page to erase up front
  NoErase = 0

  ; -- Create the view angle array
  x = INST_FOV_ANGLE_START[ Inst_Index[0] ] + $
      ( FINDGEN( n_FOVS ) * INST_FOV_ANGLE_STEP[ Inst_Index[0] ] )


  ; ------------------------------
  ; Extract the data to be plotted
  ; ------------------------------

  IF ( Component GT PENALTY_ANGLE ) THEN BEGIN
    Stat_Plot = 1  ; Mean and StdDev
    y = REFORM( Angle.(Component+2)[*,*,Region,*,*] )
  ENDIF ELSE BEGIN
    Stat_Plot = 0  ; No Mean and StdDev
    y = REFORM( Angle.(Component+2)[*,*,Region,*] )
  ENDELSE


  ; -----------------------
  ; Begin channel plot loop
  ; -----------------------

  FOR l = 0L, n_Channels-1L DO BEGIN


    ; ----------------------------
    ; Only plot requested channels
    ; ----------------------------

    Loc = WHERE( Channel_List EQ (l+1), nMatch )
    IF ( nMatch EQ 0 ) THEN CONTINUE


    ; ---------------------------------------------
    ; Write plot information at the window/page top
    ; ---------------------------------------------

    IF ( NoErase EQ 0 ) THEN BEGIN

      ; -- Clear the window/page, and set the flag for continuing plots
      ERASE
      NoErase = 1
  
      ; -- Output the plot header/title
      XYOUTS, XPOS_PLOT_HEADER, $
              YPOS_PLOT_HEADER, $
              'Platform: ' + INST_NAME[Inst_Index[0]] + '!C' + $
              'Region:   ' + REGION_NAME[Region] + '!C' + $
              'Variable: ' + ANGLE_COMPONENT_NAME[Component] + '!C' + $
              'Valid:    ' + DateTime, $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * TITLE_CHARSIZE * CharSize_Scale, $
              FONT = Font

      ; -- Output a legend
      mylegend, 0.65, 0.98, $
                ANGLE_TIME_NAME[I_ANGLE] + ' AVG', $
                COLOR = MEAN_ANGLE_COLOR[I_ANGLE], $
                LINESTYLE = MEAN_ANGLE_LINESTYLE[I_ANGLE], $
                THICK = MEAN_ANGLE_THICK[I_ANGLE] * Thick_Scale, $
                FONT = Font, $
                CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
                /NORMAL

      IF ( Stat_Plot EQ 1 ) THEN $
        mylegend, 0.8, 0.98, $
                  ANGLE_TIME_NAME[I_ANGLE] + ' SDV', $
                  COLOR = STDDEV_ANGLE_COLOR[I_ANGLE], $
                  LINESTYLE = STDDEV_ANGLE_LINESTYLE[I_ANGLE], $
                  THICK = STDDEV_ANGLE_THICK[I_ANGLE] * Thick_Scale, $
                  FONT = Font, $
                  CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
                  /NORMAL

    ENDIF


    ; -----------------------------------
    ; Determine the plot position vectors
    ; -----------------------------------

    x0 = MINX + ( DOUBLE(  nX  ) * dXplots ) + ( Xmargin[0]*Xcharsize )
    x1 = MINX + ( DOUBLE( nX+1 ) * dXplots ) - ( Xmargin[1]*Xcharsize )

    y0B = ( MAXY - DOUBLE( nY+1 ) * dYplots ) + ( Ymargin[0]*Ycharsize )
    y1T = ( MAXY - DOUBLE(  nY  ) * dYplots ) - ( Ymargin[1]*Ycharsize )

    y0T = y0B + (POINT5*( y1T-y0B )) + (POINT5*Ycharsize)
    y1B = y0B + (POINT5*( y1T-y0B )) - (POINT5*Ycharsize)

    PositionT = [ x0, y0T, x1, y1T ]
    PositionB = [ x0, y0B, x1, y1B ]

    Position = [ x0, y0B, x1, y1T ]


    ; -------------------
    ; Plot the STATISTICS
    ; -------------------

    IF ( Stat_Plot EQ 1 ) THEN BEGIN

      ; -- Plot the mean values
      yRange = [ MIN( y[*,l,MEAN_STAT,*] ), MAX( y[*,l,MEAN_STAT,*] ) ]

      PLOT, x, y[*,l,MEAN_STAT,0], $
            TITLE      = 'Channel ' + STRTRIM( l+1, 2 ), $
            YTICKLEN   = 1.0, $
            YGRIDSTYLE = 1, $
            XSTYLE     = 4+1, $  ; Suppress axis + force exact axis range
            YMINOR     = -1, $   ; No minor tickmarks
            YRANGE     = yRange, $
            YTITLE     = 'Avg dT (K)', $
            POSITION   = PositionT, $
            NOERASE    = NoErase, $
            FONT       = Font, $
            THICK      = Thick, $
            CHARSIZE   = CharSize * CharSize_Scale, $
            /NODATA

      AXIS, XAXIS       = 0, $ ; Bottom X-axis
            XRANGE      = !X.CRANGE, $
            XSTYLE      = 1, $
            XTICKFORMAT = '(A1)', $  ; No tickmark labels
            XTICKLEN    = 1.0, $
            XGRIDSTYLE  = 1, $
            FONT        = Font

      AXIS, XAXIS       = 1, $ ; Top X-axis
            XRANGE      = !X.CRANGE, $
            XSTYLE      = 1, $
            XTICKFORMAT = '(A1)', $  ; No tickmark labels
            FONT        = Font

      OPLOT, !X.CRANGE, [0,0], $
             THICK = 3, $
             LINESTYLE = 2
      FOR iUse = 0L, N_ELEMENTS( I_ANGLE )-1L DO BEGIN
        i = I_ANGLE[iUse]
        OPLOT, x, y[*,l,MEAN_STAT,i], $
               COLOR = MEAN_ANGLE_COLOR[i], $
               THICK = MEAN_ANGLE_THICK[i] * Thick_Scale, $
               LINESTYLE = MEAN_ANGLE_LINESTYLE[i]
      ENDFOR

      ; -- Write the average mean for the 30-day period
      Avg_Mean = MEAN( y[*,l,MEAN_STAT,2], /DOUBLE )

      XYOUTS, LABEL_XPOS, y0T+yCharsize, $
              'AVG: ' + STRTRIM( STRING( Avg_Mean, FORMAT = Get_FmtString(Avg_Mean) ), 2 ), $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
              FONT = Font, $
              COLOR = MEAN_COLOR


      ; -- Plot the StdDev values
      yRange = [ MIN( y[*,l,STDDEV_STAT,*] ), MAX( y[*,l,STDDEV_STAT,*] ) ]

      PLOT, x, y[*,l,STDDEV_STAT,0], $
            XTITLE      = 'View angle (degrees)', $
            XTICKLEN    = 1.0, $
            XGRIDSTYLE  = 1, $
            XSTYLE      = 1, $ ; Force exact axis range
            YSTYLE      = 4, $ ; Suppress axis
            YRANGE      = yRange, $
            POSITION    = PositionB, $
            NOERASE     = NoErase, $
            FONT        = Font, $
            THICK       = Thick, $
            CHARSIZE    = CharSize * CharSize_Scale, $
            /YNOZERO, $
            /NODATA

      AXIS, YAXIS       = 0, $ ; Left Y-axis
            YRANGE      = !Y.CRANGE, $
            YSTYLE      = 1, $ ; Force exact axis range
            YTICKFORMAT = '(A1)', $  ; No tickmark labels
            YTICKLEN    = 1.0, $
            YGRIDSTYLE  = 1, $
            YMINOR      = -1, $ ; No minor tickmarks
            FONT        = Font

      AXIS, YAXIS  = 1, $
            YTITLE = 'StdDev dT (K)', $
            YRANGE = !Y.CRANGE, $
            YSTYLE = 1, $ ; Force exact axis range
            YMINOR = -1, $ ; No minor tickmarks
            FONT   = Font

      FOR iUse = 0L, N_ELEMENTS( I_ANGLE )-1L DO BEGIN
        i = I_ANGLE[iUse]
        OPLOT, x, y[*,l,STDDEV_STAT,i], $
               COLOR = STDDEV_ANGLE_COLOR[i], $
               THICK = STDDEV_ANGLE_THICK[i] * Thick_Scale, $
               LINESTYLE = STDDEV_ANGLE_LINESTYLE[i]
      ENDFOR

      ; -- Write the average stddev for the whole period
      Avg_StdDev = MEAN( y[*,l,STDDEV_STAT,2], /DOUBLE )

      XYOUTS, LABEL_XPOS, y1B-yCharsize, $
              'SDV: ' + STRTRIM( STRING( Avg_StdDev, FORMAT = Get_FmtString(Avg_StdDev) ), 2 ), $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
              FONT = Font, $
              COLOR = STDDEV_COLOR


    ENDIF ELSE BEGIN

      ; -- Plot the count or penalty values
      yRange = [ MIN( y[*,l,*] ), MAX( y[*,l,*] ) ]

      PLOT, x, y[*,l,0], $
            TITLE      = 'Channel ' + STRTRIM( l+1, 2 ), $
            XTITLE     = 'View angle (degrees)', $
            XTICKLEN   = 1.0, $
            XGRIDSTYLE = 1, $
            XSTYLE     = 1, $  ; Force exact axis range
            YTICKLEN   = 1.0, $
            YGRIDSTYLE = 1, $
            YMINOR     = -1, $   ; No minor tickmarks
            YRANGE     = yRange, $
            POSITION   = Position, $
            NOERASE    = NoErase, $
            FONT       = Font, $
            THICK      = Thick, $
            CHARSIZE   = CharSize * CharSize_Scale, $
            /NODATA

      FOR iUse = 0L, N_ELEMENTS( I_ANGLE )-1L DO BEGIN
        i = I_ANGLE[iUse]
        OPLOT, x, y[*,l,i], $
               COLOR = MEAN_ANGLE_COLOR[i], $
               THICK = MEAN_ANGLE_THICK[i] * Thick_Scale, $
               LINESTYLE = MEAN_ANGLE_LINESTYLE[i]
      ENDFOR

      ; -- Write the mean for the 30-day period
      Avg_Mean = MEAN( y[*,l,2], /DOUBLE )

      XYOUTS, LABEL_XPOS, y0T+yCharsize, $
              'AVG: ' + STRTRIM( STRING( Avg_Mean, FORMAT = Get_FmtString(Avg_Mean) ), 2 ), $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
              FONT = Font, $
              COLOR = MEAN_COLOR

      ; -- Write the average stddev for the whole period
      Avg_StdDev = STDDEV( y[*,l,2], /DOUBLE )

      XYOUTS, LABEL_XPOS, y1B-yCharsize, $
              'SDV: ' + STRTRIM( STRING( Avg_StdDev, FORMAT = Get_FmtString(Avg_StdDev) ), 2 ), $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
              FONT = Font, $
              COLOR = STDDEV_COLOR
    ENDELSE


    ; ----------------------------
    ; Update the plotting position
    ; ----------------------------

    nX = nX + 1
    IF ( nX EQ nXplots ) THEN BEGIN
      nX = 0
      nY = nY + 1
      IF ( nY EQ nYplots ) THEN BEGIN
        nY = 0
        NoErase = 0
        IF ( !D.NAME NE 'PS' ) THEN BEGIN
          PRINT, 'Press <ENTER> to continue...'
          q = GET_KBRD(1)
        ENDIF
      ENDIF
    ENDIF

  ENDFOR


  ;#------------------------------------------------------------------------------#
  ;#                                  -- DONE --                                  # 
  ;#------------------------------------------------------------------------------#

  CATCH, /CANCEL

END ; PRO Plot_Angle_Stats
