PRO Plot_BCoef_Stats, Instrument_Tag, $
                      Path = Path, $
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

  IF ( N_ELEMENTS( Component ) EQ 0 ) THEN $
    Component = CONSTANT_COEFFICIENT $
  ELSE BEGIN
    IF ( Component LT 0 OR Component GT (N_AIRMASS_COEFFICIENTS-1L) ) THEN $
      Component = CONSTANT_COEFFICIENT
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
  ENDIF ELSE BEGIN
    Font  = -1
    Thick = 1
    CharSize_Scale = 1.0
  ENDELSE


  ;#------------------------------------------------------------------------------#
  ;#            -- GET THE FILENAMES AND EXTRACT THE DATES AND TIMES --           #
  ;#------------------------------------------------------------------------------#

  Files = FINDFILE( Path+Instrument_Tag+'*.ieee_d', COUNT = FileCount )

  DateTime  = STRARR( FileCount )
  JulianDay = DBLARR( FileCount )

  FOR i = 0L, FileCount-1L DO BEGIN
    DateTime[i] = (STRSPLIT( Files[i], '.', /EXTRACT ))[2]
    Year  = DOUBLE( STRMID( DateTime[i], 0, 4 ) )
    Month = DOUBLE( STRMID( DateTime[i], 4, 2 ) )
    Day   = DOUBLE( STRMID( DateTime[i], 6, 2 ) )
    Hour  = DOUBLE( STRMID( DateTime[i], 8, 2 ) )
    DateTime[i] = STRING( Hour, Day, MONTH_NAME[Month-1], Year, $
                          FORMAT = '(i2.2, "Z, ", i2, a, i4 )' )
    JulianDay[i] = JULDAY( Month, Day, Year, Hour )
  ENDFOR
 

  ;#------------------------------------------------------------------------------#
  ;#                              -- READ THE DATA --                             #
  ;#------------------------------------------------------------------------------#

  ; --------------------------
  ; Get the number of channels
  ; --------------------------

  Inst_Index = WHERE( INST_TAG EQ Instrument_Tag, Count )
  IF ( Count NE 1 ) THEN $
    MESSAGE, 'Single file for ' + Instrument_Tag + ' not found.', $
             /NONAME, /NOPRINT

  n_FOVs     = INST_N_FOVS[ Inst_Index[0] ]
  n_Channels = INST_N_CHANNELS[ Inst_Index[0] ]


  ; -----------------------------------------------------------------
  ; Check the Channel_List keyword now the channels have been defined
  ; -----------------------------------------------------------------

  IF ( N_ELEMENTS( Channel_List ) EQ 0 ) THEN $
    Channel_List = LINDGEN( n_Channels ) + 1L


  ; -------------------
  ; Accumulate the data
  ; -------------------

  FOR i = 0L, FileCount-1L DO BEGIN
    Result = Read_BCoef_Stats( Files[i], n_Channels, x, _EXTRA = Extra )
    IF ( i EQ 0 ) THEN $
      BCoef = x $
    ELSE $
      BCoef = [ BCoef, x ]
  ENDFOR


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

  ; -- Set the format for the time x-axis
  Result = LABEL_DATE( DATE_FORMAT='%M %D!C%Y' )


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
              'Variable: ' + COEFFICIENT_NAME[Component] + '!C' + $
              'Valid:    ' + DateTime[0] + ' to ' + DateTime[FileCount-1L], $
              /NORMAL, $
              ALIGNMENT = 0.0, $
              CHARSIZE = CharSize * TITLE_CHARSIZE * CharSize_Scale, $
              FONT = Font
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

    Position = [ x0, y0B, x1, y1T ]


    ; ------------------------
    ; Plot the MEAN statistics
    ; ------------------------

    PLOT, JulianDay, BCoef.Coefficients[l,Component], $
          TITLE       = 'Channel ' + STRTRIM( l+1, 2 ), $
          XTICKFORMAT = 'LABEL_DATE', $
          XTICKLEN    = 1.0, $
          XGRIDSTYLE  = 1, $
          XSTYLE      = 1, $  ; Force exact axis range
          YTICKLEN    = 1.0, $
          YGRIDSTYLE  = 1, $
          YMINOR      = -1, $   ; No minor tickmarks
          POSITION    = Position, $
          NOERASE     = NoErase, $
          FONT        = Font, $
          THICK       = Thick, $
          CHARSIZE    = CharSize * CharSize_Scale, $
          /YNOZERO

    ; -- Write the mean and stddev for the whole period
    Coef_Mean = MEAN( BCoef.Coefficients[l,Component], /DOUBLE )

    XYOUTS, LABEL_XPOS, y0T+yCharsize, $
            'AVG: ' + STRTRIM( STRING( Coef_Mean, FORMAT = Get_FmtString(Coef_Mean) ), 2 ), $
            /NORMAL, $
            ALIGNMENT = 0.0, $
            CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
            FONT = Font, $
            COLOR = MEAN_COLOR

    Coef_StdDev = STDDEV( BCoef.Coefficients[l,Component], /DOUBLE )

    XYOUTS, LABEL_XPOS, y1B-yCharsize, $
            'SDV: ' + STRTRIM( STRING( Coef_StdDev, FORMAT = Get_FmtString(Coef_StdDev) ), 2 ), $
            /NORMAL, $
            ALIGNMENT = 0.0, $
            CHARSIZE = CharSize * LABEL_CHARSIZE * CharSize_Scale, $
            FONT = Font, $
            COLOR = STDDEV_COLOR


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

END ; PRO Plot_BCor_Stats
