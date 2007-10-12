;+
;
; (C) Cooperative Institute for Meteorological Satellite Studies, 1999
;
; NAME:
;       plot3d
;
; PURPOSE:
;       Procedure to plot vector data in 3-dimensions.
;
; CATEGORY:
;       Graphics
;
; LANGUAGE:
;       IDL v5
;
; CALLING SEQUENCE:
;       plot3d, x, y, z
;            title = title, $
;            xlog = xlog, ylog = ylog, zlog = zlog, $
;            xrange = xrange, yrange = yrange, zrange = zrange, $
;            xstyle = xstyle, ystyle = ystyle, zstyle = zstyle, $
;            xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, $
;            xyproject = xyproject, xzproject = xzproject, yzproject = yzproject, $
;            allproject = allproject, $
;            drawgrid = drawgrid, gridstyle = gridstyle, $
;            psym = psym, symsize = symsize, thick = thick, linestyle = linestyle, $
;            color = color, project_color = project_color, $
;            oplot = oplot
;
; INPUTS:
;       x,y,z: Data vectors to plot in 3-dimensions.
;
; INPUT KEYWORD PARAMETERS:
;       title:             Set this keyword to a string containing the plot title.
;       [xyz]log:          Set this keyword to specify a logarithmic axis.
;       [xyz]range:  .     Set this keyword to the desired data range of the axis
;       [xyz]style:        This keyword allows specification of axis options. See
;                          PLOT command documentation.
;       [xyz]title:        Set this keyword to the title for the specified axis.
;       [xy,xz,yz]project: Set these keywords to plot 2-D projections of the
;                          3-D plot on the axis planes.
;       allproject:        Equivalent to setting all [xy,xz,yz]project keywords.
;       drawgrid:          Set this keyword to draw gridlines on the 2-D axis planes.
;       gridstyle:         Set this keyword to the index of the linestyle to be used
;                          for grids.
;       psym:              Set this keyword to the index of the symbol type to be used
;                          to mark each data point. If the value is negative, both
;                          symbols *and* lines are drawn.
;       symsize:           Set this keyword to the symbol size to be used when PSYM
;                          is set.
;       thick:             Set this keyword to specify the data plot line thickness.
;                          This overrides the setting of !P.THICK.
;       linestyle:         Set this keyword to the index of the linestyle to be used
;                          for the data plots (and projections).
;       color:             Set this keyword to the color table index of the data to be
;                          drawn.
;       project_color:     Set this keyword to the color table index of the 2-D
;                          data projections to be drawn.
;       oplot:             Set this keyword to overplot data on an existing 3-D plot.
;
; OUTPUTS:
;       None.
;
; OUTPUT KEYWORD PARAMETERS:
;       None.
;
; CALLS:
;       None.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None known
;
; RESTRICTIONS:
;       User has to keep his/her wits about them regarding the setting and use of
;       the 3-D transformation. Weird plots can result if one performs a regular
;       2-D plot between a plot3d and plot3d, /oplot call.
;
; PROCEDURE:
;       The SURFACE command is used to set up the 3-D transformation and the base
;       axes. Other axes are drawn in to complete a 3-sided box. The data is plotted
;       using PLOTS and the current 3-D transform in !P.T.
;
; EXAMPLE:
;       Create a test data set:
;
;         IDL> n=56
;         IDL> x=REPLICATE( 5.0, 10 )
;         IDL> x1 = 5.0 + COS( FINDGEN( 36 ) * 10 * !DTOR ) * 2.0
;         IDL> x = [ x, x1, x ]
;         IDL> y = FINDGEN( n )
;         IDL> z = REPLICATE( 5.0, 10 )
;         IDL> z1 = 5.0 + SIN( FINDGEN( 36 ) * 10 * !DTOR ) * 2.0
;         IDL> z = [ z, z1, z ]
;
;       and plot the data with the various projections turned on (assuming that
;       color index #5 is visible):
;
;         IDL> plot3d, x, y, z, /drawgrid, /xzproject, project_color = 5
;         IDL> plot3d, x, y, z, /drawgrid, /yzproject, project_color = 5
;         IDL> plot3d, x, y, z, /drawgrid, /xyproject, project_color = 5
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, CIMSS/SSEC, 13-Jun-1999
;                       paul.vandelst@ssec.wisc.edu
;
;
;-

PRO plot3d, x, y, z, $
            title = title, $
            xlog = xlog, ylog = ylog, zlog = zlog, $
            xrange = xrange, yrange = yrange, zrange = zrange, $
            xstyle = xstyle, ystyle = ystyle, zstyle = zstyle, $
            xtitle = xtitle, ytitle = ytitle, ztitle = ztitle, $
            xyproject = xyproject, xzproject = xzproject, yzproject = yzproject, $
            allproject = allproject, $
            drawgrid = drawgrid, gridstyle = gridstyle, $
            psym = psym, symsize = symsize, thick = thick, linestyle = linestyle, $
            color = color, project_color = project_color, $
            oplot = oplot, $
            _extra = extra



;------------------------------------------------------------------------------
;                             -- RCS Id keyword --
;------------------------------------------------------------------------------

  rcs_Id = '$Id: plot3d.pro,v 1.1 1999/06/13 21:51:04 paulv Exp $'



;------------------------------------------------------------------------------
;                            -- Check input --
;------------------------------------------------------------------------------

; ----------------------------
; Correct number of arguments?
; ----------------------------

  n_arguments = 3
  IF ( N_PARAMS() LT n_arguments ) THEN BEGIN
    MESSAGE, 'Invalid number of input arguments', /INFO
    RETURN
  ENDIF



; -----------------------------------------
; Check that required arguments are defined
; -----------------------------------------

; -- X data
  n = N_ELEMENTS( x )  
  IF ( n EQ 0 ) THEN BEGIN
    MESSAGE, 'Input X argument not defined!', /INFO
    RETURN
  ENDIF

; -- Y data
  IF ( N_ELEMENTS( y ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Input Y argument not defined!', /INFO
    RETURN
  ENDIF

; -- Z data
  IF ( N_ELEMENTS( z ) EQ 0 ) THEN BEGIN
    MESSAGE, 'Input Z argument not defined!', /INFO
    RETURN
  ENDIF


; --------------------------------
; Check for consistent array sizes
; --------------------------------

  IF ( N_ELEMENTS( y ) NE n OR N_ELEMENTS( z ) NE n ) THEN BEGIN
    MESSAGE, 'Inconsistent X, Y, Z vector sizes.', /INFO
    RETURN
  ENDIF


; --------------------
; Check keyword inputs
; --------------------

; -- Data range keywords
  IF ( NOT KEYWORD_SET( xrange ) ) THEN xrange = [ MIN( x ), MAX( x ) ]
  IF ( NOT KEYWORD_SET( yrange ) ) THEN yrange = [ MIN( y ), MAX( y ) ]
  IF ( NOT KEYWORD_SET( zrange ) ) THEN zrange = [ MIN( z ), MAX( z ) ]

; -- Logarithmic axes?
  IF ( NOT KEYWORD_SET( xlog ) ) THEN xlog = 0 ELSE xlog = 1
  IF ( NOT KEYWORD_SET( ylog ) ) THEN ylog = 0 ELSE ylog = 1
  IF ( NOT KEYWORD_SET( zlog ) ) THEN zlog = 0 ELSE zlog = 1

; -- Grid style
  IF ( NOT KEYWORD_SET( drawgrid ) ) THEN drawgrid = 0 ELSE drawgrid = 1
  IF ( NOT KEYWORD_SET( gridstyle ) ) THEN gridstyle = 1

; -- Plane projections
  IF ( NOT KEYWORD_SET( xyproject ) ) THEN xyproject = 0 ELSE xyproject = 1
  IF ( NOT KEYWORD_SET( xzproject ) ) THEN xzproject = 0 ELSE xzproject = 1
  IF ( NOT KEYWORD_SET( yzproject ) ) THEN yzproject = 0 ELSE yzproject = 1
  IF ( KEYWORD_SET( allproject ) ) THEN BEGIN
    xyproject = 1
    xzproject = 1
    yzproject = 1
  ENDIF
  
; -- Colors
  IF ( NOT KEYWORD_SET( color ) ) THEN color = !P.COLOR
  IF ( NOT KEYWORD_SET( project_color ) ) THEN project_color = !P.COLOR

; -- Overplotting
  IF ( NOT KEYWORD_SET( oplot ) ) THEN oplot = 0 ELSE oplot = 1



;------------------------------------------------------------------------------
;                            -- Is this a 3-D overplot? --
;------------------------------------------------------------------------------

  IF ( oplot EQ 0 ) THEN BEGIN

;   ----------------------------------------------------------------
;   No overplot. This is the first call to plot3d in current window!
;   ----------------------------------------------------------------

;   ------------------
;   Set character size
;   ------------------

    !P.CHARSIZE = 2


;   -----------------------------------------------------
;   Use SURFACE to establish the 3D transform and draw
;   the base X, Y, and Z axes.
;   The IDL documentation was very unclear on how to draw
;   axes (i.e. where to get the co-ordinate points from)
;   if T3D and SCALE3 are used. So, this method will be
;   oh-so-slow for very large n.
;   -----------------------------------------------------

    SURFACE, FLTARR( n, n ), x, y, $
             XLOG      = xlog,   YLOG      = ylog,   ZLOG      = zlog, $
             XRANGE    = xrange, YRANGE    = yrange, ZRANGE    = zrange, $
             XSTYLE    = xstyle, YSTYLE    = ystyle, ZSTYLE    = zstyle, $
             XTITLE    = xtitle, YTITLE    = ytitle, ZTITLE    = ztitle, $
             XTICK_GET = xt,     YTICK_GET = yt,     ZTICK_GET = zt, $
             AZ      = 50, $
             TICKLEN = ticklen, $
             TITLE   = title, $
             /NODATA, $
             /SAVE



;   --------------------------------------------
;   Get the axes extrema for drawing the "other"
;   XYZ axes to complete a 3-sided box
;   --------------------------------------------

;   -- X azis
    IF ( xlog EQ 0 ) THEN BEGIN
      xmin = !X.CRANGE[ 0 ]
      xmax = !X.CRANGE[ 1 ]
    ENDIF ELSE BEGIN
      xmin = 10.0^!X.CRANGE[ 0 ]
      xmax = 10.0^!X.CRANGE[ 1 ]
    ENDELSE

;   -- Y azis
    IF ( ylog EQ 0 ) THEN BEGIN
      ymin = !Y.CRANGE[ 0 ]
      ymax = !Y.CRANGE[ 1 ]
    ENDIF ELSE BEGIN
      ymin = 10.0^!Y.CRANGE[ 0 ]
      ymax = 10.0^!Y.CRANGE[ 1 ]
    ENDELSE

;   -- Z azis
    IF ( zlog EQ 0 ) THEN BEGIN
      zmin = !Z.CRANGE[ 0 ]
      zmax = !Z.CRANGE[ 1 ]
    ENDIF ELSE BEGIN
      zmin = 10.0^!Z.CRANGE[ 0 ]
      zmax = 10.0^!Z.CRANGE[ 1 ]
    ENDELSE


;   --------------------------------------------
;   Draw the "*" axes as shown below:
;
;            ***********
;           **         *
;          * *         *
;         o  *         *
;         o  *         *
;       Z o  **********o
;         o *         o
;         o*         o  X
;         ooooooooooo
;              Y
;
;   The axes designated with a "o" are drawn by
;   the initial call to SURFACE. All others, the
;   "*" axes, are drawn via the AXIS commands
;   that follow.
;   --------------------------------------------

;   -- Dummy axes name
    name = REPLICATE( ' ', 30 )

;   -- Draw the various axes with default ticklength and no names
    AXIS, xmax, ymin, zmin, $
          /YAXIS, /T3D, $
          YLOG = ylog, $
          YTICKNAME = name, YTICKLEN = 0
    AXIS, xmax, ymin, zmin, $
          /ZAXIS, /T3D, $
          ZLOG = zlog, $
          ZTICKNAME = name, ZTICKLEN = 0
    AXIS, xmin, ymax, zmin, $
          /XAXIS, /T3D, $
          XLOG = xlog, $
          XTICKNAME = name, XTICKLEN = 0
    AXIS, xmin, ymax, zmax, $
          /XAXIS, /T3D, $
          XLOG = xlog, $
          XTICKNAME = name, XTICKLEN = 0
    AXIS, xmax, ymax, zmin, $
          /ZAXIS, /T3D, $
          ZLOG = zlog, $
          ZTICKNAME = name, ZTICKLEN = 0
    AXIS, xmax, ymin, zmax, $
          /YAXIS, /T3D, $
          YLOG = ylog, $
          YTICKNAME = name, YTICKLEN = 0
           

;   ------------------------------------------
;   Draw the gridlines on the plot if required
;   ------------------------------------------

    IF ( drawgrid EQ 1 ) THEN BEGIN


;     ------------
;     YZ gridlines
;     ------------

;     -- Vertical
      FOR i = 0, N_ELEMENTS( yt ) - 1 DO $
        PLOTS, [ xmax, xmax ], $
               [ yt[ i ], yt[ i ] ], $
               [ zmin, zmax ], $
               /T3D, LINESTYLE = gridstyle

;     -- Horizontal
      FOR i = 0, N_ELEMENTS( zt ) - 1 DO $
        PLOTS, [ xmax, xmax ], $
               [ ymin, ymax ], $
               [ zt[ i ], zt[ i ] ], $
               /T3D, LINESTYLE = gridstyle


;     ------------
;     XZ gridlines
;     ------------

;     -- Vertical
      FOR i = 0, N_ELEMENTS( xt ) - 1 DO $
        PLOTS, [ xt[ i ], xt[ i ] ], $
               [ ymax, ymax ], $
               [ zmin, zmax ], $
               /T3D, LINESTYLE = gridstyle

;     -- Horizontal
      FOR i = 0, N_ELEMENTS( zt ) - 1 DO $
        PLOTS, [ xmin, xmax ], $
               [ ymax, ymax ], $
               [ zt[ i ], zt[ i ] ], $
               /T3D, LINESTYLE = gridstyle


;     ------------
;     XY gridlines
;     ------------

;     -- Xmin -> Xmax
      FOR i = 0, N_ELEMENTS( yt ) - 1 DO $
        PLOTS, [ xmin, xmax ], $
               [ yt[ i ], yt[ i ] ], $
               [ zmin, zmin ], $
               /T3D, LINESTYLE = gridstyle

;     -- Ymin -> Ymax
      FOR i = 0, N_ELEMENTS( xt ) - 1 DO $
        PLOTS, [ xt[ i ], xt[ i ] ], $
               [ ymin, ymax ], $
               [ zmin, zmin ], $
               /T3D, LINESTYLE = gridstyle

    ENDIF        ; Draw grid

  ENDIF ELSE BEGIN 


;   ----------------------------------------------------------
;   Yes, overplot is requested. Enable use of the 3D transform
;   ----------------------------------------------------------

    !P.T3D = 1

  ENDELSE      ; Over plot



;------------------------------------------------------------------------------
;                            -- Plot the data in 3-D --
;------------------------------------------------------------------------------

; --------------------
; Plot the actual data
; --------------------

  PLOTS, x, y, z, /T3D, $
         PSYM = psym, SYMSIZE = symsize, $
         THICK = thick, COLOR = color, LINESTYLE = linestyle


; --------------------
; Plot the projections
; --------------------

; -- XY projection
  IF ( xyproject EQ 1 ) THEN $
    PLOTS, x, y, FLTARR( n ) + zmin, $
           /T3D, $
           THICK = thick, COLOR = project_color, LINESTYLE = linestyle

; -- XZ projection
  IF ( xzproject EQ 1 ) THEN $
    PLOTS, x, FLTARR( n ) + ymax, z, $
           /T3D, $
           THICK = thick, COLOR = project_color, LINESTYLE = linestyle

; -- YZ projection
  IF ( yzproject EQ 1 ) THEN $
    PLOTS, FLTARR( n ) + xmax, y, z, $
           /T3D, $
           THICK = thick, COLOR = project_color, LINESTYLE = linestyle



;------------------------------------------------------------------------------
;                                 -- Done! --
;------------------------------------------------------------------------------

; ---------------------------
; Turn off 3D transformations
; ---------------------------

  IF ( oplot EQ 1 ) THEN !P.T3D = 0


; ----------------------------
; Set character size to normal
; ----------------------------

  !P.CHARSIZE = 1.0

END

;==============================================================================
; CVS/RCS keyword modification history:
;
; $Log: plot3d.pro,v $
; Revision 1.1  1999/06/13 21:51:04  paulv
; Initial version
;
;
;==============================================================================
