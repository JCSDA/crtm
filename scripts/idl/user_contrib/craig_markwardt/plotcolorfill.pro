;+
; NAME:
;   PLOTCOLORFILL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Plots colorful bar charts
;
; CALLING SEQUENCE:
;   PLOTCOLORFILL, x, y, COLOR=col, BOTTOM=bot, WIDTH=wid, ...
;
; DESCRIPTION: 
;
;   PLOTCOLORFILL plots a colorful vertical bar chart.  This may be
;   useful in cases where two dimensions of information need to be
;   conveyed in one plot.  [ I use it to show total intensity as a
;   function of time on the vertical axis, and temperature is coded
;   with color. ]
;
;   Most aspects of the bars are configurable.  The color is specified
;   by an array of colors, one for each bar.  [ Alternatively, a
;   single color for the entire plot can be given. ] Also, one color
;   can be designated as transparent.
;
;   Stacked bar charts can be constructed using two calls to
;   PLOTCOLORFILL.  See the example.
;
; INPUTS:
;
;   X, Y - Two arrays which give the X and Y position of the points.
;          In this style of plot, the x values should be monotonically
;          increasing, but not necessarily monospaced (see WIDTH).
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   COLOR - an array giving the color of each bar, or alternatively a
;           scalar color for all of the bars.  The current color table
;           is not changed.  Default is color "1"
;
;   BOTTOM - normally the bottom of the bars is set to be zero.  You
;            may either specify a scalar bottom value for all of the
;            bars, or an array giving the bottom of each bar
;            individually.  See the example to see how stacked bar
;            charts can be constructed with this keyword.
;
;   WIDTH - sets the width of each bar, globally or individually.
;           Bars are centered on the "X" value, and extend 0.5 * WIDTH
;           to either side.  Default is to assume monospacing, using
;           the separation between the first two x values.  If only
;           one data value is present, then a width of 1 is used.
;
;   MIDPOINT - if set, then ignore the WIDTH keyword value, and
;              instead construct bin edges which lie at the midpoints
;              between data points.  This is usually the most
;              straightforward way to connect irregularly sampled
;              points "like a histogram," although at the expense of
;              not having a direct relation between X and the bin
;              centers.
;
;   NOERASE - if set, do not erase an existing plot before rendering
;             colored histogram.  The effect is comparable to "OPLOT",
;             or the OVER keyword to CONTOUR.
;
;   NOTRACE - if set, do not draw a linear trace at the top of the
;             histogram.
;
;   TRANSPARENT - designates a color which is "transparent".  Any bars
;                 with this color are simply not rendered.  Default is
;                 no transparent color.
;
;   PANEL, SUBPANEL - An alternate way to more precisely specify the
;                     plot and annotation positions.  See SUBCELL.
;                     Default is full-screen.
;
;   POSITION - Position of the bar chart in normal coordinates.
;              Overrides position given by PANEL/SUBPANEL.
;
;   Other keywords are passed to the plot command directly.
;           
; OUTPUTS:
;   NONE
;
; PROCEDURE:
;
; EXAMPLE:
;
;   Stacked barcharts can be constructed by first making one chart
;   with a flat bottom, and then a second chart whose bottom is top of
;   the first.
;
;   x = findgen(30)
;   y1 = x^2
;   y2 = 400.-x
;   c1 = bindgen(30)*3+1b
;   c2 = 100b-bindgen(30)*3+1b
;   plotcolorfill, x, y1,    color=c1, bottom=0.
;   plotcolorfill, x, y1+y2, color=c2, bottom=y1, /noerase
;
; SEE ALSO:
;
;   PLOTPAN
;
; EXTERNAL SUBROUTINES:
;
;   SUBCELL, DEFSUBCELL, PLOTPAN
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Added NOERASE, NOTRACE and MIDPOINT keywords, CM 11 Feb 2000
;   Logarithmic plots now work; so does the THICK keyword, CM 02 Apr
;     2001
;   Optimize drawing when the bin is zero, CM 04 Apr 2001
;   Try to handle YRANGE more properly, since there seem to be some
;     cases where the overlayed axes were erroneous, CM 15 Mar 2002
;   This time YRANGE tweaking with PANELs, CM 13 Jun 2002
;
;  $Id$
;
;-
; Copyright (C) 1997-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
;%insert HERE
;%include defsubcell.pro
;%include plotpan.pro
pro plotcolorfill, x0, y0, color=col, bottom=bot, width=width, $
                   transparent=transparent, edge=edge, thick=thick, $
                   subpanel=subpanel, panel=panel, xlog=xlog, ylog=ylog, $
                   position=position, xstyle=xstyle, ystyle=ystyle, $
                   noerase=over, midpoint=midpoint, notrace=notrace, $
                   _EXTRA=extra


  if n_params() EQ 0 then begin
      message, 'USAGE: PLOTCOLORFILL, X, Y, WIDTH=, BOTTOM=', /info
      return
  endif

  ;; Account for a single "Y" value
  if n_params() EQ 1 then begin
      x = dindgen(n_elements(x0))
      y = x0
  endif else begin
      x = x0
      y = y0
  endelse

  ;; Set default values
  if n_elements(col) EQ 0 then col = byte(y)*0b+1b
  if n_elements(col) EQ 1 then col = byte(y)*0b+col(0)
  if n_elements(xstyle) EQ 0 then xstyle = 0
  if n_elements(ystyle) EQ 0 then ystyle = 0
  if n_elements(transparent) EQ 0 then transparent = -1L

  numx = n_elements(x)
  numy = n_elements(y)
  nump = numx < numy
  xtop = fltarr(2, nump)
  if numx LE 0 OR numy LE 0 then begin
      message, 'ERROR: X and Y must contain at least one data point'
      return
  endif
  if keyword_set(midpoint) then begin
      if n_elements(width) EQ 0 then width = 1
      if nump EQ 1 then xtop(*) = x(0)+width(0)*[-0.5,0.5] $
      else begin
          xtop(0,1:*) = 0.5*(x(1:nump-1)+x(0:nump-2))
          xtop(1,0:nump-2) = xtop(0,1:*)
          xtop(0,0)      = 2*x(0)      - xtop(1,0)
          xtop(1,nump-1) = 2*x(nump-1) - xtop(0,nump-1)
      endelse
  endif else if keyword_set(edge) then begin
      if n_elements(x) NE numy+1 then begin
          message, 'ERROR: X must contain one more element than Y'
          return
      endif
      xtop(0,*) = x(0:nump-1)
      xtop(1,*) = x(1:nump)
  endif else begin
      if n_elements(x) EQ 1 AND n_elements(width) EQ 0 then width = x(0)*0+1
      if n_elements(width) EQ 0 then width = (x(1)-x(0))
      if n_elements(width) EQ 1 then width = width(0)
      if n_elements(width) GT 1 AND n_elements(width) LT nump then begin
          message, 'ERROR: WIDTH must be the same size as X & Y (or be scalar)'
          return
      endif
      if n_elements(pixcenter) EQ 0 then pixcenter = 0.5
      xtop(0,*) = x(0:nump-1) - width*pixcenter
      xtop(1,*) = x(0:nump-1) + width*pixcenter
  endelse

  ;; Plot coordinate grid first
  xrange1 = [min(x), max(x)]
  yrange1 = [min(y), max(y)]
  if NOT keyword_set(over) then begin
  if n_elements(panel) EQ 0 AND n_elements(subpanel) EQ 0 then begin
      if n_elements(position) GT 0 then $
        extra = create_struct(extra, 'POSITION', position)
      plot, xrange1, yrange1, /nodata, xrange=xrange1, yrange=yrange1, $
        xstyle=xstyle(0) OR 4, ystyle=ystyle(0) OR 4, $
        xlog=xlog, ylog=ylog, _EXTRA=extra
  endif else begin
      ;; Set panel size
      if n_elements(panel) EQ 0 then panel=[0.0,0.0,1.0,1.0]
      if n_elements(subpanel) EQ 0 then subpanel = [-1., -1., -1., -1. ]
      subpanel = defsubcell(subpanel)
      plotpan, xrange1, yrange1, xrange=xrange1, yrange=yrange1, /nodata, $
        panel=panel, subpanel=subpanel, xlog=xlog, ylog=ylog, $
        xstyle=xstyle(0) OR 4, ystyle=ystyle(0) OR 4, _EXTRA=extra
  endelse
  endif

  xrange = !x.crange
  yrange = !y.crange
  if keyword_set(xlog) then xrange = 10d^xrange
  if keyword_set(ylog) then yrange = 10d^yrange

  if n_elements(bot) EQ 0 then bot = y * 0. + yrange(0)
  if n_elements(bot) EQ 1 then bot = y * 0. + bot(0)
  ytop = rebin(reform(y(0:nump-1),1,nump),2,nump)

  minc = min(col, max=maxc)
  if minc EQ maxc then begin
      ;; Optimize for case of all the same color
      bbot = rebin(reform(bot(0:nump-1),1,nump),2,nump)
      polyfill, [xtop(*), reverse(xtop(*))], [ytop(*), reverse(bbot(*))], $
        color=col(0), /data, noclip=0
  endif else begin        
      ;; Loop through and draw filled rectangles
      for i = 0, nump-1 do begin
          ;; The vertical size is given by "bot" and "y"
          if xtop(1,i) GE xrange(0) AND xtop(0,i) LE xrange(1) $
            AND y(i) NE bot(i) AND long(col(i)) NE transparent then begin
              polyfill, [xtop(0,i), xtop(1,i), xtop(1,i), xtop(0,i)], $
                [bot(i), bot(i), y(i), y(i)] , $
                color=col(i), /data, noclip=0
          endif
      endfor
  endelse

  ;; Overlay the coordinate grid again in case it got partially wiped.
  if NOT keyword_set(over) then begin
      xwindow = !x.window & ywindow = !y.window
      position = [xwindow(0), ywindow(0), xwindow(1), ywindow(1)]
      xrange2 = !x.crange & yrange2 = !y.crange
      plot, xrange2, yrange2, /nodata, /noerase, $
        xrange=xrange2, yrange=yrange2, position=position, $
        xstyle=xstyle(0) OR 1, ystyle=ystyle(0) OR 1, $
        xlog=xlog, ylog=ylog, _EXTRA=extra
  endif

  ;; Finally, overlay the trace at the top of the curve.
  if NOT keyword_set(notrace) then begin
      oplot, xtop, ytop, thick=thick
  endif

  return
end


