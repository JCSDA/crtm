;+
; NAME:
;   PLOTBIN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Overlays a plot in histogram style on the current graphics viewport.
;
; CALLING SEQUENCE:
;   OPLOTBIN, x, y, WIDTH=width, PIXCENTER=pixcenter, LOGCLIP=logclip, ...
;
; DESCRIPTION: 
;
;   OPLOTBIN overlays an unfilled histogram plot on an existing
;   graphics window.  The width of each histogram bin can be specified
;   individually, and the alignment of the bin centers can be given
;   explicitly.
;
;   OPLOTBIN accepts several specialized keyword parameters of its
;   own, but passes any other keywords to the built-in IDL OPLOT
;   procedure.  Thus, any keywords accepted by OPLOT can be passed to
;   OPLOTBIN.
;
; INPUTS:
;
;   X, Y - Two arrays which give the "X" and "Y" position of each bin.
;          If only the Y values are given, then the X values will be
;          the bin numbers.
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   WIDTH - The width of each histogram bin.  If a scalar, then the
;           width is assumed to be the same for all histogram bins.
;           If a vector, then WIDTH should have the same number of
;           elements as X and Y, and specify the width of each
;           individual bin.
;           Default value: width is the separation between the first
;                          two X values.
;
;   PIXCENTER - Describes the alignment of "X" values with respect to
;               the histogram bin centers:
;                   PIXCENTER = 0.0  -- "X" values are left edges of bins
;                             = 0.5  -- "X" values are bin centers
;                             = 1.0  -- "X" values are right edges of bins
;               Intermediate values are also permitted.
;               Default value: 0.5 ("X" values are bin centers)
;
;   MIDPOINT - if set, then ignore the WIDTH and PIXCENTER keyword
;              values, and instead construct bin edges which lie at
;              the midpoints between data points.  This is usually the
;              most straightforward way to connect irregularly sampled
;              points "like a histogram," although at the expense of
;              not having a direct relation between X and the bin
;              centers.
;
;   EDGE - if set, then the X values will be taken to be the bin edges
;          rather than the bin midpoints.  In this case, the number of
;          X values should be one more than the number of Y values.
;
;   PLOTVERT - plot "vertically", that is, X is vertical and Y is
;              horizontal.
;
;   LOGCLIP - If set, then Y values are clipped to the current data
;             viewport.  On a logarithmic scale, this may help some
;             negative bins be seen.  
;             Default: not set.
;
; OUTPUTS:
;   NONE
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Documented, CM, July 1999
;   Added MIDPOINT keyword, 21 Feb 2000
;   Added EDGE keyword, 21 Apr 2000
;   Corrected way that PIXCENTER works, same as PLOTBIN, just one year
;     later (Thanks to J. Guerber), CM, 17 Mar 2003
;   Changed _EXTRA handling to use EXECUTE internally.  Unfortunately
;     makes it incompatible with VM version of IDL, 03 Aug 2003, CM
;   Remove EXECUTE function, move to CALL_PROCEDURE, 23 Nov 2003, CM
;   Add PLOTVERT keyword, 19 Apr 2004, CM
;
;  $Id$
;
;-
; Copyright (C) 1997-2000, 2003, 2004, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro oplotbin, x0, y0, width=width, pixcenter=pixcenter, logclip=logclip, $
              midpoint=midpoint, edge=edge, plotvert=plotvert, $
              _EXTRA=extra

  ;; Account for a single "Y" value
  if n_params() EQ 1 then begin
      x = dindgen(n_elements(x0))
      y = x0
  endif else begin
      x = x0
      y = y0
  endelse

  numx = n_elements(x)
  numy = n_elements(y)
  nump = numx < numy
  if numx LE 0 OR numy LE 0 then begin
      message, 'ERROR: X and Y must contain at least one data point'
      return
  endif
  xtop = fltarr(2, nump)

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
      xtop(1,*) = x(0:nump-1) + width*(1.-pixcenter)
  endelse

  ytop = rebin(reform(y(0:nump-1),1,nump),2,nump)
  if keyword_set(logclip) then begin
      if !y.type EQ 1 then $
        ytop = ytop < 10D^(!y.crange(1)) > 10D^(!y.crange(0))
      if !x.type EQ 1 then $
        xtop = xtop < 10D^(!x.crange(1)) > 10D^(!x.crange(0))
  endif

  ;; Vertical plot: swap X/Y
  if keyword_set(plotvert) then begin
      temp = temporary(xtop)
      xtop = temporary(ytop)
      ytop = temporary(temp)
  endif

  ;; Default is full-screen
  call_procedure, 'oplot', xtop, ytop, _EXTRA=extra

  return
end

