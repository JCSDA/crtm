;+
; NAME:
;   PLOTPAN
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Same as PLOT command, but respects PANEL and SUBPANEL
;
; CALLING SEQUENCE:
;   PLOTPAN, x, y, ...
;
; DESCRIPTION: 
;
;   PLOTPAN is almost identical to PLOT, except that it accounts for
;   panels and subpanels in the display.  In fact, after a short
;   calculation, PLOTPAN calls PLOT to do its dirty work.
;
;   Once the coordinate grid has been set up by PLOTPAN, other plots
;   can be overlaid by calling OPLOT.
;
; INPUTS:
;
;   X, Y - Two arrays which give the x and y position of each point.
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   PANEL, SUBPANEL - An alternate way to more precisely specify the
;                     plot and annotation positions.  See SUBCELL.
;                     Default is full-screen.  Overridden by POSITION.
;
;   Other options are passed along to the PLOT command directly.
;
; OUTPUTS:
;   NONE
;
; PROCEDURE:
;
; EXAMPLE:
;
; SEE ALSO:
;
;   SUBCELL, DEFSUBCELL, SUBCELLARRAY
;
; EXTERNAL SUBROUTINES:
;
;   PLOT, SUBCELL
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;   Added copyright notice, 25 Mar 2001, CM
;
;  $Id$
;
;-
; Copyright (C) 1997,2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro plotpan, x, y, $
             subpanel=subpanel, panel=panel, $
             _EXTRA=extra

  ;; Default is full-screen
  if n_elements(panel) EQ 0 AND n_elements(subpanel) EQ 0 then begin
      plot, x, y, _EXTRA=extra
  endif else begin
      if n_elements(panel) EQ 0 then panel=[0.0,0.0,1.0,1.0]
      plot, x, y, /normal, position=subcell(subpanel, panel, /marg), $
        _EXTRA=extra
  endelse

  return
end

