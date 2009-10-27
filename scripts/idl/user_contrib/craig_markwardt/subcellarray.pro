;+
; NAME:
;   SUBCELLARRAY
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Returns a set of subcells, suitable for creating a matrix of plots.
;
; CALLING SEQUENCE:
;   subcellarray, xdivs, ydivs, newpanels, newsubpanels
;
; DESCRIPTION: 
;
;   SUBCELLARRAY generates a group of subcells.  The subcells are
;   useful for plotting a matrix of windows.
;
;   This procedure takes a list of subdivisions in X and Y,
;   designating the subdivision of the plot into num(X) X panels and
;   num(Y) Y panels, and creates a new batch of panels and subpanels,
;   which can be used in the individual plot commands of the array.
;
; INPUTS:
;
;   XDIVS - list of subdivisions in the X-direction.  Example: [1,1,2]
;           will create three panels in the X-direction such that
;           their sizes are in the ratio of 1:1:2.  These are the
;           subdivisions of the SUBPANEL, the inner plot box boundary,
;           not divisions of the PANEL, which is the boundary that
;           includes axis labels.
;
;   YDIVS - same, for Y direction
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   PANEL - Original panel (outer margin) of plot. Default is to fill
;           screen.
;
;   SUBPANEL - Original subpanel (inner margin) of plot.  Default is
;              to use defsubcell().
;
; OUTPUTS:
;
;   NEWPANELS - output array of panels.  The output is 4 x M x N where
;               M is the number of X divisions and N is the number of
;               Y divisions.
;
;   NEWSUBPANELS - output array of subpanels, with correct adjustment
;                  for margins, same format as NEWPANELS.
;
; PROCEDURE:
;
; EXAMPLE:
;
;   See PLOTCUBE for an example usage.
;
; SEE ALSO:
;
;   DEFSUBCELL, SUBCELLARRAY, PLOTCUBE
;
; EXTERNAL SUBROUTINES:
;
;   DEFSUBCELL, SUBCELL
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

pro subcellarray, xdivs, ydivs, newpanels, newsubpanels, $
                  panel=panel, subpanel=subpanel, $
                  xreverse=xreverse, yreverse=yreverse

  nx = n_elements(xdivs)
  ny = n_elements(ydivs)
  xd = double(xdivs)/total(xdivs)
  yd = double(ydivs)/total(ydivs)

  ;; Create the new panel and subpanel matrices
  newpanels    = dblarr(nx, ny, 4)
  newsubpanels = dblarr(nx, ny, 4) - 1.
  
  if n_elements(panel)    EQ 0 then panel    = [ 0D, 0., 1., 1.]
  if n_elements(subpanel) EQ 0 then subpanel = [-1D,-1.,-1.,-1.]
  
  subpanel1 = defsubcell(subpanel)
  
  xmarg = subpanel1(0)+(1.-subpanel1(2))
  ymarg = subpanel1(1)+(1.-subpanel1(3))
  
  xd = xd * (1. - xmarg)
  yd = yd * (1. - ymarg)

  if NOT keyword_set(xreverse) then begin
      istart = 0L & istop = nx-1 & istep = 1L
  endif else begin
      istart = nx-1 & istop = 0L & istep = -1L
  endelse      
  if NOT keyword_set(yreverse) then begin
      jstart = 0L & jstop = ny-1 & jstep = 1L
  endif else begin
      jstart = ny-1 & jstop = 0L & jstep = -1L
  endelse      

  xstart = 0.D
  for i = istart, istop, istep do begin
      xend = xstart + xd(i)
      spxstart = 0.
      spxend   = 1.

      ;; Special cases for the outer subcells which have a margin.
      ;; The inner ones do not.
      if i EQ 0    then xend = xend + subpanel1(0)
      if i EQ nx-1 then xend = xend + (1.-subpanel1(2))
      if i EQ 0    then spxstart = subpanel1(0)/(xend - xstart)
      if i EQ nx-1 then spxend   = 1. - (1.-subpanel1(2))/(xend-xstart)
      ystart = 0.D
      for j = jstart, jstop, jstep do begin
          yend = ystart + yd(j)
          spystart = 0.
          spyend   = 1.
          ;; Special cases for the outer subcells which have a margin.
          ;; The inner ones do not.
          if j EQ 0    then yend = yend + subpanel1(1)
          if j EQ ny-1 then yend = yend + (1.-subpanel1(3))
          if j EQ 0    then spystart = subpanel1(1)/(yend-ystart)
          if j EQ ny-1 then spyend   = 1. - (1.-subpanel1(3))/(yend-ystart)

          newpanels(i,j,*) = subcell([xstart, ystart, xend, yend], panel)
          newsubpanels(i,j,*) = [spxstart, spystart, spxend, spyend]
          
          ystart = yend
      endfor
      
      xstart = xend
  endfor
  
  return
end

