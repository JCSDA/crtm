;+
; NAME:
;   SUBCELL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Finds the position of a subwindow within a reference window.
;
; CALLING SEQUENCE:
;   sub = subcell(panel, refposition)
;
; DESCRIPTION: 
;
;   SUBCELL finds the position of a subwindow within another window.
;   This could be useful in cases where the position of one window is
;   specified relative to another one.
;
;   When plotting, one often wants to describe the position of the
;   plot box with respect to another box on the screen.  In that
;   respect, the reference window can be thought of as a virtual
;   display, and the SUBPOS as virtual a position on that display.
;   The SUBCELL function transforms the relative coordinates of the
;   virtual position back to normal screen coordinates.
;
; INPUTS:
;
;   SUBPOS - A four-element array giving the position of the
;            subwindow, *relative* to a reference window given by
;            POSITION.  Given as [XS1, YS1, XS2, YS2], which describes
;            the lower left and upper right corners of the subwindow.
;            Each value is a number between zero and one, zero being
;            the lower/left and one being the upper/right corners of
;            the reference window.
;
;   POSITION - A four-element array giving the position of the
;              reference window on the screen.  Equivalent to the
;              graphics keyword of the same name.
; 
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   MARGIN - If set, then a default value for SUBPOS is found using
;            the DEFSUBCELL function.
;
; RETURNS:
;   The position of the subwindow, in normal coordinates.
;
; PROCEDURE:
;
; EXAMPLE:
;
;
; SEE ALSO:
;
;   DEFSUBCELL, SUBCELLARRAY
;
; EXTERNAL SUBROUTINES:
;
;   DEFSUBCELL
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

function subcell, subpos, position, margin=margin

  ;; Default value for subposition
  if n_elements(subpos) EQ 0 then mysubpos = [-1.,-1,-1,-1] $
  else mysubpos = subpos

  ;; Default value for position - full screen
  if n_elements(position) EQ 0 then position = [0.,0.,1.,1.]

  ;; Get margins if necessary
  if keyword_set(margin) EQ 1 OR n_elements(subpos) EQ 0 then $
    mysubpos = defsubcell(mysubpos)

  ;; Compute new window position
  x0 = position(0)
  y0 = position(1)
  dx = position(2)-position(0)
  dy = position(3)-position(1)

  newsubpos = reform(mysubpos * 0, 4)
  newsubpos([0,2]) = x0 + dx * mysubpos([0,2])
  newsubpos([1,3]) = y0 + dy * mysubpos([1,3])

  return, newsubpos
end

  
