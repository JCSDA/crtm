;+
; NAME:
;   DEFSUBCELL
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Returns a default subcell suitable for plotting in.
;
; CALLING SEQUENCE:
;   sub = defsubcell( [default] )
;
; DESCRIPTION: 
;
;   DEFSUBCELL returns a "nice" subcell, useful for plotting in.  It
;   gives 8% margins on the left and bottom, and 5% margins on the
;   right and top.
;
;   A set of user-defined default values can be passed in.  Any that
;   are negative are replaced by this function's.
;
; INPUTS:
;
;   DEFAULT - a "default" subcell.  Any elements that are negative are
;             replaced by DEFSUBCELL's notion of the proper margins.
;             This feature is used, for example, by SUBCELLARRAY to
;             make subcells that have special margins on certain sides
;             and default ones on other sides.
;
; OPTIONAL INPUTS:
;   NONE
;
; INPUT KEYWORD PARAMETERS:
;
;   NONE
;
; RETURNS:
;   The new subcell.
;
; PROCEDURE:
;
; EXAMPLE:
;
; SEE ALSO:
;
;   DEFSUBCELL, SUBCELLARRAY
;
; EXTERNAL SUBROUTINES:
;
; MODIFICATION HISTORY:
;   Written, CM, 1997
;
;-


function defsubcell, default

  if n_elements(default) EQ 0 then default = [-1.,-1,-1,-1]
  mysubcell = default
  defaultsubpos = [ 0.08, 0.08, 0.95, 0.95 ]

  iwh = where(mysubcell LT 0, ict)
  if ict GT 0 then $
    mysubcell(iwh) = defaultsubpos(iwh)

  return, mysubcell
end
