;+
; NAME:
;   DXCLEAR
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Clears an IDL breakpoint
;
; CALLING SEQUENCE:
;   DXCLEAR, INDEX
;
; DESCRIPTION: 
;
;   DXBREAK is a convenience routine for clearing IDL breakpoints.
;   Its primary benefits are that it is symmetric with DXBREAK, and it
;   requires fewer characters to type.
;
; INPUTS:
;
;   INDEX - the breakpoint number, as listed by HELP, /BREAKPOINT.
;
;
; EXAMPLE:
;
;   dxclear, 0
;
;   Clear breakpoint number 0
;
; SEE ALSO:
;
;   BREAKPOINT, DXBREAK
;
; MODIFICATION HISTORY:
;   Written, 15 Apr 2000
;
;
;  $Id$
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro dxclear, index0
  catch, catcherr
  if catcherr NE 0 then return

  if n_elements(index0) EQ 0 then return
  index = floor(index0(0))
  breakpoint, index, /clear
end
