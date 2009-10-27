;+
; NAME:
;   PHUNWRAP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Unwrap phase jumps to recover cycle counts
;
; MAJOR TOPICS:
;   Mathematics
;
; CALLING SEQUENCE:
;   CYCLES = PHUNWRAP(PHASE, TOLERANCE=, MAXVAL=)
;
; DESCRIPTION:
;
;   PHUNWRAP unwraps a sequence of phases to produce a new series of
;   cycle counts.  Phase jumps due to crossing over the PHASE=0
;   boundary are removed by adding an integral number of cycles.  The
;   algorithm is based on the MATLAB "unwrap" function.
;
;   NOTE: the unwrapping process can be ambiguous if there is a phase
;   jump of more than a half cycle in the series.  For example, if the
;   phase changes by ~0.5 cycles, it is not possible to distinguish
;   whether there wasa +0.5 cycle or -0.5 cycle jump.  The most
;   accurate unwrapping can be performed if the PHASE series is nearly
;   continuous and does not have rapid phase changes.
;
;   Users can select the tolerance used to determine the phase jump.
;   Users can also select the definition of "1 cycle" by changing
;   MAXVAL.  By default, MAXVAL is 2*!DPI, which correspondes to 1
;   cycle = 2*!DPI radians, but other values of 1 (cycle), or 360
;   (degrees) are possible.
;
; INPUTS:
;
;   PHASE - phase series to be unwrapped.  Values should range from 0
;           to MAXVAL.  The ordering of the series is important.
;
; RETURNS:
;
;   A new series, expressed in cycles, with cycle jumps larger than
;   TOLERANCE removed.
;
; OPTIONAL KEYWORDS:
;
;   TOLERANCE - phase jump tolerance.  If the phase from one sample to
;               the next changes by more than TOLERANCE, then a single
;               cycle jump is assumed to have occurred.
;               DEFAULT: 0.5*MAXVAL
;
;   MAXVAL - Maximum value for phase. Common values are: 2*!DPI
;            (radians; DEFAULT); 1 (cycle); 360 (degrees), but any
;            positive value may be used.
;
; EXAMPLE:
;
;  ;; Set up some fake data
;  x = dindgen(100)/10d
;  y = x/2
;  ph = y MOD 1.0    ;; Mock phases
;
;  cycles = phunwrap(ph, maxval=1)
;
; MODIFICATION HISTORY:
;   Written and documented, CM, July 2003
;   Handle the case of unsigned integer input, CM, Feb 2006
;
;  $Id$
;
;-
; Copyright (C) 2003, 2006, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

function phunwrap, ph, tolerance=tol0, maxval=maxval0

  common phunwrap_common, idlver
  if n_elements(idlver) EQ 0 then begin
      idlver = !version.release
  endif


  if n_elements(maxval0) EQ 0 then maxval = 2d*!dpi else maxval = maxval0(0)
  if n_elements(tol0) EQ 0 then tol = 0.5*maxval else tol = tol0(0)*maxval

  if n_elements(ph) LT 2 then return, ph

  sz = size(ph)
  tp = sz(sz(0)+1)

  ;; First order difference 
  case tp of 
      12: dph = [0, long(ph)-long(ph(1:*))]
      13: dph = [0, long64(ph)-long64(ph(1:*))]
      15: dph = [0, long64(ph)-long64(ph(1:*))]
      else: dph = [0, ph - ph(1:*)]
  endcase
  
  p = maxval * (fix((dph GT tol) EQ 1) - fix((dph LT (-tol)) EQ 1))
  if idlver GT 5.25 then begin
      ;; Use built-in version if available
      r = total(p, /cumulative) 
  endif else begin
      ;; .. if not, then use the lame FOR loop
      r = p
      for i = 1L, n_elements(r)-1 do $
        r(i) = r(i) + r(i-1)
  endelse

  return, ph+r
end
