;+
; NAME:
;   VALUE_LOCATE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;
;   Locate one or more values in a reference array (IDL LE 5.2 compatibility)
;
; CALLING SEQUENCE:
;
;   INDICES = VALUE_LOCATE(REF, VALUES)
;
; DESCRIPTION: 
;
;   VALUE_LOCATE locates the positions of given values within a
;   reference array.  The reference array need not be regularly
;   spaced.  This is useful for various searching, sorting and
;   interpolation algorithms.
;
;   The reference array should be a monotonically increasing or
;   decreasing list of values which partition the real numbers.  A
;   reference array of NBINS numbers partitions the real number line
;   into NBINS+1 regions, like so:
;
;
; REF:           X[0]         X[1]   X[2] X[3]     X[NBINS-1]
;      <----------|-------------|------|---|----...---|--------------->
; INDICES:  -1           0          1    2       3        NBINS-1
;
;
;   VALUE_LOCATE returns which partition each of the VALUES falls
;   into, according to the figure above.  For example, a value between
;   X[1] and X[2] would return a value of 1.  Values below X[0] return
;   -1, and above X[NBINS-1] return NBINS-1.  Thus, besides the value
;   of -1, the returned INDICES refer to the nearest reference value
;   to the left of the requested value.
;
;   If the reference array is monotonically decreasing then the
;   partitions are numbered starting at -1 from the right instead (and
;   the returned INDICES refer to the nearest reference value to the
;   *right* of the requested value).  If the reference array is
;   neither monotonically increasing or decreasing the results of
;   VALUE_LOCATE are undefined.
;
;   VALUE_LOCATE appears as a built-in funcion in IDL v5.3 and later.
;   This version of VALUE_LOCATE should work under IDL v4 and later,
;   and is intended to provide a portable solution for users who do
;   not have the latest version of IDL.  The algrorithm in this file
;   is slower but not terribly so, than the built-in version.
;
;   Users should be able to place this file in their IDL path safely:
;   under IDL 5.3 and later, the built-in function will take
;   precedence; under IDL 5.2 and earlier, this function will be used.
;
; INPUTS:
;
;   REF - the reference array of monotonically increasing or
;         decreasing values.
;
;   VALUES - a scalar value or array of values to be located in the
;            reference array.
;
;
; KEYWORDS:
;
;   L64 - (ignored) for compatibility with built-in version. 
;
;   NO_CROP - if set, and VALUES is outside of the region between X[0]
;             and X[NBINS-1], then the returned indices may be *less
;             than* -1 or *greater than* NBINS-1.  The user is the
;             responsible for cropping these values appropriately.
;
; RETURNS:
;
;   An array of indices between -1L and NBINS-1.  If VALUES is an
;   array then the returned array will have the same dimensions.
;
;
; EXAMPLE:
;
;   Cast random values into a histogram with bins from 1-10, 10-100,
;   100-1000, and 1000-10,000.
;
;     ;; Make bin edges - this is the ref. array
;     xbins = 10D^dindgen(5)  
;
;     ;; Make some random data that ranges from 1 to 10,000
;     x     = 10D^(randomu(seed,1000)*4)
;
;     ;; Find the bin number of each random value
;     ii    = value_locate(xbins, x)
;
;     ;; Histogram the data
;     hh    = histogram(ii)
;
;
; SEE ALSO:
;
;   VALUE_LOCATE (IDL 5.3 and later), HISTOGRAM, CMHISTOGRAM
;
;
; MODIFICATION HISTORY:
;   Written and documented, 21 Jan 2001
;   Case of XBINS having only one element, CM, 29 Apr 2001
;   Handle case of VALUES exactly hitting REF points, CM, 13 Oct 2001
; 
;  $Id$
;
;-
; Copyright (C) 2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function value_locate, xbins, x, l64=l64, no_crop=nocrop, _EXTRA=extra

  on_error, 2
  nbins = n_elements(xbins)
  sz = size(xbins)

  ;; Error checking
  if nbins EQ 0 then message, 'ERROR: XBINS must have at least one element'
  if nbins EQ 1 then return, (x GE xbins(0)) - 1L
  
  ;; The values are computed by spline interpolation.  Here is the "y"
  ;; value of the spline, which is just the bin position.
  tp = sz(sz(0)+1)
  if tp EQ 1 OR tp EQ 2 OR tp EQ 12 then begin
      yy = findgen(nbins) - 0.5
      eps = (machar()).eps
  endif else begin
      yy = dindgen(nbins) - 0.5D
      eps = (machar(/double)).eps
  endelse      

  ;; Check if we are reversing.
  if xbins(nbins-1) GT xbins(0) then rev = 0 else rev = 1

  ;; Compute the spline interpolation.  Note here that we set the 2nd
  ;; derivative value to zero since the derivative computed by
  ;; SPL_INIT seems to be royally screwed up.  Also we do separate
  ;; computations for the increasing and non-increasing cases, since
  ;; SPL_INTERP seems to choke on the later.
  if rev EQ 0 then $
    ii = round(spl_interp(xbins, yy, yy*0, x) + eps) $
  else $
    ii = round(spl_interp(reverse(xbins), yy, yy*0, x) + eps)

  ;; Crop the end values appropriately
  if NOT keyword_set(nocrop) then begin
      ii = ii > (-1L) < (nbins-1)
  endif
      
  ;; Reverse the array
  if rev EQ 0 then $
    ret = temporary(ii) $
  else $
    ret = (nbins-2)-temporary(ii)

  ;; Reform the array to the correct dimensions (ie, add trailing
  ;; dimensions)
  sz = size(x)
  if sz(0) GT 0 then $
    ret = reform(ret, sz(1:sz(0)), /overwrite)

  return, ret
end
