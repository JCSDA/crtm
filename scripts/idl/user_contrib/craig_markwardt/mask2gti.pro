;+
; NAME:
;   MASK2GTI
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Convert a gridded set of times to a set of Good Time Intervals (GTIs)
;
; CALLING SEQUENCE:
;   GTI = MASK2GTI(TIME, MASK, COUNT, INDICES=INDICES, $
;                  TIMEDEL=, GOOD=, BAD=, PRE=, POST=)
;
; DESCRIPTION: 
;
;   The function MASK2GTI accepts an array of times and mask, and
;   converts valid data into corresponding good time intervals (GTIs).
;
;   Elements of the MASK array are clustered together according to
;   whether they are "good" or not.  Contiguous segments of good
;   elements are converted to single good time intervals.  Time
;   elements are considered to be regularly spaced, so any breaks in
;   the MASK values are considered to be discontinuities.
;
;   The time array *must* be evenly spaced and sorted in ascending
;   order.  Each element of MASK must correspond to the same element
;   of TIME.  The primary difference between GTISEG and MASK2GTI is
;   that GTI2SEG allows time values to be irregularly sampled and no
;   mask is passed.  Also, MASK2GTI allows intervals to be enlarged or
;   shrunk.
;
;   It should be noted that this function is not constrained to
;   operation only on time arrays.  It should work on any
;   one-dimensional quantity with intervals.
;
; INPUTS:
;
;   TIME - an array of evenly spaced, ascending order, times.
;
;   MASK - an array of values matched to TIME.
;
;   COUNT - upon return, the number of resulting intervals.  A value
;           of zero indicates no good time intervals.
;
; KEYWORDS:
;
;   INDICES - upon return, a 2xCOUNT array of integers which give the
;             indices of samples which lie within each interval.  The
;             times TIME(INDICES(0,i) : INDICES(1,i)) fall within the
;             ith interval.
;
;   TIMEDEL - a scalar value giving the time spacing of the array.
;             Default: TIME(1)-TIME(0)
;
;   PRE - the amount each interval should be enlarged from its leading
;         edge.  A negative value indicates the interval should
;         shrink.
;         Default: 0
;
;   POST - the amount each interval should be enlarged from its
;          trailing edge.  A negative value indicates the interval
;          should shrink. 
;          Default: 0
;   
;   GOOD - the value of "good" in the input mask array.
;          Default: 1b
;
;   BAD - the value of "bad" in the input mask array.
;         Default: 0b
;
;
; RETURNS:
;
;   A new GTI array containing the enlarged or shrunken intervals.
;   The array is 2xCOUNT where COUNT is the number of resulting
;   intervals.  GTI(*,i) represents the start and stop times of
;   interval number i.  The intervals are non-overlapping and
;   time-ordered. 
;
;   If COUNT is zero then the returned array is a scalar value of
;   zero, indicating no good intervals were found.
;
; SEE ALSO:
;
;   GTI2MASK, GTITRIM, GTIMERGE, GTIWHERE
;
; MODIFICATION HISTORY:
;   Written, CM, 1997-2001
;   Documented, CM, Apr 2001
;
;  $Id$
;
;-
; Copyright (C) 1997-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function mask2gti, time, mask, count, indices=indices, query=query, $
                   good=good, bad=bad, $
                   timepixr=timepixr, timedel=timedel, pre=pre, post=post

  if keyword_set(query) then return, 1
  if n_params() EQ 0 then begin
      message, 'GTI = MASK2GTI(TIME, MASK, COUNT, INDICES=, GOOD=, BAD=, PRE=, POST=)', /info
      return, !values.d_nan
  endif

  count = 0L

  if n_elements(good) GT 0 AND n_elements(bad) GT 0 then $
    message, 'ERROR: cannot specify both GOOD and BAD'
  if n_elements(good) EQ 0 AND n_elements(bad) EQ 0 then $
    good = 1
  if n_elements(timepixr) EQ 0 then timepixr = time(0)*0
  if n_elements(timedel) EQ 0 then timedel = time(1)-time(0)

  if n_elements(good) GT 0 then $
    goodind = where( mask EQ good(0), ct ) $
  else $
    goodind = where( mask NE bad(0), ct )

  if ct EQ 0 then return, 0L

  if ct EQ 1 then begin
      count = 1L
      return, reform(time(goodind(0))+timedel*[-timepixr,1.-timepixr], 2, 1)
  endif

  n = n_elements(goodind)

  delta = goodind(1:n-1) - goodind(0:n-2)

  trange = [ -1L , where( delta GT 1, nrange), n-1 ]
  if nrange EQ 0 then trange = [-1L, n-1]
  nrange = nrange + 1

  indices = reform(lonarr(2, nrange), 2, nrange, /overwrite)
  indices(0,*) = goodind(trange(0:nrange-1)+1)
  indices(1,*) = goodind(trange(1:nrange))

  newgti = make_array(2, nrange, value=time(0)*0)
  newgti = reform(newgti, 2, nrange, /overwrite)
  newgti(0,*) = time(indices(0,*)) - timedel*timepixr
  newgti(1,*) = time(indices(1,*)) + timedel*(1.-timepixr)

  count = nrange

  if n_elements(pre) GT 0 OR n_elements(post) GT 0 then begin

      forward_function gtienlarge
      catch, catcherr & qgti = 0
      if catcherr EQ 0 then qgti = gtienlarge(/query)
      catch, /cancel
      if catcherr NE 0 OR qgti EQ 0 then $
        message, 'ERROR: The function GTIENLARGE must be in your IDL path'

      if n_elements(pre)  EQ 0 then pre  = 0D
      if n_elements(post) EQ 0 then post = 0D
      newgti = gtienlarge(newgti, count=count, pre=pre, post=post)

      ;; This is complicated here: we want to figure out the indices
      ;; that this newly enlarged GTI contains.  This nasty code does
      ;; this, with hopefully the minimum amount of fuss and muss.
      if count GT 0 then begin
          indices = reform(lonarr(2, count), 2, count, /overwrite)
          for i = 0, count-1 do begin
              wh = where(time GE newgti(0,i), ct)
              if ct EQ 0 then goto, DO_TSTOP
              indices(0,i) = wh(0)
          endfor
          DO_TSTOP:
          for i = 0, count-1 do begin
              wh = where(time LT newgti(1,i), ct)
              if ct EQ 0 then goto, DONE_INDICES
              indices(1,i) = wh(ct-1)
          endfor
          DONE_INDICES:
      endif
  endif
      
  return, newgti
end

