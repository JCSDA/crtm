;+
; NAME:
;   GTIENLARGE
;   
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Enlarge (or shrink) each Good Time Interval (GTI) by fixed amounts
;
; CALLING SEQUENCE:
;   NEWGTI = GTIENLARGE(GTI, COUNT=COUNT, PRE=PRE, POST=POST)
;
; DESCRIPTION: 
;
;   The function GTIENLARGE accepts an existing valid Good Time
;   Interval (GTI) array and creates a new GTI array in which the
;   intervals have been enlarged (or shrunken) by a fixed amount.
;
;   The keywords PRE and POST are used to specify the enlargement.
;   Given an existing good interval such as this one:
;
;                  100                 200              GTI=[[100,200]]
;      <------------|===================|------------->
;
;   a positive value of PRE will enlarge the lead edge of the interval
;   and a positive value of POST will enlarge the trailing edge of the
;   interval.  Thus PRE=10 and POST=20 will create a new interval from
;   the above example:
;
;               90<--                   --->220      NEWGTI=[[ 90,220]]
;      <---------|==.===================.====|-------->
;             PRE=10                   POST=20
;
;   Negative values of PRE and POST are allowed, which will shrink the
;   interval from the leading and trailing edges respectively.
;
;   Users should be aware that the number of intervals may shrink
;   under this operation, since it is possible either for two
;   intervals to be merged if they are enlarged and overlap, or if
;   they are shrunken to a size of zero.
;
;   It should be noted that this function is not constrained to
;   operation only on time arrays.  It should work on any
;   one-dimensional quantity with intervals.
;
; INPUTS:
;
;   GTI - a 2xNINTERVAL array where NINTERVAL is the number of
;         intervals.  GTI(*,i) represents the start and stop times of
;         interval number i.  The intervals must be non-overlapping
;         and time-ordered (use GTITRIM to achieve this).
;
;         A scalar value of zero indicates that the GTI is empty, ie,
;         there are no good intervals.
;
; KEYWORDS:
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
;   COUNT - upon return, the number of resulting intervals.  A value
;           of zero indicates no good time intervals.
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
;
; SEE ALSO:
;
;   GTITRIM, GTIENLARGE
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
function gtienlarge, gti, count=count, pre=pre, post=post, query=query

  if keyword_set(query) then return, 1

  ;; When PRE and POST are given, we must be very careful, for the
  ;; GTIs may become overlapping.  That will confuse everything else.
  ;; The strategy I choose here is to look at adjacent GTIs and remove
  ;; any that overlap.  I think that gets everything, but I worry that
  ;; it won't get (say) triply overlapping GTIs.
  count = n_elements(gti)/2
  if count EQ 0 then return, 0L
  if n_elements(pre) EQ 0 AND n_elements(post) EQ 0 then return, gti

  newgti = gti

  if n_elements(pre)  GT 0 then newgti(0,*) = newgti(0,*) - pre(0)
  if n_elements(post) GT 0 then newgti(1,*) = newgti(1,*) + post(0)

  wh = where(newgti(1,*) GT newgti(0,*), count)
  if count EQ 0 then return, 0

  ;; If only one GTI entry, then no need for special treatment
  if count EQ 1 then return, reform(newgti(*, wh), 2, 1)

  newgti = newgti(*,wh)
  
  ;; Get first and last GTI values
  t0 = newgti(0)
  t1 = newgti(count*2-1)
  ;; Get possibly "overlapping" values
  vgti = reform(newgti(1:count*2-2), 2, count-1)
  ;; Make sure they *don't* overlap
  wh = where(vgti(1,*) GT vgti(0,*), ct)
  
  if ct GT 0 then begin
      count = ct + 1
      newgti = reform([t0, (vgti(*,wh))(*), t1], 2, count)
  endif else begin
      count = 1L
      newgti = reform([t0, t1], 2, 1)
  endelse

  return, newgti
end
