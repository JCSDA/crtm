;+
; NAME:
;   GTISEG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Convert a list of times to a set of Good Time Intervals (GTIs)
;
; CALLING SEQUENCE:
;   GTI = GTISEG(TIMES, COUNT=COUNT, INDICES=INDICES, $
;                MAXGAP=MAXGAP, MINGTI=MINGTI)
;
; DESCRIPTION: 
;
;   The function GTISEG accepts an array of times and converts
;   adjacent data into good time intervals (GTIs).
;
;   Elements of the array are clustered into intervals based on the
;   gaps between times.  If the gaps are small enough then the times
;   are grouped into a single interval.  If a gap exceeds MAXGAP, then
;   an interruption occurs and at least two intervals are formed.
;   Thus, the keyword parameter MAXGAP essentially determines how many
;   and where the intervals will be formed.
;
;   If the time samples are regularly spaced -- aside from gaps --
;   then MAXGAP should be set to a number slightly larger than the
;   spacing to prevent roundoff errors.  By default MAXGAP is set to
;   the difference between the first and second samples.
;
;   For GTISEG, the samples do not need to be regularly spaced, but
;   they *must* be given in ascending order.  Arrays can be sorted
;   with the SORT function.  The primary difference between GTISEG and
;   MASK2GTI is that MASK2GTI assumes the time samples are regularly
;   spaced while GTISEG does not.  Also, MASK2GTI allows intervals to
;   be enlarged or shrunk.
;
;   It should be noted that this function is not constrained to
;   operation only on time arrays.  It should work on any
;   one-dimensional quantity with intervals.
;
; INPUTS:
;
;   TIME - an array of times in ascending order.
;
;
; KEYWORDS:
;
;   MAXGAP - a scalar, the maximum gap between time samples before a
;            new interval is created.  Samples with gaps smaller than
;            this value are grouped into a single GTI.
;            Default: TIME(1) - TIME(0)
;
;   MINGTI - the smallest possible GTI.  Any interval smaller than
;            MINGTI is discarded.
;            Default: 0   (all intervals are accepted)
;
;   COUNT - upon return, the number of resulting intervals.  A value
;           of zero indicates no good time intervals.
;
;   INDICES - upon return, a 2xCOUNT array of integers which give the
;             indices of samples which lie within each interval.  The
;             times TIME(INDICES(0,i) : INDICES(1,i)) fall within the
;             ith interval.
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
;   MASK2GTI, GTITRIM, GTIMERGE, GTIWHERE
;
; MODIFICATION HISTORY:
;   Written, CM, 1999-2001
;   Documented, CM, Apr 2001
;   MINGTI now works as documented, in that segments *equal* to MINGTI
;     are now accepted, CM, 30 Oct 2007
;   MINGTI now also affects INDICES, CM, 03 Mar 2008
;
;  $Id$
;
;-
; Copyright (C) 1999-2001, 2007, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function gtiseg, time, maxgap=maxgap, mingti=mingti, $
                 indices=indices, count=count, query=query

  if keyword_set(query) then return, 1
  count = 0L

  if n_params() EQ 0 then begin
    message, 'GTI = GTISEG(TIME, [COUNT=,] [INDICES=,] [MAXGAP=,] [MINGTI=])',$
      /info
    return, 0
  endif

  if n_elements(maxgap) EQ 0 then maxgap = time(1) - time(0)
  nt = n_elements(time)
  tdiff = time(1:*) - time
  whdiff = [-1L, where(tdiff GT maxgap(0), ntseg), nt-1]
  if ntseg EQ 0 then whdiff = [-1L, nt-1]
  ntseg = ntseg + 1

  mintdiff = min(tdiff) > 0

  indices = reform(lonarr(2, ntseg), 2, ntseg, /overwrite)
  indices(0,*) = whdiff(0:ntseg-1)+1
  indices(1,*) = whdiff(1:ntseg)

  tgti = reform(make_array(2, ntseg, value=time(0)*0), 2, ntseg, /overwrite)
  tgti(0,*) = time(indices(0,*))
  tgti(1,*) = time(indices(1,*)) + mintdiff

  if n_elements(mingti) GT 0 then begin
      wh = where(tgti(1,*)-tgti(0,*) GE mingti(0), ntseg)
      if ntseg GT 0 then begin
          tgti = reform(tgti(*,wh),2,ntseg)
          indices = reform(indices(*,wh),2,ntseg)
      endif else begin
          tgti = -1L
          indices = -1L
      endelse
  endif

  count = ntseg
  return, tgti
end
