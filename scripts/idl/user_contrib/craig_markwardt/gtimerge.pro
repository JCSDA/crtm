;+
; NAME:
;   GTIMERGE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Merge two Good Time Interval (GTIs) arrays into a single array
;
; CALLING SEQUENCE:
;   NEWGTI = GTIMERGE(GTI1, GTI2, COUNT=COUNT, [/INTERSECT, /UNION,
;                     /INVERT1, /INVERT2, TTOLERANCE=])
;
; DESCRIPTION: 
;
;   The function GTIMERGE accepts two existing valid Good Time
;   Interval (GTI) arrays and merges them into a single array.  Either
;   the intersection or the union of the two GTIs are returned.
;
;   The intersection refers to the set of intervals which lie in both
;   intervals.  The union refers to the set of intervals which lie in
;   at least one but not necessarily both.  Here is an example of both
;   kinds of operations.  Let us start with two intervals here:
;
;        0              50              100                  170   GTI1
;   <----|==============|----------------|====================|------>
;
;                 30                         120                   GTI2
;   <--------------|==========================|---------------------->
;
;   These intervals would be represented by GTI1=[[0,50],[100,170]]
;   and GTI2=[[30,120]]. The intersection of the two sets of intervals
;   are the points which lie in both, ie [[30,50],[100,120]]:
;
;                 30   50               100  120              INTERSECT
;   <--------------|====|----------------|====|---------------------->
;
;   The union is the combination of both intervals, ie [[0,170]]:
;
;        0                                                   170  UNION
;   <----|====================================================|------>
;
;   It is also possible to treat either one of the input arrays as
;   "bad" intervals using the INVERT1 and/or INVERT2 keywords.  When
;   an interval is inverted, then the output is composed only of areas
;   *outside* the specified intervals.
;
;   It should be noted that this function is not constrained to
;   operation only on time arrays.  It should work on any
;   one-dimensional quantity with intervals.
;
;
; PERFORMANCE:  Combining many intervals
;
;   Users who wish to combine many intervals in sequence will find a
;   performance degradation.  The problem is that each GTIMERGE
;   operation is order N^2 execution time where N is the number of
;   intervals.  Thus, if N mostly distinct GTIs are merged, then the
;   running time will be order N^3.  This is unacceptable, but there
;   is a workaround.
;
;   Users can accumulate "sub" GTIs by merging subsets of the full
;   number of intervals to be merged, and then occasionally merging
;   into the final output GTI.  As an example, here first is a simple
;   merging of 1000 different GTIs:
;
;     totgti = 0L                   ;; Empty GTI
;     FOR i = 0, 999 DO BEGIN
;         gti = ...                
;         totgti = gtimerge(totgti, gti, /union)
;     ENDFOR
;
;   This computation may take a long time.  Instead the merging can be
;   broken into chunks.
;
;     totgti = 0L
;     chgti  = 0L      ;; "Chunk" GTI
;     FOR i = 0, 999 DO BEGIN
;         gti = ...
;         chgti = gtimerge(chgti, gti, /union)
;         if (n_elements(chgti) GT 100) OR (i EQ 999) then begin
;             ;; Merge "chunk" gti into final one, and reset
;             totgti = gtimerge(totgti, chgti, /union)
;             chgti = 0L
;         endif
;     ENDFOR
;
;   Note that the final merge is guaranteed because of the (i EQ 999)
;   comparison.
;
; INPUTS:
;
;   GTI1, GTI2 - the two input GTI arrays.
;
;         Each array is a 2xNINTERVAL array where NINTERVAL is the
;         number of intervals, which can be different for each array.
;         GTI(*,i) represents the start and stop times of interval
;         number i.  The intervals must be non-overlapping and
;         time-ordered (use GTITRIM to achieve this).
;
;         A scalar value of zero indicates that the GTI is empty, ie,
;         there are no good intervals.
;
; KEYWORDS:
;
;   INTERSECT - if set, then the resulting GTI contains only those
;               intervals that are in both input sets.
;
;   UNION - if set, then the resulting GTI contains those intervals
;           that are in either input set.
;
;   COUNT - upon return, the number of resulting intervals.  A value
;           of zero indicates no good time intervals.
;
;   INVERT1 - if set, then GTI1 is considered to be inverted, ie, a
;             set of "bad" intervals rather than good.
;
;   INVERT2 - if set, then GTI2 is considered to be inverted, ie, a
;             set of "bad" intervals rather than good.
;
;   TTOLERANCE - a scalar value indicating the tolerance for
;                determining whether values are equal.  This number
;                can be important for intervals that do not match
;                precisely.
;                Default: Machine precision
;
; RETURNS:
;
;   A new GTI array containing the merged intervals.  The array is
;   2xCOUNT where COUNT is the number of resulting intervals.
;   GTI(*,i) represents the start and stop times of interval number i.
;   The intervals are non-overlapping and time-ordered.
;
;   If COUNT is zero then the returned array is a scalar value of
;   zero, indicating no good intervals were found.
;
; SEE ALSO:
;
;   GTITRIM, GTIENLARGE
;
; MODIFICATION HISTORY:
;   Written, CM, 1997-2001
;   Documented, CM, Apr 2001
;   Handle case of zero-time GTIs, CM, 02 Aug 2001
;   Handle "fractured" GTIs correctly, though worriedly, CM, 15 Oct
;     2001
;   Handle case where both inputs are empty, but /INVERT1 and/or
;     /INVERT2 are set, CM, 08 Aug 2006
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
function gtimerge, gti1, gti2, count=count, intersect=intersect, union=union, $
                   invert1=invert1, invert2=invert2, quiet=quiet, $
                   ttolerance=ttol, query=query

  if keyword_set(query) then return, 1

  ;; Intialize count variable.  None found yet.
  count = 0L

  forward_function gtitrim
  catch, catcherr & qgti = 0
  if catcherr EQ 0 then qgti = gtitrim(/query)
  catch, /cancel
  if catcherr NE 0 OR qgti EQ 0 then $
    message, 'ERROR: The function GTITRIM must be in your IDL path'

  if n_elements(ttol) EQ 0 then begin
      sz1 = size(gti1) & sz2 = size(gti2)
      if sz1(sz1(0)+1) EQ 5 OR sz2(sz2(0)+1) EQ 5 then $
        double = 1 $
      else $
        double = 0
      mach = machar(double=double)
      ttol = mach.eps
  endif
  if keyword_set(intersect) and keyword_set(union) then $
    message, 'ERROR: cannot perform both INTERSECT and UNION.'

  if NOT keyword_set(intersect) and NOT keyword_set(union) then $
    message, 'ERROR: must perform either INTERSECT or UNION.'

  ;; The strategy is as follows: break the two GTIs down into a list
  ;; of times.  Associated with each time is the ON/OFF status of the
  ;; GTI.  Actually, it's ON, OFF, or NO CHANGE, since one GTI may
  ;; change when the other does not.  The list of times is sorted, and
  ;; then walked through one at a time, in order of time.  When the
  ;; merging conditions are satisfied (either intersect or union),
  ;; then a new gti entry is created.

  count = 0L
  ngti1 = 0L 
  ngti2 = 0L
  if n_elements(gti1) GT 1 then begin
      newgti1 = reform(gti1, 2, n_elements(gti1)/2)
      ;; Normalize and repack the GTI.  First order of business is to
      ;; remove any zero-time intervals.
      newgti1 = gtitrim(newgti1, mingti=ttol, maxgap=ttol, count=ngti1)

      ;; This is the ON/OFF array for gti1
      if ngti1 GT 0 then begin
          on1 = newgti1
          on1(0,*) = 1 - keyword_set(invert1)
          on1(1,*) = 0 + keyword_set(invert1)
      endif
  endif  
  if n_elements(gti2) GT 1 then begin
      newgti2 = reform(gti2, 2, n_elements(gti2)/2)
      ;; Normalize and repack the GTI.  First order of business is to
      ;; remove any zero-time intervals.
      newgti2 = gtitrim(newgti2, mingti=ttol, maxgap=ttol, count=ngti2)

      ;; This is the ON/OFF array for gti1
      if ngti2 GT 0 then begin
          on2 = newgti2
          on2(0,*) = 1 - keyword_set(invert2)
          on2(1,*) = 0 + keyword_set(invert2)
      endif
  endif  

  ;; Merge the one or two ON/OFF arrays together
  if ngti1 GT 0 AND ngti2 GT 0 then begin
      ;; Here is the merged list of times, eventually called tt
      tt = [newgti1(*), newgti2(*)]
      nt = n_elements(tt)
      ;; Now the ON/OFF states are merged as well.  Here, 0 and 1
      ;; represent off and on, and -1 represents NO CHANGE. 
      o1 = reform([on1(*), on2(*)*0-1], nt)
      o2 = reform([on1(*)*0-1, on2(*)], nt)

  endif else if ngti1 GT 0 then begin
      tt = [newgti1(*), min(newgti1)]
      nt = n_elements(tt)
      o1 = reform([on1(*),    -1                   ], nt)
      o2 = reform([on1(*)*0-1, keyword_set(invert2)], nt)
      
  endif else if ngti2 GT 0 then begin
      tt = [min(newgti2), newgti2(*)]
      nt = n_elements(tt)
      o1 = reform([keyword_set(invert1), on2(*)*0-1], nt)
      o2 = reform([-1,                   on2(*)    ], nt)

  endif else begin
      if ((keyword_set(intersect) AND $
           (keyword_set(invert1) AND keyword_set(invert2)))) OR $
        ((keyword_set(union) AND $
          (keyword_set(invert1) OR keyword_set(invert2)))) then begin
          count = 1L
          return, [-!values.f_infinity, +!values.f_infinity]
      endif
      goto, EMPTY_GTI
  endelse
  tt = reform(tt, nt, /overwrite)

  ;; Sort by time
  ts = sort(tt)
  tt = tt(ts) & o1 = o1(ts) & o2 = o2(ts)
  ;; Simulated UNIQ, with well defined properties
  un = where(tt NE shift(tt, +1), ct)
  if ct EQ 0 then wh = 0L
  uu = [un, nt]
  nu = n_elements(un)

  ;; Now we are ready to step through the list of times.  Yes1 and
  ;; yes2 are 1 when the corresponding GTI is on.
  yes1 = keyword_set(invert1) & k1 = NOT keyword_set(invert1)
  yes2 = keyword_set(invert2) & k2 = NOT keyword_set(invert2)
  ;; Within is set to 1 when we are in the middle of a new GTI entry
  within = 0
  numgti = 0L
  dbl1 =  0 & dbl2 =  0
  for j = 0L, nu-1 do begin

      oo1  = -1 &  oo2 = -1
      ;; Collect all times that are the same into one batch.  This is
      ;; important, since a single time point should be considered
      ;; atomic.  If multiple transitions occur in one channel at the
      ;; same time, which shouldn't happen because of the GTI trimming
      ;; above, then we take the first one.
      i = uu(j)
      wh = where(o1(uu(j):uu(j+1)-1) GE 0, ct)
      if ct GT 0 then begin
          o1u = o1(uu(j)+wh)
          oo1 = max(o1u)
          if ct GT 1 then if min(o1u) NE oo1 then dbl1 = 1
      endif
      wh = where(o2(uu(j):uu(j+1)-1) GE 0, ct)
      if ct GT 0 then begin
          o2u = o2(uu(j)+wh)
          oo2 = max(o2u)
          if ct GT 1 then if min(o2u) NE oo2 then dbl2 = 1
      endif
;      print, '  ', o1(uu(j):uu(j+1)-1), format='(A0,100(D4.0," ",:),$)'
;      print, ' --> ', oo1
;      print, '  ', o2(uu(j):uu(j+1)-1), format='(A0,100(D4.0," ",:),$)'
;      print, ' --> ', oo2

      ;; yes1 and yes2 are updated here, if there is a change in gti state
      if oo1 GE 0 then yes1 = (oo1 EQ 1)
      if oo2 GE 0 then yes2 = (oo2 EQ 1)

      ;; Here are the conditions for starting a new GTI entry
      if (NOT within AND (yes1 OR  yes2) AND keyword_set(union)) OR $
        ( NOT within AND (yes1 AND yes2) AND keyword_set(intersect)) then $
        begin
          tstart = tt(i)
          within = 1
      endif
      if dbl1 AND yes1 then yes1 = 0
      if dbl2 AND yes2 then yes1 = 0

      ;; And the conditions for ending it.
      if (within AND (NOT yes1 AND NOT yes2) AND keyword_set(union)) OR $
        ( within AND (NOT yes1 OR  NOT yes2) AND keyword_set(intersect)) then $
        begin
          if numgti EQ 0 then $
            newgti = [ tstart, tt(i) ] $
          else $
            newgti = [ newgti, tstart, tt(i) ]
          numgti   = numgti + 1
          within   = 0

          if dbl1 OR dbl2 then begin
              ;; Now restart the GTI if desired
              tstart = tt(i)
              within = 1
              dbl1 = 0 & dbl2 = 0
          endif
      endif

      LOOPCONT:
  endfor

  ;; I think this clause is activated only if the INVERT1 or INVERT2
  ;; keywords are activated.  This should be tested.  Boundary
  ;; conditions can be wierd.
  if within then begin
      if numgti EQ 0 then $
        newgti = [ tstart, max(tt) ] $
      else $
        newgti = [ newgti, tstart, max(tt) ]
      numgti   = numgti + 1
      within   = 0
  endif
  
  ;; Empty GTI is a special case.  Here I have opted to *not* take the
  ;; lame HEASARC route.  Instead, I define a single element GTI array
  ;; to mean no good times.
  if numgti EQ 0 then begin
      EMPTY_GTI:
      count = 0L
      return, 0L
  endif

  ;; Reconstitute the new GTI array
  count    = numgti
  newgti   = reform(newgti, 2, numgti, /overwrite)

  ;; Now do some clean-up, removing zero-time GTIs and GTIs that
  ;; adjoin.  This really should not be necessary, since by definition
  ;; the above technique should not create buggy GTIs.
  if numgti GT 0 then begin
      ocount = count
      newgti = gtitrim(newgti, maxgap=ttol, count=count)
  endif

  return, newgti
end
