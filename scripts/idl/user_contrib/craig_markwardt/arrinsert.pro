;+
; NAME:
;   ARRINSERT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Insert one array into another
;
; CALLING SEQUENCE:
;   NEWARR = ARRINSERT(INIT, INSERT, [AT=POSITION] )
;
; DESCRIPTION: 
;
;   ARRINSERT inserts the contents of one array (INSERT) into
;   another (INIT), and returns the new array (NEWARR).
;
;   ARRINSERT will handle empty lists, which are represented as
;   undefined variables.  If both input arrays are empty, then the
;   scalar -1L is returned, and the keyword COUNT is set to 0L.
;
; INPUTS:
;
;   INIT - the initial array, into which INSERT will be inserted.  Any
;          data type, including structures, is allowed.  Regardless of
;          the dimensions of INIT, it is treated as a one-dimensional
;          array.  If OVERWRITE is not set, then INIT itself is
;          unmodified.
;
;   INSERT - the array to be inserted into INIT, which must be of the
;            same or similar type to INIT.  If INSERT is empty, then
;            INIT is returned unchanged.  Regardless of the dimensions
;            of INSERT, it is treated as a one-dimensional array.
;
; KEYWORDS:
;
;    AT - a long integer indicating the position of the newly inserted
;         sub-array.  If AT is non-negative, then INSERT will appear
;         at NEWARR[AT].  If AT is negative, then INSERT will appear
;         at NEWARR[AT + (N+1)] where N is the number of elements in
;         INIT, which is to say if AT is negative, it indexes from the
;         end side of the array rather than the beginning.  Thus,
;         setting AT=-1 will concatenate INIT and INSERT.
;
;         Default: 0L (INSERT appears at beginning of INIT)
;
;   OVERWRITE - if set, then the initial array INIT will be
;               overwritten by the new array.  Upon exit INIT becomes
;               undefined.
;
;   COUNT - upon return, the number of elements in the resulting
;           array.
;
;  EMPTY1, EMPTY2 - if set, then INIT (for EMPTY1) or INSERT (for
;                   EMPTY2) are assumed to be empty (i.e., to have
;                   zero elements).  The actual values passed as INIT
;                   or INSERT are then ignored.
;
; RETURNS:
;
;   The new array, which is always one-dimensional.  If COUNT is zero,
;   then the scalar -1L is returned.
;
; EXAMPLE:
;
;   X = [1, 2, 3]
;   Y = [4, 5, 6, 7]
;
;   ; Insert Y at the beginning of X
;   result = arrinsert(x, y, at=0)
;      --> result = [4, 5, 6, 7, 1, 2, 3]
;
;   ; Insert Y in the middle of X
;   result = arrinsert(x, y, at=1)
;     --> result = [1, 4, 5, 6, 7, 2, 3]
;
;   ; Append Y at the end of X
;   result = arrinsert(x, y, at=-1)
;     --> result = [1, 2, 3, 4, 5, 6, 7]
;
; SEE ALSO:
;
;   ARRDELETE, STORE_ARRAY in IDL Astronomy Library
;
; MODIFICATION HISTORY:
;   Written, CM, 02 Mar 2000
;   Added OVERWRITE and EMPTY keywords, CM, 04 Mar 2000
;   Improved internal docs, and AT keyword docs, CM, 28 Sep 2000
;   Doc clarifications, CM, 29 Sep 2001
;   Added examples to documentation, CM, 06 Apr 2008
;
;  $Id$
;
;-
; Copyright (C) 2000,2001,2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function arrinsert, init, insert, at=at0, count=count, overwrite=overwrite, $
                    empty1=empty1, empty2=empty2

  on_error, 2

  ;; Total number of elements in output
  count = (n_elements(init)*(keyword_set(empty1) EQ 0) + $
           n_elements(insert)*(keyword_set(empty2) EQ 0))

  ;; Account for various "empty" special cases.
  ;; INIT and INSERT are empty
  if (n_elements(init) EQ 0 OR keyword_set(empty1)) AND $
    (n_elements(insert) EQ 0 OR keyword_set(empty2)) then return, -1L
  ;; INIT alone is empty
  if (n_elements(init) EQ 0 OR keyword_set(empty1)) then $
    return, reform([insert], n_elements(insert))
  ;; INSERT alone is empty
  if (n_elements(insert) EQ 0 OR keyword_set(empty2)) then $
    return, reform([init], n_elements(init), overwrite=keyword_set(overwrite))

  n1 = n_elements(init)   & sz1 = size(init)    & tp1 = sz1(sz1(0)+1)
  n2 = n_elements(insert) & sz2 = size(insert)  & tp2 = sz2(sz2(0)+1)

  ;; Compute insertion position using AT keyword
  if n_elements(at0) EQ 0 then at = 0L else at = long(at0(0))
  if at LT 0 then at = (n1 + 1L + at) > 0
  at = (at > 0) < n1

  ;; Allow data to have different types, but they must be at least of
  ;; the same "base" type.  That is, you can't combine a number with a
  ;; string, etc.
  ;; basetype 0:undefined 1:real number 6:complex number 7:string
  ;;     8:structure 10:pointer 11:object

  ;;          0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
  basetype = [0, 1, 1, 1, 1, 1, 6, 7, 8, 6,10,11, 1, 1, 1, 1]

  if tp1 LT 0 OR tp1 GE 16 OR tp2 LT 0 OR tp2 GE 16 then $
    message, 'ERROR: unrecognized data types for operands'
  if basetype(tp1) NE basetype(tp2) then $
    message, 'ERROR: operands must have same data type'

  if keyword_set(overwrite) then ct = n2 else ct = n1+n2

  ;; Create new output array
  if tp1 EQ 8 then out = make_array(value=init(0), ct) $
  else             out = make_array(type=tp1, ct, /nozero)

  if keyword_set(overwrite) then begin

      ;; Overwrite, so we try to conserve as much memory as possible,
      ;; and reduce the amount of copying

      if at LT n1/2 then begin  ;; Closer to begining
          out = [temporary(out), temporary(init)]
          if at GT 0 then out(0) = out(n2:at+n2-1)
      endif else begin          ;; Closer to end
          out = [temporary(init), temporary(out)]
          if at LT n1 then out(at+n2) = out(at:n1-1)
      endelse
  endif else begin

      ;; Otherwise, copy the old data into place

      if at GT 0  then out(0) = init(0:at-1)
      if at LT n1 then out(at+n2) = init(at:*)
  endelse

  ;; Insert the new data
  out(at) = reform([insert], n2)

  return, out
end
