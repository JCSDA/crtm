;+
; NAME:
;   ARRDELETE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Remove a portion of an existing array.
;
; CALLING SEQUENCE:
;   NEWARR = ARRDELETE(INIT, [AT=POSITION,] [LENGTH=NELEM])
;
; DESCRIPTION: 
;
;   ARRDELETE will remove or excise a portion of an existing array,
;   INIT, and return it as NEWARR.  The returned array will never be
;   larger than the initial array.
;
;   By using the keywords AT and LENGTH, which describe the position
;   and number of elements to be excised respectively, any segment of
;   interest can be removed.  By default the first element is removed.
;
; INPUTS:
;
;   INIT - the initial array, which will have a portion deleted.  Any
;          data type, including structures, is allowed.  Regardless of
;          the dimensions of INIT, it is treated as a one-dimensional
;          array.  If OVERWRITE is not set, then INIT itself is
;          unmodified.
;
; KEYWORDS:
;
;   AT - a long integer indicating the position of the sub-array to be
;        deleted.  If AT is non-negative, then the deleted portion
;        will be NEWARR[AT:AT+LENGTH-1].  If AT is negative, then it
;        represents an index counting from then *end* of INIT,
;        starting at -1L.
;        Default: 0L (deletion begins with first element).
;
;  LENGTH - a long integer indicating the number of elements to be
;           removed.  
;
;  OVERWRITE - if set, then INIT will be overwritten in the process of
;              generating the new array.  Upon return, INIT will be
;              undefined.
;
;  COUNT - upon return, the number of elements in the resulting array.
;          If all of INIT would have been deleted, then -1L is
;          returned and COUNT is set to zero.
;
;  EMPTY1 - if set, then INIT is assumed to be empty (i.e., to have
;           zero elements).  The actual value passed as INIT is
;           ignored.
;
; RETURNS:
;
;   The new array, which is always one-dimensional.  If COUNT is zero,
;   then the scalar -1L is returned.
;
; SEE ALSO:
;
;   STORE_ARRAY in IDL Astronomy Library
;
; MODIFICATION HISTORY:
;   Written, CM, 02 Mar 2000
;   Added OVERWRITE and EMPTY1 keyword, CM 04 Mar 2000
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
function arrdelete, init, at=at0, length=len0, count=count, $
                    empty1=empty1, overwrite=overwrite

  on_error, 2
  n1 = n_elements(init)   & sz1 = size(init)    & tp1 = sz1(sz1(0)+1)
  count = n1
  if (count EQ 0 OR keyword_set(empty1)) then return, -1L

  if n_elements(at0)  EQ 0 then at  = 0L else at  = long(at0(0))
  if n_elements(len0) EQ 0 then len = 1L else len = long(len0(0))
  if at LT 0 then at = (n1 + 1L + at) > 0
  at = (at > 0) < n1

  if at + len GT count then len = (count - at) > 0
  if len LE 0 then return, init
  count = n1 - len
  if len GE n1 then return, -1L

  if keyword_set(overwrite) then begin

      ;; Conserve memory as much as possible

      if at EQ 0 then      return, (temporary(init))(len:*)
      if at EQ n1-len then return, (temporary(init))(0:n1-len-1)
      if at LT n1/2 then begin       ;; Minimize the memory copying
          init(len) = init(0:at-1)
          return, (temporary(init))(len:*)
      endif else begin
          init(at) = init(at+len:*)
          return, (temporary(init))(0:count-1)
      endelse
  endif

  ;; Normal memory-hoggy part of the routine
  if tp1 EQ 8 then out = make_array(value=init(0), count) $
  else             out = make_array(type=tp1, count, /nozero)

  if at GT 0 then      out(0)  = init(0:at-1)
  if at+len LT n1 then out(at) = init(at+len:*)

  return, out
end

