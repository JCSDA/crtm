;+
; NAME:
;   MULTISORT
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Perform a sorting operation with multiple sort keys
;
; CALLING SEQUENCE:
;   INDICES = MULTISORT(KEY1, KEY2, ..., [/L64, ], [ORDER=order])
;
; DESCRIPTION: 
;
;   The function MULTISORT performs a sorting operation with multiple
;   sort keys.  Unlike the IDL built-in SORT() function, which can
;   only sort a single key, MULTISORT can accept multiple keys.  In
;   cases where the primary key is equal, the sort order is based on
;   any secondary keys provided.  The return value is an array of
;   indices which will place the key arrays into sorted order.
;
;   MULTISORT works by building an internal sort key string which can
;   be sorted in a single pass.  Because MULTISORT is not a built-in
;   function, and because it must build these auxiliary strings, it
;   cannot be as fast or memory-efficient as the built-in function.
;   Users will need several times more memory than the memory used
;   to store just the input keys.
;
;   MULTISORT() allows the user to choose the sort order for each key
;   separately.  The ORDER keyword is an N-vector, one order for each
;   input key.  ORDER[i] is +1 to sort KEYi ascending, and ORDER[i] is
;   -1 to sort KEYi descending.
;
; INPUTS:
;
;   KEY1, KEY2, ... KEY9 - input sort keys.  Any integer, floating
;                          point or string value is allowed.  The
;                          number of values must be the same for each
;                          key.
;
;
;
; KEYWORDS:
;
;   ORDER - an N-vector, giving the sort order for each key (see
;           documentation above).
;           Default: REPLICATE(+1,N_PARAMS())  (all keys ascending)
;
;   L64 - if set, then return a LONG64 index instead of LONG.
;
; RETURNS:
;
;   An array of indices which will place the keys into sorted order.
;   I.e., KEYS1[INDICES],  KEYS2[INDICES] ... will be in order.
;
; SEE ALSO:
;
;   SORT
;
; MODIFICATION HISTORY:
;   Written, CM, Jun 2007
;   Document the encoding format, and make some floating point
;     operations more efficient, CM, Jan 2008
;   Correct several bugs in the handling of floating point numbers
;     in the range -1.0 to 1.0, made more efficient, (thanks to Eric
;     Jensen);  I also saved some test cases; CM, Jul 2008
;
;
;  $Id$
;
;-
; Copyright (C) 2007, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; ========================================================
; Utility function to transform an integer into a string key
;  Integers are coded in hexidecimal, with a prefix of
;  'A' for negative and 'B' for positive.
function multisort_intkey, x, len, unsigned=u, order=order
  COMPILE_OPT strictarr
  n = n_elements(x)
  if order LT 0 then x1 = NOT temporary(x) $ ;; Reverse order
  else x1 = temporary(x)

  ;; v = [-2L^31L,-32768,-2000,-20,-1,0,1,20,2000,32767,(-2L^31L)-1]

  slen = strtrim(len,2)
  fmt = '(Z'+slen+'.'+slen+')'  ;; (In.n)  - zero-padded
  if NOT keyword_set(u) then begin
      prestr = strarr(n)+'B' ;; Prefix to indicate positive values
      wh = where(x1 LT 0, ct)
      if ct GT 0 then prestr[wh] = 'A'

      bmask = ishft((x1[0] AND 0b) + 1b,4*len)-1
      if bmask LE 1 then bmask = NOT (x1[0] AND 0b)
      x1 = x1 AND bmask

      return, temporary(prestr)+string(x1,format=fmt)
  endif else begin
      return,                   string(x1,format=fmt)
  endelse

end

; ========================================================
; Utility function to transform a float into a string key
; Floating point numbers are converted to IEEE format,
; and then examined as integers, allowing MULTISORT_INTKEY
; to be used.
;
function multisort_fltkey, x1, type, order=order
  COMPILE_OPT strictarr
  n = n_elements(x1)

  if type EQ 4 then begin
      ;; Floating point data (4 bytes)

      ;; Test case
      ;; v = [-!values.f_infinity,-2000.,-20,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,20,2000,!values.f_infinity,!values.f_nan]
      byteorder, x1, /ftoxdr
      x1 = long(temporary(x1),0,n)
      byteorder, x1, /ntohl
      wh = where(x1 LT 0, ct)
      if ct GT 0 then x1[wh] = x1[wh] XOR '7fffffff'xl

      return, multisort_intkey(x1,8,order=order)

  endif else begin
      ;; Double precision data

      ;; Test case
      ;; v = [-!values.d_infinity,-2000d,-20,-1.5,-1.0,-0.5,0,0.5,1.0,1.5,20,2000,!values.d_infinity,!values.d_nan]
      byteorder, x1, /dtoxdr
      x1 = long64(temporary(x1),0,n)
      byteorder, x1, /l64swap, /swap_if_little
      wh = where(x1 LT 0, ct)
      if ct GT 0 then x1[wh] = x1[wh] XOR '7fffffffffffffff'xll

      return, multisort_intkey(x1,16,order=order)

  endelse

end

; ========================================================
; Utility function to transform a string into a string key
function multisort_strkey, x, order=order
  COMPILE_OPT strictarr
  len = strlen(x)
  maxlen = max(len, min=minlen)
  if maxlen GT minlen then begin
      ;; Pad out to the maximum string length (i.e. left-align the strings)
      pad = string(bytarr(maxlen-minlen)+32b)
      key = strmid(x+pad,0,maxlen)
  endif else begin
      key = x
  endelse

  ;; Reverse order if requested
  if order LT 0 then begin
      key = string( (255b - byte(temporary(key))) > 1b )
  endif

  return, key
end


; ========================================================
; MAIN ROUTINE
; ========================================================
function multisort, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, L64=L64, $
                    keys=keys0, order=order0, no_builtin=nobuiltin

  COMPILE_OPT strictarr

  nkeys = n_params()
  if nkeys EQ 0 then begin
      message, 'USAGE: INDICES = MULTISORT(KEY1[,KEY2,KEY3,...])', /info
      return, -1L
  endif

  order = intarr(nkeys) + 1
  if n_elements(order0) GT 0 then order[0] = round(order0)

  ;; Special case: only one term, no need to do complicate sort key
  ;; manipulations.
  if nkeys EQ 1 AND order[0] EQ +1 AND NOT keyword_set(nobuiltin) then begin
      return, sort(x0, L64=L64)
  endif

  ;; Master key
  mkey = ''
  for i = 0, nkeys-1 do begin
      xi = 0 & dummy = temporary(xi)
      case i of
          0: xi = x0
          1: xi = x1
          2: xi = x2
          3: xi = x3
          4: xi = x4
          5: xi = x5
          6: xi = x6
          7: xi = x7
          8: xi = x8
          9: xi = x9
      endcase
      if n_elements(xi) EQ 0 then begin
          message, string(i,format='("ERROR: no data was in parameter X",I0)')
          return, -1L
      endif

      sz = size(xi)
      tp = sz[sz[0]+1]
      o = order[i]
      case tp of 
          1:  mkey = temporary(mkey) + multisort_intkey(temporary(xi),2,/u,o=o)  ;; BYTE
          2:  mkey = temporary(mkey) + multisort_intkey(temporary(xi),4,o=o)     ;; INT
          3:  mkey = temporary(mkey) + multisort_intkey(temporary(xi),8,o=o)     ;; LONG
          4:  mkey = temporary(mkey) + multisort_fltkey(temporary(xi),4,o=o)     ;; FLOAT
          5:  mkey = temporary(mkey) + multisort_fltkey(temporary(xi),5,o=o)     ;; DOUBLE
          7:  mkey = temporary(mkey) + multisort_strkey(temporary(xi),o=o)       ;; STRING
          12: mkey = temporary(mkey) + multisort_intkey(temporary(xi),4,/u,o=o)  ;; UINT
          13: mkey = temporary(mkey) + multisort_intkey(temporary(xi),8,/u,o=o)  ;; ULONG
          14: mkey = temporary(mkey) + multisort_intkey(temporary(xi),16,o=o)    ;; LONG64
          15: mkey = temporary(mkey) + multisort_intkey(temporary(xi),16,/u,o=o) ;; ULONG64
          else: begin
              message, string(tp, i, $
                format='("ERROR: data type ",I0," for parameter X,",I0," is not sortable")')
              return, -1L
          end
      endcase

      xi = 0
  endfor

  return, sort(mkey, L64=L64)
end

                    
