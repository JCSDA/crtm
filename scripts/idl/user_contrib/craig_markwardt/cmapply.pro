;+
; NAME:
;   CMAPPLY
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Applies a function to specified dimensions of an array
;
; MAJOR TOPICS:
;   Arrays
;
; CALLING SEQUENCE:
;   XX = CMAPPLY(OP, ARRAY, DIMS, [/DOUBLE], [TYPE=TYPE])
;
; DESCRIPTION:
;   CMAPPLY will apply one of a few select functions to specified 
;   dimensions of an array.  Unlike some IDL functions, you *do* have
;   a choice of which dimensions that are to be "collapsed" by this
;   function.  Iterative loops are avoided where possible, for 
;   performance reasons.
;
;   The possible functions are:             (and number of loop iterations:)
;     +     - Performs a sum (as in TOTAL)       number of collapsed dimensions
;     AND   - Finds LOGICAL "AND" (not bitwise)  same
;     OR    - Finds LOGICAL "OR"  (not bitwise)  same
;     *     - Performs a product                 LOG_2[no. of collapsed elts.]
;
;     MIN   - Finds the minimum value            number of collapsed dimensions
;     MAX   - Finds the maximum value            same
;     MEDIAN- Finds the median  value            same
;
;     USER  - Applies user-defined function      no. of output elements
;
;
;   It is possible to perform user-defined operations arrays using
;   CMAPPLY.  The OP parameter is set to 'USER:FUNCTNAME', where
;   FUNCTNAME is the name of a user-defined function.  The user
;   defined function should be defined such that it accepts a single
;   parameter, a vector, and returns a single scalar value.  Here is a
;   prototype for the function definition:
;
;      FUNCTION FUNCTNAME, x, KEYWORD1=key1, ...
;         scalar = ... function of x or keywords ...
;         RETURN, scalar
;      END
;
;   The function may accept keywords.  Keyword values are passed in to
;   CMAPPLY through the FUNCTARGS keywords parameter, and passed to
;   the user function via the _EXTRA mechanism.  Thus, while the
;   definition of the user function is highly constrained in the
;   number of positional parameters, there is absolute freedom in
;   passing keyword parameters.
;
;   It's worth noting however, that the implementation of user-defined
;   functions is not particularly optimized for speed.  Users are
;   encouraged to implement their own array if the number of output
;   elements is large.
;
;
; INPUTS:
;
;   OP - The operation to perform, as a string.  May be upper or lower
;        case.
;
;        If a user-defined operation is to be passed, then OP is of
;        the form, 'USER:FUNCTNAME', where FUNCTNAME is the name of
;        the user-defined function.
;
;   ARRAY - An array of values to be operated on.  Must not be of type
;           STRING (7) or STRUCTURE (8).
;
; OPTIONAL INPUTS:
;
;   DIMS - An array of dimensions that are to be "collapsed", where
;          the the first dimension starts with 1 (ie, same convention
;          as IDL function TOTAL).  Whereas TOTAL only allows one
;          dimension to be added, you can specify multiple dimensions
;          to CMAPPLY.  Order does not matter, since all operations
;          are associative and transitive.  NOTE: the dimensions refer
;          to the *input* array, not the output array.  IDL allows a 
;          maximum of 8 dimensions.
;          DEFAULT: 1 (ie, first dimension)
;
; KEYWORDS:
;
;   DOUBLE - Set this if you wish the internal computations to be done
;            in double precision if necessary.  If ARRAY is double 
;            precision (real or complex) then DOUBLE=1 is implied.
;            DEFAULT: not set
;
;   TYPE - Set this to the IDL code of the desired output type (refer
;          to documentation of SIZE()).  Internal results will be
;          rounded to the nearest integer if the output type is an
;          integer type.
;          DEFAULT: same is input type
;
;   FUNCTARGS - If OP is 'USER:...', then the contents of this keyword
;               are passed to the user function using the _EXTRA
;               mechanism.  This way you can pass additional data to
;               your user-supplied function, via keywords, without
;               using common blocks.
;               DEFAULT: undefined (i.e., no keywords passed by _EXTRA)
;
; RETURN VALUE:
;
;   An array of the required TYPE, whose elements are the result of
;   the requested operation.  Depending on the operation and number of
;   elements in the input array, the result may be vulnerable to
;   overflow or underflow.
;
; EXAMPLES:
;   Shows how CMAPPLY can be used to total the second dimension of the
;   array called IN.  This is equivalent to OUT = TOTAL(IN, 2)
;
;   IDL> IN  = INDGEN(5,5)
;   IDL> OUT = CMAPPLY('+', IN, [2])
;   IDL> HELP, OUT
;   OUT             INT       = Array[5]
;
;   Second example.  Input is assumed to be an 5x100 array of 1's and
;   0's indicating the status of 5 detectors at 100 points in time.
;   The desired output is an array of 100 values, indicating whether
;   all 5 detectors are on (=1) at one time.  Use the logical AND
;   operation.
;
;   IDL> IN = detector_status             ; 5x100 array
;   IDL> OUT = CMAPPLY('AND', IN, [1])    ; collapses 1st dimension
;   IDL> HELP, OUT
;   OUT             BYTE      = Array[100]
;
;   (note that MIN could also have been used in this particular case,
;   although there would have been more loop iterations).
;
;   Third example.  Shows sum over first and third dimensions in an
;   array with dimensions 4x4x4:
;
;   IDL> IN = INDGEN(4,4,4)
;   IDL> OUT = CMAPPLY('+', IN, [1,3])
;   IDL> PRINT, OUT
;        408     472     536     600
;
;   Fourth example. A user-function (MEDIAN) is used:
;
;   IDL> IN = RANDOMN(SEED,10,10,5)
;   IDL> OUT = CMAPPLY('USER:MEDIAN', IN, 3)
;   IDL> HELP, OUT
;   OUT             FLOAT     = Array[10, 10]
;
;   (OUT(i,j) is the median value of IN(i,j,*))
;
; MODIFICATION HISTORY:
;   Mar 1998, Written, CM
;   Changed usage message to not bomb, 24 Mar 2000, CM
;   Signficant rewrite for *, MIN and MAX (inspired by Todd Clements
;     <Todd_Clements@alumni.hmc.edu>); FOR loop indices are now type
;     LONG; copying terms are liberalized, CM, 22, Aug 2000
;   More efficient MAX/MIN (inspired by Alex Schuster), CM, 25 Jan
;     2002
;   Make new MAX/MIN actually work with 3d arrays, CM, 08 Feb 2002
;   Add user-defined functions, ON_ERROR, CM, 09 Feb 2002
;   Correct bug in MAX/MIN initialization of RESULT, CM, 05 Dec 2002
;   Correct bug in CMAPPLY_PRODUCT implementation when there are an
;     odd number of values to combine, CM 26 Jul 2006
;   Add fast IDL versions of '*', 'MEDIAN', 'MIN' and 'MAX', where
;     IDL supports it, CM 26 Jul 2006
;
;  $Id$
;
;-
; Copyright (C) 1998, 2000, 2002, 2006, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Utility function, adapted from CMPRODUCT
function cmapply_product, x
  sz = size(x)
  n = sz(1)

  while n GT 1 do begin
      if (n mod 2) EQ 1 then x(0,*) = x(0,*) * x(n-1,*)
      n2 = floor(n/2)
      x = x(0:n2-1,*) * x(n2:n2*2-1,*)
      n = n2
  endwhile
  return, reform(x(0,*), /overwrite)
end

;; Utility function, used to collect collaped dimensions
pro cmapply_redim, newarr, dimapply, dimkeep, nkeep, totcol, totkeep
  sz = size(newarr)
  ;; First task: rearrange dimensions so that the dimensions
  ;; that are "kept" (ie, uncollapsed) are at the back
  dimkeep = where(histogram(dimapply,min=1,max=sz(0)) ne 1, nkeep)
  if nkeep EQ 0 then return

  newarr = transpose(temporary(newarr), [dimapply-1, dimkeep])
  ;; totcol is the total number of collapsed elements
  totcol = sz(dimapply(0))
  for i = 1, n_elements(dimapply)-1 do totcol = totcol * sz(dimapply(i))
  totkeep = sz(dimkeep(0)+1)
  for i = 1, n_elements(dimkeep)-1 do totkeep = totkeep * sz(dimkeep(i)+1)

  ;; this new array has two dimensions:
  ;;   * the first, all elements that will be collapsed
  ;;   * the second, all dimensions that will be preserved
  ;; (the ordering is so that all elements to be collapsed are
  ;;  adjacent in memory)
  newarr = reform(newarr, [totcol, totkeep], /overwrite)
end

;; Main function
function cmapply, op, array, dimapply, double=dbl, type=type, $
                  functargs=functargs, nocatch=nocatch

  if n_params() LT 2 then begin
      message, "USAGE: XX = CMAPPLY('OP',ARRAY,2)", /info
      message, '       where OP is +, *, AND, OR, MIN, MAX', /info
      return, -1L
  endif
  if NOT keyword_set(nocatch) then $
    on_error, 2 $
  else $
    on_error, 0

  version = double(!version.release)
;  version = 0
;  print, 'version = ',version

  ;; Parameter checking
  ;; 1) the dimensions of the array
  sz = size(array)
  if sz(0) EQ 0 then $
    message, 'ERROR: ARRAY must be an array!'

  ;; 2) The type of the array
  if sz(sz(0)+1) EQ 0 OR sz(sz(0)+1) EQ 7 OR sz(sz(0)+1) EQ 8 then $
    message, 'ERROR: Cannot apply to UNDEFINED, STRING, or STRUCTURE'
  if n_elements(type) EQ 0 then type = sz(sz(0)+1)

  ;; 3) The type of the operation
  szop = size(op)
  if szop(szop(0)+1) NE 7 then $
    message, 'ERROR: operation OP was not a string'

  ;; 4) The dimensions to apply (default is to apply to first dim)
  if n_params() EQ 2 then dimapply = 1
  dimapply = [ dimapply ]
  dimapply = dimapply(sort(dimapply))   ; Sort in ascending order
  napply = n_elements(dimapply)

  ;; 5) Use double precision if requested or if needed
  if n_elements(dbl) EQ 0 then begin
      dbl=0
      if type EQ 5 OR type EQ 9 then dbl=1
  endif

  newop = strupcase(op)
  newarr = array
  newarr = reform(newarr, sz(1:sz(0)), /overwrite)
  case 1 of

      ;; *** Addition
      (newop EQ '+'): begin
          for i = 0L, napply-1 do begin
              newarr = total(temporary(newarr), dimapply(i)-i, double=dbl)
          endfor
      end

      ;; *** Multiplication
      (newop EQ '*'): begin ;; Multiplication (by summation of logarithms)
          forward_function product

          cmapply_redim, newarr, dimapply, dimkeep, nkeep, totcol, totkeep
          if nkeep EQ 0 then begin
              newarr = reform(newarr, n_elements(newarr), 1, /overwrite)
              if version GT 5.55 then return, product(newarr)
              return, (cmapply_product(newarr))(0)
          endif

          if version GT 5.55 then begin
              result = product(newarr,1)
          endif else begin
              result = cmapply_product(newarr)
          endelse
          result = reform(result, sz(dimkeep+1), /overwrite)
          return, result
      end

      ;; *** LOGICAL AND or OR
      ((newop EQ 'AND') OR (newop EQ 'OR')): begin
          newarr = temporary(newarr) NE 0
          totelt = 1L
          for i = 0L, napply-1 do begin
              newarr = total(temporary(newarr), dimapply(i)-i)
              totelt = totelt * sz(dimapply(i))
          endfor
          if newop EQ 'AND' then return, (round(newarr) EQ totelt)
          if newop EQ 'OR'  then return, (round(newarr) NE 0)
      end

      ;; Operations requiring a little more attention over how to
      ;; iterate
      ((newop EQ 'MAX') OR (newop EQ 'MIN') OR (newop EQ 'MEDIAN')): begin
          cmapply_redim, newarr, dimapply, dimkeep, nkeep, totcol, totkeep
          if nkeep EQ 0 then begin
              if newop EQ 'MAX' then return, max(newarr)
              if newop EQ 'MIN' then return, min(newarr)
              if newop EQ 'MEDIAN' then return, median(newarr)
          endif

          ;; IDL 5.5 introduced the DIMENSION keyword to MAX() and MIN()
          if version GT 5.45 then begin
              extra = {dimension:1}
              if newop EQ 'MAX' then result = max(newarr, _EXTRA=extra)
              if newop EQ 'MIN' then result = min(newarr, _EXTRA=extra)
              if newop EQ 'MEDIAN' then result = median(newarr, _EXTRA=extra)
          endif else begin
              
              ;; Next task: create result array
              result = make_array(totkeep, type=type)
              
              ;; Now either iterate over the number of output elements, or
              ;; the number of collapsed elements, whichever is smaller.
              if (totcol LT totkeep) AND newop NE 'MEDIAN' then begin
                  ;; Iterate over the number of collapsed elements
                  result(0) = reform(newarr(0,*),totkeep,/overwrite)
                  case newop of 
                      'MAX': for i = 1L, totcol-1 do $
                        result(0) = result > newarr(i,*)
                      'MIN': for i = 1L, totcol-1 do $
                        result(0) = result < newarr(i,*)
                  endcase
              endif else begin
                  ;; Iterate over the number of output elements
                  case newop of 
                      'MAX': for i = 0L, totkeep-1 do result(i) = max(newarr(*,i))
                      'MIN': for i = 0L, totkeep-1 do result(i) = min(newarr(*,i))
                      'MEDIAN': for i = 0L, totkeep-1 do result(i) = median(newarr(*,i))
                  endcase
              endelse
          endelse

          result = reform(result, sz(dimkeep+1), /overwrite)
          return, result
      end

      ;; User function
      (strmid(newop,0,4) EQ 'USER'): begin
          functname = strmid(newop,5)
          if functname EQ '' then $
            message, 'ERROR: '+newop+' is not a valid operation'

          cmapply_redim, newarr, dimapply, dimkeep, nkeep, totcol, totkeep
          if nkeep EQ 0 then begin
              if n_elements(functargs) GT 0 then $
                return, call_function(functname, newarr, _EXTRA=functargs)
              return, call_function(functname, newarr)
          endif
          
          ;; Next task: create result array
          result = make_array(totkeep, type=type)
          
          ;; Iterate over the number of output elements
          if n_elements(functargs) GT 0 then begin
              for i = 0L, totkeep-1 do $
                result(i) = call_function(functname, newarr(*,i), _EXTRA=functargs)
          endif else begin
              for i = 0L, totkeep-1 do $
                result(i) = call_function(functname, newarr(*,i))
          endelse

          result = reform(result, sz(dimkeep+1), /overwrite)
          return, result
      end

              
  endcase

  newsz = size(newarr)
  if type EQ newsz(newsz(0)+1) then return, newarr

  ;; Cast the result into the desired type, if necessary
  castfns = ['UNDEF', 'BYTE', 'FIX', 'LONG', 'FLOAT', $
             'DOUBLE', 'COMPLEX', 'UNDEF', 'UNDEF', 'DCOMPLEX' ]
  if type GE 1 AND type LE 3 then $
    return, call_function(castfns(type), round(newarr)) $
  else $
    return, call_function(castfns(type), newarr)
end
  
