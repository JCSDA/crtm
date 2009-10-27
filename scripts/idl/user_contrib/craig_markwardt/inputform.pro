;+
; NAME:
;   INPUTFORM
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Generates expression string from an IDL value
;
; CALLING SEQUENCE:
;   STRING = INPUTFORM(VALUE, ERRMSG=ERRMSG, STATUS=STATUS, ...)
;
; DESCRIPTION: 
;
;   The INPUTFORM function converts an IDL data value into its string
;   representation, suitable for execution at the IDL command line or
;   with EXECUTE().  This is similar to the "InForm" output
;   representation of Mathematica, which formats output so that it can
;   be entered again on the command line.  INPUTFORM() is a
;   specialized form of STRING().
;
;   For example, the value DBLARR(2,2) has the representation
;
;      '[[0,0],[0,0]]'
;
;   The formal goal of INPUTFORM is for the resulting textual
;   expression to be an exact represetation of the original data.
;   Therefore, given the original value VARIABLE, then after executing
;
;      R = EXECUTE( 'variable1 = '+INPUTFORM(variable) )
;
;   The value, type, and dimension of VARIABLE1 and VARIABLE will be
;   the same.
;
;   Such behavior might useful in several circumstances:
;
;      * for printing values meant to be "pasted" back into the
;        command line by the user;
;      * for constructing command arguments to be EXECUTE()'d;
;      * for saving values in ASCII format for later execution.
;   
;   However, it should be noted that the IDL parser is not perfect.
;   While IDL has many data types, not all expressions are
;   representable as a textual string.  Pointers and objects can be
;   represented.  Examples of the parser limitation include,
;
;      * array expressions can have no more than 90 elements;
;      * bracketed array notation cannot be nested too deeply;
;      * anonymous structure arrays have no textual representation;
;
;   Given these limitations, the user of this routine must be prepared
;   for failure and have contingency plans.  Error messages and status
;   indicators are provided to facilitate this.  INPUTFORM() does not
;   call MESSAGE, so it should never intentionally crash.
;
;   Also, consider that the textual representation can never really be
;   suitable for very large arrays.  The internal algorithm is thus
;   not optimized for speed as heavily numeric routines might be, and
;   instead tries to make the output slightly more readable.
;
; INPUTS:
;
;   VALUE - the IDL value to be converted.  Any value which has a
;           legal textual representation is permitted.
;
; KEYWORDS:
;
;   STATUS - upon return, a status indicator.  A value of zero
;            indicates failure; one indicates success.
;
;   ERRMSG - upon return, a string message indicating the reason for a
;            failure, if any.  The empty string ('') indicates
;            success.
;
;   MAX_ELEMENTS - maximum number of elements permitted in VALUE.  The
;                  conversion fails if the maximum is exceeded.
;                  Default: any number of elements is permitted.
;
;                  NOTE: the conversion may still fail if any array
;                  dimension exceeds 90.
;
;   MAX_DIMENSIONS - maximum number of array dimensions permitted in
;                    VALUE.  The conversion fails if the maximum is
;                    exceeded.
;                    Default: any number of dimensions is permitted.
;
;                    NOTE: IDL does not permit deep nesting, for
;                    dimensions greater than three.
;
;   MAX_TAGS - maximum number of structure tags permitted in VALUE.
;              The conversion fails if the maximum is exceeded.
;              Default: any number of tags is permitted.
;
;   MAX_LEN - approximate maximum length of returned string.  If large
;             string expressions exceed this size as they are being
;             composed internally, they will be terminated by a '...'
;             ellipsis and returned.  This value is to be used as a
;             guideline by INPUTFORM(); the precise limit may not be
;             adhered to.
;             Default: 16384L
;            
; RETURNS:
;   The resulting converted string, if successful.  Upon failure,
;   STATUS is set to zero and the empty string ('') is returned.
;
; EXAMPLE:
;   
;   Convert a double array to text
;   IDL> x = dblarr(2,2)
;   IDL> print, inputform(x)
;   [[0D,0],[0D,0]]
;
;   Convert a structure
;   IDL> x = create_struct('s1',5,'s2','strvalue','s3',intarr(3))
;   IDL> print, inputform(x)
;   [{S1:5,S2:'strvalue',S3:[0,0,0]}]
;    
;
; SEE ALSO:
;
;   STRING, PRINT, HELP, HELPFORM
;
; MODIFICATION HISTORY:
;   Written, CM, 10 Apr 2000
;   Added HELPFORM to SEE ALSO, CM, 04 Jul 2000
;   Corrected case of scalar float value, CM, 13 Jul 2000
;   Put a space after float types like 1E or 1D to ease parsing, CM,
;     18 Jul 2000
;   Add ability to print INPUTFORM of pointers, CM, 09 Dec 2002
;   Add ability to print INPUTFORM of object pointers, CM, 01 Oct 2003
;   Bug fix: actually obey MAX_ELEMENTS (was being ignored), CM, 22
;     Oct 2006
;   Change to square-bracket array syntax, CM, 27 Feb 2007
;
;  $Id$
;
;-
; Copyright (C) 2000,2001,2002,2003,2006,2007 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;; Forward declarations of functions, for goodness's sake
forward_function inputform_int, inputform_float, inputform_string, $
  inputform_struct, inputform_basic, inputform

;; Convert an integer style value to a string
function inputform_int, x, format
  n = n_elements(x)
  ;; Construct format like (N(format,:,","))
  fmt = '('+strtrim(n,2)+'('+format+',:,","))'
  return, string(x, format=fmt)
end

;; Convert a floating style value to a string.  Note the conversion
;; happens twice, once as a E and once as a G.  The shortest correct
;; version of the two is used.
function inputform_float, x, format, dconvert=dcon
  n = n_elements(x)
  str = string(x[*], format=format)
  sz = size(x) & tp = sz[sz[0]+1]

  ;; Sorry, there appears to be no other way to make nice looking
  ;; floating point numbers.
  str1 = string(x[*], format='(G0)')
  if tp EQ 4 then x1 = float(str1)
  if tp EQ 5 then x1 = double(str1)
  wh = where(x-x1 EQ 0, ct)
  if ct GT 0 then str[wh] = str1[wh]
  str1 = 0
  str = strtrim(str,2)

  p = strpos(str[0], 'E')  ;; Make sure at least one element is float-type
  ;; Note, the space is needed in case the string is placed inside
  ;; another expression down the line.
  if p LT 0 then begin
      if keyword_set(dcon) then str[0] = str[0] + 'D' $
      else str[0] = str[0] + 'E'
  endif
  if keyword_set(dcon) then begin
      ;; Convert from floating to double
      p = strpos(str, 'E')
      wh = where(p GE 0, ct)
      for i = 0L, ct-1 do begin
          str1 = str(wh[i])
          strput, str1, 'D', p[wh[i]]
          str[wh[i]] = str1
      endfor
  endif
  ;; Construct format like (N(A,:,","))
  fmt = '('+strtrim(n,2)+'(A,:,","))'
  return, string(str, format=fmt)
end

;; Convert a string to a string.  This means protecting against stray
;; quotation marks.
function inputform_string, x
  x1 = x
  n = n_elements(x1)

  ;; Strings must be protected against having quotation marks within
  ;; themselves
  wh = where(strpos(x1, "'") GE 0, ct)
  if ct GT 0 then begin
      for i = 0L, ct-1 do begin
          x2 = x1[wh[i]]
          ;; Find each quotation mark and replace it
          p = strpos(x2, "'")
          while p GE 0 do begin
              l = strlen(x2)
              if p GE 0 then x2 = strmid(x2, 0, p)+"'"+strmid(x2, p, l-p)
              p = strpos(x2, "'", p+2)
          endwhile
          x1[wh[i]] = x2
      endfor
  endif

  ;; Now protected, the strings can be joined
  fmt = '('+strtrim(n,2)+'("''",A,"''",:,","))'
  return, string(x1, format=fmt)
end

;; Convert a structure type.  Recursive calls to inputform() are
;; performed to convert the internal tag values.
function inputform_struct, data, status=status, errmsg=errmsg
  n = n_elements(data)
  s0 = ''
  tn = tag_names(data)
  sn = tag_names(data, /structure_name)
  for i = 0L, n-1 do begin
      s = '{'
      ;; Open braces and add structure name if possible
      if sn NE '' then s = s + sn + ','
      comma = ''
      for j = 0L, n_elements(tn)-1 do begin
          ;; Add each tag
          status = 0
          s = s + comma + tn[j] + ':' + $
            inputform(data[i].(j), status=status, errmsg=errmsg, max_dim=2)
          if status NE 1 then return, ''
          comma = ','
      endfor
      s = s + '}'
      if i NE n-1 then s = s + ','
      s0 = s0 + s
  endfor
  status = 1
  return, s0
end

;; Convert pointer
function inputform_ptr, x, tp
  nel = n_elements(x)

  ;; Convert to string representation, then fish out the integers
  strep = string(x, /print)
  stb = byte(strep)
  st0 = stb*0b + 32b
  ;; Fish out the integers...
  wh = where(stb GE (byte('0'))[0] AND stb LE (byte('9'))[0], ct)
  if ct GT 0 then st0[wh] = stb[wh]
  
  ;; .. but also replace Nulls with 0 and '>' with commas
  wh = where(stb EQ (byte('>'))[0], ct)
  if ct GT 0 then st0[wh] = (byte(','))[0]
  wh = where(stb EQ (byte('N'))[0], ct)
  if ct GT 0 then st0[wh] = (byte('0'))[0]

  sti = strcompress(string(st0),/remove_all)
  dummy = execute('ind = [0L,'+sti+'0L]')
  ind = ind[1:nel]

  ;; Convert to a list of pointers using PTR_VALID and /CAST
  if tp EQ 10 then fun = 'ptr' else fun = 'obj'
  format = '('+strtrim(nel,2)+'("'+fun+'_valid(",I0,",/cast)",:,","))'
  stf = string(ind, format=format)

  return, stf
end

;; Convert basic types
function inputform_basic, x, status=status, errmsg=errmsg, si=si

  s = ''
  si = ''
  status = 1
  sz = size(x)
  tp = sz[sz[0]+1]
  case (tp) of 
      1:  s = inputform_int(x, '(I0,"B")')      ;; BYTE
      2:  s = inputform_int(x, '(I0)')          ;; INTEGER
      3:  s = inputform_int(x, '(I0,"L")')      ;; LONG
      4:  s = inputform_float(x, '(E)')         ;; FLOAT
      5:  s = inputform_float(x, '(E)', /dconv) ;; DOUBLE
      7:  s = inputform_string(x)               ;; STRING
      10: s = inputform_ptr(x,10)               ;; POINTER
      11: s = inputform_ptr(x,11)               ;; OBJPTR
      12: s = inputform_int(x, '(I0,"U")')      ;; UNSIGNED INTEGER
      13: s = inputform_int(x, '(I0,"UL")')     ;; UNSIGNED LONG
      14: s = inputform_int(x, '(I0,"LL")')     ;; LONG64
      15: s = inputform_int(x, '(I0,"ULL")')    ;; UNSIGNED LONG64
      
      6: begin ;; COMPLEX
          s = inputform_float(float(x), '(E)')
          si = inputform_float(imaginary(x), '(E)')
      end
      9: begin ;; DCOMPLEX
          s = inputform_float(double(x), '(E)', /dconv)
          si = inputform_float(imaginary(x), '(E)', /dconv)
      end
      
      8: begin ;; STRUCTURE
          s = inputform_struct(x, status=status, errmsg=errmsg)
          if status EQ 0 then return, ''
      end

      else: return, ''
  end

  return, s
end

;; Main routine
function inputform, data, errmsg=errmsg, status=status, max_elements=nmax, $
                    max_dimensions=nmaxd, max_tags=nmaxt, max_len=nmaxl, $
                    nocatch=nocatch

  status = 0
  expr = ''
  errmsg = ''

  ;; General error catching, in case we didn't get everything
  catcherr = 0
  if NOT keyword_set(nocatch) then catch, catcherr
  if catcherr NE 0 then begin
      catch, /cancel
      status = 0
      expr = ''
      if errmsg EQ '' then errmsg = 'An unknown conversion error occurred'
      return, expr
  endif

  sz = size(data)
  typenames = ['UNDEFINED', 'BYTE', 'INTEGER', 'LONG', 'FLOAT', 'DOUBLE', $
               'COMPLEX', 'STRING', 'STRUCTURE', 'DCOMPLEX', 'POINTER', $
               'OBJECT', 'UNSIGNED INTEGER', 'UNSIGNED LONG', $
               'LONG64', 'UNSIGNED LONG64', 'UNKNOWN']

  ;; Certain types have *no* representation
  ndims = sz[0]
  tp = sz[ndims+1]
  if (tp EQ 0) OR (tp GT 15) then begin
      errmsg = 'Type '+typenames[tp<16]+' has no input representation'
      return, expr
  endif

  ;; Don't convert arrays that are too large
  if n_elements(nmax) EQ 0 then nmax = n_elements(data)
  if n_elements(data) GT nmax[0] then begin
      errmsg = 'DATA array has too many elements'
      return, expr
  endif

  ;; Arrays cannot be too big, or have anonymous structures
  MAXLEN = nmax
  if ndims GT 0 then begin
      if max(sz[1:ndims]) GT MAXLEN then begin
          errmsg = string(MAXLEN, $
            format='("Array type is too large (>",I0," elements per dim)")')
          return, expr
      endif
      ;; Structure cannot be anonymous
      if n_elements(data) GT 1 AND tp EQ 8 then begin
          if tag_names(data[0], /structure) EQ '' then begin
              errmsg = 'Arrays of anonymous structures are not permitted'
              return, expr
          endif
      endif
  endif

  odims = 1L     ;; "OUTER" dimensions 
  fdims = sz[1]  ;; "INNER" dimensions
  if ndims EQ 0 then fdims = 1L
  if ndims GE 2 then for i = 2, ndims do odims = odims * sz[i]

  ;; Look for the maximum number of dimensions or structure tags
  if n_elements(nmaxd) GT 0 then if ndims GT nmaxd[0] then begin
      errmsg = 'Array has too many dimensions'
      return, expr
  endif
  if tp EQ 8 AND n_elements(nmaxt) GT 0 then $
    if n_elements(tag_names(data[0])) GT nmaxt[0] then begin
      errmsg = 'Structure has too many tags'
      return, expr
  endif

  ;; Create a nicer array to work with
  x = reform([data], fdims, odims)
  ss  = '' & ssi = ''

  ;; If just a scalar or 1D array, then, convert it and go on
  if ndims LE 1 then begin
      ss = inputform_basic(data, si=ssi, status=status, errmsg=errmsg)
      if status EQ 0 then return, expr
      if ndims EQ 0 then goto, FINAL_COMPOSE
      ss  = '[' + ss +  ']'
      ssi = '[' + ssi + ']'
      goto, FINAL_COMPOSE
  endif

  ;; Otherwise it is a 2D array or more
  dims = sz[1:ndims]
  xdims = dims[1:*]
  for i = 1, ndims-2 do xdims[i] = xdims[i]*xdims[i-1]
  
  comma = ''

  for i = 0L, odims-1 do begin

      xx = x[*,i]

      ;; Add any opening brackets
      s = '['
      wh = where((i MOD xdims) EQ 0, nbr)
      if nbr GT 0 then s = s + string(replicate(byte('['),nbr))

      ;; Add representation itself
      ss  = ss  + comma + s + inputform_basic(xx, si=si, $
                                              errmsg=errmsg, status=status)
      ssi = ssi + comma + s + si
      if status EQ 0 then return, expr

      ;; Add closing brackets
      s = ']'
      wh = where(((i+1) MOD xdims) EQ 0, nbr)
      if nbr GT 0 then s = s + string(replicate(byte(']'),nbr))

      ssi = ssi + s
      ss  = ss  + s
      comma = ','
      if n_elements(nmaxl) GT 0 then $
        if strlen(ss)+strlen(ssi) GT nmaxl[0] then begin
          ss = ss + '...'
          ssi = ssi + '...'
          goto, FINAL_COMPOSE
      endif

  endfor

  FINAL_COMPOSE:
  ;; Merge real and imaginary parts together
  if tp EQ 6 then ss =  'COMPLEX('+ss+','+ssi+')'
  if tp EQ 9 then ss = 'DCOMPLEX('+ss+','+ssi+')'
  s = ''
  ;; Final dimensions can be lost if they are not reformed
  if ndims GT 1 then begin
      for j = ndims-1, 0, -1 do begin
          if dims[j] NE 1 then goto, DONE_DCHECK
      endfor
      DONE_DCHECK:
      if j NE ndims-1 then $
        ss = 'REFORM('+ss+','+inputform(dims)+')'
  endif

  ;; Return
  expr = ss
  status = 1
  return, expr
end
