;+
; NAME:
;   DXHELP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Perform HELP equivalent at any point in IDL call stack
;
; CALLING SEQUENCE:
;   DXHELP, X0, X1, ..., [ LEVEL=LEVEL ]
;
; DESCRIPTION: 
;
;   DXHELP performs the equivalent of HELP for the variables at any
;   level in the IDL call stack.
;
;   The call level to be examined is determined by the current
;   debugging "focus."  By default this is the deepest level in the
;   call stack -- where the breakpoint occurred.  However, this level
;   can be changed by using the DXUP and DXDOWN procedures.
;
;   If the ALL keyword is set, then all variables are examined.
;
; INPUTS:
;
;   Xi - variables to be examined, either quoted or unquoted.
;        Non-string expressions are diagnosed, but of course refer to
;        the deepest call level.  If the ALL keyword is set then the
;        Xi parameters are ignored.
;
; KEYWORDS:
;
;   LEVEL - the call level to be examined, if not the current
;           debugging focus.
;
;   ALL - if set, then all variables at the current focus level are
;         examined.
;
;
; EXAMPLE:
;
;   dxhelp
;
;   Print all variables at current debugging focus level
;
; SEE ALSO:
;
;   DXUP, DXDOWN, DXHELP, DXPRINT
;
; MODIFICATION HISTORY:
;   Written, 15 Apr 2000
;   Added ALL keyword; changed N_PARAMS() EQ 0 behavior, CM 17 Apr
;     2000
;   DXHELP_VALUE now prints correct string and byte values, CM 23 Apr
;     2000
;   Add support for printing structures with FULL_STRUCT, CM 08 Feb
;     2001
;   Added forward_function for DXHELPFORM, CM 08 Apr 2001
;   Print more info about POINTER type, CM 30 Apr 2001
;   
;
;   $Id$
;-
; Copyright (C) 2000-2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; INCLUDED FROM HELPFORM.PRO
forward_function dxhelpform
function dxhelpform, name0, value, single=single, shortform=short, $
                     width=width0, tagform=tagform, structure=struct, $
                     _EXTRA=extra

  ;; Names of all the known IDL types, as of IDL 5.2
  typenames = ['UNDEFINED', 'BYTE', 'INT', 'LONG', 'FLOAT', 'DOUBLE', $
               'COMPLEX', 'STRING', 'STRUCT', 'DCOMPLEX', 'POINTER', $
               'OBJREF', 'UINT', 'ULONG', $
               'LONG64', 'ULONG64', 'UNKNOWN']
  blanks = string(replicate(32b,80))
  if n_elements(sz) LT 3 then sz = size(value)
  tp = sz(sz(0)+1) < 16

  if n_elements(name0) EQ 0 then name0 = ''
  name = strtrim(name0(0),2)

  nlen = 15  ;; Length of name
  tlen = 9   ;; Length of type name

  if n_elements(width0) EQ 0 then width0 = 80
  width = floor(width0(0))

  if tp EQ 8 AND keyword_set(struct) then begin
      sz1 = size(value)
      if sz1(sz1(0)+1) NE 8 then goto, NOT_STRUCT
      nt = n_tags(value)
      len = n_tags(value, /length)
      tn = tag_names(value)
      sn = tag_names(value, /structure_name)
      
      if sn EQ '' then sn = '<Anonymous>'

      a = string(sn, nt, len, $
             format='("** Structure ",A0,", ",I0," tags, length=",I0,":")')

      for i = 0, nt-1 do begin
          a = [a, '   '+dxhelpform(tn(i), value(0).(i), /tagform)]
      endfor

      return, a
  endif
  NOT_STRUCT:

  if NOT keyword_set(short) then begin
      ;; Pad the name out, or else put the name on a line by itself
      if strlen(name) GT nlen then begin
          if keyword_set(single) then begin
              a1 = name+' '
          endif else begin
              a0 = name
              a1 = strmid(blanks,0,nlen)+' '
          endelse
      endif else begin
          a1 = strmid(name+blanks,0,nlen)+' '
      endelse
      
      a1 = a1 + strmid(typenames(tp)+blanks,0,tlen)
      if NOT keyword_set(tagform) then $
        a1 = a1 +' = '        
  endif else begin
      a1 = strmid(typenames(tp)+blanks,0,tlen)
  endelse

  ndims = sz(0)
  if ndims GT 0 then begin
      ;; It is an array, compose the dimensions
      dims = sz(1:ndims)  
      v = 'Array['
      for i = 0L, ndims-1 do begin
          v = v + strtrim(dims(i),2)
          if i LT ndims-1 then v = v + ', '
      endfor
      v = v + ']'

      ;; If it is a structure, add the structure name (structures are
      ;; never scalars)
      if NOT keyword_set(short) AND tp EQ 8 then begin
          ;; Protect against empty value
          if n_elements(stname) EQ 0 then begin
              if n_elements(value) GT 0 then v0 = value(0) else v0 = {dummy:0}
              sn = tag_names(v0, /structure_name)
              sn = sn(0)
          endif else begin
              sn = strtrim(stname(0),2)
          endelse
          if sn EQ '' then sn = '<Anonymous>'
          v = '-> '+sn+' ' + v 
      endif
  endif else begin
      ;; It is a scalar

      ;; Protect against empty or vector value
      if n_elements(value) GT 0 then begin
          v0 = value(0) 
      endif else begin
          if tp NE 10 AND tp NE 11 then tp = 0
      endelse

      case tp < 16 of 
          0:  v = '<Undefined>'
          1:  v = string(v0, format='(I4)')
          7:  begin
              w = (width - 35) > 5
              if strlen(v0) GT w then $
                v = "'"+strmid(v0,0,w)+"'..." $
              else $
                v = "'"+v0+"'"
          end
          10: begin
              sz = size(v0)
              if sz(sz(0)+1) EQ 10 then v = string(v0(0), /print) $
              else                      v = '<PtrHeapVar>'
          end
          11: begin
              if n_elements(stname) EQ 0 then begin
                  forward_function obj_class
                  sz = size(v0)
                  if sz(sz(0)+1) EQ 11 then sn = '('+obj_class(v0)+')' $
                  else                      sn = ''
              endif else begin
                  sn = '('+strupcase(strtrim(stname(0),2))+')'
              endelse
              v = '<ObjHeapVar'+sn+'>'
          end
          16: v = ''
          else: v = string(v0)
      endcase
  endelse

  if keyword_set(short) then return, a1 + '('+v+')'

  a1 = a1 + v
  if n_elements(a0) GT 0 then return, [a0, a1] else return, a1
end
; END INCLUDE

pro dxhelp, x0, x1, x2, x3, x4, x5, x6, x7, x8, x9, level=level0, all=all, $
            _EXTRA=extra
@dxcommon.pro

  dxlreset
  if n_elements(level0) EQ 0 then level0=dblevel
  level = floor(level0(0))

  ;; Print the current debugging levels
  dxplevel, level=level, current=keyword_set(all)

  ;; Two different behaviors, depending on whether parameters are passed.
  if keyword_set(all) then begin
      
      ;; ALL was set... we extract the names of the variables from the
      ;; procedure itself.
      vars = routine_names(variables=level)
      if n_elements(vars) EQ 1 then $
        if vars(0) EQ '' then return

      for i = 0L, n_elements(vars)-1 do begin
          ;; Retrieve the variable's value, but make sure it is not
          ;; undefined
          name = vars(i)
          val = 0
          if name EQ '' then name = '<Expression>'
          sz = size(routine_names(vars(i), fetch=level))
          dummy = temporary(val)
          if sz(sz(0)+1) NE 0 then val = routine_names(vars(i), fetch=level)
          print, dxhelpform(name, val, _EXTRA=extra), format='(A)'
      endfor
  endif else begin

      ;; ALL was not set, so we examine individual arguments
      if n_params() EQ 0 then return

      thislev = routine_names(/level)
      for i = 0L, n_params()-1 do begin

          ;; First, extract the parameter name using ROUTINE_NAMES magic
          name = ''
          ii = strtrim(i,2)
          cmd = 'name = routine_names(x'+ii+',arg_name=thislev-1)'
          if execute(cmd) NE 1 then goto, NEXT_PARM
          if n_elements(name) LT 1 then goto, NEXT_PARM
          name0 = name(0)
          name = name0
          if name0 EQ '' then begin
              ;; The value might be a quoted string... see if it is!
              cmd = 'val = x'+ii
              if execute(cmd) NE 1 then goto, NEXT_PARM

              sz = size(val)
              if sz(sz(0)+1) EQ 7 then begin
                  ;; It was a string!
                  name0 = val
                  goto, GET_VAL
              endif
              name = '<Expression>'
              val = 0
          endif else begin
              GET_VAL:
              ;; Retrieve the value, again guarding against undefined values
              sz = size(routine_names(name0, fetch=level))
              val = 0
              dummy = temporary(val)
              if sz(sz(0)+1) NE 0 then val = routine_names(name0, fetch=level)
          endelse

          ;; Print it
          print, dxhelpform(name, val, _EXTRA=extra), format='(A)'
          NEXT_PARM:
      endfor
  endelse

end
