; docformat = 'rst'

;+
; Prints basic information about a routine.
;
; :Uses:
;    mg_termcolumns
;
; :Examples:
;    Try the main-level example at the end of this file::
;
;       IDL> .run man
;
;    This::
;
;       IDL> man, 'congrid'
;       Filename: /Applications/itt/idl71/lib/congrid.pro
;       result = congrid(arr, x, y, z, CENTER=CENTER, CUBIC=CUBIC, 
;         INTERP=INTERP, MINUS_ONE=MINUS_ONE)
;       IDL> man, 'mg_*range*'
;       Filename: /Users/mgalloy/projects/idllib/trunk/src/indices/mg_makerange.pro
;       result = MG_MAKERANGE(startvalue, stopvalue, INCREMENT=INCREMENT, N=N)
;
;       Filename: /Users/mgalloy/projects/idllib/trunk/src/analysis/mg_range.pro
;       result = MG_RANGE(var)
;-


;+
; Print header docs for a routine in a given file.
;
; :Params:
;    filename : in, required, type=string
;       filename of the .pro file
;    routinename : in, required, type=string
;       name of the routine to check docs for
;
; :Keywords:
;    is_function : in, optional, type=boolean
;       set to specify that the given routine is a function
;    output : in, out, optional, type=strarr
;       accumulation of output
;-
pro man_printdocs, filename, routinename, is_function=isFunction, output=output
  compile_opt strictarr
  
  nlines = file_lines(filename)
  lines = strarr(nlines)
  openr, lun, filename, /get_lun
  readf, lun, lines
  free_lun, lun
  
  type = keyword_set(isFunction) ? 'function' : 'pro'
  re = '^[[:space:]]*' + type + '[[:space:]]+' + routinename + '($|[[:space:],])'
  
  found = stregex(lines, re, /fold_case, /boolean)
  ind = where(found, count)
  
  if (count le 0L) then begin
    _output = 'Routine not found'
    output = n_elements(output) eq 0L ? [_output] : [output, _output]
  endif else begin
    routineStart = ind[0]
    headerPresent = strmid(lines[(routineStart - 1L) > 0L], 0, 2) eq ';-'
    if (headerPresent) then begin
      i = routineStart - 1L
      while (i ge 0L && strmid(lines[i], 0, 2) ne ';+') do i--
      if (i gt 0L) then begin
        _output = strmid(lines[i:ind[0] - 1L], 2)
        output = n_elements(output) eq 0L ? [_output] : [output, _output]
      endif
    endif
  endelse
end


;+
; Routine to resolve a given routine without crashing.
;
; :Returns:
;    1 if the routine was resolved, 0 if not
;
; :Params:
;    routine : in, required, type=string
;       name of routine to resolve
;
; :Keywords:
;    resolved : out, optional, type=boolean
;       set to a named variable to find out if the routine was resolved
;    _extra : in, optional, type=keywords
;       keywords to RESOLVE_ROUTINE
;-
pro man_resolveroutine, routine, resolved=resolved, _extra=e
  compile_opt strictarr, hidden

  oldQuiet = !quiet
  !quiet = 1
  
  resolved = 0B
    
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    !quiet = oldQuiet
    return
  endif
  
  if (strlowcase(routine) ne 'man_resolveroutine') then begin
    resolve_routine, routine, _extra=e
  endif
  
  resolved = 1B
  
  !quiet = oldQuiet
end


;+
; Wrapper for MG_TERMCOLUMNS in case it is not available.
;
; :Returns:
;    long
;-
function man_width
  compile_opt strictarr

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, 80L
  endif
  
  return, mg_termcolumns()
end


;+
; Print a string by splitting it across lines on spaces and indents every line
; except the first using the value of the INDENT keyword.
;
; :Params:
;    text : in, required, type=string
;       line to print
;
; :Keywords:
;    indent : in, optional, type=string, default='  '
;       string to prefix each line except the first
;    output : in, out, optional, type=strarr
;       accumulation of output
;-
pro man_print, text, indent=indent, output=output
  compile_opt strictarr
  
  _indent = n_elements(indent) eq 0L ? '  ' : indent
  _width = man_width()
  
  _text = text
  done = 0B
  while (~done) do begin
    line = strmid(_text, 0, _width)
    done = line eq _text
    if (done) then begin
      _output = line
      output = n_elements(output) eq 0L ? [_output] : [output, _output]
    endif else begin
      spacePos = strpos(line, ' ', /reverse_search)
      line = strtrim(strmid(line, 0, spacePos), 0)
      
      _output = line
      output = n_elements(output) eq 0L ? [_output] : [output, _output]
      
      _text = _indent + strmid(_text, spacePos + 1)
    endelse    
  endwhile
end


;+
; Checks to see if routine is in list.
;
; :Returns:
;    1 if routine in in list; 0 if not
;
; :Params:
;    list : in, required, type=strarr
;       list of routines to check against
;    routine : in, required, type=string
;       name of routine to check (case-insensitive)
;-
function man_checkroutine, list, routine
  compile_opt strictarr, hidden
  
  ind = where(list eq strupcase(routine), count)
  return, count gt 0L
end


;+
; Print comments about a routine.
;
; :Params:
;    routine : in, required, type=string
;       routine name to look up
;
; :Keywords:
;    output : in, out, optional, type=strarr
;       accumulation of output
;-
pro man_routineinfo, routine, output=output
  compile_opt strictarr, hidden
  
  isSystem = 0B
  isFunction = 0B
  
  case 1 of
    man_checkroutine(routine_info(/system), routine): isSystem = 1B      
    man_checkroutine(routine_info(/system, /functions), routine): begin
        isSystem = 1B
        isFunction = 1B
      end
    man_checkroutine(routine_info(), routine): 
    man_checkroutine(routine_info(/functions), routine): isFunction = 1B
    else: begin
        man_resolveroutine, routine, resolved=resolved, /either
        
        if (~resolved) then begin          
          _output = routine + ' not found'
          output = n_elements(output) eq 0L ? [_output] : [output, _output]
          return
        endif
        
        case 1 of
          man_checkroutine(routine_info(), routine): 
          man_checkroutine(routine_info(/functions), routine): isFunction = 1B        
          else: begin
              _output = routine + ' not found'
              output = n_elements(output) eq 0L ? [_output] : [output, _output]
              return          
            end
        endcase
      end
  endcase
  
  if (isSystem) then begin
    help, /dlm, output=dlmOutput
    objectFiles = strmid(dlmOutput[2:*:3], 10)
    
    params = ''
    keywords = ''
    extraComma = ''
    separatingComma = ''
    
    found = 0B
    
    for f = 0L, n_elements(objectFiles) - 1 do begin
      dlmFile = strmid(objectFiles[f], $
                       0, $
                       strpos(objectFiles[f], '.', /reverse_search)) $
                  + '.dlm'
                  
      if (~file_test(dlmFile)) then begin
        dname = file_dirname(dlmFile)
        bname = file_basename(dlmFile)
        dotpos = strpos(bname, '.')
        dlmFile = dname + path_sep() + strmid(bname, 0, dotpos) + '.dlm'
        
        if (~file_test(dlmFile)) then continue
      endif
      
      nlines = file_lines(dlmFile)
      file = strarr(nlines)
      openr, lun, dlmFile, /get_lun
      readf, lun, file
      free_lun, lun
      
      re = (isFunction ? 'function' : 'procedure') + '[[:space:]]+' + routine + '[[:space:]]+.*'
      lines = stregex(file, re, /boolean, /fold_case)
      ind = where(lines, nmatches)
      if (nmatches gt 0L) then begin
        _output = 'Filename: ' + dlmFile
        output = n_elements(output) eq 0L ? [_output] : [output, _output]
        
        tokens = strsplit(file[ind[0]], /extract, count=ntokens)

        ; TODO: fix this for DLMs with IDL_MAXPARAMS
        nparams = long(tokens[2:3])
        params = nparams[0] eq 0L ? '' : 'arg' + strtrim(lindgen(nparams[0]) + 1L, 2)
        if (nparams[1] - nparams[0] gt 0L) then begin
          params = [params, '[arg' + strtrim(lindgen(nparams[1] - nparams[0]) + nparams[0] + 1L, 2) + ']']
        endif
        params = strjoin(params, ', ')
        
        keywords = ''
        if (ntokens gt 4L) then begin
          ind = where(strmatch(tokens[4:*], 'keywords', /fold_case), count)
          if (count gt 0L) then keywords = 'KEYWORDS=keywords'
        endif
        
        extraComma = ~isFunction && (nparams[1] + count gt 0L)
        separatingComma = (nparams[1] gt 0L) && (count gt 0L)
        
        found = 1B
        break
      endif         
    endfor
    
    if (~found) then begin
      _output = 'Filename: core system routine'
      output = n_elements(output) eq 0L ? [_output] : [output, _output]
    endif
  endif else begin
    ; necessary to resolve routines that are mentioned but not resolves i.e.
    ; ROUTINE_INFO knows they exist but not much else about them
    if (man_checkroutine(routine_info(/unresolved), routine) $
          || man_checkroutine(routine_info(/unresolved, /functions), routine)) then begin
      man_resolveroutine, routine, /either
    endif
        
    srcInfo = routine_info(routine, functions=isFunction, /source)
    _output = 'Filename: ' + srcInfo.path
    output = n_elements(output) eq 0L ? [_output] : [output, _output]
    
    argInfo = routine_info(routine, functions=isFunction, /parameters)
    extraComma = ~isFunction && (argInfo.num_args + argInfo.num_kw_args gt 0L)
    params = argInfo.num_args gt 0L $
               ? strlowcase(strjoin(argInfo.args, ', ')) $
               : ''
    keywords = argInfo.num_kw_args gt 0L $
                 ? strjoin(argInfo.kw_args + '=' + strlowcase(argInfo.kw_args), ', ') $
                 : ''
    separatingComma = (argInfo.num_args gt 0L) && (argInfo.num_kw_args gt 0L)
  endelse

  format = isFunction ? '(%"result = %s(%s%s%s%s)")' : '(%"%s%s%s%s%s")'
  man_print, string(routine, $
                    extraComma ? ', ' : '', $
                    params, $
                    separatingComma ? ', ' : '', $
                    keywords, $
                    format=format), indent='  ', output=output

  if (~isSystem) then begin  
    man_printdocs, srcInfo.path, routine, is_function=isFunction, output=output
  endif
end


;+
; Print comments about a routine or finds matching routines.
;
; :Params:
;    routine : in, required, type=string
;       routine name to look up
;
; :Keywords:
;    no_page : in, optional, type=boolean
;       set to not page the output
;-
pro man, routine, no_page=noPage
  compile_opt strictarr, hidden
  
  ; check if there are wildcards in the routine name
  if (stregex(routine, '^[_$[:alnum:]]+$', /boolean, /fold_case)) then begin
    oldQuiet = !quiet
    !quiet = 1    
    man_routineinfo, routine, output=output
    !quiet = oldQuiet
  endif else begin
    systemProcedures = routine_info(/system)
    ind = where(strmatch(systemProcedures, routine, /fold_case), nSystemProcedures)
    if (nSystemProcedures gt 0L) then systemProcedures = systemProcedures[ind]
    
    systemFunctions = routine_info(/system, /functions)
    ind = where(strmatch(systemFunctions, routine, /fold_case), nSystemFunctions)
    if (nSystemFunctions gt 0L) then systemFunctions = systemFunctions[ind]

    userProcedures = routine_info()
    ind = where(strmatch(userProcedures, routine, /fold_case), nUserProcedures)
    if (nUserProcedures gt 0L) then userProcedures = userProcedures[ind]

    userFunctions = routine_info(/functions)
    ind = where(strmatch(userFunctions, routine, /fold_case), nUserFunctions)
    if (nUserFunctions gt 0L) then userFunctions = userFunctions[ind] 

    oldQuiet = !quiet
    !quiet = 1
    files = file_search(strsplit(!path, path_sep(/search_path), /extract), $
                        routine + '.pro', $
                        count=nFiles, /fold_case)
    if (nFiles gt 0L) then begin    
      files = files[uniq(files, sort(files))]
    endif
    !quiet = oldQuiet
    
    files = file_basename(files, '.pro')
      
    allRoutines = ['']
    if (nSystemProcedures gt 0L) then allRoutines = [allRoutines, systemProcedures]
    if (nSystemFunctions gt 0L) then allRoutines = [allRoutines, systemFunctions]
    if (nUserProcedures gt 0L) then allRoutines = [allRoutines, userProcedures]
    if (nUserFunctions gt 0L) then allRoutines = [allRoutines, userFunctions]
    if (nFiles gt 0L) then allRoutines = [allRoutines, files]
    
    if (n_elements(allRoutines) eq 1L) then begin
      _output = 'No routines found'
      output = n_elements(output) eq 0L ? [_output] : [output, _output]
      return
    endif else begin
      allRoutines = allRoutines[1:*]
    endelse
    
    allRoutines = allRoutines[uniq(allRoutines, sort(allRoutines))]
    nAllRoutines = n_elements(allRoutines)
    for i = 0L, n_elements(allRoutines) - 1L do begin
      oldQuiet = !quiet
      !quiet = 1      
      man_routineinfo, allRoutines[i], output=output
      !quiet = oldQuiet
      
      if (i ne nAllRoutines - 1L) then begin
        _output = ''
        output = n_elements(output) eq 0L ? [_output] : [output, _output]
      endif
    endfor
  endelse

  if (keyword_set(noPage)) then begin
    print, transpose(output)  
  endif else begin    
    terminal = !version.os_family eq 'unix' ? '/dev/tty' : 'CON:'
    openw, outlun, '/dev/tty', /get_lun, /more
    printf, outlun, transpose(output)
    free_lun, outlun
  endelse
end


; main-level example program

print, 'man, ''congrid'''
print, '--------------'
man, 'congrid'

print
print, 'man, ''mg_*range*'''
print, '-----------------'

man, 'mg_*range*'

end
