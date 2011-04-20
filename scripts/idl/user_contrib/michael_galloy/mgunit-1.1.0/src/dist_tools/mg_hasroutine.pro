; docformat = 'rst'

;+
; Determines if a given routine is available to call.
;-


;+
; Checks to see if routine is in list.
;
; :Returns:
;    1B if routine in in list; 0B if not
;
; :Params:
;    list : in, required, type=strarr
;       list of routines to check against
;    routine : in, required, type=string
;       name of routine to check (case-insensitive)
;-
function mg_hasroutine_checkroutine, list, routine
  compile_opt strictarr
  
  ind = where(list eq strupcase(routine), count)
  return, count gt 0L
end


;+
; Determine if a given routine name is available to call.
;
; :Returns:
;    1B if a routine is available to call, 0B if not
;
; :Params:
;    routine : in, required, type=string
;       routine name to look up
;
; :Keywords:
;    is_system : out, optional, type=boolean
;       set to a named variable to determine if the routine is a system 
;       routine
;    is_function : out, optional, type=boolean
;       set to a named variable to determine if the routine is a function
;-
function mg_hasroutine, routine, is_system=isSystem, is_function=isFunction
  compile_opt strictarr, hidden
  
  isSystem = 0B
  isFunction = 0B
  
  case 1 of
    mg_hasroutine_checkroutine(routine_info(/system), routine): isSystem = 1B      
    mg_hasroutine_checkroutine(routine_info(/system, /functions), routine): 
    mg_hasroutine_checkroutine(routine_info(), routine): 
    mg_hasroutine_checkroutine(routine_info(/functions), routine): isFunction = 1B
    else: begin
        mg_resolve_routine, routine, resolved=resolved, /either
        
        if (~resolved) then return, 0B
        
        case 1 of
          mg_hasroutine_checkroutine(routine_info(), routine): 
          mg_hasroutine_checkroutine(routine_info(/functions), routine): isFunction = 1B        
          else: return, 0B
        endcase
      end
  endcase
  
  return, 1B
end
