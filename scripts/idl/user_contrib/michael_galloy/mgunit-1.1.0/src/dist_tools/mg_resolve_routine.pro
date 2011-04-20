; docformat = 'rst'

;+
; Routine to resolve a given routine without crashing.
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
pro mg_resolve_routine, routine, resolved=resolved, _extra=e
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
  
  if (strlowcase(routine) ne 'mg_resolve_routine') then begin
    resolve_routine, routine, _extra=e
  endif
  
  resolved = 1B
  
  !quiet = oldQuiet
end