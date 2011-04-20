; docformat = 'rst'

function indgen_uts::init, _extra=e
  compile_opt strictarr

  if (~self->mguttestsuite::init(_extra=e)) then return, 0

  ;self->add, ['indgen_ut', 'findgen_ut']
  self->add, /all
  
  return, 1
end


pro indgen_uts__define
  compile_opt strictarr
  
  define = { indgen_uts, inherits MGutTestSuite }
end
