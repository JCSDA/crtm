; docformat = 'rst'

;+
; Command line wrapper for mgunit. This routine is made to be called from the
; mgunit shell script.
;-
pro mgunit_wrapper
  compile_opt strictarr, hidden
  
  !quiet = 1
  
  opts = obj_new('mg_options', app_name='mgunit', version='1.0.0')
  
  opts->addOption, 'color', 'c', /boolean, help='produce color output on STDOUT'
  opts->addOption, 'filename', help='specify a log filename'
  opts->addOption, 'gui', 'g', /boolean, help='start a GUI to run the tests'
  opts->addOption, 'html', 'm', /boolean, help='produce HTML output'
  opts->addParams, [0, -1]
  
  opts->parseArgs, error_message=errorMsg

  if (errorMsg eq '') then begin
    params = opts->get(/params, n_params=nparams)

    if (nparams gt 0L && ~opts->get('help')) then begin
      filename = opts->get('filename', present=filenamePresent)
      if (filenamePresent) then begin
        mgunit, params, $
                color=opts->get('color'), $
                filename=opts->get('filename'), $
                gui=opts->get('gui'), $
                html=opts->get('html')
      endif else begin
        mgunit, params, $
                color=opts->get('color'), $
                gui=opts->get('gui'), $
                html=opts->get('html')
      endelse
    endif
  endif else begin
    !quiet = 0
    message, errorMsg, /informational, /noname
    !quiet = 1
  endelse
  
  obj_destroy, opts
end