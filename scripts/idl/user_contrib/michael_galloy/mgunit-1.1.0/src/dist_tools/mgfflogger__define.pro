; docformat = 'rst'

;+
; Logger object to control logging.
; 
; :Properties:
;    name
;       name of the logger
;    level
;       current level of logging: 0 (none), 1 (critial), 2 (error), 3 (warning), 
;       4 (info), or 5 (debug); can be set to an array of levels which will be
;       cascaded up to the parents of the logger with the logger taking the 
;       last level and passing the previous ones up to its parent
;    format
;       format string for messages
;    filename
;       filename to send output to; set to empty string to send output to 
;       STDERR; setting this property clobbers the filename if it already
;       exists
;    append 
;       boolean flag used when FILENAME property is set to determine if the
;       any previous content should be clobbered or appended to
;-


;+
; Get the minimum level value of this logger and all its parents.
;
; :Returns:
;    long
;-
function mgfflogger::_getLevel
  compile_opt strictarr

  return, obj_valid(self.parent) $
            ? (self.parent->_getLevel() < self.level) $
            : self.level
end


;+
; Finds the name of an object, even if it does not have a NAME property. 
; Returns the empty string if the object does not have a NAME property.
;
; :Returns:
;    string
; 
; :Params:
;    obj : in, required, type=object
;       object to find name of
;-
function mgfflogger::_askName, obj
  compile_opt strictarr

  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, ''
  endif
  
  obj->getProperty, name=name
  return, name
end


;+
; Returns an immediate child of a container by name.
;
; :Returns:
;    object
;
; :Params:
;    name : in, required, type=string
;       name of immediate child
;    container : in, required, type=object
;       container to search children of
;-
function mgfflogger::_getChildByName, name, container
  compile_opt strictarr
  
  for i = 0L, container.children->count() - 1L do begin
    child = container.children->get(position=i)
    childName = self->_askName(child)
    if (childName eq name) then return, child
  endfor
  
  return, obj_new()
end


;+
; Traverses a hierarchy of named objects using a path of names delimited with
; /'s.
; 
; :Returns:
;    object
;
; :Params:
;    name : in, required, type=string
;       path of names to the desired object; names are delimited with /'s
;-
function mgfflogger::getByName, name
  compile_opt strictarr
  
  tokens = strsplit(name, '/', /extract, count=ntokens)
  child = self
  for depth = 0L, ntokens - 1L do begin
    newChild = self->_getChildByName(tokens[depth], child)
    if (~obj_valid(newChild)) then begin
      newChild = obj_new('MGffLogger', name=tokens[depth], parent=child)
      child.children->add, newChild
    endif
    child = newChild
  endfor
  
  return, child
end


;+
; Set properties.
;-
pro mgfflogger::getProperty, level=level, format=format, name=name, output=output
  compile_opt strictarr
  
  if (arg_present(level)) then level = self.level
  if (arg_present(format)) then format = self.format
  if (arg_present(name)) then name = self.name
  if (arg_present(output)) then begin
    if (self.lun gt 0L) then begin
      cur = fstat(self.lun)
      free_lun, self.lun
    
      output = strarr(file_lines(cur.name))
      openr, lun, cur.name, /get_lun
      self.lun = lun
      readf, self.lun, output
    endif
  endif
end


;+
; Get properties.
;-
pro mgfflogger::setProperty, level=level, format=format, $
                             filename=filename, append=append
  compile_opt strictarr
  
  case n_elements(level) of
    0: 
    1: self.level = level
    else: begin
        self.level = level[n_elements(level) - 1L]
        if (obj_valid(self.parent)) then begin
          self.parent->setProperty, level=level[0:n_elements(level) - 2L]
        endif
      end
  endcase
  
  if (n_elements(format) gt 0L) then self.format = format
  if (n_elements(filename) gt 0L) then begin
    if (filename eq '') then begin
      if (self.lun gt 0L) then free_lun, self.lun
      self.lun = -2L
    endif else begin
      if (keyword_set(append) && file_test(filename)) then begin
        openu, lun, filename, /get_lun    
      endif else begin
        openw, lun, filename, /get_lun
      endelse
      self.lun = lun
    endelse
  endif
end


;+
; Insert the stack trace for the last error message into the log. Since stack
; traces are from run-time crashes they are considered to be at the CRITICAL 
; level.
;-
pro mgfflogger::insertLastError
  compile_opt strictarr
  
  help, /last_message, output=helpOutput
  _msg = systime() + ' ' + strupcase(self.levelNames[4L]) $
           + ': Stack trace for error:'
  printf, self.lun, _msg, format=self.format
  printf, self.lun, transpose(helpOutput), format=self.format
end


;+
; Log message to given level.
; 
; :Params:
;    msg : in, required, type=string
;       message to print
;
; :Keywords:
;    level : in, optional, type=long
;       level of message
;    from_mg_log : in, optional, private, type=boolean
;       set to indicate that the routine actually logging the message is not
;       the one that called this method, but that it called MG_LOG which 
;       called this method
;-
pro mgfflogger::print, msg, level=level, from_mg_log=fromMgLog
  compile_opt strictarr

  if (level le self->_getLevel()) then begin
    stack = scope_traceback(/structure, /system)
    routine = stack[n_elements(stack) - 2L - keyword_set(fromMgLog)].routine + ': '
    _msg = systime() + ' ' + strupcase(self.levelNames[level - 1L]) + ': ' + routine + msg
    printf, self.lun, _msg, format=self.format
  endif
end


;+
; Flushes output.
;-
pro mgfflogger::flush
  compile_opt strictarr
  
  flush, self.lun
end


;+
; Free resources.
;-
pro mgfflogger::cleanup
  compile_opt strictarr
  
  if (obj_valid(self.parent)) then begin
    (self.parent).children->remove, self
  endif
  
  obj_destroy, self.children
  if (self.lun gt 0L) then free_lun, self.lun
end


;+
; Create logger object.
;
; :Returns:
;    1 for success, 0 for failure
;-
function mgfflogger::init, parent=parent, name=name, _extra=e
  compile_opt strictarr

  self.parent = n_elements(parent) eq 0L ? obj_new() : parent
  self.name = n_elements(name) eq 0L ? '' : name
  self.children = obj_new('IDL_Container')
  
  self.level = 0L
  self.lun = -2L
  self.levelNames = ['Critical', 'Error', 'Warning',  'Informational', 'Debug']
  
  self->setProperty, _extra=e
  
  return, 1
end


;+
; Define instance variables.
; 
; :Fields:
;    level
;       current level of logging: 0=none, 1=critical, 2=error, 3=warning, 
;       4=informational, or 5=debug
;    levelNames
;       names for the different levels
;    lun
;       logical unit number to send logging output to
;    format
;       format code to send output to
;-
pro mgfflogger__define
  compile_opt strictarr
  
  define = { MGffLogger, $
             parent: obj_new(), $
             name: '', $
             children: obj_new(), $
             level: 0L, $
             levelNames: strarr(5), $
             lun: 0L, $
             format: '' $
           }
end
