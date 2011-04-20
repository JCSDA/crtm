; docformat = 'rst'

;+
; MG_LOG is a procedural interface to the logging framework.
;
; MG_LOG is a convenience routine so that the MGffLogger object does not need
; to be explicitly stored. If more than one logger is required, then create
; and use the logger objects explicitly instead of using this routine.
;
; The error levels are: none (level 0), critical (level 1), error (level 
; 2), warning (level 3), informational (level 4), debug (level 5). Only log 
; messages with a level less than or equal to the current logger level are 
; actually recorded.
; 
; Named subloggers can be created using the NAME keyword. These subloggers 
; should be used for individual applications or functional areas of an
; application.
; 
; For example, the following starts the logging framework and creates an 
; "mg_example" logger object returned via the LOGGER keyword::
;
;    mg_log, name='mg_example', logger=logger
;
; Further refinement can be done with a hierarchy of names, such as::
;
;    mg_log, name='mg_example/gui', logger=logger
; 
; :Examples:
;    Use the LOGGER keyword to retrieve the logger object in order to set the
;    level or filename of output::
;
;       IDL> mg_log, logger=logger
;       IDL> logger->setProperty, level=3
;       IDL> mg_log, 'Warning message', /warning
;-


;+
; Messages are logged via this routine. Also, the LOGGER keyword returns the
; logging object which is used to configure the logging.
;
; :Params:
;    msg : in, optional, type=string
;       message to log, if present
;
; :Keywords:
;    name : in, optional, type=string
;       path to logger to send message to
;    critical : in, optional, type=boolean
;       set to specify the message as critical
;    error : in, optional, type=boolean
;       set to specify the message as an error
;    warning : in, optional, type=boolean
;       set to specify the message as a warning
;    informational : in, optional, type=boolean
;       set to specify the message as informational
;    debug : in, optional, type=boolean
;       set to specify the message as debug
;    last_error : in, optional, type=boolean
;       set to place a stack trace for the last error in the log; placed after
;       the logging of any normal message in this call
;    logger : out, optional, type=object
;       MGffLogger object
;    quit : in, optional, type=boolean
;       set to quit logging; will log an normal message in this call before 
;       quitting
;-
pro mg_log, msg, name=name, $
            debug=debug, informational=informational, $
            warning=warning, error=error, critical=critical, $
            last_error=lastError, $
            logger=logger, quit=quit
  compile_opt strictarr
  @mg_log_common
  
  ; create the correct logger if needed
  if (~obj_valid(mgLogger)) then mgLogger = obj_new('MGffLogger', level=5)
  logger = n_elements(name) eq 0L ? mgLogger : mgLogger->getByName(name)
  
  ; log normal messages
  if (n_params() gt 0L && obj_valid(logger)) then begin
    levels = [keyword_set(critical), $
              keyword_set(error), $
              keyword_set(warning), $
              keyword_set(informational), $
              keyword_set(debug)]
    level = max(levels * (lindgen(5) + 1L))
    if (level eq 0L) then level = 5L  ; default level is DEBUG
    logger->print, msg, level=level, /from_mg_log
  endif
  
  ; do after regular messages so that a regular message and the stack trace 
  ; can be logged with one call to MG_LOG
  if (keyword_set(lastError)) then logger->insertLastError

  ; do last so that a quitting message can be logged at the same time that the
  ; logger is shutdown
  if (keyword_set(quit)) then obj_destroy, logger
end


; Main-level example program

mg_log, logger=logger

logger->setProperty, level=3

print, 'Top level logger @ LEVEL=3:'
mg_log, 'Debugging message', /debug              ; won't show up since LEVEL=3
mg_log, 'Informational message', /informational  ; won't show up since LEVEL=3
mg_log, 'Warning message', /warning
mg_log, 'Error message', /error
mg_log, 'Critical message', /critical

mg_log, name='mg_log', logger=mgLogLogger
mgLogLogger->setProperty, level=2

print
print, 'mg_log logger @ LEVEL=2:'
mg_log, 'Debugging message', name='mg_log', /debug              ; won't show up since LEVEL=2
mg_log, 'Informational message', name='mg_log', /informational  ; won't show up since LEVEL=2
mg_log, 'Warning message', name='mg_log', /warning              ; won't show up since LEVEL=2
mg_log, 'Error message', name='mg_log', /error
mg_log, 'Critical message', name='mg_log', /critical

mg_log, name='mg_log/example', logger=mgLogExampleLogger
mgLogExampleLogger->setProperty, level=1

print
print, 'mg_log/example logger @ LEVEL=1:'
mg_log, 'Debugging message', name='mg_log/example', /debug              ; won't show up since LEVEL=1
mg_log, 'Informational message', name='mg_log/example', /informational  ; won't show up since LEVEL=1
mg_log, 'Warning message', name='mg_log/example', /warning              ; won't show up since LEVEL=1
mg_log, 'Error message', name='mg_log/example', /error                  ; won't show up since LEVEL=1
mg_log, 'Critical message', name='mg_log/example', /critical

mg_log, /quit

end