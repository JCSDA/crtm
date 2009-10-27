;+
; NAME:
;   PRINTLOG
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Captures transcript of console output
;
; CALLING SEQUENCE:
;   PRINTLOG, d1, d2, ..., FORMAT=, LOG=LOG, /ONLYLOG, UNIT=UNIT
;
; DESCRIPTION: 
;
;   The PRINTLOG procedure provides the ability to print an arbitrary
;   expression to the console or an open file UNIT, and also to
;   capture the text in a "log" or archive.  This archive can be used
;   as a verbatim record of console output, which is especially useful
;   when transactional history records must be maintained.
;
;   The log itself is stored as an array of strings which is passed
;   via the LOG keyword.  PRINTLOG simply adds the current output to
;   the existing array and returns.  When the transaction is complete,
;   the resulting array may be saved or printed as appropriate.  For
;   example, the following set of commands will accumulate a log which
;   can be saved later:
;
;     IDL> x = 0 & y = 1 & u = -17. & v = 12.    ;;; CREATE A LOG
;     IDL> PRINTLOG, X, Y, LOG=LOG
;            0       1
;     IDL> PRINTLOG, U, V, LOG=LOG
;          -17.0000      12.0000
;     IDL> PRINTLOG, 'Computation done.', LOG=LOG
;     Computation done.
;
;     IDL> print, log, format='(A)'              ;;; PRINT THE LOG
;            0       1
;          -17.0000      12.0000
;     Computation done.
;     
;
;   NOTE: Output to the console can be disabled and re-enabled using
;   the DEFAULT_PRINT keyword.  The DEFAULT_PRINT keyword affects the
;   permanent state of PRINTLOG.  When it is set to 0, then *all*
;   subsequent console output will be disabled until DEFAULT_PRINT is
;   reset to 1.  Output will always be logged to the LOG;
;   DEFAULT_PRINT only controls the console output.  This can be
;   useful to have a global switch which determines the governs the
;   console activity of an application.  However, only *one* global
;   control variable is available.
;
; INPUTS:
;
;   d1, d2, ... - the variables or expressions to be printed, as in
;                 the PRINT or PRINTF commands.  A maximum of twenty
;                 parameters are allowed.
;
; KEYWORDS:
;
;   LOG - input/output keyword, containing the accumulated transaction
;         log.  Upon input, LOG should be an array of strings
;         containing previously accumulated log.  Upon return, LOG
;         will have any new output appended.  If, upon input, LOG is
;         undefined, or contains a single element (-1L or ''), then
;         LOG will be initialized.
;
;   FORMAT - a standard format statement, as used by STRING, PRINT or
;            PRINTF.
;            Default: default output formatting is used.
;
;   UNIT - a file unit to be used for output.  If UNIT is undefined or
;          0, then output is made to the console.
;          Default: undefined (console output).
;
;   ONLYLOG - if set, then output will not be made to the screen, but
;             it will still be archived to LOG.  This may useful to
;             record archane but important dianostic information that
;             normally would not appear to the user.
;
;   DEFAULT_PRINT - Change default behavior of PRINTLOG.  If
;                   DEFAULT_PRINT is 0 then all subsequent printlog's
;                   will *not* be printed to the console, until
;                   DEFAULT_PRINT is reset.  If DEFAULT_PRINT is 1
;                   then all subsequent printlog's will be printed to
;                   the console.
;
;                   Initial default: 1 (print to console)
;                   Default: none (user must explicitly set)
;
; EXAMPLE:
;   See above.
;
; SEE ALSO:
;   PRINT, PRINTF, STRING
;   STATUSLINE - To print temporary status messages to console
;
; MODIFICATION HISTORY:
;   Written, CM, June 1999
;   Documented, CM, 25 Feb 2000
;   Added STATUSLINE to "SEE ALSO," CM, 22 Jun 2000
;   Be more intelligent about growing log if PRINTLOG will be called
;     many times (secret NLOGLINES keyword parameter), CM, Feb 2003
;   Corrected bug if N_PARAMS was larger than 10, (H. Krimm) CM, 13
;     Feb 2003
;   Added DEFAULT_PRINT keyword, CM, 10 Oct 2003
;
; TODO:
;   Have a way to internally store the log, rather than the LOG
;   keyword.
;
;  $Id$
;
;-
; Copyright (C) 2000, 2002, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
pro printlog,  d1,  d2,  d3,  d4,  d5,  d6,  d7,  d8,  d9, d10, $
               d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, $
               format=format, log=log, onlylog=logonly0, unit=unit, $
               nloglines=nloglines, trim=trim, nocatch=nocatch, $
               default_print=defpr

  common printlog_common, default_print

  if n_elements(default_print) EQ 0 then default_print = 1

  np = n_params()

  if n_elements(defpr) GT 0 then begin
      default_print = keyword_set(defpr)
      if np EQ 0 then return
  endif

;  if NOT keyword_set(nocatch) then on_error, 2
  if np GT 20 then $
    message, 'ERROR: number of parameters to PRINTLOG cannot exceed 20'
  cmd = string(lindgen(np)+1, $
               format='("str = string(",50("D",I0,:,","))')
  if n_elements(format) GT 0 then cmd = cmd + ",format=format(0))" $
  else cmd = cmd + ")"

  if n_elements(nloglines) LT 1 then begin
      nloglines = n_elements(log)
  endif else begin
      nloglines = nloglines(0)
  endelse
  if nloglines GT n_elements(log) then $
    nloglines = n_elements(log)

  str = ''
  result = execute(cmd)
  if result NE 1 then return
  if n_elements(unit) EQ 0 then unit = 0

  ;; Whether or not to print to the screen... governed by evil secret
  ;; common block.
  logonly = NOT default_print
  if n_elements(logonly0) GT 0 then logonly = keyword_set(logonly0)

  if NOT logonly then begin
      if unit ne 0 then printf, unit, str, format='(A)' $
      else              print, str, format='(A)'
  endif

  first = 0
  if nloglines EQ 0 then first = 1
  sz = size(log)
  if nloglines EQ 1 then if sz(sz(0)+1) NE 7 then $
    if long(log(0)) EQ -1 then first = 1
  if nloglines EQ 1 then if sz(sz(0)+1) EQ 7 then $
    if log(0) EQ '' then first = 1

  if first then begin
      log = [str]
      nloglines = n_elements(log)
  endif else begin
      ;; Add elements to an existing list
      nneeded = nloglines + n_elements(str)
      if nneeded GT n_elements(log) then begin
          ;; Number of entries to add, plus some sanity checking
          nadd = n_elements(log) > 64L < 2048L
          nadd = nadd > (nneeded-n_elements(log))
          if arg_present(nloglines) EQ 0 then $
            nadd = nneeded - n_elements(log)
          olog = temporary(log)
          log = strarr(n_elements(olog)+nadd)
          log(0) = temporary(olog)
      endif

      ;; Insert the items into the array
      log(nloglines) = str
      nloglines = nloglines + n_elements(str)
  endelse

  return
end
