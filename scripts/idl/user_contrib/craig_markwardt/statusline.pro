;+
; NAME:
;   STATUSLINE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Maintain a one-line status line on a VT100-compatible terminal. (Unix)
;
; MAJOR TOPICS:
;   Text Output, Terminal.
;
; CALLING SEQUENCE:
;   STATUSLINE, string, column, LENGTH=length, [/CLOSE,]
;      [/CLEAR,] [/LEFT,] [/RIGHT,] [/QUIET,] [/ENABLE,] [/DISABLE]
;
; DESCRIPTION: 
;
;   STATUSLINE maintains the current line of a VT100- or
;   ANSI-compatible terminal, usually as a status line.
;
;   Programs that run for extended periods of time can inform the user
;   of the status of the computation by printing vital information.
;   Instead of cluttering the console by using the PRINT procedure,
;   which uses a new line with each call, STATUSLINE will re-use the
;   same line.  This can make a cleaner interface.
;
;   STATUSLINE interacts directly with the Unix terminal device,
;   sending VT100-compatible cursor commands.  As a side effect it
;   opens the terminal device /dev/tty and allocates a logical unit
;   number.  Picky programmers should call STATUSLINE, /CLOSE to close
;   the file unit.
;
;   Procedures that finish their computation, or wish to make normal
;   output to the console should first clear the terminal line with
;   STATUSLINE, /CLEAR.  This will ensure that the console is
;   uncluttered before printing.
;
;   By default, STATUSLINE enables output for terminal types vt100,
;   vtnnn, xterm, dec, or ansi.  *No* output appears on other
;   terminals.  You can enable it explicitly by calling STATUSLINE,
;   /ENABLE, and disable it by calling STATUSLINE, /DISABLE.
;
; INPUTS:
;
;   STRING - A string to be placed on the current line.
;
; OPTIONAL INPUTS:
;
;   COLUMN - The starting column number, beginning with zero.
;            Default: zero.
;
; INPUT KEYWORD PARAMETERS:
;
;   LENGTH - the record length, an integer.  Strings longer than this
;            length will be truncated.  
;            Default: strlen(STRING)
;
;   CLEAR - if set, clear the current line to the end.  Control
;           returns immediately (i.e., no output is made).
;
;   LEFT  - if set, then left justify the string within the record.
;           If the string is longer than the record length, then the
;           leftmost portion of the string is printed.
;           The Default (if /RIGHT is not given).
;
;   RIGHT - if set, then right jusfity the string within the record.
;           If the string is longer than the record length, then the
;           rightmost portion of the string is printed.
;
;   QUIET - if set, then no output is made (for this call only).
;
;   NOCR - if set, no carriage return operation is performed after
;          output.  This also has the side effect that in subsequent
;          calls, column "0" will not cause the cursor to move.
;          Default: cursor returns to column 0 after each output.
;
;   ENABLE - if set, then permanently enable output by STATUSLINE.
;            Normally STATUSLINE automatically enables output only for
;            vt100-compatible terminals.  By setting /ENABLE, you
;            override this automatic test.  However, /QUIET will
;            still override ENABLE in an individual call.
;
;   DISABLE - if set, then permanently disable output by STATUSLINE.
;             When disabled, no output is ever produced.  Output can
;             only be re-enabled again by using the /ENABLE flag.
;
;   CLOSE - if set, instruct STATUSLINE to close the terminal device
;           logical unit number.  Users should perform this operation
;           when the computation has finished so that the terminal
;           device is not left dangling open.  If, at a later time,
;           STATUSLINE is called again, the terminal device will be
;           re-opened.
;
; OUTPUTS:
;   NONE
;
; SEE ALSO:
;   PRINT, PRINTF
;   PRINTLOG - to maintain transcript of IDL output
;
; MODIFICATION HISTORY:
;   Written, CM, 1997-1998
;   Documented, CM, Sep 1999
;   Added NOCR keyword, CM, 28 Oct 1999
;   Doesn't crash if can't write to TTY.  Returns silently.  CM, 16
;     Nov 1999.
;   Added PRINTLOG to "SEE ALSO", CM, 22 Jun 2000
;   Keyword QUIET now causes earlier exit; catch errors in the CLEAR
;     case, CM, 12 Oct 2001
;   Allow variations on the "xterm" terminal type, CM, 26 Jun 2007
;
;  $Id$
;
;-
; Copyright (C) 1998, 1999, 2000, 2001, 2007, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-
pro statusline, str, col, length=length, clear=clear, $
                left=left, right=right, quiet=quiet, close=close,$
	        enable=enable, disable=disable, nocr=nocr

  common statusline_common, statusline_enabled, statusline_unit
  if n_elements(statusline_enabled) EQ 0 then begin
      termtype = getenv("TERM")
      statusline_enabled = 0
      if strmid(termtype,0,2) EQ 'vt' OR $
        strmid(termtype,0,5) EQ 'xterm' OR $
        strmid(termtype,0,3) EQ 'dec' OR termtype EQ 'ansi' then $
        statusline_enabled = 1
  endif
  if keyword_set(enable) then begin
    statusline_enabled = 1
    return
  endif
  if keyword_set(disable) then begin
    statusline_enabled = 0
    return
  endif

  if keyword_set(quiet) OR statusline_enabled EQ 0 then return

  do_open = 0
  if n_elements(statusline_unit) EQ 0 then do_open = 1
  if n_elements(statusline_unit) GT 0 then begin
      if statusline_unit(0) LT 0 then do_open = 1

      ;; If the user closes the file behind our back (eg. CLOSE, /ALL)
      if statusline_unit(0) GE 0 then begin
          fs = fstat(statusline_unit(0))
          if fs.open EQ 0 then do_open = 1
      endif
  endif

  if do_open then begin
      statusline_unit = -1L
      if keyword_set(close) then return
      openw, unit, '/dev/tty', /get_lun, error=open_error
      if open_error NE 0 then return
      statusline_unit = unit
  endif

  if keyword_set(close) AND n_elements(statusline_unit) GE 1 then begin
      if statusline_unit(0) LT 0 then return
      free_lun, statusline_unit(0)
      statusline_unit = -1L
      return
  endif

  ;; ASCII carriage return, used for printing the status line
  cr = string(13b)
  esc = string(27b)  ;; Escape char

  if keyword_set(clear) then begin
      outstring = string(' ', cr, format='(A79,A,$)')
      ;; Prevent errors from stopping the show
      catch, catcherr
      if catcherr EQ 0 then writeu, statusline_unit, outstring
      return
  endif

  if NOT keyword_set(right) then left = 1

  if n_params() EQ 0 then begin
      message, 'USAGE: STATUSLINE, str, col, [length=length,]', /info
      message, '       [/clear,] [/left,] [/right,] [/quiet,] [/close]', /info
      return
  endif

  if n_elements(str) EQ 0 then return
  if n_elements(length) EQ 0 then length = strlen(str)
  if n_elements(col) EQ 0 then col = 0L
  if col LT 0 then col = 0L

  newstr = str
  slen = strlen(str)
  if slen GT length then begin
      if keyword_set(left) then $
        newstr = strmid(newstr, 0, length) $
      else $
        newstr = strmid(newstr, slen-length, length)
  endif else begin
      blanks = '                                                         '
      if keyword_set(right) then $
        newstr = strmid(blanks, 0, length-slen) + newstr
  endelse

  outstring = ''
  if col GT 0 then $
    outstring = outstring + esc + '['+strtrim(col,2) + 'C'
  outstring = outstring + newstr 
  if NOT keyword_set(nocr) then outstring = outstring + cr

  ;; Prevent errors from stopping the show
  catch, catcherr
  if catcherr EQ 0 then writeu, statusline_unit, outstring
  return

end

  
