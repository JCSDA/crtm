;+
; NAME:
;   TRANSREAD
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Parse a tabular ASCII data file or string array.
;
; CALLING SEQUENCE:
;   TRANSREAD, UNIT, VARi [, FORMAT=FORMAT]  (first usage)
;      or
;   TRANSREAD, UNIT, VARi [, FORMAT=FORMAT], FILENAME=FILENAME  (second usage)
;      or
;   TRANSREAD, STRINGARRAY, VARi [, FORMAT=FORMAT]  (second usage)
;
; DESCRIPTION:
;   TRANSREAD parses an ASCII table into IDL variables, one variable
;   for each column in the table.  The tabular data is not limited to
;   numerical values, and can be processed with an IDL FORMAT
;   expression.
;
;   TRANSREAD behaves similarly to READF/READS in that it transfers
;   ASCII input data into IDL variables.  The difference is that
;   TRANSREAD reads more than one row in one pass, and returns data by
;   column.  In a sense, it forms the *transpose* of the typical
;   output from READF or READS (which returns data by row), hence the
;   name TRANSREAD.  [ TRANSREAD can parse up to 20 columns in its
;   current implementation, but that number can be easily increased. ]
;
;   TRANSREAD can optionally be provided with a FORMAT expression to
;   control the transfer of data.  The usage is the same as for
;   READ/READF/READS.  However, you may find that you need to slightly
;   modify your format statements to read properly.  In this
;   implementation, variables are intermediately parsed with READS,
;   which appears from my experimentation to require at least a
;   default length for transfers.
;
;   Hence, you should use:   ..., FORMAT='(D0.0,D0.0,I0)'       ; GOOD
;   instead of:              ..., FORMAT='(D,D,I)'          ; BAD
;
;   As with the standard IDL READ-style commands, you need to supply
;   initial values to your variables before calling TRANSREAD, which
;   are used to determine the type.  Then dimensions of the variable
;   are not important; TRANSREAD will grow the arrays to an
;   appropriate size to accomodate the input.  Lines from the input
;   which do not contain the correct number of columns or do not obey
;   the format statement are ignored.
;
;   TRANSREAD will also flexibly manage typical data files, which may
;   contain blank lines, lines with comments (see COMMENT keyword), or
;   incomplete lines.  These lines are ignored.  It can be programmed
;   to wait for a user-specified "trigger" phrase in the input before
;   beginning or ending processing, which can be useful if for example
;   the input table contains some header lines (see STARTCUE and
;   STOPCUE keywords).  [ The user can also pre-read these lines
;   before calling TRANSREAD. ]  Finally, the total number of lines
;   read can be controlled (see MAXLINES keyword).  TRANSREAD parses
;   until (a) the file ends, (b) the STOPCUE condition is met or (c)
;   the number of lines read reaches MAXLINES.
;
;   TRANSREAD has three possible usages.  In the first, the file must
;   already be open, and TRANSREAD begins reading at the current file
;   position.  In the second usage, a filename is given.  TRANSREAD
;   automatically opens the file, and reads tabular data from the
;   beginning of the file.  Normally the file is then closed, but this
;   can be prevented by using the NOCLOSE keyword.
;
;   In the third usage, a string array is passed instead of a file
;   unit.  Elements from the array are used one-by-one as if they were
;   read from the file.
;
;   Since TRANSREAD is not vectorized, and does a significant amount
;   of processing on a per-line basis, it is probably not optimal to
;   use on very large data files.
;
; INPUTS:
;
;   UNIT - in the first usage, UNIT is an open file unit which
;          contains ASCII tabular data to read.  UNIT must not be a
;          variable which could be mistaken for a string array.
;
;          In the second usage, when FILENAME is specified, then upon
;          return UNIT contains the file unit that TRANSREAD used for
;          reading.  Normally, the UNIT is closed before return, but
;          it can be kept open using the NOCLOSE keyword.  In that
;          case the unit should be closed with FREE_LUN.
;
;   STRINGARRAY - this is the third usage of TRANSREAD.  When a string
;                 array is passed, elements from the array are used as
;                 if they were lines from an input file.  The array
;                 must not be of a numeric type, so it cannot be
;                 mistaken for a file unit.  [ Of course, the string
;                 itself can contain ASCII numeric data. ]
;
; OUTPUTS:
;   VARi - List of named variables to receive columns from the table,
;          one variable for each column.  Upon output each variable
;          will be an array containing the same number of elements,
;          one for each row in the table.  If no rows were
;          successfully parsed, then the variable values are not
;          changed.  Use the COUNT output keyword to determine whether
;          any rows were parsed.
;
;          NOTE: Up to twenty columns may be parsed.  If more columns
;          are desired, then a simple modification must be made to the
;          IDL source code.  To do so, find the beginning of the
;          procdure definition, identified by the words, "pro
;          transread, ..."  and follow the instructions there.
;
; INPUT KEYWORD PARAMETERS:
;   FORMAT - an IDL format expression to be used to transfer *each*
;            row in the table.  If no format as given then the default
;            IDL transfer format is used, based on the types of the
;            input variables.  As mentioned in the description above,
;            a length should be assigned to each format code; a length
;            of zero can be used for numeric types.  Lines from the
;            input which do not contain the correct number of columns
;            or do not obey the format statement are ignored.
;
;   COMMENT - A one-character string which designates a "comment" in
;             the input.  Input lines beginning with this character
;             (preceded by optional spaces) are ignored.  FAILCOUNT
;             does not increase.
;             DEFAULT: no comments are recognized.
;
;             NOTE: lines which do not match the format statement are
;             ignored.  Comments are likely to be ignored based on
;             this behavior, even without specifying the COMMENT
;             keyword; however the FAILCOUNT will increase.
;
;   MAXLINES - the maximum number of lines to be read from input.  The
;              count begins *after* any STARTCUE is satisfied (if any)
;              DEFAULT: no maximum is imposed.
;
;   SKIPLINES - the number of lines of input to skip before beginning
;               to parse the table.
;               DEFAULT: no lines are skipped.
;               NOTE: if STARTCUE is also given, then the line count
;               does not start until after the STARTCUE phrase has
;               been encountered.
;
;   STARTCUE - a unique string phrase that triggers the start of
;              parsing.  Lines up to and including the line containing
;              the cue are ignored.  Because each line is checked for
;              this starting cue, it should be unambiguous.
;              DEFAULT: parsing begins immediately.
;
;   STOPCUE - a unique string phrase that triggers the finishing of
;             parsing.  The line including the cue is ignored, and no
;             more reads occur afterward.
;             DEFAULT: no STOPCUE is imposed.
;
;   FILENAME - the presence of this keyword signals the second usage,
;              where TRANSREAD explicitly opens the input file named
;              by the string FILENAME.  Reading begins at the start of
;              the file.
;
;              Normally TRANSREAD will close the input file when it
;              finishes.  This can be prevented by setting the NOCLOSE
;              keyword.
;
;              DEFAULT: input is either an already-opened file passed
;              via the UNIT keyword, or a string array.
;
;   NOCLOSE - if set and if FILENAME is given, then the file is not
;             closed upon return.  The file unit is returned in UNIT,
;             and must be closed by the user via FREE_LUN, UNIT.
;             DEFAULT: any files that TRANSREAD opens are closed.
;
;   DEBUG - set this keyword to enable debugging messages.  Detailed
;           error messages will be printed for each failed line.
;
; OUTPUT KEYWORDS:
;   LINES - the number of lines read, including comments and failed
;           parses.
;
;   COUNT - the number of rows successfully parsed.  Can be zero if
;           accessing the input utterly fails, or if no rows are
;           present.
;
;   FAILCOUNT - the number of rows that could not be parsed
;               successfully.  Comments and blank lines are not
;               included.
;
; EXAMPLES:
;   OPENR, UNIT, 'widgets.dat', /GET_LUN
;   A = '' & B = 0L & C = 0D
;   TRANSREAD, UNIT, A, B, C, COUNT=COUNT, FORMAT='(A10,I0,D0.0)'
;   FREE_LUN, UNIT
;
;   (First usage) Opens widgets.dat and reads three columns.  The
;   first column is a ten-character string, the second an integer, and
;   the third a double precision value.
;
;   A = '' & B = 0L & C = 0D
;   TRANSREAD, UNIT, A, B, C, COUNT=COUNT, FORMAT='(A10,I0,D0.0)', $
;      FILENAME='widgets.dat'
;
;   (Second usage) Achieves the same effect as the first example, but
;   TRANSREAD opens and closes the file automatically.
;
;   SPAWN, 'cat widgets.dat', BUF
;   A = '' & B = 0L & C = 0D
;   TRANSREAD, BUF, A, B, C, COUNT=COUNT, FORMAT='(A10,I0,D0.0)'
;
;   (Third usage) Achieves the same effect as the first two examples,
;   but input is read from the string variable BUF.
;
; MODIFICATION HISTORY:
;   Feb 1999, Written, CM
;   Mar 1999, Added SKIPLINES and moved on_ioerror out of loop, CM
;   Jun 2000, Added NOCATCH and DEBUG keyword options, CM
;
;  $Id$
;-
; Copyright (C) 1997-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

pro transread, unit, l1, l2, l3, l4, l5, l6, l7, l8, l9, l10, $
               l11, l12, l13, l14, l15, l16, l17, l18, l19, l20, $
; NOTE: ADD COLUMNS HERE, as l21, l22, etc.  Remember to end lines
; with a dollar-sign, as "l20" is above.
               skiplines=skiplines, maxlines=maxlines, $
               format=format, comment=comment, nocatch=nocatch, debug=debug, $
               startcue=startcue, stopcue=stopcue, filename=filename, $
               lines=lines, count=count, noclose=noclose, failcount=failcount

  count = 0L               
  if n_params() LE 1 then begin
      message, 'USAGES: TRANSREAD, UNIT, VAR1, VAR2, ...', /info
      message, '        TRANSREAD, UNIT, VAR1, VAR2, ..., FILENAME=FILENAME',$
        /info
      message, '        TRANSREAD, STRINGARRAY, VAR1, VAR2, ...', /info
      return
  endif

  ;; Default parameters
  if n_elements(maxlines) EQ 0 then maxlines = ishft(1L, 31) - 1
  if n_elements(skiplines) EQ 0 then skiplines = 0L
  s = strtrim(lindgen(n_params()-1)+1, 2)
  
  ;; Values are intermediately parsed into a structure.  The structure
  ;; needs to be created once, here, with the correct data types for
  ;; each column.  A special statement is composed explicitly and then
  ;; executed.  The data type of only the *first* element of the input
  ;; array is used.

  structexpr = 'st0 = create_struct('
  for i = 0L, n_params()-2 do begin
      structexpr = structexpr + '"d'+s(i)+'", l'+s(i)+'(0)'
      if i LT n_params()-2 then structexpr = structexpr + ','
  end
  st0 = 0L
  structexpr = structexpr + ')'
  dummy = execute(structexpr)
  st = st0

  ;; Initialize the statistics
  lines = 0L
  count = 0L
  failcount = 0L
  startwaiting = n_elements(startcue) GT 0  ;; If we wait for a STARTCUE
  stopwaiting  = n_elements(stopcue)  GT 0  ;; If we wait for a STOPCUE
  ccheck = n_elements(comment) GT 0
  done = 0

  ;; It saves a *lot* of execution time to avoid the x = [x, newx]
  ;; construction.  I allocate new memory for the "result" array in
  ;; chunks, which saves much time.
  outbuffersize = 0L

  ;; Check for a file unit, not a string array.
  sz = size(unit)
  if n_elements(filename) GT 0 AND sz(sz(0)+1) NE 7 then begin
      on_ioerror, OPEN_ERROR
      openr, unit, filename, /get_lun
      on_ioerror, NULL
      if 0 then begin
          OPEN_ERROR:
          message, 'ERROR: could not open '+filename
          return
      endif
  endif

  ;; If reading from a string buffer
  strread = 0
  if sz(sz(0)+1) EQ 7 then begin
      strread = 1
      xeof = 0
      nstrings = n_elements(unit)
      j = 0L   ;; j is the index into the string buffer
      goto, START_LOOP
  endif

  ;; Check for a valid file unit and that it is readable.  The catch
  ;; expression here is used to trap invalid file handles.
  catch, catcherror
  if catcherror NE 0 then begin
      catch, /cancel
      message, 'ERROR: file unit '+strtrim(unit)+' must be open and readable.'
      return
  end
  xeof = eof(unit)
  if xeof then return
  catch, /cancel

  START_LOOP:
  ;; Set up a catch handler which deals with a conversion error
  catcherror = 0
  if NOT keyword_set(nocatch) then catch, catcherror
  if catcherror NE 0 then begin

      ;; Some errors are worse than others.  If something goes wrong
      ;; during a parse, we can still go on to read more.
      if parsing then begin
          parsing = 0
          watchdog = 0
          failcount = failcount + 1  ;; but we increase the "fail" count

          DEBUG_CHECK:
          if keyword_set(debug) then begin
              print, '**DEBUGGING MESSAGE:  could not parse the following line'
              print, '**   <'+strbuffer(0)+'>'
              print, '**The error message was:'
              print, '**   '+!err_string
              print, '**The parsed variables were as follows:'
              help, /struct, st
              print, '**END OF DEBUGGING MESSAGE'
          endif
      endif
          
      goto, NEXT_LINE
  endif
  on_ioerror, DEBUG_CHECK

  ;; We keep reading until one of the three conditions are satisfied:
  ;; (a) the end of file (or end of string array) is reached; or
  ;; (b) the maximum number of lines is read; or
  ;; (c) the "stop" cue is encountered; or
  ;; (d) an "utter" failure occurs, prevent us from reading more data.

  while NOT xeof AND lines LT maxlines AND NOT done do begin

      ;; The watchdog is here to prevent infinite loops.  Since the
      ;; CATCH handler above causes the loop to restart, we could be
      ;; in trouble.  If at least the read fails, then there is no
      ;; sense in continuing the loop.  See the end of the loop where
      ;; the value of the watchdog is checked. 
      watchdog = 1
      strbuffer = ''

      ;; Either read from the file, or copy from the string array
      if strread then strbuffer = unit(j) else readf, unit, strbuffer
      
      ;; Successful read indicates that the loop can repeat.
      watchdog = 0

      ;; Check for the STARTCUE if needed
      if startwaiting then begin
          if strpos(strbuffer, startcue(0)) GE 0 then startwaiting = 0
          goto, NEXT_LINE
      endif

      ;; line count increases only once the STARTCUE is satisfied.
      lines = lines + 1

      ;; We may need to skip some lines, according to SKIPLINES
      if lines LE skiplines then goto, NEXT_LINE

      ;; Strip out surrounding white space.  Yes, white space should
      ;; not make a difference.
      trimbuffer = strtrim(strbuffer, 2)
      if trimbuffer EQ '' then goto, NEXT_LINE

      ;; Check for the STOPCUE if needed
      if stopwaiting then begin
          if strpos(strbuffer, stopcue(0)) GE 0 then begin
              done = 1
              goto, NEXT_LOOP
          endif
      endif

      ;; Check for a comment character if requested
      if ccheck then if strmid(strbuffer, 0, 1) EQ comment then $
        goto, NEXT_LINE

      ;; Parse data from the input string buffer.  Data is parsed into
      ;; the structure ST for convenience.  The PARSING variable
      ;; indicates to the CATCH handler that an error occurred here.
      st = st0
      parsing = 1
      reads, strbuffer, st, format=format
      parsing = 0

      ;; Increase the size of the result buffer as needed.  Minimum
      ;; size is 128 elements.  Growth rate doubles until the
      ;; increment exceeds 4096.
      while count GE outbuffersize do begin
          if outbuffersize EQ 0 then outbuffersize = 64L
          outbuffersize = outbuffersize + (outbuffersize < 4096L)
          newresult = make_array(outbuffersize, value=st)
          if n_elements(result) GT 0 then newresult(0) = result
          result = temporary(newresult)
      endwhile
      result(count) = st

      ;; Upon a successful parse, then increase the count.
      count = count + 1

      ;; Update status variables for either the input file or the
      ;; string array.
      NEXT_LINE:
      if strread then begin
          j = j + 1
          xeof = j GE nstrings
      endif else begin
          xeof = eof(unit)
      endelse
      
      NEXT_LOOP:
      ;; Watchdog is checked here to prevent infinite loops, as noted above.
      if watchdog then done = 1
  end

  FINISH:
  on_ioerror, NULL
  catch, /cancel
  ;; Close the file if needed
  if n_elements(filename) GT 0 AND NOT keyword_set(noclose) then begin
      free_lun, unit
  endif

  ;; Finally, extract the elements from the result structure
  if count GT 0 then begin
      result = result(0:count-1)
      for i = 0L, n_params()-2 do begin
          copyexpr = 'l'+s(i)+' = result.('+strtrim(i,2)+')'
          dummy = execute(copyexpr)
      endfor
  end

  return
end
