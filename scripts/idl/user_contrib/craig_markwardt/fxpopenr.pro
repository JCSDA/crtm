;+
; NAME:
;   FXPOPENR
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Internal routine to open a Unix pipe command for read access.
;
; DESCRIPTION:
;
;   See the following procedures for appropriate documentation.
;
;     FXGOPEN  - open resource
;     FXGCLOSE - close resource
;     FXGREAD  - read from resource
;     FXGWRITE - write to resource
;     FXGSEEK  - seek on resource (i.e., perform POINT_LUN)
;
;     FXGFILTERED - determine if resource is a normal file.
;
; Usage: FXPOPENR, UNIT, COMMAND, ERRMSG=ERRMSG
;
; PARAMETERS
;
;   unit - FXPOPENR returns the pipe LUN, created by GET_LUN in this
;          parameter.  The LUN should not be "pre-opened".
;          Unformatted reads on this LUN should be performed with
;          FXPREAD.
;
;   command - a scalar string, the pipe command to execute.  The
;             standard output of the command is redirected into UNIT.
;             Standard error is not redirected.
;
;             A failure of the command can only be discovered upon
;            trying to read from the LUN with FXPREAD.
;
; Keywords
; 
;   errmsg - If set to defined value upon input, an error message is
;            returned upon output.  If no error occurs then ERRMSG is
;            not changed.  If an error occurs and ERRMSG is not
;            defined, then FXPOPENR issues a MESSAGE.
;
; Side Effects
;
;   The pipe command is opened with SPAWN, and an additional cache file
;   is opened with read/write access.
;
;   The FXFILTER family of commons is updated.
;
; MODIFICATION HISTORY:
;   Changed copyright notice, 21 Sep 2000, CM
;   Added the OPEN,/DELETE keyword, so that tmp-files are
;     automatically deleted when closed, 18 Feb 2006, CM
;   Added quotation marks to the list of characters which are
;     protected, while making a tmpfile name, 22 Oct 2006, CM
;
;  $Id$
;
;-
; Copyright (C) 1999-2000,2006 Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

;  Utility program to protect the pipe command
PRO FXPOPENR_WRAP_CMD, CMD, SHELL
  
;  SHELL = '/bin/sh -c '

  WHILE STRMID(CMD, 0, 1) EQ '|' OR STRMID(CMD, 0, 1) EQ ' ' DO $
    CMD = STRMID(CMD, 1, STRLEN(CMD)-1)
;  CMD = SHELL + '"' + CMD + ' 2>/dev/null"'
  CMD = CMD + ' 2>/dev/null'

  RETURN
END

; Utility program to generate a name for the cache file.  It is
; uniquely generated based on the time, the command string, and the
; current call number.
; 
FUNCTION FXPOPENR_TMPNAME, CMD
@fxfilter
  COMMON FXPOPEN_TMPNAME, RANDOM_SEED, SEQ_COUNTER
  IF N_ELEMENTS(RANDOM_SEED) EQ 0 THEN BEGIN
      RANDOM_VAL  = LONG(SYSTIME(1))
      RANDOM_SEED = LONG(RANDOMU(RANDOM_VAL)*DOUBLE(ISHFT(1L,31)))
      SEQ_COUNTER = 0L
  ENDIF

  ;; Take the first fifteen and  characters of the command
  TMPNAME = STRCOMPRESS(CMD, /REMOVE_ALL)

  ;; Build a unique hash name based on the command, the current time,
  ;; and a session-specific seed.  Possible problem here: if several
  ;; sessions are started at the same time with the same command, and
  ;; the commands are executed at the same second, then the temporary
  ;; name will be the same.  I judge the likelihood of all of these
  ;; events to be small.
  
  B = BYTE(CMD) & N = N_ELEMENTS(B)
  ;; Construct a semi-unique hash value for the command string
  HASH = 0L
  FOR I = 0L, N-1 DO HASH = ISHFT(HASH, 2) XOR B(I)
  HASH = HASH XOR LONG(SYSTIME(1)) XOR RANDOM_SEED XOR ISHFT(SEQ_COUNTER,16)
  SEQ_COUNTER = SEQ_COUNTER + 1

  IF STRLEN(TMPNAME) GT 20 THEN BEGIN
      TMPNAME = STRMID(TMPNAME, 0, 15) + STRMID(TMPNAME, N-6, 5)
      N = 20L
  ENDIF
  NEWNAME = ''
  ;; Strip away any non-alpha characters
  FOR I = 0L, N-1 DO BEGIN
      CC = STRMID(TMPNAME, I, 1)
      IF NOT (CC EQ ' ' OR CC EQ '>' OR CC EQ '&' OR CC EQ '|' OR $
              CC EQ '/' OR CC EQ '*' OR CC EQ '?' OR CC EQ '<' OR $
              CC EQ '\' OR $
              CC EQ '"' OR CC EQ "'") THEN $
        NEWNAME = NEWNAME + CC
  ENDFOR
  IF NEWNAME EQ '' THEN NEWNAME = 'fxp'

  RETURN, SCRATCH_DIR + NEWNAME + STRING(ABS(HASH), FORMAT='(Z8.8)')
END

;; Main entry
PRO FXPOPENR, UNIT, CMD, ERRMSG=ERRMSG, ERROR=error, COMPRESS=compress

;; Access the general FXFILTER family of commons, and the
;; FXPIPE_COMMON, which has pipe-specific info.
  ERROR = -1
@fxfilter
@fxpcommn

  IF N_PARAMS() LT 2 THEN BEGIN
      MESSAGE = 'Syntax:  FXPOPEN, UNIT, COMMAND'
      GOTO, ERR_RETURN
  ENDIF

  ;; Initialize filter flags
  FFLAGS = 1L

  if NOT keyword_set(compress) then begin

      ;; Sorry, useful pipes are only available under Unix.
      IF STRUPCASE(!VERSION.OS_FAMILY) NE 'UNIX' THEN BEGIN
          MESSAGE = 'ERROR: FXPOPENR ONLY FUNCTIONS ON UNIX SYSTEMS.'
          GOTO, ERR_RETURN
      ENDIF

      ;; --------- Begin pipe section
      ;; Wrap the command to make sure it is safe
      NEWCMD = CMD
      FXPOPENR_WRAP_CMD, NEWCMD, SHELL
      
      ;; Run the program
      OLDSHELL = GETENV('SHELL')
      ON_IOERROR, SPAWN_FAILED
      IF OLDSHELL NE '/bin/sh' THEN SETENV, 'SHELL=/bin/sh'
      SPAWN, NEWCMD, UNIT=UNIT, PID=PID
      ON_IOERROR, NULL
      SETENV, 'SHELL='+OLDSHELL(0)
      
      ;; Check for error conditions
      IF UNIT LT 1L OR UNIT GT 128L THEN BEGIN
          SPAWN_FAILED:
          SETENV, 'SHELL='+OLDSHELL(0)
          MESSAGE = 'ERROR: SPAWN of "'+NEWCMD+'" FAILED'
          GOTO, ERR_RETURN
      ENDIF

      FFLAGS = FFLAGS OR 2  ;; This is a pipe
      ;; ---- End pipe section
  endif else begin
      
      ;; Compressed data - no PID
      PID = 0L

      OPENR, UNIT, CMD, /get_lun, /compress, error=error
      if error NE 0 then begin
          MESSAGE = 'ERROR: OPEN of compressed file "'+CMD+'" FAILED'
          GOTO, ERR_RETURN
      endif

      ;; FFLAGS (unchanged since it is not a pipe)

  endelse

  ;; Prepare the FXFILTER dispatch table for function calls
  FILTERFLAG(UNIT)  = 1            ;; Flags: XXX will be updated below!
  SEEK_CMD(UNIT)    = 'FXPSEEK'
  READ_CMD(UNIT)    = 'FXPREAD'
  WRITE_CMD(UNIT)   = '-'          ;; This pipe is not writable
  CLOSE_CMD(UNIT)   = 'FXPCLOSE'

  ;; Start filling in the FXPIPE_COMMON
  POINTER(UNIT)    = 0L            ;; Start of pipe
  PROCESS_ID(UNIT) = PID           ;; Save process ID of pipe
  
  ;; Build a unique cache name
  CACHE_FILENAME = FXPOPENR_TMPNAME(CMD)

  ;; Open the output cache file, retrieving a LUN
  ON_IOERROR, OPEN_ERROR
  OPENW, CACHE, CACHE_FILENAME, /GET_LUN, /DELETE
  ON_IOERROR, NULL

  FFLAGS = FFLAGS OR 4             ;; On-disk backing store

  ;; Error condition on the cache file
  IF CACHE LT 1 OR CACHE GT 128 THEN BEGIN
      OPEN_ERROR:

      ;; Reset to default behavior
      FILTERFLAG(UNIT)  = 0
      SEEK_CMD(UNIT)    = ''
      READ_CMD(UNIT)    = ''
      WRITE_CMD(UNIT)   = ''
      CLOSE_CMD(UNIT)   = ''
      FREE_LUN, UNIT

      MESSAGE = 'ERROR: Unable to open cache file ' + STRTRIM(CACHE_FILENAME,2)
      GOTO, ERR_RETURN
  ENDIF

  ;; Finish filling the pipe information
  CACHE_UNIT(UNIT)  = CACHE
  CACHE_FILE(UNIT)  = CACHE_FILENAME
  POINTER(UNIT)     = 0  ;; At beginning of pipe
  CACHE_LEN(UNIT)   = 0  ;; Currently no data cached
  CACHE_MAX(UNIT)   = 0  ;; Currently no storage allocated for cache
  EOF_REACHED(UNIT) = 0  ;; Not known to be at end-of-file

  ;; Update filter flags
  FILTERFLAG(UNIT)  = FFLAGS        

  GOOD_RETURN:
  ERROR = 0
  RETURN

  ERR_RETURN:
  IF N_ELEMENTS(ERRMSG) NE 0 THEN BEGIN
      ERRMSG = MESSAGE
      RETURN
  ENDIF ELSE MESSAGE, MESSAGE
  RETURN
  
END
