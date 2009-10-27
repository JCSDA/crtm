;+
; NAME:
;   FXMAKEMAP
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Change configuration of FXG - FXFILTER'ed behavior
;
; MAJOR TOPICS:
;   File I/O, Pipes, URLs, FITS
;
; CALLING SEQUENCE:
;   FXMAKEMAP, SUFFIX, COMMAND              or
;   FXMAKEMAP, /INFO                        or
;   FXMAKEMAP, [SCRATCH_DIR=scratch,] [BUFFER_MAX=bufmax,] 
;     [BUFFER_GRAN=bufgran,] [RM_COMMAND=rmcommand,] [/GET]
;
; DESCRIPTION:
;
;   FXMAKEMAP queries or sets the behavior of the FXFILTER family of
;   functions (FXGOPEN, FXGREAD, FXGWRITE, FXGSEEK, and FXGCLOSE).
;
;   To add a new file extension mapping, which associates a filename
;   suffix with a particular Unix pipe command, use the first form of
;   the command.
;
;   To print the current settings, including the file extension maps,
;   use the /INFO form of the command.
;
;   To set an individual parameter, call FXMAKEMAP with the
;   appropriate keyword argument.
;
;   To query an individual parameter, call FXMAKEMAP with /GET and the
;   appropriate keyword argument.
;
; INPUTS:
;
;   SUFFIX - the trailing suffix of the filename to be associated,
;            *without* the period.  For example, for a gzipped file,
;            the suffix is 'gz'
;
;   COMMAND - an IDL-style format command which specifies how the
;             filename should be converted into a Unix pipe command.
;             The actual command is constructed by passing the
;             filename to STRING() with this format string.  For
;             example, to convert a gzip file the proper format string
;             is, '("gzip -dc ",A0)'.
;
; KEYWORD PARAMETERS:
;
;   INFO - print the current settings and return.
;  
;   GET - if this keyword is set, then the following keyword commands
;         cause the current setting to be returned in the specified
;         keyword.  Otherwise the default is to assert a new setting.
;
;   BUFFER_GRAN - the buffer granularity in bytes.  I/O operations on
;                 pipes and streams are performed in multiples of this
;                 size.  Default: 4096 bytes.
;
;   BUFFER_MAX - the maximum allowed buffer size in bytes.  I/O
;                operations on pipes and streams are performed with at
;                most this many bytes.  Default: 32 kbytes.
;
;   RM_COMMAND - the Unix command used to delete files.  
;                Default:  '/bin/rm'
;
;   SCRATCH_DIR - the scratch directory where cache files are stored.
;                 When operations on Unix pipes or streams are
;                 performed, the data are stored in individual files
;                 in this directory.
;
; MODIFICATION HISTORY:
;   Written, 1999, CM
;   Documented, 02 Oct 1999, CM
;   Changed copyright notice, 21 Sep 2000, CM
;
;  $Id$
;
;-
; Copyright (C) 1999-2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro fxmakemap, suffix, command, get=get, $
               scratch_dir=scratch, buffer_max=buffmax, buffer_gran=buffgran, $
               rm_command=rmcmd, info=info, flags=flags

@fxfilter

  if NOT keyword_set(get) then begin
      processed = 0
      if n_elements(scratch) GT 0 then begin
          scratch_dir = strtrim(scratch, 2)
          processed = 1
      endif
      if n_elements(buffmax) GT 0 then begin
          buffer_max  = long(buffmax)
          processed = 1
      endif
      if n_elements(buffgran) GT 0 then begin
          buffer_gran = long(buffgran)
          processed = 1
      endif
      if n_elements(rmcmd) GT 0 then begin
          rm_command  = strtrim(rmcmd, 2)
          processed = 1
      endif
  endif
  if keyword_set(get) then begin
      suffix   = (filters(0,*))(*)
      command  = (filters(1,*))(*)
      scratch  = scratch_dir
      buffmax  = buffer_max
      buffgran = buffer_gran
      rmcmd    = rm_command
      return
  endif

  if keyword_set(info) then begin
      print
      print, scratch_dir, $
        format='("    Scratch Directory: ''",A0,"''")'
      print, rm_command, $
        format='("       Delete Command: ''",A0,"''")'
      print, buffer_max, $
        format='("     Buffer Max. Size: ",I0,"L")'
      print, buffer_gran, $
        format='("   Buffer Granularity: ",I0,"L")'
      print
      print, '  FILTER SUFFIX MAPPINGS'
      print, '  ----------------------'
      for i = 0L, n_elements(filters)/3 - 1 do begin
          print, filters(0,i), filters(1,i), $
            format='("  Suffix: ",A8,"  Pipe Command: ''",A0,"''")'
          if filters(2,i) NE '' then $
            print, '          (flags: ',filters(2,i),')'
      end
      return
  endif

  if n_params() EQ 0 then begin

      if processed then return
      ;; If no parameters and no keywords were passed

      message, "USAGE: FXMAKEMAP, SUFFIX, COMMAND    [, /GET ]", /info
      message, "       FXMAKEMAP, SCRATCH_DIR=string [, /GET ]", /info
      message, "       FXMAKEMAP, BUFFER_MAX=long    [, /GET ]", /info
      message, "       FXMAKEMAP, BUFFER_GRAN=long   [, /GET ]", /info
      message, "       FXMAKEMAP, RM_COMMAND=string  [, /GET ]", /info
      message, "       FXMAKEMAP, /INFO    - prints current settings", /info
      return
  endif

  ;; Add the suffix to the list if needed.
  wh = where(suffix EQ filters(0,*), ct)
  if ct EQ 0 then begin
      filters = [filters(*), suffix, command, '']
      nfilters = n_elements(filters)/3
      filters = reform(filters, 3, nfilters, /overwrite)
  endif else begin
      filters(0:1,wh) = [suffix, command]
  endelse

  ;; Add flags if necessary
  if n_elements(flags) GT 0 then begin
      wh = where(suffix EQ filters(0,*), ct)
      wh = wh(0)
      newflags = strtrim(strupcase(flags),2)
      newflags = strjoin(newflags,'|')

      oflags = filters(2,wh)
      if oflags EQ '' then begin
          filters(2,wh) = newflags
      endif else begin
          filters(2,wh) = oflags+'|'+newflags
      endelse
  endif
  

end

