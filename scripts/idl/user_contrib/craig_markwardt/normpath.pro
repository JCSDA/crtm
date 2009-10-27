;+
; NAME:
;   NORMPATH
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Construct an absolute file/directory path from a relative path
;
; MAJOR TOPICS:
;   Files, I/O
;
; CALLING SEQUENCE:
;   NORMPATH, FROM, NORMALIZED
;
; DESCRIPTION:
;
;   NORMPATH constructs a "normalized" filename or directory path from
;   a specified relative path.  The relative path may contain path
;   components which move up and/or down the hierarchy of the file
;   system.  The returned path will be the most absolute path that can
;   be specified.
;
;   If the user specifies the CURRENT keyword, then relative paths are
;   assumed to originate in the CURRENT directory.  If CURRENT is not
;   specified, then it is possible for the returned path to have
;   path components relative to the current directory.
;
;   NORMPATH should be platform independent.  Note that the paths do
;   not necessarily need to exist on the file system.
;
;
; INPUTS:
;
;   FROM - scalar string, gives the relative path.
;
;   NORMALIZED - upon return, the normalized form of FROM.
;
;
; KEYWORDS:

;   CURRENT - if specified, must be a scalar string which gives the
;             path to the current directory used in forming the
;             normalized path.  If not specified, then the returned
;             path may have some relative components.
;
; EXAMPLES:
;
;   NORMPATH, '/x/y/z', path & print, path
;     '/x/y/z'
;   The specified path is already normalized, so there is no action
;
;   NORMPATH, '/x/y/../w/z', path & print, path
;     '/x/w/z'
;   The specified path had relative components which were removed.
;
;   NORMPATH, '../x/y/../w/z', path & print, path
;     '../x/w/z'
;   The specified path had relative components which could not be
;   removed.
;
;   NORMPATH, '../x/y/../w/z', path, current='/root' & print, path
;     '/x/w/z'
;   The absolute path of the current directory was given (and then the
;   relative path moved outside of /root).
;
; MODIFICATION HISTORY:
;   Written and documented, 12 Mar 2004 CM
;   Usage message, 23 Mar 2008 CM
;   Handle case of 'a//b', which should be 'a/b', 23 Mar 2008 CM
;
;  $Id$
;
;-
; Copyright (C) 2004, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro normpath, from0, normalized, current=cwd0

  normalized = 1
  dummy = temporary(normalized)
  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, '    NORMPATH, UNNORMSTR, NORMSTR',/info
      message, '      UNNORMSTR - scalar string, path to be normalized',/info
      message, '      NORMSTR - upon return, normalized result',/info
      return
  endif
  psep = path_sep()
  up   = path_sep(/parent)
  if up EQ '..' then here = '.' else here = '@HERE/DIR@'

  ;; Default current directory is taken from task, if not otherwise
  ;; supplied
  from = from0
  if n_elements(cwd0) GT 0 AND strpos(from0,psep) NE 0 then $
    from = strtrim(cwd0(0),2) + psep + from

  fromc = str_sep(from, psep)
  if fromc(0) EQ '' then fromc(0) = '/@ROOT@'
  fromd = ['']
  for i = 0, n_elements(fromc)-1 do begin
      if fromc(i) EQ up then begin
          nd = n_elements(fromd)
          if nd EQ 1 OR fromd(nd-1) EQ up then begin
              fromd = [fromd, up]
          endif else begin
              if fromd(nd-1) NE '/@ROOT@' then $
                fromd = fromd(0:nd-2)
          endelse
      endif else if (fromc(i) EQ here or fromc(i) EQ '') then begin
          ;; Do nothing for current-dir
      endif else begin
          fromd = [fromd, fromc(i)]
      endelse
  endfor
  
  fromd = fromd(1:*)
  if fromd(0) EQ '/@ROOT@' then fromd(0) = ''

  forward_function strjoin
  normalized = strjoin(fromd, psep)

  return
end
