;+
; NAME:
;   RELPATH
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Construct a relative path between two absolute paths
;
; MAJOR TOPICS:
;   Files, I/O
;
; CALLING SEQUENCE:
;   RELPATH, FROM, [/FILE1,] TO, [/FILE2,] RELPATH, [CURRENT=, /INVERT]
;
; DESCRIPTION:
;
;   RELPATH constructs a relative path between two absolute paths.
;   That is, given two file name paths FROM and TO, RELPATH finds the
;   relative path which starts from the current directory of FROM and
;   ends in the directory of TO.
;
;   Note that the /FILE1 or /FILE2 keywords can be used to say whether
;   FROM or TO, respectively, is a file instead of a directory.  This
;   is important because RELPATH finds the relative paths between two
;   directories, and so the file components are ignored for those
;   purposes.
;
;   The INVERT keyword is allows one to invert the path: to find the
;   path from the current directory of TO, to the directory of FROM.
;
;   If the user specifies the CURRENT keyword, then relative paths are
;   assumed to originate in the CURRENT directory.  Otherwise the
;   actual current directory is used.
;
;   NORMPATH should be platform independent.  Note that the paths do
;   not necessarily need to exist on the file system.
;
; INPUTS:
;
;   FROM - scalar string, gives path of starting point (file or
;          directory).
;
;   TO - scalar string, gives path of ending point (file or
;        directory).
;
;   RELPATH - upon return, the relative path from FROM to TO.

;
; KEYWORDS:

;   CURRENT - if specified, must be a scalar string which gives the
;             path to the current directory used in forming the
;             normalized path.  If not specified, then the actual
;             current directory is used.
;
;   INVERT - invert the direction of the relative path, i.e. from TO
;            to FROM.
;
; EXAMPLES:
;
;   RELPATH, '/x/y/z', '/x/u/v', relpath & print, relpath
;     '../../u/v'
;
;   The two paths share a common root in /x.  Therefore, to get to
;   /x/u/v from /x/y/z, one must go up two directory levels and then
;   down into u/v.
;
;
; MODIFICATION HISTORY:
;   Written and documented, 12 Mar 2004 CM
;   Replaced call to STRCAT with STRJOIN, 09 Aug 2006 CM
;   Usage message, 23 Mar 2008 CM
;
;  $Id$
;
;-
; Copyright (C) 2004, 2006, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

pro relpath, from0, to0, relpath, cwd=cwd0, $
             file1=file1, file2=file2, invert=invert

  relpath = 1
  dummy = temporary(relpath)
  if n_params() EQ 0 then begin
      message, 'USAGE:', /info
      message, '    RELPATH, FROM, TO, RESULT, [/INVERT]', /info
      message, '      FROM - scalar string, starting path', /info
      message, '      TO - scalar string, ending path', /info
      message, '      RESULT - upon return, relative path from start to end', /info
      message, '      /INVERT - change sense of result: from end to start', /info
      return
  endif

  ;; OS dependent path separator and parent-directory indicator
  psep = path_sep()
  up   = path_sep(/parent)

  ;; Default current directory is taken from task, if not otherwise
  ;; supplied
  if n_elements(cwd0) EQ 0 then cd, current=cwd $
  else cwd = strtrim(cwd0(0),2)

  if n_elements(from0) EQ 0 then from = '.' $
  else from = strtrim(from0,0)
  if n_elements(to0) EQ 0 then to = '.' $
  else to = strtrim(to0,0)

  ;; Apply current directory if the specified path(s) is/are relative
  if strpos(from, psep) NE 0 then from = cwd + psep + from
  if strpos(to, psep)   NE 0 then to   = cwd + psep + to

  ;; Make sure these are normalized paths
  normpath, from, fromn
  normpath, to, ton

  ;; Break into path components
  fromc = str_sep(fromn, psep)
  if keyword_set(file1) then begin
      file1n = fromc(n_elements(fromc)-1)
      fromc = fromc(0:n_elements(fromc)-2)
  endif
  toc   = str_sep(ton, psep)
  if keyword_set(file2) then begin
      file2n = toc(n_elements(toc)-1)
      toc    = toc(0:n_elements(toc)-2)
  endif

  wh = where(fromc NE toc, ct)
  if ct EQ 0 then begin
      ;; The paths match exactly, so special processing is required
      relpath = ''
      if NOT keyword_set(invert) AND keyword_set(file2) then $
        relpath = relpath+file2n
      if keyword_set(invert) AND keyword_set(file1) then $
        relpath = relpath+file1n
      return
  endif

  wh = wh(0)
  fromc = fromc(wh:*)
  toc   = toc(wh:*)

  if NOT keyword_set(invert) then begin
      ;; As seen from the directory of FROM, how do we traverse to the
      ;; directory/file of TO
      relpathc = [replicate(up, n_elements(fromc)), toc]
      if keyword_set(file2) then relpathc = [relpathc, file2n]
  endif else begin
      ;; As seen from the directory of TO, how do we traverse to the
      ;; directory/file of FOM
      relpathc = [replicate(up, n_elements(toc)), fromc]
      if keyword_set(file1) then relpathc = [relpathc, file1n]
  endelse
  
  relpath = strjoin(relpathc, psep)

  return
end
