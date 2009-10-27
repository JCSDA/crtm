;+
; NAME:
;   FXPCLOSE
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Internal routine to close a pipe file.
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
; PARAMETERS
;   unit - LUN of the pipe, *not* the cache file.
;
; Side effects
;   The pipe is closed.
;   The cache file is closed and deleted.
;
; MODIFICATION HISTORY:
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

PRO FXPCLOSE, UNIT

@fxfilter
@fxpcommn

  ;; NOTE: Now the cache file is opened with the /DELETE keyword, so
  ;; there should be no need to delete the file explicitly

  ;; EXEC = [RM_COMMAND, '-f', CACHE_FILE(UNIT)]
  ;; NOSHELL is for speed, and because there are guaranteed no wildcards.
  ;; SPAWN, EXEC, /NOSHELL

  FREE_LUN, UNIT               ;; Close pipe (no need to kill)
  FREE_LUN, CACHE_UNIT(UNIT)   ;; Close backing store (should delete-on-close)

  RETURN
END
