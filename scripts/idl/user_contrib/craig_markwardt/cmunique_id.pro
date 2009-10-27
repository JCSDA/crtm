;+
; NAME:
;   CMUNIQUE_ID
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Return a unique ID string
;
; CALLING SEQUENCE:
;   ID = CMUNIQUE_ID([STRINGVAL])
;
; DESCRIPTION: 
;
;   CMUNIQUE_ID returns a "unique" 8 character identifier.
;   Programmers can use this routine to derive unique strings which
;   can be used to write files, etc.
;
;   Identifiers are "unique" in the sense that it is unlikely that two
;   identifiers in a given IDL session will coincide.  Thus the
;   identifier is useful for constructing temporary filenames and
;   other hash values.  User routines are encouraged to append other
;   identifying information to this string, such as a session id, a
;   hostname, or a process id number.
;
;   The identifier is computed from various sources of random
;   information.  Users may supply additional information to be
;   scrambled into the identifier by passing the FODDER parameter.
;   CMUNIQUE_ID will return a different identifier upon each call,
;   with or without the FODDER keyword.  It maintains an internal
;   sequence counter, and and also scrambles in the system time.
;   These practices should ensure that successive identifiers are
;   different from one another.
;
; INPUTS:
;
;   FODDER - Any scalar string value.  These values are used to
;            additionally scramble the identifier.
;
; KEYWORDS:
;
;   NONE
;
; RETURNS:
;   The 8-character identifier string.
;
; EXAMPLE:
;   
;   Print two distinct identifiers.
;     IDL> print, cmunique_id(), ' ', cmunique_id()
;     29C47600 79061C57
;
; SEE ALSO:
;
;   NONE
;
; MODIFICATION HISTORY:
;   Written, CM, 11 Jan 2001
;
; $Id$
;
;-
; Copyright (C) 2001, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
function cmunique_id, fodder

  ;; Store persistent information in a common block, including the
  ;; random number seed
  COMMON CMUNIQUE_ID_COMMON, RANDOM_SEED, SEQ_COUNTER
  IF N_ELEMENTS(RANDOM_SEED) EQ 0 THEN BEGIN
      RANDOM_VAL  = LONG(SYSTIME(1))
      RR          = RANDOM_VAL
      RANDOM_SEED = LONG(RANDOMU(RR)*DOUBLE(ISHFT(1L,31)))
      RANDOM_SEED = RR
      SEQ_COUNTER = 0L
  ENDIF

  ;; Mix up a few random numbers.  The low-bit behavior of IDL's
  ;; random number generator seems pretty poor.
  rr = random_seed
  hash1 = long(randomu(rr)*double(ishft(1L,31)))
  hash2 = long(randomu(rr)*double(ishft(1L,31)))
  hash = hash1 XOR (ishft(hash2,-16) OR ishft(hash2,+16))
  random_seed = rr

  ;; Include the fodder, which is any unique identifying text string
  if n_elements(fodder) GT 0 then begin
      b = byte(fodder(0))
      n = n_elements(b)
      for i = 0L, n-1 do hash = ishft(hash,2) XOR b(i)
  endif

  ;; Finally add some other entropy terms like the time, and a unique
  ;; counter.  For safety, add these at different bit zones in the
  ;; output hash so they don't overlap (since time and the sequence
  ;; counter are both monotonically increasing, they could counteract
  ;; each other).
  hash = hash XOR long(systime(1)) XOR ishft(SEQ_COUNTER,16)
  seq_counter = seq_counter+1

  ;; Return a hex-string of this hash value
  return, string(abs(hash), format='(Z8.8)')
end


