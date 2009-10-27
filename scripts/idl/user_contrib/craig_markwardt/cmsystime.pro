;+
; NAME:
;   CMSYSTIME
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Compute seconds since Jan 1, 1970 and (Modified) Julian Days
;
; CALLING SEQUENCE:
;   TIMEVAL1 = CMSYSTIME(TIMEVAL0, ...)
;
; DESCRIPTION: 
;
;   CMSYSTIME serves two functions.  It computes the current time in a
;   fashion similar to the built-in IDL system function SYSTIME().  It
;   also can convert between various time representations and systems,
;   including a textual format.
;
;   The current time can be obtained by invoking CMSYSTIME with the
;   /NOW keyword (which is entirely equivalent to SYSTIME(1)).
;
;   The most substantial part of CMSYSTIME, which distinguishes it
;   from SYSTIME, is its ability to convert between different time
;   formats.  CMSYSTIME recognizes can recognize and convert between
;   time in seconds (seconds since Jan 1, 1970 [ = SEC ]) and days
;   (Julian days [ = JDAY ] or "Modified" Julian days [ = MJD = JDAY -
;   2400000.5 ]).  It can also recognize and convert between local and
;   GM time.  
;
;   CMSYSTIME takes maximum care to preserve the full numerical
;   precision of the time values.  It converts all values to double
;   precision and may return days and seconds with fractional parts.
;
;   CMSYSTIME can also represent any time textually, not just the
;   current time.  The following textual formats are supported:
;        DOW MMM DD hh:mm:ss YYYY              - (Default - same as SYSTIME)
;        DOW MMM DD YYYY hh:mm:ss.uuuuuu TTTTT - (/EXTENDED)
;   where DOW and MMM are the abbreviated day of week and month in
;   English, DD is the day of the month, YYYY is the year, hh:mm:ss is
;   the time in 24 hr military time, uuuuuu are additional
;   microseconds, TTTTT is the timezone offset (in +hhmm
;   representation).
;
;   CMSYSTIME accepts one parameter, the input time to be converted.
;   Unlike SYSTIME, the *function* of CMSYSTIME is governed by various
;   keywords, as summarized in the following table:
;
;   Converting from                       Converting to
;   ---------------                       -------------
;   JDAY - /FROM_JULIAN                   JDAY - /JULIAN
;   MJD  - /FROM_MJD                      MJD  - /MJD
;   SEC  - (Default)                      SEC  - /SECONDS
;   Current time - /NOW                   TEXT - (Default or /EXTENDED)
;
;   Local time - /FROM_LOCAL              Local time - /LOCAL
;   GM time - (Default)                   GM time - (Default)
;   
;   If no argument is specified, the default is to report the current
;   time textually in the GM time zone.  CMSYSTIME automatically
;   determines the local time zone.
;
; INPUTS:
;
;   TIMEVAL0 - input time, in seconds or days, as described above.
;              This value is ignored if the NOW keyword is set.  Array
;              values are allowed.
;
; KEYWORDS:
;
;   NOW - If set, TIMEVAL0 is ignored and the current time is used as
;         input.
;
;   FROM_JULIAN - If set, TIMEVAL0 is in Julian days.
;   FROM_MJD    - If set, TIMEVAL0 is in Modified Julian days (MJD).
;   FROM_LOCAL  - If set, TIMEVAL0 is in the local time zone.
;                 If no FROM_ keywords are set, the input is assumed
;                 to be seconds from Jan 1, 1970.
;
;   JULIAN  - If set, the input is converted to Julian days upon output.
;   MJD     - If set, the input is converted to MJD upon output.
;   SECONDS - If set, the input is converted to seconds from Jan
;             1, 1970 upon output.
;   LOCAL   - If set, the input is converted to the local time zone.
;             If no "destination" keywords are set, the output is
;             converted to textual representation.
;
;   EXTENDED - Convert to a textual representation with additional
;              information, as noted above.
;
;   TIMEZONE - Upon output, the timezone offset is returned in this
;              keyword.  The offset is time difference in seconds
;              between GM time and the local time, such that LOCALTIME
;              = GMTIME + TIMEZONE
;
; RETURNS:
;   The resulting converted time(s), either as a double precision
;   number or a string.
;
; EXAMPLE:
;   
;   The equivalent to SYSTIME(0)
;     IDL> print, systime(0) & print, cmsystime(/now, /local)
;     Wed Jul  5 12:10:46 2000
;     Wed Jul  5 12:10:46 2000
;
;   The equivalent to SYSTIME(1)
;     IDL> print, systime(1) & print, cmsystime(/now,/seconds)
;        9.6277750e+08
;        9.6277750e+08
;
;   Comparison between local and GM time zones (I live in the Eastern
;    US, daylight savings)
;     IDL> print, cmsystime(/now,/extended)
;     Wed Jul  5 2000 16:13:15.659000 -0400
;     IDL> print, cmsystime(/now,/local,/extended)
;     Wed Jul  5 2000 12:13:15.664000 -0400
;    
;   What day of the week was it 200 days ago?  (Note, there are 86400
;    seconds in one day)
;     IDL> today = cmsystime(/now,/seconds)
;     IDL> print, cmsystime(today-86400L*200, /local)
;     Sat Dec 18 12:17:52 1999
;    
;
; SEE ALSO:
;
;   SYSTIME, JULDAY, CALDAT
;
; MODIFICATION HISTORY:
;   Written, CM, 05 Jul 2000
;   Printed time zone is zero when LOCAL=0, CM, 21 Aug 2000
;   Corrected behavior of /MJD (Thanks to Marshall Perrin), 03 Jun
;     2002
;   Corrected local vs. UTC problem caused by fractional UTC seconds,
;     (thanks to J. Wolfe) CM, 28 Dec 2005
;   Corrected problem with Julian day arrays, (thanks to W. Landsman),
;     CM, 29 Dec 2005
;
;  $Id$
;
;-
; Copyright (C) 2000,2002,2005, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
forward_function cmsystime_xmod, cmsystime

;; Comput X MOD M, ensuring positive remainder
function cmsystime_xmod, x, m
  return, (((x MOD m) + m) MOD m)
end

;; Convert from MJD to YR/MO/DAY
pro cmsystime_mjd2ymd, mjd, yr, mo, da
  offset = 2400000.5D
  offset_int = floor(offset)         ;; Integer part of offset
  offset_fra = offset - offset_int   ;; Fractional part of offset
  nn = offset_fra + mjd
  jd_fra = cmsystime_xmod(nn+0.5D, 1D) - 0.5D
  nn = nn + offset_int - jd_fra
  nn = nn + (floor(floor((nn - 4479.5D)/36524.25D) * 0.75D + 0.5D)-37.D)
  
  yr = long(floor(nn/365.25D) - 4712.D)
  dd = floor(cmsystime_xmod(nn-59.25D, 365.25D))
  
  mo = floor(cmsystime_xmod( floor((dd+0.5D)/30.6D) + 2.D, 12.D ) + 1.D)
  da = floor(cmsystime_xmod(dd+0.5D, 30.6D) + 1.D ) + 0.5D + jd_fra
end

function cmsystime, arg0, now=now, extended=extended, $
                    local=local, from_local=from_local, $
                    julian=jul, from_julian=from_julian, $
                    mjd=mjd, from_mjd=from_mjd, $
                    seconds=seconds, timezone=timezone

  common cmsystime_common, cmsystime_timezone, cmsystime_months, $
    cmsystime_dow

  ;; Precompute names of days in week and month
  if n_elements(cmsystime_months) EQ 0 then begin
      cmsystime_months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
                          'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
      cmsystime_dow = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat']
  endif

  ;; Starting epoch, expressed in MJD and Julian days
  MJD_1970 = 40587D
  JD_1970  = MJD_1970 + 2400000.5D

  ;; Figure the time zone automatically, the first time around
  if n_elements(cmsystime_timezone) EQ 0 then begin
      ;; The GM time, converted to MDY
      gmtime = systime(1)
      cltime = systime(0)
      gmfrac = gmtime MOD 86400
      gm_mjd = floor(gmtime-gmfrac)/86400D + MJD_1970
      cmsystime_mjd2ymd, gm_mjd, gm_yr, gm_mo, gm_da
      gm_da = round(gm_da)

      ;; The local time
      ltime = strtrim(str_sep(strcompress(cltime),' '),2)
      da = floor(long(ltime(2)))
      ltimes = double(str_sep(ltime(3), ':'))
      lfrac = ltimes(2) + 60D*(ltimes(1) + 60D*ltimes(0))

      ;; The timezone difference...
      tz = lfrac - gmfrac
      ;; ... but we must account for day wrap-around
      if      gm_da EQ da - 1 then tz = tz + 86400 $
      else if gm_da EQ da + 1 then tz = tz - 86400 $
      else if gm_da LT da     then tz = tz - 86400 $  ;; ...and month roll-over
      else if gm_da GT da     then tz = tz + 86400    ;; ...and month roll-over

      ;; Store the new value
      cmsystime_timezone = round(tz/60)*60  ;; Round to nearest minute
  endif
  timezone = cmsystime_timezone

  ;; Compute the timezone offset, depending on which way the
  ;; conversion will go.
  offset = 0D
  if keyword_set(from_local) then offset = offset - timezone
  if keyword_set(local)      then offset = offset + timezone

  ;; Extract the time value either from the clock, or from the user
  ;; parameter
  if keyword_set(now) then begin
      ;; From clock (GMT)
      NOW_TIME:
      arg = systime(1)
      if keyword_set(from_local) then offset = 0D

  endif else begin
      ;; From user parameter
      if n_elements(arg0) EQ 0 then goto, NOW_TIME
      arg = double(arg0)

      if keyword_set(from_mjd) then begin
          ;; Convert from MJD ... avoid loss of numerical precision
          if keyword_set(mjd) then return, arg + offset
          if keyword_set(jul) then return, arg + offset + (JD_1970-MJD_1970)

          ;; Convert to seconds
          arg = (arg - MJD_1970) * 86400D

      endif else if keyword_set(from_julian) then begin
          ;; Convert from JD ... avoid loss of numerical precision if poss.
          if keyword_set(mjd) then return, arg + offset - (JD_1970-MJD_1970)
          if keyword_set(jul) then return, arg + offset

          ;; Convert to seconds
          arg = (arg - JD_1970)  * 86400D

      endif
  endelse

  ;; Add timezone offset
  if offset NE 0 then arg = arg + offset
  if keyword_set(seconds) then return, arg
  if keyword_set(jul)     then return, (arg / 86400D) +  JD_1970
  if keyword_set(mjd)     then return, (arg / 86400D) + MJD_1970

  ;; Convert to MJD, from there to MDY
  mjd = floor(arg/86400D) + MJD_1970
  dsecs = arg-floor(arg/86400D)*86400D
  hr = floor(dsecs / 3600) & dsecs = dsecs - hr*3600
  mi = floor(dsecs / 60)   & dsecs = dsecs - mi*60
  se = dsecs
  cmsystime_mjd2ymd, mjd, yr, mo, da

  ;; Day of week is simple to calculate, assumes 13 May 2000 was a Sunday
  dow = cmsystime_xmod((floor(mjd) - 51678L), 7L)

  ;; Compute the string values, unfortunately on an individual basis
  n = n_elements(yr)
  result = strarr(n)
  if keyword_set(extended) then begin
      for i = 0L, n-1 do begin
          sei = floor(se(i))
          sef = floor((se(i) - sei)*1000000D)
          result(i) = string(cmsystime_dow(dow(i)), cmsystime_months(mo(i)-1),$
                             da(i), yr(i), hr(i), mi(i), sei, sef, $
                             format=('(A3," ",A3," ",I2," ",I4.4," ",' + $
                                     'I2.2,":",I2.2,":",I2.2,".",I6.6)'))
      endfor

      ;; Extended string value includes time zone offset
      if keyword_set(local) then tzz = timezone else tzz = 0L
      tzabs = abs(tzz)
      tzhr = floor(tzabs/3600)
      tzstring = string(tzhr, floor(tzabs - tzhr*3600), $
                        format='(I2.2,I2.2)')
      if tzz LT 0 then tzstring = ' -'+tzstring $
      else             tzstring = ' +'+tzstring
      result = result + tzstring

  endif else begin
      for i = 0L, n-1 do begin
          result(i) = string(cmsystime_dow(dow(i)), cmsystime_months(mo(i)-1),$
                             da(i), hr(i), mi(i), floor(se(i)), yr(i), $
                             format=('(A3," ",A3," ",I2," ",' + $
                                     'I2.2,":",I2.2,":",I2.2," ",I4.4)'))
      endfor
  endelse

  return, result
end

