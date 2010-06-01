PRO EPLOT, X, Y, NAME=NAME, _EXTRA=EXTRA_KEYWORDS

;- Check arguments
if (n_params() ne 2) then $
  message, 'Usage: EPLOT, X, Y'
if (n_elements(x) eq 0) then $
  message, 'Argument X is undefined'
if (n_elements(y) eq 0) then $
  message, 'Argument Y is undefined'
if (n_elements(name) eq 0) then name = 'Joe Average'

;- Plot the data
plot, x, y, _extra=extra_keywords

;- Print name and date on plot
date = systime()
xyouts, 0.0, 0.0, name, align=0.0, /normal
xyouts, 1.0, 0.0, date, align=1.0, /normal

END
