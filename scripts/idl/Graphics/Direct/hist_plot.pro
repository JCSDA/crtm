PRO HIST_PLOT, DATA, MIN=MIN_VALUE, MAX=MAX_VALUE, $
  BINSIZE=BINSIZE, NORMALIZE=NORMALIZE, FILL=FILL, $
  OVERPLOT=OVERPLOT, _EXTRA=EXTRA_KEYWORDS

; DATA         Array of data
; MIN          Minimum data value for computing histogram
;              (default is MIN(DATA))
; MAX          Maximum data value for computing histogram
;              (default is MAX(DATA))
; BINSIZE      Binsize (default is to create 100 bins)
; NORMALIZE    If set, normalize the histogram
; FILL         If set, fill the histogram
; Also accepts any keywords recognized by PLOT or POLYFILL

;- Check arguments
if n_params() ne 1 then message, 'Usage: HIST_PLOT, DATA'
if n_elements(data) eq 0 then message, 'DATA is undefined'

;- Check keywords
if n_elements(min_value) eq 0 then min_value = min(data)
if n_elements(max_value) eq 0 then max_value = max(data)
if n_elements(binsize) eq 0 then $
  binsize = (max_value - min_value) * 0.01
binsize = binsize > ((max_value - min_value) * 1.0e-5)

;- Compute histogram
hist = histogram(float(data), binsize=binsize, $
  min=min_value, max=max_value)
hist = [hist, 0L]
nhist = n_elements(hist)

;- Normalize histogram if required
if keyword_set(normalize) then $
  hist = hist / float(max(hist))

;- Compute bin values
bins = lindgen(nhist) * binsize + min_value

;- Create plot arrays
x = fltarr(2 * nhist)
x[2 * lindgen(nhist)] = bins
x[2 * lindgen(nhist) + 1] = bins
y = fltarr(2 * nhist)
y[2 * lindgen(nhist)] = hist
y[2 * lindgen(nhist) + 1] = hist
y = shift(y, 1)

;- Plot the histogram
if keyword_set(overplot) then $
 oplot, x, y, _extra=extra_keywords $
else $
 plot, x, y, _extra=extra_keywords

;- Fill the histogram if required
if keyword_set(fill) then $
  polyfill, [x, x[0]], [y, y[0]], _extra=extra_keywords

END
