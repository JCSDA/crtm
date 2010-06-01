FUNCTION IMSCALE, IMAGE, RANGE=RANGE, $
  BOTTOM=BOTTOM, NCOLORS=NCOLORS

;- Check arguments
if (n_params() ne 1) then $
  message, 'Usage: RESULT = IMSCALE(IMAGE)'
if (n_elements(image) eq 0) then $
  message, 'Argument IMAGE is undefined'

;- Check keywords
if (n_elements(range) eq 0) then begin
  min_value = min(image, max=max_value)
  range = [min_value, max_value]
endif
if (n_elements(bottom) eq 0) then bottom = 0B
if (n_elements(ncolors) eq 0) then $
  ncolors = !d.table_size - bottom

;- Return the scaled image
scaled = bytscl(image, min=range[0], max=range[1], $
  top=(ncolors - 1)) + byte(bottom)
return, scaled

END
