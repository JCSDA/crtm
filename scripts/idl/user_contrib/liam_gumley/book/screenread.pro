FUNCTION SCREENREAD, X0, Y0, NX, NY, DEPTH=DEPTH

;- Check arguments
if (n_elements(x0) eq 0) then x0 = 0
if (n_elements(y0) eq 0) then y0 = 0
if (n_elements(nx) eq 0) then nx = !d.x_vsize - x0
if (n_elements(ny) eq 0) then ny = !d.y_vsize - y0

;- Check for TVRD capable device
tvrd_true = !d.flags and 128
if (tvrd_true eq 0) then message, $
  'TVRD is not supported on this device: ' + !d.name

;- On devices which support windows, check for open window
win_true = !d.flags and 256
if (win_true gt 0) and (!d.window lt 0) then message, $
  'No graphics window are open'

;- Get IDL version number
version = float(!version.release)

;- Get display depth
depth = 8
if (win_true gt 0) then begin
  if (version ge 5.1) then begin
    device, get_visual_depth=depth
  endif else begin
    if (!d.n_colors gt 256) then depth = 24
  endelse
endif

;- Set decomposed color mode on 24-bit displays
if (depth gt 8) then begin
  entry_decomposed = 0
  if (version gt 5.1) then $
    device, get_decomposed=entry_decomposed
  device, decomposed=1
endif

;- Get the contents of the window
if (depth gt 8) then true = 1 else true = 0
image = tvrd(x0, y0, nx, ny, order=0, true=true)

;- Restore decomposed color mode on 24-bit displays
if (depth gt 8) then device, decomposed=entry_decomposed

;- Return result to caller
return, image

END
