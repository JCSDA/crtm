PRO IMSIZE, IMAGE, X0, Y0, XSIZE, YSIZE, ASPECT=ASPECT, $
  POSITION=POSITION, MARGIN=MARGIN

;- Check arguments
if (n_params() ne 5) then $
  message, 'Usage: IMSIZE, IMAGE, X0, Y0, XSIZE, YSIZE'
if (n_elements(image) eq 0) then $
  message, 'Argument IMAGE is undefined'
if (n_elements(position) eq 0) then $
  position = [0.0, 0.0, 1.0, 1.0]
if (n_elements(margin) eq 0) then margin = 0.1

;- Get image dimensions
result = size(image)
ndims = result[0]
if (ndims ne 2) then message, 'IMAGE must be a 2D array'
dims = result[1 : ndims]

;- Get aspect ratio for image
if (n_elements(aspect) eq 0) then $
  aspect = float(dims[1]) / float(dims[0])

;- Get approximate image position
position = getpos(aspect, position=position, margin=margin)

;- Compute lower left position of image (device units)
x0 = round(position[0] * !d.x_vsize) > 0L
y0 = round(position[1] * !d.y_vsize) > 0L

;- Compute size of image (device units)
xsize = round((position[2] - position[0]) * !d.x_vsize) > 2L
ysize = round((position[3] - position[1]) * !d.y_vsize) > 2L

;- Recompute the image position based on actual image size
position[0] = x0 / float(!d.x_vsize)
position[1] = y0 / float(!d.y_vsize)
position[2] = (x0 + xsize) / float(!d.x_vsize)
position[3] = (y0 + ysize) / float(!d.y_vsize)

END
