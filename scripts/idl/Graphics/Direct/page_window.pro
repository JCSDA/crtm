function page_window, landscape = landscape, $
                      title = title, $
                      xsize = xsize, $
                      ysize = ysize

  device, get_screen_size = sz
  ysize = sz( 1 ) - 100
  xsize = fix( 8.5 * ysize / 11.0 )
  if keyword_set( landscape ) then begin
    tmp = ysize
    ysize = xsize
    xsize = tmp
  endif

  window, /free, xsize = xsize, ysize = ysize, title = title

  return, !d.window

end
  
