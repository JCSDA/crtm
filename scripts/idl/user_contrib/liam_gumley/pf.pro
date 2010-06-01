pro pf, image, x0, y0, data = data, pixel = pixel, fmt = fmt

on_error,2

print, 'Press left or center mouse button for new output line'
print, 'Press right mouse button to exit'

s = size(image)
if s(0) ne 2 then message, 'Image parameter not 2d.'
s(1) = s(1)-1;To n-1
s(2) = s(2)-1

!err=0

if n_elements(x0) le 0 then x0 = 0
if n_elements(y0) le 0 then y0 = 0
if s(s(0)+1) ge 4 then form = 'F' else form = 'I'
cr = string("15b)

;- check data coordinates keyword

if n_elements( data ) eq 0 then begin
  data = 0
  form="($,'x=',i4,', y=',i4,', value=',"+form+",a)"
endif else begin
  data = 1
  sz = size( image )
  nx = sz( 1 )
  ny = sz( 2 )
  xmin = !x.crange( 0 )
  xint = !x.crange( 1 ) - xmin
  ymin = !y.crange( 0 )
  yint = !y.crange( 1 ) - ymin
  if not keyword_set( fmt ) then fmt = 'e12.5'
  if keyword_set( pixel ) then fmt = 'i5'
  form="($,'x=',1x,"+fmt+",', y=',1x,"+fmt+",', value=',"+form+",a)"
endelse

while !err ne 4 do begin

      tvrdc,x,y,2,/dev

      if (!err and 3) ne 0 then begin
         print,form="($,a)",string("12b)
         while (!err ne 0) do begin wait,.05 & tvrdc,x,y,0,/dev & end
      endif

      x = x-x0 & y = y - y0
      
      if data eq 0 then begin
      
        ;- device coordinates

        if (x le s(1)) and (y le s(2)) and (x ge 0) and (y ge 0) then begin
          if (!order eq 1) then yy = s(2) - y else yy = y
          print,form = form, x,y,Image(x,yy),cr
        endif
        
      endif else begin
      
        ;- data coordinates
        
        result = convert_coord( x, y, 0, /device, /to_norm )

        if result( 0 ) ge !x.window( 0 ) and $
           result( 0 ) le !x.window( 1 ) and $
           result( 1 ) ge !y.window( 0 ) and $
           result( 1 ) le !y.window( 1 ) then begin
           result = convert_coord( x, y, 0, /device, /to_data )
           
           xi = fix( ( result( 0 ) - xmin ) / xint * float( nx ) )
           yi = fix( ( result( 1 ) - ymin ) / yint * float( ny ) )

           if xi ge 0 and xi le ( nx - 1 ) and yi ge 0 and yi le ( ny - 1 ) then begin
             if keyword_set( pixel ) then begin
               print, form = form, xi, yi, Image( xi, yi ), cr
             endif else begin
               print, form = form, result( 0 ), result( 1 ), Image( xi, yi ), cr
             endelse
           endif

        endif
        
      endelse

endwhile

print, ''

end
