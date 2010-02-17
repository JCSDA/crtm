;+
; Name:
;	ppi
; Purpose:
;	Make a ppi (plan position indicator) plot, named for the early CRT
;	displays that presented radar data in polar coordinates. 
;	Plot is intensity versus range and azimuth.
;	THere are two input forms: for regularly and irregularly gridded data.
; Examples:
;	rcs = intarr(360,10)		; DUMMY DATA---SPIRAL RAMP
;	for i=0,9 do $
;	  rcs(*,i) = (indgen(360)+i*10) mod 360
;	azi = indgen(360)		; AZIMUTH BINS
;	range = indgen(10)+1		; RANGE BINS
;	ppi,rcs,azi,range		; PPI PLOT:
;					; 1-deg BINS x 1-UNIT RANGE BINS
;	ppi,rcs,azi,range,color=32	; USE 32 COLORS, NOT 8
;	ppi,rcs,azi,range,grid=[12,5]	; ADD GRID AT 12 AZIMUTHS & 5 RANGES
;
;	tc = bytarr(6)			; ARRAY FOR COLORS
;	col = ['black','cyan','green','yellow','red','white']
;	for i = 0,5 do tc(i) = thecolor(col(i))
;	ppi,rcs,azi,range,color=tc	; USE AN ARRAY OF COLOR INDICES
; Usage:
;	ppi,rcs,azimuth,range[,rangebin=rangebin,azimuthbin=azimuthbin][,option=opt]
;  OR
;	ppi,data[,rangebin=rangebin,azimuthbin=azimuthbin][,option=opt]
; Inputs:
;	rcs = 2-D array of rcs data with dimensions nazim x nrange
;	azimuth = 1-D vector of azimuth bins, length nazimuth,
;		 ,in degrees CW from North (up)
;	range = 1-D vector of range bins, length nrange
;  OR
;	data = 2-D array of intensities at (r,theta) with dimensions 3 x npts
;	where 
;		1st dim		contents
;		------- 	-----------------------------
;		0		range to data point
;		1		azimuth of data point in degrees
;		2		intensity of data point
;	and npts = number of points or sectors to plot
; Optional Keywords:
;	rangebin = size of range bin in same units as range (D=range spacing)
;	azimuthbin = size of azimuth bin in degrees (D=azimuth spacing)
;	help = flag to print header
;	rcsrange = data range spanned by colors (D=[min(data),max(data)])
;	colors = # of colors in plot (D=8) or array of color indices
;	step = step in color table between colors.  The default steps
;		through the entire color table. (D=!d.n_colors/colors)
;	title,xtitle,ytitle = titles like plot
;	position = 4-element array for plot position in window---!d.position
;		(D=largest square with margins)
;	rangerange = plot limits (D=auto)
;	grid = 1 or 2-element array to specify the number of azimuth and range
;		grids, (D=none) Example: grid=[12,5] for azimuth every
;		30 degrees and range every 25% of range extent
; Outputs:
;	polar plot to current device
; Common blocks:
;	none
; Procedure:
;	If keyword help is set, call doc_library to print header.
;	Only points are plotted if bin parameters are omitted; otherwise,
;	trapezoids are used to approximate sectors.  For PostScript, the
;	actual PostScript commands are written to the file; for other devices,
;	polyfill is used.  The reason is that polyfill doesn't use the
;	PostScript fill operator.  Instead polyfill uses vectors, which makes
;	a gigantic file.  I tried a few sets of PostScript commands, but I
;	found that a single arcn did the trick.  Previously, I tried the set 
;	<...setrgbcolor newpath...arcn...lineto...arc closepath fill> where
;	the ellipsis means the parameters are omitted.  Using only arcn with a
;	linewidth equal to the rangebin yields a file about half the length.
;	PostScript code is optimized to produce small PS file.
;	A halftone screen could be used for color device and few colors.
; Restrictions:
;	Only tested for Openwindows and PostScript, but should work for any
;	device that allows polyfill.  
; Modification history:
;	write, 11-19 Jun 92, F.K.Knight (knight@ll.mit.edu)
;	add optional azimuthal and range grids, 23 Jun 92, FKK
;	add optional array of color indices, 15 Oct 92, FKK
;	TBD, add an optional color bar
;-
function ppif,value		; ENCODE A FLOAT FOR THE POSTSCRIPT FILE
return,strtrim(string(format='(f6.1)',value),2)
end
function ppil,value		; ENCODE A LONG FOR THE POSTSCRIPT FILE
return,strtrim(long(value),2)
end
function ppi_square,position	; DEFINE POSITION FOR SQUARE PLOT
  xhi = position(2)
  yhi = position(3)
  xwd = !d.x_size * (xhi - position(0))
  ywd = !d.y_size * (yhi - position(1))
  if xwd lt ywd then yhi = float(xwd)/!d.y_size + position(1)
  if xwd ge ywd then xhi = float(ywd)/!d.x_size + position(0)
return,[position(0),position(1),xhi,yhi]
end

pro ppi,data,azimuth,range,rangebin=rangebin,azimuthbin=azimuthbin $
  ,help=help,rcsrange=rcsrange,colors=colors,step=step $
  ,title=title,xtitle=xtitle,ytitle=ytitle,position=position $
  ,rangerange=rangerange,grid=grid
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'ppi' & return & endif
;
;	=====>> CHECK INPUTS & SET DEFAULTS.  THEN PLOT TO SET SCALE.
;
if n_elements(rangerange) eq 0 then rangerange = [0,0]
if total(rangerange) eq 0 then style = 0 else style = 1
if n_elements(title) eq 0 then title = 'PPI PLOT'
if n_elements(xtitle) eq 0 then xtitle = 'RANGE'
if n_elements(ytitle) eq 0 then ytitle = 'RANGE'
if n_elements(colors) eq 0 then colors = 8
if n_elements(step) eq 0 then step = !d.n_colors/colors
if n_elements(rcsrange) eq 0 then rcsrange = [min(data),max(data)]
if n_elements(rangebin) eq 0 then rangebin = range(1) - range(0)
if n_elements(azimuthbin) eq 0 then azimuthbin = azimuth(1) - azimuth(0)
szd = size(data)
if szd(0) ne 2 then message,'First parameter must be 2-D array.'
case n_params(1) of
  1: begin
    if szd(1) ne 3 then message,'First parameter must have dimensions 3 x npts.'
    end
  3: begin
    szr = size(range)
    if szr(0) ne 1 then message,'3rd parameter must be vector.'
    sza = size(azimuth)
    if sza(0) ne 1 then message,'2nd parameter must be vector.'
    if szr(1) ne szd(2) then message,'rcs 2nd dim must be length of range vector.'
    if sza(1) ne szd(1) then message,'rcs 1st dim must be length of azimuth vector.'
    end
  else: message,'Must have 1 or 3 parameters; type ppi,/help.'
endcase
;
;	=====>> FOR WINDOW DEVICE, MAKE AN ~'LY SQUARE PLOTTING AREA
;	=====>> NOT EXACT BECAUSE MARGINS CHANGE WITH ASPECT RATIO
;	=====>> THEN MAKE IT EXACTLY SQUARE BY SETTING POSITION OF PLOT
;
if n_elements(position) ne 4 then begin
  if (!d.flags and 256) ne 0 then begin ; DEVICE SUPPORTS WINDOWS
    if !d.window eq -1 then begin
      httowd = 0.98		        ; ~ SQUARE, ACCOUNTS FOR MARGINS
      plot,[0,1],/nodata,xstyle=4,ystyle=4; JUST SET !x.window & !y.window
      if !x.window(1) ne !x.window(0) then $
        httowd = (!y.window(1)-!y.window(0))/(!x.window(1)-!x.window(0))
      window,xsize=640,ysize=640*httowd 
    endif
  endif
  plot,[0,1],/nodata,xstyle=4,ystyle=4	; JUST SET !x.window & !y.window
  position = ppi_square([!x.window(0),!y.window(0),!x.window(1),!y.window(1)])
endif else begin
  position = ppi_square(position)	; SQUARE UP USER-SPECIFIED POSITION
endelse
;
;	=====>> SET UP THE SECTOR VERTICES
;
dr = rangebin/2.				; RANGE RADIUS
dt = azimuthbin/2.				; AZIMUTH RADIUS
rr = [-dr,-dr,dr,dr,-dr]			; RANGE COORDS OF VERTICES
tt = [-dt,dt,dt,-dt,-dt]			; AZIMUTH COORDS OF VERTICES
;
;	=====>> SET THE PLOT SCALE AND PLOT THE AXES
;
case n_params(1) of
  1: plot,/polar,data(1,*),data(2,*)/!radeg,psym=3,position=position $
	,title=title,xtitle=xtitle,ytitle=ytitle $
	,xrange=rangerange,yrange=rangerange,xstyle=style,ystyle=style
  3: begin
     tmp = [((min(range)-dr)>0)+0*azimuth,max(range)+dr+0*azimuth]
     plot,tmp,[azimuth,azimuth]/!radeg,/polar,psym=3,position=position $
	,title=title,xtitle=xtitle,ytitle=ytitle,/nodata $
	,xrange=rangerange,yrange=rangerange,xstyle=style,ystyle=style
     end
endcase
if (rangebin eq 0) or (azimuthbin eq 0) then return    
;
;	=====>> MAKE AN ARRAY OF COLOR INDICES
;
if n_params(1) eq 3 then tmp = data else tmp = reform(data(0,*)) 
if n_elements(colors) eq 1 then begin
  color = bytscl(tmp,top=colors-1,min=rcsrange(0),max=rcsrange(1))*step
endif else begin
  delta = float(rcsrange(1)-rcsrange(0))/n_elements(colors)
  maxindex = n_elements(colors) - 1
  color = byte(0*tmp)
  for i = 0L,n_elements(tmp)-1 do begin
    index = ((tmp(i)-rcsrange(0))/delta) < maxindex
    color(i) = colors(index)
    endfor
endelse
;
;	=====>> DO THE SECTOR PLOT FOR SINGLE PARAMETER
;
if n_params(1) eq 1 then begin
for i = 0L,szd(2)-1 do begin
  r = data(1,i) + rr
  t = data(2,i)/!radeg + tt/!radeg
  polyfill,r*sin(t),r*cos(t),/fill,color=color(i)
  endfor
return
endif
;
;	=====>> DO THE SECTOR PLOT FOR THREE PARAMETERS
;
if !d.name ne 'PS' then begin
;
;	=====>> PLOT FOR NON-POSTSCRIPT DEVICE USES POLYFILL
;
  for i = 0L,szr(1)-1 do begin
    r = (range(i) + rr) > 0
    for j = 0L,sza(1)-1 do begin
      t = (azimuth(j)+ tt)/!radeg 
      polyfill,r*sin(t),r*cos(t),/fill,color=color(j,i)
    endfor
  endfor
endif else begin
;
;	=====>> GET READY TO PLOT FOR POSTSCRIPT: DEFINE POSTSCRIPT COMMANDS
;	=====>> WRITE CODE TO PS FILE; THEN USE MACROS TO SHORTEN PS FILE.
;
  printf,!d.unit,' /A {arcn} bdef'		; DEFINE ARC OPERATOR ALIAS
  printf,!d.unit,'/B {stroke} bdef'		; ALIAS TO SHORTEN PS FILE
  tvlct,red,green,blue,/get			; GET COLOR VECTORS
  colorrange = 256				; # OF POSSIBLE COLORS
  red = float(red)/colorrange			; CONVERT TO FRACTIONS
  green = float(green)/colorrange
  blue = float(blue)/colorrange
  only = indgen(colors)*step			; ONLY COLORS USED
  printf,!d.unit,'/red ['			; DEFINE COLOR ARRAYS
  printf,!d.unit,format='(10f6.3)',red(only)
  printf,!d.unit,'] bdef'
  printf,!d.unit,'/green ['
  printf,!d.unit,format='(10f6.3)',green(only)
  printf,!d.unit,'] bdef'
  printf,!d.unit,'/blue ['
  printf,!d.unit,format='(10f6.3)',blue(only)
  printf,!d.unit,'] bdef'
  printf,!d.unit $				; DEFINE SET-COLOR OPERATOR
    ,'/Q {dup red exch get exch dup ' $		; USES COLOR ARRAYS
    ,'green exch get exch blue exch get ' $
    ,'setrgbcolor} bdef'			; SAVES SPACE IN PS FILE
  center = convert_coord(0,0,/to_device)
  printf,!d.unit,'/E {',ppil(center(0)),'} bdef'; X COORD OF CENTER
  printf,!d.unit,'/G {',ppil(center(1)),'} bdef'; Y COORD OF CENTER
;  printf,!d.unit,'100 42 1 100 85 1 100 36 1 50 24 1 setcolorscreen' ; POSSIBLE HALFTONE SCREEN
  sp = ' '
  arcn_params = ' E G H'			; 1ST 3 PARAMS FOR arcn
  width = convert_coord(2*dr,0,/to_device)-center
  width = width(0)
  printf,!d.unit,'currentlinewidth /oldlinewidth exch def'
						; SAVE LINE WIDTH
  printf,!d.unit,'currentrgbcolor 3 array astore /oldrgbcolor exch def'
						; SAVE RGB COLORS
  printf,!d.unit,ppil(width),' setlinewidth'	; SET ARC WIDTH
  printf,!d.unit,'newpath'			; KILL CURRENTPOINT
  jmax = sza(1)-1
;
;	=====>> DO PLOT FOR POSTSCRIPT: LOOP THROUGH RANGE, THEN AZIMUTH
;	=====>> USE POSTSCRIPT COMMANDS Q (set color), A (arc), AND B (stroke).
;	=====>> Q ARGUMENTS ARE:<color index>/step
;	=====>> A ARGUMENTS ARE:E G = x and y device coords of center
;				H = radius in device coords
;				current azimuth extent
;	=====>> B ARGUMENTS ARE:result of A
;
  for i = 0,szr(1)-1 do begin			; LOOP THROUGH RANGE
    col = color(0,i)				; STARTING COLOR
    azi = azimuth(0)-dt				; STARTING AZIMUTH
    rim = convert_coord(range(i),0,/to_device)
    radius = sqrt((center(0)-rim(0))^2 + (center(1)-rim(1))^2)
    printf,!d.unit,'/H {',ppil(radius),'} bdef'	; FOR RADIUS
    for j = 1,jmax-1 do begin			; LOOP THROUGH AZIMUTH
      if color(j,i) ne col then begin		; OUTPUT ONLY IF NEW COLOR 
        printf,!d.unit $			; WRITE A LINE TO PS FILE
          ,ppil(col/step) + ' Q' $		; CHANGE TO PROPER COLOR
          + arcn_params $			; x y r of x y r ang1 ang2 arcn
          + sp + ppif(90.- azi) $		; ang1
          + sp + ppif(90.-azimuth(j-1)-dt) $	; ang2 
          + ' A B'				; DRAW AN ARC AT RADIUS
        col = color(j,i)			; SAVE THE NEW COLOR
        azi = azimuth(j)-dt			; SAVE THE NEW AZIMUTH
      endif
    endfor
    printf,!d.unit $			; FORCE OUTPUT AT END OF AZIMUTH RANGE
          ,ppil(col/step) + ' Q' $		; CHANGE TO PROPER COLOR
          + arcn_params $			; x y r of x y r ang1 ang2 arcn
          + sp + ppif(90.- azi) $		; ang1
          + sp + ppif(90.-azimuth(jmax)-dt) $	; ang2 
          + ' A B'				; DRAW AN ARC AT RADIUS
  endfor
;
;	=====>> MOVE TO FIRST VERTEX (CLEAN UP) & RESET LINE WIDTH
;
  t = (90.-azimuth(0))/!radeg
  first = convert_coord(range(0)*[cos(t),sin(t)],/to_device)
  printf,!d.unit,ppil(first(0)),sp,ppil(first(1)),' M'
  printf,!d.unit,'oldlinewidth setlinewidth'
  printf,!d.unit,'oldrgbcolor aload pop setrgbcolor'
endelse
;
;	=====>> ADD GRIDS IF REQUESTED
;
if n_elements(grid) ge 1 then begin
  nticks = grid(0) > 4			; MINIMUM NUMBER OF TICKS
  azt = findgen(nticks)*2.*!pi/nticks
  rmn = 0.9*(min(range)-dr) > 0
  rmx = 1.1*(max(range)+dr)
  piover2 = !pi/2.
  for i = 0,nticks-1 do begin
    oplot,[rmn,rmx],[azt(i),azt(i)],/polar
    xyouts,rmx*cos(piover2-azt(i)),rmx*sin(piover2-azt(i)),strtrim(fix(azt(i)*!radeg),2),align=azt(i) gt !pi
    endfor
  endif
if n_elements(grid) eq 2 then begin
  nrings = grid(1) > 1			; MINIMUM NUMBER OF RANGE GRIDS
  rmn = (min(range)-dr) > 0
  rmx = (max(range)+dr)
  rings = [findgen(nrings),nrings]*(rmx-rmn)/nrings + rmn
  nticks = 36
  azt = [findgen(nticks),nticks]*2.*!pi/nticks
  for i = 0,nrings do begin
    oplot,rings(i)+0*azt,azt,/polar
    endfor
  endif
return
end
