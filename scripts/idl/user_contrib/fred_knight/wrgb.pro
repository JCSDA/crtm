;+
; Name:
;	wrgb
; Purpose:
;	Read and display colors from the rgb table---good
;	for choosing X-windows colors and browsing for pleasing hues.
; Usage:
;	wrgb[,offset=offset][,dim=dim]
; Optional Inputs:
;	offset = starting point in the file; there are about 740 entries.
;	dim = length of side (in pixels) of a color square
; Outputs:
;	Colors are reported to the terminal if the left or middle
;	mouse button is pushed.
; Common blocks:
;	wrgb = variables that need to be passed between routines
; Procedure:
;	Use IDL widgets.  After setup, xmanager runs the show.  The background
;	routine (wrgb_bck) runs like rdpix.pro and reports the color at the
;	cursor position to the window.  Pushing the left or middle mouse 
;	button records the current color at the terminal, again like rdpix.pro.
;	Moving the offset slider forces an update of the color table.
;	Pushing the done button kills all the widgets.
; Example:
;	IDL> wrgb		; starts widget
; Modification history:
;	convert xrgb to widget-based wrgb, 30Sep91, FKK
;	fix bugs, e.g. cursor,x,y,2 didn't return on cursor motion, 7Oct91, FKK
;	save existing color table at start and reset it at end, 29Jan92, FKK
;	add help button, 29 Jul 92, FKK
;	use a generic file rather then $OPENWINHOME/lib/rgb.txt, 6 Oct 92, FKK 
;-
;
;	============================
;	=====>> EVENT HANDLER
;	============================
;
pro wrgb_event,event
common wrgb,rgb,names,o_rgb,o_names,offset,dim,nx,bwthres,extra,nb,ilast
;
;	=====>> GET NAME OF EVENT STRUCTURE
;
;message,'Enter wrgb event handler',/continue
widget_control,event.id,get_uvalue = test,get_value = datum
;
;	=====>> PROCESS THE EVENT
;
case test of
'donebutton': begin
  widget_control,event.top,/destroy
  end
'helpbutton': begin
  message,/inform,'Starting xdl...searching your !path...'
  xdl,'wrgb'
  end
'offsetslider': begin
;  message,strtrim(datum,2)+'=new offset',/continue
  offset = datum
  nb0 = offset+1
  nb1 = offset+nb-2
  bw = [[0,0,0],[255,255,255]]
  rgb = bytarr(3,nb)
  rgb(0,0) = bw
  rgb(0,2) = o_rgb(*,nb0:nb1)
  tvlct,rgb(0,*),rgb(1,*),rgb(2,*)
  names = ['black','white',o_names(nb0:nb1)]
  for i = nb-1,0,-1 do begin
    x = i mod nx
    y = i/nx
    if total(rgb(*,i)) gt bwthres then color = 0 else color = 1
    xyouts,x*dim+2,2+!d.y_size-dim-y*dim,strtrim(i,2),/device,color=color
    endfor
  end
else:
endcase
end
;
;	============================
;	=====>> BACKGROUND TASK 
;	============================
;
pro wrgb_bck,baseid
common wrgb,rgb,names,o_rgb,o_names,offset,dim,nx,bwthres,extra,nb,ilast
;
;	=====>> FOLLOW THE CURSOR & REPORT COLORS (LIKE rdpix.pro)
;
;message,'Enter background task with !err='+string(!err),/inform
form = "($,i3,1x,a,3i4,a,a)"
cr = string("15b)
xoff = 2
xs = nx*dim
wait,.1					; TO DECREASE CPU LOAD
cursor,xd,yd,0,/dev
if !err gt 0 then begin
  print,form="($,a)",string("12b)	; LINE FEED & WAIT FOR BUTTON RELEASE
  while (!err ne 0) do begin wait,.1 & tvrdc,xd,yd,0,/dev & end
endif
i = ((xd/dim) + (!d.y_size-yd)/dim*nx)	; COLOR INDEX
if ilast ne i then begin		; UPDATE ONLY IF COLOR CHANGED
  tv,bytarr(!d.x_size,extra)+i,0,0
if (i ge 0) and (i le (nb-1)) then begin
  print,form=form,i,names(i),rgb(*,i),'          ',cr
  if total(rgb(*,i)) gt bwthres then color = 0 else color = 1
  xyouts,xoff,32,string(form='($,i3,1x,2a)',i,names(i),'          ') $
    ,/dev,color=color,size=1.5
  xyouts,xoff,2,string(form='($,3i4)',rgb(*,i)),/dev,color=color,size=1.5
  if total(rgb(*,i)) le bwthres then color = 0 else color = 1
  xyouts,xs-xoff,32,string(form='($,i3,1x,a)',i,names(i)) $
    ,/dev,color=color,size=1.5,align=1
  xyouts,xs-xoff,2,string(form='($,3i4)',rgb(*,i)),/dev,color=color,size=1.5,align=1
endif
endif
ilast = i
;message,'Exit background task with !err='+string(!err),/inform
return
end
;
;	============================
;	=====>> INITIALIZING ROUTINE
;	============================
;
pro wrgb,offset=inoffset,dimension=indim,group=group,help=help
common wrgb,rgb,names,o_rgb,o_names,offset,dim,nx,bwthres,extra,nb,ilast
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'wrgb' & return & endif
;
;	=====>> SET DEFAULTS
;
if n_elements(inoffset) eq 0 then offset = 40 else offset = inoffset
if n_elements(indim) eq 0 then dim = 30 else dim = indim
if dim lt 20 then print,'Dimension should be at least 20.'
extra = (.75*dim) > 50			; REGION FOR TEXT AT BOTTOM
; rgbfile = '$OPENWINHOME/lib/rgb.txt'
rgbfile = filepath('thecolor.txt',subd=['lib','local'])
nx = 16
ny = 16
nb = (nx*ny) < !d.n_colors
ny = (nb-1)/nx + 1
bwthres = 400	; black-to-white threshold for text---not enough colors to invert color
ilast = 0
tvlct,rsave,gsave,bsave,/get	; GET EXISTING COLOR TABLE FOR RESET AT END
;
;	=====>> COUNT COLORS IN FILE
;
nn=' '
on_ioerror,GOTEOF
nlines = 0
openr,lun,rgbfile,/get_lun
while not eof(lun) do begin
  readf,lun,r,g,b,nn
  nlines = nlines + 1
  endwhile
GOTEOF:
if nlines eq 0 then message,'Cannot find file for colors: '+rgbfile
;
;	=====>> READ COLORS FROM TABLE
;
o_rgb = bytarr(3,nlines)
o_names = strarr(25,nlines)
i = -2						; DEFAULT FOR NO DATA READ
point_lun,lun,0
for i = 1,nlines-2 do begin
  readf,lun,r,g,b,nn
  o_rgb(0,i) = [r,g,b]
  o_names(i) = strtrim(nn,2)+' '
endfor
close,lun
free_lun,lun
;
;	=====>> CREATE AND REGISTER WIDGET
;
if xregistered("wrgb") gt 0 then return		; DON'T REGISTER A 2nd ONE
wrgbbase = widget_base(title=rgbfile,/column)	; CREATE THE BASE WIDGET
wrgbrow = widget_base(wrgbbase,/row)
dbutton = widget_button(wrgbrow,value='DONE' $	; CREATE A BUTTON WIDGET
  ,uvalue = 'donebutton')
hbutton = widget_button(wrgbrow,value='HELP' $	; CREATE A BUTTON WIDGET
  ,uvalue = 'helpbutton')
draw = widget_draw(wrgbbase $			; CREATE A DRAW WIDGET
  ,xsize=nx*dim,ysize=ny*dim+extra)
slider = widget_slider(wrgbbase,value=offset $	; CREATE A SLIDER WIDGET
  ,title='Offset',min=0,max=nlines-nb $
  ,uvalue = 'offsetslider')
widget_control,wrgbbase,/realize		; PUT THEM ON THE SCREEN
;
;	=====>> DISPLAY THE COLOR CHART
;
widget_control, get_value=window, draw
wset,window
nb0 = offset+1
nb1 = offset+nb-2
bw = [[0,0,0],[255,255,255]]
rgb = bytarr(3,nb)
rgb(0,0) = bw
rgb(0,2) = o_rgb(*,nb0:nb1)
tvlct,rgb(0,*),rgb(1,*),rgb(2,*)
names = ['black','white',o_names(nb0:nb1)]
for i = nb-1,0,-1 do begin
  tv,bytarr(dim,dim)+i,i
  x = i mod nx
  y = i/nx
  if total(rgb(*,i)) gt bwthres then color = 0 else color = 1
  xyouts,x*dim+2,2+!d.y_size-dim-y*dim,strtrim(i,2),/device,color=color
  endfor
;
;	=====>> TURN OVER CONTROL TO XMANAGER
;
xmanager,'wrgb',wrgbbase $
  ,background = 'wrgb_bck' $
  ,event_handler = 'wrgb_event'
;
;	=====>> REVERT TO ORIGINAL COLOR TABLE
;
tvlct,rsave,gsave,bsave
end
