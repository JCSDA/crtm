;+
; Name:
;	PANELPLOT
; Purpose:
;	Plot many sets of variables on one common set of axes
; Usage:
;	panelplot,xdata,ydata,/init,nplots=n,...	; make 1st plot       
;	panelplot,xdata,ydata,...			; make 2nd plot
;	...
;	panelplot,xdata,ydata,...			; make nth plot
; Inputs:
;	(the following inputs apply during initialization)
;	init	 = flag to initialize for plots
;	nplots	 = number of plots on the page
;
;	(the following inputs apply to each individual plot)
;	xdata = abscissa values
;	ydata = ordinate values
; Optional Inputs:
;	(the following inputs apply during initialization)
;	charsize = value to override the default value: !p.charsize
;	minx  	 = minimum x value to plot
;	maxx     = maximum x value to plot
;	xtitle	 = title of the x axis
;	title	 = main plot title
;
;	(the following inputs apply to each individual plot)
;	ytitle	 = the y axis title
;	rtitle	 = the plot title on right side of plot
;	ltitle	 = the plot title on left side of plot
;	yrange	 = array of y ranges for data
; Outputs:
;	plot to current device of data
; Restrictions:
;	To suppress tick labels, labels are set to blanks.  However, there may
;	be more ticks than blanks, producing unwanted labels.  The plot area
;	hardwired and will be incorrect for large charsizes.
; Modification history:
;	modify multiplot, 12 Nov 91, FKK
;	eliminate legend, fills with polyh, etc. 5 Jun 92, FKK
;	change keyword mtitle to title, 25 Aug 92, FKK
;	add ystyle and xstyle keywords, 2 Oct 92, FKK
;-
pro PANELPLOT,xdata,ydata	$		; DATA TO BE PLOTTED
	     ,help=help $
	     ,init=init		$		; INITIALIZATION FLAG
             ,charsize=charsize	$		; OVERRIDE OF !p.charsize
             ,minx=minx		$		; MINIMUM X VALUE FOR ALL PLOTS
             ,maxx=maxx		$		; MAXIMUM X VALUE FOR ALL PLOTS
             ,nplots=nplots	$		; NUMBER OF PLOTS ON PAGE
	     ,ltitle=ltit	$		; PLOT TITLE FOR CURRENT PLOT
	     ,rtitle=rtit	$		; PLOT TITLE FOR CURRENT PLOT
	     ,xtitle=xtit	$		; X TITLE FOR WHOLE PAGE
             ,ytitles=ytit	$		; Y TITLE FOR CURRENT PLOT
	     ,xstyle=xstyle	$		; X STYLE FOR WHOLE PAGE
             ,ystyle=ystyle	$		; Y STYLE FOR CURRENT PLOT
             ,title=mtit	$		; MAIN TITLE FOR WHOLE PAGE
             ,yrange=yrange			; MIN/MAX RANGE FOR Y
;
COMMON PANELPLOT_COMMON		$		; KEEP CONTINUING DATA HERE
	,num_plots		$		; NUMBER OF PLOTS ON PAGE
	,xw			$		; WIDTH OF EACH PLOT
	,xll			$		; X LOWER LEFT
	,xur			$		; X UPPER RIGHT
	,yw			$		; HEIGHT OF EACH PLOT
	,yll			$		; Y LOWER LEFT
	,yur			$		; Y UPPER RIGHT
	,xrange			$		; MIN/MAX X
	,charht 		$		; CHARACTER HEIGHT
	,ncur					; CURRENT PLOT NUMBER
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin & doc_library,'panelplot' & return & endif
;
;	=====>> IS THIS AN INITIALIZATION REQUEST?
;
ok_data = (n_elements(xdata) gt 0) and (n_elements(ydata) gt 0)
if (keyword_set(init)) then begin
  if n_elements(charsize) eq 0 then charht = !p.charsize else charht = charsize
  if charht eq 0 then charht = 1
  if (keyword_set(nplots))	$		; NUMBER OF PLOTS ON PAGE
    then num_plots = nplots $
    else num_plots = 1
  ncur = 0					; CURRENT PLOT NUMBER
  if (keyword_set(mtit)) 	$		; MAIN TITLE
    then cmtitle = mtit	$
    else cmtitle = ''
  if (keyword_set(xtit)) 	$		; X-AXIS TITLE
    then cxtitle = xtit	$
    else cxtitle = ''
  xrange = fltarr(2)
  if n_elements(minx) eq 1 then begin
    xrange(0) = minx 
  endif else begin
    if ok_data then xrange(0) = min(xdata) else message,'No minimum x given.'
  endelse
  if n_elements(maxx) eq 1 then begin
    xrange(1) = maxx 
  endif else begin
    if ok_data then xrange(1) = max(xdata) else message,'No maximum x given.'
  endelse
;
;	=====>> CALCULATE POSITION OF PLOTS WITHIN GRAPHICS WINDOW.
;
  xll = .15					; TOTAL LOWER LEFT X
  yll = .15					; TOTAL LOWER LEFT Y
  xur = .9					; TOTAL UPPER RIGHT X
  yur = .9					; TOTAL UPPER RIGHT Y
  xw = xur - xll				; WIDTH OF EACH PLOT
  yw = (yur-yll)/num_plots			; HEIGHT OF EACH PLOT
  erase
;
;     =====>> OUTPUT THE MAIN TITLE
;
  xloc = (xur + xll)/2.0			; CENTER OF PAGE
  yloc = yur + 0.02				; ABOVE THE PLOTS
  xyouts,xloc,yloc,cmtitle,/normal,alignment=0.5,size=1.7*charht
;
;     =====>> OUTPUT THE X-AXIS TITLE
;
  yloc = yll - 0.07				; BELOW THE PLOTS
  xyouts,xloc,yloc,cxtitle,/normal,alignment=0.5,size=charht
;
;     =====>> RETURN UNLESS DATA WERE INPUT
;
if not ok_data then return
endif
;
; 	=====>> DECODE THE OPTIONS AND SET DEFAULTS
;
if (n_elements(yrange) eq 0) then yrange = fltarr(2)
if (keyword_set(ytit)) then ytitle = ytit else ytitle = ' '
if n_elements(xstyle) eq 0 then xstyle = 0
if n_elements(ystyle) eq 0 then ystyle = 0
;
; 	=====>> CHOOSE THE DATA TO PLOT; NPL IS INDEX FOR SELECTING DATA
;
syd = size(ydata)
if (syd(0) ne 1) or (syd(1) le 0) then message,'Second parameter must be vector.'
pos = [xll,yur-yw-ncur*yw,xur,yur-ncur*yw]
case ncur of
   num_plots-1: xtn = ['','','','','','','','','','','','']
          else: xtn = [' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
   endcase
plot,xdata,ydata	 	$		; PLOT THE DATA
    ,xrange=xrange		$
    ,yrange=yrange		$
    ,xstyle=xstyle		$
    ,ystyle=ystyle		$
    ,pos=pos,/noerase		$
    ,xtickname=xtn		$
    ,charsize=charht
;
; 	=====>> OUTPUT THE Y-AXIS TITLE
;
xloc = .040
yloc = yll + (num_plots-ncur-0.5)*yw
xyouts,xloc,yloc,ytitle,/normal,alignment=0.5,orientation=90,size=charht
;
;	=====>> OUTPUT THE CAPTIONS (LTITLE AND RTITLE) OF THE PLOT
;
xtoff = 0.02			; OK APPROXIMATIONS FOR X AND PS
ytoff = 0.05
yloc = yur - ncur*yw - ytoff
if n_elements(rtit) gt 0 then begin
  xloc = xur - xtoff
  xyouts,xloc,yloc,rtit,/normal,alignment=1.0,size=charht
  endif
if n_elements(ltit) gt 0 then begin
  xloc = xll + xtoff
  xyouts,xloc,yloc,ltit,/normal,alignment=0.0,size=charht
  endif
;
ncur = ncur + 1			; POINT TO NEXT PLOT
;
return
end
