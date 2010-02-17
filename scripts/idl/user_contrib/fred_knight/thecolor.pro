;+
; Name:
;	thecolor
; Purpose:
;	Function to rob a color index from the current color table and set
;	it to the specified color.  Then return the index.
;	Options exist for where in the color table the index is chosen.
;	Color names come from the Openwindows colors file, q.v. wrgb.pro.
; Usage:
;	plot,x,color=thecolor('red')			; plot is red.
;	plot,x,color=thecolor('LightGoldenrod3')	; case insensitive
;	plot,x,color=thecolor('red',index=!d.n_color-1)
;		; the last index set to red
;	plot,x,color=thecolor('red',/first)
;		; 1st available at the start of the color table (Default)
;	plot,x,color=thecolor('red',/last)
;		; ditto except at the end of the color table
;	tmp = thecolor(/reset)		; forget all previous thecolor calls
; Inputs:
;	name = name of color, a string
; Optional Inputs:
;	none
; Keywords:
;	/help = flag to print header
;	index = force the color index to be this value
;	/last = choose index from first available at end of color table
;	/first = choose the first available at the bottom. (D)
;	rgb = output RGB values for the color
;	/noindex = flag to simply return the rgb values, leaving the color
;		table unchanged
;	/reset = flag to kill all current reserved colors but doesn't restore
;		the previous color table. Use loadct,n, tvlct,r,g,b or xloadct.
;	/list = list the current colors and their indices
;	/show = list all available colors
;	/view = show swatches of all available colors; call wrgb widget
; Outputs:
;	index = function return
; Optional outputs:
;	none (cf. keyword rgb)
; Common blocks:
;	thecolor, used to remember available names and info from
;	previous calls, containing:
;	  thecolors = names of colors stored in previous calls to thecolor
;	  thecolorindices = corresponding color table indices (those replaced)
;	  thecolornames = list of available color names, read from file
;	  thecolorrgb = corresponding rgb triples
;	  previousfirstindex = previous index stored at start of color table
;	  previouslastindex = previous index stored at end of color table
; Procedure:
;	If keyword help is set, call doc_library to print header.
; Restrictions:
;	Needs a file where color names and RGB values are found.
;	Two files are hardwired; at least one needs to be present.
;	The widget xdisplayfile is called if /show is set.
;	The widget wrgb is called if /view is set.
; Modification history:
;	write, 29 Nov 91, F.K.Knight (knight@ll.mit.edu)
;	add keyword noindex to simply return the rgb values and not alter the
;	  color table, 27 Apr 92, FKK
;	add keyword reset, 30 Jun 92, FKK
;	fix bug when /last occurs after /first, 30 Jun 92, FKK
;	make a generic file the default, 6 Oct 92, FKK
;-
function thecolor,name,help=help,index=index,first=first,last=last,rgb=rgb,noindex=noindex,reset=reset,list=list,show=show,view=view
common thecolor,thecolors,thecolorindices,thecolornames,thecolorrgb $
  ,previousfirstindex,previouslastindex,thecolorfile
;
;	=====>> HELP
;
on_error,2
if keyword_set(help) then begin
  doc_library,'thecolor'
  if (n_params() eq 0) then return,-1
endif
;
;	=====>> SET DEFAULTS & TEST USER INPUTS
;
startoffset=1			; RESERVE LOWEST INDEX FOR FORE/BACKGROUND
endoffset=!d.n_colors-2		; RESERVE HIGHEST INDEX FOR FORE/BACKGROUND
bogusindex = -1			; INVALID INDEX: FLAG FOR RESET & STARTUP
if keyword_set(reset) or (n_elements(thecolorindices) eq 0) then begin
  thecolors = ''
  thecolorindices = bogusindex
  previousfirstindex = bogusindex
  previouslastindex = bogusindex
  if n_params(1) eq 0 then return,bogusindex	; RETURN IF NO REQUESTED COLOR
  endif
rgb = [-1,-1,-1]
if keyword_set(list) then begin
  if thecolorindices(0) eq bogusindex then message,'No assigned colors.'
  sorti = sort(thecolorindices)
  for i = 0,n_elements(thecolorindices)-1 do $
    print,thecolorindices(sorti(i)),' ',thecolors(sorti(i))
  if n_params(1) eq 0 then return,bogusindex	; RETURN IF NO REQUESTED COLOR
  endif
if n_elements(name) gt 1 then message,'Only single color name allowed; no action.'
;
;	=====>> INITIALIZATION: READ IN COLORS FROM A FILE
;
if n_elements(thecolornames) eq 0 then begin
  rgbfiles = [filepath('thecolor.txt',subd=['lib','local']),'$OPENWINHOME/lib/rgb.txt']
  searchlist = ''
  for fi = 0, n_elements(rgbfiles)-1 do begin
;    print,rgbfiles(fi)
    rgbfile = findfile(rgbfiles(fi),count=cts)
    if cts gt 0 then goto,GOTDATA
    searchlist = searchlist + ' ' + rgbfiles(fi)
    endfor
  message,'No data.  Can''t find any of these files:' + searchlist
  return,-1
  GOTDATA:
  nn=' '
  on_ioerror,GOTEOF
  thecolorfile = rgbfiles(fi)
  openr,lun,thecolorfile,/get_lun
  ;
  ;	=====>> COUNT COLORS IN FILE
  ;
  nlines = 0
  while not eof(lun) do begin
    readf,lun,r,g,b,nn
    nlines = nlines + 1
    endwhile
  GOTEOF:
  ;
  ;	=====>> READ COLORS FROM TABLE
  ;
  thecolorrgb = bytarr(3,nlines)
  thecolornames = strarr(25,nlines)
  i = -2			; DEFAULT FOR NO DATA READ
  point_lun,lun,0
  on_ioerror,EOF
  for i = 1,nlines-2 do begin
    readf,lun,r,g,b,nn
    thecolorrgb(0,i) = [r,g,b]
    thecolornames(i) = strtrim(nn,2)+' '
  endfor
  EOF:
    close,lun
    free_lun,lun
    thecolornames = strtrim(strlowcase(thecolornames),2)
endif
;
;	=====>> SHOW THE AVAIALBLE COLORS.
;
if keyword_set(show) then begin
  if !d.name eq 'X' then xdisplayfile,thecolorfile else message,'/show only for X windows.'
  endif
;
;	=====>> VIEW THE AVAILABLE COLORS.
;
if keyword_set(view) then begin
  if !d.name eq 'X' then wrgb else message,'/view only for X windows.'
  endif
;
;	=====>> SET RGB RETURN VALUE.  RETURN IF REQUESTED OR NO MATCH
;
if n_elements(name) ne 1 then return,-1
w = where(thecolornames eq strlowcase(name))
w = w(0)		; CHOOSE FIRST INSTANCE, CAST TO SCALAR
if w ge 0 then begin
  rgb = thecolorrgb(*,w)
  if keyword_set(noindex) then return,-1
endif else begin
  message,/inform,name + ' is not an available color in ' + thecolorfile
  return,-1
endelse
;
;	=====>> SET THE INDEX: EITHER FROM EXISTING COLORS OR FROM TABLE
;
update = 1			; ASSUME NEW COLOR
if n_elements(index) ne 1 then begin
  index = -1
  if thecolorindices(0) ne bogusindex then begin	; LOOK FOR MATCH
    temp = where(thecolors eq strlowcase(name))
    temp = temp(0)
    update = temp eq -1
    if not update then index = thecolorindices(temp)
  endif
  if index eq -1 then begin		; CHOOSE A NEW INDEX
    nc = n_elements(thecolors)
    atlast = keyword_set(last)
    if atlast and (previouslastindex eq bogusindex) then nc = 0
					; IN CASE /last AFTER /first
    if (not atlast) and (previousfirstindex eq bogusindex) then nc = 0
					; IN CASE /first AFTER /last
    if nc eq 0 then begin  
      if atlast then index = endoffset else index = startoffset
    endif else begin
      if atlast then begin
        index = (previouslastindex - 1) > startoffset
      endif else begin
        index = (previousfirstindex + 1) < endoffset
      endelse
    endelse
    if atlast then previouslastindex = index else previousfirstindex = index
  endif
endif
;
;	=====>> SET COLOR OF THE INDEX
;
tvlct,r,g,b,/get
r(index) = thecolorrgb(0,w)
g(index) = thecolorrgb(1,w)
b(index) = thecolorrgb(2,w)
if update then begin
  if (thecolorindices(0) eq bogusindex) then begin
    thecolors = thecolornames(w) 
    thecolorindices = [index]
  endif else begin
    thecolors = [thecolors,thecolornames(w)]
    thecolorindices = [thecolorindices,index]
  endelse
endif
tvlct,r,g,b
return,index
end
