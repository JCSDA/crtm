pro plot_attenuation, swap = swap, frange = frange

openr,1,'freq.bin', swap_endian=swap
info = fstat(1)
n = info.size / 8L & print, n
f=dblarr(n)
readu,1,f
close,1

xrange = [ min(f), max(f) ]
if ( keyword_set( frange ) ) then xrange = [ min(frange), max(frange) ]

np=48
nl=100

Liebe_WetLine           = dblarr(n)
Liebe_WetContinuum      = dblarr(n)
Liebe_DryLine           = dblarr(n)
Liebe_DryContinuum      = dblarr(n)

Rosenkranz_WetLine      = dblarr(n)
Rosenkranz_WetContinuum = dblarr(n)
Rosenkranz_DryLine      = dblarr(n)
Rosenkranz_DryContinuum = dblarr(n)

openr,21,'liebe_wetline.bin',      swap_endian=swap
openr,22,'liebe_wetcontinuum.bin', swap_endian=swap
openr,23,'liebe_dryline.bin',      swap_endian=swap
openr,24,'liebe_drycontinuum.bin', swap_endian=swap

openr,31,'rosenkranz_wetline.bin',      swap_endian=swap
openr,32,'rosenkranz_wetcontinuum.bin', swap_endian=swap
openr,33,'rosenkranz_dryline.bin',      swap_endian=swap
openr,34,'rosenkranz_drycontinuum.bin', swap_endian=swap

!x.margin=[12,3]
!y.omargin=[0,5]
xtitle = 'Frequency (GHz)'
ytitle = 'Attenuation (dB/km)'
charsize = 2.0

for m = 0, np-1 do begin
  for k = 0, nl-1 do begin

    !p.multi = [0,4,3]

    readu,21,Liebe_WetLine
    readu,22,Liebe_WetContinuum
    readu,23,Liebe_DryLine     
    readu,24,Liebe_DryContinuum
    Liebe = Liebe_WetLine + Liebe_WetContinuum + Liebe_DryLine + Liebe_DryContinuum

    readu,31,Rosenkranz_WetLine
    readu,32,Rosenkranz_WetContinuum
    readu,33,Rosenkranz_DryLine     
    readu,34,Rosenkranz_DryContinuum
    Rosenkranz = Rosenkranz_WetLine + Rosenkranz_WetContinuum + Rosenkranz_DryLine + Rosenkranz_DryContinuum

    dAtt   = Liebe - Rosenkranz
    dAttpc = 100.0d0 * dAtt / Liebe

;    yrange = [ min(Liebe_DryLine)<min(Rosenkranz_DryLine),max(Liebe_DryLine)>max(Rosenkranz_DryLine)]
;    wplot, f, Liebe_DryLine,title='Liebe dry line',$
;          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize,/nodata
;    woplot, f, Liebe_DryLine,color=4
;    woplot, f, Rosenkranz_DryLine,color=5
;goto,skip

    yrange = [ min(Liebe_WetLine)<min(Rosenkranz_WetLine),max(Liebe_WetLine)>max(Rosenkranz_WetLine)]
    plot, f, Liebe_WetLine,title='Liebe wet line',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize
    plot, f, Rosenkranz_WetLine,title='Rosenkranz wet line',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize

    yrange = [ min(Liebe_WetContinuum)<min(Rosenkranz_WetContinuum),max(Liebe_WetContinuum)>max(Rosenkranz_WetContinuum)]
    plot, f, Liebe_WetContinuum,title='Liebe wet continuum',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize
    plot, f, Rosenkranz_WetContinuum,title='Rosenkranz wet continuum',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize

    yrange = [ min(Liebe_DryLine)<min(Rosenkranz_DryLine),max(Liebe_DryLine)>max(Rosenkranz_DryLine)]
    plot, f, Liebe_DryLine,title='Liebe dry line',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize
    plot, f, Rosenkranz_DryLine,title='Rosenkranz dry line',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize

    yrange = [ min(Liebe_DryContinuum)<min(Rosenkranz_DryContinuum),max(Liebe_DryContinuum)>max(Rosenkranz_DryContinuum)]
    plot, f, Liebe_DryContinuum,title='Liebe dry continuum',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize
    plot, f, Rosenkranz_DryContinuum,title='Rosenkranz dry continuum',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize

    yrange = [ min(Liebe)<min(Rosenkranz),max(Liebe)>max(Rosenkranz)]
    plot, f, Liebe,title='Liebe total',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize
    plot, f, Rosenkranz,title='Rosenkranz total',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,ytitle=ytitle,charsize=charsize

    yrange = [ min(dAtt),max(dAtt)]
    plot, f, dAtt,title='Difference',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,$
          ytitle='Attenuation difference (dB/km)',charsize=charsize
    oplot, !x.crange, [0,0],linestyle=2

    yrange = [ min(dAttpc),max(dAttpc)]
    plot, f, dAttpc,title='% Difference',$
          xrange=xrange,yrange=yrange,xtitle=xtitle,$
          ytitle='Attenuation difference (%)',charsize=charsize
    oplot, !x.crange, [0,0],linestyle=2

    title = 'Attenuation for layer '+strtrim(k+1,2)+', profile '+strtrim(m+1,2)
    xyouts, 0.5, 0.97, title, /norm, align=0.5, charsize = charsize

skip:
    q = get_kbrd(1)
    if ( strupcase(q) EQ 'Q' ) then goto, done

  endfor ; k
endfor ; m

done:

close,21,22,23,24
close,31,32,33,34

!p.multi=0
!x.margin=[10,3]

end
