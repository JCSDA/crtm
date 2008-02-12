pro plot_atmosphere

  @color_db
  charsize  = (!D.NAME eq 'PS') ? 1.75 : 1.0
  lcharsize = (!D.NAME eq 'PS') ? charsize : 1.5
  font     = (!D.NAME eq 'PS') ? 1 : -1
  thick    = (!D.NAME eq 'PS') ? 2 :  1
  xlp      = (!D.NAME eq 'PS') ? 1.6 : 1.4
  
  result = crtm_read_atmosphere_binary('Truncated.ECMWF.Atmosphere.bin',os)
  result = crtm_read_atmosphere_binary('NoShift.Extra_Layers.ECMWF.Atmosphere.bin',ns)
  result = crtm_read_atmosphere_binary('Extra_Layers.ECMWF.Atmosphere.bin',es)
  
  for m = 0, n_elements(os)-1 do begin
    o = *os[m]
    n = *ns[m]
    e = *es[m]
    
    yrange=[5,0.005]
    ytitle='Pressure (hPa)'
    
    yo = *o.pressure
    ye = *e.pressure
    loco = where( yo le yrange[0] and yo ge yrange[1] )
    loce = where( ye lt yo[loco[0]], nloce)
    
    ybridge=[ yo[loco[0]],ye[loce[nloce-1]] ]

    !p.multi=[0,2,2]
    
    ox = (*o.temperature)
    nx = (*n.temperature)
    ex = (*e.temperature)
    xrange=[min([ox[loco],ex[loce],nx[loce]]),max([ox[loco],ex[loce],nx[loce]])]
    plot, ox, yo, $
          title='Profile '+strtrim(m+1,2)+' temperature extension', $
          xrange=xrange, xtitle='Temperature (K)', $
          yrange=yrange, /ystyle, /ylog, ytitle=ytitle, ytickformat='logticks',$
          charsize=charsize, font=font, thick=thick, $
          psym=-6
    oplot, nx[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=red
    oplot, ex[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=green
    oplot, ex[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=green
    xbridge = [ ox[loco[0]],ex[loce[nloce-1]] ]
    oplot, xbridge, ybridge, $
           thick=thick, $
           color=blue
    
    ox = (*o.absorber)[*,0]
    nx = (*n.absorber)[*,0]
    ex = (*e.absorber)[*,0]
    xrange=[min([ox[loco],ex[loce],nx[loce]]),max([ox[loco],ex[loce],nx[loce]])]
    plot, ox, yo, $
          title='Profile '+strtrim(m+1,2)+' water vapour extension', $
          xrange=xrange, xtitle='Water vapour (g/kg)', $
          yrange=yrange, /ystyle, /ylog, ytitle=ytitle, ytickformat='logticks', $
          charsize=charsize, font=font, thick=thick, $
          psym=-6
    oplot, nx[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=red
    oplot, ex[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=green
    xbridge = [ ox[loco[0]],ex[loce[nloce-1]] ]
    oplot, xbridge, ybridge, $
           thick=thick, $
           color=blue
    
    ox = (*o.absorber)[*,1]
    nx = (*n.absorber)[*,1]
    ex = (*e.absorber)[*,1]
    xrange=[min([ox[loco],ex[loce],nx[loce]]),max([ox[loco],ex[loce],nx[loce]])]
    plot, ox, yo, $
          title='Profile '+strtrim(m+1,2)+' ozone extension', $
          xrange=xrange, xtitle='Ozone (ppmv)', $
          yrange=yrange, /ystyle, /ylog, ytitle=ytitle, ytickformat='logticks', $
          charsize=charsize, font=font, thick=thick, $
          psym=-6
    oplot, nx[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=red
    oplot, ex[loce], ye[loce], $
           thick=thick, $
           psym=-4, color=green
    xbridge = [ ox[loco[0]],ex[loce[nloce-1]] ]
    oplot, xbridge, ybridge, $
           thick=thick, $
           color=blue
    
    mylegend, xlp, 0.6, $
              ['Input profile','Climatology, no shift','Climatology, shifted'], $
              color = [!p.color,red, green], psym=[-6,-4,-4],thick=[thick,thick,thick], $
              charsize=lcharsize, font=font
    if ( !d.name eq 'X' ) then begin
      q = get_kbrd(1)
      if ( strupcase(q) eq 'Q' ) then break
    endif
  endfor
end
  
  
  
  
  
