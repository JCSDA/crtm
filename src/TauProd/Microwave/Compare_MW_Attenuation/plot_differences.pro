pro plot_differences, lfile, rfile, n, pcdiff=pcdiff

  @error_codes

  lstatus = read_spectra_netcdf(lfile,n,ls)
  rstatus = read_spectra_netcdf(rfile,n,rs)

  if ( lstatus ne SUCCESS or rstatus ne SUCCESS ) then goto, Done
      

  if ( keyword_set(pcdiff) ) then begin
    loc = where( *ls.spectra gt 0.0d )
    diff=DBLARR(ls.n_frequencies,ls.n_layers)
    diff[loc] = 100.0d0 * (*ls.spectra - *rs.spectra)[loc]/(*ls.spectra)[loc]
    loc = where(diff gt 100.0d0, count )
    if ( count gt 0 ) then $
      diff[loc]=100.0d0
    loc = where(diff lt -100.0d0, count )
    if ( count gt 0 ) then $
      diff[loc]=-100.0d0
    ztitle = '% Attenuation Difference'
  endif else begin
    diff = *ls.spectra - *rs.spectra
    ztitle = 'Attenuation Difference (dB)'
  endelse

  if ( !d.name eq 'X' ) then $
    if ( !d.window ne 0 ) then $
      window, 0, xsize=900, ysize=700

  !y.omargin=[0,3]
  shade_surf, diff, *ls.frequency, reverse(lindgen(ls.n_layers)+1), $
              xtitle=ls.frequency_longname+' ('+ls.frequency_units+')', $
              ytitle='Layer (1==TOA)',$
              ztitle=ztitle,$
              yrange=[ls.n_layers,0], $
              charsize=4.0, ax=35,pixels=1000

  xyouts, 0.5, 0.95, $
          'Liebe-Rosenkranz Difference in ' + ls.spectra_longname + '!C!CProfile #'+strtrim(n,2), $
          alignment=0.5, /norm, $
          charsize=2.0

  Done:
  lstatus = destroy_spectra(ls)
  rstatus = destroy_spectra(rs)

end
