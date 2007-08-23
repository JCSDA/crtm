pro sfcplot, z1, z2
  charsize=4
  zrange=[MIN(z1<z2),MAX(z1>z2)]
  surface, z1, zrange=zrange,charsize=charsize
  surface, z2, zrange=zrange,charsize=charsize
  surface, z1-z2, charsize=charsize
end

pro plotac, sensor_id

  v=['R1V1','R1V2','R1V3','R1V4']
  nv = n_elements(v)

  for i=0, nv-1 do begin
    result = read_netcdf('Data/'+v[i]+'/'+sensor_id+'.SpcCoeff.nc',x,/quiet)
    if i eq 0 then begin
      ac = x
      tn = tag_names(x)
    endif else $
      ac = [ac,x]
  endfor
  

  !p.multi=[0,3,3]
  for n=0,nv-2 do begin
    n1=n
    n2=n+1
    z1 = ac[n1].a_earth
    z2 = ac[n2].a_earth
    sfcplot, z1, z2
    z1 = ac[n1].a_platform
    z2 = ac[n2].a_platform
    sfcplot, z1, z2
    z1 = ac[n1].a_space
    z2 = ac[n2].a_space
    sfcplot, z1, z2
    q=get_kbrd(1)
  endfor


end
