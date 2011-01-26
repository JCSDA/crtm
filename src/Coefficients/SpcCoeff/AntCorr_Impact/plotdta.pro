pro plotdta, sensor_id

  read_dta, sensor_id+'.dTa.bin', ta, ta_ra, data_id
  
  dta = ta - ta_ra
  
  info=size(ta,/structure)
  nfov=info.dimensions[0]
  nch =info.dimensions[1]
  nt  =info.dimensions[2]
  nv  =info.dimensions[3]


  !p.multi=[0,3,3]
  !x.omargin=[5,0]
  !y.omargin=[0,5]
  charsize=5
  ztitle='Temperature (K)'
  k = nt/2
  n1=0
  n2=n1+1
  for n = 1, nv-1 do begin
    n1=0
    n2=n
    
    surface, ta[*,*,k,n1],charsize=charsize
    surface, ta_ra[*,*,k,n1],charsize=charsize
    surface, dta[*,*,k,n1],charsize=charsize
  
    surface, ta[*,*,k,n2],charsize=charsize
    surface, ta_ra[*,*,k,n2],charsize=charsize
    surface, dta[*,*,k,n2],charsize=charsize

    surface, ta[*,*,k,n1]-ta[*,*,k,n2],charsize=charsize
    surface, ta_ra[*,*,k,n1]-ta_ra[*,*,k,n2],charsize=charsize
    surface, dta[*,*,k,n1]-dta[*,*,k,n2],charsize=charsize

    x=0.02
    y=0.73    
    xyouts, x,y,data_id[n1],/norm,align=0,charsize=charsize/2
    y=0.45
    xyouts, x,y,data_id[n2],/norm,align=0,charsize=charsize/2
    y=0.17
    xyouts, x,y,'Difference',/norm,align=0,charsize=charsize/2

    y=0.9
    x=0.18
    xyouts, x,y,'AC applied to T!Db!N',/norm,align=0,charsize=charsize/2
    x=0.49
    xyouts, x,y,'AC applied to R,!Cconverted to T!Da!N',/norm,align=0,charsize=charsize/2
    x=0.81
    xyouts, x,y,'Difference',/norm,align=0,charsize=charsize/2
    
    xyouts, 0.5, 0.95, 'Antenna correction (AC) results for '+sensor_id, $
            /norm,align=0.5,charsize=charsize/1.75
      
    q=get_kbrd(1)
  endfor
  
end

