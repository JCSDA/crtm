pro create_threshold_plot, tol=tol, gref=gref

  n = 250L
  x1 = 800.0d0
  x2 = 950.0d0
  x = dindgen(n)/double(n-1)
  x = x*(x2-x1) + x1
  
  x0_a = (x1+x2)/2.0d0
  dx_a =  20.0d0
  x0_b = 920.0d0
  dx_b =  10.0d0
  
  result = gaussian_srf(x0_a,dx_a,4,x,ya)
  result = gaussian_srf(x0_b,dx_b,3,x,yb)

  y = (ya + yb/100.0d0) + randomn(seed,n)/1000.0d0
  
 
  osrf=obj_new('osrf')
  osrf->allocate, n
  osrf->set_property,frequency=x,response=y, sensor_id='Test', channel=1
  osrf->apply_response_threshold,tol,gref=gref
;  p = plot(x,y)
;  p.refresh, /disable
;  !null = plot(p.xrange,[tol,tol],overplot=p,linestyle='dash')
;  p.refresh
    
end
