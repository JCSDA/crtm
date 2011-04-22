;-------------------------------------------------
; Program to select predictors by try-and-error
;-------------------------------------------------

;sensor='crisB3_npp'
sensor='iasiB3_metop-a'

TranSet_file='/u/wx23yh/noscrub_jcsda/work_nlte/'+sensor+'_radSpectrum.nc'
f_spccoff='/u/wx23yh/CRTM/EXP-NLTE/fix/SpcCoeff/netCDF/'+sensor+'.SpcCoeff.nc'

D2R = !PI/180.0
;Rd = 287.0
;G = 9.81

print, read_ncdf(TranSet_file, d)
print, read_ncdf(f_spccoff, s)

wn=s.wavenumber
ch_idx=s.SENSOR_CHANNEL
nw=n_elements(wn)

n_channels = n_Elements(d.LTE_radiance[*, 0, 0])
n_sensor_angles = n_Elements(d.Sensor_zenith_angle)
n_sun_angles = n_Elements(d.Sun_zenith_angle)
n_levels = n_Elements(d.Level_pressure[*, 0])
n_profiles = n_Elements(d.Level_pressure[0, *])

d.NLTE_radiance=d.NLTE_radiance*1.0e7
d.LTE_radiance=d.LTE_radiance*1.0e7

drad = dblarr(n_channels, n_sensor_angles, n_sun_angles, n_profiles)
drad_pred = dblarr(n_channels, n_sensor_angles, n_sun_angles, n_profiles)
pred_radiance = dblarr(n_channels, n_sensor_angles, n_sun_angles, n_profiles)

Tm1 =dblarr(n_profiles)
p1=0.005
p2=0.2
for iprof = 0, n_profiles-1 do begin
  idx=WHERE(d.LEVEL_PRESSURE[*,iprof] ge p1 AND d.LEVEL_PRESSURE[*,iprof] le p2, np)
  t = d.LEVEL_TEMPERATURE[idx,iprof]
  tt = 0.5*(t[0:np-2]+t[1:np-1])
  Tm1[iprof] = MEAN(tt) ;MEAN(d.LEVEL_TEMPERATURE[idx,iprof])
endfor

Tm2 =dblarr(n_profiles)
p1=0.2
p2=52.0
for iprof = 0, n_profiles-1 do begin
  idx=WHERE(d.LEVEL_PRESSURE[*,iprof] ge p1 AND d.LEVEL_PRESSURE[*,iprof] le p2, np)
  t = d.LEVEL_TEMPERATURE[idx,iprof]
  tt = 0.5*(t[0:np-2]+t[1:np-1])
  Tm2[iprof] = MEAN(tt) ;MEAN(d.LEVEL_TEMPERATURE[idx,iprof])
;  Tm2[iprof] = d.level_pressure[40, iprof]

endfor

n_pred = 2
;n_samples = n_profiles*n_sun_angles*n_sensor_angles
n_samples = n_profiles
y = dblarr(n_samples)
x = dblarr(n_pred, n_samples)
c = dblarr(n_pred, n_channels)
c0 = dblarr(n_channels)
n=0
isen=0
isun=0

bias = dblarr(n_channels, n_sensor_angles, n_sun_angles)
rms = dblarr(n_channels, n_sensor_angles, n_sun_angles)
nlte_eff = dblarr(n_channels, n_sensor_angles, n_sun_angles)

bias_sum = dblarr(n_channels)
rms_sum = dblarr(n_channels)
nlte_eff_sum = dblarr(n_channels)
bias_max = dblarr(n_channels)
bias_max[*] = 1.0e-9

for isun = 0, n_sun_angles-1 do begin
  for isen = 0, n_sensor_angles-1 do begin

   for ich = 0, n_channels-1 do begin

     n=0
     for iprof = 0, n_profiles-1 do begin

         drad[ich, isen, isun, iprof] = d.NLTE_radiance[ich, isen, isun, iprof] - $
                                    d.LTE_radiance[ich, isen, iprof]
         y[n] = drad[ich, isen, isun, iprof]
         x[0, n] = Tm1[iprof]
         x[1, n] = Tm2[iprof]

         n = n + 1
     endfor

     c[*,ich]=REGRESS(x, y, CONST=const)
     c0[ich]=const

   endfor

   for ich = 0, n_channels-1 do begin
     n = 0
     for iprof = 0, n_profiles-1 do begin
            drad_pred[ich, isen, isun, iprof] = c0[ich] + TOTAL(c[*, ich]*x[*, n])
            pred_radiance[ich, isen, isun, iprof] = d.LTE_radiance[ich, isen, iprof] + $
                                                    drad_pred[ich, isen, isun, iprof]
            n = n + 1
     endfor
   endfor

   bias_sum[*] = 0.0
   rms_sum[*] = 0.0
   nlte_eff_sum[*] = 0.0
   for iprof = 0, n_profiles-1 do begin                                                                              
      error_status=Planck_Temperature(wn, pred_radiance[*, isen, isun, iprof], pred_tb)                                                         
      error_status=Planck_Temperature(wn, d.LTE_radiance[*, isen, iprof], lte_tb)                            
      error_status=Planck_Temperature(wn, d.NLTE_radiance[*, isen, isun, iprof], nlte_tb)      
      diff = pred_tb - nlte_tb                                                                 
      bias_sum = bias_sum + diff                                                                   
      rms_sum = rms_sum + diff^2                                                               
      nlte_eff_sum =  nlte_eff_sum + (nlte_tb - lte_tb)^2 
      for ich = 0, n_channels-1 do begin    
        if( ABS(diff[ich]) gt ABS(bias_max[ich]) )then begin
          bias_max[ich] = diff[ich]
        endif
      endfor                                 
   endfor                                                                                                            
   bias[*, isen, isun] = bias_sum / n_samples
   rms[*, isen, isun] = SQRT(rms_sum / n_samples)
   nlte_eff[*, isen, isun] = SQRT(nlte_eff_sum / n_samples)


 endfor ; sensor angle loop
endfor  ; sun angle loop

bias_avg = fltarr(n_channels)
;bias_max = fltarr(n_channels)
rms_avg = fltarr(n_channels)
rms_max = fltarr(n_channels)
nlte_avg = fltarr(n_channels)
nlte_max = fltarr(n_channels)

for ich = 0, n_channels-1 do begin
  bias_avg[ich] = MEAN(bias[ich, *, *])
;  bias_max[ich] = MAX(ABS(bias[ich, *, *]))
  rms_avg[ich] = rms[ich, 0, 0]; MEAN(rms[ich, *, *])
  rms_max[ich] = MAX(rms[ich, *, *])
  nlte_avg[ich] = MEAN(nlte_eff[ich, *, *])
  nlte_max[ich] = MAX(nlte_eff[ich, *, *])
endfor

;idx=where(wn gt 2200.0 and wn lt 2400.0)

;OPENW, 10, sensor + '_fittingErr.txt'
;printf, 10, n_channels
;printf, 10, 'wn, chIdx, bias, max_bias, rms, max_rms, nlteEff, max_nlteEff'

for ich = 0, n_channels-1 do begin
  print, format='(f10.4, i5, 6f9.3)', wn[ich], ch_idx[ich], $ 
     bias_avg[ich], bias_max[ich], rms_avg[ich], rms_max[ich], nlte_avg[ich], nlte_max[ich]                                              
endfor

close, /all


end
