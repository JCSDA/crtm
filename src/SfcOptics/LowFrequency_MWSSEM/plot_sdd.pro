pro plot_sdd, ps=ps

  @color_db
  
;  N_FREQUENCIES = 11
;  N_WIND_SPEEDS = 40
;
;  FREQUENCY_SDD=[ $
;      3.0,  5.0,  7.0,  9.0, 11.0, $
;     13.0, 15.0, 17.0, 19.0, 21.0, $
;     23.0 ]
;
;  WIND_SPEED_SDD=[ $
;     0.5, 1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, $
;     4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0, $
;     8.5, 9.0, 9.5,10.0,10.5,11.0,11.5,12.0, $
;    12.5,13.0,13.5,14.0,14.5,15.0,15.5,16.0, $
;    16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0 ]
;
;  sdd=dblarr(N_FREQUENCIES,N_WIND_SPEEDS)
;
;  sdd[*,0]=[$
;     3.45700E-03, 9.60280E-03, 1.86610E-02, 2.63970E-02, 2.99600E-02, $
;     3.11630E-02, 3.11300E-02, 3.02250E-02, 2.87810E-02, 2.73210E-02, $
;     2.57200E-02]
;
;  sdd[*,1]=[$
;     4.04780E-02, 4.26570E-02, 4.30630E-02, 4.22650E-02, 4.07540E-02, $
;     3.94740E-02, 3.78900E-02, 3.60050E-02, 3.41810E-02, 3.24220E-02, $
;     3.04150E-02]
;
;  sdd[*,2]=[$
;     4.59590E-02, 4.48090E-02, 4.46420E-02, 4.36330E-02, 4.24890E-02, $
;     4.15140E-02, 4.01950E-02, 3.88670E-02, 3.74780E-02, 3.60030E-02, $
;     3.53260E-02]
;
;  sdd[*,3]=[$
;     4.59450E-02, 4.58550E-02, 4.47040E-02, 4.46270E-02, 4.41120E-02, $
;     4.32700E-02, 4.28010E-02, 4.19510E-02, 4.10440E-02, 4.08170E-02, $
;     4.11710E-02]
;
;  sdd[*,4]=[$
;     4.64550E-02, 4.56360E-02, 4.59130E-02, 4.55520E-02, 4.53160E-02, $
;     4.51440E-02, 4.53070E-02, 4.51610E-02, 4.52720E-02, 4.59090E-02, $
;     4.67220E-02]
;
;  sdd[*,5]=[$
;     4.66200E-02, 4.59520E-02, 4.58590E-02, 4.60200E-02, 4.59480E-02, $
;     4.64690E-02, 4.65140E-02, 4.70470E-02, 4.72740E-02, 4.85060E-02, $
;     4.90510E-02]
;
;  sdd[*,6]=[$
;     4.61430E-02, 4.62460E-02, 4.63810E-02, 4.67430E-02, 4.77040E-02, $
;     4.85840E-02, 4.95640E-02, 5.12130E-02, 5.25020E-02, 5.29830E-02, $
;     5.38460E-02]
;
;  sdd[*,7]=[$
;     4.66920E-02, 4.60310E-02, 4.66870E-02, 4.77130E-02, 4.86120E-02, $
;     4.99180E-02, 5.17960E-02, 5.37230E-02, 5.46800E-02, 5.54400E-02, $
;     5.60010E-02]
;
;  sdd[*,8]=[$
;     4.63630E-02, 4.69850E-02, 4.79640E-02, 4.94210E-02, 5.30620E-02, $
;     5.50180E-02, 5.59020E-02, 5.75140E-02, 5.88560E-02, 5.99310E-02, $
;     6.00960E-02]
;
;  sdd[*,9]=[$
;     4.65880E-02, 4.71020E-02, 4.87240E-02, 5.08410E-02, 5.46400E-02, $
;     5.65520E-02, 5.82880E-02, 5.97860E-02, 6.10270E-02, 6.20060E-02, $
;     6.20490E-02]
;
;  sdd[*,10]=[$
;     4.63770E-02, 4.79790E-02, 5.07890E-02, 5.49620E-02, 5.83330E-02, $
;     6.02050E-02, 6.19130E-02, 6.33790E-02, 6.45780E-02, 6.55040E-02, $
;     6.61630E-02]
;
;  sdd[*,11]=[$
;     4.66490E-02, 4.87630E-02, 5.48840E-02, 5.66550E-02, 6.01050E-02, $
;     6.20090E-02, 6.37430E-02, 6.52270E-02, 6.64330E-02, 6.73580E-02, $
;     6.80090E-02]
;
;  sdd[*,12]=[$
;     4.73370E-02, 5.04070E-02, 5.78990E-02, 5.98650E-02, 6.35580E-02, $
;     6.55950E-02, 6.74360E-02, 6.90000E-02, 7.02620E-02, 7.12200E-02, $
;     7.18850E-02]
;
;  sdd[*,13]=[$
;     4.79880E-02, 5.43970E-02, 6.08900E-02, 6.31260E-02, 6.56630E-02, $
;     6.93250E-02, 7.01740E-02, 7.19480E-02, 7.33710E-02, 7.44530E-02, $
;     7.52120E-02]
;
;  sdd[*,14]=[$
;     4.88910E-02, 5.87050E-02, 6.30810E-02, 6.58100E-02, 6.87010E-02, $
;     7.13730E-02, 7.37010E-02, 7.56450E-02, 7.72040E-02, 7.83900E-02, $
;     7.83620E-02]
;
;  sdd[*,15]=[$
;     5.08950E-02, 6.31620E-02, 6.58390E-02, 7.13070E-02, 7.45450E-02, $
;     7.61300E-02, 7.88420E-02, 7.99710E-02, 8.18550E-02, 8.23400E-02, $
;     8.34470E-02]
;
;  sdd[*,16]=[$
;     5.54760E-02, 6.46340E-02, 6.81130E-02, 7.41470E-02, 7.61420E-02, $
;     7.96030E-02, 8.12890E-02, 8.38430E-02, 8.48200E-02, 8.54770E-02, $
;     8.67520E-02]
;
;  sdd[*,17]=[$
;     5.87440E-02, 6.91110E-02, 7.31410E-02, 7.77400E-02, 8.03610E-02, $
;     8.28910E-02, 8.64080E-02, 8.81030E-02, 8.94090E-02, 9.03320E-02, $
;     9.08970E-02]
;
;  sdd[*,18]=[$
;     6.37450E-02, 7.11440E-02, 7.64340E-02, 8.18560E-02, 8.50240E-02, $
;     8.79790E-02, 9.05320E-02, 9.26230E-02, 9.31090E-02, 9.43720E-02, $
;     9.52240E-02]
;
;  sdd[*,19]=[$
;     6.75330E-02, 7.14800E-02, 7.78540E-02, 8.39610E-02, 8.75990E-02, $
;     9.09100E-02, 9.23600E-02, 9.47960E-02, 9.67050E-02, 9.70420E-02, $
;     9.80910E-02]
;
;  sdd[*,20]=[$
;     6.83640E-02, 7.42600E-02, 8.16470E-02, 8.63360E-02, 9.07880E-02, $
;     9.47130E-02, 9.66050E-02, 9.94340E-02, 1.00460E-01, 1.01120E-01, $
;     1.02470E-01]
;
;  sdd[*,21]=[$
;     7.34960E-02, 7.95300E-02, 8.45850E-02, 9.00820E-02, 9.51380E-02, $
;     9.79040E-02, 1.00300E-01, 1.02240E-01, 1.04930E-01, 1.05860E-01, $
;     1.06410E-01]
;
;  sdd[*,22]=[$
;     7.10970E-02, 8.02870E-02, 8.95720E-02, 9.32400E-02, 9.89780E-02, $
;     1.02230E-01, 1.03530E-01, 1.05910E-01, 1.07750E-01, 1.09070E-01, $
;     1.09960E-01]
;
;  sdd[*,23]=[$
;     7.36820E-02, 8.40180E-02, 9.12120E-02, 9.81670E-02, 1.02370E-01, $
;     1.04470E-01, 1.07850E-01, 1.10580E-01, 1.11430E-01, 1.13090E-01, $
;     1.14270E-01]
;
;  sdd[*,24]=[$
;     8.18210E-02, 8.72020E-02, 9.52010E-02, 1.00400E-01, 1.05370E-01, $
;     1.08020E-01, 1.11860E-01, 1.13550E-01, 1.16100E-01, 1.16820E-01, $
;     1.18310E-01]
;
;  sdd[*,25]=[$
;     8.28020E-02, 8.99060E-02, 9.88250E-02, 1.02260E-01, 1.08030E-01, $
;     1.11260E-01, 1.15580E-01, 1.17650E-01, 1.19200E-01, 1.20280E-01, $
;     1.22110E-01]
;
;  sdd[*,26]=[$
;     8.31730E-02, 9.21910E-02, 9.91400E-02, 1.06140E-01, 1.10390E-01, $
;     1.16000E-01, 1.17460E-01, 1.20050E-01, 1.22050E-01, 1.23510E-01, $
;     1.25680E-01]
;
;  sdd[*,27]=[$
;     8.30700E-02, 9.41130E-02, 1.02090E-01, 1.09790E-01, 1.14520E-01, $
;     1.18740E-01, 1.22290E-01, 1.23670E-01, 1.26010E-01, 1.27770E-01, $
;     1.29050E-01]
;
;  sdd[*,28]=[$
;     8.91170E-02, 9.98440E-02, 1.04780E-01, 1.13210E-01, 1.16370E-01, $
;     1.21240E-01, 1.25310E-01, 1.27100E-01, 1.29790E-01, 1.30580E-01, $
;     1.32220E-01]
;
;  sdd[*,29]=[$
;     8.81060E-02, 1.01160E-01, 1.07230E-01, 1.13940E-01, 1.20090E-01, $
;     1.25390E-01, 1.28140E-01, 1.30340E-01, 1.33410E-01, 1.34500E-01, $
;     1.35220E-01]
;
;  sdd[*,30]=[$
;     9.35810E-02, 1.02220E-01, 1.12600E-01, 1.16940E-01, 1.23650E-01, $
;     1.27510E-01, 1.30780E-01, 1.33410E-01, 1.35440E-01, 1.38270E-01, $
;     1.39280E-01]
;
;  sdd[*,31]=[$
;     9.35500E-02, 1.04710E-01, 1.13220E-01, 1.21530E-01, 1.26710E-01, $
;     1.31310E-01, 1.35180E-01, 1.38300E-01, 1.40760E-01, 1.41310E-01, $
;     1.42850E-01]
;
;  sdd[*,32]=[$
;     9.85130E-02, 1.09590E-01, 1.18240E-01, 1.24170E-01, 1.29910E-01, $
;     1.34960E-01, 1.37450E-01, 1.41030E-01, 1.43870E-01, 1.44760E-01, $
;     1.46610E-01]
;
;  sdd[*,33]=[$
;     9.61960E-02, 1.09960E-01, 1.19920E-01, 1.26660E-01, 1.32990E-01, $
;     1.36530E-01, 1.41320E-01, 1.43620E-01, 1.46860E-01, 1.48090E-01, $
;     1.50270E-01]
;
;  sdd[*,34]=[$
;     1.00720E-01, 1.14610E-01, 1.21450E-01, 1.29020E-01, 1.35940E-01, $
;     1.39920E-01, 1.43320E-01, 1.47690E-01, 1.49730E-01, 1.51310E-01, $
;     1.52550E-01]
;
;  sdd[*,35]=[$
;     1.06720E-01, 1.16120E-01, 1.24380E-01, 1.32860E-01, 1.38180E-01, $
;     1.44960E-01, 1.48770E-01, 1.50260E-01, 1.52880E-01, 1.54980E-01, $
;     1.56700E-01]
;
;  sdd[*,36]=[$
;     1.03610E-01, 1.15990E-01, 1.28950E-01, 1.34940E-01, 1.40860E-01, $
;     1.46100E-01, 1.50500E-01, 1.54100E-01, 1.57010E-01, 1.57970E-01, $
;     1.60030E-01]
;
;  sdd[*,37]=[$
;     1.09100E-01, 1.21630E-01, 1.31540E-01, 1.38450E-01, 1.45050E-01, $
;     1.50820E-01, 1.53830E-01, 1.57980E-01, 1.59840E-01, 1.62740E-01, $
;     1.63900E-01]
;
;  sdd[*,38]=[$
;     1.13210E-01, 1.21190E-01, 1.32510E-01, 1.40300E-01, 1.47510E-01, $
;     1.53760E-01, 1.57150E-01, 1.59950E-01, 1.63770E-01, 1.65520E-01, $
;     1.67040E-01]
;
;  sdd[*,39]=[$
;     1.06990E-01, 1.22540E-01, 1.33950E-01, 1.41810E-01, 1.49100E-01, $
;     1.53340E-01, 1.58850E-01, 1.61700E-01, 1.64030E-01, 1.65960E-01, $
;     1.68950E-01]

  charsize=(keyword_set(ps)) ? 1.5 : 1.0
  lcharsize=(keyword_set(ps)) ? 1.0 : 1.0
  font    =(keyword_set(ps)) ? 1   : -1
  thick   =(keyword_set(ps)) ? 2.0 : 1.0
  ct      =(keyword_set(ps)) ? 40  : 39
  bg      =(keyword_set(ps)) ? white : black
  fg      =(keyword_set(ps)) ? black : white
  xmargin =(keyword_set(ps)) ? [10,12] : [10,20]
  zeta    =(keyword_set(ps)) ? '!9z!X' : '!4f!X'
  psave=!p

  
  ztitle = '4k!U2!N'+zeta+'!S!U2!R!DR!N'

  o = read_sdd()
  frequency_sdd = o.f
  n_frequencies = n_elements(frequency_sdd)
  wind_speed_sdd = o.w
  n_wind_speeds = n_elements(wind_speed_sdd)
  sdd = transpose(o.sdd)

print, o.f
;  surface, sdd, frequency_sdd, wind_speed_sdd, $
;           xtitle='Frequency (GHz)', $
;           ytitle='Wind speed (m.s!U-1!N)', $
;           ztitle='Ocean height variance, '+ztitle+' (m!U2!N)', $
;           charsize=3.0, font=font,thick=thick
;  xyouts, 0.5, 0.95, 'Ocean height variance used in the Low Frequency!C'+$
;                     'Microwave Sea Surface Emissivity model', $
;          alignment=0.5,/normal, $
;          charsize=charsize*1.5,font=font
;
;  q=get_kbrd(1)


  sdd_smth = sdd
  
  edge_truncate=1
  edge_wrap=0
  edge_zero=0
  ; Smooth along wind speed direction
  for i=0,n_frequencies-1 do begin
    plot, wind_speed_sdd, sdd[i,*], $
    xtitle='Wind speed (m.s!U-1!N)', $
    ytitle='Ocean height variance (m!U2!N)', $
    title='Ocean height variance wind speed spectra for different f='+$
          strtrim(string(frequency_sdd[i],format='(f6.2,"GHz")'),2), $
    charsize=charsize,font=font,thick=thick
    n=13
    filt = savgol(n/2,n/2,0,4)
    smth = convol(reform(sdd[i,*]),filt,$
           edge_truncate=edge_truncate,edge_wrap=edge_wrap,edge_zero=edge_zero)
    sdd_smth[i,*] = smth
    oplot, wind_speed_sdd, smth,thick=thick,color=green
;    q=get_kbrd(1)
;    if ( strupcase(q) eq 'Q') then break
  endfor
    
  ; Smooth along frequency direction
  for i=0,n_wind_speeds-1 do begin
    plot, frequency_sdd, sdd_smth[*,i], $
          xtitle='Frequency (GHz)', $
          ytitle='Ocean height variance (m!U2!N)', $
          title='Ocean height variance frequency spectra for different w='+$
                strtrim(string(wind_speed_sdd[i],format='(f4.1,"m.s!U-1!N")'),2), $
          charsize=charsize,font=font,thick=thick
    n=21
    filt = savgol(n/2,n/2,0,4)
    smth = convol(reform(sdd_smth[*,i]),filt,$
           edge_truncate=edge_truncate,edge_wrap=edge_wrap,edge_zero=edge_zero)
    oplot, frequency_sdd, smth, thick=thick,color=red
    sdd_smth[*,i] = smth
;    q=get_kbrd(1)
;    if ( strupcase(q) eq 'Q') then break
  endfor
  
  sdd = sdd_smth
;    
;  surface, sdd_smth, frequency_sdd, wind_speed_sdd, $
;           xtitle='Frequency (GHz)', $
;           ytitle='Wind speed (m.s!U-1!N)', $
;           ztitle='Ocean height variance (m!U2!N)', $
;           charsize=3.0, font=font,thick=thick
;  xyouts, 0.5, 0.95, 'Smoothed ocean height variance', $
;          alignment=0.5,/normal, $
;          charsize=charsize*1.5,font=font
;          
;  q=get_kbrd(1)
;
;  surface, sdd-sdd_smth, frequency_sdd, wind_speed_sdd, $
;           xtitle='Frequency (GHz)', $
;           ytitle='Wind speed (m.s!U-1!N)', $
;           ztitle='d(Ocean height variance (m!U2!N))', $
;           charsize=3.0, font=font,thick=thick
;  xyouts, 0.5, 0.95, 'Original-Smoothed ocean height variance', $
;          alignment=0.5,/normal, $
;          charsize=charsize*1.5,font=font
;          
;RETURN




  locf = where(frequency_sdd  ge 15.0 and frequency_sdd  le 23.0, n_frequencies)
  locw = where(wind_speed_sdd ge 0.5  and wind_speed_sdd le 20.0, n_wind_speeds)
  
  frequency_sdd  = frequency_sdd[locf]
  wind_speed_sdd = wind_speed_sdd[locw]
  sdd = sdd[locf[0]:locf[n_frequencies-1],locw[0]:locw[n_wind_speeds-1]]

;print, format='(2x,"INTEGER , PARAMETER :: N_FREQUENCIES = ",i2)', n_frequencies
;print, format='(2x,"REAL(fp), PARAMETER :: FREQUENCY_SDD(N_FREQUENCIES) = (/ &")'
;print, format='(5x,5(f4.1,"_fp, "), f4.1,"_fp /)")', frequency_sdd
;
;print, format='(2x,"INTEGER , PARAMETER :: N_WIND_SPEEDS = ",i2)', n_wind_speeds
;print, format='(2x,"REAL(fp), PARAMETER :: WIND_SPEED_SDD(N_WIND_SPEEDS) = (/ &")'
;print, format='(5x,5(f4.1,"_fp,")," &",/)', wind_speed_sdd
;
;for i=0,n_wind_speeds-1 do begin
;  print, format='(2x,"DATA (sdd(n,",i2,"),n=1,N_FREQUENCIES) / &")', i+1  
;  print, format='(5(e12.5,"_fp,"),e12.5,"_fp /")', sdd[*,i]
;endfor


;  tvlct, r, g, b, /get
;  loadct,ct,bottom=16,ncolors=254
;  colors
;  !p.color=fg
;  !p.background=bg
  
  yrange=[min(sdd),max(sdd)]
  
;;  AMSRE_AQUA_F0 = [6.925,  10.650, 18.700 ]
;;  
;;  ; plot individual wind speeds
;;  plot, frequency_sdd, sdd[*,0], $
;;        xtitle='Frequency (GHz)', $
;;        ytitle='Ocean height variance (m!U2!N)', $
;;        title='Ocean height variance frequency spectra for different wind speeds used in the!C'+$
;;              'Low Frequency Microwave Sea Surface Emissivity model', $
;;;        xrange=[2,27], /xstyle, $
;;;        xmargin=xmargin,$
;;        yrange=yrange, $
;;        ymargin=[4,4], $
;;        charsize=charsize,font=font,thick=thick, $
;;        /nodata
;;  for j=0,n_wind_speeds-1 do begin
;;    color=(j+5)*5
;;    oplot, frequency_sdd, sdd[*,j], color=color,thick=thick
;;    if ( j eq 0 ) then begin
;;      lcolor=color
;;      ltxt=string(wind_speed_sdd[j],format='(f4.1)')
;;    endif else begin
;;      lcolor=[lcolor,color]
;;      ltxt=[ltxt,string(wind_speed_sdd[j],format='(f4.1)')]
;;    endelse
;;  endfor
;;  for j=0,n_elements(AMSRE_AQUA_F0)-1 do begin
;;    oplot, [AMSRE_AQUA_F0[j],AMSRE_AQUA_F0[j]],!y.crange,$
;;           thick=thick,linestyle=2
;;  endfor
;;
;;  mylegend, 0.875,0.96,ltxt,color=lcolor,thick=replicate(thick,n_wind_speeds), $
;;            font=font
;;              
;;  q=get_kbrd(1)

  avgsdd = total(sdd,1,/double)/double(n_frequencies)
  c = linfit(wind_speed_sdd,avgsdd,yfit=avgsddfit)
  !P.multi=[0,1,2]
  pcolor = (keyword_set(ps)) ? [red,green,blue,magenta,gold,aquamarine]: [red,green,cyan,magenta,gold,aquamarine]

  if ( keyword_set(ps) ) then pson,file='sdd_wind_speed_spectra.ps'

  ; plot individual frequencies
  plot, wind_speed_sdd, sdd[0,*], $
        xtitle='Wind speed (m.s!U-1!N)', $
        ytitle='Height variance (m!U2!N)', $
;        title='Ocean height variance wind speed spectra for different frequencies used in the!C'+$
;              'Low Frequency Microwave Sea Surface Emissivity model', $
;        title='Ocean height variance ('+ztitle+') wind speed spectra', $
        yrange=yrange, $
;        ymargin=[4,4], $
        charsize=charsize,font=font,thick=thick, $
        /nodata
  for i=0,n_frequencies-1 do begin
;    color=(i+3)*18
;    color=(i+3)*32
    color = pcolor[i]
    oplot, wind_speed_sdd, sdd[i,*], color=color,thick=thick
    if ( i eq 0 ) then begin
      lcolor=color
      ltxt=string(frequency_sdd[i],format='(f4.1,"GHz")')
    endif else begin
      lcolor=[lcolor,color]
      ltxt=[ltxt,string(frequency_sdd[i],format='(f4.1,"GHz")')]
    endelse
  endfor
  oplot, wind_speed_sdd, avgsddfit, thick=thick, linestyle=2
  
  mylegend, 0.1,0.9,ltxt,color=lcolor,thick=replicate(thick,n_frequencies), $
            font=font,charsize=lcharsize

  normsdd = sdd - transpose(rebin(avgsddfit,n_wind_speeds,n_frequencies))
  yrange = [min(normsdd),max(normsdd)]

  ; plot individual frequencies with the average slope removed
  plot, wind_speed_sdd, normsdd[0,*], $
        xtitle='Wind speed (m.s!U-1!N)', $
        ytitle='Height variance mean difference (m!U2!N)', $
;        title='Ocean height variance wind speed spectra for different frequencies used in the!C'+$
;              'Low Frequency Microwave Sea Surface Emissivity model', $
;        title='Normalised ocean height variance ('+ztitle+') wind speed spectra', $
        yrange=yrange, $
;        ymargin=[4,4], $
        charsize=charsize,font=font,thick=thick, $
        /nodata
  for i=0,n_frequencies-1 do begin
;    color=(i+3)*18
;    color=(i+3)*32
    color = pcolor[i]
    oplot, wind_speed_sdd, normsdd[i,*], color=color,thick=thick
    if ( i eq 0 ) then begin
      lcolor=color
      ltxt=string(frequency_sdd[i],format='(f4.1,"GHz")')
    endif else begin
      lcolor=[lcolor,color]
      ltxt=[ltxt,string(frequency_sdd[i],format='(f4.1,"GHz")')]
    endelse
  endfor
  oplot, !x.crange,[0,0], thick=thick, linestyle=2
  
  mylegend, 0.075,0.42,ltxt,color=lcolor,thick=replicate(thick,n_frequencies), $
            font=font,charsize=lcharsize
  
  if ( keyword_set(ps) ) then psoff

;  tvlct, r, g, b
  !p=psave
  
end
