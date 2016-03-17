FUNCTION dy, new, old
  RETURN, 100.0d0*(new - old)/old
END

PRO SpcCoeff_Difference_Display, $
  new_file, old_file, $
  Output=output

  result = read_netcdf(new_file, new, /global_attributes)
  result = read_netcdf(old_file, old)

  sensor_id = new.sensor_id
  n_channels = N_ELEMENTS(new.sensor_channel)

  color = 'black'  
  fill_color = 'red'  
  nx_plots = 2
  ny_plots = 3
  index = 1
  pidx=0
  ysize = 750
  IF ( n_channels GT 15 ) THEN BEGIN
    xtickvalues = []
    xminor      = -1
  ENDIF ELSE BEGIN
    xtickvalues = new.sensor_channel
    xminor      = 0
  ENDELSE
  
  p = OBJARR(6)
  w = WINDOW( DIMENSIONS   = [650,ysize], $
              BUFFER       = KEYWORD_SET(output), $
              WINDOW_TITLE = sensor_id + ' dSpcCoeff' )

  p[pidx++] = BARPLOT( new.sensor_channel, $
                       dy(new.planck_c1, old.planck_c1), $
                       TITLE       = 'First Planck coefficient', $
                       XTITLE      = 'Channel', $
                       YTITLE      = '$\Delta fc_{1} (%)$', $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )

  p[pidx++] = BARPLOT( new.sensor_channel, $
                       dy(new.planck_c2, old.planck_c2), $
                       TITLE       = 'Second Planck coefficient', $
                       XTITLE      = 'Channel', $
                       YTITLE      = '$\Delta fc_{2} (%)$', $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )

  p[pidx++] = BARPLOT( new.sensor_channel, $
                       dy(new.band_c1, old.band_c1), $
                       TITLE       = 'Band correction offset', $
                       XTITLE      = 'Channel', $
                       YTITLE      = '$\Delta bc_{1}$ (%)', $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )

  p[pidx++] = BARPLOT( new.sensor_channel, $
                       dy(new.band_c2, old.band_c2), $
                       TITLE       = 'Band correction slope', $
                       XTITLE      = 'Channel', $
                       YTITLE      = '$\Delta bc_{2}$ (%)', $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )

  IF ( new.sensor_type EQ 1 ) THEN BEGIN
    delta  = new.frequency - old.frequency
    ytitle = '$\Delta \nu (GHz)$'
  ENDIF ELSE BEGIN
    delta  = new.wavenumber - old.wavenumber
    ytitle = '$\Delta \nu (cm^{-1})$'
  ENDELSE
  p[pidx++] = BARPLOT( new.sensor_channel, $
                       delta, $
                       TITLE       = 'Central frequency', $
                       XTITLE      = 'Channel', $
                       YTITLE      = ytitle, $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )


  IF ( new.sensor_type EQ 1 ) THEN BEGIN
    delta  = dy(new.cosmic_background_radiance, old.cosmic_background_radiance)
    title  = 'Cosmic background'
    ytitle = '$\Delta R_{cosmic} (%)$'
  ENDIF ELSE BEGIN
    delta  = dy(new.solar_irradiance, old.solar_irradiance)
    title  = 'Solar irradiance'
    ytitle = '$\Delta I_{solar} (%)$'
  ENDELSE
  p[pidx++] = BARPLOT( new.sensor_channel, $
                       delta, $
                       TITLE       = title, $
                       XTITLE      = 'Channel', $
                       YTITLE      = ytitle, $
                       XTICKVALUES = xtickvalues, $
                       COLOR       = color, $
                       FILL_COLOR  = fill_color, $
                       XMINOR      = xminor, $
                       MARGIN      = [0.20,0.16,0.05,0.1], $
                       LAYOUT      = [nx_plots, ny_plots, index++], $
                       CURRENT     = w )

  IF ( KEYWORD_SET(output) ) THEN $
    w.Save, sensor_id+'.dSpcCoeff.png',HEIGHT=ysize

END
