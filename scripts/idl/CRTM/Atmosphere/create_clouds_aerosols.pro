;+
PRO Create_Clouds_Aerosols, Atm      , $ ; Output
                            SWAP=Swap    ; Input keyword
;-
 
  ; Read the Model Atmosphere datafile
  result = CRTM_Read_Atmosphere_Binary('Model.Atmosphere.bin', Model)
  n_Profiles = N_ELEMENTS(Model)
  
  ; Define cloud and aerosol types
  Cloud_Type_Name = [ 'Water'  , $
                      'Ice'    , $
                      'Rain'   , $
                      'Snow'   , $
                      'Graupel', $
                      'Hail'     ]
  Cloud_Type = LINDGEN(n_Profiles) + 1L
  Aerosol_Type_Name = [ 'Dust'              , $
                        'Sea salt (SSAM)'   , $
                        'Dry organic carbon', $
                        'Wet organic carbon', $
                        'Sea salt (SSCM)'  , $
                        'Sulfate'             ]
  Aerosol_Type = [ 1, 2, 4, 5, 3, 8 ]
  
  ; Setup a pointer array for loading the Atm data
  Atm = PTRARR(n_Profiles)
  a = {CRTM_Atmosphere}

  ; Define pressure layers
  x = Create_AIRS_Pressure_Levels(Layer_Pressure=p)
  n_Layers = N_ELEMENTS(p)
  
  ; Set up for plots
  psave=!P
  xsave=!X
  !P.MULTI=[0,2,2]
  yRange = [1100,0.01]
  yStyle = 1
  font     = (!D.NAME EQ 'PS') ? 1   : -1
  thick    = (!D.NAME EQ 'PS') ? 2.0 : 1.0
  charsize = (!D.NAME EQ 'PS') ? 1.5 : 1.0
  xmargin  = (!D.NAME EQ 'PS') ? [7,3] : !X.MARGIN
  
  ; Create the cloud and aerosol profiles
  ; -------------------------------------
  FOR n = 0, n_Profiles-1 DO BEGIN
    PRINT, 'Cloud   type: ', Cloud_Type_Name[n]
    PRINT, 'Aerosol type: ', Aerosol_Type_Name[n]
  
    CASE n OF
      ; Water cloud and dust
      0: BEGIN
           cld_p0          = 700.0d0
           cld_fwhm        = 100.0d0
           cld_max_reff    = 20.0d0
           cld_max_amount  = 5.0d0
           aer_p0          = 750.0d0
           aer_fwhm        = 200.0d0
           aer_max_reff    = 2.0d0
           aer_max_amount  = 2.0d0
           climatology = 1L  ; Tropical
         END
      ; Ice cloud and sea salt (SSAM)
      1: BEGIN
           cld_p0          = 325.0d0
           cld_fwhm        = 200.0d0
           cld_max_reff    = 500.0d0
           cld_max_amount  = 2.0d0
           aer_p0          = 900.0d0
           aer_fwhm        = 400.0d0
           aer_max_reff    = 1.5d0
           aer_max_amount  = 1.0d0
           climatology = 4L  ; Subarctic summer
         END
      ; Rain cloud and dry organic carbon
      2: BEGIN
           cld_p0          = 800.0d0
           cld_fwhm        = 400.0d0
           cld_max_reff    = 1000.0d0
           cld_max_amount  = 5.0d0
           aer_p0          = [800.0d0,250.0d0]
           aer_fwhm        = [200.0d0,100.0d0]
           aer_max_reff    = [0.15d0,0.09d0]
           aer_max_amount  = [0.06d0,0.03d0]
           climatology = 6L  ; US Std Atm
         END
      ; Snow cloud and wet organic carbon
      3: BEGIN
           cld_p0          = 400.0d0
           cld_fwhm        = 200.0d0
           cld_max_reff    = 500.0d0
           cld_max_amount  = 1.0d0
           aer_p0          = [800.0d0,250.0d0]
           aer_fwhm        = [200.0d0,150.0d0]
           aer_max_reff    = [0.15d0,0.09d0]
           aer_max_amount  = [0.4d0,0.2d0]
           climatology = 3L  ; Midlatitude winter
         END
      ; Graupel cloud and sea salt (SSCM)
      4: BEGIN
           cld_p0          = 800.0d0
           cld_fwhm        = 100.0d0
           cld_max_reff    = 1000.0d0
           cld_max_amount  = 3.0d0
           aer_p0          = 1000.0d0
           aer_fwhm        = 200.0d0
           aer_max_reff    = 12.0d0
           aer_max_amount  = 0.05d0
           climatology = 5L  ; Subarctic winter
         END
      ; Hail cloud and sulfate
      5: BEGIN
           cld_p0          = 800.0d0
           cld_fwhm        = 200.0d0
           cld_max_reff    = 2000.0d0
           cld_max_amount  = 2.0d0
           aer_p0          = [875.0d0,600.0d0,200.0d0]
           aer_fwhm        = [150.0d0,200.0d0,100.0d0]
           aer_max_reff    = [0.7d0,0.45d0,0.3d0]
           aer_max_amount  = [0.125d0,0.05d0,0.03d0]
           climatology = 2L  ; Midlatitude summer
         END
      ELSE:
    ENDCASE
  
    ; Create the profile data
    cld_reff   = Gaussian(cld_p0, cld_fwhm, 2, p) * cld_max_reff
    cld_amount = Gaussian(cld_p0, cld_fwhm, 3, p) * cld_max_amount
    aer_reff   = DBLARR(n_Layers)
    aer_amount = DBLARR(n_Layers)
    FOR i = 0, N_ELEMENTS(aer_p0) - 1 DO BEGIN
      aer_reff   = aer_reff   + (Gaussian(aer_p0[i], aer_fwhm[i], 2, p) * aer_max_reff[i])
      aer_amount = aer_amount + (Gaussian(aer_p0[i], aer_fwhm[i], 3, p) * aer_max_amount[i] * 1.0d-03)
    ENDFOR

    ; Add the profiles to the Atm structure
    ; -------------------------------------
    m = climatology - 1
    Atm[m] = PTR_NEW({CRTM_Atmosphere})
    result = CRTM_Allocate_Atmosphere( n_Layers, 2, 1, 1, *Atm[m] )
    ; Copy over the data
    (*Atm[m]).Climatology      = climatology
    *(*Atm[m]).Absorber_ID     = [1L, 3L]
    *(*Atm[m]).Absorber_Units  = [3L, 1L]
    *(*Atm[m]).Level_Pressure  = *(*Model[m]).Level_Pressure
    *(*Atm[m]).Pressure        = *(*Model[m]).Pressure
    *(*Atm[m]).Temperature     = *(*Model[m]).Temperature
    (*(*Atm[m]).Absorber)[*,0] = (*(*Model[m]).Absorber)[*,0]  ; H2O
    (*(*Atm[m]).Absorber)[*,1] = (*(*Model[m]).Absorber)[*,2]  ; O3
    (*(*Atm[m]).Cloud).Type              = Cloud_Type[n]
    *(*(*Atm[m]).Cloud).Effective_Radius = cld_reff
    *(*(*Atm[m]).Cloud).Water_Content    = cld_amount
    (*(*Atm[m]).Aerosol).Type              = Aerosol_Type[n]
    *(*(*Atm[m]).Aerosol).Effective_Radius = aer_reff
    *(*(*Atm[m]).Aerosol).Concentration    = aer_amount
  
    ; Plot the data
    xp = 0.875
    yp = 0.9
    PLOT, cld_amount,p, $
          TITLE='Water content for '+Cloud_Type_Name[n]+' cloud', $
          XTITLE='Amount (kg/m!U2!N)', $
          YTITLE='Pressure (hPa)', $
          XMARGIN=xmargin, $
          YRANGE=yRange,YSTYLE=ystyle, $
          FONT=font,CHARSIZE=charsize,THICK=thick
    myxyouts, xp,yp,'(a1)', $
              FONT=font,CHARSIZE=charsize
    PLOT, cld_reff  ,p, $
          TITLE='Effective radius for '+Cloud_Type_Name[n]+' cloud', $
          XTITLE='R!Deff!N (microns)', $
          YTITLE='Pressure (hPa)', $
          XMARGIN=xmargin, $
          YRANGE=yRange,YSTYLE=ystyle, $
          FONT=font,CHARSIZE=charsize,THICK=thick
    myxyouts, xp,yp,'(a2)', $
              FONT=font,CHARSIZE=charsize
    axis_scale, [0,MAX(aer_amount)], 'Amount (kg/m!U2!N)', $
                xfactor, xtitle
    PLOT, aer_amount*xfactor,p, $
          TITLE='Concentration for '+Aerosol_Type_Name[n]+' aerosol', $
          XTITLE=xtitle, $
          YTITLE='Pressure (hPa)', $
          XMARGIN=xmargin, $
          YRANGE=yRange,YSTYLE=ystyle, $
          FONT=font,CHARSIZE=charsize,THICK=thick
    myxyouts, xp,yp,'(b1)', $
              FONT=font,CHARSIZE=charsize
    PLOT, aer_reff  ,p, $
          TITLE='Effective radius for '+Aerosol_Type_Name[n]+' aerosol', $
          XTITLE='R!Deff!N (microns)', $
          YTITLE='Pressure (hPa)', $
          XMARGIN=xmargin, $
          YRANGE=yRange,YSTYLE=ystyle, $
          FONT=font,CHARSIZE=charsize,THICK=thick
    myxyouts, xp,yp,'(b2)', $
              FONT=font,CHARSIZE=charsize
    
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    
  ENDFOR

  ; Done
  !P=psave
  !X=xsave
  
  ; Write the data to file
  result = CRTM_Write_Atmosphere_Binary('Test.Atmosphere.Cloud.Aerosol.bin',Atm,SWAP=Swap,/Debug)
  
END
