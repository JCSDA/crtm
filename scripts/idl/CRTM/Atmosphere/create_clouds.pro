PRO Create_Clouds, Atm, SWAP=Swap
 
  ; Read the Model Atmosphere datafile
  result = CRTM_Read_Atmosphere_Binary('Model.Atmosphere.bin', Model)
  
  ; Define cloud types
  Cloud_Type_Name = [ 'Water'  , $
                      'Ice'    , $
                      'Rain'   , $
                      'Snow'   , $
                      'Graupel', $
                      'Hail'     ]
  n_Clouds = N_ELEMENTS(Cloud_Type_Name)
  
  ; Setup a pointer array for loading the Atm data
  Atm = PTRARR(n_Clouds)
  a = {CRTM_Atmosphere}

  ; Define pressure layers
  x = Create_AIRS_Pressure_Levels(Layer_Pressure=p)
  n_Layers = N_ELEMENTS(p)
  
  ; Set up for plots
  psave=!P
  !P.MULTI=[0,2,1]
  yRange = [1100,0.01]
  yStyle = 1
  
  
  ; Create the cloud profiles
  ; -------------------------
  FOR n = 0, n_Clouds-1 DO BEGIN
    PRINT, 'Cloud type: ', Cloud_Type_Name[n]
  
    ; Default cloud parameters
    CASE n OF
      ; Water cloud
      0: BEGIN
           p0          = 700.0d0
           fwhm        = 100.0d0
           max_reff    = 20.0d0
           max_amount  = 5.0d0
           climatology = 1L  ; Tropical
         END
      ; Ice cloud
      1: BEGIN
           p0          = 325.0d0
           fwhm        = 200.0d0
           max_reff    = 500.0d0
           max_amount  = 2.0d0
           climatology = 4L  ; Subarctic summer
         END
      ; Rain cloud
      2: BEGIN
           p0          = 800.0d0
           fwhm        = 400.0d0
           max_reff    = 1000.0d0
           max_amount  = 5.0d0
           climatology = 6L  ; US Std Atm
         END
      ; Snow cloud
      3: BEGIN
           p0          = 400.0d0
           fwhm        = 200.0d0
           max_reff    = 500.0d0
           max_amount  = 1.0d0
           climatology = 3L  ; Midlatitude winter
         END
      ; Graupel cloud
      4: BEGIN
           p0          = 800.0d0
           fwhm        = 100.0d0
           max_reff    = 1000.0d0
           max_amount  = 3.0d0
           climatology = 5L  ; Subarctic winter
         END
      ; Hail cloud
      5: BEGIN
           p0          = 800.0d0
           fwhm        = 200.0d0
           max_reff    = 2000.0d0
           max_amount  = 2.0d0
           climatology = 2L  ; Midlatitude summer
         END
      ELSE:
    ENDCASE
  
    ; Create the profile data
    reff   = Gaussian(p0, fwhm, 2, p) * max_reff
    amount = Gaussian(p0, fwhm, 3, p) * max_amount

    ; Add the profiles to the Atm structure
    ; -------------------------------------
    m = climatology - 1
    Atm[m] = PTR_NEW({CRTM_Atmosphere})
    result = CRTM_Allocate_Atmosphere( n_Layers, 2, 1, 0, *Atm[m] )
    ; Copy over the data
    (*Atm[m]).Climatology      = climatology
    *(*Atm[m]).Absorber_ID     = [1L, 3L]
    *(*Atm[m]).Absorber_Units  = [3L, 1L]
    *(*Atm[m]).Level_Pressure  = *(*Model[m]).Level_Pressure
    *(*Atm[m]).Pressure        = *(*Model[m]).Pressure
    *(*Atm[m]).Temperature     = *(*Model[m]).Temperature
    (*(*Atm[m]).Absorber)[*,0] = (*(*Model[m]).Absorber)[*,0]  ; H2O
    (*(*Atm[m]).Absorber)[*,1] = (*(*Model[m]).Absorber)[*,2]  ; O3
    (*(*Atm[m]).Cloud).Type              = n+1
    *(*(*Atm[m]).Cloud).Effective_Radius = reff
    *(*(*Atm[m]).Cloud).Water_Content    = amount
  
    ; Plot the data
    PLOT, amount,p, $
          TITLE='Water content for '+Cloud_Type_Name[n]+' cloud', $
          XTITLE='Amount (kg/m!U2!N)', $
          YTITLE='Pressure (hPa)', $
          YRANGE=yRange,YSTYLE=ystyle
    PLOT, reff  ,p, $
          TITLE='Effective radius for '+Cloud_Type_Name[n]+' cloud', $
          XTITLE='R!Deff!N (microns)', $
          YTITLE='Pressure (hPa)', $
          YRANGE=yRange,YSTYLE=ystyle
    
    q = GET_KBRD(1)
    IF ( STRUPCASE(q) EQ 'Q' ) THEN BREAK
    
  ENDFOR

  ; Done
  !P=psave
  
  ; Write the data to file
  result = CRTM_Write_Atmosphere_Binary('Test.Atmosphere.Cloud.bin',Atm,SWAP=Swap,/Debug)
  
END
