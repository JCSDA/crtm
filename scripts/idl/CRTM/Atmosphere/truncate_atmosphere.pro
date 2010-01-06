;+
PRO Truncate_Atmosphere, Atm      , $  ; Input. Output from CRTM_Read_Atmosphere_Binary()
                         n_Layers , $  ; Input. No. of layers to truncate
                         Trunc_Atm     ; Output
;-

  ; Set up error handler
  @error_codes
  CATCH, Error_Status
  IF ( Error_Status NE 0 ) THEN BEGIN
    CATCH, /CANCEL
    MESSAGE, !ERROR_STATE.MSG, /CONTINUE
    RETURN
  ENDIF

  ; Only operate on non-channel dimensioned Atm input
  Info = SIZE(Atm, /STRUCTURE)
  IF ( Info.N_DIMENSIONS EQ 2 ) THEN $
    MESSAGE, 'Truncation not designed for adjoint/K-matrix Atm data.', /NONAME, /NOPRINT
  n_Profiles = Info.DIMENSIONS[0]

  ; Check if layer truncation specified for all atmospheres, or per atmosphere
  m = N_ELEMENTS( n_Layers )
  CASE m OF
    1: BEGIN
      n_Trunc_Layers = MAKE_ARRAY( n_Profiles, VALUE=n_Layers )
    END
    n_Profiles: BEGIN
      n_Trunc_Layers = n_Layers
    END
    ELSE: BEGIN
      MESSAGE, 'Size of n_Layers must be 1 or N_ELEMENTS(Atm).', /NONAME, /NOPRINT
    END
  ENDCASE

x=create_airs_pressure_levels(layer_pressure=p)

  ; Start copying over truncated data  
  Trunc_Atm = PTRARR(n_Profiles)
  FOR m = 0, n_Profiles-1 DO BEGIN
    
    Trunc_Atm[m] = PTR_NEW({CRTM_Atmosphere})

    n_k  = n_Trunc_Layers[m]
    n_kt = (*Atm[m]).n_Layers - n_k
    result = CRTM_Allocate_Atmosphere( n_kt                 , $  ; Input
                                       (*Atm[m]).n_Absorbers, $  ; Input
                                       (*Atm[m]).n_Clouds   , $  ; Input
                                       (*Atm[m]).n_Aerosols , $  ; Input
                                       *Trunc_Atm[m]          )  ; Output
                                       
    (*Trunc_Atm[m]).Climatology     = (*Atm[m]).Climatology
    *(*Trunc_Atm[m]).Absorber_Id    = *(*Atm[m]).Absorber_Id 
    *(*Trunc_Atm[m]).Absorber_Units = *(*Atm[m]).Absorber_Units
    *(*Trunc_Atm[m]).Level_Pressure = (*(*Atm[m]).Level_Pressure)[n_k:*]
    *(*Trunc_Atm[m]).Pressure       = (*(*Atm[m]).Pressure)[n_k:*]
    *(*Trunc_Atm[m]).Temperature    = (*(*Atm[m]).Temperature)[n_k:*]
    *(*Trunc_Atm[m]).Absorber       = (*(*Atm[m]).Absorber)[n_k:*,*]
    FOR i = 0, (*Trunc_Atm[m]).n_Clouds - 1 DO BEGIN
      (*(*Trunc_Atm[m]).Cloud[i]).Type                = (*(*Atm[m]).Cloud[i]).Type
      *(*(*Trunc_Atm[m]).Cloud[i]).Effective_Radius   = (*(*(*Atm[m]).Cloud[i]).Effective_Radius)[n_k:*]
      *(*(*Trunc_Atm[m]).Cloud[i]).Effective_Variance = (*(*(*Atm[m]).Cloud[i]).Effective_Variance)[n_k:*]
      *(*(*Trunc_Atm[m]).Cloud[i]).Water_Content      = (*(*(*Atm[m]).Cloud[i]).Water_Content)[n_k:*]
    ENDFOR
    FOR i = 0, (*Trunc_Atm[m]).n_Aerosols - 1 DO BEGIN
      (*(*Trunc_Atm[m]).Aerosol[i]).Type                = (*(*Atm[m]).Aerosol[i]).Type
      *(*(*Trunc_Atm[m]).Aerosol[i]).Effective_Radius   = (*(*(*Atm[m]).Aerosol[i]).Effective_Radius)[n_k:*]
      *(*(*Trunc_Atm[m]).Aerosol[i]).Concentration      = (*(*(*Atm[m]).Aerosol[i]).Concentration)[n_k:*]
    ENDFOR

print, (*(*Atm[m]).Level_Pressure)[0:n_k]
print, x[0:n_k]
q=get_kbrd(1)
  ENDFOR
    
  ; Done
  CATCH, /CANCEL

END
