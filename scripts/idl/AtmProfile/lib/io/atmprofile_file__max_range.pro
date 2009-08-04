FUNCTION AtmProfile_File::Max_Range, $
  Debug = Debug ; Input keyword

  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_func_err_handler

  
  ; Get the profile set dimensions
  self->Get_Property, $
    Debug = Debug, $
    n_Absorbers = n_Absorbers, $
    n_Profiles  = n_Profiles


  ; Initialise the ranges
  max_range = {t:DBLARR(2), a:DBLARR(2,n_Absorbers)}
  max_range.t = [1.0d+10,-1.0d+10] 
  max_range.a[0,*] =  1.0d+10
  max_range.a[1,*] = -1.0d+10
  

  ; Get the object references
  p = self->Get(/ALL, ISA='AtmProfile', Debug = Debug)

    
  ; Loop over profiles
  FOR m = 0L, n_Profiles-1L DO BEGIN
    ; Extract the profile data
    p[m]->Get_Property, $
      Debug = Debug, $
      n_Absorbers = n_Absorbers, $
      Absorber_Id = Absorber_Id, $
      Level_Temperature = t, $
      Level_Absorber    = a
    ; Determine the ranges
    max_range.t[0] = max_range.t[0] < MIN(t)
    max_range.t[1] = max_range.t[1] > MAX(t)
    FOR j = 0, n_Absorbers-1 DO BEGIN
      max_range.a[0,j] = max_range.a[0,j] < MIN(a[*,j])
      max_range.a[1,j] = max_range.a[1,j] > MAX(a[*,j])      
    ENDFOR
  ENDFOR


  ; Done
  CATCH, /CANCEL
  RETURN, max_range
  
END ; FUNCTION AtmProfile_File::Max_Range
