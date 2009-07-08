;+
; NAME:
;       AtmProfile_File::Plot
;
; PURPOSE:
;       The AtmProfile_File::Plot procedure method displays all the
;       valid AtmProfile objects in the AtmProfile_File container.
;
; CALLING SEQUENCE:
;       Obj->[AtmProfile_File::]Plot, $
;         Debug=Debug  ; Input keyword
;
; INPUT KEYWORD PARAMETERS:
;       Debug:       Set this keyword for debugging.
;                    If NOT SET => Error handler is enabled. (DEFAULT)
;                       SET     => Error handler is disabled; Routine
;                                  traceback output is enabled.
;                    UNITS:      N/A
;                    TYPE:       INTEGER
;                    DIMENSION:  Scalar
;                    ATTRIBUTES: INTENT(IN), OPTIONAL
;
; INCLUDE FILES:
;       atmprofile_parameters: Include file containing AtmProfile specific
;                              parameter value definitions.
;
;       atmprofile_pro_err_handler: Error handler code for AtmProfile procedures.
;
; EXAMPLE:
;       Given an instance of a AtmProfile object,
;
;         IDL> HELP, x
;         X               OBJREF    = <ObjHeapVar8(AtmProfile_File)>
;
;       the data is plotted like so:
;
;         IDL> x->Plot
;
; CREATION HISTORY:
;       Written by:     Paul van Delst, 07-Jul-2009
;                       paul.vandelst@noaa.gov
;
;-

PRO AtmProfile_File::Plot, $
  Debug = Debug, $ ; Input keyword
  Absorber_Id = Absorber_Id, $
  Plot_Extra  = Plot_Extra, $
  OPlot_Extra = OPlot_Extra

  ; ...AtmProfile parameters
  @atmprofile_parameters
  ; ...Set up error handler
  @atmprofile_pro_err_handler

  
  ; Compute the maximum ranges
  max_range = self->Max_Range(Debug = Debug)
    
    
  ; Get the number of profiles
  self->Get_Property, $
    Debug = Debug, $
    n_Profiles = n_Profiles


  ; Plot the first profile to set the range
  root_p = self->Get(Profile = 1, Debug = Debug)
  root_p->Plot, $
    Debug = Debug, $
    /NODATA, $
    Absorber_Id = Absorber_Id, $
    MAX_RANGE = max_range, $
    _EXTRA = Plot_Extra


  ; Plot all the profiles
  FOR m = 1, n_Profiles DO BEGIN
    p = self->Get(Profile = m, Debug = Debug)
    root_p->OPlot, p, $
      Debug = Debug, $
      Absorber_Id = Absorber_Id, $
      _EXTRA = OPlot_Extra
  ENDFOR


  ; Done
  CATCH, /CANCEL
  
END ; PRO AtmProfile_File::Plot
