;+
;
; Procedure to replace a netCDF variable
;
PRO Replace_netCDF_Variable, NCfile , $  ; Input. File to modify.
                             VarName, $  ; Input. Name of variable to replace
                             VarData     ; Input. Variable data to put in file
;-

  IF ( NOT Valid_String(VarName) ) THEN RETURN
  
  NCid = NCDF_OPEN(NCfile,/WRITE)
  NCDF_VARPUT, NCid, VarName, VarData
  NCDF_CLOSE, NCid
END 
