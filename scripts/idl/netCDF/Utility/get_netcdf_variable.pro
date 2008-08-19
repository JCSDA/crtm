;+
;
; Procedure to get a netCDF variable from file
;
PRO Get_netCDF_Variable, NCfile , $  ; Input.  File containing variable.
                         VarName, $  ; Input.  Name of variable to get
                         VarData     ; Output. Variable data from file
;-

  IF ( NOT Valid_String(VarName) ) THEN RETURN
  
  NCid = NCDF_OPEN(NCfile)
  NCDF_VARGET, NCid, VarName, VarData
  NCDF_CLOSE, NCid
END 
