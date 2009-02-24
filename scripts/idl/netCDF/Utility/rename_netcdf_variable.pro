;+
;
; Procedure to rename a netCDF variable
;
PRO Rename_netCDF_Variable, NCfile    , $  ; Input. File to modify.
                            OldVarName, $  ; Input. Current variable name
                            NewVarName     ; Input. New variable name
;-

  IF ( (NOT Valid_String(OldVarName)) OR $
       (NOT Valid_String(NewVarName))    ) THEN RETURN
  
  NCid = NCDF_OPEN(NCfile,/WRITE)
  NCDF_CONTROL, NCid, /REDEF
  VarId = NCDF_VARID( NCid, OldVarName )
  NCDF_VARRENAME, NCid, VarId, NewVarName
  NCDF_CLOSE, NCid
END 
