;+
;
; Procedure to rename a netCDF dimension
;
PRO Rename_netCDF_Dimension, NCfile    , $  ; Input. File to modify.
                             OldDimName, $  ; Input. Current dimension name
                             NewDimName     ; Input. New dimension name
;-

  IF ( (NOT Valid_String(OldDimName)) OR $
       (NOT Valid_String(NewDimName))    ) THEN RETURN
  
  NCid = NCDF_OPEN(NCfile,/WRITE)
  NCDF_CONTROL, NCid, /REDEF
  DimId = NCDF_DIMID( NCid, OldDimName )
  NCDF_DIMRENAME, NCid, DimId, NewDimName
  NCDF_CLOSE, NCid
END 
