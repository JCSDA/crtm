;+
;
; Procedure to set ("turn on") the solar channel flag for all channels 
; in an SpcCoeff netCDF file.
;
PRO Set_SpcCoeff_Solar, NCfile

  ; Set up
  ; ------
  RCS_Id = '$Id$'
  SOLAR_FLAG = 1L
  VarName = 'Channel_Flag'
  
  ; Update the channel flag array
  ; -----------------------------
  Get_netCDF_Variable, NCfile, VarName, VarData
  VarData = VarData OR SOLAR_FLAG
  Replace_netCDF_Variable, NCfile, VarName, VarData
  
  ; Update the global attributes
  ; ----------------------------
  Update_netCDF_Version, NCfile, /Increment, /Attribute
  Update_netCDF_Comment, NCfile, 'Solar flag set for all channels'
  Update_netCDF_History, NCfile, RCS_Id

END

