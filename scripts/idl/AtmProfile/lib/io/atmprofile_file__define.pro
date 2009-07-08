;+
; AtmProfile file object definition procedure

PRO AtmProfile_File__Define

;-

  @atmprofile_parameters
  
  void = { AtmProfile_File, $
           ; File information
           filename: '', $
           Release : 0L, $  ; Release: this identified structure and file format
           Version : 0L, $  ; Version: this is just the data version
           ; Dimension data
           n_Levels    : 0L, $  ; Number of levels dimensions,   K+1
           n_Layers    : 0L, $  ; Number of layers dimension,    K
           n_Absorbers : 0L, $  ; Number of absorbers dimension, J
           n_Profiles  : 0L, $  ; Number of profiles dimension,  M (UNLIMITED)
           ; Profile independent data
           Profile_Set_Id : '', $  ; Profile set identifier (or "tag")
           ; Global attributes
           Title            : '', $  ; File data title
           History          : '', $  ; File data history
           Comment          : '', $  ; File data comment
           ; Container for AtmProfile objects
           INHERITS IDL_Container }

END ; PRO AtmProfile_File__Define
