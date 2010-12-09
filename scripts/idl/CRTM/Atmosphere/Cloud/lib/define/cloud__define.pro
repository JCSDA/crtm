;+
; Cloud object definition procedure

PRO Cloud__Define

;-
  void = { Cloud, $
           Is_Allocated       : 0,  $
           n_Layers           : 0L, $
           Type               : 0L, $
           Effective_Radius   : LIST(), $
           Effective_Variance : LIST(), $
           Water_Content      : LIST()  }
END
