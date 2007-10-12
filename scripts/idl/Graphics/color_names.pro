PRO Color_Names
  @color_db
  FOR i = 0, N_ELEMENTS(COLOR_NAMES)-1 DO BEGIN
    PRINT, FORMAT='("Index ",i2," => ",a)', i, COLOR_NAMES[i]
  ENDFOR
END
