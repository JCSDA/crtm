PRO CatRestoreDefault, defaultName, FILENAME=filename, SUCCESS=success

   Compile_Opt idl2

   COMMON __$CatDefaults$_, defaultsObj

   ; Error handling.
   CATCH, theError
   IF (theError NE 0) THEN BEGIN
      void = Error_Message()
      success = 0
      RETURN
   ENDIF
   
   ; Create a filename if you need one.
   IF N_Elements(fileName) EQ 0 THEN filename = StrLowCase(IDL_ValidName(defaultname[0], /CONVERT_ALL)) + '.sav
   
   ; Does the file exist?
   IF File_Test(filename) EQ 0 THEN Message, 'The save file cannot be found: ' + filename 
   
   ; If no defaults container object exists, create one
   IF (~OBJ_VALID (defaultsObj)) THEN defaultsObj = OBJ_NEW ('CatList')
   
   ; Make sure the methods for the save objects are available.
   Resolve_Object, 'CATLISTVALUE'

   ; Restore the file. Variable is "saveCatDefaultContainer". 
   Restore, FILE=filename, /RELAXED_STRUCTURE_ASSIGNMENT
   
   ; How many objects are in the restored container?
   count = saveCatDefaultContainer -> Count()
   theObjects = saveCatDefaultContainer -> Get(/ALL)
   
   
   ; Get the names of the saved objects.
   names = StrArr(count)
   FOR j=0,count-1 DO names = theObjects[j] -> GetName()
   
   FOR j=0,N_Elements(defaultName)-1 DO BEGIN
       index = Where(StrUpCase(names) EQ StrUpCase(defaultName), cnt)
       IF cnt EQ 0 THEN CONTINUE
       defaultsObj -> Add, theObjects[j]
   ENDFOR

   success = 1
   
END