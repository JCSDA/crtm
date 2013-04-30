PRO CatSaveDefault, defaultName, FILENAME=fileName, SUCCESS=success

   Compile_Opt idl2

   COMMON __$CatDefaults$_, defaultsObj

   ; Error handling.
   CATCH, theError
   IF (theError NE 0) THEN BEGIN
      void = Error_Message()
      success = 0
      RETURN
   ENDIF
   
   ; Is the defaults object here?
   IF N_Elements(defaultsObj) EQ 0 THEN Message, 'The CatDefaults object is missing.'
   
   ; If there is no defaultName, save everything.
   IF N_Elements(defaultName) EQ 0 THEN BEGIN
       defaultObjs = defaultsObj -> Get(/ALL, COUNT=numObjs)
       defaultName = StrArr(numObjs)
       FOR j=0,numObjs-1 DO defaultName[j] = defaultObjs[j] -> GetName()
   ENDIF

   ; Create a CatList object as a container for the defaults you wish to save.
   saveCatDefaultContainer = Obj_New('CatList')
   
   FOR j=0,N_Elements(defaultName)-1 DO BEGIN
   
       ; Attempt to retrieve the default from the defaults container
       valueObj = defaultsObj -> CatContainer::Get (defaultName[j], COUNT=count)
       IF count EQ 0 THEN Message, 'Cannot find a default with name: ' + defaultName
       saveCatDefaultContainer -> Add, valueObj
       
   ENDFOR
   
   ; Create a filename if you need one.
   IF N_Elements(fileName) EQ 0 THEN filename = StrLowCase(IDL_ValidName(defaultname[0], /CONVERT_ALL)) + '.sav
   
   ; Save the CatListValue object.
   Save, File=filename, saveCatDefaultContainer
   success = 1
   
   ; Destroy the local container after removing all the objects.
   objects = saveCatDefaultContainer -> Get(/ALL)
   Obj_Destroy, saveCatDefaultContainer
END
