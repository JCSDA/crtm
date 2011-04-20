; docformat = 'rst'

;+
; This class represents a information about .pro file.
; 
; :Properties:
;    basename : get, set, type=string
;       basename of filename
;    directory
;       directory tree object
;    system
;       system object
;
;-


;+
; Get variables for use with templates.
;
; :Returns: 
;    variable
;
; :Params:
;    name : in, required, type=string
;       name of variable
;
; :Keywords:
;    found : out, optional, type=boolean
;       set to a named variable, returns if variable name was found
;-
function doctreesavfile::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  switch strlowcase(name) of
    'basename' : return, self.basename
    'local_url' : begin
        self.system->getProperty, extension=outputExtension      
        return, file_basename(self.basename, '.sav') + '-sav.' + outputExtension
      end

    'output_path': begin      
         self.directory->getProperty, url=url     
         self.system->getProperty, extension=ext
         return, url + file_basename(self.basename, '.sav') + '-sav.' + ext
       end
             
    'creation_date': begin
        savFile = obj_new('IDL_Savefile', self.savFilename)
        contents = savFile->contents()
        obj_destroy, savFile
        return, contents.date
      end
    'modification_time': return, self.modificationTime

    'size': return, self.size
    'filename':
    'description': 
    'filetype': 
    'user':
    'host':
    'arch': 
    'os':   
    'release': 
    'n_common':
    'n_var':
    'n_sysvar':
    'n_procedure':
    'n_function':
    'n_object_heapvar':
    'n_pointer_heapvar':
    'n_structdef': begin
        savFile = obj_new('IDL_Savefile', self.savFilename)
        contents = savFile->contents()
        obj_destroy, savFile
        
        ind = where(strupcase(name) eq tag_names(contents))
        val = contents.(ind[0])
        return, mg_is_int(val) ? mg_int_format(val) : val
      end

    'procedures': return, self.procedures->get(/all)
    'functions': return, self.functions->get(/all)
    'variables': return, self.variables->get(/all)
    'system_variables': return, self.systemVariables->get(/all)
    'common_blocks': return, self.commonBlocks->get(/all)
    'structure_definitions': return, self.structureDefinitions->get(/all)
    'pointers': return, self.pointers->get(/all)
    'objects': return, self.objects->get(/all)
    
    'index_name': return, self.basename
    'index_type': begin
        location = self.directory->getVariable('location')
        
        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text='.sav file in ')
        link_node = obj_new('MGtmTag', type='link')
        link_node->addAttribute, 'reference', self.directory->getVariable('index_url')
        link_node->addChild, obj_new('MGtmText', text=location + ' directory')
        type_tree->addChild, link_node
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree

        return, comments
      end
    'index_url': begin
        return, self.directory->getVariable('url') + self->getVariable('local_url')
      end
    
    'has_comments': begin
        return, 0B
        break
      end
    'comments':
    'comments_first_line': begin
        return, ''
        break
      end
    
    else: begin
        ; search in the directory object if the variable is not found here
        var = self.directory->getVariable(name, found=found)
        if (found) then return, var
        
        found = 0B
        return, -1L
      end
  endswitch
end


;+
; Get properties.
;-
pro doctreesavfile::getProperty, basename=basename, directory=directory
  compile_opt strictarr, hidden
  
  if (arg_present(basename)) then basename = self.basename
  if (arg_present(directory)) then directory = self.directory
end


;+
; Restores the contents of the .sav file item specified by itemname.
; 
; :Returns: data of itemname
; 
; :Params:
;    itemname : in, required, type=string
; 
; :Keywords:
;    system_variable : in, optional, type=boolean
;       set to indicate itemname represents a system variable
;    structure_definition : in, optional, type=boolean
;       set to indicate itemname represents a structure definition
;    pointer_heapvar : in, optional, type=boolean
;       set to indicate itemname represents a pointer
;    object_heapvar : in, optional, type=boolean
;       set to indicate itemname represents an object
;-
function doctreesavfile::loadItem, itemName, $
                                   system_variable=systemVariable, $
                                   structure_definition=structureDefinition, $
                                   pointer_heapvar=pointerHeapvar, $
                                   object_heapvar=objectHeapvar
  compile_opt strictarr, hidden
  
  switch 1 of
    keyword_set(systemVariable): begin
        result = execute('temp = ' + itemName, 1, 1)
        
        savFile = obj_new('IDL_Savefile', self.savFilename)
        savFile->restore, itemName
        obj_destroy, savFile
        
        result = execute('var = ' + itemName, 1, 1)
        result = execute(itemName + ' = temp', 1, 1)

        return, var
      end
      
    keyword_set(structureDefinition): begin
        savFile = obj_new('IDL_Savefile', self.savFilename)
        savFile->restore, itemName, /structure_definition
        obj_destroy, savFile
        
        return, create_struct(name=itemName)
      end
      
    keyword_set(pointerHeapvar):
    keyword_set(objectHeapvar): begin
        savFile = obj_new('IDL_Savefile', self.savFilename)
        savFile->restore, itemName, new_heapvar=var, $
                               pointer_heapvar=pointerHeapvar, $
                               object_heapvar=objectHeapvar
        obj_destroy, savFile
        
        return, var           
      end
    
    else: begin
        savFile = obj_new('IDL_Savefile', self.savFilename)
        savFile->restore, itemName
        obj_destroy, savFile
        
        return, scope_varfetch(itemName)      
      end
    endswitch
end


;+
; Read contents of the .sav file.
;-
pro doctreesavfile::loadSavContents
  compile_opt strictarr, hidden
  
  savFile = obj_new('IDL_Savefile', self.savFilename)
  
  procedureNames = savFile->names(count=nProcedures, /procedure)
  if (nProcedures gt 0) then self.procedures->add, procedureNames
  
  functionNames = savFile->names(count=nFunctions, /function)
  if (nFunctions gt 0) then self.functions->add, functionNames
  
  varNames = savFile->names(count=nVars)
  for i = 0L, nVars - 1L do begin
    data = self->loadItem(varNames[i])
    
    var = obj_new('DOCtreeSavVar', varNames[i], data, self, system=self.system)
    self.variables->add, var
  endfor

  systemVariableNames = savFile->names(count=nSystemVariables, /system_variable)
  for i = 0L, nSystemVariables - 1L do begin
    data = self->loadItem(systemVariableNames[i], /system_variable)
  
    var = obj_new('DOCtreeSavVar', systemVariableNames[i], data, self, system=self.system)
    self.systemVariables->add, var
  endfor
  
  commonBlockNames = savFile->names(count=nCommonBlocks, /common_block)
  for i = 0L, nCommonBlocks - 1L do begin
    varNames = savFile->names(common_variable=commonBlockNames[i])
    
    var = obj_new('DOCtreeSavVar', commonBlockNames[i], '', self, system=self.system)
    var->setProperty, declaration='common ' + commonBlockNames[i] + ', ' + strjoin(varNames, ', ')
    self.commonBlocks->add, var
  endfor
  
  structureNames = savFile->names(count=nStructureDefinitions, /structure_definition)
  for i = 0L, nStructureDefinitions - 1L do begin
    data = self->loadItem(structureNames[i], /structure_definition)
    
    var = obj_new('DOCtreeSavVar', $
                  structureNames[i], $
                  data, self, system=self.system)
    self.structureDefinitions->add, var
  endfor
    
  pointerNames = savFile->names(count=nPointers, /pointer_heapvar)
  for i = 0L, nPointers - 1L do begin
    data = self->loadItem(pointerNames[i], /pointer_heapvar)
    
    var = obj_new('DOCtreeSavVar', $
                  'PtrHeapVar' + strtrim(pointerNames[i], 2), $
                  data, self, system=self.system)
    self.pointers->add, var
  endfor
  
  objectNames = savFile->names(count=nObjects, /object_heapvar)
  for i = 0L, nObjects - 1L do begin
    data = self->loadItem(objectNames[i], /object_heapvar)
    
    var = obj_new('DOCtreeSavVar', $
                  'ObjHeapVar' + strtrim(objectNames[i], 2), $
                  data, self, system=self.system)
    self.objects->add, var
  endfor
  
  obj_destroy, savFile
end


;+
; Set properties.
;-
pro doctreesavfile::setProperty
  compile_opt strictarr, hidden

end


;+
; All .sav files are visible.
; 
; :Returns: 
;    1 if visible, 0 if not visible
;-
function doctreesavfile::isVisible
  compile_opt strictarr, hidden
  
  return, self.directory->isVisible(/no_check_children)
end


;+
; Generate output documenting the .sav file.
;
; :Params:
;    outputRoot : in, required, type=string
;       absolute path to the output root directory
;    directory : in, required, type=string
;       directory name relative to the root for the .sav file
;-
pro doctreesavfile::generateOutput, outputRoot, directory
  compile_opt strictarr, hidden
  on_error, 2
  
  self.system->print, '  Generating output for ' + self.basename + '...'
  self.system->getProperty, extension=outputExtension
  
  self->loadSavContents
  
  savFileTemplate = self.system->getTemplate('savefile')
  
  outputDir = outputRoot + directory
  outputFilename = outputDir + file_basename(self.basename, '.sav') + '-sav.' + outputExtension
  
  savFileTemplate->reset
  savFileTemplate->process, self, outputFilename
end


;+
; Fill the links in comments for a save file.
;-
pro doctreesavfile::fillLinks
  compile_opt strictarr
  
  variables = self.variables->get(/all, count=nvariables)
  for i = 0L, nvariables - 1L do (variables[i])->fillLinks

  systemVariables = self.systemVariables->get(/all, count=nsystemVariables)
  for i = 0L, nsystemVariables - 1L do (systemVariables[i])->fillLinks

  commonBlocks = self.commonBlocks->get(/all, count=ncommonBlocks)
  for i = 0L, ncommonBlocks - 1L do (commonBlocks[i])->fillLinks

  structureDefinitions = self.structureDefinitions->get(/all, count=nstructureDefinitions)
  for i = 0L, nstructureDefinitions - 1L do (structureDefinitions[i])->fillLinks

  pointers = self.pointers->get(/all, count=npointers)
  for i = 0L, npointers - 1L do (pointers[i])->fillLinks

  objects = self.objects->get(/all, count=nobjects)
  for i = 0L, nobjects - 1L do (objects[i])->fillLinks
end


;+
; Return an URL from the root for the given item name.
; 
; :Returns:
;    string
;    
; :Params:
;    name : in, required, type=string
;       name of item
;   
; :Keywords:
;    exclude : in, optional, type=object
;       object to exclude looking at
;-
function doctreesavfile::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (name eq self.basename) then return, self->getVariable('index_url')
  
  variables = self.variables->get(/all, count=nvariables)
  for i = 0L, nvariables - 1L do begin
    if (obj_valid(exclude) && exclude eq variables[i]) then continue
    url = (variables[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor

  systemVariables = self.systemVariables->get(/all, count=nsystemVariables)
  for i = 0L, nsystemVariables - 1L do begin
    if (obj_valid(exclude) && exclude eq systemVariables[i]) then continue
    url = (systemVariables[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor

  commonBlocks = self.commonBlocks->get(/all, count=ncommonBlocks)
  for i = 0L, ncommonBlocks - 1L do begin
    if (obj_valid(exclude) && exclude eq commonBlocks[i]) then continue
    url = (commonBlocks[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor

  structureDefinitions = self.structureDefinitions->get(/all, count=nstructureDefinitions)
  for i = 0L, nstructureDefinitions - 1L do begin
    if (obj_valid(exclude) && exclude eq structureDefinitions[i]) then continue
    url = (structureDefinitions[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor

  pointers = self.pointers->get(/all, count=npointers)
  for i = 0L, npointers - 1L do begin
    if (obj_valid(exclude) && exclude eq pointers[i]) then continue
    url = (pointers[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor

  objects = self.objects->get(/all, count=nobjects)
  for i = 0L, nobjects - 1L do begin
    if (obj_valid(exclude) && exclude eq objects[i]) then continue
    url = (objects[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  return, obj_valid(exclude) && exclude eq self.directory $
            ? '' $
            : self.directory->lookupName(name, exclude=self)
end


;+
; Free resources.
;-
pro doctreesavfile::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, [self.procedures, $
                self.functions, $
                self.variables, $
                self.systemVariables, $
                self.commonBlocks, $
                self.structureDefinitions, $
                self.pointers, $
                self.objects]
end


;+
; Create file tree object.
;
; :Returns: 
;    1 for success, 0 for failure
;-
function doctreesavfile::init, basename=basename, directory=directory, $
                               system=system
  compile_opt strictarr, hidden
  
  self.basename = basename
  self.directory = directory
  self.system = system
  
  self.system->getProperty, root=root
  self.directory->getProperty, location=location
  self.savFilename = root + location + self.basename
  
  info = file_info(self.savFilename)
  self.modificationTime = systime(0, info.mtime)
  self.size = mg_int_format(info.size) + ' bytes'

  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 1L) then self.system->createIndexEntry, self.basename, self
  
  self.system->print, '  Parsing ' + self.basename + '...'
  
  self.procedures = obj_new('MGcoArrayList', type=7, block_size=20)
  self.functions = obj_new('MGcoArrayList', type=7, block_size=20)
  self.variables = obj_new('MGcoArrayList', type=11, block_size=10)
  self.systemVariables = obj_new('MGcoArrayList', type=11, block_size=20)
  self.commonBlocks = obj_new('MGcoArrayList', type=11, block_size=5)
  self.structureDefinitions = obj_new('MGcoArrayList', type=11, block_size=5)
  self.pointers = obj_new('MGcoArrayList', type=11, block_size=10)
  self.objects = obj_new('MGcoArrayList', type=11, block_size=10)
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    system
;       system object
;    directory
;       directory tree object
;    basename
;       basename of file
;    savFilename
;       absolute filename of the .sav file
;    modificationTime
;       formatted string representing time/date of last modification time of the
;       .sav file
;    size
;       formatted size of .sav file with units
;    procedures
;       array list of procedure names
;    functions
;       array list of function names
;    systemVariables
;       array list of sav variable tree objects
;    commonBlocks
;       array list of sav variable tree objects
;    structureDefinitions
;       array list of sav variable tree objects
;    pointers
;       array list of sav variable tree objects
;    objects
;       array list of sav variables tree objects
;-
pro doctreesavfile__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeSavFile, $
             system: obj_new(), $
             directory: obj_new(), $
             
             basename: '', $
             
             savFilename: '', $
             modificationTime: '', $
             size: '', $
             
             procedures: obj_new(), $
             functions: obj_new(), $
             variables: obj_new(), $
             systemVariables: obj_new(), $
             commonBlocks: obj_new(), $
             structureDefinitions: obj_new(), $
             pointers: obj_new(), $
             objects: obj_new() $
           }
end