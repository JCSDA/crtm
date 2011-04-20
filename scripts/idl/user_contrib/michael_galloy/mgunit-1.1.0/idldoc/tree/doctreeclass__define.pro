; docformat = 'rst'

;+
; Tree object representing a class.
;
; :Properties:
;    ancestors
;       ancestor classes
;    classname
;       name of the class
;    pro_file
;       name of the pro_file where the class is defined (where the __define 
;       routine is) 
;    properties
;       hash table of properties (name -> property tree object)
;    system
;       system object
;-

;+
; Get variables for use with templates.
;
; :Returns: 
;    variable value
;    
; :Params:
;    name : in, required, type=string
;       name of variable
;
; :Keywords:
;    found : out, optional, type=boolean
;       set to a named variable, returns if variable name was found
;-
function doctreeclass::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  case strlowcase(name) of
    'classname': return, self.classname
    'has_url': return, obj_valid(self.proFile)              
    'url': begin
        if (~obj_valid(self.proFile)) then return, ''
        
        self.proFile->getProperty, directory=directory
        dirUrl = directory->getVariable('url')
        proUrl = self.proFile->getVariable('local_url')
        return, dirUrl + proUrl
      end
      
    'n_parents': return, self.parents->count()
    'parents': return, self.parents->get(/all)
          
    'n_ancestors': return, self.ancestors->count()
    'ancestors': return, self.ancestors->get(/all)

    'n_children': return, self.children->count()
    'children': return, self.children->get(/all)
    
    'n_fields': return, self.fields->count()
    'fields': return, self.fields->values()
    'field_names': return, self->getFieldNames()
    
    'n_properties': return, self.properties->count()
    'properties': return, self.properties->values()

    'n_visible_properties': begin
        nVisible = 0
        values = self.properties->values()
        for p = 0L, self.properties->count() - 1L do begin
          prop = values[p]
          nVisible += prop->isVisible()
        endfor
        
        return, nVisible
      end
    'visible_properties': begin
        properties = self.properties->values(count=nProperties)
        if (nProperties eq 0L) then return, -1L
        
        isVisibleProperties = bytarr(nProperties)
        for p = 0L, nProperties - 1L do begin
          isVisibleProperties[p] = properties[p]->isVisible()
        endfor
        
        ind = where(isVisibleProperties eq 1B, nVisibleProperties)
        if (nVisibleProperties eq 0L) then return, -1L
        
        return, properties[ind]
      end         
             
    'index_name': return, self.classname
    'index_type': return, 'class'
    'index_url': return, self->getVariable('url')
    
    'has_comments': return, 0B
    'comments': return, self.system->processComments(obj_new())   
    'comments_first_line': return, ''
    
    else: begin
        ; search in the system object if the variable is not found here
        var = self.proFile->getVariable(name, found=found)
        if (found) then return, var
        
        found = 0B
        return, -1L
      end    
  endcase
end


;+
; Easy to use accessor for classname.
;
; :Returns: 
;    string
;-
function doctreeclass::getClassname
  compile_opt strictarr, hidden
  
  return, self.classname
end


;+
; Easy to use accessor for whether the class has an URL.
; 
; :Returns: boolean
;-
function doctreeclass::hasUrl
  compile_opt strictarr, hidden
  
  return, obj_valid(self.proFile)
end


;+
; Easy to use accessor for URL to class file relative to doc root.
;
; :Returns: 
;    string
;-
function doctreeclass::getUrl
  compile_opt strictarr, hidden
  
  if (~obj_valid(self.proFile)) then return, ''
  
  self.proFile->getProperty, directory=directory
  dirUrl = directory->getVariable('url')
  proUrl = self.proFile->getVariable('local_url')
  return, dirUrl + proUrl  
end


;+
; Easy to use accessor for number of fields.
;
; :Returns: 
;    strarr or string
;-
function doctreeclass::getFieldCount
  compile_opt strictarr, hidden
  
  return, self.fields->count()
end


;+
; Easy to use accessor for field names.
;
; :Returns: 
;    strarr or string
;-
function doctreeclass::getFieldNames
  compile_opt strictarr, hidden
  
  nFields = self.fields->count()
  if (nFields eq 0) then return, ''
  
  fieldNames = strarr(nFields)
  fields = self.fields->values()
  for f = 0L, nFields - 1L do begin
    fields[f]->getProperty, name=name
    fieldNames[f] = name
  endfor
  
  return, fieldNames
end


;+
; Easy to use accessor for field types.
;
; :Returns: 
;    strarr or string
;-
function doctreeclass::getFieldTypes
  compile_opt strictarr, hidden
  
  nFields = self.fields->count()
  if (nFields eq 0) then return, ''
  
  fieldTypes = strarr(nFields)
  fields = self.fields->values()
  for f = 0L, nFields - 1L do begin
    fields[f]->getProperty, type=type
    fieldTypes[f] = type
  endfor
  
  return, fieldTypes
end


;+
; Set properties.
;-
pro doctreeclass::setProperty, pro_file=proFile, classname=classname
  compile_opt strictarr, hidden
  
  if (n_elements(proFile) gt 0) then self.proFile = proFile
  if (n_elements(classname) gt 0) then self.classname = classname
end


;+
; Get properties.
;-
pro doctreeclass::getProperty, ancestors=ancestors, classname=classname, $
                               properties=properties, pro_file=proFile
  compile_opt strictarr, hidden

  if (arg_present(ancestors)) then ancestors = self.ancestors
  if (arg_present(classname)) then classname = self.classname
  if (arg_present(properties)) then properties = self.properties
  if (arg_present(proFile)) then proFile = self.proFile
end


;+
; Add child class for this class.
;
; :Params:
;    child : in, required, type=object
;       class tree object
;-
pro doctreeclass::addChild, child
  compile_opt strictarr, hidden
  
  self.children->add, child
end


;+
; Classes are visible if their files are visible.
;
; :Returns: 
;    1 if visible, 0 if not visible
;-
function doctreeclass::isVisible
  compile_opt strictarr, hidden
  
  return, obj_valid(self.proFile) ? self.proFile->isVisible() : 1B
end


;+
; Indicates if the class definition for this class is in the documented library.
;
; :Returns:
;    1 if in library, 0 if not
;-
function doctreeclass::isInLibrary
  compile_opt strictarr

  return, obj_valid(self.proFile)
end


;+
; Compile a given procedure. This is in a separate routine because if it's done
; directly in _createClassStructure, then errors like::
; 
;    % CATCH: Unexpected keyword cleanup stack found on return.
;
; are generated on each call to _createClassStructure.
;
; :Params:
;    routineName : in, required, type=string
;       name of procedure to compile
;-
pro doctreeclass::_compileRoutine, routineName
  compile_opt strictarr, hidden
  
  error = 0L
  catch, error
  if (error ne 0L) then return
  
  resolve_routine, routineName
end


;+
; Pulled RESOLVE_ROUTINE out of _createClassStructure routine so that if 
; routineName is not found, it doesn't generate an error that is caught by
; the CATCH statement in _createClassStructure (which is reserved for the 
; error of an invalid structure definition attempt).
;
; :Params:
;    routineName : in, required, type=string
;       name of the procedure to compile
;-
pro doctreeclass::_compileFile, routineName
  compile_opt strictarr, hidden
  
  error = 0L
  catch, error
  if (error ne 0L) then return
  resolve_routine, routineName, /either, /compile_full_file
end


;+
; Create a structure containing the fields of the class.
;
; :Returns: 
;    structure
;
; :Params:
;    classname : in, required, type=string
;       name of the named structure i.e. the classname
;
; :Keywords:
;    error : out, optional, type=long
;       set to a named variable to contain any error code; 0 indicates no error
;    compile_file : in, optional, type=boolean
;       set to compile .pro file before trying to define structure
;-
function doctreeclass::_createClassStructure, classname, error=error, $
                                              compile_file=compileFile
  compile_opt strictarr, hidden
  
  error = 0L
  catch, error
  if (error ne 0L) then begin
    error = 1L
    return, -1L
  endif
    
  if (keyword_set(compileFile)) then begin
    self.proFile->getProperty, basename=basename
    self->_compileFile, file_basename(basename, '.pro')
  endif
    
  s = create_struct(name=classname)
  return, s
end


;+
; Find parent classes for class and figure out where each field was defined.
;-
pro doctreeclass::findParents
  compile_opt strictarr, hidden
  
  ; get all fields defined in class
  s = self->_createClassStructure(self.classname, error=error)
  if (error ne 0L) then begin
    s = self->_createClassStructure(self.classname, error=error, /compile_file)
    if (error ne 0L) then begin
      self.system->warning, 'cannot construct definition for class ' $
                              + self.classname
      return
    endif
  endif
    
  ; get direct parent classes
  parents = obj_class(self.classname, /superclass)
  nParents = parents[0] eq '' ? 0 : n_elements(parents)  
  
  ; this list will contain the names of fields of ancestor classes, any fields
  ; in s that are not in ancestorFieldNameList then are defined in this class
  ancestorFieldNameList = obj_new('MGcoArrayList', type=7, block_size=10)
  
  for i = 0L, nParents - 1L do begin
    ; lookup parent class in system class hash table
    p = self.classes->get(strlowcase(parents[i]), found=found)
    if (~found) then begin
      p = obj_new('DOCtreeClass', parents[i], system=self.system)
      self.classes->put, strlowcase(parents[i]), p
    endif

    ; connect classes
    p->addChild, self
    self.parents->add, p
    self.ancestors->add, p
    
    ; ancestors of parents of this class are ancestors of this class 
    p->getProperty, ancestors=ancestors
    if (ancestors->count() gt 0) then begin
      self.ancestors->add, ancestors->get(/all)
    endif
  endfor

  for a = 0L, self.ancestors->count() - 1L do begin
    anc = self.ancestors->get(position=a)
    
    ; add all the fields of the ancestor class to the ancestorFieldNameList
    ancestorFieldNameList->add, anc.fields->keys()
  endfor
  
  ancestorFieldNames = ancestorFieldNameList->get(/all, count=nAncestorFieldNames)
  fieldNames = tag_names(s)

  for f = 0L, n_tags(s) - 1L do begin  
    if (nAncestorFieldNames ne 0) then begin
      ind = where(strlowcase(fieldNames[f]) eq ancestorFieldNames, nMatches)
    endif
    if (nAncestorFieldNames eq 0 || nMatches eq 0) then begin
      field = self->addField(fieldNames[f])
      field->setProperty, type=mg_variable_declaration(s.(f))
    endif
  endfor  
  
  ; don't need the array list object any more
  obj_destroy, ancestorFieldNameList
end


;+
; Add a field to the class.
; 
; :Returns: 
;    field tree object
;
; :Params:
;    fieldname : in, required, type=string
;       name of the field to create and add to the class
; 
; :Keywords:
;    get_only : in, optional, type=boolean
;       if set, do not create a field tree object; just return an exising field
;-
function doctreeclass::addField, fieldName, get_only=getOnly
  compile_opt strictarr, hidden
  
  field = self.fields->get(strlowcase(fieldName), found=found)
  if (~found && ~keyword_set(getOnly)) then begin
    field = obj_new('DOCtreeField', fieldName, $
                    class=self, system=self.system)
    self.fields->put, strlowcase(fieldName), field
  endif
  return, field
end


;+
; Adds the given property to this class.
;
; :Params:
;    property : in, out, required, type=object
;       property tree object to add
;
; :Keywords:
;    property_name : in, out, optional, type=string
;       name of the property
;-
pro doctreeclass::addProperty, property, property_name=propertyName
  compile_opt strictarr, hidden
  
  if (n_elements(propertyName) ne 0) then begin
    property = self.properties->get(strlowcase(propertyName), found=found)
    if (~found) then begin
      property = obj_new('DOCtreeProperty', propertyName, system=self.system)
      self.properties->put, strlowcase(propertyName), property
    endif
    property->setProperty, class=self
  endif

  property->getProperty, name=propertyName
      
  ; check before adding property, this works around an IDL bug
  prop = self.properties->get(strlowcase(propertyName), found=found)
  if (~found) then begin
    property->setProperty, class=self
    self.properties->put, strlowcase(propertyName), property
  endif else begin
    if (prop ne property) then obj_destroy, property
  endelse
end


;+
; Fill the links in comments for a class.
;-
pro doctreeclass::fillLinks
  compile_opt strictarr
  
  fields = self.fields->values(count=nfields)
  for f = 0L, nfields - 1L do (fields[f])->fillLinks
  
  properties = self.properties->values(count=nproperties)
  for p = 0L, nproperties - 1L do (properties[p])->fillLinks
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
function doctreeclass::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (strlowcase(name) eq strlowcase(self.classname)) then begin
    return, self->getVariable('index_url')
  endif

  ; check properties
  propNames = self.properties->keys(count=nprops)
  for p = 0L, nprops - 1L do begin
    property = self.properties->get(propNames[p])
    if (obj_valid(exclude) && exclude eq property) then continue
    url = property->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
    
  ; check fields
  fieldNames = self.fields->keys(count=nfields)
  for f = 0L, nfields - 1L do begin
    field = self.fields->get(fieldNames[f])
    if (obj_valid(exclude) && exclude eq field) then continue
    url = field->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  ; check ancestors
  nancestors = self.ancestors->count()
  for a = 0L, nancestors - 1L do begin
    ancestor = self.ancestors->get(position=a)
    if (strlowcase(name) eq strlowcase(ancestor.classname)) then begin
      return, ancestor->getVariable('index_url')
    endif
  endfor
  
  ; check children
  nchildren = self.children->count()
  for c = 0L, nchildren - 1L do begin
    child = self.children->get(position=c)
    if (strlowcase(name) eq strlowcase(child.classname)) then begin
      return, child->getVariable('index_url')
    endif
  endfor
  
  ; if nothing found yet, pass along to file
  return, obj_valid(exclude) && exclude eq self.profile $
            ? '' $
            : self.profile->lookupName(name, exclude=self)
end


;+
; Free resources.
;-
pro doctreeclass::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, [self.parents, self.ancestors, self.children]
  
  if (self.fields->count() gt 0) then obj_destroy, self.fields->values()
  obj_destroy, self.fields
  
  if (self.properties->count() gt 0) then obj_destroy, self.properties->values()
  obj_destroy, self.properties
end


;+
; Create a class tree object.
;
; :Returns: 
;    1 for success, 0 otherwise
;
; :Params:
;    classname : in, required, type=string
;       name of the class
;-
function doctreeclass::init, classname, pro_file=proFile, system=system
  compile_opt strictarr, hidden
  
  self.classname = classname
  if (n_elements(proFile) gt 0) then self.proFile = proFile
  self.system = system
   
  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 1L && self->isInLibrary()) then begin
    self.system->createIndexEntry, self.classname, self
  endif
  
  self.system->getProperty, classes=classes
  self.classes = classes
  self.classes->put, strlowcase(self.classname), self

  self.parents = obj_new('MGcoArrayList', type=11, block_size=4)
  self.ancestors = obj_new('MGcoArrayList', type=11, block_size=10)
  self.children = obj_new('MGcoArrayList', type=11, block_size=10)
  
  self.fields = obj_new('MGcoHashtable', key_type=7, value_type=11)
  self.properties = obj_new('MGcoHashtable', key_type=7, value_type=11)
  
  self->findParents
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    system
;       system tree object
;    classes
;       classes hashtable (classname -> class object) from system tree object
;    proFile
;       pro file which contains the class
;    classname
;       string classname of the class
;    parents
;       array list of parent classes
;    ancestors
;       array list of ancestor classes
;    children
;       array list of (direct) children classes
;    fields
;       hash table of field tree classes
;    properties
;       hash table of property tree classes
;-
pro doctreeclass__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeClass, $
             system: obj_new(), $
             classes: obj_new(), $
             proFile: obj_new(), $
             
             classname: '', $
             
             parents: obj_new(), $             
             ancestors: obj_new(), $
             children: obj_new(), $
             
             fields: obj_new(), $
             properties: obj_new() $
           }
end
