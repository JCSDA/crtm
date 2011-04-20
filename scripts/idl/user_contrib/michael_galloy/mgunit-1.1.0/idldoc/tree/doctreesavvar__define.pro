; docformat = 'rst'

;+
; Represents a variable in .sav file.
;
; :Properties:
;    declaration
;       string representing IDL code to create variable
;    system
;       system object
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
function doctreesavvar::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  switch strlowcase(name) of
    'name': return, self.name
    'declaration': return, self.declaration
    
    'has_thumbnail': return, self.hasThumbnail
    'thumbnail_url': return, self.localThumbnailUrl
      
    'index_name': return, self.name
    'index_type': begin
        basename = self.savFile->getVariable('basename')
        
        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text='variable in .sav file ')
        link_node = obj_new('MGtmTag', type='link')
        link_node->addAttribute, 'reference', self.savFile->getVariable('index_url')
        link_node->addChild, obj_new('MGtmText', text=basename)
        type_tree->addChild, link_node
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree
        
        return, comments
      end
    'index_url': begin
        self.savFile->getProperty, directory=directory
        return, directory->getVariable('url') + self.savFile->getVariable('local_url')
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
        ; search in the system object if the variable is not found here
        var = self.savFile->getVariable(name, found=found)
        if (found) then return, var
        
        found = 0B
        return, -1L
      end
  endswitch
end


;+
; Set properties.
;-
pro doctreesavvar::setProperty, declaration=declaration
  compile_opt strictarr, hidden

  if (n_elements(declaration) gt 0) then self.declaration = declaration
end


;+
; All sav variables are visible.
;
; :Returns: 
;    1 if visible, 0 if not visible
;-
function doctreesavvar::isVisible
  compile_opt strictarr, hidden
  
  return, 1B
end


;+
; Fill the links in comments for a save file variable.
;-
pro doctreesavvar::fillLinks
  compile_opt strictarr
  
  ; nothing to check
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
function doctreesavvar::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (strlowcase(name) eq strlowcase(self.name)) then begin
    return, self->getVariable('index_url') 
  endif
  
  return, obj_valid(exclude) && exclude eq self.directory $
            ? '' $
            : self.savFile->lookupName(name, exclude=self)
end


;+
; Free resources.
;-
pro doctreesavvar::cleanup
  compile_opt strictarr, hidden

end


;+
; Creates a sav variable object.
;
; :Returns: 
;    1 for success, 0 for failure
;
; :Params:
;    name : in, required, type=string
;       name of the variable
;    data : in, required, type=any
;       data contained in the variable in the .sav file
;    savFile : in, required, type=object
;       sav file tree object
;-
function doctreesavvar::init, name, data, savFile, system=system
  compile_opt strictarr, hidden
  
  self.name = name
  self.savFile = savFile
  self.system = system

  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 2L) then self.system->createIndexEntry, self.name, self
  
  im = mg_thumbnail(data, valid=valid)
  self.hasThumbnail = valid
  if (self.hasThumbnail) then begin 
    self.savFile->getProperty, directory=directory, basename=basename
    directory->getProperty, location=location
    self.system->getProperty, output=output
    self.localThumbnailUrl = file_basename(basename, '.sav') + '-sav-' + self.name + '.png'
    filename = output + location + self.localThumbnailUrl
    
    write_png, filename, im
  endif
  
  self.declaration = mg_variable_declaration(data)
  
  ; free data
  heap_free, data
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    system
;       system object
;    savfile
;       sav file object in which the variable is contained
;    name
;       name of the variable
;    declaration
;       IDL code to specify variable type
;    localThumbnailUrl
;       URL to the thumbnail image
;    hasThumbnail
;       1 if a thumbnail image for the variable could be derived
;-
pro doctreesavvar__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeSavVar, $
             system: obj_new(), $
             savFile: obj_new(), $
             
             name: '', $
             declaration: '', $
             localThumbnailUrl: '', $
             hasThumbnail: 0B $
           }
end