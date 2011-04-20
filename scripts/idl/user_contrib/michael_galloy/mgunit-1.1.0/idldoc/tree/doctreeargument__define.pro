; docformat = 'rst'

;+
; Argument class representing a positional parameter or keyword for a routine. 
; 
; :Properties:
;    routine
;       DOCtreeRoutine object that contains this argument
;    name 
;       name of the routine
;    is_first
;       set to indicate that this argument is the first of its parent routine
;    is_keyword
;       set to indicate that this argument is a keyword
;    is_optional
;       set to indicate that this argument is optional
;    is_required
;       set to indicate that this argument is required
;    is_input
;       set to indicate that this argument is an input
;    is_output
;       set to indicate that this arugment is an output
;    type
;       string indicating the IDL variable type of the argument
;    default_value
;       string indicating the default value if this argument is not present
;    is_hidden
;       set to indicate that this argument is hidden (hidden from users and
;       developers)
;    is_private
;       set to indicate that this argument is private (hidden from users)
;    comments
;       text node hierarchy
;-

;+
; The getVariable method is required for objects passed as an input to a
; template.
;
; :Returns: 
;    value or -1L if variable name not found
;
; :Params:
;    name : in, required, type=string
;       name of variable (case insensitive)
; 
; :Keywords: 
;    found : out, optional, type=boolean
;       pass a named variable to get whether the variable was found
;-
function doctreeargument::getVariable, name, found=found
  compile_opt strictarr, hidden
  on_error, 2
  
  ; make sure name is present, a string, and only 1 element
  if (n_elements(name) ne 1 || size(name, /type) ne 7) then begin
    message, 'name parameter invalid'
  endif
  
  ; return value if name is ok
  found = 1B
  case name of
    'name': return, self.name
    'id': begin
        self.routine->getProperty, name=name
        return, name + ':' + (self.isKeyword ? 'k' : 'p') + ':' + self.name
      end
      
    'index_name': return, self.name
    'index_type': begin
        type = self.isKeyword ? 'keyword' : 'parameter'
        self.routine->getProperty, file=file, name=routineName
        file->getProperty, basename=basename

        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text=type + ' in routine ')
        
        routine_node = obj_new('MGtmTag', type='link')
        routine_node->addAttribute, 'reference', self.routine->getVariable('index_url')
        routine_node->addChild, obj_new('MGtmText', text=routineName)
        type_tree->addChild, routine_node

        type_tree->addChild, obj_new('MGtmText', text=' in file ')

        file_node = obj_new('MGtmTag', type='link')
        file_node->addAttribute, 'reference', file->getVariable('index_url')
        file_node->addChild, obj_new('MGtmText', text=basename)
        type_tree->addChild, file_node
        
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree

        return, comments
      end      
    'index_url': begin
        self.routine->getProperty, file=file, name=routineName
        file->getProperty, directory=directory, basename=basename
        directory->getProperty, url=dirUrl
        msg = '(%"%s%s#%s:%s:%s")'
        return, string(format=msg, $
                       dirUrl, $
                       file_basename(basename, '.pro') + '.html', $
                       routineName, $
                       (self.isKeyword ? 'k' : 'p'), $
                       self.name)
      end
            
    'is_keyword' : return, self.isKeyword
    'is_optional': return, self.isOptional
    'is_required': return, self.isRequired
    'is_input': return, self.isInput
    'is_output': return, self.isOutput
    'type': return, self.type
    'is_boolean': return, strlowcase(self.type) eq 'boolean'
    'default_value': return, self.defaultValue
    'is_hidden': return, self.isHidden
    'is_private': return, self.isPrivate
    'is_obsolete': return, self.isObsolete
    
    'prefix': begin
      self.routine->getProperty, is_function=isFunction
      return, (isFunction && self.isFirst) ? '' : ', '
    end
    'suffix': begin
      self.routine->getProperty, is_function=isFunction
      return, (isFunction && self.isLast) ? '' : ''
    end
    
    'has_comments': return, obj_valid(self.comments)
    'comments': return, self.system->processComments(self.comments)       
    'comments_first_line': begin
        if (~obj_valid(self.comments)) then return, ''
        
        if (~obj_valid(self.firstline)) then begin
          self.firstline = mg_tm_firstline(self.comments)
        endif
        
        return, self.system->processComments(self.firstline) 
      end    
    
    else : begin
        ; search in the routine object if the variable is not found here
        var = self.routine->getVariable(name, found=found)
        if (found) then return, var
            
        found = 0B
        return, -1L
      end
  endcase
end


;+
; Set properties of the argument.
;-
pro doctreeargument::getProperty, routine=routine, name=name, $
    is_first=isFirst, is_last=isLast, is_keyword=isKeyword, is_optional=isOptional, $
    is_required=isRequired, is_input=isInput, is_output=isOutput, $
    type=type, default_value=defaultValue, is_hidden=isHidden, $
    is_private=isPrivate, is_obsolete=isObsolete, $
    comments=comments, documented=documented  
  compile_opt strictarr, hidden
  
  if (arg_present(routine)) then routine = self.routine
  if (arg_present(name)) then name = self.name
  if (arg_present(isFirst)) then isFirst = self.isFirst  
  if (arg_present(isLast)) then isLast = self.isLast  
  if (arg_present(isKeyword)) then isKeyword = self.isKeyword  
  if (arg_present(isOptional)) then isOptional = self.isOptional    
  if (arg_present(isRequired)) then isRequired = self.isRequired      
  if (arg_present(isInput)) then isInput = self.isInput    
  if (arg_present(isOutput)) then isOutput = self.isOutput      
  if (arg_present(type)) then type = self.type      
  if (arg_present(defaultValue)) then defaultValue = self.defaultValue      
  if (arg_present(isHidden)) then isHidden = self.isHidden      
  if (arg_present(isPrivate)) then isPrivate = self.isPrivate      
  if (arg_present(isObsolete)) then isObsolete = self.isObsolete      
  if (arg_present(comments)) then comments = self.comments
  if (arg_present(documented)) then documented = self.documented
end


;+
; Set properties of the argument.
;-
pro doctreeargument::setProperty, is_keyword=isKeyword, $
                                  is_first=isFirst, is_last=isLast, $
                                  is_optional=isOptional, $
                                  is_required=isRequired, $
                                  is_input=isInput, $
                                  is_output=isOutput, $
                                  type=type, $
                                  default_value=defaultValue, $
                                  is_hidden=isHidden, $
                                  is_private=isPrivate, $
                                  is_obsolete=isObsolete, $
                                  comments=comments
  compile_opt strictarr, hidden
  
  if (n_elements(isFirst) gt 0) then self.isFirst = isFirst
  if (n_elements(isLast) gt 0) then self.isLast = isLast  
  if (n_elements(isKeyword) gt 0) then self.isKeyword = isKeyword
  
  if (n_elements(isOptional) gt 0) then begin
    self.isOptional = isOptional
    self.documented = 1B
  endif
  
  if (n_elements(isRequired) gt 0) then begin
    self.isRequired = isRequired
    self.documented = 1B
  endif
  
  if (n_elements(isInput) gt 0) then begin
    self.isInput = isInput
    self.documented = 1B
  endif
  
  if (n_elements(isOutput) gt 0) then begin
    self.isOutput = isOutput
    self.documented = 1B
  endif
  
  if (n_elements(type) gt 0) then begin
    self.type = type
    self.documented = 1B
  endif
  
  if (n_elements(defaultValue) gt 0) then begin
    self.defaultValue = defaultValue
    self.documented = 1B
  endif
  
  if (n_elements(isHidden) gt 0) then begin
    self.isHidden = isHidden
    self.documented = 1B
  endif
  
  if (n_elements(isPrivate) gt 0) then begin
    self.isPrivate = isPrivate
    self.documented = 1B
  endif

  if (n_elements(isObsolete) gt 0) then begin
    self.isObsolete = isObsolete
    self.documented = 1B
  endif
    
  if (n_elements(comments) gt 0) then begin
    self.comments = comments
    self.documented = 1B
  endif
end


;+
; Arguments are visible if their routine is visible and they don't have hidden 
; or private (with system variable user) set.
;
; :Returns: 1 if visible, 0 if not
;-
function doctreeargument::isVisible
  compile_opt strictarr, hidden

  ; each argument in a not-visible routine is not visible
  if (~self.routine->isVisible()) then return, 0B
    
  if (self.isHidden) then return, 0B
  
  ; if creating user-level docs and private then not visible
  self.system->getProperty, user=user
  if (self.isPrivate && user) then return, 0B  
  
  return, 1B  
end


;+
; Fill the links in comments for an argument.
;-
pro doctreeargument::fillLinks
  compile_opt strictarr
  
  doctree_fill_links, self.comments, self
  doctree_fill_links, self.firstline, self
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
function doctreeargument::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (strlowcase(name) eq strlowcase(self.name)) then begin
    return, self->getVariable('index_url')
  endif
  
  return, obj_valid(exclude) && exclude eq self.routine $
            ? '' $
            : self.routine->lookupName(name, exclude=self)
end


;+
; Free resources lower in the hierarchy.
;-
pro doctreeargument::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, self.firstline
  obj_destroy, self.comments
end


;+
; Create an argument: positional parameter or keyword.
; 
; :Returns: 1 for success, 0 for failure
;
; :Params: 
;    routine : in, required, type=object
;       DOCtreeRoutine object
;-
function doctreeargument::init, routine, name=name, is_keyword=isKeyword, $
                                system=system
  compile_opt strictarr, hidden
  
  self.system = system
  self.routine = routine
  
  if (n_elements(name) gt 0) then self.name = name
  
  self.isKeyword = keyword_set(isKeyword)
  
  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 2L) then self.system->createIndexEntry, self.name, self
  
  return, 1
end


;+
; Define the instance variables.
;
; :Fields:
;    routine 
;       DOCtreeRoutine object that contains this argument
;    name 
;       name of the argument
;    isFirst
;       indicates that this argument is the first of its parent routine
;    isLast
;       indicates that this argument is the first of its parent routine
;    isKeyword
;       indicates that this argument is a keyword
;    isOptional
;       indicates that this argument is optional
;    isRequired
;       indicates that this argument is required
;    isInput
;       indicates that this argument is an input
;    isOutput
;       indicates that this arugment is an output
;    type
;       string indicating the IDL variable type of the argument
;    defaultValue
;       string indicating the default value if this argument is not present
;    isHidden
;       indicates that this argument is hidden (hidden from users and
;       developers)
;    isPrivate
;       indicates that this argument is private (hidden from users)
;    comments
;       text node hierarchy
;-
pro doctreeargument__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeArgument, $
             system: obj_new(), $
             routine: obj_new(), $
             name: '', $
             isFirst: 0B, $
             isLast: 0B, $
             isKeyword: 0B, $
             isOptional: 0B, $
             isRequired: 0B, $
             isInput: 0B, $
             isOutput: 0B, $
             type: '', $
             defaultValue: '', $
             isHidden: 0B, $
             isPrivate: 0B, $
             isObsolete: 0B, $
             comments: obj_new(), $
             firstline: obj_new(), $
             documented: 0B $
           }
end