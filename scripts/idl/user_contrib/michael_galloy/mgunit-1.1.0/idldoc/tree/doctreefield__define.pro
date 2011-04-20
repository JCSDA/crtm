; docformat = 'rst'

;+
; Represents a field of a class.
;
; :Properties:
;    name
;       name of the field
;    type
;       declaration of IDL type
;    comments
;       parse tree
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
function doctreefield::getVariable, name, found=found
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
    'type': return, self.type      
    
    'has_comments': return, obj_valid(self.comments)
    'comments': return, self.system->processComments(self.comments)  
    'comments_first_line': begin
        if (~obj_valid(self.comments)) then return, ''
        
        firstline = mg_tm_firstline(self.comments)
        text_firstline = self.system->processComments(firstline)
        obj_destroy, firstline
        
        return, text_firstline
      end 
              
    'index_name': return, self.name
    'index_type': begin
        self.class->getProperty, classname=classname
        
        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text='field in class ')
        link_node = obj_new('MGtmTag', type='link')
        link_node->addAttribute, 'reference', self.class->getVariable('index_url')
        link_node->addChild, obj_new('MGtmText', text=classname)
        type_tree->addChild, link_node
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree

        return, comments
      end     
    'index_url': return, self.class->getVariable('url')
                
    else: begin
        var = self.class->getVariable(name, found=found)
        if (found) then return, var
            
        found = 0B
        return, -1L
      end    
  endcase
end


;+
; Get properties of the field.
;-
pro doctreefield::getProperty, name=name, type=type
  compile_opt strictarr, hidden
  
  if (arg_present(name)) then name = self.name
  if (arg_present(type)) then type = self.type
end


;+
; Set properties of the field.
;-
pro doctreefield::setProperty, name=name, type=type, comments=comments
  compile_opt strictarr, hidden

  if (n_elements(name) gt 0) then self.name = name
  if (n_elements(type) gt 0) then self.type = type
  if (n_elements(comments) gt 0) then self.comments = comments
end


;+
; Fields are visible if their class is visible.
;
; :Returns: 1 if visible, 0 if not visible
;-
function doctreefield::isVisible
  compile_opt strictarr, hidden
  
  return, self.class->isVisible()
end


;+
; Fill the links in comments for a field.
;-
pro doctreefield::fillLinks
  compile_opt strictarr
  
  doctree_fill_links, self.comments, self
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
function doctreefield::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (strlowcase(name) eq strlowcase(self.name)) then begin
    return, self->getVariable('index_url')
  endif
    
  return, obj_valid(exclude) && exclude eq self.class $
            ? '' $
            : self.class->lookupName(name, exclude=self)   
end


;+
; Free resources.
;-
pro doctreefield::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, self.comments
end


;+
; Creates a field.
;
; :Returns: 
;    1 for success, 0 for failure
;    
; :Params:
;    name : in, required, type=string
;       name of the field
;-
function doctreefield::init, name, class=class, system=system
  compile_opt strictarr, hidden
  
  self.name = name
  self.class = class
  self.system = system
  
  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 2L) then self.system->createIndexEntry, self.name, self
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    system
;       system object
;    class
;       class tree object
;    name
;       name of the field
;    type
;       IDL type declaration
;    comments
;       parse tree object
;-
pro doctreefield__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeField, $
             system: obj_new(), $
             class: obj_new(), $
             
             name: '', $
             type: '', $
             comments: obj_new() $
           }
end