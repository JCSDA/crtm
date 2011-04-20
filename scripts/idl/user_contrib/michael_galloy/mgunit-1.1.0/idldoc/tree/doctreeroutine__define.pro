; docformat = 'rst'

;+
; Represents a routine (procedure or function, method or regular).
; 
; :Properties:
;    file
;       file tree object
;    name
;       name of the routine
;    is_function
;       1 if a function, 0 if not 
;    is_method
;       1 if a method, 0 if not
;    parameters
;       list object of positional parameter objects for routine
;    keywords
;       list object of keyword objects for routine
;    classname
;       name of class associated with this routine (if a method)
;    undocumented
;       true if not documented
;    partially_documented
;       true if partially documented
;    is_obsolete
;       1 if obsolete, 0 if not
;    is_abstract
;       1 if abstract (not implemented), 0 if not
;    is_hidden
;       1 if hidden (not visible), 0 if not
;    is_private
;       1 if private (hidden to users, but not developers), 0 if not
;    comments
;       markup tree of comments for routine
;    returns
;       markup tree for return value of routine
;    examples
;       markup tree for usage examples for routine
;    bugs
;       markup tree for known bugs for the routine
;    pre
;       markup tree for pre-condition for the routine
;    post
;       markup tree for post-condition for the routine
;    customer_id
;       markup tree for customer identification
;    author
;       markup tree for author
;    copyright
;       markup tree for copyright information
;    history
;       markup tree for history
;    version
;       markup tree for version of routine
;    todo
;       markup tree for todo items for this routine
;    restrictions
;       markup tree for restrictions on routine usage
;    uses
;       markup tree for routines, classes, etc used by routine
;    requires
;       markup tree for IDL version requirements
;    n_lines
;       number of lines in routine
;-

;+
; Get properties.
;-
pro doctreeroutine::getProperty, file=file, name=name, is_function=isFunction, $
                                 is_method=isMethod, parameters=parameters, $
                                 accepts_max_params=acceptsMaxParams, $
                                 keywords=keywords, classname=classname, $
                                 undocumented=undocumented, $
                                 partially_documented=partiallyDocumented
                                 
  compile_opt strictarr, hidden
  
  if (arg_present(file)) then file = self.file
  if (arg_present(name)) then name = self.name
  if (arg_present(isFunction)) then isFunction = self.isFunction
  if (arg_present(isMethod)) then isMethod = self.isMethod
  if (arg_present(parameters)) then parameters = self.parameters
  if (arg_present(keywords)) then keywords = self.keywords
  if (arg_present(acceptsMaxParams)) then acceptsMaxParams = self.acceptsMaxParams
  if (arg_present(classname)) then begin
    isDefine = strlowcase(strmid(self.name, 7, /reverse_offset)) eq '__define'
    colonPos = strpos(self.name, ':')
    case 1 of
      isDefine: classname = strmid(self.name, 0, strlen(self.name) - 8)
      colonPos ne -1: classname = strmid(self.name, 0, colonPos)
      else: classname = ''
    endcase
  endif
  
  if (arg_present(undocumented)) then undocumented = self.undocumented
  if (arg_present(partiallyDocumented)) then begin
    partiallyDocumented = self.partiallyDocumented
  endif
end


;+
; Determine if a routine is not documented, partially documented, or fully
; documented.
;-
pro doctreeroutine::checkDocumentation
  compile_opt strictarr, hidden
 
  fullyDocumented = 1B
  partiallyDocumented = 0B
  
  fullyDocumented and= obj_valid(self.comments)
  partiallyDocumented or= obj_valid(self.comments)  
  
  ; check return value
  if (self.isFunction) then begin
    fullyDocumented and= obj_valid(self.returns)
    partiallyDocumented or= obj_valid(self.returns)
  endif
  
  ; check each param
  for p = 0L, self.parameters->count() - 1L do begin
    param = self.parameters->get(position=p)
    param->getProperty, documented=paramDocumented
    
    fullyDocumented and= paramDocumented
    partiallyDocumented or= paramDocumented
  endfor
  
  ; check each keyword
  for k = 0L, self.keywords->count() - 1L do begin
    keyword = self.keywords->get(position=k)
    keyword->getProperty, documented=keywordDocumented
    
    fullyDocumented and= keywordDocumented
    partiallyDocumented or=keywordDocumented
  endfor    

  self.documentationLevel = partiallyDocumented + fullyDocumented
  if (~fullyDocumented) then self.system->createDocumentationEntry, self
end


;+
; Set properties.
;-
pro doctreeroutine::setProperty, name=name, $
                                 is_Function=isFunction, $
                                 is_method=isMethod, $
                                 is_obsolete=isObsolete, $
                                 is_abstract=isAbstract, $
                                 is_hidden=isHidden, $
                                 is_private=isPrivate, $
                                 comments=comments, $
                                 returns=returns, $
                                 accepts_max_params=acceptsMaxParams, $
                                 examples=examples, $
                                 bugs=bugs, pre=pre, post=post, $
                                 customer_id=customerId, $
                                 author=author, copyright=copyright, $
                                 history=history, $
                                 version=version, $
                                 todo=todo, $
                                 restrictions=restrictions, $
                                 uses=uses, $
                                 requires=requires, $
                                 n_lines=nLines, $
                                 lines=lines
  compile_opt strictarr, hidden
  
  if (n_elements(name) gt 0) then begin
    self.name = name
    self.isMethod = strpos(self.name, '::') eq -1L ? 0B : 1B
    self.system->getProperty, index_level=indexLevel
    if (indexLevel ge 1L) then self.system->createIndexEntry, self.name, self
  endif
  
  if (n_elements(isFunction) gt 0) then self.isFunction = isFunction
  if (n_elements(isMethod) gt 0) then self.isMethod = isMethod  
  if (n_elements(isHidden) gt 0) then self.isHidden = isHidden
  if (n_elements(isPrivate) gt 0) then self.isPrivate = isPrivate
  if (n_elements(isObsolete) gt 0) then self.isObsolete = isObsolete
  if (n_elements(isAbstract) gt 0) then self.isAbstract = isAbstract

  if (n_elements(comments) gt 0) then begin
    if (obj_valid(self.comments)) then begin
      parent = obj_new('MGtmTag')
      parent->addChild, self.comments
      parent->addChild, comments
      self.comments = parent
    endif else self.comments = comments
  endif

  if (n_elements(returns) gt 0L) then self.returns = returns  
  if (n_elements(acceptsMaxParams) gt 0L) then self.acceptsMaxParams = acceptsMaxParams
  if (n_elements(examples) gt 0L) then self.examples = examples
  if (n_elements(nLines) gt 0L) then self.nLines = nLines
  if (n_elements(lines) gt 0L) then self->_computeComplexity, lines
  
  ; "author info" attributes
  if (n_elements(author) gt 0) then begin
    self.hasAuthorInfo = 1B
    self.author = author
  endif

  if (n_elements(copyright) gt 0) then begin
    self.hasAuthorInfo = 1B
    self.copyright = copyright
  endif
  
  if (n_elements(history) gt 0) then begin
    self.hasAuthorInfo = 1B
    self.history = history
  endif

  if (n_elements(version) gt 0) then begin
    self.hasAuthorInfo = 1B
    self.version = version
  endif
    
  ; "other" attributes  
  if (n_elements(bugs) gt 0) then begin
    self.hasOthers = 1B
    self.bugs = bugs
  endif  
  
  if (n_elements(pre) gt 0) then begin
    self.hasOthers = 1B
    self.pre = pre
  endif 
  
  if (n_elements(post) gt 0) then begin
    self.hasOthers = 1B
    self.post = post
  endif     
  
  if (n_elements(customerId) gt 0) then begin
    self.hasOthers = 1B
    self.customerId = customerId
  endif 
  
  if (n_elements(todo) gt 0) then begin
    self.hasOthers = 1B
    self.todo = todo
  endif
  
  if (n_elements(restrictions) gt 0) then begin
    self.hasOthers = 1B
    self.restrictions = restrictions
  endif  
       
  if (n_elements(uses) gt 0) then begin
    self.hasOthers = 1B
    self.uses = uses
  endif  
  
  if (n_elements(requires) gt 0) then begin
    self.hasOthers = 1B
    self.requires = requires
  endif    
end


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
function doctreeroutine::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  case strlowcase(name) of
    'name': return, self.name

    'is_function': return, self.isFunction
    'is_private': return, self.isPrivate
    'is_abstract': return, self.isAbstract  
    'is_private': return, self.isPrivate   
    'is_visible': return, self->isVisible() 
    'is_obsolete': return, self.isObsolete
    'is_method': return, self.isMethod
    
    'complexity': return, self.complexity
    'complexity_color': begin
        self.system->getProperty, complexity_cutoffs=cutoffs
        return, (['green', 'orange', 'red'])[value_locate(cutoffs, self.complexity)]
      end
    'n_lines': return, self.nLines
    'n_lines_color': begin
        self.system->getProperty, routine_line_cutoffs=cutoffs
        return, (['green', 'orange', 'red'])[value_locate(cutoffs, self.nlines)]
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
    'plain_comments': return, self.system->processPlainComments(self.comments)

    'has_returns': return, obj_valid(self.returns)
    'returns': return, self.system->processComments(self.returns)
    
    'accepts_max_params': return, self.acceptsMaxParams
    
    'has_categories': return, self.categories->count() gt 0
    'categories': return, self.categories->get(/all)
    
    'has_examples': return, obj_valid(self.examples)
    'examples': return, self.system->processComments(self.examples)
    
    'has_author_info': return, self.hasAuthorInfo
    
    'has_author': return, obj_valid(self.author)
    'author': return, self.system->processComments(self.author)
    'plain_author': return, self.system->processPlainComments(self.author)

    'has_copyright': return, obj_valid(self.copyright)
    'copyright': return, self.system->processComments(self.copyright)
    
    'has_history': return, obj_valid(self.history)
    'history': return, self.system->processComments(self.history)

    'has_version': return, obj_valid(self.version)
    'version': return, self.system->processComments(self.version)
        
    'has_others': return, self.hasOthers
    
    'has_bugs': return, obj_valid(self.bugs)
    'bugs': return, self.system->processComments(self.bugs)

    'has_pre': return, obj_valid(self.pre)
    'pre': return, self.system->processComments(self.pre)
    
    'has_post': return, obj_valid(self.post)
    'post': return, self.system->processComments(self.post)
      
    'has_customer_id': return, obj_valid(self.customerId)
    'customer_id': return, self.system->processComments(self.customerId)

    'has_todo': return, obj_valid(self.todo)
    'todo': return, self.system->processComments(self.todo)

    'has_restrictions': return, obj_valid(self.restrictions)
    'restrictions': return, self.system->processComments(self.restrictions)

    'has_uses': return, obj_valid(self.uses)
    'uses': begin
        self.file->getProperty, directory=directory
        directory->getProperty, url=dirUrl
        if (dirUrl eq './') then begin
          root = '.'
        endif else begin
          dummy = strsplit(dirUrl, '/', count=ndirs)
          root = strjoin(strarr(ndirs) + '..', '/')
        endelse
        return, self.system->processUses(self.uses, root=root)
      end
                           
    'has_requires': return, obj_valid(self.requires)
    'requires': return, self.system->processComments(self.requires)
    
    'n_parameters': return, self.parameters->count()
    'parameters': return, self.parameters->get(/all)
    'n_visible_parameters': begin
        nVisible = 0L
        for p = 0L, self.parameters->count() - 1L do begin
          parameter = self.parameters->get(position=p)          
          nVisible += parameter->isVisible()          
        endfor
        return, nVisible
      end
    'visible_parameters': begin        
        parameters = self.parameters->get(/all, count=nParameters)
        if (nParameters eq 0L) then return, -1L
        
        isVisibleParameters = bytarr(nParameters)
        for p = 0L, nParameters - 1L do begin
          isVisibleParameters[p] = parameters[p]->isVisible()
        endfor
        
        ind = where(isVisibleParameters eq 1B, nVisibleParameters)
        if (nVisibleParameters eq 0L) then return, -1L
        
        return, parameters[ind]
      end 
    'n_keywords': return, self.keywords->count()
    'keywords': return, self.keywords->get(/all)
    'n_visible_keywords': begin
        nVisible = 0L
        for k = 0L, self.keywords->count() - 1L do begin
          keyword = self.keywords->get(position=k)          
          nVisible += keyword->isVisible()          
        endfor
        return, nVisible
      end
    'visible_keywords': begin        
        keywords = self.keywords->get(/all, count=nKeywords)
        if (nKeywords eq 0L) then return, -1L
        
        isVisibleKeywords = bytarr(nKeywords)
        for k = 0L, nKeywords - 1L do begin
          isVisibleKeywords[k] = keywords[k]->isVisible()
        endfor
        
        ind = where(isVisibleKeywords eq 1B, nVisibleKeywords)
        if (nVisibleKeywords eq 0L) then return, -1L
        
        return, keywords[ind]
      end
    'plain_parameters': begin
        result = ''
        
        ; for each keyword: grab each keyword name, comments, type, default
        for k = 0L, self.keywords->count() - 1L do begin
          keyword = self.keywords->get(position=k)
          keyword->getProperty, name=name, comments=comments, $
                                type=type, default_value=defaultValue
          result += name
          result += strjoin(self.system->processPlainComments(comments), ' ')
          result += strjoin(self.system->processPlainComments(type), ' ')
          result += strjoin(self.system->processPlainComments(defaultValue), ' ')                    
        endfor
        
        ; for each param: grab each param name, comments, type, default
        for p = 0L, self.parameters->count() - 1L do begin
          param = self.parameters->get(position=p)
          param->getProperty, name=name, comments=comments, $
                              type=type, default_value=defaultValue
          result += name
          result += strjoin(self.system->processPlainComments(comments), ' ')
          result += strjoin(self.system->processPlainComments(type), ' ')
          result += strjoin(self.system->processPlainComments(defaultValue), ' ')                    
        endfor        
        return, result
      end
      
    'index_name': return, self.name
    'index_type': begin
        self.file->getProperty, basename=basename
        
        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text='routine in ')
        link_node = obj_new('MGtmTag', type='link')
        link_node->addAttribute, 'reference', self.file->getVariable('index_url')
        link_node->addChild, obj_new('MGtmText', text=basename)
        type_tree->addChild, link_node
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree

        return, comments
      end
    'index_url': begin
        self.file->getProperty, directory=directory
        return, directory->getVariable('url') + self.file->getVariable('local_url') + '#' + self.name
      end
        
    'documentation_level': return, self.documentationLevel
    
    'plain_attributes': begin
        attributes = [self.bugs, self.version, self.history, self.copyright, $
                      self.examples, self.customerId, self.requires, $
                      self.restrictions, self.todo, self.uses, self.returns, $
                      self.pre, self.post]
        
        result = ''
        for a = 0L, n_elements(attributes) - 1L do begin
          result += strjoin(self.system->processPlainComments(attributes[a]), ' ')
        endfor
        
        return, result
      end
    
    else: begin
        ; search in the file object if the variable is not found here
        var = self.file->getVariable(name, found=found)
        if (found) then return, var
        
        found = 0B
        return, -1L
      end
  endcase
end


;+
; Uses file hidden/private attributes, system wide user/developer level, and
; the status of the containing file to determine if this routine should be 
; visible.
;
; :Returns: 
;    1 if visible, 0 if not visible
;-
function doctreeroutine::isVisible
  compile_opt strictarr, hidden

  ; each routine in a not-visible file is not visible
  if (~self.file->isVisible()) then return, 0B
    
  if (self.isHidden) then return, 0B
  
  ; if creating user-level docs and private then not visible
  self.system->getProperty, user=user
  if (self.isPrivate && user) then return, 0B  
  
  return, 1B
end


;+
; Add a parameter to the list of parameters for this routine.
; 
; :Params:
;    param : in, required, type=object
;       argument tree object
;-
pro doctreeroutine::addParameter, param
  compile_opt strictarr, hidden
  
  self.parameters->add, param
end


;+
; Get a positional parameter by name.
;
; :Returns: 
;    argument tree object
;
; :Params:
;    name : in, required, type=string
;       name of the parameter to find
;
; :Keywords:
;    found : out, optional, type=boolean
;       set to a named variable to find out if the parameter was found
;-
function doctreeroutine::getParameter, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  for i = 0L, self.parameters->count() - 1L do begin
    p = self.parameters->get(position=i)
    p->getProperty, name=n
    if (strlowcase(name) eq strlowcase(n)) then return, p
  endfor
  found = 0B
  return, -1L
end


;+
; Add a keyword to the list of keywords for this routine.
; 
; :Params:
;    keyword : in, required, type=object
;       argument tree object
;-
pro doctreeroutine::addKeyword, keyword
  compile_opt strictarr, hidden
  
  self.keywords->add, keyword

  ; create a property for a keyword of getProperty, setProperty, or init
  self.file->getProperty, has_class=hasClass
  if (self.isMethod && hasClass) then begin
  
    keyword->getProperty, name=propertyName, type=keywordType
    self->getProperty, classname=classname
    
    ; lookup class
    self.system->getProperty, classes=classes
    class = classes->get(strlowcase(classname), found=found)
    
    propertyType = ''
    case 1 of
      strlowcase(strmid(self.name, 10, /reverse_offset)) eq 'getproperty': begin
          class->addProperty, property, property_name=propertyName
          property->setProperty, is_get=1B
          property->getProperty, type=propertyType
        end
      strlowcase(strmid(self.name, 10, /reverse_offset)) eq 'setproperty': begin
          class->addProperty, property, property_name=propertyName
          property->setProperty, is_set=1B
          property->getProperty, type=propertyType
        end
      strlowcase(strmid(self.name, 3, /reverse_offset)) eq 'init': begin
          class->addProperty, property, property_name=propertyName
          property->setProperty, is_init=1B
          property->getProperty, type=propertyType
        end
      else:   ; just a normal keyword
    endcase
    
    if (propertyType ne '' && keywordType eq '') then begin
      keyword->setProperty, type=propertyType
    endif
  endif  
end


;+
; Get a keyword by name.
;
; :Returns: 
;    argument tree object
;
; :Params:
;    name : in, required, type=string
;       name of the keyword to find
;
; :Keywords:
;    found : out, optional, type=boolean
;       set to a named variable to find out if the keyword was found
;-
function doctreeroutine::getKeyword, name, found=found
  compile_opt strictarr, hidden

  found = 1B
  for i = 0L, self.keywords->count() - 1L do begin
    k = self.keywords->get(position=i)
    k->getProperty, name=n
    if (strlowcase(name) eq strlowcase(n)) then return, k 
  endfor
  found = 0B
  return, -1L
end


;+
; Add a category name to the routine.
;
; :Params:
;    name : in, required, type=string
;       name of category to add to this routine
;-
pro doctreeroutine::addCategory, name
  compile_opt strictarr, hidden

  self.categories->add, name
end


;+
; Compute McCabe complexity for the routine.
;
; :Params:
;    lines : in, optional, type=strarr
;       code to analyze
;-
pro doctreeroutine::_computeComplexity, lines
  compile_opt strictarr
  
  self.system->getProperty, statistics=statistics, $
                            routine_line_cutoffs=routineLineCutoffs, $
                            complexity_cutoffs=complexityCutoffs
  if (~statistics) then return

  self.complexity = doc_complexity(lines)
  
  if (value_locate(routineLineCutoffs, self.nlines) gt 0L $
        || value_locate(complexityCutoffs, self.complexity) gt 0L) then begin
    self.system->createComplexityEntry, self
  endif
end


;+
; Mark first and last arguments of a routine. Needs to be called after parsing
; the routine, but before the output is started.
;-
pro doctreeroutine::markArguments
  compile_opt strictarr, hidden
  
  nArgs = self.parameters->count() + self.keywords->count()
  if (nArgs le 0) then return
  
  arguments = objarr(nArgs)
  
  if (self.parameters->count() gt 0) then begin
    arguments[0] = self.parameters->get(/all)
  endif
  
  if (self.keywords->count() gt 0) then begin
    arguments[self.parameters->count()] = self.keywords->get(/all)
  endif

  arguments[0]->setProperty, is_first=1B
  arguments[n_elements(arguments) - 1L]->setProperty, is_last=1B
end


;+
; Free resources.
;-
pro doctreeroutine::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, self.firstline
  obj_destroy, self.comments
  
  obj_destroy, [self.author, self.copyright, self.history, self.version]
  
  obj_destroy, [self.bugs, self.todo, self.restrictions, self.uses, $
                self.requires, self.customerId, self.pre, self.post]
  
  obj_destroy, self.examples
  obj_destroy, self.returns
  obj_destroy, self.parameters 
  obj_destroy, self.keywords

  obj_destroy, self.categories
end


;+
; Fill the links in comments for a routine.
;-
pro doctreeroutine::fillLinks
  compile_opt strictarr
  
  doctree_fill_links, self.comments, self
  doctree_fill_links, self.firstline, self
  
  doctree_fill_links, self.returns, self
  doctree_fill_links, self.examples, self

  doctree_fill_links, self.author, self
  doctree_fill_links, self.copyright, self
  doctree_fill_links, self.history, self
  doctree_fill_links, self.version, self

  doctree_fill_links, self.bugs, self
  doctree_fill_links, self.pre, self
  doctree_fill_links, self.post, self
  doctree_fill_links, self.uses, self
  doctree_fill_links, self.requires, self
  doctree_fill_links, self.customerId, self
  doctree_fill_links, self.todo, self
  doctree_fill_links, self.restrictions, self                         
               
  parameters = self.parameters->get(/all, count=nparameters)
  for i = 0L, nparameters - 1L do (parameters[i])->fillLinks
  
  keywords = self.keywords->get(/all, count=nkeywords)
  for i = 0L, nkeywords - 1L do (keywords[i])->fillLinks
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
function doctreeroutine::lookupName, name, exclude=exclude
  compile_opt strictarr
    
  if (strlowcase(name) eq strlowcase(self.name)) then return, self->getVariable('index_url')
  
  ; search parameters
  parameters = self.parameters->get(/all, count=nparameters)
  for i = 0L, nparameters - 1L do begin
    if (obj_valid(exclude) && exclude eq parameters[i]) then continue
    url = (parameters[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  ; search keywords
  keywords = self.keywords->get(/all, count=nkeywords)
  for i = 0L, nkeywords - 1L do begin
    if (obj_valid(exclude) && exclude eq keywords[i]) then continue
    url = (keywords[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  return, obj_valid(exclude) && exclude eq self.file $
            ? '' $
            : self.file->lookupName(name, exclude=self) 
end


;+
; Create a routine object.
;
; :Returns:
;    1 for success, 0 for failure
;
; :Params:
;    file : in, required, type=object
;       file tree object
;-
function doctreeroutine::init, file, system=system
  compile_opt strictarr, hidden
  
  self.file = file
  self.system = system
  
  self.categories = obj_new('MGcoArrayList', type=7, block_size=5)
  
  self.parameters = obj_new('MGcoArrayList', type=11, block_size=5)
  self.keywords = obj_new('MGcoArrayList', type=11, block_size=5)
  
  return, 1B
end


;+
; Define instance variables for routine class. 
;
; :Fields:
;    system
;       system object
;    file 
;       file object containing this routine
;    name
;       string name of this routine
;    isFunction
;       true if this routine is a function
;    isMethod
;       true if this routine is a method of a class
;    isAbstract
;       true if this routine is abstract (not implemented)
;    isObsolete
;       true if this routine is obsolete
;    isHidden
;       true if this routine hidden (i.e. not visible)
;    isPrivate
;       true if this routine is not visible to users (but visible to 
;       developers)
;    nLines
;       number of lines in the routine 
;    parameters
;       list of parameter objects
;    keywords
;       list of keyword objects
;    comments
;       tree node hierarchy
;    firstline
;       first line in first paragraph of routine comments
;    returns
;       markup tree representing return value for functions
;    categories
;       array list of strings indicating categories for routine
;    examples
;       markup tree representing example usage of the routine
;    hasOthers
;       true if it has one of the "other" attributes: bugs, pre, post, uses, 
;       requires, customerId, todo, or restrictions
;    bugs
;       markup tree representing known bugs for the routine
;    pre
;       markup tree representing pre-condition for the routine
;    post
;       markup tree representing post-condition for the routine
;    uses
;       markup tree representing what other routines, classes, etc. are used
;       by the routine
;    requires
;       markup tree representing IDL requirements for the routine
;    customerId
;       markup tree representing customer identification
;    todo
;       markup tree representing todo items for the routine
;    restrictions
;       markup tree representing routine restrictions
;    documentationLevel
;       level of documentation for the routine: 0 (none), 1 (partial), 2 (fully)
;-
pro doctreeroutine__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeRoutine, $
             system: obj_new(), $
             file: obj_new(), $
             
             name: '', $
             isFunction: 0B, $
             isMethod: 0B, $
             isAbstract: 0B, $
             isObsolete: 0B, $
             isHidden: 0B, $
             isPrivate: 0B, $
             nLines: 0L, $
             complexity: 0L, $
             
             parameters: obj_new(), $
             keywords: obj_new(), $
             acceptsMaxParams: 0B, $
             
             comments: obj_new(), $
             firstline: obj_new(), $
             returns: obj_new(), $

             categories: obj_new(), $
             examples: obj_new(), $
             
             hasAuthorInfo: 0B, $
             author: obj_new(), $
             copyright: obj_new(), $
             history: obj_new(), $
             version: obj_new(), $
                          
             hasOthers: 0B, $
             bugs: obj_new(), $
             pre: obj_new(), $
             post: obj_new(), $     
             uses: obj_new(), $       
             requires: obj_new(), $ 
             customerId: obj_new(), $
             todo: obj_new(), $
             restrictions: obj_new(), $        
             
             documentationLevel: 0L $ 
           }
end