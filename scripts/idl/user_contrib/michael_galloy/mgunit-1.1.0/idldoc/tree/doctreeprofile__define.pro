; docformat = 'rst'

;+
; This class represents a information about .pro file.
;
; :Properties:
;    basename : get, set, type=string
;       basename of filename
;    has_main_level : get, set, type=boolean
;       true if the file has a main-level program at the end
;    is_batch : get, set, type=boolean
;       true if the file is a batch file
;    comments : get, set, type=object
;       text tree hierarchy for file level comments
;    n_routines : get, type=integer
;       number of routines in the file
;    routines : get, type=object
;       list object containing routine objects in file
;-


;+
; Get properties.
;-
pro doctreeprofile::getProperty, basename=basename, $
                                 has_main_level=hasMainLevel, $
                                 is_batch=isBatch, $
                                 has_class=hasClass, classes=classes, $
                                 comments=comments, $
                                 n_routines=nRoutines, routines=routines, $
                                 n_lines=nLines, directory=directory                                 
  compile_opt strictarr, hidden
  
  if (arg_present(basename)) then basename = self.basename
  if (arg_present(directory)) then directory = self.directory
  if (arg_present(hasMainLevel)) then hasMainLevel = self.hasMainLevel
  if (arg_present(isBatch)) then isBatch = self.isBatch 
  if (arg_present(hasClass)) then hasClass = self.classes->count() gt 0   
  if (arg_present(classes)) then classes = self.classes  
  if (arg_present(comments)) then comments = self.comments
  if (arg_present(nRoutines)) then nRoutines = self.routines->count()
  if (arg_present(routines)) then routines = self.routines
  if (arg_present(nLines)) then nLines = self.nLines  
end


;+
; Set properties.
;-
pro doctreeprofile::setProperty, code=code, has_main_level=hasMainLevel, $
                                 is_hidden=isHidden, is_private=isPrivate, $
                                 is_batch=isBatch, $
                                 is_abstract=isAbstract, $
                                 is_obsolete=isObsolete, $
                                 comments=comments, $
                                 modification_time=mTime, n_lines=nLines, $ 
                                 format=format, markup=markup, $
                                 examples=examples, $
                                 author=author, copyright=copyright, $
                                 history=history, version=version, $
                                 bugs=bugs, $
                                 customer_id=customerId, $
                                 requires=requires, $
                                 restrictions=restrictions, $
                                 todo=todo, $
                                 uses=uses                                 
  compile_opt strictarr, hidden
  
  if (n_elements(code) gt 0) then begin
    ; translate strarr to parse tree, also mark comments
    
    proFileParser = self.system->getParser('profile')
    self.code = obj_new('MGtmTag')
    
    nameRe = '^[[:space:]]*(pro|function)[[:space:]]([^ ,]*)'
    
    for l = 0L, n_elements(code) - 1L do begin
      line = code[l]
      lessPos = strpos(line, '<')
      while (lessPos ne -1) do begin
        case lessPos of
          0: line = '&lt;' + strmid(line, 1)
          strlen(line) - 1L: line = strmid(line, 0, strlen(line) - 1L) + '&lt;'
          else: line = strmid(line, 0L, lessPos) + '&lt;' + strmid(line, lessPos + 1L)
        endcase
        lessPos = strpos(line, '<')
      endwhile
      
      regularLines = proFileParser->_stripComments(line, comments=commentsLines)
            
      if (stregex(line, nameRe, /boolean)) then begin        
        tokens = stregex(line, nameRe, /subexpr, /extract) 
        
        anchor = obj_new('MGtmTag', type='anchor')
        anchor->addAttribute, 'identifier', tokens[2] + ':source'
         
        self.code->addChild, anchor        
      endif
            
      self.code->addChild, obj_new('MGtmText', text=regularLines)
      
      if (strlen(commentsLines) gt 0) then begin
        commentsNode = obj_new('MGtmTag', type='comments')
        self.code->addChild, commentsNode
        commentsNode->addChild, obj_new('MGtmText', text=commentsLines)
      endif
      
      self.code->addChild, obj_new('MGtmTag', type='newline')
    endfor  
  endif
  
  if (n_elements(isHidden) gt 0) then self.isHidden = isHidden
  if (n_elements(isPrivate) gt 0) then self.isPrivate = isPrivate
  
  if (n_elements(hasMainLevel) gt 0) then self.hasMainLevel = hasMainLevel
  if (n_elements(isAbstract) gt 0) then self.isAbstract = isAbstract
  if (n_elements(isBatch) gt 0) then self.isBatch = isBatch
  if (n_elements(isObsolete) gt 0) then self.isObsolete = isObsolete
  if (n_elements(comments) gt 0) then begin
    if (obj_valid(self.comments)) then begin
      parent = obj_new('MGtmTag')
      parent->addChild, self.comments
      parent->addChild, comments
      self.comments = parent
    endif else self.comments = comments
  endif
  if (n_elements(format) gt 0) then self.format = format
  if (n_elements(markup) gt 0) then self.markup = markup
  if (n_elements(nLines) gt 0) then self.nLines = nLines
  if (n_elements(mTime) gt 0) then self.modificationTime = mTime
  
  if (n_elements(examples) gt 0) then self.examples = examples
  
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

  if (n_elements(bugs) gt 0) then begin
    self.hasOthers = 1B
    self.bugs = bugs
  endif 
    
  if (n_elements(customerId) gt 0) then begin
    self.hasOthers = 1B    
    self.customerId = customerId
  endif  

  if (n_elements(requires) gt 0) then begin
    self.hasOthers = 1B
    self.requires = requires
  endif 
    
  if (n_elements(restrictions) gt 0) then begin
    self.hasOthers = 1B
    self.restrictions = restrictions
  endif

  if (n_elements(todo) gt 0) then begin
    self.hasOthers = 1B
    self.todo = todo
  endif 
      
  if (n_elements(uses) gt 0) then begin
    self.hasOthers = 1B
    self.uses = uses
  endif  
end


;+
; Get a class of the given name if defined in the file or system already, create 
; it if not.
;
; :Returns: 
;    class tree object
;
; :Params:
;    classname : in, required, type=string
;       classname of the class
;-
function doctreeprofile::getClass, classname
  compile_opt strictarr, hidden
  
  ; first, check the file: if it's there, everything is set up already
  for c = 0L, self.classes->count() - 1L do begin
    class = self.classes->get(position=c)
    class->getProperty, classname=checkClassname
    if (strlowcase(classname) eq strlowcase(checkClassname)) then return, class
  endfor

  ; next, check the system for the class, it may have been referenced by 
  ; another class in another file
  self.system->getProperty, classes=classes
  class = classes->get(strlowcase(classname), found=found)
  if (found) then begin
    class->getProperty, pro_file=proFile
    
    if (~obj_valid(proFile)) then begin
      class->setProperty, pro_file=self, classname=classname
      self.classes->add, class
    endif
    
    return, class
  endif
    
  ; create the class if there is no record of it
  class = obj_new('DOCtreeClass', classname, pro_file=self, system=self.system)
  self.classes->add, class
  
  return, class
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
function doctreeprofile::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  case strlowcase(name) of
    'basename': return, self.basename
    'local_url': begin
        self.system->getProperty, extension=ext
        return, file_basename(self.basename, '.pro') + '.' + ext
      end
    'source_url': begin
        self.system->getProperty, extension=ext      
        return, file_basename(self.basename, '.pro') + '-code.' + ext
      end
    'direct_source_url': begin
        self.system->getProperty, source_link=sourceLink, output=output
        case sourceLink of
          0: return, file_basename(self.basename)   ; URL to copy
          1: begin   ; relative URL               
               self.directory->getProperty, url=url
               return, mg_relative_path(output + url, self.fullpath, /web)
             end
          2: begin   ; absolute URL
               abspath = file_expand_path(self.fullpath)
               pathTokens = strsplit(abspath, path_sep(), /extract, /preserve_null)
               return, 'file://' + strjoin(pathTokens, '/')
             end
          else:
        endcase        
      end
    'output_path': begin      
         self.directory->getProperty, url=url     
         self.system->getProperty, extension=ext
         return, url + file_basename(self.basename, '.pro') + '.' + ext
       end
    'code': return, self.system->processComments(self.code)
    
    'is_batch': return, self.isBatch
    'has_main_level': return, self.hasMainLevel
    'has_class': return, self.classes->count() gt 0
    'classes': return, self.classes->get(/all)
    'is_private': return, self.isPrivate
    'is_abstract': return, self.isAbstract
    'is_obsolete': return, self.isObsolete
    
    'modification_time': return, self.modificationTime
    'n_lines': return, mg_int_format(self.nLines)
    
    'format': return, self.format
    'markup': return, self.markup

    'has_categories': return, self.categories->count() gt 0
    'categories': return, self.categories->get(/all)
    
    'has_examples': return, obj_valid(self.examples)
    'examples': return, self.system->processComments(self.examples) 
        
    'has_comments': return, obj_valid(self.comments)
    'comments': return, self.system->processComments(self.comments)       
    'comments_first_line': begin
        ; if no file comments, but there is only one routine then return the
        ; first line of the routine's comments           
        if (~obj_valid(self.comments)) then begin
          filename = strlowcase(strmid(self.basename, 0, strpos(self.basename, '.')))
          for r = 0L, self.routines->count() - 1L do begin
            routine = self.routines->get(position=r)
            routine->getProperty, name=routineName
            if (strlowcase(routineName) eq filename) then begin
              return, routine->getVariable('comments_first_line', found=found)
            endif
          endfor
          
          return, ''
        endif
        
        if (~obj_valid(self.firstline)) then begin
          self.firstline = mg_tm_firstline(self.comments)
        endif
        
        return, self.system->processComments(self.firstline)      
      end
    'plain_comments': return, self.system->processPlainComments(self.comments)
                
    'n_routines' : return, self.routines->count()
    'routines' : return, self.routines->get(/all)
    'n_visible_routines': begin
        nVisible = 0L
        for r = 0L, self.routines->count() - 1L do begin
          routine = self.routines->get(position=r)          
          nVisible += routine->isVisible()          
        endfor
        return, nVisible
      end
    'visible_routines': begin        
        routines = self.routines->get(/all, count=nRoutines)
        if (nRoutines eq 0L) then return, -1L
        
        isVisibleRoutines = bytarr(nRoutines)
        for r = 0L, nRoutines - 1L do begin
          isVisibleRoutines[r] = routines[r]->isVisible()
        endfor
        
        ind = where(isVisibleRoutines eq 1B, nVisibleRoutines)
        if (nVisibleRoutines eq 0L) then return, -1L
        
        return, routines[ind]
      end
    
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

    'index_name': return, self.basename
    'index_type': begin
        location = self.directory->getVariable('location')
        
        type_tree = obj_new('MGtmTag')
        type_tree->addChild, obj_new('MGtmText', text='.pro file in ')
        link_node = obj_new('MGtmTag', type='link')
        link_node->addAttribute, 'reference', self.directory->getVariable('index_url')
        link_node->addChild, obj_new('MGtmText', text=location + ' directory')
        type_tree->addChild, link_node
        comments = self.system->processComments(type_tree)
        obj_destroy, type_tree

        return, comments
      end      
    'index_url': begin
        self.directory->getProperty, url=dirUrl
        return, dirUrl + file_basename(self.basename, '.pro') + '.html'
      end
            
    'has_others': return, self.hasOthers

    'has_bugs': return, obj_valid(self.bugs)
    'bugs': return, self.system->processComments(self.bugs)
        
    'has_customer_id': return, obj_valid(self.customerId)
    'customer_id': return, self.system->processComments(self.customerId)
    
    'has_requires': return, obj_valid(self.requires)
    'requires': return, self.system->processComments(self.requires)
        
    'has_restrictions': return, obj_valid(self.restrictions)
    'restrictions': return, self.system->processComments(self.restrictions)

    'has_todo': return, obj_valid(self.todo)
    'todo': return, self.system->processComments(self.todo)
        
    'has_uses': return, obj_valid(self.uses)
    'uses': begin
        self.directory->getProperty, url=dirUrl
        if (dirUrl eq './') then begin
          root = '.'
        endif else begin
          dummy = strsplit(dirUrl, '/', count=ndirs)
          root = strjoin(strarr(ndirs) + '..', '/')
        endelse
        return, self.system->processUses(self.uses, root=root)
      end

    'plain_attributes': begin
        attributes = [self.bugs, self.version, self.history, self.copyright, $
                      self.examples, self.customerId, self.requires, $
                      self.restrictions, self.todo, self.uses]
        
        result = ''
        for a = 0L, n_elements(attributes) - 1L do begin
          result += strjoin(self.system->processPlainComments(attributes[a]), ' ')
        endfor

        return, result
      end
          
    else: begin
        ; search in the system object if the variable is not found here
        var = self.directory->getVariable(name, found=found)
        if (found) then return, var
        
        found = 0B
        return, -1L
      end
  endcase
end


;+
; Uses file hidden/private attributes and system wide user/developer level to
; determine if this file should be visible.
;
; :Returns: 
;    boolean
;-
function doctreeprofile::isVisible
  compile_opt strictarr, hidden
  
  if (self.isHidden) then return, 0B
  
  ; if creating user-level docs and private then not visible
  self.system->getProperty, user=user
  if (self.isPrivate && user) then return, 0B
  
  if (~self.directory->isVisible(/no_check_children)) then return, 0B
  
  return, 1B
end


;+
; Add a routine to the list of routines in the file.
; 
; :Params:
;    routine : in, required, type=object
;       routine object
;-
pro doctreeprofile::addRoutine, routine
  compile_opt strictarr, hidden
  
  self.routines->add, routine
end


;+
; Add property comments to the appropriate keyword if there are no keyword 
; comments.
;
; :Params:
;    methodname : in, required, type=string
;       method to check for keywords; either getProperty, setProperty, init
;    propertyname : in, required, type=string
;       name of the property
;    comments : in, required, type=object
;       property comments
;    class : in, required, type=object
;       class tree object in which to check properties
;-
pro doctreeprofile::_propertyCheck, methodname, propertyname, comments, class
  compile_opt strictarr, hidden
  
  class->getProperty, classname=classname
  
  for r = 0L, self.routines->count() - 1L do begin
    routine = self.routines->get(position=r)    
    routine->getProperty, name=name
    
    if (strlowcase(name) eq strlowcase(classname + '::' + methodname)) then begin
      routine->getProperty, keywords=keywords
      for k = 0L, keywords->count() - 1L do begin
        keyword = keywords->get(position=k)
        keyword->getProperty, name=keywordname, comments=keywordComments
        
        if (strlowcase(propertyname) eq strlowcase(keywordname)) then begin
          if (~obj_valid(keywordComments)) then begin
            keyword->setProperty, comments=comments
          endif
        endif
      endfor
      break
    endif
  endfor
end


;+
; Add a category name to the file.
;
; :Params:
;    name : in, required, type=string
;       name of category to add to this file
;-
pro doctreeprofile::addCategory, name
  compile_opt strictarr, hidden

  self.categories->add, name
end



;+
; Do any analysis necessary on information gathered during the "parseTree"
; phase.
;-
pro doctreeprofile::process
  compile_opt strictarr, hidden
  
  if (self.classes->count() gt 0) then begin
    ; if has properties, then place properties' comment into keyword comments
    ; for getProperty, setProperty, and init if there are no comments there
    ; already
    
    for c = 0L, self.classes->count() - 1L do begin      
      class = self.classes->get(position=c)
      class->getProperty, properties=properties
      propertyNames = properties->keys(count=nProperties)
      
      for p = 0L, nProperties - 1L do begin
        property = properties->get(propertyNames[p])
        property->getProperty, is_get=isGet, is_set=isSet, is_init=isInit, $
                               comments=comments
        
        if (isGet) then begin
          self->_propertyCheck, 'getProperty', propertyNames[p], comments, class
        endif
        
        if (isSet) then begin
          self->_propertyCheck, 'setProperty', propertyNames[p], comments, class
        endif
        
        if (isInit) then begin
          self->_propertyCheck, 'init', propertyNames[p], comments, class
        endif
      endfor
    endfor
  endif
  
  for r = 0L, self.routines->count() - 1L do begin
    routine = self.routines->get(position=r)
    routine->markArguments
    routine->checkDocumentation
        
    routine->getProperty, name=routineName
    if (routine->isVisible()) then begin
      self.system->addVisibleRoutine, routineName, routine
    endif
  endfor
end


pro doctreeprofile::addImageRef, filename
  compile_opt strictarr, hidden
  
  self.imagerefs->add, filename
end


;+
; Generate output documenting the .pro file.
;
; :Params:
;    outputRoot : in, required, type=string
;       absolute path to the output root directory
;    directory : in, required, type=string
;       directory name relative to the root for the .pro file
;-
pro doctreeprofile::generateOutput, outputRoot, directory
  compile_opt strictarr, hidden
  
  ; don't produce output if not visible
  if (~self->isVisible()) then return
  
  self.system->print, '  Generating output for ' + self.basename + '...'
  self.system->getProperty, extension=outputExtension
  
  proFileTemplate = self.system->getTemplate('profile')
  
  outputDir = outputRoot + directory
  outputFilename = outputDir + file_basename(self.basename, '.pro') + '.' + outputExtension
  
  proFileTemplate->reset
  proFileTemplate->process, self, outputFilename  

  self.system->getProperty, comment_style=commentStyle

  ; copy images references in the documentation
  for i = 0L, self.imagerefs->count() - 1L do begin
    path = file_dirname(self.fullpath, /mark_directory)
    filename = self.imagerefs->get(position=i)

    ; if extension of imageref is svg, then look for a PDF file if 
    ; commentstyle is LaTeX
    if (commentStyle eq 'latex') then begin
      dotpos = strpos(filename, '.', /reverse_search)
      ext = strmid(filename, dotpos + 1L)
      if (strlowcase(ext) eq 'svg') then begin
        filename = strmid(filename, 0, dotpos) + '.pdf'
      endif
    endif
    
    if (file_test(path + filename)) then begin
      _outputDir = file_dirname(outputDir + filename)
      if (~file_test(_outputDir, /directory)) then file_mkdir, _outputDir

      file_copy, path + filename, outputDir + filename, /allow_same, /overwrite
    endif else begin
      self.system->warning, 'image at ' + path + filename + ' not found'
    endelse
  endfor
    
  self.system->getProperty, nosource=nosource
  if (nosource) then return
  
  ; chromocoded version of the source code
  sourceTemplate = self.system->getTemplate('source')
  
  outputFilename = outputDir + file_basename(self.basename, '.pro') + '-code.' + outputExtension
  
  sourceTemplate->reset
  sourceTemplate->process, self, outputFilename    
  
  ; direct version of the source code
  self.system->getProperty, source_link=sourceLink
  if (sourceLink eq 0) then begin
    file_copy, self.fullpath, outputDir + file_basename(self.basename), $
               /allow_same, /overwrite
  endif
end


;+
; Fill the links in comments for a .pro file.
;-
pro doctreeprofile::fillLinks
  compile_opt strictarr
  
  doctree_fill_links, self.comments, self
  doctree_fill_links, self.firstline, self
  doctree_fill_links, self.examples, self

  doctree_fill_links, self.author, self
  doctree_fill_links, self.copyright, self
  doctree_fill_links, self.history, self
  doctree_fill_links, self.version, self

  doctree_fill_links, self.bugs, self
  doctree_fill_links, self.customerId, self
  doctree_fill_links, self.restrictions, self
  doctree_fill_links, self.requires, self
  doctree_fill_links, self.todo, self
  doctree_fill_links, self.uses, self
                            
  routines = self.routines->get(/all, count=nroutines)
  for i = 0L, nroutines - 1L do (routines[i])->fillLinks
  
  classes = self.classes->get(/all, count=nclasses)
  for i = 0L, nclasses - 1L do (classes[i])->fillLinks
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
function doctreeprofile::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (name eq self.basename) then return, self->getVariable('index_url')

  ; check classes
  classes = self.classes->get(/all, count=nclasses) 
  for i = 0L, nclasses - 1L do begin
    if (obj_valid(exclude) && exclude eq classes[i]) then continue
    url = (classes[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
   
  ; check routines
  routines = self.routines->get(/all, count=nroutines) 
  for i = 0L, nroutines - 1L do begin
    if (obj_valid(exclude) && exclude eq routines[i]) then continue
    url = (routines[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
   
  return, obj_valid(exclude) && exclude eq self.directory $
            ? '' $
            : self.directory->lookupName(name, exclude=self)
end


;+
; Free resources.
;-
pro doctreeprofile::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, self.firstline
  obj_destroy, self.comments
  
  obj_destroy, self.routines
  
  obj_destroy, self.classes
  
  obj_destroy, [self.author, self.copyright, self.history, self.version]
  obj_destroy, self.code
  
  obj_destroy, self.examples
  
  obj_destroy, self.categories
  obj_destroy, [self.bugs, self.customerId]
  obj_destroy, [self.requires, self.restrictions, self.todo, self.uses]
  
  obj_destroy, self.imagerefs
end


;+
; Create file tree object.
;
; :Returns: 
;    1 for success, 0 for failure
;
; :Keywords:
;    basename : in, required, type=string
;       basename of the .pro file
;    directory : in, required, type=object
;       directory tree object
;    system : in, required, type=object
;       system object
;    fullpath : in, required, type=string
;       full filename of the .pro file
;-
function doctreeprofile::init, basename=basename, directory=directory, $
                               system=system, fullpath=fullpath
  compile_opt strictarr, hidden
  
  self.basename = basename
  self.directory = directory
  self.system = system
  self.fullpath = fullpath
  
  self.classes = obj_new('MGcoArrayList', type=11, block_size=3)
  self.routines = obj_new('MGcoArrayList', type=11, block_size=10)
  self.categories = obj_new('MGcoArrayList', type=7, block_size=3)
  self.imagerefs = obj_new('MGcoArrayList', type=7, block_size=3)

  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 1L) then self.system->createIndexEntry, self.basename, self
  
  self.system->print, '  Parsing ' + self.basename + '...'
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    directory
;       directory tree object
;    basename
;       basename of file
;    hasMainLevel
;       true if the file has a main level program at the end
;    isBatch
;       true if the file is a batch file
;    routines 
;       list of routine objects
;-
pro doctreeprofile__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeProFile, $
             system: obj_new(), $
             directory: obj_new(), $
             
             fullpath: '', $
             basename: '', $
             code: obj_new(), $
             
             hasMainLevel: 0B, $
             isBatch: 0B, $
             
             classes: obj_new(), $
             
             modificationTime: '', $
             nLines: 0L, $
             format: '', $
             markup: '', $
             
             comments: obj_new(), $
             firstline: obj_new(), $
             
             routines: obj_new(), $
             
             isAbstract: 0B, $
             isHidden: 0B, $
             isObsolete: 0B, $
             isPrivate: 0B, $
             
             examples: obj_new(), $
             
             hasAuthorInfo: 0B, $
             author: obj_new(), $
             copyright: obj_new(), $
             history: obj_new(), $
             version: obj_new(), $
             
             categories: obj_new(), $
             
             hasOthers: 0B, $
             bugs: obj_new(), $
             customerId: obj_new(), $
             restrictions: obj_new(), $
             requires: obj_new(), $
             todo: obj_new(), $
             uses: obj_new(), $
             
             imagerefs: obj_new() $             
           }
end