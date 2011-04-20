; docformat = 'rst'

;+
; Represents a directory.
; 
; :Properties:
;    files
;       .sav/.pro/.idldoc files in directory
;    location
;       location of the directory relative to the ROOT (w/ trailing slash)
;    overview_comments
;       markup comment tree representing the overview comments for the 
;       directory
;    system
;       system object
;    url
;       location of the directory as an URL
;    private
;       set to make the directory hidden in user output
;    hidden
;       set to make the directory hidden in the output
;-


;+
; Get properties.
;-
pro doctreedirectory::getProperty, location=location, url=url, $
                                   is_private=isPrivate, is_hidden=isHidden
  compile_opt strictarr, hidden
  
  if (arg_present(location)) then location = self.location
  if (arg_present(url)) then url = self.url
  if (arg_present(isPrivate)) then isPrivate = self.isPrivate
  if (arg_present(isHidden)) then isHidden = self.isHidden
end


;+
; Set properties.
;-
pro doctreedirectory::setProperty, overview_comments=overviewComments, $
                                   comments=comments, $
                                   is_private=isPrivate, is_hidden=isHidden, $
                                   author=author, copyright=copyright, $
                                   history=history
  compile_opt strictarr, hidden

  if (n_elements(overviewComments) gt 0) then begin
    if (obj_valid(self.overviewComments)) then begin
      parent = obj_new('MGtmTag')
      parent->addChild, self.overviewComments
      parent->addChild, overviewComments
      self.overviewComments = parent
    endif else self.overviewComments = overviewComments
  endif

  if (n_elements(comments) gt 0) then begin
    if (obj_valid(self.comments)) then begin
      parent = obj_new('MGtmTag')
      parent->addChild, self.comments
      parent->addChild, comments
      self.comments = parent
    endif else self.comments = comments
  endif
    
  if (n_elements(isPrivate) gt 0L) then self.isPrivate = isPrivate
  if (n_elements(isHidden) gt 0L) then self.isHidden = isHidden

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
function doctreedirectory::getVariable, name, found=found
  compile_opt strictarr, hidden

  found = 1B
  case strlowcase(name) of
    'location' : return, self.location
    'url' : return, self.url
    'relative_root' : begin
        if (self.location eq '.' + path_sep()) then return, ''
        dummy = strsplit(self.location, path_sep(), count=nUps)
        return, strjoin(replicate('../', nUps))
      end
      
    'has_overview_comments': return, obj_valid(self.overviewComments) || obj_valid(self.comments)
    'overview_comments': begin
        if (obj_valid(self.overviewComments)) then begin
          return, self.system->processComments(self.overviewComments)
        endif

        if (obj_valid(self.comments)) then begin
          firstLineTree = mg_tm_firstline(self.comments)
          comments = self.system->processComments(firstLineTree)
          obj_destroy, firstLineTree
          return, comments
        endif
                
        return, ''
      end
    'system_overview_comments': begin
        if (obj_valid(self.systemOverviewComments)) then begin
          return, self.system->processComments(self.systemOverviewComments)
        endif

        if (obj_valid(self.systemComments)) then begin
          firstLineTree = mg_tm_firstline(self.systemComments)
          comments = self.system->processComments(firstLineTree)
          obj_destroy, firstLineTree
          return, comments
        endif
                
        return, ''
      end      
    'has_comments': return, obj_valid(self.comments) || obj_valid(self.overviewComments)
    'comments': begin
        if (obj_valid(self.comments)) then begin
          return, self.system->processComments(self.comments)
        endif
        
        if (obj_valid(self.overviewComments)) then begin
          return, self.system->processComments(self.overviewComments)
        endif
        
        return, obj_new()
      end
    'comments_first_line': begin
        comments = obj_valid(self.comments) $
                     ? self.comments $
                     : (obj_valid(self.overviewComments) $
                          ? self.overviewComments $
                          : obj_new()) 

        if (~obj_valid(comments)) then return, ''
        
        firstline = mg_tm_firstline(comments)
        text_firstline = self.system->processComments(firstline)
        obj_destroy, firstline
        
        return, text_firstline
      end 
    'n_pro_files' : return, self.proFiles->count()
    'pro_files' : return, self.proFiles->get(/all)
    'n_visible_pro_files': begin
        nVisible = 0L
        for f = 0L, self.proFiles->count() - 1L do begin
          file = self.proFiles->get(position=f)          
          nVisible += file->isVisible()          
        endfor
        return, nVisible
      end
    'visible_pro_files': begin        
        files = self.proFiles->get(/all, count=nFiles)
        if (nFiles eq 0L) then return, -1L
        
        isVisibleFiles = bytarr(nFiles)
        for f = 0L, nFiles - 1L do begin
          isVisibleFiles[f] = files[f]->isVisible()
        endfor
        
        ind = where(isVisibleFiles eq 1B, nVisibleFiles)
        if (nVisibleFiles eq 0L) then return, -1L
        
        return, files[ind]
      end
      
    'n_dlm_files' : return, self.dlmFiles->count()
    'dlm_files' : return, self.dlmFiles->get(/all)
    'n_visible_dlm_files': begin
        nVisible = 0L
        for f = 0L, self.dlmFiles->count() - 1L do begin
          file = self.dlmFiles->get(position=f)          
          nVisible += file->isVisible()          
        endfor
        return, nVisible
      end
    'visible_dlm_files': begin        
        files = self.dlmFiles->get(/all, count=nFiles)
        if (nFiles eq 0L) then return, -1L
        
        isVisibleFiles = bytarr(nFiles)
        for f = 0L, nFiles - 1L do begin
          isVisibleFiles[f] = files[f]->isVisible()
        endfor
        
        ind = where(isVisibleFiles eq 1B, nVisibleFiles)
        if (nVisibleFiles eq 0L) then return, -1L
        
        return, files[ind]
      end
          
    'n_sav_files' : return, self.savFiles->count()
    'sav_files' : return, self.savFiles->get(/all)
    'n_visible_sav_files': begin
        nVisible = 0L
        for f = 0L, self.savFiles->count() - 1L do begin
          file = self.savFiles->get(position=f)          
          nVisible += file->isVisible()          
        endfor
        return, nVisible
      end
    'visible_sav_files': begin        
        files = self.savFiles->get(/all, count=nFiles)
        if (nFiles eq 0L) then return, -1L
        
        isVisibleFiles = bytarr(nFiles)
        for f = 0L, nFiles - 1L do begin
          isVisibleFiles[f] = files[f]->isVisible()
        endfor
        
        ind = where(isVisibleFiles eq 1B, nVisibleFiles)
        if (nVisibleFiles eq 0L) then return, -1L
        
        return, files[ind]
      end
          
    'n_idldoc_files' : return, self.idldocFiles->count()
    'idldoc_files' : return, self.idldocFiles->get(/all)
    'n_visible_idldoc_files': begin
        nVisible = 0L
        for f = 0L, self.idldocFiles->count() - 1L do begin
          file = self.idldocFiles->get(position=f)          
          nVisible += file->isVisible()          
        endfor
        return, nVisible
      end
    'visible_idldoc_files': begin        
        files = self.idldocFiles->get(/all, count=nFiles)
        if (nFiles eq 0L) then return, -1L
        
        isVisibleFiles = bytarr(nFiles)
        for f = 0L, nFiles - 1L do begin
          isVisibleFiles[f] = files[f]->isVisible()
        endfor
        
        ind = where(isVisibleFiles eq 1B, nVisibleFiles)
        if (nVisibleFiles eq 0L) then return, -1L
        
        return, files[ind]
      end
          
    'fullname' : return, strjoin(strsplit(self.location, path_sep(), /extract), '.')

    'has_author_info': return, self.hasAuthorInfo
    
    'has_author': return, obj_valid(self.author)
    'author': return, self.system->processComments(self.author)
    'plain_author': return, self.system->processPlainComments(self.author)

    'has_copyright': return, obj_valid(self.copyright)
    'copyright': return, self.system->processComments(self.copyright)
    
    'has_history': return, obj_valid(self.history)
    'history': return, self.system->processComments(self.history)
        
    'index_name': return, self.location
    'index_type': return, 'directory'
    'index_url': return, self.url + 'dir-overview.html'
    
    else: begin
        ; search in the system object if the variable is not found here
        var = self.system->getVariable(name, found=found)
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
;    
; :Keywords:
;    no_check_children : in, optional, type=boolean
;       set to not check children, i.e., .pro files, .sav files, etc.
;-
function doctreedirectory::isVisible, no_check_children=noCheckChildren
  compile_opt strictarr, hidden
  
  if (self.isHidden) then return, 0B
  
  ; if creating user-level docs and private then not visible
  self.system->getProperty, user=user
  if (self.isPrivate && user) then return, 0B
  
  if (keyword_set(noCheckChildren)) then return, 1
  
  ; not visible if nothing in it that is visible
  
  for p = 0L, self.profiles->count() - 1L do begin
    profile = self.profiles->get(position=p)
    if (profile->isVisible()) then return, 1B
  endfor
  
  for s = 0L, self.savfiles->count() - 1L do begin
    savfile = self.savfiles->get(position=s)
    if (savfile->isVisible()) then return, 1B
  endfor

  for d = 0L, self.dlmfiles->count() - 1L do begin
    dlmfile = self.dlmfiles->get(position=d)
    if (dlmfile->isVisible()) then return, 1B
  endfor

  for i = 0L, self.idldocfiles->count() - 1L do begin
    idldocfile = self.idldocfiles->get(position=i)
    if (idldocfile->isVisible()) then return, 1B
  endfor
      
  return, 0B
end


;+
; Handle .idldoc file in directory.
;-
pro doctreedirectory::_handleDirOverview
  ; look for a .idldoc file in this directory
  self.system->getProperty, root=root
  dirOverviewFile = root + self.location + '.idldoc'
  if (file_test(dirOverviewFile)) then begin
    nLines = file_lines(dirOverviewFile)
    if (nLines le 0) then return
    
    lines = strarr(nLines)
    openr, lun, dirOverviewFile, /get_lun
    readf, lun, lines
    free_lun, lun
    
    self.system->getProperty, format=format, markup=markup
    formatParser = self.system->getParser(format + 'format')
    markupParser = self.system->getParser(markup + 'markup')
    
    formatParser->parseDirOverviewComments, lines, $
                                            directory=self, $
                                            markup_parser=markupParser
  endif 
end


;+
; Do any analysis necessary on information gathered during the "parseTree"
; phase.
;-
pro doctreedirectory::process
  compile_opt strictarr, hidden
  
  for f = 0L, self.proFiles->count() - 1L do begin
    file = self.proFiles->get(position=f)
    file->process
  endfor  
end


;+
; Generate all the output for the directory.
;
; :Params: 
;    outputRoot : in, required, type=string
;       output root directory (w/ trailing slash)
;-
pro doctreedirectory::generateOutput, outputRoot
  compile_opt strictarr, hidden
  on_error, 2
  
  if (~self->isVisible()) then return
  
  self.system->print, 'Generating output for ' + self.location + '...'

  ; create directory in the output if necessary
  outputDir = outputRoot + self.location
  if (~file_test(outputDir)) then begin
    self.system->makeDirectory, outputDir, error=error
    if (error ne 0L) then begin
      self.system->error, 'unable to make directory ' + outputDir
    endif
  endif
    
  ; generate docs for each .pro/.dlm/.sav/.idldoc file in directory
  for f = 0L, self.proFiles->count() - 1L do begin
    file = self.proFiles->get(position=f)
    file->generateOutput, outputRoot, self.location
  endfor

  for f = 0L, self.dlmFiles->count() - 1L do begin
    file = self.dlmFiles->get(position=f)
    file->generateOutput, outputRoot, self.location
  endfor
  
  for f = 0L, self.savFiles->count() - 1L do begin
    file = self.savFiles->get(position=f)
    file->generateOutput, outputRoot, self.location
  endfor
  
  for f = 0L, self.idldocFiles->count() - 1L do begin
    file = self.idldocFiles->get(position=f)
    file->generateOutput, outputRoot, self.location
  endfor
  
  self.system->getProperty, extension=outputExtension
  ; generate directory overview
  dirOverviewFilename = filepath('dir-overview.' + outputExtension, root=outputDir)
  dirOverviewTemplate = self.system->getTemplate('dir-overview')
  dirOverviewTemplate->reset
  dirOverviewTemplate->process, self, dirOverviewFilename
    
  ; generate file listing
  listingFilename = filepath('dir-files.' + outputExtension, root=outputDir)
  listingTemplate = self.system->getTemplate('file-listing')
  listingTemplate->reset
  listingTemplate->process, self, listingFilename
end


;+
; Fill the links in comments for a directory.
;-
pro doctreedirectory::fillLinks
  compile_opt strictarr
  
  if (obj_isa(self.overviewComments, 'MGtmNode')) then begin
    self.systemOverviewComments = self.overviewComments->_clone()
    doctree_fill_links, self.overviewComments, self
    doctree_fill_links, self.systemOverviewComments, self.system
  endif else begin
    if (obj_isa(self.comments, 'MGtmNode')) then begin
      self.systemComments = self.comments->_clone()
      doctree_fill_links, self.systemComments, self.system
    endif    
  endelse
  
  doctree_fill_links, self.comments, self
  
  proFiles = self.proFiles->get(/all, count=nproFiles)
  for i = 0L, nproFiles - 1L do (proFiles[i])->fillLinks
  
  dlmFiles = self.dlmFiles->get(/all, count=ndlmFiles)
  for i = 0L, ndlmFiles - 1L do (dlmFiles[i])->fillLinks

  savFiles = self.savFiles->get(/all, count=nsavFiles)
  for i = 0L, nsavFiles - 1L do (savFiles[i])->fillLinks

  idldocFiles = self.idldocFiles->get(/all, count=nidldocFiles)
  for i = 0L, nidldocFiles - 1L do (idldocFiles[i])->fillLinks
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
function doctreedirectory::lookupName, name, exclude=exclude
  compile_opt strictarr
  
  if (name eq self.location) then return, self->getVariable('index_url')
  
  proFiles = self.proFiles->get(/all, count=nproFiles) 
  for i = 0L, nproFiles - 1L do begin
    if (obj_valid(exclude) && exclude eq proFiles[i]) then continue
    url = (proFiles[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
    
  dlmFiles = self.dlmFiles->get(/all, count=ndlmFiles) 
  for i = 0L, ndlmFiles - 1L do begin
    if (obj_valid(exclude) && exclude eq dlmFiles[i]) then continue
    url = (dlmFiles[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  savFiles = self.savFiles->get(/all, count=nsavFiles) 
  for i = 0L, nsavFiles - 1L do begin
    if (obj_valid(exclude) && exclude eq savFiles[i]) then continue
    url = (savFiles[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  idldocFiles = self.idldocFiles->get(/all, count=nidldocFiles) 
  for i = 0L, nidldocFiles - 1L do begin
    if (obj_valid(exclude) && exclude eq idldocFiles[i]) then continue
    url = (idldocFiles[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
      
  return, obj_valid(exclude) && exclude eq self.system $
            ? '' $
            : self.system->lookupName(name, exclude=self)
end


;+
; Free resources, including items lower in the hierarchy
;-
pro doctreedirectory::cleanup
  compile_opt strictarr, hidden
  
  obj_destroy, [self.overviewComments, self.systemOverviewComments]
  obj_destroy, [self.systemComments, self.comments]
  obj_destroy, [self.author, self.copyright, self.history]
  obj_destroy, [self.proFiles, self.dlmFiles, self.savFiles, self.idldocFiles]
end


;+
; Create a directory object.
;
; :Returns: 
;    1 for success, 0 for failure
;-
function doctreedirectory::init, location=location, files=files, system=system
  compile_opt strictarr, hidden
  
  self.location = location
  self.system = system
  
  self.system->getProperty, index_level=indexLevel
  if (indexLevel ge 1L) then self.system->createIndexEntry, self.location, self
  
  self.proFiles = obj_new('MGcoArrayList', type=11, block_size=10)
  self.dlmFiles = obj_new('MGcoArrayList', type=11, block_size=5)
  self.savFiles = obj_new('MGcoArrayList', type=11, block_size=5)  
  self.idldocFiles = obj_new('MGcoArrayList', type=11, block_size=4)
  
  self.url = strjoin(strsplit(self.location, path_sep(), /extract), '/') + '/'
  
  self.system->getProperty, root=root  

  self->_handleDirOverview
  if (~self->isVisible(/no_check_children)) then return, 1
  
  self.system->print, 'Parsing ' + self.location + '...'
  
  for f = 0L, n_elements(files) - 1L do begin
    dotpos = strpos(files[f], '.', /reverse_search)
    extension = strmid(files[f], dotpos + 1L)
    case strlowcase(extension) of
      'pro': begin
          proFileParser = self.system->getParser('profile')
          file = proFileParser->parse(root + files[f], directory=self)
          self.proFiles->add, file
        end
      'dlm': begin
          file = obj_new('DOCtreeDLMFile', $
                         basename=file_basename(files[f]), $
                         directory=self, $
                         system=self.system)
          self.dlmFiles->add, file
        end
      'sav': begin
          file = obj_new('DOCtreeSavFile', $
                         basename=file_basename(files[f]), $
                         directory=self, $
                         system=self.system)
          self.savFiles->add, file
        end
      'idldoc': begin
          idldocFileParser = self.system->getParser('idldocfile')
          file = idldocFileParser->parse(root + files[f], directory=self)
          self.idldocFiles->add, file
        end                
    endcase
  endfor
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    system
;       system object
;    location
;       location of the directory relative to the ROOT (w/ trailing slash)
;    url
;       location of the directory relative to the ROOT as an URL (w/ trailing 
;       slash) 
;    overviewComments
;       markup tree representing the overview comments for the directory found
;       in the OVERVIEW file
;    comments
;       markup tree representing the comments for the directory found in an
;       .idldoc file
;    proFiles
;       array list of .pro file objects
;    savFiles
;       array list of .sav file objects
;    idldocFiles
;       array list of .idldoc file objects
;-
pro doctreedirectory__define
  compile_opt strictarr, hidden
  
  define = { DOCtreeDirectory, $
             system: obj_new(), $
             location: '', $
             url: '', $
             
             isPrivate: 0B, $
             isHidden: 0B, $
             
             hasAuthorInfo: 0B, $             
             author: obj_new(), $
             copyright: obj_new(), $
             history: obj_new(), $
                          
             overviewComments: obj_new(), $
             systemOverviewComments: obj_new(), $
             comments: obj_new(), $
             systemComments: obj_new(), $
             
             proFiles: obj_new(), $
             dlmFiles: obj_new(), $
             savFiles: obj_new(), $
             idldocFiles: obj_new() $
           }
end