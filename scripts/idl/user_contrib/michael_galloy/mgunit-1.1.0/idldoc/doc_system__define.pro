; docformat = 'rst'

;+
; This class represents the entire IDLdoc run. All information/settings for 
; the run are stored (or at least accessible from) here.
;
; :Author:
;    Michael Galloy
;
; :Properties:
;    root
;       the directory containing the code to document
;    output
;       the directory to which to output the documentation
;    classes
;       hash table (classname -> `DOCtreeClass`) containing all class 
;       definitions
;    format
;       format style
;    markup
;       markup style
;    overview_comments
;       comment tree
;    overview
;       filename of overview file
;    directories
;       array list of directory objects
;    nosource
;       true if the source code is not to be copied into the output
;    user
;       true if user documentation is to be generated (as opposed to developer
;       level documentation)
;    quiet
;       true if only warnings should be printed
;    silent
;       true if nothing should be printed
;    n_warnings
;       number of warnings generated
;    log_file
;       filename of log file, '' if no log file is being used
;    assistant
;       set to produce output formatted for the IDL Assistant (obsolete)
;    embed
;    footer
;    title
;    subtitle
;    nonavbar
;    statistics
;    format_style
;    markup_style
;    comment_style
;    preformat
;    browse_routines
;    template_prefix
;    template_location
;    help
;    version
;    charset
;-


;+
; Get variables for use with templates.
;
; :Returns: variable
;
; :Params:
;    name : in, required, type=string
;       name of variable
;
; :Keywords:
;    found : out, optional, type=boolean
;       set to a named variable, returns if variable name was found
;-
function doc_system::getVariable, name, found=found
  compile_opt strictarr, hidden
  
  found = 1B
  case strlowcase(name) of
    'system': return, self
    
    'idldoc_version': return, self.idldoc_version
    'charset': return, self.charset
    'date': return, systime()
    'title': return, self.title
    'subtitle': return, self.subtitle
    'user': return, self.user
    'statistics': return, self.statistics
    'index_level': return, self.indexLevel
    
    'preformat': return, self.preformat
    'embed': return, self.embed
    'nonavbar': return, self.nonavbar
    'nosource': return, self.nosource
    
    'has_overview_comments': return, obj_valid(self.overviewComments)
    'overview_comments': return, self->processComments(self.overviewComments)

    'footer': return, self.footer
    
    'output_root': return, self.output
    'relative_root': return, ''

    'has_author_info': return, self.hasAuthorInfo
    
    'has_author': return, obj_valid(self.author)
    'author': return, self->processComments(self.author)
    'plain_author': return, self->processPlainComments(self.author)

    'has_copyright': return, obj_valid(self.copyright)
    'copyright': return, self->processComments(self.copyright)
    
    'has_history': return, obj_valid(self.history)
    'history': return, self->processComments(self.history)

    'has_version': return, obj_valid(self.version)
    'version': return, self->processComments(self.version)
        
    'n_dirs': return, self.directories->count()
    'dirs': return, self.directories->get(/all)   
    'n_visible_dirs': begin
        nVisible = 0L
        for d = 0L, self.directories->count() - 1L do begin
          dir = self.directories->get(position=d)                   
          nVisible += dir->isVisible()          
        endfor
        return, nVisible    
      end
    'visible_dirs': begin
        dirs = self.directories->get(/all, count=nDirs)
        if (nDirs eq 0L) then return, -1L
        
        isVisibleDirs = bytarr(nDirs)
        for d = 0L, nDirs - 1L do begin
          isVisibleDirs[d] = dirs[d]->isVisible()
        endfor
        
        ind = where(isVisibleDirs eq 1B, nVisibleDirs)
        if (nVisibleDirs eq 0L) then return, -1L
              
        return, dirs[ind]    
      end 
    'n_pro_files': return, self.proFiles->count()
    'pro_files': return, self.proFiles->get(/all)
    'just_files': begin
        dirs = self.directories->get(/all, count=nDirs)
        if (nDirs eq 0L) then return, -1L
        
        isVisibleDirs = bytarr(nDirs)
        for d = 0L, nDirs - 1L do begin
          isVisibleDirs[d] = dirs[d]->isVisible()
        endfor
        
        ind = where(isVisibleDirs eq 1B, nVisibleDirs)      
        return, nVisibleDirs le 1
      end
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
      
    'n_dlm_files': return, self.dlmFiles->count()
    'dlm_files': return, self.dlmFiles->get(/all)
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
      
    'n_sav_files': return, self.savFiles->count()
    'sav_files': return, self.savFiles->get(/all)
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
          
    'n_idldoc_files': return, self.idldocFiles->count()
    'idldoc_files': return, self.idldocFiles->get(/all)
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
      
    'css_location': return, filepath('main.css', $
                                     subdir='resources', $
                                     root=self.sourceLocation)
    'print_css_location': return, filepath('main-print.css', $
                                           subdir='resources', $
                                           root=self.sourceLocation)
    'listing_css_location': return, filepath('listing.css', $
                                             subdir='resources', $
                                             root=self.sourceLocation)
    'print_listing_css_location': return, filepath('listing-print.css', $
                                                   subdir='resources', $
                                                   root=self.sourceLocation)

    'n_categories': return, self.categories->count()
    'categories': begin
        categories = self.categories->keys()
        return, categories[sort(categories)]
      end
    'n_todos': return, self.todos->count()
    'todos': return, self.todos->get(/all)
    'n_obsolete': return, self.obsolete->count()
    'obsolete': return, self.obsolete->get(/all)
    'n_undocumented': return, self.undocumented->count()
    'undocumented': return, self.undocumented->get(/all)
    'n_complex_routines': return, self.complexRoutines->count()
    'complex_routines': return, self.complexRoutines->get(/all)
    'n_bugs': return, self.bugs->count()
    'bugs': return, self.bugs->get(/all)
    
    'index_empty': return, self.index->count() eq 0
    'index_first_letters': begin
        if (self.index->count() eq 0) then return, -1L
        
        index = self.index->get(/all)
        firstLetters = strmid(index.name, 0, 1)
        sind = sort(firstLetters)
        uind = uniq(firstLetters, sind)
        
        return, strupcase(firstLetters[uind]) 
      end
    'index_type': return, 'unknown'
    'index_name': return, 'unknown'
    'index_url': return, ''
    
    'n_routines': begin
        if (self.proFiles->count() eq 0) then return, '0'
        
        nRoutines = 0L
        proFiles = self.proFiles->get(/all)
        for f = 0L, n_elements(proFiles) - 1L do begin
          proFiles[f]->getProperty, n_routines=fileRoutines
          nRoutines += fileRoutines
        endfor  
        
        return, mg_int_format(nRoutines)
      end
    'n_lines': begin
        if (self.proFiles->count() eq 0) then return, '0'
        
        nLines = 0L
        
        proFiles = self.proFiles->get(/all)
        for f = 0L, n_elements(proFiles) - 1L do begin
          if (proFiles[f]->isVisible()) then begin
            proFiles[f]->getProperty, n_lines=fileLines          
            nLines += fileLines
          endif
        endfor
        
        return, mg_int_format(nLines)
      end
    'requires_version': return, self.requiresVersion
    'requires_items': return, self.requiresItems->get(/all)
    
    'current_template': return, self.currentTemplate
                                    
    'idldoc_header_location' : begin
        dir = self.templateLocation eq '' $
          ? filepath('', subdir='templates', root=self.sourceLocation) $
          : self.templateLocation      
        return, filepath(self.templatePrefix + 'header.tt', $
                         root=dir)
      end   
    'idldoc_footer_location' : begin
        dir = self.templateLocation eq '' $
          ? filepath('', subdir='templates', root=self.sourceLocation) $
          : self.templateLocation      
        return, filepath(self.templatePrefix + 'footer.tt', $
                         root=dir)
      end   
    else: begin
        found = 0B
        return, -1L
      end
  endcase
end


;+
; Get properties of the system.
;-
pro doc_system::getProperty, root=root, output=output, classes=classes, $
                             format=format, markup=markup, $
                             comment_style=commentStyle, $
                             extension=outputExtension, $
                             overview=overview, $
                             directories=directories, $
                             nosource=nosource, source_link=sourceLink, $
                             user=user, index_level=indexLevel, $
                             routine_line_cutoffs=routineLineCutoffs, $
                             complexity_cutoffs=complexityCutoffs, $
                             statistics=statistics
  compile_opt strictarr, hidden

  if (arg_present(root)) then root = self.root
  if (arg_present(output)) then output = self.output
  if (arg_present(classes)) then classes = self.classes
  if (arg_present(format)) then format = self.format
  if (arg_present(markup)) then markup = self.markup
  if (arg_present(commentStyle)) then commentStyle = self.commentStyle
  if (arg_present(outputExtension)) then outputExtension = self.outputExtension  
  if (arg_present(overview)) then overview = self.overview
  if (arg_present(directories)) then directories = self.directories
  if (arg_present(nosource)) then nosource = self.nosource  
  if (arg_present(sourceLink)) then sourceLink = self.sourceLink
  if (arg_present(user)) then user = self.user
  if (arg_present(indexLevel)) then indexLevel = self.indexLevel
  if (arg_present(routineLineCutoffs)) then routineLineCutoffs = self.routineLineCutoffs
  if (arg_present(complexityCutoffs)) then complexityCutoffs = self.complexityCutoffs
  if (arg_present(statistics)) then statistics = self.statistics
end


;+
; Set properties of the system.
;-
pro doc_system::setProperty, overview_comments=overviewComments, $
                             author=author, copyright=copyright, $
                             history=history, version=version
  compile_opt strictarr, hidden

  if (n_elements(overviewComments) gt 0) then begin
    self.overviewComments = overviewComments
  endif
  
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
end


;+
; Print out debugging information about the system object.
;-
pro doc_system::debug
  compile_opt strictarr, hidden
  
end


;+
; Print error messages respecting /QUIET and /SILENT.
;
; :Params:
;    msg : in, required, type=string
;       error message to print 
;-
pro doc_system::error, msg
  compile_opt strictarr, hidden
  on_error, 2

  if (self.logFile ne '') then printf, self.logLun, 'IDLDOC: ' + msg

  if (self.isTty) then begin
    message, mg_ansicode(msg, /red), /noname
  endif else begin
    message, msg, /noname
  endelse
end


;+
; Print warning messages respecting /QUIET and /SILENT.
;
; :Params:
;    msg : in, required, type=string
;       warning message to print 
;-
pro doc_system::warning, msg
  compile_opt strictarr, hidden
  
  if (self.logFile ne '') then printf, self.logLun, 'IDLDOC: ' + msg
  
  if (~self.silent) then begin
    if (self.isTty) then begin
      message, mg_ansicode('IDLDOC: '+ msg, /red), /informational, /noname
    endif else begin
      message, 'IDLDOC: '+ msg, /informational, /noname
    endelse
  endif
  
  ++self.nWarnings
end


;+
; Print messages respecting /QUIET and /SILENT.
;
; :Params:
;    msg : in, required, type=string
;       message to print 
;-
pro doc_system::print, msg
  compile_opt strictarr, hidden
  
  if (self.logFile ne '') then printf, self.logLun, msg
  
  if (~self.quiet && ~self.silent) then print, msg
end


;+
; Print basic help message.
;-
pro doc_system::printHelp
  compile_opt strictarr, hidden
  
  msg = ['IDLdoc ' + self.idldoc_version, $
         '', $
         'Usage:', $
         '', $
         '  IDL> idldoc, ROOT=string [, OUTPUT=string] $', $
         '               [, TITLE=string] [, SUBTITLE=string] $', $
         '               [, /EMBED] [, OVERVIEW=string] [, FOOTER] $', $
         '               [, /NONAVBAR], [, /NOSOURCE] [, SOURCE_LINK={0, 1, 2}] $', $
         '               [, /USER] [, INDEX_LEVEL={0, 1, 2}] $', $
         '               [, /STATISTICS] [, COMPLEXITY_CUTOFFS=lonarr(2)] $', $
         '               [, ROUTINE_LINE_CUTOFFS=lonarr(2)] $', $
         '               [, /QUIET] [, /SILENT] $', $
         '               [, FORMAT_STYLE=string] [, MARKUP_STYLE=string] $', $
         '               [, COMMENT_STYLE=string] [, CHARSET=string] $', $
         '               [, TEMPLATE_PREFIX=string] [, TEMPLATE_LOCATION=string] $', $
         '               [, ERROR=named variable] [, /DEBUG] [N_WARNINGS=named variable] $', $
         '               [, LOG_FILE=string] $', $
         '               [, /COLOR_OUTPUTLOG] $', $
         '               [, /HELP] [, /VERSION] $', $
         '', $
         'See the help for more detailed information about the keywords.']
  
  self->print, transpose(msg)
end


;+
; Do any analysis necessary on information gathered during the "parseTree"
; phase.
;-
pro doc_system::process
  compile_opt strictarr, hidden
  
  self->print, 'Processing library structure...'
  
  ; first, organize the pro/sav/idldoc files
  index = self.index->get(/all, count=nEntries)

  if (nEntries gt 0) then begin
    entries = index.item
    names = index.name
      
    ind = where(obj_isa(entries, 'DOCtreeProFile'), nProFiles)
    if (nProFiles gt 0) then begin
      proFiles = entries[ind]
      proFileNames = names[ind]
      sind = sort(proFileNames)
      self.proFiles->add, proFiles[sind]      
    endif

    ind = where(obj_isa(entries, 'DOCtreeDLMFile'), nDLMFiles)
    if (nDLMFiles gt 0) then begin
      dlmFiles = entries[ind]
      dlmFileNames = names[ind]
      sind = sort(dlmFileNames)
      self.dlmFiles->add, dlmFiles[sind]
    endif
          
    ind = where(obj_isa(entries, 'DOCtreeSavFile'), nSavFiles)
    if (nSavFiles gt 0) then begin
      savFiles = entries[ind]
      savFileNames = names[ind]
      sind = sort(savFileNames)
      self.savFiles->add, savFiles[sind]
    endif
    
    ind = where(obj_isa(entries, 'DOCtreeIDLdocFile'), nIDLdocFiles)
    if (nIDLdocFiles gt 0) then begin
      idldocFiles = entries[ind]
      idldocFileNames = names[ind]
      sind = sort(idldocFileNames)
      self.idldocFiles->add, idldocFiles[sind]
    endif    
  endif    
  
  ; generate files per directory
  for d = 0L, self.directories->count() - 1L do begin
    directory = self.directories->get(position=d)
    directory->process
  endfor 
  
  if (nEntries gt 0) then self->processIndex 
  
  doctree_fill_links, self.overviewComments, self
  
  directories = self.directories->get(/all, count=ndirectories)
  for i = 0L, ndirectories - 1L do (directories[i])->fillLinks
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
function doc_system::lookupName, name, exclude=exclude
  compile_opt strictarr

  dirs = self.directories->get(/all, count=ndirs) 
  for i = 0L, ndirs - 1L do begin
    if (obj_valid(exclude) && exclude eq dirs[i]) then continue
    url = (dirs[i])->lookupName(name, exclude=self)
    if (url ne '') then return, url
  endfor
  
  return, ''  
end

  
;+
; Build the tree of directories, files, routines, and parameters.
;-
pro doc_system::parseTree
  compile_opt strictarr, hidden
  
  ; search for special files
  proFiles = file_search(self.root, '*.pro', /test_regular, count=nProFiles)
  dlmFiles = file_search(self.root, '*.dlm', /test_regular, count=nDLMFiles)    
  savFiles = file_search(self.root, '*.sav', /test_regular, count=nSavFiles)
  idldocFiles = file_search(self.root, '*.idldoc', /test_regular, count=nIDLdocFiles)
  
  ; quit if no files found
  if (nProFiles + nSavFiles + nIDLdocFiles eq 0) then return
  
  ; add all the files together
  allFiles = ['']
  if (nProFiles gt 0) then allFiles = [allFiles, proFiles]
  if (nDLMFiles gt 0) then allFiles = [allFiles, dlmFiles]  
  if (nSavFiles gt 0) then allFiles = [allFiles, savFiles]
  if (nIDLdocFiles gt 0) then allFiles = [allFiles, idldocFiles]
  allFiles = allFiles[1:*]
  
  ; remove the common root location
  allFiles = strmid(allFiles, strlen(self.root))
  
  ; get the unique directories
  dirs = file_dirname(allFiles, /mark_directory)
  uniqueDirIndices = uniq(dirs, sort(dirs))  
  
  ; create the directory objects
  for d = 0L, n_elements(uniqueDirIndices) - 1L do begin
    location = dirs[uniqueDirIndices[d]]
    filesIndices = where(dirs eq location)
    directory = obj_new('DOCtreeDirectory', $
                        location=location, $
                        files=allFiles[filesIndices], $
                        system=self)
    self.directories->add, directory
  endfor
  
  ; parse overview file if present
  if (self.overview ne '') then begin
    if (~file_test(self.overview)) then begin
      self->warning, 'overview file ' + self.overview + ' does not exist'
      return
    endif
    
    nLines = file_lines(self.overview)
    if (nLines le 0) then return
    
    lines = strarr(nLines)
    openr, lun, self.overview, /get_lun
    readf, lun, lines
    free_lun, lun
    
    formatParser = self->getParser(self.format + 'format')
    markupParser = self->getParser(self.markup + 'markup')
    
    formatParser->parseOverviewComments, lines, $
                                         system=self, $
                                         markup_parser=markupParser
  endif
end


;+
; Get a template by name (as used when loaded in loadTemplates).
; 
; :Returns: 
;    template object or -1 if not found
;
; :Params:
;    name : in, required, type=string
;       name of template as used when loaded in loadTemplates
;
; :Keywords:
;    found : out, optional, type=boolean
;       indicates if the template name was found and returned
;-
function doc_system::getTemplate, name, found=found
  compile_opt strictarr, hidden
  
  self.currentTemplate = name
  return, self.templates->get(name, found=found)
end


;+
; Create the templates to be used to generate all the output and store the 
; templates in a hash table.
;-
pro doc_system::loadTemplates
  compile_opt strictarr, hidden
  
  templates = ['file-listing', 'all-files', 'dir-listing',  $
               'index', 'overview', 'help', 'warnings', 'index-entries', $
               'categories', 'search', 'libdata', $
               'dir-overview', $
               'dlmfile', 'savefile', 'profile', 'source', 'idldocfile']
  for t = 0L, n_elements(templates) - 1L do begin
    dir = self.templateLocation eq '' $
      ? filepath('', subdir='templates', root=self.sourceLocation) $
      : self.templateLocation
    templateFilename = filepath(self.templatePrefix + templates[t] + '.tt', $
                                root=dir) 
    self.templates->put, templates[t], $
                         obj_new('MGffTemplate', templateFilename)
  endfor
end


;+
; Convert a parse tree into a string array using the current comment style.
;
; :Returns: 
;    strarr
;
; :Params:
;    tree : in, required, type=object
;       parse tree object
;-
function doc_system::processComments, tree
  compile_opt strictarr, hidden
  
  if (~obj_valid(tree)) then return, ''
  commentParser = self->getParser(self.commentStyle + 'output')
  return, commentParser->process(tree)  
end


;+
; Add a routine to the visible routines list.
; 
; :Params:
;    name : in, required, type=string
;       name of routine
;    routine : in, required, type=object
;       DOCtreeRoutine object
;-
pro doc_system::addVisibleRoutine, name, routine
  compile_opt strictarr, hidden

  self.visibleRoutines->put, strlowcase(name), routine
end


;+
; Find link to given resource.
; 
; :Returns:
;    string, returns empty string if no resource found
;    
; :Params:
;    resource : in, required, type=string   
;-
function doc_system::_findResourceLink, resource
  compile_opt strictarr, hidden
  
  class = self.classes->get(strlowcase(resource), found=found)
  if (found) then begin
    if (class->hasUrl()) then return, class->getUrl()
  endif
  
  routine = self.visibleRoutines->get(strlowcase(resource), found=found)
  if (found) then begin
    return, routine->getVariable('index_url')
  endif
  
  return, ''
end


;+
; Convert the uses clause into a string array using the current comment style.
; 
; :Returns:
;    strarr
;
; :Params:
;    tree : in, required, type=object
;       parse tree object
;  
; :Keywords:
;    root : in, required, type=string
;       relative location of root from the calling routine or file
;-
function doc_system::processUses, tree, root=root
  compile_opt strictarr, hidden
  
  if (~obj_valid(tree)) then return, ''
  
  ; get plain text
  crlf = string(!version.os_family eq 'unix' ? [10B] : [13B, 10B])
  plainParser = self->getParser('plainoutput')
  s = plainParser->process(tree)
  s = strjoin(s, crlf)
  resources = strsplit(s, '[[:space:],]', /regex, /extract, count=nresources)
  
  ; get new tree based with links to resources used
  _tree = obj_new('MGtmTag', type='paragraph')
  for r = 0L, nresources - 1L do begin
    href = self->_findResourceLink(resources[r])
    if (href eq '') then begin
      parent = _tree
    endif else begin
      link = obj_new('MGtmTag', type='link')
      link->addAttribute, 'reference', root + '/' + href
      _tree->addchild, link
      parent = link
    endelse
    
    resourceName = obj_new('MGtmText', text=resources[r])
    parent->addChild, resourceName
    
    if (r ne nresources - 1L) then begin
      comma = obj_new('MGtmText', text=', ')
      _tree->addChild, comma
    endif
  endfor

  ; create output using current comment style
  commentParser = self->getParser(self.commentStyle + 'output')
  comments = commentParser->process(_tree)
    
  ; destroy new tree
  obj_destroy, _tree
  
  return, comments
end


;+
; Convert a parse tree into a string array using the plain output parser.
;
; :Returns: 
;    strarr
;
; :Params:
;    tree : in, required, type=object
;       parse tree object with the plain output parser
;-
function doc_system::processPlainComments, tree
  compile_opt strictarr, hidden
  
  if (~obj_valid(tree)) then return, ''
  
  plainParser = self->getParser('plainoutput')
  comments = strjoin(plainParser->process(tree), ' ')  

  bcomments = byte(comments)
  quote = (byte('"'))[0]
  space = (byte(' '))[0]
  quoteIndices = where(bcomments eq quote, nQuotes)
  if (nQuotes gt 0) then begin
    bcomments[quoteIndices] = space
  endif
  comments = string(bcomments)
          
  return, comments
end


;+
; Get a parser by name (as used when loaded in loadParsers).
; 
; :Returns: 
;    parser object or -1 if not found
;
; :Params:
;    name : in, required, type=string
;       name of parser as used when loaded in loadTemplates
;
; :Keywords:
;    found : out, optional, type=boolean
;       indicates if the parser name was found and returned
;-
function doc_system::getParser, name, found=found
  compile_opt strictarr, hidden
  
  return, self.parsers->get(strlowcase(name), found=found)
end


;+
; Create the parsers to be used to parse all the code/input files and store
; the templates in a hash table.
;-
pro doc_system::loadParsers
  compile_opt strictarr, hidden
  
  ; file parsers
  self.parsers->put, 'profile', obj_new('DOCparProFileParser', system=self)
  self.parsers->put, 'idldocfile', obj_new('DOCparIDLdocFileParser', system=self)
  
  ; header comment parsers
  self.parsers->put, 'verbatimformat', obj_new('DOCparVerbatimFormatParser', system=self)
  self.parsers->put, 'rstformat', obj_new('DOCparRSTFormatParser', system=self)
  self.parsers->put, 'idldocformat', obj_new('DOCparIDLdocFormatParser', system=self)
  self.parsers->put, 'idlformat', $
                     obj_new('DOCparIDLFormatParser', system=self)
  
  ; markup parsers
  self.parsers->put, 'verbatimmarkup', obj_new('DOCparVerbatimMarkupParser', system=self)
  self.parsers->put, 'preformattedmarkup', obj_new('DOCparPreformattedMarkupParser', system=self)
  self.parsers->put, 'rstmarkup', obj_new('DOCparRSTMarkupParser', system=self)
  
  ; tree node parsers
  self.parsers->put, 'htmloutput', obj_new('MGtmHTML')
  self.parsers->put, 'rstoutput', obj_new('MGtmRST')
  self.parsers->put, 'latexoutput', obj_new('MGtmLaTeX')
  self.parsers->put, 'plainoutput', obj_new('MGtmPlain')
  self.parsers->put, 'docbookoutput', obj_new('MGtmDocbook')
end


;+
; Generate all output for the run.
;-
pro doc_system::generateOutput
  compile_opt strictarr, hidden
  
  ; generate files per directory
  for d = 0L, self.directories->count() - 1L do begin
    directory = self.directories->get(position=d)
    directory->generateOutput, self.output
  endfor
      
  ; generate all-files
  self->print, 'Generating file listing...'
  allFilesTemplate = self->getTemplate('all-files')
  allFilesTemplate->reset
  allFilesTemplate->process, self, filepath('all-files.' + self.outputExtension, root=self.output)
    
  ; generate all-dirs
  self->print, 'Generating directory listing...'
  allDirsTemplate = self->getTemplate('dir-listing')
  allDirsTemplate->reset
  allDirsTemplate->process, self, filepath('all-dirs.' + self.outputExtension, root=self.output)
  
  ; generate overview page
  self->print, 'Generating overview page...'
  overviewTemplate = self->getTemplate('overview')
  overviewTemplate->reset
  overviewTemplate->process, self, filepath('overview.' + self.outputExtension, root=self.output)
    
  ; generate index entries page
  if (self.indexLevel gt 0L) then begin
    self->print, 'Generating index entries page...'
    indexEntriesTemplate = self->getTemplate('index-entries')
    indexEntriesTemplate->reset
    indexEntriesTemplate->process, self, filepath('idldoc-index.' + self.outputExtension, $
                                                  root=self.output)
  endif
    
  ; generate warnings page
  if (~self.user) then begin
    self->print, 'Generating warnings page...'
    warningsTemplate = self->getTemplate('warnings')
    warningsTemplate->reset
    warningsTemplate->process, self, filepath('idldoc-warnings.' + self.outputExtension, $
                                              root=self.output)
  endif

  ; generate search page
  self->print, 'Generating search page...'
  searchTemplate = self->getTemplate('search')
  searchTemplate->reset
  searchTemplate->process, self, filepath('search.' + self.outputExtension, root=self.output)
  
  libdataTemplate = self->getTemplate('libdata')
  libdataTemplate->reset
  libdataTemplate->process, self, filepath('libdata.js', root=self.output)
                                          
  ; generate categories page
  self->print, 'Generating categories page...'
  categoriesTemplate = self->getTemplate('categories')
  categoriesTemplate->reset
  categoriesTemplate->process, self, filepath('categories.' + self.outputExtension, $
                                              root=self.output)
  ; generate help page
  self->print, 'Generating help page...'
  helpTemplate = self->getTemplate('help')
  helpTemplate->reset
  helpTemplate->process, self, filepath('idldoc-help.' + self.outputExtension, root=self.output)
    
  ; generate index.html
  self->print, 'Generating index page...'
  indexTemplate = self->getTemplate('index')
  indexTemplate->reset
  indexTemplate->process, self, filepath('index.' + self.outputExtension, root=self.output)
  
  self->print, strtrim(self.nWarnings, 2) + ' warnings generated'
end


;+
; Enter the item in the index.
; 
; :Params:
;    name : in, required, type=string
;       name to register the entry under
;    value : in, required, type=object
;       tree object, i.e., directory, file, param, etc.
;-
pro doc_system::createIndexEntry, name, value
  compile_opt strictarr, hidden
  
  self.index->add, { name: strlowcase(name), item: value }
end


;+
; Remove items that are not visible from the index.
;-
pro doc_system::processIndex
  compile_opt strictarr, hidden

  entries = self.index->get(/all, count=nEntries)

  ; filter index for visible entries    
  isVisibleEntries = bytarr(nEntries)
  
  for i = 0L, nEntries - 1L do begin
    isVisibleEntries[i] = obj_valid(entries[i].item) ? entries[i].item->isVisible() : 0B
  endfor

  ind = where(isVisibleEntries, nVisibleEntries)
  self.index->remove, /all
  if (nVisibleEntries gt 0) then self.index->add, entries[ind]
end


;+
; Return the items which begin with the given letter.
;
; :Returns: 
;    `objarr`
;
; :Params:
;    letter : in, required, type=string
;       first letter of items to return
;-
function doc_system::getIndexEntries, letter
  compile_opt strictarr, hidden
    
  entries = self.index->get(/all)
  ind = where(strmid(entries.name, 0, 1) eq strlowcase(letter), count)
  
  entries = entries[ind]
    
  ind = sort(strlowcase(entries.name))
  return, (entries.item)[ind]
end


;+
; Add a routine to a given category.
;
; :Params:
;    name : in, required, type=string
;       category name
;    item : in, required, type=object
;       routine or file tree object
;-
pro doc_system::createCategoryEntry, name, item
  compile_opt strictarr, hidden
  
  if (~item->isVisible()) then return
  
  lname = strlowcase(name)
  categoryList = self.categories->get(lname, found=found)
  
  if (~found) then begin
    categoryList = obj_new('MGcoArrayList', type=11, block_size=20)
    self.categories->put, lname, categoryList
  endif
  
  categoryList->add, item
end


;+
; Return the routines in a given category.
;
; :Returns: strarr
; :Params:
;    name : in, required, type=string
;       category name
;-
function doc_system::getCategoryEntries, name
  compile_opt strictarr, hidden

  lname = strlowcase(name)
  categoryList = self.categories->get(lname, found=found)
  if (~found) then return, ''
  
  return, categoryList->get(/all)
end


;+
; Remember that the given routine has a todo attached to it.
;
; :Params:
;    routine : in, required, type=object
;       routine tree object which has an attached todo tag
;-
pro doc_system::createTodoEntry, routine
  compile_opt strictarr, hidden
  
  self.todos->add, routine
end


;+
; Remember that the given routine is obsolete.
;
; :Params:
;    item : in, required, type=object
;       routine or file tree object which is obsolete
;-
pro doc_system::createObsoleteEntry, item
  compile_opt strictarr, hidden
  
  self.obsolete->add, item
end


;+
; Remember that the given routine has a bug.
;
; :Params:
;    item : in, required, type=object
;       routine or file tree object which contains a bug
;-
pro doc_system::createBugEntry, item
  compile_opt strictarr, hidden
  
  self.bugs->add, item
end


;+
; Remember that the given routine is not fully documented.
;
; :Params:
;    routine : in, required, type=object
;       routine tree object which is missing documentation
;-
pro doc_system::createDocumentationEntry, routine
  compile_opt strictarr, hidden
  
  self.undocumented->add, routine
end


;+
; Remember that the given routine is complex.
;
; :Params:
;    routine : in, required, type=object
;       routine tree object which is complex
;-
pro doc_system::createComplexityEntry, routine
  compile_opt strictarr, hidden
  
  self.complexRoutines->add, routine
end


;+
; Compare given version to the current highest version. Keeps track of the 
; routines that have the highest version.
;
; :Params:
;    version : in, required, type=string
;       required version
;    item : in, required, type=object
;       routine or file tree object
;-
pro doc_system::checkRequiredVersion, version, item
  compile_opt strictarr, hidden
  
  case mg_cmp_version(version, self.requiresVersion) of
    -1: ; don't do anything if version is not at least equal to requires version
     0: self.requiresItems->add, item
     1: begin
        self.requiresItems->remove, /all
        self.requiresItems->add, item
        self.requiresVersion = version       
      end
    else:   ; should not happen
  endcase
end


;+
; Determine if the output directory can be written to.
;
; :Returns: error code (0 indicates no error)
;-
function doc_system::testOutput
  compile_opt strictarr, hidden
    
  testfile = self.output + 'idldoc.test'
  openw, lun, testfile, error=error, /get_lun
  if (error eq 0L) then free_lun, lun
  file_delete, testfile, /allow_nonexistent
  
  return, error
end


;+
; Copy everything that is in the resources directory of the distribution to
; the idldoc-resources directory in the output root.
;-
pro doc_system::copyResources
  compile_opt strictarr, hidden
  
  ; copy *.* to avoid .svn/ if running from a Subversion checkout 
  resourceLocation = filepath('*.*', subdir=['resources'], $
                              root=self.sourceLocation)
  resourceDestination = filepath('', subdir=['idldoc-resources'], $
                                 root=self.output)

  ; allow idldoc-resources directory to stay if it is a directory
  if (~file_test(resourceDestination, /directory)) then begin
    file_delete, resourceDestination, /recursive, /allow_nonexistent
    file_mkdir, resourceDestination    
  endif
  
  ; copy the resource files
  file_copy, resourceLocation, resourceDestination, /recursive, /overwrite
  
  ; move the LaTeX files up a directory if needed
  if (self.commentstyle eq 'latex') then begin
    file_move, resourceDestination + 'idldoc' + ['.cls', '.sty'], $
               resourceDestination + ['..', '..'], $
               /overwrite
  endif
end


;+
; Creates a directory.
;
; :Params:
;    dir : in, required, type=string
;       directory to create
; :Keywords:
;    error : out, optional, type=long
;       error code; 0 indicates no error
;-
pro doc_system::makeDirectory, dir, error=error
  compile_opt strictarr, hidden
  
  error = 0L
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return
  endif
  
  file_mkdir, dir
end



;+
; Safe method of determining if the current terminal is TTY.
;
; :Returns:
;    1 if the terminal is TTY, 0 if not
;-
function doc_system::_findIfTty
  compile_opt strictarr, hidden
  
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    return, 0
  endif

  return, mg_termIsTty()
end


;+
; Free resources.
;-
pro doc_system::cleanup
  compile_opt strictarr, hidden
  
  if (self.logLun ne '') then free_lun, self.logLun
  
  obj_destroy, [self.index, self.proFiles, self.dlmFiles, self.savFiles, self.idldocFiles]
  obj_destroy, self.visibleRoutines
  
  classes = self.classes->values(count=nClasses)
  if (nClasses gt 0) then obj_destroy, classes
  obj_destroy, self.classes
  
  obj_destroy, self.directories
  
  categoryLists = self.categories->values(count=nCategories)
  if (nCategories gt 0) then obj_destroy, categoryLists
  obj_destroy, self.categories

  obj_destroy, [self.author, self.copyright, self.history, self.version]
    
  obj_destroy, self.overviewComments
  
  obj_destroy, [self.todos, self.obsolete, self.undocumented, $
                self.complexRoutines, self.bugs]
  obj_destroy, self.requiresItems
  
  obj_destroy, self.templates->values()
  obj_destroy, self.templates
  
  obj_destroy, self.parsers->values()
  obj_destroy, self.parsers  
end


;+
; Create system object.
; 
; :Returns: 
;    1 for success, 0 for failure
;
; :Keywords:
;    root : in, required, type=string
;       root of directory hierarchy to document
;    output : in, optional, type=string
;       directory to place output
;
;    quiet : in, optional, type=boolean
;       if set, don't print info messages, only print warnings and errors
;    silent : in, optional, type=boolean
;       if set, don't print anything
;    n_warnings : out, optional, type=long
;       set to a named variable to return the number of warnings for the run
;    log_file : in, optional, type=string
;       if present, send messages to this filename instead of stdout
;    embed : in, optional, type=boolean
;       embed CSS stylesheet instead of linking to it (useful for documentation
;       where individual pages must stand by themselves)
;    overview : in, optional, type=string
;       filename of overview text and directory information
;    footer : in, optional, type=string
;       filename of file to insert into the bottom of each page of docs
;    title : in, optional, type=string
;       title of docs
;    subtitle : in, optional, type=string
;       subtitle for docs
;    nonavbar : in, optional, type=boolean
;       set to not display the navbar
;    nosource : in, optional, type=boolean
;       set to not display the source code for .pro files
;    source_link : in, optional, type=long, default=0
;       by default, IDLdoc copies the source code into the output; if this
;       keyword is set to 1 (relative link) or 2 (absolute link), then the 
;       output documentation will point to the ROOT location of the original 
;       source code
;    user : in, optional, type=boolean
;       set to generate user-level docs (private parameters, files are not
;       shown); the default is developer-level docs showing files and 
;       parameters
;    statistics : in, optional, type=boolean
;       generate complexity statistics for routines
;    index_level : in, optional, type=integer, default=2
;       level of index generation: 0 for no index; 1 for directories, classes,
;       files, and routines; 2 for level 1 items plus parameters, keywords,
;       fields, properties, and sav file variables
;    routine_line_cutoffs : in, optional, type=lonarr(2), default="[75, 150]"
;       number of lines before warning or flagged number of lines in a routine
;    complexity_cutoffs : in, optional, type=lonarr(2), default="[10, 20]"
;       McCabe complexity to exceed for a warning or flagged
;
;    format_style : in, optional, type=string, default='idldoc'
;       style to use to parse file and routine comments ("idl", "idldoc", 
;       "verbatim", or "rst")
;    markup_style : in, optional, type=string, default='verbatim'
;       markup used in comments ("rst" or "verbatim")
;    comment_style : in, optional, type=string, default='html'
;       output format for comments ("html", "rst", "latex", or "docbook")
;
;    assistant : in, optional, type=boolean, obsolete
;       no longer used
;    preformat : in, optional, type=boolean, obsolete
;       no longer used
;    browse_routines : in, optional, type=boolean, obsolete
;       no longer used
;
;    template_prefix : in, optional, type=string
;       prefix for template's names
;    template_location : in, optional, type=string
;       directory to find templates in
;    charset : in, optional, type=string, default=utf-8
;       character set to use
;       
;    color_outputlog : in, optional, type=boolean
;       set to color output log messages
;-
function doc_system::init, root=root, output=output, $
                           quiet=quiet, silent=silent, n_warnings=nWarnings, $
                           log_file=logFile, $
                           assistant=assistant, embed=embed, overview=overview, $
                           footer=footer, title=title, subtitle=subtitle, $
                           nonavbar=nonavbar, $
                           nosource=nosource, source_link=sourceLink, $
                           user=user, statistics=statistics, $
                           index_level=indexLevel, $   
                           routine_line_cutoffs=routineLineCutoffs, $
                           complexity_cutoffs=complexityCutoffs, $                           
                           format_style=formatStyle, markup_style=markupStyle, $
                           comment_style=commentStyle, $
                           preformat=preformat, browse_routines=browseRoutines, $
                           template_prefix=templatePrefix, $
                           template_location=templateLocation, $
                           help=help, version=version, charset=charset, $
                           color_outputlog=colorOutputlog                  
  compile_opt strictarr, hidden
  
  self.idldoc_version = idldoc_version()
  
  self.isTty = keyword_set(colorOutputlog) ? 1B : self->_findIfTty()
  
  if (keyword_set(version)) then begin
    self->print, 'IDLdoc ' + self.idldoc_version
    return, 0
  endif
  
  if (keyword_set(help)) then begin
    self->printHelp
    return, 0
  endif
  
  ; check root directory
  if (n_elements(root) gt 0) then begin
    self.root = file_search(root, /mark_directory, /test_directory)
    if (self.root eq '') then self->error, 'ROOT directory does not exist'
    
    ; temporarily add project to the path
    rootDirs = file_search(self.root, '*', /test_directory, count=nRootDirs)
    rootDirs = nRootDirs eq 0 ? self.root : [self.root, rootDirs]
    rootDirs = file_expand_path(rootDirs) 
    
    !path += path_sep(/search_path) + strjoin(rootDirs, path_sep(/search_path))
    path_cache, /clear, /rebuild
  endif else begin
    self->error, 'ROOT keyword must be defined'
  endelse
  
  ; fix up output directory
  if (n_elements(output) gt 0) then begin
    if (~file_test(output)) then begin
      self->makeDirectory, output, error=error
      if (error ne 0L) then self->error, 'cannot create output directory'
    endif
    self.output = file_search(output, /mark_directory, /test_directory)
  endif else begin
    self.output = self.root
  endelse
  
  ; get location of IDLdoc in order to find locations of data files like
  ; images, templates, etc.
  self.sourceLocation = mg_src_root()
  
  self.quiet = keyword_set(quiet)
  self.silent = keyword_set(silent)
  
  self.overview = n_elements(overview) gt 0 ? overview : ''
  self.footer = n_elements(footer) gt 0 ? footer : ''
  
  self.title = n_elements(title) gt 0 ? title : 'Documentation for ' + self.root
  self.subtitle = n_elements(subtitle) gt 0 ? subtitle : 'Generated by IDLdoc' 
  self.user = keyword_set(user)
  self.statistics = keyword_set(statistics)  
  self.indexLevel = n_elements(indexLevel) eq 0L ? 2L : indexLevel
  
  case n_elements(routineLineCutoffs) of
    0: self.routineLineCutoffs = [0, 75, 150]
    1: self.routineLineCutoffs = [0, lonarr(2) + routineLineCutoffs[0]]
    else: self.routineLineCutoffs = [0, routineLineCutoffs[0:1]]
  endcase

  case n_elements(complexityCutoffs) of
    0: self.complexityCutoffs = [0, 10, 20]
    1: self.complexityCutoffs = [0, lonarr(2) + complexityCutoffs[0]]
    else: self.complexityCutoffs = [0, complexityCutoffs[0:1]]
  endcase

  self.preformat = keyword_set(preformat)
  self.assistant = keyword_set(assistant)
  self.embed = keyword_set(embed)
  
  self.nonavbar = keyword_set(nonavbar)
  self.nosource = keyword_set(nosource)
  
  self.sourceLink = n_elements(sourceLink) eq 0 ? 0L : sourceLink
  
  ; check to make sure sourceLink is 0, 1, or 2
  if (total(self.sourceLink eq [0, 1, 2]) lt 1) then begin
    self.sourceLink = 0L
    self->warning, 'invalid SOURCE_LINK value, copying instead'
  endif
  
  ; check if using relative link on Windows AND ROOT and OUTPUT are on different 
  ; drives
  if (self.sourceLink eq 1 && strlowcase(!version.os_family) eq 'windows') then begin
    rootDrive = mg_getdrive(self.root)
    outputDrive = mg_getdrive(self.output)  
    if (strlowcase(rootDrive) ne strlowcase(outputDrive)) then begin
      self.sourceLink = 0L
      self->warning, 'cannot use relative link across Windows drives, copying instead'
    endif 
  endif
  
  self.logFile = n_elements(logFile) gt 0 ? logFile : ''
  if (self.logFile ne '') then begin
    openw, lun, self.logFile, /get_lun
    self.logLun = lun
  endif
    
  self.templatePrefix = n_elements(templatePrefix) gt 0 ? templatePrefix : ''
  self.templateLocation = n_elements(templateLocation) gt 0 ? templateLocation : ''
  self.charset = n_elements(charset) eq 0 ? 'utf-8' : charset
  
  ; test output directory for write permission
  outputError = self->testOutput()
  if (outputError ne 0L) then self->error, 'unable to write to ' + self.output
  
  self.index = obj_new('MGcoArrayList', example={name:'', item: obj_new() }, $
                       block_size=100)
  self.classes = obj_new('MGcoHashTable', key_type=7, value_type=11)
  self.categories = obj_new('MGcoHashTable', key_type=7, value_type=11)
  self.todos = obj_new('MGcoArrayList', type=11, block_size=10)
  self.obsolete = obj_new('MGcoArrayList', type=11, block_size=20)
  self.undocumented = obj_new('MGcoArrayList', type=11, block_size=20)
  self.complexRoutines = obj_new('MGcoArrayList', type=11, block_size=20)
  self.bugs = obj_new('MGcoArrayList', type=11, block_size=20)
  
  self.proFiles = obj_new('MGcoArrayList', type=11, block_size=20)
  self.dlmFiles = obj_new('MGcoArrayList', type=11, block_size=20)
  self.savFiles = obj_new('MGcoArrayList', type=11, block_size=20)  
  self.idldocFiles = obj_new('MGcoArrayList', type=11, block_size=20)
  
  self.visibleRoutines = obj_new('MGcoHashTable', key_type=7, value_type=11)
  
  self.requiresItems = obj_new('MGcoArrayList', type=11, block_size=20)
  
  ; initialize some data structures
  self.directories = obj_new('MGcoArrayList', type=11, block_size=8)
  
  ; load templates
  self.templates = obj_new('MGcoHashTable', key_type=7, value_type=11)
  self->loadTemplates
  
  ; load parsers
  self.parsers = obj_new('MGcoHashTable', key_type=7, value_type=11)
  self->loadParsers
  
  self.format = n_elements(formatStyle) eq 0 ? 'idldoc' : formatStyle
  self.markup = n_elements(markupStyle) eq 0 $
                  ? (self.format eq 'rst' ? 'rst' : 'verbatim') $
                  : markupStyle
  self.commentStyle = n_elements(commentStyle) eq 0 ? 'html' : commentStyle  
  commentparser = self->getParser(self.commentStyle + 'output', found=found)

  if (~found) then begin
    self->warning, self.commentStyle + ' output style not found, using HTML output'
    self.commentStyle = 'html'
  end
    
  case self.commentStyle of
    'html': self.outputExtension = 'html'
    'latex': self.outputExtension = 'tex'
    'rst': self.outputExtension = 'rst'
    'plain': self.outputExtension = 'txt'
    'docbook': self.outputExtension = 'xml'
  endcase

  ; copy resources
  self->print, 'Copying resources...'
  self->copyResources
      
  formatparser = self->getParser(self.format + 'format', found=found)
  if (~found) then begin
    self->warning, self.format + ' format parser not found, using IDLdoc parser'
    self.format = 'idldoc'
  end

  markupparser = self->getParser(self.markup + 'markup', found=found)
  if (~found) then begin
    self->warning, self.format + ' markup parser not found, using verbatim parser'
    self.markup = 'verbatim'
  end
      
  ; parse tree of directories, files, routines, parameters 
  self->parseTree
  
  ; do any processing that might be necessary on the tree (analysis, etc.)  
  self->process
  
  ; generate output for directories, files (of various kinds), index, etc.
  self->generateOutput
  
  nWarnings = self.nWarnings
  
  return, 1
end


;+
; Define instance variables.
;
; :Fields:
;    idldoc_version
;       IDLdoc version
;    root 
;       root directory of hierarchy to document; full path ending with slash
;    output
;       directory to place output
;    nWarnings 
;       number of warning messages printed
;    quiet
;       set to only print errors and warnings
;    silent
;       don't print anything
;    sourceLocation
;       directory containing the DOC_System__define.pro file
;    directories
;       array list of directories in current run
;    templates
;       hash table of template names to template objects
;    parsers
;       hash table of parser names to parser objects
;    overview
;       filename of file to parse for overview comments and comments for
;       each directory
;    footer
;       filename of file to include at the bottom of each main page
;    title
;       title of the documentation
;    subtitle
;       subtitle of the documentation
;    user
;       set to generate user-level documentation (as opposed to developer-level
;       documentation)
;    statistics
;       set to generate statistics
;    format
;       style for parsing comments: 'idldoc', 'idl', 'rst', or 'verbatim'
;    markup
;       style for comments body markup: 'rst' or 'verbatim'
;    preformat
;       set if comments should be formatted as given in the source
;    assistant
;       set to produce IDL Assistant output
;    embed
;       set to embed CSS in the HTML output
;    currentTemplate
;       most recently asked for template
;    charset
;       character set to use
;    index
;       hash table of names to tree objects
;    proFiles
;       array list of .pro files in current run
;    savFiles
;       array list of .sav files in current run
;    idldocFiles
;       array list of .idldoc files in current run
;-
pro doc_system__define
  compile_opt strictarr, hidden
  
  define = { DOC_System, $
             idldoc_version: '', $
             
             root: '', $
             output: '', $
             
             nWarnings: 0L, $
             
             quiet: 0B, $
             silent: 0B, $
             
             sourceLocation: '', $

             hasAuthorInfo: 0B, $
             author: obj_new(), $
             copyright: obj_new(), $
             history: obj_new(), $
             version: obj_new(), $
                          
             directories: obj_new(), $ 
              
             templates: obj_new(), $
             parsers: obj_new(), $
             
             overview: '', $
             overviewComments: obj_new(), $
             footer: '', $
             
             title: '', $
             subtitle: '', $
             user: 0B, $
             statistics: 0B, $
             
             format: '', $
             markup: '', $
             commentStyle: '', $
             preformat: 0B, $             
             assistant: 0B, $
             embed: 0B, $
             nonavbar: 0B, $
             nosource: 0B, $
             sourceLink: 0L, $
             outputExtension: '', $
             
             logFile: '', $
             logLun: 0L, $
             isTty: 0B, $
             
             templatePrefix: '', $
             templateLocation: '', $
             currentTemplate: '', $
             charset: '', $
             
             index: obj_new(), $
             indexLevel: 0L, $
             routineLineCutoffs: lonarr(3), $
             complexityCutoffs: lonarr(3), $
             
             classes: obj_new(), $ 
             categories: obj_new(), $
             todos: obj_new(), $
             obsolete: obj_new(), $
             undocumented: obj_new(), $
             complexRoutines: obj_new(), $
             bugs: obj_new(), $
             
             proFiles: obj_new(), $
             dlmFiles: obj_new(), $
             savFiles: obj_new(), $
             idldocFiles: obj_new(), $
             
             visibleRoutines: obj_new(), $
             
             requiresVersion: '', $
             requiresItems: obj_new() $                              
           }
end