; docformat = 'rst'

;+
; Handles parsing of IDLdoc format comment blocks.
;-


;+
; Removes leading blank lines from string arrays.
;
; :Params:
;    lines : in, out, required, type=strarr
;       line from which to remove leading blank lines
;-
pro docparidldocformatparser::_removeSpace, lines
  compile_opt strictarr, hidden

  ; line is all space
  re = '^[[:space:]]*$'
  
  ; stop at first line that is not all space
  i = 0
  while (i lt n_elements(lines) && stregex(lines[i], re, /boolean) eq 1) do i++
  
  ; return empty string if no lines left
  lines = i lt n_elements(lines) ? lines[i:*] : ''
end


;+
; Parse the lines from a tag.
; 
; :Returns: strarr
;
; :Params:
;    lines : in, out, required, type=strarr
;
; :Keywords: 
;    has_argument : in, optional, type=boolean
;       set to indicate that this tag has an argument
;    tag : out, optional, type=string
;       set to a named variable to return the name of the tag
;    argument : out, optional, type=string
;       set to a named variable to return the argument
;    n_attributes : out, optional, type=long
;       set to a named variable to return the number of attributes in curly 
;       braces
;    attribute_names : out, optional, type=strarr
;       set to a named variable to return an array of attribute names
;    attribute_values : out, optional, type=strarr
;       set to a named variable to return an array of attribute values (value
;       will be '' if the attribute has no value)
;-
function docparidldocformatparser::_parseTag, lines, $
                                              has_argument=hasArgument, $
                                              tag=tag, argument=argument, $
                                              n_attributes=nAttributes, $
                                              attribute_names=attributeNames, $
                                              attribute_values=attributeValues
  compile_opt strictarr, hidden
  
  myLines = lines
  
  ; find tag
  re = '^[[:space:]]*@([[:alpha:]_]+)'
  tagStart = stregex(myLines[0], re, length=tagLength, /subexpr)
  if (tagStart[0] lt 0) then begin
    self.system->warning, 'invalid syntax: ' + myLines[0]
    return, ''
  endif
  tag = strmid(myLines[0], tagStart[1], tagLength[1])
  myLines[0] = strmid(myLines[0], tagStart[1] + tagLength[1])
  
  if (~keyword_set(hasArgument)) then return, myLines
  
  ; find argument
  
  self->_removeSpace, myLines
  
  re = '^[[:space:]]*([[:alnum:]_$]+)'
  argStart = stregex(myLines[0], re, length=argLength, /subexpr)
  ; if argStart[0] eq -1 then ERROR
  argument = strmid(myLines[0], argStart[1], argLength[1])
  myLines[0] = strmid(myLines[0], argStart[1] + argLength[1])
  
  ; find attributes

  attributeNamesList = obj_new('MGcoArrayList', type=7, block_size=10)
  attributeValuesList = obj_new('MGcoArrayList', type=7, block_size=10)
  
  re = '^[[:space:]]*{([^}]*)}.*'
  starts = 0
  while (starts[0] ge 0) do begin
    self->_removeSpace, myLines
    starts = stregex(myLines[0], re, /subexpr, length=lengths)
    attribute = strmid(myLines[0], starts[1], lengths[1])
    myLines[0] = strmid(myLines[0], starts[1] + lengths[1] + 1L)
    if (starts[0] ge 0) then begin
      equalPos = strpos(attribute, '=')
      if (equalPos ge 0) then begin
        attributeNamesList->add, strmid(attribute, 0L, equalPos)
        attributeValuesList->add, strmid(attribute, equalPos + 1L)
      endif else begin
        attributeNamesList->add, attribute
        attributeValuesList->add, ''
      endelse
    endif
  endwhile
  
  ; return attribute information
  nAttributes = attributeNamesList->count()
  if (nAttributes gt 0) then begin
    attributeNames = attributeNamesList->get(/all)
    attributeValues = attributeValuesList->get(/all)    
  endif
  
  obj_destroy, [attributeNamesList, attributeValuesList]
  
  return, myLines
end


;+
; Handles a tag with attributes (i.e. {} enclosed arguments like in param or 
; keyword).
; 
; :Params:
;    lines : in, required, type=strarr
;       lines of raw text for that tag
;
; :Keywords:
;    routine : in, required, type=object
;       routine tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::_handleArgumentTag, lines, $
                                                  routine=routine, $
                                                  markup_parser=markupParser
  compile_opt strictarr, hidden
  
  lines = self->_parseTag(lines, /has_argument, $
                          tag=tag, argument=argument, $
                          n_attributes=nAttributes, $
                          attribute_names=attributeNames, $
                          attribute_values=attributeValues) 
  
  case strlowcase(tag) of
    'param': arg = routine->getParameter(argument, found=found)
    'keyword': arg = routine->getKeyword(argument, found=found)
    else:   ; shouldn't happen
  endcase
  
  routine->getProperty, name=routineName
  if (~found) then begin
    self.system->warning, strlowcase(tag) + ' ' + argument $
                            + ' not found in ' + routineName
    return
  endif

  routine->getProperty, file=file
  
  for i = 0L, nAttributes - 1L do begin
    case strlowcase(attributeNames[i]) of
      'in': arg->setProperty, is_input=1
      'out': arg->setProperty, is_output=1
      'optional': arg->setProperty, is_optional=1
      'required': arg->setProperty, is_required=1
      'private': arg->setProperty, is_private=1
      'hidden': arg->setProperty, is_hidden=1
      'obsolete': arg->setProperty, is_obsolete=1

      'type': arg->setProperty, type=attributeValues[i]
      'default': arg->setProperty, default_value=attributeValues[i]
      else: begin
          self.system->warning, $
            'unknown argument attribute "' + attributeNames[i] $
              + '" for argument ' + argument + ' in ' + routineName           
        end
    endcase
  endfor
    
  arg->setProperty, comments=markupParser->parse(lines, file=file)
end


;+
; Handles one tag in a routine's comments.
; 
; :Params:
;    tag : in, required, type=string
;       rst tag, i.e. returns, params, keywords, etc.
;    lines : in, required, type=strarr
;       lines of raw text for that tag
;
; :Keywords:
;    routine : in, required, type=object
;       routine tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::_handleRoutineTag, tag, lines, $
                                                 routine=routine,  $
                                                 markup_parser=markupParser
  compile_opt strictarr, hidden
  
  routine->getProperty, file=file
  
  case strlowcase(tag) of
    'abstract': routine->setProperty, is_abstract=1B
    'author': routine->setProperty, author=markupParser->parse(self->_parseTag(lines), file=file)
    'bugs': begin
        routine->setProperty, bugs=markupParser->parse(self->_parseTag(lines), file=file)
        self.system->createBugEntry, routine
      end      
    'categories': begin
        comments = self->_parseTag(lines)
        categories = strtrim(strsplit(strjoin(comments), ',', /extract), 2)
        for i = 0L, n_elements(categories) - 1L do begin
          if (categories[i] ne '') then begin
            routine->addCategory, categories[i]
            self.system->createCategoryEntry, categories[i], routine
          endif
        endfor
      end
    'copyright': routine->setProperty, copyright=markupParser->parse(self->_parseTag(lines), file=file)
    'customer_id': routine->setProperty, customer_id=markupParser->parse(self->_parseTag(lines), file=file)
    'examples': routine->setProperty, examples=markupParser->parse(self->_parseTag(lines), file=file)
    'field': begin
        ; fields are only allowed in routine named "classname__define"
        routine->getProperty, file=file, name=name
        classname = strmid(name, 0, strlen(name) - 8)        
        if (strlowcase(strmid(name, 7, /reverse_offset)) ne '__define') then begin
          self.system->warning, 'field not allowed in non-class definition routine'
          break
        endif
   
        ; parse
        comments = self->_parseTag(lines, /has_argument, $
                                   argument=fieldName, $
                                   n_attributes=nAttributes, $
                                   attribute_names=attributeNames, $
                                   attribute_values=attributeValues)                                        

        ; get the class tree object
        class = file->getClass(classname)
        
        ; add the field
        field = class->addField(fieldName, /get_only)
        if (obj_valid(field)) then begin        
          field->setProperty, name=fieldName, $
                              comments=markupParser->parse(comments)
        endif else begin
          self.system->warning, 'invalid field ' + fieldName
        endelse
      end
    'file_comments': begin
        routine->getProperty, file=file
        file->setProperty, comments=markupParser->parse(self->_parseTag(lines), file=file)
      end
    'hidden': routine->setProperty, is_hidden=1
    'hidden_file': begin
        routine->getProperty, file=file
        file->setProperty, is_hidden=1B
      end
    'history': routine->setProperty, history=markupParser->parse(self->_parseTag(lines), file=file)
    'inherits': begin
        routine->getProperty, name=name
        msg = '(%"obsolete tag ''%s'' at routine level in %s")'
        self.system->warning, string(format=msg, tag, name) 
      end    
    'keyword': self->_handleArgumentTag, lines, routine=routine, markup_parser=markupParser
    'obsolete': begin
        routine->setProperty, is_obsolete=1B
        self.system->createObsoleteEntry, routine
      end
    'param': self->_handleArgumentTag, lines, routine=routine, markup_parser=markupParser
    'post': routine->setProperty, post=markupParser->parse(self->_parseTag(lines), file=file)
    'pre': routine->setProperty, pre=markupParser->parse(self->_parseTag(lines), file=file)
    'private': routine->setProperty, is_private=1B
    'private_file': begin
        routine->getProperty, file=file
        file->setProperty, is_private=1B
      end
    'properties': begin
        routine->getProperty, name=name
        msg = '(%"properties tag at routine level in %s")'
        self.system->warning, string(format=msg, tag, name)        
      end      
    'requires': begin        
        requires = self->_parseTag(lines)
        
        ; look for an IDL version
        for i = 0L, n_elements(requires) - 1L do begin
          version = stregex(lines[i], '[[:digit:].]+', /extract)
          if (version ne '') then break
        endfor
         
        ; if you have a real version then check in with system
        if (version ne '') then begin
          self.system->checkRequiredVersion, version, routine
        endif
        
        routine->setProperty, requires=markupParser->parse(requires, file=file)
      end
    'restrictions': routine->setProperty, restrictions=markupParser->parse(self->_parseTag(lines), file=file)
    'returns': routine->setProperty, returns=markupParser->parse(self->_parseTag(lines), file=file)
    'todo': begin
        routine->setProperty, todo=markupParser->parse(self->_parseTag(lines), file=file)
        self.system->createTodoEntry, routine
      end
    'uses': routine->setProperty, uses=markupParser->parse(self->_parseTag(lines), file=file)
    'version': routine->setProperty, version=markupParser->parse(self->_parseTag(lines), file=file)
    else: begin
        routine->getProperty, name=name
        msg = '(%"unknown tag ''%s'' at routine level in %s")'
        self.system->warning, string(format=msg, tag, name)
      end
  endcase
end


;+
; Handles one tag in a file's comments.
; 
; :Params:
;    tag : in, required, type=string
;       rst tag, i.e. returns, params, keywords, etc.
;    lines : in, required, type=strarr
;       lines of raw text for that tag
;
; :Keywords:
;    file : in, required, type=object
;       file tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::_handleFileTag, tag, lines, $
                                              file=file, $
                                              markup_parser=markupParser
  compile_opt strictarr, hidden
  
  case strlowcase(tag) of
    'file_comments': begin
        file->setProperty, comments=markupParser->parse(self->_parseTag(lines), file=file)    
      end
    'property': begin
        comments = self->_parseTag(lines, /has_argument, $
                                   argument=propertyName, $
                                   n_attributes=nAttributes, $
                                   attribute_names=attributeNames, $
                                   attribute_values=attributeValues)                                        

        property = self->_addToHeldProperties(propertyName)
        for a = 0L, nAttributes - 1L do begin
          case strlowcase(attributeNames[a]) of
            'type': property->setProperty, type=attributeValues[a]
            else: self.system->warning, 'unknown property attribute: ' + attributeNames[a]
          endcase
        endfor
                
        property->setProperty, comments=markupParser->parse(comments, file=file)
      end
    
    'hidden': file->setProperty, is_hidden=1B
    'private': file->setProperty, is_private=1B
    
    'examples': file->setProperty, examples=markupParser->parse(self->_parseTag(lines), file=file)
    
    'author': file->setProperty, author=markupParser->parse(self->_parseTag(lines), file=file)
    'copyright': file->setProperty, copyright=markupParser->parse(self->_parseTag(lines), file=file)
    'history': file->setProperty, history=markupParser->parse(self->_parseTag(lines), file=file)
    'version': file->setProperty, version=markupParser->parse(self->_parseTag(lines), file=file)
    
    'abstract': file->setProperty, is_abstract=1B
    'bugs': begin
        self.system->createBugEntry, file
        file->setProperty, bugs=markupParser->parse(self->_parseTag(lines), file=file)
      end
    'categories': begin
        comments = self->_parseTag(lines)
        categories = strtrim(strsplit(strjoin(comments), ',', /extract), 2)
        for i = 0L, n_elements(categories) - 1L do begin
          if (categories[i] ne '') then begin
            file->addCategory, categories[i]
            self.system->createCategoryEntry, categories[i], file
          endif
        endfor
      end
    'customer_id': file->setProperty, customer_id=markupParser->parse(self->_parseTag(lines))
    'obsolete': begin
        self.system->createObsoleteEntry, file
        file->setProperty, is_obsolete=1B
      end
    'requires': begin
        requires = self->_parseTag(lines)
        
        ; look for an IDL version
        for i = 0L, n_elements(requires) - 1L do begin
          version = stregex(lines[i], '[[:digit:].]+', /extract)
          if (version ne '') then break
        endfor
         
        ; if you have a real version then check in with system
        if (version ne '') then begin
          self.system->checkRequiredVersion, version, file
        endif
            
        file->setProperty, requires=markupParser->parse(requires, file=file)
      end
    'restrictions': file->setProperty, restrictions=markupParser->parse(self->_parseTag(lines), file=file)
    'todo': begin
        file->setProperty, todo=markupParser->parse(self->_parseTag(lines), file=file)
        self.system->createTodoEntry, file
      end
    'uses': file->setProperty, uses=markupParser->parse(self->_parseTag(lines), file=file)

    'inherits': begin
        file->getProperty, basename=basename
        msg = '(%"obsolete tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename) 
      end         
    'field': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'post': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'pre': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'param': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'keyword': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'returns': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)        
      end
    
    else: begin
        file->getProperty, basename=basename
        msg = '(%"unknown tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)
      end
  endcase
end


;+
; Handles parsing of a comment block associated with a routine using IDLdoc 
; syntax. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
; :Keywords:
;    routine : in, required, type=object
;       routine tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::parseRoutineComments, lines, routine=routine, $
                                                    markup_parser=markupParser
  compile_opt strictarr, hidden
  
  ; find @ symbols that are the first non-whitespace character on the line
  tagLocations = where(stregex(lines, '^[[:space:]]*@') ne -1, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    routine->getProperty, file=file
    comments = markupParser->parse(lines[0:tagsStart - 1L], file=file)
    routine->setProperty, comments=comments
  endif

  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    tag = strmid(strtrim(stregex(lines[tagStart], '^[ ]*@[[:alpha:]_]+', /extract), 1), 1)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    self->_handleRoutineTag, tag, lines[tagStart:tagEnd], $
                             routine=routine, markup_parser=markupParser
  endfor
end


;+
; Handles parsing of a comment block associated with a file. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    file : in, required, type=object
;       file tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::parseFileComments, lines, file=file, $
                                                 markup_parser=markupParser                          
  compile_opt strictarr, hidden
  
  ; find @ symbols that are the first non-whitespace character on the line
  tagLocations = where(stregex(lines, '^[[:space:]]*@') ne -1L, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L], file=file)
    file->setProperty, comments=comments
  endif

  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    tag = strmid(stregex(lines[tagStart], '@[[:alpha:]_]+', /extract), 1)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    self->_handleFileTag, tag, lines[tagStart:tagEnd], $
                          file=file, markup_parser=markupParser
  endfor
end


;+
; Handles parsing of a comment block in the directory overview file using 
; IDLdoc syntax. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    directory : in, required, type=object
;       directory object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::parseDirOverviewComments, lines, directory=directory, $
                                                        markup_parser=markupParser
  compile_opt strictarr, hidden 
  
  ; find @ symbols that are the first non-whitespace character on the line
  tagLocations = where(stregex(lines, '^[[:space:]]*@') ne -1, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L])
    directory->setProperty, comments=comments
  endif

  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    tag = strmid(stregex(lines[tagStart], '@[[:alpha:]_]+', /extract), 1)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    tagLines = self->_parseTag(lines[tagStart:tagEnd])
    
    case strlowcase(tag) of
      'private': directory->setProperty, is_private=1B
      'hidden': directory->setProperty, is_hidden=1B
      'author': directory->setProperty, author=markupParser->parse(tagLines)
      'copyright': directory->setProperty, copyright=markupParser->parse(tagLines)
      'history': directory->setProperty, history=markupParser->parse(tagLines)      
      else: begin
          directory->getProperty, location=location
          self.system->warning, 'unknown tag "' + tag + '" in directory overview file for ' + location
        end
    endcase
  endfor  
end


;+
; Handles parsing of a comment block in the overview file using IDLdoc syntax. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    system : in, required, type=object
;       system object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparidldocformatparser::parseOverviewComments, lines, system=system, $
                                                     markup_parser=markupParser
  compile_opt strictarr, hidden

  ; find @ symbols that are the first non-whitespace character on the line
  tagLocations = where(stregex(lines, '^[[:space:]]*@') ne -1, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L])
    system->setProperty, overview_comments=comments
  endif

  system->getProperty, directories=directories

  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    tag = strmid(stregex(lines[tagStart], '@[[:alpha:]_]+', /extract), 1)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    tagLines = self->_parseTag(lines[tagStart:tagEnd])
    
    case strlowcase(tag) of
      'author': system->setProperty, author=markupParser->parse(tagLines)
      'copyright': system->setProperty, copyright=markupParser->parse(tagLines)
      'history': system->setProperty, history=markupParser->parse(tagLines)
      'version': system->setProperty, version=markupParser->parse(tagLines)

      'dir': begin
          re = '^[[:space:]]*([[:alnum:]._$\-\/]+)[[:space:]]*'
          argStart = stregex(tagLines[0], re, /subexpr, length=argLength)
          if (argStart[0] eq -1L) then begin
            system->getProperty, overview=overview
            system->warning, 'directory argument not present for dir tag in overview file ' + overview
            break            
          endif
          
          dirName = strmid(tagLines[0], argStart[1], argLength[1])
          if (strmid(dirName, 0, /reverse_offset) ne path_sep()) then begin
            dirName += path_sep()
          endif
          
          tagLines[0] = strmid(tagLines[0], argStart[1] + argLength[1])
          self->_removeSpace, tagLines
                    
          for d = 0L, directories->count() - 1L do begin
            dir = directories->get(position=d)
            dir->getProperty, location=location
            if (dirName eq location) then begin
              tree = markupParser->parse(tagLines)
              dir->setProperty, overview_comments=tree
              break
            endif
          endfor
        end
      else: begin
          system->getProperty, overview=overview
          system->warning, 'unknown tag "' + tag + '" in overview file ' + overview
        end
    endcase
  endfor
end


;+
; Define instance variables.
;-
pro docparidldocformatparser__define
  compile_opt strictarr, hidden

  define = { DOCparIDLdocFormatParser, inherits DOCparFormatParser }
end