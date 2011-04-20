; docformat = 'rst'

;+
;  Handles parsing of the rst (restructured text) style comment blocks.
;-


;+
; Parse the lines from a tag; simply removes the tag and passes along the rest.
; 
; :Returns: 
;    strarr
;
; :Params:
;    lines : in, out, required, type=strarr
;       lines of text to parse
;-
function docparrstformatparser::_parseTag, lines
  compile_opt strictarr, hidden
  
  mylines = lines
  pos = stregex(lines[0], '^[[:space:]]*:[[:alpha:]_]+:[[:space:]]*', length=len)
  mylines[0] = strmid(lines[0], pos + len)
  
  while (strtrim(mylines[0], 2) eq '' && n_elements(mylines) gt 1L) do begin
    mylines = mylines[1:*]
  endwhile
  
  return, mylines                                           
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
pro docparrstformatparser::_handleFileTag, tag, lines, $
                                           file=file, $
                                           markup_parser=markupParser
  compile_opt strictarr, hidden
  
  case strlowcase(tag) of
    'file_comments' : begin
        file->setProperty, comments=markupParser->parse(self->_parseTag(lines), file=file)    
      end
    'properties': begin        
        ; there is no way to tell right now if this properties tag is allowed or
        ; not, must check later
        
        ; find number of spaces that properties' names are indented
        l = 1L
        nameIndent = -1L
        while (l lt n_elements(lines) && nameIndent eq -1L) do begin 
          nameIndent = stregex(lines[l++], '[[:alnum:]_$]')          
        endwhile
        
        ; empty properties tag, so quit
        if (nameIndent eq -1L) then break
        
        ; must indent property names
        if (nameIndent lt 1) then begin
          self.system->warning, 'invalid properties syntax'
          return
        endif              

        ; find properties' names lines (ignore first line, first property starts 
        ; on the line after :Properties:)        
        propLines = lines[1:*]
        re = string(format='(%"^[ ]{%d}([[:alnum:]_$]+)")', nameIndent)        
        propertyNamesStart = stregex(propLines, re, $
                                     /subexpr, length=propertyNamesLength)
        propertyDefinitionLines = where(propertyNamesStart[1, *] ne -1L, nProperties)
        
        ; add each property
        for p = 0L, nProperties - 1L do begin
          propertyName = strmid(propLines[propertyDefinitionLines[p]], $
                                propertyNamesStart[1, propertyDefinitionLines[p]], $
                                propertyNamesLength[1, propertyDefinitionLines[p]])
          property = self->_addToHeldProperties(propertyName)
          
          ; add property attributes: right now only TYPE=
          propattrs = strmid(propLines[propertyDefinitionLines[p]], $
                             propertyNamesStart[1, propertyDefinitionLines[p]] $ 
                               + propertyNamesLength[1, propertyDefinitionLines[p]])
                                                                        
          if (propattrs ne '') then begin
            colonpos = strpos(propattrs, ':')
            if (colonpos gt 0L && colonpos lt strlen(propattrs) - 1L) then begin
              propattrs = strmid(propattrs, colonpos + 1L)
              propattrs = strsplit(propattrs, ',', /extract, count=npropatts)  
              for pa = 0L, npropatts - 1L do begin                  
                eqpos = strpos(propattrs[pa], '=')
                if (eqpos ge 0L) then begin
                  attrnameTokens = strsplit(strmid(propattrs[pa], 0, eqpos), /extract, $
                                            count=nAttrnameTokens)
                  attrname = attrnameTokens[nAttrnameTokens - 1L]
                  if (strlowcase(attrname) ne 'type') then begin
                    self.system->warning, 'invalid property attribute: ' + attrname
                  endif else begin
                    proptype = strtrim(strmid(propattrs[pa], eqpos + 1), 2)                                
                    property->setProperty, type=proptype
                  endelse
                endif else begin
                  case strtrim(strlowcase(propattrs[pa]), 2) of
                    'hidden': property->setProperty, is_hidden=1B
                    'private': property->setProperty, is_private=1B
                    else:
                  endcase
                endelse
              endfor
            endif
          endif
          propertyDefinitionEnd = p eq nProperties - 1L $
                                    ? n_elements(propLines) - 1L $
                                    : propertyDefinitionLines[p + 1L] - 1L
          if (propertyDefinitionLines[p] + 1 le propertyDefinitionEnd) then begin
            comments = propLines[propertyDefinitionLines[p] + 1L:propertyDefinitionEnd] 
            property->setProperty, comments=markupParser->parse(comments, file=file)        
          endif  
        endfor                     
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
    'customer_id': file->setProperty, customer_id=markupParser->parse(self->_parseTag(lines), file=file)      
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
    
    'fields': begin
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
    'params': begin
        file->getProperty, basename=basename
        msg = '(%"routine level tag ''%s'' at file level in %s")'
        self.system->warning, string(format=msg, tag, basename)    
      end
    'keywords': begin
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
pro docparrstformatparser::_handleRoutineTag, tag, lines, routine=routine, $
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
    
    ; handle description tag for ITT VIS Workbench integration
    'description': routine->setProperty, comments=markupParser->parse(self->_parseTag(lines), file=file)
    'examples': routine->setProperty, examples=markupParser->parse(self->_parseTag(lines), file=file)
        
    'fields': begin
        ; fields are only allowed in routine named "classname__define"
        routine->getProperty, file=file, name=name
        classname = strmid(name, 0, strlen(name) - 8)        
        if (strlowcase(strmid(name, 7, /reverse_offset)) ne '__define') then begin
          self.system->warning, 'field not allowed in non-class definition routine'
          break
        endif
                                          
        ; find number of spaces that fields' names are indented
        l = 1L
        nameIndent = -1L
        while (l lt n_elements(lines) && nameIndent eq -1L) do begin 
          nameIndent = stregex(lines[l++], '[[:alnum:]_$]')          
        endwhile
        
        ; this is an empty fields tag, so quit
        if (nameIndent eq -1L) then break
        
        ; must indent fields names
        if (nameIndent lt 1) then begin
          self.system->warning, 'invalid fields syntax'
          return
        endif              

        ; find fields' names lines (ignore first line, first field starts 
        ; on the line after :Fields:)        
        fieldLines = lines[1:*]
        re = string(format='(%"^[ ]{%d}([[:alnum:]_$]+)")', nameIndent)        
        fieldNamesStart = stregex(fieldLines, re, $
                                  /subexpr, length=fieldNamesLength)
        fieldDefinitionLines = where(fieldNamesStart[1, *] ne -1L, nFields)
        
        ; get the class tree object
        class = file->getClass(classname)
        
        ; add each field
        for f = 0L, nFields - 1L do begin
         fieldName = strmid(fieldLines[fieldDefinitionLines[f]], $
                            fieldNamesStart[1, fieldDefinitionLines[f]], $
                            fieldNamesLength[1, fieldDefinitionLines[f]])
         field = class->addField(fieldName, /get_only)
         
         fieldDefinitionEnd = f eq nFields - 1L $
                                ? n_elements(fieldLines) - 1L $
                                : fieldDefinitionLines[f + 1L] - 1L
         if (fieldDefinitionLines[f] + 1L le fieldDefinitionEnd) then begin
           if (obj_valid(field)) then begin
             comments = fieldLines[fieldDefinitionLines[f] + 1L:fieldDefinitionEnd] 
             field->setProperty, name=fieldName, comments=markupParser->parse(comments, file=file)
           endif else begin
             self.system->warning, 'invalid field ' + fieldName
           endelse        
         endif  
        endfor             
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
    'inherits':  begin
        routine->getProperty, name=name
        msg = '(%"obsolete tag ''%s'' at routine level in %s")'
        self.system->warning, string(format=msg, tag, name)            
      end
    'keywords': self->_handleArgumentTag, lines, routine=routine, $
                                          markup_parser=markupParser, /keyword     
    'obsolete': begin
        routine->setProperty, is_obsolete=1B
        self.system->createObsoleteEntry, routine
      end          
    'params': self->_handleArgumentTag, lines, routine=routine, $
                                        markup_parser=markupParser    
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
;    keyword : in, optional, type=boolean
;       set to indicate the tag is a keyword
;-
pro docparrstformatparser::_handleArgumentTag, lines, $
                                               routine=routine, $
                                               markup_parser=markupParser, $
                                               keyword=keyword
  compile_opt strictarr, hidden
  
  ; find params/keywords
  tag = keyword_set(keyword) ? 'keyword' : 'param'
  
  ; find number of spaces that properties' names are indented
  l = 1L
  nameIndent = -1L
  while (l lt n_elements(lines) && nameIndent eq -1L) do begin 
    nameIndent = stregex(lines[l++], '[[:alnum:]_$]')          
  endwhile
  
  ; empty Params or Keywords tag, so quit      
  if (nameIndent eq -1L) then return
  
  ; must indent property names
  if (nameIndent lt 1) then begin
    self.system->warning, 'invalid ' + tag + 's syntax'
    return
  endif              

  ; find properties' names lines (ignore first line, first property starts 
  ; on the line after :Properties:)        
  paramLines = lines[1:*]
  re = string(format='(%"^[ ]{%d}([[:alnum:]_$]+)")', nameIndent)        
  paramNamesStart = stregex(paramLines, re, $
                            /subexpr, length=paramNamesLength)
  paramDefinitionLines = where(paramNamesStart[1, *] ne -1L, nParams)

  routine->getProperty, name=routineName
  
  ; add each property
  for p = 0L, nParams - 1L do begin
    paramName = strmid(paramLines[paramDefinitionLines[p]], $
                       paramNamesStart[1, paramDefinitionLines[p]], $
                       paramNamesLength[1, paramDefinitionLines[p]])
    param = keyword_set(keyword) $
              ? routine->getKeyword(paramName, found=found) $
              : routine->getParameter(paramName, found=found)
             
    if (~found) then begin     
      msg = string(format='(%"%s %s not found in %s")', tag, paramName, routineName)
      self.system->warning, msg       
      continue                      
    endif         
   
    headerLine = paramLines[paramDefinitionLines[p]]
    colonPos = strpos(headerLine, ':')
    if (colonPos ne -1L) then begin
      attributes = strsplit(strmid(headerLine, colonPos + 1L), ',', $
                            /extract, escape='\')
                           
      ; handle quoted attributes, like:
      ;
      ;   vertices : in, type="fltarr(2, m, n)" lost text, required
      ;
      ; breaks down into:
      ;
      ;   0: in
      ;   1: type="fltarr(
      ;   2:  m
      ;   3:  n)" lost text
      ;   4: required    
      for a = 0L, n_elements(attributes) - 1L do begin
        quote = strmid(attributes[a], strpos(attributes[a], '=') + 1L, 1)
        if (quote eq '"' || quote eq '`' || quote eq '''') then begin
          ; add following attributes until closing quote or end of line
          i = a + 1L
          while (i lt n_elements(attributes)) do begin
            t = attributes[i]
            attributes[a] += ',' + t
            attributes[i] = ''
            
            if (strpos(t, quote) ne -1L) then break
            
            i++
          endwhile
          
          ; fix up attributes[a] by removing quotes
          firstQuote = strpos(attributes[a], quote)
          secondQuote = strpos(attributes[a], quote, firstQuote + 1L)
          if (secondQuote eq -1L) then secondQuote = strlen(attributes[a])
          valueLength = secondQuote - firstQuote - 1L
          value = strmid(attributes[a], firstQuote + 1L, valueLength)
          attributes[a] = strmid(attributes[a], 0, firstQuote) + value                          
        endif
      endfor
     
      for a = 0L, n_elements(attributes) - 1L do begin
        self->_handleAttribute, param, strtrim(attributes[a], 2), routine=routine
      endfor
    endif
   
    paramDefinitionEnd = p eq nParams - 1L $
                           ? n_elements(paramLines) - 1L $
                           : paramDefinitionLines[p + 1L] - 1L
    if (paramDefinitionLines[p] + 1 le paramDefinitionEnd) then begin
      comments = paramLines[paramDefinitionLines[p] + 1L:paramDefinitionEnd] 
      routine->getProperty, file=file
      param->setProperty, comments=markupParser->parse(comments, file=file)        
    endif  
  endfor                       
end


;+
; Handle parameter/keyword attributes.
; 
; :Params:
;    param : in, required, type=object
;       argument tree object
;    attribute : in, required, type=string
;       attribute name and, optionally, value i.e. "in" or "type=string"
;
; :Keywords:
;    routine : in, required, type=object
;       routine tree object
;-
pro docparrstformatparser::_handleAttribute, param, attribute, routine=routine
  compile_opt strictarr, hidden
  
  if (attribute eq '') then return
  
  param->getProperty, name=paramName
  routine->getProperty, name=routineName  
  
  result = strsplit(attribute, '=', /extract)
  attributeName = strtrim(result[0], 2)
  attributeValue = (n_elements(result) gt 1) ? result[1] : ''
  
  case attributeName of
    'in': param->setProperty, is_input=1
    'out': param->setProperty, is_output=1
    'optional': param->setProperty, is_optional=1
    'required': param->setProperty, is_required=1
    'private': param->setProperty, is_private=1
    'hidden': param->setProperty, is_hidden=1
    'obsolete': param->setProperty, is_obsolete=1

    'type': param->setProperty, type=attributeValue
    'default': param->setProperty, default_value=attributeValue
    else: begin
        self.system->warning, $
          'unknown argument attribute "' + attributeName $
            + '" for argument ' + paramName + ' in ' + routineName           
      end
  endcase  
end
                                                  
;+
; Handles parsing of a routine level comment block using rst syntax. 
;
; :Params:
;    lines : in, required, type=strarr
;       all lines of the comment block
;
; :Keywords:
;    routine : in, required, type=object
;       routine tree object 
;    markup_parser : in, required, type=object
;       markup parser object
;-
pro docparrstformatparser::parseRoutineComments, lines, routine=routine,  $
                                                 markup_parser=markupParser
  compile_opt strictarr, hidden
  
  ; find tags enclosed by ":"s that are the first non-whitespace character on 
  ; the line
  tagLocations = where(stregex(lines, '^[[:space:]]*:[[:alpha:]_]+:') ne -1L, nTags)
  
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
    tagFull = stregex(lines[tagStart], ':[[:alpha:]_]+:', /extract)
    tag = strmid(tagFull, 1, strlen(tagFull) - 2L)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    self->_handleRoutineTag, tag, lines[tagStart:tagEnd], $
                             routine=routine, markup_parser=markupParser
  endfor
end


;+
; Handles parsing of a file level comment block using rst syntax. 
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
pro docparrstformatparser::parseFileComments, lines, file=file,  $
                                              markup_parser=markupParser
  compile_opt strictarr, hidden
  
  ; find tags enclosed by ":"s that are the first non-whitespace character on 
  ; the line
  re = '^[[:space:]]*:[[:alpha:]_]+:'
  tagLocations = where(stregex(lines, re) ne -1L, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L], file=file)
    file->setProperty, comments=comments
  endif  
  
  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    tagFull = stregex(lines[tagStart], ':[[:alpha:]_]+:', /extract)
    tag = strmid(tagFull, 1, strlen(tagFull) - 2L)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    self->_handleFileTag, tag, lines[tagStart:tagEnd], $
                          file=file, markup_parser=markupParser
  endfor  
end


;+
; Handles parsing of a comment block in the directory overview file using rst 
; syntax. 
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
pro docparrstformatparser::parseDirOverviewComments, lines, directory=directory, $
                                                     markup_parser=markupParser
  compile_opt strictarr, hidden

  ; find tags enclosed by ":"s that are the first non-whitespace character on 
  ; the line
  re = '^[[:space:]]*:[[:alpha:]_]+:'
  tagLocations = where(stregex(lines, re) ne -1L, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L])
    directory->setProperty, comments=comments
  endif
  
  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    re = ':[[:alpha:]_]+:'
    fullTag = stregex(lines[tagStart], re, /extract)
    tag = strmid(fullTag, 1, strlen(fullTag) - 2L)
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
; Handles parsing of a comment block in the overview file using rst syntax. 
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
pro docparrstformatparser::parseOverviewComments, lines, system=system, $
                                                  markup_parser=markupParser
  compile_opt strictarr, hidden

  ; find tags enclosed by ":"s that are the first non-whitespace character on 
  ; the line
  re = '^[[:space:]]*:[[:alpha:]_]+:'
  tagLocations = where(stregex(lines, re) ne -1L, nTags)
  
  ; parse normal comments
  tagsStart = nTags gt 0 ? tagLocations[0] : n_elements(lines)
  if (tagsStart ne 0) then begin
    comments = markupParser->parse(lines[0:tagsStart - 1L])
    system->setProperty, overview_comments=comments
  endif

  ; go through each tag
  for t = 0L, nTags - 1L do begin
    tagStart = tagLocations[t]
    re = ':[[:alpha:]_]+:'
    fullTag = stregex(lines[tagStart], re, /extract)
    tag = strmid(fullTag, 1, strlen(fullTag) - 2L)
    tagEnd = t eq nTags - 1L $
               ? n_elements(lines) - 1L $
               : tagLocations[t + 1L] - 1L
    tagLines = self->_parseTag(lines[tagStart:tagEnd])
    
    case strlowcase(tag) of
      'author': system->setProperty, author=markupParser->parse(tagLines)
      'copyright': system->setProperty, copyright=markupParser->parse(tagLines)
      'history': system->setProperty, history=markupParser->parse(tagLines)
      'version': system->setProperty, version=markupParser->parse(tagLines)
      
      'dirs': begin
          system->getProperty, directories=directories
          
          ; find number of spaces that directories' names are indented
          l = 0L
          nameIndent = -1L
          while (l lt n_elements(tagLines) && nameIndent eq -1L) do begin 
            nameIndent = stregex(tagLines[l++], '[[:alnum:]_$./]')          
          endwhile
  
          ; must indent directories' names
          if (nameIndent lt 1) then begin
            self.system->warning, 'invalid directories syntax'
            return
          endif
          
          ; find directories' names lines (ignore first line, first directory 
          ; starts on the line after :Dirs:)        
          dirLines = tagLines[0L:*]
          re = string(format='(%"^[ ]{%d}([[:alnum:]._$\-\/]+)")', nameIndent)        
          dirNamesStart = stregex(dirLines, re, $
                                  /subexpr, length=dirNamesLength)
          dirDefinitionLines = where(dirNamesStart[1, *] ne -1L, nDirs)                  
        
          ; add each property
          for d = 0L, nDirs - 1L do begin
            dirName = strmid(dirLines[dirDefinitionLines[d]], $
                             dirNamesStart[1, dirDefinitionLines[d]], $
                             dirNamesLength[1, dirDefinitionLines[d]])    

            if (strmid(dirName, 0, /reverse_offset) ne path_sep()) then begin
              dirName += path_sep()
            endif                                                                  
          
            dd = 0L
            done = 0B
            while (dd lt directories->count() && ~done) do begin
              dir = directories->get(position=dd)
              dir->getProperty, location=location              
              
              if (dirName eq location) then begin
                def_line = dirLines[dirDefinitionLines[d]]
                colon_pos = strpos(def_line, ':')
                if (colon_pos ne -1) then begin
                  attributes = strmid(def_line, colon_pos + 1L)
                  attrs = strsplit(attributes, ', ', /extract, count=nAttributes)
                  for a = 0L, nAttributes - 1L do begin
                    case strlowcase(attrs[a]) of
                      'private': dir->setProperty, is_private=1B
                      'hidden': dir->setProperty, is_hidden=1B
                      else: begin
                        system->getProperty, overview=overview
                        system->warning, 'unknown attribute "' + attrs[a] + '" for directory ' + location + ' in overview file ' + overview                        
                      end
                    endcase
                  endfor
                endif
                
                dirDefinitionEnd = d eq nDirs - 1L $
                                     ? n_elements(dirLines) - 1L $
                                     : dirDefinitionLines[d + 1L] - 1L
                if (dirDefinitionLines[d] + 1L le dirDefinitionEnd) then begin
                  comments = dirLines[dirDefinitionLines[d] + 1L:dirDefinitionEnd] 
                  dir->setProperty, overview_comments=markupParser->parse(comments)        
                endif                            
                done = 1B
              endif
              dd++
            endwhile
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
pro docparrstformatparser__define
  compile_opt strictarr, hidden

  define = { DOCparRstFormatParser, inherits DOCparFormatParser }
end
