; docformat = 'rst'

;+
; :History:
;    October 1, 2009: Ronn Kling donates original code
;-

pro xidldoc_event, event
  compile_opt strictarr
  
  widget_control, event.top, get_uvalue=object
  method = widget_info(event.id, /uname)
  if (method eq 'createNewFile') then call_method, method, object, event
  if (method eq 'Exit') then call_method, method, object, event
end


function xidldoc::parseToFirstBlankLine, i, numLines
  compile_opt strictarr
  
  value = ''
  for j = i + 1L, 100L do begin   ; limit of 100 description lines
    if strlen(strcompress((*self.pHeader)[j],/remove)) eq 0 then break ; first blank line
    if (j eq (i + 1L)) then begin
      value = (*self.pHeader)[j] 
    endif else begin
      value = [value, (*self.pHeader)[j]]
      numLines++
    endelse  
  endfor

  return, value
end


function xidldoc::parseToFirstColonOrBlankLine, i, numLines
  compile_opt strictarr
  
  for j=i+1,100 do begin ;limit of 100 description lines
    if strpos((*self.pHeader)[j],' : ') ge 0 then break ;first colon
    if strlen(strcompress((*self.pHeader)[j],/remove)) eq 0 then break ;first blank line
    if j eq (i+1) then value = (*self.pHeader)[j] else begin
      value = [value, (*self.pHeader)[j]]
      numLines++
    endelse  
  endfor

  return, value
end


function xidldoc::parseHeader, match, numLines, xsize, $
                               inout=inout, required=required, type=type
  compile_opt strictarr
  
  inout='in'
  required='required'
  type=' '
  if n_elements(xsize) ne 0 then void = temporary(xsize)
  found = 0
  numLines = 2
  value = ''
  if self.docIsPresent eq 0 then return,value ;nothing there

  for i=0,n_elements(*self.pHeader)-1 do begin
    line = (*self.pHeader)[i]
    if strpos(strupcase(line), match) lt 0 then continue
    case strupcase(match) of
    ':DATE:' : begin
      value = self->parseToFirstBlankLine(i, numLines)
      found = 1
    end  
    ':DESCRIPTION:' : begin
      value = self->parseToFirstBlankLine(i, numLines)
      found = 1
    end
    ':RETURNS:': begin
      value = self->parseToFirstBlankLine(i, numLines)
      found = 1
    end
    ':AUTHOR:': begin
      value = self->parseToFirstBlankLine(i, numLines)
      found = 1
    end  
    else: begin
      ;parse on the :
      tempStr = strsplit(line,':', /extract)
      ;parse on the ,
      tempStr = strsplit(tempStr[1],',', /extract)
      inout = strtrim(tempStr[0],2)
      required = strtrim(tempStr[1],2)
      ;type can have commas in it eg fltarr(3,3)
      type = strjoin(tempStr[2:*],',')
      value = self->parseToFirstColonOrBlankLine(i, numLines)
      found = 1
    end
    endcase
    if found eq 1 then break
  endfor

  xsize = max(strlen(value))
  return, value
end


pro xidldoc::findHeader, routine, isfunction=isfunction
  compile_opt strictarr
  
  if keyword_set(isfunction) then prefix = 'function ' else prefix = 'pro '

  openr,lun,/get,self.file
  line = ''
  headerArray = strarr(1000) ;if you have a header longer than a 1000 lines then you have other problems
  count = 0L
  record = 0
  while not eof(lun) do begin
    readf,lun,line
    if strpos(line,';+') eq 0 then record=1
    if record eq 1 then headerArray[count++] = strmid(line,1) ;dont need the semi-colon
    if strpos(line,';-') eq 0 then begin
      record = 0
      readf, lun, line
      if strpos(line,prefix + routine) eq 0 then begin  
        *self.pHeader = headerArray[0:count-1]
        break
      endif else begin
        headerArray = strarr(1000)
        count = 0L
      endelse
    endif
  endwhile
  free_lun, lun

end


pro xidldoc::buildItems, wTab1, list, isfunction=isfunction, Author=Author, Date=Date
  compile_opt strictarr
  
  device, get_screen_size=scrsz

  for i=0,n_elements(list)-1 do begin
    if self.docIsPresent eq 1 then self->findHeader, list[i], isfunction=isfunction
    if keyword_set(isfunction) then title = list[i] + '()' else title = list[i]
    wCommon = widget_base(wTab1,frame=0, column=1,title=title, uname=title,/scroll,y_scroll_size=scrsz[1]*0.8)
    void = cw_field(wCommon, title='Description',value=self->parseHeader(':DESCRIPTION:', numLines, xsize), /string ,$
           uname='Description', ysize=numLines, xsize=xsize)
    if keyword_set(isfunction) then begin
      void = cw_field(wCommon, title='Returns',value=self->parseHeader(':RETURNS:', numLines, xsize), /string , $
                      uname='Returns', ysize=numLines, xsize=xsize)
    endif
    if strlen(author) eq 0 then begin
      if self.docIsPresent eq 1 then author=self->parseHeader(':AUTHOR:', numLines, xsize)
    endif
    void = cw_field(wCommon, title='Author',value=Author, /string ,uname='Author', ysize=numLines, xsize=xsize)
    if n_elements(date) eq 0 then begin
      if self.docIsPresent eq 1 then date=self->parseHeader(':DATE:', numLines, xsize)
    endif
    void = cw_field(wCommon, title='Date', value=date, /string,uname='Date', ysize=numLines, xsize=xsize)
    ;get the param and keyword info
    resolve_routine, list[i], /either
    params = routine_info(list[i],/param, functions=isfunction)
    void = widget_label(wCommon,value='Parameters',uname='Parameters')
    if params.num_args eq 0 then void = widget_label(wCommon,value='NONE',uname='NONE')
    if params.num_args eq 1 && params.args[0] eq 'SELF' then void = widget_label(wCommon,value='NONE',uname='NONE')
    for j=0,params.num_args -1 do begin
      if params.args[j] eq 'SELF' then continue ;invisible passed parameter!
      wRow = widget_base(wCommon,/row, uname='row')
      void = cw_field(wRow, title=params.args[j], $
         value=self->parseHeader(params.args[j] +' :', inout=inout, required=required, Type=Type), $
         /string ,uname=params.args[j])
      void = widget_droplist(wRow,value=['in','out'],uname='inout')
      if inout eq 'in' then widget_control, void, set_droplist_select=0 else widget_control, void, set_droplist_select=1
      void = widget_droplist(wRow,value=['required','optional'],uname='required')
      if required eq 'required' then widget_control, void, set_droplist_select=0 else widget_control, void, set_droplist_select=1
      void = cw_field(wRow, title='Type',value=type, /string ,uname='type')
    endfor
    void = widget_label(wCommon,value=' ',uname='blank')
    void = widget_label(wCommon,value='Keywords', uname='Keywords')
    if params.num_kw_args eq 0 then void = widget_label(wCommon,value='NONE',uname='NONE')
    for j=0,params.num_kw_args -1 do begin
      wRow = widget_base(wCommon,/row,uname='row')
      void = cw_field(wRow, title=params.kw_args[j], $
      value=self->parseHeader(params.kw_args[j] +' :', inout=inout, required=required, Type=Type), $ 
      /string ,uname=params.kw_args[j])
      void = widget_droplist(wRow,value=['in','out'],uname='inout')
      if inout eq 'in' then widget_control, void, set_droplist_select=0 else widget_control, void, set_droplist_select=1    
      void = widget_droplist(wRow,value=['required','optional'],uname='required')
      if required eq 'required' then widget_control, void, set_droplist_select=0 else widget_control, void, set_droplist_select=1
      void = cw_field(wRow, title='Type',value=type, /string ,uname='type')
    endfor
  endfor

end


pro xidldoc::extractItems, lunOut,  tabName, isfunction=isfunction
  compile_opt strictarr
  
  printf, lunOut, ';+'
  printf, lunOut, ';'
  wRoot = widget_info(self.wBase, find_by_uname=tabName)
  wAllChildren = widget_info(wRoot, /all_children)
  for i=0,n_elements(wAllChildren)-1 do begin 
    case widget_info( wAllChildren[i],/uname) of
    'Description' : begin
      widget_control,wAllChildren[i],get_value=value
      printf, lunOut, ';  :Description:'
      for j=0,n_elements(value)-1 do printf, lunOut, ';  ' + value[j]
      printf, lunOut, ';'
    end
    'Returns' : begin
      printf,lunOut, ';  :Returns:'
      widget_control,wAllChildren[i],get_value=value
      for j=0,n_elements(value)-1 do printf, lunOut, ';  ' + value[j]
      printf, lunOut, ';'
    end
    'Parameters' : begin
      printf, lunOut, ';  :Params:'
    end
    'row' : begin
      wRowChildren = widget_info(wAllChildren[i], /all_children)
      ;    numROIs : out, optional, type=long
      ;      number of ROIs in this object
      varName = widget_info( wRowChildren[0],/uname)
      widget_control, wRowChildren[1], get_value=inout
      inoutIndex = widget_info(wRowChildren[1], /droplist_select)
      widget_control, wRowChildren[2], get_value=required
      ireqIndex = widget_info(wRowChildren[2], /droplist_select)
      widget_control, wRowChildren[3], get_value=type
      ;typeIndex = widget_info(wRowChildren[3], /droplist_select)
      widget_control, wRowChildren[0], get_value=value
      printf, lunOut, ';    ' + varName + ' : ' + strjoin([inout[inoutIndex],required[ireqIndex],type],',')
     for j=0,n_elements(value)-1 do  printf, lunOut, ';     ' + value[j]
    
    end
    'blank' :
    'Keywords' : begin
      printf, lunOut, ';'
      printf, lunOut, ';  :Keywords: '
    end
    'NONE' : begin
    printf, lunOut, ';    none'
    end
    else : begin
      printf, lunOut,  ';  :' + strcompress(widget_info( wAllChildren[i],/uname) + ':',/remove)
      widget_control,wAllChildren[i],get_value=value
      for j=0,n_elements(value)-1 do printf, lunOut, ';  ' + value[j]
      printf, lunOut, ';'
    end
    endcase
  endfor

  printf, lunOut, ';'

  ;common ones last
  ;wRoot = widget_info(self.wBase, find_by_uname='Common Entries')
  ;wAllChildren = widget_info(wRoot, /all_children)
  ;for i=0,n_elements(wAllChildren)-1 do begin 
  ;  printf, lunOut,  ';  :' + strcompress(widget_info( wAllChildren[i],/uname) + ':',/remove)
  ;  widget_control,wAllChildren[i],get_value=value
  ;  printf, lunOut, ';  ' + value
  ;  printf, lunOut, ';'
  ;endfor

  printf, lunOut, ';-'

end


pro xidldoc::createNewFile, event
  compile_opt strictarr
  
  ;make sure we aren't alread in the doc directory
  cd, current=pwd
  tempStr = strsplit(pwd,path_sep(),/extract, count=count)
  if tempStr[count-1] eq 'doc' then cd,'..'
  file_mkdir,'doc'

  openw,lunOut,/get,'doc/'+self.file
  openr,lun,/get,self.file
  line = ''
  writeLine = 1
  ;check to see if it already has the ; docformat = 'rst' header
  readf,lun,line
  if strpos(line, "; docformat = 'rst'") eq 0 then begin
    printf,lunOut,line
  endif else begin
    printf, lunout, "; docformat = 'rst'"
    point_lun,lun,0
  endelse

  while not eof(lun) do begin
    readf,lun,line
    if strpos(line,'pro') eq 0 then begin
      tempStr = strsplit(line,',',/extract)
      tempStr = strsplit(tempStr[0],' ',/extract)
      self->extractItems, lunOut,tempStr[1]
      printf,lunOut,line
    endif else if strpos(line,'function') eq 0 then begin
      tempStr = strsplit(line,',',/extract)
      tempStr = strsplit(tempStr[0],' ',/extract)
      self->extractItems, lunOut,tempStr[1] + '()'
      printf,lunOut,line
    endif else begin
      if strpos(line,';+') eq 0 then writeLine = 0
      if strpos(line,';-') eq 0 then begin
        writeLine = 1
        continue
      endif
      if writeLine eq 1 then printf,lunOut,line
    endelse
  endwhile

  free_lun, lun
  free_lun, lunOut
  
end


pro xidldoc::exit, event
  compile_opt strictarr
  
  widget_control, event.top, get_uvalue=object
  obj_destroy, object
  widget_control, event.top, /destroy
end



function xidldoc::find, file, procedures=procedures, functions=functions, $
                        lineNumbers=lineNumbers

  if keyword_set(functions) then prefix = 'function ' else prefix = 'pro '

  returnList = ''
  lineNumbers = 0
  openr,lun,/get,file
  line = ''
  ;see if it has already been documented
  readf,lun, line
  if strpos(line,"docformat = 'rst'") ge 0 then self.docIsPresent = 1
  ;rewind file
  point_lun,lun,0
  counter = 0L
  while not eof(lun) do begin
    readf,lun,line
    counter++
    if strpos(line,prefix) eq 0 then begin
      ;separate first on commas
      tempStr = strsplit(line,',',/extract)
      ;next on space
      tempStr = strsplit(tempStr[0],' ',/extract)
      returnList = [returnList, tempStr[1]]
      lineNumbers = [lineNumbers, counter]
    endif
  endwhile
  free_lun,lun

  if n_elements(lineNumbers) gt 1 then  lineNumbers = lineNumbers[1:*]
  if n_elements(returnList) gt 1 then return,returnList[1:*] else return,''
end


pro xidldoc::cleanup
  compile_opt strictarr
  
  ptr_free, self.pHeader
end


function xidldoc::init, file, wBase, Author=Author, Date=Date
  compile_opt strictarr
  
  if not keyword_set(Author) then Author=''

  file = file_basename(file,'.pro')
  self.file = file + '.pro'

  self.pHeader = ptr_new(0) ;header will get stored here
  ;make sure it is compiled
  ;resolve_routine,file, /compile_full, /either, /no_recompile

  ;get the procedures first
  procedureList = self->find(file+'.pro',/procedures, lineNumbers=proLineNumbers)

  ;next the functions
  functionList = self->find(file+'.pro',/functions, lineNumbers=funcLineNumber)

  ;now build the GUI
  wBase = widget_base(title='IDLdocGUI',/column, mbar=barBase,tab_mode=1)
  ;pull down menu next
  fileId = widget_button(barBase, value='File', /menu)
  void = widget_button(fileID, value='Create New File',uname='createNewFile')
  void = widget_button(fileId, /separator, value='Exit', uname='Exit')

  ;things that are shared among all procedures and functions
  subBase = widget_base(wBase, row=1)
  wTab1 = widget_tab(subBase, uname='');, xsize=scrsz[0]/4)
  ;wCommon = widget_base(wTab1,frame=0, column=1,title='Common Entries', uname='Common Entries')


  if procedureList[0] ne '' then self->buildItems, wTab1, procedureList, Author=Author, Date=Date
  if functionList[0] ne '' then self->buildItems, wTab1, functionList, /isfunction, Author=Author, Date=Date

  widget_control, wBase,/realize
  widget_control, wBase, set_uvalue=self

  self.wBase = wBase

  return, 1
end


pro xidldoc__define
  compile_opt strictarr
  
  void = { xidldoc, $        
           file : '', $
           docIsPresent : 0, $        
           pHeader : ptr_new(), $        
           wBase : 0L $
         }
end


pro xidldoc, author=author, date=date
  compile_opt strictarr
  
  file = dialog_pickfile(title='Choose File to Document', filter='*.pro', $
                         /must_exist, get_path=path)
  if (file eq '') then return

  cd, path

  xidldoc = obj_new('xidldoc', file, wBase, author=author, date=date)

  xmanager, 'xidldoc', wBase, /no_block, event_handler='xidldoc_event'
end
