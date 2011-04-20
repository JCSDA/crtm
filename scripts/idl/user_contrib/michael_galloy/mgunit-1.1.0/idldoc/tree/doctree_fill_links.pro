; docformat = 'rst'

;+
; Traverses the markup tree for some comments on a particular doctree* class 
; object. 
; 
; :Params:
;    comments : in, required, type=markup tree
;       comments to check for links in
;    tree : in, required, type=tree class object
;       one of the doctree* classes
;-
pro doctree_fill_links, comments, tree
  compile_opt strictarr

  if (~obj_valid(comments) || ~obj_isa(comments, 'MGtmTag')) then return
  
  comments->getProperty, type=type, n_children=nChildren
  
  if (type eq 'link') then begin
    reference = comments->getAttribute('reference')
    if (reference eq '' && nChildren gt 0L) then begin
      nameNode = comments->getChild(0L)
      
      ; check to make sure we haven't already made this code
      if (~obj_isa(nameNode, 'MGtmText')) then return
      
      nameNode->getProperty, text=name      
      reference = tree->lookupName(name)
      
      relative_root = tree->getVariable('relative_root')
      comments->addAttribute, 'reference', $
                              reference eq '' ? '' : (relative_root + reference)
      
      comments->removeChild, 0L
      
      codeTag = obj_new('MGtmTag', type='code')
      comments->addChild, codeTag
      codeTag->addChild, nameNode
    endif 
  endif else begin
    for c = 0L, nChildren - 1L do begin
      doctree_fill_links, comments->getChild(c), tree
    endfor
  endelse
end
