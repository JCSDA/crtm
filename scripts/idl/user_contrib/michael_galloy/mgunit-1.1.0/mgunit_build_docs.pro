; docformat = 'rst'

;+
; Build mgunit docs.
;-
pro mgunit_build_docs
  compile_opt strictarr
  
  root = mg_src_root()

  idldoc, root=filepath('', subdir='src', root=root), $
          output=filepath('', subdir='api-docs', root=root), $
          /nosource, $
          title='MGunit documentation', subtitle='Unit testing for IDL', $
          /embed
end
