; docformat = 'rst'

;+
; Generate documentation for IDL code. This is a wrapper routine for the 
; doc_system class; this routine only handles errors, saving/restoring !path, 
; and creating the doc_system object.
;
; :Author:
;    Michael Galloy
; 
; :Copyright:
;    IDLdoc is released under a BSD-type license
; 
; :Requires: 
;    IDL 6.2
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
;    source_link : in, optional, type=long, default=0L
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
;    error : out, optional, type=long
;       error code from run; 0 indicates no error
;    debug : in, optional, type=boolean
;       set to allow a crash with stack trace instead of just a simple message
;    help : in, optional, type=boolean
;       if set, print out a help message instead of running IDLdoc
;    version : in, optional, type=boolean
;       if set, print the IDLdoc version instead of running IDLdoc
;
;    color_outputlog : in, optional, type=boolean
;       set to color output log messages
;-
pro idldoc, root=root, $
            output=output, $
            quiet=quiet, $
            silent=silent, $
            n_warnings=nWarnings, $
            log_file=logFile, $
            assistant=assistant, $
            embed=embed, $
            overview=overview, $
            footer=footer, $
            title=title, $
            subtitle=subtitle, $
            nonavbar=nonavbar, $
            nosource=nosource, $
            source_link=sourceLink, $
            user=user, $
            statistics=statistics, $
            index_level=indexLevel, $
            routine_line_cutoffs=routineLineCutoffs, $
            complexity_cutoffs=complexityCutoffs, $
            format_style=formatStyle, $
            markup_style=markupStyle, $
            comment_style=commentStyle, $
            preformat=preformat, $
            browse_routines=browseRoutines, $
            template_prefix=templatePrefix, $
            template_location=templateLocation, $
            charset=charset, $
            error=error, debug=debug, $
            help=help, $
            version=version, $
            color_outputlog=colorOutputlog 
  compile_opt strictarr

  origPath = !path
  
  if (~keyword_set(debug) || arg_present(error)) then begin
    error = 0L
    catch, error
    if (error ne 0L) then begin
      catch, /cancel
      
      case 1 of
        keyword_set(unix): crlf = string([10B])
        keyword_set(windows): crlf = string([13B, 10B])
        else: crlf = string(!version.os_family eq 'unix' ? [10B] : [13B, 10B])
      endcase        
      errormsg = strcompress(strjoin(strsplit(!error_state.msg, crlf, /extract), ' '))
      
      message, errormsg, /informational
      !path = origPath
      return
    endif
  endif
  
  system = obj_new('DOC_System', $
                   root=root, $
                   output=output, $
                   quiet=quiet, $
                   silent=silent, $
                   n_warnings=nWarnings, $
                   log_file=logFile, $
                   assistant=assistant, $
                   embed=embed, $
                   overview=overview, $
                   footer=footer, $
                   title=title, $
                   subtitle=subtitle, $
                   nonavbar=nonavbar, $
                   nosource=nosource, $
                   source_link=sourceLink, $
                   user=user, $
                   statistics=statistics, $
                   index_level=indexLevel, $
                   routine_line_cutoffs=routineLineCutoffs, $
                   complexity_cutoffs=complexityCutoffs, $                   
                   format_style=formatStyle, $
                   markup_style=markupStyle, $
                   comment_style=commentStyle, $
                   preformat=preformat, $
                   browse_routines=browseRoutines, $
                   template_prefix=templatePrefix, $
                   template_location=templateLocation, $
                   charset=charset, $
                   help=help, $
                   version=version, $
                   color_outputlog=colorOutputlog)
  
  !path = origPath
  path_cache, /clear, /rebuild
  obj_destroy, system
end