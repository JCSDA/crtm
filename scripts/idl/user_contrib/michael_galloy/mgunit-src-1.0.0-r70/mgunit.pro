; docformat = 'rst'

;+
; Runs unit tests. 
; 
; :Params:
;    tests : in, optional, type=strarr
;       array of test suites and/or test cases
;
; :Keywords:
;    filename : in, optional, type=string
;       name of file to send output to; if not present sends output to the 
;       output log
;    color : in, optional, type=boolean
;       set to print color output to the output log
;    html : in, optional, type=boolean
;       set to indicate HTML output instead of plain text
;    gui : in, optional, type=boolean
;       set to bring up an interactive GUI to run the tests
;    npass : out, optional, type=long
;       number of tests that passed
;    nfail : out, optional, type=long
;       number of tests that failed
;    ntests : out, optional, type=long
;       number of tests      
;-
pro mgunit, tests, filename=filename, html=html, gui=gui, $
            color=color, $
            npass=npass, nfail=nfail, ntests=ntests
  compile_opt strictarr

  case 1 of 
    keyword_set(gui): runnerName = 'MGutGuiRunner'
    keyword_set(html): runnerName = 'MGutHtmlRunner'
    else: runnerName = 'MgutCliRunner'
  endcase
    
  if (n_elements(tests) gt 0) then begin
    testRunner = obj_new('MGutCompoundRunner')
    
    npass = 0L
    nfail = 0L

    testsuite = obj_new('MGutTestSuite', $
                        test_runner=testRunner, $
                        name='All tests')

    testRunner->add, obj_new(runnerName, parent=testRunner, $
                             filename=filename, color=color, $
                             test_suite=testsuite)
  
    testsuite->add, tests
    testsuite->run
    testsuite->getProperty, npass=npass, nfail=nfail, ntests=ntests
    
    if (~keyword_set(gui)) then obj_destroy, testRunner
  endif
end
