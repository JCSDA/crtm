; docformat = 'rst'

;+
; Results for tests, test cases, and test suites are reported to the test 
; runner. The MGutHTMLRunner displays the results in the output HTML file.
;-

;+
; Report a test suite has begun.
; 
; :Params:
;    testsuite : in, required, type=string
;       name of test suite
;
; :Keywords:
;    ntestcases : in, required, type=integer 
;       number of test suites/cases contained by the test suite
;    ntests : in, required, type=integer
;       number of tests contained in the hierarchy below this test suite
;    level : in, required, type=level
;       level of test suite
;-
pro mguthtmlrunner::reportTestSuiteStart, testsuite, $
                                          ntestcases=ntestcases, $
                                          ntests=ntests, $
                                          level=level
  compile_opt strictarr

  self->_print, self.lun, $
                '<ul class="testsuite"><li><span class="suitename">' $
                  + testsuite $
                  + '</span> test suite starting (' $
                  + strtrim(ntestcases, 2) + ' test suite' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + '/case' $
                  + (ntestcases eq 1 ? '' : 's') $
                  + ', ' $
                  + strtrim(ntests, 2) + ' test' + (ntests eq 1 ? '' : 's') $
                  + ')</li>'
end


;+
; Report the results of a test suite.
;
; :Keywords:
;    npass : in, required, type=integer
;       number of passing tests contained in the hierarchy below the test 
;       suite
;    nfail : in, required, type=integer 
;       number of failing tests contained in the hierarchy below the test 
;       suite
;    level : in, required, type=integer
;       level of test suite
;-
pro mguthtmlrunner::reportTestSuiteResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  self->_print, self.lun, $
                '<span class="results">Results: ' $
                  + strtrim(npass, 2) + ' / ' + strtrim(npass + nfail, 2) $
                  + ' tests passed</span></ul>'
end


;+
; Report a test case has begun.
; 
; :Params:
;    testcase : in, required, type=string
;       name of test case
;
; :Keywords:
;    ntests : in, required, type=integer
;       number of tests contained in this test case
;    level : in, required, type=level
;       level of test case
;-
pro mguthtmlrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  self->_print, self.lun, $
                '<ul class="testcase"><li><span class="casename">' + testcase $
                  + '</span> test case starting (' + strtrim(ntests, 2) $
                  + ' test' + (ntests eq 1 ? '' : 's') + ')</li>' 
  self->_print, self.lun, '<ol>'
end


;+
; Report the results of a test case.
;
; :Keywords:
;    npass : in, required, type=integer
;       number of passing tests
;    nfail : in, required, type=integer
;       number of failing tests
;    level : in, required, type=integer
;       level of test case
;-
pro mguthtmlrunner::reportTestCaseResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  self->_print, self.lun, '</ol>'
  self->_print, self.lun, $
                '<span class="results">Results: ' $
                  + strtrim(npass, 2) + ' / ' + strtrim(npass + nfail, 2) $
                  + ' tests passed</span></ul>'
end


;+
; Report the start of single test.
; 
; :Params:
;    testname : in, required, type=string
;       name of test
;
; :Keywords:
;    level : in, required, type=integer
;       level of test case
;-
pro mguthtmlrunner::reportTestStart, testname, level=level
  compile_opt strictarr

  self->_print, self.lun, '<li>' + testname + ': ', format='(A, $)'
end


;+
; Report the result of a single test.
; 
; :Params:
;    msg : in, required, type=string
;       message to display when test fails
;
; :Keywords:
;    passed : in, required, type=boolean
;       whether the test passed
;    time : in, required, type=float
;       time for the test to run
;-
pro mguthtmlrunner::reportTestResult, msg, passed=passed, time=time
  compile_opt strictarr

  result = keyword_set(passed) ? 'passed' : 'failed'

  s = string(result, $
             result, $
             keyword_set(passed) ? '': (msg eq '' ? '' : ' "' + msg + '"'), $
             time, $
             format='(%"<span class=\"%s\">%s%s</span> <span class=\"time\">%f seconds</span></li>")')

  self->_print, self.lun, s
end


;+
; Prints a message to a LUN.
;
; :Params:
;    lun : in, required, type=long
;       logical unit number to print to
;    text : in, required, type=string
;       text to print
;
; :Keywords:
;    _extra : in, optional, type=keywords
;       keywords to MG_ANSICODE i.e. RED or GREEN
;-
pro mguthtmlrunner::_print, lun, text, _extra=e
  compile_opt strictarr
  
  printf, lun, text, _extra=e
  if (lun gt 0L) then flush, lun
end

     
;+
; Free resources.
;-
pro mguthtmlrunner::cleanup
  compile_opt strictarr

  self->_print, self.lun, '<span id="dateline">Test results from ' + systime() + '</span>'
  self->_print, self.lun, '</body></html>'
  
  if (self.lun gt 0) then free_lun, self.lun
  self->mguttestrunner::cleanup
end


;+
; Initialize the test runner.
;
; :Returns: 
;    1 for success, 0 for failure
; 
; :Keywords: 
;    filename : in, optional, type=string
;       if present, output is sent that file, otherwise output is sent to 
;       stdout
;    color : in, optional, type=boolean
;       unused for MGutHtmlRunner
;    _extra : in, optional, type=keywords
;       keywords to MGutTestRunner::init
;-
function mguthtmlrunner::init, filename=filename, color=color, _extra=e
  compile_opt strictarr

  if (~self->mguttestrunner::init(_extra=e)) then return, 0B

  ; make the directory the output file is in if it doesn't exist
  if (n_elements(filename) gt 0) then begin
    dir = file_dirname(filename)
    if (~file_test(dir)) then file_mkdir, dir
  endif
  
  ; setup the LUN for the output
  if (n_elements(filename) gt 0) then begin
    openw, lun, filename, /get_lun
    self.lun = lun
  endif else begin
    self.lun = -1L
  endelse

  self->_print, self.lun, '<html><head>'
  self->_print, self.lun, '<title>Test results</title>'
  self->_print, self.lun, '<style type="text/css" media="all">'
  
  styleFilename = mg_src_root() + 'style.css'
  styles = strarr(file_lines(styleFilename))
  openr, styleLun, styleFilename, /get_lun
  readf, styleLun, styles
  free_lun, styleLun
  
  self->_print, self.lun, transpose(styles)
  self->_print, self.lun, '</style></head><body>'
  
  return, 1B
end


;+
; Define member variables.
;
; :Fields:
;    lun 
;       the logical unit number to send output to (-1L by default)
;-
pro mguthtmlrunner__define
  compile_opt strictarr
  
  define = { MGutHTMLRunner, inherits MGutTestRunner, $
             lun: 0L $
           }
end