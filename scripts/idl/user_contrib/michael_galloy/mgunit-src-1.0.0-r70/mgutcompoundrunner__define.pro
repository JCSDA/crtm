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
pro mgutcompoundrunner::reportTestSuiteStart, testsuite, $
                                              ntestcases=ntestcases, $
                                              ntests=ntests, $
                                              level=level
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestSuiteStart, testsuite, ntestcases=ntestcases, ntests=ntests, $
                             level=level
  endfor
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
pro mgutcompoundrunner::reportTestSuiteResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestSuiteResult, npass=npass, nfail=nfail, level=level
  endfor
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
pro mgutcompoundrunner::reportTestCaseStart, testcase, ntests=ntests, level=level
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestCaseStart, testcase, ntests=ntests, level=level
  endfor
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
pro mgutcompoundrunner::reportTestCaseResult, npass=npass, nfail=nfail, level=level
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestCaseResult, npass=npass, nfail=nfail, level=level
  endfor
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
pro mgutcompoundrunner::reportTestStart, testname, level=level
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestStart, testname, level=level
  endfor
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
pro mgutcompoundrunner::reportTestResult, msg, passed=passed, time=time
  compile_opt strictarr

  for i = 0L, self->count() - 1L do begin
    r = self->get(position=i)
    r->reportTestResult, msg, passed=passed, time=time
  endfor
end

     
;+
; Free resources.
;-
pro mgutcompoundrunner::cleanup
  compile_opt strictarr

  self->mguttestrunner::cleanup
  self->idl_container::cleanup
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
;-
function mgutcompoundrunner::init, filename=filename, _extra=e
  compile_opt strictarr

  if (~self->mguttestrunner::init(_extra=e)) then return, 0B
  if (~self->idl_container::init()) then return, 0B
  
  return, 1B
end


;+
; Define member variables.
;-
pro mgutcompoundrunner__define
  compile_opt strictarr
  
  define = { MGutCompoundRunner, $
             inherits MGutTestRunner, $
             inherits IDL_Container $
           }
end