; docformat = 'rst'

;+
; To create a test case just inherit from MGtestCase and create method with 
; names that start with "test". This test can be run with the command::
;
;    mgunit, cases='findgentest' ;-
;-


;+
; This test should pass the assertion and return 1 (i.e. success). Tests can
; also return 0 or generate an error to indicate failure.
;-
function findgen_ut::test_basic
  compile_opt strictarr
  
  a = findgen(5)
  assert, array_equal(a, [0.0, 1.0, 2.0, 3.0, 4.0]), 'Correct elements'

  return, 1
end


;+
; This test fails because the assertion is wrong. 
;-
function findgen_ut::test_fail_example
  compile_opt strictarr
  
  a = findgen(5)  
  assert, n_elements(a) eq 6, 'Wrong number of elements' 

  return, 1
end


;+
; This is a test that will pass because the code of the test is supposed to
; cause an error. To do this kind of test, use the "error_is_pass" batch file.
;-
function findgen_ut::test_error
  compile_opt strictarr
  @error_is_pass

  a = findgen('string')

  return, 0
end


;+
; This is a test that will fail on an io error because of the use of the
; "error_is_fail" batch file. IO errors don't normally cause a test to fail.
;-
function findgen_ut::test_baderror
  compile_opt strictarr  
  @error_is_fail

  a = findgen('another_string')

  return, 1
end


;+
; Inherit from MGtestCase.
;-
pro findgen_ut__define
  compile_opt strictarr
  
  define = { findgen_ut, inherits MGutTestCase }
end
