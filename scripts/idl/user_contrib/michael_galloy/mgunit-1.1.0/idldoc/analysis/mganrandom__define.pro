; docformat = 'rst'

;+
; Pull random data from `random.org <http://random.org>`.
;-

;+
; Read from an URL (with error checking).
;
; :Returns: 
;    strarr
;
; :Params:
;    urlString : in, required, type=string
;       complete URL to query
;
; :Keywords:
;    error : out, optional, type=long
;       pass a named variable to get the response code: 0 for success, 
;       anything else indicates a failure
;-
function mganrandom::_getData, urlString, error=error
  compile_opt strictarr
  
  error = 0L
  catch, error
  if (error ne 0L) then begin
    catch, /cancel
    self.url->getProperty, response_code=error
    return, ''
  endif
  
  return, self.url->get(url=urlString, /string_array)
end


;+
; Returns a permutation of the given range of integers.
; 
; :Returns: 
;    lonarr
;
; :Keywords:
;    minimum : in, optional, type=long, default=0
;       minimum value of returned integers
;    maximum : in, optional, type=long, default=100
;       maximum value of returned integers
;    error : out, optional, type=long
;       pass a named variable to get the response code: 0 for success, 
;       anything else indicates a failure
;-
function mganrandom::getSequence, minimum=minimum, maximum=maximum, $
                                  error=error
  compile_opt strictarr
  
  _minimum = n_elements(minimum) eq 0 ? 0 : minimum
  _maximum = n_elements(maximum) eq 0 ? 100 : maximum
  
  format = '(%"%s/sequences/?min=%d&max=%d&format=plain&rnd=new")'
  urlString = string(self.randomUrl, _minimum, _maximum, format=format)
  
  result = self->_getData(urlString, error=error)
  
  return, long(result)
end


;+
; Return the given number of random integers (with repetition).
;
; :Returns: 
;    lonarr
;
; :Params:
;    number : in, required, type=long
;       number of random numbers to generate
;
; :Keywords:
;    minimum : in, optional, type=long, default=0
;       minimum value of returned integers
;    maximum : in, optional, type=long, default=100
;       maximum value of returned integers
;    error : out, optional, type=long
;       pass a named variable to get the response code: 0 for success, 
;       anything else indicates a failure
;-
function mganrandom::getIntegers, number, minimum=minimum, maximum=maximum, $
                                   error=error
  compile_opt strictarr
  on_error, 2
  
  if (n_elements(number) eq 0) then begin
    message, 'number parameter required'
  endif
  
  _minimum = n_elements(minimum) eq 0 ? 0 : minimum
  _maximum = n_elements(maximum) eq 0 ? 100 : maximum
  
  format = '(%"%s/integers/?num=%d&min=%d&max=%d&col=1&base=10&format=plain&rnd=news")'
  urlString = string(self.randomUrl, number, _minimum, _maximum, $
                     format=format)

  result = self->_getData(urlString, error=error)
    
  return, long(result)
end


;+
; Free resources.
;-
pro mganrandom::cleanup
  compile_opt strictarr
  
  obj_destroy, self.url
end
 

;+
; Creates a random number generator.
;
; :Returns: 
;    1 if success, 0 if failure
;-
function mganrandom::init
  compile_opt strictarr
  
  self.url = obj_new('IDLnetURL')  
  self.randomURL = 'http://random.org'
  
  return, 1
end


;+
; Define instance variables.
; 
; :Fields:
;    url
;       IDLnetURL object used to communicate with random.org
;    randomURL 
;       URL of the random.org website which generates the random 
;       numbers
;-
pro mganrandom__define
  compile_opt strictarr
  
  define = { mganrandom, $
             url: obj_new(), $
             randomUrl: '' $
           }
end


; main-level example program

r = obj_new('MGanRandom')

window, /free
plot, r->getIntegers(100)

window, /free
plot, r->getSequence()

obj_destroy, r

end
