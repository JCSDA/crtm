PRO sort_uniq_data, Frequency,  $ ; Input/Output
                     Response     ; Input/Output
                    
  ; Ensure Frequencies are 
  ; monotonically increasing                      
  idx = SORT(Frequency) 
  Frequency = Frequency[idx]
  Response = Response[idx]
  
  ; Ensure there are no duplicate
  ; frequencies
  idx = UNIQ(Frequency)
  Frequency = Frequency[idx] 
  Response = Response[idx]
   
END ; FUNCTION sort_uniq_data
