;+
; Function to determine if a sensorinfo linked list is empty

FUNCTION SensorInfo_List::Is_Empty
;- 
  ; Setup
  @error_codes
  
  ; Is there a valid first node?
  IF ( NOT PTR_VALID(self.First) ) THEN RETURN, TRUE
  IF ( NOT PTR_VALID((*self.First).Next) ) THEN RETURN, TRUE

  ; Yes, there is.
  RETURN, FALSE
  
END ; FUNCTION SensorInfo_List::Is_Empty
