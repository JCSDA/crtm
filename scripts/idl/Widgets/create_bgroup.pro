;---------------------
; Procedure to cleanup
;---------------------
PRO CleanUp_BGroup, id

  ; Get top level base info state
  GetState, id, info

  ; Print debug statement if required
  IF ( info.debug EQ 1 ) THEN PRINT, 'DEBUG: CleanUp_BGroup'

  ; Free the top level base info state
  FreeState, id

END ; PRO CleanUp_BGroup


; ---------------------------------
; Function to create a button group
; ---------------------------------
FUNCTION Create_BGroup, event_func, $
                        button_name, $
                        PARENT       =parent, $
                        GROUP_LEADER =group_leader, $
                        VARIABLE_NAME=variable_name, $
                        SENSITIVE    =sensitive, $
                        VALUE        =value, $
                        TITLE        =title, $
                        MAP          =map, $
                        DEBUG        =debug

  ; Check keywords
  ; --------------
  IF ( N_ELEMENTS(group_leader) EQ 0 ) THEN group_leader = 0
  IF ( N_ELEMENTS(value       ) EQ 0 ) THEN value = 'Select'
  IF ( N_ELEMENTS(title       ) EQ 0 ) THEN title = 'BGroup'
  IF ( N_ELEMENTS(debug       ) EQ 0 ) THEN debug = 0

  ; Print debug statement if required
  IF ( debug EQ 1 ) THEN PRINT, 'DEBUG: Create_BGroup'

  ; Create a new base and label
  ; ---------------------------
  column = 1
  IF ( N_ELEMENTS(parent) EQ 0 ) THEN BEGIN
    base_id = WIDGET_BASE( GROUP_LEADER  =group_leader, $
                           COLUMN        =column, $
                           KILL_NOTIFY   ='CleanUp_BGroup', $
                           TITLE         =title, $
                           TLB_FRAME_ATTR=1+8 )
  ENDIF ELSE BEGIN
    base_id = WIDGET_BASE( parent, $
                           GROUP_LEADER=Group_leader, $
                           COLUMN      =column, $
                           KILL_NOTIFY ='CleanUp_BGroup', $
                           FRAME       =2 )
  ENDELSE
  label_id = WIDGET_LABEL( base_id, $
                           GROUP_LEADER=base_id, $
                           VALUE       =value )

  ; Create the button group
  ; -----------------------
  bgroup_id = CW_BGROUP( base_id, $
                         button_name, $
                         COLUMN      =1, $
                         EVENT_FUNC  =event_func, $
                         EXCLUSIVE   =1, $
                         IDS         =button_id, $
                         MAP         =map, $
                         NO_RELEASE  =1, $
                         RETURN_INDEX=1 )

  ; Create the info structure
  ; -------------------------
  bgroup_info = { debug       : debug, $
                  tlb_id      : group_leader, $
                  base_id     : base_id, $
                  bgroup_id   : bgroup_id, $
                  button_name : button_name, $
                  button_id   : button_id }
                  
  ; Store the info structure in its top level base id
  ; -------------------------------------------------
  WIDGET_CONTROL, base_id, SET_UVALUE=PTR_NEW(bgroup_info)
  SetState, base_id, bgroup_info

  ; Realise the button group heirarchy and return
  ; ---------------------------------------------
  WIDGET_CONTROL, base_id, /REALIZE, SENSITIVE=sensitive
  RETURN, base_id

END ; PRO Create_BGroup
