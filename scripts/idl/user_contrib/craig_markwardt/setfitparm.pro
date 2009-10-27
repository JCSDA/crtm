;+
; NAME:
;   SetFitParm.pro
;
; AUTHOR:
;	F.Bringezu, denet - Internetservice, Halle Germany,
;	bringezu@denet.de
;
; PURPOSE:
;   Provide a widget interface for creating a parinfo structure.
;   This parinfo structure can by used by mpfit routines of Craig B. Markwardt.
;
; MAJOR TOPICS:
;   Widget, mpfit.
;
; CALLING SEQUENCE:
;   parinfo=SetFitParm(used_parinfo)
;
; DESCRIPTION:
;
;   SetFitParm creates PARINFO using a widget interface.
;   PARINFO provides constraints for paramters used by the mpfit routines.
;
;   PARINFO is an array of structures, one for each parameter.
;
;   A detailed description can be found in the documentation of mpcurvefit.pro
;   This routine creates an array that contains a structure for each element.
;   The structure has the following entries.
;
;   - VALUE (DOUBLE): The starting parameter
;   - FIXED (BOOLEAN): 1 fix the parameter, 0 don't fix it at the
;     point given in VALUE.
;   - LIMITS (DBLARRAY(2)): Set upper and lower limit.
;   - LIMITED (BOOLEAN ARRAY 2):  Fix the limit.
;
;
;   The parameter OLDPARINFO is optional. OLDPARINFO is used to set
;   the default values in the widget.
;
;   You can simply run:
;   test=SetFitParm() to create the array for the first time.
;   Once the array is created it can be used to set the default values
;   in the widget by calling
;
;   test2=SetFitParm(test)
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;   OLDFITPARM - The default values of the new array
;
; INPUT KEYWORD PARAMETERS:
;
;   PARENT - if this widget is to be a child, set this keyword to the
;            parent widget ID.
;
; OUTPUT KEYWORD PARAMETERS:
;
;   CANCEL - if the user selected the cancel button on the SETFITPARM
;            widget, then this keyword will be set upon exit.
;
; OUTPUTS:
;   PARINFO array of structures
;
; SEE ALSO:
;   mpcurvefit
;
; MODIFICATION HISTORY:
;   Written, FB, 12/1999
;   Documented, FB, Jan 2000
;   Generalized positioning code, CM 01 Feb 2000
;
;-
; Copyright (C) 1999, F.Bringezu
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy and distribute unmodified copies for
; non-commercial purposes, and to modify and use for personal or
; internal use, is granted.  All other rights are reserved.
;-

Pro SetFitParm_Events, event

Widget_Control, event.id, Get_Value=buttonValue
Widget_Control, event.id, Get_UValue=buttonUValue

Widget_Control, event.top, Get_UValue=info, /No_Copy

CASE buttonUValue OF

	'Cancel' : Widget_Control, event.top, /Destroy

        'FIX_HEIGHT': (*info.ptr).fparm(0).fixed = fix(event.select)
        'FIX_XMAX':   (*info.ptr).fparm(1).fixed = fix(event.select)
        'FIX_WIDTH':  (*info.ptr).fparm(2).fixed = fix(event.select)
        'FIX_OFFSET': (*info.ptr).fparm(3).fixed = fix(event.select)
        'FIX_SLOPE':  (*info.ptr).fparm(4).fixed = fix(event.select)

        'LIMIT_HEIGHT_LOW': (*info.ptr).fparm(0).limited(0) = fix(event.select)
        'LIMIT_XMAX_LOW':   (*info.ptr).fparm(1).limited(0) = fix(event.select)
        'LIMIT_WIDTH_LOW':  (*info.ptr).fparm(2).limited(0) = fix(event.select)
        'LIMIT_OFFSET_LOW': (*info.ptr).fparm(3).limited(0) = fix(event.select)
        'LIMIT_SLOPE_LOW':  (*info.ptr).fparm(4).limited(0) = fix(event.select)

        'LIMIT_HEIGHT_UP':  (*info.ptr).fparm(0).limited(1) = fix(event.select)
        'LIMIT_XMAX_UP':    (*info.ptr).fparm(1).limited(1) = fix(event.select)
        'LIMIT_WIDTH_UP':   (*info.ptr).fparm(2).limited(1) = fix(event.select)
        'LIMIT_OFFSET_UP':  (*info.ptr).fparm(3).limited(1) = fix(event.select)
        'LIMIT_SLOPE_UP':   (*info.ptr).fparm(4).limited(1) = fix(event.select)

   'Accept' : BEGIN

         ; OK, get the information the user put into the form.
         ; Should do error checking, but...maybe later!

      Widget_Control, info.text(0), Get_Value=height
      Widget_Control, info.text(1), Get_Value=xmax
      Widget_Control, info.text(2), Get_Value=width
      Widget_Control, info.text(3), Get_Value=offset
      Widget_Control, info.text(4), Get_Value=slope

      Widget_Control, info.text(5), Get_Value=ll_height
      Widget_Control, info.text(6), Get_Value=ll_xmax
      Widget_Control, info.text(7), Get_Value=ll_width
      Widget_Control, info.text(8), Get_Value=ll_offset
      Widget_Control, info.text(9), Get_Value=ll_slope

      Widget_Control, info.text(10), Get_Value=ul_height
      Widget_Control, info.text(11), Get_Value=ul_xmax
      Widget_Control, info.text(12), Get_Value=ul_width
      Widget_Control, info.text(13), Get_Value=ul_offset
      Widget_Control, info.text(14), Get_Value=ul_slope

         ; Fill out the data structure with information
         ; collected from the form.

      (*info.ptr).fparm(0).value = height
      (*info.ptr).fparm(1).value = xmax
      (*info.ptr).fparm(2).value = width
      (*info.ptr).fparm(3).value = offset
      (*info.ptr).fparm(4).value = slope

      (*info.ptr).fparm(0).limits(0) = ll_height
      (*info.ptr).fparm(1).limits(0) = ll_xmax
      (*info.ptr).fparm(2).limits(0) = ll_width
      (*info.ptr).fparm(3).limits(0) = ll_offset
      (*info.ptr).fparm(4).limits(0) = ll_slope
      
      (*info.ptr).fparm(0).limits(1) = ul_height
      (*info.ptr).fparm(1).limits(1) = ul_xmax
      (*info.ptr).fparm(2).limits(1) = ul_width
      (*info.ptr).fparm(3).limits(1) = ul_offset
      (*info.ptr).fparm(4).limits(1) = ul_slope

      (*info.ptr).cancel = 0

         ; Destroy the widget program

      Widget_Control, event.top, /Destroy
      END
else:

ENDCASE





if buttonValue ne 'Cancel' and buttonValue ne 'Accept' then begin
Widget_Control, event.top, Set_UValue=info, /No_Copy

endif

END ;*******************************************************************

Function SetFitParm,	thisFParm,$
						Parent=parent,$
						Cancel=cancel

On_Error, 2

IF N_Elements(thisFParm) EQ 0 THEN $
fparm = replicate({value:0.D, fixed:0, limited:[0,0], limits:[0.D,0]}, 5) $
  	ELSE FParm=thisFParm

Device, Get_Screen_Size=screenSize
xCenter=FIX(screenSize(0)/2.)
yCenter=FIX(screenSize(1)/2.)
xoff=xCenter-150
yoff=yCenter-150


   ; Create a top-level base. Must have a Group Leader defined
   ; for Modal operation. If this widget is NOT modal, then it
   ; should only be called from the IDL command line as a blocking
   ; widget.

IF N_Elements(parent) NE 0 THEN $
   tlb = Widget_Base( GROUP_LEADER=parent $
      , UNAME='WID_BASE_0', XOffset=xoff, YOffset=yoff   $
      , XSIZE=380, YSIZE=320 ,TITLE='Parinfo Setup' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3, /Floating, /Modal ,/Base_Align_Center) ELSE $
   tlb = Widget_Base( UNAME='WID_BASE_0', XOffset=xoff, YOffset=yoff   $
      , XSIZE=380, YSIZE=320 ,TITLE='Parinfo Setup' ,SPACE=3 ,XPAD=3  $
      ,YPAD=3, /Base_Align_Center)


  WID_BASE_1 = Widget_Base(tlb, UNAME='WID_BASE_1' ,FRAME=1  $
      ,XOFFSET=10 ,YOFFSET=55 , XSIZE=350, YSIZE=200  $
      ,TITLE='IDL' ,SPACE=3 ,XPAD=3 ,YPAD=3)


  WID_LABEL_0 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_0'  $
      ,XOFFSET=10 ,YOFFSET=45, XSIZE=42, YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Ampl.')


  WID_LABEL_1 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_1'  $
      ,XOFFSET=10 ,YOFFSET=75 , XSIZE=42, YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='X(max)')


  WID_LABEL_2 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_2'  $
      ,XOFFSET=10 ,YOFFSET=105 , XSIZE=42 ,YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Width')

  WID_LABEL_3 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_3'  $
      ,XOFFSET=10 ,YOFFSET=135, XSIZE=42 ,YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Offset')

  WID_LABEL_4 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_4'  $
      ,XOFFSET=10 ,YOFFSET=165 ,XSIZE=42 ,YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Slope')


;;;;;;;;;;;;;;;;;;;  table headline ;;;;;;;;;;;;;;;;;;;;;;;;
;; with top labels VALUE AND LIMIT

  WID_LABEL_6 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_6'  $
      ,XOFFSET=60 ,YOFFSET=15, XSIZE=42, YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Value')



  WID_LABEL_8 = Widget_Label(WID_BASE_1, UNAME='WID_LABEL_8'  $
      ,XOFFSET=250 ,YOFFSET=15 ,XSIZE=40,YSIZE=18  $
      ,/ALIGN_LEFT ,VALUE='Limits')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  WID_TEXT_0 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_0' ,FRAME=1  $
      ,XOFFSET=60 ,YOFFSET=40 , VALUE=[strtrim(string(fparm(0).value),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable,/ALIGN_LEFT) ;;; Amplitude


  WID_TEXT_1 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_1' ,FRAME=1  $
      ,XOFFSET=60 ,YOFFSET=70, VALUE=[strtrim(string(fparm(1).value),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; X(max)


  WID_TEXT_2 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_2' ,FRAME=1  $
      ,XOFFSET=60 ,YOFFSET=100, VALUE=[strtrim(string(fparm(2).value),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Width

  WID_TEXT_3 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_3' ,FRAME=1  $
      ,XOFFSET=60 ,YOFFSET=130,VALUE=[strtrim(string(fparm(3).value),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Offset

  WID_TEXT_4 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_4' ,FRAME=1  $
      ,XOFFSET=60 ,YOFFSET=160 ,VALUE=[strtrim(string(fparm(4).value),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Slope

;;;;; Text widgets for lower limits

  WID_TEXT_5 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_5' ,FRAME=1  $
      ,XOFFSET=210 ,YOFFSET=40 ,VALUE=[strtrim(string(fparm(0).limits(0)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable,/ALIGN_LEFT) ;;; Amplitude


  WID_TEXT_6 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_6' ,FRAME=1  $
      ,XOFFSET=210 ,YOFFSET=70, VALUE=[strtrim(string(fparm(1).limits(0)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; X(max)


  WID_TEXT_7 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_7' ,FRAME=1  $
      ,XOFFSET=210 ,YOFFSET=100,VALUE=[strtrim(string(fparm(2).limits(0)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Width

  WID_TEXT_8 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_8' ,FRAME=1  $
      ,XOFFSET=210 ,YOFFSET=130, VALUE=[strtrim(string(fparm(3).limits(0)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Offset

  WID_TEXT_9 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_9' ,FRAME=1  $
      ,XOFFSET=210 ,YOFFSET=160 ,VALUE=[strtrim(string(fparm(4).limits(0)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Slope


;;;;; Text widgets for upper limits

  WID_TEXT_10 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_10' ,FRAME=1  $
      ,XOFFSET=290 ,YOFFSET=40, VALUE=[strtrim(string(fparm(0).limits(1)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable,/ALIGN_LEFT) ;;; Amplitude


  WID_TEXT_11 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_11' ,FRAME=1  $
      ,XOFFSET=290 ,YOFFSET=70, VALUE=[strtrim(string(fparm(1).limits(1)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; X(max)


  WID_TEXT_12 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_12' ,FRAME=1  $
      ,XOFFSET=290 ,YOFFSET=100, VALUE=[strtrim(string(fparm(2).limits(1)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Width

  WID_TEXT_13 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_13' ,FRAME=1  $
      ,XOFFSET=290 ,YOFFSET=130, VALUE=[strtrim(string(fparm(3).limits(1)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Offset

  WID_TEXT_14 = Widget_Text(WID_BASE_1, UNAME='WID_TEXT_14' ,FRAME=1  $
      ,XOFFSET=290 ,YOFFSET=160, VALUE=[strtrim(string(fparm(4).limits(1)),1)]  $
      ,XSIZE=5 ,YSIZE=1,/editable) ;;; Slope




;;;;;;;;;;;; Container for checkboxes and checkboxes for FIXED ;;;;;;;;;;;;;;;;

  WID_BASE_2 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_2'  $
      ,XOFFSET=110 ,YOFFSET=40 ,XSIZE=20 ,YSIZE=20  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER , ROW=1 ,/NONEXCLUSIVE)


  WID_BUTTON_0 = Widget_Button(WID_BASE_2,/ALIGN_CENTER,UVALUE='FIX_HEIGHT' $
      , VALUE='')
      ;;; Amplitude


  WID_BASE_3 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_3'  $
      ,XOFFSET=110 ,YOFFSET=70 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,/NONEXCLUSIVE)


  WID_BUTTON_1 = Widget_Button(WID_BASE_3 ,/ALIGN_LEFT,UVALUE='FIX_XMAX' $
      , VALUE='')
      ;;; X(max)


  WID_BASE_4 = Widget_Base(WID_BASE_1  $
      ,XOFFSET=110 ,YOFFSET=100 ,XSIZE=20 ,YSIZE=27 ,/ALIGN_TOP ,/BASE_ALIGN_CENTER, /NONEXCLUSIVE)


  WID_BUTTON_2 = Widget_Button(WID_BASE_4, /ALIGN_LEFT,UVALUE='FIX_WIDTH' $
      , VALUE='')
      ;;; Width

  WID_BASE_5 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_5'  $
      ,XOFFSET=110 ,YOFFSET=130 ,XSIZE=20 ,YSIZE=27, /ALIGN_TOP, /BASE_ALIGN_CENTER, /NONEXCLUSIVE)


  WID_BUTTON_3 = Widget_Button(WID_BASE_5, /ALIGN_LEFT, UVALUE='FIX_OFFSET' $
      ,  VALUE='')
      ;;; Slope

  WID_BASE_6 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_6'  $
      , XOFFSET=110 ,YOFFSET=160, XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER, /NONEXCLUSIVE)


  WID_BUTTON_4 = Widget_Button(WID_BASE_6,/ALIGN_LEFT, UVALUE='FIX_SLOPE' $
      ,  value='')
      ;;; Slope

;;;;;;;;;;;; Container for checkboxes and checkboxes for lower limited ;;;;;;;;;;;;;;;;

  WID_BASE_7 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_7'  $
      ,XOFFSET=180 ,YOFFSET=40 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_5 = Widget_Button(WID_BASE_7 $
       ,/ALIGN_LEFT,UVALUE='LIMIT_HEIGHT_LOW',  VALUE='')
      ;;; Height

  WID_BASE_8 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_8'  $
      ,XOFFSET=180 ,YOFFSET=70 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_6 = Widget_Button(WID_BASE_8 $
      ,/ALIGN_LEFT,UVALUE='LIMIT_XMAX_LOW', VALUE='')
      ;;; Xmax

  WID_BASE_9 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_9'  $
      ,XOFFSET=180 ,YOFFSET=100 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_7 = Widget_Button(WID_BASE_9  $
      ,/ALIGN_LEFT,UVALUE='LIMIT_WIDTH_LOW',  value='')
      ;;; Width

  WID_BASE_10 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_10'  $
      ,XOFFSET=180 ,YOFFSET=130 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_8 = Widget_Button(WID_BASE_10, UNAME='WID_BUTTON_8'  $
     ,/ALIGN_LEFT,UVALUE='LIMIT_OFFSET_LOW', value= '')
      ;;; Offset

  WID_BASE_11 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_11'  $
      ,XOFFSET=180 ,YOFFSET=160 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_9 = Widget_Button(WID_BASE_11, UNAME='WID_BUTTON_9'  $
      ,/ALIGN_LEFT,UVALUE='LIMIT_SLOPE_LOW',  value='')
      ;;; Offset


;;;;;;;;;;;; Container for checkboxes and checkboxes for upper limited ;;;;;;;;;;;;;;;;

  WID_BASE_12 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_12'  $
      ,XOFFSET=265 ,YOFFSET=40 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_10 = Widget_Button(WID_BASE_12, UNAME='WID_BUTTON_10' $
  ,/ALIGN_LEFT,UVALUE='LIMIT_HEIGHT_UP',  value='')
      ;;; Height

  WID_BASE_13 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_13'  $
      ,XOFFSET=265 ,YOFFSET=70 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_11 = Widget_Button(WID_BASE_13, UNAME='WID_BUTTON_11'  $
       ,/ALIGN_LEFT,UVALUE='LIMIT_XMAX_UP',  value='')
      ;;; Xmax

  WID_BASE_14 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_14'  $
      ,XOFFSET=265 ,YOFFSET=100 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_12 = Widget_Button(WID_BASE_14, UNAME='WID_BUTTON_12'  $
 	 ,/ALIGN_LEFT,UVALUE='LIMIT_WIDTH_UP',  value='')
      ;;; Width

  WID_BASE_15 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_15'  $
      ,XOFFSET=265 ,YOFFSET=130 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_13 = Widget_Button(WID_BASE_15, UNAME='WID_BUTTON_13'  $
    	,/ALIGN_LEFT,UVALUE='LIMIT_OFFSET_UP',  VALUE='')
      ;;; Offset

  WID_BASE_16 = Widget_Base(WID_BASE_1, UNAME='WID_BASE_16'  $
      ,XOFFSET=265 ,YOFFSET=160 ,XSIZE=20 ,YSIZE=27  $
      ,/ALIGN_TOP ,/BASE_ALIGN_CENTER ,TITLE='IDL' ,SPACE=2 ,ROW=1  $
      ,/GRID_LAYOUT ,/NONEXCLUSIVE)


  WID_BUTTON_14 = Widget_Button(WID_BASE_16, UNAME='WID_BUTTON_14'  $
     ,/ALIGN_LEFT,UVALUE='LIMIT_SLOPE_UP',  value='')
      ;;; Offset


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


  BUT_BASE=Widget_Base(tlb,Row=1,yoffset=260,xoffset=120,/ALIGN_CENTER)
  CANCEL=Widget_Button(BUT_BASE,Value='Cancel', UVALUE='Cancel')
  ACCEPT=Widget_Button(BUT_BASE,Value='Accept', UVALUE='Accept')

   ; Create a pointer. This will point to the location where the
   ; information collected from the user will be stored.


ptr = Ptr_New({fparm:fparm, Cancel:1})

   ; Create info structure to hold information needed in event handler. This info structure containes also the parinfo
   ; that is used for mpfit.


buttons=[WID_BUTTON_0,WID_BUTTON_1,WID_BUTTON_2,WID_BUTTON_3,WID_BUTTON_4,$
		 WID_BUTTON_5,WID_BUTTON_6,WID_BUTTON_7,WID_BUTTON_8,WID_BUTTON_9,$
		 WID_BUTTON_10,WID_BUTTON_11,WID_BUTTON_12,WID_BUTTON_13,WID_BUTTON_14,$
		 CANCEL]
text = 	[WID_TEXT_0,WID_TEXT_1,WID_TEXT_2,WID_TEXT_3,WID_TEXT_4,$
		 WID_TEXT_5,WID_TEXT_6,WID_TEXT_7,WID_TEXT_8,WID_TEXT_9,$
		 WID_TEXT_10,WID_TEXT_11,WID_TEXT_12,WID_TEXT_13,WID_TEXT_14]

for i=0,4 do begin
widget_control,buttons(i),set_button=fparm(i).fixed
endfor

for i=5,9 do begin
widget_control,buttons(i),set_button=fparm(i-5).limited(0)
endfor

for i=10,14 do begin
widget_control,buttons(i),set_button=fparm(i-10).limited(1)
endfor


info = {buttons:buttons, $ 	; Identifier of widget holding buttons (checkboxes).
		text:text,$			; Identifier of widget holding textfields.
		fparm:fparm,$		; The actual parinfo
		ptr:ptr} 			; The pointer


            ; Store the info structure in the top-level base

Widget_Control, tlb, Set_UValue=info, /No_Copy
Widget_Control, /REALIZE, tlb

   ; Register the program, set up event loop. Make this program a
   ; blocking widget. This will allow the program to also be called
   ; from IDL command line without a PARENT parameter. The program
   ; blocks here until the entire program is destroyed.

XManager, 'SetFitParm', tlb, Event_Handler='SetFitParm_Events'

  ; OK,

newInfo = *ptr
Ptr_Free, ptr

  ; All kinds of things can go wrong now. Let's CATCH them all.

Catch, error
IF error NE 0 THEN BEGIN
   Catch, /Cancel

		      ; If an error occurs, set the CANCEL flag and return -1.

   ok = Dialog_Message(!Err_String)
   cancel = 1
   RETURN, -1
ENDIF


   ; If the error flag is set, let's disappear!

cancel = newInfo.cancel
IF cancel THEN RETURN, FParm

   ; OK, try to read the data file. Watch out!


RETURN, newInfo.FParm
END ;*******************************************************************
