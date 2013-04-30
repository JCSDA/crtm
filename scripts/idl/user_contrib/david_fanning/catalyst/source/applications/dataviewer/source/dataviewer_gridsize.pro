;+
; NAME:
;  DATAVIEWER_GRIDSIZE
;
; PURPOSE:
;
;  This function allows the user to change the grid dimensions in DAAC DataViewer.
;
; AUTHOR:
;
;       David W. Fanning, Ph.D
;       National Snow and Ice Data Center (NSIDC)
;       NSIDC/CIRES University of Colorado
;       Boulder, CO 80309
;       E-Mail: fanning@nsidc.org
;
; CATEGORY:
;
;  Utility, Widgets
;
; CALLING SEQUENCE:
;
;  theGrid = DataViewer_GridSize()
;
; INPUTS:
;
;  None.
;
; KEYWORD PARAMETERS:
;
;  CANCEL: An output parameter. If the user kills the widget or clicks the Cancel
;       button this keyword is set to 1. It is set to 0 otherwise. It
;       allows you to determine if the user canceled the dialog without
;       having to check the validity of the answer.
;
;       theText = GridSize(Title='Provide Phone Number...', Label='Number:', Cancel=cancelled)
;       IF cancelled THEN Return
;
;  GRID: A two-element array [xsize, ysize] with the initial parameters for the widget.
;
;  GROUP_LEADER: The widget ID of the group leader of this pop-up
;       dialog. This should be provided if you are calling
;       the program from within a widget program:
;
;          thetext = GridSize(Group_Leader=event.top)
;
;       If a group leader is not provided, an unmapped top-level base widget
;       will be created as a group leader.
;
; OUTPUTS:
;
;  theGrid: A two-element array of the grid size [xsize, ysize].
;
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, June 26, 2008.
;-
;******************************************************************************************;
;  Copyright (c) 2008, Regents of the University of Colorado. All rights reserved.         ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      1. Redistributions of source code must retain the above copyright                   ;
;         notice, this list of conditions and the following disclaimer.                    ;
;      2. Redistributions in binary form must reproduce the above copyright                ;
;         notice, this list of conditions and the following disclaimer in the              ;
;         documentation and/or other materials provided with the distribution.             ;
;      3. Neither the name of the Regents of the University of Colorado nor the names      ;
;         of its contributors may be used to endorse or promote products derived from      ;
;          this software without specific prior written permission.                        ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY THE REGENTS OF THE UNIVERISTY OF COLORADO ''AS IS'' AND    ;
;  ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED           ;
;  WARRANTIES OF MERCHANTABILITY  AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.     ;
;  IN NO EVENT SHALL THE REGENTS OF THE UNIVERSITY OF COLORADO BE LIABLE FOR ANY DIRECT,   ;
;  INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT      ;
;  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR      ;         
;  PROFITS OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,        ;
;  WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)      ;
;  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE              ;
;  POSSIBILITY OF SUCH DAMAGE.                                                             ;
;******************************************************************************************;



PRO DataViewer_GridSize_Event, event

   ; This event handler responds to all events. Widget
   ; is always destoyed. The text is recorded if ACCEPT
   ; button is selected or user hits CR in text widget.
    Widget_Control, event.top, Get_UValue=info
    CASE event.ID OF
    
        info.cancelID: Widget_Control, event.top, /Destroy
        
        ELSE: BEGIN
        
            ; Get the grid sizes and store them in the pointer location.
            info.xsizeObj -> GetProperty, Value=xsize
            info.ysizeObj -> GetProperty, Value=ysize
            (*info.ptr).grid = [xsize, ysize]
            (*info.ptr).cancel = 0
            Widget_Control, event.top, /Destroy
            END
            
     ENDCASE
     
END ;-----------------------------------------------------



FUNCTION DataViewer_GridSize, GRID=grid, Cancel=cancel, Group_Leader=groupleader

    ; Return to caller if there is an error. Set the cancel
    ; flag and destroy the group leader if it was created.
    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /Cancel
        ok = Dialog_Message(!Error_State.Msg)
        IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy
        cancel = 1
        RETURN, ""
    ENDIF
    
    ; Check keywords.
    IF N_Elements(grid) EQ 0 THEN grid = [3,2] ELSE grid = Round(grid)

    ; Provide a group leader if not supplied with one. This
    ; is required for modal operation of widgets. Set a flag
    ; for destroying the group leader widget before returning.
    IF N_Elements(groupleader) EQ 0 THEN BEGIN
        groupleader = Widget_Base(Map=0)
        Widget_Control, groupleader, /Realize
        destroy_groupleader = 1
    ENDIF ELSE destroy_groupleader = 0

    ; Create modal base widget.
    tlb = Widget_Base(Title='Select Size of Grid', Column=1, /Modal, $
        /Base_Align_Center, Group_Leader=groupleader)

    ; Create the rest of the widgets.
    xsizeObj = FSC_INPUTFIELD(tlb, Title='Colums: ', Value=grid[0], /IntegerValue)
    ysizeObj = FSC_INPUTFIELD(tlb, Title='Rows:   ', Value=grid[1], /IntegerValue)
    buttonBase = Widget_Base(tlb, Row=1)
    cancelID = Widget_Button(buttonBase, Value='Cancel')
    acceptID = Widget_Button(buttonBase, Value='Accept')
    
    ; Set up tabbing between widgets.
    xsizeObj -> SetTabNext, ysizeObj -> GetTextID()
    ysizeObj -> SetTabNext, xsizeObj -> GetTextID()
    Widget_Control, xsizeObj -> GetTextID(), /INPUT_FOCUS, /KBRD_FOCUS_EVENTS
    Widget_Control, ysizeObj -> GetTextID(), /KBRD_FOCUS_EVENTS

    ; Center the widgets on display.
    cgCenterTLB, tlb
    Widget_Control, tlb, /Realize
    
    ; Set up the text widget selection.
    Widget_Control, xsizeObj -> GetTextID(), Get_Value=theText
    Widget_Control, xsizeObj -> GetTextID(), Set_Text_Select=[0,StrLen(theText)]
    Widget_Control, xsizeObj -> GetTextID(), /INPUT_FOCUS

    ; Create a pointer for the text the user will type into the program.
    ; The cancel field is set to 1 to indicate that the user canceled
    ; the operation. Only if a successful conclusion is reached (i.e.,
    ; a Carriage Return or Accept button selection) is the cancel field
    ; set to 0.
    ptr = Ptr_New({grid:grid, cancel:1})

    ; Store the program information:
    info = {ptr:ptr, xsizeObj:xsizeObj, ysizeObj:ysizeObj, cancelID:cancelID}
    Widget_Control, tlb, Set_UValue=info, /No_Copy

    ; Blocking or modal widget, depending upon group leader.
    XManager, 'dataviewer_gridsize', tlb

    ; Return from block. Return the text to the caller of the program,
    ; taking care to clean up pointer and group leader, if needed.
    ; Set the cancel keyword.
    outGrid = (*ptr).grid
    cancel = (*ptr).cancel
    Ptr_Free, ptr
    IF destroy_groupleader THEN Widget_Control, groupleader, /Destroy

    RETURN, outGrid
END ;-----------------------------------------------------


