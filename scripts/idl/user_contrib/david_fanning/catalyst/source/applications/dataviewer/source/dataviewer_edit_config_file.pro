;*****************************************************************************************************
;+
; NAME:
;       DataViewer_Edit_Config_File
;
; PURPOSE:
;
;       The purpose of this routine is to allow the user to edit and make
;       changes to the DataViewer configuration file from within a graphical
;       user interface. The program is designed as a utility program that will
;       be called from within DataViewer.
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
;       Data Visualization
;
; SYNTAX:
;
;       IDL> DataViewer_Edit_Config_File, configFile, group_leader, dataViewerObj
;       
; ARGUMENTS:
; 
;       configFile:   The name of a configuration file you wish to edit. If not
;                     provided, the configuration file name is obtained from the
;                     program default variable DATAVIEWER_CONFIG_FILE.
;                     
;       group_leader: A widget identifier that can serve as the group leader for
;                     this program. When the group leader dies, this program will
;                     also die. A required parameter.
;                     
;       dataViewerObj: The DataViewer object reference. A required parameter.
;                
; MODIFICATION_HISTORY:
;
;       Written by: David W. Fanning 28 July 2010. DWF.
;-
;******************************************************************************************;
;  Copyright (c) 2010, Regents of the University of Colorado. All rights reserved.         ;
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
PRO DataViewer_Edit_Config_File_Restore, info, SUCCESS=success

    ; Restore the widgets in the graphical user interface to their
    ; default values. The keyword SUCCESS is used to judge whether
    ; this was successfully accomplished.
    
    ; info --    The information structure of the program. Required parameter.
    ; success -- An output variable set to 1 if program module is successfully
    ;            executed and to 0 otherwise.

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        success = 0
        RETURN
    ENDIF

    ; Assume success.
    success = 1
    
    ; Set the widgets in the interface to their default values.
    Widget_Control, info.dirLabel, SET_VALUE='DEFAULT' + String(Replicate(32B, 10))
    info.backgroundColorObj -> Set_Color, 'WHITE'
    info.missingColorObj -> Set_Color, 'WHITE'
    info.oob_low_colorObj -> Set_Color, 'CRIMSON'
    info.oob_high_colorObj -> Set_Color, 'GOLD'
    info.landmask_colorObj -> Set_Color, 'DARK GRAY'
    info.annotate_colorObj -> Set_Color, 'BLACK'
    info.outline_colorObj -> Set_Color, 'INDIAN RED'
    info.grid_colorObj -> Set_Color, 'CHARCOAL'
    info.vector_colorObj -> Set_Color, 'PBG7'
    info.colorTableObj -> SetProperty, CT_INDEX=4, BREWER=1
    info.xsizeObj -> Set_Value, 674
    info.ysizeObj -> Set_Value, 746
    info.colsObj -> Set_Value, 3
    info.rowsObj -> Set_Value, 2
    info.animateSizeObj -> Set_Value, 550
    info.annotateSizeObj -> Set_Value, 550
    info.namesOffObj -> SetSelection, 'True'
    info.cbOffObj -> SetSelection, 'True'
    info.outlineOnObj -> SetSelection, 'True'
    info.gridOnObj -> SetSelection, 'True'
    info.cbFormatObj -> SetSelection, '(F0.1)'
    
END


FUNCTION DataViewer_Edit_Config_File_Struct, info, SUCCESS=success

    ; Create a structure of DATAVIEWER default values and their corresponding
    ; values from the graphical user interface.

    ; info --    The information structure of the program. Required parameter.
    ; success -- An output variable set to 1 if program module is successfully
    ;            executed and to 0 otherwise.

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        success = 0
        RETURN, info.startingConfig
    ENDIF

    ; Assume success.
    success = 1

    ; Create the structure filled with field definitions.
    configStruct = Create_Struct( $
        'DATAVIEWER_CONFIG_FILE', '', $
        'DATAVIEWER_DATA_DIRECTORY', '', $
        'DATAVIEWER_BACKGROUND_COLOR', '', $
        'DATAVIEWER_MISSING_COLOR', '', $
        'DATAVIEWER_OUTOFBOUNDS_LOW_COLOR', '', $
        'DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR', '', $
        'DATAVIEWER_LANDMASK_COLOR', '', $
        'DATAVIEWER_ANNOTATE_COLOR', '', $
        'DATAVIEWER_OUTLINE_COLOR', '', $
        'DATAVIEWER_GRID_COLOR', '', $
        'DATAVIEWER_VECTOR_COLOR', '', $
        'DATAVIEWER_DEFAULT_CT', 0L, $
        'DATAVIEWER_DEFAULT_CT_TYPE', '', $
        'DATAVIEWER_GRIDWINDOW_XSIZE', 0L, $
        'DATAVIEWER_GRIDWINDOW_YSIZE', 0L, $
        'DATAVIEWER_GRIDWINDOW_GRID', IntArr(2), $
        'DATAVIEWER_ANIMATE_MAXSIZE', 0L, $
        'DATAVIEWER_ANNOTATE_MAXSIZE', 0L, $
        'DATAVIEWER_IMAGE_NAMES_OFF', 0L, $
        'DATAVIEWER_COLORBARS_OFF', 0L, $
        'DATAVIEWER_MAP_OUTLINE_ON', 0L, $
        'DATAVIEWER_MAP_GRID_ON', 0L, $
        'DATAVIEWER_CB_FORMAT', '')
        
    ; Fill in the actual values of the structure from information obtained from
    ; the interface.
    Widget_Control, info.dirLabel, GET_VALUE=dataDir
    configStruct.DATAVIEWER_DATA_DIRECTORY = StrTrim(dataDir,2)
    configStruct.DATAVIEWER_BACKGROUND_COLOR = info.backgroundColorObj -> Get_Color()
    configStruct.DATAVIEWER_MISSING_COLOR = info.missingColorObj -> Get_Color()
    configStruct.DATAVIEWER_OUTOFBOUNDS_LOW_COLOR = info.oob_low_colorObj -> Get_Color()
    configStruct.DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR = info.oob_high_colorObj -> Get_Color()
    configStruct.DATAVIEWER_LANDMASK_COLOR = info.landmask_colorObj -> Get_Color()
    configStruct.DATAVIEWER_ANNOTATE_COLOR = info.annotate_colorObj -> Get_Color()
    configStruct.DATAVIEWER_OUTLINE_COLOR = info.outline_colorObj -> Get_Color()
    configStruct.DATAVIEWER_GRID_COLOR = info.grid_colorObj -> Get_Color()
    configStruct.DATAVIEWER_VECTOR_COLOR = info.vector_colorObj -> Get_Color()
    configStruct.DATAVIEWER_DEFAULT_CT = info.index
    IF info.brewer THEN ct_type = 'BREWER' ELSE ct_type = 'IDL'
    configStruct.DATAVIEWER_DEFAULT_CT_TYPE = ct_type
    configStruct.DATAVIEWER_GRIDWINDOW_XSIZE = info.xsizeObj -> Get_Value()
    configStruct.DATAVIEWER_GRIDWINDOW_YSIZE = info.ysizeObj -> Get_Value()
    cols = info.colsObj -> Get_Value()
    rows = info.rowsObj -> Get_Value()
    configStruct.DATAVIEWER_GRIDWINDOW_GRID = [cols,rows]
    configStruct.DATAVIEWER_ANIMATE_MAXSIZE = info.animateSizeObj -> Get_Value()
    configStruct.DATAVIEWER_ANNOTATE_MAXSIZE = info.annotateSizeObj -> Get_Value()
    configStruct.DATAVIEWER_IMAGE_NAMES_OFF = (StrUpCase(info.namesOffObj -> GetSelection())) EQ 'TRUE' ? 0 : 1
    configStruct.DATAVIEWER_COLORBARS_OFF = (StrUpCase(info.cbOffObj -> GetSelection())) EQ 'TRUE' ? 0 : 1
    configStruct.DATAVIEWER_MAP_OUTLINE_ON = (StrUpCase(info.outlineOnObj -> GetSelection())) EQ 'TRUE' ? 1 : 0
    configStruct.DATAVIEWER_MAP_GRID_ON = (StrUpCase(info.gridOnObj -> GetSelection())) EQ 'TRUE' ? 1 : 0
    configStruct.DATAVIEWER_CB_FORMAT = info.cbFormatObj -> GetSelection()
        
    RETURN, configStruct
    
END

PRO DataViewer_Edit_Config_File_Events, event

    ; The main widget event handler for the program. Events are distinguished
    ; by their USER_VALUE.

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN
    ENDIF
    
    ; Get the info structure from storage.
    Widget_Control, event.top, GET_UVALUE=info
     
    ; Get the User Value and branch accordingly.
    Widget_Control, event.id, GET_UVALUE=uvalue
    CASE StrUpCase(uvalue) OF
    
        'APPLY': BEGIN ; Apply the configuration in the GUI to the DataViewer program.
        
           ; Create a structure of DataViewer defaults and values collected from the GUI.
           configStruct = DataViewer_Edit_Config_File_Struct(info, SUCCESS=success)
           IF success EQ 0 THEN RETURN
           
           ; Add the configuration file name to the structure.
           configStruct.DATAVIEWER_CONFIG_FILE = info.configFile
           
           ; Set the DataViewer defaults to the values you collected
           tagNames = Tag_Names(configStruct)
           FOR j=0, N_Tags(configStruct)-1 DO BEGIN
               CatSetDefault, tagNames[j], configStruct.(j)
           ENDFOR
           
           ; Update the DataViewer GUI with this new information.
           info.dataViewerObj -> Update_GUI_From_Config, info.configFile

           END
            
        'DISMISS': BEGIN ; Forget about it. Do something else.
            Widget_Control, event.top, /DESTROY
            END
            
        'RESTORE': BEGIN ; Restore default values in the graphical user interface.
            DataViewer_Edit_Config_File_Restore, info, SUCCESS=success
            IF success EQ 0 THEN BEGIN
               tagNames = Tag_Names(info.startingConfig)
               FOR j=0, N_Tags(info.startingConfig)-1 DO BEGIN
                   CatSetDefault, tagNames[j], configStruct.(j)
               ENDFOR
               info.dataViewerObj -> Update_GUI_From_Config, info.configFile
            ENDIF
            END

        'SELECT_DATA_DIRECTORY': BEGIN ; Events from the Select Data Directory button.
            Widget_Control, info.dirLabel, GET_VALUE=dataDir
            dataDir = StrTrim(dataDir,2)
            newDir = Dialog_Pickfile(PATH=dataDir,  /DIRECTORY, TITLE='Select Data Directory')
            IF newDir EQ "" THEN RETURN
            Widget_Control, info.dirLabel, SET_VALUE=newDir + String(Replicate(32B, 10))
            END
            
        'SET_COLORTABLE': BEGIN ; Events from FSC_ColorSelect widget for changing color tables.
        
            ; You need to keep track of the color table index and whether this
            ; is a Brewer color table or not, because FSC_ColorSelect can't really
            ; do this for you.
            info.brewer = event.brewer
            info.index = event.index
            END
            
         'SAVE': BEGIN ; Save the current GUI configuation to a file.
         
           ; Is this a relative file name or a complete file name?
           IF StrUpCase(File_BaseName(info.configFile)) EQ StrUpCase(info.configFile) THEN BEGIN
               IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
               configfile = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), $
                  SUBDIR='config', info.configFile)
           ENDIF ELSE configFile = info.configFile
           root_file = cgRootName(configFile, DIRECTORY=configDir, EXTENSION=ext)
           
           ; What would you like to name the file?
           configFile = Dialog_Pickfile(PATH=configDir, FILE=root_file + '.' + ext, /WRITE)
           IF configFile EQ "" THEN RETURN
           
           ; Create a structure of DataViewer defaults and values collected from the GUI.
           configStruct = DataViewer_Edit_Config_File_Struct(info, SUCCESS=success)
           IF success EQ 0 THEN RETURN
           
           ; Add the configuration file name to the structure.
           configStruct.DATAVIEWER_CONFIG_FILE = configFile
           
           
           ; Set the DataViewer defaults to the values you collected
           tagNames = Tag_Names(configStruct)
           FOR j=0, N_Tags(configStruct)-1 DO BEGIN
               CatSetDefault, tagNames[j], configStruct.(j)
           ENDFOR
           
           ; Write the configuraton file.
           info.dataViewerObj -> Write_Config_File, configFile, /NOCURRENTWINDOW
           
           ; Now set everything back to the way it was when you opened the GUI.
           tagNames = Tag_Names(info.startingConfig)
           FOR j=0, N_Tags(info.startingConfig)-1 DO BEGIN
               CatSetDefault, tagNames[j], configStruct.(j)
           ENDFOR
           
           END
           
           ELSE:
            
    ENDCASE
    
    ; Made changes to the info structure, so put if back, if you can.
    IF Widget_Info(event.top, /VALID_ID) THEN Widget_Control, event.top, SET_UVALUE=info
END ;-----------------------------------------------------------------------------------



FUNCTION DataViewer_Edit_Config_File, configurationFile, group_leader, dataViewerObj

    Catch, theError
    IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        void = Error_Message()
        RETURN, 0
    ENDIF
    
    ; Check arguments.
    IF N_Elements(configurationFile) EQ 0 THEN BEGIN
        configFile = CatGetDefault('DATAVIEWER_CONFIG_FILE', SUCCESS = success)
        IF ~success THEN Message, 'Must supply the name of a DataViewer configuration file.'
    ENDIF ELSE configFile = configurationFile
    IF N_Elements(group_leader) EQ 0 THEN $
        Message, 'A group leader ID must be supplied as an argument.'
    IF ~Obj_Valid(dataViewerObj) THEN $
        Message, 'A valid DataViewer object  must be supplied as an argument.'
        
    ; Parse the configuration file and obtain a record (structure) of the
    ; DataViewer defaults and their values.
    DataViewer_Parse_Configuration, configFile, CONFIGURATION=startingConfig
    
    ; Create the widgets for the GUI.
    tlb = Widget_Base(TITLE='Edit Configuration File', COLUMN=1, BASE_ALIGN_CENTER=1)
    itembase = Widget_Base(tlb, COLUMN=2, FRAME=1)
    buttonbase = Widget_Base(tlb, ROW=1)
    
    labelsize = 175
    backgroundColor = CatGetDefault('DATAVIEWER_BACKGROUND_COLOR')
    backgroundColorObj = FSC_ColorSelect(itembase, TITLE='Background Color', $
        COLOR=backgroundColor, LABELSIZE=labelsize, LABELALIGN=1)
    missingColor = CatGetDefault('DATAVIEWER_MISSING_COLOR')
    missingColorObj = FSC_ColorSelect(itembase, TITLE='Missing Color', COLOR=missingColor, $
        LABELSIZE=labelsize, LABELALIGN=1)
    oob_low_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_LOW_COLOR')
    oob_low_colorObj = FSC_ColorSelect(itembase, TITLE='Out-of-Bounds Low Color', $
        COLOR=oob_low_color, LABELSIZE=labelsize, LABELALIGN=1)
    oob_high_color = CatGetDefault('DATAVIEWER_OUTOFBOUNDS_HIGH_COLOR')
    oob_high_colorObj = FSC_ColorSelect(itembase, TITLE='Out-of-Bounds High Color', $
        COLOR=oob_high_color, LABELSIZE=labelsize, LABELALIGN=1)
    landmask_color = CatGetDefault('DATAVIEWER_LANDMASK_COLOR')
    landmask_colorObj = FSC_ColorSelect(itembase, TITLE='Landmask Color', COLOR=landmask_color, $
        LABELSIZE=labelsize, LABELALIGN=1)
    annotate_color = CatGetDefault('DATAVIEWER_ANNOTATE_COLOR')
    annotate_colorObj = FSC_ColorSelect(itembase, TITLE='Annotation Color', COLOR=annotate_color, $
        LABELSIZE=labelsize, LABELALIGN=1)
    outline_color = CatGetDefault('DATAVIEWER_OUTLINE_COLOR')
    outline_colorObj = FSC_ColorSelect(itembase, TITLE='Map Outline Color', COLOR=outline_color, $
        LABELSIZE=labelsize, LABELALIGN=1)
    grid_color = CatGetDefault('DATAVIEWER_GRID_COLOR')
    grid_colorObj = FSC_ColorSelect(itembase, TITLE='Map Grid Color', COLOR=grid_color, $
        LABELSIZE=labelsize, LABELALIGN=1)
    vector_color = CatGetDefault('DATAVIEWER_VECTOR_COLOR')
    vector_colorObj = FSC_ColorSelect(itembase, TITLE='Map Vector Color', COLOR=vector_color, $
        LABELSIZE=labelsize, LABELALIGN=1)
    xsize = CatGetDefault('DATAVIEWER_GRIDWINDOW_XSIZE')
    xsizeObj = FSC_Inputfield(itembase, TITLE='Window X Size', $
             VALUE=Long(xsize), LABELSIZE=labelsize, LABELALIGN=1)
    ysize = CatGetDefault('DATAVIEWER_GRIDWINDOW_YSIZE')
    ysizeObj = FSC_Inputfield(itembase, TITLE='Window Y Size', $
             VALUE=Long(ysize), LABELSIZE=labelsize, LABELALIGN=1)
    colortable = CatGetDefault('DATAVIEWER_DEFAULT_CT')
    brewer = CatGetDefault('DATAVIEWER_DEFAULT_CT_TYPE')
    IF StrUpCase(brewer) EQ 'BREWER' THEN brewer = 1 ELSE brewer = 0
    colorTableObj = FSC_ColorSelect(itembase, TITLE='Image Color Table', $
        BREWER=brewer, CT_INDEX=colortable, CT_NCOLORS=256, LABELSIZE=labelsize, LABELALIGN=1, $
        EVENT_PRO='DataViewer_Edit_Config_File_Events', UVALUE='SET_COLORTABLE')
    grid = CatGetDefault('DATAVIEWER_GRIDWINDOW_GRID')
    colsObj = FSC_Inputfield(itembase, TITLE='Columns in Grid Layout', $
             VALUE=Long(grid[0]), LABELSIZE=labelsize, LABELALIGN=1)
    rowsObj = FSC_Inputfield(itembase, TITLE='Rows in Grid Layout', $
             VALUE=Long(grid[1]), LABELSIZE=labelsize, LABELALIGN=1)
    animateSize = CatGetDefault('DATAVIEWER_ANIMATE_MAXSIZE')   
    animateSizeObj = FSC_Inputfield(itembase, TITLE='Size of Animation Window', $
             VALUE=Long(animateSize), LABELSIZE=labelsize, LABELALIGN=1)
    annotateSize = CatGetDefault('DATAVIEWER_ANNOTATE_MAXSIZE')   
    annotateSizeObj = FSC_Inputfield(itembase, TITLE='Size of Annotation Window', $
             VALUE=Long(annotateSize), LABELSIZE=labelsize, LABELALIGN=1)
             
    ; The droplist widgets.
    droplistValues = ['False', 'True']
    namesoff = CatGetDefault('DATAVIEWER_IMAGE_NAMES_OFF')
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, VALUE='Image Names On:', SCR_XSIZE=labelsize)
    namesOffObj = FSC_Droplist(labelbase, TITLE='',  VALUE=droplistValues, $
       INDEX=1 - namesoff)
 
    cboff = CatGetDefault('DATAVIEWER_COLORBARS_OFF')
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, VALUE='Colorbars On:', SCR_XSIZE=labelsize)
    cbOffObj = FSC_Droplist(labelbase, TITLE='',  VALUE=droplistValues, $
       INDEX=1 - cboff)
       
    outlineOn =   CatGetDefault('DATAVIEWER_MAP_OUTLINE_ON')
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, VALUE='Map Outlines On:', SCR_XSIZE=labelsize)
    outlineOnObj = FSC_Droplist(labelbase, TITLE='',  VALUE=droplistValues, $
       INDEX=outlineOn) 
       
    gridOn =   CatGetDefault('DATAVIEWER_MAP_GRID_ON')
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, VALUE='Map Grids On:', SCR_XSIZE=labelsize)
    gridOnObj = FSC_Droplist(labelbase, TITLE='',  VALUE=droplistValues, $
       INDEX=gridOn) 
            
    formats = ['(I0)', '(F0)', '(F0.1)', '(F0.2)', '(F0.3)']
    cbFormat = CatGetDefault('DATAVIEWER_CB_FORMAT')
    cbindex = Where(formats EQ cbFormat, count)
    IF count EQ 0 THEN BEGIN
        IF cbFormat NE "" THEN BEGIN
            formats = [formats, cbFormat]
            cbindex = N_Elements(formats)-1
        ENDIF ELSE cbindex = 2
    ENDIF
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, VALUE='Colorbar Format:', SCR_XSIZE=labelsize)
    cbformatObj = FSC_Droplist(labelbase, TITLE='',  VALUE=formats, $
       INDEX=cbindex) 

    dataDir = CatGetDefault('DATAVIEWER_DATA_DIRECTORY')
    labelbase = Widget_Base(itembase, ROW=1)
    label = Widget_Label(labelbase, Value = 'Data Directory', SCR_XSIZE=labelsize)
    dirLabel = Widget_Label(labelbase, VALUE=dataDir + String(Replicate(32B, 10)), $
        /DYNAMIC_RESIZE, FRAME=1)
    dataDir = Widget_Button(labelbase, VALUE='Select', UVALUE='SELECT_DATA_DIRECTORY')

    button = Widget_Button(buttonBase, Value='Dismiss', UVALUE='DISMISS')
    button = Widget_Button(buttonBase, Value='Restore Defaults', UVALUE='RESTORE')
    button = Widget_Button(buttonBase, Value='Apply Configuration', UVALUE='APPLY')
    button = Widget_Button(buttonBase, Value='Save Configuration', UVALUE='SAVE')
    
    ; Realize the program.
    cgCenterTLB, tlb
    Widget_Control, tlb, /REALIZE
    
    ; Info structure to hold all needed program information. This structure
    ; is passed around in the program to modules that need this information.
    info = { index:colortable, $
             brewer:brewer, $
             startingConfig:startingConfig, $
             dirLabel: dirLabel, $
             configFile:configurationFile, $
             dataViewerObj:dataViewerObj, $
             backgroundColorObj:backgroundColorObj, $
             missingColorObj:missingColorObj, $
             oob_low_colorObj:oob_low_colorObj, $
             oob_high_colorObj:oob_high_colorObj, $
             landmask_colorObj:landmask_colorObj, $
             annotate_colorObj: annotate_colorObj, $
             outline_colorObj: outline_colorObj, $
             grid_colorObj: grid_colorObj, $
             vector_colorObj:vector_colorObj, $
             colorTableObj: colorTableObj, $
             xsizeObj:xsizeObj, $
             ysizeObj:ysizeObj, $
             colsObj:colsObj, $
             rowsObj:rowsObj, $
             animateSizeObj:animateSizeObj, $
             annotateSizeObj:annotateSizeObj,$
             namesOffObj:namesOffObj, $
             cbOffObj:cbOffObj, $
             outlineOnObj:outlineOnObj, $
             gridOnObj:gridOnObj, $
             cbformatObj:cbformatObj}
             
    Widget_Control, tlb, SET_UVALUE=info
    XManager, 'dataviewer_edit_config_file', tlb, GROUP_LEADER=group_leader, $
        /NO_BLOCK, EVENT_HANDLER='DataViewer_Edit_Config_File_Events'
        
    RETURN, 1
   
END