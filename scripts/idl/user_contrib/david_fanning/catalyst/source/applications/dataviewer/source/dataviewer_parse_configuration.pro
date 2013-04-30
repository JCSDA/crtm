;+
; NAME:
;  DATAVIEWER_PARSE_CONFIGURATION
;
; PURPOSE:
;
;  This program parses the configuration file for the DATAVIEWER program.
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
;  DataViewer_Parse_Configuration, config_file
;
; INPUTS:
;
;  config_file:    The name of the configuration file to parse.
;
; KEYWORD PARAMETERS:
;
;  CONFIGURATION:   An output keyword containing an IDL structure variable with
;                   the configuration of the parsed config file.
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
PRO DataViewer_Parse_Configuration, config_file, CONFIGURATION=currentStruct

   ; Error handling.
   Catch, theError
   IF theError NE 0 THEN BEGIN
        Catch, /CANCEL
        ok = Error_Message()
        RETURN
   ENDIF

   ; Look for the default dataviewer configuration file.
   IF N_Elements(config_file) EQ 0 THEN BEGIN
      IF LMGR(/RUNTIME) OR LMGR(/VM) THEN oneup = 0 ELSE oneup=1
      config_file = Filepath(ROOT_DIR=ProgramRootDir(ONEUP=oneup), SUBDIRECTORY='config', 'dataviewer_default.txt')
   ENDIF
      
   ; Read the entire file into a text array.
   rows = File_Lines(config_file)
   text = StrArr(rows)
   OpenR, lun, config_file, /GET_LUN
   ReadF, lun, text
   Free_Lun, lun
   
   ; For each line, find out if it is a comment line, or blank line. If
   ; neither of these, then parse the VARIABLE:VALUE pair and store as
   ; program variables.
   FOR j=0,rows-1 DO BEGIN
   
        thisLine = text[j]
        
        ; Blank line? Ignore.
        IF thisLine EQ "" THEN CONTINUE
        
        ; Comment line? Ignore.
        IF StrMid(thisLine, 0, 1) EQ ';' THEN CONTINUE
        
        ; First character a black? Ignore.
        IF StrMid(thisLine, 0, 1) EQ ' ' THEN CONTINUE

        ; Separate the line into parts.
        parts = StrSplit(thisLine, /EXTRACT)
        parts[0] = StrMid(parts[0], 0, StrLen(parts[0])-1)
        
        ; Process the value according to first character.
        CASE StrMid(parts[1],0,1) OF
        
            "'": BEGIN ; String
                 start = StrPos(thisLine, ',') + 2
                 theString = StrMid(thisLine, start)
                 theString = StrMid(theString,1,StrLen(theString)-2)
                 CatSetDefault, parts[0], theString
                 IF N_Elements(currentStruct) EQ 0 THEN BEGIN
                    currentStruct = Create_Struct(parts[0], theString)
                 ENDIF ELSE bEGIN
                     currentStruct = Create_Struct(currentStruct, parts[0], theString)
                 ENDELSE
                 END
                 
            '"': BEGIN ; String.
                 start = StrPos(thisLine, ',') + 2
                 theString = StrMid(thisLine, start)
                 theString = StrMid(theString,1,StrLen(theString)-2)
                 CatSetDefault, parts[0], theString
                 IF N_Elements(currentStruct) EQ 0 THEN BEGIN
                    currentStruct = Create_Struct(parts[0], theString)
                 ENDIF ELSE bEGIN
                     currentStruct = Create_Struct(currentStruct, parts[0], theString)
                 ENDELSE
                 END
                 
            '[': BEGIN ; Two-element array.
                 nums = StrSplit(parts[1], ',', /EXTRACT)
                 num1 = StrMid(nums[0], 1)
                 dot = StrPos(num1, '.')
                 IF dot EQ -1 THEN num1 = Long(num1) ELSE num1 = FLOAT(num1)
                 num2 = StrMid(nums[1], 0, StrLen(nums[1])-1)
                 IF dot EQ -1 THEN num2 = Long(num2) ELSE num2 = FLOAT(num2)
                 CatSetDefault, parts[0], [num1,num2]
                 IF N_Elements(currentStruct) EQ 0 THEN BEGIN
                    currentStruct = Create_Struct(parts[0], [num1,num2])
                 ENDIF ELSE bEGIN
                     currentStruct = Create_Struct(currentStruct, parts[0], [num1,num2])
                 ENDELSE
                 END
  
              ELSE: BEGIN ; Number value of some sort.
                 dot = StrPos(parts[1], '.')
                 IF dot EQ -1 THEN theValue = Long(parts[1]) ELSE theValue = Float(parts[1])
                 CatSetDefault, parts[0], theValue
                 IF N_Elements(currentStruct) EQ 0 THEN BEGIN
                    currentStruct = Create_Struct(parts[0], theValue)
                 ENDIF ELSE bEGIN
                     currentStruct = Create_Struct(currentStruct, parts[0], theValue)
                 ENDELSE
                 END
                 
        ENDCASE
   
   ENDFOR
   
END