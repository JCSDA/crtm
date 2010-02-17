;+
; NAME:
;  GREEK
;
; PURPOSE:
;
;   This function provides a device-independent way to ask for a Greek letter as
;   a string that can be included, for example, in a plot title. It uses the Greek
;   simplex font (!4) when used with Hershey fonts, and the Symbol font (!9) when
;   used with PostScript or True-Type fonts.
;   
; AUTHOR:
;
;   FANNING SOFTWARE CONSULTING
;   David Fanning, Ph.D.
;   1645 Sheely Drive
;   Fort Collins, CO 80526 USA
;   Phone: 970-221-0438
;   E-mail: davidf@dfanning.com
;   Coyote's Guide to IDL Programming: http://www.dfanning.com/
;
; CATEGORY:
;
;   Graphics, Utilities
;
; CALLING SEQUENCE:
;
;   greekString = Greek(greekLetter)
;
; RETURN VALUE:
;
;   greekString    A string that represents the Greek letter.
;
; ARGUMENTS:
;
;  greekLetter:    The name of the Greek letter desired. A string. Default: 'alpha'.
;                  Valid string names are the 24 characters of the Greek alphabet.
;                     alpha        nu
;                     beta         xi
;                     gamma        omicron
;                     delta        pi
;                     epsilon      rho
;                     zeta         sigma
;                     eta          tau
;                     theta        upsilon
;                     iota         phi
;                     kappa        chi
;                     lambda       psi
;                     mu           omega
;                    
;                   Note that if the first letter of the name is capitalized, this is
;                   the equivalent of setting the CAPITAL keyword. 
;
; KEYWORDRS:
;
;  CAPTIAL:        If this keyword is set, the captial Greek letter is returned rather 
;                  than the lowercase Greek letter. An alternative way of capitalizing
;                  the letter is to make the first letter of the name an uppercase letter.
;                  
;  EXAMPLE:        If this keyword is set, the names of the Greek characters and their
;                  symbols are written out in the current graphics window.
;
; EXAMPLE:
;
;  Lowercase PSI:
;  
;     IDL> Plot, findgen(11), XTitle='This title contains ' + $
;           Greek('psi') + ' as a Greek letter' 
;
;  Uppercase PSI:
;  
;     IDL> Plot, findgen(11), XTitle='This title contains ' + $
;           Greek('Psi') + ' as a Greek letter' 
; NOTES:
; 
;  See the following article for additional information: 
;  
;       /http://www.dfanning.com/ps_tips/greeksym.pro
;       
; RESTRICTIONS:
; 
;  For this program to work correctly on your graphics display, you should be using
;  Hershey fonts (!P.Font = -1 or the FONT keyword set to -1). It will work correctly
;  in PostScript with either harwarde fonts (!P.Font=0) or True-Type fonts (!P.Font=1).
;  
; MODIFICATION HISTORY:
;
;  Written by: David W. Fanning, 9 January 2010.
;  An alternative way to get an uppercase letter is to make the first letter of
;     the Greek name uppercase. (Maarten Sneep's suggestion!) 11 Jan 2010. DWF
;-
;******************************************************************************************;
;  Copyright (c) 2010, by Fanning Software Consulting, Inc.                                ;
;  All rights reserved.                                                                    ;
;                                                                                          ;
;  Redistribution and use in source and binary forms, with or without                      ;
;  modification, are permitted provided that the following conditions are met:             ;
;                                                                                          ;
;      * Redistributions of source code must retain the above copyright                    ;
;        notice, this list of conditions and the following disclaimer.                     ;
;      * Redistributions in binary form must reproduce the above copyright                 ;
;        notice, this list of conditions and the following disclaimer in the               ;
;        documentation and/or other materials provided with the distribution.              ;
;      * Neither the name of Fanning Software Consulting, Inc. nor the names of its        ;
;        contributors may be used to endorse or promote products derived from this         ;
;        software without specific prior written permission.                               ;
;                                                                                          ;
;  THIS SOFTWARE IS PROVIDED BY FANNING SOFTWARE CONSULTING, INC. ''AS IS'' AND ANY        ;
;  EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES    ;
;  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT     ;
;  SHALL FANNING SOFTWARE CONSULTING, INC. BE LIABLE FOR ANY DIRECT, INDIRECT,             ;
;  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED    ;
;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;         ;
;  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND             ;
;  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT              ;
;  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS           ;
;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                            ;
;******************************************************************************************;
PRO Greek_Example

    Compile_Opt hidden
    
    ; The 24 Greek letters.
    letter = [  'alpha', 'beta', 'gamma', 'delta', 'epsilon',  'zeta', $
                'eta', 'theta', 'iota', 'kappa', 'lambda', 'mu', $
                'nu', 'xi', 'omicron', 'pi', 'rho', 'sigma', 'tau', $
                'upsilon', 'phi', 'chi', 'psi', 'omega' ]
    
    ; Output positions.
    x = [0.25, 0.6]
    y = Reverse((Indgen(12) + 1) * (1.0 / 13))
    
    ; Create a window, if needed.
    IF (!D.Flags AND 256) NE 0 THEN BEGIN
        thisWindow = !D.Window
        Window, XSIZE=600, YSIZE=500, /Free
        ERASE, COLOR=FSC_Color('white')
    ENDIF
    
    ; Output the letters.
    FOR j=0,11 DO BEGIN
        XYOuts, x[0], y[j], letter[j] + ': ' + $
            Greek(letter[j]) + Greek(letter[j], /CAPITAL), $
            /NORMAL, COLOR=FSC_Color('Black'), CHARSIZE=1.5
        XYOuts, x[1], y[j], letter[j+12] + ': ' + $
            Greek(letter[j+12]) + Greek(letter[j+12], /CAPITAL), $
            /NORMAL, COLOR=FSC_Color('Black'), CHARSIZE=1.5
    ENDFOR
    
    ; Restore the users window.
    IF N_Elements(thisWindow) NE 0 THEN BEGIN
       IF thisWindow GE 0 THEN WSet, thisWindow
    ENDIF
    
END ; --------------------------------------------------------------------------------------


FUNCTION Greek, letter, CAPITAL=capital, EXAMPLE=example

    Compile_Opt idl2
    
    ; Return to caller on error.
    ON_Error, 2
    
    ; Do you wish to see an example?
    IF Keyword_Set(example) THEN BEGIN
        Greek_Example
        RETURN, ""
    ENDIF

    ; Check keywords.
    IF N_Elements(letter) EQ 0 THEN letter = 'alpha'
    capital = Keyword_Set(capital)
    
    ; If the first letter of the "letter" variable is uppercase
    ; the user wants an uppercase Greek letter.
    firstLetter = StrMid(letter, 0, 1)
    IF firstLetter EQ StrUpCase(firstLetter) THEN capital = 1
    
    IF !D.NAME EQ 'PS' THEN BEGIN
    
        ; Make sure ISOLATIN1 encoding is turned on.
        DEVICE, /ISOLATIN1
        
        CASE StrLowCase(letter) OF
            'alpha':   greekLetter = (capital) ? '!9' + String("101B) + '!X' : '!9' + String("141B) + '!X'
            'beta':    greekLetter = (capital) ? '!9' + String("102B) + '!X' : '!9' + String("142B) + '!X'
            'gamma':   greekLetter = (capital) ? '!9' + String("107B) + '!X' : '!9' + String("147B) + '!X'
            'delta':   greekLetter = (capital) ? '!9' + String("104B) + '!X' : '!9' + String("144B) + '!X'
            'epsilon': greekLetter = (capital) ? '!9' + String("105B) + '!X' : '!9' + String("145B) + '!X'
            'zeta':    greekLetter = (capital) ? '!9' + String("132B) + '!X' : '!9' + String("172B) + '!X'
            'eta':     greekLetter = (capital) ? '!9' + String("110B) + '!X' : '!9' + String("150B) + '!X'
            'theta':   greekLetter = (capital) ? '!9' + String("121B) + '!X' : '!9' + String("161B) + '!X'
            'iota':    greekLetter = (capital) ? '!9' + String("111B) + '!X' : '!9' + String("151B) + '!X'
            'kappa':   greekLetter = (capital) ? '!9' + String("113B) + '!X' : '!9' + String("153B) + '!X'
            'lambda':  greekLetter = (capital) ? '!9' + String("114B) + '!X' : '!9' + String("154B) + '!X'
            'mu':      greekLetter = (capital) ? '!9' + String("115B) + '!X' : '!9' + String("155B) + '!X'
            'nu':      greekLetter = (capital) ? '!9' + String("116B) + '!X' : '!9' + String("156B) + '!X'
            'xi':      greekLetter = (capital) ? '!9' + String("130B) + '!X' : '!9' + String("170B) + '!X'
            'omicron': greekLetter = (capital) ? '!9' + String("117B) + '!X' : '!9' + String("157B) + '!X'
            'pi':      greekLetter = (capital) ? '!9' + String("120B) + '!X' : '!9' + String("160B) + '!X'
            'rho':     greekLetter = (capital) ? '!9' + String("122B) + '!X' : '!9' + String("162B) + '!X'
            'sigma':   greekLetter = (capital) ? '!9' + String("123B) + '!X' : '!9' + String("163B) + '!X'
            'tau':     greekLetter = (capital) ? '!9' + String("124B) + '!X' : '!9' + String("164B) + '!X'
            'upsilon': greekLetter = (capital) ? '!9' + String("125B) + '!X' : '!9' + String("165B) + '!X'
            'phi':     greekLetter = (capital) ? '!9' + String("121B) + '!X' : '!9' + String("161B) + '!X'
            'chi':     greekLetter = (capital) ? '!9' + String("103B) + '!X' : '!9' + String("143B) + '!X'
            'psi':     greekLetter = (capital) ? '!9' + String("131B) + '!X' : '!9' + String("171B) + '!X'
            'omega':   greekLetter = (capital) ? '!9' + String("127B) + '!X' : '!9' + String("167B) + '!X'
            ELSE: Message, 'The greek letter ' + letter + ' is unrecognized.'
        ENDCASE
    
    ENDIF ELSE BEGIN
    
        CASE StrLowCase(letter) OF
            'alpha':   greekLetter = (capital) ? '!4' + String("101B) + '!X' : '!4' + String("141B) + '!X'
            'beta':    greekLetter = (capital) ? '!4' + String("102B) + '!X' : '!4' + String("142B) + '!X'
            'gamma':   greekLetter = (capital) ? '!4' + String("103B) + '!X' : '!4' + String("143B) + '!X'
            'delta':   greekLetter = (capital) ? '!4' + String("104B) + '!X' : '!4' + String("144B) + '!X'
            'epsilon': greekLetter = (capital) ? '!4' + String("105B) + '!X' : '!4' + String("145B) + '!X'
            'zeta':    greekLetter = (capital) ? '!4' + String("106B) + '!X' : '!4' + String("146B) + '!X'
            'eta':     greekLetter = (capital) ? '!4' + String("107B) + '!X' : '!4' + String("147B) + '!X'
            'theta':   greekLetter = (capital) ? '!4' + String("110B) + '!X' : '!4' + String("150B) + '!X'
            'iota':    greekLetter = (capital) ? '!4' + String("111B) + '!X' : '!4' + String("151B) + '!X'
            'kappa':   greekLetter = (capital) ? '!4' + String("112B) + '!X' : '!4' + String("152B) + '!X'
            'lambda':  greekLetter = (capital) ? '!4' + String("113B) + '!X' : '!4' + String("153B) + '!X'
            'mu':      greekLetter = (capital) ? '!4' + String("114B) + '!X' : '!4' + String("154B) + '!X'
            'nu':      greekLetter = (capital) ? '!4' + String("115B) + '!X' : '!4' + String("155B) + '!X'
            'xi':      greekLetter = (capital) ? '!4' + String("116B) + '!X' : '!4' + String("156B) + '!X'
            'omicron': greekLetter = (capital) ? '!4' + String("117B) + '!X' : '!4' + String("157B) + '!X'
            'pi':      greekLetter = (capital) ? '!4' + String("120B) + '!X' : '!4' + String("160B) + '!X'
            'rho':     greekLetter = (capital) ? '!4' + String("121B) + '!X' : '!4' + String("161B) + '!X'
            'sigma':   greekLetter = (capital) ? '!4' + String("122B) + '!X' : '!4' + String("162B) + '!X'
            'tau':     greekLetter = (capital) ? '!4' + String("123B) + '!X' : '!4' + String("163B) + '!X'
            'upsilon': greekLetter = (capital) ? '!4' + String("124B) + '!X' : '!4' + String("164B) + '!X'
            'phi':     greekLetter = (capital) ? '!4' + String("125B) + '!X' : '!4' + String("165B) + '!X'
            'chi':     greekLetter = (capital) ? '!4' + String("126B) + '!X' : '!4' + String("166B) + '!X'
            'psi':     greekLetter = (capital) ? '!4' + String("127B) + '!X' : '!4' + String("167B) + '!X'
            'omega':   greekLetter = (capital) ? '!4' + String("130B) + '!X' : '!4' + String("170B) + '!X'
            ELSE: Message, 'The greek letter ' + letter + ' is unrecognized.'      
        ENDCASE
        
      ENDELSE

    RETURN, greekLetter
    
END
