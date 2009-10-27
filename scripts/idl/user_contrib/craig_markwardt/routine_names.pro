;+
; NAME:
;   ROUTINE_NAMES  (DOCUMENTATION ONLY)
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;
; PURPOSE:
;   Examine variables and parameters of procedures and call stack (OBSOLETE)
;
; CALLING SEQUENCE:
;   Various, see USAGE VARIATIONS.
;
; DESCRIPTION: 
;
;   ROUTINE_NAMES obtains information about routines, and their
;   variables and keywords.  Using these functions, a subroutine can
;   interrogate, and in some cases change, the values and names of
;   variables and parameters in its calling routine, or at the $MAIN$
;   level.  Some functionality of ROUTINE_NAMES is also in the IDL
;   system function ROUTINE_INFO, and other functionality is exclusive
;   to ROUTINE_NAMES.
;
;   ROUTINE_NAMES has been designated as "OBSOLETE" by RSI, although
;   it will probably not disappear soon since their own software
;   appears to use it.
;
;   ROUTINE_NAMES can be invoked in several different ways, which are
;   detailed below, under USAGE VARIATIONS.
;
;   ROUTINE_NAMES uses a notion of the current IDL "call level," which
;   is the numerical stack depth of the currently executing routine.
;   At each procedure or function call, the call level becomes one
;   *deeper*, and upon each RETURN, the call level becomes one
;   *shallower*.  The call stack always begins at the $MAIN$ level.
;   The current call stack can always be printed by executing HELP.
;
;   When specifying the call level to ROUTINE_NAMES, one can use one
;   of two numbering systems, depending on whichever is most
;   convenient.  In the *absolute* numbering system, the $MAIN$ level
;   starts at number 1, and becomes deeper with increasing numbers.
;   In the *relative* numbering system, the current (deepest) call
;   level is number 0, and becomes shallower with more negative
;   numbers.  Hence, if the deepest level is N, then the
;   correspondence is thus:
;
;      VALUE        MEANING
;      --------------------------------
;      1 or -N+1    $MAIN$ level
;      2 or -N+2    NEXT deeper level
;        ...           ...
;      N or 0       DEEPEST (currently executing) level
;
; USAGE VARIATIONS:
;
;   PROCS  = ROUTINE_NAMES(             [/UNRESOLVED])
;   PROCS  = ROUTINE_NAMES(/PROCEDURES [,/UNRESOLVED])
;   FUNCS  = ROUTINE_NAMES(/FUNCTIONS  [,/UNRESOLVED])
;
;            The currently compiled procedures and functions are
;            returned, respectively, as a string array.  Functions
;            declared via FORWARD_FUNCTION are also returned.  If the
;            UNRESOLVED keyword is set then the currently unresolved
;            procedures and functions are returned.  These are known
;            routines which have not yet been compiled.
;
;   PROCS  = ROUTINE_NAMES(/S_PROCEDURES)
;   FUNCS  = ROUTINE_NAMES(/S_FUNCTIONS)
;
;            The lists of system procedures and functions is returned,
;            as a string array.
;
;   LEVNUM = ROUTINE_NAMES(/LEVEL)
;
;            The call level of the calling routine is returned.
;
;   NAMES  = ROUTINE_NAMES(ARG0, ARG1, ..., ARGN, ARG_NAME=LEVEL)
;
;            The names of variables ARGi at call level LEVEL are
;            returned, as a string array.  Note that ARGi are the
;            actual parameters, not strings containing their names.
;            ARGi must be parameters that have been passed to the
;            calling procedure.  Variables that are unnamed at the
;            specified call level will return the empty string.
;            [IDL v5.0 and above only]
;            
;
;   VARS   = ROUTINE_NAMES(VARIABLES=LEVEL)
;
;            The names of variables at call level LEVEL are returned,
;            as a string array.
;
;   VARS   = ROUTINE_NAMES(PROC, /P_VARIABLES, /P_PARAMETERS)
;   VARS   = ROUTINE_NAMES(FUNC, /F_VARIABLES, /F_PARAMETERS)
;
;            The names of the variables and parameters, respectively,
;            defined in compiled procedure PROC, or compiled function
;            FUNC, are returned as a string array.
;
;   VALUE  = ROUTINE_NAMES(NAME, FETCH=LEVEL)
;
;            The value of the named variable NAME at call level LEVEL
;            is returned.  If the value is undefined, then the
;            assignment will cause an error.  Therefore, the only safe
;            way to retrieve a value is by using a variant of the
;            following:
;              IF N_ELEMENTS(ROUTINE_NAMES(NAME, FETCH=LEVEL)) GT 0 THEN $
;                VALUE  = ROUTINE_NAMES(NAME, FETCH=LEVEL)
;
;   DUMMY  = ROUTINE_NAMES(NAME, VALUE, STORE=LEVEL)
;
;            The value VALUE is stored into the named variable NAME at
;            call level LEVEL.  Note that there is no way to cause the
;            named variable to become undefined.  The value returned
;            in DUMMY can be ignored.
;            [IDL v5.2 and earlier: new variables cannot be created]
;            [IDL v5.3 and later: new variables can be created]
;
; SEE ALSO:
;
;   ROUTINE_INFO, ARG_PRESENT, DXDEBUG (Markwardt Debug Library)
;
; MODIFICATION HISTORY:
;   Written, 20 Jul 2000
;   Documented differences between IDL versions, 21 Sep 2000, CM
;
;
;  $Id$
;
;-
; Copyright (C) 2000, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-
forward_function routine_names
end

