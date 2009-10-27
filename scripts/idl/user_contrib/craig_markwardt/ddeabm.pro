;+
; NAME:
;   DDEABM
;
; AUTHOR:
;   Craig B. Markwardt, NASA/GSFC Code 662, Greenbelt, MD 20770
;   craigm@lheamail.gsfc.nasa.gov
;   UPDATED VERSIONs can be found on my WEB PAGE: 
;      http://cow.physics.wisc.edu/~craigm/idl/idl.html
;
; PURPOSE:
;   Integrate Ordinary Differential Equation with Adams-Bashford-Moulton
;
; MAJOR TOPICS:
;   Numerical Analysis.
;
; CALLING SEQUENCE:
;   DDEABM, FUNCT, T0, F0, TOUT, [ PRIVATE, FUNCTARGS=, STATE= , ]
;     [ /INIT, /INTERMEDIATE, TSTOP=, EPSREL=, EPSABS=, ]
;     [ TGRID=, YGRID=, YPGRID=, NGRID=, NFEV=, ]
;     [ STATUS=, ERRMSG= ]
;
; DESCRIPTION:
;
;  DDEABM performs integration of a system of one or more ordinary
;  differential equations using a Predictor-Corrector technique.  An
;  adaptive Adams-Bashford-Moulton method of variable order between
;  one and twelve, adaptive stepsize, and error control, is used to
;  integrate equations of the form:
;
;     DF_DT = FUNCT(T, F)
;
;  T is the independent variable, F is the (possibly vector) function
;  value at T, and DF_DT is the derivative of F with respect to T,
;  evaluated at T.  FUNCT is a user function which returns the
;  derivative of one or more equations.  
;
;  DDEABM is based on the public domain procedure DDEABM.F written by
;  L. F. Shampine and M. K. Gordon, and available in the DEPAC package
;  of solvers within SLATEC library.
;
;  DDEABM is used primarily to solve non-stiff and mildly stiff
;  ordinary differential equations, where evaluation of the user
;  function is expensive, or high precision is demanded (close to the
;  machine precision).  A Runge-Kutta technique may be more
;  appropriate if lower precision is acceptable.  For stiff problems
;  neither Adams-Bashford-Moulton nor Runge-Kutta techniques are
;  appropriate.  DDEABM has code which detects the stiffness of the
;  problem and reports it.
;
;  The user can operate DDEABM in three different modes:
;
;    * the user requests one output at a specific point (the default),
;      and maintains the integrator state using the STATE keyword;
;
;    * the user requests a grid of points (by passing an array to
;      TOUT), and optionally maintains the integrator state for
;      subsequent integrations using the STATE keyword;
;
;    * the user requests output at the natural adaptive stepsizes
;      using the /INTERMEDIATE keyword.
;
;  When the user requests explicit output points using TOUT, it is
;  likely that the output will be interpolated between two natural
;  stepsize points.
;
;  If the user requests a grid of outputs, and the integration fails
;  before reaching the requested endpoint, then control returns
;  immediately to the user with the appropriate STATUS code.
;
;  The initial conditions are given by F0, when T = T0.  The number of
;  equations is determined by the number of elements in F0.
;  Integration stops when the independent variable T reaches the final
;  value of TOUT.  If the user wants to continue the integration, it
;  can be restarted with a new call to DDEABM, and a new value of
;  TOUT.
;
; USER FUNCTION
;
;  The user function FUNCT must be of the following form:
;
;  FUNCTION FUNCT, T, F, PRIVATE, ...  [ CONTROL=CONTROL ] ...
;     ; ... compute derivatives ...
;     RETURN, DF_DT
;  END
;
;  The function must accept at least two, but optionally three,
;  parameters.  The first, 'T', is the scalar independent variable
;  where the derivatives are to be evaluated.  The second, 'F', is the
;  vector of function values.  The function must return an array of
;  same size as 'F'.  The third positional parameter, 'PRIVATE', is a
;  purely optional PRIVATE parameter.  FUNCT may accept more
;  positional parameters, but DDEABM will not use them.  Any number of
;  keyword parameters can be passed using the FUNCTARGS keyword.
;
;  The user function FUNCT must not modify either the independent
;  variable 'T' or the function values 'F'.
;
; PASSING 'CONTROL' MESSAGES TO THE USER FUNCTION
;
;  DDEABM may pass control information to the user function, other
;  than requests for function evaluation.  DDEABM will only do this if
;  the /CONTROL keyword is set when DDEABM was invoked.
;
;  When control information has been enabled, the user function *must*
;  accept a keyword named CONTROL.  A message may be passed in this
;  keyword.  If the user function is invoked with the CONTROL keyword
;  defined, the user function should not evaluate the function, but
;  rather it must respond to the message and return the appropriate
;  value.
;
;  The CONTROL message will be a structure of the form,
;     { MESSAGE: 'name', ... }
;  where the MESSAGE field indicates a control message.  Additional
;  fields may carry more information, depending on the message.
;
;  The following control messages are implemented:
;    * { MESSAGE: 'INITIALIZE' } - the integrator has been initialized
;      and the user function may also initialize any state variables,
;      load large data tables, etc.
;      RETURN: 0 for success, negative number to indicate failure.
;
;  If the user function does not recognize a message, a value of 0
;  should be returned.
;
;
; RESTARTING THE INTEGRATOR
;
;  When restarting the integrator, the STATE keyword can be used to
;  save some computation time.  Upon return from DDEABM the STATE
;  keyword will contain a structure which describes the state of the
;  integrator at the output point.  The user need merely pass this
;  variable back into the next call to continue integration.  The
;  value of T, and the function value F, must not be adjusted between
;  calls to DDEABM.
;
;  If T or F0 are to be adjusted, then the integrator must be
;  restarted afresh *without* the previous state.  This can be
;  achieved either by reseting STATE to undefined, or by setting the
;  /INIT keyword to DDEABM.
;
; ERROR CONTROL
;
;  Local error control is governed by two keywords, EPSABS and EPSREL.
;  The former governs the absolute error, while the latter governs the
;  relative or fractional error.  The error test at each iteration is:
;
;     ABS(ERROR)   LE   EPSREL*ABS(F) + EPSABS
;  
;  A scalar value indicates the same constraint should be applied to
;  every equation; a vector array indicates constraints should be
;  applied individually to each component.
;
;  Setting EPSABS=0.D0 results in a pure relative error test on that
;  component. Setting EPSREL=0. results in a pure absolute error test
;  on that component.  A mixed test with non-zero EPSREL and EPSABS
;  corresponds roughly to a relative error test when the solution
;  component is much bigger than EPSABS and to an absolute error test
;  when the solution component is smaller than the threshold EPSABS.
;  
;  Proper selection of the absolute error control parameters EPSABS
;  requires you to have some idea of the scale of the solution
;  components.  To acquire this information may mean that you will
;  have to solve the problem more than once. In the absence of scale
;  information, you should ask for some relative accuracy in all the
;  components (by setting EPSREL values non-zero) and perhaps impose
;  extremely small absolute error tolerances to protect against the
;  danger of a solution component becoming zero.
;  
;  The code will not attempt to compute a solution at an accuracy
;  unreasonable for the machine being used.  It will advise you if you
;  ask for too much accuracy and inform you as to the maximum accuracy
;  it believes possible.
;
; HARD LIMIT ON T
;
;  If for some reason there is a hard limit on the independent
;  variable T, then the user should specify TSTOP.  For efficiency
;  DDEABM may try to integrate *beyond* the requested point of
;  termination, TOUT, and then interpolate backwards to obtain the
;  function value at the requested point.  If this is not possible
;  because the function because the equation changes, or if there is a
;  discontinuity, then users should specify a value for TSTOP; the
;  integrator will not go past this point.
;
; DISCONTINUITIES
;
;  If the ODE or solution has discontinuities at known points, these
;  should be passed to DDEABM in order to aid the solution.  The
;  TIMPULSE and YIMPULSE keyword variables allow the user to identify
;  the positions of the discontinuities and their amplitudes.  As T
;  crosses TIMPULSE(i) the solution will change from Y to
;  Y+YIMPULSE(*,i) in a stepwise fashion.  
;
;  Discontinuities in the function to be integrated can also be
;  entered in this way.  Although DDEABM can adapt the integration
;  step size to accomodate changes in the user function, it may be
;  better to identify such discontinuities.  In that case TIMPULSE(i)
;  should still identify the position of discontinuity, and
;  YIMPULSE(*,i) should be 0.
;
;  Currently this functionality is implemented with restarts of the
;  integrator at the crossing points of the discontinuities.
;
;  This technique handles only discontinuities at explicitly known
;  values of T.  If the discontinuity condition is defined in terms of
;  Y  (or Y and T), then the condition is implicit.  DDEABM does not
;  handle that type of condition.
;
;  You may list the TIMPULSE points in the TOUT output grid.  If an
;  impulse point appears once in TOUT, the corresponding function
;  values reported in YGRID and YPGRID will be from *before* crossing
;  the discontinuity.  If the same TIMPULSE point appears *twice* in
;  TOUT, then the first and second values will correspond to before
;  and after crossing the discontinuity, respectively.
;
;
; INPUTS:
;
;  FUNCT - by default, a scalar string containing the name of an IDL
;          function to be integrated.  See above for the formal
;          definition of FUNCT.  (No default).
;
;  T0 - scalar number, upon input the starting value of the
;       independent variable T.  Upon output, the final value of T.
;
;  Y - vector.  Upon input the starting values of the function for T =
;      T0.  Upon output, the final (vector) value of the function.
;
;  TOUT - must be at least a scalar, but optionally a vector,
;         specifies the desired points of output.
;
;         If TOUT is a scalar and INTERMEDIATE is not set, then DDEABM
;         integrates up to TOUT.  A scalar value of the function at
;         TOUT is returned in F.
;
;         If TOUT is a scalar and /INTERMEDIATE is set, then DDEABM
;         computes a grid of function values at the optimal step
;         points of the solution.  The grid of values is returned in
;         TGRID, YGRID, and YPGRID.  The final function value,
;         evaluated at the last point in TOUT, is returned in F.
;
;         If TOUT is a vector, then DDEABM computes a grid of function
;         values at the requested points.  The grid of values is
;         returned in TGRID, YGRID and YPGRID.  The final function
;         value, evaluated at the last point in TOUT, is returned in
;         F.  If integrating forwards (TOUT greater than T0), TOUT
;         must be strictly increasing.  Generally speaking, TOUT
;         output points should not repeat, except for discontinuities
;         as noted above.
;
;         It is possible for TOUT to be less than T0, i.e., to
;         integrate backwards, in which case TOUT must be strictly
;         decreasing instead.
; 
;  PRIVATE - any optional variable to be passed on to the function to
;            be integrated.  For functions, PRIVATE is passed as the
;            second positional parameter.  DDEABM does not examine or
;            alter PRIVATE.
;
; KEYWORD PARAMETERS:
;
;   CONTROL - if set, then control messages will be set to the user
;             function as described above.  If not set, then no
;             control messages will be passed.
;
;   EPSABS - a scalar number, the absolute error tolerance requested
;            in the integral computation.  If less than or equal to
;            zero, then the value is ignored.
;            Default: 0
;
;   EPSREL - a scalar number, the relative (i.e., fractional) error
;            tolerance in the integral computation.  If less than or
;            equal to zero, then the value is ignored.
;            Default: 1e-4 for float, 1d-6 for double
;
;   ERRMSG - upon return, a descriptive error message.
;
;   FUNCTARGS - A structure which contains the parameters to be passed
;               to the user-supplied function specified by FUNCT via
;               the _EXTRA mechanism.  This is the way you can pass
;               additional data to your user-supplied function without
;               using common blocks.  By default, no extra parameters
;               are passed to the user-supplied function.
;
;   INIT - if set, indicates that the integrator is to be restarted
;          afresh.
;
;   INTERMEDIATE - if set, indicates that the integrator is to compute
;                  at the natural step points.
;
;   MAX_STEPSIZE - a positive scalar value, the maximum integration
;                  step size to take per iteration.  The lesser of the
;                  "natural" step size and MAX_STEPSIZE is used.  If
;                  MAX_STEPSIZE is not specified, there is no maximum.
;
;   NFEV - upon output, the scalar number of function evaluations.
;
;   NGRID - if /INTERMEDIATE is set, the number of points to compute
;           before returning.
;           Default: 1
;
;   NOUTGRID - upon output, the number of grid points computed.  This
;              may be smaller than the requested number of grid points
;              (either via NGRID or TOUT) if an error occurs.
;
;   STATE - upon input and return, the integrator state.  Users should
;           not modify this structure.  If the integrator is to be
;           restarted afresh, then the /INIT keyword should be set, in
;           order to clear out the old state information.
;
;   STATUS - upon output, the integer status of the integration.
;
;           1 - A step was successfully taken in the
;               intermediate-output mode.  The code has not yet
;               reached TOUT.
; 
;           2 - The integration to TOUT was successfully completed
;               (T=TOUT) by stepping exactly to TOUT.
; 
;           3 - The integration to TOUT was successfully completed
;               (T=TOUT) by stepping past TOUT.  Y(*) is obtained by
;               interpolation.
; 
;                       *** Task Interrupted ***
;                 Reported by negative values of STATUS
; 
;           -1 - A large amount of work has been expended.  (500 steps
;                attempted)
; 
;           -2 - The error tolerances are too stringent.
; 
;           -3 - The local error test cannot be satisfied because you
;                specified a zero component in EPSABS and the
;                corresponding computed solution component is zero.
;                Thus, a pure relative error test is impossible for
;                this component.
; 
;           -4 - The problem appears to be stiff.
; 
;                       *** Task Terminated ***
;                 Reported by the value of STATUS=-33
; 
;           -33 - The code has encountered trouble from which it
;                 cannot recover.  A error message is printed
;                 explaining the trouble and control is returned to
;                 the calling program. For example, this occurs when
;                 invalid input is detected.
;
;           -16 - The user function returned a non-finite 
;
;   TGRID - upon output, the grid of values of T for which the
;           integration is provided.
;
;   TIMPULSE - array of values of T where discontinuities occur.  The
;              array should be in ascending order.  TIMPULSE must
;              match YIMPULSE.
;
;   TSTOP - a scalar, specifies a hard limit for T, beyond which the
;           integration must not proceed.
;           Default:  none, i.e., integration is allowed to
;                    "overshoot"
;
;   YGRID - upon output, the grid of function values for which the
;           integration is provided.
;
;   YIMPULSE - array of discontinuity offset values, of the form
;              DBLARR(NEQ,NIMPULSE), where NEQ is the size of Y and
;              NIMPULSE is the size of TIMPULSE.  YIMPULSE(*,I) is the
;              amount to *add* to Y when T crosses TIMPULSE(I) going
;              in the positive direction.
;
;   YPGRID - upon ouptut, the grid of function derivative values at
;            the points where the integration is provided.
;
; EXAMPLES:
;
;
; REFERENCES:
;
;   SAND79-2374 , DEPAC - Design of a User Oriented Package of ODE
;                         Solvers.
;
;   "Solving Ordinary Differential Equations with ODE, STEP, and INTRP",
;       by L. F. Shampine and M. K. Gordon, SLA-73-1060.
;
;    SLATEC Common Mathematical Library, Version 4.1, July 1993
;    a comprehensive software library containing over
;    1400 general purpose mathematical and statistical routines
;    written in Fortran 77.  (http://www.netlib.org/slatec/)
;
;
; MODIFICATION HISTORY:
;    Fix bug in TSTOP keyword, 09 May 2002, CM
;    Fix behavior of KSTEPS, which caused premature termination, 26
;      May 2002, CM
;    Fix two errors in the DDEABM_DINTP interpolation step, 04 Jul 2002, CM
;    Handle case of IMPULSES more correctly, 25 Sep 2002, CM
;    Handle case when INIT is not set (default to 1); detect
;      non-finite user function values and error out with STATUS code
;      -16; promote integer values to LONG integers; some internal 
;      function renaming, 28 Jun 2003, CM
;    Fixed bug in handling of DOIMPULSE and INTERMEDIATE, 08 Mar 2004, CM
;    Corrected interface error in usage of NGRID.  Now NGRID is
;      actually the number of INTERMEDIATE points to compute (and is
;      input only).  NOUTGRID is a new keyword, which provides the
;      number of output grid points upon return.  08 Mar 2004, CM
;    Early termination is possible for INTERMEDIATE case.  Handle it
;      properly , 08 Mar 2004, CM
;    Fix a bug in the handling of INIT (strangely the internal
;      code keeps two different INIT variables!); this really only 
;      had an effect when continuing a previous integration; handle
;      impulses properly when integrating in the negative direction; 
;      document the TIMPULSE/YIMPULSE keyword parameters; some other
;      small code cleanups;  16 Jul 2008, CM
;    Handle the case when TOUT EQ TIMPULSE, 05 Sep 2008, CM
;    Further work on TOUT EQ TIMPULSE, also allowing reporting of
;      function values on either side of a discontinuity, 07 Sep 2008, CM
;    Add the MAX_STEPSIZE keyword, 01 Oct 2008, CM
;    Make sure new impulse checks work when integrating in reverse
;      direction, 09 Oct 2008, CM
;    New interface requirement: user function must be able to handle 
;      control messages from DDEABM; first one is INITIALIZE,
;      20 Oct 2008, CM
;    Change the control message interface so that it is
;      backward-compatible; the user must now set the /CONTROL keyword
;      to enable control messages; they are passed to the user
;      function via the CONTROL keyword, 08 Nov 2008, CM

;
;  $Id$
;-
; Portions Copyright (C) 2002, 2003, 2004, 2008, Craig Markwardt
; This software is provided as is without any warranty whatsoever.
; Permission to use, copy, modify, and distribute modified or
; unmodified copies is granted, provided this copyright and disclaimer
; are included unchanged.
;-

; *DECK DDEABM
; C***BEGIN PROLOGUE  DDEABM
; C***PURPOSE  Solve an initial value problem in ordinary differential
; C            equations using an Adams-Bashforth method.
; C***LIBRARY   SLATEC (DEPAC)
; C***CATEGORY  I1A1B
; C***TYPE      DOUBLE PRECISION (DEABM-S, DDEABM-D)
; C***KEYWORDS  ADAMS-BASHFORTH METHOD, DEPAC, INITIAL VALUE PROBLEMS,
; C             ODE, ORDINARY DIFFERENTIAL EQUATIONS, PREDICTOR-CORRECTOR
; C***AUTHOR  Shampine, L. F., (SNLA)
; C           Watts, H. A., (SNLA)
; C***DESCRIPTION
; C
; C   This is the Adams code in the package of differential equation
; C   solvers DEPAC, consisting of the codes DDERKF, DDEABM, and DDEBDF.
; C   Design of the package was by L. F. Shampine and H. A. Watts.
; C   It is documented in
; C        SAND79-2374 , DEPAC - Design of a User Oriented Package of ODE
; C                              Solvers.
; C   DDEABM is a driver for a modification of the code ODE written by
; C             L. F. Shampine and M. K. Gordon
; C             Sandia Laboratories
; C             Albuquerque, New Mexico 87185
;
;   $Id$
;
; C
; C **********************************************************************
; C * ABSTRACT *
; C ************
; C
; C   Subroutine DDEABM uses the Adams-Bashforth-Moulton
; C   Predictor-Corrector formulas of orders one through twelve to
; C   integrate a system of NEQ first order ordinary differential
; C   equations of the form
; C                         DU/DX = DF(X,U)
; C   when the vector Y(*) of initial values for U(*) at X=T is given.
; C   The subroutine integrates from T to TOUT. It is easy to continue the
; C   integration to get results at additional TOUT.  This is the interval
; C   mode of operation.  It is also easy for the routine to return with
; C   the solution at each intermediate step on the way to TOUT.  This is
; C   the intermediate-output mode of operation.
; C
; C   DDEABM uses subprograms DDEABM_DDES, DDEABM_DSTEPS, DDEABM_DINTP, DHSTRT, DHVNRM,
; C   D1MACH, and the error handling routine XERMSG.  The only machine
; C   dependent parameters to be assigned appear in D1MACH.
; C
; C **********************************************************************
; C * Description of The Arguments To DDEABM (An Overview) *
; C **********************************************************************
; C
; C   The Parameters are
; C
; C      DF -- This is the name of a subroutine which you provide to
; C             define the differential equations.
; C
; C      NEQ -- This is the number of (first order) differential
; C             equations to be integrated.
; C
; C      T -- This is a DOUBLE PRECISION value of the independent
; C           variable.
; C
; C      Y(*) -- This DOUBLE PRECISION array contains the solution
; C              components at T.
; C
; C      TOUT -- This is a DOUBLE PRECISION point at which a solution is
; C              desired.
; C
; C      INFO(*) -- The basic task of the code is to integrate the
; C             differential equations from T to TOUT and return an
; C             answer at TOUT.  INFO(*) is an INTEGER array which is used
; C             to communicate exactly how you want this task to be
; C             carried out.
; C
; C      RTOL, ATOL -- These DOUBLE PRECISION quantities represent
; C                    relative and absolute error tolerances which you
; C                    provide to indicate how accurately you wish the
; C                    solution to be computed.  You may choose them to be
; C                    both scalars or else both vectors.
; C
; C      IDID -- This scalar quantity is an indicator reporting what
; C             the code did.  You must monitor this INTEGER variable to
; C             decide what action to take next.
; C
; C      RWORK(*), LRW -- RWORK(*) is a DOUBLE PRECISION work array of
; C             length LRW which provides the code with needed storage
; C             space.
; C
; C      IWORK(*), LIW -- IWORK(*) is an INTEGER work array of length LIW
; C             which provides the code with needed storage space and an
; C             across call flag.
; C
; C      RPAR, IPAR -- These are DOUBLE PRECISION and INTEGER parameter
; C             arrays which you can use for communication between your
; C             calling program and the DF subroutine.
; C
; C  Quantities which are used as input items are
; C             NEQ, T, Y(*), TOUT, INFO(*),
; C             RTOL, ATOL, RWORK(1), LRW and LIW.
; C
; C  Quantities which may be altered by the code are
; C             T, Y(*), INFO(1), RTOL, ATOL,
; C             IDID, RWORK(*) and IWORK(*).
; C
; C **********************************************************************
; C * INPUT -- What To Do On The First Call To DDEABM *
; C **********************************************************************
; C
; C   The first call of the code is defined to be the start of each new
; C   problem.  Read through the descriptions of all the following items,
; C   provide sufficient storage space for designated arrays, set
; C   appropriate variables for the initialization of the problem, and
; C   give information about how you want the problem to be solved.
; C
; C
; C      DF -- Provide a subroutine of the form
; C                               DF(X,U,UPRIME,PAR,IPAR)
; C             to define the system of first order differential equations
; C             which is to be solved.  For the given values of X and the
; C             vector  U(*)=(U(1),U(2),...,U(NEQ)) , the subroutine must
; C             evaluate the NEQ components of the system of differential
; C             equations  DU/DX=DF(X,U)  and store the derivatives in the
; C             array UPRIME(*), that is,  UPRIME(I) = * DU(I)/DX *  for
; C             equations I=1,...,NEQ.
; C
; C             Subroutine DF must NOT alter X or U(*).  You must declare
; C             the name df in an external statement in your program that
; C             calls DDEABM.  You must dimension U and UPRIME in DF.
; C
; C             RPAR and IPAR are DOUBLE PRECISION and INTEGER parameter
; C             arrays which you can use for communication between your
; C             calling program and subroutine DF. They are not used or
; C             altered by DDEABM.  If you do not need RPAR or IPAR,
; C             ignore these parameters by treating them as dummy
; C             arguments. If you do choose to use them, dimension them in
; C             your calling program and in DF as arrays of appropriate
; C             length.
; C
; C      NEQ -- Set it to the number of differential equations.
; C             (NEQ .GE. 1)
; C
; C      T -- Set it to the initial point of the integration.
; C             You must use a program variable for T because the code
; C             changes its value.
; C
; C      Y(*) -- Set this vector to the initial values of the NEQ solution
; C             components at the initial point.  You must dimension Y at
; C             least NEQ in your calling program.
; C
; C      TOUT -- Set it to the first point at which a solution
; C             is desired.  You can take TOUT = T, in which case the code
; C             will evaluate the derivative of the solution at T and
; C             return. Integration either forward in T  (TOUT .GT. T)  or
; C             backward in T  (TOUT .LT. T)  is permitted.
; C
; C             The code advances the solution from T to TOUT using
; C             step sizes which are automatically selected so as to
; C             achieve the desired accuracy.  If you wish, the code will
; C             return with the solution and its derivative following
; C             each intermediate step (intermediate-output mode) so that
; C             you can monitor them, but you still must provide TOUT in
; C             accord with the basic aim of the code.
; C
; C             The first step taken by the code is a critical one
; C             because it must reflect how fast the solution changes near
; C             the initial point.  The code automatically selects an
; C             initial step size which is practically always suitable for
; C             the problem. By using the fact that the code will not step
; C             past TOUT in the first step, you could, if necessary,
; C             restrict the length of the initial step size.
; C
; C             For some problems it may not be permissible to integrate
; C             past a point TSTOP because a discontinuity occurs there
; C             or the solution or its derivative is not defined beyond
; C             TSTOP.  When you have declared a TSTOP point (see INFO(4)
; C             and RWORK(1)), you have told the code not to integrate
; C             past TSTOP.  In this case any TOUT beyond TSTOP is invalid
; C             input.
; C
; C      INFO(*) -- Use the INFO array to give the code more details about
; C             how you want your problem solved.  This array should be
; C             dimensioned of length 15 to accommodate other members of
; C             DEPAC or possible future extensions, though DDEABM uses
; C             only the first four entries.  You must respond to all of
; C             the following items which are arranged as questions.  The
; C             simplest use of the code corresponds to answering all
; C             questions as YES ,i.e. setting ALL entries of INFO to 0.
; C
; C        INFO(1) -- This parameter enables the code to initialize
; C               itself.  You must set it to indicate the start of every
; C               new problem.
; C
; C            **** Is this the first call for this problem ...
; C                  YES -- set INFO(1) = 0
; C                   NO -- not applicable here.
; C                         See below for continuation calls.  ****
; C
; C        INFO(2) -- How much accuracy you want of your solution
; C               is specified by the error tolerances RTOL and ATOL.
; C               The simplest use is to take them both to be scalars.
; C               To obtain more flexibility, they can both be vectors.
; C               The code must be told your choice.
; C
; C            **** Are both error tolerances RTOL, ATOL scalars ...
; C                  YES -- set INFO(2) = 0
; C                         and input scalars for both RTOL and ATOL
; C                   NO -- set INFO(2) = 1
; C                         and input arrays for both RTOL and ATOL ****
; C
; C        INFO(3) -- The code integrates from T in the direction
; C               of TOUT by steps.  If you wish, it will return the
; C               computed solution and derivative at the next
; C               intermediate step (the intermediate-output mode) or
; C               TOUT, whichever comes first.  This is a good way to
; C               proceed if you want to see the behavior of the solution.
; C               If you must have solutions at a great many specific
; C               TOUT points, this code will compute them efficiently.
; C
; C            **** Do you want the solution only at
; C                 TOUT (and not at the next intermediate step) ...
; C                  YES -- set INFO(3) = 0
; C                   NO -- set INFO(3) = 1 ****
; C
; C        INFO(4) -- To handle solutions at a great many specific
; C               values TOUT efficiently, this code may integrate past
; C               TOUT and interpolate to obtain the result at TOUT.
; C               Sometimes it is not possible to integrate beyond some
; C               point TSTOP because the equation changes there or it is
; C               not defined past TSTOP.  Then you must tell the code
; C               not to go past.
; C
; C            **** Can the integration be carried out without any
; C                 Restrictions on the independent variable T ...
; C                  YES -- set INFO(4)=0
; C                   NO -- set INFO(4)=1
; C                         and define the stopping point TSTOP by
; C                         setting RWORK(1)=TSTOP ****
; C
; C      RTOL, ATOL -- You must assign relative (RTOL) and absolute (ATOL)
; C             error tolerances to tell the code how accurately you want
; C             the solution to be computed.  They must be defined as
; C             program variables because the code may change them.  You
; C             have two choices --
; C                  Both RTOL and ATOL are scalars. (INFO(2)=0)
; C                  Both RTOL and ATOL are vectors. (INFO(2)=1)
; C             In either case all components must be non-negative.
; C
; C             The tolerances are used by the code in a local error test
; C             at each step which requires roughly that
; C                     ABS(LOCAL ERROR) .LE. RTOL*ABS(Y)+ATOL
; C             for each vector component.
; C             (More specifically, a Euclidean norm is used to measure
; C             the size of vectors, and the error test uses the magnitude
; C             of the solution at the beginning of the step.)
; C
; C             The true (global) error is the difference between the true
; C             solution of the initial value problem and the computed
; C             approximation.  Practically all present day codes,
; C             including this one, control the local error at each step
; C             and do not even attempt to control the global error
; C             directly.  Roughly speaking, they produce a solution Y(T)
; C             which satisfies the differential equations with a
; C             residual R(T),    DY(T)/DT = DF(T,Y(T)) + R(T)   ,
; C             and, almost always, R(T) is bounded by the error
; C             tolerances.  Usually, but not always, the true accuracy of
; C             the computed Y is comparable to the error tolerances. This
; C             code will usually, but not always, deliver a more accurate
; C             solution if you reduce the tolerances and integrate again.
; C             By comparing two such solutions you can get a fairly
; C             reliable idea of the true error in the solution at the
; C             bigger tolerances.
; C
; C             Setting ATOL=0.D0 results in a pure relative error test on
; C             that component. Setting RTOL=0. results in a pure absolute
; C             error test on that component.  A mixed test with non-zero
; C             RTOL and ATOL corresponds roughly to a relative error
; C             test when the solution component is much bigger than ATOL
; C             and to an absolute error test when the solution component
; C             is smaller than the threshold ATOL.
; C
; C             Proper selection of the absolute error control parameters
; C             ATOL  requires you to have some idea of the scale of the
; C             solution components.  To acquire this information may mean
; C             that you will have to solve the problem more than once. In
; C             the absence of scale information, you should ask for some
; C             relative accuracy in all the components (by setting  RTOL
; C             values non-zero) and perhaps impose extremely small
; C             absolute error tolerances to protect against the danger of
; C             a solution component becoming zero.
; C
; C             The code will not attempt to compute a solution at an
; C             accuracy unreasonable for the machine being used.  It will
; C             advise you if you ask for too much accuracy and inform
; C             you as to the maximum accuracy it believes possible.
; C
; C      RWORK(*) -- Dimension this DOUBLE PRECISION work array of length
; C             LRW in your calling program.
; C
; C      RWORK(1) -- If you have set INFO(4)=0, you can ignore this
; C             optional input parameter.  Otherwise you must define a
; C             stopping point TSTOP by setting   RWORK(1) = TSTOP.
; C             (for some problems it may not be permissible to integrate
; C             past a point TSTOP because a discontinuity occurs there
; C             or the solution or its derivative is not defined beyond
; C             TSTOP.)
; C
; C      LRW -- Set it to the declared length of the RWORK array.
; C             You must have  LRW .GE. 130+21*NEQ
; C
; C      IWORK(*) -- Dimension this INTEGER work array of length LIW in
; C             your calling program.
; C
; C      LIW -- Set it to the declared length of the IWORK array.
; C             You must have  LIW .GE. 51
; C
; C      RPAR, IPAR -- These are parameter arrays, of DOUBLE PRECISION and
; C             INTEGER type, respectively.  You can use them for
; C             communication between your program that calls DDEABM and
; C             the  DF subroutine.  They are not used or altered by
; C             DDEABM.  If you do not need RPAR or IPAR, ignore these
; C             parameters by treating them as dummy arguments.  If you do
; C             choose to use them, dimension them in your calling program
; C             and in DF as arrays of appropriate length.
; C
; C **********************************************************************
; C * OUTPUT -- After Any Return From DDEABM *
; C **********************************************************************
; C
; C   The principal aim of the code is to return a computed solution at
; C   TOUT, although it is also possible to obtain intermediate results
; C   along the way.  To find out whether the code achieved its goal
; C   or if the integration process was interrupted before the task was
; C   completed, you must check the IDID parameter.
; C
; C
; C      T -- The solution was successfully advanced to the
; C             output value of T.
; C
; C      Y(*) -- Contains the computed solution approximation at T.
; C             You may also be interested in the approximate derivative
; C             of the solution at T.  It is contained in
; C             RWORK(21),...,RWORK(20+NEQ).
; C
; C      IDID -- Reports what the code did
; C
; C                         *** Task Completed ***
; C                   Reported by positive values of IDID
; C
; C             IDID = 1 -- A step was successfully taken in the
; C                       intermediate-output mode.  The code has not
; C                       yet reached TOUT.
; C
; C             IDID = 2 -- The integration to TOUT was successfully
; C                       completed (T=TOUT) by stepping exactly to TOUT.
; C
; C             IDID = 3 -- The integration to TOUT was successfully
; C                       completed (T=TOUT) by stepping past TOUT.
; C                       Y(*) is obtained by interpolation.
; C
; C                         *** Task Interrupted ***
; C                   Reported by negative values of IDID
; C
; C             IDID = -1 -- A large amount of work has been expended.
; C                       (500 steps attempted)
; C
; C             IDID = -2 -- The error tolerances are too stringent.
; C
; C             IDID = -3 -- The local error test cannot be satisfied
; C                       because you specified a zero component in ATOL
; C                       and the corresponding computed solution
; C                       component is zero.  Thus, a pure relative error
; C                       test is impossible for this component.
; C
; C             IDID = -4 -- The problem appears to be stiff.
; C
; C             IDID = -5,-6,-7,..,-32  -- Not applicable for this code
; C                       but used by other members of DEPAC or possible
; C                       future extensions.
; C
; C                         *** Task Terminated ***
; C                   Reported by the value of IDID=-33
; C
; C             IDID = -33 -- The code has encountered trouble from which
; C                       it cannot recover.  A message is printed
; C                       explaining the trouble and control is returned
; C                       to the calling program. For example, this occurs
; C                       when invalid input is detected.
; C
; C      RTOL, ATOL -- These quantities remain unchanged except when
; C             IDID = -2.  In this case, the error tolerances have been
; C             increased by the code to values which are estimated to be
; C             appropriate for continuing the integration.  However, the
; C             reported solution at T was obtained using the input values
; C             of RTOL and ATOL.
; C
; C      RWORK, IWORK -- Contain information which is usually of no
; C             interest to the user but necessary for subsequent calls.
; C             However, you may find use for
; C
; C             RWORK(11)--which contains the step size H to be
; C                        attempted on the next step.
; C
; C             RWORK(12)--if the tolerances have been increased by the
; C                        code (IDID = -2) , they were multiplied by the
; C                        value in RWORK(12).
; C
; C             RWORK(13)--Which contains the current value of the
; C                        independent variable, i.e. the farthest point
; C                        integration has reached. This will be different
; C                        from T only when interpolation has been
; C                        performed (IDID=3).
; C
; C             RWORK(20+I)--Which contains the approximate derivative
; C                        of the solution component Y(I).  In DDEABM, it
; C                        is obtained by calling subroutine DF to
; C                        evaluate the differential equation using T and
; C                        Y(*) when IDID=1 or 2, and by interpolation
; C                        when IDID=3.
; C
; C **********************************************************************
; C * INPUT -- What To Do To Continue The Integration *
; C *             (calls after the first)             *
; C **********************************************************************
; C
; C        This code is organized so that subsequent calls to continue the
; C        integration involve little (if any) additional effort on your
; C        part. You must monitor the IDID parameter in order to determine
; C        what to do next.
; C
; C        Recalling that the principal task of the code is to integrate
; C        from T to TOUT (the interval mode), usually all you will need
; C        to do is specify a new TOUT upon reaching the current TOUT.
; C
; C        Do not alter any quantity not specifically permitted below,
; C        in particular do not alter NEQ, T, Y(*), RWORK(*), IWORK(*) or
; C        the differential equation in subroutine DF. Any such alteration
; C        constitutes a new problem and must be treated as such, i.e.
; C        you must start afresh.
; C
; C        You cannot change from vector to scalar error control or vice
; C        versa (INFO(2)) but you can change the size of the entries of
; C        RTOL, ATOL.  Increasing a tolerance makes the equation easier
; C        to integrate.  Decreasing a tolerance will make the equation
; C        harder to integrate and should generally be avoided.
; C
; C        You can switch from the intermediate-output mode to the
; C        interval mode (INFO(3)) or vice versa at any time.
; C
; C        If it has been necessary to prevent the integration from going
; C        past a point TSTOP (INFO(4), RWORK(1)), keep in mind that the
; C        code will not integrate to any TOUT beyond the currently
; C        specified TSTOP.  Once TSTOP has been reached you must change
; C        the value of TSTOP or set INFO(4)=0.  You may change INFO(4)
; C        or TSTOP at any time but you must supply the value of TSTOP in
; C        RWORK(1) whenever you set INFO(4)=1.
; C
; C        The parameter INFO(1) is used by the code to indicate the
; C        beginning of a new problem and to indicate whether integration
; C        is to be continued.  You must input the value  INFO(1) = 0
; C        when starting a new problem.  You must input the value
; C        INFO(1) = 1  if you wish to continue after an interrupted task.
; C        Do not set  INFO(1) = 0  on a continuation call unless you
; C        want the code to restart at the current T.
; C
; C                         *** Following A Completed Task ***
; C         If
; C             IDID = 1, call the code again to continue the integration
; C                     another step in the direction of TOUT.
; C
; C             IDID = 2 or 3, define a new TOUT and call the code again.
; C                     TOUT must be different from T. You cannot change
; C                     the direction of integration without restarting.
; C
; C                         *** Following An Interrupted Task ***
; C                     To show the code that you realize the task was
; C                     interrupted and that you want to continue, you
; C                     must take appropriate action and reset INFO(1) = 1
; C         If
; C             IDID = -1, the code has attempted 500 steps.
; C                     If you want to continue, set INFO(1) = 1 and
; C                     call the code again. An additional 500 steps
; C                     will be allowed.
; C
; C             IDID = -2, the error tolerances RTOL, ATOL have been
; C                     increased to values the code estimates appropriate
; C                     for continuing.  You may want to change them
; C                     yourself.  If you are sure you want to continue
; C                     with relaxed error tolerances, set INFO(1)=1 and
; C                     call the code again.
; C
; C             IDID = -3, a solution component is zero and you set the
; C                     corresponding component of ATOL to zero.  If you
; C                     are sure you want to continue, you must first
; C                     alter the error criterion to use positive values
; C                     for those components of ATOL corresponding to zero
; C                     solution components, then set INFO(1)=1 and call
; C                     the code again.
; C
; C             IDID = -4, the problem appears to be stiff.  It is very
; C                     inefficient to solve such problems with DDEABM.
; C                     The code DDEBDF in DEPAC handles this task
; C                     efficiently.  If you are absolutely sure you want
; C                     to continue with DDEABM, set INFO(1)=1 and call
; C                     the code again.
; C
; C             IDID = -5,-6,-7,..,-32  --- cannot occur with this code
; C                     but used by other members of DEPAC or possible
; C                     future extensions.
; C
; C                         *** Following A Terminated Task ***
; C         If
; C             IDID = -33, you cannot continue the solution of this
; C                     problem.  An attempt to do so will result in your
; C                     run being terminated.
; C
; C **********************************************************************
; C *Long Description:
; C
; C **********************************************************************
; C *             DEPAC Package Overview           *
; C **********************************************************************
; C
; C ....   You have a choice of three differential equation solvers from
; C ....   DEPAC. The following brief descriptions are meant to aid you in
; C ....   choosing the most appropriate code for your problem.
; C
; C ....   DDERKF is a fifth order Runge-Kutta code. It is the simplest of
; C ....   the three choices, both algorithmically and in the use of the
; C ....   code. DDERKF is primarily designed to solve non-stiff and
; C ....   mildly stiff differential equations when derivative evaluations
; C ....   are not expensive. It should generally not be used to get high
; C ....   accuracy results nor answers at a great many specific points.
; C ....   Because DDERKF has very low overhead costs, it will usually
; C ....   result in the least expensive integration when solving
; C ....   problems requiring a modest amount of accuracy and having
; C ....   equations that are not costly to evaluate. DDERKF attempts to
; C ....   discover when it is not suitable for the task posed.
; C
; C ....   DDEABM is a variable order (one through twelve) Adams code.
; C ....   Its complexity lies somewhere between that of DDERKF and
; C ....   DDEBDF.  DDEABM is primarily designed to solve non-stiff and
; C ....   mildly stiff differential equations when derivative evaluations
; C ....   are expensive, high accuracy results are needed or answers at
; C ....   many specific points are required. DDEABM attempts to discover
; C ....   when it is not suitable for the task posed.
; C
; C ....   DDEBDF is a variable order (one through five) backward
; C ....   differentiation formula code. it is the most complicated of
; C ....   the three choices. DDEBDF is primarily designed to solve stiff
; C ....   differential equations at crude to moderate tolerances.
; C ....   If the problem is very stiff at all, DDERKF and DDEABM will be
; C ....   quite inefficient compared to DDEBDF. However, DDEBDF will be
; C ....   inefficient compared to DDERKF and DDEABM on non-stiff problems
; C ....   because it uses much more storage, has a much larger overhead,
; C ....   and the low order formulas will not give high accuracies
; C ....   efficiently.
; C
; C ....   The concept of stiffness cannot be described in a few words.
; C ....   If you do not know the problem to be stiff, try either DDERKF
; C ....   or DDEABM. Both of these codes will inform you of stiffness
; C ....   when the cost of solving such problems becomes important.
; C
; C *********************************************************************
; C
; C***REFERENCES  L. F. Shampine and H. A. Watts, DEPAC - design of a user
; C                 oriented package of ODE solvers, Report SAND79-2374,
; C                 Sandia Laboratories, 1979.
; C***ROUTINES CALLED  DDEABM_DDES, XERMSG
; C***REVISION HISTORY  (YYMMDD)
; C   820301  DATE WRITTEN
; C   890531  Changed all specific intrinsics to generic.  (WRB)
; C   890831  Modified array declarations.  (WRB)
; C   891006  Cosmetic changes to prologue.  (WRB)
; C   891024  Changed references from DVNORM to DHVNRM.  (WRB)
; C   891024  REVISION DATE from Version 3.2
; C   891214  Prologue converted to Version 4.0 format.  (BAB)
; C   900510  Convert XERRWV calls to XERMSG calls.  (RWC)
; C   920501  Reformatted the REFERENCES section.  (WRB)
; C***END PROLOGUE  DDEABM
; C
;       INTEGER IALPHA, IBETA, IDELSN, IDID, IFOURU, IG, IHOLD,
;      1      INFO, IP, IPAR, IPHI, IPSI, ISIG, ITOLD, ITSTAR, ITWOU,
;      2      IV, IW, IWORK, IWT, IYP, IYPOUT, IYY, LIW, LRW, NEQ
;       DOUBLE PRECISION ATOL, RPAR, RTOL, RWORK, T, TOUT, Y
;       LOGICAL START,PHASE1,NORND,STIFF,INTOUT
; C
;       DIMENSION Y(*),INFO(15),RTOL(*),ATOL(*),RWORK(*),IWORK(*),
;      1          RPAR(*),IPAR(*)
; C
;       CHARACTER*8 XERN1
;       CHARACTER*16 XERN3
; C
;       EXTERNAL DF
; C
; C     CHECK FOR AN APPARENT INFINITE LOOP
; C

;; -----------  SOI start of IDL code ----------

pro ddeabm_dummy
  common ddeabm_func_common, ddeabm_nfev, ddeabm_funcerror
end

function ddeabm_func0n, func, a, y, private, _extra=fa
  common ddeabm_func_common, nfev, error
  nfev = nfev + 1
  dydx = call_function(func, a, y)
  if min(finite(dydx)) EQ 0 then error = -16
  return, dydx
end

function ddeabm_func1n, func, a, y, private, _extra=fa
  common ddeabm_func_common, nfev, error
  nfev = nfev + 1
  dydx = call_function(func, a, y, private)
  if min(finite(dydx)) EQ 0 then error = -16
  return, dydx
end

function ddeabm_func0e, func, a, y, private, _extra=fa
  common ddeabm_func_common, nfev, error
  nfev = nfev + 1
  dydx = call_function(func, a, y, _extra=fa)
  if min(finite(dydx)) EQ 0 then error = -16
  return, dydx
end

function ddeabm_func1e, func, a, y, private, _extra=fa
  common ddeabm_func_common, nfev, error
  nfev = nfev + 1
  dydx = call_function(func, a, y, private, _extra=fa)
  if min(finite(dydx)) EQ 0 then error = -16
  return, dydx
end

; *DECK DDEABM_DHSTRT
pro ddeabm_dhstrt, DF, NEQ, A, B, Y, YPRIME, ETOL, MORDER, SMALL, $
          BIG, SPY, PV, YP, SF, PRIVATE, FA, H, DFNAME
; C***BEGIN PROLOGUE  DDEABM_DHSTRT
; C***SUBSIDIARY
; C***PURPOSE  Subsidiary to DDEABM, DDEBDF and DDERKF
; C***LIBRARY   SLATEC
; C***TYPE      DOUBLE PRECISION (HSTART-S, DHSTRT-D)
; C***AUTHOR  Watts, H. A., (SNLA)
; C***DESCRIPTION
; C
; C   DDEABM_DHSTRT computes a starting step size to be used in solving initial
; C   value problems in ordinary differential equations.
; C
; C **********************************************************************
; C  ABSTRACT
; C
; C     Subroutine DDEABM_DHSTRT computes a starting step size to be used by an
; C     initial value method in solving ordinary differential equations.
; C     It is based on an estimate of the local Lipschitz constant for the
; C     differential equation   (lower bound on a norm of the Jacobian) ,
; C     a bound on the differential equation  (first derivative) , and
; C     a bound on the partial derivative of the equation with respect to
; C     the independent variable.
; C     (all approximated near the initial point A)
; C
; C     Subroutine DDEABM_DHSTRT uses a function subprogram DHVNRM for computing
; C     a vector norm. The maximum norm is presently utilized though it
; C     can easily be replaced by any other vector norm. It is presumed
; C     that any replacement norm routine would be carefully coded to
; C     prevent unnecessary underflows or overflows from occurring, and
; C     also, would not alter the vector or number of components.
; C
; C **********************************************************************
; C  On input you must provide the following
; C
; C      DF -- This is a subroutine of the form
; C                               DF(X,U,UPRIME,RPAR,IPAR)
; C             which defines the system of first order differential
; C             equations to be solved. For the given values of X and the
; C             vector  U(*)=(U(1),U(2),...,U(NEQ)) , the subroutine must
; C             evaluate the NEQ components of the system of differential
; C             equations  DU/DX=DF(X,U)  and store the derivatives in the
; C             array UPRIME(*), that is,  UPRIME(I) = * DU(I)/DX *  for
; C             equations I=1,...,NEQ.
; C
; C             Subroutine DF must not alter X or U(*). You must declare
; C             the name DF in an external statement in your program that
; C             calls DDEABM_DHSTRT. You must dimension U and UPRIME in DF.
; C
; C             RPAR and IPAR are DOUBLE PRECISION and INTEGER parameter
; C             arrays which you can use for communication between your
; C             program and subroutine DF. They are not used or altered by
; C             DDEABM_DHSTRT. If you do not need RPAR or IPAR, ignore these
; C             parameters by treating them as dummy arguments. If you do
; C             choose to use them, dimension them in your program and in
; C             DF as arrays of appropriate length.
; C
; C      NEQ -- This is the number of (first order) differential equations
; C             to be integrated.
; C
; C      A -- This is the initial point of integration.
; C
; C      B -- This is a value of the independent variable used to define
; C             the direction of integration. A reasonable choice is to
; C             set  B  to the first point at which a solution is desired.
; C             You can also use  B, if necessary, to restrict the length
; C             of the first integration step because the algorithm will
; C             not compute a starting step length which is bigger than
; C             ABS(B-A), unless  B  has been chosen too close to  A.
; C             (it is presumed that DDEABM_DHSTRT has been called with  B
; C             different from  A  on the machine being used. Also see the
; C             discussion about the parameter  SMALL.)
; C
; C      Y(*) -- This is the vector of initial values of the NEQ solution
; C             components at the initial point  A.
; C
; C      YPRIME(*) -- This is the vector of derivatives of the NEQ
; C             solution components at the initial point  A.
; C             (defined by the differential equations in subroutine DF)
; C
; C      ETOL -- This is the vector of error tolerances corresponding to
; C             the NEQ solution components. It is assumed that all
; C             elements are positive. Following the first integration
; C             step, the tolerances are expected to be used by the
; C             integrator in an error test which roughly requires that
; C                        ABS(LOCAL ERROR)  .LE.  ETOL
; C             for each vector component.
; C
; C      MORDER -- This is the order of the formula which will be used by
; C             the initial value method for taking the first integration
; C             step.
; C
; C      SMALL -- This is a small positive machine dependent constant
; C             which is used for protecting against computations with
; C             numbers which are too small relative to the precision of
; C             floating point arithmetic.  SMALL  should be set to
; C             (approximately) the smallest positive DOUBLE PRECISION
; C             number such that  (1.+SMALL) .GT. 1.  on the machine being
; C             used. The quantity  SMALL**(3/8)  is used in computing
; C             increments of variables for approximating derivatives by
; C             differences.  Also the algorithm will not compute a
; C             starting step length which is smaller than
; C             100*SMALL*ABS(A).
; C
; C      BIG -- This is a large positive machine dependent constant which
; C             is used for preventing machine overflows. A reasonable
; C             choice is to set big to (approximately) the square root of
; C             the largest DOUBLE PRECISION number which can be held in
; C             the machine.
; C
; C      SPY(*),PV(*),YP(*),SF(*) -- These are DOUBLE PRECISION work
; C             arrays of length NEQ which provide the routine with needed
; C             storage space.
; C
; C      RPAR,IPAR -- These are parameter arrays, of DOUBLE PRECISION and
; C             INTEGER type, respectively, which can be used for
; C             communication between your program and the DF subroutine.
; C             They are not used or altered by DDEABM_DHSTRT.
; C
; C **********************************************************************
; C  On Output  (after the return from DDEABM_DHSTRT),
; C
; C      H -- is an appropriate starting step size to be attempted by the
; C             differential equation method.
; C
; C           All parameters in the call list remain unchanged except for
; C           the working arrays SPY(*),PV(*),YP(*), and SF(*).
; C
; C **********************************************************************
; C
; C***SEE ALSO  DDEABM, DDEBDF, DDERKF
; C***ROUTINES CALLED  DHVNRM
; C***REVISION HISTORY  (YYMMDD)
; C   820301  DATE WRITTEN
; C   890531  Changed all specific intrinsics to generic.  (WRB)
; C   890831  Modified array declarations.  (WRB)
; C   890911  Removed unnecessary intrinsics.  (WRB)
; C   891024  Changed references from DVNORM to DHVNRM.  (WRB)
; C   891214  Prologue converted to Version 4.0 format.  (BAB)
; C   900328  Added TYPE section.  (WRB)
; C   910722  Updated AUTHOR section.  (ALS)
; C***END PROLOGUE  DDEABM_DHSTRT
; C
;       INTEGER IPAR, J, K, LK, MORDER, NEQ
;       DOUBLE PRECISION A, ABSDX, B, BIG, DA, DELF, DELY,
;      1      DFDUB, DFDXB, DHVNRM,
;      2      DX, DY, ETOL, FBND, H, PV, RELPER, RPAR, SF, SMALL, SPY,
;      3      SRYDPB, TOLEXP, TOLMIN, TOLP, TOLSUM, Y, YDPB, YP, YPRIME
;       DIMENSION Y(*),YPRIME(*),ETOL(*),SPY(*),PV(*),YP(*),
;      1          SF(*),RPAR(*),IPAR(*)
;       EXTERNAL DF
; C
; C     ..................................................................
; C
; C     BEGIN BLOCK PERMITTING ...EXITS TO 160
; C***FIRST EXECUTABLE STATEMENT  DDEABM_DHSTRT
         common ddeabm_func_common
         DX = B - A
         ABSDX = ABS(DX)
         RELPER = SMALL^0.375D0
; C
; C        ...............................................................
; C
; C             COMPUTE AN APPROXIMATE BOUND (DFDXB) ON THE PARTIAL
; C             DERIVATIVE OF THE EQUATION WITH RESPECT TO THE
; C             INDEPENDENT VARIABLE. PROTECT AGAINST AN OVERFLOW.
; C             ALSO COMPUTE A BOUND (FBND) ON THE FIRST DERIVATIVE
; C             LOCALLY.
; C
         DA = MAX([MIN([RELPER*ABS(A),ABSDX]), 100.0D0*SMALL*ABS(A)])
         DA = (DX GE 0)?(+DA):(-DA)
         IF (DA EQ 0.0D0) THEN DA = RELPER*DX

         SF = CALL_FUNCTION(DFNAME, DF, A+DA, Y, PRIVATE, _EXTRA=FA)
         if ddeabm_funcerror NE 0 then return

         YP = SF - YPRIME
         DELF = max(abs(YP))
         DFDXB = BIG
         IF (DELF LT BIG*ABS(DA)) THEN DFDXB = DELF/ABS(DA)
         FBND = max(abs(SF))
; C
; C        ...............................................................
; C
; C             COMPUTE AN ESTIMATE (DFDUB) OF THE LOCAL LIPSCHITZ
; C             CONSTANT FOR THE SYSTEM OF DIFFERENTIAL EQUATIONS. THIS
; C             ALSO REPRESENTS AN ESTIMATE OF THE NORM OF THE JACOBIAN
; C             LOCALLY.  THREE ITERATIONS (TWO WHEN NEQ=1) ARE USED TO
; C             ESTIMATE THE LIPSCHITZ CONSTANT BY NUMERICAL DIFFERENCES.
; C             THE FIRST PERTURBATION VECTOR IS BASED ON THE INITIAL
; C             DERIVATIVES AND DIRECTION OF INTEGRATION. THE SECOND
; C             PERTURBATION VECTOR IS FORMED USING ANOTHER EVALUATION OF
; C             THE DIFFERENTIAL EQUATION.  THE THIRD PERTURBATION VECTOR
; C             IS FORMED USING PERTURBATIONS BASED ONLY ON THE INITIAL
; C             VALUES. COMPONENTS THAT ARE ZERO ARE ALWAYS CHANGED TO
; C             NON-ZERO VALUES (EXCEPT ON THE FIRST ITERATION). WHEN
; C             INFORMATION IS AVAILABLE, CARE IS TAKEN TO ENSURE THAT
; C             COMPONENTS OF THE PERTURBATION VECTOR HAVE SIGNS WHICH ARE
; C             CONSISTENT WITH THE SLOPES OF LOCAL SOLUTION CURVES.
; C             ALSO CHOOSE THE LARGEST BOUND (FBND) FOR THE FIRST
; C             DERIVATIVE.
; C
; C                               PERTURBATION VECTOR SIZE IS HELD
; C                               CONSTANT FOR ALL ITERATIONS. COMPUTE
; C                               THIS CHANGE FROM THE
; C                                       SIZE OF THE VECTOR OF INITIAL
; C                                       VALUES.
         DELY = RELPER*max(abs(y))
         IF (DELY EQ 0.0D0) THEN DELY = RELPER
         DELY = (DX GE 0)?(+DELY):(-DELY)
         DELF = max(abs(YPRIME))
         FBND = MAX([FBND,DELF])
         IF (DELF NE 0.0D0) THEN BEGIN
; C           USE INITIAL DERIVATIVES FOR FIRST PERTURBATION
             SPY = YPRIME
             YP = YPRIME
         ENDIF ELSE BEGIN
; C           CANNOT HAVE A NULL PERTURBATION VECTOR
             SPY(*) = 0
             YP(*) = 1
             DELF = max(abs(yp))
         ENDELSE
; C
         DFDUB = 0.0D0
         LK = MIN([NEQ+1,3])
         FOR K = 1L, LK DO BEGIN

; C           DEFINE PERTURBED VECTOR OF INITIAL VALUES
             PV = Y + DELY*(YP/DELF)
             IF (K NE 2) THEN BEGIN
; C              EVALUATE DERIVATIVES ASSOCIATED WITH PERTURBED
; C              VECTOR  AND  COMPUTE CORRESPONDING DIFFERENCES

                 YP = CALL_FUNCTION(DFNAME, DF, A, PV, PRIVATE, _EXTRA=FA)
                 if ddeabm_funcerror NE 0 then return
                 PV = YP - YPRIME
            ENDIF ELSE BEGIN
; C              USE A SHIFTED VALUE OF THE INDEPENDENT VARIABLE
; C                                    IN COMPUTING ONE ESTIMATE

                YP = CALL_FUNCTION(DFNAME, DF, A+DA, PV, PRIVATE, _EXTRA=FA)
                if ddeabm_funcerror NE 0 then return
                PV = YP - SF
            ENDELSE
; C           CHOOSE LARGEST BOUNDS ON THE FIRST DERIVATIVE
; C                          AND A LOCAL LIPSCHITZ CONSTANT
            FBND = MAX([FBND,max(abs(yp))])
            DELF = max(abs(pv))
; C        ...EXIT
            IF (DELF GE BIG*ABS(DELY)) THEN GOTO, DHSTRT_150
            DFDUB = MAX([DFDUB,DELF/ABS(DELY)])
; C     ......EXIT
            IF (K EQ LK) THEN GOTO, DHSTRT_160
; C           CHOOSE NEXT PERTURBATION VECTOR
            IF (DELF EQ 0.0D0) THEN DELF = 1.0D0
            IF (K NE 2) THEN BEGIN
                DY = ABS(PV)
                wh = where(dy EQ 0, ct)
                if ct GT 0 then dy(wh) = DELF
            endif else begin
                DY = Y
                wh = where(dy EQ 0, ct)
                if ct GT 0 then dy(wh) = DELY/RELPER
            endelse
            wh = where(spy EQ 0, ct)
            if ct GT 0 then spy(wh) = yp(wh)
            wh = where(spy LT 0, ct)
            if ct GT 0 then dy(wh) = -dy(wh)
            yp(*) = dy
            DELF = max(abs(YP))
        ENDFOR
        DHSTRT_150:
; C
; C        PROTECT AGAINST AN OVERFLOW
        DFDUB = BIG
        DHSTRT_160:
; C
; C     ..................................................................
; C
; C          COMPUTE A BOUND (YDPB) ON THE NORM OF THE SECOND DERIVATIVE
; C
      YDPB = DFDXB + DFDUB*FBND
; C
; C     ..................................................................
; C
; C          DEFINE THE TOLERANCE PARAMETER UPON WHICH THE STARTING STEP
; C          SIZE IS TO BE BASED.  A VALUE IN THE MIDDLE OF THE ERROR
; C          TOLERANCE RANGE IS SELECTED.
; C
      TOLEXP = ALOG10(ETOL)
      TOLMIN = MIN(TOLEXP)
      TOLSUM = TOTAL(TOLEXP)
      TOLP = 10.0D0^(0.5D0*(TOLSUM/NEQ + TOLMIN)/(MORDER+1))
; C
; C     ..................................................................
; C
; C          COMPUTE A STARTING STEP SIZE BASED ON THE ABOVE FIRST AND
; C          SECOND DERIVATIVE INFORMATION
; C
; C                            RESTRICT THE STEP LENGTH TO BE NOT BIGGER
; C                            THAN ABS(B-A).   (UNLESS  B  IS TOO CLOSE
; C                            TO  A)
      H = ABSDX
; C
      IF (YDPB EQ 0.0D0 AND FBND EQ 0.0D0) THEN BEGIN
; GO TO 180
; C
; C        BOTH FIRST DERIVATIVE TERM (FBND) AND SECOND
; C                     DERIVATIVE TERM (YDPB) ARE ZERO
         IF (TOLP LT 1.0D0) THEN H = ABSDX*TOLP
     ENDIF ELSE IF (YDPB EQ 0.0D0) THEN BEGIN
; C        ONLY SECOND DERIVATIVE TERM (YDPB) IS ZERO
         IF (TOLP LT FBND*ABSDX) THEN H = TOLP/FBND
     ENDIF ELSE BEGIN
; C        SECOND DERIVATIVE TERM (YDPB) IS NON-ZERO
         SRYDPB = SQRT(0.5D0*YDPB)
         IF (TOLP LT SRYDPB*ABSDX) THEN H = TOLP/SRYDPB
     ENDELSE

; C     FURTHER RESTRICT THE STEP LENGTH TO BE NOT
; C                               BIGGER THAN  1/DFDUB
      IF (H*DFDUB GT 1.0D0) THEN H = 1.0D0/DFDUB

; C     FINALLY, RESTRICT THE STEP LENGTH TO BE NOT
; C     SMALLER THAN  100*SMALL*ABS(A).  HOWEVER, IF
; C     A=0. AND THE COMPUTED H UNDERFLOWED TO ZERO,
; C     THE ALGORITHM RETURNS  SMALL*ABS(B)  FOR THE
; C                                     STEP LENGTH.
      H = MAX([H,100.0D0*SMALL*ABS(A)])
      IF (H EQ 0.0D0) THEN H = SMALL*ABS(B)

; C     NOW SET DIRECTION OF INTEGRATION
      H = (DX GE 0)?(+H):(-H)

      RETURN
  END





; *DECK DDEABM_DDES

      pro ddeabm_ddes, DF, NEQ, T, Y, TOUT, INFO, RTOL, ATOL, IDID, $
        YPOUT, YP, YY, WT, P, PHI, ALPHA, BETA, PSI, V, W, SIG, G, GI, $
        H, EPS, X, XOLD, HOLD, TOLD, DELSGN, TSTOP, TWOU, FOURU, START, $
        PHASE1, NORND, STIFF, INTOUT, NS, KORD, KOLD, INIT, KSTEPS, $
        KLE4, IQUIT, KPREV, IVC, IV, KGI, PRIVATE, FA, dfname, $
                errmsg=errmsg, max_stepsize=max_stepsize
; C***BEGIN PROLOGUE  DDEABM_DDES
; C***SUBSIDIARY
; C***PURPOSE  Subsidiary to DDEABM
; C***LIBRARY   SLATEC
; C***TYPE      DOUBLE PRECISION (DES-S, DDES-D)
; C***AUTHOR  Watts, H. A., (SNLA)
; C***DESCRIPTION
; C
; C   DDEABM merely allocates storage for DDEABM_DDES to relieve the user of the
; C   inconvenience of a long call list.  Consequently  DDEABM_DDES  is used as
; C   described in the comments for  DDEABM .
; C
; C***SEE ALSO  DDEABM
; C***ROUTINES CALLED  D1MACH, DDEABM_DINTP, DDEABM_DSTEPS, XERMSG
; C***REVISION HISTORY  (YYMMDD)
; C   820301  DATE WRITTEN
; C   890531  Changed all specific intrinsics to generic.  (WRB)
; C   890831  Modified array declarations.  (WRB)
; C   891214  Prologue converted to Version 4.0 format.  (BAB)
; C   900328  Added TYPE section.  (WRB)
; C   900510  Convert XERRWV calls to XERMSG calls, cvt GOTOs to
; C           IF-THEN-ELSE.  (RWC)
; C   910722  Updated AUTHOR section.  (ALS)
; C***END PROLOGUE  DDEABM_DDES
; C
;       INTEGER IDID, INFO, INIT, IPAR, IQUIT, IV, IVC, K, KGI, KLE4,
;      1      KOLD, KORD, KPREV, KSTEPS, L, LTOL, MAXNUM, NATOLP, NEQ,
;      2      NRTOLP, NS
;       DOUBLE PRECISION A, ABSDEL, ALPHA, ATOL, BETA, D1MACH,
;      1      DEL, DELSGN, DT, EPS, FOURU, G, GI, H,
;      2      HA, HOLD, P, PHI, PSI, RPAR, RTOL, SIG, T, TOLD, TOUT,
;      3      TSTOP, TWOU, U, V, W, WT, X, XOLD, Y, YP, YPOUT, YY
;       LOGICAL STIFF,CRASH,START,PHASE1,NORND,INTOUT
; C
;      DIMENSION Y(*),YY(*),WT(*),PHI(NEQ,16),P(*),YP(*),
;     1  YPOUT(*),PSI(12),ALPHA(12),BETA(12),SIG(13),V(12),W(12),G(13),
;     2  GI(11),IV(10),INFO(15),RTOL(*),ATOL(*),RPAR(*),IPAR(*)
;      CHARACTER*8 XERN1
;      CHARACTER*16 XERN3, XERN4
; C
;       EXTERNAL DF
; C
; C.......................................................................
; C
; C  THE EXPENSE OF SOLVING THE PROBLEM IS MONITORED BY COUNTING THE
; C  NUMBER OF  STEPS ATTEMPTED. WHEN THIS EXCEEDS  MAXNUM, THE COUNTER
; C  IS RESET TO ZERO AND THE USER IS INFORMED ABOUT POSSIBLE EXCESSIVE
; C  WORK.
; C
      MAXNUM = 500L
; C
; C.......................................................................
; C
; C***FIRST EXECUTABLE STATEMENT  DDEABM_DDES
      common ddeabm_func_common
      IF (INFO(1-1) EQ 0) THEN BEGIN
; C
; C ON THE FIRST CALL , PERFORM INITIALIZATION --
; C        DEFINE THE MACHINE UNIT ROUNDOFF QUANTITY  U  BY CALLING THE
; C        FUNCTION ROUTINE  D1MACH. THE USER MUST MAKE SURE THAT THE
; C        VALUES SET IN D1MACH ARE RELEVANT TO THE COMPUTER BEING USED.
; C
         U=(machar(/double)).eps  ;; XXX
; C                       -- SET ASSOCIATED MACHINE DEPENDENT PARAMETERS
         TWOU=2.D0*U
         FOURU=4.D0*U
; C                       -- SET TERMINATION FLAG
         IQUIT=0L
; C                       -- SET INITIALIZATION INDICATOR
         INIT=0L
; C                       -- SET COUNTER FOR ATTEMPTED STEPS
         KSTEPS=0L
; C                       -- SET INDICATOR FOR INTERMEDIATE-OUTPUT
         INTOUT= 0L
; C                       -- SET INDICATOR FOR STIFFNESS DETECTION
         STIFF= 0L
; C                       -- SET STEP COUNTER FOR STIFFNESS DETECTION
         KLE4=0L
; C                       -- SET INDICATORS FOR STEPS CODE
         START= 1L
         PHASE1= 1L
         NORND= 1L
; C                       -- RESET INFO(1) FOR SUBSEQUENT CALLS
         INFO(1-1)=1L
      ENDIF
; C
; C.......................................................................
; C
; C      CHECK VALIDITY OF INPUT PARAMETERS ON EACH ENTRY
; C
      IF (INFO(1-1) NE 0  AND  INFO(1-1) NE 1) THEN BEGIN
          errmsg = 'IN DDEABM, INFO(1-1) MUST BE '+ $
            'SET TO 0 FOR THE START OF A NEW PROBLEM, AND MUST BE '+ $
            'SET TO 1 FOLLOWING AN INTERRUPTED TASK.  YOU ARE '+ $
            'ATTEMPTING TO CONTINUE THE INTEGRATION ILLEGALLY BY '+ $
            'CALLING THE CODE WITH INFO(1-1) = ' + strtrim(info(1-1),2)
          IDID=-33L
      ENDIF

      IF (INFO(2-1) NE 0  AND  INFO(2-1) NE 1) THEN BEGIN
          errmsg = 'IN DDEABM, INFO(2-1) MUST BE '+ $
            '0 OR 1 INDICATING SCALAR AND VECTOR ERROR TOLERANCES, '+ $
            'RESPECTIVELY.  YOU HAVE CALLED THE CODE WITH INFO(2-1) = '+ $
            strtrim(info(2-1),2)
         IDID=-33L
      ENDIF

      IF (INFO(3-1) NE 0  AND  INFO(3-1) NE 1) THEN BEGIN
          errmsg = 'IN DDEABM, INFO(3-1) MUST BE '+ $
            '0 OR 1 INDICATING THE INTERVAL OR INTERMEDIATE-OUTPUT '+ $
            'MODE OF INTEGRATION, RESPECTIVELY.  YOU HAVE CALLED '+ $
            'THE CODE WITH  INFO(3-1) = '+strtrim(info(3-1),2)
         IDID=-33L
      ENDIF

      IF (INFO(4-1) NE 0  AND  INFO(4-1) NE 1) THEN BEGIN
         errmsg = 'IN DDEABM, INFO(4-1) MUST BE '+ $
           '0 OR 1 INDICATING WHETHER OR NOT THE INTEGRATION '+ $
           'INTERVAL IS TO BE RESTRICTED BY A POINT TSTOP.  YOU '+ $
           'HAVE CALLED THE CODE WITH INFO(4-1) = '+strtrim(info(4-1),2)
         IDID=-33L
      ENDIF

      IF (NEQ LT 1) THEN BEGIN
         errmsg = 'IN DDEABM,  THE NUMBER OF '+ $
           'EQUATIONS NEQ MUST BE A POSITIVE INTEGER.  YOU HAVE '+ $
           'CALLED THE CODE WITH  NEQ = '+strtrim(neq,2)
         IDID=-33L
      ENDIF

      whr = where(rtol LT 0, nrtolp)
      wha = where(atol LT 0, natolp)

      NRTOLP = total( (RTOL LT 0) ) NE 0
      NATOLP = total( (ATOL LT 0) ) NE 0

      if nrtolp NE 0 then begin
          errmsg = 'IN DDEABM, THE RELATIVE '+ $
            'ERROR TOLERANCES RTOL MUST BE NON-NEGATIVE.  YOU '+ $
            'HAVE CALLED THE CODE WITH  RTOL('+strtrim(whr(0),2)+') = '+ $
            strtrim(rtol(whr(0)),2)+ $
            '. IN THE CASE OF VECTOR ERROR TOLERANCES, '+$
            'NO FURTHER CHECKING OF RTOL COMPONENTS IS DONE.'
          IDID = -33L
      endif else if natolp NE 0 then begin
          errmsg = 'IN DDEABM, THE ABSOLUTE '+ $
            'ERROR TOLERANCES ATOL MUST BE NON-NEGATIVE.  YOU '+ $
            'HAVE CALLED THE CODE WITH  ATOL('+strtrim(wha(0),2)+'-1) = '+ $
            strtrim(atol(wha(0)),2)+ $
            '.  IN THE CASE OF VECTOR ERROR TOLERANCES, '+ $
            'NO FURTHER CHECKING OF ATOL COMPONENTS IS DONE.'
          IDID = -33L
      ENDIF

     DDES_100:
     IF (INFO(4-1) EQ 1) THEN BEGIN
         IF ( (TOUT-T)*(TSTOP-T) LT 0 $
              OR ABS(TOUT-T) GT ABS(TSTOP-T)) THEN BEGIN
            errmsg = 'IN DDEABM, YOU HAVE '+ $
              'CALLED THE CODE WITH  TOUT = '+strtrim(tout,2)+' BUT '+ $
              'YOU HAVE ALSO TOLD THE CODE (INFO(4-1) = 1) NOT TO '+ $
              'INTEGRATE PAST THE POINT TSTOP = '+strtrim(tstop,2)+ $
              ' THESE INSTRUCTIONS CONFLICT.'
            IDID=-33L
         ENDIF
      ENDIF
; C
; C     CHECK SOME CONTINUATION POSSIBILITIES
; C
      IF (INIT NE 0) THEN BEGIN
         IF (T EQ TOUT) THEN BEGIN
            errmsg = 'IN DDEABM, YOU HAVE '+ $
              'CALLED THE CODE WITH  T = TOUT = '+strtrim(T,2)+ $
              '.  THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
            IDID=-33L
         ENDIF

         IF (T NE TOLD) THEN BEGIN
            errmsg = 'IN DDEABM, YOU HAVE '+ $
              'CHANGED THE VALUE OF T FROM '+strtrim(told,2)+' TO '+ $
              strtrim(t,2)+'  THIS IS NOT ALLOWED ON CONTINUATION CALLS.'
            IDID=-33L
         ENDIF

         IF (INIT NE 1) THEN BEGIN
            IF (DELSGN*(TOUT-T) LT 0.D0) THEN BEGIN
               errmsg = 'IN DDEABM, BY '+ $
                 'CALLING THE CODE WITH TOUT = '+strtrim(tout,2)+ $
                 ' YOU ARE ATTEMPTING TO CHANGE THE DIRECTION OF '+ $
                 'INTEGRATION.  THIS IS NOT ALLOWED WITHOUT '+ $
                 'RESTARTING.'
               IDID=-33L
            ENDIF
         ENDIF
      ENDIF
; C
; C     INVALID INPUT DETECTED
; C
      IF (IDID EQ (-33)) THEN BEGIN
         IF (IQUIT NE (-33)) THEN BEGIN
            IQUIT = -33L
            INFO(1-1) = -1L
         ENDIF ELSE BEGIN
             errmsg = 'IN DDEABM, INVALID '+ $
               'INPUT WAS DETECTED ON SUCCESSIVE ENTRIES.  IT IS '+ $
               'IMPOSSIBLE TO PROCEED BECAUSE YOU HAVE NOT '+ $
               'CORRECTED THE PROBLEM, SO EXECUTION IS BEING '+ $
               'TERMINATED.'
         ENDELSE
         RETURN
      ENDIF
; C
; C.......................................................................
; C
; C     RTOL = ATOL = 0. IS ALLOWED AS VALID INPUT AND INTERPRETED AS
; C     ASKING FOR THE MOST ACCURATE SOLUTION POSSIBLE. IN THIS CASE,
; C     THE RELATIVE ERROR TOLERANCE RTOL IS RESET TO THE SMALLEST VALUE
; C     FOURU WHICH IS LIKELY TO BE REASONABLE FOR THIS METHOD AND MACHINE
; C
      wh = where(rtol+atol EQ 0, ct)
      if ct GT 0 then begin
          ;; Expand RTOL if necessary to be per-vector
          if n_elements(rtol) LT n_elements(atol) then $
            rtol = rtol + atol*0
          rtol(wh) = fouru
          IDID = -2L
      endif

      DDES_190:
      IF (IDID EQ (-2)) THEN BEGIN
; C                       RTOL=ATOL=0 ON INPUT, SO RTOL IS CHANGED TO A
; C                                                SMALL POSITIVE VALUE
          INFO(1-1)=-1
          RETURN
      ENDIF
; C
; C     BRANCH ON STATUS OF INITIALIZATION INDICATOR
; C            INIT=0 MEANS INITIAL DERIVATIVES AND NOMINAL STEP SIZE
; C                   AND DIRECTION NOT YET SET
; C            INIT=1 MEANS NOMINAL STEP SIZE AND DIRECTION NOT YET SET
; C            INIT=2 MEANS NO FURTHER INITIALIZATION REQUIRED
; C
      IF (INIT EQ 0) THEN GOTO, DDES_210
      IF (INIT EQ 1) THEN GOTO, DDES_220
      GOTO, DDES_240
; C
; C.......................................................................
; C
; C     MORE INITIALIZATION --
; C                         -- EVALUATE INITIAL DERIVATIVES
; C
      DDES_210:
      INIT=1L
      A=T
      YP = CALL_FUNCTION(DFNAME, DF, A, Y, PRIVATE, _EXTRA=FA)
      if ddeabm_funcerror NE 0 then return
      

      IF (T EQ TOUT) THEN BEGIN
          IDID=2L
          YPOUT = YP
          TOLD=T
          RETURN
      ENDIF
; C
; C                         -- SET INDEPENDENT AND DEPENDENT VARIABLES
; C                                              X AND YY(*) FOR STEPS
; C                         -- SET SIGN OF INTEGRATION DIRECTION
; C                         -- INITIALIZE THE STEP SIZE
; C
      DDES_220:
      INIT = 2L
      X = T
      YY = Y
      DELSGN = (TOUT GE T)?(+1):(-1)
      DELSGNX = (TOUT GE X)?(+1):(-1)
      H = MAX([FOURU*ABS(X),ABS(TOUT-X)])
      if n_elements(max_stepsize) GT 0 then h = h < max_stepsize(0)
      H = H * DELSGNX
; C
; C.......................................................................
; C
; C   ON EACH CALL SET INFORMATION WHICH DETERMINES THE ALLOWED INTERVAL
; C   OF INTEGRATION BEFORE RETURNING WITH AN ANSWER AT TOUT
; C
      DDES_240:
      DEL = TOUT - T
      ABSDEL = ABS(DEL)
; C
; C.......................................................................
; C
; C   IF ALREADY PAST OUTPUT POINT, INTERPOLATE AND RETURN
; C
      DDES_250:
      IF (ABS(X-T) GE ABSDEL) THEN BEGIN
          DDEABM_DINTP, X,YY,TOUT,Y,YPOUT,NEQ,KOLD,PHI,IVC,IV,KGI,GI, $
                                             ALPHA,G,W,XOLD,P
          IDID = 3L
          IF (X EQ TOUT) THEN BEGIN
              IDID = 2L
              INTOUT = 0L
          ENDIF
          T = TOUT
          TOLD = T
          RETURN
      ENDIF
; C
; C   IF CANNOT GO PAST TSTOP AND SUFFICIENTLY CLOSE,
; C   EXTRAPOLATE AND RETURN
; C
      IF ((INFO(4-1) EQ 1) AND $
          (ABS(TSTOP-X) LT FOURU*ABS(X))) THEN BEGIN
          DT = TOUT - X
          Y = YY + DT*YP

          YPOUT = CALL_FUNCTION(DFNAME, DF, TOUT, Y, PRIVATE, _EXTRA=FA)
          if ddeabm_funcerror NE 0 then return
          IDID = 3L
          T = TOUT
          TOLD = T
          RETURN
      ENDIF

      IF (INFO(3-1) EQ 0  OR  INTOUT EQ 0) EQ 0 THEN BEGIN
; C
; C   INTERMEDIATE-OUTPUT MODE
; C
          IDID = 1L
          Y = YY
          YPOUT = YP
          T = X
          TOLD = T
          INTOUT = 0L
          RETURN
      ENDIF
; C
; C.......................................................................
; C
; C     MONITOR NUMBER OF STEPS ATTEMPTED
; C
      IF (KSTEPS GT MAXNUM) THEN BEGIN
; C
; C                       A SIGNIFICANT AMOUNT OF WORK HAS BEEN EXPENDED
          IDID=-1L
          KSTEPS=0L
          IF (STIFF) THEN BEGIN
; C
; C                       PROBLEM APPEARS TO BE STIFF
              IDID=-4L
              STIFF= 0L
              KLE4=0L
          ENDIF

          Y = YY
          YPOUT = YP
          T = X
          TOLD = T
          INFO(1-1) = -1
          INTOUT = 0L
          RETURN
      ENDIF
; C
; C.......................................................................
; C
; C   LIMIT STEP SIZE, SET WEIGHT VECTOR AND TAKE A STEP
; C
      HA = ABS(H)
      IF (INFO(4-1) EQ 1) THEN BEGIN
          HA = MIN([HA,ABS(TSTOP-X)])
      ENDIF
      H = (H GE 0)?(HA):(-HA)
      EPS = 1.0D0
      LTOL = 1L

      wt = rtol*abs(yy) + atol
      wh = where(wt LE 0, ct)

; C
; C                       RELATIVE ERROR CRITERION INAPPROPRIATE
      if ct GT 0 then begin
          IDID = -3L
          Y = YY
          YPOUT = YP
          T = X
          TOLD = T
          INFO(1-1) = -1L
          INTOUT = 0L
          RETURN
      endif

      DDES_380:
      DDEABM_DSTEPS, DF,NEQ,YY,X,H,EPS,WT,START,HOLD,KORD,KOLD,CRASH,PHI,P, $
                YP,PSI,ALPHA,BETA,SIG,V,W,G,PHASE1,NS,NORND,KSTEPS, $
                TWOU,FOURU,XOLD,KPREV,IVC,IV,KGI,GI,PRIVATE,FA, DFNAME, $
                max_stepsize=max_stepsize
      if ddeabm_funcerror NE 0 then return

; C
; C.......................................................................
; C
      IF (CRASH) THEN BEGIN
; C
; C                       TOLERANCES TOO SMALL
          IDID = -2L
          RTOL = EPS*RTOL
          ATOL = EPS*ATOL
          Y = YY
          YPOUT = YP
          T = X
          TOLD = T
          INFO(1-1) = -1L
          INTOUT = 0L
          RETURN
      ENDIF
; C
; C   (STIFFNESS TEST) COUNT NUMBER OF CONSECUTIVE STEPS TAKEN WITH THE
; C   ORDER OF THE METHOD BEING LESS OR EQUAL TO FOUR
; C
  DDES_420:
      KLE4 = KLE4 + 1
      IF(KOLD GT 4) THEN KLE4 = 0L
      IF(KLE4 GE 50) THEN STIFF = 1L
      INTOUT = 1L
      GOTO, DDES_250
    END


; *DECK DDEABM_DINTP
pro DDEABM_DINTP, X, Y, XOUT, YOUT, YPOUT, NEQN, KOLD, PHI, IVC, $
       IV, KGI, GI, ALPHA, OG, OW, OX, OY
; C***BEGIN PROLOGUE  DDEABM_DINTP
; C***PURPOSE  Approximate the solution at XOUT by evaluating the
; C            polynomial computed in DDEABM_DSTEPS at XOUT.  Must be used in
; C            conjunction with DDEABM_DSTEPS.
; C***LIBRARY   SLATEC (DEPAC)
; C***CATEGORY  I1A1B
; C***TYPE      DOUBLE PRECISION (SINTRP-S, DINTP-D)
; C***KEYWORDS  ADAMS METHOD, DEPAC, INITIAL VALUE PROBLEMS, ODE,
; C             ORDINARY DIFFERENTIAL EQUATIONS, PREDICTOR-CORRECTOR,
; C             SMOOTH INTERPOLANT
; C***AUTHOR  Watts, H. A., (SNLA)
; C***DESCRIPTION
; C
; C   The methods in subroutine  DDEABM_DSTEPS  approximate the solution near  X
; C   by a polynomial.  Subroutine  DDEABM_DINTP  approximates the solution at
; C   XOUT  by evaluating the polynomial there.  Information defining this
; C   polynomial is passed from  DDEABM_DSTEPS  so  DDEABM_DINTP  cannot be used alone.
; C
; C   Subroutine DDEABM_DSTEPS is completely explained and documented in the text
; C   "Computer Solution of Ordinary Differential Equations, the Initial
; C   Value Problem"  by L. F. Shampine and M. K. Gordon.
; C
; C   Input to DDEABM_DINTP --
; C
; C   The user provides storage in the calling program for the arrays in
; C   the call list
; C      DIMENSION Y(NEQN),YOUT(NEQN),YPOUT(NEQN),PHI(NEQN,16),OY(NEQN)
; C                AND ALPHA(12),OG(13),OW(12),GI(11),IV(10)
; C   and defines
; C      XOUT -- point at which solution is desired.
; C   The remaining parameters are defined in  DDEABM_DSTEPS  and passed to
; C   DDEABM_DINTP  from that subroutine
; C
; C   Output from  DDEABM_DINTP --
; C
; C      YOUT(*) -- solution at  XOUT
; C      YPOUT(*) -- derivative of solution at  XOUT
; C   The remaining parameters are returned unaltered from their input
; C   values.  Integration with  DDEABM_DSTEPS  may be continued.
; C
; C***REFERENCES  H. A. Watts, A smoother interpolant for DE/STEP, INTRP
; C                 II, Report SAND84-0293, Sandia Laboratories, 1984.
; C***ROUTINES CALLED  (NONE)
; C***REVISION HISTORY  (YYMMDD)
; C   840201  DATE WRITTEN
; C   890831  Modified array declarations.  (WRB)
; C   890831  REVISION DATE from Version 3.2
; C   891214  Prologue converted to Version 4.0 format.  (BAB)
; C   920501  Reformatted the REFERENCES section.  (WRB)
; C***END PROLOGUE  DDEABM_DINTP
; C
;       INTEGER I, IQ, IV, IVC, IW, J, JQ, KGI, KOLD, KP1, KP2,
;      1        L, M, NEQN
;       DOUBLE PRECISION ALP, ALPHA, C, G, GDI, GDIF, GI, GAMMA, H, HI,
;      1       HMU, OG, OW, OX, OY, PHI, RMU, SIGMA, TEMP1, TEMP2, TEMP3,
;      2       W, X, XI, XIM1, XIQ, XOUT, Y, YOUT, YPOUT
; C
;       DIMENSION Y(*),YOUT(*),YPOUT(*),PHI(NEQN,16),OY(*)
;       DIMENSION G(13),C(13),W(13),OG(13),OW(12),ALPHA(12),GI(11),IV(10)
; C
; C***FIRST EXECUTABLE STATEMENT  DDEABM_DINTP
      KP1 = KOLD + 1
      KP2 = KOLD + 2

      HI = XOUT - OX
      H = X - OX
      XI = HI/H
      XIM1 = XI - 1.D0

      G = DBLARR(13) & C = G & W = G
; C
; C   INITIALIZE W(*) FOR COMPUTING G(*)
; C
      XIQ = XI
      IQ = dindgen(kp1)+1
      XIQ = XI^(IQ+1)
      W(0:KP1-1) = XIQ/(IQ*(IQ+1))

; C
; C   COMPUTE THE DOUBLE INTEGRAL TERM GDI
; C
      IF (KOLD LE KGI) THEN BEGIN
          GDI = GI(KOLD-1)
          GOTO, DINTP_60
      ENDIF
          
      IF (IVC LE 0) THEN BEGIN
          GDI = 1.0D0/(KP1*(KP1+1))
          M = 2L
      ENDIF ELSE BEGIN
          IW = IV(IVC-1)
          GDI = OW(IW-1)
          M = KOLD - IW + 3
      ENDELSE

      IF (M LE KOLD) THEN BEGIN
;;        XXX:    (M>1) is a kludge
          FOR I = (M>1), KOLD DO $
            GDI = OW(KP2-I-1) - ALPHA(I-1)*GDI
      ENDIF

; C
; C   COMPUTE G(*) AND C(*)
; C
      DINTP_60:
      G(0:1) = [XI, XI^2/2]
      C(0:1) = [1d, XI]
      IF (KOLD GE 2) THEN BEGIN
          FOR I = 2L, KOLD DO BEGIN
              ALP = ALPHA(I-1)
              GAMMA = 1.0D0 + XIM1*ALP
              L = KP2 - I
              W(0:L-1) = GAMMA*W(0:L-1) - ALP*W(1:L)
              G(I) = W(0)
              C(I) = GAMMA*C(I-1)
          ENDFOR
      ENDIF
; C
; C   DEFINE INTERPOLATION PARAMETERS
; C
      SIGMA = (W(1) - XIM1*W(0))/GDI
      RMU = XIM1*C(KOLD)/GDI  ;; *** NOTE: KP1-1 is KOLD
      HMU = RMU/H

; C   INTERPOLATE FOR THE SOLUTION -- YOUT
; C   AND FOR THE DERIVATIVE OF THE SOLUTION -- YPOUT

      YOUT(*) = 0
      YPOUT(*) = 0
      J = lindgen(KOLD)+1
      I = KP2 - J - 1 ;; *** NOTE: -1 is here
      GDIF = OG(I) - OG(I-1)
      TEMP2 = (G(I) - G(I-1)) - SIGMA*GDIF
      TEMP3 = (C(I) - C(I-1)) + RMU*GDIF

      FOR J = 0L, KOLD-1 DO BEGIN
        YOUT  = YOUT  + TEMP2(j)*PHI(*,I(j))
        YPOUT = YPOUT + TEMP3(j)*PHI(*,I(j))
      ENDFOR

      YOUT = ( ((1.0D0 - SIGMA)*OY + SIGMA*Y) + $
               H*(YOUT + (G(0) - SIGMA*OG(0))*PHI(*,0)) )
      YPOUT = ( HMU*(OY - Y) + $
                (YPOUT + (C(0) + RMU*OG(0))*PHI(*,0)) )

      RETURN
  END

; *DECK DDEABM_DSTEPS
  pro DDEABM_DSTEPS, DF, NEQN, Y, X, H, EPS, WT, START, HOLD, K, $
              KOLD, CRASH, PHI, P, YP, PSI, ALPHA, BETA, SIG, V, W, G, $
              PHASE1, NS, NORND, KSTEPS, TWOU, FOURU, XOLD, KPREV, IVC, IV, $
              KGI, GI, PRIVATE, FA, dfname, max_stepsize=max_stepsize
; C***BEGIN PROLOGUE  DDEABM_DSTEPS
; C***PURPOSE  Integrate a system of first order ordinary differential
; C            equations one step.
; C***LIBRARY   SLATEC (DEPAC)
; C***CATEGORY  I1A1B
; C***TYPE      DOUBLE PRECISION (STEPS-S, DSTEPS-D)
; C***KEYWORDS  ADAMS METHOD, DEPAC, INITIAL VALUE PROBLEMS, ODE,
; C             ORDINARY DIFFERENTIAL EQUATIONS, PREDICTOR-CORRECTOR
; C***AUTHOR  Shampine, L. F., (SNLA)
; C           Gordon, M. K., (SNLA)
; C             MODIFIED BY H.A. WATTS
; C***DESCRIPTION
; C
; C   Written by L. F. Shampine and M. K. Gordon
; C
; C   Abstract
; C
; C   Subroutine  DDEABM_DSTEPS  is normally used indirectly through subroutine
; C   DDEABM .  Because  DDEABM  suffices for most problems and is much
; C   easier to use, using it should be considered before using  DDEABM_DSTEPS
; C   alone.
; C
; C   Subroutine DDEABM_DSTEPS integrates a system of  NEQN  first order ordinary
; C   differential equations one step, normally from X to X+H, using a
; C   modified divided difference form of the Adams Pece formulas.  Local
; C   extrapolation is used to improve absolute stability and accuracy.
; C   The code adjusts its order and step size to control the local error
; C   per unit step in a generalized sense.  Special devices are included
; C   to control roundoff error and to detect when the user is requesting
; C   too much accuracy.
; C
; C   This code is completely explained and documented in the text,
; C   Computer Solution of Ordinary Differential Equations, The Initial
; C   Value Problem  by L. F. Shampine and M. K. Gordon.
; C   Further details on use of this code are available in "Solving
; C   Ordinary Differential Equations with ODE, STEP, and INTRP",
; C   by L. F. Shampine and M. K. Gordon, SLA-73-1060.
; C
; C
; C   The parameters represent --
; C      DF -- subroutine to evaluate derivatives
; C      NEQN -- number of equations to be integrated
; C      Y(*) -- solution vector at X
; C      X -- independent variable
; C      H -- appropriate step size for next step.  Normally determined by
; C           code
; C      EPS -- local error tolerance
; C      WT(*) -- vector of weights for error criterion
; C      START -- logical variable set .TRUE. for first step,  .FALSE.
; C           otherwise
; C      HOLD -- step size used for last successful step
; C      K -- appropriate order for next step (determined by code)
; C      KOLD -- order used for last successful step
; C      CRASH -- logical variable set .TRUE. when no step can be taken,
; C           .FALSE. otherwise.
; C      YP(*) -- derivative of solution vector at  X  after successful
; C           step
; C      KSTEPS -- counter on attempted steps
; C      TWOU -- 2.*U where U is machine unit roundoff quantity
; C      FOURU -- 4.*U where U is machine unit roundoff quantity
; C      RPAR,IPAR -- parameter arrays which you may choose to use
; C            for communication between your program and subroutine F.
; C            They are not altered or used by DDEABM_DSTEPS.
; C   The variables X,XOLD,KOLD,KGI and IVC and the arrays Y,PHI,ALPHA,G,
; C   W,P,IV and GI are required for the interpolation subroutine SINTRP.
; C   The remaining variables and arrays are included in the call list
; C   only to eliminate local retention of variables between calls.
; C
; C   Input to DDEABM_DSTEPS
; C
; C      First call --
; C
; C   The user must provide storage in his calling program for all arrays
; C   in the call list, namely
; C
; C     DIMENSION Y(NEQN),WT(NEQN),PHI(NEQN,16),P(NEQN),YP(NEQN),PSI(12),
; C    1  ALPHA(12),BETA(12),SIG(13),V(12),W(12),G(13),GI(11),IV(10),
; C    2  RPAR(*),IPAR(*)
; C
; C    **Note**
; C
; C   The user must also declare  START ,  CRASH ,  PHASE1  and  NORND
; C   logical variables and  DF  an EXTERNAL subroutine, supply the
; C   subroutine  DF(X,Y,YP)  to evaluate
; C      DY(I)/DX = YP(I) = DF(X,Y(1),Y(2),...,Y(NEQN))
; C   and initialize only the following parameters.
; C      NEQN -- number of equations to be integrated
; C      Y(*) -- vector of initial values of dependent variables
; C      X -- initial value of the independent variable
; C      H -- nominal step size indicating direction of integration
; C           and maximum size of step.  Must be variable
; C      EPS -- local error tolerance per step.  Must be variable
; C      WT(*) -- vector of non-zero weights for error criterion
; C      START -- .TRUE.
; C      YP(*) -- vector of initial derivative values
; C      KSTEPS -- set KSTEPS to zero
; C      TWOU -- 2.*U where U is machine unit roundoff quantity
; C      FOURU -- 4.*U where U is machine unit roundoff quantity
; C   Define U to be the machine unit roundoff quantity by calling
; C   the function routine  D1MACH,  U = D1MACH(4), or by
; C   computing U so that U is the smallest positive number such
; C   that 1.0+U .GT. 1.0.
; C
; C   DDEABM_DSTEPS  requires that the L2 norm of the vector with components
; C   LOCAL ERROR(L)/WT(L)  be less than  EPS  for a successful step.  The
; C   array  WT  allows the user to specify an error test appropriate
; C   for his problem.  For example,
; C      WT(L) = 1.0  specifies absolute error,
; C            = ABS(Y(L))  error relative to the most recent value of the
; C                 L-th component of the solution,
; C            = ABS(YP(L))  error relative to the most recent value of
; C                 the L-th component of the derivative,
; C            = MAX(WT(L),ABS(Y(L)))  error relative to the largest
; C                 magnitude of L-th component obtained so far,
; C            = ABS(Y(L))*RELERR/EPS + ABSERR/EPS  specifies a mixed
; C                 relative-absolute test where  RELERR  is relative
; C                 error,  ABSERR  is absolute error and  EPS =
; C                 MAX(RELERR,ABSERR) .
; C
; C      Subsequent calls --
; C
; C   Subroutine  DDEABM_DSTEPS  is designed so that all information needed to
; C   continue the integration, including the step size  H  and the order
; C   K , is returned with each step.  With the exception of the step
; C   size, the error tolerance, and the weights, none of the parameters
; C   should be altered.  The array  WT  must be updated after each step
; C   to maintain relative error tests like those above.  Normally the
; C   integration is continued just beyond the desired endpoint and the
; C   solution interpolated there with subroutine  SINTRP .  If it is
; C   impossible to integrate beyond the endpoint, the step size may be
; C   reduced to hit the endpoint since the code will not take a step
; C   larger than the  H  input.  Changing the direction of integration,
; C   i.e., the sign of  H , requires the user set  START = .TRUE. before
; C   calling  DDEABM_DSTEPS  again.  This is the only situation in which  START
; C   should be altered.
; C
; C   Output from DDEABM_DSTEPS
; C
; C      Successful Step --
; C
; C   The subroutine returns after each successful step with  START  and
; C   CRASH  set .FALSE. .  X  represents the independent variable
; C   advanced one step of length  HOLD  from its value on input and  Y
; C   the solution vector at the new value of  X .  All other parameters
; C   represent information corresponding to the new  X  needed to
; C   continue the integration.
; C
; C      Unsuccessful Step --
; C
; C   When the error tolerance is too small for the machine precision,
; C   the subroutine returns without taking a step and  CRASH = .TRUE. .
; C   An appropriate step size and error tolerance for continuing are
; C   estimated and all other information is restored as upon input
; C   before returning.  To continue with the larger tolerance, the user
; C   just calls the code again.  A restart is neither required nor
; C   desirable.
; C
; C***REFERENCES  L. F. Shampine and M. K. Gordon, Solving ordinary
; C                 differential equations with ODE, STEP, and INTRP,
; C                 Report SLA-73-1060, Sandia Laboratories, 1973.
; C***ROUTINES CALLED  D1MACH, DDEABM_DHSTRT
; C***REVISION HISTORY  (YYMMDD)
; C   740101  DATE WRITTEN
; C   890531  Changed all specific intrinsics to generic.  (WRB)
; C   890831  Modified array declarations.  (WRB)
; C   890831  REVISION DATE from Version 3.2
; C   891214  Prologue converted to Version 4.0 format.  (BAB)
; C   920501  Reformatted the REFERENCES section.  (WRB)
; C***END PROLOGUE  DDEABM_DSTEPS
; C
;       INTEGER I, IFAIL, IM1, IP1, IPAR, IQ, J, K, KM1, KM2, KNEW,
;      1      KOLD, KP1, KP2, KSTEPS, L, LIMIT1, LIMIT2, NEQN, NS, NSM2,
;      2      NSP1, NSP2
;       DOUBLE PRECISION ABSH, ALPHA, BETA, BIG, D1MACH,
;      1      EPS, ERK, ERKM1, ERKM2, ERKP1, ERR,
;      2      FOURU, G, GI, GSTR, H, HNEW, HOLD, P, P5EPS, PHI, PSI, R,
;      3      REALI, REALNS, RHO, ROUND, RPAR, SIG, TAU, TEMP1,
;      4      TEMP2, TEMP3, TEMP4, TEMP5, TEMP6, TWO, TWOU, U, V, W, WT,
;      5      X, XOLD, Y, YP
;       LOGICAL START,CRASH,PHASE1,NORND
;       DIMENSION Y(*),WT(*),PHI(NEQN,16),P(*),YP(*),PSI(12),
;      1  ALPHA(12),BETA(12),SIG(13),V(12),W(12),G(13),GI(11),IV(10),
;      2  RPAR(*),IPAR(*)
;       DIMENSION TWO(13),GSTR(13)
;       EXTERNAL DF
;       SAVE TWO, GSTR

;       DATA TWO(1),TWO(2),TWO(3),TWO(4),TWO(5),TWO(6),TWO(7),TWO(8),
;      1     TWO(9),TWO(10),TWO(11),TWO(12),TWO(13)
;      2     /2.0D0,4.0D0,8.0D0,16.0D0,32.0D0,64.0D0,128.0D0,256.0D0,
;      3      512.0D0,1024.0D0,2048.0D0,4096.0D0,8192.0D0/
;       DATA GSTR(1),GSTR(2),GSTR(3),GSTR(4),GSTR(5),GSTR(6),GSTR(7),
;      1     GSTR(8),GSTR(9),GSTR(10),GSTR(11),GSTR(12),GSTR(13)
;      2     /0.5D0,0.0833D0,0.0417D0,0.0264D0,0.0188D0,0.0143D0,0.0114D0,
;      3      0.00936D0,0.00789D0,0.00679D0,0.00592D0,0.00524D0,0.00468D0/

    common ddeabm_func_common
    TWO = 2d^(dindgen(13)+1)
    GSTR = [ 0.5D0,0.0833D0,0.0417D0,0.0264D0,0.0188D0,0.0143D0,0.0114D0, $
             0.00936D0,0.00789D0,0.00679D0,0.00592D0,0.00524D0,0.00468D0 ]

; C
; C       ***     BEGIN BLOCK 0     ***
; C   CHECK IF STEP SIZE OR ERROR TOLERANCE IS TOO SMALL FOR MACHINE
; C   PRECISION.  IF FIRST STEP, INITIALIZE PHI ARRAY AND ESTIMATE A
; C   STARTING STEP SIZE.
; C                   ***
; C
; C   IF STEP SIZE IS TOO SMALL, DETERMINE AN ACCEPTABLE ONE
; C
; C***FIRST EXECUTABLE STATEMENT  DDEABM_DSTEPS
      CRASH = 1L
      IF (ABS(H) LT FOURU*ABS(X)) THEN BEGIN
          H = (FOURU*ABS(X)) * ( (H GE 0)?(+1):(-1) )
          RETURN
      ENDIF

      P5EPS = 0.5D0*EPS
; C
; C   IF ERROR TOLERANCE IS TOO SMALL, INCREASE IT TO AN ACCEPTABLE VALUE
; C
      ROUND = TOTAL( (Y/WT)^2 )
      ROUND = TWOU*SQRT(ROUND)

      IF (P5EPS LT ROUND) THEN BEGIN
          EPS = 2.0D0*ROUND*(1.0D0 + FOURU)
          RETURN
      ENDIF

      CRASH = 0L
      G(0) = 1.0D0
      G(1) = 0.5D0
      SIG(0) = 1.0D0
      IF (NOT START) THEN GOTO, DSTEPS_99
; C
; C   INITIALIZE.  COMPUTE APPROPRIATE STEP SIZE FOR FIRST STEP
; C
; C     CALL DF(X,Y,YP,RPAR,IPAR)
; C     SUM = 0.0
      PHI(*,0) = YP
      PHI(*,1) = 0
; C20     SUM = SUM + (YP(L-1)/WT(L-1))**2
; C     SUM = SQRT(SUM)
; C     ABSH = ABS(H)
; C     IF(EPS .LT. 16.0*SUM*H*H) ABSH = 0.25*SQRT(EPS/SUM)
; C     H = SIGN(MAX(ABSH,FOURU*ABS(X)),H)
; C
      U = (machar(/double)).eps          ;; XXX
      BIG = SQRT((machar(/double)).xmax) ;; XXX


      ;; Save and restore values from PHI
      phi3 = phi(*,2) & phi4 = phi(*,3)
      phi5 = phi(*,4) & phi6 = phi(*,5)
      DDEABM_DHSTRT, DF,NEQN,X,X+H,Y,YP,WT,1,U,BIG, $
        phi3, phi4, phi5, phi6, private, fa, h, dfname
      phi(*,2) = phi3 & phi(*,3) = phi4 
      phi(*,4) = phi5 & phi(*,5) = phi6
      if ddeabm_funcerror NE 0 then return

      HOLD = 0.0D0
      K = 1L
      KOLD = 0L
      KPREV = 0L
      START = 0L
      PHASE1 = 1L
      NORND = 1L
      IF (P5EPS LE 100.0D0*ROUND) THEN BEGIN
          NORND = 0L
          PHI(*,14) = 0
      ENDIF

      DSTEPS_99:
      IFAIL = 0L
; C       ***     END BLOCK 0     ***
; C
; C       ***     BEGIN BLOCK 1     ***
; C   COMPUTE COEFFICIENTS OF FORMULAS FOR THIS STEP.  AVOID COMPUTING
; C   THOSE QUANTITIES NOT CHANGED WHEN STEP SIZE IS NOT CHANGED.
; C                   ***
; C
      DSTEPS_100:
      KP1 = K+1
      KP2 = K+2
      KM1 = K-1
      KM2 = K-2
; C
; C   NS IS THE NUMBER OF DSTEPS TAKEN WITH SIZE H, INCLUDING THE CURRENT
; C   ONE.  WHEN K.LT.NS, NO COEFFICIENTS CHANGE
; C
      IF (H NE HOLD) THEN NS = 0L
      IF (NS LE KOLD) THEN NS = NS+1
      NSP1 = NS+1
      IF (K LT NS) THEN GOTO, DSTEPS_199
; C
; C   COMPUTE THOSE COMPONENTS OF ALPHA(*),BETA(*),PSI(*),SIG(*) WHICH
; C   ARE CHANGED
; C
      BETA(NS-1) = 1.0D0
      ALPHA(NS-1) = 1.0D0/NS
      TEMP1 = H*NS
      SIG(NSP1-1) = 1.0D0
      IF (K GE NSP1) THEN BEGIN
          FOR I = NSP1, K DO BEGIN
              IM1 = I-1-1  ;; *** Note IM1-1 here!
              II = I-1
              TEMP2 = PSI(IM1)
              PSI(IM1) = TEMP1
              BETA(II) = BETA(IM1)*PSI(IM1)/TEMP2
              TEMP1 = TEMP2 + H
              ALPHA(II) = H/TEMP1
              SIG(I) = I*ALPHA(II)*SIG(II)
          ENDFOR
      ENDIF
      PSI(K-1) = TEMP1
; C
; C   COMPUTE COEFFICIENTS G(*)
; C
; C   INITIALIZE V(*) AND SET W(*).
; C
      IF (NS LE 1) THEN BEGIN
          KK = dindgen(K)+1
          V(0:K-1) = 1.0D0/(KK*(KK+1))
          W(0:K-1) = V(0:K-1)
          IVC = 0L
          KGI = 0L
          IF (K EQ 1) THEN GOTO, DSTEPS_140
          KGI = 1L
          GI(0) = W(1)
          GOTO, DSTEPS_140
      ENDIF
; C
; C   IF ORDER WAS RAISED, UPDATE DIAGONAL PART OF V(*)
; C
      IF (K LE KPREV) THEN GOTO, DSTEPS_130
      IF (IVC NE 0) THEN BEGIN 
          JV = KP1 - IV(IVC-1)
          IVC = IVC - 1
      ENDIF ELSE BEGIN
          JV = 1L
          TEMP4 = K*KP1
          V(K-1) = 1.0D0/TEMP4
          W(K-1) = V(K-1)
          IF (K EQ 2) THEN BEGIN
              KGI = 1L
              GI(0) = W(1)
          ENDIF
      ENDELSE
      NSM2 = NS-2
      IF (NSM2 GE JV) THEN BEGIN
          
          FOR J = JV, NSM2 DO BEGIN
              I = K-J-1  ;; *** NOTE: I-1 here!
              V(I) = V(I) - ALPHA(J)*V(I+1)
              W(I) = V(I)
          ENDFOR

          IF (I EQ 2) THEN BEGIN
              KGI = NS - 1
              GI(KGI-1) = W(1)
          ENDIF
      ENDIF
; C
; C   UPDATE V(*) AND SET W(*)
; C
      DSTEPS_130:
      LIMIT1 = KP1 - NS
      TEMP5 = ALPHA(NS-1)
      V(0:LIMIT1-1) = V(0:LIMIT1-1) - TEMP5*V(1:LIMIT1)
      W(0:LIMIT1-1) = V(0:LIMIT1-1)
      G(NSP1-1) = W(0)
      IF (LIMIT1 NE 1) THEN BEGIN
          KGI = NS
          GI(KGI-1) = W(1)
      ENDIF
      W(LIMIT1) = V(LIMIT1)
      IF (K LT KOLD) THEN BEGIN 
          IVC = IVC + 1
          IV(IVC-1) = LIMIT1 + 2
      ENDIF
; C
; C   COMPUTE THE G(*) IN THE WORK VECTOR W(*)
; C
      DSTEPS_140:
      NSP2 = NS + 2
      KPREV = K
      IF (KP1 GE NSP2) THEN BEGIN
          FOR I = NSP2, KP1 DO BEGIN
              LIMIT2 = KP2 - I
              TEMP6 = ALPHA(I-2)
              W(0:LIMIT2-1) = W(0:LIMIT2-1) - TEMP6*W(1:LIMIT2)
              G(I-1) = W(0)
          ENDFOR
      ENDIF

      DSTEPS_199:
; C       ***     END BLOCK 1     ***
; C
; C       ***     BEGIN BLOCK 2     ***
; C   PREDICT A SOLUTION P(*), EVALUATE DERIVATIVES USING PREDICTED
; C   SOLUTION, ESTIMATE LOCAL ERROR AT ORDER K AND ERRORS AT ORDERS K,
; C   K-1, K-2 AS IF CONSTANT STEP SIZE WERE USED.
; C                   ***
; C
; C   INCREMENT COUNTER ON ATTEMPTED DSTEPS
; C
      KSTEPS = KSTEPS + 1
; C
; C   CHANGE PHI TO PHI STAR
; C
      IF (K GE NSP1) THEN BEGIN
          FOR I = NSP1, K DO BEGIN
              TEMP1 = BETA(I-1)
              PHI(*,I-1) = TEMP1*PHI(*,I-1)
          ENDFOR
      ENDIF
; C
; C   PREDICT SOLUTION AND DIFFERENCES
; C
      PHI(*,KP2-1) = PHI(*,KP1-1)
      PHI(*,KP1-1) = 0
      P(*) = 0
      FOR J = 1L, K DO BEGIN
        I = KP1 - J - 1 ;; *** NOTE: I-1 here!
        TEMP2 = G(I)
        P = P + TEMP2*PHI(*,I)
        PHI(*,I) = PHI(*,I) + PHI(*,I+1)
      ENDFOR
      IF NOT (NORND) THEN BEGIN
          TAU = H*P - PHI(*,14)
          P = Y + TAU
          PHI(*,15) = (P - Y) - TAU
      ENDIF ELSE BEGIN
          P = Y + H*P
      ENDELSE
      XOLD = X

      X = X + H
      ABSH = ABS(H)

      YP = CALL_FUNCTION(DFNAME, DF, X, P, PRIVATE, _EXTRA=FA)
      if ddeabm_funcerror NE 0 then return

; C
; C   ESTIMATE ERRORS AT ORDERS K,K-1,K-2
; C
      ERKM2 = 0.0D0
      ERKM1 = 0.0D0
      TEMP3 = 1.0D0/WT
      TEMP4 = YP - PHI(*,0)
      ERK = total((temp4*temp3)^2)
      IF (KM2 GT 0) THEN $
        ERKM2 = TOTAL( ((PHI(*,KM1-1)+TEMP4)*TEMP3)^2 )
      IF (KM2 GE 0) THEN $
        ERKM1 = TOTAL( ((PHI(*,K-1)+TEMP4)*TEMP3)^2 )

      IF (KM2 GT 0) THEN $
        ERKM2 = ABSH*SIG(KM1-1)*GSTR(KM2-1)*SQRT(ERKM2)
      IF (KM2 GE 0) THEN $
        ERKM1 = ABSH*SIG(K-1)*GSTR(KM1-1)*SQRT(ERKM1)
      TEMP5 = ABSH*SQRT(ERK)
      ERR = TEMP5*(G(K-1)-G(KP1-1))
      ERK = TEMP5*SIG(KP1-1)*GSTR(K-1)
      KNEW = K
; C
; C   TEST IF ORDER SHOULD BE LOWERED
; C
      IF (KM2 GT 0) THEN BEGIN
          IF(MAX([ERKM1,ERKM2]) LE ERK) THEN KNEW = KM1
      ENDIF ELSE IF (KM2 EQ 0) THEN BEGIN
          IF(ERKM1 LE 0.5D0*ERK) THEN KNEW = KM1
      ENDIF
; C
; C   TEST IF STEP SUCCESSFUL
; C
      IF (ERR LE EPS) THEN GOTO, DSTEPS_400
; C       ***     END BLOCK 2     ***
; C
; C       ***     BEGIN BLOCK 3     ***
; C   THE STEP IS UNSUCCESSFUL.  RESTORE  X, PHI(*,*), PSI(*) .
; C   IF THIRD CONSECUTIVE FAILURE, SET ORDER TO ONE.  IF STEP FAILS MORE
; C   THAN THREE TIMES, CONSIDER AN OPTIMAL STEP SIZE.  DOUBLE ERROR
; C   TOLERANCE AND RETURN IF ESTIMATED STEP SIZE IS TOO SMALL FOR MACHINE
; C   PRECISION.
; C                   ***
; C
; C   RESTORE X, PHI(*,*) AND PSI(*)
; C
      PHASE1 = 0L
      X = XOLD
      FOR I = 0L, K-1 DO BEGIN
        PHI(*,I) = (PHI(*,I) - PHI(*,I+1))/BETA(I)
      ENDFOR
      IF (K GE 2) THEN BEGIN 
          PSI(0:K-2) = PSI(1:K-1) - H
      ENDIF

; C
; C   ON THIRD FAILURE, SET ORDER TO ONE.  THEREAFTER, USE OPTIMAL STEP
; C   SIZE
; C
      IFAIL = IFAIL + 1
      TEMP2 = 0.5D0
      IF (IFAIL - 3 GT 0) THEN BEGIN
          IF (P5EPS LT 0.25D0*ERK) THEN TEMP2 = SQRT(P5EPS/ERK)
      ENDIF
      IF (IFAIL - 3 GE 0) THEN KNEW = 1L
      H = TEMP2*H
      K = KNEW
      NS = 0L
      IF (ABS(H) LT FOURU*ABS(X)) THEN BEGIN
          CRASH = 1L
          H = (FOURU*ABS(X))*( (H GE 0)?(+1):(-1) )
          EPS = EPS + EPS
          RETURN
      ENDIF

      GOTO, DSTEPS_100
; C       ***     END BLOCK 3     ***
; C
; C       ***     BEGIN BLOCK 4     ***
; C   THE STEP IS SUCCESSFUL.  CORRECT THE PREDICTED SOLUTION, EVALUATE
; C   THE DERIVATIVES USING THE CORRECTED SOLUTION AND UPDATE THE
; C   DIFFERENCES.  DETERMINE BEST ORDER AND STEP SIZE FOR NEXT STEP.
; C                   ***
      DSTEPS_400:
      KOLD = K
      HOLD = H
; C
; C   CORRECT AND EVALUATE
; C
      TEMP1 = H*G(KP1-1)
      IF NOT (NORND) THEN BEGIN
          
          TEMP3 = Y
          RHO = TEMP1*(YP - PHI(*,0)) - PHI(*,15)
          Y = P + RHO
          PHI(*,14) = (Y - P) - RHO
          P = TEMP3
      ENDIF ELSE BEGIN
          TEMP3 = Y
          Y = P + TEMP1*(YP - PHI(*,0))
          P = TEMP3
      ENDELSE

      YP = CALL_FUNCTION(DFNAME, DF, X, Y, PRIVATE, _EXTRA=FA)
      if ddeabm_funcerror NE 0 then return


; C
; C   UPDATE DIFFERENCES FOR NEXT STEP
; C
      PHI(*,KP1-1) = YP - PHI(*,0)
      PHI(*,KP2-1) = PHI(*,KP1-1) - PHI(*,KP2-1)
      FOR I = 0L, K-1 DO BEGIN
          PHI(*,I) = PHI(*,I) + PHI(*,KP1-1)
      ENDFOR
; C
; C   ESTIMATE ERROR AT ORDER K+1 UNLESS:
; C     IN FIRST PHASE WHEN ALWAYS RAISE ORDER,
; C     ALREADY DECIDED TO LOWER ORDER,
; C     STEP SIZE NOT CONSTANT SO ESTIMATE UNRELIABLE
; C
      ERKP1 = 0.0D0
      IF (KNEW EQ KM1  OR  K EQ 12) THEN PHASE1 = 0L
      IF (PHASE1) THEN GOTO, DSTEPS_450
      IF (KNEW EQ KM1) THEN GOTO, DSTEPS_455
      IF (KP1 GT NS) THEN GOTO, DSTEPS_460
      ERKP1 = TOTAL( (PHI(*,KP2-1)/WT)^2 )
      ERKP1 = ABSH*GSTR(KP1-1)*SQRT(ERKP1)
; C
; C   USING ESTIMATED ERROR AT ORDER K+1, DETERMINE APPROPRIATE ORDER
; C   FOR NEXT STEP
; C
      IF (K LE 1) THEN BEGIN
          IF (ERKP1 GE 0.5D0*ERK) THEN GOTO, DSTEPS_460
      ENDIF ELSE BEGIN
          IF (ERKM1 LE MIN([ERK,ERKP1])) THEN GOTO, DSTEPS_455
          IF (ERKP1 GE ERK  OR  K EQ 12) THEN GOTO, DSTEPS_460
      ENDELSE
; C
; C   HERE ERKP1 .LT. ERK .LT. MAX(ERKM1,ERKM2) ELSE ORDER WOULD HAVE
; C   BEEN LOWERED IN BLOCK 2.  THUS ORDER IS TO BE RAISED
; C
; C   RAISE ORDER
; C
      DSTEPS_450:
      K = KP1
      ERK = ERKP1
      GOTO, DSTEPS_460
; C
; C   LOWER ORDER
; C
      DSTEPS_455:
      K = KM1
      ERK = ERKM1
; C
; C   WITH NEW ORDER DETERMINE APPROPRIATE STEP SIZE FOR NEXT STEP
; C
      DSTEPS_460:
      HNEW = H + H
      IF NOT ( (PHASE1) OR $
               (P5EPS GE ERK*TWO(K)) ) THEN BEGIN
          HNEW = H
          IF (P5EPS LT ERK) THEN BEGIN
              TEMP2 = K+1
              R = (P5EPS/ERK)^(1.0D0/TEMP2)
              HNEW = ABSH*MAX([0.5D0,MIN([0.9D0,R])])
              HNEW = MAX([HNEW,FOURU*ABS(X)])
              HNEW = (H GE 0)?(+HNEW):(-HNEW)
          ENDIF
      ENDIF

      if n_elements(max_stepsize) GT 0 then begin
          HNEW = HNEW < max_stepsize(0) > (-max_stepsize(0))
      endif

      H = HNEW
      RETURN
; C       ***     END BLOCK 4     ***
  END


; ------------------------------------------------------------------------

pro DDEABM, DF, T, Y, TOUT0, PRIVATE, FUNCTARGS=fa, STATE=state, $
            CONTROL=control, $
            init=init0, intermediate=intermediate, tstop=TSTOP0, $
            epsrel=RTOL, epsabs=ATOL, status=IDID, $
            TGRID=tgrid, YGRID=ygrid, YPGRID=ypgrid, $
            NGRID=ngrid0, NOUTGRID=nsamp, $
            TIMPULSE=timpulse, YIMPULSE=yimpulse, $
            MAX_STEPSIZE=max_stepsize, $
            NFEV=nfev, errmsg=errmsg, dostatusline=dostatusline

      common ddeabm_func_common, ddeabm_nfev, ddeabm_funcerror

      IDID = -33
      errmsg = ''
      if n_params() EQ 0 then begin
          message, 'USAGE:', /info
          message, '  DDEABM, FUNCNAME, T0, Y0, TOUT, STATE, PRIVATE, '+$
            'FUNCTARGS=fa, INIT=init, [EPSREL=epsrel, EPSABS=epsabs, '+$
            'STATUS=status, /INTERMEDIATE, ...]', /info
          return
      endif

; C***FIRST EXECUTABLE STATEMENT  DDEABM
      NEQ = N_ELEMENTS(Y)

      IF NEQ LT 1 THEN BEGIN
          errmsg = 'The number of equations, NEQ, must be greater than '+$
            'or equal to 1'
          idid = -33L
          RETURN
      ENDIF

      ;; Initialize the number of function evaluations
      ddeabm_nfev = 0L
      nfev = 0L

      ;; Construct the wrapper function to be used
      dfname = 'ddeabm_func'
      dfname = dfname + ((n_elements(private) GT 0)?'1':'0')
      dfname = dfname + ((n_elements(fa) GT 0     )?'e':'n')
      
      ;; If either of the tolerances are undefined, then define with
      ;; default tolerances and with the same number of elements
      if n_elements(rtol) EQ 0 AND n_elements(atol) EQ 0 then begin
          rtol = 1d-4
          atol = 1d-6
      endif else if n_elements(rtol) GT 0 AND n_elements(atol) EQ 0 then begin
          atol = rtol*0d
      endif else if n_elements(atol) GT 0 AND n_elements(rtol) EQ 0 then begin
          rtol = atol*0D
      endif

      ;; Compare to be sure the same number 
      if n_elements(rtol) NE n_elements(atol) $
        OR (n_elements(y) NE n_elements(rtol) AND n_elements(rtol) NE 1) $
        then begin
          errmsg = 'The number of absolute and relative tolerance values '+ $
            'must match the number of equations being solved.'
          idid = -33L
      endif

      ;; Be sure to initialize if there is no state variable
      ;; NOTE: DDEABM uses INIT=0 to mean initialize; INIT=1 means
      ;;       don't initialize, which is the opposite sense from the
      ;;       input keyword.
      userinit = 1-keyword_set(init0)
      if n_elements(state) EQ 0 then userinit = 0L

      ;; Construct the INFO array from keywords
      INFO = [ userinit, n_elements(rtol) GT 1, $
               keyword_set(intermediate), n_elements(tstop0) GT 0]

      ;; Construct the STATE array if this is the first pass
      IF ( INFO(1-1) EQ 0 ) OR N_ELEMENTS(STATE) EQ 0 THEN BEGIN
          STATE = {YPOUT: dblarr(neq), TSTAR: 0D, YP: dblarr(NEQ), $
                   YY: dblarr(NEQ), WT: dblarr(NEQ), P: dblarr(NEQ), $
                   PHI: dblarr(NEQ,16), ALPHA: dblarr(12), BETA: dblarr(12), $
                   PSI: dblarr(12), V: dblarr(12), W: dblarr(12), $
                   SIG: dblarr(13), G: dblarr(13), GI: dblarr(11), $
                   XOLD: 0D, HOLD: 0D, TOLD: 0D, DELSN: 0D, TWOU: 0D, $
                   FOURU: 0D, H: 0D, EPS: 0D, X: 0D, TSTOP: 0D, $
                   START: 0L, PHASE1: 0L, NORND: 0L, STIFF: 0L, $
                   INTOUT: 0L, NS: 0L, KORD: 0L, KOLD: 0L, INTERNAL_INIT: 0L, $
                   KSTEPS: 0L, KLE4: 0L, IQUIT: 0L, KPREV: 0L, IVC: 0L, $
                   IV: lonarr(10), KGI: 0L, NEQ: NEQ, COUNT: 0L}
      ENDIF

      if n_elements(tstop0) GT 0 then $
        state.tstop = tstop0(0)

      IF (STATE.COUNT GE 5) THEN BEGIN
         IF (T EQ STATE.TSTAR) THEN BEGIN
             errmsg = 'AN APPARENT INFINITE LOOP HAS BEEN DETECTED.  '+ $
               'YOU HAVE MADE REPEATED CALLS AT T = '+strtrim(t,2)+ $
               ' AND THE INTEGRATION HAS NOT ADVANCED.  CHECK THE '+ $
               'WAY YOU HAVE SET PARAMETERS FOR THE CALL TO THE '+ $
               'CODE, PARTICULARLY INFO(1-1).'
             RETURN
         ENDIF
     ENDIF

     IF NEQ NE STATE.NEQ THEN BEGIN
         errmsg = 'You have initialized DDEABM with a different number '+$
           'of equations, NEQ, than this call has provided.'
         RETURN
     ENDIF
; C
; C     CHECK LRW AND LIW FOR SUFFICIENT STORAGE ALLOCATION
; C
      IDID=0L
; C
; C     COMPUTE THE INDICES FOR THE ARRAYS TO BE STORED IN THE WORK ARRAY
; C
     YPOUT = STATE.YPOUT
     TSTAR = STATE.TSTAR
     YP    = STATE.YP   
     YY    = STATE.YY   
     WT    = STATE.WT   
     P     = STATE.P    
     PHI   = STATE.PHI  
     ALPHA = STATE.ALPHA
     BETA  = STATE.BETA 
     PSI   = STATE.PSI  
     V     = STATE.V    
     W     = STATE.W    
     SIG   = STATE.SIG  
     G     = STATE.G    
     GI    = STATE.GI   
     XOLD  = STATE.XOLD 
     HOLD  = STATE.HOLD 
     TOLD  = STATE.TOLD 
     DELSN = STATE.DELSN
     TWOU  = STATE.TWOU 
     FOURU = STATE.FOURU

     H     = STATE.H
     EPS   = STATE.EPS
     X     = STATE.X
     TSTOP = STATE.TSTOP


     STATE.TSTAR = T

      IF (INFO(1-1) NE 0) THEN BEGIN
          START = STATE.START
          PHASE1 = STATE.PHASE1
          NORND = STATE.NORND
          STIFF = STATE.STIFF
          INTOUT = STATE.INTOUT
      ENDIF
      
     NS     = STATE.NS
     KORD   = STATE.KORD
     KOLD   = STATE.KOLD
     INTERNAL_INIT   = STATE.INTERNAL_INIT
     KSTEPS = STATE.KSTEPS
     KLE4   = STATE.KLE4
     IQUIT  = STATE.IQUIT
     KPREV  = STATE.KPREV
     IVC    = STATE.IVC
     IV     = STATE.IV
     KGI    = STATE.KGI

     if n_elements(ngrid0) GT 0 then begin
         if NOT keyword_set(intermediate) then begin
             errmsg = 'ERROR: NGRID and /INTERMEDIATE must be specified '+$
               'together'
             return
         endif
         ngrid = round(ngrid0(0))
     endif else begin
         ngrid = n_elements(tout0)
     endelse

     tgrid = dblarr(ngrid)
     ygrid = dblarr(neq, ngrid)
     ypgrid = dblarr(neq, ngrid)
     forward = tout0(0) GT t     ;; 1=FORWARD; 0=BACKWARD

     ki = 1L
     nimpulse = n_elements(timpulse)
     if nimpulse GT 0 then begin
         if nimpulse NE n_elements(yimpulse)/neq then begin
             errmsg = 'ERROR: TIMPULSE and YIMPULSE must have the same '+$
               'number of samples'
             return
         endif

         if forward then begin
             wh = where(timpulse GT tout0(0), ct)
             if ct EQ 0 then ki = nimpulse else ki = min(wh)
         endif else begin
             wh = where(timpulse LT tout0(0), ct)
             if ct EQ 0 then ki = 0L else ki = max(wh)
         endelse

     endif

     ;; Initialize the user function
     if info(1-1) EQ 0 AND keyword_set(control) then begin
         ddeabm_funcerror = call_function(dfname, df, t, $
                                          CONTROL={message: 'INITIALIZE'}, $
                                          y, private, _EXTRA=fa)
         if ddeabm_funcerror LT 0 then begin
             errmsg = 'ERROR: user function failed to initialize'
             goto, FINISH_INTEGRATION
         endif
     endif


     i = 0L   ;; Output grid position counter
     nsamp = 0L
     while (i LT ngrid) do begin
         if keyword_set(dostatusline) then $
           statusline, string(i, ngrid, format='(I8,"/",I8)'), 0, /left
         doimpulse = 0  ;; Signal to process an impulse (0=no; 1=yes; 2=both)
         if keyword_set(intermediate) then begin
             TOUT = TOUT0(0)
         endif else begin
             TOUT = TOUT0(i)

             if (ki GE 0) AND (ki LT nimpulse) then begin
                 if abs(timpulse(ki)-t) LE abs(tout-t) then begin
                     doimpulse = 1
                     if TIMPULSE(ki) EQ TOUT then doimpulse = 2
                     TOUT = TIMPULSE(ki)
                 endif
             endif
         endelse

         ddeabm_funcerror = 0
         DDEABM_DDES, DF,NEQ,T,Y,TOUT,INFO,RTOL,ATOL,IDID,YPOUT, $
           YP,YY,WT,P,PHI, $
           ALPHA,BETA,PSI,V, $
           W,SIG,G,GI,H, $
           EPS,X,XOLD,HOLD, $
           TOLD,DELSN,TSTOP,TWOU, $
           FOURU,START,PHASE1,NORND,STIFF,INTOUT, NS, KORD, KOLD, INTERNAL_INIT, $
           KSTEPS, KLE4, IQUIT, KPREV, IVC, IV, KGI, PRIVATE, FA, dfname, $
           ERRMSG=errmsg, max_stepsize=max_stepsize

         if ddeabm_funcerror NE 0 then begin
             case ddeabm_funcerror of
                 -16:  errmsg = 'ERROR: user function returned non-finite values'
                 else: errmsg = 'ERROR: unknown internal error occurred'
             endcase
             goto, FINISH_INTEGRATION
         endif

         if IDID GT 0 then begin

             ;; === Store the result
             if (doimpulse EQ 0) OR (doimpulse EQ 2) then begin
                 ;; This was not an impulse-only stopping point...
                 ;; Store the result
                 tgrid(i) = T
                 ygrid(*,i) = Y
                 ypgrid(*,i) = YPOUT
                 nsamp = nsamp + 1
                 i = i + 1
             endif

             ;; === Handle any impulse changes
             if (doimpulse GT 0) then begin    
                 ;; Apply an impulse
                 if forward then begin
                     Y = Y + YIMPULSE(*,ki)
                     ki = ki + 1
                 endif else begin
                     Y = Y - YIMPULSE(*,ki)
                     ki = ki - 1
                 endelse

                 ;; Special case: the same TOUT can be listed twice
                 ;;   for before and after an impulse.  In that case
                 ;;   store the same values after we have incremented.
                 if (doimpulse EQ 2) then if (TOUT0(i) EQ TOUT0(i-1)) then begin
                     tgrid(i) = T
                     ygrid(*,i) = Y
                     ;; We have to re-call the function since we
                     ;; crossed the discontinuity.
                     ypgrid(*,i) = call_function(dfname,df, t, y, private,_EXTRA=fa)
                     nsamp = nsamp + 1
                     i = i + 1
                 endif

                 doimpulse = 0
                 info(1-1) = 0L  ;; NOTE that INIT=0 means initialize!!!
             endif

             ;; Reset KSTEPS since we successfully integrated this step
             ksteps = 0L

             ;; End if we reach the stopping point early
             if keyword_set(intermediate) then begin
                 if idid EQ 2 OR idid EQ 3 OR t EQ tout(0) then $
                   goto, FINISH_INTEGRATION
             endif

         endif else begin
             goto, FINISH_INTEGRATION
         endelse

     endwhile

     FINISH_INTEGRATION:

     STATE.YPOUT = YPOUT
     STATE.YP    = YP   
     STATE.YY    = YY   
     STATE.WT    = WT   
     STATE.P     = P    
     STATE.PHI   = PHI  
     STATE.ALPHA = ALPHA
     STATE.BETA  = BETA 
     STATE.PSI   = PSI  
     STATE.V     = V    
     STATE.W     = W    
     STATE.SIG   = SIG  
     STATE.G     = G    
     STATE.GI    = GI   
     STATE.XOLD  = XOLD 
     STATE.HOLD  = HOLD 
     STATE.TOLD  = TOLD 
     STATE.DELSN = DELSN
     STATE.TWOU  = TWOU 
     STATE.FOURU = FOURU
      
     STATE.H     = H
     STATE.EPS   = EPS
     STATE.X     = X
     STATE.TSTOP = TSTOP

     STATE.NS     = NS
     STATE.KORD   = KORD
     STATE.KOLD   = KOLD
     STATE.INTERNAL_INIT   = INTERNAL_INIT
     STATE.KSTEPS = KSTEPS
     STATE.KLE4   = KLE4
     STATE.IQUIT  = IQUIT
     STATE.KPREV  = KPREV
     STATE.IVC    = IVC
     STATE.IV     = IV
     STATE.KGI    = KGI

     STATE.START  = START
     STATE.PHASE1 = PHASE1
     STATE.NORND  = NORND
     STATE.STIFF  = STIFF
     STATE.INTOUT = INTOUT
     NFEV = DDEABM_NFEV

     ;; Pass back INIT to user, and remember to invert the sense
     ;; between the internal variable and the external variable.
     INIT0 = (info(0) EQ 0)
     TSTOP0 = STATE.TSTOP
     ;; XXX what to do about interrupted case where INFO(1-1) is
     ;; negative, and the user must reset it?
     ;; Answer: Add RESUME keyword, and enforce the behavior that
     ;; RESUME must only be set after an interruption.  Must save one
     ;; more variable in STATE with the previous INFO(1-1) value.

      IF (IDID NE (-2)) THEN STATE.COUNT = STATE.COUNT + 1L
      IF (T NE STATE.TSTAR) THEN STATE.COUNT = 0L

      if keyword_set(dostatusline) then begin
          statusline, /close
      endif

      RETURN
      END
