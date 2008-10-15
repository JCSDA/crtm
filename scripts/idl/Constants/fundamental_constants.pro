;+
; NAME:
;       fundamental_constants
;
; PURPOSE:
;       Include file containing various fundamental physical constants.
;
; CALLING SEQUENCE:
;       @fundamental_constants
;
; SIDE EFFECTS:
;       Values can be changed during program execution since there is no way to
;       declare parameters in IDL.
;
; PROCEDURE:
;       The fundamental constants used are taken from the NIST Reference on
;       Constants, Units, and Uncertainty website:
;
;         http://physics.nist.gov/cuu/Constants/
;
;       See also:
;
;         Mohr, P.J. and B.N. Taylor, "CODATA recommended values of the
;           fundamental physical constants: 1998", Reviews of Modern Physics, 
;           Vol.72, No.2, 2000.
;
; CREATION HISTORY
;       Written by:     Paul van Delst, CIMSS/SSEC 27-Sep-1999
;                       paul.vandelst@noaa.gov
;
;-


  ;#----------------------------------------------------------------------------#
  ;#                       -- LOCAL LITERAL CONSTANTS --                        #
  ;#----------------------------------------------------------------------------#

  ONE = 1.0d0
  TWO = 2.0d0



  ;#----------------------------------------------------------------------------#
  ;#                -- IRRATIONAL NUMBERS AND ASSOCIATED BITS --                #
  ;#----------------------------------------------------------------------------#

  ; PI
  PI             = 3.141592653589793238462643d0
  PI_RECIPROCAL  = 0.318309886183790671537767d0
  PI_SQUARED     = 9.869604401089358618834491d0
  PI_SQUARE_ROOT = 1.772453850905516027298167d0
  PI_LN          = 1.144729885849400174143427d0
  PI_LOG10       = 0.497149872694133854351268d0

  ; E
  E              = 2.718281828459045235360287d0
  E_RECIPROCAL   = 0.367879441171442321595523d0
  E_SQUARED      = 7.389056098930650227230427d0
  E_LOG10        = 0.434294481903251827651129d0



  ;#----------------------------------------------------------------------------#
  ;#                            -- UNIVERAL CONSTANTS --                        #
  ;#----------------------------------------------------------------------------#

  ; ----------------------------------------------
  ; Speed of light
  ; Symbol:c,  Units:m/s,  Rel.Uncert.(ppm): exact
  ; ----------------------------------------------
  SPEED_OF_LIGHT = 2.99792458d+08

  ; --------------------------------------------------
  ; Permeability of vacuum
  ; Symbol:mu0,  Units:N/A^2,  Rel.Uncert.(ppm): exact
  ; --------------------------------------------------
  PERMEABILITY = PI * 4.0d-07

  ; -----------------------------------------------------
  ; Permittivity of vacuum
  ; Symbol:epsilon0,  Units:F/m,  Rel.Uncert.(ppm): exact
  ; -----------------------------------------------------
  PERMITTIVITY =                ONE                  / $
  ;              -----------------------------------
                 ( PERMEABILITY * SPEED_OF_LIGHT^2 )

  ; ---------------------------------------------
  ; Planck constant
  ; Symbol:h,  Units:Js,  Rel.Uncert.(ppm): 0.078
  ; ---------------------------------------------
  PLANCK_CONSTANT = 6.62606876d-34

  ; ----------------------------------------------------
  ; Gravitational constant
  ; Symbol:G,  Units:m^3/kg/s^2,  Rel.Uncert.(ppm): 1500
  ; ----------------------------------------------------
  GRAVITATIONAL_CONSTANT = 6.673d-11



  ;#----------------------------------------------------------------------------#
  ;#                          -- CONVERSION FACTORS --                          #
  ;#----------------------------------------------------------------------------#

  ; ---------------------------------------------
  ; Electron volt
  ; Symbol:eV,  Units:J,  Rel.Uncert.(ppm): 0.039
  ; ---------------------------------------------
  ELECTRON_VOLT = 1.602176462d-19   

  ; ---------------------------------------------
  ; Unified atomic mass unit
  ; Symbol:u,  Units:kg,  Rel.Uncert.(ppm): 0.079
  ; ---------------------------------------------
  UNIFIED_ATOMIC_MASS_UNIT = 1.66053873d-27   

  ; ----------------------------------------------
  ; Standard atmosphere
  ; Symbol:P0,  Units:Pa,  Rel.Uncert.(ppm): exact
  ; ----------------------------------------------
  STANDARD_ATMOSPHERE = 101325.0d0

  ; ----------------------------------------------------------------------
  ; Standard temperature
  ; Symbol:T0,  Units:Kelvin,  Rel.Uncert.(ppm): exact
  ;
  ; Note that the unit of thermodynamic temperature, the Kelvin, is the
  ; fraction 1/273.16 of the thermodynamic temperature of the triple point
  ; of water. The standard temperature is the ice point of water, NOT the
  ; triple point, hence the 0.01K difference.
  ; ----------------------------------------------------------------------
  STANDARD_TEMPERATURE = 273.15d0

  ; ------------------------------------------------
  ; Standard gravity
  ; Symbol:g,  Units:m/s^2,  Rel.Uncert.(ppm): exact
  ; ------------------------------------------------
  STANDARD_GRAVITY = 9.80665d0



  ;#----------------------------------------------------------------------------#
  ;#                        -- PHYSICOCHEMICAL CONSTANTS --                     #
  ;#----------------------------------------------------------------------------#

  ; -----------------------------------------------------
  ; Avogadro constant
  ; Symbol:N(A),  Units:mole^-1,  Rel.Uncert.(ppm): 0.079
  ; -----------------------------------------------------
  AVOGADRO_CONSTANT = 6.02214199d+23


  ; -------------------------------------------------
  ; Molar gas constant
  ; Symbol:R,  Units:J/mole/K,  Rel.Uncert.(ppm): 1.7
  ; -------------------------------------------------
  MOLAR_GAS_CONSTANT = 8.314472d0

  ; --------------------------------------------
  ; Boltzmann constant
  ; Symbol:k,  Units:J/K,  Rel.Uncert.(ppm): 1.7
  ;
  ;         R
  ;   k = ------
  ;        N(A)
  ;
  ;     = 1.3806503(24)e-23
  ;
  ; --------------------------------------------
  BOLTZMANN_CONSTANT = MOLAR_GAS_CONSTANT / $
  ;                    ------------------
                       AVOGADRO_CONSTANT

  ; ------------------------------------------------------
  ; Stefan-Boltzmann constant
  ; Symbol:sigma,  Units:W/m^2/K^4,  Rel.Uncert.(ppm): 7.0
  ;
  ;             PI^2
  ;             ----.k^4
  ;              60                     h
  ;   sigma = ------------   ( hbar = ----- )
  ;            hbar^3.c^2              2PI
  ;
  ;         = 5.670400(40)e-08
  ;
  ; I just placed the value here due to the mathematical
  ; gymnastics required to calculate it directly.
  ; ------------------------------------------------------
  STEFAN_BOLTZMANN_CONSTANT = 5.670400d-08

  ; -------------------------------------------------------
  ; First Planck function constant
  ; Symbol:c1,  Units:W.m^2.sr^-1,  Rel.Uncert.(ppm): 0.078
  ;
  ;   c1 = 2.h.c^2
  ;
  ;      = 1.191042722(93)e-16
  ;
  ; -------------------------------------------------------
  C1 = TWO * PLANCK_CONSTANT * SPEED_OF_LIGHT^2

  ; ---------------------------------------------
  ; Second Planck function constant
  ; Symbol:c2,  Units:K.m,  Rel.Uncert.(ppm): 1.7
  ;
  ;         h.c
  ;   c2 = -----
  ;          k
  ;
  ;      = 1.4387752(25)e-02
  ;
  ; ---------------------------------------------
  C2 = PLANCK_CONSTANT * SPEED_OF_LIGHT / $
  ;    ----------------------------------
              BOLTZMANN_CONSTANT

  ; -----------------------------------------------------------------
  ; Molar volume of an ideal gas at standard temperature and pressure
  ; Symbol:Vm,  Units:m^3/mol,  Rel.Uncert.(ppm): 1.7
  ;
  ;         R.T0
  ;   Vm = ------
  ;          P0
  ;
  ;      = 2.2413996(39)e-02
  ;
  ; -----------------------------------------------------------------
  STP_MOLAR_VOLUME = ( MOLAR_GAS_CONSTANT * STANDARD_TEMPERATURE ) / $
  ;                  ---------------------------------------------
                                   STANDARD_ATMOSPHERE

  ; ------------------------------------------------------------------
  ; Loschmidt constant: The number density of one mole of an ideal gas
  ; at standard temperature and pressure
  ; Symbol:n0,  Units:m^-3,  Rel.Uncert.(ppm): 1.7
  ;
  ;         N(A).P0
  ;   n0 = ---------
  ;          R.T0
  ;
  ;         N(A)
  ;      = ------     .....(1)
  ;          Vm
  ;
  ;      = 2.6867775(47)e+25
  ;
  ; Alternatively, using the ideal gas law directly, we know,
  ;
  ;   P.V = n.k.T     .....(2)
  ;
  ; For V = 1m^3 (unit volume), and P = P0, T = T0, then eqn.(2)
  ; becomes,
  ;
  ;   P0 = n0.k.T0
  ;
  ; which rearranges to
  ;
  ;          P0  
  ;   n0 = ------     .....(3)
  ;         k.T0 
  ;
  ; Equation (1) rather than eqn(3) is used here.
  ; ------------------------------------------------------------------
  LOSCHMIDT_CONSTANT = AVOGADRO_CONSTANT / $
  ;                    -----------------
                       STP_MOLAR_VOLUME
