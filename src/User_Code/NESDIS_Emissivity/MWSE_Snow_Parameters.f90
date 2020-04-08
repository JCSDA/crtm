MODULE MWSE_Snow_Parameters


  ! ----------
  ! Module use
  ! ----------

  USE kinds


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! -----------------------------
  ! Everything private by default
  ! -----------------------------

  PRIVATE



  !#----------------------------------------------------------------------------#
  !#                            -- THE SNOW TYPES --                            #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------
  ! The total number of snow types
  ! ** Referred to as "ncand" in old code **
  ! ----------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_SNOW_TYPES = 16


  ! --------------------------------------------
  ! Invalid snow type. AMSU measurements are not
  ! available or over non-snow conditions
  ! ** This is set to a value of 0 in CRTM **
  ! --------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: INVALID_SNOW_TYPE   = -999


  ! ----------------
  ! Valid snow types
  ! ----------------

  INTEGER, PUBLIC, PARAMETER :: WET_SNOW            =  1
  INTEGER, PUBLIC, PARAMETER :: GRASS_AFTER_SNOW    =  2
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_A           =  3
  INTEGER, PUBLIC, PARAMETER :: POWDER_SNOW         =  4
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_B           =  5
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_C           =  6
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_D           =  7
  INTEGER, PUBLIC, PARAMETER :: THIN_CRUST_SNOW     =  8
  INTEGER, PUBLIC, PARAMETER :: RS_SNOW_E           =  9
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_A = 10
  INTEGER, PUBLIC, PARAMETER :: SHALLOW_SNOW        = 11
  INTEGER, PUBLIC, PARAMETER :: DEEP_SNOW           = 12
  INTEGER, PUBLIC, PARAMETER :: CRUST_SNOW          = 13
  INTEGER, PUBLIC, PARAMETER :: MEDIUM_SNOW         = 14
  INTEGER, PUBLIC, PARAMETER :: BOTTOM_CRUST_SNOW_B = 15
  INTEGER, PUBLIC, PARAMETER :: THICK_CRUST_SNOW    = 16


 
  !#----------------------------------------------------------------------------#
  !#                    -- THE NUMBER OF POLARIZATIONS --                       #
  !#----------------------------------------------------------------------------#

  INTEGER, PUBLIC, PARAMETER :: N_POLARIZATIONS = 2


  ! ------------------------------
  ! The valid polarization indices
  ! ------------------------------

  INTEGER, PUBLIC, PARAMETER ::   VERTICAL_POLARIZATION = 1
  INTEGER, PUBLIC, PARAMETER :: HORIZONTAL_POLARIZATION = 2



  !#----------------------------------------------------------------------------#
  !#    -- THE AMSU_A/B CHANNEL NUMBERS AND INDICES USED IN THE ALGORITHMS --   #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------
  ! The number of AMSU-A only window channels used
  ! ** Referred to as "nwcha" in old code **
  ! ----------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_AMSUA_WINDOW_CHANNELS = 4


  ! ----------------------------------------------
  ! The number of AMSU-B only window channels used
  ! ** Referred to as "nwchb" in old code **
  ! ----------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_AMSUB_WINDOW_CHANNELS = 2


  ! ------------------------------------------------
  ! The number of AMSU-A and -B window channels used
  ! ** Referred to as "nwch" in old code **
  ! ------------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_AMSUAB_WINDOW_CHANNELS = 5




  !#----------------------------------------------------------------------------#
  !#                   -- THE DEFAULT EMISSIVITY SPECTRA --                     #
  !#----------------------------------------------------------------------------#

  ! ----------------------------------------------
  ! The number of frequencies at which the default
  ! emissivity spectra are defined.
  ! ** Referred to as "nch" in old code **
  ! ----------------------------------------------

  INTEGER, PUBLIC, PARAMETER :: N_FREQUENCIES = 10


  ! -------------------------------------------------
  ! The frequencies of the default emissivity spectra
  ! ** Referred to as "freq" in old code **
  ! -------------------------------------------------

  REAL( r_kind ), PUBLIC, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    DEFAULT_FREQUENCY = (/  4.9_r_kind,  6.93_r_kind, 10.65_r_kind, 18.7_r_kind,  23.8_r_kind, &
                           31.4_r_kind, 50.3_r_kind,  52.5_r_kind,  89.0_r_kind, 150.0_r_kind /)


  ! ----------------------------------
  ! Each spectrum defined individually
  ! ----------------------------------

  ! -- Wet snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_WET_SNOW = (/ 0.87_r_kind,0.89_r_kind,0.91_r_kind,0.93_r_kind,0.94_r_kind, &
                             0.94_r_kind,0.94_r_kind,0.93_r_kind,0.92_r_kind,0.90_r_kind /)

  ! -- Grass after snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_GRASS_AFTER_SNOW = (/ 0.91_r_kind,0.91_r_kind,0.92_r_kind,0.91_r_kind,0.90_r_kind, &
                                     0.90_r_kind,0.91_r_kind,0.91_r_kind,0.91_r_kind,0.86_r_kind /)

  ! -- RS snow (A) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_RS_SNOW_A = (/ 0.90_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,0.86_r_kind, &
                              0.86_r_kind,0.85_r_kind,0.85_r_kind,0.82_r_kind,0.82_r_kind /)

  ! -- Powder snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_POWDER_SNOW = (/ 0.91_r_kind,0.91_r_kind,0.93_r_kind,0.93_r_kind,0.93_r_kind, &
                                0.93_r_kind,0.89_r_kind,0.88_r_kind,0.79_r_kind,0.79_r_kind /)

  ! -- RS snow (B) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_RS_SNOW_B = (/ 0.90_r_kind,0.89_r_kind,0.88_r_kind,0.85_r_kind,0.84_r_kind, &
                              0.83_r_kind,0.83_r_kind,0.82_r_kind,0.79_r_kind,0.73_r_kind /)

  ! -- RS snow (C) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_RS_SNOW_C = (/ 0.90_r_kind,0.89_r_kind,0.86_r_kind,0.82_r_kind,0.80_r_kind, &
                              0.79_r_kind,0.78_r_kind,0.78_r_kind,0.77_r_kind,0.77_r_kind /)

  ! -- RS snow (D) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_RS_SNOW_D = (/ 0.88_r_kind,0.86_r_kind,0.85_r_kind,0.80_r_kind,0.78_r_kind, &
                              0.77_r_kind,0.77_r_kind,0.76_r_kind,0.72_r_kind,0.72_r_kind /)

  ! -- Thin crust snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_THIN_CRUST_SNOW = (/ 0.93_r_kind,0.94_r_kind,0.96_r_kind,0.96_r_kind,0.95_r_kind, &
                                    0.93_r_kind,0.87_r_kind,0.86_r_kind,0.74_r_kind,0.65_r_kind /)

  ! -- RS snow (E) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_RS_SNOW_E = (/ 0.87_r_kind,0.86_r_kind,0.84_r_kind,0.80_r_kind,0.76_r_kind, &
                              0.76_r_kind,0.75_r_kind,0.75_r_kind,0.70_r_kind,0.69_r_kind /)

  ! -- Bottom crust snow (A) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_BOTTOM_CRUST_SNOW_A = (/ 0.87_r_kind,0.86_r_kind,0.83_r_kind,0.77_r_kind,0.73_r_kind, &
                                        0.68_r_kind,0.66_r_kind,0.66_r_kind,0.68_r_kind,0.67_r_kind /)

  ! -- Shallow snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_SHALLOW_SNOW = (/ 0.89_r_kind,0.89_r_kind,0.88_r_kind,0.87_r_kind,0.86_r_kind, &
                                 0.82_r_kind,0.77_r_kind,0.76_r_kind,0.69_r_kind,0.64_r_kind /)

  ! -- Deep snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_DEEP_SNOW = (/ 0.88_r_kind,0.87_r_kind,0.86_r_kind,0.83_r_kind,0.81_r_kind, &
                              0.77_r_kind,0.74_r_kind,0.73_r_kind,0.69_r_kind,0.64_r_kind /)

  ! -- Crust snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_CRUST_SNOW = (/ 0.86_r_kind,0.86_r_kind,0.86_r_kind,0.85_r_kind,0.82_r_kind, &
                               0.78_r_kind,0.69_r_kind,0.68_r_kind,0.51_r_kind,0.47_r_kind /)

  ! -- Medium snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_MEDIUM_SNOW = (/ 0.89_r_kind,0.88_r_kind,0.87_r_kind,0.83_r_kind,0.80_r_kind, &
                                0.75_r_kind,0.70_r_kind,0.70_r_kind,0.64_r_kind,0.60_r_kind /)

  ! -- Bottom crust snow (B) default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_BOTTOM_CRUST_SNOW_B = (/ 0.91_r_kind,0.92_r_kind,0.93_r_kind,0.88_r_kind,0.84_r_kind, &
                                        0.76_r_kind,0.66_r_kind,0.64_r_kind,0.48_r_kind,0.44_r_kind /)

  ! -- Thick crust snow default spectrum
  REAL( r_kind ), PRIVATE, PARAMETER, DIMENSION( N_FREQUENCIES ) :: &
    EMISSIVITY_THICK_CRUST_SNOW = (/ 0.94_r_kind,0.95_r_kind,0.97_r_kind,0.91_r_kind,0.86_r_kind, &
                                     0.74_r_kind,0.63_r_kind,0.63_r_kind,0.50_r_kind,0.45_r_kind /)


  ! -------------------------------------------------------------------
  ! The publicly accessible default emissivity spectra. The spectra are
  ! specified individually above and then gathered here to make it
  ! easier to determine what numbers are for what snow type emissivity
  ! ** Referred to as "em" in old code **
  ! -------------------------------------------------------------------

  REAL( r_kind ), PUBLIC, PARAMETER, DIMENSION( N_FREQUENCIES, N_SNOW_TYPES ) :: &
    DEFAULT_EMISSIVITY = RESHAPE( (/ EMISSIVITY_WET_SNOW,            &  ! Snow type =  1
                                     EMISSIVITY_GRASS_AFTER_SNOW,    &  ! Snow type =  2
                                     EMISSIVITY_RS_SNOW_A,           &  ! Snow type =  3
                                     EMISSIVITY_POWDER_SNOW,         &  ! Snow type =  4
                                     EMISSIVITY_RS_SNOW_B,           &  ! Snow type =  5
                                     EMISSIVITY_RS_SNOW_C,           &  ! Snow type =  6
                                     EMISSIVITY_RS_SNOW_D,           &  ! Snow type =  7
                                     EMISSIVITY_THIN_CRUST_SNOW,     &  ! Snow type =  8
                                     EMISSIVITY_RS_SNOW_E,           &  ! Snow type =  9
                                     EMISSIVITY_BOTTOM_CRUST_SNOW_A, &  ! Snow type = 10
                                     EMISSIVITY_SHALLOW_SNOW,        &  ! Snow type = 11
                                     EMISSIVITY_DEEP_SNOW,           &  ! Snow type = 12
                                     EMISSIVITY_CRUST_SNOW,          &  ! Snow type = 13
                                     EMISSIVITY_MEDIUM_SNOW,         &  ! Snow type = 14
                                     EMISSIVITY_BOTTOM_CRUST_SNOW_B, &  ! Snow type = 15
                                     EMISSIVITY_THICK_CRUST_SNOW /), &  ! Snow type = 16
                                  (/ N_FREQUENCIES, N_SNOW_TYPES /) )
  

END MODULE MWSE_Snow_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/12/09 20:07:32 $
!
! $Revision: 1.1 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MWSE_Snow_Parameters.f90,v $
! Revision 1.1  2004/12/09 20:07:32  paulv
! Initial checkin.
!
!
!
