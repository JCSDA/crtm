MODULE MWSE_Snow


  ! ----------
  ! Module use
  ! ----------

  USE kinds
  USE constants

  USE MWSE_Snow_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  ! -- Everything private by default
  PRIVATE

  ! -- Public procedures to compute microwave emissivities
  PUBLIC :: snwem_amsu


  ! ---------------------
  ! RCS Id for the module
  ! ---------------------

  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &

  
CONTAINS


  SUBROUTINE  snwem_amsu(Theta,Frequency,Snow_Depth,Skin_Temperature,Tb_AMSUA,Tb_AMSUB,esh,esv)

  !$$$  subprogram Documentation block
  !                .      .    .                                       .
  ! subprogram:  noaa/nesdis emissivity model over snow/ice for AMSU-A/B
  !
  !   prgmmr: Banghua Yan      org: nesdis              date: 2003-08-18
  !
  ! abstract: noaa/nesdis emissivity model to compute microwave emissivity over
  !    snow for AMSU-A/B. The processing varies according to input parameters
  !         Option 1 :  AMSU-A & B window channels of brightness temperatures (Tb)
  !                      and surface temperature (Skin_Temperature) are available
  !         Option 2 :  AMSU-A window channels of Tb and Skin_Temperature are available
  !         Option 3 :  AMSU-A & B window channels of Tb are available
  !         Option 4 :  AMSU-A window channels of Tb are available
  !         Option 5 :  AMSU-B window channels of Tb and Skin_Temperature are available
  !         Option 6 :  AMSU-B window channels of Tb are available
  !         Option 7 :  snow depth and Skin_Temperature are available
  !
  ! references:
  !    Yan, B., F. Weng and K.Okamoto,2004:
  !       "A microwave snow emissivity model, submitted to TGRS
  !
  !   version: 3
  !
  ! program history log:
  !     beta       : November 28, 2000
  !
  !     version 2.0: June 18, 2003.
  !
  !                  Version 2.0 enhances the capability/performance of beta version of
  !               land emissivity model (LandEM) over snow conditions. Two new subroutines
  !               (i.e., snowem_tb and six_indices) are added as replacements of the
  !               previous snow emissivity. If AMSU measurements are not available, the
  !               results are the same as these in beta version. The new snow emissivity
  !               model is empirically derived from satellite retrievals and ground-based
  !               measurements.
  !
  !     version 3.0: August 18, 2003.
  !
  !                  Version 3.0 is an extended version of LandEM 2.0 over snow conditions.
  !               It covers seven different options (see below for details) for LandEM
  !               inputs over snow conditions. When All or limited AMSU measurements are
  !               available, one of the subroutines sem_ABTs, sem_ATs, sem_AB, sem_amsua,
  !               sem_BTs and sem_amsub, which are empirically derived from satellite
  !               retrievals and ground-based measurements, are called to simuate snow
  !               emissivity; when no AMSU measurements are avalaiable, the subroutine
  !               ALandEM_Snow is called where the results over snow conditions in beta
  !               version are adjusted with a bias correction that is obtained using a
  !               statistical algorithm. Thus, LandEM 3.0 significantly enhances the
  !               flexibility/performance of LandEM 2.0 in smulating emissivity over snow
  !               conditions.
  !
  !
  !               July 26, 2004: modified the version 3.0 for GSI subsystem by Kozo Okamoto
  !
  !
  ! input argument list:
  !     Theta            -  local zenith angle in radian
  !     Frequency        -  Frequency in GHz
  !     Skin_Temperature -  scattering layer temperature (K)   (gdas)
  !     Snow_Depth       -  scatter medium depth (mm)          (gdas)
  !     Tb_AMSUA[1] ~ Tb_AMSUA[4]  -  Tb at four AMSU-A window channels
  !                              Tb_AMSUA[1] : 23.8 GHz
  !                              Tb_AMSUA[2] : 31.4 GHz
  !                              Tb_AMSUA[3] : 50.3 GHz
  !                              Tb_AMSUA[4] : 89 GHz
  !     Tb_AMSUB[1] ~ Tb_AMSUB[2]  -  Tb at two AMSU-B window channels:
  !                              Tb_AMSUB[1] : 89 GHz
  !                              Tb_AMSUB[2] : 150 GHz
  !       When Tb_AMSUA[ ] or Tb_AMSUB[ ] = -999.9: a missing value (no available data)
  !
  ! output argument list:
  !       esv        -  emissivity at vertical polarization
  !       esh        -  emissivity at horizontal polarization
  !       Snow_Type  -  snow type (not output here)
  !                     1 : Wet Snow
  !                     2 : Grass_after_Snow
  !                     3 : RS_Snow (A)
  !                     4 : Powder Snow
  !                     5 : RS_Snow (B)
  !                     6 : RS_Snow (C)
  !                     7 : RS_Snow (D)
  !                     8 : Thin Crust Snow
  !                     9 : RS_Snow (E)
  !                     10: Bottom Crust Snow (A)
  !                     11: Shallow Snow
  !                     12: Deep Snow
  !                     13: Crust Snow
  !                     14: Medium Snow
  !                     15: Bottom Crust Snow (B)
  !                     16: Thick Crust Snow
  !                    999: AMSU measurements are not available or over non-snow conditions
  ! important internal variables/parameters:
  !
  !       tb[1] ~ tb[5]  -  Tb at five AMSU-A & B window channels:
  !                              tb[1] = Tb_AMSUA[1]
  !                              tb[2] = Tb_AMSUA[2]
  !                              tb[3] = Tb_AMSUA[3]
  !                              tb[4] = Tb_AMSUA[4]
  !                              tb[5] = Tb_AMSUB[2]
  !
  ! remarks:
  !
  !  Questions/comments: Please send to Fuzhong.Weng@noaa.gov or Banghua.Yan@noaa.gov
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$


    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( r_kind ),                                       INTENT( IN ) :: Theta
    REAL( r_kind ),                                       INTENT( IN ) :: Frequency
    REAL( r_kind ),                                       INTENT( IN ) :: Snow_Depth
    REAL( r_kind ),                                       INTENT( IN ) :: Skin_Temperature
    REAL( r_kind ), DIMENSION( N_AMSUA_WINDOW_CHANNELS ), INTENT( IN ) :: Tb_AMSUA
    REAL( r_kind ), DIMENSION( N_AMSUB_WINDOW_CHANNELS ), INTENT( IN ) :: Tb_AMSUB

    ! -- Output
    REAL( r_kind ), INTENT( OUT ) :: esh
    REAL( r_kind ), INTENT( OUT ) :: esv


    ! ----------------
    ! Local PARAMETERs
    ! ----------------

    ! -- The number of algorithm choices
    INTEGER, PARAMETER :: N_ALGORITHMS = 7

    INTEGER, PARAMETER ::   ABTs_ALGORITHM = 1
    INTEGER, PARAMETER ::    ATs_ALGORITHM = 2
    INTEGER, PARAMETER :: AMSUAB_ALGORITHM = 3
    INTEGER, PARAMETER ::  AMSUA_ALGORITHM = 4
    INTEGER, PARAMETER ::    BTs_ALGORITHM = 5
    INTEGER, PARAMETER ::  AMSUB_ALGORITHM = 6
    INTEGER, PARAMETER ::   MODL_ALGORITHM = 7

    INTEGER, PARAMETER ::   VALID_ALGORITHM =  1
    INTEGER, PARAMETER :: INVALID_ALGORITHM = VALID_ALGORITHM - 1

    ! -- Temperature valid range
    REAL( r_kind ), PARAMETER :: MIN_TEMPERATURE = 100.0_r_kind
    REAL( r_kind ), PARAMETER :: MAX_TEMPERATURE = 320.0_r_kind

    ! -- Snow depth valid range
    REAL( r_kind ), PARAMETER :: MIN_SNOW_DEPTH = ZERO
    REAL( r_kind ), PARAMETER :: MAX_SNOW_DEPTH = 3000.0_r_kind

    ! -- Frequency cutoff
    REAL( r_kind ), PARAMETER :: FREQUENCY_CUTOFF = 80.0_r_kind


    ! ---------------
    ! Local variables
    ! ---------------

    REAL( r_kind ), DIMENSION( N_POLARIZATIONS ) :: Emissivity_Vector
    REAL( r_kind ), DIMENSION( N_AMSUAB_WINDOW_CHANNELS ) :: Tb
    INTEGER :: Snow_Type,i,ich,np,k

    INTEGER, DIMENSION( N_ALGORITHMS ) :: Algorithm_List
    INTEGER :: Selected_Algorithm



    !#--------------------------------------------------------------------------#
    !#                            -- INITIALIZATION --                          #
    !#--------------------------------------------------------------------------#

    CALL em_initialization(Frequency,Emissivity_Vector)
    Snow_Type  = -999

    ! -- Flag all algorithms as valid
    Algorithm_List = VALID_ALGORITHM



    !#--------------------------------------------------------------------------#
    !#    -- LOAD INPUT AMSU DATA INTO ARRAY FOR COMBINED AMSU-A/B OPTION --    #
    !#--------------------------------------------------------------------------#

    ! -- Load ALL the AMSU-A window channel DATA
    Tb(1:N_AMSUA_WINDOW_CHANNELS) = Tb_AMSUA

    ! -- Load just the 150GHz channel DATA for AMSU-B
    Tb(N_AMSUAB_WINDOW_CHANNELS)  = Tb_AMSUB(2)



    !#--------------------------------------------------------------------------#
    !#              -- CHECK INPUT DATA FOR ALGORITHM SELECTION --              #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check the skin temperature
    ! --------------------------

    IF ( Skin_Temperature <= MIN_TEMPERATURE .OR. &
         Skin_Temperature >= MAX_TEMPERATURE      ) THEN

      ! -- Deselect all skin temperature dependent algorithms
      Algorithm_List( ABTs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(  ATs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(  BTs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List( MODL_ALGORITHM ) = INVALID_ALGORITHM
    END IF


    ! ---------------------
    ! Check the AMSU-A DATA
    ! ---------------------

    IF ( ANY( Tb_AMSUA <= MIN_TEMPERATURE ) .OR. &
         ANY( Tb_AMSUA >= MAX_TEMPERATURE )      ) THEN

      ! -- Deselect all AMSU-A DATA dependent algorithms
      Algorithm_List(   ABTs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(    ATs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List( AMSUAB_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(  AMSUA_ALGORITHM ) = INVALID_ALGORITHM
    END IF


    ! ---------------------
    ! Check the AMSU-B DATA
    ! ---------------------

    IF ( ANY( Tb_AMSUB <= MIN_TEMPERATURE ) .OR. &
         ANY( Tb_AMSUB >= MAX_TEMPERATURE )      ) THEN

      ! -- Deselect all AMSU-B DATA dependent algorithms
      Algorithm_List(   ABTs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List( AMSUAB_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(    BTs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List(  AMSUB_ALGORITHM ) = INVALID_ALGORITHM
    END IF


    ! --------------------
    ! Check the snow depth
    ! --------------------

    IF ( Snow_Depth <  MIN_SNOW_DEPTH .OR. &
         Snow_Depth >= MAX_SNOW_DEPTH      ) THEN

      ! -- Deselect and snow depth dependent algorithms
      Algorithm_List( MODL_ALGORITHM ) = INVALID_ALGORITHM
    END IF


    ! -------------------
    ! Check the frequency
    ! -------------------

    IF ( Frequency >= FREQUENCY_CUTOFF .AND. &
         Algorithm_List( BTs_ALGORITHM ) == VALID_ALGORITHM ) THEN

      ! -- Deselect some AMSU-A algorithms
      Algorithm_List(    ATs_ALGORITHM ) = INVALID_ALGORITHM
      Algorithm_List( AMSUAB_ALGORITHM ) = INVALID_ALGORITHM
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- COMPUTE THE MICROWAVE EMISSIVITY --                 #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------------
    ! Select the *first* valid algorithm in the list
    ! ----------------------------------------------

    Selected_Algorithm = MAXLOC( Algorithm_List, DIM = 1 )

                                 
    ! -------------------------
    ! Call the required routine
    ! -------------------------

    SELECT CASE ( Selected_Algorithm )

      CASE ( ABTs_ALGORITHM )
         CALL sem_ABTs(Theta,Frequency,Tb,Skin_Temperature,Snow_Type,Emissivity_Vector)

      CASE ( ATs_ALGORITHM )
         CALL sem_ATs(Theta,Frequency,Tb_AMSUA,Skin_Temperature,Snow_Type,Emissivity_Vector)

      CASE ( AMSUAB_ALGORITHM )
         CALL sem_AB(Theta,Frequency,Tb,Snow_Type,Emissivity_Vector)

      CASE ( AMSUA_ALGORITHM )
         CALL sem_amsua(Theta,Frequency,Tb_AMSUA,Snow_Type,Emissivity_Vector)

      CASE ( BTs_ALGORITHM )
         CALL sem_BTs(Theta,Frequency,Tb_AMSUB,Skin_Temperature,Snow_Type,Emissivity_Vector)

      CASE ( AMSUB_ALGORITHM )
         CALL sem_amsub(Theta,Frequency,Tb_AMSUB,Snow_Type,Emissivity_Vector)

      CASE ( MODL_ALGORITHM )
         CALL ALandEM_Snow(Theta,Frequency,Snow_Depth,Skin_Temperature,Snow_Type,Emissivity_Vector)

    END SELECT
    
    esv = Emissivity_Vector(1)
    esh = Emissivity_Vector(2)
    
  END SUBROUTINE snwem_amsu


  !---------------------------------------------------------------------!
  SUBROUTINE em_initialization( Frequency, Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:   AMSU-A/B snow emissivity initialization
  !
  !   prgmmr:  Banghua Yan                org: nesdis              date: 2003-08-18
  !
  ! abstract: AMSU-A/B snow emissivity initialization
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency   - Frequency in GHz
  !
  ! output argument list:
  !
  !     Emissivity_Vector[1] and [2]  -  initial emissivity at two polarizations.
  !
  ! important internal variables:
  !
  !      Freq[1~10]  - ten frequencies for sixteen snow types of emissivity
  !      em[1~16,*]  - sixteen snow emissivity spectra
  !      Snow_Type   - snow type
  !                    where it is initialized to as the type 4,i.e, Powder Snow
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    !#--------------------------------------------------------------------------#
    !#                        -- TYPE DECLARATIONS --                           #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    REAL( r_kind ),                               INTENT( IN )  :: Frequency

    ! -- Output
    REAL( r_kind ), DIMENSION( N_POLARIZATIONS ), INTENT( OUT ) :: Emissivity_Vector


    ! ----------------
    ! Local PARAMETERs
    ! ----------------

    INTEGER, PARAMETER :: DEFAULT_SNOW_TYPE = POWDER_SNOW

    REAL( r_kind ), PARAMETER :: MIN_EMISSIVITY = 0.8_r_kind


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: Idx
    REAL( r_kind ) :: m
    REAL( r_kind ) :: Emissivity



    !#--------------------------------------------------------------------------#
    !#          -- COMPUTE DEFAULT EMISSIVITY AT REQUESTED FREQUENCY --         #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------
    ! For frequencies GREATER THAN the maximum covered
    ! by the default spectrum, the emissivity is set
    ! to a constant value. Note that the text here
    ! *must* be >= so that the Idx determination in
    ! the ELSE clause below doesn't fail due to all
    ! elements of the MASK being false.
    ! ------------------------------------------------

    IF ( Frequency >= DEFAULT_FREQUENCY( N_FREQUENCIES ) ) THEN

      Emissivity = DEFAULT_EMISSIVITY( N_FREQUENCIES, DEFAULT_SNOW_TYPE )


    ! -----------------------------------------------
    ! For frequencies LESS THAN the minimum covered
    ! by the default spectrum, the emissivity is
    ! linearly extrapolated and corrected if required
    ! -----------------------------------------------

    ELSE IF ( Frequency < DEFAULT_FREQUENCY(1) ) THEN

      ! -- The gradient, dE/dF
      m = ( DEFAULT_EMISSIVITY(2,DEFAULT_SNOW_TYPE) - DEFAULT_EMISSIVITY(1,DEFAULT_SNOW_TYPE) ) / &
      !   -------------------------------------------------------------------------------------
                             ( DEFAULT_FREQUENCY(2) - DEFAULT_FREQUENCY(1) )

      ! -- The linear interpolation, y = y1 + m(x-x1)
      Emissivity = DEFAULT_EMISSIVITY(1,DEFAULT_SNOW_TYPE) + ( m * ( Frequency - DEFAULT_FREQUENCY(1) ) )

      ! -- Correct the result if necessary so that
      ! --   MIN_EMISSIVITY <= Emissivity <= ONE
      Emissivity = MAX( MIN( Emissivity, ONE ), MIN_EMISSIVITY )


    ! -------------------------------------------
    ! Frequency is within valid range, so simply
    ! interpolate the default emissivity spectrum
    ! -------------------------------------------

    ELSE

      ! -- Find the closest default frequency index such that
      ! --   Frequency < DEFAULT_FREQUENCY
      Idx = MINLOC( DEFAULT_FREQUENCY - Frequency, MASK = DEFAULT_FREQUENCY - Frequency > ZERO, DIM = 1 )

      ! -- The gradient, dE/dF
      m = ( DEFAULT_EMISSIVITY(Idx,DEFAULT_SNOW_TYPE) - DEFAULT_EMISSIVITY(Idx-1,DEFAULT_SNOW_TYPE) ) / &
      !   -------------------------------------------------------------------------------------------
                             ( DEFAULT_FREQUENCY(Idx) - DEFAULT_FREQUENCY(Idx-1) )

      ! -- The linear interpolation, y = y1 + m(x-x1)
      Emissivity = DEFAULT_EMISSIVITY(Idx-1,DEFAULT_SNOW_TYPE) + ( m * ( Frequency - DEFAULT_FREQUENCY(Idx-1) ) )

    END IF



    !#--------------------------------------------------------------------------#
    !#      -- ASSIGN THE RETURN VALUE. SAME VALUE FOR ALL POLARIZATIONS --     #
    !#--------------------------------------------------------------------------#

    Emissivity_Vector = Emissivity

  END SUBROUTINE em_initialization



  !---------------------------------------------------------------------!
  SUBROUTINE  em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)

  !$$$  subprogram Documentation block
  !
  ! subprogram:  determine Snow_Type and calculate emissivity
  !
  !   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract: 1. Find one snow emissivity spectrum to mimic the emission
  !              property of the realistic snow condition using a set of
  !              discrminators
  !           2. Interpolate/extrapolate emissivity at a required frequency
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency        - Frequency in GHz
  !      discriminators   - emissivity discriminators at five AMSU-A & B window
  !                         channels
  !            discriminator[1]   :  emissivity discriminator at 23.8 GHz
  !            discriminator[2]   :  emissivity discriminator at 31.4 GHz
  !            discriminator[3]   :  emissivity discriminator at 50.3 GHz
  !            discriminator[4]   :  emissivity discriminator at 89   GHz
  !            discriminator[5]   :  emissivity discriminator at 150  GHz
  !
  !       Note: discriminator(1) and discriminator(3) are missing value in
  !            'AMSU-B & Ts','AMUS-B' and 'MODL' options., which are defined to as -999.9,
  ! output argument list:
  !
  !     Emissivity_Vector[1] and [2]  -  emissivity at two polarizations.
  !     Snow_Type             - snow type
  !
  ! important internal variables:
  !
  !     Freq[1 ~ 10]  -  ten frequencies for sixteen snow types of emissivity
  !     em[1~16,*]    -  sixteen snow emissivity spectra
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER:: ich,ichmin,ichmax,i,j,k,s,Snow_Type
    REAL( r_kind )   :: dem,demmin0
    REAL( r_kind )   :: Frequency,Emissivity,discriminator(*),emis(N_FREQUENCIES)
    REAL( r_kind )   :: cor_factor,adjust_check,kratio, bconst

  ! Adjust unreasonable discriminator
    IF (discriminator(4) > discriminator(2))    &
         discriminator(4) = discriminator(2) +(discriminator(5) - discriminator(2))*  &
         (150.0_r_kind - 89.0_r_kind)/(150.0_r_kind - 31.4_r_kind)

    IF ( (discriminator(3) /= -999.9_r_kind) .and.       &
         ( ((discriminator(3)-0.01_r_kind) > discriminator(2)) .or.     &
         ((discriminator(3)-0.01_r_kind) < discriminator(4)))    )    &
         discriminator(3) = discriminator(2) +  &
         (discriminator(4) - discriminator(2))*(89.0_r_kind - 50.3_r_kind) &
         / (89.0_r_kind - 31.4_r_kind)
    
  ! Find a snow emissivity spectrum
    IF (Snow_Type .eq. -999) THEN
       demmin0 = 10.0_r_kind
       DO k = 1, N_SNOW_TYPES
          dem = zero
          ichmin = 1
          ichmax = 3
          IF (discriminator(1) == -999.9_r_kind) THEN
             ichmin = 2
             ichmax = 2
          END IF
          DO ich = ichmin,ichmax
             dem = dem + abs(discriminator(ich) - DEFAULT_EMISSIVITY(ich+4,k))
          END DO
          DO ich = 4,5
             dem = dem + abs(discriminator(ich) - DEFAULT_EMISSIVITY(ich+5,k))
          END DO
          IF (dem < demmin0) THEN
             demmin0 = dem
             Snow_Type = k
          END IF
       END DO
    END IF
     
  ! Shift snow emissivity according to discriminator at 31.4 GHz
    cor_factor = discriminator(2) - DEFAULT_EMISSIVITY(6,Snow_Type)
    DO ich = 1, N_FREQUENCIES
       emis(ich) = DEFAULT_EMISSIVITY(ich,Snow_Type) + cor_factor
       IF (emis(ich) .gt. one)         emis(ich) = one
       IF (emis(ich) .lt. 0.3_r_kind) emis(ich) = 0.3_r_kind
    END DO
     
  ! Emisivity data quality control
    adjust_check = zero
    DO ich = 5, 9
       IF (ich .le. 7) THEN
          IF (discriminator(ich - 4) .ne. -999.9_r_kind) &
               adjust_check = adjust_check + abs(emis(ich) - discriminator(ich - 4))
       ELSE
          IF (discriminator(ich - 4) .ne. -999.9_r_kind)  &
               adjust_check = adjust_check + abs(emis(ich+1) - discriminator(ich - 4))
       END IF
    END DO
     
    IF (adjust_check >= 0.04_r_kind) THEN
       IF (discriminator(1) /= -999.9_r_kind) THEN
          IF (discriminator(1) < emis(4)) THEN
             emis(5) = emis(4) + &
                  (31.4_r_kind - 23.8_r_kind) * &
                  (discriminator(2) - emis(4))/(31.4_r_kind - 18.7_r_kind)
          ELSE
             emis(5) = discriminator(1)
          END IF
       END IF
       emis(6) = discriminator(2)
       IF (discriminator(3) /= -999.9_r_kind) THEN
          emis(7) = discriminator(3)
       ELSE
  !       In case of missing the emissivity discriminator at 50.3 GHz
          emis(7) = emis(6) + (89.0_r_kind - 50.3_r_kind) * &
               (discriminator(4) - emis(6))/(89.0_r_kind - 31.4_r_kind)
       END IF
       emis(8) = emis(7)
       emis(9) = discriminator(4)
       emis(10) = discriminator(5)
    END IF
    
  ! Estimate snow emissivity at a required frequency
    DO i = 2, N_FREQUENCIES
       IF (Frequency <  DEFAULT_FREQUENCY(1))   EXIT
       IF (Frequency >= DEFAULT_FREQUENCY(N_FREQUENCIES)) EXIT
       IF (Frequency <  DEFAULT_FREQUENCY(i)) THEN
          Emissivity = emis(i-1) + (emis(i) - emis(i-1))*(Frequency - DEFAULT_FREQUENCY(i-1))  &
               /(DEFAULT_FREQUENCY(i) - DEFAULT_FREQUENCY(i-1))
          EXIT
       END IF
    END DO
    
  ! Extrapolate to lower frequencies than 4.9GHz
    IF (Frequency <= DEFAULT_FREQUENCY(1)) THEN
       kratio = (emis(2) - emis(1))/(DEFAULT_FREQUENCY(2) - DEFAULT_FREQUENCY(1))
       bconst = emis(1) - kratio*DEFAULT_FREQUENCY(1)
       Emissivity =  kratio*Frequency + bconst
       IF (Emissivity > one)          Emissivity = one
       IF (Emissivity <= 0.8_r_kind) Emissivity = 0.8_r_kind
    END IF
    
  ! Assume emissivity = constant at frequencies >= 150 GHz
    IF (Frequency >= DEFAULT_FREQUENCY(N_FREQUENCIES)) Emissivity = emis(N_FREQUENCIES)
    
  END SUBROUTINE em_interpolate


  !---------------------------------------------------------------------!
  SUBROUTINE sem_ABTs(Theta,Frequency,tb,ts,Snow_Type,Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr:Banghua Yan                  org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at a required Frequency with respect to secenery ABTs
  !
  ! program history log:
  !
  ! input argument list:
  !
  !     Frequency        -  Frequency in GHz
  !     Theta            -  local zenith angle (not used here)
  !     tb[1] ~ tb[5]    -  brightness temperature at five AMSU window channels:
  !                              tb[1] : 23.8 GHz
  !                              tb[2] : 31.4 GHz
  !                              tb[3] : 50.3 GHz
  !                              tb[4] : 89.0 GHz
  !                              tb[5] : 150  GHz
  !
  ! output argument list:
  !
  !      Emissivity_Vector[1] and [2]  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !      Snow_Type        -  snow type
  !
  ! important internal variables:
  !
  !     nind           -  number of threshold in decision trees
  !                          to identify each snow type  ( = 6)
  !     em(1~16,*)     -  sixteen snow emissivity spectra
  !     DI_coe         -  coefficients to generate six discriminators to describe
  !                       the overall emissivity variability within a wider frequency range
  !     threshold      -  thresholds in decision trees to identify snow types
  !     index_in       -  six indices to discriminate snow type
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER,PARAMETER:: nthresh=38
    INTEGER,PARAMETER:: nind=6,ncoe=8,nLIcoe=6,nHIcoe=12
    INTEGER:: ich,i,j,k,num,npass,Snow_Type,md0,md1,nmodel(N_SNOW_TYPES-1)
    REAL( r_kind )   :: Theta,Frequency,tb150,LI,HI,DS1,DS2,DS3
    REAL( r_kind )   :: Emissivity_Vector(*)
    REAL( r_kind )   :: tb(*),DTB(nind-1),DI(nind-1),       &
         DI_coe(nind-1,0:ncoe-1),threshold(nthresh,nind),       &
         index_in(nind),threshold0(nind)
    REAL( r_kind )   :: LI_coe(0:nLIcoe-1),HI_coe(0:nHIcoe-1)
    REAL( r_kind )   :: ts,Emissivity
    REAL( r_kind )   :: discriminator(5)
    logical:: pick_status,tindex(nind)
    SAVE      threshold,DI_coe,LI_coe, HI_coe,nmodel
    
    DATA  nmodel/5,10,13,16,18,24,30,31,32,33,34,35,36,37,38/
    
  ! Fitting coefficients for five discriminators
    DATA (DI_coe(1,k),k=0,ncoe-1)/ &
         3.285557e-002_r_kind,  2.677179e-005_r_kind,  &
         4.553101e-003_r_kind,  5.639352e-005_r_kind,  &
         -1.825188e-004_r_kind,  1.636145e-004_r_kind,  &
         1.680881e-005_r_kind, -1.708405e-004_r_kind/
    DATA (DI_coe(2,k),k=0,ncoe-1)/ &
         -4.275539e-002_r_kind, -2.541453e-005_r_kind,  &
         4.154796e-004_r_kind,  1.703443e-004_r_kind,  &
         4.350142e-003_r_kind,  2.452873e-004_r_kind,  &
         -4.748506e-003_r_kind,  2.293836e-004_r_kind/
    DATA (DI_coe(3,k),k=0,ncoe-1)/ &
         -1.870173e-001_r_kind, -1.061678e-004_r_kind,  &
        2.364055e-004_r_kind, -2.834876e-005_r_kind,  &
        4.899651e-003_r_kind, -3.418847e-004_r_kind,  &
        -2.312224e-004_r_kind,  9.498600e-004_r_kind/
    DATA (DI_coe(4,k),k=0,ncoe-1)/ &
         -2.076519e-001_r_kind,  8.475901e-004_r_kind,  &
         -2.072679e-003_r_kind, -2.064717e-003_r_kind,  &
         2.600452e-003_r_kind,  2.503923e-003_r_kind,  &
         5.179711e-004_r_kind,  4.667157e-005_r_kind/
    DATA (DI_coe(5,k),k=0,ncoe-1)/ &
         -1.442609e-001_r_kind, -8.075003e-005_r_kind,  &
         -1.790933e-004_r_kind, -1.986887e-004_r_kind,  &
         5.495115e-004_r_kind, -5.871732e-004_r_kind,  &
         4.517280e-003_r_kind,  7.204695e-004_r_kind/
    
  ! Fitting coefficients for emissivity index at 31.4 GHz
    DATA  LI_coe/ &
         7.963632e-001_r_kind,  7.215580e-003_r_kind,  &
         -2.015921e-005_r_kind, -1.508286e-003_r_kind,  &
         1.731405e-005_r_kind, -4.105358e-003_r_kind/

  ! Fitting coefficients for emissivity index at 150 GHz
    DATA  HI_coe/ &
         1.012160e+000_r_kind,  6.100397e-003_r_kind, &
         -1.774347e-005_r_kind, -4.028211e-003_r_kind, &
         1.224470e-005_r_kind,  2.345612e-003_r_kind, &
         -5.376814e-006_r_kind, -2.795332e-003_r_kind, &
         8.072756e-006_r_kind,  3.529615e-003_r_kind, &
         1.955293e-006_r_kind, -4.942230e-003_r_kind/

  ! Six thresholds for sixteen candidate snow types
  ! Note: some snow type contains several possible
  !      selections for six thresholds

  !1 Wet Snow
    DATA (threshold(1,k),k=1,6)/0.88_r_kind,0.86_r_kind,-999.9_r_kind,&
         0.01_r_kind,0.01_r_kind,200._r_kind/
    DATA (threshold(2,k),k=1,6)/0.88_r_kind,0.85_r_kind,-999.9_r_kind,&
         0.06_r_kind,0.10_r_kind,200._r_kind/
    DATA (threshold(3,k),k=1,6)/0.88_r_kind,0.83_r_kind,-0.02_r_kind,&
         0.12_r_kind,0.16_r_kind,204._r_kind/
    DATA (threshold(4,k),k=1,6)/0.90_r_kind,0.89_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(5,k),k=1,6)/0.92_r_kind,0.85_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !2 Grass_after_Snow
    DATA (threshold(6,k),k=1,6)/0.84_r_kind,0.83_r_kind,-999.9_r_kind,&
         0.08_r_kind,0.10_r_kind,195._r_kind/
    DATA (threshold(7,k),k=1,6)/0.85_r_kind,0.85_r_kind,-999.9_r_kind,&
         0.10_r_kind,-999.9_r_kind,190._r_kind/
    DATA (threshold(8,k),k=1,6)/0.86_r_kind,0.81_r_kind,-999.9_r_kind,&
         0.12_r_kind,-999.9_r_kind,200._r_kind/
    DATA (threshold(9,k),k=1,6)/0.86_r_kind,0.81_r_kind,0.0_r_kind,&
         0.12_r_kind,-999.9_r_kind,189._r_kind/
    DATA (threshold(10,k),k=1,6)/0.90_r_kind,0.81_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,195._r_kind/
    
  !3 RS_Snow (A)
    DATA (threshold(11,k),k=1,6)/0.80_r_kind,0.76_r_kind,-999.9_r_kind,&
         0.05_r_kind,-999.9_r_kind,185._r_kind/
    DATA (threshold(12,k),k=1,6)/0.82_r_kind,0.78_r_kind,-999.9_r_kind,&
         -999.9_r_kind,0.25_r_kind,180._r_kind/
    DATA (threshold(13,k),k=1,6)/0.90_r_kind,0.76_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,180._r_kind/
    
  !4 Powder  Snow
    DATA (threshold(14,k),k=1,6)/0.89_r_kind,0.73_r_kind,-999.9_r_kind,&
         0.20_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(15,k),k=1,6)/0.89_r_kind,0.75_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(16,k),k=1,6)/0.93_r_kind,0.72_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !5 RS_Snow (B)
    DATA (threshold(17,k),k=1,6)/0.82_r_kind,0.70_r_kind,-999.9_r_kind,&
         0.20_r_kind,-999.9_r_kind,160._r_kind/
    DATA (threshold(18,k),k=1,6)/0.83_r_kind,0.70_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,160._r_kind/

  !6 RS_Snow (C)
    DATA (threshold(19,k),k=1,6)/0.75_r_kind,0.76_r_kind,-999.9_r_kind,&
         0.08_r_kind,-999.9_r_kind,172._r_kind/
    DATA (threshold(20,k),k=1,6)/0.77_r_kind,0.72_r_kind,-999.9_r_kind,&
         0.12_r_kind,0.15_r_kind,175._r_kind/
    DATA (threshold(21,k),k=1,6)/0.78_r_kind,0.74_r_kind,-999.9_r_kind,&
         -999.9_r_kind,0.20_r_kind,172._r_kind/
    DATA (threshold(22,k),k=1,6)/0.80_r_kind,0.77_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,170._r_kind/
    DATA (threshold(23,k),k=1,6)/0.82_r_kind,-999.9_r_kind,-999.9_r_kind,&
         0.15_r_kind,0.22_r_kind,170._r_kind/
    DATA (threshold(24,k),k=1,6)/0.82_r_kind,0.73_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,170._r_kind/

  !7 RS_Snow (D)
    DATA (threshold(25,k),k=1,6)/0.75_r_kind,0.70_r_kind,-999.9_r_kind,&
         0.15_r_kind,0.25_r_kind,167._r_kind/
    DATA (threshold(26,k),k=1,6)/0.77_r_kind,0.76_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(27,k),k=1,6)/0.80_r_kind,0.72_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(28,k),k=1,6)/0.77_r_kind,0.73_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    
    DATA (threshold(29,k),k=1,6)/0.81_r_kind,0.71_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    DATA (threshold(30,k),k=1,6)/0.82_r_kind,0.69_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    
  !8 Thin Crust Snow
    DATA (threshold(31,k),k=1,6)/0.88_r_kind,0.58_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    
  !9 RS_Snow (E)
    DATA (threshold(32,k),k=1,6)/0.73_r_kind,0.67_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    
  !10 Bottom Crust Snow (A)
    DATA (threshold(33,k),k=1,6)/0.83_r_kind,0.66_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/
    
  !11 Shallow Snow
    DATA (threshold(34,k),k=1,6)/0.82_r_kind,0.60_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !12 Deep Snow
    DATA (threshold(35,k),k=1,6)/0.77_r_kind,0.60_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !13 Crust Snow
    DATA (threshold(36,k),k=1,6)/0.77_r_kind,0.7_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !14 Medium Snow
    DATA (threshold(37,k),k=1,6)/-999.9_r_kind,0.55_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !15 Bottom Crust Snow(B)
    DATA (threshold(38,k),k=1,6)/0.74_r_kind,-999.9_r_kind,-999.9_r_kind,&
         -999.9_r_kind,-999.9_r_kind,-999.9_r_kind/

  !16 Thick Crust Snow
  ! lowest priority: No constraints

  !***  DEFINE SIX DISCRIMINATORS

    dtb(1) = tb(1) - tb(2)
    dtb(2) = tb(2) - tb(4)
    dtb(3) = tb(2) - tb(5)
    dtb(4) = tb(3) - tb(5)
    dtb(5) = tb(4) - tb(5)
    tb150  = tb(5)
    
    LI = LI_coe(0)
    DO i=0,1
       LI = LI + LI_coe(2*i+1)*tb(i+1) + LI_coe(2*i+2)*tb(i+1)*tb(i+1)
    END DO
    LI = LI + LI_coe(nLIcoe-1)*ts
    
    HI = HI_coe(0)
    DO i=0,4
       HI = HI + HI_coe(2*i+1)*tb(i+1) + HI_coe(2*i+2)*tb(i+1)*tb(i+1)
    END DO
    HI = HI + HI_coe(nHIcoe-1)*ts
    
    DO num=1,nind-1
       DI(num) = DI_coe(num,0) + DI_coe(num,1)*tb(2)
       DO i=1,5
          DI(num) = DI(num) + DI_coe(num,1+i)*DTB(i)
       END DO
       DI(num) = DI(num) +  DI_coe(num,ncoe-1)*ts
    END DO
    
  !*** DEFINE FIVE INDIES
    !HI = DI(0) - DI(3)
    DS1 = DI(1) + DI(2)
    DS2 = DI(4) + DI(5)
    DS3 = DS1 + DS2 + DI(3)
    
    index_in(1) = LI
    index_in(2) = HI
    index_in(3) = DS1
    index_in(4) = DS2
    index_in(5) = DS3
    index_in(6) = tb150

  !*** IDENTIFY SNOW TYPE


  ! Initialization
    md0 = 1
    Snow_Type = N_SNOW_TYPES
    pick_status = .false.

  ! Pick one snow type
  ! Check all possible selections for six thresholds for each snow type
    DO i = 1, N_SNOW_TYPES - 1
       md1 = nmodel(i)
       DO j = md0, md1
          npass = 0
          DO k = 1 , nind
             threshold0(k) = threshold(j,k)
          END DO
          CALL six_indices(nind,index_in,threshold0,tindex)

  ! Corrections
          IF ((i == 5)  .and. (index_in(2) >  0.75_r_kind)) tindex(2) = .false.
          IF ((i == 5)  .and. (index_in(4) >  0.20_r_kind)                        &
               .and. (index_in(1) >  0.88_r_kind)) tindex(1) = .false.
          IF ((i == 10) .and. (index_in(1) <= 0.83_r_kind)) tindex(1) = .true.
          IF ((i == 13) .and. (index_in(2) <  0.52_r_kind)) tindex(2) = .true.
          DO k = 1, nind
             IF (.not.tindex(k)) EXIT
             npass = npass + 1
          END DO
          IF (npass == nind) EXIT
       END DO
       
       IF (npass == nind) THEN
          pick_status = .true.
          Snow_Type  = i
       END IF
       IF (pick_status) EXIT
       md0 = md1 + 1
    END DO
    
    discriminator(1) = LI + DI(1)
    discriminator(2) = LI
    discriminator(3) = DI(4) + HI
    discriminator(4) = LI - DI(2)
    discriminator(5) = HI
    
    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
  END SUBROUTINE sem_ABTs


  !---------------------------------------------------------------------!
  SUBROUTINE six_indices(nind,index_in,threshold,tindex)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      nind        -  Number of threshold in decision trees
  !                     to identify each snow type  ( = 6)
  !      index_in    -  six indices to discriminate snow type
  !      threshold   -  Thresholds in decision trees to identify snow types
  !
  ! output argument list:
  !
  !      tindex      - state vaiable to show surface snow emissivity feature
  !              tindex[ ] = .T.: snow emissivity feature matches the
  !                                corresponding threshold for certain snow type
  !              tindex[ ] = .F.: snow emissivity feature DOesn't match the
  !                                corresponding threshold for certain snow type
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER ::  i,nind
    REAL( r_kind )    ::  index_in(*),threshold(*)
    logical ::  tindex(*)
    
    DO i=1,nind
       tindex(i) = .false.
       IF (threshold(i) .eq. -999.9_r_kind) THEN
          tindex(i) = .true.
       ELSE
          IF ( (i .le. 2) .or. (i .gt. (nind-1)) ) THEN
             IF (index_in(i) .ge. threshold(i)) tindex(i) = .true.
          ELSE
             IF (index_in(i) .le. threshold(i)) tindex(i) = .true.
          END IF
       END IF
    END DO
    
  END SUBROUTINE six_indices


  !---------------------------------------------------------------------!
  SUBROUTINE sem_AB(Theta,Frequency,tb,Snow_Type,Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to option AMSUAB
  !
  ! program history log:
  !   2004-10-28  treadon - correct problem in declared dimensions of array coe
  !
  ! input argument list:
  !
  !      Frequency    -  Frequency in GHz
  !      Theta        -  local zenith angle (not used here)
  !      tb[1]~tb[5]  -  brightness temperature at five AMSU-A & B window channels:
  !                              tb[1] : 23.8 GHz
  !                              tb[2] : 31.4 GHz
  !                              tb[3] : 50.3 GHz
  !                              tb[4] : 89   GHz
  !                              tb[5] : 150  GHz
  !
  ! output argument list:
  !
  !     Emissivity_Vector[1] and [2] - emissivity at two polarizations.
  !                            set esv = esh here and will be updated
  !     Snow_Type       - snow type (reference [2])
  !
  ! important internal variables:
  !
  !     coe23    - fitting coefficients to estimate discriminator at 23.8 GHz
  !     coe31    - fitting coefficients to estimate discriminator at 31.4 GHz
  !     coe50    - fitting coefficients to estimate discriminator at 50.3 GHz
  !     coe89    - fitting coefficients to estimate discriminator at 89   GHz
  !     coe150   - fitting coefficients to estimate discriminator at 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$
    
    INTEGER,PARAMETER:: nwch = 5,ncoe = 10
    REAL( r_kind )    :: tb(*),Theta,Frequency
    REAL( r_kind )    :: Emissivity_Vector(*),Emissivity,discriminator(nwch)
    INTEGER :: i,Snow_Type,k,ich,nvalid_ch
    REAL( r_kind )  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
    REAL( r_kind )  :: coe(N_FREQUENCIES*(ncoe+1))
    
    Equivalence (coe(1),coe23)
    Equivalence (coe(12),coe31)
    Equivalence (coe(23),coe50)
    Equivalence (coe(34),coe89)
    Equivalence (coe(45),coe150)

  ! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
    DATA (coe23(k),k=0,6)/&
         -1.326040e+000_r_kind,  2.475904e-002_r_kind, &
         -5.741361e-005_r_kind, -1.889650e-002_r_kind, &
         6.177911e-005_r_kind,  1.451121e-002_r_kind, &
         -4.925512e-005_r_kind/
    
  ! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
    DATA (coe31(k),k=0,6)/ &
         -1.250541e+000_r_kind,  1.911161e-002_r_kind, &
         -5.460238e-005_r_kind, -1.266388e-002_r_kind, &
         5.745064e-005_r_kind,  1.313985e-002_r_kind, &
         -4.574811e-005_r_kind/

  ! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
    DATA (coe50(k),k=0,6)/  &
         -1.246754e+000_r_kind,  2.368658e-002_r_kind, &
         -8.061774e-005_r_kind, -3.206323e-002_r_kind, &
         1.148107e-004_r_kind,  2.688353e-002_r_kind, &
         -7.358356e-005_r_kind/
    
  ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
    DATA (coe89(k),k=0,8)/ &
         -1.278780e+000_r_kind,  1.625141e-002_r_kind, &
         -4.764536e-005_r_kind, -1.475181e-002_r_kind, &
         5.107766e-005_r_kind,  1.083021e-002_r_kind, &
         -4.154825e-005_r_kind,  7.703879e-003_r_kind, &
         -6.351148e-006_r_kind/

  ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb5
    DATA coe150/&
       -1.691077e+000_r_kind,  3.352403e-002_r_kind, &
       -7.310338e-005_r_kind, -4.396138e-002_r_kind, &
       1.028994e-004_r_kind,  2.301014e-002_r_kind, &
       -7.070810e-005_r_kind,  1.270231e-002_r_kind, &
       -2.139023e-005_r_kind, -2.257991e-003_r_kind, &
       1.269419e-005_r_kind/
    
    SAVE coe23,coe31,coe50,coe89,coe150

  ! Calculate emissivity discriminators at five AMSU window channels
    DO ich = 1, nwch
       discriminator(ich) = coe(1+(ich-1)*11)
       IF (ich .le. 3) nvalid_ch = 3
       IF (ich .eq. 4) nvalid_ch = 4
       IF (ich .eq. 5) nvalid_ch = 5
       DO i=1,nvalid_ch
          discriminator(ich) = discriminator(ich) + coe((ich-1)*11 + 2*i)*tb(i) +  &
               coe((ich-1)*11 + 2*i+1)*tb(i)*tb(i)
       END DO
    END DO
  !  Identify one snow emissivity spectrum and interpolate/extrapolate emissivity
  !  at a required Frequency
    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
    return
  END SUBROUTINE sem_AB


  !---------------------------------------------------------------------!
  SUBROUTINE sem_ATs(Theta,Frequency,Tb_AMSUA,ts,Snow_Type,Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr:Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to secenery AMSUAB
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency        -  Frequency in GHz
  !      Theta            -  local zenith angle (not used here)
  !      ts               -  surface temperature
  !      Tb_AMSUA[1] ~ Tb_AMSUA[4]  -  brightness temperature at five AMSU-A window channels:
  !                              Tb_AMSUA[1] : 23.8 GHz
  !                              Tb_AMSUA[2] : 31.4 GHz
  !                              Tb_AMSUA[3] : 50.3 GHz
  !                              Tb_AMSUA[4] : 89   GHz
  ! output argument list:
  !
  !     Emissivity_Vector[1] and [2]  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !     Snow_Type        -  snow type (reference [2])
  !
  ! important internal variables:
  !
  !     coe23      - fitting coefficients to estimate discriminator at 23.8 GHz
  !     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
  !     coe50      - fitting coefficients to estimate discriminator at 50.3 GHz
  !     coe89      - fitting coefficients to estimate discriminator at 89   GHz
  !     coe150     - fitting coefficients to estimate discriminator at 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER,PARAMETER:: nwch = 5,ncoe = 9
    REAL( r_kind )    :: Tb_AMSUA(*),Theta
    REAL( r_kind )    :: Emissivity_Vector(*),Emissivity,ts,Frequency,discriminator(nwch)
    INTEGER :: Snow_Type,i,k,ich,nvalid_ch
    REAL( r_kind )  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
    REAL( r_kind )  :: coe(N_FREQUENCIES*(ncoe+1))
    
    Equivalence (coe(1),coe23)
    Equivalence (coe(11),coe31)
    Equivalence (coe(21),coe50)
    Equivalence (coe(31),coe89)
    Equivalence (coe(41),coe150)

  ! Fitting Coefficients at 23.8 GHz: Using Tb1, Tb2 and Ts
    DATA (coe23(k),k=0,5)/ &
         8.210105e-001_r_kind,  1.216432e-002_r_kind,  &
         -2.113875e-005_r_kind, -6.416648e-003_r_kind,  &
         1.809047e-005_r_kind, -4.206605e-003_r_kind/
    
  ! Fitting Coefficients at 31.4 GHz: Using Tb1, Tb2 and Ts
    DATA (coe31(k),k=0,5)/ &
         7.963632e-001_r_kind,  7.215580e-003_r_kind,  &
         -2.015921e-005_r_kind, -1.508286e-003_r_kind,  &
         1.731405e-005_r_kind, -4.105358e-003_r_kind/
    
  ! Fitting Coefficients at 50.3 GHz: Using Tb1, Tb2, Tb3 and Ts
    DATA (coe50(k),k=0,7)/ &
         1.724160e+000_r_kind,  5.556665e-003_r_kind, &
         -2.915872e-005_r_kind, -1.146713e-002_r_kind, &
         4.724243e-005_r_kind,  3.851791e-003_r_kind, &
         -5.581535e-008_r_kind, -5.413451e-003_r_kind/

  ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4 and Ts
    DATA coe89/ &
         9.962065e-001_r_kind,  1.584161e-004_r_kind, &
         -3.988934e-006_r_kind,  3.427638e-003_r_kind, &
         -5.084836e-006_r_kind, -6.178904e-004_r_kind, &
         1.115315e-006_r_kind,  9.440962e-004_r_kind, &
         9.711384e-006_r_kind, -4.259102e-003_r_kind/

  ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4 and Ts
    DATA coe150/ &
         -5.244422e-002_r_kind,  2.025879e-002_r_kind,  &
         -3.739231e-005_r_kind, -2.922355e-002_r_kind, &
         5.810726e-005_r_kind,  1.376275e-002_r_kind, &
         -3.757061e-005_r_kind,  6.434187e-003_r_kind, &
         6.190403e-007_r_kind, -2.944785e-003_r_kind/

    SAVE coe23,coe31,coe50,coe89,coe150

  ! Calculate emissivity discriminators at five AMSU window channels
    DO ich = 1, nwch
       discriminator(ich) = coe(1+(ich-1)*10)
       IF (ich .le. 2) nvalid_ch = 2
       IF (ich .eq. 3) nvalid_ch = 3
       IF (ich .ge. 4) nvalid_ch = 4
       DO i=1,nvalid_ch
          discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*Tb_AMSUA(i) +  &
               coe((ich-1)*10 + 2*i+1)*Tb_AMSUA(i)*Tb_AMSUA(i)
       END DO
       discriminator(ich) = discriminator(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
    END DO
    
    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
    return
  END SUBROUTINE sem_ATs

  !---------------------------------------------------------------------!
  SUBROUTINE sem_amsua(Theta,Frequency,Tb_AMSUA,Snow_Type,Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to secenery AMSUA
  !
  ! program history log:
  !   2004-10-28  treadon - correct problem in declared dimensions of array coe
  !
  ! input argument list:
  !
  !      Frequency      -  Frequency in GHz
  !      Theta          -  local zenith angle (not used here)
  !      Tb_AMSUA[1]~Tb_AMSUA[4]  -  brightness temperature at five AMSU-A window channels:
  !                            Tb_AMSUA[1] : 23.8 GHz
  !                            Tb_AMSUA[2] : 31.4 GHz
  !                            Tb_AMSUA[3] : 50.3 GHz
  !                            Tb_AMSUA[4] : 89   GHz
  !
  ! output argument list:
  !
  !     Emissivity_Vector(1) and (2)  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !     Snow_Type        -  snow type
  !
  ! important internal variables:
  !
  !     coe23      - fitting coefficients to estimate discriminator at 23.8 GHz
  !     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
  !     coe50      - fitting coefficients to estimate discriminator at 50.3 GHz
  !     coe89      - fitting coefficients to estimate discriminator at 89   GHz
  !     coe150     - fitting coefficients to estimate discriminator at 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER,PARAMETER:: nwch = 5,ncoe = 8
    REAL( r_kind )    :: Tb_AMSUA(*),Theta
    REAL( r_kind )    :: Emissivity_Vector(*),Emissivity,Frequency,discriminator(nwch)
    INTEGER :: Snow_Type,i,k,ich,nvalid_ch
    REAL( r_kind )  :: coe23(0:ncoe),coe31(0:ncoe),coe50(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
    REAL( r_kind )  :: coe(N_FREQUENCIES*(ncoe+1))
    
    Equivalence (coe(1),coe23)
    Equivalence (coe(11),coe31)
    Equivalence (coe(21),coe50)
    Equivalence (coe(31),coe89)
    Equivalence (coe(41),coe150)
    
  ! Fitting Coefficients at 23.8 GHz: Using Tb1 ~ Tb3
    DATA (coe23(k),k=0,6)/ &
         -1.326040e+000_r_kind,  2.475904e-002_r_kind, -5.741361e-005_r_kind, &
         -1.889650e-002_r_kind,  6.177911e-005_r_kind,  1.451121e-002_r_kind, &
         -4.925512e-005_r_kind/
    
  ! Fitting Coefficients at 31.4 GHz: Using Tb1 ~ Tb3
    DATA (coe31(k),k=0,6)/ &
         -1.250541e+000_r_kind,  1.911161e-002_r_kind, -5.460238e-005_r_kind, &
         -1.266388e-002_r_kind,  5.745064e-005_r_kind,  1.313985e-002_r_kind, &
         -4.574811e-005_r_kind/

  ! Fitting Coefficients at 50.3 GHz: Using Tb1 ~ Tb3
    DATA (coe50(k),k=0,6)/ &
         -1.246754e+000_r_kind,  2.368658e-002_r_kind, -8.061774e-005_r_kind, &
         -3.206323e-002_r_kind,  1.148107e-004_r_kind,  2.688353e-002_r_kind, &
         -7.358356e-005_r_kind/
    
  ! Fitting Coefficients at 89 GHz: Using Tb1 ~ Tb4
    DATA coe89/ &
         -1.278780e+000_r_kind, 1.625141e-002_r_kind, -4.764536e-005_r_kind, &
         -1.475181e-002_r_kind, 5.107766e-005_r_kind,  1.083021e-002_r_kind, &
         -4.154825e-005_r_kind,  7.703879e-003_r_kind, -6.351148e-006_r_kind/
    
  ! Fitting Coefficients at 150 GHz: Using Tb1 ~ Tb4
    DATA coe150/ &
         -1.624857e+000_r_kind, 3.138243e-002_r_kind, -6.757028e-005_r_kind, &
         -4.178496e-002_r_kind, 9.691893e-005_r_kind,  2.165964e-002_r_kind, &
         -6.702349e-005_r_kind, 1.111658e-002_r_kind, -1.050708e-005_r_kind/
    
    SAVE coe23,coe31,coe50,coe150


  ! Calculate emissivity discriminators at five AMSU window channels
    DO ich = 1, nwch
       discriminator(ich) = coe(1+(ich-1)*10)
       IF (ich .le. 2) nvalid_ch = 3
       IF (ich .ge. 3) nvalid_ch = 4
       DO i=1,nvalid_ch
          discriminator(ich) = discriminator(ich) + coe((ich-1)*10 + 2*i)*Tb_AMSUA(i) +  &
               coe((ich-1)*10 + 2*i+1)*Tb_AMSUA(i)*Tb_AMSUA(i)
       END DO
    END DO

  ! Quality Control
    IF (discriminator(4) .gt. discriminator(2))   &
         discriminator(4) = discriminator(2) + (150.0_r_kind - 89.0_r_kind)*  &
         (discriminator(5) - discriminator(2))/ &
         (150.0_r_kind - 31.4_r_kind)
    
  ! Quality control at 50.3 GHz
    IF ((discriminator(3) .gt. discriminator(2)) .or.  &
         (discriminator(3) .lt. discriminator(4)))      &
         discriminator(3) = discriminator(2) + (89.0_r_kind - 50.3_r_kind)*   &
         (discriminator(4) - discriminator(2))/(89.0_r_kind - 31.4_r_kind)
    
    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
    return
  END SUBROUTINE sem_amsua


  !---------------------------------------------------------------------!
  SUBROUTINE sem_BTs(Theta,Frequency,Tb_AMSUB,ts,Snow_Type,Emissivity_Vector)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to secenery BTs
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency        -  Frequency in GHz
  !      Theta            -  local zenith angle (not used here)
  !      ts               -  surface temperature in degree
  !      Tb_AMSUB[1] ~ Tb_AMSUB[2]  -  brightness temperature at five AMSU-B window channels:
  !                              Tb_AMSUB[1] : 89  GHz
  !                              Tb_AMSUB[2] : 150 GHz
  !
  ! output argument list:
  !
  !     Emissivity_Vector(1) and (2)  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !     Snow_Type        -  snow type
  !
  ! important internal variables:
  !
  !     coe31      - fitting coefficients to estimate discriminator at 31.4 GHz
  !     coe89      - fitting coefficients to estimate discriminator at 89   GHz
  !     coe150     - fitting coefficients to estimate discriminator at 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$
    INTEGER,PARAMETER:: nwch = 3,ncoe = 5
    REAL( r_kind )    :: Tb_AMSUB(*),Theta
    REAL( r_kind )    :: Emissivity_Vector(*),Emissivity,ts,Frequency,ed0(nwch),discriminator(5)
    INTEGER :: Snow_Type,i,k,ich,nvalid_ch
    REAL( r_kind )  :: coe31(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
    REAL( r_kind )  :: coe(N_FREQUENCIES*(ncoe+1))
    
    Equivalence (coe(1),coe31)
    Equivalence (coe(11),coe89)
    Equivalence (coe(21),coe150)
    
  ! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5 and Ts
    DATA coe31/ 3.110967e-001_r_kind,  1.100175e-002_r_kind, -1.677626e-005_r_kind,    &
         -4.020427e-003_r_kind,  9.242240e-006_r_kind, -2.363207e-003_r_kind/
  ! Fitting Coefficients at 89 GHz: Using Tb4, Tb5 and Ts
    DATA coe89/  1.148098e+000_r_kind,  1.452926e-003_r_kind,  1.037081e-005_r_kind, &
         1.340696e-003_r_kind, -5.185640e-006_r_kind, -4.546382e-003_r_kind /
  ! Fitting Coefficients at 150 GHz: Using Tb4, Tb5 and Ts
    DATA coe150/ 1.165323e+000_r_kind, -1.030435e-003_r_kind,  4.828009e-006_r_kind,  &
         4.851731e-003_r_kind, -2.588049e-006_r_kind, -4.990193e-003_r_kind/
    SAVE coe31,coe89,coe150

  ! Calculate emissivity discriminators at five AMSU window channels
    DO ich = 1, nwch
       ed0(ich) = coe(1+(ich-1)*10)
       nvalid_ch = 2
       DO i=1,nvalid_ch
          ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*Tb_AMSUB(i) +   &
               coe((ich-1)*10 + 2*i+1)*Tb_AMSUB(i)*Tb_AMSUB(i)
       END DO
       ed0(ich) = ed0(ich) + coe( (ich-1)*10 + (nvalid_ch+1)*2 )*ts
    END DO

  ! Quality control
    IF (ed0(2) .gt. ed0(1))     &
         ed0(2) = ed0(1) + (150.0_r_kind - 89.0_r_kind)*(ed0(3) - ed0(1)) / &
         (150.0_r_kind - 31.4_r_kind)

  ! Match the format of the input variable
  ! Missing value at 23.8 GHz
    discriminator(1) = -999.9_r_kind;  discriminator(2) = ed0(1)
  ! Missing value at 50.3 GHz
    discriminator(3) = -999.9_r_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
    return
  END SUBROUTINE sem_BTs


  !---------------------------------------------------------------------!
  SUBROUTINE sem_amsub(Theta,Frequency,Tb_AMSUB,Snow_Type,Emissivity_Vector)


  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to secenery AMSUB
  !
  ! program history log:
  !   2004-10-28  treadon - correct problem in declared dimensions of array coe
  !
  ! input argument list:
  !
  !      Frequency        -  Frequency in GHz
  !      Theta            -  local zenith angle (not used here)
  !      Tb_AMSUB[1] ~ Tb_AMSUB[2]  -  brightness temperature at five AMSU-B window channels:
  !                              Tb_AMSUB[1] : 89  GHz
  !                              Tb_AMSUB[2] : 150 GHz
  !
  ! output argument list:
  !     Emissivity_Vector(1) and (2)  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !     Snow_Type        -  snow type (reference [2])
  !
  ! important internal variables:
  !
  !     coe31    - fitting coefficients to estimate discriminator at 31.4 GHz
  !     coe89    - fitting coefficients to estimate discriminator at 89   GHz
  !     coe150   - fitting coefficients to estimate discriminator at 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$
    INTEGER,PARAMETER:: nwch = 3,ncoe = 4
    REAL( r_kind )    :: Tb_AMSUB(*)
    REAL( r_kind )    :: Emissivity_Vector(*),Emissivity,Frequency,ed0(nwch),discriminator(5)
    INTEGER :: Snow_Type,i,k,ich,nvalid_ch
    REAL( r_kind )  :: coe31(0:ncoe),coe89(0:ncoe),coe150(0:ncoe)
    REAL( r_kind )  :: coe(N_FREQUENCIES*(ncoe+1))
    REAL( r_kind )    :: Theta,dem,demmin0
    
    Equivalence (coe(1),coe31)
    Equivalence (coe(11),coe89)
    Equivalence (coe(21),coe150)

  ! Fitting Coefficients at 31.4 GHz: Using Tb4, Tb5
    DATA coe31/-4.015636e-001_r_kind,9.297894e-003_r_kind, -1.305068e-005_r_kind, &
         3.717131e-004_r_kind, -4.364877e-006_r_kind/
  ! Fitting Coefficients at 89 GHz: Using Tb4, Tb5
    DATA coe89/-2.229547e-001_r_kind, -1.828402e-003_r_kind,1.754807e-005_r_kind, &
         9.793681e-003_r_kind, -3.137189e-005_r_kind/
  ! Fitting Coefficients at 150 GHz: Using Tb4, Tb5
    DATA coe150/-3.395416e-001_r_kind,-4.632656e-003_r_kind,1.270735e-005_r_kind, &
         1.413038e-002_r_kind,-3.133239e-005_r_kind/
    SAVE coe31,coe89,coe150

  ! Calculate emissivity discriminators at five AMSU window channels
    DO ich = 1, nwch
       ed0(ich) = coe(1+(ich-1)*10)
       nvalid_ch = 2
       DO i=1,nvalid_ch
          ed0(ich) = ed0(ich) + coe((ich-1)*10 + 2*i)*Tb_AMSUB(i) +  &
               coe((ich-1)*10 + 2*i+1)*Tb_AMSUB(i)*Tb_AMSUB(i)
       END DO
    END DO

  ! Quality Control
    IF (ed0(2) .gt. ed0(1))     &
         ed0(2) = ed0(1) + (150.0_r_kind - 89.0_r_kind) * &
         (ed0(3) - ed0(1))/(150.0_r_kind - 31.4_r_kind)

  ! Match the format of the input variable
  ! Missing value at 23.8 GHz
    discriminator(1) = -999.9_r_kind; discriminator(2) = ed0(1)
  ! Missing value at 50.3 GHz
    discriminator(3) = -999.9_r_kind; discriminator(4) = ed0(2); discriminator(5) = ed0(3)

    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)

    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity

    return
  END SUBROUTINE sem_amsub


  !---------------------------------------------------------------------!
  SUBROUTINE ALandEM_Snow(Theta,Frequency,Snow_Depth,t_skin,Snow_Type,Emissivity_Vector)


  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity at required Frequency with respect to option MODL
  !   using the LandEM and a bias correction algorithm, where the original LandEM with a
  !   bias correction algorithm is referred to as value-added LandEM or AlandEM.
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency        -  Frequency in GHz
  !      Theta            -  local zenith angle (not used here)
  !      Snow_Depth       -  snow depth in mm
  !      t_skin           -  surface temperature
  !
  ! output argument list:
  !
  !     Emissivity_Vector(1) and (2)  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !       Snow_Type        -  snow type
  !
  ! important internal variables:
  !
  !    esv_3w and esh_3w   -  initial emissivity discriminator at two polarizations
  !                           at three AMSU window channels computed using LandEM
  !    esv_3w[1] and esh_3w[1] : 31.4 GHz
  !    esv_3w[2] and esh_3w[2] : 89   GHz
  !    esv_3w[3] and esh_3w[3] : 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER :: nw_ind
    PARAMETER(nw_ind=3)
    REAL( r_kind ) Theta, Frequency, Freq,Snow_Depth, mv, t_soil, t_skin, Emissivity_Vector(2)
    REAL( r_kind ) esv,esh,esh0,esv0,Theta0,b
    INTEGER Snow_Type,ich
    REAL( r_kind )   Freq_3w(nw_ind),esh_3w(nw_ind),esv_3w(nw_ind)
    complex( r_kind )  eair
    DATA   Freq_3w/31.4_r_kind,89.0_r_kind,150.0_r_kind/
    
    eair = CMPLX(one,-zero,r_kind)
    b = t_skin
    Snow_Type = -999
    
    CALL snowem_default(Theta,Frequency,Snow_Depth,t_skin,b,esv0,esh0)
    
    Theta0 = Theta
    DO ich = 1, nw_ind
       Freq =Freq_3w(ich)
       Theta = Theta0
       CALL snowem_default(Theta,Freq,Snow_Depth,t_skin,b,esv,esh)
       esv_3w(ich) = esv
       esh_3w(ich) = esh
    END DO
    
    CALL ems_adjust(Theta,Frequency,Snow_Depth,t_skin,esv_3w,esh_3w,Emissivity_Vector,Snow_Type)
    
    return
  END SUBROUTINE ALandEM_Snow


  !---------------------------------------------------------------------!
  SUBROUTINE snowem_default(Theta,Freq,Snow_Depth,t_soil,b,esv,esh)

  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Initialize discriminator using LandEM
  !
  ! program history log:
  !
  ! input argument list:
  !
  !      Frequency        -  Frequency in GHz
  !      Theta            -  local zenith angle in radian
  !      Snow_Depth       -  snow depth in mm
  !      t_skin           - surface temperature
  !
  ! output argument list:
  !
  !       esv              -  initial discriminator at vertical polarization
  !       esh              -                        at horizontal polarization
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    REAL( r_kind ) rhob,rhos,sand,clay
    Parameter(rhob = 1.18_r_kind, rhos = 2.65_r_kind, &
         sand = 0.8_r_kind, clay = 0.2_r_kind)
    REAL( r_kind ) Theta, Freq, mv, t_soil, Snow_Depth,b
    REAL( r_kind ) Theta_i,Theta_t, mu, r12_h, r12_v, r21_h, r21_v, r23_h, r23_v, &
         t21_v, t21_h, t12_v, t12_h, gv, gh, ssalb_h,ssalb_v,tau_h,     &
         tau_v, esh, esv,rad, sigma, va ,ep_REAL,ep_imag
    complex( r_kind ) esoil, esnow, eair
    
    eair = CMPLX(one,-zero,r_kind)
  !     ep = CMPLX(3.2_r_kind,-0.0005_r_kind,r_kind)
    sigma = one
    Theta_i  = Theta
    mv = 0.1_r_kind
    ep_REAL = 3.2_r_kind
    ep_imag = -0.0005_r_kind
    va = 0.4_r_kind + 0.0004_r_kind*Snow_Depth
    rad = one + 0.005_r_kind*Snow_Depth

    CALL snow_diel(Freq, ep_REAL, ep_imag, rad, va, esnow)
  !    CALL snow_diel(Freq, ep, rad, va, esnow)
    CALL soil_diel(Freq, t_soil, mv, rhob, rhos, sand, clay, esoil)
    Theta_t = ASIN(REAL(SIN(Theta_i)*SQRT(eair)/SQRT(esnow),r_kind))
    CALL reflectance(eair, esnow, Theta_i, Theta_t, r12_v, r12_h)
    CALL transmitance(eair, esnow, Theta_i, Theta_t, t12_v, t12_h)
    
    Theta_t  = Theta
    Theta_i = ASIN(REAL(SIN(Theta_t)*SQRT(eair)/SQRT(esnow),r_kind))
    CALL reflectance(esnow, eair, Theta_i,  Theta_t, r21_v, r21_h)
    CALL transmitance(esnow, eair, Theta_i, Theta_t, t21_v, t21_h)
    
    mu  = COS(Theta_i)
    Theta_t = ASIN(REAL(SIN(Theta_i)*SQRT(esnow)/SQRT(esoil),r_kind))
    CALL reflectance(esnow, esoil, Theta_i, Theta_t, r23_v, r23_h)
    CALL rough_reflectance(Freq, Theta_i, sigma, r23_v, r23_h)

  !    CALL snow_optic(Freq, rad, Snow_Depth, va, ep, gv, gh, ssalb_v, ssalb_h, tau_v, tau_h)
    CALL snow_optic(Freq,rad,Snow_Depth,va,ep_REAL, ep_imag,gv,gh,&
         ssalb_v,ssalb_h,tau_v,tau_h)
    
    CALL two_stream_solution(b,mu,gv,gh,ssalb_h, ssalb_v, tau_h, tau_v, r12_h, &
         r12_v, r21_h, r21_v, r23_h, r23_v, t21_v, t21_h, t12_v, t12_h, esv, esh)

  END SUBROUTINE snowem_default


  !---------------------------------------------------------------------!
  SUBROUTINE ems_adjust(Theta,Frequency,depth,ts,esv_3w,esh_3w,Emissivity_Vector,Snow_Type)


  !$$$  subprogram Documentation block
  !
  ! subprogram:
  !
  !   prgmmr: Banghua Yan                 org: nesdis              date: 2003-08-18
  !
  ! abstract:
  !         Calculate the emissivity discriminators and interpolate/extrapolate
  !  emissivity at required Frequency with respect to secenery MODL
  !
  ! program history log:
  !   2004-10-28  treadon - remove nch from parameter declaration below (not used)
  !
  ! input argument list:
  !
  !      Frequency   -  Frequency in GHz
  !      Theta       -  local zenith angle (not used here)
  !      depth       -  snow depth in mm
  !      ts          -  surface temperature
  !
  ! output argument list:
  !
  !     Emissivity_Vector(1) and (2)  -  emissivity at two polarizations.
  !                              set esv = esh here and will be updated
  !     Snow_Type        -  snow type
  !
  ! important internal variables:
  !
  !     dem_coe  -  fitting coefficients to compute discriminator correction value
  !              dem_coe[1,*]   : 31.4 GHz
  !              dem_coe[2,*]   : 89   GHz
  !              dem_coe[3,*]   : 150  GHz
  !
  ! remarks:
  !
  ! attributes:
  !   language: f90
  !   machine:  ibm rs/6000 sp
  !
  !$$$

    INTEGER,PARAMETER:: nw_3=3
    INTEGER,PARAMETER:: ncoe=6
    REAL( r_kind ),PARAMETER  :: earthrad = 6374._r_kind, satheight = 833.4_r_kind
    INTEGER         :: Snow_Type,ich,j,k
    REAL( r_kind )    :: Theta,Frequency,depth,ts,esv_3w(*),esh_3w(*)
    REAL( r_kind )    :: discriminator(5),emmod(nw_3),dem(nw_3)
    REAL( r_kind )    :: Emissivity,Emissivity_Vector(2)
    REAL(r_DOuble)  :: dem_coe(nw_3,0:ncoe-1),sinThetas,cosThetas
    
    SAVE  dem_coe
    
    DATA (dem_coe(1,k),k=0,ncoe-1)/ 2.306844e+000_r_DOuble, -7.287718e-003_r_DOuble, &
         -6.433248e-004_r_DOuble,  1.664216e-005_r_DOuble,  &
         4.766508e-007_r_DOuble, -1.754184e+000_r_DOuble/
    DATA (dem_coe(2,k),k=0,ncoe-1)/ 3.152527e+000_r_DOuble, -1.823670e-002_r_DOuble, &
         -9.535361e-004_r_DOuble,  3.675516e-005_r_DOuble,  &
         9.609477e-007_r_DOuble, -1.113725e+000_r_DOuble/
    DATA (dem_coe(3,k),k=0,ncoe-1)/ 3.492495e+000_r_DOuble, -2.184545e-002_r_DOuble,  &
         6.536696e-005_r_DOuble,  4.464352e-005_r_DOuble, &
         -6.305717e-008_r_DOuble, -1.221087e+000_r_DOuble/
    
    sinThetas = SIN(Theta*deg2rad)* earthrad/(earthrad + satheight)
    sinThetas = sinThetas*sinThetas
    cosThetas = one - sinThetas
    DO ich = 1, nw_3
       emmod(ich) = cosThetas*esv_3w(ich) + sinThetas*esh_3w(ich)
    END DO
    DO ich=1,nw_3
       dem(ich) = dem_coe(ich,0) + dem_coe(ich,1)*ts + dem_coe(ich,2)*depth +   &
            dem_coe(ich,3)*ts*ts + dem_coe(ich,4)*depth*depth         +   &
            dem_coe(ich,5)*emmod(ich)
    END DO
    emmod(1) = emmod(1) + dem(1)
    emmod(2) = emmod(2) + dem(2)
    emmod(3) = emmod(3) + dem(3)

  ! Match the format of the input variable

  ! Missing value at 23.8 GHz
    discriminator(1) = -999.9_r_kind; discriminator(2) = emmod(1)

  ! Missing value at 50.3 GHz
    discriminator(3) = -999.9_r_kind; discriminator(4) = emmod(2); discriminator(5) = emmod(3)

    CALL em_interpolate(Frequency,discriminator,Emissivity,Snow_Type)
    
    Emissivity_Vector(1) = Emissivity
    Emissivity_Vector(2) = Emissivity
    
  END SUBROUTINE ems_adjust

END MODULE MWSE_Snow


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2004/12/09 20:07:10 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: MWSE_Snow.f90,v $
! Revision 1.3  2004/12/09 20:07:10  paulv
! - Renamed module.
! - Removed all the snow related parameters and placed them in the
!   MWSE_Snow_Parameters module which this module now uses.
!
! Revision 1.2  2004/12/09 19:44:06  paulv
! - Added all the snow spectra separately as parameters and then collected
!   them into a combined array.
! - Added snow types as parameters.
! - Refactored the following subroutines:
!    o snwem_amsu
!    o em_initialization
!
! Revision 1.1  2004/12/08 19:22:06  paulv
! Initial checkin of conversion of microwave surface emissivity software to
! modular form. Only snwem_amsu implemented here.
!
!
!
