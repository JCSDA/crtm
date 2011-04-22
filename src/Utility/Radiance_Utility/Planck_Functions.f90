!
! Planck_Functions
!
! Module containing Planck function radiance, temperature, dB/dT, and 
! dT/dB routines.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 14-Oct-1999
!                     paul.vandelst@noaa.gov
!

MODULE Planck_Functions

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Modules used
  USE Type_Kinds           , ONLY: fp
  USE Fundamental_Constants, ONLY: C_1, C_2
  USE Message_Handler      , ONLY: SUCCESS, FAILURE, Display_Message
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  PUBLIC :: Planck_Radiance
  PUBLIC :: Planck_Temperature
  PUBLIC :: Planck_dBdT
  PUBLIC :: Planck_dTdB


  ! ---------------------
  ! Procedure overloading
  ! ---------------------
  INTERFACE Planck_Radiance
    MODULE PROCEDURE scalar_Planck_Radiance       ! Main function
    MODULE PROCEDURE R1_x_S_t_Planck_Radiance     ! I/P Rank1  x,  Scalar Temperature
    MODULE PROCEDURE S_x_R1_t_Planck_Radiance     ! I/P Scalar x,  Rank1  Temperature
    MODULE PROCEDURE R1_xt_Planck_Radiance        ! I/P Rank1  x,  Rank1  Temperature
    MODULE PROCEDURE R1_x_R2_t_Planck_Radiance    ! I/P Rank1  x,  Rank2  Temperature
  END INTERFACE Planck_Radiance

  INTERFACE Planck_Temperature
    MODULE PROCEDURE scalar_Planck_Temperature    ! Main function
    MODULE PROCEDURE R1_x_S_r_Planck_Temperature  ! I/P Rank1  x,  Scalar Radiance
    MODULE PROCEDURE S_x_R1_r_Planck_Temperature  ! I/P Scalar x,  Rank1  Radiance
    MODULE PROCEDURE R1_xr_Planck_Temperature     ! I/P Rank1  x,  Rank1  Radiance
    MODULE PROCEDURE R1_x_R2_r_Planck_Temperature ! I/P Rank1  x,  Rank2  Radiance
  END INTERFACE Planck_Temperature

  INTERFACE Planck_dBdT
    MODULE PROCEDURE scalar_Planck_dBdT           ! Main function
    MODULE PROCEDURE R1_x_S_t_Planck_dBdT         ! I/P Rank1  x,  Scalar Temperature
    MODULE PROCEDURE S_x_R1_t_Planck_dBdT         ! I/P Scalar x,  Rank1  Temperature
    MODULE PROCEDURE R1_xt_Planck_dBdT            ! I/P Rank1  x,  Rank1  Temperature
    MODULE PROCEDURE R1_x_R2_t_Planck_dBdT        ! I/P Rank1  x,  Rank2  Temperature
  END INTERFACE Planck_dBdT

  INTERFACE Planck_dTdB
    MODULE PROCEDURE scalar_Planck_dTdB           ! Main function
    MODULE PROCEDURE R1_x_S_r_Planck_dTdB         ! I/P Rank1  x,  Scalar Radiance
    MODULE PROCEDURE S_x_R1_r_Planck_dTdB         ! I/P Scalar x,  Rank1  Radiance
    MODULE PROCEDURE R1_xr_Planck_dTdB            ! I/P Rank1  x,  Rank1  Radiance
    MODULE PROCEDURE R1_x_R2_r_Planck_dTdB        ! I/P Rank1  x,  Rank2  Radiance
  END INTERFACE Planck_dTdB


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
    '$Id$'
  ! Keyword set value
  INTEGER, PARAMETER :: SET = 1
  ! Numeric literals
  REAL(fp), PARAMETER :: ONE = 1.0_fp
  ! Floating point precision
  REAL(fp), PARAMETER :: TOLERANCE = EPSILON(ONE)
  ! Unit types
  INTEGER, PARAMETER :: N_UNIT_TYPES = 2
  INTEGER, PARAMETER :: FREQUENCY_INDEX  = 1
  INTEGER, PARAMETER :: WAVELENGTH_INDEX = 2
  ! Scale factors. One for each unit type.
  REAL(fp), PARAMETER :: RADIANCE_SCALE_FACTOR(N_UNIT_TYPES) = (/  1000.0_fp, 1.0_fp /) 
  REAL(fp), PARAMETER :: C_1_SCALE_FACTOR(N_UNIT_TYPES)      = (/ 1.0e+08_fp, 1.0e+24_fp /)
  REAL(fp), PARAMETER :: C_2_SCALE_FACTOR(N_UNIT_TYPES)      = (/   100.0_fp, 1.0e+06_fp /)


CONTAINS


!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_Radiance
!
! PURPOSE:
!       Function to calculate the Planck Radiance given the spectral ordinate
!       (frequency or wavelength) and temperature.
!
! CALLING SEQUENCE:
!       Error_Status = Planck_Radiance( x                                , & ! Input
!                                       Temperature                      , & ! Input
!                                       Radiance                         , & ! Output
!                                       Wavelength_Units=Wavelength_Units  ) ! Optional input
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or Rank-1
!                                       See output radiance dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
!       Temperature:        Temperature(s) for which the Planck Radiance(s)
!                           is(are) required. Can be a SCALAR or VECTOR.
!                           See Radiance output description for allowed
!                           dimensionality.
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output radiance dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Radiance:           The Planck Radiance(s) for the supplied Temperature(s).
!                           The output dimensions are determined by the input.
!                           In the following chart N == frequencies, K == temperatures or
!                           spectra (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Temperature    Output Radiance
!                              dimension       dimension            dimension
!                           ------------------------------------------------------
!                               scalar          scalar                scalar
!                                 N             scalar                  N
!                               scalar            K                     K
!                                 N               N                     N
!                                 N               K                not allowed
!                                 N             N x K                 N x K
!
!                           UNITS:      mW/(m2.sr.cm-1) 
!                                         or
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                  } DEFAULT
!                                    Ouptut Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                              == 1, Input x units are microns
!                                    Ouptut Radiance units are W/(m2.sr.micron)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! PROCEDURE:
!       For frequency input, the Planck Radiance is calculated using:
!
!                   c1 * frequency^3
!         B =  ---------------------------
!                  ( c2 * frequency )
!               EXP( -------------- ) - 1
!                  (        T       )
!
!       For wavelength input:
!
!                                    c1
!         B = --------------------------------------------------
!                             [    (        c2      )     ]
!              wavelength^5 * [ EXP( -------------- ) - 1 ]
!                             [    ( wavelength * T )     ]
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar                scalar                #
  !############################################################################

  FUNCTION scalar_Planck_Radiance( x               , &  ! Input
                                   Temperature     , &  ! Input
                                   Radiance        , &  ! Output
                                   Wavelength_Units) &  ! Optional input
                                 RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Temperature
    REAL(fp),               INTENT(OUT) :: Radiance
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance(scalar)'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: Frequency, Wavelength
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Exponential

    ! Set up
    ! ------
    Error_Status = SUCCESS
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    Unit_Index      = FREQUENCY_INDEX
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT(Wavelength_Units) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

    ! Calculate spectral parameters
    ! -----------------------------
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Frequency = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency
    ELSE
      ! Spectral Units in microns
      Wavelength = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength
    END IF

    ! Calculate radiance
    ! ------------------
    Exponential = EXP(x_c_2/Temperature)
    Radiance    = RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 / ( Exponential - ONE )

  END FUNCTION scalar_Planck_Radiance


  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                 N            scalar                  N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_t_Planck_Radiance( x               , &  ! Input
                                     Temperature     , &  ! Input
                                     Radiance        , &  ! Output
                                     Wavelength_Units) &  ! Optional input
                                   RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)         ! N
    REAL(fp),               INTENT(IN)  :: Temperature
    REAL(fp),               INTENT(OUT) :: Radiance(:)  ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance(N,scalar,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Radiance) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X input and Radiance output.', &
                            Error_Status )
      RETURN
    ENDIF
    
    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_Radiance( x(i), Temperature, Radiance(i), &
                                             Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_x_S_t_Planck_Radiance


  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar            K                     K                   #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_t_Planck_Radiance( x               , &  ! Input
                                     Temperature     , &  ! Input
                                     Radiance        , &  ! Output
                                     Wavelength_Units) &  ! Optional input
                                   RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Temperature(:)  ! K
    REAL(fp),               INTENT(OUT) :: Radiance(:)     ! K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance(scalar,K,K)'
    ! Local variables
    INTEGER :: i, k


    ! Check input
    ! -----------
    k = SIZE(Temperature)
    IF ( SIZE(Radiance) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature and Radiance arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over temperature points
    ! ----------------------------
    DO i = 1, k
      Error_Status = scalar_Planck_Radiance( x, &
                                             Temperature(i), &
                                             Radiance(i), &
                                             Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION S_x_R1_t_Planck_Radiance


  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N               N                     N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xt_Planck_Radiance( x               , &  ! Input
                                  Temperature     , &  ! Input
                                  Radiance        , &  ! Output
                                  Wavelength_Units) &  ! Optional input
                                RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)            ! N
    REAL(fp),               INTENT(IN)  :: Temperature(:)  ! N
    REAL(fp),               INTENT(OUT) :: Radiance(:)     ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance(N,N,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Temperature) /= n .OR. &
         SIZE(Radiance)    /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Temperature, and Radiance arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_Radiance( x(i), Temperature(i), Radiance(i), &
                                             Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_xt_Planck_Radiance


  !############################################################################
  !#              Input X     Input Temperature    Output Radiance            #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N             N x K                 N x K                 #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_t_Planck_Radiance( x               , &  ! Input
                                      Temperature     , &  ! Input
                                      Radiance        , &  ! Output
                                      Wavelength_Units) &  ! Optional input
                                    RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)              ! N
    REAL(fp),               INTENT(IN)  :: Temperature(:,:)  ! N x K
    REAL(fp),               INTENT(OUT) :: Radiance(:,:)     ! N x K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Radiance(N,NxK,NxK)'
    ! Local variables
    INTEGER :: i, j, n, k

    ! Check input
    ! -----------
    n = SIZE(Temperature,DIM=1)
    k = SIZE(Temperature,DIM=2)
    IF ( SIZE(x)              /= n .OR. &
         SIZE(Radiance,DIM=1) /= n .OR. &
         SIZE(Radiance,DIM=2) /= k      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Temperature, and Radiance arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral and temperature points
    ! -----------------------------------------
    Spectra_Loop: DO j = 1, k
      Frequency_loop: DO i = 1, n
        Error_Status = scalar_Planck_Radiance( x(i), Temperature(i,j), Radiance(i,j), &
                                               Wavelength_Units=Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) EXIT Spectra_loop
      END DO Frequency_loop
    END DO Spectra_loop

  END FUNCTION R1_x_R2_t_Planck_Radiance




!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_Temperature
!
! PURPOSE:
!       Function to calculate the Planck temperature given the spectral
!       ordinate (frequency or wavelength), and radiance.
!
! CALLING SEQUENCE:
!       Error_Status = Planck_Temperature( x                                , & ! Input
!                                          Radiance                         , & ! Input
!                                          Temperature                      , & ! Output
!                                          Wavelength_Units=Wavelength_Units  ) ! Optional input
!  
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or Rank-1
!                           ATTRIBUTES: INTENT(IN)
!
!       Radiance:           Planck radiance(s) for which temperature(s) are
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1) OR W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output Temperature dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Temperature:        The brightness temperature(s) corresponding to the
!                           supplied radiance(s). The output dimensions are
!                           determined by the input. In the following chart
!                           N == frequencies, K == temperatures or spectra
!                           (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Radiance     Output Temperature
!                              dimension       dimension           dimension
!                           ------------------------------------------------------
!                               scalar          scalar               scalar
!                                 N             scalar                 N
!                               scalar            K                    K
!                                 N               N                    N
!                                 N               K               not allowed
!                                 N             N x K                N x K
!
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                 } DEFAULT
!                                    Input Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                              == 1, Input x units are microns
!                                    Input Radiance units are W/(m2.sr.micron)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! PROCEDURE:
!       For frequency input, the Planck temperature is calculated using:
!
!                    c2 * frequency
!         T = -----------------------------
!                ( c1 * frequency^3      )
!              LN( ---------------- +  1 )
!                (          B            )
!
!       For wavelength input :
!
!                                c2
!          T = -----------------------------------------
!                              (        c1            )
!               wavelength * LN( ---------------- + 1 )
!                              ( wavelength^5 * B     )
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiance results to return
!       Radiances in the Units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar               scalar                 #
  !############################################################################

  FUNCTION scalar_Planck_Temperature( x               , &  ! Input
                                      Radiance        , &  ! Input
                                      Temperature     , &  ! output
                                      Wavelength_Units) &  ! Optional input
                                    RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Radiance
    REAL(fp),               INTENT(OUT) :: Temperature
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature(scalar)'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: Frequency, Wavelength
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Logarithm

    ! Setup
    ! -----
    Error_Status = SUCCESS
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    Unit_Index      = FREQUENCY_INDEX
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

    ! Calculate spectral parameters
    ! -----------------------------
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Frequency = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency
    ELSE
      ! Spectral Units in microns
      Wavelength = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength
    END IF

    ! Calculate temperature
    ! ---------------------
    Logarithm   = LOG( ( RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 / Radiance ) + ONE )
    Temperature = x_c_2 / Logarithm

  END FUNCTION scalar_Planck_Temperature


  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             scalar                 N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_r_Planck_Temperature( x               , &  ! Input
                                        Radiance        , &  ! Input
                                        Temperature     , &  ! output
                                        Wavelength_Units) &  ! Optional input
                                      RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)            ! N
    REAL(fp),               INTENT(IN)  :: Radiance
    REAL(fp),               INTENT(OUT) :: Temperature(:)  ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature(N,scalar,N)'
    ! Local variables
    INTEGER :: i, n
    
    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Temperature) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and Temperature arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_Temperature( x(i), Radiance, Temperature(i), &
                                                Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_x_S_r_Planck_Temperature


  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar            K                    K                    #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_r_Planck_Temperature( x               , &  ! Input
                                        Radiance        , &  ! Input
                                        Temperature     , &  ! output
                                        Wavelength_Units) &  ! Optional input
                                      RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Radiance(:)     ! K
    REAL(fp),               INTENT(OUT) :: Temperature(:)  ! K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature(scalar,K,K)'
    ! Local variables
    INTEGER :: i, k


    ! Check input
    ! -----------
    k = SIZE(Radiance)
    IF ( SIZE(Temperature) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Radiance and Temperature arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over temperature points
    ! ----------------------------
    DO i = 1, k
      Error_Status = scalar_Planck_Temperature( x, Radiance(i), Temperature(i), &
                                                Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION S_x_R1_r_Planck_Temperature


  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N               N                    N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xr_Planck_Temperature( x               , &  ! Input
                                     Radiance        , &  ! Input
                                     Temperature     , &  ! output
                                     Wavelength_Units) &  ! Optional input
                                   RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)            ! N
    REAL(fp),               INTENT(IN)  :: Radiance(:)     ! N
    REAL(fp),               INTENT(OUT) :: Temperature(:)  ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature(N,N,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Radiance)    /= n .OR. &
         SIZE(Temperature) /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Radiance, and Temperature arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_Temperature( x(i), Radiance(i), Temperature(i), &
                                                Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_xr_Planck_Temperature


  !############################################################################
  !#              Input X     Input Radiance     Output Temperature           #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             N x K                N x K                  #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_r_Planck_Temperature( x               , &  ! Input
                                         Radiance        , &  ! Input
                                         Temperature     , &  ! output
                                         Wavelength_Units) &  ! Optional input
                                       RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)              ! N
    REAL(fp),               INTENT(IN)  :: Radiance(:,:)     ! N x K
    REAL(fp),               INTENT(OUT) :: Temperature(:,:)  ! N x K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_Temperature(N,NxK,NxK)'
    ! Local variables
    INTEGER :: i, j, n, k

    ! Check input
    ! -----------
    n = SIZE(Radiance,DIM=1)
    k = SIZE(Radiance,DIM=2)
    IF ( SIZE(x)                 /= n .OR. &
         SIZE(Temperature,DIM=1) /= n .OR. &
         SIZE(Temperature,DIM=2) /= k      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Radiance, and Temperature arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral and temperature points
    ! -----------------------------------------
    Spectra_Loop: DO j = 1, k
      Frequency_loop: DO i = 1, n
        Error_Status = scalar_Planck_Temperature( x(i), Radiance(i,j), Temperature(i,j), &
                                                  Wavelength_Units=Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) EXIT Spectra_loop
      END DO Frequency_loop
    END DO Spectra_loop

  END FUNCTION R1_x_R2_r_Planck_Temperature


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_dBdT
!
! PURPOSE:
!       Function to calculate the derivative of the Planck radiance with
!       respect to temperature given the spectral ordinate (frequency or
!       wavelength) and temperature.
!
! CALLING SEQUENCE:
!       Error_Status = Planck_dBdT( x                                , &  ! Input
!                                   Temperature                      , &  ! Input
!                                   dBdT                             , &  ! Output
!                                   Wavelength_Units=Wavelength_Units  )  ! Optional input
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or Rank-1
!                                       See output dBdT dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
!       Temperature:        Temperature(s) for which the dBdT(s) is(are)
!                           required.
!                           UNITS:      Kelvin
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output dBdT dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       dBdT:               The derivative(s) of Planck radiance with respect to
!                           temperature for the supplied temperature(s). The
!                           output dimensions are determined by the input. In the
!                           following chart N == frequencies, K == Temperatures or
!                           spectra (i.e. separate temperatures or temperature spectra):
!
!                               Input X     Input Temperature      Output dBdT
!                              dimension       dimension            dimension
!                           ------------------------------------------------------
!                               scalar          scalar                scalar
!                                 N             scalar                  N
!                               scalar            K                     K
!                                 N               N                     N
!                                 N               K                not allowed
!                                 N             N x K                 N x K
!
!                           UNITS:      mW/(m2.sr.cm-1.K) 
!                                         or
!                                       W/(m2.sr.um.K)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                } DEFAULT
!                                    Ouptut dBdT units are mW/(m2.sr.cm-1.K) } DEFAULT
!                              == 1, Input x units are microns
!                                    Ouptut dBdT units are W/(m2.sr.um.K)
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! PROCEDURE:
!       For frequency input, the Planck radiance derivative with respect
!       to temperature is calculated using :
!
!                                              ( c2 * frequency )
!                   c1 * c2 * frequency^4 * EXP( -------------- )
!                                              (        T       )             
!          dB/dT = -----------------------------------------------
!                      {     [    ( c2 * frequency )     ] }^2
!                      { T * [ EXP( -------------- ) - 1 ] }
!                      {     [    (        T       )     ] }
!
!
!       For wavelength input :
!                                            (       c2       )
!                               c1 * c2 * EXP( -------------- )
!                                            ( wavelength * T )
!          dB/dT = --------------------------------------------------------
!                                  {     [    (       c2       )     ] }^2
!                   wavelength^6 * { T * [ EXP( -------------- ) - 1 ] }
!                                  {     [    ( wavelength * T )     ] }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck radiance results to return
!       radiances in the units of mW/(m2.sr.cm-1) for frequency input
!       or W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar                scalar                #
  !############################################################################

  FUNCTION scalar_Planck_dBdT( x               , &  ! Input
                               Temperature     , &  ! Input
                               dBdT            , &  ! Output
                               Wavelength_Units) &  ! Optional input
                             RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Temperature
    REAL(fp),               INTENT(OUT) :: dBdT
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT(scalar)'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: Frequency, Wavelength
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Exponential

    ! Setup
    ! -----
    Error_Status = SUCCESS
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    Unit_Index      = FREQUENCY_INDEX
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

    ! Calculate spectral parameters
    ! -----------------------------
    IF ( Frequency_Units ) THEN
      ! Spectral Units in cm^-1
      Frequency = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * &
              C_2_SCALE_FACTOR( Unit_Index ) * C_2 * ( Frequency**4 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency
    ELSE
      ! Spectral Units in microns
      Wavelength = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * &
              C_2_SCALE_FACTOR( Unit_Index ) * C_2 / ( Wavelength**6 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength
    END IF

    ! Calculate dBdT
    ! --------------
    Exponential = EXP( x_c_2 / Temperature )
    dBdT        = RADIANCE_SCALE_FACTOR( Unit_Index ) * x_c_1 * Exponential / &
                  ( Temperature * ( Exponential - ONE ) )**2

  END FUNCTION scalar_Planck_dBdT


  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                 N            scalar                  N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_t_Planck_dBdT( x               , &  ! Input
                                 Temperature     , &  ! Input
                                 dBdT            , &  ! Output
                                 Wavelength_Units) &  ! Optional input
                               RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)         ! N
    REAL(fp),               INTENT(IN)  :: Temperature 
    REAL(fp),               INTENT(OUT) :: dBdT(:)      ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT(N,scalar,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(dBdT) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and dBdT arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_dBdT( x(i), Temperature, dBdT(i), &
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_x_S_t_Planck_dBdT


  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#              scalar            K                     K                   #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_t_Planck_dBdT( x               , &  ! Input
                                 Temperature     , &  ! Input
                                 dBdT            , &  ! Output
                                 Wavelength_Units) &  ! Optional input
                               RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Temperature(:)  ! K
    REAL(fp),               INTENT(OUT) :: dBdT(:)         ! K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT(scalar,K,K)'
    ! Local variables
    INTEGER :: i, k

    ! Check input
    ! -----------
    k = SIZE(Temperature)
    IF ( SIZE(dBdT) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Temperature and dBdT arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over temperature points
    ! ----------------------------
    DO i = 1, k
      Error_Status = scalar_Planck_dBdT( x, Temperature(i), dBdT(i), &
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION S_x_R1_t_Planck_dBdT





  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N               N                     N                   #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xt_Planck_dBdT( x               , &  ! Input
                              Temperature     , &  ! Input
                              dBdT            , &  ! Output
                              Wavelength_Units) &  ! Optional input
                            RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)            ! N
    REAL(fp),               INTENT(IN)  :: Temperature(:)  ! N
    REAL(fp),               INTENT(OUT) :: dBdT(:)         ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT(N,N,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Temperature) /= n .OR. &
         SIZE(dBdT)        /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Temperature, and dBdT arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_dBdT( x(i), Temperature(i), dBdT(i), &
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_xt_Planck_dBdT


  !############################################################################
  !#              Input X     Input Temperature      Output dBdT              #
  !#             dimension       dimension            dimension               #
  !#          ------------------------------------------------------          #
  !#                N             N x K                 N x K                 #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_t_Planck_dBdT( x               , &  ! Input
                                  Temperature     , &  ! Input
                                  dBdT            , &  ! Output
                                  Wavelength_Units) &  ! Optional input
                                RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)              ! N
    REAL(fp),               INTENT(IN)  :: Temperature(:,:)  ! N x K
    REAL(fp),               INTENT(OUT) :: dBdT(:,:)         ! N x K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dBdT(N,NxK,NxK)'
    ! Local variables
    INTEGER :: i, j, n, k


    ! Check input
    ! -----------
    n = SIZE(Temperature,DIM=1)
    k = SIZE(Temperature,DIM=2)
    IF ( SIZE(x) /= n .OR. &
         SIZE(dBdT,DIM=1) /= n .OR. &
         SIZE(dBdT,DIM=2) /= k      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Temperature, and dBdT arguments', &
                            Error_Status )
      RETURN
    ENDIF


    ! Loop over spectral and temperature points
    ! -----------------------------------------
    Spectra_loop: DO j = 1, k
      Frequency_loop: DO i = 1, n
        Error_Status = scalar_Planck_dBdT( x(i), Temperature(i,j), dBdT(i,j), &
                                           Wavelength_Units=Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) EXIT Spectra_loop
      END DO Frequency_loop
    END DO Spectra_loop

  END FUNCTION R1_x_R2_t_Planck_dBdT



!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Planck_dTdB
!
! PURPOSE:
!       Function to calculate the Planck temperature derivative with respect
!       to radiance given the spectral ordinate (frequency or wavelength)
!       and radiance.
!
! CALLING SEQUENCE:
!       Error_Status = Planck_dTdB( x                                , & ! Input
!                                   Radiance                         , & ! Input
!                                   dTdB                             , & ! Output
!                                   Wavelength_Units=Wavelength_Units  )  ! Optional input
!
!
! INPUT ARGUMENTS:
!       x:                  Spectral ordinate.
!                           UNITS:      Inverse centimetres (cm^-1)
!                                         or
!                                       Microns (um).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar or Rank-1
!                                       See output dTdB dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
!       Radiance:           Planck radiance(s) for which dTdB(s) is(are)
!                           required.
!                           UNITS:      mW/(m2.sr.cm-1)
!                                         or
!                                       W/(m2.sr.micron)
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See output dTdB dimensionality chart
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       dTdB:               The derivative(s) of Planck temperature with respect
!                           to radiance. The output dimensions are determined by
!                           the input. In the following chart N == frequencies,
!                           K == radiances or spectra (i.e. separate radiances
!                           or radiance spectra):
!
!                               Input X     Input Radiance        Output dTdB
!                              dimension       dimension           dimension
!                           ------------------------------------------------------
!                               scalar          scalar               scalar
!                                 N             scalar                 N
!                               scalar            K                    K
!                                 N               N                    N
!                                 N               K               not allowed
!                                 N             N x K                N x K
!
!                           UNITS:      (K.m2.sr.cm-1)/mW
!                                         or
!                                       (K.m2.sr.um)/W
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar, Rank-1, or Rank-2
!                                       See chart above.
!                           ATTRIBUTES: INTENT(OUT)
!
! OPTIONAL INPUT ARGUMENTS:
!       Wavelength_Units:   Set this optional argument to specify the spectral
!                           ordinate and radiance units in terms of wavelength
!                           rather than frequency (the default).
!                           If == 0, Input x units are cm^-1,                 } DEFAULT
!                                    Input Radiance units are mW/(m2.sr.cm-1) } DEFAULT
!                                    Output dTdB units are (K.m2.sr.cm-1)/mW  } DEFAULT
!                              == 1, Input x units are microns
!                                    Input Radiance units are W/(m2.sr.micron)
!                                    Output dTdB units are (K.m2.sr.um)/W
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the Message_Handler module.
!                           If == SUCCESS the Planck calculation was successful
!                              == FAILURE an error was found with the input.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! PROCEDURE:
!       For frequency input, the Planck temperature differential with respect
!       to radiance is calculated using:
!
!                               c1 * c2 * frequency^4
!  dT/dB = ------------------------------------------------------------------
!           { c1 * frequency^3     }   {       ( c1 * frequency^3      ) }^2
!           { ---------------- + 1 } * { B * LN( ---------------- +  1 ) }
!           {         B            }   {       (          B            ) }
!
!
!       For wavelength input:
!
!                                           c1 * c2
!  dT/dB = --------------------------------------------------------------------------------
!                          {        c1            }   {       (        c1            ) }^2
!           wavelength^6 * { ---------------- + 1 } * { B * LN( ---------------- + 1 ) }
!                          { wavelength^5 * B     }   {       ( wavelength^5 * B     ) }
!
!       c1 and c2 are determined using:
!          c1 = 2*h*c*c  [W.m2]
!          c2 = h*c/k    [K.m]
!
!       A scaling is applied to the Planck Radiances such that radiances
!       are in the units of mW/(m2.sr.cm-1) for frequency input or
!       W/(m2.sr.micron) for wavelength input.
!
!       FREQUENCY
!       ---------
!       To alter c1 from W.m2 to W/(m2.cm-4) a multiplier of 1.0e+08
!       is required. The solid angle is dimensionless and implied,
!       i.e. W/(m2.cm-4) => W/(m2.st.cm-4). Similarly for c2, K.m -> 
!       K.cm a multiplier of 100 is required. In addition, to return
!       mW rather than W, an additional scaling of 1000 is applied.
!
!       WAVELENGTH
!       ----------
!       To alter c1 from W.m2 to W/(m2.um-4) a multiplier of 1.0e+24
!       is required. The solid angle is dimensionless and implied, 
!       i.e. W/(m2.um-4) => W/(m2.sr.um-4). Similarly for c2, K.m ->
!       K.um a multiplier of 1.0e+06 is required.
!
!:sdoc-:
!------------------------------------------------------------------------------

  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar          scalar               scalar                 #
  !############################################################################

  FUNCTION scalar_Planck_dTdB( x               , &  ! Input
                               Radiance        , &  ! Input
                               dTdB            , &  ! Output
                               Wavelength_Units) &  ! Optional input
                             RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Radiance
    REAL(fp),               INTENT(OUT) :: dTdB
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB(scalar)'
    ! Local variables
    LOGICAL :: Frequency_Units
    INTEGER :: Unit_Index
    REAL(fp) :: Frequency, Wavelength
    REAL(fp) :: x_c_1, x_c_2
    REAL(fp) :: Scaled_Radiance
    REAL(fp) :: Argument
    
    ! Setup
    ! -----
    Error_Status = SUCCESS
    ! Default units are in terms of frequency....
    Frequency_Units = .TRUE.
    Unit_Index      = FREQUENCY_INDEX
    ! ....unless the WAVELENGTH_UNITS argument is set
    IF ( PRESENT( Wavelength_Units ) ) THEN
      IF ( Wavelength_Units == SET ) THEN
        Frequency_Units = .FALSE.
        Unit_Index      = WAVELENGTH_INDEX
      END IF
    END IF

    ! Calculate spectral parameters
    ! -----------------------------
    IF ( Frequency_Units ) THEN
      ! Spectral units in cm^-1
      Frequency = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 * ( Frequency**3 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 * Frequency
    ELSE
      ! Spectral units in microns
      Wavelength = x
      x_c_1 = C_1_SCALE_FACTOR( Unit_Index ) * C_1 / ( Wavelength**5 )
      x_c_2 = C_2_SCALE_FACTOR( Unit_Index ) * C_2 / Wavelength
    END IF

    ! Calculate dTdB
    ! --------------
    ! Radiance in terms of W
    Scaled_Radiance = Radiance / RADIANCE_SCALE_FACTOR( Unit_Index )
    ! Common term in dT/dB formulation
    Argument = ( x_c_1 / Scaled_Radiance ) + ONE
    ! Calculate dT/dB in (K.....)/W
    dTdB = x_c_1 * x_c_2 / ( Argument * ( Scaled_Radiance * LOG(Argument) )**2 )
    ! Convert dT/dB Units to (K...cm-1)/mW if required.
    dTdB = dTdB / RADIANCE_SCALE_FACTOR( Unit_Index )

  END FUNCTION scalar_Planck_dTdB





  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             scalar                 N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_x_S_r_Planck_dTdB( x               , &  ! Input
                                 Radiance        , &  ! Input
                                 dTdB            , &  ! Output
                                 Wavelength_Units) &  ! Optional input
                               RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)      ! N
    REAL(fp),               INTENT(IN)  :: Radiance
    REAL(fp),               INTENT(OUT) :: dTdB(:)   ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB(N,scalar,N)'
    ! Local variables
    INTEGER :: i, n
    
    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(dTdB) /= n ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X and dTdB arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_dTdB( x(i), Radiance, dTdB(i), &
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_x_S_r_Planck_dTdB


  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#              scalar            K                    K                    #
  !#                                                                          #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION S_x_R1_r_Planck_dTdB( x               , &  ! Input
                                 Radiance        , &  ! Input
                                 dTdB            , &  ! Output
                                 Wavelength_Units) &  ! Optional input
                               RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x
    REAL(fp),               INTENT(IN)  :: Radiance(:)
    REAL(fp),               INTENT(OUT) :: dTdB(:)
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB(scalar,K,K)'
    ! Local variables
    INTEGER :: i, k

    ! Check input
    ! -----------
    k = SIZE(Radiance)
    IF ( SIZE(dTdB) /= k ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of Radiance and dTdB arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over temperature points
    ! ----------------------------
    DO i = 1, k
      Error_Status = scalar_Planck_dTdB( x, Radiance(i), dTdB(i), &
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION S_x_R1_r_Planck_dTdB


  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N               N                    N                    #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !############################################################################

  FUNCTION R1_xr_Planck_dTdB( x               , &  ! Input
                              Radiance        , &  ! Input
                              dTdB            , &  ! Output
                              Wavelength_Units) &  ! Optional input
                            RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)         ! N
    REAL(fp),               INTENT(IN)  :: Radiance(:)  ! N
    REAL(fp),               INTENT(OUT) :: dTdB(:)      ! N
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB(N,N,N)'
    ! Local variables
    INTEGER :: i, n

    ! Check input
    ! -----------
    n = SIZE(x)
    IF ( SIZE(Radiance) /= n .OR. &
         SIZE(dTdB)     /= n      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Radiance, and dTdB arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral points
    ! -------------------------
    DO i = 1, n
      Error_Status = scalar_Planck_dTdB( x(i), Radiance(i), dTdB(i), & 
                                         Wavelength_Units=Wavelength_Units )
      IF ( Error_Status /= SUCCESS ) EXIT
    END DO

  END FUNCTION R1_xr_Planck_dTdB


  !############################################################################
  !#              Input X     Input Radiance        Output dTdB               #
  !#             dimension       dimension           dimension                #
  !#          ------------------------------------------------------          #
  !#                N             N x K                N x K                  #
  !#                                                                          #
  !# N == number of spectral points                                           #
  !# K == number of temperature points                                        #
  !############################################################################

  FUNCTION R1_x_R2_r_Planck_dTdB( x               , &  ! Input
                                  Radiance        , &  ! Input
                                  dTdB            , &  ! Output
                                  Wavelength_Units) &  ! Optional input
                                RESULT( Error_Status )
    ! Arguments
    REAL(fp),               INTENT(IN)  :: x(:)           ! N
    REAL(fp),               INTENT(IN)  :: Radiance(:,:)  ! N x K
    REAL(fp),               INTENT(OUT) :: dTdB(:,:)      ! N x K
    INTEGER,      OPTIONAL, INTENT(IN)  :: Wavelength_Units
    ! Result status
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Planck_dTdB(N,NxK,NxK)'
    ! Local variables
    INTEGER :: i, j, n, k

    ! Check input
    ! -----------
    n = SIZE(Radiance,DIM=1)
    k = SIZE(Radiance,DIM=2)
    IF ( SIZE(x)          /= n .OR. &
         SIZE(dTdB,DIM=1) /= n .OR. &
         SIZE(dTdB,DIM=2) /= k      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Inconsistent size of X, Radiance, and dTdB arguments', &
                            Error_Status )
      RETURN
    ENDIF

    ! Loop over spectral and temperature points
    ! -----------------------------------------
    Spectra_loop: DO j = 1, k
      Frequency_loop: DO i = 1, n
        Error_Status = scalar_Planck_dTdB( x(i), Radiance(i,j), dTdB(i,j), &
                                           Wavelength_Units=Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) EXIT Spectra_loop
      END DO Frequency_loop
    END DO Spectra_loop

  END FUNCTION R1_x_R2_r_Planck_dTdB

END MODULE Planck_Functions
