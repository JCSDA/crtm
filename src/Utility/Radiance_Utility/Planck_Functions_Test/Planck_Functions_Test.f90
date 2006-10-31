!
! Planck_Functions_Test
!
! Program to test the Planck function module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 14-Oct-1999
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Planck_Functions_Test

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds, fp=>fp_kind
  USE Message_Handler
  USE Planck_Functions
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Planck_Functions_Test'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Planck_Functions_Test.f90,v 1.6 2006/08/15 21:24:50 wd20pd Exp $'
  ! Unit name strings
  INTEGER,      PARAMETER :: N_UNITS = 2
  CHARACTER(*), PARAMETER, DIMENSION(N_UNITS) :: V_UNIT_STRING  = (/ 'frequency (cm-1)', &
                                                                     'wavelength (um) ' /)
  CHARACTER(*), PARAMETER, DIMENSION(N_UNITS) :: R_UNIT_STRING  = (/ 'mW/(m2.sr.cm-1)', &
                                                                     'W/(m2.sr.um)   ' /)
  CHARACTER(*), PARAMETER, DIMENSION(N_UNITS) :: D1_UNIT_STRING = (/ 'mW/(m2.sr.cm-1.K)', &
                                                                     'W/(m2.sr.um.K)   ' /)
  CHARACTER(*), PARAMETER, DIMENSION(N_UNITS) :: D2_UNIT_STRING = (/ '(K.m2.sr.cm-1)/mW', &
                                                                     '(K.m2.sr.um)/W   ' /)
  ! Array sizes
  INTEGER,      PARAMETER :: N_FREQUENCIES  = 5
  INTEGER,      PARAMETER :: N_TEMPERATURES = 3

  ! Number of perturbations and the size
  INTEGER,  PARAMETER :: N_PERTURBATIONS = 11
  REAL(fp), PARAMETER :: D_PERTURBATION  = 1.0_fp

  ! Default temeprature
  REAL(fp), PARAMETER :: T0 = 300.0_fp


  ! ---------
  ! Variables
  ! ---------

  INTEGER :: Error_Status
  INTEGER :: i, j, n
  CHARACTER( 80 ) :: Output_Fmt
  REAL(fp), DIMENSION(N_FREQUENCIES) :: Frequency
  REAL(fp), DIMENSION(N_FREQUENCIES) :: x
  REAL(fp), DIMENSION(N_FREQUENCIES,N_TEMPERATURES) :: Radiance
  REAL(fp), DIMENSION(N_FREQUENCIES,N_TEMPERATURES) :: Temperature
  REAL(fp), DIMENSION(N_FREQUENCIES,N_TEMPERATURES) :: dBdT
  REAL(fp), DIMENSION(N_FREQUENCIES,N_TEMPERATURES) :: dTdB
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: Temperature_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: Radiance_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dTemperature_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dRadiance_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dRadiance_dBdT
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dTemperature_dTdB
  INTEGER :: Wavelength_Units


  ! Output program header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the Planck functions module routines for '//&
                       'scalar, rank-1, and rank-2 input.', &
                       '$Revision: 1.6 $' )

  ! Set up
  DO i = 1, N_FREQUENCIES
    Frequency( i ) = 800.0_fp + ( REAL( i-1, fp ) * 400.0_fp )
  END DO
  DO i = 1, N_TEMPERATURES
    Temperature( :, i ) = T0 + ( REAL( i-1, fp ) * 5.0_fp )
  END DO

  WRITE( Output_Fmt, '( "(",i2,"(1x,f12.6))" )' ) N_FREQUENCIES

  ! Begin generic interface check
  WRITE( *, '( //15x, "GENERIC INTERFACE CHECK", //)' )

  ! Loop over spectral ordinate units
  Unit_Loop: DO i = 1, n_Units

    ! Determine input units and unit flag
    IF ( i == 1 ) THEN
      x = Frequency
      Wavelength_Units = 0
    ELSE
      x   = 10000.0_fp / Frequency
      Wavelength_Units = 1
    END IF

    ! Calculate Radiances
    WRITE( *, '( /5x, "TEMPERATURE (K) -> RADIANCE (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Scalar Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(1), &
                                    Temperature(1,1), &
                                    Radiance(1,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Nx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(1,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)
    WRITE( *, '( 5x, "Output: Kx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(1), &
                                    Temperature(1,:), &
                                    Radiance(1,:), &
                                    Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)
    WRITE( *, '( 5x, "Output: Nx1 Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(:,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)
    WRITE( *, '( 5x, "Output: NxK Radiance (",a,")", /5x, 7("-") )' ) TRIM( R_UNIT_STRING( i ) )
    Error_Status = Planck_Radiance( x(:), &
                                    Temperature(:,:), &
                                    Radiance(:,:), &
                                    Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )

    ! Calculate temperatures
    WRITE( *, '( //5x, "RADIANCE (", a, ") -> TEMPERATURE (K)..." )' ) TRIM( R_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Scalar Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(1), &
                                       Radiance(1,1), &
                                       Temperature(1,1), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Nx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(1,1), &
                                       Temperature(:,1), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)
    WRITE( *, '( 5x, "Output: Kx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(1), &
                                       Radiance(1,:), &
                                       Temperature(1,:), &
                                       Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)
    WRITE( *, '( 5x, "Output: Nx1 Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(:,1), &
                                       Temperature(:,1), &
                                       Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)
    WRITE( *, '( 5x, "Output: NxK Temperature (K)", /5x, 7("-") )' )
    Error_Status = Planck_Temperature( x(:), &
                                       Radiance(:,:), &
                                       Temperature(:,:), &
                                       Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )

    ! Calculate dB/dT
    WRITE( *, '( //5x, "TEMPERATURE (K) -> dB/dT (", a, ")..." )' ) TRIM( D1_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Scalar dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(1), &
                                Temperature(1,1), &
                                dBdT(1,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,1)
    WRITE( *, '( 5x, "Output: Nx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(1,1), &
                                dBdT(:,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(1,:)
    WRITE( *, '( 5x, "Output: Kx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(1), &
                                Temperature(1,:), &
                                dBdT(1,:), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,1)
    WRITE( *, '( 5x, "Output: Nx1 dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(:,1), &
                                dBdT(:,1), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Temperature (K)", /5x, 6("-") )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Temperature(:,:)
    WRITE( *, '( 5x, "Output: NxK dB/dT (",a,")", /5x, 7("-") )' ) TRIM( D1_UNIT_STRING( i ) )
    Error_Status = Planck_dBdT( x(:), &
                                Temperature(:,:), &
                                dBdT(:,:), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dBdT(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )

    ! Calculate dT/dB
    WRITE( *, '( //5x, "RADIANCE (", a, ") -> dT/dB (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) ), &
                                                                       TRIM( D2_UNIT_STRING( i ) )

    WRITE( *, '( /5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Scalar dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(1), &
                                Radiance(1,1), &
                                dTdB(1,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(1,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Scalar Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,1)
    WRITE( *, '( 5x, "Output: Nx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(1,1), &
                                dTdB(:,1), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Scalar ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(1)
    WRITE( *, '( 5x, "Input:  Kx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(1,:)
    WRITE( *, '( 5x, "Output: Kx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(1), &
                                Radiance(1,:), &
                                dTdB(1,:), &
                                Wavelength_Units = Wavelength_Units   )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(1,:)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  Nx1 Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,1)
    WRITE( *, '( 5x, "Output: Nx1 dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(:,1), &
                                dTdB(:,1), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,1)

    WRITE( *, '( 70("=") )' )

    WRITE( *, '( 5x, "Input:  Nx1 ", a, /5x, 6("-") )' ) V_UNIT_STRING( i )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x(:)
    WRITE( *, '( 5x, "Input:  NxK Radiance (",a,")", /5x, 6("-") )' ) TRIM( R_UNIT_STRING( i ) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) Radiance(:,:)
    WRITE( *, '( 5x, "Output: NxK dT/dB (",a,")", /5x, 7("-") )' ) TRIM( D2_UNIT_STRING( i ) )
    Error_Status = Planck_dTdB( x(:), &
                                Radiance(:,:), &
                                dTdB(:,:), &
                                Wavelength_Units = Wavelength_Units )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) dTdB(:,:)

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )
              
  END DO Unit_Loop


  ! Begin derivative routine checks
  WRITE( *, '( //15x, "DERIVATIVE ROUTINE CHECKS")' )

  ! Loop over spectral ordinate units
  Unit_Loop_Derivative_Check: DO i = 1, n_Units

    ! Determine input units and unit flag
    IF ( i == 1 ) THEN
      x = Frequency
      Wavelength_Units = 0
    ELSE
      x   = 10000.0_fp / Frequency
      Wavelength_Units = 1
    END IF

    ! Construct temperature perturbation array
    j = 0
    DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2
      j = j+1
      Temperature_NL(  :, j ) = T0 + ( REAL( n, fp ) * D_PERTURBATION )
      dTemperature_NL( :, j ) = Temperature_NL(  :, j ) - T0
    END DO

    ! Compute the dB/dT derivative
    WRITE( *, '( //5x, "TEMPERATURE (K) -> dB/dT (", a, ")..." )' ) TRIM( D1_UNIT_STRING( i ) )

    ! The finite difference
    Error_Status = Planck_Radiance( x, &
                                    Temperature_NL, &
                                    Radiance_NL, &
                                    Wavelength_Units = Wavelength_Units )

    DO j = 1, N_PERTURBATIONS
      dRadiance_NL(:,j) = Radiance_NL(:,j) - Radiance_NL(:,N_PERTURBATIONS/2+1)
    END DO

    ! The derivative
    Error_Status = Planck_dBdT( x, &
                                Temperature_NL, &
                                dRadiance_dBdT, &
                                Wavelength_Units = Wavelength_Units )

    dRadiance_dBdT = dRadiance_dBdT * dTemperature_NL

    ! Output the two datasets
    WRITE( *, '( /10x, "Finite difference dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dB/dT routine dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_dBdT(:,j)
    END DO

    WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
    READ( *, * )


    ! Compute the dT/dB derivative
    WRITE( *, '( //5x, "RADIANCE (", a, ") -> dT/dB (", a, ")..." )' ) TRIM( R_UNIT_STRING( i ) ), &
                                                                       TRIM( D2_UNIT_STRING( i ) )
    ! The finite difference
    Error_Status = Planck_Temperature( x, &
                                       Radiance_NL, &
                                       Temperature_NL, &
                                       Wavelength_Units = Wavelength_Units )

    DO j = 1, N_PERTURBATIONS
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO

    ! The derivative
    Error_Status = Planck_dTdB( x, &
                                Radiance_NL, &
                                dTemperature_dTdB, &
                                Wavelength_Units = Wavelength_Units )

    dTemperature_dTdB = dTemperature_dTdB * dRadiance_NL

    ! Output the two datasets
    WRITE( *, '( /10x, "Finite difference result (K):" )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_NL(:,j)
    END DO

    WRITE( *, '( /10x, "dT/dB routine result (K):" )' )
    WRITE( *, FMT = TRIM( Output_Fmt ) ) x
    WRITE( *, '( 70("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_dTdB(:,j)
    END DO

    IF ( i < N_UNITS ) THEN
      WRITE( *, '( /10x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF
              
  END DO Unit_Loop_Derivative_Check

END PROGRAM Planck_Functions_Test
