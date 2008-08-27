!
! Sensor_Planck_Functions_Test
!
! Program to test the sensor Planck function module
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-May-2001
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Sensor_Planck_Functions_Test

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE Message_Handler
  USE SpcCoeff_Define
  USE SpcCoeff_netCDF_IO
  USE Sensor_Planck_Functions
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ---------- 
  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME = 'Sensor_Planck_Functions_Test'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Sensor_Planck_Functions_Test.f90,v 2.3 2006/09/21 17:58:25 wd20pd Exp $'
  ! Unit name strings
  INTEGER,      PARAMETER :: N_UNITS = 2
  CHARACTER(*), PARAMETER :: V_UNIT_STRING(N_UNITS)    = (/ 'frequency (cm-1)' , 'wavelength (um) '  /)
  CHARACTER(*), PARAMETER :: R_UNIT_STRING(N_UNITS)    = (/ 'mW/(m2.sr.cm-1)'  , 'W/(m2.sr.um)   '   /)
  CHARACTER(*), PARAMETER :: DBDT_UNIT_STRING(N_UNITS) = (/ 'mW/(m2.sr.cm-1.K)', 'W/(m2.sr.um.K)   ' /)
  CHARACTER(*), PARAMETER :: DTDB_UNIT_STRING(N_UNITS) = (/ '(K.m2.sr.cm-1)/mW', '(K.m2.sr.um)/W   ' /)

  ! -- SpcCoeff datafile name
  CHARACTER( * ),  PARAMETER :: SPCCOEFF_FILENAME = 'hirs3_n17.SpcCoeff.nc'

  ! -- Array sizes
  INTEGER,         PARAMETER :: N_CHANNELS = 19
  INTEGER,         PARAMETER :: N_TEMPERATURES = 5
  CHARACTER(*),    PARAMETER :: C_TEMPERATURES = '5'

  ! -- Number of perturbations and the size
  INTEGER,  PARAMETER :: N_PERTURBATIONS = 11
  REAL(fp), PARAMETER :: D_PERTURBATION  = 1.0_fp

  ! -- Default temeprature
  REAL(fp), PARAMETER :: T0 = 273.0_fp


  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  TYPE(SpcCoeff_type) :: SC
  CHARACTER( 12 )  ::    R_Fmt
  CHARACTER( 12 )  ::    T_Fmt
  CHARACTER( 12 )  :: dBdT_Fmt
  CHARACTER( 12 )  :: dTdB_Fmt
  CHARACTER( 200 ) :: Channel_Fmt
  CHARACTER( 200 ) :: Output_Fmt

  INTEGER :: Wavelength_Units

  INTEGER :: i, j, l, n
  REAL(fp), DIMENSION(N_TEMPERATURES) :: Temperature, Radiance, dBdT, dTdB

  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: Temperature_NL
  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: Radiance_NL
  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dTemperature_NL
  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dRadiance_NL
  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dRadiance_dBdT
  REAL(fp), DIMENSION( N_CHANNELS, N_PERTURBATIONS ) :: dTemperature_dTdB


  ! Output program header
  ! ---------------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the sensor Planck functions module routines.', &
                       '$Revision: 2.3 $' )

  ! Read the test SpcCoeff datafile
  ! -------------------------------
  Error_Status = Read_SpcCoeff_netCDF( SPCCOEFF_FILENAME, SC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading SpcCoeff file '//SPCCOEFF_FILENAME, &
                          Error_Status )
    STOP
  END IF


  ! Create temperatures
  ! -------------------
  DO j = 1, N_TEMPERATURES
    Temperature(j) = T0 + ( REAL(j-1,fp) * 5.0_fp )
  END DO


  ! =============================
  ! Begin generic interface check
  ! =============================
  WRITE( *,'(//15x,"GENERIC INTERFACE CHECK")' )

  ! Loop over spectral ordinate units
  ! ---------------------------------
  Unit_Loop_1: DO i = 1, N_UNITS

    ! Set units flag
    Wavelength_Units = i-1

    ! Loop over channels
    ! ------------------
    WRITE( *,'(/5x,"Spectral ordinate: ",a,/)' ) V_UNIT_STRING( i )
    Channel_Loop_1: DO l = 1, SC%n_Channels

      ! Loop over temperatures
      ! ----------------------
      Temperature_Loop_1: DO j = 1, N_TEMPERATURES

        ! Calculate channel values
        Error_Status = Sensor_Radiance( SC, &
                                        SC%Sensor_Channel(l), &
                                        Temperature(j), &
                                        Radiance(j), &
                                        Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_Temperature( SC, &
                                           SC%Sensor_Channel(l), &
                                           Radiance(j), &
                                           Temperature(j), &
                                           Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_dBdT( SC, &
                                    SC%Sensor_Channel(l), &
                                    Temperature(j), &
                                    dBdT(j), &
                                    Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

        Error_Status = Sensor_dTdB( SC, &
                                    SC%Sensor_Channel(l), &
                                    Radiance(j), &
                                    dTdB(j), &
                                    Wavelength_Units = Wavelength_Units )
        IF ( Error_Status /= SUCCESS ) STOP

      END DO Temperature_Loop_1


      ! Futz around with the output formats
      IF ( ANY( Radiance < 1.0e-06_fp ) ) THEN
        R_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        R_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      T_Fmt = C_TEMPERATURES//'(1x,f10.6) '

      IF ( ANY( dBdT < 1.0e-06_fp ) ) THEN
        dBdT_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        dBdT_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      IF ( ANY( ABS( dTdB ) > 999.0_fp ) ) THEN
        dTdB_Fmt = C_TEMPERATURES//'(1x,es10.3)'
      ELSE
        dTdB_Fmt = C_TEMPERATURES//'(1x,f10.6) '
      END IF

      Output_Fmt = '( /2x, "Ch. ", i2, '// &
                          '     " R     = ", '//R_Fmt//', 1x,a, '// &
                          '/8x, " T     = ", '//T_Fmt//', 1x,"K   ", '// &
                          '/8x, " dB/dT = ", '//dBdT_Fmt//', 1x,a, '// &
                          '/8x, " dT/dB = ", '//dTdB_Fmt//', 1x,a )'

      ! Write results to screen
      WRITE( *, FMT = TRIM(Output_Fmt) ) SC%Sensor_Channel(l), &
                                         Radiance, R_UNIT_STRING(i), &
                                         Temperature, &
                                         dBdT, DBDT_UNIT_STRING(i), &
                                         dTdB, DTDB_UNIT_STRING(i)

    END DO Channel_Loop_1
  END DO Unit_Loop_1



  ! ==============================
  ! Begin derivative routine check
  ! ==============================
  WRITE( *,'(//15x,"DERIVATIVE ROUTINE CHECK")' )

  ! Create the output format strings for the derivative check
  WRITE( Channel_Fmt,'("(",i2,"(4x,i2,3x))")' ) N_CHANNELS
  WRITE( Output_Fmt, '("(",i2,"(1x,f8.4))")'  ) N_CHANNELS

  ! Loop over spectral ordinate units
  ! ---------------------------------
  Unit_Loop_2: DO i = 1, N_UNITS

    ! Set units flag
    Wavelength_Units = i-1

    ! Construct temperature perturbation array
    j = 0
    DO n = -N_PERTURBATIONS/2, N_PERTURBATIONS/2
      j = j+1
      Temperature_NL(:,j)  = T0 + ( REAL(n,fp) * D_PERTURBATION )
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO

    ! Compute the dB/dT derivative
    ! ----------------------------
    WRITE( *,'(//5x,"TEMPERATURE (K) -> dB/dT (",a,")...")' ) TRIM(DBDT_UNIT_STRING(i))
    Channel_Loop_dBdT: DO l = 1, SC%n_Channels

      ! Loop over the perturbations
      ! ---------------------------
      DO j = 1, N_PERTURBATIONS
        ! The finite difference
        Error_Status = Sensor_Radiance( SC, &
                                        SC%Sensor_Channel(l), &
                                        Temperature_NL(l,j), &
                                        Radiance_NL(l,j), &
                                        Wavelength_Units = Wavelength_Units )
        ! The derivative
        Error_Status = Sensor_dBdT( SC, &
                                    SC%Sensor_Channel(l), &
                                    Temperature_NL(l,j), &
                                    dRadiance_dBdT(l,j), &
                                    Wavelength_Units = Wavelength_Units )
      END DO
    END DO Channel_Loop_dBdT

    ! Compute the radiance differences
    ! --------------------------------
    ! The finite difference
    DO j = 1, N_PERTURBATIONS
      dRadiance_NL(:,j) = Radiance_NL(:,j) - Radiance_NL(:,N_PERTURBATIONS/2+1)
    END DO
    ! The derivative difference
    dRadiance_dBdT = dRadiance_dBdT * dTemperature_NL

    ! Output the two results
    WRITE( *, '( /10x, "Finite difference dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SC%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_NL(:,j)
    END DO
    WRITE( *, '( /10x, "dB/dT routine dR result (", a, "):" )' ) TRIM( R_UNIT_STRING(i) )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SC%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dRadiance_dBdT(:,j)
    END DO


    ! Compute the dT/dB derivative
    ! ----------------------------
    WRITE( *,'(//5x,"RADIANCE (",a,") -> dT/dB (",a,")...")' ) TRIM(R_UNIT_STRING(i)), &
                                                               TRIM(DTDB_UNIT_STRING(i))
    Channel_Loop_dTdB: DO l = 1, SC%n_Channels

      ! Loop over the perturbations
      ! ---------------------------
      DO j = 1, N_PERTURBATIONS
        ! The finite difference
        Error_Status = Sensor_Temperature( SC, &
                                           SC%Sensor_Channel(l), &
                                           Radiance_NL(l,j), &
                                           Temperature_NL(l,j), &
                                           Wavelength_Units = Wavelength_Units )
        ! The derivative
        Error_Status = Sensor_dTdB( SC, &
                                    SC%Sensor_Channel(l), &
                                    Radiance_NL(l,j), &
                                    dTemperature_dTdB(l,j), &
                                    Wavelength_Units = Wavelength_Units )
      END DO
    END DO Channel_Loop_dTdB

    ! Compute the temperature differences
    ! -----------------------------------
    ! The finite difference
    DO j = 1, N_PERTURBATIONS
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO
    ! The derivative difference
    dTemperature_dTdB = dTemperature_dTdB * dRadiance_NL

    ! Output the two results
    WRITE( *, '( /10x, "Finite difference dT result (K):" )' )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SC%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_NL(:,j)
    END DO
    WRITE( *, '( /10x, "dT/dB routine dT result (K):" )' )
    WRITE( *, FMT = TRIM( Channel_Fmt ) ) SC%Sensor_Channel
    WRITE( *, '( 173("=") )' )
    DO j = 1, N_PERTURBATIONS
      WRITE( *, FMT = TRIM( Output_Fmt ) ) dTemperature_dTdB(:,j)
    END DO

  END DO Unit_Loop_2


  ! Clean up
  ! --------
  Error_Status = Destroy_SpcCoeff( SC )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error destroying SpcCoeff structure', &
                          WARNING )
  END IF

END PROGRAM Sensor_Planck_Functions_Test
