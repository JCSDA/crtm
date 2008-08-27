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
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Planck_Functions
  USE Unit_Test
  ! Disable implicit typing
  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Planck_Functions_Test'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id$'
  ! Unit name strings
  INTEGER,      PARAMETER :: N_UNITS = 2
  CHARACTER(*), PARAMETER :: V_UNIT_STRING(N_UNITS)  = (/ 'frequency (cm-1)' , 'wavelength (um) '  /)
  CHARACTER(*), PARAMETER :: R_UNIT_STRING(N_UNITS)  = (/ 'mW/(m2.sr.cm-1)'  , 'W/(m2.sr.um)   '   /)
  CHARACTER(*), PARAMETER :: D1_UNIT_STRING(N_UNITS) = (/ 'mW/(m2.sr.cm-1.K)', 'W/(m2.sr.um.K)   ' /)
  CHARACTER(*), PARAMETER :: D2_UNIT_STRING(N_UNITS) = (/ '(K.m2.sr.cm-1)/mW', '(K.m2.sr.um)/W   ' /)
  ! Array sizes
  INTEGER,      PARAMETER :: N_FREQUENCIES  = 5
  INTEGER,      PARAMETER :: N_TEMPERATURES = 3
  ! Number of perturbations and the size
  INTEGER,  PARAMETER :: N_PERTURBATIONS = 11
  REAL(fp), PARAMETER :: D_PERTURBATION  = 1.0_fp
  ! Default temperature
  REAL(fp), PARAMETER :: T0 = 300.0_fp
  ! Derivative test output file
  CHARACTER(*),PARAMETER :: OUTPUT_FILE = 'Planck_Function_Derivatives.dat'

  ! -----------
  ! Test values
  ! -----------
  ! Test threshold
  REAL(fp), PARAMETER :: THRESHOLD = 1.0e-6_fp
  ! Frequency values
  REAL(fp), PARAMETER :: FREQUENCY(N_FREQUENCIES) = &
    (/800.0_fp, 1200.0_fp, 1600.0_fp, 2000.0_fp, 2400.0_fp/)
  ! Temperature inputs
  REAL(fp), PARAMETER :: INPUT_T(N_FREQUENCIES,N_TEMPERATURES) = &
    RESHAPE( (/300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, &
               305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, &
               310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp/), &
             (/N_FREQUENCIES,N_TEMPERATURES/) )
    
  ! Radiance values
  ! ---------------
  ! Scalar
  REAL(fp), PARAMETER :: SCALAR_R(N_UNITS) = (/134.397886494_fp, 8.601464736_fp/)
  ! N,scalar,N
  REAL(fp), PARAMETER :: NsN_R(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/134.397886494_fp, 65.379243476_fp, 22.695747046_fp, 6.506777898_fp, 1.651004357_fp, &
                 8.601464736_fp,  9.414611061_fp,  5.810111244_fp, 2.602711159_fp, 0.950978509_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! scalar,K,K
  REAL(fp), PARAMETER :: sKK_R(N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/134.397886494_fp, 143.327716453_fp, 152.547833854_fp, &
                 8.601464736_fp,   9.172973853_fp,   9.763061367_fp /), &
             (/N_TEMPERATURES,N_UNITS/) )
  ! N,N,N
  REAL(fp), PARAMETER :: NNN_R(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/134.397886494_fp, 65.379243476_fp, 22.695747046_fp, 6.506777898_fp, 1.651004357_fp, &
                 8.601464736_fp,  9.414611061_fp,  5.810111244_fp, 2.602711159_fp, 0.950978509_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! N,NxK,NxK
  REAL(fp), PARAMETER :: NNKNK_R(N_FREQUENCIES,N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/134.397886494_fp, 65.379243476_fp, 22.695747046_fp, 6.506777898_fp, 1.651004357_fp, &
               143.327716453_fp, 71.870442902_fp, 25.739698101_fp, 7.614841803_fp, 1.993870494_fp, &
               152.547833854_fp, 78.767580023_fp, 29.073858783_fp, 8.866527062_fp, 2.393328679_fp, &
                 8.601464736_fp,  9.414611061_fp,  5.810111244_fp, 2.602711159_fp, 0.950978509_fp, &
                 9.172973853_fp, 10.349343778_fp,  6.589362714_fp, 3.045936721_fp, 1.148469405_fp, &
                 9.763061367_fp, 11.342531523_fp,  7.442907848_fp, 3.546610825_fp, 1.378557319_fp /), &
             (/N_FREQUENCIES,N_TEMPERATURES,N_UNITS/) )
             
  ! Temperature values
  ! ------------------
  ! Scalar
  REAL(fp), PARAMETER :: SCALAR_T(N_UNITS) = T0
  ! N,scalar,N
  REAL(fp), PARAMETER :: NsN_T(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/ 300.000000000_fp, 342.712520718_fp, 390.366438035_fp, 438.302493417_fp, 485.554153069_fp, &
                300.000000000_fp, 295.377876388_fp, 316.155340080_fp, 342.703697756_fp, 370.974277083_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! scalar,K,K
  REAL(fp), PARAMETER :: sKK_T(N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/ 300.0_fp, 305.0_fp, 310.0_fp, &
                300.0_fp, 305.0_fp, 310.0_fp /), &
             (/N_TEMPERATURES,N_UNITS/) )
  ! N,N,N
  REAL(fp), PARAMETER :: NNN_T(N_FREQUENCIES,N_UNITS) = 300.0_fp
  REAL(fp), PARAMETER :: NNKNK_T(N_FREQUENCIES,N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, &
               305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, &
               310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp, &
               300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, 300.0_fp, &
               305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, 305.0_fp, &
               310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp, 310.0_fp /), &
             (/N_FREQUENCIES,N_TEMPERATURES,N_UNITS/) )

  ! dB/dT values
  ! ------------
  ! Scalar
  REAL(fp), PARAMETER :: SCALAR_DB(N_UNITS) = (/1.756711371_fp, 0.112429528_fp/)
  ! N,scalar,N
  REAL(fp), PARAMETER :: NsN_DB(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/1.756711371_fp, 1.258198022_fp, 0.580787028_fp, 0.208054006_fp, 0.063345280_fp, &
               0.112429528_fp, 0.181180515_fp, 0.148681479_fp, 0.083221602_fp, 0.036486882_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! scalar,K,K
  REAL(fp), PARAMETER :: sKK_DB(N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/1.756711371_fp, 1.815109321_fp, 1.872819935_fp, &
               0.112429528_fp, 0.116166997_fp, 0.119860476_fp /), &
             (/N_TEMPERATURES,N_UNITS/) )
  ! N,N,N
  REAL(fp), PARAMETER :: NNN_DB(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/1.756711371_fp, 1.258198022_fp, 0.580787028_fp, 0.208054006_fp, 0.063345280_fp, &
               0.112429528_fp, 0.181180515_fp, 0.148681479_fp, 0.083221602_fp, 0.036486882_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! N,NxK,NxK
  REAL(fp), PARAMETER :: NNKNK_DB(N_FREQUENCIES,N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/1.756711371_fp, 1.258198022_fp, 0.580787028_fp, 0.208054006_fp, 0.063345280_fp, &
               1.815109321_fp, 1.338562881_fp, 0.637302745_fp, 0.235569395_fp, 0.074012782_fp, &
               1.872819935_fp, 1.420552405_fp, 0.696868723_fp, 0.265517726_fp, 0.085998221_fp, &
               0.112429528_fp, 0.181180515_fp, 0.148681479_fp, 0.083221602_fp, 0.036486882_fp, &
               0.116166997_fp, 0.192753055_fp, 0.163149503_fp, 0.094227758_fp, 0.042631362_fp, &
               0.119860476_fp, 0.204559546_fp, 0.178398393_fp, 0.106207090_fp, 0.049534976_fp /), &
             (/N_FREQUENCIES,N_TEMPERATURES,N_UNITS/) )
  
  ! dT/dB values
  ! ------------
  ! Scalar
  REAL(fp), PARAMETER :: SCALAR_DT(N_UNITS) = (/0.569245476_fp, 8.894460557_fp/)
  ! N,scalar,N
  REAL(fp), PARAMETER :: NsN_DT(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/0.569245476_fp, 0.502882501_fp, 0.491184636_fp, 0.496044167_fp, 0.507603202_fp, &
               8.894460557_fp, 5.858017893_fp, 5.044483251_fp, 4.743994905_fp, 4.633099159_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! scalar,K,K
  REAL(fp), PARAMETER :: sKK_DT(N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/0.569245476_fp, 0.550931004_fp, 0.533954162_fp, &
               8.894460557_fp, 8.608296931_fp, 8.343033788_fp /), &
             (/N_TEMPERATURES,N_UNITS/) )
  ! N,N,N
  REAL(fp), PARAMETER :: NNN_DT(N_FREQUENCIES,N_UNITS) = &
    RESHAPE( (/0.569245476_fp, 0.794787452_fp, 1.721801541_fp,  4.806444339_fp, 15.786495728_fp, &
               8.894460557_fp, 5.519357308_fp, 6.725787270_fp, 12.016110849_fp, 27.407110639_fp /), &
             (/N_FREQUENCIES,N_UNITS/) )
  ! N,NxK,NxK
  REAL(fp), PARAMETER :: NNKNK_DT(N_FREQUENCIES,N_TEMPERATURES,N_UNITS) = &
    RESHAPE( (/0.569245476_fp, 0.794787452_fp, 1.721801541_fp,  4.806444339_fp, 15.786495728_fp, &
               0.550931004_fp, 0.747069872_fp, 1.569112964_fp,  4.245033618_fp, 13.511179818_fp, &
               0.533954162_fp, 0.703951503_fp, 1.434990504_fp,  3.766226890_fp, 11.628147469_fp, &
               8.894460557_fp, 5.519357308_fp, 6.725787270_fp, 12.016110849_fp, 27.407110639_fp, &
               8.608296931_fp, 5.187985219_fp, 6.129347517_fp, 10.612584046_fp, 23.456909407_fp, &
               8.343033788_fp, 4.888552102_fp, 5.605431656_fp,  9.415567224_fp, 20.187756023_fp /), &
             (/N_FREQUENCIES,N_TEMPERATURES,N_UNITS/) )

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  INTEGER :: Error_Status
  INTEGER :: i, j, n, fid
  CHARACTER(80) :: Output_Fmt
  REAL(fp), DIMENSION(N_FREQUENCIES) :: x
  REAL(fp), DIMENSION(N_FREQUENCIES,N_TEMPERATURES) :: Radiance, Temperature, dBdT, dTdB
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: Temperature_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: Radiance_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dTemperature_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dRadiance_NL
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dRadiance_dBdT
  REAL(fp), DIMENSION(N_FREQUENCIES,N_PERTURBATIONS) :: dTemperature_dTdB
  INTEGER :: Wavelength_Units
  TYPE(UTest_type) :: UTest


  ! Output program header
  ! ---------------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to test the Planck functions module routines for '//&
                       'scalar, rank-1, and rank-2 input.', &
                       '$Revision$' )

  ! Initialise the tests
  ! --------------------
  CALL Init_AllTests(UTest)


  ! =============================
  ! Begin generic interface check
  ! =============================
  WRITE( *,'(15x,"GENERIC INTERFACE CHECK",/)' )

  ! Loop over spectral ordinate units
  ! ---------------------------------
  Unit_Loop: DO i = 1, n_Units

    ! Determine input units and unit flag
    IF ( i == 1 ) THEN
      x = FREQUENCY
      Wavelength_Units = 0
    ELSE
      x = Wavelength(FREQUENCY)
      Wavelength_Units = 1
    END IF


    ! Test Radiances
    ! --------------
    WRITE( Message,'("TEMPERATURE (K) -> RADIANCE (",a,") tests...")' ) TRIM(R_UNIT_STRING(i))
    CALL Init_Test(UTest,Message,Caller=PROGRAM_NAME)
    
    ! Scalar test
    Error_Status = Planck_Radiance( x(1), &
                                    INPUT_T(1,1), &
                                    Radiance(1,1), &
                                    Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(Radiance(1,1), SCALAR_R(i), THRESHOLD, UTest)

    ! N,scalar,N test
    Error_Status = Planck_Radiance( x, &
                                    INPUT_T(1,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( Radiance(:,1), NsN_R(:,i), THRESHOLD, UTest)

    ! Scalar,K,K test
    Error_Status = Planck_Radiance( x(1), &
                                    INPUT_T(1,:), &
                                    Radiance(1,:), &
                                    Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within( Radiance(1,:), sKK_R(:,i), THRESHOLD, UTest)

    ! N,N,N test
    Error_Status = Planck_Radiance( x, &
                                    INPUT_T(:,1), &
                                    Radiance(:,1), &
                                    Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( Radiance(:,1), NNN_R(:,i), THRESHOLD, UTest)

    ! N,NxK,NxK test
    Error_Status = Planck_Radiance( x, &
                                    INPUT_T, &
                                    Radiance, &
                                    Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( Radiance, NNKNK_R(:,:,i), THRESHOLD, UTest)

    ! Issue radiance test report
    CALL Report_Test(UTest)



    ! Test temperatures
    ! -----------------
    WRITE( Message,'("RADIANCE (",a,") -> TEMPERATURE (K) tests...")' ) TRIM(R_UNIT_STRING(i))
    CALL Init_Test(UTest,Message,Caller=PROGRAM_NAME)

    ! Scalar test
    Error_Status = Planck_Temperature( x(1), &
                                       SCALAR_R(i), &
                                       Temperature(1,1), &
                                       Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(Temperature(1,1), SCALAR_T(i), THRESHOLD, UTest)

    ! N,scalar,N test
    Error_Status = Planck_Temperature( x, &
                                       NsN_R(1,i), &
                                       Temperature(:,1), &
                                       Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(Temperature(:,1), NsN_T(:,i), THRESHOLD, UTest)

    ! Scalar,K,K test
    Error_Status = Planck_Temperature( x(1), &
                                       sKK_R(:,i), &
                                       Temperature(1,:), &
                                       Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(Temperature(1,:), sKK_T(:,i), THRESHOLD, UTest)

    ! N,N,N test
    Error_Status = Planck_Temperature( x, &
                                       NNN_R(:,i), &
                                       Temperature(:,1), &
                                       Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(Temperature(:,1), NNN_T(:,i), THRESHOLD, UTest)

    ! N,NxK,NxK test
    Error_Status = Planck_Temperature( x, &
                                       NNKNK_R(:,:,i), &
                                       Temperature(:,:), &
                                       Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(Temperature, NNKNK_T(:,:,i), THRESHOLD, UTest)

    ! Issue temperature test report
    CALL Report_Test(UTest)


    ! Test dB/dT
    ! ----------
    WRITE( Message,'("TEMPERATURE (K) -> dB/dT (",a,") tests...")' ) TRIM(D1_UNIT_STRING(i))
    CALL Init_Test(UTest,Message,Caller=PROGRAM_NAME)
    
    ! Scalar test
    Error_Status = Planck_dBdT( x(1), &
                                INPUT_T(1,1), &
                                dBdT(1,1), &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(dBdT(1,1), SCALAR_DB(i), THRESHOLD, UTest)

    ! N,scalar,N test
    Error_Status = Planck_dBdT( x, &
                                INPUT_T(1,1), &
                                dBdT(:,1), &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( dBdT(:,1), NsN_DB(:,i), THRESHOLD, UTest)

    ! Scalar,K,K test
    Error_Status = Planck_dBdT( x(1), &
                                INPUT_T(1,:), &
                                dBdT(1,:), &
                                Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within( dBdT(1,:), sKK_DB(:,i), THRESHOLD, UTest)

    ! N,N,N test
    Error_Status = Planck_dBdT( x, &
                                INPUT_T(:,1), &
                                dBdT(:,1), &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( dBdT(:,1), NNN_DB(:,i), THRESHOLD, UTest)

    ! N,NxK,NxK test
    Error_Status = Planck_dBdT( x, &
                                INPUT_T, &
                                dBdT, &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within( dBdT, NNKNK_DB(:,:,i), THRESHOLD, UTest)

    ! Issue dB/dT test report
    CALL Report_Test(UTest)


    ! Test dT/dB
    ! ----------
    WRITE( Message,'("RADIANCE (",a,") -> dT/dB (",a,") tests...")' ) TRIM(R_UNIT_STRING(i)), TRIM(D2_UNIT_STRING(i))
    CALL Init_Test(UTest,Message,Caller=PROGRAM_NAME)

    ! Scalar test
    Error_Status = Planck_dTdB( x(1), &
                                SCALAR_R(i), &
                                dTdB(1,1), &
                                Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(dTdB(1,1), SCALAR_DT(i), THRESHOLD, UTest)

    ! N,scalar,N test
    Error_Status = Planck_dTdB( x, &
                                NsN_R(1,i), &
                                dTdB(:,1), &
                                Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(dTdB(:,1), NsN_DT(:,i), THRESHOLD, UTest)

    ! Scalar,K,K test
    Error_Status = Planck_dTdB( x(1), &
                                sKK_R(:,i), &
                                dTdB(1,:), &
                                Wavelength_Units=Wavelength_Units   )
    CALL Is_Equal_Within(dTdB(1,:), sKK_DT(:,i), THRESHOLD, UTest)

    ! N,N,N test
    Error_Status = Planck_dTdB( x, &
                                NNN_R(:,i), &
                                dTdB(:,1), &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(dTdB(:,1), NNN_DT(:,i), THRESHOLD, UTest)

    ! N,NxK,NxK test
    Error_Status = Planck_dTdB( x, &
                                NNKNK_R(:,:,i), &
                                dTdB(:,:), &
                                Wavelength_Units=Wavelength_Units )
    CALL Is_Equal_Within(dTdB, NNKNK_DT(:,:,i), THRESHOLD, UTest)

    ! Issue dT/dB test report
    CALL Report_Test(UTest)

  END DO Unit_Loop

  ! Report test results
  ! -------------------
  CALL Report_AllTests(UTest)


  ! ===============================
  ! Begin derivative routine checks
  ! ===============================
  WRITE( *,'(//15x,"DERIVATIVE ROUTINE CHECKS")' )
  
  WRITE( Output_Fmt,'("(",i2,"(1x,f15.9))")' ) N_FREQUENCIES

  ! Open the output file
  ! --------------------
  fid = Get_Lun()
  OPEN( fid, FILE  =OUTPUT_FILE, &
             STATUS='REPLACE', &
             FORM  ='FORMATTED', &
             ACCESS='SEQUENTIAL' )
             
  ! Loop over spectral ordinate units
  ! ---------------------------------
  Unit_Loop_Derivative_Check: DO i = 1, n_Units

    ! Determine input units and unit flag
    IF ( i == 1 ) THEN
      x = Frequency
      Wavelength_Units = 0
    ELSE
      x = Wavelength(Frequency)
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
    ! ----------------------------
    WRITE( fid,'(//5x,"TEMPERATURE (K) -> dB/dT (",a,")...")' ) TRIM(D1_UNIT_STRING(i))

    ! The finite difference
    Error_Status = Planck_Radiance( x, &
                                    Temperature_NL, &
                                    Radiance_NL, &
                                    Wavelength_Units=Wavelength_Units )
    DO j = 1, N_PERTURBATIONS
      dRadiance_NL(:,j) = Radiance_NL(:,j) - Radiance_NL(:,N_PERTURBATIONS/2+1)
    END DO

    ! The derivative
    Error_Status = Planck_dBdT( x, &
                                Temperature_NL, &
                                dRadiance_dBdT, &
                                Wavelength_Units=Wavelength_Units )

    dRadiance_dBdT = dRadiance_dBdT * dTemperature_NL

    ! Output the two datasets
    WRITE( fid,'(/10x,"Finite difference dR result (",a,"):")' ) TRIM(R_UNIT_STRING(i))
    WRITE( fid,FMT=TRIM(Output_Fmt) ) x
    WRITE( fid,'(82("="))' )
    DO j = 1, N_PERTURBATIONS
      WRITE( fid,FMT=TRIM(Output_Fmt) ) dRadiance_NL(:,j)
    END DO
    WRITE( fid,'(/10x,"dB/dT routine dR result (",a,"):")' ) TRIM(R_UNIT_STRING(i))
    WRITE( fid,FMT=TRIM(Output_Fmt) ) x
    WRITE( fid,'(82("="))' )
    DO j = 1, N_PERTURBATIONS
      WRITE( fid,FMT=TRIM(Output_Fmt) ) dRadiance_dBdT(:,j)
    END DO


    ! Compute the dT/dB derivative
    ! ----------------------------
    WRITE( fid,'(//5x,"RADIANCE (",a,") -> dT/dB (",a,")...")' ) TRIM(R_UNIT_STRING(i)), &
                                                                 TRIM(D2_UNIT_STRING(i))
    ! The finite difference
    Error_Status = Planck_Temperature( x, &
                                       Radiance_NL, &
                                       Temperature_NL, &
                                       Wavelength_Units=Wavelength_Units )

    DO j = 1, N_PERTURBATIONS
      dTemperature_NL(:,j) = Temperature_NL(:,j) - T0
    END DO

    ! The derivative
    Error_Status = Planck_dTdB( x, &
                                Radiance_NL, &
                                dTemperature_dTdB, &
                                Wavelength_Units=Wavelength_Units )

    dTemperature_dTdB = dTemperature_dTdB * dRadiance_NL

    ! Output the two datasets
    WRITE( fid,'(/10x,"Finite difference result (K):")' )
    WRITE( fid,FMT=TRIM(Output_Fmt) ) x
    WRITE( fid,'(82("="))' )
    DO j = 1, N_PERTURBATIONS
      WRITE( fid,FMT=TRIM(Output_Fmt) ) dTemperature_NL(:,j)
    END DO
    WRITE( fid,'(/10x,"dT/dB routine result (K):")' )
    WRITE( fid,FMT=TRIM(Output_Fmt) ) x
    WRITE( fid,'(82("="))' )
    DO j = 1, N_PERTURBATIONS
      WRITE( fid,FMT=TRIM(Output_Fmt) ) dTemperature_dTdB(:,j)
    END DO

  END DO Unit_Loop_Derivative_Check
  
  ! Close the output file
  ! ---------------------
  CLOSE(fid)
  WRITE( *,'(/5x,"Results written to ",a)' ) OUTPUT_FILE
    
CONTAINS

  FUNCTION Wavelength(Frequency)
    REAL(fp), INTENT(IN) :: Frequency(:)
    REAL(fp) :: Wavelength(SIZE(Frequency))
    REAL(fp), PARAMETER :: SCALE_FACTOR = 10000.0_fp
    Wavelength = SCALE_FACTOR/Frequency
  END FUNCTION Wavelength

END PROGRAM Planck_Functions_Test
