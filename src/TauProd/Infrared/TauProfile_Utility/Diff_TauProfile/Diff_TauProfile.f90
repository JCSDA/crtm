!
! Diff_TauProfile
!
! Program to compare values in two TauProfile files and report the
! location of the differences.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 06-Feb-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Diff_TauProfile


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers
  USE String_Utility

  USE TauProfile_Define
  USE TauProfile_netCDF_IO


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Diff_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: Diff_TauProfile.f90,v 1.3 2006/07/27 21:58:58 wd20pd Exp $'


  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: i, j, m
  CHARACTER( 256 ) :: InFile1
  CHARACTER( 256 ) :: InFile2
  REAL( fp_kind ), DIMENSION(:,:), ALLOCATABLE :: Tau1, Tau2
  INTEGER :: nK1, nL1, nI1, nM1, nJ1
  INTEGER :: NCEP_Sensor_ID1
  CHARACTER( 80 ) :: ID_Tag1, Sensor_Name1, Platform_Name1
  REAL( fp_kind ), DIMENSION(:), POINTER :: Level_Pressure1
  INTEGER,         DIMENSION(:), POINTER :: Channel_List1
  REAL( fp_kind ), DIMENSION(:), POINTER :: Angle_List1
  INTEGER,         DIMENSION(:), POINTER :: Profile_List1
  INTEGER,         DIMENSION(:), POINTER :: Molecule_Set_List1
  INTEGER :: nK2, nL2, nI2, nM2, nJ2
  INTEGER :: NCEP_Sensor_ID2
  CHARACTER( 80 ) :: ID_Tag2, Sensor_Name2, Platform_Name2
  REAL( fp_kind ), DIMENSION(:), POINTER :: Level_Pressure2
  INTEGER,         DIMENSION(:), POINTER :: Channel_List2
  REAL( fp_kind ), DIMENSION(:), POINTER :: Angle_List2
  INTEGER,         DIMENSION(:), POINTER :: Profile_List2
  INTEGER,         DIMENSION(:), POINTER :: Molecule_Set_List2

  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                       'Program to compare values in two TauProfile files and '//&
                       'report the location of the differences.', &
                       '$Revision: 1.3 $' )


  !#----------------------------------------------------------------------------#
  !#                    -- ENTER THE FILENAMES TO COMPARE --                    #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter TauProfile filename #1: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) InFile1
  InFile1 = ADJUSTL( InFile1 )

  WRITE( *, FMT     = '( /5x, "Enter TauProfile filename #2: " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) InFile2
  InFile2 = ADJUSTL( InFile2 )


  ! ---------------------------
  ! Check that both files exist
  ! ---------------------------

  IF ( .NOT. File_Exists( InFile1 ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//&
                          TRIM(InFile1)//&
                          ' not found.', &
                          Error_Status )
    STOP
  END IF

  IF ( .NOT. File_Exists( InFile2 ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//&
                          TRIM(InFile2)//&
                          ' not found.', &
                          Error_Status )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                             -- INQUIRE THE FILES --                        #
  !#----------------------------------------------------------------------------#

  ! ------------------------------
  ! Inquire the TauProfile file #1
  ! ------------------------------

  CALL Get_TauProfile_DimData( InFile1, &
                               nK1, &
                               nL1, &
                               nI1, &
                               nM1, &
                               nJ1, &
                               NCEP_Sensor_ID1, &
                               ID_Tag1, &
                               Sensor_Name1, &
                               Platform_Name1, &
                               Level_Pressure1, &
                               Channel_List1, &
                               Angle_List1, &
                               Profile_List1, &
                               Molecule_Set_List1 )


  ! ------------------------------
  ! Inquire the TauProfile file #2
  ! ------------------------------

  CALL Get_TauProfile_DimData( InFile2, &
                               nK2, &
                               nL2, &
                               nI2, &
                               nM2, &
                               nJ2, &
                               NCEP_Sensor_ID2, &
                               ID_Tag2, &
                               Sensor_Name2, &
                               Platform_Name2, &
                               Level_Pressure2, &
                               Channel_List2, &
                               Angle_List2, &
                               Profile_List2, &
                               Molecule_Set_List2 )


  !#----------------------------------------------------------------------------#
  !#                  -- COMPARE THE DIMENSION/ATTRIBUTE DATA --                #
  !#----------------------------------------------------------------------------#

  ! -------------------------
  ! All dimensions must agree
  ! -------------------------

  IF ( nK1 /= nK2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Layer dimensions different', &
                          FAILURE )
    STOP
  END IF

  IF ( nL1 /= nL2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Channel dimensions different', &
                          FAILURE )
    STOP
  END IF

  IF ( nI1 /= nI2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Angle dimensions different', &
                          FAILURE )
    STOP
  END IF

  IF ( nM1 /= nM2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Profile dimensions different', &
                          FAILURE )
    STOP
  END IF

  IF ( nJ1 /= nJ2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Molecule set dimensions different', &
                          FAILURE )
    STOP
  END IF


  ! -----------------------------------
  ! All dimensions list data must agree
  ! -----------------------------------

  IF ( .NOT. ALL(( Level_Pressure1 .EqualTo. Level_Pressure2 )) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Level pressures are different', &
                          FAILURE )
    STOP
  END IF

  IF ( COUNT( (Channel_List1-Channel_List2) /= 0 ) /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Channel lists are different', &
                          FAILURE )
    STOP
  END IF

  IF ( .NOT. ALL(( Angle_List1 .EqualTo. Angle_List2 )) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Angle lists are different', &
                          FAILURE )
    STOP
  END IF

  IF ( COUNT( (Profile_List1-Profile_List2) /= 0 ) /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Profile lists are different', &
                          FAILURE )
    STOP
  END IF

  IF ( COUNT( (Molecule_Set_List1-Molecule_Set_List2) /= 0 ) /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Molecule set lists are different', &
                          FAILURE )
    STOP
  END IF


  ! -------------------
  ! Other IDs/names/etc
  ! -------------------

  IF ( NCEP_Sensor_ID1 /= NCEP_Sensor_ID2 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'NCEP Sensor IDs are different', &
                          FAILURE )
    STOP
  END IF

  IF ( TRIM(ID_Tag1) /= TRIM(ID_Tag2) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Profile ID tags are different', &
                          FAILURE )
    STOP
  END IF

  IF ( StrUpCase(TRIM(Sensor_Name1)) /= StrUpCase(TRIM(Sensor_Name2)) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Sensor identifiers are different:'//&
                          TRIM(Sensor_Name1)//&
                          ' and '//TRIM(Sensor_Name2), &
                          FAILURE )
    STOP
  END IF

  IF ( StrUpCase(TRIM(Platform_Name1)) /= StrUpCase(TRIM(Platform_Name2)) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Platform identifiers are different: '//&
                          TRIM(Platform_Name1)//&
                          ' and '//TRIM(Platform_Name2), &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- COMPARE THE TRANSMITTANCE DATA --                    #
  !#----------------------------------------------------------------------------#

  ! --------------------------------
  ! Allocate the data arrays to read
  ! --------------------------------

  ALLOCATE( Tau1( nK1, nL1 ), Tau2( nK2, nL2 ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating transmittance data arrays. STAT = ", i5 )' ) &
                    Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! --------------------------
  ! Loop over other dimensions
  ! --------------------------

  WRITE( *, '( /5x, "Comparing TauProfile data..." )' )

  j_Loop: DO j = 1, nJ1
    m_Loop: DO m = 1, nM1
      i_Loop: DO i = 1, nI1

        ! -- KxL data from TauProfile #1
        Error_Status = Read_TauProfile_netCDF( InFile1, &
                                               Angle_List1(i), &
                                               Profile_List1(m), &
                                               Molecule_Set_List1(j), &
                                               Tau1 )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading angle ", i3, "(", f4.2, "), profile ", i3, &
                            &" and molecule set ", i3, " data from ", a )' ) &
                          i, Angle_List1(i), Profile_List1(m), Molecule_Set_List1(j), &
                          TRIM( InFile1 )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- KxL data from TauProfile #2
        Error_Status = Read_TauProfile_netCDF( InFile2, &
                                               Angle_List2(i), &
                                               Profile_List2(m), &
                                               Molecule_Set_List2(j), &
                                               Tau2 )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error reading angle ", i3, "(", f4.2, "), profile ", i3, &
                            &" and molecule set ", i3, " data from ", a )' ) &
                          i, Angle_List2(i), Profile_List2(m), Molecule_Set_List2(j), &
                          TRIM( InFile2 )
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                Error_Status )
          STOP
        END IF

        ! -- Compare the numbers
        IF ( .NOT. ALL( Tau1 .EqualTo. Tau2 ) ) THEN
          WRITE( Message, '( "Difference at angle ", i3, "(", f4.2, "), profile ", i3, &
                            &" and molecule set ", i3)' ) &
                          i, Angle_List2(i), Profile_List2(m), Molecule_Set_List2(j)
          CALL Display_Message( PROGRAM_NAME, &
                                TRIM( Message ), &
                                INFORMATION )
        END IF

      END DO i_Loop
    END DO m_Loop
  END DO j_Loop


CONTAINS


  SUBROUTINE Get_TauProfile_DimData( InFile, &
                                     n_Layers, &
                                     n_Channels, &
                                     n_Angles, &
                                     n_Profiles, &
                                     n_Molecule_Sets, &
                                     NCEP_Sensor_ID, &
                                     ID_Tag, &
                                     Sensor_Name, &
                                     Platform_Name, &
                                     Level_Pressure, &
                                     Channel_List, &
                                     Angle_List, &
                                     Profile_List, &
                                     Molecule_Set_List )

    CHARACTER(*),    INTENT(IN)  :: InFile
    INTEGER,         INTENT(OUT) :: n_Layers, n_Channels, n_Angles, n_Profiles, n_Molecule_Sets
    INTEGER,         INTENT(OUT) :: NCEP_Sensor_ID
    CHARACTER(*),    INTENT(OUT) :: ID_Tag, Sensor_Name, Platform_Name
    REAL( fp_kind ), DIMENSION(:), POINTER :: Level_Pressure     ! INTENT(OUT)
    INTEGER,         DIMENSION(:), POINTER :: Channel_List       ! INTENT(OUT)
    REAL( fp_kind ), DIMENSION(:), POINTER :: Angle_List         ! INTENT(OUT)
    INTEGER,         DIMENSION(:), POINTER :: Profile_List       ! INTENT(OUT)
    INTEGER,         DIMENSION(:), POINTER :: Molecule_Set_List  ! INTENT(OUT)

    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_TauProfile_DimData'

    INTEGER :: Error_Status
    INTEGER :: Allocate_Status

    ! -- Get the dimensions
    Error_Status = Inquire_TauProfile_netCDF( Infile, &
                                              n_Layers        = n_Layers, &       
                                              n_Channels      = n_Channels, &     
                                              n_Angles        = n_Angles, &       
                                              n_Profiles      = n_Profiles, &     
                                              n_Molecule_Sets = n_Molecule_Sets, &
                                              NCEP_Sensor_ID  = NCEP_Sensor_ID, & 
                                              ID_Tag          = ID_Tag, &         
                                              Sensor_Name     = Sensor_Name, &    
                                              Platform_Name   = Platform_Name ) 

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM(InFile)//&
                            ' for dimensions and global attributes', &
                            Error_Status )
      STOP
    END IF

    ! -- Allocate the dimension list arrays
    ALLOCATE( Level_Pressure( n_Layers+1 ), &
              Channel_List( n_Channels ), &
              Angle_List( n_Angles ), &
              Profile_List( n_Profiles ), &
              Molecule_Set_List( n_Molecule_Sets ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating dimension list arrays for input file ", a, &
                        &". STAT = ", i5 )' ) &
                      TRIM( InFile ), Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

    ! -- Get the dimension lists
    Error_Status = Inquire_TauProfile_netCDF( InFile, &
                                              Level_Pressure    = Level_Pressure,   &
                                              Channel_List      = Channel_List,     &
                                              Angle_List        = Angle_List,       &
                                              Profile_List      = Profile_List,     &
                                              Molecule_Set_List = Molecule_Set_List )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM(InFile)//&
                            ' for dimensions lists', &
                            Error_Status )
      STOP
    END IF

  END SUBROUTINE Get_TauProfile_DimData

END PROGRAM Diff_TauProfile
