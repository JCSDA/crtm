!
! Compute_Effective_TauProfile
!
! Program to compute the effective molecular transmittances profiles
! from the various available molecular combinations.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 27-June-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Compute_Effective_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE ProcessControl_Define
  USE ProcessControl_IO
  USE TauProfile_Define
  USE TauProfile_netCDF_IO
  USE Tau_Production_Parameters
  USE Tau_Production_Utility
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Compute_Effective_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &

  REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
  REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind

  INTEGER, PARAMETER :: N_DIRECTIONS = 2
  INTEGER, PARAMETER :: UP_DIRECTION   = 1
  INTEGER, PARAMETER :: DOWN_DIRECTION = 2
  INTEGER, PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_ID = (/ UP_DIRECTION,   &
                      DOWN_DIRECTION /)
  CHARACTER( * ),  PARAMETER, DIMENSION( N_DIRECTIONS ) :: &
    DIRECTION_NAME = (/ 'upwelling  ', &
                        'downwelling' /)

  INTEGER, PARAMETER :: WLO_IDX =  1  ! H2O lines only, no continua
  INTEGER, PARAMETER :: ALL_IDX = 10  ! First 7 molecules with continua
  INTEGER, PARAMETER :: WVO_IDX = 11  ! H2O and O3 only with continua
  INTEGER, PARAMETER :: WET_IDX = 12  ! H2O lines and continua
  INTEGER, PARAMETER :: DRY_IDX = 13  ! Dry gases (no H2O and O3) and continua
  INTEGER, PARAMETER :: OZO_IDX = 14  ! O3 lines and continua
  INTEGER, PARAMETER :: WCO_IDX = 15  ! H2O continua only, no line absorption
  INTEGER, PARAMETER :: EFFECTIVE_WLO_IDX = WLO_IDX + 100  ! WET/WCO
  INTEGER, PARAMETER :: EFFECTIVE_DRY_IDX = DRY_IDX + 100  ! ALL/WVO
  INTEGER, PARAMETER :: EFFECTIVE_OZO_IDX = OZO_IDX + 100  ! WVO/WET


  ! ---------
  ! Variables
  ! ---------
  CHARACTER( 256 ) :: message
  INTEGER :: IO_Status
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  CHARACTER( 256 ) :: Control_Filename
  CHARACTER( 256 ) :: TauProfile_Filename
  CHARACTER( 256 ) :: Signal_Filename
  INTEGER :: Direction
  INTEGER :: l, n_l ! n_Channels
  INTEGER :: i, n_i ! n_Angles
  INTEGER :: m, n_m ! n_Profiles
  INTEGER :: j, n_j ! n_Molecule_Sets
  INTEGER :: n
  INTEGER :: nGT1, k
  INTEGER, DIMENSION(N_LAYERS) :: IdxGT1
  CHARACTER( 5000 ) :: History
  TYPE( ProcessControl_type ) :: ProcessControl
  INTEGER, DIMENSION( : ), ALLOCATABLE :: Profile_List
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_ALL
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WVO
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WET
  REAL( fp_kind ), DIMENSION( :, :, : ), ALLOCATABLE :: Tau_WCO

  ! Output descriptive header
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to compute the effective molecular transmittance '//&
                       'profiles from the various available molecular combinations.', &
                       '$Revision: 2.9 $' )


  !#----------------------------------------------------------------------------#
  !#                        -- DETERMINE THE DIRECTION --                       #
  !#----------------------------------------------------------------------------#

  WRITE( *, '( /5x, "Select atmospheric path" )' )
  DO n = 1, N_DIRECTIONS
    WRITE( *, '( 10x, i1, ") ", a )' ) n, DIRECTION_NAME(n)
  END DO
  WRITE( *, FMT = '( /5x, "Enter choice: " )', ADVANCE = 'NO' )
  READ( *, FMT = '( i5 )', IOSTAT = IO_Status ) Direction

  IF ( IO_Status /= 0 ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPHERIC PATH identifier input.', &
                          FAILURE )
    STOP
  END IF

  IF ( Direction < DIRECTION_ID(1) .OR. Direction > DIRECTION_ID(N_DIRECTIONS) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Invalid ATMOSPERIC PATH identifier value.', &
                          FAILURE )
    STOP
  ENDIF



  !#----------------------------------------------------------------------------#
  !#                      -- READ A PROCESS CONTROL FILE --                     #
  !#----------------------------------------------------------------------------#

  WRITE( *, FMT     = '( /5x, "Enter a Process Control filename : " )', &
            ADVANCE = 'NO' )
  READ( *, FMT = '( a )' ) Control_Filename
  Control_Filename = ADJUSTL( Control_Filename )


  ! -----------------------------
  ! Read the Process Control data
  ! -----------------------------

  Error_Status = Read_ProcessControl( TRIM( Control_Filename ), &
                                      ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Process Control file '//&
                          TRIM( Control_Filename )//'.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#            -- BEGIN MAIN LOOP OVER SENSOR/PLATFORM FILES --                #
  !#----------------------------------------------------------------------------#

  Sensor_Loop: DO n = 1, ProcessControl%n_Files


    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE THE TauProfile FILENAME --                    #
    !#--------------------------------------------------------------------------#

    TauProfile_Filename = './TauProfile_data/'//&
                          TRIM( DIRECTION_NAME( Direction ) )//'.'//&
                          TRIM( ProcessControl%File_Prefix( n ) )//'.TauProfile.nc'




    !#--------------------------------------------------------------------------#
    !#                 -- INQUIRE THE INPUT TauProfile FILE --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------
    ! Inquire the file to get dimensions
    ! and global attributes
    ! ----------------------------------

    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              n_Channels      = n_l, &
                                              n_Angles        = n_i, &
                                              n_Profiles      = n_m, &
                                              n_Molecule_Sets = n_j, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' dimensions.', &
                            Error_Status )
      STOP
    END IF


    ! ---------------------------
    ! Inquire the file to get the
    ! dimension list data
    ! ---------------------------

    ! Allocate the array
    ALLOCATE( Profile_List( n_m ), STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      WRITE( Message, '( "Error allocating dimension list arrays. STAT = ", i5 )' ) &
                      Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! Read the profile list
    Error_Status = Inquire_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Profile_List = Profile_List, &
                                              History = History )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( PROGRAM_NAME, &
                            'Error inquiring netCDF TauProfile file '//&
                            TRIM( TauProfile_Filename )//' profile list.', &
                            Error_Status )
      STOP
    END IF


    ! -----------------------------------
    ! Modify the HISTORY global attribute
    ! -----------------------------------

    Error_Status = Modify_TauProfile_GAtts( TRIM( TauProfile_Filename ), &
                                            History = PROGRAM_RCS_ID//':'//&
                                                      TRIM( History ) )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( PROGRAM_NAME, &
                            'Error modifying HISTORY global attribute in '//&
                            TRIM( TauProfile_Filename ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#      -- ALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --       #
    !#--------------------------------------------------------------------------#
 
    ALLOCATE( Tau_ALL( N_LAYERS, n_l, n_i ), &
              Tau_WVO( N_LAYERS, n_l, n_i ), &
              Tau_WET( N_LAYERS, n_l, n_i ), &
              Tau_WCO( N_LAYERS, n_l, n_i ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating transmittance arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- BEGIN LOOP OVER PROFILES --                      #
    !#--------------------------------------------------------------------------#
 
    WRITE( *, '( 5x, "Computing the effective TauProfile data for the ", a, " sensor..." )' ) &
              TRIM( ProcessControl%File_Prefix( n ) )


    Profile_Loop: DO m = 1, n_m


      ! ----------------------------
      !  Read the transmittance data
      ! ----------------------------

      ! Total transmittance, Tau_ALL
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             ALL_IDX, &
                                             Tau_ALL )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_ALL from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! Water vapor + ozone transmittance, Tau_WVO
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WVO_IDX, &
                                             Tau_WVO )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WVO from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! Water vapor only transmittance, Tau_WET
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WET_IDX, &
                                             Tau_WET )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WET from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF


      ! Water vapor continua only transmittance, Tau_WCO
      Error_Status = Read_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                             Profile_List(m), &
                                             WCO_IDX, &
                                             Tau_WCO )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading profile ", i3, " Tau_WCO from ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF



      ! ---------------------------------------------------------
      ! Compute the effective transmittances.
      ! The multi-nested loops are used to minimise memory usage.
      ! ---------------------------------------------------------

      DO i = 1, n_i
        DO l = 1, n_l

          ! Effective DRY transmittance
          CALL Compute_EffTau( Tau_ALL(:,l,i), Tau_WVO(:,l,i) )

          ! Effective OZO transmittance
          CALL Compute_EffTau( Tau_WVO(:,l,i), Tau_WET(:,l,i) )

          ! Effective WLO transmittance
          CALL Compute_EffTau( Tau_WET(:,l,i), Tau_WCO(:,l,i) )

        END DO
      END DO


      ! ----------------------------------
      ! Output the current profile to file
      ! ----------------------------------

      ! Effective DRY transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_ALL, &
                                              Profile_List(m), &
                                              EFFECTIVE_DRY_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_DRY to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Effective OZO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_WVO, &
                                              Profile_List(m), &
                                              EFFECTIVE_OZO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      ! Effective WLO transmittance
      Error_Status = Write_TauProfile_netCDF( TRIM( TauProfile_Filename ), &
                                              Tau_WET, &
                                              Profile_List(m), &
                                              EFFECTIVE_WLO_IDX )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error writing profile ", i3, " Effective Tau_OZO to ", a )' ) &
                        Profile_List(m), TRIM( TauProfile_Filename )
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              Error_Status )
        STOP
      END IF

      WRITE( *, '( 15x, "Profile # ", i3, " effective transmittances written..." )' ) &
                Profile_List(m)

    END DO Profile_Loop



    !#--------------------------------------------------------------------------#
    !#     -- DEALLOCATE ARRAYS FOR EFFECTIVE TRANSMITTANCE COMPUTATION --      #
    !#--------------------------------------------------------------------------#
 
    DEALLOCATE( Tau_ALL, Tau_WVO, Tau_WET, Tau_WCO, Profile_List, &
                STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error deallocating arrays for ", a, ". STAT = ", i5 )' ) &
                      TRIM( TauProfile_Filename ), Allocate_Status
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            Error_Status )
      STOP
    END IF

  END DO Sensor_Loop



  !#----------------------------------------------------------------------------#
  !#                -- CREATE A DIRECTION DEPENDENT SIGNAL FILE --              #
  !#----------------------------------------------------------------------------#

  WRITE( Signal_Filename, '( a, ".",  a )' ) &
                          PROGRAM_NAME, &
                         TRIM( DIRECTION_NAME( Direction ) )

  Error_Status = Create_Signal_File( TRIM( Signal_Filename ) )



  !#----------------------------------------------------------------------------#
  !#                                -- CLEAN UP --                              #
  !#----------------------------------------------------------------------------#

  Error_Status = Destroy_ProcessControl( ProcessControl )

  IF ( Error_Status /= SUCCESS ) THEN
    Error_Status = WARNING
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying ProcessControl data structure.', &
                          Error_Status )
  END IF


CONTAINS


  ! Subroutine to compute the effective transmittances
  ! The effective transmittance is returned in the first argument,
  ! the numerator transmittance
  ! Only compute effective transmittance if the denominator
  ! is greater than numerical precision.
  SUBROUTINE Compute_EffTau( TauNUM, TauDENOM )
    REAL( fp_kind ), DIMENSION(:), INTENT( IN OUT ) :: TauNUM
    REAL( fp_kind ), DIMENSION(:), INTENT( IN )     :: TauDENOM
    REAL( fp_kind ), PARAMETER :: TOLERANCE = EPSILON( ONE )
    WHERE( TauDENOM > TOLERANCE )
      TauNUM = TauNUM/TauDENOM
    ELSEWHERE
      TauNUM = ZERO
    END WHERE
  END SUBROUTINE Compute_EffTau

END PROGRAM Compute_Effective_TauProfile
