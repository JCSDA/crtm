!
! Check_TauProfile
!
! Program to check TauProfile datafiles for invalid or inconsistent data.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 09-Feb-2006
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM Check_TauProfile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds          , ONLY: fp
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, WARNING, &
                                  Display_Message, Program_Message
  USE TauProfile_netCDF_IO, ONLY: Inquire_TauProfile_netCDF, &
                                  Read_TauProfile_netCDF
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER( * ), PARAMETER :: PROGRAM_NAME = 'Check_TauProfile'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
  INTEGER,  PARAMETER :: SET = 1
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: TOLERANCE     = EPSILON( ONE )
  REAL(fp), PARAMETER :: TAU_TOLERANCE = 100.0_fp * TOLERANCE


  ! ---------
  ! Variables
  ! ---------

  CHARACTER(256) :: Message
  CHARACTER(256) :: InFile
  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER ::  k,  l,  i,  m,  j
  INTEGER :: nK, nL, nI, nM, nJ
  INTEGER,  ALLOCATABLE :: L_List(:)
  REAL(fp), ALLOCATABLE :: I_List(:)
  INTEGER,  ALLOCATABLE :: M_List(:)
  INTEGER,  ALLOCATABLE :: J_List(:)
  REAL(fp), ALLOCATABLE :: Tau(:)
  CHARACTER(20) :: L_Tag, I_Tag, M_Tag, J_Tag
  LOGICAL :: Bad_Tau
  INTEGER :: n
  INTEGER :: n_TauGT1_Eps
  INTEGER :: n_TauLT0_Eps
  INTEGER :: n_TauGT1    
  INTEGER :: n_TauLT0    


  ! Program header
  ! --------------
  CALL Program_Message(PROGRAM_NAME, &
                       'Program to check TauProfile datafiles for invalid data.', &
                       '$Revision$' )


  ! Prompt for filename to check
  ! ----------------------------
  WRITE( *,FMT    ='(/5x,"Enter TauProfile filename: ")', &
           ADVANCE='NO' )
  READ( *,FMT='(a)' ) InFile
  InFile = ADJUSTL(InFile)

  ! Check that it exists
  IF ( .NOT. File_Exists( InFile ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Input file '//TRIM(InFile)//' not found.', &
                          FAILURE )
    STOP
  END IF


  ! Inquire the TauProfile file
  ! ---------------------------
  ! Get the dimensions
  Error_Status = Inquire_TauProfile_netCDF( InFile            , &
                                            n_Layers       =nK, &
                                            n_Channels     =nL, &
                                            n_Angles       =nI, &
                                            n_Profiles     =nM, &
                                            n_Molecule_Sets=nJ  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          TRIM(InFile)//' for dimensions', &
                          Error_Status )
    STOP
  END IF

  ! Allocate the dimension list and transmittance arrays
  ALLOCATE( L_List(nL), I_List(nI), M_List(nM), J_List(nJ), Tau(nK), &
            STAT=Allocate_Status )
  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message,'("Error allocating arrays for input file ",a,&
                    &". STAT = ",i0)' ) &
                    TRIM(InFile), Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          Error_Status )
    STOP
  END IF

  ! Get the dimension lists
  Error_Status = Inquire_TauProfile_netCDF( InFile                  , &
                                            Channel_List     =L_List, &
                                            Angle_List       =I_List, &
                                            Profile_List     =M_List, &
                                            Molecule_Set_List=J_List  )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error inquiring netCDF TauProfile file '//&
                          TRIM(InFile)//' for dimension lists', &
                          Error_Status )
    STOP
  END IF


  ! Initialise flag and counters
  ! ----------------------------
  Bad_Tau = .FALSE.
  n_TauGT1_Eps = 0
  n_TauLT0_Eps = 0
  n_TauGT1     = 0
  n_TauLT0     = 0

  
  ! Check the transmittances
  ! ------------------------
  J_Loop: DO j = 1, nJ
    WRITE( J_Tag,'("; Molecule Set:",i3)' ) J_List(j)
    
    M_Loop: DO m = 1, nM
      WRITE( M_Tag,'("; Profile:",i3)' ) M_List(m)
      
      I_Loop: DO i = 1, nI
        WRITE( I_Tag,'("; Angle secant:",f4.2)' ) I_List(i)
        
        L_Loop: DO l = 1, nL
          WRITE( L_Tag,'("; Channel:",i4)' ) L_List(l)


          ! Read the current transmittance profile
          ! --------------------------------------
          Error_Status = Read_TauProfile_netCDF( TRIM(InFile), &
                                                 L_List(l)   , &
                                                 I_List(i)   , &
                                                 M_List(m)   , &
                                                 J_List(j)   , &
                                                 Tau         , &
                                                 Quiet=SET     )
          IF ( Error_Status /= SUCCESS ) THEN
            CALL Display_Message( PROGRAM_NAME, &
                                  'Error reading netCDF TauProfile file '//&
                                  TRIM(InFile)//&
                                  TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag), &
                                  Error_Status )
            STOP
          END IF


          ! Correct transmittances close to 0 or 1
          ! but non-physical due to numerics
          ! --------------------------------------
          ! Check for 1 < tau < 1+e
          n = COUNT( Tau > ONE .AND. Tau < (ONE+TAU_TOLERANCE) )
          IF ( n > 0 ) THEN
            Bad_Tau = .TRUE.
            n_TauGT1_Eps = n_TauGT1_Eps + n
          END IF
          WHERE( Tau > ONE .AND. Tau < (ONE+TAU_TOLERANCE) )
            Tau = ONE
          END WHERE

          ! Check for 0-e < tau < 0
          n = COUNT( Tau > (ZERO-TAU_TOLERANCE) .AND. Tau < ZERO )
          IF ( n > 0 ) THEN
            Bad_Tau = .TRUE.
            n_TauLT0_Eps = n_TauLT0_Eps + n
          END IF
          WHERE( Tau > (ZERO-TAU_TOLERANCE) .AND. Tau < ZERO )
            Tau = ZERO
          END WHERE


          ! Now check for grossly crappy transmittances
          ! -------------------------------------------
          ! Check for tau > 1
          IF ( ANY( Tau > ONE ) ) THEN
            Bad_Tau = .TRUE.
            n_TauGT1 = n_TauGT1_Eps + COUNT( Tau > ONE )
            WRITE( Message,'("Transmittances > 1.0 found in ",a,a)' ) &
                            TRIM(InFile), &
                            TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  WARNING )
          END IF

          ! Check for tau < 0
          IF ( ANY( Tau < ZERO ) ) THEN
            Bad_Tau = .TRUE.
            n_TauLT0 = n_TauLT0_Eps + COUNT( Tau < ZERO )
            WRITE( Message,'("Transmittances < 0.0 found in ",a,a)' ) &
                            TRIM(InFile), &
                            TRIM(L_Tag)//TRIM(I_Tag)//TRIM(M_Tag)//TRIM(J_Tag)
            CALL Display_Message( PROGRAM_NAME, &
                                  TRIM(Message), &
                                  WARNING )
          END IF
        END DO L_Loop
      END DO I_Loop
    END DO M_Loop
  END DO J_Loop


  ! Output bad transmittance count
  ! ------------------------------
  IF ( Bad_Tau ) THEN
    WRITE( *,'(/5x,"Number 1 < tau < 1+e : ",i8,&
              &/5x,"Number 0-e < tau < 0 : ",i8,&
              &/5x,"Number tau > 1       : ",i8,&
              &/5x,"Number tau < 0       : ",i8)' ) &
              n_TauGT1_Eps, n_TauLT0_Eps, n_TauGT1, n_TauLT0
  ELSE
    WRITE( *,'(/5x,"All transmittances o.k.")' )
  END IF
  
END PROGRAM Check_TauProfile
