!
! Create_LBL_Input
!
! Program to create LBL input data files given an input 
! netCDF AtmProfile dataset
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
  
PROGRAM Create_LBL_Input

  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler

  USE AtmProfile_Define
  USE AtmProfile_netCDF_IO

  USE LBLRTM_Input,  ONLY: Create_LBLRTM_TAPE5
  USE MONORTM_Input, ONLY: Calculation_Flags_Type, &
                           Create_MONORTM_TAPE5
                            
  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE

  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Create_LBL_Input'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &

  ! Liternal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp

  ! Required absorber IDs
  INTEGER, PARAMETER :: H2O_ID = 1
  INTEGER, PARAMETER ::  O3_ID = 3

  ! Default climatology model
  INTEGER, PARAMETER :: US_STD_ATM = 6

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: msg
  INTEGER :: Error_Status
  INTEGER :: alloc_status
  CHARACTER(256) :: Filename
  CHARACTER(256) :: Profile_Set_ID_Tag
  CHARACTER(256) :: TAPE5_File
  CHARACTER( 78) :: TAPE5_Header
  INTEGER :: j, m, n, n_Profiles
  INTEGER :: Begin_Profile, End_Profile
  INTEGER :: Increment
  INTEGER, DIMENSION(2) :: j_idx
  INTEGER :: File_Type
  TYPE(AtmProfile_type), ALLOCATABLE :: ap(:)
  TYPE(Calculation_Flags_type) :: calculation_flags

  ! Output header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the input data for MONORTM and LBLRTM', &
                        '$Revision$' )

  ! Read the AtmProfile filename
  ! ...Get it
  WRITE( *, FMT = '( /5x, "Enter the netCDF AtmProfile filename: " )', &
            ADVANCE = 'NO' )
  READ( *, '( a ) ' ) Filename
  Filename = ADJUSTL( Filename )
  ! ...Inquire the file
  error_status = Inquire_AtmProfile_netCDF( Filename, &
                                            n_Profiles = n_Profiles )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Inquire of '//TRIM(Filename)//' failed', &
                          FAILURE )
    STOP
  END IF
  
  ! Read the specified input file type (MONORTM or LBLRTM)
  WRITE( *, FMT = '( /5x, "Enter 1 for LBLRTM or 2 for MONORTM: " )', &
            ADVANCE = 'NO' )
  READ( *,* ) File_Type
  
  ! ...Allocate the output array
  ALLOCATE( ap(n_Profiles), STAT=alloc_status )
  IF ( alloc_status /= 0 ) THEN
    WRITE( msg, '("AtmProfile array allocation failed. STAT = ",i0)' ) alloc_status
    CALL display_message( PROGRAM_NAME, TRIM(msg), FAILURE )
    STOP
  END IF
  
  ! ...Read the file
  Error_Status = Read_AtmProfile_netCDF( Filename, &
                                         ap, &
                                         Profile_Set_ID = Profile_Set_ID_Tag )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//TRIM(Filename), &
                          FAILURE )
    STOP
  END IF
  
  ! Read the specified begin 
  ! and end profiles from set
  WRITE( *, FMT = '( /5x, "Enter the begin profile: ")', &
            ADVANCE = 'NO' )
  READ( *,* ) Begin_Profile
  
  WRITE( *, FMT = '( /5x, "Enter the end profile: ")', &
            ADVANCE = 'NO' )
  READ( *,* ) End_Profile
  
  IF( End_Profile > n_Profiles .OR. End_Profile <= ZERO ) THEN 
    Error_Status = FAILURE
  ENDIF
  
  IF( Begin_Profile > n_Profiles .OR. Begin_Profile <= ZERO ) THEN 
    Error_Status = FAILURE
  ENDIF
  
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME, &
                          'Error reading AtmProfile file '//TRIM(Filename), &
                          FAILURE )
    STOP
  END IF 
  
  IF( Begin_Profile > End_Profile ) THEN
    Increment = -1
  ELSE
    Increment = 1
  ENDIF
    
  calculation_flags%emit=1

  ! Loop over profile
  DO m = Begin_Profile, End_Profile, Increment
    WRITE( *, '( 5x, "Processing profile #", i4.4, "...." )' ) m

    ! Find the absorber indices for H2O and O3 only
    n = COUNT( ap(m)%Absorber_ID == H2O_ID .OR. &
               ap(m)%Absorber_ID ==  O3_ID      )
    IF ( n /= 2 ) THEN
      CALL display_message( PROGRAM_NAME,'No H2O and O3 in absorber set.',FAILURE )
      STOP
    END IF
    j_idx = PACK( (/ ( j, j = 1, ap(m)%n_Absorbers ) /), &
                  ( ap(m)%Absorber_ID == H2O_ID .OR. &
                    ap(m)%Absorber_ID ==  O3_ID      ) )

    ! Construct TAPE5 filename and header
    WRITE( TAPE5_Header, '( a, " profile #", i4.4, "; ", a )' ) TRIM(Profile_Set_Id_Tag), m
    WRITE( TAPE5_File, '( "./TAPE5_files/TAPE5.", a, "_profile", i4.4 )' ) TRIM(Profile_Set_Id_Tag), m
    
    ! Need to have case here to decide MONORTM or LBLRTM
    ! Pass profile to complete monortm input files written
    SELECT CASE (File_Type)
      ! Create the LBLRTM TAPE5 file
      CASE (1)
        Error_Status = Create_LBLRTM_TAPE5( ap(m)%Level_Pressure, &
                                            ap(m)%Level_Temperature, &
                                            ap(m)%Level_Absorber( :, j_idx ), &
                                            ap(m)%Absorber_Units_LBL( j_idx ), &
                                            ap(m)%Absorber_ID( j_idx ), &
                                            ZERO,         &    ! Surface altitude
                                            ONE, ONE+ONE, &    ! Dummy frequencies
                                            Climatology_model = US_STD_ATM, &
                                            Header   = TRIM(TAPE5_Header), &
                                            Filename = TRIM(TAPE5_File),   &
                                            Placeholder   = 1, &  ! Frequency/angle placeholder
                                            No_Terminator = 1  )  ! Do not output input terminator
        IF ( Error_Status /= 0 ) THEN
          WRITE( msg, '( "Error writing TAPE5 file for ", a, " profile #", i2, "." )' ) &
                      TRIM( Profile_Set_Id_Tag ), m
          CALL display_message( PROGRAM_NAME, TRIM(msg), FAILURE )
          STOP
        END IF
      CASE (2)  
        ! Create the MONORTM TAPE5 file
        Error_Status = Create_MONORTM_TAPE5( ap(m)%Level_Pressure( : ), &
                                             ap(m)%Level_Temperature( : ), &
                                             ap(m)%Level_Absorber( :, j_idx ), &
                                             ap(m)%Absorber_Units_LBL( j_idx ), &
                                             ap(m)%Absorber_ID( j_idx ), &
                                             ZERO,       &      ! Surface altitude
                                             -ONE, -ONE, &      ! Dummy frequencies
                                             calculation_flags=calculation_flags, &
                                             boundary_temperature = ap(m)%Layer_Temperature(1), &
                                             Climatology_model = US_STD_ATM, &
                                             Header = TRIM(TAPE5_Header) , &
                                             Filename = TRIM( TAPE5_File ),   &
                                             No_Terminator = 1 ) 
        IF ( Error_Status /= 0 ) THEN
          WRITE( msg, '( "Error writing TAPE5 file for ", a, " profile #", i2, "." )' ) &
                      TRIM( Profile_Set_Id_Tag ), m
          CALL display_message( PROGRAM_NAME, TRIM(msg), FAILURE )
          STOP
        END IF   
    END SELECT
  END DO

  ! Clean up
  Error_Status = Destroy_AtmProfile( ap )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL display_message( PROGRAM_NAME,'Error destroying AtmProfile structure.',WARNING )
  END IF
  DEALLOCATE( ap, STAT=alloc_status )
  IF ( alloc_status /= 0 ) THEN
    WRITE( msg, '("AtmProfile array deallocation failed. STAT = ",i0)' ) alloc_status
    CALL display_message( PROGRAM_NAME, TRIM(msg), WARNING )
  END IF

END PROGRAM Create_LBL_Input
