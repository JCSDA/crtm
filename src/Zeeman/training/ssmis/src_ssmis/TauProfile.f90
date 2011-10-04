MODULE TauProfile

  USE Type_Kinds         , ONLY : fp_kind, & 
                                  Single                           
  USE Message_Handler    , ONLY : SUCCESS, FAILURE, Display_Message     
  USE File_Utility

  IMPLICIT NONE

  PRIVATE
  PUBLIC :: TauProfile_Type
  PUBLIC :: Get_Profile

  TYPE TauProfile_Type
    INTEGER :: n_Profile
    INTEGER :: n_Layer
    INTEGER :: n_Be
    INTEGER :: n_CosBK
    INTEGER :: n_Angle
    INTEGER :: n_Freq

    INTEGER :: Channel_Index  ! ZSSMIS channel index (a number 1 - 4 coresponding
                              ! to 19 - 22)
    
    REAL(fp_kind), DIMENSION( : ),   POINTER :: p_level => NULL()     ! 0:n_layer
    REAL(fp_kind), DIMENSION( :,: ), POINTER :: tk_layer => NULL()    ! n_layer x n_Profile
    REAL(fp_kind), DIMENSION( : ),   POINTER :: Be => NULL()          ! n_Be
    REAL(fp_kind), DIMENSION( : ),   POINTER :: CosBK => NULL()       ! n_CosBK
    REAL(fp_kind), DIMENSION( : ),   POINTER :: Secant_Ang => NULL()  ! n_Angle
    REAL(fp_kind), DIMENSION( : ),   POINTER :: Freq => NULL()        ! n_Freq
                ! 0:Nlayer x N_Be x n_CosBK x n_Angle x n_Profile x n_Freq
    REAL(fp_kind), DIMENSION(:,:,:,:,:,:), POINTER :: Tau => NULL()  
  END Type TauProfile_Type
  
  REAL(fp_kind), PARAMETER :: ONE = 1.0_fp_kind

CONTAINS

  ! allocate memory and obtaining data 
  SUBROUTINE Get_Profile(inFilename_AtmProfile, inFilename_TauProfile, TauProfile)
    CHARACTER(*),    INTENT(IN)  :: inFilename_AtmProfile 
    CHARACTER(*),    INTENT(IN)  :: inFilename_TauProfile 
    TYPE(TauProfile_Type), INTENT(INOUT) :: TauProfile
    
    ! Local parameters and variables
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Get_Profile'
    CHARACTER(256) :: Message
    INTEGER :: fid = 12
    INTEGER :: n_profile, n_layer, n_angle, n_Be, n_CosBK, n_freq, channel
    REAL( fp_kind ), Allocatable :: tk_lev(:), z_lev(:)
    INTEGER         :: error_status, iskip, iang, ifreq, ib, ic, k, m
    REAL( fp_kind ) :: skip


    PRINT *, 'Reading data ...'

    fid = Get_Lun()
        
    OPEN(fid, FILE=inFilename_TauProfile, STATUS='old')
    READ(fid, *)channel
    READ(fid, *)n_profile, n_layer, n_Be, n_CosBK, n_angle, n_freq
    
    TauProfile%Channel_Index = channel - 18  ! SSMIS channel index to ZSSMIS channel index
    
    TauProfile%n_Profile = n_Profile
    TauProfile%n_Layer   = n_Layer
    TauProfile%n_Be      = n_Be
    TauProfile%n_CosBK   = n_CosBK
    TauProfile%n_Angle   = n_Angle
    TauProfile%n_Freq    = n_freq
    
    ALLOCATE(TauProfile%p_level(0:n_layer), &
             TauProfile%tau(0:n_layer, n_Be, n_CosBK, n_angle, n_profile, n_freq), &
             TauProfile%tk_layer(n_layer, n_profile), &
             TauProfile%secant_ang(n_angle), &
             TauProfile%Be(n_Be), &
             TauProfile%CosBK(n_CosBK), &
             TauProfile%Freq(n_freq),   &
             tk_lev(0:n_layer), &
             z_lev(0:n_layer),  &
             STAT=error_status)   
    IF ( error_status /= 0 ) THEN                                                                 
      WRITE( Message, '( "Error allocating arrays for the input profiles. STAT = ", i5 )' ) &                
                      error_status                                                              
      CALL Display_Message( ROUTINE_NAME, &                                                         
                            TRIM( Message ), &                                                      
                            FAILURE )                                                               
      STOP                                                                                          
    END IF
              
    READ(fid, *)TauProfile%Be
    READ(fid, *)TauProfile%CosBK
    READ(fid, *)TauProfile%secant_ang  ! read in angles in degree
    TauProfile%secant_ang = ONE/COS(TauProfile%secant_ang*3.14159/180.0)  ! convert to secant angles
    READ(fid, *)TauProfile%Freq
    DO ifreq = 1, n_freq
    DO iang = 1, n_angle
    DO m = 1, n_profile
    DO ib = 1, n_Be
    DO ic = 1, n_CosBK
      READ(fid, *)TauProfile%tau(1:n_layer, ib, ic, iang, m, ifreq)
      TauProfile%tau(0, ib, ic, iang, m, ifreq) = ONE
    END DO
    END DO
    END DO
    END DO
    END DO
    CLOSE(fid)

    fid = Get_Lun()
    
    OPEN(fid, FILE=inFilename_AtmProfile, STATUS='OLD')
    READ(fid, *)iskip, iskip
    DO m = 1, n_profile
      READ(fid, *)iskip
      READ(fid, *)(z_lev(k), TauProfile%p_level(k), tk_lev(k), skip, k = 0, n_layer)
      TauProfile%tk_layer(:,m) = 0.5*(tk_lev(0:n_layer-1)+tk_lev(1:n_layer))
    END DO
    CLOSE(fid)
    
    Deallocate(tk_lev, z_lev, STAT=error_status)
    IF ( error_status /= 0 ) THEN                                                                 
      WRITE( Message, '( "Error deallocating tk_lev and z_le. STAT = ", i5 )' ) &                
                      error_status                                                              
      CALL Display_Message( ROUTINE_NAME, &                                                         
                            TRIM( Message ), &                                                      
                            FAILURE )                                                               
      STOP                                                                                          
    END IF
    
    PRINT *, 'End of reading data ...'
    
  END SUBROUTINE Get_Profile                                                                                        

END MODULE TauProfile
