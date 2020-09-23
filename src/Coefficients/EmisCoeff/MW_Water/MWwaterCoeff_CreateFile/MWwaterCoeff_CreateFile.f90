!
! MWwaterCoeff_CreateFile
!
! Program to create the MWwaterCoeff file to be used by the CRTM
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Nov-2011
!                       paul.vandelst@noaa.gov
!

PROGRAM MWwaterCoeff_CreateFile

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE File_Utility        , ONLY: File_Exists
  USE Message_Handler     , ONLY: SUCCESS, FAILURE, Program_Message, Display_Message
  USE MWwaterCoeff_FASTEM4, ONLY: FASTEM4_LoadCoeffs  => MWwaterCoeff_LoadCoeffs, &
                                  FASTEM4_LoadVersion => MWwaterCoeff_LoadVersion
  USE MWwaterCoeff_FASTEM5, ONLY: FASTEM5_LoadCoeffs  => MWwaterCoeff_LoadCoeffs, &
                                  FASTEM5_LoadVersion => MWwaterCoeff_LoadVersion
  USE MWwaterCoeff_FASTEM6, ONLY: FASTEM6_LoadCoeffs  => MWwaterCoeff_LoadCoeffs, &
                                  FASTEM6_LoadVersion => MWwaterCoeff_LoadVersion
  USE MWwaterCoeff_Define , ONLY: MWwaterCoeff_type, &
                                  MWwaterCoeff_Associated, &
                                  MWwaterCoeff_Destroy, &
                                  MWwaterCoeff_Inspect, &
                                  MWwaterCoeff_WriteFile
  ! Disable implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'MWwaterCoeff_CreateFile'
  CHARACTER(*), PARAMETER :: PROGRAM_VERSION_ID = &

  INTEGER     , PARAMETER :: N_MODELS = 3
  CHARACTER(*), PARAMETER :: MODEL_NAME(N_MODELS) = ['FASTEM4','FASTEM5','FASTEM6']
  
  
  ! ---------
  ! Variables
  ! ---------
  INTEGER :: i
  INTEGER :: err_stat
  CHARACTER(256) :: msg
  CHARACTER(256) :: filename
  CHARACTER(256) :: id
  TYPE(MWwaterCoeff_type) :: mwwatercoeff


  ! Output program header
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to create the MWwaterCoeff file to be '//&
                        'used by the CRTM.', &
                        '$Revision$' )


  ! Loop over number of model/coefficient sets
  Model_Loop: DO i = 1, N_MODELS
    
    WRITE(*,'(//5x,"Creating ",a," EmisCoeff datafile...",/)') MODEL_NAME(i)

  
    ! Load the data
    SELECT CASE (i)
      CASE (1)
        CALL FASTEM4_LoadCoeffs(mwwatercoeff)
        CALL FASTEM4_LoadVersion(id)
      CASE (2)
        CALL FASTEM5_LoadCoeffs(mwwatercoeff)
        CALL FASTEM5_LoadVersion(id)
      CASE (3)
        CALL FASTEM6_LoadCoeffs(mwwatercoeff)
        CALL FASTEM6_LoadVersion(id)
      CASE DEFAULT
        msg = 'Invalid model selection! How did that happen?'
        CALL Display_Message( PROGRAM_NAME, msg, FAILURE )
        STOP
    END SELECT
    ! ...Test structure
    IF ( .NOT. MWwaterCoeff_Associated( mwwatercoeff ) ) THEN
      msg = 'Error loading '//MODEL_NAME(i)//' MWwaterCoeff data'
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF


    ! Write it to file
    filename = MODEL_NAME(i)//'.MWwater.EmisCoeff.bin'
    err_stat = MWwaterCoeff_WriteFile( mwwatercoeff, filename, &
                 Title   = 'Coefficient data for the '//MODEL_NAME(i)//&
                           ' microwave sea surface emissivity model.', &
                 History = PROGRAM_VERSION_ID//'; '//TRIM(id) )
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing to '//TRIM(filename)
      CALL Display_Message( PROGRAM_NAME, msg, FAILURE ); STOP
    END IF
  
  
    ! Clean up
    CALL MWwaterCoeff_Destroy( mwwatercoeff )

  END DO Model_Loop
  
END PROGRAM MWwaterCoeff_CreateFile
