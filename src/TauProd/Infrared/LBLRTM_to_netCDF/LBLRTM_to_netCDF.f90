!
! LBLRTM_to_netCDF
!
! Program to read an LBLRTM format transmittance production data file
! and write it out in netCDF format.
!
! Note: This is *not* a generic LBLRTM -> netCDF format converter.
!       It is specifically for use with the transmittance production
!       code suite.
!
!
! FILES ACCESSED:
!       Input:  LBLRTM format data file.
!
!       Output: - netCDF format data file.
!               - Completion signal file.
!
! SIDE EFFECTS:
!       Output files are overwritten if they already exist.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 10-Apr-2002
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM LBLRTM_to_netCDF

  ! ------------------
  ! Environment set up
  ! ------------------
  ! Module usage
  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Compare_Float_Numbers
  USE LBLRTM_Parameters
  USE LBLRTM_Utility
  USE LBLRTM_Fhdr_IO
  USE LBLRTM_Layer_IO
  USE LBLRTM_netCDF_IO
  USE Tau_Production_Parameters
  USE Tau_Production_Utility
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'LBLRTM_to_netCDF'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &


  REAL(fp), PARAMETER :: ONEpointFIVE = 1.5_fp
  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: LBLRTM_Filename
  INTEGER        :: LBLRTM_FileID
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: Title
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Id_Tag
  INTEGER :: Error_Status
  INTEGER :: LBLRTM_EOF
  INTEGER :: i, k, n
  TYPE(LBLRTM_Fhdr_type)  :: LBLRTM_Fhdr
  TYPE(LBLRTM_Layer_type) :: LBLRTM_Layer
  INTEGER :: Direction, iDirection
  REAL(fp) :: f1
  REAL(fp) :: f2
  REAL(fp) :: df
!  INTEGER :: fIdx
  INTEGER :: n_freq


  ! Program header
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read LBLRTM format data files and write '//&
                        'netCDF format output files.', &
                        ' $Revision$' )

  ! Get user input
  ! --------------
  ! Get the input binary filename
  WRITE( *, FMT     = '( /5x, "Enter input LBLRTM file:  " )', &
            ADVANCE = 'NO' )
  READ( *,'(a)' ) LBLRTM_Filename
  LBLRTM_Filename = ADJUSTL(LBLRTM_Filename)
  IF ( .NOT. File_Exists( TRIM(LBLRTM_Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'File not found', &
                          FAILURE )
    STOP
  END IF
  NC_Filename = TRIM(LBLRTM_Filename)//'.nc'

  WRITE(*,'(/5x,"Calculation direction")')
  DO i = 1, N_DIRECTIONS
    WRITE(*,'(7x,i1,") ", a)') i-1, TRIM(DIRECTION_NAME(i))
  END DO
  WRITE(*,FMT='( 5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,*) iDirection

  ! Read in the global attributes
  WRITE( *,'(/5x, "Enter a TITLE global attribute string:")' )
  READ( *,'(a)' ) Title
  Title = ADJUSTL(Title)
  WRITE( *,'( /5x, "Enter a COMMENT global attribute string:")' )
  READ( *,'(a)' ) Comment
  Comment = ADJUSTL(Comment)
  WRITE( *,'( /5x, "Enter a PROFILE SET ID_TAG global attribute string:")' )
  READ( *,'(a)' ) Id_Tag
  ID_Tag = ADJUSTL(ID_Tag)


  ! Open the LBLRTM format file
  ! ---------------------------
  Error_Status = Open_LBLRTM( TRIM(LBLRTM_Filename), &
                              LBLRTM_FileID )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening '//TRIM(LBLRTM_Filename)//'.', &
                          FAILURE )
    STOP
  END IF


  ! Read the first layer file header
  ! for direction and frequency data
  ! --------------------------------
  Error_Status = Read_LBLRTM_Fhdr( LBLRTM_FileID, &
                                   LBLRTM_Fhdr, &
                                   LBLRTM_EOF )
  IF ( Error_Status /= SUCCESS ) THEN
    WRITE( Message, '( "Error reading layer #1 file header from file ", a )' ) &
                    TRIM(LBLRTM_Filename)
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM(Message), &
                          FAILURE )
    STOP
  END IF


  ! Rewind the LBLRTM data file
  ! ---------------------------
  REWIND( LBLRTM_FileID )


  ! Assign the frequency values
  ! ---------------------------
  f1 = REAL(LBLRTM_Fhdr%Begin_Frequency,    fp)
  f2 = REAL(LBLRTM_Fhdr%End_Frequency,      fp)
  df = REAL(LBLRTM_Fhdr%Frequency_Interval, fp)

  n_freq = INT((f2-f1)/df + ONEpointFIVE) 

  ! Determine the frequency interval index
  ! --------------------------------------
!  fIdx = Compute_dF_Index( df )

  ! Check the result
!  IF ( fIdx < 0 ) THEN
!    CALL Display_Message( PROGRAM_NAME, &
!                          'Frequency interval mismatch', &
!                          FAILURE )
!    STOP
!  END IF


  ! Create the netCDF format file
  ! -----------------------------
  Error_Status = Create_LBLRTM_netCDF( NC_Filename             , &
!                                       N_FREQUENCIES(fIdx)     , &
                                       n_freq                  , &
                                       iDirection              , &
                                       f1                      , &
                                       f2                      , &
                                       df                      , &
                                       Id_Tag  = TRIM(Id_Tag)  , &
                                       Title   = TRIM(Title)   , &
                                       History = PROGRAM_RCS_ID, &
                                       Comment = TRIM(Comment)   )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM(NC_Filename)//'.', &
                          FAILURE )
    STOP
  END IF


  ! Read and write layers of data
  ! -----------------------------
  WRITE(*,*)

  ! Loop over layers
  Layer_Loop: DO k = 1, N_LAYERS


    ! Read the layer
    Error_Status = Read_LBLRTM_Layer( LBLRTM_FileID   , &
                                      LBLRTM_FILE_TYPE, &
                                      LBLRTM_Layer    , &
                                      LBLRTM_EOF        )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading layer #", i3, " from file ", a )' ) &
                      k, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    ! Write the layer data to the netCDF file
    n = LBLRTM_Layer%n_Points
!    IF ( n /= N_FREQUENCIES( fIdx ) ) THEN
    IF ( n /= n_freq ) THEN
      WRITE( Message, '( "Actual number of LBLRTM points, ", i5, &
                        &" is different from expected, ", i5, &
                        &" for layer #", i3, "." )' ) &
!                      n, N_FREQUENCIES( fIdx ), k
                      n, n_freq, k
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    Error_Status = Write_LBLRTM_netCDF( NC_Filename, &
                                        Spectrum=REAL(LBLRTM_Layer%Spectrum(1:n,1),fp) )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing layer #", i0, " to file ", a )' ) &
                      k, TRIM(NC_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF
    WRITE( *, '( 5x, "Layer #", i0, " written..." )' ) k

    ! Destroy the LBLRTM_Layer structure for the next read
    Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer )
    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error destroying LBLRTM_Layer structure for layer #", i3, &
                        &"; Input file ", a )' ) k, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM(Message), &
                            FAILURE )
      STOP
    END IF

    IF(iDirection == RADIANCE)THEN
      EXIT Layer_Loop
    ENDIF
    
  END DO Layer_Loop


  ! Close the input file
  ! --------------------
  CLOSE( LBLRTM_FileID )


  ! Create a signal file indicating successful completion
  ! -----------------------------------------------------
  Error_Status = Create_Signal_File( TRIM(NC_Filename) )

END PROGRAM LBLRTM_to_netCDF
