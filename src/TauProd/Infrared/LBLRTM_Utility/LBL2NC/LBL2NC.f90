!
! LBL2NC
!
! Program to read an LBLRTM format single panel transmittance file
! where every layer has the same number of data points, and write
! the data in netCDF format.
!
! Modifed from the LBLRTM_to_netCDF.f90 program to not require a 
! specified number of spectral points or atmospheric layers.
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 26-Feb-2007
!                       paul.vandelst@ssec.wisc.edu
!

PROGRAM LBL2NC


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Type_Kinds       , ONLY: fp
  USE File_Utility     , ONLY: File_Exists
  USE Message_Handler  , ONLY: SUCCESS, FAILURE, INFORMATION, &
                               Display_Message, Program_Message
  USE LBLRTM_Parameters, ONLY: LBLRTM_SINGLE_PANEL_TYPE, &
                               LBLRTM_FILE_PTR_EOF
  USE LBLRTM_Utility   , ONLY: Open_LBLRTM, Compute_n_Points
  USE LBLRTM_Fhdr_IO   , ONLY: LBLRTM_Fhdr_type, &
                               Read_LBLRTM_Fhdr
  USE LBLRTM_Layer_IO  , ONLY: LBLRTM_Layer_type, &
                               Read_LBLRTM_Layer, &
                               Destroy_LBLRTM_Layer
  USE LBLRTM_netCDF_IO , ONLY: Create_LBLRTM_netCDF, &
                               Write_LBLRTM_netCDF
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'LBL2NC'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  '$Id: $'
  INTEGER     , PARAMETER :: DOWNWELLING = 0
  INTEGER     , PARAMETER :: UPWELLING   = 1
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(0:1) = (/ 'downwelling', &
                                                      'upwelling  ' /)
  REAL(fp)    , PARAMETER :: ONE = 1.0_fp

  ! ---------
  ! Variables
  ! ---------
  CHARACTER(256) :: Message
  CHARACTER(256) :: Title
  CHARACTER(256) :: Comment
  CHARACTER(256) :: Id_Tag
  CHARACTER(256) :: NC_Filename
  CHARACTER(256) :: LBLRTM_Filename
  INTEGER        :: LBLRTM_FileID
  INTEGER :: Error_Status
  INTEGER :: LBLRTM_EOF
  INTEGER :: n
  INTEGER :: n_Frequencies
  INTEGER :: n_Layers
  INTEGER  :: Direction
  REAL(fp) :: f1
  REAL(fp) :: f2
  REAL(fp) :: df
  TYPE(LBLRTM_Fhdr_type)  :: LBLRTM_Fhdr
  TYPE(LBLRTM_Layer_type) :: LBLRTM_Layer


  ! --------------
  ! Output heeader
  ! --------------
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to read LBLRTM format data files and write '//&
                        'netCDF format output files.', &
                        '$Revision: $')

  ! --------------
  ! Get user input
  ! --------------
  ! Get the input binary filename
  WRITE(*, FMT='(/5x,"Enter input LBLRTM file:  " )', ADVANCE='NO' )
  READ(*,'(a)') LBLRTM_Filename
  LBLRTM_Filename = ADJUSTL(LBLRTM_Filename)
  IF ( .NOT. File_Exists( TRIM(LBLRTM_Filename) ) ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'LBLRTM file '//TRIM(LBLRTM_Filename)//' not found', &
                          FAILURE )
    STOP
  END IF
  NC_Filename = TRIM(LBLRTM_Filename)//'.nc'

  ! Read in the global attributes
  WRITE(*,'(/5x,"Enter a TITLE global attribute string:")')
  READ(*,'(a)') Title
  Title = ADJUSTL(Title)

  WRITE(*,'(/5x,"Enter a COMMENT global attribute string:")')
  READ(*,'(a)') Comment
  Comment = ADJUSTL(Comment)

  WRITE(*,'(/5x,"Enter a ID_TAG global attribute string:")')
  READ(*,'(a)') Id_Tag
  Id_Tag = ADJUSTL(Id_Tag)


  ! --------------------
  ! Open the LBLRTM file
  ! --------------------
  Error_Status = Open_LBLRTM( TRIM(LBLRTM_Filename), &
                              LBLRTM_FileID )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error opening '//TRIM(LBLRTM_Filename)//'.', &
                          FAILURE )
    STOP
  END IF



  ! --------------------------------
  ! Read the first layer file header
  ! for direction and frequency data
  ! --------------------------------
  Error_Status = Read_LBLRTM_Fhdr( LBLRTM_FileID, &
                                   LBLRTM_Fhdr, &
                                   LBLRTM_EOF )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading layer #1 file header from file '//&
                          TRIM(LBLRTM_Filename), &
                          FAILURE )
    STOP
  END IF

  ! Rewind the LBLRTM data file
  REWIND(LBLRTM_FileID)

  ! Determine the direction value
  IF ( LBLRTM_Fhdr%Run_Flags%layr1 == LBLRTM_Fhdr%Run_Flags%nlayr ) THEN
    Direction = UPWELLING
  ELSE
    Direction = DOWNWELLING
  END IF
  CALL Display_Message( PROGRAM_NAME, &
                        'Detected calculation direction for '//&
                        TRIM(LBLRTM_Filename)//' is '//&
                        TRIM( DIRECTION_NAME( Direction ) ), &
                        INFORMATION )

  ! Assign the frequency values
  f1 = REAL(LBLRTM_Fhdr%Begin_Frequency,    fp)
  f2 = REAL(LBLRTM_Fhdr%End_Frequency,      fp)
  df = REAL(LBLRTM_Fhdr%Frequency_Interval, fp)

  ! Compute the number of spectral points
  n_Frequencies = Compute_n_Points(f1,f2,df)


  ! -----------------------------
  ! Create the netCDF format file
  ! -----------------------------
  Error_Status = Create_LBLRTM_netCDF( NC_Filename           , &
                                       n_Frequencies         , &
                                       Direction             , &
                                       f1                    , &
                                       f2                    , &
                                       df                    , &
                                       Id_Tag =TRIM(Id_Tag)  , &
                                       Title  =TRIM(Title)   , &
                                       History=PROGRAM_RCS_ID, &
                                       Comment=TRIM(Comment)   )
  IF ( Error_Status /= SUCCESS ) THEN 
    CALL Display_Message( PROGRAM_NAME, &
                          'Error creating '//TRIM(NC_Filename)//'.', &
                          FAILURE )
    STOP
  END IF


  ! ---------------------------
  ! Begin loop over data layers
  ! ---------------------------
  WRITE(*,*)
  n_Layers = 0
  Layer_Loop: DO
    n_Layers = n_Layers + 1
    
    ! Read the layer
    Error_Status = Read_LBLRTM_Layer( LBLRTM_FileID, &
                                      LBLRTM_SINGLE_PANEL_TYPE, &
                                      LBLRTM_Layer, &
                                      LBLRTM_EOF )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading layer #", i3, " from file ", a )' ) &
                      n_Layers, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    ! Exit loop if EOF reached
    IF ( LBLRTM_EOF == LBLRTM_FILE_PTR_EOF ) EXIT Layer_loop


    ! Check the number of spectral points
    n = LBLRTM_Layer%n_Points
    IF ( n /= n_Frequencies ) THEN
      WRITE( Message, '( "Actual number of LBLRTM points, ", i5, &
                        &" is different from expected, ", i5, &
                        &" for layer #", i3, "." )' ) &
                      n, n_Frequencies, n_Layers
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

    ! Write the layer data to the netCDF file
    Error_Status = Write_LBLRTM_netCDF( NC_Filename, &
                                        Transmittance=REAL(LBLRTM_Layer%Spectrum(1:n,1),fp))
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing layer #", i3, " to file ", a )' ) &
                      n_Layers, TRIM( NC_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    WRITE( *, '( 5x, "Layer #", i3, " written..." )' ) n_Layers


    ! Destroy the LBLRTM_Layer structure for the next read
    Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer )
    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error destroying LBLRTM_Layer structure for layer #", i3, &
                        &"; Input file ", a )' ) n_Layers, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO Layer_Loop

  CLOSE( LBLRTM_FileID )

END PROGRAM LBL2NC
