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
  USE Type_Kinds            , ONLY: fp
  USE File_Utility          , ONLY: File_Exists
  USE Message_Handler       , ONLY: SUCCESS, FAILURE, INFORMATION, &
                                    Display_Message, Program_Message
  USE LBLRTM_Parameters     , ONLY: LBLRTM_N_PANEL_TYPES, &
                                    LBLRTM_N_PANELS, &
                                    LBLRTM_PANEL_TYPE, &
                                    LBLRTM_PANEL_TYPE_NAME, &
                                    LBLRTM_SINGLE_PANEL_TYPE, &
                                    LBLRTM_FILE_PTR_EOF
  USE LBLRTM_Utility        , ONLY: Open_LBLRTM, Compute_n_Points
  USE LBLRTM_Fhdr_IO        , ONLY: LBLRTM_Fhdr_type, &
                                    Read_LBLRTM_Fhdr
  USE LBLRTM_Layer_IO       , ONLY: LBLRTM_Layer_type, &
                                    Read_LBLRTM_Layer, &
                                    Destroy_LBLRTM_Layer
  USE LBLRTM_netCDF_IO      , ONLY: Create_LBLRTM_netCDF, &
                                    Write_LBLRTM_netCDF
  USE Tau_Production_Utility, ONLY: Create_Signal_File
  ! Disable all implicit typing
  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'LBL2NC'
  CHARACTER(*), PARAMETER :: PROGRAM_RCS_ID = &
  
  INTEGER,      PARAMETER :: N_DIRECTIONS = 2
  INTEGER,      PARAMETER :: UPWELLING   = 1
  INTEGER,      PARAMETER :: DOWNWELLING = 2
  CHARACTER(*), PARAMETER :: DIRECTION_NAME(N_DIRECTIONS) = &
    (/ 'upwelling  ','downwelling' /)
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
  INTEGER :: i, n, idx(1)
  INTEGER :: n_Frequencies
  INTEGER :: n_Layers
  INTEGER  :: iPanel, iDirection
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
                        '$Revision$')

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

  WRITE(*,'(/5x,"Calculation direction")')
  DO i = 1, N_DIRECTIONS
    WRITE(*,'(7x,i1,") ", a)') i, TRIM(DIRECTION_NAME(i))
  END DO
  WRITE(*,FMT='( 5x,"Enter choice: ")',ADVANCE='NO')
  READ(*,*) iDirection
  iDirection = MIN(MAX(iDirection,UPWELLING),DOWNWELLING)
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(DIRECTION_NAME(iDirection))//' direction selected.', &
                        INFORMATION )

  WRITE(*,FMT='(/5x,"Number of panels",&
               &/7x,"1) single panel",&
               &/7x,"2) double panel",&
               &/5x,"Enter choice: ")',&
          ADVANCE='NO')
  READ(*,*) iPanel
  iPanel = MIN(MAX(iPanel,1),2)
  idx = PACK( (/(i,i=1,LBLRTM_N_PANEL_TYPES)/), &
              LBLRTM_N_PANELS == iPanel )
  iPanel = idx(1)
  CALL Display_Message( PROGRAM_NAME, &
                        TRIM(LBLRTM_PANEL_TYPE_NAME(iPanel))//' panel file type selected.', &
                        INFORMATION )
  

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


  ! Begin loop over data layers
  ! ---------------------------
  WRITE(*,*)
  n_Layers = 0
  Layer_Loop: DO
    n_Layers = n_Layers + 1


    ! Read the layer
    ! --------------
    Error_Status = Read_LBLRTM_Layer( LBLRTM_FileID, &
                                      LBLRTM_PANEL_TYPE(iPanel), &
                                      LBLRTM_Layer, &
                                      LBLRTM_EOF )
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error reading layer #", i0, " from file ", a )' ) &
                      n_Layers, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    
    
    ! Exit loop if EOF reached, or number of output points is zero
    ! ------------------------------------------------------------
    IF ( LBLRTM_EOF == LBLRTM_FILE_PTR_EOF .OR. &
         LBLRTM_Layer%n_Points == 0 ) EXIT Layer_loop


    ! Create the netCDF file
    ! ----------------------
    IF ( n_Layers == 1 ) THEN
      Error_Status = Create_LBLRTM_netCDF( NC_Filename                    , & 
                                           LBLRTM_Layer%n_Points          , & 
                                           iDirection                     , & 
                                           LBLRTM_Layer%Begin_Frequency   , &
                                           LBLRTM_Layer%End_Frequency     , &
                                           LBLRTM_Layer%Frequency_Interval, &
                                           Id_Tag =TRIM(Id_Tag)           , &
                                           Title  =TRIM(Title)            , &
                                           History=PROGRAM_RCS_ID         , &
                                           Comment=TRIM(Comment)            )
      IF ( Error_Status /= SUCCESS ) THEN 
        CALL Display_Message( PROGRAM_NAME, &
                              'Error creating '//TRIM(NC_Filename)//'.', &
                              FAILURE )
        STOP
      END IF
    END IF


    ! Write the layer data to the netCDF file
    ! ---------------------------------------
    n = LBLRTM_Layer%n_Points
    Error_Status = Write_LBLRTM_netCDF( NC_Filename, &
                                        Transmittance=REAL(LBLRTM_Layer%Spectrum(1:n,1),fp))
    IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing layer #", i0, " to file ", a )' ) &
                      n_Layers, TRIM( NC_Filename )
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF
    WRITE( *, '( 5x, "Layer #", i0, " written..." )' ) n_Layers


    ! Destroy the LBLRTM_Layer structure for the next read
    ! ----------------------------------------------------
    Error_Status = Destroy_LBLRTM_Layer( LBLRTM_Layer )
    IF ( Error_Status /= SUCCESS ) THEN 
      WRITE( Message, '( "Error destroying LBLRTM_Layer structure for layer #", i0, &
                        &"; Input file ", a )' ) n_Layers, TRIM(LBLRTM_Filename)
      CALL Display_Message( PROGRAM_NAME, &
                            TRIM( Message ), &
                            FAILURE )
      STOP
    END IF

  END DO Layer_Loop

  CLOSE( LBLRTM_FileID )


  ! Create a signal file indicating successful completion
  ! -----------------------------------------------------
  Error_Status = Create_Signal_File( TRIM(NC_Filename) )

END PROGRAM LBL2NC
