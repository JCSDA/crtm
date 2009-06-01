PROGRAM Test_RadDiag_IO
  USE Type_Kinds
  USE Message_Handler, ONLY: SUCCESS, FAILURE, EOF, Program_Message, Display_Message
  USE RadDiag_IO, ONLY: RadDiag_Hdr_type, &
                        RadDiag_Data_type, &
                        RadDiag_ReadMode, &
                        RadDiag_WriteMode, &
                        RadDiag_AppendMode, &
                        Open_RadDiag, &
                        Read_RadDiag_Hdr, &
                        Read_RadDiag_Data
  IMPLICIT NONE

  ! Parameters
  CHARACTER(*), PARAMETER :: PROGRAM_NAME = 'Test_RadDiag_IO'

  ! Variables
  CHARACTER(256) :: Filename
  INTEGER :: Error_Status
  INTEGER :: Read_Status
  INTEGER :: FileID
  TYPE(RadDiag_Hdr_type)  :: RadDiag_Hdr
  TYPE(RadDiag_Data_type) :: RadDiag_Data
  INTEGER :: i, n

  ! Output a program header message
  CALL Program_Message( PROGRAM_NAME, &
                        'Program to test the GSI radiance diagnostic'//&
                        ' (RadDiag) I/O functions.', &
                        '$Revision$' )

  ! Get a filename to read
  WRITE(*,'(/5x,"Enter a RadDiag file to read: ")',ADVANCE='NO')
  READ(*,'(a)') Filename
  Filename = ADJUSTL(Filename)

  ! Open the RadDiag file
  Error_Status = Open_RadDiag( Filename, FileID )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error opening '//TRIM(Filename), Error_Status)
    STOP
  END IF

  ! Read the RadDiag file header
  Error_Status = Read_RadDiag_Hdr( Filename, FileID, RadDiag_Hdr )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message(PROGRAM_NAME, 'Error reading header from '//TRIM(Filename), Error_Status)
    STOP
  END IF

  ! Print out the RadDiag file header
  WRITE(*,*)
  WRITE(*,*) 'RadDiag_Hdr isis    = ', RadDiag_Hdr%Scalar%isis   
  WRITE(*,*) 'RadDiag_Hdr id      = ', RadDiag_Hdr%Scalar%id     
  WRITE(*,*) 'RadDiag_Hdr obstype = ', RadDiag_Hdr%Scalar%obstype
  WRITE(*,*) 'RadDiag_Hdr jiter   = ', RadDiag_Hdr%Scalar%jiter  
  WRITE(*,*) 'RadDiag_Hdr nchan   = ', RadDiag_Hdr%Scalar%nchan  
  WRITE(*,*) 'RadDiag_Hdr npred   = ', RadDiag_Hdr%Scalar%npred  
  WRITE(*,*) 'RadDiag_Hdr idate   = ', RadDiag_Hdr%Scalar%idate  
  WRITE(*,*) 'RadDiag_Hdr ireal   = ', RadDiag_Hdr%Scalar%ireal  
  WRITE(*,*) 'RadDiag_Hdr ipchan  = ', RadDiag_Hdr%Scalar%ipchan 
  WRITE(*,*) 'RadDiag_Hdr iextra  = ', RadDiag_Hdr%Scalar%iextra 
  WRITE(*,*) 'RadDiag_Hdr jextra  = ', RadDiag_Hdr%Scalar%jextra 

  DO i = 1, RadDiag_Hdr%nChannels
    WRITE(*,*)
    WRITE(*,*) 'Channel Index: ', i
    WRITE(*,*) '  freq     = ', RadDiag_Hdr%Channel(i)%freq
    WRITE(*,*) '  polar    = ', RadDiag_Hdr%Channel(i)%polar
    WRITE(*,*) '  wave     = ', RadDiag_Hdr%Channel(i)%wave
    WRITE(*,*) '  varch    = ', RadDiag_Hdr%Channel(i)%varch
    WRITE(*,*) '  tlapmean = ', RadDiag_Hdr%Channel(i)%tlapmean
    WRITE(*,*) '  iuse     = ', RadDiag_Hdr%Channel(i)%iuse
    WRITE(*,*) '  nuchan   = ', RadDiag_Hdr%Channel(i)%nuchan
    WRITE(*,*) '  iochan   = ', RadDiag_Hdr%Channel(i)%iochan
    READ(*,*)
  END DO

  ! Loop to read RadDiag data until end-of-file
  n = 0
  Read_Loop: DO

    Read_Status = Read_RadDiag_Data( Filename, FileID, RadDiag_Hdr, RadDiag_Data )
    SELECT CASE (Read_Status)
      CASE (EOF)
        EXIT Read_Loop
      CASE (FAILURE)
        CALL Display_Message(PROGRAM_NAME, 'Error reading data from '//TRIM(Filename), Read_Status)
        STOP
      CASE DEFAULT
        ! Success or warning: do nothing
    END SELECT

    n = n + 1

    ! Print out the RadDiag data
    WRITE(*,*)
    WRITE(*,*) 'Location Index: ', n
    WRITE(*,*) '  RadDiag_Data lat        = ', RadDiag_Data%Scalar%lat       
    WRITE(*,*) '  RadDiag_Data lon        = ', RadDiag_Data%Scalar%lon       
    WRITE(*,*) '  RadDiag_Data zsges      = ', RadDiag_Data%Scalar%zsges     
    WRITE(*,*) '  RadDiag_Data obstime    = ', RadDiag_Data%Scalar%obstime   
    WRITE(*,*) '  RadDiag_Data senscn_pos = ', RadDiag_Data%Scalar%senscn_pos
    WRITE(*,*) '  RadDiag_Data satzen_ang = ', RadDiag_Data%Scalar%satzen_ang
    WRITE(*,*) '  RadDiag_Data satazm_ang = ', RadDiag_Data%Scalar%satazm_ang
    WRITE(*,*) '  RadDiag_Data solzen_ang = ', RadDiag_Data%Scalar%solzen_ang
    WRITE(*,*) '  RadDiag_Data solazm_ang = ', RadDiag_Data%Scalar%solazm_ang
    WRITE(*,*) '  RadDiag_Data sungln_ang = ', RadDiag_Data%Scalar%sungln_ang
    WRITE(*,*) '  RadDiag_Data water_frac = ', RadDiag_Data%Scalar%water_frac
    WRITE(*,*) '  RadDiag_Data land_frac  = ', RadDiag_Data%Scalar%land_frac 
    WRITE(*,*) '  RadDiag_Data ice_frac   = ', RadDiag_Data%Scalar%ice_frac  
    WRITE(*,*) '  RadDiag_Data snow_frac  = ', RadDiag_Data%Scalar%snow_frac 
    WRITE(*,*) '  RadDiag_Data water_temp = ', RadDiag_Data%Scalar%water_temp
    WRITE(*,*) '  RadDiag_Data land_temp  = ', RadDiag_Data%Scalar%land_temp 
    WRITE(*,*) '  RadDiag_Data ice_temp   = ', RadDiag_Data%Scalar%ice_temp  
    WRITE(*,*) '  RadDiag_Data snow_temp  = ', RadDiag_Data%Scalar%snow_temp 
    WRITE(*,*) '  RadDiag_Data soil_temp  = ', RadDiag_Data%Scalar%soil_temp 
    WRITE(*,*) '  RadDiag_Data soil_mois  = ', RadDiag_Data%Scalar%soil_mois 
    WRITE(*,*) '  RadDiag_Data land_type  = ', RadDiag_Data%Scalar%land_type 
    WRITE(*,*) '  RadDiag_Data veg_frac   = ', RadDiag_Data%Scalar%veg_frac  
    WRITE(*,*) '  RadDiag_Data snow_depth = ', RadDiag_Data%Scalar%snow_depth
    WRITE(*,*) '  RadDiag_Data sfc_wndspd = ', RadDiag_Data%Scalar%sfc_wndspd
    WRITE(*,*) '  RadDiag_Data qcdiag1    = ', RadDiag_Data%Scalar%qcdiag1   
    WRITE(*,*) '  RadDiag_Data qcdiag2    = ', RadDiag_Data%Scalar%qcdiag2   

    DO i = 1, RadDiag_Data%nChannels
      WRITE(*,*)
      WRITE(*,*) '  Channel Index: ', i
      WRITE(*,*) '    tbobs  = ', RadDiag_Data%Channel(i)%tbobs 
      WRITE(*,*) '    omgbc  = ', RadDiag_Data%Channel(i)%omgbc 
      WRITE(*,*) '    omgnbc = ', RadDiag_Data%Channel(i)%omgnbc
      WRITE(*,*) '    errinv = ', RadDiag_Data%Channel(i)%errinv
      WRITE(*,*) '    qcmark = ', RadDiag_Data%Channel(i)%qcmark
      WRITE(*,*) '    emiss  = ', RadDiag_Data%Channel(i)%emiss 
      WRITE(*,*) '    tlap   = ', RadDiag_Data%Channel(i)%tlap  
      WRITE(*,*) '    bifix  = ', RadDiag_Data%Channel(i)%bifix 
      WRITE(*,*) '    bilap  = ', RadDiag_Data%Channel(i)%bilap 
      WRITE(*,*) '    bilap2 = ', RadDiag_Data%Channel(i)%bilap2
      WRITE(*,*) '    bicons = ', RadDiag_Data%Channel(i)%bicons
      WRITE(*,*) '    biang  = ', RadDiag_Data%Channel(i)%biang 
      WRITE(*,*) '    biclw  = ', RadDiag_Data%Channel(i)%biclw 
      READ(*,*)
    END DO

  END DO Read_Loop

END PROGRAM Test_RadDiag_IO
