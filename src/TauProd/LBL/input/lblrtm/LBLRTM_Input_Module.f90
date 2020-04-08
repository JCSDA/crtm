!
! LBLRTM_Input_Module
!
! Module containing routines for creating LBLRTM input files.
!
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, 23-Jan-2000
!                     paul.vandelst@noaa.gov
!

MODULE LBLRTM_Input_Module

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module usage
  USE Message_Handler, ONLY: SUCCESS, FAILURE, INFORMATION, Display_Message
  USE File_Utility   , ONLY: Get_Lun
  ! LBL module usage
  USE COMMON_r1p1_Module  , LBLRTM_r1p1_type  => COMMON_r1p1_type, &
                            LBLRTM_r1p1_Write => COMMON_r1p1_Write
  USE LBLRTM_r1p2_Module
  USE COMMON_r1p2a_Module , LBLRTM_r1p2a_type  => COMMON_r1p2a_type, &
                            LBLRTM_r1p2a_Write => COMMON_r1p2a_Write
  USE LBLRTM_r1p2p1_Module
  USE LBLRTM_r1p3_Module
  USE COMMON_r1p3a_Module , LBLRTM_r1p3a_type  => COMMON_r1p3a_type, &
                            LBLRTM_r1p3a_Write => COMMON_r1p3a_Write
  USE COMMON_r1p3b_Module , LBLRTM_r1p3b_type  => COMMON_r1p3b_type, &
                            LBLRTM_r1p3b_Write => COMMON_r1p3b_Write
  USE LBLRTM_r1p4_Module
  USE COMMON_r2p1_Module  , LBLRTM_r2p1_type  => COMMON_r2p1_type, &
                            LBLRTM_r2p1_Write => COMMON_r2p1_Write
  USE LBLRTM_r2p1p1_Module
  USE COMMON_r2p1p2_Module, LBLRTM_r2p1p2_type  => COMMON_r2p1p2_type, &
                            LBLRTM_r2p1p2_Write => COMMON_r2p1p2_Write
  USE LBLRTM_r2p2_Module
  USE LBLRTM_r2p2p1_Module
  USE LBLRTM_r2p2p2_Module
  USE LBLRTM_r3p1_Module
  USE COMMON_r3p2_Module  , LBLRTM_r3p2_type  => COMMON_r3p2_type, &
                            LBLRTM_r3p2_Write => COMMON_r3p2_Write
  USE COMMON_r3p2h_Module , LBLRTM_r3p2h_type  => COMMON_r3p2h_type, &
                            LBLRTM_r3p2h_Write => COMMON_r3p2h_Write
  USE COMMON_r3p3a_Module , LBLRTM_r3p3a_type  => COMMON_r3p3a_type, &
                            LBLRTM_r3p3a_Write => COMMON_r3p3a_Write
  USE COMMON_r3p3b_Module , LBLRTM_r3p3b_type  => COMMON_r3p3b_type, &
                            LBLRTM_r3p3b_Write => COMMON_r3p3b_Write
  USE COMMON_r3p4_Module  , LBLRTM_r3p4_type  => COMMON_r3p4_type, &
                            LBLRTM_r3p4_Write => COMMON_r3p4_Write
  USE COMMON_r3p5_Module  , LBLRTM_r3p5_type  => COMMON_r3p5_type, &
                            LBLRTM_r3p5_Write => COMMON_r3p5_Write
  USE COMMON_r3p6_Module  , LBLRTM_r3p6_type  => COMMON_r3p6_type, &
                            LBLRTM_r3p6_Write => COMMON_r3p6_Write
  USE COMMON_r3p7_Module  , LBLRTM_r3p7_type  => COMMON_r3p7_type, &
                            LBLRTM_r3p7_Write => COMMON_r3p7_Write
  USE COMMON_r3p7p1_Module, LBLRTM_r3p7p1_type  => COMMON_r3p7p1_type, &
                            LBLRTM_r3p7p1_Write => COMMON_r3p7p1_Write
  USE COMMON_r3p8_Module  , LBLRTM_r3p8_type  => COMMON_r3p8_type, &
                            LBLRTM_r3p8_Write => COMMON_r3p8_Write
  USE COMMON_r3p8p1_Module, LBLRTM_r3p8p1_type  => COMMON_r3p8p1_type, &
                            LBLRTM_r3p8p1_Write => COMMON_r3p8p1_Write
  USE COMMON_r3p8p2_Module, LBLRTM_r3p8p2_type  => COMMON_r3p8p2_type, &
                            LBLRTM_r3p8p2_Write => COMMON_r3p8p2_Write
  USE LBLRTM_r6_Module
  USE LBLRTM_r6p1_Module
  USE LBLRTM_r7p1_Module
  USE LBLRTM_r7p2_Module
  USE LBLRTM_r7p3_Module
  USE LBLRTM_r8p1_Module
  USE LBLRTM_r9p1_Module
  USE LBLRTM_r10p1_Module
  USE LBLRTM_r10p2_Module
  USE LBLRTM_r11p1_Module
  USE LBLRTM_r11p2_Module
  USE LBLRTM_r11p3_Module
  ! Disable implicit typing
  IMPLICIT NONE


  ! -------------
  ! Derived types
  ! -------------
  TYPE :: LBLRTM_Input_type
    TYPE(LBLRTM_r1p1_type)                :: r1p1
    TYPE(LBLRTM_r1p2_type)                :: r1p2
    TYPE(LBLRTM_r1p2a_type)               :: r1p2a
    TYPE(LBLRTM_r1p2p1_type)              :: r1p2p1
    TYPE(LBLRTM_r1p3_type)                :: r1p3
    TYPE(LBLRTM_r1p3a_type)               :: r1p3a
    TYPE(LBLRTM_r1p3b_type)               :: r1p3b
    TYPE(LBLRTM_r1p4_type)                :: r1p4
    TYPE(LBLRTM_r2p1_type)                :: r2p1
    TYPE(LBLRTM_r2p1p1_type), ALLOCATABLE :: r2p1p1(:)  ! r2p1%nlayrs
    TYPE(LBLRTM_r2p1p2_type), ALLOCATABLE :: r2p1p2(:)  ! r2p1%nlayrs
    TYPE(LBLRTM_r2p1p3_type), ALLOCATABLE :: r2p1p3(:)  ! r2p1%nlayrs
    TYPE(LBLRTM_r2p2_type)                :: r2p2   
    TYPE(LBLRTM_r2p2p1_type)              :: r2p2p1 
    TYPE(LBLRTM_r2p2p2_type)              :: r2p2p2 
    TYPE(LBLRTM_r2p2p3_type), ALLOCATABLE :: r2p2p3(:)  ! r2p2p2%nlayxs
    TYPE(LBLRTM_r2p2p4_type), ALLOCATABLE :: r2p2p4(:)  ! r2p2p2%nlayxs
    TYPE(LBLRTM_r2p2p5_type), ALLOCATABLE :: r2p2p5(:)  ! r2p2p2%nlayxs
    TYPE(LBLRTM_r3p1_type)                :: r3p1
    TYPE(LBLRTM_r3p2_type)                :: r3p2
    TYPE(LBLRTM_r3p2h_type)               :: r3p2h
    TYPE(LBLRTM_r3p3a_type)               :: r3p3a
    TYPE(LBLRTM_r3p3b_type)               :: r3p3b
    TYPE(LBLRTM_r3p4_type)                :: r3p4
    TYPE(LBLRTM_r3p5_type)  , ALLOCATABLE :: r3p5(:)    ! r3p4%immax
    TYPE(LBLRTM_r3p6_type)  , ALLOCATABLE :: r3p6(:)    ! r3p4%immax
    TYPE(LBLRTM_r3p7_type)                :: r3p7
    TYPE(LBLRTM_r3p7p1_type)              :: r3p7p1
    TYPE(LBLRTM_r3p8_type)                :: r3p8
    TYPE(LBLRTM_r3p8p1_type), ALLOCATABLE :: r3p8p1(:)  ! r3p8%layx
    TYPE(LBLRTM_r3p8p2_type), ALLOCATABLE :: r3p8p2(:)  ! r3p8%layx
    TYPE(LBLRTM_r6_type)                  :: r6
    TYPE(LBLRTM_r6p1_type)                :: r6p1
    TYPE(LBLRTM_r7p1_type)                :: r7p1
    TYPE(LBLRTM_r7p2_type)                :: r7p2
    TYPE(LBLRTM_r7p3_type)                :: r7p3
    TYPE(LBLRTM_r8p1_type)  , ALLOCATABLE :: r8p1(:)    ! # of scanning functions
    TYPE(LBLRTM_r9p1_type)  , ALLOCATABLE :: r9p1(:)    ! # of interpolating functions
    TYPE(LBLRTM_r10p1_type) , ALLOCATABLE :: r10p1(:)   ! # of FFT scanning functions
    TYPE(LBLRTM_r10p2_type) , ALLOCATABLE :: r10p2(:)   ! # of FFT scanning functions
    TYPE(LBLRTM_r11p1_type) , ALLOCATABLE :: r11p1(:)   ! # of SRFs
    TYPE(LBLRTM_r11p2_type) , ALLOCATABLE :: r11p2(:)   ! # of SRFs
    TYPE(LBLRTM_r11p3_type) , ALLOCATABLE :: r11p3(:)   ! # of SRFs
  END TYPE LBLRTM_Input_type
  

  ! ----------
  ! Visibility
  ! ----------
!  PRIVATE
  ! Datatypes
  PUBLIC :: LBLRTM_Input_type
  ! Procedures
  PUBLIC :: LBLRTM_Input_WriteFile


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*), PARAMETER :: MODULE_VERSION_ID = &
  INTEGER, PARAMETER :: ML = 256
  
  
CONTAINS


  FUNCTION LBLRTM_Input_WriteFile( &
    lblrtm_input, &
    filename    , &
    quiet       ) &
  RESULT( err_stat )
    ! Arguments
    TYPE(LBLRTM_Input_type), INTENT(IN) :: lblrtm_input
    CHARACTER(*)           , INTENT(IN) :: filename
    LOGICAL,       OPTIONAL, INTENT(IN) :: quiet
    ! Function result
    INTEGER :: err_stat
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'LBLRTM_Input_WriteFile'
    ! Local variables
    CHARACTER(ML) :: msg
    CHARACTER(ML) :: io_msg
    LOGICAL :: noisy
    INTEGER :: fid
    INTEGER :: io_stat

    ! Setup
    err_stat = SUCCESS
    ! ...Check Quiet argument
    noisy = .TRUE.
    IF ( PRESENT(Quiet) ) noisy = .NOT. Quiet


    ! Open the file
    fid = Get_Lun()
    IF ( fid < 0 ) THEN
      err_stat = FAILURE
      msg = 'Error obtaining unit number for output file '//TRIM(filename)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF
    OPEN( UNIT   = fid        , &
          FILE   = filename   , &
          STATUS = 'REPLACE'  , &
          FORM   = 'FORMATTED', &
          IOSTAT = io_stat    , &
          IOMSG  = io_msg       )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error opening '//TRIM(filename)//' for output - '//TRIM(io_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF


    ! Write Record 1.1
    err_stat = LBLRTM_R1p1_Write(lblrtm_input%R1p1,fid)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Record 1.1 to '//TRIM(filename)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF
    

    ! Write Record 1.2
    err_stat = LBLRTM_R1p2_Write(lblrtm_input%R1p2,fid)
    IF ( err_stat /= SUCCESS ) THEN
      msg = 'Error writing Record 1.2 to '//TRIM(filename)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF
    
    
    ! Write Record 1.2a
    IF ( lblrtm_input%R1p2%icntnm == 6 ) THEN
      err_stat = LBLRTM_R1p2a_Write(lblrtm_input%R1p2a,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.2a to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF


    ! Write Record 1.2.1
    IF ( lblrtm_input%R1p2%iemit == 2 ) THEN
      err_stat = LBLRTM_R1p2p1_Write(lblrtm_input%R1p2p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.2.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF


    ! Write Record 1.3x
    IF ( lblrtm_input%R1p2%ihirac > 0 .OR. &
         lblrtm_input%R1p2%iaersl > 0 .OR. &
         lblrtm_input%R1p2%iemit == 1 .OR. &
         lblrtm_input%R1p2%iatm  == 1 .OR. &
         lblrtm_input%R1p2%ilas   > 0      ) THEN
      err_stat = LBLRTM_R1p3_Write(lblrtm_input%R1p3,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.3 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! ...Write Record 1.3.a - 1.3.b...N
      IF ( lblrtm_input%R1p3%nmol_scal > 0 ) THEN
        err_stat = LBLRTM_R1p3a_Write(lblrtm_input%R1p3a,fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing Record 1.3a to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        err_stat = LBLRTM_R1p3b_Write(lblrtm_input%R1p3b,fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing Record 1.3b...N to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
      END IF
    END IF


    ! Write Record 1.4
    IF ( lblrtm_input%R1p2%iemit == 1 .OR. &
         (lblrtm_input%R1p2%iemit == 2 .AND. lblrtm_input%R1p2p1%iotflg == 2) ) THEN
      err_stat = LBLRTM_R1p4_Write(lblrtm_input%R1p4,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.4 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF


    ! Write Record 1.5
    IF ( lblrtm_input%R1p2%iemit == 3 .AND. &
         (lblrtm_input%R1p2%imrg == 40 .OR. &
          lblrtm_input%R1p2%imrg == 41 .OR. &
          lblrtm_input%R1p2%imrg == 42 .OR. &
          lblrtm_input%R1p2%imrg == 43      ) ) THEN
      err_stat = LBLRTM_R1p5_Write(lblrtm_input%R1p5,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.5 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF

    
    ! Write Record 1.6
    IF ( lblrtm_input%R1p2%imrg == 35 .OR. lblrtm_input%R1p2%imrg == 36 .OR. &
         lblrtm_input%R1p2%imrg == 40 .OR. lblrtm_input%R1p2%imrg == 41 .OR. &
         lblrtm_input%R1p2%imrg == 45 .OR. lblrtm_input%R1p2%imrg == 46      ) THEN
      err_stat = LBLRTM_R1p6_Write(lblrtm_input%R1p6,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 1.6 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF

    
    ! Write records applicable only if LBLATM not selected (IATM=0)
    IF ( lblrtm_input%R1p2%iatm == 0 ) THEN
      ! Record 2.1
      err_stat = LBLRTM_R2p1_Write(lblrtm_input%R2p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 2.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Loop over layers
      nlayrs_loop: DO k = 1, lblrtm_input%R2p1%nlayrs
        WRITE(clayer,'(i0)') k
        ! ...Record 2.1.1
        err_stat = LBLRTM_R2p1p1_Write(lblrtm_input%R2p1p1(k),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing layer '//TRIM(clayer)//' Record 2.1.1 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! ...Record 2.1.2
        err_stat = LBLRTM_R2p1p2_Write(lblrtm_input%R2p1p2(k),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing layer '//TRIM(clayer)//' Record 2.1.2 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! ...Record 2.1.3
        IF ( lblrtm_input%R2p1p1%nmol > 7 ) THEN
          err_stat = LBLRTM_R2p1p3_Write(lblrtm_input%R2p1p3(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing layer '//TRIM(clayer)//' Record 2.1.3 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
        END IF
      END DO nlayrs_loop
    END IF


    ! Write records applicable only if LBLATM not selected (IATM=0)
    ! and cross-sections are selection (IXSECT=1)
    IF ( lblrtm_input%R1p2%iatm == 0 .AND. lblrtm_input%R1p2%ixsect == 1) THEN
      ! Record 2.2
      err_stat = LBLRTM_R2p2_Write(lblrtm_input%R2p2,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 2.2 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 2.2.1
      err_stat = LBLRTM_R2p2p1_Write(lblrtm_input%R2p2p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 2.2.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 2.2.2
      err_stat = LBLRTM_R2p2p2_Write(lblrtm_input%R2p2p2,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 2.2.2 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Loop over layers
      nlayxs_loop: DO k = 1, lblrtm_input%R2p2p2%nlayxs
        WRITE(clayer,'(i0)') k
        ! ...Record 2.2.3
        err_stat = LBLRTM_R2p2p3_Write(lblrtm_input%R2p2p3(k),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing layer '//TRIM(clayer)//' Record 2.2.3 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! ...Record 2.2.4
        err_stat = LBLRTM_R2p2p4_Write(lblrtm_input%R2p2p4(k),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing layer '//TRIM(clayer)//' Record 2.2.4 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! ...Record 2.2.5
        IF ( lblrtm_input%R2p2p2%ixmol > 7 ) THEN
          err_stat = LBLRTM_R2p2p5_Write(lblrtm_input%R2p2p5(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing layer '//TRIM(clayer)//' Record 2.2.5 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
        END IF
      END DO nlayxs_loop
    END IF
    
    
    ! Write records applicable if LBLATM selected (IATM=1)
    IF ( lblrtm_input%R1p2%iatm == 1 ) THEN
      ! Record 3.1
      err_stat = LBLRTM_R3p1_Write(lblrtm_input%R3p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 3.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Path type (slant or horizontal)
      path_type: SELECT CASE(lblrtm_input%R3p1%itype)
        CASE(1)   ! Horizontal path
          ! Record 3.2h
          err_stat = LBLRTM_R3p2h_Write(lblrtm_input%R3p2h,fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing Record 3.2h to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
        CASE(2,3) ! Slant path
          ! Record 3.2
          err_stat = LBLRTM_R3p2_Write(lblrtm_input%R3p2,fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing Record 3.2 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          ! Records applicable to calculation boundary definition
          IF ( lblrtm_input%R3p1%ibmax == 0 ) THEN
            ! Record 3.3a - automatic definition based on spectroscopy
            err_stat = LBLRTM_R3p3a_Write(lblrtm_input%R3p3a,fid)
            IF ( err_stat /= SUCCESS ) THEN
              msg = 'Error writing Record 3.3a to '//TRIM(filename)
              CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
            END IF
          ELSE
            ! Record 3.3b - explicit definition of boundaries
            err_stat = LBLRTM_R3p3b_Write(lblrtm_input%R3p3b,fid)
            IF ( err_stat /= SUCCESS ) THEN
              msg = 'Error writing Record 3.3b to '//TRIM(filename)
              CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
            END IF
          END IF
        CASE DEFAULT
          err_stat = FAILURE
          msg = 'Invalid type of path specified in Record 3.1'
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END SELECT path_type
      ! User defined atmospheric profile
      user_atm_profile: IF ( lblrtm_input%R3p1%model == 0 ) THEN
        ! Record 3.4
        err_stat = LBLRTM_R3p4_Write(lblrtm_input%R3p4,fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing Record 3.4 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! Loop over levels
        immax_loop: DO k = 1, lblrtm_input%R3p4%immax
          WRITE(clevel,'(i0)') k
          ! ...Record 3.5
          err_stat = LBLRTM_R3p5_Write(lblrtm_input%R3p5(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing level '//TRIM(clevel)//' Record 3.5 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          ! ...Record 3.6
          err_stat = LBLRTM_R3p6_Write(lblrtm_input%R3p6(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing level '//TRIM(clevel)//' Record 3.6 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
        END DO immax_loop
      END IF user_atm_profile
    END IF
    
    
    ! Write records applicable only if both LBLATM selected (IATM=1)
    ! and cross-sections are selected (IXSECT=1)
    IF ( lblrtm_input%R1p2%iatm == 1 .AND. lblrtm_input%R1p2%ixsect == 1) THEN
      ! Record 3.7
      err_stat = LBLRTM_R3p7_Write(lblrtm_input%R3p7,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 3.7 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 3.7.1
      err_stat = LBLRTM_R3p7p1_Write(lblrtm_input%R3p7p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 3.7.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! User defined atmospheric profile
      user_atmx_profile: IF ( lblrtm_input%R3p7%iprfl == 0 ) THEN
        ! Record 3.8
        err_stat = LBLRTM_R3p8_Write(lblrtm_input%R3p8,fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing Record 3.8 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! Loop over layers
        layx_loop: DO k = 1, lblrtm_input%R3p8%layx
          WRITE(clayer,'(i0)') k
          ! ...Record 3.8.1
          err_stat = LBLRTM_R3p8p1_Write(lblrtm_input%R3p8p1(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing layer '//TRIM(clayer)//' Record 3.8.1 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          ! ...Record 3.8.2
          err_stat = LBLRTM_R3p8p1_Write(lblrtm_input%R3p8p2(k),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing layer '//TRIM(clayer)//' Record 3.8.2 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
        END DO layx_loop
      END IF user_atmx_profile
    END IF


    ! Record 4 and 5 output not implemented
    
    
    ! SCNMRG Scanned sequential results
    IF ( (lblrtm_input%R1p2%imrg >= 13 .AND. lblrtm_input%R1p2%imrg <= 18) .OR. &
         lblrtm_input%R1p2%imrg == 35 .OR. &
         lblrtm_input%R1p2%imrg == 36 .OR. &
         lblrtm_input%R1p2%imrg == 45 .OR. &
         lblrtm_input%R1p2%imrg == 46      ) THEN
      ! Record 6
      err_stat = LBLRTM_R6_Write(lblrtm_input%R6,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 6 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 6.1
      IF ( lblrtm_input%R1p2%imrg > 18 ) THEN
        err_stat = LBLRTM_R6p1_Write(lblrtm_input%R6p1,fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing Record 6.1 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
      END IF
    END IF
    
    
    ! FLTMRG Filtered sequential results
    IF ( lblrtm_input%R1p2%imrg >= 23 .AND. lblrtm_input%R1p2%imrg <= 28 ) THEN
      ! Record 7.1
      err_stat = LBLRTM_R7p1_Write(lblrtm_input%R7p1,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 7.1 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 7.2
      err_stat = LBLRTM_R7p2_Write(lblrtm_input%R7p2,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 7.2 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
      ! Record 7.3
      err_stat = LBLRTM_R7p3_Write(lblrtm_input%R7p3,fid)
      IF ( err_stat /= SUCCESS ) THEN
        msg = 'Error writing Record 7.3 to '//TRIM(filename)
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
      END IF
    END IF
    
    
    ! Post-processing options
    post_select: SELECT CASE(lblrtm_input%R1p2%iscan)
      ! Scanning function
      CASE(1)
        scanfn_loop: DO i = 1, SIZE(lblrtm_input%R8p1)
          WRITE(icount,'(i0)') i
          ! Record 8.1
          err_stat = LBLRTM_R8p1_Write(lblrtm_input%R8p1(i),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing SCANFN #'//TRIM(icount)//' Record 8.1 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          IF ( lblrtm_input%R8p1(i)%hwhm < ZERO ) EXIT scanfn_loop
        END DO scanfn_loop
      ! Interpolation
      CASE(2)
        intrpl_loop: DO i = 1, SIZE(lblrtm_input%R9p1)
          WRITE(icount,'(i0)') i
          ! Record 9.1
          err_stat = LBLRTM_R9p1_Write(lblrtm_input%R9p1(i),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing INTRPL #'//TRIM(icount)//' Record 9.1 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          IF ( lblrtm_input%R9p1(i)%dvo < ZERO ) EXIT intrpl_loop
        END DO intrpl_loop
      ! FFT scanning function
      CASE(3)
        fftscn_loop: DO i = 1, SIZE(lblrtm_input%R10p1)
          WRITE(icount,'(i0)') i
          ! Record 10.1
          err_stat = LBLRTM_R10p1_Write(lblrtm_input%R10p1(i),fid)
          IF ( err_stat /= SUCCESS ) THEN
            msg = 'Error writing FFTSCN #'//TRIM(icount)//' Record 10.1 to '//TRIM(filename)
            CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
          END IF
          IF ( lblrtm_input%R10p1(i)%hwhm < ZERO ) EXIT fftscn_loop
          IF ( ABS(lblrtm_input%R10p1(i)%jfnin) > 10 ) THEN
            ! Record 10.2
            err_stat = LBLRTM_R10p2_Write(lblrtm_input%R10p2(i),fid)
            IF ( err_stat /= SUCCESS ) THEN
              msg = 'Error writing FFTSCN #'//TRIM(icount)//' Record 10.2 to '//TRIM(filename)
              CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
            END IF
          END IF
        END DO fftscn_loop
      CASE DEFAULT
        err_stat = FAILURE
        msg = 'Invalid ISCAN post-processing option specified in Record 1.2'
        CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END SELECT post_select
    
    
    ! Spectral response function convolution
    IF ( lblrtm_input%R1p2%ifiltr == 1 ) THEN
      fltrfn_loop: DO i = 1, SIZE(lblrtm_input%R11p1)
        WRITE(icount,'(i0)') i
        ! Record 11.1
        err_stat = LBLRTM_R11p1_Write(lblrtm_input%R11p1(i),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing FLTRFN #'//TRIM(icount)//' Record 11.1 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        IF ( lblrtm_input%R11p1(i)%v1f < ZERO ) EXIT fltrfn_loop
        ! Record 11.2
        err_stat = LBLRTM_R11p2_Write(lblrtm_input%R11p2(i),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing FLTRFN #'//TRIM(icount)//' Record 11.2 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
        ! Record 11.3
        err_stat = LBLRTM_R11p3_Write(lblrtm_input%R11p3(i),fid)
        IF ( err_stat /= SUCCESS ) THEN
          msg = 'Error writing FLTRFN #'//TRIM(icount)//' Record 11.3 to '//TRIM(filename)
          CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
        END IF
      END DO fltrfn_loop
    END IF
    
    
    ! Close the file
    CLOSE( fid, IOSTAT=io_stat, IOMSG=io_msg )
    IF ( io_stat /= 0 ) THEN
      msg = 'Error closing '//TRIM(filename)//' - '//TRIM(io_msg)
      CALL Display_Message(ROUTINE_NAME, msg, err_stat); RETURN
    END IF


    ! Output an info message
    IF ( noisy ) THEN
      msg = 'FILE: '//TRIM(filename)//'; stuff and things...'
      CALL Display_Message( ROUTINE_NAME, msg, INFORMATION )
    END IF
    
  END FUNCTION LBLRTM_Input_WriteFile

END MODULE LBLRTM_Input_Module
