!
! ECMWF83_Profile_Set
!
! Module containing the ECMWF83 atmospheric profile set data 
! definitions and access routines
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, 11-Nov2008
!                       paul.vandelst@noaa.gov
!

MODULE ECMWF83_Profile_Set

  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds      , ONLY: fp
  USE File_Utility    , ONLY: Get_Lun
  USE Message_Handler , ONLY: SUCCESS, FAILURE, WARNING, INFORMATION, &
                              Display_Message
  USE Units_Conversion, ONLY: PPMV_to_MR
  ! Disable implicit typing
  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------
  PRIVATE
  ! Parameters
  PUBLIC :: N_ECMWF83_LEVELS   
  PUBLIC :: N_ECMWF83_LAYERS   
  PUBLIC :: N_ECMWF83_ABSORBERS
  PUBLIC :: N_ECMWF83_PROFILES 
  ! Procedures
  PUBLIC :: Load_ECMWF83_Profile


  ! ----------
  ! Parameters
  ! ----------
  ! Module RCS Id string
  CHARACTER(*), PARAMETER :: MODULE_RCS_ID = &
  ! Message string length
  INTEGER,  PARAMETER :: ML = 512
  ! Invalid flag
  INTEGER,  PARAMETER :: INVALID = -1
  ! Literal constants
  REAL(fp), PARAMETER :: ZERO = 0.0_fp
  REAL(fp), PARAMETER :: ONE  = 1.0_fp
  REAL(fp), PARAMETER :: THOUSAND = 1000.0_fp
  ! The ECMWF data file name
  CHARACTER(*), PARAMETER :: ECMWF_DATA_FILE = './ECMWF83_Profile_Set/ECMWF_83P_91L.dat'
  ! The number of levels, layers, absorbers, and profiles
  INTEGER, PARAMETER :: N_ECMWF83_LEVELS    = 101
  INTEGER, PARAMETER :: N_ECMWF83_LAYERS    = N_ECMWF83_LEVELS - 1
  INTEGER, PARAMETER :: N_ECMWF83_ABSORBERS = 6
  INTEGER, PARAMETER :: N_ECMWF83_PROFILES  = 83
  ! The climatology model.
  INTEGER, PARAMETER :: ECMWF_CLIMATOLOGY_MODEL(N_ECMWF83_PROFILES) = &
    (/ 3, 1, 2, 1, 5, 5, 1, 4, 1, 5, &
       5, 1, 5, 3, 1, 5, 3, 6, 3, 4, &
       5, 1, 3, 1, 1, 1, 6, 3, 3, 6, &
       6, 5, 3, 6, 1, 5, 6, 4, 1, 5, &
       1, 1, 6, 1, 5, 5, 1, 3, 1, 2, &
       1, 5, 1, 3, 5, 6, 1, 4, 1, 5, &
       6, 5, 2, 1, 3, 1, 3, 3, 5, 3, &
       5, 5, 5, 5, 4, 5, 5, 3, 5, 5, &
       6, 6, 6 /)

  ! Absorber info
  INTEGER, PARAMETER :: ECMWF_ABSORBER_ID(N_ECMWF83_ABSORBERS) = (/ 1,2,3,4,5,6 /)
  INTEGER, PARAMETER :: ECMWF_ABSORBER_UNITS_ID(N_ECMWF83_ABSORBERS) = (/ 3,1,1,1,1,1 /)


CONTAINS


!------------------------------------------------------------------------------
!:sdoc+:
!
! NAME:
!       Load_ECMWF83_Profile
!
! PURPOSE:
!       Function to return the requested profile from the ECMWF83 set of
!       atmospheric profiles.
!
! CALLING SEQUENCE:
!       Error_Status = Load_ECMWF83_Profile( Profile,                               &  ! Input
!                                            Level_Pressure,                        &  ! Output
!                                            Level_Temperature,                     &  ! Output
!                                            Level_Absorber,                        &  ! Output
!                                            Absorber_ID       = Absorber_ID,       &  ! Optional output
!                                            Absorber_Units_ID = Absorber_Units_ID, &  ! Optional output
!                                            Description       = Description,       &  ! Optional output
!                                            Climatology_Model = Climatology_Model, &  ! Optional output
!                                            Year              = Year,              &  ! Optional output
!                                            Month             = Month,             &  ! Optional output
!                                            Day               = Day,               &  ! Optional output
!                                            Hour              = Hour,              &  ! Optional output
!                                            Latitude          = Latitude,          &  ! Optional output
!                                            Longitude         = Longitude,         &  ! Optional output
!                                            Surface_Altitude  = Surface_Altitude,  &  ! Optional output
!                                            RCS_Id            = RCS_Id,            &  ! Revision control
!                                            Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Profile:            The requested atmospheric profile number from the
!                           ECMWF set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN)
!
! OUTPUT ARGUMENTS:
!       Level_Pressure:     Pressure profile for the requested
!                           atmospheric profile.
!                           UNITS:      hectoPascals, hPa
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Temperature:  Temperature profile for the requested
!                           atmospheric profile.
!                           UNITS:      Kelvin, K
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Absorber:     Absorber profiles for the requested atmospheric
!                           profile.
!                           UNITS:      Variable. See ABSORBER_UNITS_ID argument
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Rank-2, n_levels x n_absorbers
!                           ATTRIBUTES: POINTER
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT(IN), OPTIONAL
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Absorber_ID:        The list of the HITRAN absorber numbers for the 
!                           molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Absorber_Units_ID:  The list of the absorber units ID numbers for
!                           the molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Description:        Description of the requested profile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Climatology_Model:  Climatology model for the requested profile.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Year:               Year in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Month:              Month in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Day:                Day on which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Hour:               Hour in which the requested profile sonde was launched.
!                           UNITS:      UTC
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Latitude:           Latitude for the requested profile.
!                           UNITS:      Degrees North (-90 -> +90).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Longitude:          Longitude for the requested profile.
!                           UNITS:      Degrees East (0 -> 360).
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       Surface_Altitude:   Surface altitude for the requested profile.
!                           UNITS:      Metres, m.
!                           TYPE:       REAL(fp)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER(*)
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT(OUT)
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error
!                           status. The error codes are defined in the
!                           ERROR_HANDLER module.
!                           If == SUCCESS the profile data load was successful.
!                              == FAILURE an unrecoverable error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
!:sdoc-:
!------------------------------------------------------------------------------

  FUNCTION Load_ECMWF83_Profile( Profile,           &  ! Input
                                 Level_Pressure,    &  ! Output           
                                 Level_Temperature, &  ! Output           
                                 Level_Absorber,    &  ! Output           
                                 Absorber_ID,       &  ! Optional output  
                                 Absorber_Units_ID, &  ! Optional output  
                                 Description,       &  ! Optional output  
                                 Climatology_Model, &  ! Optional output  
                                 Year,              &  ! Optional output  
                                 Month,             &  ! Optional output  
                                 Day,               &  ! Optional output  
                                 Hour,              &  ! Optional output  
                                 Latitude,          &  ! Optional output  
                                 Longitude,         &  ! Optional output  
                                 Surface_Altitude,  &  ! Optional output  
                                 RCS_Id,            &  ! Revision control 
                                 Message_Log )      &  ! Error messaging  
                               RESULT( Error_Status )
    ! Arguments
    INTEGER     ,            INTENT(IN)  :: Profile
    REAL(fp)    ,            POINTER     :: Level_Pressure(:)
    REAL(fp)    ,            POINTER     :: Level_Temperature(:)
    REAL(fp)    ,            POINTER     :: Level_Absorber(:,:)
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Absorber_ID(:)
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Absorber_Units_ID(:)
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: Description
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Climatology_Model
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Year
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Month
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Day
    INTEGER     , OPTIONAL,  INTENT(OUT) :: Hour
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Latitude
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Longitude
    REAL(fp)    , OPTIONAL,  INTENT(OUT) :: Surface_Altitude
    CHARACTER(*), OPTIONAL,  INTENT(OUT) :: RCS_Id
    CHARACTER(*), OPTIONAL,  INTENT(IN)  :: Message_Log
    ! Function result
    INTEGER :: Error_Status
    ! Local parameters
    CHARACTER(*), PARAMETER :: ROUTINE_NAME = 'Load_ECMWF83_Profile'
    ! Local variables
    CHARACTER(ML) :: Message
    CHARACTER(80) :: Line_Buffer, Hdr_Buffer
    INTEGER :: Allocate_Status
    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: j, m
    REAL(fp) :: Data_Array(8,N_ECMWF83_LEVELS)
    REAL(fp) :: ECMWF_ppmv_H2O(N_ECMWF83_LEVELS)
    REAL(fp) :: ECMWF_Latitude
    REAL(fp) :: ECMWF_Longitude
    REAL(fp) :: ECMWF_Surface_Pressure
    REAL(fp) :: ECMWF_Surface_Altitude
    INTEGER  :: ECMWF_Year
    INTEGER  :: ECMWF_Month
    INTEGER  :: ECMWF_Day
    INTEGER  :: ECMWF_Hour


    ! Setup
    ! -----
    Error_Status = SUCCESS
    IF ( PRESENT(RCS_Id) ) RCS_Id = MODULE_RCS_ID

    ! Is requested profile valid?
    IF ( Profile < 1 .OR. Profile > N_ECMWF83_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Invalid model profile number ",i0," specified.")' ) Profile
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Check output pointers
    IF ( ASSOCIATED(Level_Pressure) ) THEN
      DEALLOCATE( Level_Pressure, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Pressure output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( ASSOCIATED(Level_Temperature) ) THEN
      DEALLOCATE( Level_Temperature, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Temperature output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( ASSOCIATED(Level_Absorber) ) THEN
      DEALLOCATE( Level_Absorber, STAT=Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error deallocating Level_Absorber output array. STAT = ",i0)' ) &
                       Allocate_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF

    ! Check output array sizes
    IF ( PRESENT(Absorber_ID) ) THEN
      IF ( SIZE(Absorber_ID) /= N_ECMWF83_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Size of output Absorber_ID array must be ",i0," elements.")' ) &
                       N_ECMWF83_ABSORBERS
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF
    IF ( PRESENT(Absorber_Units_ID) ) THEN
      IF ( SIZE(Absorber_Units_ID) /= N_ECMWF83_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Size of output Absorber_Units_ID array must be ",i0," elements.")' ) &
                        N_ECMWF83_ABSORBERS
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        RETURN
      END IF
    END IF


    ! Allocate output pointer arrays
    ! ------------------------------
    ALLOCATE( Level_Pressure( N_ECMWF83_LEVELS ), &
              Level_Temperature( N_ECMWF83_LEVELS ), &
              Level_Absorber( N_ECMWF83_LEVELS, N_ECMWF83_ABSORBERS ), &
              STAT = Allocate_Status )
    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error allocating output arrays. STATs = ",i0)' ) Allocate_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF


    ! Open the data file
    ! ------------------
    FileID = Get_Lun()
    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF
    OPEN( FileID, FILE   = ECMWF_DATA_FILE, &
                  STATUS = 'OLD', &
                  ACCESS = 'SEQUENTIAL', &
                  FORM   = 'FORMATTED', &
                  ACTION = 'READ', &
                  IOSTAT = IO_Status )
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//ECMWF_DATA_FILE, &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Loop over comments in data file
    ! -------------------------------
    Comment_Read_loop: DO

      ! Read a line of the file
      READ( FileID,FMT='(a)',IOSTAT=IO_Status ) Line_Buffer
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading ",a," file in comment skip. IOSTAT = ",i0)' ) &
                        ECMWF_DATA_FILE, IO_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! Exit loop if this is NOT a comment line
      IF ( Line_Buffer(1:1) /= '!' ) THEN
        BACKSPACE( FileID )
        EXIT Comment_Read_loop
      END IF

    END DO Comment_Read_loop


    ! Read through data file
    ! ----------------------
    Profile_Read_Loop: DO m = 1, Profile

      ! Read profile header lines
      Line_Buffer = ' '; Hdr_Buffer = ' '
      READ( FileID,FMT='(a,/,a)',IOSTAT=IO_Status ) Line_Buffer, Hdr_Buffer
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading header line for profile #",i0,". IOSTAT = ",i0)' ) &
                        m, IO_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! Read profile data
      READ( FileID,FMT=*,IOSTAT=IO_Status ) Data_Array
      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message,'("Error reading profile data for profile #",i0,". IOSTAT = ",i0)' ) &
                        m, IO_Status
        CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO Profile_Read_Loop
    CLOSE( FileID )


    ! Extract data from the header lines. Note that the land-fraction is not read
    ! ---------------------------------------------------------------------------
    ! Read the data from the line buffer
    READ( Line_Buffer,FMT='(8x,i5)',IOSTAT=IO_Status) m
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error reading Line_Buffer for profile #",i0,". IOSTAT = ",i0)' ) &
                      Profile, IO_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! Check that the profile number is in the correct sequence
    IF ( Profile /= m ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Profile # from header, ",i0, &
                      &", is different from that requested, ",i0,".")' ) m, Profile
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      RETURN
    END IF

    ! Read the data from the header line buffer
    READ( Hdr_Buffer,FMT=*,IOSTAT=IO_Status) ECMWF_Year, &
                                             ECMWF_Month, &
                                             ECMWF_Day, &
                                             ECMWF_Latitude, &
                                             ECMWF_Longitude, &
                                             ECMWF_Surface_Pressure
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message,'("Error reading Hdr_Buffer for profile #",i0,". IOSTAT = ",i0)' ) &
                      Profile, IO_Status
      CALL Display_Message( ROUTINE_NAME,TRIM(Message),Error_Status,Message_Log=Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
    
    ! Set defaults
    ECMWF_Hour = 0
    ECMWF_Surface_Altitude = ZERO


    ! Extract data from the array block
    ! ---------------------------------
    Level_Pressure     = Data_Array( 1, N_ECMWF83_LEVELS:1:-1 )
    Level_Temperature  = Data_Array( 2, N_ECMWF83_LEVELS:1:-1 )
    DO j = 1, N_ECMWF83_ABSORBERS
      Level_Absorber(:,j) = Data_Array( j+2, N_ECMWF83_LEVELS:1:-1 )
    END DO
    ECMWF_ppmv_H2O = Level_Absorber(:,1)


    ! Convert water vapour profile data, ppmv -> mass mixing ratio (g/kg)
    ! -------------------------------------------------------------------
    CALL PPMV_to_MR( ECMWF_ppmv_H2O, &
                     Level_Absorber(:,1), &
                     Molecule_ID=ECMWF_ABSORBER_ID(1) )
    IF ( ANY(Level_Absorber(:,1) < ZERO) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error converting water vapor units.', &
                            Error_Status, &
                            Message_Log=Message_Log )
      RETURN
    END IF


    ! Assign the optional output arguments
    ! ------------------------------------
    IF ( PRESENT(Absorber_ID)       ) Absorber_ID       = ECMWF_ABSORBER_ID
    IF ( PRESENT(Absorber_Units_ID) ) Absorber_Units_ID = ECMWF_ABSORBER_UNITS_ID

    IF ( PRESENT(Description) ) THEN
      Description = ' '
      WRITE( Description,'("ECMWF 83 diverse profiles on 101L. Profile #",i0)' ) Profile
    END IF
    IF ( PRESENT(Climatology_Model) ) Climatology_Model = ECMWF_CLIMATOLOGY_MODEL( Profile )

    IF ( PRESENT(Year)  ) Year  = ECMWF_Year
    IF ( PRESENT(Month) ) Month = ECMWF_Month
    IF ( PRESENT(Day)   ) Day   = ECMWF_Day
    IF ( PRESENT(Hour)  ) Hour  = ECMWF_Hour

    IF ( PRESENT(Latitude)         ) Latitude         = ECMWF_Latitude
    IF ( PRESENT(Longitude)        ) Longitude        = ECMWF_Longitude
    IF ( PRESENT(Surface_Altitude) ) Surface_Altitude = ECMWF_Surface_Altitude

  END FUNCTION Load_ECMWF83_Profile

END MODULE ECMWF83_Profile_Set
