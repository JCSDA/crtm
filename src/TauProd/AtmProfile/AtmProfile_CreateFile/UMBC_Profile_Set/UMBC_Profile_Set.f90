
MODULE UMBC_Profile_Set


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Profile_Utility_Parameters
  USE Units_Conversion


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Load_UMBC_Profile


  ! ----------
  ! Parameters
  ! ----------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  ! -- The number of absorbers and profiles
  INTEGER, PUBLIC, PARAMETER :: N_UMBC_ABSORBERS = 5
  INTEGER, PUBLIC, PARAMETER :: N_UMBC_PROFILES  = 48

  ! -- The climatology model.
  ! -- u == unknown
  ! -- g == guess
  ! -- o == o.k.
  ! -- Some of the profiles have TIGR designations
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_UMBC_PROFILES ) :: UMBC_CLIMATOLOGY_MODEL = &
    (/ 1, 2, 3, 4, 5, &  ! AGFL standard atmospheres

    !  u  g  g  g  u
       6, 3, 3, 2, 6, &

    !  u  g  g  g  g  g  u  u  u  o
       6, 3, 2, 2, 3, 3, 6, 6, 6, 1, &

    !  u  u  u  u  u  g  g  g  g  g
       6, 6, 6, 6, 6, 3, 2, 3, 3, 3, &

    !  g  g  g  g  g  g  u  u  u  g
       3, 3, 3, 2, 2, 2, 6, 6, 6, 2, &

    !  u  u  u  u  u  u  g  u
       6, 6, 6, 6, 6, 6, 2, 6 /) 



  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC ABS, &
            ASSOCIATED, &
            PRESENT, &
            REAL, &
            SIZE, &
            TRIM


CONTAINS



  FUNCTION Load_UMBC_Profile( Profile,           &  ! Input

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

                              RCS_Id,            &  ! Optional output
                              Message_Log )      &  ! Error messaging

                            RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                                      INTENT( IN )  :: Profile

    ! -- Output
    REAL( fp_kind ),           DIMENSION( : ),    POINTER       :: Level_Pressure
    REAL( fp_kind ),           DIMENSION( : ),    POINTER       :: Level_Temperature
    REAL( fp_kind ),           DIMENSION( :, : ), POINTER       :: Level_Absorber

    ! -- Optional output
    INTEGER,         OPTIONAL, DIMENSION( : ),    INTENT( OUT ) :: Absorber_ID
    INTEGER,         OPTIONAL, DIMENSION( : ),    INTENT( OUT ) :: Absorber_Units_ID
    CHARACTER( * ),  OPTIONAL,                    INTENT( OUT ) :: Description
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Climatology_Model
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Year
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Month
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Day
    INTEGER,         OPTIONAL,                    INTENT( OUT ) :: Hour
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Latitude
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Longitude
    REAL( fp_kind ), OPTIONAL,                    INTENT( OUT ) :: Surface_Altitude

    CHARACTER( * ),  OPTIONAL,                    INTENT( OUT ) :: RCS_Id

    ! -- Error handler message log
    CHARACTER( * ),  OPTIONAL,                    INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Load_UMBC_Profile'
    INTEGER,         PARAMETER :: INVALID = -1
    REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status
    INTEGER :: IO_Status

    CHARACTER( 256 ) :: FileNAME
    INTEGER          :: FileID

    CHARACTER( 13 ) :: Fmt

    CHARACTER( 100 ) :: Buffer
    INTEGER :: n_Lines, i
    INTEGER :: n_Levels,    k
    INTEGER :: n_Absorbers, j, j_H2O, n
    INTEGER, DIMENSION(1) :: j_Idx

    REAL( fp_kind ) :: dummy

    INTEGER, DIMENSION( N_UMBC_ABSORBERS ) :: UMBC_Absorber_ID
    INTEGER, DIMENSION( N_UMBC_ABSORBERS ) :: UMBC_Absorber_Units_ID
    CHARACTER( 512 )                       :: UMBC_Description
    INTEGER                                :: UMBC_Year
    INTEGER                                :: UMBC_Month
    INTEGER                                :: UMBC_Day
    INTEGER                                :: UMBC_Hour
    REAL( fp_kind )                        :: UMBC_Latitude
    REAL( fp_kind )                        :: UMBC_Longitude
    REAL( fp_kind )                        :: UMBC_Surface_Altitude



    !#--------------------------------------------------------------------------#
    !#                   -- SET A SUCCESSFUL ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Is requested profile valid?
    ! ---------------------------

    IF ( Profile < 1 .OR. Profile > N_UMBC_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid model profile number ", i5, " specified." )' ) &
                      Profile
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------
    ! Does the requested profile file exist?
    ! --------------------------------------

    ! -- Construct the filename
    FileNAME = ' '

    IF ( Profile < 10 ) THEN
      Fmt = '( "myp", i1 )'
    ELSE
      Fmt = '( "myp", i2 )'
    END IF

    WRITE( FileNAME, FMT = Fmt ) Profile
    FileNAME = './UMBC_Profile_Set/'//TRIM( FileNAME )

    ! -- Check for existence
    IF ( .NOT. file_exists( TRIM( FileNAME ) ) ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'MYP file '//TRIM( FileNAME )//' not found.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ---------------------
    ! Check output pointers
    ! ---------------------

    ! -- Pressure
    IF ( ASSOCIATED( Level_Pressure ) ) THEN
      DEALLOCATE( Level_Pressure, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Pressure output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Temperature
    IF ( ASSOCIATED( Level_Temperature ) ) THEN
      DEALLOCATE( Level_Temperature, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Temperature output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF

    ! -- Absorber
    IF ( ASSOCIATED( Level_Absorber ) ) THEN
      DEALLOCATE( Level_Absorber, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Absorber output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! ------------------------
    ! Check output array sizes
    ! ------------------------

    ! -- Absorber ID array
    IF ( PRESENT( Absorber_ID ) ) THEN
      IF ( SIZE( Absorber_ID ) /= N_UMBC_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Size of output Absorber_ID array must be ", i1, " elements." )' ) &
                        N_UMBC_ABSORBERS
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -- Absorber Units ID array
    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      IF ( SIZE( Absorber_Units_ID ) /= N_UMBC_ABSORBERS ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Size of output Absorber_Units_ID array must be ", i1, " elements." )' ) &
                        N_UMBC_ABSORBERS
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF
    END IF


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- INITIALISE LOCAL VALUES --                      #
    !#--------------------------------------------------------------------------#

    UMBC_Description = ' '
    UMBC_Year              = 0
    UMBC_Month             = 0
    UMBC_Day               = 0
    UMBC_Hour              = 0
    UMBC_Latitude          = -999.0_fp_kind
    UMBC_Longitude         = -999.0_fp_kind
    UMBC_Surface_Altitude  = ZERO



    !#--------------------------------------------------------------------------#
    !#                            -- OPEN THE FILE --                           #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Get a file unit number
    ! ----------------------

    FileID = get_lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Could not obtain file ID for '//TRIM( FileNAME ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------
    ! Open the file
    ! -------------

    OPEN( FileID, FILE   = TRIM( FileNAME ), &
                  FORM   = 'FORMATTED',      &
                  ACCESS = 'SEQUENTIAL',     &
                  STATUS = 'OLD',            &
                  ACTION = 'READ',           &
                  IOSTAT = IO_Status         )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error opening ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( FileNAME ), IO_Status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE DIMENSIONS --                       #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Read the comment lines
    ! ----------------------

    n_Lines = 0

    DO

      READ( FileID, FMT = '( a )' ) Buffer
      IF ( Buffer(1:1) /= '!' ) EXIT

      ! -- Count the number of lines
      n_Lines = n_Lines + 1

      ! -- Determine trimmed length of comment
      i = LEN_TRIM( Buffer )

      ! -- DOn't want blank lines
      IF ( i == 1 ) CYCLE

      ! -- Create description string
      IF ( n_Lines > 1 ) THEN
        UMBC_Description = TRIM( UMBC_Description )//&
                           '; '//&
                           TRIM( Buffer(2:i) )
      ELSE
        UMBC_Description = TRIM( Buffer(2:i) )
      END IF

    END DO


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( Buffer, FMT    = '( 3x, i4, i3 )', &
                  IOSTAT = IO_Status ) n_Levels, n_Absorbers

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading dimensions in ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( FileNAME ), IO_Status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Check the absorber dimension
    IF ( n_Absorbers /= N_UMBC_ABSORBERS ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Input file absorber dimension, ", i5, &
                        &", different from expected, ", i2, "." )' ) &
                      n_Absorbers, N_UMBC_ABSORBERS
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                   -- ALLOCATE OUTPUT POINTER ARRAYS --                   #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Level_Pressure( n_Levels ), &
              Level_Temperature( n_Levels ), &
              Level_Absorber( n_Levels, N_UMBC_ABSORBERS ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error allocating output arrays. STATs = ", 4(1x,i5) )' ) &
                      Allocate_Status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE ABSORBER IDs --                       #
    !#--------------------------------------------------------------------------#

    READ( FileID, FMT    = *, &
                  IOSTAT = IO_Status ) UMBC_absorber_ID

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading absorber IDs from ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( FileNAME ), IO_Status
      CALL display_message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------------
    ! Determine the index for water vapour
    ! ------------------------------------
    
    ! -- Is there a H2O index?
    n = COUNT( UMBC_Absorber_ID == ID_H2O )

    IF ( n /= 1 ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'No H2O in absorber set.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

    ! -- Get it
    j_Idx = PACK( (/ ( j, j = 1, N_UMBC_ABSORBERS ) /), &
                  UMBC_Absorber_ID == ID_H2O  )
    j_H2O = j_idx(1)



    ! -------------------------------
    ! Set the absorber units ID array
    ! -------------------------------

    ! -- Default is ppmv...
    UMBC_Absorber_Units_ID(:)     = PPMV_UNITS

    ! -- ...except for H2O
    UMBC_Absorber_Units_ID(j_H2O) = MR_UNITS



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE LATITUDE --                         #
    !#--------------------------------------------------------------------------#

    READ( FileID, FMT = * ) UMBC_Latitude



    !#--------------------------------------------------------------------------#
    !#                   -- LOAD UP THE PROFILE DATA ARRAYS --                  #
    !#--------------------------------------------------------------------------#

    DO k = 1, n_Levels


      ! -------------------------------
      ! Read the profile data from file
      ! -------------------------------

      READ( FileID, FMT    = *, &
                    IOSTAT = IO_Status ) Level_Pressure( k ), &
                                         Level_Temperature( k ), &
                                         dummy, &
                                         ( Level_Absorber( k, j ), j = 1, N_UMBC_ABSORBERS )

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading profile data in ", a, ". IOSTAT = ", i5 )' ) &
                        TRIM( FileNAME ), IO_Status
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! ------------------------------------------------
      ! Convert the water vapour units from ppmv to g/kg
      ! ------------------------------------------------

      dummy = Level_Absorber( k, j_H2O )
      CALL PPMV_to_MR( dummy, &
                       Level_Absorber( k, j_H2O ) )

      IF ( Level_Absorber( k, j_H2O ) < ZERO ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error converting water vapour from ppmv->g/kg at level ", i3, "." )' ) &
                        k
        CALL display_message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO

    CLOSE( FileID )



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN THE OPTIONAL OUTPUT ARGUMENTS --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Absorber_ID ) ) THEN
      Absorber_ID = UMBC_Absorber_ID
    END IF

    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      Absorber_Units_ID = UMBC_Absorber_Units_ID
    END IF

    IF ( PRESENT( Description ) ) THEN
      Description = TRIM( UMBC_Description )
    END IF

    IF ( PRESENT( Climatology_Model ) ) THEN
      Climatology_Model = UMBC_CLIMATOLOGY_MODEL( Profile )
    END IF

    IF ( PRESENT( Year ) ) THEN
      Year = UMBC_Year
    END IF

    IF ( PRESENT( Month ) ) THEN
      Month = UMBC_Month
    END IF

    IF ( PRESENT( Day ) ) THEN
      Day = UMBC_Day
    END IF

    IF ( PRESENT( Hour ) ) THEN
      Hour = UMBC_Hour
    END IF

    IF ( PRESENT( Latitude ) ) THEN
      Latitude = UMBC_Latitude
    END IF

    IF ( PRESENT( Longitude ) ) THEN
      Longitude = UMBC_Longitude
    END IF

    IF ( PRESENT( Surface_Altitude ) ) THEN
      Surface_Altitude = UMBC_Surface_Altitude
    END IF

  END FUNCTION Load_UMBC_Profile

END MODULE UMBC_Profile_Set


