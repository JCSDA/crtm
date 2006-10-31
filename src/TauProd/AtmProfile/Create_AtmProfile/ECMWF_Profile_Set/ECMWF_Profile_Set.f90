!------------------------------------------------------------------------------
!M+
! NAME:
!       ECMWF_Profile_Set
!
! PURPOSE:
!       Module containing the ECMWF atmospheric profile set data 
!       definitions and access routines
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE ECMWF_Profile_Set
!
! MODULES:
!       Type_Kinds:         Module containing definitions for kinds
!                           of variable types.
!
!       File_Utility:       Module containing generic file utility
!                           routines
!
!       Message_Handler:      Module to define simple error codes and
!                           handle error conditions
!                           USEs: FILE_UTILITY module
!
!       Units_Conversion:   Module containing routines to convert
!                           atmospheric profile concentration units.
!                           USEs: TYPE_KINDS module
!                                 ERROR_HANDLER module
!                                 PROFILE_UTILITY_PARAMETERS module
!                                 ATMOSPHERIC_PROPERTIES module
!
! CONTAINS:
!       Load_ECMWF_Profile:  Function to load a requested atmospheric profile
!                            from the ECMWF set.
!
! INCLUDE FILES:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! FILES ACCESSED:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 29-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2002 Paul van Delst
!
!  This program is free software; you can redistribute it and/or
!  modify it under the terms of the GNU General Public License
!  as published by the Free Software Foundation; either version 2
!  of the License, or (at your option) any later version.
!
!  This program is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.
!
!  You should have received a copy of the GNU General Public License
!  along with this program; if not, write to the Free Software
!  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
!M-
!------------------------------------------------------------------------------

MODULE ECMWF_Profile_Set


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler

  USE Units_Conversion


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Load_ECMWF_Profile


  ! ----------
  ! Parameters
  ! ----------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
  '$Id: ECMWF_Profile_Set.f90,v 2.9 2006/06/30 16:47:16 dgroff Exp $'

  ! -- The ECMWF data file name
  CHARACTER( * ), PRIVATE, PARAMETER :: ECMWF_DATA_FILE = './ECMWF_Profile_Set/diverse_52profiles_101L.dat'

  ! -- The number of levels, layers, absorbers, and profiles
  INTEGER, PUBLIC, PARAMETER :: N_ECMWF_LEVELS    = 101
  INTEGER, PUBLIC, PARAMETER :: N_ECMWF_LAYERS    = N_ECMWF_LEVELS - 1
  INTEGER, PUBLIC, PARAMETER :: N_ECMWF_ABSORBERS = 2
  INTEGER, PUBLIC, PARAMETER :: N_ECMWF_PROFILES  = 52

  ! -- The climatology model.
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ECMWF_PROFILES ) :: ECMWF_CLIMATOLOGY_MODEL = &
    (/  1, 6, 5, 3, 4, 1, 2, 3, 5, 5, &
        4, 5, 5, 5, 5, 1, 3, 6, 4, 5, &
        1, 5, 4, 5, 1, 1, 5, 5, 6, 6, &
        3, 5, 1, 5, 6, 1, 4, 5, 2, 1, &
        5, 6, 4, 6, 1, 5, 4, 5, 5, 6, &
        6, 6 /)


  ! -- Absorber info
  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ECMWF_ABSORBERS ) :: &
    ECMWF_ABSORBER_ID = (/ 1, &  ! H2O
                           3 /)  ! O3

  INTEGER, PRIVATE, PARAMETER, DIMENSION( N_ECMWF_ABSORBERS ) :: &
    ECMWF_ABSORBER_UNITS_ID = (/ 3, &  ! g/kg
                                 1 /)  ! ppmv


CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       Load_ECMWF_Profile
!
! PURPOSE:
!       Function to return the requested profile from the ECMWF set of
!       atmospheric profiles.
!
! CATEGORY:
!       AtmProfile
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Load_ECMWF_Profile( Profile,                               &  ! Input
!                                          Level_Pressure,                        &  ! Output
!                                          Level_Temperature,                     &  ! Output
!                                          Level_Absorber,                        &  ! Output
!                                          Absorber_ID       = Absorber_ID,       &  ! Optional output
!                                          Absorber_Units_ID = Absorber_Units_ID, &  ! Optional output
!                                          Description       = Description,       &  ! Optional output
!                                          Climatology_Model = Climatology_Model, &  ! Optional output
!                                          Year              = Year,              &  ! Optional output
!                                          Month             = Month,             &  ! Optional output
!                                          Day               = Day,               &  ! Optional output
!                                          Hour              = Hour,              &  ! Optional output
!                                          Latitude          = Latitude,          &  ! Optional output
!                                          Longitude         = Longitude,         &  ! Optional output
!                                          Surface_Altitude  = Surface_Altitude,  &  ! Optional output
!                                          RCS_Id            = RCS_Id,            &  ! Revision control
!                                          Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Profile:            The requested atmospheric profile number from the
!                           ECMWF set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Level_Pressure:     Pressure profile for the requested
!                           atmospheric profile.
!                           UNITS:      hectoPascals, hPa
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Temperature:  Temperature profile for the requested
!                           atmospheric profile.
!                           UNITS:      Kelvin, K
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-1, n_levels
!                           ATTRIBUTES: POINTER
!
!       Level_Absorber:     Absorber profiles for the requested atmospheric
!                           profile.
!                           UNITS:      Variable. See ABSORBER_UNITS_ID argument
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Rank-2, n_levels x n_absorbers
!                           ATTRIBUTES: POINTER
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Absorber_ID:        The list of the HITRAN absorber numbers for the 
!                           molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Absorber_Units_ID:  The list of the absorber units ID numbers for
!                           the molecular absorbers in the profile set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Rank-1, n_absorbers
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Description:        Description of the requested profile.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Climatology_Model:  Climatology model for the requested profile.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Year:               Year in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Month:              Month in which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Day:                Day on which the requested profile sonde was launched.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Hour:               Hour in which the requested profile sonde was launched.
!                           UNITS:      UTC
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Latitude:           Latitude for the requested profile.
!                           UNITS:      Degrees North (-90 -> +90).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Longitude:          Longitude for the requested profile.
!                           UNITS:      Degrees East (0 -> 360).
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       Surface_Altitude:   Surface altitude for the requested profile.
!                           UNITS:      Metres, m.
!                           TYPE:       REAL( fp_kind )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER( * )
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
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
! CALLS:
!       Get_Lun:            Function to return a free logical unit number for
!                           file access.
!                           SOURCE: FILE_UTILITY module
!
!       SA_to_MR:           Function to convert gas concentrations from
!                           specific amount to mixing ratio.
!                           SOURCE: UNITS_CONVERSION module
!      
!       MR_to_PPMV:         Function to convert gas concentrations from
!                           mixing ratio to parts-per-million by volume.
!                           SOURCE: UNITS_CONVERSION module
!      
!       Display_Message:    Subroutine to output messages
!                           SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 12-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Load_ECMWF_Profile( Profile,           &  ! Input
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

    ! -- Revision control
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

    CHARACTER( * ),  PARAMETER :: ROUTINE_NAME = 'Load_ECMWF_Profile'
    INTEGER,         PARAMETER :: INVALID = -1
    REAL( fp_kind ), PARAMETER :: ZERO = 0.0_fp_kind
    REAL( fp_kind ), PARAMETER :: ONE  = 1.0_fp_kind
    REAL( fp_kind ), PARAMETER :: THOUSAND = 1000.0_fp_kind


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 256 ) :: Message

    INTEGER :: Allocate_Status
    INTEGER :: IO_Status
    INTEGER :: FileID

    INTEGER :: m

    CHARACTER( 80 ) :: Line_Buffer
    REAL( fp_kind ), DIMENSION( 4, N_ECMWF_LEVELS ) :: Data_Array

    REAL( fp_kind ), DIMENSION( N_ECMWF_LEVELS ) :: ECMWF_Specific_H2O
    REAL( fp_kind ), DIMENSION( N_ECMWF_LEVELS ) :: ECMWF_Specific_O3
    REAL( fp_kind ), DIMENSION( N_ECMWF_LEVELS ) :: ECMWF_Mixing_Ratio_O3

    REAL( fp_kind )  :: ECMWF_Latitude
    REAL( fp_kind )  :: ECMWF_Longitude
    REAL( fp_kind )  :: ECMWF_Surface_Pressure
    REAL( fp_kind )  :: ECMWF_Surface_Altitude
    INTEGER          :: ECMWF_Year
    INTEGER          :: ECMWF_DateTime
    INTEGER          :: ECMWF_Month
    INTEGER          :: ECMWF_Day
    INTEGER          :: ECMWF_Hour



    !#--------------------------------------------------------------------------#
    !#                   -- SET A SUCCESSFUL ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- SET THE RCS ID ARGUMENT IF SUPPLIED --                 #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Is requested profile valid?
    ! ---------------------------

    IF ( Profile < 1 .OR. Profile > N_ECMWF_PROFILES ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Invalid model profile number ", i5, " specified." )' ) &
                      profile
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
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
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF

    ! -- Temperature
    IF ( ASSOCIATED( Level_Temperature ) ) THEN
      DEALLOCATE( Level_Temperature, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Temperature output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF

    ! -- Absorber
    IF ( ASSOCIATED( Level_Absorber ) ) THEN
      DEALLOCATE( Level_Absorber, STAT = Allocate_Status )
      IF ( Allocate_Status /= 0 ) THEN
        WRITE( Message, '( "Error deallocating Level_Absorber output array. STAT = ", i5 )' ) &
                        Allocate_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF


    ! ------------------------
    ! Check output array sizes
    ! ------------------------

    ! -- Absorber ID array
    IF ( PRESENT( Absorber_ID ) ) THEN
      IF ( SIZE( Absorber_ID ) /= N_ECMWF_ABSORBERS ) THEN
        error_status = FAILURE
        WRITE( Message, '( "Size of output Absorber_ID array must be ", i1, " elements." )' ) &
                        N_ECMWF_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF


    ! -- Absorber Units ID array
    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      IF ( SIZE( Absorber_Units_ID ) /= N_ECMWF_ABSORBERS ) THEN
        error_status = FAILURE
        WRITE( Message, '( "Size of output Absorber_Units_ID array must be ", i1, " elements." )' ) &
                        N_ECMWF_ABSORBERS
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              error_status, &
                              message_log = message_log )
        RETURN
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- ALLOCATE OUTPUT POINTER ARRAYS --                   #
    !#--------------------------------------------------------------------------#

    ALLOCATE( Level_Pressure( N_ECMWF_LEVELS ), &
              Level_Temperature( N_ECMWF_LEVELS ), &
              Level_Absorber( N_ECMWF_LEVELS, N_ECMWF_ABSORBERS ), &
              STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN
      error_status = FAILURE
      WRITE( Message, '( "Error allocating output arrays. STATs = ", 4(1x,i5) )' ) &
                      Allocate_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            error_status, &
                            message_log = message_log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                       -- OPEN THE DATA FILE --                           #
    !#--------------------------------------------------------------------------#

    ! ---------------------------
    ! Get a free file unit number
    ! ---------------------------

    FileID = Get_Lun()

    IF ( FileID < 0 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error obtaining file unit number.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    ! --------------------------
    ! Open the profile data file
    ! --------------------------

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
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#               -- LOOP OVER COMMENT LINES IN DATA FILE --                 #
    !#--------------------------------------------------------------------------#

    Comment_Read_loop: DO

      ! -- Read a line of the file
      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Line_Buffer

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading ", a, " file in comment skip. IOSTAT = ", i5 )' ) &
                        ECMWF_DATA_FILE, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

      ! -- Exit loop if this is NOT a comment line
      IF ( Line_Buffer(1:1) /= '!' ) THEN
        BACKSPACE( FileID )
        EXIT Comment_Read_loop
      END IF

    END DO Comment_Read_loop



    !#--------------------------------------------------------------------------#
    !#                       -- READ THROUGH DATA FILE --                       #
    !#--------------------------------------------------------------------------#

    m_Profile_Read_Loop: DO m = 1, Profile


      ! ------------------------
      ! Read profile header line
      ! ------------------------

      Line_Buffer = ' '
      READ( FileID, FMT    = '( a )',  &
                    IOSTAT = IO_Status ) Line_Buffer

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading header line for profile #", i3, ". IOSTAT = ", i5 )' ) &
                        m, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! -----------------
      ! Read profile data
      ! -----------------

      READ( FileID, FMT    = *,  &
                    IOSTAT = IO_Status ) Data_Array

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading profile data for profile #", i3, ". IOSTAT = ", i5 )' ) &
                        m, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO m_Profile_Read_Loop


    ! -------------------------
    ! Close the input data file
    ! -------------------------

    CLOSE( FileID )



    !#--------------------------------------------------------------------------#
    !#                   -- EXTRACT DATA FROM FINAL READ --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------------------------------------
    ! Data from the header line. Note that the land-fraction is not read
    ! ------------------------------------------------------------------

    ! -- Read the data from the header line buffer
    READ( Line_Buffer, '( 1x, i2, 1x, i4, i6, 3(3x,e13.6) )' ) m, &
                                                               ECMWF_Year, &
                                                               ECMWF_DateTime, &
                                                               ECMWF_Latitude, &
                                                               ECMWF_Longitude, &
                                                               ECMWF_Surface_Pressure

    ! -- Parse the Date/Time to handle cases where it's undefined
    IF ( ECMWF_DateTime > 0 ) THEN
      ECMWF_Month = ECMWF_DateTime/10000
      ECMWF_Day   = MOD(ECMWF_DateTime,10000)/100
      ECMWF_Hour  = MOD(ECMWF_DateTime,100)
    ELSE
      ECMWF_Year  = -1
      ECMWF_Month = -1
      ECMWF_Day   = -1
      ECMWF_Hour  = -1
    END IF

    ! -- Default surface altitude
    ECMWF_Surface_Altitude  = ZERO

    ! -- Check that the profile number is in the correct sequence
    IF ( Profile /= m ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Profile # from header line, ", i2, &
                        &", is different from that requested, ", i2, "." )' ) m, Profile
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------
    ! Data from the array block
    ! -------------------------

    Level_Pressure    = Data_Array( 1, N_ECMWF_LEVELS:1:-1 )
    Level_Temperature = Data_Array( 2, N_ECMWF_LEVELS:1:-1 )

    ECMWF_Specific_H2O = THOUSAND * Data_Array( 3, N_ECMWF_LEVELS:1:-1 )  ! g/kg
    ECMWF_Specific_O3  = THOUSAND * Data_Array( 4, N_ECMWF_LEVELS:1:-1 )  ! g/kg



    !#--------------------------------------------------------------------------#
    !#                      -- CONVERT PROFILE DATA  --                         #
    !#--------------------------------------------------------------------------#
 
    ! -------------------------------------------------------------------
    ! Water vapour. Specific humidity (g/kg) -> mass mixing ratio (g/kg)
    ! -------------------------------------------------------------------

    Level_Absorber( :, 1 ) = SA_to_MR( ECMWF_Specific_H2O, &
                                       Message_Log = Message_Log )

    IF ( ANY( Level_Absorber( :, 1 ) < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error converting water vapor units.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ----------------------------------------------------------
    ! Ozone. Specific ozone (g/kg) -> volume mixing ratio (ppmv)
    ! ----------------------------------------------------------

    ! -- First convert from specific ozone to mass mixing ratio
    ECMWF_Mixing_Ratio_O3 = SA_to_MR( ECMWF_Specific_O3, &
                                      Water_Vapor = Level_Absorber( :, 1 ), &
                                      Message_Log = Message_Log )

    ! -- Now convert from mass mixing ratio to ppmv
    Level_Absorber( :, 2 ) = MR_to_PPMV( ECMWF_Mixing_Ratio_O3, &
                                         Molecule_ID = ECMWF_ABSORBER_ID(2), &
                                         Message_Log = Message_Log )

    IF ( ANY( Level_Absorber( :, 2 ) < ZERO ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error converting ozone units.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- ASSIGN THE OPTIONAL OUTPUT ARGUMENTS --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Absorber_ID ) ) THEN
      Absorber_ID = ECMWF_ABSORBER_ID
    END IF

    IF ( PRESENT( Absorber_Units_ID ) ) THEN
      Absorber_Units_ID = ECMWF_ABSORBER_UNITS_ID
    END IF

    IF ( PRESENT( Description ) ) THEN
      Description = ' '
      WRITE( Description, '( "ECMWF 52 diverse profiles from 60L set. Profile #", i3 )' ) Profile
    END IF

    IF ( PRESENT( Climatology_Model ) ) THEN
      Climatology_Model = ECMWF_CLIMATOLOGY_MODEL( Profile )
    END IF

    IF ( PRESENT( Year ) ) THEN
      Year = ECMWF_Year
    END IF

    IF ( PRESENT( Month ) ) THEN
      Month = ECMWF_Month
    END IF

    IF ( PRESENT( Day ) ) THEN
      Day = ECMWF_Day
    END IF

    IF ( PRESENT( Hour ) ) THEN
      Hour = ECMWF_Hour
    END IF

    IF ( PRESENT( Latitude ) ) THEN
      Latitude = ECMWF_Latitude
    END IF

    IF ( PRESENT( Longitude ) ) THEN
      Longitude = ECMWF_Longitude
    END IF

    IF ( PRESENT( Surface_Altitude ) ) THEN
      Surface_Altitude = ECMWF_Surface_Altitude
    END IF

  END FUNCTION Load_ECMWF_Profile

END MODULE ECMWF_Profile_Set


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: ECMWF_Profile_Set.f90,v 2.9 2006/06/30 16:47:16 dgroff Exp $
!
! $Date: 2006/06/30 16:47:16 $
!
! $Revision: 2.9 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: ECMWF_Profile_Set.f90,v $
! Revision 2.9  2006/06/30 16:47:16  dgroff
! Changed "Error_Handler" references to "Message_Handler"
!
! Revision 2.8  2005/01/04 22:04:19  paulv
! - Updated header documentation.
! - Removed unused variable declarations.
!
! Revision 2.7  2003/12/05 22:24:04  paulv
! - Now using the SA_to_MR function to convert both the specific humidity and
!   specific ozone to mixing ratio.
!
! Revision 2.6  2003/12/05 15:33:26  paulv
! - Corrected bug in conversion of specific ozone to ozone mixing ratio.
!   Previously I was doing,
!     ECMWF_O3 = ( Level_Absorber( :, 1 ) + ONE ) * ECMWF_O3
!   but this is only correct for the water vapour mixing ratio, the
!   Level_Absorber( :, 1 ), being in units of KG/KG which it is not -- the
!   units are G/KG. The above equation was corrected to,
!     ECMWF_O3 = ( ( 0.001_fp_kind * Level_Absorber( :, 1 ) ) + ONE ) * ECMWF_O3
!
! Revision 2.5  2003/12/01 16:51:01  paulv
! - Error message for invalid profile number is a bit more descriptive to
!   allow any bad data in the original profile data file to be easily found.
!
! Revision 2.4  2003/11/17 19:34:38  paulv
! - Corrected bug in reading header data. Needed to parse the date and time
!   separately for those cases where they are undefined.
!
! Revision 2.3  2003/11/17 18:00:05  paulv
! - Corrected bug in format specification for reading header data from the
!   Line_Buffer internal file.
!
! Revision 2.2  2003/09/09 14:35:46  paulv
! - Corrected bug in conversion of specific amounts in kg/kg to g/kg. The
!   multiplication factor was changed from 0.001 to 1000.0.
! - Updated documentation.
!
! Revision 2.1  2003/07/23 14:11:13  paulv
! - Updated ECMWF profile set filename.
!
! Revision 2.0  2003/07/21 19:53:35  paulv
! - New version for the 52 profile subset.
!
! Revision 1.2  2002/09/11 20:30:53  paulv
! - Added in profile data conversion code.
! - Added data for climatology model selection.
! - Added in output array pointer checks/deallocation.
!
! Revision 1.1  2002/09/10 21:38:37  paulv
! Initial checkin.
!
!
!
!
