!------------------------------------------------------------------------------
!M+
! NAME:
!       CRTM_Atmosphere_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write CRTM_Atmosphere Binary 
!       format files.
!       
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       USE CRTM_Atmosphere_Binary_IO
!
! MODULES:
!       Type_Kinds:              Module containing definitions for kinds
!                                of variable types.
!
!       File_Utility:            Module containing generic file utility routines
!
!       Message_Handler:         Module to define simple error codes and
!                                handle error conditions
!                                USEs: FILE_UTILITY module
!
!       Binary_File_Utility:     Module for utility routines for "Binary"
!                                datafiles (unformatted, sequential).
!                                USEs: TYPE_KINDS module
!                                      FILE_UTILITY module
!                                      ERROR_HANDLER module
!
!       CRTM_Atmosphere_Define:  Module defining the CRTM_Atmosphere data
!                                structure and containing routines to
!                                manipulate it.
!                                USEs: TYPE_KINDS module
!                                      ERROR_HANDLER module
!
! CONTAINS:
!       Write_CRTM_Atmosphere_Binary:   Function to write CRTM_Atmosphere
!                                       structures to a Binary CRTM_Atmosphere
!                                       file.
!
!       Read_CRTM_Atmosphere_Binary:    Function to read CRTM_Atmosphere
!                                       structures from a Binary
!                                       CRTM_Atmosphere file.
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
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2004 Paul van Delst
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

MODULE CRTM_Atmosphere_Binary_IO_old


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE CRTM_Atmosphere_Define_old


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: CRTM_Write_Atmosphere_Binary
  PUBLIC :: CRTM_Read_Atmosphere_Binary


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE CRTM_Write_Atmosphere_Binary
    MODULE PROCEDURE Write_Atmosphere_Scalar
    MODULE PROCEDURE Write_Atmosphere_Rank1
  END INTERFACE ! CRTM_Write_Atmosphere_Binary

  INTERFACE CRTM_Read_Atmosphere_Binary
    MODULE PROCEDURE Read_Atmosphere_Scalar
    MODULE PROCEDURE Read_Atmosphere_Rank1
  END INTERFACE ! CRTM_Read_Atmosphere_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CRTM_Atmosphere_Binary_IO.f90,v 1.2 2004/07/02 18:57:01 paulv Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1


CONTAINS




!##################################################################################
!##################################################################################
!##                                                                              ##
!##                          ## PRIVATE MODULE ROUTINES ##                       ##
!##                                                                              ##
!##################################################################################
!##################################################################################

  FUNCTION Read_Atmosphere_Record( FileID,       &  ! Input
                                   Atmosphere,   &  ! Output
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )  :: FileID

    ! -- Outut
    TYPE( CRTM_Atmosphere_type ),  INTENT( OUT ) :: Atmosphere

    ! -- Error handler Message log
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_CRTM_Atmosphere_Binary(Record)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: n_Layers, n_Absorbers, n_Clouds
    INTEGER :: n
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE RECORD HEADER --                      #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Read the data dimensions
    ! ------------------------

    READ( FileID, IOSTAT = IO_Status ) n_Layers, &
                                       n_Absorbers, &
                                       n_Clouds

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Atmosphere data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------------------
    ! Allocate the Atmosphere structure
    ! ---------------------------------

    Error_Status = CRTM_Allocate_Atmosphere( n_Layers, &
                                             n_Absorbers, &
                                             n_Clouds, &
                                             Atmosphere, &
                                             Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error allocating Atmosphere data structure.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------------------------------
    ! Read the climatology model flag and absorber IDs
    ! ------------------------------------------------

    READ( FileID, IOSTAT = IO_Status ) Atmosphere%Climatology, &
                                       Atmosphere%Absorber_ID, &
                                       Atmosphere%Absorber_Units

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading Atmosphere climatology and absorber IDs. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- READ THE ATMOSPHERIC PROFILE DATA --                 #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Pressure, &
                                       Atmosphere%Pressure, &
                                       Atmosphere%Temperature, &
                                       Atmosphere%Absorber
                                         

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading atmospheric profile data. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- READ THE CLOUD DATA --                       #
    !#--------------------------------------------------------------------------#

    DO n = 1, Atmosphere%n_Clouds


      ! ------------------------
      ! Read the data dimensions
      ! ------------------------

      READ( FileID, IOSTAT = IO_Status ) n_Layers

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading Atmosphere Cloud #", i5, " data dimensions. IOSTAT = ", i5 )' ) &
                        n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! ----------------------------
      ! Allocate the Cloud structure
      ! ----------------------------

      Error_Status = CRTM_Allocate_Cloud( n_Layers, &
                                          Atmosphere%Cloud(n), &
                                          Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error allocating Atmosphere Cloud #", i5, "." )' ) n
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF


      ! -------------------------------
      ! Read the rest of the cloud data
      ! -------------------------------

      READ( FileID, IOSTAT = IO_Status ) Atmosphere%Cloud(n)%Type, &
                                         Atmosphere%Cloud(n)%Effective_Radius, &
                                         Atmosphere%Cloud(n)%Effective_Variance, &
                                         Atmosphere%Cloud(n)%Water_Content

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error reading Atmosphere Cloud #", i5, " data. IOSTAT = ", i5 )' ) &
                        n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID )
        RETURN
      END IF

    END DO

  END FUNCTION Read_Atmosphere_Record


  FUNCTION Write_Atmosphere_Record( FileID,       &  ! Input
                                    Atmosphere,   &  ! Input
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                       INTENT( IN )  :: FileID
    TYPE( CRTM_Atmosphere_type ),  INTENT( IN )  :: Atmosphere

    ! -- Error handler Message log
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_CRTM_Atmosphere_Binary(Record)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: n
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                      -- WRITE THE RECORD HEADER --                       #
    !#--------------------------------------------------------------------------#

    ! -------------------------
    ! Write the data dimensions
    ! -------------------------

    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%n_Layers, &
                                        Atmosphere%n_Absorbers, &
                                        Atmosphere%n_Clouds

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Atmosphere data dimensions. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! -------------------------------------------------
    ! Write the climatology model flag and absorber IDs
    ! -------------------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Climatology, &
                                        Atmosphere%Absorber_ID, &
                                        Atmosphere%Absorber_Units

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing Atmosphere climatology and absorber IDs. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                 -- WRITE THE ATMOSPHERIC PROFILE DATA --                 #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Level_Pressure, &
                                        Atmosphere%Pressure, &
                                        Atmosphere%Temperature, &
                                        Atmosphere%Absorber
                                         

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing atmospheric profile data. IOSTAT = ", i5 )' ) &
                      IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                         -- WRITE THE CLOUD DATA --                       #
    !#--------------------------------------------------------------------------#

    DO n = 1, Atmosphere%n_Clouds


      ! -------------------------
      ! Write the data dimensions
      ! -------------------------

      WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Cloud(n)%n_Layers

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing Atmosphere Cloud #", i5, " data dimensions. IOSTAT = ", i5 )' ) &
                        n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
        RETURN
      END IF


      ! --------------------------------
      ! Write the rest of the cloud data
      ! --------------------------------

      WRITE( FileID, IOSTAT = IO_Status ) Atmosphere%Cloud(n)%Type, &
                                          Atmosphere%Cloud(n)%Effective_Radius, &
                                          Atmosphere%Cloud(n)%Effective_Variance, &
                                          Atmosphere%Cloud(n)%Water_Content

      IF ( IO_Status /= 0 ) THEN
        Error_Status = FAILURE
        WRITE( Message, '( "Error writing Atmosphere Cloud #", i5, " data. IOSTAT = ", i5 )' ) &
                        n, IO_Status
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( Message ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
        RETURN
      END IF

    END DO

  END FUNCTION Write_Atmosphere_Record





!################################################################################
!################################################################################
!##                                                                            ##
!##                         ## PUBLIC MODULE ROUTINES ##                       ##
!##                                                                            ##
!################################################################################
!################################################################################

!------------------------------------------------------------------------------
!S+
! NAME:
!       CRTM_Read_Atmosphere_Binary
!
! PURPOSE:
!       Function to read Binary format CRTM Atmosphere structure files.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = CRTM_Read_Atmosphere_Binary( Filename,                 &  ! Input
!                                                   Atmosphere,               &  ! output
!                                                   n_Profiles  = n_Profiles, &  ! Optional output
!                                                   RCS_Id      = RCS_Id,     &  ! Revision control
!                                                   Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an
!                     Atmosphere format data file to read.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Atmosphere:   Structure containing the Atmosphere data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT( OUT )
!
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_Profiles:   The actual number of profiles read in.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file read was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       CRTM_Init_Atmosphere:      Subroutine to initialize a CRTM_Atmosphere
!                                  structure.
!                                  SOURCE: CRTM_ATMOSPHERE_DEFINE module
!
!       CRTM_Destroy_Atmosphere:   Function to re-initialize a CRTM_Atmosphere
!                                  structure.
!                                  SOURCE: CRTM_ATMOSPHERE_DEFINE module
!
!       Open_Binary_File:          Function to open Binary format
!                                  data files.
!                                  SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:           Subroutine to output messages
!                                  SOURCE: ERROR_HANDLER module
!
!       File_Exists:               Function to test for the existance
!                                  of files.
!                                  SOURCE: FILE_UTILITY module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       This function checks the association status of the Atmosphere structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Atmosphere structure has been filled with data.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Atmosphere structure pointer
!       members will be undefined until they are initialised (via the 
!       Initialize_Atmosphere() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_Atmosphere_Scalar( Filename,     &  ! Input
                                   Atmosphere,   &  ! Output
                                   n_Profiles,   &  ! Optional output
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                INTENT( IN )  :: Filename

    ! -- Output
    TYPE( CRTM_Atmosphere_type ),  INTENT( OUT ) :: Atmosphere

    ! -- Optional output
    INTEGER,             OPTIONAL, INTENT( OUT ) :: n_Profiles

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Scalar)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: n_Input_Profiles, n_Profiles_Read

    TYPE( CRTM_Atmosphere_type ) :: Dummy_Atmosphere
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE ATMOSPHERE DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE NUMBER OF PROFILES --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Profiles

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Profiles data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ---------------------------------------
    ! Issue warning message if n_Profiles > 1
    ! ---------------------------------------

    IF ( n_Input_Profiles > 1 ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Number of profiles > 1 and output Atmosphere structure '//&
                            'is scalar. Only the first Atmosphere structure will be read.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                        -- READ THE STRUCTURE DATA --                     #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Initialize profiles read
    ! ------------------------

    n_Profiles_Read = 0


    ! ----------------------------------------------
    ! Read the structure data into a dummy structure
    ! ----------------------------------------------

    Error_Status = Read_Atmosphere_Record( FileID, &
                                           Dummy_Atmosphere, &
                                           Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading Atmosphere record from '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------
    ! Copy dummy structure to output
    ! only if it contains valid data
    ! ------------------------------

    IF ( Dummy_Atmosphere%n_Layers    > 1 .OR. &
         Dummy_Atmosphere%n_Absorbers > 1      ) THEN

      ! -- Copy the data into the output array
      Error_Status = CRTM_Assign_Atmosphere( Dummy_Atmosphere, &
                                             Atmosphere, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        CALL Display_Message( ROUTINE_NAME, &
                              'Error copying Atmosphere structure.', &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Set value for the number of profiles read
      n_Profiles_Read = 1

    END IF


    ! ---------------------------
    ! Destroy the dummy structure
    ! ---------------------------

    Error_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error destroying dummy Atmosphere structure.', &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                  -- SAVE THE NUMBER OF PROFILES READ --                  #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( n_Profiles ) ) THEN
      n_Profiles = n_Profiles_Read
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

!    CLOSE( FileID, IOSTAT = IO_Status )
!
!    IF ( IO_Status /= 0 ) THEN
!      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
!                      TRIM( Filename ), IO_Status
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            WARNING, &
!                            Message_Log = Message_Log )
!    END IF

  END FUNCTION Read_Atmosphere_Scalar


  FUNCTION Read_Atmosphere_Rank1( Filename,     &  ! Input
                                  Atmosphere,   &  ! Output
                                  n_Profiles,   &  ! Optional output
                                  RCS_Id,       &  ! Revision control
                                  Message_Log ) &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                               INTENT( IN )  :: Filename

    ! -- Output
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( OUT ) :: Atmosphere

    ! -- Optional output
    INTEGER,                      OPTIONAL,       INTENT( OUT ) :: n_Profiles

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Read_Atmosphere_Binary(Rank-1)'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Input_Profiles, n_Profiles_Read

    TYPE( CRTM_Atmosphere_type ) :: Dummy_Atmosphere
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! --------------------------
    ! Check that the file exists
    ! --------------------------

    IF ( .NOT. File_Exists( TRIM( Filename ) ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'File '//TRIM( Filename )//' not found.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- INITIALIZE THE LOCAL Atmosphere STRUCTURE --             #
    !#--------------------------------------------------------------------------#

    CALL CRTM_Init_Atmosphere( Dummy_Atmosphere )



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE ATMOSPHERE DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE NUMBER OF PROFILES --                   #
    !#--------------------------------------------------------------------------#

    READ( FileID, IOSTAT = IO_Status ) n_Input_Profiles

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_Profiles data dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ----------------------------------------------------------
    ! Issue warning message if n_Profiles > size of output array
    ! ----------------------------------------------------------

    IF ( n_Input_Profiles > SIZE( Atmosphere ) ) THEN
      WRITE( Message, '( "Number of profiles, ", i5, " > size of the output Atmosphere ", &
                        &"structure array, ", i5, ". Only the first ", i5, &
                        &" Atmosphere structures will be read." )' ) &
                      n_Input_Profiles, SIZE( Atmosphere ), SIZE( Atmosphere )
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
      n_Input_Profiles = SIZE( Atmosphere )
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- LOOP OVER PROFILES --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------
    ! Initialize profiles read
    ! ------------------------

    n_Profiles_Read = 0


    ! --------------------------------------------------------
    ! Loop over all the profiles (even potentially empty ones)
    ! --------------------------------------------------------

    Profile_Loop: DO m = 1, n_Input_Profiles


      ! ----------------------------------------------
      ! Read the structure data into a dummy structure
      ! ----------------------------------------------

      Error_Status = Read_Atmosphere_Record( FileID, &
                                             Dummy_Atmosphere, &
                                             Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error reading Atmosphere element #", i5, " from ", a )' ) &
                        m, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( MEssage ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF


      ! ------------------------------------
      ! Copy dummy structure to output array
      ! only if it contains valid data.
      ! ------------------------------------

      IF ( Dummy_Atmosphere%n_Layers    > 1 .OR. &
           Dummy_Atmosphere%n_Absorbers > 1      ) THEN

        ! -- Increment profiles read
        n_Profiles_Read = n_Profiles_Read + 1

        ! -- Copy the data into the output array
        Error_Status = CRTM_Assign_Atmosphere( Dummy_Atmosphere, &
                                               Atmosphere( n_Profiles_Read ), &
                                               Message_Log = Message_Log )

        IF ( Error_Status /= SUCCESS ) THEN
          WRITE( Message, '( "Error copying Atmosphere element #", i5, "." )' ) m
          CALL Display_Message( ROUTINE_NAME, &
                                TRIM( MEssage ), &
                                Error_Status, &
                                Message_Log = Message_Log )
          RETURN
        END IF

      END IF


      ! ---------------------------
      ! Destroy the dummy structure
      ! ---------------------------

      Error_Status = CRTM_Destroy_Atmosphere( Dummy_Atmosphere )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error destroying dummy Atmosphere structure at profile #", i5, "." )' ) m
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( MEssage ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END DO Profile_Loop



    !#--------------------------------------------------------------------------#
    !#                  -- SAVE THE NUMBER OF PROFILES READ --                  #
    !#--------------------------------------------------------------------------#

    ! ----------------------
    ! Output an info message
    ! ----------------------

    WRITE( Message, '( "Number of profiles read from ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Profiles_Read
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )


    ! ---------------------------------------
    ! Assign a value to the optional argument
    ! ---------------------------------------

    IF ( PRESENT( n_Profiles ) ) THEN
      n_Profiles = n_Profiles_Read
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

!    CLOSE( FileID, STATUS = 'KEEP',   &
!                   IOSTAT = IO_Status )
!
!    IF ( IO_Status /= 0 ) THEN
!      Error_Status = WARNING
!      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
!                      TRIM( Filename ), IO_Status
!      CALL Display_Message( ROUTINE_NAME, &
!                            TRIM( Message ), &
!                            Error_Status, &
!                            Message_Log = Message_Log )
!    END IF

  END FUNCTION Read_Atmosphere_Rank1





!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_Atmosphere_Binary
!
! PURPOSE:
!       Function to write Binary format Atmosphere files.
!
! CATEGORY:
!       CRTM : Atmosphere : I/O
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       Error_Status = Write_Atmosphere_Binary( Filename,                 &  ! Input
!                                               Atmosphere,               &  ! Input
!                                               RCS_Id      = RCS_Id,     &  ! Revision control
!                                               Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     Atmosphere format data file.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
!       Atmosphere:   Structure containing the Atmosphere data.
!                     UNITS:      N/A
!                     TYPE:       CRTM_Atmosphere_type
!                     DIMENSION:  Scalar or Rank-1
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      None
!                     TYPE:       CHARACTER( * )
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE an unrecoverable error occurred.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:        Function to open Binary format
!                                data files.
!                                SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:         Subroutine to output messages
!                                SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       This function checks the association status of the Atmosphere structure
!       pointer members. Therefore, this function should *only* be called
!       *after* the Atmosphere structure has been filled with data.
!
!       This restriction is due to Fortran-90 not providing any mechanism
!       for initialising pointer association status in derived type definitions.
!       This means the association status of the Atmosphere structure pointer
!       members will be undefined until they are initialised (via the 
!       Initialize_Atmosphere() subroutine).
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 01-Jul-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_Atmosphere_Scalar( Filename,     &  ! Input
                                    Atmosphere,   &  ! Input
                                    RCS_Id,       &  ! Revision control
                                    Message_Log ) &  ! Error messaging
                                  RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                INTENT( IN )  :: Filename
    TYPE( CRTM_Atmosphere_type ),  INTENT( IN )  :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),      OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),      OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(Scalar)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: l
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                            -- CHECK INPUT --                             #
    !#--------------------------------------------------------------------------#

    ! -----------------------------------------
    ! Check the Atmosphere structure dimensions
    ! -----------------------------------------

    IF ( Atmosphere%n_Layers    < 1 .OR. &
         Atmosphere%n_Absorbers < 1      ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'n_Layers or n_Absorbers dimension of Atmosphere structure are < or = 0.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE ATMOSPHERE DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF PROFILES --                   #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) 1

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing n_Profiles data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    !#--------------------------------------------------------------------------#
    !#                        -- WRITE THE STRUCTURE DATA --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = Write_Atmosphere_Record( FileID, &
                                            Atmosphere, &
                                            Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing Atmosphere record to '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Atmosphere_Scalar


  FUNCTION Write_Atmosphere_Rank1( Filename,     &  ! Input
                                   Atmosphere,   &  ! Input
                                   RCS_Id,       &  ! Revision control
                                   Message_Log ) &  ! Error messaging
                                 RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),                               INTENT( IN )  :: Filename
    TYPE( CRTM_Atmosphere_type ), DIMENSION( : ), INTENT( IN )  :: Atmosphere

    ! -- Revision control
    CHARACTER( * ),               OPTIONAL,       INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),               OPTIONAL,       INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'CRTM_Write_Atmosphere_Binary(Rank-1)'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: m, n_Output_Profiles
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- SET THE RCS ID ARGUMENT IF SUPPLIED --                #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- CHECK THE INPUT ATMOSPHERE --                    #
    !#--------------------------------------------------------------------------#

    n_Output_Profiles = COUNT( Atmosphere%n_Layers    > 0 .AND. &
                               Atmosphere%n_Absorbers > 0       )

    IF ( n_Output_Profiles < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME,    &
                            'No profiles with non-zero dimensions!', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- OPEN THE ATMOSPHERE DATA FILE --                  #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     For_Output  = SET, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                     -- WRITE THE NUMBER OF PROFILES --                   #
    !#--------------------------------------------------------------------------#

    WRITE( FileID, IOSTAT = IO_Status ) n_Output_Profiles

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing n_Profiles data dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- LOOP OVER PROFILES --                        #
    !#--------------------------------------------------------------------------#

    ! --------------------------------------------------------
    ! Loop over all the profiles (even potentially empty ones)
    ! --------------------------------------------------------

    Profile_Loop: DO m = 1, SIZE( Atmosphere )


      ! -----------------------------------------
      ! Check the Atmosphere structure dimensions
      ! -----------------------------------------

      IF ( Atmosphere(m)%n_Layers    < 1 .OR. &
           Atmosphere(m)%n_Absorbers < 1      ) CYCLE Profile_Loop


      ! ------------------------
      ! Write the structure data
      ! ------------------------

      Error_Status = Write_Atmosphere_Record( FileID, &
                                              Atmosphere(m), &
                                              Message_Log = Message_Log )

      IF ( Error_Status /= SUCCESS ) THEN
      WRITE( Message, '( "Error writing Atmosphere element #", i5, " to ", a )' ) &
                      m, TRIM( Filename )
        CALL Display_Message( ROUTINE_NAME, &
                              TRIM( MEssage ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

    END DO Profile_Loop



    !#--------------------------------------------------------------------------#
    !#                       -- OUTPUT AND INFO MESSAGE --                      #
    !#--------------------------------------------------------------------------#

    WRITE( Message, '( "Number of profiles written to ", a, ": ", i5 )' ) &
                    TRIM( Filename ), n_Output_Profiles
    CALL Display_Message( ROUTINE_NAME, &
                          TRIM( MEssage ), &
                          INFORMATION, &
                          Message_Log = Message_Log )



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP',   &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_Atmosphere_Rank1

END MODULE CRTM_Atmosphere_Binary_IO_old


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CRTM_Atmosphere_Binary_IO.f90,v 1.2 2004/07/02 18:57:01 paulv Exp $
!
! $Date: 2004/07/02 18:57:01 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CRTM_Atmosphere_Binary_IO.f90,v $
! Revision 1.2  2004/07/02 18:57:01  paulv
! - Tested version. Corrected various bugs and errors.
!
! Revision 1.1  2004/07/01 20:45:43  paulv
! Initial checkin. Incomplete.
!
!
!
!
