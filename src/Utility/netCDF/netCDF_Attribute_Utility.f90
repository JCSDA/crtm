!--------------------------------------------------------------------------------
!M+
! NAME:
!       netCDF_Attribute_Utility
!
! PURPOSE:
!       Module containing some utility routines for netCDF file attribute access.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE netCDF_Attribute_Utility
!
! MODULES:
!       Type_Kinds:    Module containing data type kind definitions.
!
!       Message_Handler: Module to define error codes and handle error
!                      conditions
!                      USEs: FILE_UTILITY module
!
!       netcdf:        Module supplied with the Fortran 90 version of the
!                      netCDF libraries (at least v3.5.0).
!                      See http://www.unidata.ucar.edu/packages/netcdf
!
! CONTAINS:
!       Get_netCDF_Attribute: Function to retrieve a netCDF variable attribute
!                             by name. This function is simply a wrapper
!                             for some of the NetCDF library functions to
!                             simplify the retrieval of an attribute with
!                             error checking.
!
!       Put_netCDF_Attribute: Function to write a netCDF variable attribute
!                             by name. This function is simply a wrapper
!                             for some of the NetCDF library functions to
!                             simplify the writing of an attribute with
!                             error checking.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       For PUT operations, attribute values can be overwritten if they
!       already exist in the output file.
!
! RESTRICTIONS:
!       Interfaces are available only for scalar and rank-1 attribute data,
!       except for character attributes which only have a scalar interface.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 11-Feb-2003
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2003, 2004 Paul van Delst
!
!M-
!--------------------------------------------------------------------------------

MODULE netCDF_Attribute_Utility


  ! --------------------
  ! Declare modules used
  ! --------------------

  USE Type_Kinds
  USE Message_Handler
  USE netcdf


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  PRIVATE
  PUBLIC :: Get_netCDF_Attribute
  PUBLIC :: Put_netCDF_Attribute


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  ! -- Functions to get attributes
  INTERFACE Get_netCDF_Attribute
    MODULE PROCEDURE get_scalar_Character
    MODULE PROCEDURE get_scalar_Byte
    MODULE PROCEDURE get_scalar_Short
    MODULE PROCEDURE get_scalar_Long
    MODULE PROCEDURE get_scalar_Single
    MODULE PROCEDURE get_scalar_Double
    MODULE PROCEDURE get_rank1_Byte
    MODULE PROCEDURE get_rank1_Short
    MODULE PROCEDURE get_rank1_Long
    MODULE PROCEDURE get_rank1_Single
    MODULE PROCEDURE get_rank1_Double
  END INTERFACE Get_netCDF_Attribute

  ! -- Functions to put attributes
  INTERFACE Put_netCDF_Attribute
    MODULE PROCEDURE put_scalar_Character
    MODULE PROCEDURE put_scalar_Byte
    MODULE PROCEDURE put_scalar_Short
    MODULE PROCEDURE put_scalar_Long
    MODULE PROCEDURE put_scalar_Single
    MODULE PROCEDURE put_scalar_Double
    MODULE PROCEDURE put_rank1_Byte
    MODULE PROCEDURE put_rank1_Short
    MODULE PROCEDURE put_rank1_Long
    MODULE PROCEDURE put_rank1_Single
    MODULE PROCEDURE put_rank1_Double
  END INTERFACE Put_netCDF_Attribute


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = 'Placeholder'

CONTAINS




  FUNCTION get_scalar_Byte( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Byte ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------
    ! Check that the attribute is a scalar
    ! ------------------------------------

    IF ( n > 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is not a scalar attribute.', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Byte



  FUNCTION get_scalar_Short( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Short ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------
    ! Check that the attribute is a scalar
    ! ------------------------------------

    IF ( n > 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is not a scalar attribute.', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Short



  FUNCTION get_scalar_Long( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Long ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------
    ! Check that the attribute is a scalar
    ! ------------------------------------

    IF ( n > 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is not a scalar attribute.', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Long



  FUNCTION get_scalar_Single( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    REAL( Single ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------
    ! Check that the attribute is a scalar
    ! ------------------------------------

    IF ( n > 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is not a scalar attribute.', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Single



  FUNCTION get_scalar_Double( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    REAL( Double ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------------
    ! Check that the attribute is a scalar
    ! ------------------------------------

    IF ( n > 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is not a scalar attribute.', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_Double



  FUNCTION get_scalar_CHARACTER( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    CHARACTER( * ), INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(scalar CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n_Attribute
    INTEGER :: n_Argument
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -------------------------------
    ! Get the length of the attribute
    ! -------------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n_Attribute )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! ------------------------------
    ! Get the length of the argument
    ! ------------------------------

    n_Argument = LEN( Attribute_Value )


    ! -------------------------------------------------
    ! Check that the attribute argument is large enough
    ! -------------------------------------------------

    IF ( n_Attribute > n_Argument ) THEN
      Error_Status = WARNING
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' string is longer than argument. Truncating.', &
                            Error_Status, &
                            Message_Log = Message_Log )
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    ! ------------------------------
    ! Initialise the output argument
    ! ------------------------------

    Attribute_Value = ' '


    ! ---------------------------------------------
    ! Determine the length of attribute to retrieve
    ! ---------------------------------------------

    n = MIN( n_Attribute, n_Argument )


    ! ------------------
    ! Read the attribute
    ! ------------------

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value(1:n) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_scalar_CHARACTER


  FUNCTION get_rank1_Byte( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Byte ), DIMENSION( : ), &
                              INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(rank1 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------
    ! Check that the dummy output argument is big enough
    ! --------------------------------------------------

    IF ( SIZE( Attribute_Value ) < n ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is too large to fit in return argument', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value( 1:n ) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                            TRIM( Attribute_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Byte

  FUNCTION get_rank1_Short( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Short ), DIMENSION( : ), &
                              INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(rank1 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------
    ! Check that the dummy output argument is big enough
    ! --------------------------------------------------

    IF ( SIZE( Attribute_Value ) < n ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is too large to fit in return argument', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value( 1:n ) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                            TRIM( Attribute_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Short

  FUNCTION get_rank1_Long( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    INTEGER( Long ), DIMENSION( : ), &
                              INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(rank1 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------
    ! Check that the dummy output argument is big enough
    ! --------------------------------------------------

    IF ( SIZE( Attribute_Value ) < n ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is too large to fit in return argument', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value( 1:n ) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                            TRIM( Attribute_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Long

  FUNCTION get_rank1_Single( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    REAL( Single ), DIMENSION( : ), &
                              INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(rank1 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------
    ! Check that the dummy output argument is big enough
    ! --------------------------------------------------

    IF ( SIZE( Attribute_Value ) < n ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is too large to fit in return argument', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value( 1:n ) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                            TRIM( Attribute_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Single

  FUNCTION get_rank1_Double( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Output
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name

    ! -- Type specific output
    REAL( Double ), DIMENSION( : ), &
                              INTENT( OUT ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Attribute(rank1 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME

    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL display_message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- INQUIRE THE ATTRIBUTE --                         #
    !#--------------------------------------------------------------------------#

    ! -----------------------------
    ! Get the size of the attribute
    ! -----------------------------

    NF90_Status = NF90_INQUIRE_ATTRIBUTE( NC_fileID, &
                                          varID, &
                                          TRIM( Attribute_Name ), &
                                          LEN = n )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error inquiring '//&
                            TRIM( varNAME )//' '//&
                            ' attribute '//TRIM( Attribute_Name )//' length - '//&
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! --------------------------------------------------
    ! Check that the dummy output argument is big enough
    ! --------------------------------------------------

    IF ( SIZE( Attribute_Value ) < n ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            TRIM( varNAME )//&
                            ' attribute '//&
                            TRIM( Attribute_Name )//&
                            ' is too large to fit in return argument', &
                            Error_Status, &
                            MessagE_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- GET THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_GET_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value( 1:n ) )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL display_message( ROUTINE_NAME, &
                            'Error reading '//TRIM( varNAME )//' attribute '// &
                            TRIM( Attribute_Name )// &
                            ' - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION get_rank1_Double





  FUNCTION put_scalar_Byte( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    INTEGER( Byte ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Byte


  FUNCTION put_scalar_Short( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    INTEGER( Short ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Short


  FUNCTION put_scalar_Long( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    INTEGER( Long ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Long


  FUNCTION put_scalar_Single( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    REAL( Single ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Single


  FUNCTION put_scalar_Double( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    REAL( Double ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_Double


  FUNCTION put_scalar_CHARACTER( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN )  :: NC_fileID
    CHARACTER( * ),           INTENT( IN )  :: Attribute_Name
    CHARACTER( * ), INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(scalar CHARACTER)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_scalar_CHARACTER


  FUNCTION put_rank1_Byte( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN ) :: NC_fileID
    CHARACTER( * ),           INTENT( IN ) :: Attribute_Name
    INTEGER( Byte ), DIMENSION( : ), &
                              INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(rank1 Byte)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Byte


  FUNCTION put_rank1_Short( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN ) :: NC_fileID
    CHARACTER( * ),           INTENT( IN ) :: Attribute_Name
    INTEGER( Short ), DIMENSION( : ), &
                              INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(rank1 Short)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Short


  FUNCTION put_rank1_Long( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN ) :: NC_fileID
    CHARACTER( * ),           INTENT( IN ) :: Attribute_Name
    INTEGER( Long ), DIMENSION( : ), &
                              INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(rank1 Long)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Long


  FUNCTION put_rank1_Single( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN ) :: NC_fileID
    CHARACTER( * ),           INTENT( IN ) :: Attribute_Name
    REAL( Single ), DIMENSION( : ), &
                              INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(rank1 Single)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Single


  FUNCTION put_rank1_Double( &
    NC_fileID,       &  ! Input
    Attribute_Name,  &  ! Input
    Attribute_Value, &  ! Input
    Variable_Name,   &  ! Optional input
    Message_Log )    &  ! Error messaging
  RESULT ( Error_Status )


    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                  INTENT( IN ) :: NC_fileID
    CHARACTER( * ),           INTENT( IN ) :: Attribute_Name
    REAL( Double ), DIMENSION( : ), &
                              INTENT( IN ) :: Attribute_Value

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Variable_Name

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Put_netCDF_Attribute(rank1 Double)'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status

    INTEGER :: varID
    CHARACTER( 256 ) :: varNAME



    !#--------------------------------------------------------------------------#
    !#                  -- ASSIGN A SUCCESSFUL RETURN STATUS --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                 -- BRANCH ON PRESCENCE OF VARIABLE NAME --               #
    !#--------------------------------------------------------------------------#

    IF ( PRESENT( Variable_Name ) ) THEN


      ! ---------------------------
      ! Attribute is for a variable
      ! ---------------------------

      ! -- Get the variable ID
      NF90_Status = NF90_INQ_VARID( NC_fileID, &
                                    TRIM( Variable_Name ), &
                                    varID )

      IF ( NF90_Status /= NF90_NOERR ) THEN
        Error_Status = FAILURE
        CALL Display_Message( ROUTINE_NAME, &
                              'Error inquiring variable ID for '// &
                                TRIM( Variable_Name )// &
                                ' - '// &
                                TRIM( NF90_STRERROR( NF90_Status ) ), &
                              Error_Status, &
                              Message_Log = Message_Log )
        RETURN
      END IF

      ! -- Assign a generic descriptor
      varNAME = TRIM( Variable_Name )

    ELSE


      ! -------------------
      ! Attribute is global
      ! -------------------

      ! -- Assign a "variable" id
      varID = NF90_GLOBAL

      ! -- Assign a generic descriptor
      varNAME = 'global'

    END IF



    !#--------------------------------------------------------------------------#
    !#                   -- PUT THE REQUESTED ATTRIBUTE --                      #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_PUT_ATT( NC_fileID, &
                                varID, &
                                TRIM( Attribute_Name ), &
                                Attribute_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error writing '//TRIM( varNAME )//' attribute '// &
                              TRIM( Attribute_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION put_rank1_Double



END MODULE netCDF_Attribute_Utility


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
!
! $Date: 2006/07/26 21:39:05 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: netCDF_Attribute_Utility.f90,v $
! Revision 1.2  2006/07/26 21:39:05  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 1.1  2006/06/08 21:47:55  wd20pd
! Initial checkin.
!
! Revision 1.4  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.3  2005/01/11 18:49:42  paulv
! - Regenerated from updated pp files.
!
! Revision 1.1  2003/02/12 20:07:18  paulv
! Initial checkin.
!
!
!
!
!
