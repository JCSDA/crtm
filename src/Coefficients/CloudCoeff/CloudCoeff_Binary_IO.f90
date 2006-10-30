!------------------------------------------------------------------------------
!M+
! NAME:
!       CloudCoeff_Binary_IO
!
! PURPOSE:
!       Module containing routines to read and write Binary format
!       CloudCoeff files. The coefficients are used to calculate
!       cloud optical properties.
!       
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE CloudCoeff_Binary_IO
!
! MODULES:
!       Type_Kinds:            Module containing definitions for kinds
!                              of variable types.
!
!       File_Utility:          Module containing generic file utility routines
!
!       Message_Handler:       Module to define simple error codes and
!                              handle error conditions
!                              USEs: FILE_UTILITY module
!
!       Binary_File_Utility:   Module containing utility routines for "Coeff" 
!                              datafiles in Binary format.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
!       CloudCoeff_Define:   Module defining the CloudCoeff data structure and
!                              containing routines to manipulate it.
!                              USEs: TYPE_KINDS module
!                                    FILE_UTILITY module
!                                    ERROR_HANDLER module
!
! CONTAINS:
!       Inquire_CloudCoeff_Binary: Function to inquire a binary format
!                                    CloudCoeff file.
!
!       Read_CloudCoeff_Binary:    Function to read a binary format
!                                    CloudCoeff file.
!
!       Write_CloudCoeff_Binary:   Function to write a binary format
!                                    CloudCoeff file.
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
!       User specified CloudCoeff data files for both input and output.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
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

MODULE CloudCoeff_Binary_IO


  ! ----------
  ! Module use
  ! ----------

  USE Type_Kinds
  USE File_Utility
  USE Message_Handler
  USE Binary_File_Utility

  USE CloudCoeff_Define


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------
  ! Visibilities
  ! ------------

  PRIVATE
  PUBLIC :: Inquire_CloudCoeff_Binary
  PUBLIC :: Read_CloudCoeff_Binary
  PUBLIC :: Write_CloudCoeff_Binary


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &
    '$Id: CloudCoeff_Binary_IO.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'

  ! -- Keyword set value
  INTEGER, PRIVATE, PARAMETER :: SET = 1

CONTAINS


!------------------------------------------------------------------------------
!S+
! NAME:
!       Inquire_CloudCoeff_Binary
!
! PURPOSE:
!       Function to inquire a Binary format CloudCoeff file.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Inquire_CloudCoeff_Binary( Filename,                            &  ! Input
!                                                   n_DummyDimension = n_DummyDimension, &  ! Optional Output
!                                                   Release          = Release,          &  ! Optional Output
!                                                   Version          = Version,          &  ! Optional Output
!                                                   RCS_Id           = RCS_Id,           &  ! Revision control
!                                                   Message_Log      = Message_Log       )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of a
!                           CloudCoeff format data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar, LEN = *
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:        Character string specifying a filename in which any
!                           messages will be logged. If not specified, or if an
!                           error occurs opening the log file, the default action
!                           is to output messages to standard output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar, LEN = *
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       n_DummyDimension:   The number of channels dimension for the data 
!                           in the coefficient file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Release:            The coefficient file release number.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       Version:            The coefficient file version number.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( OUT ), OPTIONAL
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the Binary file inquiry was successful
!                              == FAILURE an error occurred.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:  Function to open Binary format Coeff
!                          data files.
!                          SOURCE: BINARY_FILE_UTILITY module
!
!       Display_Message:   Subroutine to output messages
!                          SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Yong Han,       NOAA/NESDIS;     Yong.Han@noaa.gov
!                       Quanhua Liu,    QSS Group, Inc;  Quanhua.Liu@noaa.gov
!                       Paul van Delst, CIMSS/SSEC;      paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Yong Han, Quanhua Liu, Paul van Delst
!S-
!------------------------------------------------------------------------------

  FUNCTION Inquire_CloudCoeff_Binary( Filename,         &  ! Input
                                        n_DummyDimension, &  ! Optional Output
                                        Release,          &  ! Optional Output
                                        Version,          &  ! Optional Output
                                        RCS_Id,           &  ! Revision control
                                        Message_Log )     &  ! Error messaging
                                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: Filename

    ! -- Optional Output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: n_DummyDimension
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Release
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Version

    ! -- Revision control
    CHARACTER( * ), OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Inquire_CloudCoeff_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    INTEGER :: IO_Status

    INTEGER :: FileID

    INTEGER( Long ) :: File_Release
    INTEGER( Long ) :: File_Version

    INTEGER( Long ) :: File_n_DummyDimension
 



    !#--------------------------------------------------------------------------#
    !#                  -- DEFINE A SUCCESSFUL EXIT STATUS --                   #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS


    ! -----------------------------------
    ! Set the RCS Id argument if supplied
    ! -----------------------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- OPEN THE BINARY FORMAT CloudCoeff DATA FILE --          #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID, &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening CloudCoeff file '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the Release/Version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) File_Release, &
                                       File_Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading CloudCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! ------------------------
    ! Read the dummy dimension
    ! ------------------------

    READ( FileID, IOSTAT = IO_Status ) File_n_DummyDimension

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading n_DummyDimension dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- ASSIGN THE RETURN ARGUMENTS --                     #
    !#--------------------------------------------------------------------------#

    ! ----------
    ! Dimensions
    ! ----------

    IF ( PRESENT( n_DummyDimension ) ) THEN
      n_DummyDimension = File_n_DummyDimension
    END IF


    ! --------------
    ! Ancillary info
    ! --------------

    IF ( PRESENT( Release ) ) THEN
      Release = File_Release
    END IF


    IF ( PRESENT( Version ) ) THEN
      Version = File_Version
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      Error_Status = WARNING
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Inquire_CloudCoeff_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Read_CloudCoeff_Binary
!
! PURPOSE:
!       Function to read Binary format CloudCoeff files.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Read_CloudCoeff_Binary( Filename,                              &  ! Input
!                                                CloudCoeff,                          &  ! Output
!                                                Quiet             = Quiet,             &  ! Optional input
!                                                Process_ID        = Process_ID,        &  ! Optional input
!                                                Output_Process_ID = Output_Process_ID, &  ! Optional input
!                                                RCS_Id            = RCS_Id,            &  ! Revision control
!                                                Message_Log       = Message_Log        )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:           Character string specifying the name of the
!                           input binary format CloudCoeff data file.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar, LEN = *
!                           ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:              Set this argument to suppress Information messages
!                           being printed to standard output (or the Message
!                           log file if the Message_Log optional argument is
!                           used.) By default, Information messages are printed.
!                           If QUIET = 0, Information messages are OUTPUT.
!                              QUIET = 1, Information messages are SUPPRESSED.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Process_ID:         Set this argument to the MPI process ID that this
!                           function call is running under. This value is used
!                           solely for controlling INFORMATIOn Message output.
!                           If MPI is not being used, ignore this argument.
!                           This argument is ignored if the Quiet argument is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Output_Process_ID:  Set this argument to the MPI process ID in which
!                           all Information messages are to be output. If
!                           the passed Process_ID value agrees with this value
!                           the Information messages are output. 
!                           This argument is ignored if the Quiet argument
!                           is set.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:        Character string specifying a filename in which
!                           any messages will be logged. If not specified,
!                           or if an error occurs opening the log file, the
!                           default action is to output messages to standard
!                           output.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar, LEN = *
!                           ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       CloudCoeff:       Structure to contain the scattering coefficient
!                           data read from the file.
!                           UNITS:      N/A
!                           TYPE:       CloudCoeff_type
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: INTENT( IN OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:             Character string containing the Revision Control
!                           System Id field for the module.
!                           UNITS:      N/A
!                           TYPE:       CHARACTER
!                           DIMENSION:  Scalar
!                           ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status:       The return value is an integer defining the error status.
!                           The error codes are defined in the ERROR_HANDLER module.
!                           If == SUCCESS the Binary file read was successful
!                              == FAILURE a read error occurred.
!                              == WARNING an error occurred closing the Binary file.
!                           UNITS:      N/A
!                           TYPE:       INTEGER
!                           DIMENSION:  Scalar
!
! CALLS:
!       Open_Binary_File:            Function to open Binary format Coeff
!                                    data files.
!                                    SOURCE: BINARY_FILE_UTILITY module
!
!       Check_CloudCoeff_Release:  Function to check the Release value of
!                                    the CloudCoeff data.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Allocate_CloudCoeff:       Function to allocate the pointer members
!                                    of the CloudCoeff structure.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Version_CloudCoeff:        Subroutine to return a string containing
!                                    version and dimension information about
!                                    a CloudCoeff data structure.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! COMMENTS:
!       Note the INTENT on the output CloudCoeff argument is IN OUT rather
!       than just OUT. This is necessary because the argument may be defined on
!       input. To prevent memory leaks, the IN OUT INTENT is a must.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Read_CloudCoeff_Binary( Filename,          &  ! Input
                                     CloudCoeff,      &  ! Output
                                     Quiet,             &  ! Optional input
                                     Process_ID,        &  ! Optional input
                                     Output_Process_ID, &  ! Optional input
                                     RCS_Id,            &  ! Revision control
                                     Message_Log )      &  ! Error messaging
                                   RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                          -- TYPE DECLARATIONS --                         #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),            INTENT( IN )     :: Filename

    ! -- Output
    TYPE( CloudCoeff_type ), INTENT( IN OUT ) :: CloudCoeff

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )     :: Quiet
    INTEGER,         OPTIONAL, INTENT( IN )     :: Process_ID
    INTEGER,         OPTIONAL, INTENT( IN )     :: Output_Process_ID

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT )    :: RCS_Id

    ! -- Error Message log file
    CHARACTER( * ),  OPTIONAL, INTENT( IN )     :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Read_CloudCoeff_Binary'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message
    LOGICAL :: Noisy
    INTEGER :: IO_Status
    CHARACTER( 128 ) :: Process_ID_Tag
    INTEGER :: FileID
    INTEGER( Long ) :: n_Frequency,n_Size_MW,n_wavenumber,n_Size_IR,n_Temperature
    INTEGER( Long ) :: n_Density,Max_Legendre_Terms,Max_Phase_Elements 
    INTEGER( Long ) :: n_Items, i, j, k, l, m, n
    INTEGER( Long ), DIMENSION( N_CloudCoeff_ITEMS ) :: Data_Type
 



    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless....
    IF ( PRESENT( Quiet ) ) THEN
      ! -- ....the QUIET keyword is set.
      IF ( Quiet == SET ) Noisy = .FALSE.
    ELSE
      ! -- ....the Process_ID is not selected for output
      IF ( PRESENT( Process_ID ) .AND. PRESENT( Output_Process_ID ) ) THEN
        IF ( Process_ID /= Output_Process_ID ) Noisy = .FALSE.
      END IF
    END IF


    ! -----------------------------------
    ! Create a process ID Message tag for
    ! WARNING and FAILURE Messages
    ! -----------------------------------

    IF ( PRESENT( Process_ID ) ) THEN
      WRITE( Process_ID_Tag, '( ";  MPI Prcess ID: ", i5 )' ) Process_ID
    ELSE
      Process_ID_Tag = ' '
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#                    -- OPEN THE CloudCoeff DATA FILE --                 #
    !#--------------------------------------------------------------------------#

    Error_Status = Open_Binary_File( TRIM( Filename ), &
                                     FileID,   &
                                     Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( Filename )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                      -- READ THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! ------------------------------------
    ! Read the release/version information
    ! ------------------------------------

    READ( FileID, IOSTAT = IO_Status ) CloudCoeff%Release, &
                                       CloudCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading CloudCoeff file Release/Version values from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -----------------
    ! Check the release
    ! -----------------

    Error_Status = Check_CloudCoeff_Release( CloudCoeff, &
                                               Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'CloudCoeff Release check failed for '//TRIM( Filename ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF


    ! -------------------
    ! Read the dimensions
    ! -------------------

    READ( FileID, IOSTAT = IO_Status ) n_Frequency, n_Size_MW, n_wavenumber, n_Size_IR, &
                                       n_Temperature, n_Density, Max_Legendre_Terms, Max_Phase_Elements 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading dimension from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF

                                  
    !#--------------------------------------------------------------------------#
    !#                 -- ALLOCATE THE CloudCoeff STRUCTURE --                #
    !#--------------------------------------------------------------------------#
    
    Error_Status = Allocate_CloudCoeff( n_Frequency, n_Size_MW, n_wavenumber, n_Size_IR, & ! Input
                                          n_Temperature, n_Density, Max_Legendre_Terms,    & ! Input
                                          Max_Phase_Elements, &  ! Input
                                          CloudCoeff, &
                                          Message_Log = Message_Log )

                             
    !#--------------------------------------------------------------------------#
    !#                         -- READ THE DATA ITEMS --                        #
    !#--------------------------------------------------------------------------#
                                                       
    READ( FileID, IOSTAT = IO_Status )  CloudCoeff%frequency,CloudCoeff%wavenumber, &
                            CloudCoeff%Reff_MW,CloudCoeff%Reff_IR, &
                            CloudCoeff%Temperature,CloudCoeff%Density

    !#--------------------------------------------------------------------------#
    !#   Converting effective radius from mm to micrometer                                #
    !#--------------------------------------------------------------------------#
        CloudCoeff%Reff_MW = 1000.0 * CloudCoeff%Reff_MW
        CloudCoeff%Reff_IR = 1000.0 * CloudCoeff%Reff_IR

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading frequency et al. from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
                                                    
    !    Microwave liquid phase part
  DO k =1, n_Frequency
    DO i= 1, n_Size_MW 
      DO j= 1, n_Temperature
      READ( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_L_MW(k,i,j), &
        CloudCoeff%w_L_MW(k,i,j),CloudCoeff%g_L_MW(k,i,j)

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading MW (L) extinction, single scattering albedo, asymmetric factor from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
       DO L= 0, Max_Legendre_Terms 
       READ( FileID, IOSTAT = IO_Status ) (CloudCoeff%phase_coeff_L_MW(k,i,j,L,m),m=1,Max_Phase_Elements)
       IF ( IO_Status /= 0 ) THEN
       Error_Status = FAILURE
       WRITE( Message, '( "Error reading MW (L) expansion coefficients for phase functions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
       CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
       CLOSE( FileID )
       RETURN
       END IF
       ENDDO 
    ENDDO
  ENDDO
  ENDDO
           

    !    Microwave solid phase part
  DO k =1, n_Frequency
    DO i= 1, n_Size_MW 
      DO j= 1, n_Density
      READ( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_S_MW(k,i,j), &
        CloudCoeff%w_S_MW(k,i,j),CloudCoeff%g_S_MW(k,i,j)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading MW (S) extinction, single scattering albedo, asymmetric factor from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
       DO L= 0, Max_Legendre_Terms 
       READ( FileID, IOSTAT = IO_Status ) (CloudCoeff%phase_coeff_S_MW(k,i,j,L,m),m=1,Max_Phase_Elements)
       IF ( IO_Status /= 0 ) THEN
       Error_Status = FAILURE
       WRITE( Message, '( "Error reading MW (S) expansion coefficients for phase functions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
       CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
       CLOSE( FileID )
       RETURN
       END IF
       ENDDO 
    ENDDO
  ENDDO
  ENDDO


    !    IR liquid phase part
  DO k =1, n_wavenumber
    DO i= 1, n_Size_IR 
      READ( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_L_IR(k,i), &
        CloudCoeff%w_L_IR(k,i),CloudCoeff%g_L_IR(k,i)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading IR (L) extinction, single scattering albedo, asymmetric factor from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
       DO L= 0, Max_Legendre_Terms 
       READ( FileID, IOSTAT = IO_Status ) CloudCoeff%phase_coeff_L_IR(k,i,L)
       IF ( IO_Status /= 0 ) THEN
       Error_Status = FAILURE
       WRITE( Message, '( "Error reading IR (L) expansion coefficients for phase functions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
       CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
       CLOSE( FileID )
       RETURN
       END IF
       ENDDO 
    ENDDO
  ENDDO


    !    IR solid phase part
  DO k =1, n_wavenumber
    DO i= 1, n_Size_IR 
      DO j= 1, n_Density
      READ( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_S_IR(k,i,j), &
        CloudCoeff%w_S_IR(k,i,j),CloudCoeff%g_S_IR(k,i,j)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error reading IR (S) extinction, single scattering albedo, asymmetric factor from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID )
      RETURN
    END IF
       DO L= 0, Max_Legendre_Terms 
       READ( FileID, IOSTAT = IO_Status ) CloudCoeff%phase_coeff_S_IR(k,i,j,L)
       IF ( IO_Status /= 0 ) THEN
       Error_Status = FAILURE
       WRITE( Message, '( "Error reading IR (S) expansion coefficients for phase functions from ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
       CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            Error_Status, &
                            Message_Log = Message_Log )
       CLOSE( FileID )
       RETURN
       END IF
       ENDDO 
    ENDDO
  ENDDO
  ENDDO


    !#--------------------------------------------------------------------------#
    !#                          -- CLOSE THE FILE --                            #
    !#--------------------------------------------------------------------------#

    CLOSE( FileID, STATUS = 'KEEP', &
                   IOSTAT = IO_Status )

    IF ( IO_Status /= 0 ) THEN
      WRITE( Message, '( "Error closing ", a, ". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message )//TRIM( Process_ID_Tag ), &
                            WARNING, &
                            Message_Log = Message_Log )
    END IF


    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_CloudCoeff( CloudCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Read_CloudCoeff_Binary




!------------------------------------------------------------------------------
!S+
! NAME:
!       Write_CloudCoeff_Binary
!
! PURPOSE:
!       Function to write Binary format CloudCoeff files.
!
! CATEGORY:
!       Scattering : CloudCoeff
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Write_CloudCoeff_Binary( Filename,                 &  ! Input
!                                                 CloudCoeff,             &  ! Input
!                                                 Quiet       = Quiet,      &  ! Optional input
!                                                 RCS_Id      = RCS_Id,     &  ! Revision control
!                                                 Message_Log = Message_Log )  ! Error messaging
!
! INPUT ARGUMENTS:
!       Filename:     Character string specifying the name of an output
!                     CloudCoeff format data file.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN )
!
!       CloudCoeff: Structure containing the scattering coefficient data.
!                     UNITS:      N/A
!                     TYPE:       CloudCoeff_type
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Quiet:        Set this keyword to suppress information messages being
!                     printed to standard output (or the Message log file if 
!                     the Message_Log optional argument is used.) By default,
!                     information messages are printed.
!                     If QUIET = 0, information messages are OUTPUT.
!                        QUIET = 1, information messages are SUPPRESSED.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
!       Message_Log:  Character string specifying a filename in which any
!                     messages will be logged. If not specified, or if an
!                     error occurs opening the log file, the default action
!                     is to output messages to standard output.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar, LEN = *
!                     ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       None.
!
! OPTIONAL OUTPUT ARGUMENTS:
!       RCS_Id:       Character string containing the Revision Control
!                     System Id field for the module.
!                     UNITS:      N/A
!                     TYPE:       CHARACTER
!                     DIMENSION:  Scalar
!                     ATTRIBUTES: OPTIONAL, INTENT( OUT )
!
! FUNCTION RESULT:
!       Error_Status: The return value is an integer defining the error status.
!                     The error codes are defined in the ERROR_HANDLER module.
!                     If == SUCCESS the Binary file write was successful
!                        == FAILURE a write error occurred.
!                        == WARNING an error occurred closing the Binary file.
!                     UNITS:      N/A
!                     TYPE:       INTEGER
!                     DIMENSION:  Scalar
!
! CALLS:
!       Associated_CloudCoeff:     Function to test the association status
!                                    of the pointer members of a CloudCoeff
!                                    structure.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Check_CloudCoeff_Release:  Function to check the Release value of
!                                    the CloudCoeff data.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Open_Binary_File:            Function to open Binary format Coeff
!                                    data files.
!                                    SOURCE: BINARY_FILE_UTILITY module
!
!       Version_CloudCoeff:        Subroutine to return a string containing
!                                    version and dimension information about
!                                    a CloudCoeff data structure.
!                                    SOURCE: CloudCoeff_DEFINE module
!
!       Display_Message:             Subroutine to output messages
!                                    SOURCE: ERROR_HANDLER module
!
! SIDE EFFECTS:
!       - If the output file already exists, it is overwritten.
!       - If an error occurs *during* the write phase, the output file is deleted
!         before returning to the calling routine.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 24-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  FUNCTION Write_CloudCoeff_Binary( Filename,     &  ! Input
                                      CloudCoeff, &  ! Input
                                      Quiet,        &  ! Optional input
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
    CHARACTER( * ),            INTENT( IN ) :: Filename
    TYPE( CloudCoeff_type ), INTENT( IN ) :: CloudCoeff

    ! -- Optional input
    INTEGER,         OPTIONAL, INTENT( IN )  :: Quiet

    ! -- Revision control
    CHARACTER( * ),  OPTIONAL, INTENT( OUT ) :: RCS_Id

    ! -- Error handler Message log
    CHARACTER( * ),  OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! -------------------
    ! Function parameters
    ! -------------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Write_CloudCoeff_Binary'
    CHARACTER( * ), PARAMETER :: FILE_STATUS_ON_ERROR = 'DELETE'


    ! ------------------
    ! Function variables
    ! ------------------

    CHARACTER( 256 ) :: Message

    LOGICAL :: Noisy

    INTEGER :: IO_Status
    INTEGER :: FileID
    INTEGER :: i, j, k, l, m 
 


    !#--------------------------------------------------------------------------#
    !#                     -- INITIALISE THE ERROR STATUS --                    #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- CHECK/SET OPTIONAL KEYWORD ARGUMENTS --                #
    !#--------------------------------------------------------------------------#

    ! -------------------
    ! Info Message output
    ! -------------------

    ! -- Output informational Messages....
    Noisy = .TRUE.

    ! -- ....unless the QUIET keyword is set.
    IF ( PRESENT( Quiet ) ) THEN
      IF ( Quiet == SET ) Noisy = .FALSE.
    END IF


    ! -------------------
    ! Module version info
    ! -------------------

    IF ( PRESENT( RCS_Id ) ) THEN
      RCS_Id = ' '
      RCS_Id = MODULE_RCS_ID
    END IF



    !#--------------------------------------------------------------------------#
    !#             -- CHECK STRUCTURE POINTER ASSOCIATION STATUS --             #
    !#                                                                          #
    !#                ALL structure pointers must be associated                 #
    !#--------------------------------------------------------------------------#

    IF ( .NOT. Associated_CloudCoeff( CloudCoeff ) ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Some or all INPUT CloudCoeff pointer '//&
                            'members are NOT associated.', &
                            Error_Status,    &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                          -- CHECK INPUT --                               #
    !#--------------------------------------------------------------------------#

    ! ----------------------------------------
    ! Check the CloudCoeff structure Release
    ! ----------------------------------------

    Error_Status = Check_CloudCoeff_Release( CloudCoeff, &
                                               Message_Log = Message_Log )

    IF ( Error_Status /= SUCCESS ) THEN
      CALL Display_Message( ROUTINE_NAME, &
                            'CloudCoeff Release check failed.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -------------------------------------------
    ! Check the CloudCoeff structure dimensions
    ! -------------------------------------------

    IF ( CloudCoeff%n_Frequency < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Frequency must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                                
    IF ( CloudCoeff%n_Size_MW < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Size_MW must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                            
    IF ( CloudCoeff%n_wavenumber < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_wavenumber must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                                      
    IF ( CloudCoeff%n_Size_IR < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Size_IR must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                                       
    IF ( CloudCoeff%n_Temperature < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Temperature must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                          
    IF ( CloudCoeff%n_Density < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input n_Density must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                                     
    IF ( CloudCoeff%Max_Legendre_Terms < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Max_Legendre_Terms must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF
                              
    IF ( CloudCoeff%Max_Phase_Elements < 1 ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Input Max_Phase_Elements must be > 0.', &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

    !#--------------------------------------------------------------------------#
    !#              -- OPEN THE SCATTERING COEFFICIENT DATA FILE --             #
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
    !#                     -- WRITE THE "FILE HEADER" --                        #
    !#--------------------------------------------------------------------------#

    ! -------------------------------------
    ! Write the release/version information
    ! -------------------------------------

    WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%Release, &
                                        CloudCoeff%Version

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing CloudCoeff file Release/Version values to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF


    ! --------------------
    ! Write the dimensions
    ! --------------------

    WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%n_Frequency, CloudCoeff%n_Size_MW, &
                                       CloudCoeff%n_wavenumber, CloudCoeff%n_Size_IR, &
                                       CloudCoeff%n_Temperature, CloudCoeff%n_Density, &
                                       CloudCoeff%Max_Legendre_Terms, CloudCoeff%Max_Phase_Elements 

    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing dimension to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

                                                       
    WRITE( FileID, IOSTAT = IO_Status )  CloudCoeff%frequency,CloudCoeff%wavenumber, &
                            CloudCoeff%Reff_MW,CloudCoeff%Reff_IR, &
                            CloudCoeff%Temperature,CloudCoeff%Density
                            
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing frequency et al to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

    !    Microwave liquid phase part
  DO k =1, CloudCoeff%n_Frequency
    DO i= 1, CloudCoeff%n_Size_MW 
      DO j= 1, CloudCoeff%n_Temperature
      WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_L_MW(k,i,j), &
        CloudCoeff%w_L_MW(k,i,j),CloudCoeff%g_L_MW(k,i,j)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing ext_L_MW et al to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       DO L= 0, CloudCoeff%Max_Legendre_Terms 
       WRITE( FileID, IOSTAT = IO_Status ) (CloudCoeff%phase_coeff_L_MW(k,i,j,L,m),m=1,CloudCoeff%Max_Phase_Elements)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing phase_coeff_L_MW to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       ENDDO 
    ENDDO
  ENDDO
  ENDDO
           

    !    Microwave solid phase part
  DO k =1, CloudCoeff%n_Frequency
    DO i= 1, CloudCoeff%n_Size_MW 
      DO j= 1, CloudCoeff%n_Density
      WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_S_MW(k,i,j), &
        CloudCoeff%w_S_MW(k,i,j),CloudCoeff%g_S_MW(k,i,j)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing ext_S_MW et al to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       DO L= 0, CloudCoeff%Max_Legendre_Terms 
       WRITE( FileID, IOSTAT = IO_Status ) (CloudCoeff%phase_coeff_S_MW(k,i,j,L,m),m=1,CloudCoeff%Max_Phase_Elements)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing phase_coeff_S_MW to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       ENDDO 
    ENDDO
  ENDDO
  ENDDO


    !    IR liquid phase part
  DO k =1, CloudCoeff%n_wavenumber
    DO i= 1, CloudCoeff%n_Size_IR 
      WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_L_IR(k,i), &
        CloudCoeff%w_L_IR(k,i),CloudCoeff%g_L_IR(k,i)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing ext_L_IR et al to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       DO L= 0, CloudCoeff%Max_Legendre_Terms 
       WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%phase_coeff_L_IR(k,i,L)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing phase_coeff_L_IR to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       ENDDO 
    ENDDO
  ENDDO


    !    IR solid phase part
  DO k =1, CloudCoeff%n_wavenumber
    DO i= 1, CloudCoeff%n_Size_IR 
      DO j= 1, CloudCoeff%n_Density
      WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%ext_S_IR(k,i,j), &
        CloudCoeff%w_S_IR(k,i,j),CloudCoeff%g_S_IR(k,i,j)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing ext_S_IR et al to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       DO L= 0, CloudCoeff%Max_Legendre_Terms 
       WRITE( FileID, IOSTAT = IO_Status ) CloudCoeff%phase_coeff_S_IR(k,i,j,L)
    IF ( IO_Status /= 0 ) THEN
      Error_Status = FAILURE
      WRITE( Message, '( "Error writing phase_coeff_S_IR to ", a, &
                        &". IOSTAT = ", i5 )' ) &
                      TRIM( Filename ), IO_Status
      CALL Display_Message( ROUTINE_NAME, &
                            TRIM( Message ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      CLOSE( FileID, STATUS = FILE_STATUS_ON_ERROR )
      RETURN
    END IF

       ENDDO 
    ENDDO
  ENDDO
  ENDDO


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



    !#--------------------------------------------------------------------------#
    !#                      -- OUTPUT AN INFO Message --                        #
    !#--------------------------------------------------------------------------#

    IF ( Noisy ) THEN
      CALL Version_CloudCoeff( CloudCoeff, Message )
      CALL Display_Message( ROUTINE_NAME, &
                            'FILE: '//TRIM( Filename )//'; '//TRIM( Message ), &
                            INFORMATION, &
                            Message_Log = Message_Log )
    END IF

  END FUNCTION Write_CloudCoeff_Binary

END MODULE CloudCoeff_Binary_IO


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: CloudCoeff_Binary_IO.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: CloudCoeff_Binary_IO.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/08/19 20:05:08  qliu
! -- rename ScatterCoeff_Binary_IO.f90 to CloudCoeff_Binary_IO.f90
!    first working version for reading cloud optical coefficients.
!
! Revision 1.1  2004/11/04 21:09:29  paulv
! Initial checkin.
!
!
!
