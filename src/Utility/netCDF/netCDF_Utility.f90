!--------------------------------------------------------------------------------
!M+
! NAME:
!       netCDF_Utility
!
! PURPOSE:
!       Module containing utility routines for netCDF file access. This module
!       also provides access to the Dimension, Attribute, and Variable module
!       routines.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE netCDF_Utility
!
! MODULES:
!       Type_Kinds:                Module containing data type kind
!                                  definitions.
!
!       Message_Handler:           Module to define error codes and handle
!                                  error conditions
!                                  USEs: FILE_UTILITY module
!
!       netcdf:                    Module supplied with the Fortran 90 version
!                                  of the netCDF libraries (at least v3.5.0).
!                                  See http://www.unidata.ucar.edu/packages/netcdf
!
!       netCDF_Dimension_Utility:  Module containing utility routines
!                                  for netCDF file dimension access.
!                                  USEs: TYPE_KINDS module
!                                        Message_Handler module
!                                        netcdf module
!
!       netCDF_Variable_Utility:   Module containing utility routines
!                                  for netCDF file variable access.
!                                  USEs: TYPE_KINDS module
!                                        Message_Handler module
!                                        netcdf module
!
!       netCDF_Attribute_Utility:  Module containing utility routines
!                                  for netCDF file attribute access.
!                                  USEs: TYPE_KINDS module
!                                        Message_Handler module
!                                        netcdf module
!
! CONTAINS:
!       Remove_NULL_Characters:  Subroutine to remove NULL characters (ASCII 0)
!                                from an input string. Strings retrieved from
!                                netCDF files are terminated with the NULL
!                                character (\0 in C).
!
!       Open_netCDF:             Function to open an existing netCDF data file.
!                                This is simply a wrapper for NF90_OPEN so that
!                                the netcdf library module is not required in
!                                the user calling routine.
!
!       Close_netCDF:            Function to close an open netCDF data file.
!                                This is simply a wrapper for NF90_CLOSE so
!                                that the netcdf library module is not required
!                                in the user calling routine.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None.
!
! CREATION HISTORY:
!       Written by:   Paul van Delst, CIMSS/SSEC, 20-Nov-2000
!                     paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2000 Paul van Delst
!
!M-
!--------------------------------------------------------------------------------

MODULE netCDF_Utility

  ! --------------------
  ! Declare modules used
  ! --------------------

  USE Type_Kinds
  USE Message_Handler
  USE netcdf

  USE netCDF_Dimension_Utility
  USE netCDF_Variable_Utility
  USE netCDF_Attribute_Utility


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ----------
  ! Visibility
  ! ----------

  ! -- Everything is private by default
  PRIVATE

  ! -- The public members of the Dimension, 
  ! -- Variable, and Attribute modules
  PUBLIC Get_netCDF_Dimension
  PUBLIC Get_netCDF_Variable
  PUBLIC Put_netCDF_Variable
  PUBLIC Get_netCDF_Attribute
  PUBLIC Put_netCDF_Attribute

  ! -- The public routines in this module
  PUBLIC Remove_NULL_Characters
  PUBLIC Open_netCDF
  PUBLIC Close_netCDF


  ! ---------------------
  ! Procedure overloading
  ! ---------------------

  INTERFACE Remove_NULL_Characters
    MODULE PROCEDURE Remove_NULL_Characters_scalar
    MODULE PROCEDURE Remove_NULL_Characters_rank1
  END INTERFACE Remove_NULL_Characters


CONTAINS



  SUBROUTINE Remove_NULL_Characters_scalar( String )
    CHARACTER( * ), INTENT( IN OUT ) :: String
    INTEGER :: i
    Character_Loop: DO i = 1, LEN( String )
      IF ( IACHAR( String(i:i) ) == 0 ) THEN
        String(i:LEN( String ) ) = ' '
        EXIT Character_Loop
      END IF
    END DO Character_Loop
  END SUBROUTINE Remove_NULL_Characters_scalar


  SUBROUTINE Remove_NULL_Characters_rank1( String )
    CHARACTER( * ), DIMENSION(:), INTENT( IN OUT ) :: String
    INTEGER :: n
    DO n = 1, SIZE( String )
      CALL Remove_NULL_Characters_scalar( String(n) )
    END DO
  END SUBROUTINE Remove_NULL_Characters_rank1





  FUNCTION Open_netCDF( NC_Filename,  &  ! Input
                        NC_FileID,    &  ! Output
                        Mode,         &  ! Optional input
                        Message_Log ) &  ! Error messaging
                      RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    CHARACTER( * ),           INTENT( IN )  :: NC_Filename

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: NC_FileID

    ! -- Optional input
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Mode

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Open_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( 10 ) :: Open_cMode
    INTEGER         :: Open_Mode

    INTEGER :: NF90_Status
    INTEGER :: FileID



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                       -- SET THE OPEN MODE VALUE --                      #
    !#--------------------------------------------------------------------------#

    ! -- Default is read-only
    Open_cMode = 'READ'
    Open_Mode  = NF90_NOWRITE

    IF ( PRESENT( Mode ) ) THEN
      Open_cMode = TRIM( ADJUSTL( Mode ) )
      SELECT CASE ( TRIM( Open_cMode ) )
        CASE ('READ')
          Open_Mode  = NF90_NOWRITE
        CASE ('READWRITE')
          Open_Mode  = NF90_WRITE
        CASE ('SHARE')
          Open_Mode  = NF90_SHARE
        CASE ('WRITESHARE')
          Open_Mode  = IOR( NF90_WRITE, NF90_SHARE )
        CASE DEFAULT
          Open_Mode  = NF90_NOWRITE
          Open_cMode = 'READ'
      END SELECT
    END IF



    !#--------------------------------------------------------------------------#
    !#                -- OPEN THE NETCDF DATA FILE FOR READING --               #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_OPEN( TRIM( NC_Filename ), &
                             Open_Mode, &
                             FileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error opening '//TRIM( NC_Filename )//&
                            ' for '//TRIM( Open_cMode )//' access - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#                               -- DONE --                                 #
    !#--------------------------------------------------------------------------#

    NC_FileID = FileID

  END FUNCTION Open_netCDF






  FUNCTION Close_netCDF( NC_FileID,    &  ! Input
                         Message_Log ) &  ! Error messaging
                       RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! -- Input
    INTEGER,                   INTENT( IN ) :: NC_FileID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL,  INTENT( IN ) :: Message_Log


    ! ---------------
    ! Function result
    ! ---------------

    INTEGER :: Error_Status


    ! ----------------
    ! Local parameters
    ! ----------------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Close_netCDF'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status



    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                -- OPEN THE NETCDF DATA FILE FOR READING --               #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_CLOSE( NC_FileID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error closing netCDF file - '// &
                            TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Close_netCDF

END MODULE netCDF_Utility


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
! $Log: netCDF_Utility.f90,v $
! Revision 1.2  2006/07/26 21:39:05  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 1.1  2006/06/08 21:47:55  wd20pd
! Initial checkin.
!
! Revision 1.11  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.10  2005/01/11 18:50:21  paulv
! - Updated header documentation.
!
! Revision 1.9  2004/12/29 21:03:45  paulv
! - Removed unused variable declarations.
!
! Revision 1.8  2003/05/23 21:11:39  paulv
! - Overloaded Remove_NULL_Characters() routine for use with scalar and rank-1
!   arguments.
!
! Revision 1.7  2003/02/14 20:53:12  paulv
! - All character arguments are now TRIMmed in the argument list.
!
! Revision 1.6  2003/02/12 20:08:01  paulv
! - Added netCDF Attribute module to USE list.
!
! Revision 1.5  2002/12/23 21:16:50  paulv
! - Added Put_netCDF_Variable() to PUBLIC list.
!
! Revision 1.4  2002/06/14 17:26:01  paulv
! - Added WRITESHARE Mode to Open_netCDF() function.
!
! Revision 1.3  2002/06/07 14:47:00  paulv
! - Altered Remove_Null_Characters() subroutine to clear the rest of the
!   input string and exit when the _first_ null character is found.
!
! Revision 1.2  2002/06/07 14:33:59  paulv
! - Added generic OPEN and CLOSE netCDF file functions
! - Added Remove_Null_Characters() subroutine to strip out the null character
!   padding that accompanies global attribute reads.
!
! Revision 1.1  2002/05/20 18:00:14  paulv
! Initial checkin.
!
!
!
!
!
