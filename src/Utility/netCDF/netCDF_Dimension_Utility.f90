!--------------------------------------------------------------------------------
!M+
! NAME:
!       netCDF_Dimension_Utility
!
! PURPOSE:
!       Module containing utility routines for netCDF file dimension access.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       USE netCDF_Dimension_Utility
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
!       Get_netCDF_Dimension:  Function to retrieve a netCDF file dimension
!                              by name. This function is a wrapper for some
!                              of the NetCDF library functions to simplify
!                              the retrieval of a dimension with error
!                              checking.
!
! EXTERNALS:
!       None
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
!--------------------------------------------------------------------------------

MODULE netCDF_Dimension_Utility


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
  PUBLIC :: Get_netCDF_Dimension


  ! -----------------
  ! Module parameters
  ! -----------------

  ! -- Module RCS Id string
  CHARACTER( * ), PRIVATE, PARAMETER :: MODULE_RCS_ID = &


CONTAINS


!--------------------------------------------------------------------------------
!S+
! NAME:
!       Get_netCDF_Dimension
!
! PURPOSE:
!       Function to retrieve a netCDF file dimension by name.
!
!       This function is a wrapper for some of the NetCDF library functions
!       to simplify the retrieval of a dimension value with error checking.
!
! CATEGORY:
!       netCDF
!
! LANGUAGE:
!       Fortran-95
!
! CALLING SEQUENCE:
!       Error_Status = Get_netCDF_Dimension ( NC_FileID,                   & ! Input
!                                             Dimension_Name,              & ! Input
!                                             Dimension_Value,             & ! Output
!                                             Dimension_ID = Dimension_ID, & ! Optional Output
!                                             Message_Log  = Message_Log   ) ! Error messaging
!
! INPUT ARGUMENTS:
!       NC_FileID:       File ID of a netCDF format file returned from an
!                        netCDF library OPEN call.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
!       Dimension_Name:  Name of the netCDF dimension to retrieve.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       Message_Log:     Character string specifying a filename in which any
!                        messages will be logged. If not specified, or if an
!                        error occurs opening the log file, the default action
!                        is to output messages to the screen.
!                        UNITS:      N/A
!                        TYPE:       CHARACTER( * )
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( IN )
!
! OUTPUT ARGUMENTS:
!       Dimension_Value: Value of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       Dimension_ID     NetCDF ID of the requested dimension.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: OPTIONAL, INTENT( OUT )
!                          
! FUNCTION RESULT
!       Error_Status:    The return value is an integer defining the error status.
!                        The error codes are defined in the Message_Handler module.
!                        If == SUCCESS the netCDF dimension retrieval was successful
!                           == FAILURE an unrecoverable error occurred.
!                        UNITS:      N/A
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!
! CALLS:
!       NF90_INQ_DIMID:         Function to get a dimension id.
!                               SOURCE: netCDF module and library.
!
!       NF90_INQUIRE_DIMENSION: Function to get a dimension value.
!                               SOURCE: netCDF module and library.
!
!       Display_Message:        Subroutine to output messages
!                               SOURCE: Message_Handler module
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
!S-
!--------------------------------------------------------------------------------

  FUNCTION Get_netCDF_Dimension ( NC_FileID,       &  ! Input
                                  Dimension_Name,  &  ! Input
                                  Dimension_Value, &  ! Output
                                  Dimension_ID,    &  ! Optional output
                                  Message_Log )    &  ! Error messaging
                                RESULT ( Error_Status )



    !#--------------------------------------------------------------------------#
    !#                       -- TYPE DECLARATIONS --                            #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    ! --Input
    INTEGER,                  INTENT( IN )  :: NC_FileID
    CHARACTER( * ),           INTENT( IN )  :: Dimension_Name

    ! -- Output
    INTEGER,                  INTENT( OUT ) :: Dimension_Value

    ! -- Optional output
    INTEGER,        OPTIONAL, INTENT( OUT ) :: Dimension_ID

    ! -- Error messaging
    CHARACTER( * ), OPTIONAL, INTENT( IN )  :: Message_Log


    ! ------
    ! Result
    ! ------

    INTEGER :: Error_Status


    ! ----------
    ! Parameters
    ! ----------

    CHARACTER( * ), PARAMETER :: ROUTINE_NAME = 'Get_netCDF_Dimension'


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER :: NF90_Status
    INTEGER :: dimID




    !#--------------------------------------------------------------------------#
    !#                  -- SET SUCCESSFUL RETURN STATUS --                      #
    !#--------------------------------------------------------------------------#

    Error_Status = SUCCESS



    !#--------------------------------------------------------------------------#
    !#                       -- GET THE DIMENSION ID --                         #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQ_DIMID( NC_FileID, &
                                  Dimension_Name, &
                                  dimID )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error inquiring dimension ID for '// &
                              TRIM( Dimension_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF


    ! -----------------------------
    ! Fill optional return argument
    ! -----------------------------

    IF ( PRESENT( Dimension_ID ) ) Dimension_ID = dimID



    !#--------------------------------------------------------------------------#
    !#                      -- GET THE DIMENSION VALUE --                       #
    !#--------------------------------------------------------------------------#

    NF90_Status = NF90_INQUIRE_DIMENSION( NC_FileID, &
                                          dimID, &
                                          Len = Dimension_Value )

    IF ( NF90_Status /= NF90_NOERR ) THEN
      Error_Status = FAILURE
      CALL Display_Message( ROUTINE_NAME, &
                            'Error reading dimension value for '// &
                              TRIM( Dimension_Name )// &
                              ' - '// &
                              TRIM( NF90_STRERROR( NF90_Status ) ), &
                            Error_Status, &
                            Message_Log = Message_Log )
      RETURN
    END IF

  END FUNCTION Get_netCDF_Dimension

END MODULE netCDF_Dimension_Utility


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
! $Log: netCDF_Dimension_Utility.f90,v $
! Revision 1.2  2006/07/26 21:39:05  wd20pd
! Additional replacement of "Error_Handler" string with "Message_Handler"
! in documentaiton blocks.
!
! Revision 1.1  2006/06/08 21:47:55  wd20pd
! Initial checkin.
!
! Revision 1.3  2006/05/02 16:58:03  dgroff
! *** empty log message ***
!
! Revision 1.2  2005/01/11 18:50:21  paulv
! - Updated header documentation.
!
! Revision 1.1  2002/05/20 18:00:14  paulv
! Initial checkin.
!
!
!
!
!
