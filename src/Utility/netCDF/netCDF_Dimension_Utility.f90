
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
    '$Id: netCDF_Dimension_Utility.f90,v 1.2 2006/07/26 21:39:05 wd20pd Exp $'


CONTAINS



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


