!------------------------------------------------------------------------------
!M+
! NAME:
!       Coefficient_Parameter
!
! PURPOSE:
!       Module for parameters used in reading and writing coefficient data
!       files.
!
! CATEGORY:
!       NCEP RTM
!
! CALLING SEQUENCE:
!       USE Coefficient_Parameters
!
! OUTPUTS:
!       Parameters
!
! MODULES:
!       None.
!
! CONTAINS:
!       None.
!
! EXTERNALS:
!       None
!
! COMMON BLOCKS:
!       None.
!
! SIDE EFFECTS:
!       None.
!
! RESTRICTIONS:
!       None.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS@NOAA/NCEP/EMC 31-Jul-2002
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

MODULE Coefficient_Parameters


  ! -----------------------
  ! Disable implicit typing
  ! -----------------------

  IMPLICIT NONE


  ! ------------------
  ! Default visibility
  ! ------------------

  PRIVATE


  ! -------------------------
  ! Maximum number of sensors
  ! -------------------------

  INTEGER, PUBLIC, PARAMETER :: MAX_N_SENSORS = 41


  ! -----------------------------------------------------
  ! The SATELLITE and SENSOR names for the available data
  ! -----------------------------------------------------

  CHARACTER( * ), PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_SATELLITE_NAME = (/ 'NOAA-05', 'NOAA-05', 'NOAA-06', 'NOAA-06', &
                              'NOAA-07', 'NOAA-07', 'NOAA-08', 'NOAA-08', &
                              'NOAA-09', 'NOAA-09', 'NOAA-10', 'NOAA-10', &
                              'NOAA-11', 'NOAA-11', 'NOAA-12', 'NOAA-12', &
                              'NOAA-14', 'NOAA-14', 'NOAA-15', 'NOAA-15', &
                              'NOAA-15', 'NOAA-16', 'NOAA-16', 'NOAA-16', &
                              'NOAA-17', 'NOAA-17', 'NOAA-17', 'GOES-08', &
                              'GOES-08', 'GOES-09', 'GOES-09', 'GOES-10', &
                              'GOES-10', 'GOES-11', 'GOES-11', 'GOES-12', &
                              'GOES-12', 'DMSP-14', 'AQUA   ', 'AQUA   ', &
                              'AQUA   ' /)

  CHARACTER( * ), PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_SENSOR_NAME = (/ 'HIRS/2 ', 'MSU    ', 'HIRS/2 ', 'MSU    ', &
                           'HIRS/2 ', 'MSU    ', 'HIRS/2 ', 'MSU    ', &
                           'HIRS/2 ', 'MSU    ', 'HIRS/2 ', 'MSU    ', &
                           'HIRS/2 ', 'MSU    ', 'HIRS/2 ', 'MSU    ', &
                           'HIRS/2 ', 'MSU    ', 'HIRS/3 ', 'AMSU-A ', &
                           'AMSU-B ', 'HIRS/3 ', 'AMSU-A ', 'AMSU-B ', &
                           'HIRS/3 ', 'AMSU-A ', 'AMSU-B ', 'SOUNDER', &
                           'IMAGER ', 'SOUNDER', 'IMAGER ', 'SOUNDER', &
                           'IMAGER ', 'SOUNDER', 'IMAGER ', 'SOUNDER', &
                           'IMAGER ', 'SSM/T-2', 'AIRS   ', 'AMSU-A ', &
                           'HSB    ' /)

  CHARACTER( * ), PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_FILE_PREFIX = (/ 'hirs2_n05 ', 'msu_n05   ', 'hirs2_n06 ', 'msu_n06   ', &
                           'hirs2_n07 ', 'msu_n07   ', 'hirs2_n08 ', 'msu_n08   ', &
                           'hirs2_n09 ', 'msu_n09   ', 'hirs2_n10 ', 'msu_n10   ', &
                           'hirs2_n11 ', 'msu_n11   ', 'hirs2_n12 ', 'msu_n12   ', &
                           'hirs2_n14 ', 'msu_n14   ', 'hirs3_n15 ', 'amsua_n15 ', &
                           'amsub_n15 ', 'hirs3_n16 ', 'amsua_n16 ', 'amsub_n16 ', &
                           'hirs3_n17 ', 'amsua_n17 ', 'amsub_n17 ', 'sndr_g08  ', &
                           'imgr_g08  ', 'sndr_g09  ', 'imgr_g09  ', 'sndr_g10  ', &
                           'imgr_g10  ', 'sndr_g11  ', 'imgr_g11  ', 'sndr_g12  ', &
                           'imgr_g12  ', 'ssmt2_f14 ', 'airs_aqua ', 'amsua_aqua', &
                           'hsb_aqua  ' /)


  ! -------------------------------------------------------------------------------
  ! The valid NCEP sensor ID values:
  !
  !   hirs2_n05 ==   5   msu_n05    == 205   hirs2_n06   ==   6   msu_n06    == 206
  !   hirs2_n07 ==   7   msu_n07    == 207   hirs2_n08   ==   8   msu_n08    == 208
  !   hirs2_n09 ==   9   msu_n09    == 209   hirs2_n10   ==  10   msu_n10    == 210
  !   hirs2_n11 ==  11   msu_n11    == 211   hirs2_n12   ==  12   msu_n12    == 212
  !   hirs2_n14 ==  14   msu_n14    == 214   hirs3_n15   ==  15   amsua_n15  == 315
  !   amsub_n15 == 415   hirs3_n16  ==  16   amsua_n16   == 316   amsub_n16  == 416
  !   hirs3_n17 ==  17   amsua_n17  == 317   amsub_n17   == 417   sndr_g08   ==  58
  !   imgr_g08  == 258   sndr_g09   ==  59   imgr_g09    == 259   sndr_g10   ==  60
  !   imgr_g10  == 260   sndr_g11   ==  61   imgr_g11    == 261   sndr_g12   ==  62
  !   imgr_g12  == 262   ssmt2_f14  == 114   airs_aqua   ==  49   amsua_aqua == 349
  !   hsb_aqua  == 449
  !
  ! -------------------------------------------------------------------------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_NCEP_SENSOR_ID = (/   5, 205,   6, 206, &
                                7, 207,   8, 208, &
                                9, 209,  10, 210, &
                               11, 211,  12, 212, &
                               14, 214,  15, 315, &
                              415,  16, 316, 416, &
                               17, 317, 417,  58, &
                              258,  59, 259,  60, &
                              260,  61, 261,  62, &
                              262, 114,  49, 349, &
                              449  /)
                              
                              



  ! -------------------------------------------------------------------------------
  ! The corresponding WMO satellite and sensor ID values:
  !
  ! Satellite ID:
  !   hirs2_n05 == 708   msu_n05    == 708   hirs2_n06   == 706   msu_n06    == 706
  !   hirs2_n07 == 707   msu_n07    == 707   hirs2_n08   == 200   msu_n08    == 200
  !   hirs2_n09 == 201   msu_n09    == 201   hirs2_n10   == 202   msu_n10    == 202
  !   hirs2_n11 == 203   msu_n11    == 203   hirs2_n12   == 204   msu_n12    == 204
  !   hirs2_n14 == 205   msu_n14    == 205   hirs3_n15   == 206   amsua_n15  == 206
  !   amsub_n15 == 206   hirs3_n16  == 207   amsua_n16   == 207   amsub_n16  == 207
  !   hirs3_n17 == 208   amsua_n17  == 208   amsub_n17   == 208   sndr_g08   == 252
  !   imgr_g08  == 252   sndr_g09   == 253   imgr_g09    == 253   sndr_g10   == 254
  !   imgr_g10  == 254   sndr_g11   == 255   imgr_g11    == 255   sndr_g12   == 256
  !   imgr_g12  == 256   ssmt2_f14  == 247   airs_aqua   == 784   amsua_aqua == 784
  !   hsb_aqua  == 784
  !
  !   Note the Aqua satellite ID is a guess right now. WMO tables haven't been
  !   updated since Nov.2001.
  !
  ! Sensor ID:
  !   hirs2_n05 == 605   msu_n05    == 623   hirs2_n06   == 605   msu_n06    == 623
  !   hirs2_n07 == 605   msu_n07    == 623   hirs2_n08   == 605   msu_n08    == 623
  !   hirs2_n09 == 605   msu_n09    == 623   hirs2_n10   == 605   msu_n10    == 623
  !   hirs2_n11 == 605   msu_n11    == 623   hirs2_n12   == 605   msu_n12    == 623
  !   hirs2_n14 == 605   msu_n14    == 623   hirs3_n15   == 606   amsua_n15  == 570
  !   amsub_n15 == 574   hirs3_n16  == 606   amsua_n16   == 570   amsub_n16  == 574
  !   hirs3_n17 == 606   amsua_n17  == 570   amsub_n17   == 574   sndr_g08   == 626
  !   imgr_g08  == 615   sndr_g09   == 626   imgr_g09    == 615   sndr_g10   == 626
  !   imgr_g10  == 615   sndr_g11   == 626   imgr_g11    == 615   sndr_g12   == 626
  !   imgr_g12  == 615   ssmt2_f14  == 907   airs_aqua   == 420   amsua_aqua == 570
  !   hsb_aqua  == 574
  !
  !   Note that the HSB sensor ID is set to the AMSU-B value. The WMO tables do
  !   not have a separate entry for HSB.
  ! -------------------------------------------------------------------------------

  INTEGER, PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_WMO_SATELLITE_ID = (/ 708, 708, 706, 706, &
                                707, 707, 200, 200, &
                                201, 201, 202, 202, &
                                203, 203, 204, 204, &
                                205, 205, 206, 206, &
                                206, 207, 207, 207, &
                                208, 208, 208, 252, &
                                252, 253, 253, 254, &
                                254, 255, 255, 256, &
                                256, 247, 784, 784, &
                                784  /)

  INTEGER, PRIVATE, PARAMETER, DIMENSION( MAX_N_SENSORS ) :: &
    VALID_WMO_SENSOR_ID = (/ 605, 623, 605, 623, &
                             605, 623, 605, 623, &
                             605, 623, 605, 623, &
                             605, 623, 605, 623, &
                             605, 623, 606, 570, &
                             574, 606, 570, 574, &
                             606, 570, 574, 626, &
                             615, 626, 615, 626, &
                             615, 626, 615, 626, &
                             615, 907, 420, 570, &
                             203  /)

                                                                 

  ! ---------------------
  ! Subprogram visibility
  ! ---------------------

  PUBLIC :: Get_Sensor_and_Satellite_IDs
  PUBLIC :: Get_Sensor_Name
  PUBLIC :: Get_Satellite_Name
  PUBLIC :: Get_WMO_IDs


  ! ----------
  ! Intrinsics
  ! ----------

  INTRINSIC COUNT, &
            PACK


CONTAINS



!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Sensor_and_Satellite_IDs
!
! PURPOSE:
!       Subroutine to get the Sensor and Satellite IDs for a specified
!       satellite and sensor.
!
! CATEGORY:
!       NCEP RTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Get_Sensor_and_Satellite_IDs( Sensor_Name,      &  ! Input
!                                          Satellite_Name,   &  ! Input
!                                          NCEP_Sensor_ID,   &  ! Output
!                                          WMO_Sensor_ID,    &  ! Output
!                                          WMO_Satellite_ID, &  ! Output
!
! INPUT ARGUMENTS:
!       Sensor_Name:       A character string containing the sensor name.
!                          UNITS:      None
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
!       Satellite_Name:    A character string containing the satellite name.
!                          UNITS:      None
!                          TYPE:       CHARACTER( * )
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       NCEP_Sensor_ID:    The NCEP/EMC "in-house" value used to distinguish
!                          between different sensor/platform combinations.
!                          A value of -1 indicates that no matching value
!                          was found.
!                          UNITS:      None
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT )
!
!       WMO_Sensor_ID:     The WMO Sensor ID number taken from Common
!                          Code Table C-8 in documentation at
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          A value of -1 indicates that no matching value
!                          was found.
!                          UNITS:      None
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT )
!
!       WMO_Satellite_ID:  The WMO Satellite ID number taken from Common
!                          Code Table C-5 in documentation at
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          A value of -1 indicates that no matching value
!                          was found.
!                          UNITS:      None
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 31-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Get_Sensor_and_Satellite_IDs( Sensor_Name,      &  ! Input
                                           Satellite_Name,   &  ! Input
                                           NCEP_Sensor_ID,   &  ! Output
                                           WMO_Sensor_ID,    &  ! Output
                                           WMO_Satellite_ID, &  ! Output
                                           File_Prefix       )  ! Output

    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    CHARACTER( * ), INTENT( IN )  :: Sensor_Name
    CHARACTER( * ), INTENT( IN )  :: Satellite_Name

    INTEGER,        INTENT( OUT ) :: NCEP_Sensor_ID
    INTEGER,        INTENT( OUT ) :: WMO_Sensor_ID
    INTEGER,        INTENT( OUT ) :: WMO_Satellite_ID
    CHARACTER( * ), INTENT( OUT ) :: File_Prefix


    ! ---------------
    ! Local variables
    ! ---------------

    CHARACTER( LEN( VALID_SENSOR_NAME(1) )    ) :: Sensor
    CHARACTER( LEN( VALID_SATELLITE_NAME(1) ) ) :: Satellite

    INTEGER, DIMENSION( 1 ) :: i
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALIZE THE RETURN VALUES --                     #
    !#--------------------------------------------------------------------------#

    NCEP_Sensor_ID   = -1
    WMO_Sensor_ID    = -1
    WMO_Satellite_ID = -1
    File_Prefix      = ' '



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( LEN_TRIM( Sensor_Name    ) == 0 .OR. &
         LEN_TRIM( Satellite_Name ) == 0      ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    ! -- Truncate, or pad, the input names to the
    ! -- same length as the parameter values
    Sensor    = Sensor_Name
    Satellite = Satellite_Name

    ! -- Count the matchups
    n = COUNT( VALID_SENSOR_NAME    == Sensor    .AND. &
               VALID_SATELLITE_NAME == Satellite       )

    IF ( n == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- DETERMINE THE MATCHING INDEX AND EXTRACT THE NUMBERS --        #
    !#--------------------------------------------------------------------------#

    ! -- Get the matching index
    i = PACK( (/ ( n, n = 1, MAX_N_SENSORS ) /), &
              ( VALID_SENSOR_NAME    == Sensor    .AND. &
                VALID_SATELLITE_NAME == Satellite        ) )

    ! -- Extract the IDs
    NCEP_Sensor_ID   = VALID_NCEP_SENSOR_ID( i(1) )
    WMO_Sensor_ID    = VALID_WMO_SENSOR_ID( i(1) )
    WMO_Satellite_ID = VALID_WMO_SATELLITE_ID( i(1) )
    File_Prefix      = VALID_FILE_PREFIX( i(1) )

  END SUBROUTINE Get_Sensor_and_Satellite_IDs



!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Satellite_Name
!
! PURPOSE:
!       Subroutine to obtain a character string containing a satellite name.
!
! CATEGORY:
!       NCEP RTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Get_Satellite_Name( Satellite_ID,   &  ! Input
!                                Satellite_Name, &  ! Output
!                                WMO             )  ! Optional input
!
! INPUT ARGUMENTS:
!       Satellite_ID:     The satellite ID number for which the name is required.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       WMO:              Set this optional argument to identify the input
!                         SATELLITE_ID argument as a WMO Satellite ID.
!                         If = 0; Satellite ID is interpreted using NCEP ID values.
!                            = 1; Satellite ID is interpreted using WMO ID values.
!                         If not specified, the default action is to use the NCEP
!                         sensor ID values.
!                         UNITS:      None
!                         TYPE:       INTEGER
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Satellite_Name:   A character string containing the satellite name.
!                         If an invalid or unrecognised satellite ID was
!                         input, a blank string is returned.
!                         UNITS:      None
!                         TYPE:       CHARACTER( * )
!                         DIMENSION:  Scalar
!                         ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Get_Satellite_Name( Satellite_ID,   &  ! Input
                                 Satellite_Name, &  ! Output
                                 WMO             )  ! Optional input


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,           INTENT( IN )  :: Satellite_ID

    CHARACTER( * ),    INTENT( OUT ) :: Satellite_Name

    INTEGER, OPTIONAL, INTENT( IN )  :: WMO


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( MAX_N_SENSORS ) :: Valid_Satellite_ID
    INTEGER, DIMENSION( 1 ) :: i
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALIZE THE RETURN VALUES --                     #
    !#--------------------------------------------------------------------------#

    Satellite_Name = ' '



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( Satellite_ID < 0 ) THEN
      RETURN
    END IF

    Valid_Satellite_ID = VALID_NCEP_SENSOR_ID
    IF ( PRESENT( WMO ) ) THEN
      IF ( WMO == 1 ) THEN
        Valid_Satellite_ID = VALID_WMO_SATELLITE_ID
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    n = COUNT( Valid_Satellite_ID == Satellite_ID )

    IF ( n == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- DETERMINE THE MATCHING INDEX AND EXTRACT THE SATELLITE NAME --     #
    !#--------------------------------------------------------------------------#

    i = PACK( (/ ( n, n = 1, MAX_N_SENSORS ) /), &
              Valid_Satellite_ID == Satellite_ID )

    Satellite_Name = TRIM( VALID_SATELLITE_NAME( i(1) ) )

  END SUBROUTINE Get_Satellite_Name



!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_Sensor_Name
!
! PURPOSE:
!       Subroutine to obtain a character string containing a snesor name.
!
! CATEGORY:
!       NCEP RTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Get_Sensor_Name( Sensor_ID,   &  ! Input
!                             Sensor_Name, &  ! Output
!                             WMO          )  ! Optional input
!
! INPUT ARGUMENTS:
!       Sensor_ID:     The sensor ID number for which the name is required.
!                      UNITS:      None
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       WMO:           Set this optional argument to identify the input
!                      SENSOR_ID argument as a WMO Sensor ID.
!                      If = 0; Sensor ID is interpreted using NCEP ID values.
!                         = 1; Sensor ID is interpreted using WMO ID values.
!                      If not specified, the default action is to use the NCEP
!                      sensor ID values.
!                      UNITS:      None
!                      TYPE:       INTEGER
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( IN ), OPTIONAL
!
! OUTPUT ARGUMENTS:
!       Sensor_Name:   A character string containing the sensor name. If an
!                      invalid or unrecognised sensor ID was input, a blank
!                      string is returned.
!                      UNITS:      None
!                      TYPE:       CHARACTER( * )
!                      DIMENSION:  Scalar
!                      ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 07-Aug-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Get_Sensor_Name( Sensor_ID,   &  ! Input
                              Sensor_Name, &  ! Output
                              WMO          )  ! Optional input


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER,           INTENT( IN )  :: Sensor_ID

    CHARACTER( * ),    INTENT( OUT ) :: Sensor_Name

    INTEGER, OPTIONAL, INTENT( IN )  :: WMO


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( MAX_N_SENSORS ) :: Valid_Sensor_ID
    INTEGER, DIMENSION( 1 ) :: i
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALIZE THE RETURN VALUES --                     #
    !#--------------------------------------------------------------------------#

    Sensor_Name = ' '



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( Sensor_ID < 0 ) THEN
      RETURN
    END IF

    Valid_Sensor_ID = VALID_NCEP_SENSOR_ID
    IF ( PRESENT( WMO ) ) THEN
      IF ( WMO == 1 ) THEN
        Valid_Sensor_ID = VALID_WMO_SENSOR_ID
      END IF
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    n = COUNT( Valid_Sensor_ID == Sensor_ID )

    IF ( n == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#       -- DETERMINE THE MATCHING INDEX AND EXTRACT THE SENSOR NAME --     #
    !#--------------------------------------------------------------------------#

    i = PACK( (/ ( n, n = 1, MAX_N_SENSORS ) /), &
              Valid_Sensor_ID == Sensor_ID )

    Sensor_Name = TRIM( VALID_SENSOR_NAME( i(1) ) )

  END SUBROUTINE Get_Sensor_Name



!------------------------------------------------------------------------------
!S+
! NAME:
!       Get_WMO_IDs
!
! PURPOSE:
!       Subroutine to get the WMO Satellite and Sensor IDs for a particular
!       NCEP Sensor ID
!
! CATEGORY:
!       NCEP RTM
!
! LANGUAGE:
!       Fortran-90
!
! CALLING SEQUENCE:
!       CALL Get_WMO_IDs( NCEP_Sensor_ID,  &  ! Input
!                         WMO_Sensor_ID,   &  ! Output
!                         WMO_Satellite_ID )  ! Output
!
! INPUT ARGUMENTS:
!       NCEP_Sensor_ID:  The NCEP/EMC "in-house" value used to distinguish
!                        between different sensor/platform combinations.
!                        UNITS:      None
!                        TYPE:       INTEGER
!                        DIMENSION:  Scalar
!                        ATTRIBUTES: INTENT( IN )
!
! OPTIONAL INPUT ARGUMENTS:
!       None.
!
! OUTPUT ARGUMENTS:
!       WMO_Sensor_ID:     The WMO Sensor ID number taken from Common
!                          Code Table C-8 in documentation at
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          A value of -1 indicates that no matching value
!                          for the NCEP_Sensor_ID input was found.
!                          UNITS:      None
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT )
!
!       WMO_Satellite_ID:  The WMO Satellite ID number taken from Common
!                          Code Table C-5 in documentation at
!                            http://www.wmo.ch/web/ddbs/Code-tables.html
!                          A value of -1 indicates that no matching value
!                          for the NCEP_Sensor_ID input was found.
!                          UNITS:      None
!                          TYPE:       INTEGER
!                          DIMENSION:  Scalar
!                          ATTRIBUTES: INTENT( OUT )
!
! OPTIONAL OUTPUT ARGUMENTS:
!       None.
!
! CALLS:
!       None.
!
! EXTERNALS:
!       None.
!
! COMMON BLOCKS:
!       None
!
! SIDE EFFECTS:
!       None
!
! RESTRICTIONS:
!       None
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 31-Jul-2002
!                       paul.vandelst@ssec.wisc.edu
!S-
!------------------------------------------------------------------------------

  SUBROUTINE Get_WMO_IDs ( NCEP_Sensor_ID,  &  ! Input
                           WMO_Sensor_ID,   &  ! Output
                           WMO_Satellite_ID )  ! Output


    !#--------------------------------------------------------------------------#
    !#                         -- TYPE DECLARATIONS --                          #
    !#--------------------------------------------------------------------------#

    ! ---------
    ! Arguments
    ! ---------

    INTEGER, INTENT( IN )  :: NCEP_Sensor_ID

    INTEGER, INTENT( OUT ) :: WMO_Sensor_ID
    INTEGER, INTENT( OUT ) :: WMO_Satellite_ID


    ! ---------------
    ! Local variables
    ! ---------------

    INTEGER, DIMENSION( 1 ) :: i
    INTEGER :: n



    !#--------------------------------------------------------------------------#
    !#                   -- INITIALIZE THE RETURN VALUES --                     #
    !#--------------------------------------------------------------------------#

    WMO_Sensor_ID    = -1
    WMO_Satellite_ID = -1



    !#--------------------------------------------------------------------------#
    !#                           -- CHECK INPUT --                              #
    !#--------------------------------------------------------------------------#

    IF ( NCEP_Sensor_ID < 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#              -- DETERMINE IF THERE ARE ANY MATCHING IDs --               #
    !#--------------------------------------------------------------------------#

    n = COUNT( VALID_NCEP_SENSOR_ID == NCEP_Sensor_ID )

    IF ( n == 0 ) THEN
      RETURN
    END IF



    !#--------------------------------------------------------------------------#
    !#        -- DETERMINE THE MATCHING INDEX AND EXTRACT THE NUMBERS --        #
    !#--------------------------------------------------------------------------#

    i = PACK( (/ ( n, n = 1, MAX_N_SENSORS ) /), &
              VALID_NCEP_SENSOR_ID == NCEP_Sensor_ID )

    WMO_Sensor_ID    = VALID_WMO_SENSOR_ID( i(1) )
    WMO_Satellite_ID = VALID_WMO_SATELLITE_ID( i(1) )

  END SUBROUTINE Get_WMO_IDs

END MODULE Coefficient_Parameters


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Coefficient_Parameters.f90,v 1.3 2002/10/02 20:37:47 paulv Exp $
!
! $Date: 2002/10/02 20:37:47 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Coefficient_Parameters.f90,v $
! Revision 1.3  2002/10/02 20:37:47  paulv
! - Added Get_Sensor_and_Satellite_IDs() function.
! - Updated parameter lists.
!
! Revision 1.2  2002/08/07 20:14:56  paulv
! - Added GET_SENSOR_NAME() and GET_SATELLITE_NAME() subroutines.
!
! Revision 1.1  2002/08/01 20:22:15  paulv
! Initial checkin prior to testing.
!
!
!
!
