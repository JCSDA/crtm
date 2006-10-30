!------------------------------------------------------------------------------
!P+
! NAME:
!       Atmosphere_Rewrite
!
! PURPOSE:
!       Program to convert old Atmosphere data file formats to the latest one.
!
! CATEGORY:
!       CRTM : Atmosphere : Rewrite
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:                  Module containing definitions for kinds
!                                    of variable types.
!
!       Message_Handler:             Module to define simple error codes and
!                                    handle error conditions
!                                    USEs: FILE_UTILITY module
!
!       CRTM_Atmosphere_Define:      Module defining the Atmosphere data structure
!                                    and containing routines to manipulate it.
!                                    USEs: TYPE_KINDS module
!                                          ERROR_HANDLER module
!
!       CRTM_Atmosphere_Binary_IO:   Module containing routines to read and write
!                                    Atmosphere Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_ATMOSPHERE_DEFINE module
!                                          CRTM_CLOUD_BINARY_IO module
!                                          CRTM_AEROSOL_BINARY_IO module
!
!       CRTM_Cloud_Binary_IO:        Module containing routines to read and write
!                                    Cloud Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_CLOUD_DEFINE module
!
!       CRTM_Aerosol_Binary_IO:      Module containing routines to read and write
!                                    Aerosol Binary format files.
!                                    USEs: TYPE_KINDS module
!                                          FILE_UTILITY module
!                                          ERROR_HANDLER module
!                                          BINARY_FILE_UTILITY module
!                                          CRTM_AEROSOL_DEFINE module
!
! CONTAINS:
!       None.
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
!       Output: Binary format Aerosol, Cloud, and Atmosphere file.
!
! SIDE EFFECTS:
!       If the output files already exists, they are overwritten.
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 08-Apr-2005
!                       paul.vandelst@ssec.wisc.edu
!
!  Copyright (C) 2005 Paul van Delst
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
!P-
!------------------------------------------------------------------------------

PROGRAM Atmosphere_Rewrite


  ! ------------
  ! Module usage
  ! ------------

  USE Type_Kinds
  USE Message_Handler
  USE File_Utility

  USE CRTM_Atmosphere_Define
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Cloud_Binary_IO
  USE CRTM_Aerosol_Binary_IO

  USE CRTM_Atmosphere_Define_old, ONLY: Atmosphere_type_old =>     CRTM_Atmosphere_type, &
                                        Init_Atmosphere_old =>     CRTM_Init_Atmosphere, &
                                        Destroy_Atmosphere_old =>  CRTM_Destroy_Atmosphere, &
                                        Allocate_Atmosphere_old => CRTM_Allocate_Atmosphere, &
                                        Assign_Atmosphere_old =>   CRTM_Assign_Atmosphere
  USE CRTM_Atmosphere_Binary_IO_old, Read_Atmosphere_Binary_old =>  CRTM_Read_Atmosphere_Binary, &
                                     Write_Atmosphere_Binary_old => CRTM_Write_Atmosphere_Binary


  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ),  PARAMETER :: PROGRAM_NAME   = 'Atmosphere_Rewrite'
  CHARACTER( * ),  PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Atmosphere_Rewrite.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  INTEGER, PARAMETER :: N_PROFILES = 52
  INTEGER, PARAMETER :: N_AEROSOLS = 7
  INTEGER, PARAMETER :: SET = 1

  CHARACTER( * ), PARAMETER :: OLD_ATMOSPHERE_FILE = 'ECMWF.Atmosphere.bin.From_Quanhua'

  CHARACTER( * ), PARAMETER :: ATMOSPHERE_FILE = 'Atmosphere.bin'
  CHARACTER( * ), PARAMETER :: CLOUD_FILE      = 'Cloud.bin'
  CHARACTER( * ), PARAMETER :: AEROSOL_FILE    = 'Aerosol.bin'

  CHARACTER( * ), PARAMETER :: NEW_ATMOSPHERE_FILE = 'ECMWF-Atmosphere.Cloud.Aerosol.bin'

  ! ---------
  ! Variables
  ! ---------

  CHARACTER( 256 ) :: Message
  INTEGER          :: pn_pos
  CHARACTER( 80 )  :: pn_fmt

  CHARACTER( 256 ) :: Filename_old
  CHARACTER( 256 ) :: Filename

  INTEGER :: Error_Status
  INTEGER :: Allocate_Status
  INTEGER :: j, m, mc, ma, n, n_Clouds, n1, n2
 
  CHARACTER( 8 ) :: Marker

  TYPE( Atmosphere_type_old ),          DIMENSION( N_PROFILES ) :: Atmosphere_old

  TYPE( CRTM_Atmosphere_type ),         DIMENSION( N_PROFILES ) :: Atmosphere
  TYPE( CRTM_Cloud_type ), ALLOCATABLE, DIMENSION( : )          :: Cloud
  TYPE( CRTM_Aerosol_type ),            DIMENSION( N_AEROSOLS ) :: Aerosol



  !#----------------------------------------------------------------------------#
  !#                 -- INITIALIZE THE OLD Atmosphere STRUCTURE --              #
  !#----------------------------------------------------------------------------#

  CALL Init_Atmosphere_old( Atmosphere_old )



  !#----------------------------------------------------------------------------#
  !#                       -- OUTPUT DESCRIPTIVE HEADER --                      #
  !#----------------------------------------------------------------------------#

  pn_pos = ( LEN( PROGRAM_HEADER ) / 2 ) - &
           ( LEN( PROGRAM_NAME ) / 2 )
  pn_pos = MAX( pn_pos, 0 ) + 5
  WRITE( pn_fmt, '( "( ",i2,"x, a )" )' ) pn_pos

  WRITE( *, '(/5x, a)' ) PROGRAM_HEADER
  WRITE( *, FMT = TRIM( pn_fmt ) ) PROGRAM_NAME
  WRITE( *, '(/5x, " Program to read old format datafiles and write the new",&
             &/5x, "   format files.")' )
  WRITE( *, '(/5x, " $Revision: 1.2 $")' )
  WRITE( *, '( 5x, a, /)' ) PROGRAM_HEADER



  !#----------------------------------------------------------------------------#
  !#                  -- READ THE OLD FORMAT Atmosphere FILE --                 #
  !#----------------------------------------------------------------------------#

  Error_Status = Read_Atmosphere_Binary_old( OLD_ATMOSPHERE_FILE, &
                                             Atmosphere_old )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading old format Atmosphere file.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                       -- WRITE THE Cloud DATA --                           #
  !#----------------------------------------------------------------------------#

  ! -------------------------------------------------
  ! Count the total number of clouds in the structure
  ! -------------------------------------------------

  n_Clouds = SUM( Atmosphere_old%n_Clouds )


  ! ----------------------------------
  ! Allocate the cloud structure array
  ! ----------------------------------

  ALLOCATE( Cloud( n_Clouds ), STAT = Allocate_Status )

  IF ( Allocate_Status /= 0 ) THEN
    WRITE( Message, '( "Error allocating Cloud structure array. STAT = ", i5 )' ) Allocate_Status
    CALL Display_Message( PROGRAM_NAME, &
                          TRIM( Message ), &
                          FAILURE )
    STOP
  END IF


  ! ------------------------------------------------------------
  ! Copy over the clouds from the old Atmosphere structure array
  ! ------------------------------------------------------------

  ! -- Initialise begin index
  n1 = 1

  ! -- Loop over profiles
  DO m = 1, N_PROFILES

    ! -- Are there any clouds to copy?
    IF ( Atmosphere_old(m)%n_Clouds > 0 ) THEN

      ! -- Scale the Cloud data
      DO n = 1, Atmosphere_old(m)%n_Clouds
        Atmosphere_old(m)%Cloud(n)%Effective_Radius = 1000.0_fp_kind * Atmosphere_old(m)%Cloud(n)%Effective_Radius
        Atmosphere_old(m)%Cloud(n)%Water_Content    = 1000.0_fp_kind * Atmosphere_old(m)%Cloud(n)%Water_Content
      END DO

      ! -- Set the end index
      n2 = n1 + Atmosphere_old(m)%n_Clouds - 1

      ! -- Copy the cloud data
      Error_Status = CRTM_Assign_Cloud( Atmosphere_old(m)%Cloud, &
                                        Cloud(n1:n2) )

      IF ( Error_Status /= SUCCESS ) THEN
        WRITE( Message, '( "Error copying Cloud data for profile #", i5 )' ) m
        CALL Display_Message( PROGRAM_NAME, &
                              TRIM( Message ), &
                              FAILURE )
        STOP
      END IF

      ! -- Update the begin index for the next set of clouds
      n1 = n2 + 1

    END IF

  END DO


  ! --------------------------------
  ! Write the extracted data to file
  ! --------------------------------

  Error_Status = CRTM_Write_Cloud_Binary( CLOUD_FILE, &
                                          Cloud )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing Cloud datafile.', &
                          FAILURE )
    STOP
  END IF


  ! ----------------------------
  ! Destroy the Cloud structures
  ! ----------------------------

  Error_Status = CRTM_Destroy_Cloud( Cloud )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Cloud structures.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#              -- READ THE Atmosphere, Cloud, AND Aerosol DATA --            #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Read_Atmosphere_Binary( ATMOSPHERE_FILE, &
                                              Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Atmosphere datafile.', &
                          FAILURE )
    STOP
  END IF


  Error_Status = CRTM_Read_Cloud_Binary( CLOUD_FILE, &
                                         Cloud )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Cloud datafile.', &
                          FAILURE )
    STOP
  END IF


  Error_Status = CRTM_Read_Aerosol_Binary( AEROSOL_FILE, &
                                           Aerosol )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error reading Aerosol datafile.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                    -- COMBINE ALL THE BIOTS AND PIECES --                  #
  !#----------------------------------------------------------------------------#

  DO m = 1, N_PROFILES


    ! -----------------
    ! Insert cloud data
    ! -----------------

    SELECT CASE ( m )

      ! -- WATER clouds
      CASE ( 1, 7, 18, 24, 37, 48 )
        ALLOCATE( Atmosphere(m)%Cloud(1), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Cloud( Cloud(1), &  ! WATER cloud
                                          Atmosphere(m)%Cloud(1) )
        Atmosphere(m)%n_Clouds = 1

      ! -- RAIN clouds
      CASE ( 2, 15:17, 27, 32, 44, 51 )
        ALLOCATE( Atmosphere(m)%Cloud(1), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Cloud( Cloud(2), &  ! RAIN cloud
                                          Atmosphere(m)%Cloud(1) )
        Atmosphere(m)%n_Clouds = 1

      ! -- WATER and SNOW clouds
      CASE ( 10:14, 30, 49 )
        ALLOCATE( Atmosphere(m)%Cloud(2), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Cloud( Cloud(1:3:2), &
                                          Atmosphere(m)%Cloud )
        Atmosphere(m)%n_Clouds = 2

      ! -- RAIN and SNOW clouds
      CASE ( 5, 19:23, 39, 45:47, 50  )
        ALLOCATE( Atmosphere(m)%Cloud(2), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Cloud( Cloud(2:3), &
                                          Atmosphere(m)%Cloud )
        Atmosphere(m)%n_Clouds = 2

      ! -- WATER, RAIN, and SNOW clouds
      CASE ( 3, 31, 35 )
        ALLOCATE( Atmosphere(m)%Cloud(3), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Cloud( Cloud(1:3), &
                                          Atmosphere(m)%Cloud )
        Atmosphere(m)%n_Clouds = 3

      CASE DEFAULT

    END SELECT


    ! -------------------
    ! Insert aerosol data
    ! -------------------

    SELECT CASE ( m )

      ! -- SULFATE aerosol
      CASE ( 1:3 )
        ALLOCATE( Atmosphere(m)%Aerosol(1), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Aerosol( Aerosol(1), &  ! SULFATE aerosol
                                            Atmosphere(m)%Aerosol(1) )
        Atmosphere(m)%n_Aerosols = 1


      ! -- DUST aerosol
      CASE ( 8:11 )
        ALLOCATE( Atmosphere(m)%Aerosol(1), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Aerosol( Aerosol(2), &  ! DUST aerosol
                                            Atmosphere(m)%Aerosol(1) )
        Atmosphere(m)%n_Aerosols = 1


      ! -- DRY BLACK CARBON and DRY ORGANIC CARBON aerosol
      CASE ( 15, 27, 46 )
        ALLOCATE( Atmosphere(m)%Aerosol(2), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Aerosol( Aerosol(3:4), &  ! DRY BLACK and ORGANIC CARBON aerosol
                                            Atmosphere(m)%Aerosol )
        Atmosphere(m)%n_Aerosols = 2


      ! -- WET BLACK CARBON and WET ORGANIC CARBON aerosol
      CASE ( 18, 34, 51 )
        ALLOCATE( Atmosphere(m)%Aerosol(2), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Aerosol( Aerosol(5:6), &  ! WET BLACK and ORGANIC CARBON aerosol
                                            Atmosphere(m)%Aerosol )
        Atmosphere(m)%n_Aerosols = 2


      ! -- SEA SALT aerosol
      CASE ( 21:26, 35:40 )
        ALLOCATE( Atmosphere(m)%Aerosol(1), STAT = Allocate_Status )
        Error_Status = CRTM_Assign_Aerosol( Aerosol(7), &  ! SEA SALT aerosol
                                            Atmosphere(m)%Aerosol(1) )
        Atmosphere(m)%n_Aerosols = 1

      CASE DEFAULT

    END SELECT



    WRITE( *, '(  /5x, "Profile #", i2, ":", &
                &/10x, "n_Layers   = ", i5 )' ) m, Atmosphere(m)%n_Layers

    Marker = ' '
    IF ( Atmosphere(m)%n_Clouds > 0 ) Marker = ' <---<<<'
    WRITE( *, '( 10x, "n_Clouds   = ", i5, a )' ) Atmosphere(m)%n_Clouds, Marker

    Marker = ' '
    IF ( Atmosphere(m)%n_Aerosols > 0 ) Marker = ' <---<<<'
    WRITE( *, '( 10x, "n_Aerosols = ", i5, a )' ) Atmosphere(m)%n_Aerosols, Marker


    IF ( MOD( m, 10 ) == 0 ) THEN
      WRITE( *, '( /5x, "Press <ENTER> to continue..." )' )
      READ( *, * )
    END IF

  END DO



  !#----------------------------------------------------------------------------#
  !#                   -- WRITE THE NEW Atmosphere DATA --                      #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Write_Atmosphere_Binary( NEW_ATMOSPHERE_FILE, &
                                               Atmosphere )
  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error writing reading new Atmosphere datafile.', &
                          FAILURE )
    STOP
  END IF



  !#----------------------------------------------------------------------------#
  !#                     -- DESTROY THE STRUCTURE ARRAYS --                     #
  !#----------------------------------------------------------------------------#

  Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying new Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF

  Error_Status = Destroy_Atmosphere_old( Atmosphere_old )

  IF ( Error_Status /= SUCCESS ) THEN
    CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying old Atmosphere structure array.', &
                          Error_Status )
    STOP
  END IF

END PROGRAM Atmosphere_Rewrite


!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Atmosphere_Rewrite.f90,v 1.2 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.2 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Atmosphere_Rewrite.f90,v $
! Revision 1.2  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.1  2005/04/14 19:47:37  paulv
! Initial checkin.
!
!
!
!
