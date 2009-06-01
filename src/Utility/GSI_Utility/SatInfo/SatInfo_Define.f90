
!  SatInfo_Define module
!
!  Module defining the SatInfo data structure.
!       
!  Written by:     Paul van Delst, CIMSS/SSEC 18-Mar-2002
!                  paul.vandelst@ssec.wisc.edu
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

MODULE SatInfo_Define


  ! -----------------
  ! Environment setup
  ! -----------------
  ! Module use
  USE Type_Kinds, ONLY: fp=>fp_kind
  ! Disable implicit typing
  IMPLICIT NONE


  ! --------------------------
  ! Module entity visibilities
  ! --------------------------
  PRIVATE
  ! Derived type defintions
  PUBLIC :: SatInfo_type
  ! Module procedures
  PUBLIC :: SatInfo_CountSensors
  PUBLIC :: SatInfo_IndexChannels
  PUBLIC :: SatInfo_UniqueSensors


  ! -----------------
  ! Module parameters
  ! -----------------
  CHARACTER(*),  PARAMETER :: MODULE_RCS_ID = &
  '$Id$'

  INTEGER,  PARAMETER :: IP_INVALID = -999
  REAL(fp), PARAMETER :: FP_INVALID = -999.0_fp
  INTEGER,  PARAMETER :: SL = 20
                                                                                                                        

  ! ----------------------------
  ! SatInfo data type definition
  ! ----------------------------

  TYPE :: SatInfo_type
    CHARACTER(SL) :: Sensor_Id = ' '
    INTEGER       :: Channel    = IP_INVALID
    INTEGER       :: Use_Flag   = IP_INVALID
    REAL(fp)      :: Error      = FP_INVALID
    REAL(fp)      :: Error_Max  = FP_INVALID
    REAL(fp)      :: Var_b      = FP_INVALID
    REAL(fp)      :: Var_pg     = FP_INVALID
  END TYPE SatInfo_type


CONTAINS


  ! ------------------------------------------------
  ! Function to count sensors in a SatInfo structure
  ! ------------------------------------------------

  FUNCTION SatInfo_CountSensors(SatInfo,Idx) RESULT(nSensors)

    TYPE(SatInfo_type), DIMENSION(:), INTENT(IN)  :: SatInfo
    INTEGER,  OPTIONAL, DIMENSION(:), INTENT(OUT) :: Idx
    INTEGER :: nSensors
    INTEGER       :: i
    CHARACTER(SL) :: Id, Local_Id
    INTEGER,  DIMENSION(SIZE(SatInfo)) :: Local_Idx

    ! Initialisation
    nSensors  = 0
    Local_Id  = ' '
    Local_Idx = -1

    ! Loop over Sensor IDs
    DO i = 1, SIZE( SatInfo )
      Id = ADJUSTL(SatInfo(i)%Sensor_Id)
      IF ( LEN_TRIM(Id) > 0 ) THEN
        IF ( Local_Id /= Id ) THEN
          Local_Id  = Id
          Local_Idx = i
          nSensors = nSensors + 1
        END IF
      END IF
    END DO

    IF ( PRESENT(Idx) ) THEN
      Idx = -1
      IF ( SIZE(Idx) >= nSensors ) THEN
        Idx(1:nSensors) = Local_Idx(1:nSensors)
      END IF
    END IF

  END FUNCTION SatInfo_CountSensors


  ! ----------------------------------------------------------------
  ! Function to find sensor/channel locations in a SatInfo structure
  ! ----------------------------------------------------------------

  FUNCTION SatInfo_IndexChannels(Sensor_Id, Channel, SatInfo) RESULT(ChannelIndex)

    CHARACTER(*),                     INTENT(IN) :: Sensor_Id
    INTEGER,                          INTENT(IN) :: Channel
    TYPE(SatInfo_type), DIMENSION(:), INTENT(IN) :: SatInfo
    INTEGER :: ChannelIndex
    LOGICAL, DIMENSION(SIZE(SatInfo)) :: IndexMatch
    INTEGER :: n
    INTEGER :: Idx(1)

    ! Specify the logical match test
    IndexMatch = ( SatInfo%Sensor_Id == Sensor_Id .AND. &
                   SatInfo%Channel   == Channel         )

    ! Count the number of matches
    n = COUNT(IndexMatch)

    ! Get the matching index
    SELECT CASE (n)
      CASE (1)
        Idx = PACK( (/(n,n=1,SIZE(IndexMatch))/), IndexMatch )
        ChannelIndex = Idx(1)
      CASE DEFAULT
        ChannelIndex = 0
    END SELECT

  END FUNCTION SatInfo_IndexChannels


  ! ---------------------------------------------------------------------
  ! Subroutine to extract all the unique sensors from a SatInfo structure
  ! ---------------------------------------------------------------------

  SUBROUTINE SatInfo_UniqueSensors(SatInfo, Sensor_Id)

    TYPE(SatInfo_type), DIMENSION(:), INTENT(IN)  :: SatInfo
    CHARACTER(*),       DIMENSION(:), INTENT(OUT) :: Sensor_Id
    INTEGER :: i, n, nSensors, Id
    CHARACTER(SL) :: Local_Id
    INTEGER, DIMENSION(SIZE(SatInfo)) :: Idx

    ! Initialisation
    Sensor_Id = ' '

    ! Count sensors
    nSensors = SatInfo_CountSensors( SatInfo, Idx=Idx )

    ! Exit if output not large enough
    IF ( SIZE(Sensor_Id) < nSensors ) RETURN

    ! Assign the unique sensor ids
    Sensor_Id(1:nSensors) = SatInfo(Idx(1:nSensors))%Sensor_Id

  END SUBROUTINE SatInfo_UniqueSensors

END MODULE SatInfo_Define
