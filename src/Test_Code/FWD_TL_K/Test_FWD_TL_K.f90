!------------------------------------------------------------------------------
!P+
! NAME:
!       Test_CRTM
!
! PURPOSE:
!       Program can be used to test CRTM Forward and K-matrix code.
!       In this example, all mandatory parameters are inputed from a file (test_crtm.dat)
!       and user-specified sensor is required by terminal input, for example,
!       amsua_n16  for NOAA-16 AMSU-A 
!
! CATEGORY:
!       CRTM : Test 
!
! LANGUAGE:
!       Fortran-95
!
! MODULES:
!       Type_Kinds:     Module containing definitions for kinds
!                       of variable types.
!
!       Message_Handler:Module to define simple error codes and
!                       handle error conditions
!                       USEs: FILE_UTILITY module
!
!       CRTM_Module:    The main CRTM module.
!
!       CRTM_Atmosphere_Binary_IO: Module to read data into a CRTM_Atmosphere
!                                  structure/array
!
!       CRTM_Surface_Binary_IO:    Module to read data into a CRTM_Surface
!                                  structure/array
!
! CONTAINS:
!       Print_ChannelInfo:  Subroutine to print the contents of the
!                           ChannelInfo structure returned from the
!                           initialisation.
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
!
!
! CREATION HISTORY:
!       Written by:     Paul van Delst, CIMSS/SSEC 30-Jun-2004
!                       paul.vandelst@ssec.wisc.edu
!       modified by:    Yong Han, NOAA/NESDIS/ORA & JCSDA 
!                       Yong.Han@noaa.gov
!       modified by:    Quanhua Liu, QSS at JCSDA
!                       Quanhua.Liu@noaa.gov
!
!  Copyright (C) 2004 Paul van Delst, Yong Han, and Quanhua Liu
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

PROGRAM Test_CRTM


  ! ------------
  ! Module usage
  ! ------------
  USE CRTM_SpcCoeff

  ! -- Utility modules
  USE Type_Kinds
  USE Message_Handler

  ! -- CRTM module
  USE CRTM_Module
  USE CRTM_Atmosphere_Define

  ! -- Modules to read in Atmosphere and Surface data
  USE CRTM_Atmosphere_Binary_IO
  USE CRTM_Surface_Binary_IO

  ! ---------------------------
  ! Disable all implicit typing
  ! ---------------------------

  IMPLICIT NONE


  ! ----------
  ! Parameters
  ! ----------

  CHARACTER( * ), PARAMETER :: PROGRAM_NAME   = 'Test_CRTM'
  CHARACTER( * ), PARAMETER :: PROGRAM_RCS_ID = &
    '$Id: Test_FWD_TL_K.f90,v 1.3 2006/05/02 14:58:34 dgroff Exp $'
  CHARACTER( * ),  PARAMETER :: PROGRAM_HEADER = &
  '**********************************************************'

  ! ---------
  ! Variables
  ! ---------
  INTEGER :: Error_Status, L, i, j, k, n, cloud_layer_idx, LL
  INTEGER :: Allocate_Status

  CHARACTER( 256 ) :: File_Prefix
  CHARACTER( 256 ) :: SpcCoeff_File
  CHARACTER( 256 ) :: TauCoeff_File
  CHARACTER( 256 ) :: AerosolCoeff_File
  CHARACTER( 256 ) :: CloudCoeff_File
  CHARACTER( 256 ) :: EmisCoeff_File 

  REAL( fp_kind ), ALLOCATABLE, DIMENSION( : ) :: BT_fd
  REAL( fp_kind ) :: delta, Temporal
  TYPE( CRTM_ChannelInfo_type )  :: ChannelInfo

  TYPE( CRTM_Atmosphere_type ) :: Atmosphere, Atmosphere_AD,Atmosphere_TL
  TYPE( CRTM_Surface_type ) :: Surface, Surface_TL, Surface_AD
  TYPE( CRTM_GeometryInfo_type ) :: GeometryInfo

  !  Channel dependent forward and K-matrix 
  TYPE( CRTM_Atmosphere_type ), ALLOCATABLE, DIMENSION( : ) :: Atmosphere_K
  TYPE( CRTM_Surface_type ),    ALLOCATABLE, DIMENSION( : ) :: Surface_K

  TYPE( CRTM_RTSolution_type ), ALLOCATABLE, DIMENSION( : ) :: RTSolution, RTSolution_TL
  TYPE( CRTM_RTSolution_type ), ALLOCATABLE, DIMENSION( : ) :: RTSolution_K

    ! -- Internal variable input
    TYPE( CRTM_Options_type ) :: Options
  
  CHARACTER(128), DIMENSION(300) :: Sensor_Descriptors
  INTEGER       , DIMENSION(300) :: Sensor_Channels


  !#----------------------------------------------------------------------------#
  !#         -- READ THE Atmosphere AND Surface STRUCTURE DATA FILES --         #
  !#----------------------------------------------------------------------------#
     OPEN(68,file='crtm.out',status='unknown')
!     OPEN(61,file='crtm_clear_input.dat',status='old')
     OPEN(61,file='crtm_cloud_input.dat',status='old')

     ! 
     READ(61,*) Atmosphere%n_Layers,Atmosphere%n_Absorbers, &
      Atmosphere%n_Clouds,Atmosphere%n_Aerosols

  !#----------------------------------------------------------------------------#
  !#         -- Allocate arrays in Atmosphere structure --                      #
  !#----------------------------------------------------------------------------#
      Error_Status = CRTM_Allocate_Atmosphere( Atmosphere%n_Layers, &
                                             Atmosphere%n_Absorbers, &
                                             Atmosphere%n_Clouds, &
                                             Atmosphere%n_Aerosols, &
                                             Atmosphere)

      READ(61,*) Atmosphere%Climatology
      READ(61,*) Atmosphere%Absorber_ID(1), Atmosphere%Absorber_ID(2)
      READ(61,'(A,A)') Atmosphere%Absorber_Units(1),Atmosphere%Absorber_Units(2)
!!      Atmosphere%Level_Temperature_Input = 1

      READ(61,*) GeometryInfo%Sensor_Zenith_Angle,GeometryInfo%Sensor_Scan_Angle,GeometryInfo%Source_Zenith_Angle

      READ(61,*) Atmosphere%Level_Temperature(0), Atmosphere%Level_Pressure(0)

      READ(61,*) 

      ! Atmosphere%Absorber(*,1) : water vapor (mass mixing ratio g/kg); Atmosphere%Absorber(*,2) : O3 (ppmv)
      DO i = 1, Atmosphere%n_Layers
        READ(61,'(I5,4f9.3,4e15.5)') j,Atmosphere%Level_Pressure(i),Atmosphere%Pressure(i),Atmosphere%Temperature(i), &
        Atmosphere%Level_Temperature(i),(Atmosphere%Absorber(i,j),j=1,Atmosphere%n_Absorbers)
      ENDDO

     ! read surface data

      READ(61,*) 
      READ(61,*) Surface%Land_Coverage,Surface%Water_Coverage,Surface%Snow_Coverage,Surface%Ice_Coverage

      READ(61,'(f10.4)') Surface%Wind_Speed
      READ(61,'(I5,5f10.4)') Surface%Land_Type, &
                             Surface%Land_Temperature, &
                             Surface%Soil_Moisture_Content, &
                             Surface%Canopy_Water_Content , &
                             Surface%Vegetation_Fraction, &
                             Surface%Soil_Temperature

      READ(61,'(I5,3f10.4)') Surface%Water_Type, &
                                       Surface%Water_Temperature, &
                                       Surface%Wind_Direction, &
                                       Surface%Salinity

      READ(61,'(I5,4f10.4)')  Surface%Snow_Type, &
                                       Surface%Snow_Temperature, &
                                       Surface%Snow_Depth, &
                                       Surface%Snow_Density, &
                                       Surface%Snow_Grain_Size

      READ(61,'(I5,4f10.4)')  Surface%Ice_Type, &
                                       Surface%Ice_Temperature, &
                                       Surface%Ice_Thickness, &
                                       Surface%Ice_Density, &
                                       Surface%Ice_Roughness
     !
     !  in case clouds present
     IF( Atmosphere%n_Clouds > 0 ) THEN

       READ(61,*)

      !  i : cloud index,  cloud type, cloud_layer_idx
       READ(61,*) i, Atmosphere%cloud(i)%Type,cloud_layer_idx
       READ(61,*) Atmosphere%Cloud(i)%Effective_Radius(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Effective_Variance(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Water_Content(cloud_layer_idx)

      !  i : cloud index,  cloud type, cloud_layer_idx
       READ(61,*) i, Atmosphere%cloud(i)%Type,cloud_layer_idx
       READ(61,*) Atmosphere%Cloud(i)%Effective_Radius(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Effective_Variance(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Water_Content(cloud_layer_idx)

      !  i : cloud index,  cloud type, cloud_layer_idx
       READ(61,*) i, Atmosphere%cloud(i)%Type,cloud_layer_idx
       READ(61,*) Atmosphere%Cloud(i)%Effective_Radius(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Effective_Variance(cloud_layer_idx), &
          Atmosphere%Cloud(i)%Water_Content(cloud_layer_idx)

     ENDIF

     close(61)
     !#----------------------------------------------------------------------------#
     !#  -- END of READ THE Atmosphere AND Surface STRUCTURE DATA FILES --         #
     !#----------------------------------------------------------------------------#



     !#----------------------------------------------------------------------------#
     !#                     -- GET THE COEFFICIENT FILENAMES --                    #
     !#----------------------------------------------------------------------------#

     ! ------------------------------------------------
     ! Enter the instrument file prefix, e.g. amsua_n16
     ! ------------------------------------------------

     WRITE( *, FMT     = '( /5x, "Enter the instrument file prefix : " )', &
                ADVANCE = 'NO' )
     READ( *, '( a )' ) File_Prefix
     File_Prefix = ADJUSTL( File_Prefix )


     ! --------------------
     ! Create the filenames
     ! --------------------

     SpcCoeff_File = TRIM( File_Prefix )//'.Sensor.SpcCoeff.bin.Big_Endian'
     TauCoeff_File = TRIM( File_Prefix )//'.TauCoeff.bin.Big_Endian'
     AerosolCoeff_File = 'dummy.AerosolCoeff.bin.Big_Endian'
     CloudCoeff_File = 'Cloud_scatter_IR_MW.bin.Big_Endian'
     EmisCoeff_File    = 'EmisCoeff.bin.Big_Endian'

     !#----------------------------------------------------------------------------#
     !#                          -- INITIALISE THE CRTM --                         #
     !#----------------------------------------------------------------------------#

     WRITE( *, '( /5x, "Initializing the CRTM..." )' )

     Error_Status = CRTM_Init( ChannelInfo, &
                              SpcCoeff_File     = SpcCoeff_File, &
                              TauCoeff_File     = TauCoeff_File, &
                              AerosolCoeff_File = AerosolCoeff_File, &
                              EmisCoeff_File    = EmisCoeff_File, &
                              CloudCoeff_File = CloudCoeff_File )

     IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                           'Error initializing CRTM', & 
                            Error_Status)  
       STOP
     END IF

     IF( trim(File_Prefix) == 'amsuab_n16' ) THEN
       ! User may select a single sensor, for example, amsua_n16. 
       Error_Status = CRTM_Set_ChannelInfo("amsua_n16", ChannelInfo)
       ! User may choose channles from the selected sensor, for example, channels 1, 3, 6, 15.
       !  Sensor_Descriptors(1:4) = "amsua_n16"
       !  Sensor_Channels(1:4) = (/1,3,6,15/)
       !  Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptors(1:4), Sensor_Channels(1:4), ChannelInfo)

       IF ( Error_Status /= SUCCESS ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                           'Error in indexing ChannelInfo', & 
                            Error_Status)  
         STOP
       END IF
     ENDIF
       ! User may select a single sensor, for example, amsua_n16. 
       Error_Status = CRTM_Set_ChannelInfo("amsua_n16", ChannelInfo)
       ! User may choose channles from the selected sensor, for example, channels 1, 3, 6, 15.
         Sensor_Descriptors(1:1) = "amsua_n16"
         Sensor_Channels(1:1) = (/1/)
         Error_Status = CRTM_Set_ChannelInfo(Sensor_Descriptors(1:1), Sensor_Channels(1:1), ChannelInfo)

     !#----------------------------------------------------------------------------#
     !#    -- ALLOCATE THE OUTPUT RTSolution ARRAY TO THE NUMBER OF CHANNELS --    #
     !#----------------------------------------------------------------------------#

       ALLOCATE( RTSolution( ChannelInfo%n_Channels ), RTSolution_TL( ChannelInfo%n_Channels ), &
            RTSolution_K( ChannelInfo%n_Channels ),STAT = Allocate_Status )

       IF ( Allocate_Status /= 0 ) THEN 
         CALL Display_Message( PROGRAM_NAME, &
                           'Error allocating RTSolution structure array', & 
                            Error_Status)  
         STOP
       END IF

       ALLOCATE( Atmosphere_K( ChannelInfo%n_Channels ), &
            Surface_K(    ChannelInfo%n_Channels ), STAT = Allocate_Status )
     
       Error_Status = CRTM_Allocate_Atmosphere( Atmosphere%n_Layers, &
                                               Atmosphere%n_Absorbers, &
                                               Atmosphere%n_Clouds, &
                                               Atmosphere%n_Aerosols, &
                                               Atmosphere_K )
                                       
       IF ( Error_Status /= SUCCESS ) THEN
         CALL Display_Message( PROGRAM_NAME, &
                              'Error allocating Atmosphere_K structure.', &
                              Error_Status )
         STOP
       END IF
                                 
       Error_Status = CRTM_Allocate_Atmosphere( Atmosphere%n_Layers, &
                                           Atmosphere%n_Absorbers, &
                                           Atmosphere%n_Clouds, &
                                           Atmosphere%n_Aerosols, &
                                           Atmosphere_TL)
 
     !#----------------------------------------------------------------------------#
     !#                          -- PRINT OUT SOME INFO --                         #
     !#----------------------------------------------------------------------------#

     CALL Print_ChannelInfo( ChannelInfo )

     !#----------------------------------------------------------------------------#
     !#                         -- CALL THE FORWARD MODEL --                       #
     !#----------------------------------------------------------------------------#

     WRITE( *, '( /5x, "Calling the Forward CRTM..." )' )
       print *,' statrt CRTM_Forward '

     Error_Status = CRTM_Forward( Atmosphere, &  ! Input
                                     Surface, &  ! Input
                                GeometryInfo, &  ! Input
                                 ChannelInfo, &  ! Input
!                                     Options, &  ! Optional input, Scalar
                                 RTSolution )    ! Output

     print *,' after ',Error_Status
     IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                           'Error in CRTM Forward Model', & 
                            Error_Status)  
       STOP
     END IF

     write(68,501)
 501 FORMAT(8X,' wavenumber    Frequency       Brightness Temperature ')

   ! test
   !  ChannelInfo%n_Channels = 1

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(6,'(I5,3f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature
       write(68,'(I5,3f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature
     ENDDO  


     IF( Atmosphere%n_Clouds > 0 ) THEN
              ! -- Call the tangent-linear model                                                                                                            
     DO k = 1, Atmosphere%n_Layers
     write(68,'(I10)') k
     Atmosphere_TL%Temperature(k) = 1.0
     
     Error_Status = CRTM_Tangent_Linear( Atmosphere, &
                                            Surface, &                                     
                                      Atmosphere_TL, &                                   
                                         Surface_TL, &                                
                                       GeometryInfo, &                                     
                                        ChannelInfo, &                                    
                                         RTSolution, &                                      
                                        RTSolution_TL ) 

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(68,'(I5,4f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature,RTSolution_TL(i)%Brightness_Temperature
     ENDDO  

     Atmosphere_TL%Temperature(k) = 0.0
     
   ENDDO

     DO k = 1, Atmosphere%n_Layers
     write(68,'(I10)') k
     Atmosphere_TL%Absorber(k,1) = 1.0
     
     Error_Status = CRTM_Tangent_Linear( Atmosphere, &
                                            Surface, &                                     
                                      Atmosphere_TL, &                                   
                                         Surface_TL, &                                
                                       GeometryInfo, &                                     
                                        ChannelInfo, &                                    
                                         RTSolution, &                                      
                                        RTSolution_TL ) 

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(68,'(I5,4f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature,RTSolution_TL(i)%Brightness_Temperature
     ENDDO  

     Atmosphere_TL%Absorber(k,1) = 0.0
     
   ENDDO

              ! -- Call the tangent-linear model                                                                                                            
     DO k = 1, Atmosphere%n_Layers
     write(68,'(I10)') k
     Atmosphere_TL%Cloud(3)%Effective_Radius(k) = 1.0
     
     Error_Status = CRTM_Tangent_Linear( Atmosphere, &
                                            Surface, &                                     
                                      Atmosphere_TL, &                                   
                                         Surface_TL, &                                
                                       GeometryInfo, &                                     
                                        ChannelInfo, &                                    
                                         RTSolution, &                                      
                                        RTSolution_TL ) 

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(68,'(I5,4f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature,RTSolution_TL(i)%Brightness_Temperature
     ENDDO  

     Atmosphere_TL%Cloud(3)%Effective_Radius(k) = 0.0
     
   ENDDO

     DO k = 1, Atmosphere%n_Layers
     write(68,'(I10)') k
     Atmosphere_TL%Cloud(3)%Water_Content(k) = 1.0
     
     Error_Status = CRTM_Tangent_Linear( Atmosphere, &
                                            Surface, &                                     
                                      Atmosphere_TL, &                                   
                                         Surface_TL, &                                
                                       GeometryInfo, &                                     
                                        ChannelInfo, &                                    
                                         RTSolution, &                                      
                                        RTSolution_TL ) 

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(68,'(I5,4f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature,RTSolution_TL(i)%Brightness_Temperature
     ENDDO  

     Atmosphere_TL%Cloud(3)%Water_Content(k) = 0.0
     
   ENDDO

 ENDIF
     !#----------------------------------------------------------------------------#
     !#                         -- CALL K-Matrix MODEL --                          #
     !#----------------------------------------------------------------------------#

     ! Request for brightness temperature jacobian 
     DO l = 1, ChannelInfo%n_channels
       RTSolution_K(l)%Radiance = 0.0_fp_kind
       RTSolution_K(l)%Brightness_Temperature = 1.0_fp_kind
     ENDDO

     ! CALL K-Matrix Model
     Error_Status = CRTM_K_Matrix(  Atmosphere,                 &  ! Input
                                     Surface,                   &  ! Input
                                     RTSolution_K,              &  ! Input
                                     GeometryInfo,              &  ! Input
                                     ChannelInfo,               &  ! Input
                                     Atmosphere_K,              &  ! Output
                                     Surface_K,                 &  ! Output
                                     RTSolution )

     IF ( Error_Status /= SUCCESS ) THEN 
       CALL Display_Message( PROGRAM_NAME, &
                           'Error in CRTM K_Matrix Model', & 
                            Error_Status)  
       STOP
     END IF

     DO i = 1, ChannelInfo%n_Channels
       l = ChannelInfo%Channel_Index(i)
       write(6,'(I5,3f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature
       write(68,'(I5,3f15.8)') l,SC%Wavenumber(l),SC%Frequency(l),RTSolution(i)%Brightness_Temperature
     ENDDO  

    ! OUTPUT jacobian for temperature and water vapor
     write(68,502)
 502 FORMAT(8X,' Pressure       T_K            H2O_K ')
    DO l = 1, ChannelInfo%n_Channels
      write(6,'(A,I10)') ' channel index = ',l
      write(68,'(A,I10)') ' channel index = ',l
      DO k = 1, Atmosphere%n_Layers
      write(6,'(I5,4E15.6)') k,Atmosphere%Pressure(k),Atmosphere_K(l)%Temperature(k), &
        Atmosphere_K(l)%Absorber(k,1)

     IF( Atmosphere%n_Clouds > 0 ) THEN

      write(68,'(I5,5E15.7)') k,Atmosphere%Pressure(k),Atmosphere_K(l)%Temperature(k), &
        Atmosphere_K(l)%Absorber(k,1), &
        Atmosphere_K(l)%Cloud(3)%Water_Content(k), &
        Atmosphere_K(l)%Cloud(3)%Effective_Radius(k)

     ELSE
      write(68,'(I5,5E15.7)') k,Atmosphere%Pressure(k),Atmosphere_K(l)%Temperature(k), &
        Atmosphere_K(l)%Absorber(k,1) 


     ENDIF


      ENDDO
    ENDDO

    CLOSE(68)
    !#----------------------------------------------------------------------------#
    !#                           -- DESTROY THE CRTM --                           #
    !#----------------------------------------------------------------------------#

    WRITE( *, '( /5x, "Destroying the CRTM..." )' )


    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere )
    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere structure', & 
                           Error_Status )
      STOP
    END IF

    Error_Status = CRTM_Destroy_Atmosphere( Atmosphere_K )

    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying Atmosphere_K structure ', & 
                           Error_Status )
      STOP
    END IF

    ! --------------------------
    ! The local array
    ! --------------------------

    DEALLOCATE( RTSolution, RTSolution_K, Atmosphere_K, Surface_K, &
               STAT = Allocate_Status )

    IF ( Allocate_Status /= 0 ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                           'Error deallocating structure array', & 
                            Error_Status)  
    END IF


    ! ---------------
    ! The entire CRTM
    ! ---------------

    Error_Status = CRTM_Destroy( ChannelInfo )

    IF ( Error_Status /= SUCCESS ) THEN 
      CALL Display_Message( PROGRAM_NAME, &
                          'Error destroying CRTM', & 
                           Error_Status )
      STOP
    END IF


CONTAINS


  SUBROUTINE Print_ChannelInfo( ChannelInfo )
    TYPE( CRTM_ChannelInfo_type ), INTENT( IN ) :: ChannelInfo
    INTEGER :: l

    WRITE( *, '( /5x, "Number of channels indexed: ", i5 )' ) ChannelInfo%n_Channels
    WRITE( *, '(  /2x, "Channel         Sensor             NCEP          WMO           WMO     Channel", &
                 &/2x, " Index        Descriptor         Sensor ID   Satellite ID   Sensor ID   Number", &
                 &/2x, "------------------------------------------------------------------------------" )' )
    DO l = 1, ChannelInfo%n_Channels
      WRITE( *, '( 2x, 2x, i4, 2x, ">", a, "<", 5x, i3, 11x, i3, 11x, i3, 7x, i4 )' ) &
                ChannelInfo%Channel_Index( l ), &
                ChannelInfo%Sensor_Descriptor( l ), &
                ChannelInfo%NCEP_Sensor_ID( l ), &
                ChannelInfo%WMO_Satellite_ID( l ), &
                ChannelInfo%WMO_Sensor_ID( l ), &
                ChannelInfo%Sensor_Channel( l )
    END DO
 
  END SUBROUTINE Print_ChannelInfo

END PROGRAM Test_CRTM

!

!-------------------------------------------------------------------------------
!                          -- MODIFICATION HISTORY --
!-------------------------------------------------------------------------------
!
! $Id: Test_FWD_TL_K.f90,v 1.3 2006/05/02 14:58:34 dgroff Exp $
!
! $Date: 2006/05/02 14:58:34 $
!
! $Revision: 1.3 $
!
! $Name:  $
!
! $State: Exp $
!
! $Log: Test_FWD_TL_K.f90,v $
! Revision 1.3  2006/05/02 14:58:34  dgroff
! - Replaced all references of Error_Handler with Message_Handler
!
! Revision 1.2  2006/04/24 18:05:42  wd20pd
! - Merged CRTM_Sensor branch onto main trunk.
!
! Revision 1.1.2.1  2005/10/12 18:26:45  qliu
! - Initial checkin. Based on originl Test_Forward code and makefile.
!
! Revision 1.9  2005/02/16 22:11:50  paulv
! - Updated to use new Aerosol modules.
!
! Revision 1.8  2005/02/03 20:44:26  paulv
! - Changed names of atmosphere and surface input data files.
!
! Revision 1.7  2005/01/28 21:02:02  paulv
! - Simplified the method to determine the coefficient filenames.
!
! Revision 1.6  2004/11/05 16:40:40  paulv
! - Upgraded to Fortran-95.
! - Removed all Init() calls.
!
! Revision 1.5  2004/08/06 20:45:10  paulv
! - Changed RTSolution to an allocatable array and allocate it after the
!   model initialization.
!
! Revision 1.4  2004/08/06 19:26:30  paulv
! - Added Surface binary I/O.
!
! Revision 1.3  2004/07/21 15:56:14  paulv
! - Added definition of view angles to the GeometryInfo structure.
!
! Revision 1.2  2004/07/02 21:13:04  paulv
! - Added call to read function to read Atmosphere data file.
!
! Revision 1.1  2004/07/01 20:50:01  paulv
! Initial checkin.
!
!
!
!
