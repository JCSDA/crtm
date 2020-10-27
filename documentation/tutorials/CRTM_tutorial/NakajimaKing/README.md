# Short Description
This is a tutorial on how to use the CRTM `Forward` solver to compute a database for the bispectral retrieval of cloud optical thickness and particle effective radius for a `SNOW`-type cloud in the CRTM.
To this end, a two-dimensional mapping is created for the MODIS-Aqua instrument, channels 2 and 7.
# Code Description
## Walkthrough
First, calling CRTM procedures requires loading the CRTM module:
```Fortran
USE CRTM_Module
```
In the code, the database will be computed by creating 100 atmospheric profiles as objects of type `CRTM_Atmosphere_type` and calling the `Forward` model on each of these profiles:
```Fortran
! Profile dimensions...
INTEGER, PARAMETER :: N_PROFILES  = 100
...
TYPE(CRTM_Atmosphere_type) :: Atm(N_PROFILES)
```
For this test case, we will specify the shortwave channels of MODIS Aqua (`v.` for visible) as the sensor target:
```Fortran
CHARACTER(*), PARAMETER :: SENSOR_ID = 'v.modis_aqua'
```
Note that the corresponding `*.SpcCoeff.bin` and `*.TauCoeff.bin` files can be found in the `./coefficients/` folder.
As we are interested in channels 2 and 7 only, this subset can be selected with the following command:
```Fortran
! 2b. Select the MODIS channel subset
!     that is to be processed.
!     NOTE: Channel list is sorted
!     into ascending order for
!     processing.
! ----------------------------------
Error_Status = CRTM_ChannelInfo_Subset( ChannelInfo(1), &
                                        Channel_Subset = (/ 2, 7 /) )
```
The atmospheric data structure is allocated by calling `CRTM_Atmosphere_Create`:
```Fortran
! 3b. Allocate the STRUCTURES
! ---------------------------
! The input FORWARD structure
CALL CRTM_Atmosphere_Create( Atm, N_LAYERS, N_ABSORBERS, N_CLOUDS, N_AEROSOLS )
IF ( ANY(.NOT. CRTM_Atmosphere_Associated(Atm)) ) THEN
  Message = 'Error allocating CRTM Atmosphere structures'
  CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
  STOP
END IF
! ============================================================================
```
while the associated data is loaded in a subroutine at the end of the source code file:
```
! 4a. Atmosphere and Surface input
! --------------------------------
CALL Load_AtmSfc_Data()
```
The CRTM `Forward` model is called once for the entire atmospheric profile set `Atm`:
```Fortran
! ============================================================================
! 5. **** CALL THE CRTM FORWARD MODEL ****
!
Error_Status = CRTM_Forward( Atm        , &
                             Sfc        , &
                             Geometry   , &
                             ChannelInfo, &
                             RTSolution, &
                             Options = Opt )
IF ( Error_Status /= SUCCESS ) THEN
  Message = 'Error in CRTM Forward Model'
  CALL Display_Message( PROGRAM_NAME, Message, FAILURE )
  STOP
END IF
! ============================================================================
```
## Creating an Atmospheric Profile Input Data Set for the CRTM
As seen before, the `CRTM_Atmosphere_type` object `Atm` is declared in the `Load_AtmSfc_Data()` subroutine.
In this subroutine two loops go over all desired effective radius and cloud optical depth values:
```Fortran
SUBROUTINE Load_AtmSfc_Data()

  INTEGER, PARAMETER :: SEA_WATER_TYPE  = 1
  INTEGER :: mm, nn 

Effective_Rad_loop: DO mm = 0, 9
  Tau_loop: DO nn = 1, 10
  ...
  END DO Tau_loop
END DO Effective_Rad_loop
```
Inside these two loops, each atmospheric profile is assigned a single `SNOW` cloud scattering layer.
```
Atm(10*mm + nn)%Cloud(1)%Type = SNOW_CLOUD
```
Then for each snow cloud the associated particle effective radius and water content (and thus the related optical depth) are steadily increased.
```Fortran
Atm(10*mm + nn)%Cloud(1)%Effective_Radius(k1:k2) = &
    (/1.5_fp + mm*1.0_fp/)  ! microns
Atm(10*mm + nn)%Cloud(1)%Water_Content(k1:k2) = &
    (/0.06_fp + (nn-1)*0.01_fp/) ! kg/m^2
```
    

# Running the test case
Below is a walkthrough for all the steps of this tutorial case.

## Compilation
In order to compile the test, simply type 
```shell
make 
```
in this folder.

### If something goes wrong...
The `makefile` expects the CRTM installation to reside in `/usr/local/crtm_v2.3.0/`.
If this is not the case for installation, you need to modify the CRTM path in the makefile accordingly.
The `makefile` is written for the `gfortran`  compiler. If you are using a different compiler, please modify the `FCC` variable accordingly.

## Running the computations
If the compilation was successful, an executable called `NK.x` is created.
To run the computations, simply type:
```
./run.sh
```
You should see the following output in the terminal:
```Fortran
        .
        .
        .
      Profile 100 output for v.modis_aqua

    Channel 2 results
RTSolution OBJECT
  Sensor Id                     : v.modis_aqua
  WMO Satellite Id              : 784
  WMO Sensor Id                 : 389
  Channel                       : 2
  RT Algorithm Name             : ADA                 
  Scattering Optical Depth      :  2.772730E+02
  Surface Emissivity            :  0.000000E+00
  Surface Reflectivity          :  0.000000E+00
  Up Radiance                   :  0.000000E+00
  Down Radiance                 :  0.000000E+00
  Down Solar Radiance           :  0.000000E+00
  Surface Planck Radiance       :  0.000000E+00
  Total cloud cover             :  0.000000E+00
  Radiance (clear)              :  0.000000E+00
  Brightness Temperature (clear):  0.000000E+00
  Radiance                      :  5.875433E+00
  Brightness Temperature        :  1.119890E+03

    Channel 7 results
RTSolution OBJECT
  Sensor Id                     : v.modis_aqua
  WMO Satellite Id              : 784
  WMO Sensor Id                 : 389
  Channel                       : 7
  RT Algorithm Name             : ADA                 
  Scattering Optical Depth      :  1.338794E+02
  Surface Emissivity            :  0.000000E+00
  Surface Reflectivity          :  0.000000E+00
  Up Radiance                   :  0.000000E+00
  Down Radiance                 :  0.000000E+00
  Down Solar Radiance           :  0.000000E+00
  Surface Planck Radiance       :  0.000000E+00
  Total cloud cover             :  0.000000E+00
  Radiance (clear)              :  0.000000E+00
  Brightness Temperature (clear):  0.000000E+00
  Radiance                      :  1.283064E+00
  Brightness Temperature        :  4.937087E+02

    Destroying the CRTM...
The program has been successfully completed.
```

## Plotting the results
In order to plot the resuts, simply type:
```
python grid.py
```
This python script will read the `output.txt` output file and produce the following plot of the mapping between radiance space and cloud physical parameters:
![A Nakajima-King diagram.](NK_diagram.png)
