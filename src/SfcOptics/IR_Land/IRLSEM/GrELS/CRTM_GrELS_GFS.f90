!
! MODULE NAME: 
!      CRTM_GrELS_GFS
!
!         Greenness-adjusted Emissivity for Land Surface (GrELS)
!         using the GFS Vegetation Types (GrELS-GFS)
!
! PURPOSE:
!      Module to compute infrared land surface emissivity.
!
!      The emissivity is adjusted by green vegetation fraction (GVF)
!      to account for temporal change in emissivity.
!
!      The reflectances (from which emissivity is calculated) are 
!      derived from the JPL Spectral Library and designed for 
!      the 13-category vegetation types used in the Global Forecast 
!      System (GFS)
!
! CREATION HISTORY:
!       Written by:    Ron Vogel, IMSG
!                      ronald.vogel@noaa.gov
!                      August 21, 2009
!

MODULE CRTM_GrELS_GFS

  ! Module use
  USE Type_Kinds,               ONLY: fp
!use input structures in non-test version
!  USE CRTM_Surface_Define,      ONLY: CRTM_Surface_type
!  USE CRTM_GeometryInfo_Define, ONLY: CRTM_GeometryInfo_type
  USE CRTM_Interpolation,       ONLY: NPTS, &
                                      Find_Index, &
                                      LPoly, &
                                      LPoly_type, &
                                      Interp_1D

  ! Disable implicit typing
  IMPLICIT NONE

  ! Visibilities
  PRIVATE

  PUBLIC :: open_grels_refl_table
  PUBLIC :: open_gvf_climatol_table
  PUBLIC :: get_gvf_climatol
  PUBLIC :: compute_grels_emis

  ! Module parameters
  INTEGER, PARAMETER :: N_REFL_PTS = 1361
  INTEGER, PARAMETER :: N_SFC_CLASSES = 13
  INTEGER, PARAMETER :: N_SOIL_TYPES = 1
  INTEGER, PARAMETER :: N_TABLE_COLS = N_SFC_CLASSES + N_SOIL_TYPES +1
  REAL :: grels_refl_table( N_TABLE_COLS, N_REFL_PTS )
  REAL :: gvf_climatol_table( 52, 72, 13 )

  ! --------------------------------------
  ! Structure definition to hold forward
  ! variables across FWD, TL, and AD calls
  ! --------------------------------------
  ! The interpolation routine structure
  TYPE :: Refl_interp_type
    ! The interpolating polynomial
    TYPE(LPoly_type) :: wlp  ! polynomial results from LPoly
    ! The LUT interpolation indices
    INTEGER :: i1, i2        ! wavelength indices at which polynomial is calc
    ! The LUT interpolation boundary check
    LOGICAL :: w_outbound    ! logical true if w_int is beyond wavelength range
    ! The interpolation input
    REAL(fp) :: w_int        ! the wavelength at which refl is 2be interpolated
    ! The data to be interpolated
    REAL(fp) :: w(NPTS)      ! the 4 wavelength points for polynomial calc
    ! The interpolation results
    REAL(fp) :: rveg_int    ! interpolated veg refl for computing emissivity
    REAL(fp) :: rsoil_int   ! interpolated soil refl for computing emissivity
  END TYPE Refl_interp_type


  CONTAINS

  !----------------------------------------------------------
  !
  ! SUBROUTINE NAME:
  !       open_grels_refl_table
  !
  ! PURPOSE:
  !       opens file of GrELS reflectances (for the 13 GFS vegetation types)
  !       so the file only needs to be opened once rather than for each CRTM
  !       simulation.
  !
  ! CALLING SEQUENCE:
  !       CALL open_grels_refl_table ( path, file )
  !
  ! INPUT ARGUMENTS:
  !
  !       path:            Directory path to file containing GrELS
  !                        reflectances
  !                        UNITS:      none
  !                        TYPE:       CHARACTER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       file:            Filename of GrELS reflectance table
  !                        UNITS:      none
  !                        TYPE:       CHARACTER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  ! OUTPUT ARGUMENTS:
  !       None ( grels_refl_table is a global variable ).
  !
  ! COMMENTS:
  !   Structure of Table:
  !     GFS Vegetation Types are arranged by column.
  !     Wavelengths are in column 1.
  !     Table entries are surface reflectance.
  !     Surface reflectance is in range 0.0 - 1.0
  !   columns:
  !     column 1: wavelength (units in micrometer)
  !     column 2: broadleaf evergreen tree (tropical forest)
  !     column 3: broadleaf deciduous tree (mid-latitude forest)
  !     column 4: broadleaf & needleleaf tree (mixed forest)
  !     column 5: needleleaf evergreen tree (high-latitude pine forest)
  !     column 6: needleleaf deciduous tree (high-lat pine forest, Siberia only)
  !     column 7: broadleaf tree with groundcover (savanna)
  !     column 8: groundcover (grassland)
  !     column 9: broadleaf shrub with perennial groundcover (desert, drygrassl)
  !     column 10: broadleaf shrub with bare soil (desert)
  !     column 11: dwarf tree and shrub with groundcover (tundra)
  !     column 12: bare soil (desert)
  !     column 13: cultivations (agricultural land)
  !     column 14: glacial-ice (Greenland/Antarctica ice sheet only)
  !     column 15: soil reflectance (not a surface type!  This is used 
  !                           in seasonal GVF adjustment to emissivity)
  !
  !-----------------------------------------------------------

  SUBROUTINE open_grels_refl_table ( path, file )

      CHARACTER(*), INTENT(IN) :: path
      CHARACTER(*), INTENT(IN) :: file

      OPEN ( unit=17, file=TRIM(path)//TRIM(file), access='direct', &
             form='unformatted', status='old', &
             recl=4*N_TABLE_COLS*N_REFL_PTS )

      READ ( 17, rec=1 ) grels_refl_table

      CLOSE (17)

  END SUBROUTINE open_grels_refl_table


  !----------------------------------------------------------
  !
  ! SUBROUTINE NAME:
  !       open_gvf_climatol_table
  !
  ! PURPOSE:
  !       opens file of Green Vegetation Fraction climatology
  !       which is used in determining seasonal emissivity
  !       (these GVF values are averaged spatially for surface types
  !        and latitude bands)
  !
  ! CALLING SEQUENCE:
  !       CALL open_gvf_climatol_table ( path, file )
  !
  ! INPUT ARGUMENTS:
  !
  !       path:            Directory path to file containing GVF climatology
  !                        UNITS:      none
  !                        TYPE:       CHARACTER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       file:            Filename of GVF climatology
  !                        UNITS:      none
  !                        TYPE:       CHARACTER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  ! OUTPUT ARGUMENTS:
  !       None ( gvf_climatol_table is a global variable ).
  !
  ! COMMENTS:
  !     Structure of Table:
  !     3 dimensions:  52  x       72      x     13
  !                   week x latitude band x surface type
  !         52 weeks (52 weekly averages from 25 years of AVHRR GVF)
  !         72 latitude bands in 2.5 deg intervals from -90 to +90
  !         13 surface types (GFS Vegetation Type classification scheme)
  !     Each table value is a REAL value of climatological weekly GVF
  !     averaged over 2.5deg-interval latitude band and surface type.
  !     If a surface type does not exist in a latitude band, a missing value
  !     is given.
  !     - valid GVF values are 0.1 - 1.0
  !     - missing value is -999.9
  !
  !-----------------------------------------------------------

  SUBROUTINE open_gvf_climatol_table ( path, file )

      CHARACTER(*), INTENT(IN) :: path
      CHARACTER(*), INTENT(IN) :: file

      OPEN ( unit=17, file=TRIM(path)//TRIM(file), access='direct', &
             form='unformatted', status='old', &
             recl=4*52*72*13 )

      READ ( 17, rec=1 ) gvf_climatol_table

      CLOSE (17)

  END SUBROUTINE open_gvf_climatol_table


  !----------------------------------------------------------
  !
  ! SUBROUTINE NAME:
  !       get_gvf_climatol
  !
  ! PURPOSE:
  !       retrieves green vegetation fraction (GVF) from the
  !       GFV Climatology look-up table (25-year climatology of AVHRR GVF)
  !
  ! CALLING SEQUENCE:
  !       CALL get_gvf_climatol ( dayofyear, year, lat, gfsclass, gvf )
  !
  ! INPUT ARGUMENTS:
  !
  !       dayofyear:       Day of year for which emissivity is calculated
  !                        (uses week in which the day falls)
  !                        UNITS:      day-of-year (1-365 or 1-366)
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       year:            Year in which the day falls for which emissivity
  !                        is calculated
  !                        UNITS:      4-digit year
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       lat:             Latitude for which emissivity is calculated
  !                        (because the GVF varies by week and latitude)
  !                        UNITS:      degrees (-90 to +90)
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       gfsclass:        GFS vegetation type (integer 1-13)
  !                        UNITS:      none (category)
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  ! OUTPUT ARGUMENTS:
  !
  !       gvf:             Green Vegetation Fraction (GVF) obtained from the
  !                        GVF Climatology look-up table
  !                        UNITS:      none (fraction 0-1)
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(OUT)
  !       IMPORTANT: if a GFS vegetation type does not exist at a particular
  !                  latitude, then GVF is given a missing value of -999.9
  !
  !
  ! COMMENTS:
  !       GVF Climatology look-up table is derived from Jiang et al., 2008,
  !       IEEE Transactions on Geoscience & Remote Sensing 46(2), pp. 409-422.
  !       (25-year GVF weekly climatology from AVHRR)
  !
  !-----------------------------------------------------------

  SUBROUTINE get_gvf_climatol ( dayofyear, year, lat, gfsclass, gvf )

      INTEGER,  INTENT(IN)  :: dayofyear
      INTEGER,  INTENT(IN)  :: year
      REAL(fp), INTENT(IN)  :: lat
      INTEGER,  INTENT(IN)  :: gfsclass
      REAL(fp), INTENT(OUT) :: gvf

      INTEGER  :: iweek, ilat, j
      REAL(fp) :: week_num, lat1, lat2


      ! calculate the week in which the date falls
      ! -- weeks contain 7 days, starting Jan 1 each year
      ! -- in leap years, week 9 (Feb 26 - Mar 4) has 8 days (i.e. Feb 29)
      ! -- in non-leap years, week 9 has 7 days
      ! -- week 52 (Dec 24-31) has 8 days in both leap years and non-leap years

      IF ( MOD ( REAL(year) , 4.0 ) .EQ. 0.0 ) THEN   ! Leap Year case
          IF ( dayofyear .LT. 60 ) THEN           ! days before Feb 29
             week_num = MOD ( REAL(dayofyear-1), 7.0 ) + 1.0
          ELSE                                    ! days after Feb 29 inclusive
             week_num = MOD ( REAL(dayofyear-2), 7.0 ) + 1.0
          END IF
          IF (dayofyear .EQ. 366) week_num = 52.0  ! last week holds 8 days
      ELSE                                         ! Non-Leap Year case
          week_num = MOD ( REAL(dayofyear-1), 7.0 ) + 1.0
          IF (dayofyear .EQ. 365) week_num = 52.0  ! last week holds 8 days
      END IF

      iweek = INT(week_num)


      ! calculate 2.5 deg latitude band from input latitude

      Latitude_Search: DO j = 1,72
         lat1 = -90.0_fp + ( REAL(j-1) * 2.5_fp )
         lat2 = -90.0_fp + ( REAL(j) * 2.5_fp )
         IF ((lat .GE. lat1) .AND. (lat .LT. lat2)) THEN
              ilat=j
              EXIT Latitude_Search
         END IF
      END DO Latitude_Search
      IF (lat .EQ. 90.0_fp) ilat=72


      ! get GVF
      gvf = gvf_climatol_table(iweek,ilat,gfsclass)  ! gvf_climatol_table
                                                     ! contains -999.9
                                                     ! as missing value

  END SUBROUTINE get_gvf_climatol


  !----------------------------------------------------------
  !
  ! SUBROUTINE NAME:
  !       compute_grels_emis
  !
  ! PURPOSE:
  !       computes greenness-adjusted emissivity for land surface (GrELS)
  !       from GrELS reflectance table given the desired wavelength, one of 
  !       the 13 GFS vegetation types, and green vegetation fraction.
  !
  !       The green vegetation fraction seasonally adjusts the emissivity.
  !
  !       Green vegetation fraction (0.0-1.0 fractional value) is input 
  !       by the user in the CRTM Surface structure.  If no green vegetation
  !       fraction is input, then a climatological green vegetation fraction 
  !       is read from the Green Vegetation Fraction look-up table. 
  !
  !       To obtain the climatological green vegetation fraction from the LUT,
  !       the user must input dayofyear, year and latitude.
  !
  ! CALLING SEQUENCE:
  !       CALL compute_grels_emis ( wavelength, gfsclass, &
  !                                 greenvegfrac, dayofyear, year, lat, &
  !                                 emis )
  !
  ! INPUT ARGUMENTS:
  !
  !       wavelength:      Wavelength for which emissivity is desired
  !                        UNITS:      micrometer
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       gfsclass:        GFS surface type (integer 1-13)
  !                        UNITS:      none (category)
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN)
  !
  !       greenvegfrac:    Green vegetation fraction (GVF) input from user
  !                        in CRTM Surface structure
  !                        OPTIONAL: if not present, climatological GVF
  !                           can be used. NOTE, to get climatological GVF,
  !                           dayofyear, year and lat must be input.
  !                        UNITS:      none (fraction 0.0-1.0)
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN), OPTIONAL
  !
  !       dayofyear:       Day of year for which emissivity is calculated
  !                        OPTIONAL: not needed if greenvegfrac is input by user
  !                           IMPORTANT: if greenvegfrac is not input, dayofyear
  !                           is required in order to get climatological GVF
  !                        UNITS:      day-of-year (1-365 or 1-366)
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN), OPTIONAL
  !
  !       year:            Year in which the day falls for which emissivity
  !                        is calculated
  !                        OPTIONAL: not needed if greenvegfrac is input by user
  !                           IMPORTANT: if greenvegfrac is not input, year
  !                           is required in order to get climatological GVF
  !                        UNITS:      4-digit year
  !                        TYPE:       INTEGER
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN), OPTIONAL
  !
  !       lat:             Latitude for which emissivity is calculated
  !                        OPTIONAL: not needed if greenvegfrac is input by user
  !                           IMPORTANT: if greenvegfrac is not input, lat
  !                           is required in order to get climatological GVF
  !                        UNITS:      degrees (-90 to +90)
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(IN), OPTIONAL
  !
  ! OUTPUT ARGUMENTS:
  !
  !       emis:            Emissivity calculated from GrELS reflectance table
  !                        and green vegetation fraction
  !                        UNITS:      none (fraction 0.0-1.0)
  !                        TYPE:       REAL
  !                        DIMENSION:  scalar
  !                        ATTRIBUTES: INTENT(OUT)
  !                  IMPORTANT: if GVF is a missing value, then emis cannot
  !                             be calculated and -999.9 is returned
  !
  !-----------------------------------------------------------

  SUBROUTINE compute_grels_emis ( wavelength, &    ! Input
                                  gfsclass, &      ! Input
                                  emis, &          ! Output
                                  greenvegfrac, &  ! Input, Optional
                                  dayofyear, &     ! Input, Optional
                                  year, &          ! Input, Optional
                                  lat )            ! Input, Optional

      REAL(fp), INTENT(IN)           :: wavelength
      INTEGER,  INTENT(IN)           :: gfsclass
      REAL(fp), INTENT(OUT)          :: emis
      REAL(fp), INTENT(IN), OPTIONAL :: greenvegfrac
      INTEGER,  INTENT(IN), OPTIONAL :: dayofyear
                                          ! required if greenvegfrac is absent
      INTEGER,  INTENT(IN), OPTIONAL :: year
                                          ! required if greenvegfrac is absent
      REAL(fp), INTENT(IN), OPTIONAL :: lat
                                          ! required if greenvegfrac is absent


      TYPE(Refl_interp_type)          :: refl_i
      REAL(fp), DIMENSION(N_REFL_PTS) :: wavl_spectrum, &
                                         rveg_spectrum, &
                                         rsoil_spectrum
      REAL(fp), PARAMETER             :: wavl_interval = 0.01_fp
      REAL(fp), DIMENSION(NPTS)       :: rveg_poly_pts, rsoil_poly_pts
      REAL(fp)                        :: gvf, adj_refl


      wavl_spectrum  = grels_refl_table(1,:)           ! all wavelengths
      rveg_spectrum  = grels_refl_table(gfsclass+1,:)  ! veg refl spectrum
      rsoil_spectrum = grels_refl_table(15,:)          ! soil refl spectrum
      refl_i%w_int   = wavelength  ! the wavelength at which an interpolated
                                   ! reflectance is desired

      ! Find the indices for the interpolation
      CALL Find_Index( wavl_spectrum, wavl_interval, refl_i%w_int, & ! Input
                       refl_i%i1, refl_i%i2, refl_i%w_outbound )     ! Output

      refl_i%w       = wavl_spectrum(refl_i%i1:refl_i%i2)
      rveg_poly_pts  = rveg_spectrum(refl_i%i1:refl_i%i2)
      rsoil_poly_pts = rsoil_spectrum(refl_i%i1:refl_i%i2)

      ! Calculate the interpolating polynomials
      CALL LPoly( refl_i%w, refl_i%w_int, &   ! Input
                  refl_i%wlp )                ! Output

      ! Perform interpolation
      ! 1) vegetation reflectance
      CALL Interp_1D ( rveg_poly_pts, refl_i%wlp, &   ! Input
                       refl_i%rveg_int )              ! Output
      ! 2) soil reflectance
      CALL Interp_1D ( rsoil_poly_pts, refl_i%wlp, &  ! Input
                       refl_i%rsoil_int )             ! Output


      ! Check for presence of Green Vegetation Fraction.
      ! Then calculate seasonally GVF-adjusted reflectance.
      ! Then calculate emissivity.
      IF ( PRESENT(greenvegfrac) ) THEN
         adj_refl = ( greenvegfrac * refl_i%rveg_int ) + &
                    ( (1.0_fp-greenvegfrac) * refl_i%rsoil_int )
         emis = 1.0_fp - adj_refl
      ELSE IF ( PRESENT(dayofyear) .AND. PRESENT(year) .AND. PRESENT(lat) ) THEN
             CALL get_gvf_climatol ( dayofyear, year, lat, gfsclass, gvf )
             IF (gvf .EQ. -999.9) THEN
                emis = -999.9
             ELSE
                adj_refl = ( gvf * refl_i%rveg_int ) + &
                           ( (1.0_fp-gvf) * refl_i%rsoil_int )
                emis = 1.0_fp - adj_refl
             END IF
      ELSE
         PRINT '(3A)', "When GVF is absent, climatological GVF can only ", &
                       "be used when dayofyear, year and lat are input. ", &
                       "Please input dayofyear, year and lat."
      END IF

  END SUBROUTINE compute_grels_emis


END MODULE CRTM_GrELS_GFS
