<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface:Public Structures:GeometryInfo</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<!-- if you need to include your own .css files
     put the links HERE, before the closing head tag. -->

<link href="crtm.css" type="text/css" rel="stylesheet">
<!-- DO NOT DELETE -->
<!--[if IE]>
  <link href="crtm.ie.css" type="text/css" rel="stylesheet">
<![endif]-->

</head>
<body>
<?php
// insert banner rows
require('includes/banner.inc');

?>
  <tr>
    <td id="navCell">
			<?php
			// insert navigation div
			require('includes/NavDiv.inc');
			?>
		</td>
		<td class="mainPanel"><a name="skipTarget"></a><?php require('includes/noScriptWarning.inc'); ?>
			<div class="padding">
				<!-- DO NOT DELETE OR ALTER CODE ABOVE THIS COMMENT -->
				<!-- EXCEPT for the contents of the <title></title> TAG!! -->
				<!-- You can start project specific content HERE -->
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Public Structures: GeometryInfo</h1>

<p>The <tt>GeometryInfo</tt> data structure is used to contain information about the sensor field-of-view (FOV)
location and geometry. Some example declarations,

<pre>
  <strong>! Example declaration for a scalar structure</strong>
  TYPE(CRTM_GeometryInfo_type) :: GeoInfo

  <strong>! Example declaration for a single location/profile</strong>
  TYPE(CRTM_GeometryInfo_type) :: GeoInfo(1)

  <strong>! Example declaration for a fixed number of locations/profiles</strong>
  INTEGER, PARAMETER :: N_PROFILES = 10
  TYPE(CRTM_GeometryInfo_type) :: GeoInfo(N_PROFILES)

  <strong>! Example declaration as an allocatable</strong>
  TYPE(CRTM_GeometryInfo_type), ALLOCATABLE :: GeoInfo(:)
</pre>

<p>For use with the CRTM functions - <tt>CRTM_Forward()</tt>, <tt>CRTM_Tangent_Linear()</tt>, <tt>CRTM_Adjoint()</tt>,
or <tt>CRTM_K_Matrix()</tt>) - the <tt>GeometryInfo</tt> structure should <em>always</em> be declared as a rank-1 array with each
element corresponding to a different FOV location/atmospheric profile.

<h3> <code>GeometryInfo</code> Definition</h3>
<!-- ==================================== -->
The <tt>GeometryInfo</tt> structure definition is shown below,

<pre>
  TYPE :: CRTM_GeometryInfo_type
    <strong>! User Input
    ! ----------
    ! Earth radius and satellite height</strong>
    REAL(fp) :: Earth_Radius     = EARTH_RADIUS
    REAL(fp) :: Satellite_Height = SATELLITE_HEIGHT
    REAL(fp) :: Distance_Ratio   = EARTH_RADIUS / (EARTH_RADIUS + SATELLITE_HEIGHT)
    <strong>! Earth location</strong>
    REAL(fp) :: Longitude        = ZERO
    REAL(fp) :: Latitude         = ZERO
    REAL(fp) :: Surface_Altitude = ZERO
    <strong>! Sensor angle information</strong>
    REAL(fp) :: Sensor_Zenith_Angle  = ZERO
    REAL(fp) :: Sensor_Azimuth_Angle = ZERO 
    <strong>! Source angle information</strong>
    REAL(fp) :: Source_Zenith_Angle  = ZERO
    REAL(fp) :: Source_Azimuth_Angle = ZERO
    <strong>! Flux angle information</strong>
    REAL(fp) :: Flux_Zenith_Angle = DIFFUSIVITY_ANGLE

    <strong>! Derived from User Input
    ! -----------------------
    ! Sensor angle information</strong>
    REAL(fp) :: Sensor_Scan_Angle     = ZERO
    REAL(fp) :: Sensor_Scan_Radian    = ZERO
    REAL(fp) :: Sensor_Zenith_Radian  = ZERO
    REAL(fp) :: Sensor_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Sensor_Zenith  = ZERO
    <strong>! Source angle information</strong>
    REAL(fp) :: Source_Zenith_Radian  = ZERO
    REAL(fp) :: Source_Azimuth_Radian = ZERO
    REAL(fp) :: Secant_Source_Zenith  = ZERO
    <strong>! Flux angle information</strong>
    REAL(fp) :: Flux_Zenith_Radian = DIFFUSIVITY_RADIAN
    REAL(fp) :: Secant_Flux_Zenith = SECANT_DIFFUSIVITY
  END TYPE CRTM_GeometryInfo_type
</pre>

<p>where the <tt>SATELLITE_HEIGHT</tt> parameter is a nominal value for low-earth orbit satellites; and 
the <tt>DIFFUSIVITY_ANGLE</tt> parameter is cos<sup>-1</sup>(3/5), used to approximate downwelling flux
in the infrared.

<p>The <tt>GeometryInfo</tt> components listed as "User Input" are those that
<em>must</em> be set by the user before calling the CRTM functions. The components
listed as "Derived" are computed from the inputs via the <tt>CRTM_Compute_GeometryInfo()</tt>
function which is called within the CRTM.

<p>Note that all of the <tt>GeometryInfo</tt> components are scalar and thus there are no allocation,
assignment, or destruction functions required.

				<!-- END your project-specific content HERE -->
				<!-- DO NOT DELETE OR ALTER BELOW THIS COMMENT! -->
			</div>
		</td>
	</tr>
<?php
// insert footer & javascript include for menuController
require('includes/footer.inc');
?>
</table>
</body>
</html>
