<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface:Public Structures:ChannelInfo</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Public Structures: ChannelInfo</h1>

<p>The <tt>ChannelInfo</tt> structure is used to specify channel specific information related to the
sensor, satellite and channel numbers used in the CRTM. This structure is used to access the
data loaded during the initialization phase. The <tt>ChannelInfo</tt> structure is always a rank-1
array with its dimension being the number of sensors to be processed. For example, some 
<tt>ChannelInfo</tt> structure declarations,
<pre>
  ! Example declaration for four sensors
  INTEGER, PARAMETER :: N_SENSORS = 4
  TYPE(CRTM_ChannelInfo_type) :: ChInfo1(N_SENSORS)
  
  ! Example declaration as allocatable
  TYPE(CRTM_ChannelInfo_type), ALLOCATABLE :: ChInfo2(:)
</pre>
<p>The <tt>ChannelInfo</tt> structure is allocated and populated during the CRTM initialization via
the <tt>CRTM_Init</tt> function; users should not explicitly allocate and fill the <tt>ChannelInfo</tt>
structure as it is used to coordinate amongst the different types of data loaded during initialization.
Similarly for the destruction of the <tt>ChannelInfo</tt> structure; this is handled during the CRTM 
destruction when all the relevant data structures are deallocated via the <tt>CRTM_Destroy</tt>
function.

<h3> <code>ChannelInfo</code> Definition</h3>

<p>The <tt>ChannelInfo</tt> structure definition is shown below.

<pre>
  TYPE :: CRTM_ChannelInfo_type
    INTEGER :: n_Channels = 0  ! L dimension
    INTEGER :: StrLen = STRLEN
    CHARACTER(STRLEN), DIMENSION(:), POINTER :: <a href="javascript:open_table_window('tables/sensorid.html')">SensorID</a>         => NULL()  ! L
    INTEGER,           DIMENSION(:), POINTER :: <a href="javascript:open_table_window('tables/sensorid.html')">WMO_Satellite_ID</a> => NULL()  ! L
    INTEGER,           DIMENSION(:), POINTER :: <a href="javascript:open_table_window('tables/sensorid.html')">WMO_Sensor_ID</a>    => NULL()  ! L
    INTEGER,           DIMENSION(:), POINTER :: Sensor_Channel   => NULL()  ! L
    INTEGER,           DIMENSION(:), POINTER :: Channel_Index    => NULL()  ! L
  END TYPE CRTM_ChannelInfo_type
</pre>


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
