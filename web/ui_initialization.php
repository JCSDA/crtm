<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface: Initialization</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Initialization</h1>
				
<p>This is the first step to use the CRTM as it initializes the model by loading the various
coefficients from datafiles into memory. The CRTM initialization function is contained within
the <code>CRTM_LifeCycle.f90</code> module and its calling sequence is,
<pre>
  Error_Status = CRTM_Init( <a href="#channelinfo">ChannelInfo</a>,                         &  ! Output
                            SensorID         =<a href="#sensorid">SensorID</a>,          &  ! Optional input
                            CloudCoeff_File  =<a href="#coeffs">CloudCoeff_File</a>,   &  ! Optional input
                            AerosolCoeff_File=<a href="#coeffs">AerosolCoeff_File</a>, &  ! Optional input
                            EmisCoeff_File   =<a href="#coeffs">EmisCoeff_File</a>,    &  ! Optional input
                            File_Path        =<a href="#path">File_Path</a>,         &  ! Optional input
                            Quiet            =<a href="#quiet">Quiet</a>,             &  ! Optional input
                            Process_ID       =Process_ID,        &  ! Optional input
                            Output_Process_ID=Output_Process_ID, &  ! Optional input
                            RCS_Id           =RCS_Id,            &  ! Revision control
                            Message_Log      =Message_Log        )  ! Error messaging</pre>


<a name="channelinfo"></a><h3><code>ChannelInfo</code> argument (mandatory)</h3>
<!-- ======================================================================= -->
The <code>ChannelInfo</code> argument is the only mandatory argument to the <code>CRTM_Init</code> function and it
is an <em>output</em> argument. After initialization, the <code>ChannelInfo</code> structure contains all
the channel information required for each sensor in later CRTM function calls.

<p>An example of defining the <code>ChannelInfo</code> argument to use in initializing the CRTM for four
sensors is shown below,
<pre>
  USE CRTM_Module
  ...
  INTEGER, PARAMETER :: NSENSORS = 4
  ...
  TYPE(CRTM_ChannelInfo_type) :: ChannelInfo(NSENSORS)
</pre>
 

<a name="sensorid"></a><h3><code>SensorID</code> argument (optional)</h3>
<!-- ================================================================ -->
The <code>SensorID</code> argument lets the CRTM know what sensor coefficient files should
be loaded. Sensor ids are specified as an array of character strings in the format of
<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&lt;<em>sensor name</em>&gt;_&lt;<em>platform</em>&gt;<br><br>

Examples of sensor ids are <code>hirs3_n17</code> for the NOAA-17 HIRS/3 instrument,
<code>ssmis_f16</code> for the DMSP-16 SSMIS instrument, etc. The sensor ids are used
to construct the instrument specific spectral (SpcCoeff) and optical depth (TauCoeff)
coefficient filenames which are read during the initialization.

<p>An example of the initialization call for a set of defined sensor ids is shown below,

<pre>
  USE CRTM_Module
  ...
  INTEGER, PARAMETER :: NSENSORS = 4
  ...
  TYPE(CRTM_ChannelInfo_type) :: ChannelInfo(NSENSORS)
  CHARACTER(STRLEN) :: SensorID(NSENSORS)   ! Note: STRLEN is defined via CRTM_Module
  ...
  SensorID = (/'hirs3_n17       ', & ! Note: Some f95 compilers complain if
               'amsre_aqua      ', & !       all the character array elements
               'windsat_coriolis', & !       are not the same length. To avoid
               'imgr_g12        ' /) !       problems, make them so.
  ...
  Error_Status = CRTM_Init( ChannelInfo      , &
                            SensorID=SensorID  )</pre>

During the initialization, sensor coefficient files such as <code>hirs3_n17.SpcCoeff.bin</code>,
<code>hirs3_n17.TauCoeff.bin</code>, <code>amsre_aqua.SpcCoeff.bin</code>, etc. are loaded.

<p>If no sensor id argument is specified, the initialization process tries to load
default SpcCoeff and TauCoeff coefficient files using the naming convention,
<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&lt;<em>structure name</em>&gt;<code>.bin</code><br><br>
i.e. <code>SpcCoeff.bin</code> and <code>TauCoeff.bin</code>.


<a name="coeffs"></a><h3>Coefficient file arguments (optional)</h3>
<!-- ========================================================== -->
The other coefficient file arguments are listed separately as they are not sensor
specific. The cloud, aerosol, and emissivity <acronym title="Look Up Table">LUT</acronym>
coefficient datafile arguments are all optional since additional coefficient files may be
added to the list of input arguments as required.

<p>As with the sensor id default specification,
if no filenames are passed the default coefficient file naming convention is
<br><br>
&nbsp;&nbsp;&nbsp;&nbsp;&lt;<em>structure name</em>&gt;<tt>.bin</tt><br><br>
such as <tt>CloudCoeff.bin</tt>, <tt>AerosolCoeff.bin</tt>, etc. This means the following initialization
function call example,

<pre>
  Error_Status = CRTM_Init( ChannelInfo )</pre>

is equivalent to,

<pre>
  Error_Status = CRTM_Init( ChannelInfo                         , &
                            CloudCoeff_File  ='CloudCoeff.bin'  , &
                            AerosolCoeff_File='AerosolCoeff.bin', &
                            EmisCoeff_File   ='EmisCoeff.bin'     )</pre>


<a name="path"></a><h3><code>File_Path</code> argument (optional)</h3>
<!-- ============================================================= -->
<p>The default location for the various files read by the <code>CRTM_Init</code> function
is the current directory. The <code>File_Path</code> argument allows the user to define 
an alternate location for all the coefficient data files. In the following call example,
<pre>
  Error_Status = CRTM_Init( ChannelInfo                   , &
                            File_Path='/data/coefficients'  )</pre>
the default coefficient files <code>SpcCoeff.bin</code>, <code>TauCoeff.bin</code>,
<code>CloudCoeff.bin</code>, <code>AerosolCoeff.bin</code>, and <code>EmisCoeff.bin</code>
are expected to reside in the directory <code>/data/coefficients</code>.


<a name="quiet"></a><h3><code>Quiet</code> argument</h3>
<!-- =============================================== -->
By default, all information messages regarding the coefficient file loading are written
to standard output (or to a log file if the optional <code>Message_Log</code> argument
is specified). The type of messages one sees look like,
<pre>
  Insert typical message format here....
</pre>
To suppress these messages, set the <code>Quiet</code> argument to a value of 1.

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
