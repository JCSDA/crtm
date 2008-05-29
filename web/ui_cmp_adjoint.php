<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface:Components:Adjoint</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Components: Adjoint</h1>
				
<p>The CRTM adjoint model calling sequence is,

<pre>
  Error_Status = CRTM_Adjoint( Atmosphere             , &  ! FWD Input
                               Surface                , &  ! FWD Input
                               RTSolution_AD          , &  ! AD  Input
                               GeometryInfo           , &  ! Input
                               ChannelInfo            , &  ! Input
                               Atmosphere_AD          , &  ! AD  Output
                               Surface_AD             , &  ! AD  Output
                               RTSolution             , &  ! FWD Output
                               Options    =Options    , &  ! Optional FWD input
                               RCS_Id     =RCS_Id     , &  ! Revision control
                               Message_Log=Message_Log  )  ! Error messaging</pre>
<p>The same structure definitions are used for both the forward and
adjoint structures.

<p>The table below shows the dimensionalities of the input and output
arguments for the adjoint model in terms of the number of satellite
sensors, <code>nSensors</code>, the number of atmospheric profiles, <code>nProfiles</code>,
and the total number of sensor channels, <code>nChannels</code>.

<div class="tablecontent2">
  <table cellspacing="0" cellpadding="0" border="0">
	<caption align="bottom">
	Dimensionality for <code>CRTM_Adjoint</code> function arguments
	</caption>
	<tr>
	  <th colspan="3">INPUTS</th>
	  <th>OPTIONAL INPUT</th>
	  <th colspan="2">OUTPUTS</th>
	</tr>
	<tr>
	  <th scope="col">Atmosphere,<br>Surface,<br>GemometryInfo</th>
	  <th scope="col">RTSolution_AD</th>
	  <th scope="col">ChannelInfo</th>
	  <th scope="col">Options</th>
	  <th scope="col">Atmosphere_AD,<br>Surface_AD</th>
	  <th scope="col">RTSolution</th>
	</tr>
	<tr>
	  <td><code>nProfiles</code></td>
	  <td><code>nChannels<br>
					x<br>
				nProfiles</code></td>
	  <td><code>nSensors</code></td>
	  <td><code>nProfiles</code></td>
	  <td><code>nProfiles</code></td>
	  <td><code>nChannels<br>
				   x<br>
				nProfiles</code></td>
	</tr>
  </table>
</div>

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
