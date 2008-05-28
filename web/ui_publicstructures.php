<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface:Public Structures</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Public Structures</h1>

<p>Where possible, data passed into and within the CRTM are encapsulated
within structures. This allows for changes to be made (data components added or removed as
necessary) with minimal impact to the various CRTM interfaces.

<p>This section describes the public structure types used in the CRTM. Structures of these
types are arguments in the CRTM user interfaces. The table below is an overview of all the
public CRTM structure types.

<p><a name="struct_table"></a>
<div class="tablecontent2">
  <table cellspacing="0" cellpadding="0" border="0">
	<caption align="top">
	List of the Public CRTM structure types
	</caption>
    <tr>
      <th scope="col">TYPE NAME</th>
      <th scope="col">DESCRIPTION</th>
    </tr>
    <tr>
      <td><tt>CRTM_ChannelInfo_type</tt></td>
      <td align="left">Structure containing sensor channel information for correct indexing of
          the CRTM coefficient data. This structure is filled during the CRTM
          initialization phase. Defined in the <tt>CRTM_ChannelInfo_Define</tt>
          module.</td>
    </tr>
    <tr>
      <td><tt>CRTM_Atmosphere_type</tt></td>
      <td>Structure containing atmospheric state variable profile data, including profiles
          of cloud parameters. Defined in the <tt>CRTM_Atmosphere_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>CRTM_Cloud_type</tt></td>
      <td>Structure containing cloud state variable profile data. Defined in the
          <tt>CRTM_Cloud_Define</tt> module. This structure is contained within
          the <tt>Atmosphere</tt> structure.</td>
    </tr>
    <tr>
      <td><tt>CRTM_Aerosol_type</tt></td>
      <td>Structure containing aerosol state variable profile data. Defined in the
          <tt>CRTM_Aerosol_Define</tt> module. This structure is contained within
          the <tt>Atmosphere</tt> structure.</td>
    </tr>
    <tr>
      <td><tt>CRTM_Surface_type</tt></td>
      <td>Structure containing surface type and state information. Defined in the
          <tt>CRTM_Surface_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>CRTM_SensorData_type</tt></td>
      <td>Structure containing satellite instrument brightness temperature data used as input
          for some surface emissivity algorithms. Defined in the
          <tt>CRTM_SensorData_Define</tt> module. This structure is contained within
          the <tt>Surface</tt> structure.</td>
    </tr>
    <tr>
      <td><tt>CRTM_GeometryInfo_type</tt></td>
      <td>Structure containing geometry information such as Earth location, satellite
          view angle, etc. Defined in the <tt>CRTM_GeometryInfo_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>CRTM_RTSolution_type</tt></td>
      <td>Structure containing results of the radiative transfer calculation such as
          radiance and brightness temperature, etc. Defined in the <tt>CRTM_RTSolution_Define</tt>
          module.</td>
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
