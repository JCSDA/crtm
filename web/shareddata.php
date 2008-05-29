<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Shared Data</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> Shared Data</h1>

The shared data in the CRTM are structures that are loaded with precomputed data during the CRTM initialization phase. They are <em>not</em> visible via the CRTM user interface.


<p><a name="sd_struct_table"></a>
<div class="tablecontent2">
  <table cellspacing="0" cellpadding="0" border="0">
    <caption align="top">List of the Shared Data CRTM structure types</caption>
    <tr>
      <th scope="col">TYPE NAME</th>
      <th scope="col">DESCRIPTION</th>
    </tr>
    <tr>
      <td><tt>SpcCoeff_type</tt></td>
      <td align="left">Structure containing precomputed instrument channel spectral coefficient
        data such as frequency, polarization, Planck function coefficients, etc., loaded
        during initialization. Defined in the <tt>SpcCoeff_Define</tt> module. Data
        shared in the CRTM via the <tt>CRTM_SpcCoeff</tt> module.</td>
    </tr>
    <tr>
      <td><tt>TauCoeff_type</tt></td>
      <td align="left">Structure containing precomputed coefficient data used in the
        <tt>AtmAbsorption</tt> algorithm to compute the optical depths due to gaseous
        absorption. Defined in the <tt>TauCoeff_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>CloudCoeff_type</tt></td>
      <td align="left">Structure containing cloud scattering optical properties to compute
        cloud absorption and scattering contributions. Defined in the
        <tt>CloudCoeff_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>AerosolCoeff_type</tt></td>
      <td align="left">Structure containing aerosol scattering optical properties to compute
        aerosol absorption and scattering contributions. Defined in the
        <tt>AerosolCoeff_Define</tt> module.</td>
    </tr>
    <tr>
      <td><tt>EmisCoeff_type</tt></td>
      <td align="left">Structure containing precomputed infrared sea surface emissivity data
        to compute instrument specific sea surface emissivities. Defined in the
        <tt>EmisCoeff_Define</tt> module.
      </td>
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
