<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Shared Data:SpcCoeff</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> Shared Data : SpcCoeff</h1>

The <tt>SpcCoeff</tt> data is used to make instrument specific spectral data available 
within the CRTM. The structure definition and the data itself is not specific to the 
CRTM.

<h3><a name="CRTM_SPCCOEFF"></a>The <tt>CRTM_SpcCoeff</tt> Module</h3>

<p>For use in the CRTM, the initialization phase loads the <tt>SpcCoeff</tt> data into a public
data structure array that resides in the <tt>CRTM_SpcCoeff</tt>
module. To gain access to the <tt>SpcCoeff</tt> data, one simply uses it in their CRTM
application module,

<pre>
  USE CRTM_SpcCoeff
</pre>

<p>In the <tt>CRTM_SpcCoeff</tt> module, the <tt>SpcCoeff</tt> data structure is declared as a publicly visible allocatable array,

<pre>
  PUBLIC :: SC
  TYPE(SpcCoeff_type), SAVE, ALLOCATABLE :: SC(:)
</pre>

During the CRTM initialisation, the <tt>SC()</tt> array is allocated to the size of the number of sensors requested by the user via the Sensor_ID argument. When the <tt>CRTM_SpcCoeff</tt> module is <tt>USE</tt>d, application modules can access all the <tt>SC</tt> structure contents loaded during initialization.
				
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
