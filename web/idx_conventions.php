<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Conventions</title>
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
			
				<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> - Conventions</h1>
				
The following are conventions that have been adhered to in the current release of the 
CRTM framework. Please note that they are guidelines intended to make understanding the
code at a glance easier, to provide a recognisable "look and feel", and to minimise name
space clashes. They are not intended to be requirements imposed from on-high onto developers.


<h2><a name="TYPE_CONVENTIONS"></a>Naming of Structure Types and Instances of Structure</h2>
<!-- =================================================================================== -->

The structure type naming convention adopted for use in the CRTM is,
<br><br>
<span class="indent1">[<tt>CRTM_</tt>]<em>&lt;name&gt;</em><tt>_type</tt></span>
<br><br>
where <em>&lt;name&gt;</em> is some sort of identifier that indicates for what a
structure is to be used. <em>All</em> structure type names are suffixed with
"<tt>_type</tt>" and CRTM-specific structure types are prefixed with "<tt>CRTM_</tt>".
Some examples,

<pre>
  SpcCoeff_type
  CRTM_Atmosphere_type
  CRTM_AtmScatter_type
</pre>

<p>An instance of a structure is then referred to via its <em>&lt;name&gt;</em>, or some
sort of derivate of its <em>&lt;name&gt;</em>. Some structure declarations examples are then,

<pre>
  TYPE( SpcCoeff_type )        :: SpcCoeff
  TYPE( CRTM_Atmosphere_type ) :: Atmosphere, Atmosphere_AD
  TYPE( CRTM_AtmScatter_type ) :: AtmScatter, AtmScatter_AD
</pre>

<p>where the adjoint structure variables are identified with an "<tt>_AD</tt>" suffix.


<h2><a name="DEFN_CONVENTIONS"></a>Naming of Definition Modules</h2>
<!-- =========================================================== -->

Modules containing structure type definitions are termed <em>definition modules</em>. The naming
convention adopted for definition modules in the CRTM is,
<br><br>
<span class="indent1">[<tt>CRTM_</tt>]<em>&lt;name&gt;</em><tt>_Define</tt></span>
<br><br>
where, as with the structure type names, <em>all</em> definition module names are suffixed
with "<tt>_Define</tt>" and CRTM-specific definition modules are prefixed with "<tt>CRTM_</tt>".
Some examples,

<pre>
  SpcCoeff_Define
  CRTM_Atmosphere_Define
  CRTM_AtmAbsorption_Define
  CRTM_AtmScatter_Define
</pre>

<p>The actual source code files for these modules have the same name with an "<tt>.f90</tt>" suffix.


<h2><a name="APPL_CONVENTIONS"></a>Naming of Application Modules</h2>
<!-- ============================================================ -->

Modules containing the routines that perform the calculations for the various components of the
CRTM are termed <em>application modules</em>. The naming convention adopted for application
modules in the CRTM is,
<br><br>
<span class="indent1"><tt>CRTM_</tt><em>&lt;name&gt;</em></span>
<br><br>
Some examples,

<pre>
  CRTM_AtmAbsorption
  CRTM_SfcOptics
</pre>

<p>An exception to this convention is for the scattering application code. Separate modules
have been provided to contain the aerosol and cloud absorption/scattering code,

<pre>
  CRTM_AerosolScatter
  CRTM_CloudScatter
</pre>

<p>Both of these modules use the <tt>AtmScatter</tt> structure definitions. For more details, see
the <a href="CRTM_Developer_Interface.html#ATMSCATTER">AtmScatter section in the Developer's Interface.</a>

<p>Again, the actual source code files for these modules have the same name with an "<tt>.f90</tt>"
suffix. Note that not all definition modules have a corresponding application module since some
structures (e.g. <tt>SpcCoeff</tt> and <tt>Atmosphere</tt> structures) are simply data containers.

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
