<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Overview</title>
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
			
				<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> - Overview</h1>
				
<p>The Community Radiative Transfer Model (CRTM) under development at the
<a href="http://www.jcsda.noaa.gov">Joint Center for Satellite Data Assimilation (JCSDA)</a>
is an important component in the NOAA/NCEP data analysis system. External research
groups funded by JCSDA have also been making their contributions to its development.
In order to help the developers to understand the requirements from the analysis system
and to reduce the inconsistencies among the components developed by various groups, this
web page will serve as an online source for information about the software framework for
the development of the CRTM.

<p>The main goal of this CRTM framework is to provide developers, and users, with the
information, examples, and utilities necessary to produce software that is flexible (in
both development and usage), understandable, and easily maintained.

<p>The CRTM framework breaks the radiative transfer model into components (e.g. gaseous
absorption, scattering, surface optics); each of which defines its own data structure and
algorithm modules to faciliate independent development of each component. This characterisation
is obviously an ideal one since there are dependencies -- in terms of both the physical
problem being solved and in software details such as data sharing -- that will not allow
every component to be developed in isolation from the others. So, this is not intended to
be a replacement for necessary dialogue between developers, but to provide some form of
"big picture" for all those involved to minimise or eliminate potential software conflicts
and/or redundencies.

<p>Due to the complexity of the radiative transfer problem and the difficulty of balancing
the code efficiency and flexibility, we fully anticipate problems arising when the framework
is applied in the development process. We sincerely welcome
<a href="mailto:paul.vandelst@noaa.gov?subject=CRTM Framework Comments and Suggestions">
comments and suggestions</a> for its improvement.

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
