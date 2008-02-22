<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Downloads</title>
<?php
// style links and the icon link
// pulls in .css files for regular and print display
require('includes/styleLinks.inc');
?>

<!-- if you need to include your own .css files
     put the links HERE, before the closing head tag. -->

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
			
				<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> Downloads</h1>
				
				<p>To download any of the files listed on this page, site users must register 
					with the CRTM project organizers. To do so, send an e-mail to 
					<a href="mailto:CRTMdownload@jcsda.noaa.gov">CRTM code download</a>
					that includes your name, your organization, address, phone, and e-mail address, and 
					we will send you an ID and password. When you click on any of the links
					listed here, you will be prompted for your ID and password, and after you
				enter them, you will be able to download various project files. </p>
				
				<h2>Files available for download by registered users</h2>
				<ul>
					<li><a href="CRTMcode/">Source code</a></li>
					<li><a href="CRTMcode/">Coefficient files</a></li>
					<li><a href="CRTMcode/">Test Code</a></li>
				</ul>

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
