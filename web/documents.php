<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - Documents</title>
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
			
				<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> Documents</h1>
				
				<h2>User Guide</h2>
				
				<ul>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">User Guide</a> (PDF, X MB)</li>
				</ul>
				
				<h2>Developer Guides</h2>
				
				<ul>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">First Developer Guide</a> (PDF, X MB)</li>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">Second Developer Guide</a> (PDF, X MB)</li>
				</ul>
					
				<h2>Algorithm Descriptions</h2>
					
				<ul>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">First Algorithm Description</a> (PDF, X MB)</li>
				</ul>
				
				<h2>Technical Reports</h2>
				<ul>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">First Technical Report</a> (PDF, X MB)</li>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">Second Technical Report</a> (PDF, X MB)</li>
				</ul>


				<h2>Validation Reports</h2>
				<ul>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">First Validation Report</a> (PDF, X MB)</li>
					<li><a href="documents/"
						title="this document opens in a new window"
						target="_blank">Second Validation Report</a> (PDF, X MB)</li>
				</ul>
				
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
