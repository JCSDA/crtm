<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface: Destruction</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Destruction</h1>
				
During the model initialization, the <tt>ChannelInfo</tt> input structure and the coefficient
shared data structures are allocated to the required size, determined by dimensions in the
coefficient datafiles. To release this memory, the model destruction routine should be called,

<pre>
  Error_Status = CRTM_Destroy( ChannelInfo            , &  ! Output
                               Process_ID =Process_ID , &  ! Optional input
                               RCS_Id     =RCS_Id     , &  ! Revision control
                               Message_Log=Message_Log  )  ! Error messaging
</pre>

<p>Note also that any allocated CRTM input and output structures may also need to be destroyed.
This is achieved via the <tt>CRTM_Destroy_</tt>&lt;<em>Structure name</em>&gt;. These are discussed
further below.

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
