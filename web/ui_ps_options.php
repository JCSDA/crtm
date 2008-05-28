<?php
// insert header that forbids caching and carries doctype
// and html tag;
require('includes/noCacheHeader.inc');
?>
<META name="verify-v1" content="CwbLBcFt9+GqRTgaLZsENmPnSWNB5MStHHdYsB7U2nI=">
<title>Community Radiative Transfer Model (CRTM) - User Interface:Public Structures:Options</title>
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
			
<h1><acronym title="Community Radiative Transfer Model">CRTM</acronym> User Interface: Public Structures: Options</h1>

<p>The optional <tt>Options</tt> structure is how optional inputs are specified to the CRTM. The
<tt>Options</tt> structure should be dimensioned for the number of input profiles. Some example declarations,
<pre>
  <strong>! Example declaration for a known number of profiles</strong>
  INTEGER, PARAMETER :: N_PROFILES = 10
  TYPE(CRTM_Options_type) :: Options(N_PROFILES)
  
  <strong>! Example declaration as an allocatable</strong>
  TYPE(CRTM_Options_type), ALLOCATABLE :: Options(:)
</pre>
<p>For use with the CRTM functions <a href="/crtm/user_interface/components/forward.shtml"><tt>CRTM_Forward()</tt></a>,
<a href="/crtm/user_interface/components/tangent_linear.shtml"><tt>CRTM_Tangent_Linear()</tt></a>,
<a href="/crtm/user_interface/components/adjoint.shtml"><tt>CRTM_Adjoint()</tt></a>, or 
<a href="/crtm/user_interface/components/k_matrix.shtml"><tt>CRTM_K_Matrix()</tt></a>, the <tt>Options</tt> structure
(if specified as an actual argument) <em>must</em> be declared as a rank-1 array with the dimension corresponding
to the number of atmospheric profiles to be processed.


<h3><a name="optdefine"></a><code>Options</code> Definition</h3>

<p>The <tt>Options</tt> structure definition is shown below,

<pre>
  TYPE :: CRTM_Options_type
    <strong>! Dimensions</strong>
    INTEGER :: n_Channels = 0  ! L dimension
    <strong>! Index into channel-specific components</strong>
    INTEGER :: Channel = 0
    <strong>! Emissivity optional arguments</strong>
    INTEGER                         :: Emissivity_Switch =  NOT_SET
    REAL(fp), DIMENSION(:), POINTER :: Emissivity        => NULL() ! L
    <strong>! Direct reflectivity optional arguments</strong>
    INTEGER                         :: Direct_Reflectivity_Switch =  NOT_SET
    REAL(fp), DIMENSION(:), POINTER :: Direct_Reflectivity        => NULL() ! L
  END TYPE CRTM_Options_type
</pre>

<p>The only optional inputs currently defined are spectral emissivity and direct reflectivity. As more optional
inputs are required, they can be added to the <tt>Options</tt> structure without changing the CRTM function
interfaces.

<p>An important point to note is that the options structure is only used as a <em>forward</em> variable
regardless of which CRTM function is being called. Because the <tt>Options</tt> structure is only used
as a forward variable, the channel dimension can be internal to the structure - unlike, for example, the
<a href="/crtm/user_interface/public_structures/rtsolution.shtml"><tt>RTSolution</tt> structure</a>
in general, or the
<a href="/crtm/user_interface/public_structures/atmosphere.shtml"><tt>Atmosphere</tt> structure</a>
when it is used as a K-matrix variable. In these cases, the channel dimension is one of the ranks of the structure array.



<h3><a name="optalloc"></a>Allocation of <code>Options</code> structures</h3>

<p>The current calling sequence for the <tt>Options</tt> structure allocation function is,

<pre>
  Error_Status = CRTM_Allocate_Options( n_Channels             , &  ! Input
                                        Options                , &  ! Output
                                        RCS_Id     =RCS_Id     , &  ! Revision control
                                        Message_Log=Message_Log  )  ! Error messaging
</pre>

<p>Because the contents of the <tt>Options</tt> structure may change in the future as additional optional inputs
are added, the dimensionality of those contents may require the above interface to be altered by adding the required
dimension arguments.

<p>The <tt>Options</tt> allocation function has been overloaded such that the <tt>Options</tt> argument can be
scalar or rank-1. The allowed input argument dimensioality for the <code>CRTM_Allocate_Options()</code>
function is shown below,

<div class="tablecontent2">
  <table cellspacing="0" cellpadding="0" border="0">
	 <caption>Allowable dimensionality combinations for the <tt>CRTM_Allocate_Options()</tt> function
           <br><tt>M</tt> = number of atmospheric profiles.
  </caption>
  <tbody>
    <tr align="center">
      <th>Input<br><tt>n_Channels</tt><br>dimension</th>
      <th>Output<br><tt>Options</tt><br>dimension</th>
    </tr>
    <tr align="center"><td>Scalar    </td><td>Scalar    </td></tr>
    <tr align="center"><td>Scalar    </td><td><tt>M</tt></td></tr>
  </tbody>
</table>
</div>

<p>As stated above, the rank-1 dimensionality of the <tt>Options</tt> structure array is what is required
for all the CRTM functions.

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
