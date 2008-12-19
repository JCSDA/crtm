<?php
// Version 1.1

include('/var/apache/cimss/htdocs/webcode/php-includes/db.php');

// INITIALIZE GLOBAL VARIABLES
$maxFieldLength['email'] = 60;
$maxFieldLength['fname'] = 40;
$maxFieldLength['lname'] = 40;
$maxFieldLength['organization'] = 255;
$maxFieldLength['purpose'] = 500;

$fieldLength['email'] = 30;
$fieldLength['fname'] = 30;
$fieldLength['lname'] = 30;
$fieldLength['organization'] = 81;
$fieldLength['purpose'] = 500;

/*
$dbInfo['username'] = 'use PHP code';
$dbInfo['password'] = 'billb@ssec';
*/
$db = new Db();
$dbInfo['username'] = $db->username;
$dbInfo['password'] = $db->password;

$dbInfo['db'] = 'ssec_general';
$dbInfo['table-register'] = 'crtm_register';
$dbInfo['table-download'] = 'crtm_download';

$fieldNames['table-register'] = array('uid',
                                      'date_time',
                                      'fname',
                                      'lname',
                                      'email',
                                      'organization',
                                      'purpose',
                                      'mailinglist');
$fieldNames['table-download'] = array('did',
                                      'uid',
                                      'date_time',
                                      'IP',
                                      'user_agent');

//////////////////////////////// FUNCTIONS /////////////////////////////////

//------------------------------------------------------------------
function printHeader($title, $style='') {
?>
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN"
"http://www.w3.org/TR/html4/loose.dtd">
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">
<title><?php echo $title;?></title>

<meta http-equiv="expires" content="0">
<link href="/stylesheets/style.css" type="text/css" rel="stylesheet">
<link href="/stylesheets/download.css" type="text/css" rel="stylesheet">
<!-- DO NOT DELETE -->
<!--[if IE]>
  <link href="/crtm/stylesheets/style.ie.css" type="text/css" rel="stylesheet">
<![endif]-->
<script language="javascript" type="text/javascript" src="/webcode/eventutil.js"></script>
<script language="javascript" type="text/javascript" src="/webcode/cookie.js"></script>
<script language="javascript" type="text/javascript" src="/crtm/javascript/javascript.js"></script>
<script language="javascript" type="text/javascript">
//----------------------------------------------------
function textCounter(field, countfield, maxlimit) {
  if (field.value.length > maxlimit) {// if too long...trim it!
     field.value = field.value.substring(0, maxlimit);
  } else { // otherwise, update 'characters left' counter
    countfield.value = maxlimit - field.value.length;
  }
}
</script>
<style type="text/css">
  <?php echo $style."\n";?>
</style>

</head>
<body>
<div id="header">
  <h1><a href="http://www.jcsda.noaa.gov"><img src="/images/jcsda_logo_small.png" alt="" name="jcsda_logo" width="125" height="125" border="0" align="middle"></a>Community Radiative Transfer Model (CRTM)</h1>
  <div id="mainnav">
    <?php include('/var/apache/cimss/htdocs/crtm/ssi/mainnav.ssi.html');?>
  </div>
</div>
<table cellspacing="0" cellpadding="0" id="pageLayoutTable">
  <tr>
    <td id="secondarynav"><div>
           <?php include('/var/apache/cimss/htdocs/crtm/ssi/download.ssi.html');?>
           </div><div class="spacer">&nbsp;</div></td>
    <td id="content">
<?php
}

//------------------------------------------------------------------
function printFooter() {
?>
<input type="hidden" name="category" id="category" value="download">
      <div id="footer"> Last updated
        <!-- #BeginDate format:En2 -->19-Dec-2008<!-- #EndDate -->
        by <a href="mailto:paul.vandelst@noaa.gov">CRTM Download</a>Paul van Delst</a></div></td>
  </tr>
</table>
</body>
</html>
<?php
}



?>