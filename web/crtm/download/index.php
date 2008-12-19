<?php
/*
Version 1.1

==========================================================
TO DO LIST:
* use getDbAuth for MySQL username and password

Log:
2007 February 21
----------------
* copied this from http://cimss.ssec.wisc.edu/iremis/ and modified appropriately

==========================================================
*/

include('./download-include.php');

// CLEAN ENVIRONMENTAL/SERVER VARIABLES
$_SERVER['PHP_SELF'] = htmlentities($_SERVER['PHP_SELF']);

// START
printHeader('Community Radiative Transfer Model (CRTM) — Download');

// MAIN PROGRAM LOGIC
if ( isset($_COOKIE['uid']) ) { // user has a cookie
//if (1 == 1) { // DEBUG
  if ( validateInput('uid') && userIsInDb($_COOKIE['uid'], NULL) ) { // user has a valid uid in cookie and is in database
//  if (1 == 1) {// DEBUG
    if ( addDbDownloadRecord($_COOKIE['uid']) ) {
//    if (1 == 1) {// DEBUG
      printDownloadPage();
    } else {
      echo 'ERROR: could not log your automatic download attempt.  Please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
    }
  } else {
    echo 'ERROR: You are not a valid user or there was a problem with your automatic authentication.  Please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
  }
} else if ( isset($_POST['submit_new']) ) { // user is new and has submitted their registration info
  if ( validateInput('submit_new') ) {
    $uid = insertDbRegisterRecord();
    if ( is_int($uid) && addDbDownloadRecord($uid) ) {
      writeJavaScriptCookie($uid);
      sendConfirmation();
      printDownloadPage();
    } else {
      echo 'ERROR: could not log your registration and download attempt.  Please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
    }
  } else {
    echo 'ERROR: invalid attempt to register.  Contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
  }
} else if ( isset($_POST['login']) ) { // user is trying to log in
  if ( validateInput('login') && userIsInDb(NULL, $_POST['email']) ) {
    $uid = getUID($_POST['email']);
    if ( addDbDownloadRecord($uid) ) {
      writeJavaScriptCookie($uid);
      printDownloadPage();
    } else {
      echo 'ERROR: could not log your manual download attempt.  Please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
    }
  } else {
    echo 'ERROR: You are not a valid user or you mistyped your email address (which should be the email address you registered with and which is case-sensitive).  If you have not registered yet, then please do so by filling out the form in the &quot;New Users&quot; section.  If you need help, please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
  }
} else { // user is new and has not submitted their registration info yet
  printForm();
}

printFooter();
  

//////////////////////////////// FUNCTIONS /////////////////////////////////

//------------------------------------------------------------------
function sendConfirmation() {
//  $cc = 'billb@ssec.wisc.edu'; // DEBUG
  $cc = 'Paul.Vandelst@noaa.gov';
  $from = $cc;

  $ml = 'No, do not add me to the CIMSS CRTM';
  if ($_POST['mailinglist'] == 'yes') {
    $ml = 'Yes, please add me to the CIMSS CRTM';
  } 

  $message = "You have successfully registered to gain access to the CIMSS Community Radiative Transfer Model (CRTM).
Name:      " . $_POST['fname'] . " " . $_POST['lname'] . "
Organization:      " . $_POST['organization'] . "
Mailing List:      " . $ml . '

To view the CRTM download page in the future or to download updates when they become available, please visit our download
page at:
            http://cimss.ssec.wisc.edu/crtm/download/
If you are using a different computer than what you used originally or do not have cookies and JavaScript enabled, then you
will need to login with your email address.

If you have chosen to subscribe to the CIMSS CRTM Mailing List, you can
unsubscribe yourself from this list via the instructions on this page:
           http://cimss.ssec.wisc.edu/crtm/download/notes.php?note=list
Please note that it can take up to 48 hours for you to become subscribed to the mailing list when you register if you
have elected to join the mailing list.

If you have any questions, please respond back to this email.';

  // In case any of our lines are larger than 70 characters, we should use wordwrap()
  $message = wordwrap($message, 70);

  // subject
  $subject = 'CIMSS CRTM Download';

  // other header info
  $headers = 'From: ' . $from . "\r\n" .
   'Reply-To: ' . $from . "\r\n" .
   'cc: ' . $cc . "\r\n" .
   'X-Mailer: PHP/' . phpversion();

  // Send
  mail($_POST['email'], $subject, $message, $headers, '-f' . $from);
}

//------------------------------------------------------------------
function getUID($email) {
  global $dbInfo, $fieldNames;

  // connect to database
  $link = mysql_connect('localhost:3306', $dbInfo['username'], $dbInfo['password'])
     or die('Could not connect: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  mysql_select_db($dbInfo['db']) or die('Could not select database');
  $date_time = date("Y-m-d_G:i:s");

  if ( $email != '' ) {
    $sql = "SELECT uid FROM " . $dbInfo['table-register'] . " WHERE email = '" . addslashes($email) . "'";
  } else {
    die ('ERROR: null email address - contact SSEC Webmaster');
  }

  $result = mysql_db_query($dbInfo['db'], $sql);
  if (!$result) {
    die('ERROR: could not find valid id for given email address - contact SSEC Webmaster');
  }
  
  $uid = mysql_result($result, 0);

  // close database connection
  mysql_close($link);

  return $uid;
}

//------------------------------------------------------------------
function printDownloadPage() {
  include('/var/apache/cimss/htdocs/crtm/download/download.html');
}

//------------------------------------------------------------------
/*function printAutoForward($otherText) {
  $url = 'ftp://ftp.ssec.wisc.edu/pub/g_emis/';

  if ( isset($otherText) ) {
    echo '<p>' . $otherText . "</p>\n";
  }

  echo '<p id="inlineNote">If you do not see any content in the box below, then most likely you are
            using Internet Explorer and must add the current URL to your list of Trusted Sites
            <span style="white-space: nowrap;">(Tools -&gt; Internet Options -&gt; Security).</span>  If you continue to 
        have problems, please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a></p>' . "\n";

      echo '<ul style="font-size: 12px;">
        <li><a href="./notes.php?note=list">instructions on how to unsubscribe yourself from the CRTM mailing list</a></li>
        <li><a href="./notes.php?note=privacy">privacy policy</a></li>
      </ul>' . "\n";

  echo '<iframe src="'.$url.'" id="inlineContent">  </iframe>';

  return;
}*/

//------------------------------------------------------------------
function userIsInDb($uid, $email) {
  global $dbInfo, $fieldNames;

  // look up uid in database to verify user is a valid user

  // connect to database
  $link = mysql_connect('localhost:3306', $dbInfo['username'], $dbInfo['password'])
     or die('Could not connect: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  mysql_select_db($dbInfo['db']) or die('Could not select database');
  $date_time = date("Y-m-d_G:i:s");

  if ( !is_null($uid) ) {
    $sql = "SELECT uid FROM " . $dbInfo['table-register'] . " WHERE uid = " . addslashes($uid);
  } else if ( !is_null($email) ) {
    $sql = "SELECT email FROM " . $dbInfo['table-register'] . " WHERE email = '" . addslashes($email) . "'";
  } else {
    mysql_close($link);
    return false;
  }

  if ( mysql_numrows(mysql_db_query($dbInfo['db'], $sql)) == 1 ) {
    return true;
  } 

  // close database connection
  mysql_close($link);
  return false;
}

//------------------------------------------------------------------
function getIP() {
  if ($_SERVER["HTTP_X_FORWARDED_FOR"]) {
    $ip = $_SERVER["HTTP_X_FORWARDED_FOR"];
  } if ($_SERVER["HTTP_CLIENT_IP"]) {
    $ip = $_SERVER["HTTP_CLIENT_IP"];
  } else {
    $ip = $_SERVER["REMOTE_ADDR"]; 
  }

  if ( $ip != '') {
    return $ip;
  } else {
    return 'NULL';
  }
}

//------------------------------------------------------------------
function getUserAgent() {
  $useragent = $_SERVER['HTTP_USER_AGENT'];
  if ($useragent != '') {
    return $useragent;
  } else {
    return 'NULL';
  }
}

//------------------------------------------------------------------
function addDbDownloadRecord($uid) {
  global $dbInfo, $fieldNames;

  // connect to database
  $link = mysql_connect('localhost:3306', $dbInfo['username'], $dbInfo['password'])
     or die('Could not connect: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  mysql_select_db($dbInfo['db']) or die('Could not select database');
  $date_time = date("Y-m-d_G:i:s");

  // make SQL statement
  $fieldValues[0] = 'NULL';
  $fieldValues[1] = "'" . addslashes(htmlspecialchars($uid)) . "'";
  $fieldValues[2] = "'" . addslashes(htmlspecialchars($date_time)) . "'";
  $fieldValues[3] = "'" . addslashes(htmlspecialchars( getIP() )) . "'";
  $fieldValues[4] = "'" . addslashes(htmlspecialchars( getUserAgent() )) . "'";

  $sql_1 = $dbInfo['table-download'] . "(" .implode(',', $fieldNames['table-download']) . ")";
  $sql_2 = " VALUES(" . implode(',', $fieldValues) . ")";
  $sql = "INSERT INTO " . $sql_1 . $sql_2;
  $sql = preg_replace("/\'NULL\'/",'NULL', $sql);  // a quick and dirty way of unquoting NULL
//    echo '<p>insert SQL: '. $sql . "</p>\n";

  // execute SQL statement
  if ( mysql_query($sql) ) {
    mysql_close($link);
    return true;
  } else {
    mysql_close($link);
    return false;
  }
}

//------------------------------------------------------------------
function insertDbRegisterRecord() {
  global $dbInfo, $fieldNames;
  /*
  - Insert a registration record for user into registration table - there should only be one
    record per user in the registration table.
  - Assume that the user will then be successfully forwarded to the dataset - log this by inserting
    a record in the download table.  each download attempt should be logged in the download table.
  - uid is the field that connects these two tables
  */

  // connect to database
  $link = mysql_connect('localhost:3306', $dbInfo['username'], $dbInfo['password'])
     or die('Could not connect: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  mysql_select_db($dbInfo['db']) or die('Could not select database');
  $date_time = date("Y-m-d_G:i:s");

  // generate random, unique uid
  $uid = makeUID();

  if ( is_int($uid) ) {
    // make SQL statement
    $fieldValues[0] = "'" . addslashes(htmlspecialchars($uid)) . "'";
    $fieldValues[1] = "'" . addslashes(htmlspecialchars($date_time)) . "'";
    for ($i=2; $i < count($fieldNames['table-register']); $i++) {
      $fieldValues[$i] = "'" . addslashes(htmlspecialchars($_POST[$fieldNames['table-register'][$i]])) . "'";
    }
    $sql_1 = $dbInfo['table-register'] . "(" .implode(',', $fieldNames['table-register']) . ")";
    $sql_2 = " VALUES(" . implode(',', $fieldValues) . ")";
    $sql = "INSERT INTO " . $sql_1 . $sql_2;
    $sql = preg_replace("/\'NULL\'/",'NULL', $sql);  // a quick and dirty way of unquoting NULL

    // execute SQL statement
    $result = mysql_query($sql) or die('Query failed: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  } 
  
  // closes database connection
  mysql_close($link);

  return $uid;
}

//------------------------------------------------------------------
function makeUID() {
  global $dbInfo;

  srand((double)microtime()*1000000);
  $test_id = rand(1000000,9999999);
  $checkid = 1;
  $maxAttempts = 100;

  for ($i=0; $i < $maxAttempts; $i++) {
    $uid = rand(1000000000,9999999999);
    $sql = "SELECT uid FROM " . $dbInfo['table-register'] . " WHERE uid = " . $uid;
    $checkid = mysql_numrows(mysql_db_query($dbInfo['db'], $sql));

    if ($checkid == 0) { 
      return $uid;
    } 
  } 

  return 'ERROR';
}

//------------------------------------------------------------------
function validateInput($control) {
  global $maxFieldLength;
  $errors = array();
  $inputIsValid = true;

  // $control = 'uid' || 'submit_new' || 'login'
  switch ($control) {
  case 'uid': // user has a cookie
    $_COOKIE['uid'] = trim($_COOKIE['uid']); 
    if ($_COOKIE['uid'] != '') {
      preg_match('/([0-9]{1,11})/', $_COOKIE['uid'], $matches);
      if (count($matches) > 0) {
        $_COOKIE['uid'] = $matches[1];
      }  else {
        $errors[] = 'invalid cookie - contact SSEC Webmaster';
      }
    }
    break;

  case 'submit_new':
    $_POST['fname'] = trim($_POST['fname']); 
    if ($_POST['fname'] != '') {
      preg_match('/^[ \t]*([a-zA-Z][a-zA-Z0-9 \.\-\'\,]{1,'.$maxFieldLength['fname'].'})[ \t]*$/', $_POST['fname'], $matches);
      if (count($matches) > 0) {
        $_POST['fname'] = $matches[1];
      }  else {
        $errors[] = 'invalid input parameter: "First Name".  Only alphanumeric characters, periods, spaces, dashes, single quotes, and commas are allowed or submitted field was too long.';
      }
    } else {
      $errors[] = 'You must enter in a valid "First Name".';
    }

    $_POST['lname'] = trim($_POST['lname']); 
    if ($_POST['lname'] != '') {
      preg_match('/^[ \t]*([a-zA-Z][a-zA-Z0-9 \.\-\'\,]{1,'.$maxFieldLength['lname'].'})[ \t]*$/', $_POST['lname'], $matches);
      if (count($matches) > 0) {
        $_POST['lname'] = $matches[1];
      }  else {
        $errors[] = 'invalid input parameter: "Last Name".  Only alphanumeric characters, periods, spaces, dashes, single quotes, and commas are allowed.  Or the submitted field was too long.';
      }
    } else {
      $errors[] = 'You must enter in a valid "Last Name".';
    }

    $_POST['organization'] = trim($_POST['organization']); 
    if ($_POST['organization'] != '') {
      preg_match('/^[ \t]*([a-zA-Z0-9 \.\-:\/\(\)\'\,\;\"]{1,'.$maxFieldLength['organization'].'})[ \t]*$/', $_POST['organization'], $matches);
      if (count($matches) > 0) {
        $_POST['organization'] = $matches[1];
      }  else {
        $errors[] = 'invalid input parameter: "Organization".  Only alphanumeric characters, periods, spaces, dashes, single quotes, and commas are allowed. Or the submitted field was too long.';
      }
    } else {
      $errors[] = 'You must enter in a valid "Organization".';
    }

    $_POST['email'] = trim($_POST['email']); 
    if ($_POST['email'] != '') {
      preg_match('/^[ \t]*(([\w][\w\,\-\.]*\@[\w]+[\w\-\.]*){1,'.$maxFieldLength['email'].'})[ \t]*$/', $_POST['email'], $matches);
      if (count($matches) > 0) {
        $_POST['email'] = $matches[1];
      }  else {
        $errors[] = 'invalid input parameter: "Email".  Only alphanumeric characters, periods, dashes, and commas are allowed.  Or the submitted field was too long.';
      }
    } else {
      $errors[] = 'You must enter in a valid "Email".';
    }

    $_POST['purpose'] = trim($_POST['purpose']); 
    if ($_POST['purpose'] != '') {
      preg_match('/^.{1,'.$maxFieldLength['purpose'].'}$/s', $_POST['purpose'], $matches);
      if (count($matches) > 0) {
        $_POST['purpose'] = $matches[0];
        $_POST['purpose'] = htmlentities($_POST['purpose']);
      }  else {
        $errors[] = 'invalid input parameter: "How do you intend to use this dataset".  The submitted field was too long or started with a non-alphanumeric character.';
      }
    }

    $_POST['mailinglist'] = trim($_POST['mailinglist']); 
    if ($_POST['mailinglist'] != '') {
      preg_match('/^yes$/', $_POST['mailinglist'], $matches);
      if (count($matches) > 0) {
        $_POST['mailinglist'] = $matches[0];
      }  else {
        $errors[] = 'invalid input parameter: "Mailing List".  Please contact the SSEC Webmaster.';
      }
    } else {
      $_POST['mailinglist'] = 'no';
    }
    break;

  case 'login':
    $_POST['email'] = trim($_POST['email']); 
    if ($_POST['email'] != '') {
      preg_match('/^[ \t]*(([\w][\w\,\-\.]*\@[\w]+[\w\-\.]*){1,'.$maxFieldLength['email'].'})[ \t]*$/', $_POST['email'], $matches);
      if (count($matches) > 0) {
        $_POST['email'] = $matches[1];
      }  else {
        $errors[] = 'invalid input parameter: "Email".  Only alphanumeric characters, periods, dashes, and commas are allowed.  Or the submitted field was too long.';
      }
    } else {
      $errors[] = 'You must enter in a valid "Email".';
    }
    break;

  default:
    // do something by default here
  }

  // print errors 
  if (count($errors) > 0) {
    $inputIsValid = false;
    echo 'ERRORS were encountered - if you need help, please contact the <a href="http://www.ssec.wisc.edu/cgi-bin/email_form.cgi?name=SSEC%20Webmaster">SSEC Webmaster</a>';
    echo '<ul>';
    for ($i=0; $i < count($errors); $i++) {
      echo "<li>" . $errors[$i] . "</li>\n";
    }
    echo '</ul>';
  }

  return $inputIsValid;
}

//------------------------------------------------------------------
function writeJavaScriptCookie($uid) {
  echo '<script language="javascript" type="text/javascript">setCookie("uid",' . $uid . ',"","");</script>';
  return;
}

//------------------------------------------------------------------
function printForm() {
  global $maxFieldLength, $fieldLength;
?>
<form name="loginform" id="loginform" action="<?php echo $_SERVER['PHP_SELF'];?>" method="post">
  <h2 class="title">Login to download data:<span class="notes">(*)</span></h2>
  <table width="100%"  border="0" cellspacing="0" cellpadding="0" style="margin-top: 0em;" id="loginTable">
    <tr>
      <td><span class="field">Email Address:</span>
        <input type="text" name="email" id="email1" size="<?php echo $fieldLength['email'];?>" maxlength="<?php echo $maxFieldLength['email'];?>"> <span class="required" style="color: #333333;">(case sensitive)</span></td>
    </tr>
    <tr>
      <td align="center"><input type="submit" id="login" name="login" value="Login and begin download"></td>
    </tr>
  </table>
  <p style="font-size: 12px; color: #666666;"><span class="notes">*</span>
    If you have already registered and you are seeing this page, <span style="color: #000000; font-weight: bold;">then you do not have cookies or JavaScript enabled or you are using a different computer than the one
    that you registered with.</span><br>  You can manually log in by entering in (in the above form) the email address that you registered with.
    When you register (or when you login 
    by entering in your email address above), a cookie should be
    stored on your computer so that you can automatically bypass this step, IF you have JavaScript and cookies enabled.</p>
</form>
<form name="newuserform" id="newuserform" action="<?php echo $_SERVER['PHP_SELF'];?>" method="post">
  <div class="title">
    <h2>New Users:</h2>
    <p style="margin: 0px; padding-bottom: 3px; ">Please register to gain access
      to this dataset. We use this information for planning purposes. Please
      see our <a href="./notes.php?note=privacy">privacy policy</a>.</p>
  </div>
  <table cellspacing="0" cellpadding="0" id="registerTable">
    <tr>
      <td rowspan="2" valign="top" class="field">First Name:</td>
      <td style="padding-bottom: 0px;"><input type="text" name="fname" id="fname" size="<?php echo $fieldLength['fname'];?>" maxlength="<?php echo $maxFieldLength['fname'];?>"></td>
      <td rowspan="2" align="right" valign="top" style="padding-bottom: 0px;" class="field" >Last Name:</td>
      <td style="padding-bottom: 0px;"><input type="text" name="lname" id="lname" size="<?php echo $fieldLength['lname'];?>" maxlength="<?php echo $maxFieldLength['lname'];?>"></td>
    </tr>
    <tr>
      <td><span class="required">(required)</span></td>
      <td><span class="required">(required)</span></td>
    </tr>
    <tr>
      <td class="field" valign="top">Organization:</td>
      <td colspan="3"><input type="text" name="organization" id="organization" size="<?php echo $fieldLength['organization'];?>" maxlength="<?php echo $maxFieldLength['organization'];?>"><br><span class="required">(required)</span></td>
    </tr>
    <tr>
      <td class="field">Email Address:</td>
      <td colspan="3"><span style="white-space: nowrap;"><input type="text" name="email" id="email2" size="<?php echo $fieldLength['email'];?>" maxlength="<?php echo $maxFieldLength['email'];?>">&nbsp;<span style="color: #333333;" class="required">(case sensitive)</span></span><br><span class="required">(required)</span></td>
    </tr>
    <tr>
      <td valign="top" class="field">How do you intend to use this dataset:</td>
      <td colspan="3"><textarea name="purpose" id="purpose" cols="60" rows="5" onKeyDown="textCounter(this.form.purpose,this.form.remLen,<?php echo $maxFieldLength['purpose']; ?>);"
                       onKeyUp="textCounter(this.form.purpose,this.form.remLen, <?php echo $maxFieldLength['purpose']; ?>);"></textarea><br>
                       <input type="text" readonly  maxlength="3" size="3" value="<?php echo $maxFieldLength['purpose']; ?>" name="remLen" id="remLen"> characters left</td>
    </tr>
    <tr>
      <td valign="top" class="field">Mailing List:</td>
      <td colspan="3" style="padding-bottom: 0px;"><table width="100%"  border="0" cellspacing="0" cellpadding="0">
          <tr>
            <td valign="top" align="left" style="padding-bottom: 0px;"><input type="checkbox" name="mailinglist" id="mailinglist" value="yes"></td>
            <td valign="top" align="left" style="padding-left: 5px; padding-bottom: 0px;"><p style="margin-top: 0px;">Please
                add me to the CIMSS CRTM mailing list so that I
                may receive updates and information regarding this dataset.</p></td>
          </tr>
        </table>
        <p style="margin-top: 0px;">You will be emailed a confirmation email
          when you submit this form that will include instructions on <a href="./notes.php?note=list">how
          to unsubscribe</a> yourself from this list if you decide that you no
          longer wish to receive emails.</p></td>
    </tr>
    <tr>
      <td colspan="4" align="center" style="padding-top: 1em; "><input type="submit" name="submit_new" id="submit_new" value="Register and begin download"></td>
    </tr>
  </table>
</form>
<?php
}
?>
