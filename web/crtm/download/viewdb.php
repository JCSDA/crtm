<?php 
include('./download-include.php');

$paramTitles = array("# of downloads", "uid", "First Name", 
                      "Last Name", "Date/time", "Email", "Organization", "Purpose", "Mailing List",
                      "IP", "User Agent");

printHeader('View / download Global Emissivity Dataset Download Statistics', '#secondarynav {display: none !important;');
printContent();
printFooter();

//////////////////////////////// FUNCTIONS /////////////////////////////////

//----------------------------------------------------------
function makecsvrecord($record) {
  for ($i=0; $i < count($record); $i++) {
    $record[$i] = preg_replace('/"/', '\\"', $record[$i]); 
    $record[$i] = preg_replace('/\\"/', '""', $record[$i]); 
    $record[$i] = '"'.$record[$i].'"';
  }

  return implode(',', $record);
}

//----------------------------------------------------------
function printContent() {
?>
  <p style="background-color: white; border: solid 1px #DBEAF5; width: 800px; margin: 10px 10px 10px 10px; padding: 5px; color: black;">
  <span style="color: #999999; text-decoration: underline;">Database in csv format (NOT ENABLED YET)</span> - can be opened in Excel or similar program (wait for table to finish loading before viewing/downloading).</p>
<?php
  printTable();
}

//----------------------------------------------------------
function printTable() {
  global $paramTitles, $dbInfo;


  echo '<table cellspacing="0" id="datatable"><tr>' . "\n";
  echo '<th scope="col">View Download Records</th>';
  for ($i=0; $i < count($paramTitles); $i++) {
    echo '<th class="'.$paramTitles[$i].'" scope="col">'.$paramTitles[$i]."</th>";
  }
  echo "</tr>\n";

  // connect to database
  $link = mysql_connect('localhost:3306', $dbInfo['username'], $dbInfo['password'])
     or die('Could not connect: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  mysql_select_db($dbInfo['db']) or die('Could not select database');

  $t1 = $dbInfo['table-register'];
  $t2 = $dbInfo['table-download'];
  $sql1 = "SELECT COUNT(*), $t2.uid, $t1.fname, $t1.lname, $t1.date_time, $t1.email, $t1.organization, $t1.purpose, $t1.mailinglist, $t2.IP, $t2.user_agent ";
  $sql2 = "FROM $t1, $t2 WHERE $t1.uid=$t2.uid GROUP BY $t2.uid";
  $sql = $sql1 . $sql2;
//  echo "<p>".$sql."</p>\n";

  if ($result = mysql_query($sql)) {
    $rownumber = 1;
    if (mysql_num_rows($result) > 0) {
      while ($record = mysql_fetch_array($result, MYSQL_NUM)) {
        if ($rownumber % 2 == 1) {
          echo '<tr class="altrow">' . "\n";
        } else {
          echo "<tr>\n";
        }
        echo '<td><input type="button" name="viewrecords" value="Not Enabled Yet" disabled>'."</td>\n";
        for ($i=0; $i < count($record); $i++) {
          echo '<td><div>'.$record[$i]."</div></td>";
        }
        echo "</tr>\n";
        $rownumber++;
      } 
    }
    mysql_close($link);
  } else {
    die('Query failed: ' . htmlspecialchars(mysql_error(), ENT_QUOTES));
  }
  echo '</table>';
}
?>
