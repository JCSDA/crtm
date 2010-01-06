var html;           // accumulated results page
var searchString;   // original search string
var nResults = 0;   // number of search results
var searchTerms;    // array of words in search string

// indices into a libdata entry
var URL          = 0;
var NAME         = 1;
var TYPE         = 2;
var FILENAME     = 3;
var AUTHORS      = 4;
var ROUTINE_NAME = 5;
var COMMENTS     = 6;
var PARAMETERS   = 7;
var CATEGORIES   = 8;
var ATTRIBUTES   = 9;
var MATCH_TYPE   = 10;
var N_MATCHES    = 11;
var SCORE        = 12;
var MATCHES      = 13;

var SCORE_VALUES = new Array(0, 0, 0, 8, 6, 10, 4, 6, 8, 4);


/*
   Find results from the search.
*/



//  Returns true for a-zA-Z0-9 and &, false otherwise.
function isAlnum(ch) {
  if ((ch >= "a" && ch <= "z") || (ch == "&") || (ch >= "A" && ch <= "Z") || (ch >= "0" && ch <="9")) {
    return true;
  } else {
    return false;
  }
}


// find all the matches in a single item
function searchElement(item, matchType, upperSearchString) {
  var element = libdata[item][matchType].toUpperCase();
  var pos, origPos = 0;
  
  libdata[item][N_MATCHES] = 0;
  pos = element.indexOf(upperSearchString);
  
  while (pos >= 0) {
    origPos += pos + 1;
    
    libdata[item][MATCHES + libdata[item][N_MATCHES]] = origPos - 1;
    libdata[item][N_MATCHES]++;

    element = element.substring(pos + 1, element.length);
    while (isAlnum(element.charAt(0)) && element.length > 0) {
      element = element.substring(1, element.length);
      origPos++;
    }
    
    pos = element.indexOf(upperSearchString);
  }  
}


function searchItem(item, upperSearchString) {
  var matchType = TYPE;
  
  //html += "Searching " + item + "<br/>";
  
  // mark item as not matching
  libdata[item][MATCH_TYPE] = -1;
  libdata[item][SCORE] = 0;
  
  // search FILENAME, AUTHORS, ROUTINE_NAME, COMMENTS, PARAMETERS, 
  //   CATEGORIES, and ATTRIBUTES fields
  while (++matchType <= ATTRIBUTES && libdata[item][MATCH_TYPE] == -1) {
    searchElement(item, matchType, upperSearchString);
    if (libdata[item][N_MATCHES] > 0) {
      libdata[item][MATCH_TYPE] = matchType;
    }
  }
}

function sortByScore(a, b) {
  return b[SCORE] - a[SCORE];
}


function sortResults() {
  for (item = 0; item < libdata.length; item++) {
    if (libdata[item][N_MATCHES] > 0) {
      matchType = libdata[item][MATCH_TYPE];
      typeMultiplier = SCORE_VALUES[matchType];      
      matchPercentage = libdata[item][N_MATCHES] * searchString.length / libdata[item][matchType].length;
      libdata[item][SCORE] = typeMultiplier * matchPercentage;
    }
  }
  
  libdata = libdata.sort(sortByScore);
}


function findResults() {
  upperSearchString = searchString.toUpperCase();
  for (var item = 0; item < libdata.length; item++) {
    searchItem(item, upperSearchString);
  }
  
  sortResults();
}


/*
   Create results web page. Results are written to the html variable
   and then sent to the browser.
*/


function putHeader() {
  html = "<html><head><title>Search results</title>";
  html += "<link rel=\"stylesheet\" type=\"text/css\" href=\"idldoc-resources/main.css\" />";

  html += "<style type=\"text/css\" media=\"all\">";
  html += "  span.score { color: black; }";
  html += "  span.context { color: #A0A0A0; }";
  html += "  span.term { color: black; background: yellow; }";
  html += "  li { margin-bottom: 0.5em; }";
  html += "</style>";
  
  html += "</head><body>";
  
  html += "<div class=\"header smaller\">";
  html += "<h1>" + title + "</h1>";
  html += "<h2>" + subtitle + "</h2>";  
  html += "</div";
  
  html += "<div class=\"content\">";
  html += "<h2>Search results for \"" + searchString + "\"</h2>";  
}


function putItem(item) {
  mType = libdata[item][MATCH_TYPE];
  width = Math.round(2 * libdata[item][SCORE]);
  
  html += "<li>";
  html += "<img src=\"idldoc-resources/searchbar.png\" height=\"10\" width=\"" + width + "\" />&nbsp;";
  html += "<a href=\"" + libdata[item][URL] + "\" target=\"main_frame\">" + libdata[item][NAME] + "</a><br/>";
  html += libdata[item][TYPE] + "<br/>";
  
  nPreCharacters = 25;
  nPostCharacters = 40;
  html += "<span class=\"context\">";
  if (libdata[item][MATCHES] > nPreCharacters) { 
    html += "...";
  }
  html += libdata[item][mType].substring(libdata[item][MATCHES] - nPreCharacters, libdata[item][MATCHES]);
  html += "<span class=\"term\">";
  html += libdata[item][mType].substring(libdata[item][MATCHES], libdata[item][MATCHES] + searchString.length);
  html += "</span>";
  html += libdata[item][mType].substring(libdata[item][MATCHES] + searchString.length, libdata[item][MATCHES] + searchString.length + nPostCharacters);
  if (libdata[item][MATCHES] + searchString.length + nPostCharacters < libdata[item][mType].length) {
    html += "...";
  }
  html += "</span><br/>";
  
  html += "<span class=\"score\">Score: " + Math.round(10 * libdata[item][SCORE]) / 10;
  plural = libdata[item][N_MATCHES] > 1 ? "es" : "";
  html += " - " + libdata[item][N_MATCHES] + " match" + plural + " in ";
  
  if (mType == 3) {
    type = "the filename";
  } else if (mType == 4) {
    type = "the author names";
  } else if (mType == 5) {
    type = "the routine name";
  } else if (mType == 6) {
    type = "the comments";
  } else if (mType == 7) {
    type = "the parameters";
  } else if (mType == 8) {
    type = "the categories";
  } else if (mType == 9) {
    type = "the attributes";
  } else {
    type = "an unspecified comment";
  }
     
  html += type + "</span></li>";
}


function putResults() {
  for (var item = 0; item < libdata.length; item++) {
    if (libdata[item][N_MATCHES] > 0) {
    nResults++;
    }
  }
  
  if (nResults > 0) {
    html += "<ol>";
  }
  
  for (var item = 0; item < libdata.length; item++) {
    if (libdata[item][N_MATCHES] > 0) {
      putItem(item);
    }
  }
  
  if (nResults > 0) {
    html += "</ol>";
  }
}


function putFooter() {
  var plural = nResults == 1 ? "" : "s";
  html += "<p>" + nResults + " item" + plural + " found.</p>"
  
  html += "</div></body></html>";
}


function writeResultsPage() {
  var htmlCode = html;
  
  iu = open("", "Object", "resizable=yes,scrollbars=yes,toolbar=no,menubar=no,location=no,directories=no,width=475,height=600");
  
  iu.document.open();
  iu.document.write(htmlCode);
  iu.document.close();
}


/* 
   Event handlers for forms on search page.
*/


function basicsearch() {
  searchString = document.basicForm.basicText.value;
  searchTerms = searchString.split(/\s/);
  
  putHeader();
    
  findResults();
  putResults();
  
  putFooter();
  
  writeResultsPage();
}


function advancedsearch() {
  routineName = document.advancedForm.routinename.value;
  comments = document.advancedForm.comments.value;
  parameters = document.advancedForm.parameters.value;
  authors = document.advancedForm.authors.value;
  
  alert("Advanced searching...\n\nRoutine name = " + routineName + "\nComments = " + comments + "\nParameters = " + parameters + "\nAuthors = " + authors);
}