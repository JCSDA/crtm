/* Index used for searching */
/*
   Fields used:
     url, name, type, filename, authors, routine name, comments, parameters,
     categories, and attributes
*/
title = "MGunit documentation";
subtitle = "Unit testing for IDL";
libdata = new Array();
libdataItem = 0;



libdata[libdataItem++] = new Array("./assert.html", "assert.pro", ".pro file in ./ directory", "assert.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./assert.html#assert", "assert", "routine in assert.pro", "assert.pro", "", "assert", " Raises an error if the given condition is not met. Uses logical_predicate  to determine truth of condition: so zero or null values are false, anything  else is true. Be careful of conditions like:    assert, not file_test(filename)   ", "condition       condition to assert  msg       message to throw if condition is not met  ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./error_is_fail.html", "error_is_fail.pro", ".pro file in ./ directory", "error_is_fail.pro", "", "", "", "", "          -1", "");
  

libdata[libdataItem++] = new Array("./error_is_pass.html", "error_is_pass.pro", ".pro file in ./ directory", "error_is_pass.pro", "", "", "", "", "          -1", "");
  

libdata[libdataItem++] = new Array("cmdline_tools/ls.html", "ls.pro", ".pro file in cmdline_tools/ directory", "ls.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/ls.html#ls_get_columns", "ls_get_columns", "routine in ls.pro", "ls.pro", "", "ls_get_columns", " Get number of columns in display.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("cmdline_tools/ls.html#ls_permissions", "ls_permissions", "routine in ls.pro", "ls.pro", "", "ls_permissions", " Returns the mode line for each file in the given list.    ", "infofiles       array of files  ", "          -1", "    strarr    ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/ls.html#ls_human_size", "ls_human_size", "routine in ls.pro", "ls.pro", "", "ls_human_size", " Return a human readable array of sizes using bytes, kilobytes, megabytes,  gigabytes, terabytes, and petabytes (in powers of two).    ", "sizes       array of sizes in bytes  ", "          -1", "    strarr    ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/ls.html#ls_modification_times", "ls_modification_times", "routine in ls.pro", "ls.pro", "", "ls_modification_times", " Convert modification times from long to normal date/time format.    ", "mtimes       array of modification times  ", "          -1", "    strarr    ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/ls.html#ls", "ls", "routine in ls.pro", "ls.pro", "", "ls", " Substitute for unix ls command. Automatically does -hF.    ", "all       report all files (even .*)  long       more information about each file is listed    pattern", "          -1", "    doesn't handle directories matching pattern the same as ls does; shows     year if it is not this year (ls shows year if it is not in the last six     months)  ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/man.html", "man.pro", ".pro file in cmdline_tools/ directory", "man.pro", "", "", " Prints basic information about a routine.    ", "", "          -1", "    Try the main-level example at the end of this file:    IDL&gt; .run man      This:    IDL&gt; man, 'congrid'   Filename: /Applications/itt/idl71/lib/congrid.pro   result = congrid(arr, x, y, z, CENTER=CENTER, CUBIC=CUBIC,     INTERP=INTERP, MINUS_ONE=MINUS_ONE)   IDL&gt; man, 'mg_*range*'   Filename: /Users/mgalloy/projects/idllib/trunk/src/indices/mg_makerange.pro   result = MG_MAKERANGE(startvalue, stopvalue, INCREMENT=INCREMENT, N=N)    Filename: /Users/mgalloy/projects/idllib/trunk/src/analysis/mg_range.pro   result = MG_RANGE(var)      mg_termcolumns    ");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man_resolveroutine", "man_resolveroutine", "routine in man.pro", "man.pro", "", "man_resolveroutine", " Routine to resolve a given routine without crashing.    ", "resolved       set to a named variable to find out if the routine was resolved  _extra       keywords to RESOLVE_ROUTINE  routine       name of routine to resolve    ", "          -1", "    1 if the routine was resolved, 0 if not    ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man_width", "man_width", "routine in man.pro", "man.pro", "", "man_width", " Wrapper for MG_TERMCOLUMNS in case it is not available.    ", "", "          -1", "    long  ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man_print", "man_print", "routine in man.pro", "man.pro", "", "man_print", " Print a string by splitting it across lines on spaces and indents every line  except the first using the value of the INDENT keyword.    ", "indent        string to prefix each line except the first  text", "          -1", "");
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man_checkroutine", "man_checkroutine", "routine in man.pro", "man.pro", "", "man_checkroutine", " Checks to see if routine is in list.    ", "list       list of routines to check against  routine       name of routine to check (case-insensitive)  ", "          -1", "    1 if routine in in list; 0 if not    ");
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man_routineinfo", "man_routineinfo", "routine in man.pro", "man.pro", "", "man_routineinfo", " Print comments about a routine.    ", "routine       routine name to look up  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("cmdline_tools/man.html#man", "man", "routine in man.pro", "man.pro", "", "man", " Print comments about a routine or finds matching routines.    ", "routine       routine name to look up  ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_ansicode.html", "mg_ansicode.pro", ".pro file in cmdline_tools/ directory", "mg_ansicode.pro", "", "", " Set ANSI escape codes for the given text.    ", "", "          -1", "    Support more codes, available at:    http://en.wikipedia.org/wiki/ANSI_escape_code  ");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_ansicode.html#mg_ansicode", "mg_ansicode", "routine in mg_ansicode.pro", "mg_ansicode.pro", "", "mg_ansicode", " Set ANSI color codes for the given text.    ", "boldbright       set to color foreground text to a brighter shade  black       set to color foreground text black  red       set to color foreground text red  greenyellowbluemagentacyanwhitebackground_brightbackground_blackbackground_redbackground_greenbackground_yellowbackground_bluebackground_magentabackground_cyanbackground_whitetext       text to colorize    ", "          -1", "    string/strarr    ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_breakpoint.html", "mg_breakpoint.pro", ".pro file in cmdline_tools/ directory", "mg_breakpoint.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_breakpoint.html#mg_breakpoint_getpath", "mg_breakpoint_getpath", "routine in mg_breakpoint.pro", "mg_breakpoint.pro", "", "mg_breakpoint_getpath", "", "name", "          -1", "");
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_breakpoint.html#mg_breakpoint", "mg_breakpoint", "routine in mg_breakpoint.pro", "mg_breakpoint.pro", "", "mg_breakpoint", " A helpful wrapper for BREAKPOINT which finds files in the !path and allows  relative line numbers within routines.    ", "routine       set to specify a routine name and a line number within the routine        definition  _extraname       name of file (with or without the .pro extension) or routine (when the        ROUTINE keyword is set)  line       line number with the file (normally) or routine (when ROUTINE keyword        is set)    ", "          -1", "    fix up line number when ROUTINE is set    ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_build_cmdline_tools.html", "mg_build_cmdline_tools.pro", ".pro file in cmdline_tools/ directory", "mg_build_cmdline_tools.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_build_cmdline_tools.html#mg_build_cmdline_tools", "mg_build_cmdline_tools", "routine in mg_build_cmdline_tools.pro", "mg_build_cmdline_tools.pro", "", "mg_build_cmdline_tools", " Build the cmdline_tools DLM.    ", "_extra       keywords to MG_MAKE_DLL  ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_build_dist_tools.html", "mg_build_dist_tools.pro", ".pro file in dist_tools/ directory", "mg_build_dist_tools.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_build_dist_tools.html#mg_build_dist_tools", "mg_build_dist_tools", "routine in mg_build_dist_tools.pro", "mg_build_dist_tools.pro", "", "mg_build_dist_tools", " Build the dist_tools DLM.  ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_cmp_version.html", "mg_cmp_version.pro", ".pro file in dist_tools/ directory", "mg_cmp_version.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_cmp_version.html#mg_cmp_version", "mg_cmp_version", "routine in mg_cmp_version.pro", "mg_cmp_version.pro", "", "mg_cmp_version", " Compares two version numbers for the more updated number. Returns 0 for  equal versions, 1 if version1 is later than version2, and -1 if version1 is  less than version2. Strings such as 'alpha' and 'beta' may be tacked on to  the end of a version, but are compared alphabetically.      ", "version1       first version number  version2       second version number  ", "          -1", "    For example, 1.2 is later than 1.1.2:    IDL&gt; print, mg_cmp_version('1.2', '1.1.2')          1       -1, 0, or 1    ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_cprint.html", "mg_cprint.pro", ".pro file in cmdline_tools/ directory", "mg_cprint.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_cprint.html#mg_cprint", "mg_cprint", "routine in mg_cprint.pro", "mg_cprint.pro", "", "mg_cprint", " PRINT statement which makes C format codes easier to use.    ", "_extra       keywords to PRINT  format       C format code    abcdefghijkl", "          -1", "    This should eventually be moved to a DLM so that an aribitrary number of     parameters can be used (or at least up to 665).    ");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_hasroutine.html", "mg_hasroutine.pro", ".pro file in dist_tools/ directory", "mg_hasroutine.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_hasroutine.html#mg_hasroutine_checkroutine", "mg_hasroutine_checkroutine", "routine in mg_hasroutine.pro", "mg_hasroutine.pro", "", "mg_hasroutine_checkroutine", " Checks to see if routine is in list.    ", "list       list of routines to check against  routine       name of routine to check (case-insensitive)  ", "          -1", "    1 if routine in in list; 0 if not    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_hasroutine.html#mg_hasroutine", "mg_hasroutine", "routine in mg_hasroutine.pro", "mg_hasroutine.pro", "", "mg_hasroutine", " Determine if a given routine name is available to call.    ", "is_system       set to a named variable to determine if the routine is a system        routine  is_function       set to a named variable to determine if the routine is a function  routine       routine name to look up    ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_idlversion.html", "mg_idlversion.pro", ".pro file in dist_tools/ directory", "mg_idlversion.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_idlversion.html#mg_idlversion", "mg_idlversion", "routine in mg_idlversion.pro", "mg_idlversion.pro", "", "mg_idlversion", " Returns the IDL version number as a string or a boolean indicating whether  a required version is met.    ", "require       IDL version required; if set, VIS_IDLVERSION returns a boolean of        whether the version requirement is met  ", "          -1", "    string version number or boolean    ");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_log.html", "mg_log.pro", ".pro file in dist_tools/ directory", "mg_log.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_log.html#mg_log", "mg_log", "routine in mg_log.pro", "mg_log.pro", "", "mg_log", " Messages are logged via this routine. Also, the LOGGER keyword returns the  logging object which is used to configure the logging.    ", "debug       set to specify the message as debug  informational       set to specify the message as informational  warning       set to specify the message as a warning  error       set to specify the message as an error  critical       set to specify the message as critical  logger       MGlogInfo object  quit       set to quit logging  msg       message to output; message is optional if the LOGGER keyword is passed        a named variable    ", "          -1", "    Use the LOGGER keyword to retrieve the logger object in order to set the     level or filename of output:    IDL&gt; mg_log, logger=logger   IDL&gt; logger-&gt;setProperty, level=3   ");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_log_common.html", "mg_log_common.pro", ".pro file in dist_tools/ directory", "mg_log_common.pro", "", "", "", "", "          -1", "");
  

libdata[libdataItem++] = new Array("dist_tools/mg_make_cl_wrapper.html", "mg_make_cl_wrapper.pro", ".pro file in dist_tools/ directory", "mg_make_cl_wrapper.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_make_cl_wrapper.html#mg_make_cl_wrapper", "mg_make_cl_wrapper", "routine in mg_make_cl_wrapper.pro", "mg_make_cl_wrapper.pro", "", "mg_make_cl_wrapper", " Create a UNIX wrapper script to call an IDL routine.    ", "location       directory to place the wrapper script  app_name       name of routine to call  script_name       basename of wrapper script    ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_make_dll.html", "mg_make_dll.pro", ".pro file in dist_tools/ directory", "mg_make_dll.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_make_dll.html#mg_make_dll", "mg_make_dll", "routine in mg_make_dll.pro", "mg_make_dll.pro", "", "mg_make_dll", " Wrapper for MAKE_DLL that handles input and output directories more  intelligently.    ", "_extra       keywords to MAKE_DLL  cfile       C filename to create DLL from    ", "          -1", "    IDL 7.1    ");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html", "mg_options__define.pro", ".pro file in dist_tools/ directory", "mg_options__define.pro", "", "", " An object to facilitate parsing of command line options.     Execute this program with something like:    idl -IDL_QUIET 1 -quiet -e  .run mg_options__define  -args --verbose--name=Mike   or:    idl -IDL_QUIET 1 -quiet -e  .run mg_options__define  -args --help  ", "", "          -1", "    IDL 6.2      handle the following cases: -nNAME, -n NAME, --name NAME     display other info afterward usage: and options: in the help        MGcoHashTable    ");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::setProperty", "mg_opt::setProperty", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::setProperty", " Set properties.  ", "short_name", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::getProperty", "mg_opt::getProperty", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::getProperty", " Get properties.  ", "short_namekey_column_widthhelp_header", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::isPresent", "mg_opt::isPresent", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::isPresent", " Returns whether the option has had a value set i.e. it is present on the  current command line.    ", "", "          -1", "    byte  ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::getHelp", "mg_opt::getHelp", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::getHelp", " Returns the help text for the option.    ", "", "          -1", "    string  ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::getValue", "mg_opt::getValue", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::getValue", " Get value of the option.    ", "present       set to a named variable to determine if the option is present  ", "          -1", "    string (normally) or byte (if boolean)    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::setValue", "mg_opt::setValue", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::setValue", " Set the value of the option.    ", "value       value of the option  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt::init", "mg_opt::init", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt::init", " Create an option.    ", "long_name       long name of the option  boolean       set to indicate the option is boolean i.e. it does not take a value,        being present  sets  it  help       help text to display for the option  default       default value of the option  ", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_opt__define", "mg_opt__define", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_opt__define", " Define instance variables.    ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::get", "mg_options::get", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::get", " Get value of option.    ", "params       set to return parameters  n_params       number of parameters returned, only used if PARAMS is set  present       set to a named variable to determine if the option was present  optname       long name of option    ", "          -1", "    string    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::displayHelp", "mg_options::displayHelp", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::displayHelp", " Display the help for the defined options.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::displayVersion", "mg_options::displayVersion", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::displayVersion", " Print version information.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::parseArgs", "mg_options::parseArgs", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::parseArgs", " Parse arguments.    ", "error_message       set to a named variable to receive any error message generated from        parsing the parameters  args       string array of arguments    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::addOption", "mg_options::addOption", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::addOption", " Add the definition of an option to the parser.    ", "help       help text for the option  default       default value  boolean       set to indicate the option is a boolean switch  longForm       long name of the option, used with two dashes i.e. --help  shortForm       single character name of an option, used with a single dash i.e. -h    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::addParams", "mg_options::addParams", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::addParams", " Add a range of positional parameters.    ", "nparamsRange       valid range for number of positional parameters, use -1 for the max        value to allow an unlimited number of parameters  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::cleanup", "mg_options::cleanup", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options::init", "mg_options::init", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options::init", " Create option parsing object.    ", "app_name       application name  version       version of the application  ", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mg_options__define.html#mg_options__define", "mg_options__define", "routine in mg_options__define.pro", "mg_options__define.pro", "", "mg_options__define", " Define instance variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_platform_extension.html", "mg_platform_extension.pro", ".pro file in dist_tools/ directory", "mg_platform_extension.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_platform_extension.html#mg_platform_extension", "mg_platform_extension", "routine in mg_platform_extension.pro", "mg_platform_extension.pro", "", "mg_platform_extension", " Returns the platform extension used by the PLATFORM_EXTENSION keyword to  MAKE_DLL.  ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_print.html", "mg_print.pro", ".pro file in cmdline_tools/ directory", "mg_print.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_print.html#mg_print", "mg_print", "routine in mg_print.pro", "mg_print.pro", "", "mg_print", " PRINT statement which makes C format codes easier to use.    ", "format       C format code  _extra       keywords to PRINT  abcdefghijkl", "          -1", "    This should eventually be moved to a DLM so that an aribitrary number of     parameters can be used (or at least up to 665).    ");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_resolveroutine.html", "mg_resolveroutine.pro", ".pro file in dist_tools/ directory", "mg_resolveroutine.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_resolveroutine.html#mg_resolveroutine", "mg_resolveroutine", "routine in mg_resolveroutine.pro", "mg_resolveroutine.pro", "", "mg_resolveroutine", " Routine to resolve a given routine without crashing.    ", "resolved       set to a named variable to find out if the routine was resolved  _extra       keywords to RESOLVE_ROUTINE  routine       name of routine to resolve    ", "          -1", "    1 if the routine was resolved, 0 if not    ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_set_path.html", "mg_set_path.pro", ".pro file in cmdline_tools/ directory", "mg_set_path.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_set_path.html#mg_set_path", "mg_set_path", "routine in mg_set_path.pro", "mg_set_path.pro", "", "mg_set_path", " Set the IDL path (!path) given an array of directories.    ", "dlm       set to set IDL_DLM_PATH instead of IDL_PATH  dirs       string array of directories in the path in the correct order; +,        &lt;IDL_DEFAULT&gt;, and other abbreviations used by EXPAND_PATH are legal;        elements that begin with  ;  are ignored    ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mg_src_root.html", "mg_src_root.pro", ".pro file in dist_tools/ directory", "mg_src_root.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mg_src_root.html#mg_src_root", "mg_src_root", "routine in mg_src_root.pro", "mg_src_root.pro", "", "mg_src_root", " Returns the absolute directory name (with a trailing slash) of the location  of the source code for the routine that called this function. Returns the  the current working directory (./) if called from the command line.    ", "", "          -1", "    IDL 6.2        string  ");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/mg_sysreport.html", "mg_sysreport.pro", ".pro file in cmdline_tools/ directory", "mg_sysreport.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/mg_sysreport.html#mg_sysreport", "mg_sysreport", "routine in mg_sysreport.pro", "mg_sysreport.pro", "", "mg_sysreport", " Prints system information.    ", "filename       if present, filename to send output to  ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html", "mgcoabstractiterator__define.pro", ".pro file in dist_tools/collection/ directory", "mgcoabstractiterator__define.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator::hasNext", "mgcoabstractiterator::hasNext", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator::hasNext", " Determine if the underlying collection has another element to retrieve.    ", "", "          -1", "    1 if underlying collection has another element, 0 otherwise  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator::next", "mgcoabstractiterator::next", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator::next", " Return the next item in the underlying collection.    ", "", "          -1", "    list item  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator::remove", "mgcoabstractiterator::remove", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator::remove", " Removes from the underlying MGArrayList the last element returned.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator::cleanup", "mgcoabstractiterator::cleanup", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator::cleanup", " Free resources of the iterator (not the underlying collection).  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator::init", "mgcoabstractiterator::init", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator::init", " Initialize an iterator.    ", "", "          -1", "    1 for success, 0 otherwise  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractiterator__define.html#mgcoabstractiterator__define", "mgcoabstractiterator__define", "routine in mgcoabstractiterator__define.pro", "mgcoabstractiterator__define.pro", "", "mgcoabstractiterator__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html", "mgcoabstractlist__define.pro", ".pro file in dist_tools/collection/ directory", "mgcoabstractlist__define.pro", "", "", " Abstract class to define a list interface. This class is not intended to be  instantiated, just to be inherited from.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::getProperty", "mgcoabstractlist::getProperty", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::getProperty", " Get properties.    ", "version       a counter that is incremented as the list is modified (so iterators        know if the underlying list has changed)  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::add", "mgcoabstractlist::add", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::add", " Add elements to the list.    ", "position       index to insert elements at (NOT IMPLEMENTED)  elements       scalar or vector array of the same type as the list    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::count", "mgcoabstractlist::count", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::count", " Returns the number of elements in the list.    ", "", "          -1", "    long integer  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::get", "mgcoabstractlist::get", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::get", " Get elements of the list.    ", "all       set to return all elements  position       set to an index or an index array of elements to return; defaults to 0        if ALL keyword not set  count       set to a named variable to get the number of elements returned by this        function  isa       classname(s) of objects to return; only allowable if list type is        object  ", "          -1", "    element(s) of the list or -1L if no elements to return    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::isContained", "mgcoabstractlist::isContained", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::isContained", " Determines whether a list contains specified elements.    ", "position       set to a named variable that will return the position of the first        instance of the corresponding element of the specified elements  elements       scalar or vector of elements of the same type as the list    ", "          -1", "    1B if contained or 0B if otherwise    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::move", "mgcoabstractlist::move", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::move", " Move an element of the list to another position.    ", "source       index of the element to move  destination       index of position to move element  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::remove", "mgcoabstractlist::remove", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::remove", " Remove specified elements from the list.    ", "position       set to a scalar or vector array of indices to remove from the list  all       set to remove all elements of the list  elements       elements of the list to remove    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::iterator", "mgcoabstractlist::iterator", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::iterator", " Creates an iterator to iterate through the elements of the list. The  destruction of the iterator is the responsibility of the caller of this  method.    ", "", "          -1", "    MGAbstractIterator object  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::cleanup", "mgcoabstractlist::cleanup", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::cleanup", " Free resouces.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist::init", "mgcoabstractlist::init", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist::init", " Initialize list.    ", "", "          -1", "    1B  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoabstractlist__define.html#mgcoabstractlist__define", "mgcoabstractlist__define", "routine in mgcoabstractlist__define.pro", "mgcoabstractlist__define.pro", "", "mgcoabstractlist__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html", "mgcoarraylist__define.pro", ".pro file in dist_tools/collection/ directory", "mgcoarraylist__define.pro", "Michael Galloy  ", "", " An array list is a way to have an arbitrary length list of any particular  IDL variable (but all elements must be the same type). An MGcoArrayList  implements the same interface as IDL_Container, but can contain any IDL  type.    ", "", "          -1", "1.0        For example:    a = obj_new('MGcoArrayList', type=7)    a-&gt;add, 'a'   a-&gt;add, ['b', 'c', 'd']    print, a-&gt;count()   print, a-&gt;get(/all)   print, a-&gt;get(position=1)    obj_destroy, a  ");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::getProperty", "mgcoarraylist::getProperty", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::getProperty", " Get properties of the list.  ", "type       type code as in SIZE function to specify the type of elements in the        list; TYPE or EXAMPLE keyword must be used when initializing the array        list  block_size       initial size of the data array; defaults to 1000 if not specified  example       type defined by an example instead of a type code (required for array        lists of structures)  count       number of elements in the array list  _ref_extra       keywords to MGcoAbstractList::getProperty    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::setProperty", "mgcoarraylist::setProperty", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::setProperty", " Set properties of the list.  ", "type       type code as in SIZE function to specify the type of elements in the        list; TYPE or EXAMPLE keyword must be used when initializing the array        list  block_size       initial size of the data array; defaults to 1000 if not specified  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::remove", "mgcoarraylist::remove", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::remove", " Remove specified elements from the list.    ", "position       set to a scalar or vector array of indices to remove from the list  all       set to remove all elements of the list  elements       elements of the list to remove    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::move", "mgcoarraylist::move", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::move", " Move an element of the list to another position.    ", "source       index of the element to move  destination       index of position to move element  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::isContained", "mgcoarraylist::isContained", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::isContained", " Determines whether a list contains specified elements.    ", "position       set to a named variable that will return the position of the first        instance of the corresponding element of the specified elements  elements       scalar or vector of elements of the same type as the list    ", "          -1", "1B if contained or 0B if otherwise    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::add", "mgcoarraylist::add", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::add", " Add elements to the list.    ", "position       index or index array to insert elements at; if array, must match        number of elements  elements       scalar or vector array of the same type as the list    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::isaGet", "mgcoarraylist::isaGet", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::isaGet", " Private method to screen for given class(es). Indices returned are indices  POSITION (or data array if ALL is set).    ", "position       indices of elements to check  isa       classes to check objects for  all       screen from all elements  count       number of matched items  ", "          -1", "index array or -1L if none    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::get", "mgcoarraylist::get", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::get", " Get elements of the list.    ", "all       set to return all elements  position       set to an index or an index array of elements to return; defaults to 0        if ALL keyword not set  count       set to a named variable to get the number of elements returned by this        function  isa       classname(s) of objects to return; only allowable if list type is        object  ", "          -1", "element(s) of the list or -1L if no elements to return    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::count", "mgcoarraylist::count", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::count", " Returns the number of elements in the list.    ", "", "          -1", "long integer  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::iterator", "mgcoarraylist::iterator", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::iterator", " Creates an iterator to iterate through the elements of the ArrayList. The  destruction of the iterator is the responsibility of the caller of this  method.    ", "", "          -1", "MGcoArrayListIterator object  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::cleanup", "mgcoarraylist::cleanup", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::cleanup", " Cleanup list resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist::init", "mgcoarraylist::init", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist::init", " Create a list.    ", "type       type code as in SIZE function to specify the type of elements in the        list; TYPE or EXAMPLE keyword must be used when initializing the array        list  example       type defined by an example instead of a type code (required for array        lists of structures)  block_size       initial size of the data array; defaults to 1000 if not specified  ", "          -1", "1B for succes, 0B otherwise  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylist__define.html#mgcoarraylist__define", "mgcoarraylist__define", "routine in mgcoarraylist__define.pro", "mgcoarraylist__define.pro", "", "mgcoarraylist__define", " Define member variables.    ", "", "object collection", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html", "mgcoarraylistiterator__define.pro", ".pro file in dist_tools/collection/ directory", "mgcoarraylistiterator__define.pro", "", "", " This class provides a nice way to iterate through all the elements of an  array list.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator::hasNext", "mgcoarraylistiterator::hasNext", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator::hasNext", " Determine if the underlying collection has another element to retrieve.    ", "", "          -1", "    1 if underlying collection has another element, 0 otherwise  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator::next", "mgcoarraylistiterator::next", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator::next", " Return the next item in the underlying collection.    ", "", "          -1", "    list item  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator::remove", "mgcoarraylistiterator::remove", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator::remove", " Removes from the underlying MGArrayList the last element returned.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator::cleanup", "mgcoarraylistiterator::cleanup", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator::cleanup", " Free resources of the iterator (not the underlying collection).  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator::init", "mgcoarraylistiterator::init", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator::init", " Initialize an MGArrayListIterator.    ", "arraylist       MGcoArrayList to iterator over  ", "          -1", "    1 for success, 0 otherwise    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoarraylistiterator__define.html#mgcoarraylistiterator__define", "mgcoarraylistiterator__define", "routine in mgcoarraylistiterator__define.pro", "mgcoarraylistiterator__define.pro", "", "mgcoarraylistiterator__define", " Define member variables.    ", "", "          -1", "    IDL 6.0    ");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html", "mgcohashtable__define.pro", ".pro file in dist_tools/collection/ directory", "mgcohashtable__define.pro", "    Michael D. Galloy    ", "", " A hash table which can hash any kind of IDL variables. To hash objects,  simply make sure each object implements the hashCode method. See the help  for the calcHashCode method for details.    ", "", "collection", "    Try the main-level example program at the end of this file:    IDL&gt; .run mgcohashtable__define  ");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::_getHistogram", "mgcohashtable::_getHistogram", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::_getHistogram", " Returns an array with the same number of elements as the hash array.  The  value each element of the array is the number of elements in that  bin  of  the mgcohashtable. This could be useful in determining the effectiveness of  the hash code calculations.    ", "", "          -1", "    long array  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::print", "mgcohashtable::print", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::print", " Prints keys and values to a given LUN. Prints to STDOUT if LUN not given.    ", "lun       logical unit number for output  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::keys", "mgcohashtable::keys", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::keys", " Returns an array of the keys of the hash table.    ", "count       number of keys in the hash table  ", "          -1", "    an array of the keys of the hash table or -1 if no keys    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::values", "mgcohashtable::values", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::values", " Returns an array of the values of the hash table.    ", "count       number of values in the hash table  ", "          -1", "    an array of the values of the hash table or -1 if no values    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::_calcHashCode", "mgcohashtable::_calcHashCode", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::_calcHashCode", " Calculates the hash code of the given key.  The index of the array element  the key's value will be stored in will be the hash code value MOD the array  size.    If a hash tbale of object references is desired, then the objects should  implement the hashCode method.  This function should accept no parameters  and return an unsigned long.    This method should not normally be called directly.    If the given default hash function is not doing well (use the _getHistogram  method to find out how well it's spreading out the keys), subclass this  class and implement a more appropriate hash function.    ", "key       key to find hash code of  ", "          -1", "    hash code (unsigned long integer); 0 if null pointer or object, undefined     variable; or an object that does not implement hashCode    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::get", "mgcohashtable::get", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::get", " Finds the value associated with the given key.    ", "found       true if value found for given key  key       key to look up    ", "          -1", "    the value of the associated key or -1L if not found    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::remove", "mgcohashtable::remove", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::remove", " Removes the value associated with the given key.    ", "found       true if value found for given key  key       key to look up    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::put", "mgcohashtable::put", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::put", " Puts the key-value pair into the hash table or updates the value for the key  if it is already in the hash table.    ", "found       pass a named variable that is set to true if the key was already in        the table and is updated  key       key to place in the table  value       value to place in the table    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::count", "mgcohashtable::count", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::count", " Find the number of key-value pairs in the hash table    ", "", "          -1", "    the number of key-value pairs in the hash table  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::isEmpty", "mgcohashtable::isEmpty", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::isEmpty", " Determines if the hash table is empty.    ", "", "          -1", "    0 if the table is empty, 1 if it contains any key-value pairs  ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::cleanup", "mgcohashtable::cleanup", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::cleanup", " Frees hash table resources, but the resources contained by the hash table.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable::init", "mgcohashtable::init", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable::init", " Create a hash table.    ", "array_size       the size of the hash table; generally a prime is a good choice  key_type       type code for keys; key_type or key_example must be present  value_type       type code for values; value_type or key_example must be present  key_example       example of key type; key_type or key_example must be present  value_example       example of value type; value_type or value_example must be present  ", "          -1", "    1 if successful; 0 otherwise    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcohashtable__define.html#mgcohashtable__define", "mgcohashtable__define", "routine in mgcohashtable__define.pro", "mgcohashtable__define.pro", "", "mgcohashtable__define", " Hash table implementation.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html", "mgcoraggedarray__define.pro", ".pro file in dist_tools/collection/ directory", "mgcoraggedarray__define.pro", "", "", " This class represents an array where each element is another array (of  differing sizes).  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html#mgcoraggedarray::add", "mgcoraggedarray::add", "routine in mgcoraggedarray__define.pro", "mgcoraggedarray__define.pro", "", "mgcoraggedarray::add", " Add an array to the ragged array.    ", "array       array to add  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html#mgcoraggedarray::get", "mgcoraggedarray::get", "routine in mgcoraggedarray__define.pro", "mgcoraggedarray__define.pro", "", "mgcoraggedarray::get", " Get elements of the array.    ", "all       set to return all elements  position       position of element to return  count       number of elements returned  isa       classname to test elements for  reverse_indices       when a named variable is present routine returns HISTOGRAM type output        as the return value and REVERSE_INDICES through this keyword  connectivity_list       set to return a connectivity list format of the results; only valid        if the type is a numeric type  ", "          -1", "    element(s)    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html#mgcoraggedarray::cleanup", "mgcoraggedarray::cleanup", "routine in mgcoraggedarray__define.pro", "mgcoraggedarray__define.pro", "", "mgcoraggedarray::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html#mgcoraggedarray::init", "mgcoraggedarray::init", "routine in mgcoraggedarray__define.pro", "mgcoraggedarray__define.pro", "", "mgcoraggedarray::init", " Create a ragged array.    ", "type       type code as in SIZE function to specify the type of elements in the        list; TYPE or EXAMPLE keyword must be used     example : in, optional, type=any        used to specify the type of the list by example; necessary if defining        a list of structures  exampleblock_size       initial size of data array  ", "          -1", "    1B for succes, 0B otherwise    ");
  
  libdata[libdataItem++] = new Array("dist_tools/collection/mgcoraggedarray__define.html#mgcoraggedarray__define", "mgcoraggedarray__define", "routine in mgcoraggedarray__define.pro", "mgcoraggedarray__define.pro", "", "mgcoraggedarray__define", " Define instance variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html", "mgfflogger__define.pro", ".pro file in dist_tools/ directory", "mgfflogger__define.pro", "", "", " Logger object to control logging.    ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::getProperty", "mgfflogger::getProperty", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::getProperty", " Set properties.  ", "level       current level of logging: 0 (none), 1 (debug), 2 (info), 3 (warning),        4 (error), or 5 (critical)  format       format string for messages  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::setProperty", "mgfflogger::setProperty", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::setProperty", " Get properties.  ", "level       current level of logging: 0 (none), 1 (debug), 2 (info), 3 (warning),        4 (error), or 5 (critical)  format       format string for messages  filename       filename to send output to; set to empty string to send output to        STDOUT/STDERR  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::print", "mgfflogger::print", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::print", " Log message to given level.    ", "level       level of message  msg       message to print    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::cleanup", "mgfflogger::cleanup", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::init", "mgfflogger::init", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::init", " Create logger object.    ", "", "          -1", "    1 for success, 0 for failure  ");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger::flush", "mgfflogger::flush", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger::flush", " Flushes output.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgfflogger__define.html#mgfflogger__define", "mgfflogger__define", "routine in mgfflogger__define.pro", "mgfflogger__define.pro", "", "mgfflogger__define", " Define instance variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html", "mgffprefs__define.pro", ".pro file in dist_tools/ directory", "mgffprefs__define.pro", "", "", " Class responsible for storing and retrieving preferences. Preferences are  persistent across IDL sessions on the same computer.    ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::set", "mgffprefs::set", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::set", " Save the value of a preference.    ", "prefname       case-insensitive name of preference to retrieve  prefvalue       value of the preference  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::get", "mgffprefs::get", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::get", " Retrieve the value of a preference.    ", "found       set to a named variable to return whether the preference was found  default       default value to use if no preference value is found for the given        preference name  names       set to return a list of the preference names instead of a value; names        may not agree exactly with the names given in the set method because        they have been modified to make valid filename  prefname       case-insensitive name of preference to retrieve    ", "          -1", "    preference value    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::_prefnameToFilename", "mgffprefs::_prefnameToFilename", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::_prefnameToFilename", " Converts a preference name to a valid save filename.    ", "prefname       name of preference  ", "          -1", "    string    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::_getAppDir", "mgffprefs::_getAppDir", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::_getAppDir", " Returns directory for application data.    ", "author_description       full name of the author  app_description       full name of the application  authorName       short name of the author  appName       short application name    ", "          -1", "    string    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::getProperty", "mgffprefs::getProperty", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::getProperty", " Get properties.  ", "app_directory       location of the directory for the application using these preferences  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::cleanup", "mgffprefs::cleanup", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs::init", "mgffprefs::init", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs::init", " Initialize a prefs object.    ", "author_name       short name of the author  app_name       short name of the application  author_description       full name of the author  app_description       full name of the application  ", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("dist_tools/mgffprefs__define.html#mgffprefs__define", "mgffprefs__define", "routine in mgffprefs__define.pro", "mgffprefs__define.pro", "", "mgffprefs__define", " Define instance variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mgunit.html", "mgunit.pro", ".pro file in ./ directory", "mgunit.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mgunit.html#mgunit", "mgunit", "routine in mgunit.pro", "mgunit.pro", "", "mgunit", " Runs unit tests.    ", "filename       name of file to send output to; if not present sends output to the        output log  html       set to indicate HTML output instead of plain text  gui       set to bring up an interactive GUI to run the tests  color       set to print color output to the output log  npass       number of tests that passed  nfail       number of tests that failed  ntests       number of tests  tests       array of test suites and/or test cases    ", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mgunit_wrapper.html", "mgunit_wrapper.pro", ".pro file in ./ directory", "mgunit_wrapper.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mgunit_wrapper.html#mgunit_wrapper", "mgunit_wrapper", "routine in mgunit_wrapper.pro", "mgunit_wrapper.pro", "", "mgunit_wrapper", " Command line wrapper for mgunit. This routine is made to be called from the  mgunit shell script.  ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mgutclirunner__define.html", "mgutclirunner__define.pro", ".pro file in ./ directory", "mgutclirunner__define.pro", "", "", " Results for tests, test cases, and test suites are reported to the test  runner. The MGutCliRunner displays the results in the output log or in a  log file.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestSuiteStart", "mgutclirunner::reportTestSuiteStart", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestSuiteStart", " Report a test suite has begun.    ", "ntestcases       number of test suites/cases contained by the test suite  ntests       number of tests contained in the hierarchy below this test suite  level       level of test suite  testsuite       name of test suite    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestSuiteResult", "mgutclirunner::reportTestSuiteResult", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestSuiteResult", " Report the results of a test suite.    ", "npass       number of passing tests contained in the hierarchy below the test        suite  nfail       number of failing tests contained in the hierarchy below the test        suite  level       level of test suite  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestCaseStart", "mgutclirunner::reportTestCaseStart", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestCaseStart", " Report a test case has begun.    ", "ntests       number of tests contained in this test case  level       level of test case  testcase       name of test case    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestCaseResult", "mgutclirunner::reportTestCaseResult", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestCaseResult", " Report the results of a test case.    ", "npass       number of passing tests  nfail       number of failing tests  level       level of test case  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestStart", "mgutclirunner::reportTestStart", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestStart", " Report the start of single test.    ", "level       level of test case  testname       name of test    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::reportTestResult", "mgutclirunner::reportTestResult", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::reportTestResult", " Report the result of a single test.    ", "passed       whether the test passed  time       time for the test to run  msg       message to display when test fails    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::_print", "mgutclirunner::_print", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::_print", " Prints a message to a LUN.    ", "_extra       keywords to MG_ANSICODE i.e. RED or GREEN  lun       logical unit number to print to  text       text to print    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::_findIfTty", "mgutclirunner::_findIfTty", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::_findIfTty", " Safe method of determining if the current terminal is TTY.    ", "", "          -1", "    1 if the terminal is TTY, 0 if not  ");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::cleanup", "mgutclirunner::cleanup", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner::init", "mgutclirunner::init", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner::init", " Initialize the test runner.    ", "filename       if present, output is sent to that file, otherwise output is sent to        stdout  color       set to print color output  _extra", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("./mgutclirunner__define.html#mgutclirunner__define", "mgutclirunner__define", "routine in mgutclirunner__define.pro", "mgutclirunner__define.pro", "", "mgutclirunner__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html", "mgutcompoundrunner__define.pro", ".pro file in ./ directory", "mgutcompoundrunner__define.pro", "", "", " Results for tests, test cases, and test suites are reported to the test  runner. The MGutHTMLRunner displays the results in the output HTML file.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestSuiteStart", "mgutcompoundrunner::reportTestSuiteStart", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestSuiteStart", " Report a test suite has begun.    ", "ntestcases       number of test suites/cases contained by the test suite  ntests       number of tests contained in the hierarchy below this test suite  level       level of test suite  testsuite       name of test suite    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestSuiteResult", "mgutcompoundrunner::reportTestSuiteResult", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestSuiteResult", " Report the results of a test suite.    ", "npass       number of passing tests contained in the hierarchy below the test        suite  nfail       number of failing tests contained in the hierarchy below the test        suite  level       level of test suite  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestCaseStart", "mgutcompoundrunner::reportTestCaseStart", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestCaseStart", " Report a test case has begun.    ", "ntests       number of tests contained in this test case  level       level of test case  testcase       name of test case    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestCaseResult", "mgutcompoundrunner::reportTestCaseResult", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestCaseResult", " Report the results of a test case.    ", "npass       number of passing tests  nfail       number of failing tests  level       level of test case  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestStart", "mgutcompoundrunner::reportTestStart", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestStart", " Report the start of single test.    ", "level       level of test case  testname       name of test    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::reportTestResult", "mgutcompoundrunner::reportTestResult", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::reportTestResult", " Report the result of a single test.    ", "passed       whether the test passed  time       time for the test to run  msg       message to display when test fails    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::cleanup", "mgutcompoundrunner::cleanup", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner::init", "mgutcompoundrunner::init", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner::init", " Initialize the test runner.    ", "filename       if present, output is sent that file, otherwise output is sent to        stdout  _extra", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("./mgutcompoundrunner__define.html#mgutcompoundrunner__define", "mgutcompoundrunner__define", "routine in mgutcompoundrunner__define.pro", "mgutcompoundrunner__define.pro", "", "mgutcompoundrunner__define", " Define member variables.  ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mgutguirunner__define.html", "mgutguirunner__define.pro", ".pro file in ./ directory", "mgutguirunner__define.pro", "", "", " Results for tests, test cases, and test suites are reported to the test  runner. The mgutguirunner displays the results in the output log or in a  log file.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner_event", "mgutguirunner_event", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner_event", "", "event", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner_cleanup", "mgutguirunner_cleanup", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner_cleanup", "", "tlb", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestSuiteStart", "mgutguirunner::reportTestSuiteStart", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestSuiteStart", " Report a test suite has begun.    ", "ntestcases       number of test suites/cases contained by the test suite  ntests       number of tests contained in the hierarchy below this test suite  level       level of test suite  testsuite       name of test suite    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestSuiteResult", "mgutguirunner::reportTestSuiteResult", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestSuiteResult", " Report the results of a test suite.    ", "npass       number of passing tests contained in the hierarchy below the test        suite  nfail       number of failing tests contained in the hierarchy below the test        suite  level       level of test suite  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestCaseStart", "mgutguirunner::reportTestCaseStart", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestCaseStart", " Report a test case has begun.    ", "ntests       number of tests contained in this test case  level       level of test case  testcase       name of test case    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestCaseResult", "mgutguirunner::reportTestCaseResult", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestCaseResult", " Report the results of a test case.    ", "npass       number of passing tests  nfail       number of failing tests  level       level of test case  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestStart", "mgutguirunner::reportTestStart", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestStart", " Report the start of single test.    ", "level       level of test case  testname       name of test    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::reportTestResult", "mgutguirunner::reportTestResult", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::reportTestResult", " Report the result of a single test.    ", "passed       whether the test passed  time       time for the test to run  msg       message to display when test fails    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_print", "mgutguirunner::_print", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_print", "", "continued_extratext", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::cleanup", "mgutguirunner::cleanup", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_createWidgets", "mgutguirunner::_createWidgets", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_createWidgets", " Creates the user-interface for the GUI test runner.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_realizeWidgets", "mgutguirunner::_realizeWidgets", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_realizeWidgets", " Realizes the user-interface for the GUI test runner.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_startXManager", "mgutguirunner::_startXManager", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_startXManager", " Starts up XMANAGER.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_eventHandler", "mgutguirunner::_eventHandler", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_eventHandler", " Handles all events from the GUI test runner.    ", "event       event structure from any widget generating events in the GUI test        runner  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::_cleanupWidgets", "mgutguirunner::_cleanupWidgets", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::_cleanupWidgets", "", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner::init", "mgutguirunner::init", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner::init", " Initialize the test runner.    ", "filenamecolor_extra", "          -1", "    1 for success, 0 for failure  ");
  
  libdata[libdataItem++] = new Array("./mgutguirunner__define.html#mgutguirunner__define", "mgutguirunner__define", "routine in mgutguirunner__define.pro", "mgutguirunner__define.pro", "", "mgutguirunner__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html", "mguthtmlrunner__define.pro", ".pro file in ./ directory", "mguthtmlrunner__define.pro", "", "", " Results for tests, test cases, and test suites are reported to the test  runner. The MGutHTMLRunner displays the results in the output HTML file.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestSuiteStart", "mguthtmlrunner::reportTestSuiteStart", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestSuiteStart", " Report a test suite has begun.    ", "ntestcases       number of test suites/cases contained by the test suite  ntests       number of tests contained in the hierarchy below this test suite  level       level of test suite  testsuite       name of test suite    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestSuiteResult", "mguthtmlrunner::reportTestSuiteResult", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestSuiteResult", " Report the results of a test suite.    ", "npass       number of passing tests contained in the hierarchy below the test        suite  nfail       number of failing tests contained in the hierarchy below the test        suite  level       level of test suite  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestCaseStart", "mguthtmlrunner::reportTestCaseStart", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestCaseStart", " Report a test case has begun.    ", "ntests       number of tests contained in this test case  level       level of test case  testcase       name of test case    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestCaseResult", "mguthtmlrunner::reportTestCaseResult", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestCaseResult", " Report the results of a test case.    ", "npass       number of passing tests  nfail       number of failing tests  level       level of test case  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestStart", "mguthtmlrunner::reportTestStart", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestStart", " Report the start of single test.    ", "level       level of test case  testname       name of test    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::reportTestResult", "mguthtmlrunner::reportTestResult", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::reportTestResult", " Report the result of a single test.    ", "passed       whether the test passed  time       time for the test to run  msg       message to display when test fails    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::_print", "mguthtmlrunner::_print", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::_print", " Prints a message to a LUN.    ", "_extra       keywords to MG_ANSICODE i.e. RED or GREEN  lun       logical unit number to print to  text       text to print    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::cleanup", "mguthtmlrunner::cleanup", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner::init", "mguthtmlrunner::init", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner::init", " Initialize the test runner.    ", "filename       if present, output is sent that file, otherwise output is sent to        stdout  color       unused for MGutHtmlRunner  _extra       keywords to MGutTestRunner::init  ", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("./mguthtmlrunner__define.html#mguthtmlrunner__define", "mguthtmlrunner__define", "routine in mguthtmlrunner__define.pro", "mguthtmlrunner__define.pro", "", "mguthtmlrunner__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mguttestcase__define.html", "mguttestcase__define.pro", ".pro file in ./ directory", "mguttestcase__define.pro", "", "", " Subclass MGtestCase to actually write tests. Any function method whose name  starts with  test  will be considered a test. Tests are executed and results  are reported to the test runner object.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::setup", "mguttestcase::setup", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::setup", " Override in subclasses to perform setup actions before each test.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::teardown", "mguttestcase::teardown", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::teardown", " Override in subclasses to perform teardown actions after each test.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::runTest", "mguttestcase::runTest", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::runTest", " This is a safe place to actually run a single test. Any errors that occur are  assumed to be from the test and recorded as a failure for it.    ", "message       error message if test failed  testname       name of method    ", "          -1", "    boolean    ");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::_runSetup", "mguttestcase::_runSetup", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::_runSetup", " Run setup method before each test.    ", "fail       set to a named variable to determine if the setup method failed  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::_runTeardown", "mguttestcase::_runTeardown", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::_runTeardown", " Run teardown method before each test.    ", "fail       set to a named variable to determine if the teardown method failed  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::_removePrefix", "mguttestcase::_removePrefix", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::_removePrefix", " Removes the given prefix from the msg if present.    ", "msg       string to remove prefix from, may be undefined  prefix       prefix to remove from msg  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::run", "mguttestcase::run", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::run", " Run the tests for this class (i.e. methods with names that start with  test ).  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::findTestnames", "mguttestcase::findTestnames", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::findTestnames", " Find the name and number of tests (i.e. methods with names that start with   test ).  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::getProperty", "mguttestcase::getProperty", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::getProperty", " Get properties of the object.    ", "npass       number of passing tests  nfail       number of failing tests  ntests       number of tests  testnames       array of method names which begin with  test   ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::setLevel", "mguttestcase::setLevel", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::setLevel", " Test suites can contain other test suites or test cases. The level is the  number of layers down from the top most test suite (level 0).    ", "level       new level of object  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::cleanup", "mguttestcase::cleanup", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase::init", "mguttestcase::init", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase::init", " Intialize test case.    ", "test_runner       subclass of MGutTestRunner  ", "          -1", "    1 for succcess, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("./mguttestcase__define.html#mguttestcase__define", "mguttestcase__define", "routine in mguttestcase__define.pro", "mguttestcase__define.pro", "", "mguttestcase__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mguttestrunner__define.html", "mguttestrunner__define.pro", ".pro file in ./ directory", "mguttestrunner__define.pro", "", "", " Results for tests, test cases, and test suites are reported to the test  runner. Each subclass of MGutTestRunner displays them in some way.  MGutTestRunner itself is abstract and shouldn't be instantiated.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestSuiteStart", "mguttestrunner::reportTestSuiteStart", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestSuiteStart", " Report a test suite has begun.    ", "ntestcases       number of test suites/cases contained by the test suite  ntests       number of tests contained in the hierarchy below this test suite  level       level of test suite  testsuite       name of test suite    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestSuiteResult", "mguttestrunner::reportTestSuiteResult", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestSuiteResult", " Report the results of a test suite.    ", "npass       number of passing tests contained in the hierarchy below the test        suite  nfail       number of failing tests contained in the hierarchy below the test        suite  level       level of test suite  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestCaseStart", "mguttestrunner::reportTestCaseStart", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestCaseStart", " Report a test case has begun.    ", "ntests       number of tests contained in this test case  level       level of test case  testcase       name of test case    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestCaseResult", "mguttestrunner::reportTestCaseResult", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestCaseResult", " Report the results of a test case.    ", "npass       number of passing tests  nfail       number of failing tests  level       level of test case  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestStart", "mguttestrunner::reportTestStart", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestStart", " Report the start of single test.    ", "level       level of test case  testname       name of test    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::reportTestResult", "mguttestrunner::reportTestResult", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::reportTestResult", " Report the result of a single test.    ", "passed       whether the test passed  time       time for the test to run  msg       message to display when test fails    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::cleanup", "mguttestrunner::cleanup", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner::init", "mguttestrunner::init", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner::init", " Initialize the test runner.    ", "parenttest_suite", "          -1", "    1 for success, 0 for failure  ");
  
  libdata[libdataItem++] = new Array("./mguttestrunner__define.html#mguttestrunner__define", "mguttestrunner__define", "routine in mguttestrunner__define.pro", "mguttestrunner__define.pro", "", "mguttestrunner__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("./mguttestsuite__define.html", "mguttestsuite__define.pro", ".pro file in ./ directory", "mguttestsuite__define.pro", "", "", " Test suites are containers for test cases. Either subclass MGutTestSuite and  add test suites/test cases in its init method or create a MGutTestSuite and  use the add method to add test suites/cases.  ", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::_recompile", "mguttestsuite::_recompile", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::_recompile", " Recompile the class definition before creating the object to make sure it is  the newest definition (convenient when making changes to a test).  ", "classname", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::_makeTestCase", "mguttestsuite::_makeTestCase", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::_makeTestCase", " Create a new test case or test suite, but check for errors while creating  it.    ", "error       0 if no error and 1 if an error  _extra       keywords to OBJ_NEW for test cases and test suites  testName       classname of test case or test suite to create    ", "          -1", "    obj    ");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::recompileTestCases", "mguttestsuite::recompileTestCases", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::recompileTestCases", " Recompiles all test cases contained by the suite or contained by child  suites.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::run", "mguttestsuite::run", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::run", " Run the contained test suites or test cases.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::add", "mguttestsuite::add", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::add", " Add a scalar or array of test suites or test cases.    ", "all       set to add all the files in the current directory that end in         _ut__define.pro  (the current directory is defined to be the        directory where the method calls this method is located)  tests       classnames of test suites or test cases    ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::getProperty", "mguttestsuite::getProperty", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::getProperty", " Get properties of the object.    ", "name       name of the object  npass       number of passing tests contained in the hierarchy below this object  nfail       number of failing tests contained in the hierarchy below this object  ntestcases       number of directly contained test suites or test cases  ntests       number of tests contained in the hierarchy below this object  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::setLevel", "mguttestsuite::setLevel", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::setLevel", " Test suites can contain other test suites or test cases. The level is the  number of layers down from the top most test suite (level 0).    ", "level       new level of object  ", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::cleanup", "mguttestsuite::cleanup", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::cleanup", " Free resources.  ", "", "          -1", "");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite::init", "mguttestsuite::init", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite::init", " Initialize test suite.    ", "name       name of the test suite  home       location of the root of the test suite  test_runner       subclass of MGtestRunner  ", "          -1", "    1 for success, 0 for failure    ");
  
  libdata[libdataItem++] = new Array("./mguttestsuite__define.html#mguttestsuite__define", "mguttestsuite__define", "routine in mguttestsuite__define.pro", "mguttestsuite__define.pro", "", "mguttestsuite__define", " Define member variables.    ", "", "          -1", "");
  
  

libdata[libdataItem++] = new Array("cmdline_tools/pwd.html", "pwd.pro", ".pro file in cmdline_tools/ directory", "pwd.pro", "", "", "", "", "          -1", "");
  
  
  libdata[libdataItem++] = new Array("cmdline_tools/pwd.html#pwd", "pwd", "routine in pwd.pro", "pwd.pro", "", "pwd", " Prints the IDL's current directory to the output log.  ", "", "          -1", "");
  
  

