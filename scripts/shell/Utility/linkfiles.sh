#!/bin/sh

# $Id$

script_id()
{
  REVISION='$Revision$'
  LAST_CHANGED_DATE='$LastChangedDate$'
  echo
  echo "${SCRIPT_NAME} ${REVISION} ${LAST_CHANGED_DATE}"
  echo " "`date`
  echo " Support email: NCEP.List.EMC.JCSDA_CRTM.Support@noaa.gov"
}

usage()
{
  echo
  echo " Usage: linkfiles.sh [-hqsx] [-d filter-dir] dir file [file2 | file3 | ... | fileN]"
  echo
  echo "   Script to search a directory tree and symlink in the requested"
  echo "   file(s) to the current directory."
  echo
  echo "   * If multiple files of the same name exist in the directory tree,"
  echo "     the first one found is linked in."
  echo
  echo
  echo " Options:"
  echo "   -d filter-dir"
  echo "         Use this option to filter the directory location"
  echo "         of the requested file(s) to symlink."
  echo
  echo "   -h"
  echo "         Print this message and exit."
  echo
  echo "   -q"
  echo "         Suppress informational output."
  echo
  echo "   -s"
  echo '         Allow symbolic links to themselves be linked, i.e. "chain-linking"'
  echo "         the files. Default action is to *NOT* link in symlinks."
  echo
  echo "   -x"
  echo "         Turn on execution tracing. This also causes extra script"
  echo "         information to be printed out."
  echo
  echo " Arguments:"
  echo "   dir"
  echo "         Directory at which to begin the search for the"
  echo "         requested file(s) to symlink."
  echo
  echo "   file"
  echo "         File for which a symlink is required."
  echo
  echo " Optional arguments:"
  echo "   file2 | file3 | ... | fileN"
  echo "         Additional list of files for which symlinks are required."
  echo
}

error_message()
{
  MESSAGE=$1
  echo >&2
  echo "  *********************" >&2
  echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}" >&2
  echo "  *********************" >&2
}



########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Setup
# ...Script name for error messages
SCRIPT_NAME=$(basename $0)
# ...Defintiions
SUCCESS=0
FAILURE=1
LINK="ln -sf"
EXCLUDE_SYMLINK="YES"
NOISY="YES"


# Parse the command line options
while getopts :hqsxd: OPTVAL; do

  # Exit if option argument looks like another option
  case ${OPTARG} in
    -*) break;;
  esac

  # Parse the valid options
  case ${OPTVAL} in
    d)    FILTER_DIR=${OPTARG} ;;
    h)    usage | more; exit ${SUCCESS} ;;
    q)    NOISY= ;;
    s)    EXCLUDE_SYMLINK= ;;
    x)    script_id; set -x ;;
    :|\?) OPTVAL=${OPTARG}; break ;;
  esac
done

# ...Remove the options processed
shift $(expr ${OPTIND} - 1)

# ...Output invalidities based on OPTVAL
case ${OPTVAL} in

  # If OPTVAL contains nothing, then all options
  # have been successfully parsed and all that
  # remains are the arguments
  \?) if [ $# -lt 2 ]; then
        usage
        error_message "Missing 'dir' and 'file' arguments"
        exit ${FAILURE}
      fi;;

  # Valid options, but missing arguments
  d) usage
     error_message "'-${OPTVAL}' option requires an argument"
     exit ${FAILURE} ;;

  # Invalid option
  ?) usage
     error_message "Invalid option '-${OPTARG}'"
     exit ${FAILURE} ;;
esac


# Assign command line arguments
LINKFILE_ROOT=$1
shift 1
LINKFILE_LIST=$*


# Output some user info
if [ ${NOISY} ]; then
  echo "${SCRIPT_NAME}: Searching for file(s) to link..."
fi

# Build argument list for find
OR="-o"
FINDFILE_LIST=""
for FILE in ${LINKFILE_LIST}; do
  # Skip if file already exists
  if [ -f ${FILE} ]; then
    continue
  fi
  FINDFILE_LIST="${FINDFILE_LIST} -name ${FILE} ${OR}"
done
FINDFILE_LIST=${FINDFILE_LIST%$OR}


# Exit if FINDFILE_LIST is empty
if [ -z "${FINDFILE_LIST}" ]; then
  echo "${SCRIPT_NAME}: All required files are present so nothing to do. Exiting."
  exit ${SUCCESS}
fi


# Find the required files
# ...Specify expressions for find
IGNORE_SVN='-type d -name ".svn" -prune'  # Do not search the .svn directories
FILES_TO_FIND="-type f ${FINDFILE_LIST}"  # The list of files to actually find
# ...Construct the expression for the find command
EXPRESSION="${IGNORE_SVN} -o ${FILES_TO_FIND}"
# ...Run the command
if [ -z "${FILTER_DIR}" ]; then
  LINKFILES=$(find -L ${LINKFILE_ROOT}/ \( ${EXPRESSION} \) | sort -r)
else
  LINKFILES=$(find -L ${LINKFILE_ROOT}/ \( ${EXPRESSION} \) | grep ${FILTER_DIR} | sort -r)
fi
if [ $? -ne 0 ]; then
  error_message "Find command failed for: ${FINDFILE_LIST}"
  exit ${FAILURE}
fi


# Set the condition for skipping existing files or symlinks
if [ ${EXCLUDE_SYMLINK} ]; then
  SKIP_CONDITION='[ -f $(basename ${FILE}) ] || [ -h ${FILE} ]'
  if [ ${NOISY} ]; then
    echo "${SCRIPT_NAME}: Skipping symbolic links..."
  fi
else
  SKIP_CONDITION='[ -f $(basename ${FILE}) ]'
fi


# Link in the files found
for FILE in ${LINKFILES}; do
  # Skip file if necessary
  if eval "${SKIP_CONDITION}"; then
    continue
  fi
  # Link in the file
  ${LINK} ${FILE} .
  if [ $? -ne 0 ]; then
    error_message "Link command failed for ${FILE}"
    exit ${FAILURE}
  fi
done


# Check that all the required files were found
for FILE in ${LINKFILE_LIST}; do
  if [ ! -f ${FILE} ]; then
    echo "File ${FILE} not found in ${LINKFILE_ROOT} hierarchy."
  fi
done
