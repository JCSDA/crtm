#!/bin/bash

script_id()
{
  REVISION='$Revision$'
  LAST_CHANGED_DATE='$LastChangedDate$'
  echo
  echo "${SCRIPT_NAME} ${REVISION} ${LAST_CHANGED_DATE}"
  echo " "`date`
  echo " Support email: NCEP.List.EMC.JCSDA_CRTM.Support@noaa.gov"
  echo
}


usage()
{
  echo
  echo "Usage: crtm_install_scripts.sh [-hux] [bindir]"
  echo
  echo "  Utility script to link in the various CRTM shell scripts in a user directory"
  echo "  to contain executable commands, bindir."
  echo
  echo "  * If the optional bindir argument is not specified, the default is ~/bin."
  echo
  echo "  * If bindir does not exist it is created."
  echo
  echo '  * If bindir is not in $PATH a "crtmrc" file is created that can be sourced to'
  echo '    add bindir to the $PATH in the current shell:'
  echo "      $ . crtmrc"
  echo
  echo
  echo " Options:"
  echo
  echo "   -h"
  echo "         Print this message."
  echo
  echo "   -u"
  echo "         Use this option to UNINSTALL (i.e. remove) the CRTM scripts in bindir."
  echo
  echo "   -x"
  echo "         Turn on execution tracing"
  echo
  echo
  echo " Optional arguments:"
  echo
  echo "   bindir"
  echo "         A specified directory in which to (un)install the scripts. If it is"
  echo "         specified and does not exist it is created for installation."
  echo; echo
}


error_message()
{
  MESSAGE=$1
  echo >&2
  echo "  *********************" >&2
  echo "  ${SCRIPT_NAME}(ERROR): ${MESSAGE}" >&2
  echo "  *********************" >&2
}


info_message()
{
  MESSAGE=$1
  echo "  ${SCRIPT_NAME}(INFORMATION): ${MESSAGE}"
}


########################################################################
#                           MAIN SCRIPT BEGINS                         #
########################################################################

# Set up
# ...Script name for error messages
SCRIPT_NAME=$(basename $BASH_SOURCE)
# ...Commands
LINK="ln -sf"
REMOVE="rm -f"
# ...Definitions
SUCCESS=0; TRUE=0
FAILURE=1; FALSE=1
BINDIR_DEFAULT="${HOME}/bin"
INSTALL=${TRUE}
SOURCEFILE="crtmrc"


# Parse the command line options
while getopts :hux OPTVAL; do
  # If option argument looks like another option exit the loop
  case ${OPTARG} in
    -*) break;;
  esac
  # Parse the valid options
  case ${OPTVAL} in
    h) usage | more; exit ${SUCCESS} ;;
    u) INSTALL=${FALSE} ;;
    x) script_id; set -x ;;
    \?) OPTVAL=${OPTARG}; break ;;
  esac
done
# Remove the options processed
shift $(expr ${OPTIND} - 1)
# Output invalidities based on OPTVAL
case ${OPTVAL} in
  # If OPTVAL contains nothing, then all options
  # have been successfully parsed
  \?) : ;;
  # Invalid option
  *) usage; error_message " Invalid option '-${OPTARG}'"; exit ${FAILURE};;
esac


# Check if bindir has been specified
BINDIR=${BINDIR_DEFAULT}
if [ $# -ne 0 ]; then
  BINDIR=$1
fi


# If no BINDIR for uninstallation, do nothing...
if [ ! -d ${BINDIR} -a ${INSTALL} -eq ${FALSE} ]; then
  info_message "${BINDIR} does not exist. Cannot uninstall from a non-existant directory"
  exit ${SUCCESS}
fi

  
# Does BINDIR exist for installation?
if [ ! -d ${BINDIR} -a ${INSTALL} -eq ${TRUE} ]; then
  # Create it
  info_message "Creating ${BINDIR} for script installation..."
  mkdir ${BINDIR}
  if [ $? -ne 0 ]; then
    error_message "Error creating ${BINDIR} directory for script installation"
    exit ${FAILURE}
  fi
else
  # Just output info message
  info_message "${BINDIR} exists..."
fi


# Is BINDIR in PATH?
if [[ ":$PATH:" != *":$BINDIR:"* ]]; then
  info_message 'Your $PATH does NOT contain '"$BINDIR..."
  if [ ${INSTALL} -eq ${TRUE} ]; then
    info_message 'Creating a crtmrc file with $PATH modification. For a permanent change modify your .bash_profile (or similar) file.'
    echo "export PATH="'${PATH}'":${BINDIR}" > ${SOURCEFILE}
    cat ${SOURCEFILE}
  fi
fi


# (Un)install all the scripts in BINDIR
# ...Get the script directory list
SCRIPTDIR_LIST=`ls -d`                          # current directory
SCRIPTDIR_LIST="${SCRIPTDIR_LIST} `ls -d */`"   # subdirectories
# ...Install from each directory.
for SCRIPTDIR in ${SCRIPTDIR_LIST}; do
  (
    cd $SCRIPTDIR
    MSG_ROOT="`basename ${PWD}` scripts in ${BINDIR}"
    
    # Define the install type specifics
    if [ ${INSTALL} -eq ${TRUE} ]; then
      MSG_TYPE="Installing"
      COMMAND='${LINK} ${PWD}/${SCRIPT} ${BINDIR}'
    else
      MSG_TYPE="Uninstalling"
      COMMAND='${REMOVE} ${BINDIR}/${SCRIPT}'
    fi
    
    # Perform the (un)installation
    info_message "${MSG_TYPE} ${MSG_ROOT}..."
    for SCRIPT in `ls *.sh`; do
      eval "${COMMAND}"
      if [ $? -ne 0 ]; then
        error_message "${MSG_TYPE} ${MSG_ROOT} failed"
        exit ${FAILURE}
      fi
    done
  )
done
