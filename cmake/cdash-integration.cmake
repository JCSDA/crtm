# This CMake script customizes the site and build names for CDash
# By default, it tries to autodetect the system hostname, compiler, 
# and if you are in a Charliecloud environment. Then, it sets
# the ctest/cdash variables SITE and BUILDNAME based on a combination
# of auto-detected values and user overrides.

# Valid options / overrides (you can pass these as cmake/ecbuild options):
#
# CDASH_OVERRIDE_GIT_BRANCH: tell CDash which git branch you are operating on.
#  If submitting to cdash, you should always set this option.
# CDASH_OVERRIDE_SITE: tell CDash your computer name. Not all systems have hostnames
#  or fully-qualified domain names. In some cases, you want to just set a standard value,
#  like "hera", "discover", or "yourname_work_laptop".
# CDASH_OVERRIDE_SYSTEM_NAME: Usually, we can do a good job of guessing the system name,
#  which should look like "Ubuntu_18.04". Occasionally, we cannot extract this, so you
#  can override this here.
# CDASH_OVERRIDE_BUILDNAME: Set this to fully override the BUILDNAME variable.

# Note: ecbuild likes to do strange things with the SITE and BUILDNAME variables, which are
#  actually used by ctest/cdash. This is why this file has to be included at the end of a
#  top-level CMakeLists.txt file.

if(DEFINED CDASH_OVERRIDE_GIT_BRANCH)
	set(GITBRANCH "${CDASH_OVERRIDE_GIT_BRANCH}")
else()
	set(GITBRANCH "GIT_BRANCH_UNSPECIFIED")
endif()

if(DEFINED CDASH_OVERRIDE_SITE)
	set(SITE "${CDASH_OVERRIDE_SITE}" CACHE STRING "Site name (use CDASH_OVERRIDE_SITE to customize)" FORCE)
else()
	cmake_host_system_information(RESULT HN QUERY
		HOSTNAME FQDN)
	list(GET HN 0 iHostname)
	list(GET HN 1 iFQDN)

	message(STATUS "Hostname: ${iHostname}")
	message(STATUS "Host FQDN: ${iFQDN}")
	set(SITE "${iFQDN}" CACHE STRING "Site name (use CDASH_OVERRIDE_SITE to customize)" FORCE)
endif()

if (DEFINED CDASH_OVERRIDE_SYSTEM_NAME)
	set(TESTING_SYSTEM_NAME "${CDASH_OVERRIDE_SYSTEM_NAME}")
else()
	if(CMAKE_VERSION VERSION_GREATER "3.10.0")
		cmake_host_system_information(RESULT OSprops QUERY
			OS_NAME OS_RELEASE OS_VERSION OS_PLATFORM)
		list(GET OSprops 0 iOS_NAME)
		list(GET OSprops 1 iOS_RELEASE)
		list(GET OSprops 2 iOS_VERSION)
		list(GET OSprops 3 iOS_PLATFORM)

		# Get OS info
		if("${CMAKE_HOST_SYSTEM_NAME}" MATCHES "Linux")
			# The lsb_release program knows distribution names (it's part of the Linux
			# Standard Base environment that has existed for over a decade in almost every
			# Linux system).
			# If lsb_release cannot be found, then we only can extract information about
			# the Linux kernel.
			find_program(LSB_RELEASE_EXEC lsb_release)
			mark_as_advanced(LSB_RELEASE_EXEC)
			if(LSB_RELEASE_EXEC)
				execute_process(COMMAND ${LSB_RELEASE_EXEC} -is
					OUTPUT_VARIABLE LSB_DISTRIBUTION_NAME_SHORT
					OUTPUT_STRIP_TRAILING_WHITESPACE
					)
				execute_process(COMMAND ${LSB_RELEASE_EXEC} -rs
					OUTPUT_VARIABLE LSB_RELEASE_ID_SHORT
					OUTPUT_STRIP_TRAILING_WHITESPACE
					)
				set(TESTING_SYSTEM_NAME "${LSB_DISTRIBUTION_NAME_SHORT}_${LSB_RELEASE_ID_SHORT}")
			else()
				message(STATUS "The lsb_release command cannot be found. Using defaults.")
				set(TESTING_SYSTEM_NAME "${iOS_NAME}_${iOS_RELEASE}_${iOS_VERSION}")
			endif()
		elseif("${CMAKE_HOST_SYSTEM_NAME}" MATCHES "FreeBSD")
			set(TESTING_SYSTEM_NAME "${iOS_NAME}_${iOS_RELEASE}_${iOS_VERSION}")
		elseif("${CMAKE_HOST_SYSTEM_NAME}" MATCHES "Darwin")
			set(TESTING_SYSTEM_NAME "${iOS_NAME}_${iOS_RELEASE}_${iOS_VERSION}")
		elseif("${CMAKE_HOST_SYSTEM_NAME}" MATCHES "Windows")
			set(TESTING_SYSTEM_NAME "${iOS_NAME}_${iOS_RELEASE}_${iOS_VERSION}")
		elseif("${CMAKE_HOST_SYSTEM_NAME}" MATCHES "CYGWIN")
			set(TESTING_SYSTEM_NAME "${iOS_NAME}_${iOS_RELEASE}_${iOS_VERSION}")
		else()
			message(STATUS "Unknown OS ${CMAKE_HOST_SYSTEM_NAME}")
			set(TESTING_SYSTEM_NAME "UNKNOWN_SYSTEM_NAME")
		endif()
		if(EXISTS "/WEIRD_AL_YANKOVIC")
			set(TESTING_SYSTEM_NAME "Charliecloud_${TESTING_SYSTEM_NAME}")
		endif()

	else()
		set(TESTING_SYSTEM_NAME "UNKNOWN_SYSTEM_NAME")
	endif()
endif()

# Compiler info
set(TESTING_CXX_COMPILER_ID "${CMAKE_CXX_COMPILER_ID}_${CMAKE_CXX_COMPILER_VERSION}")
set(TESTING_COMPILER "${TESTING_CXX_COMPILER_ID}")

if (DEFINED CDASH_OVERRIDE_BUILDNAME)
	set(BUILDNAME_BASE "${CDASH_OVERRIDE_BUILDNAME}")
else()
	set(BUILDNAME_BASE "${GITBRANCH};${TESTING_SYSTEM_NAME}")
	if (DEFINED ENV{CONDA_PREFIX})
		set(BUILDNAME_BASE "${BUILDNAME_BASE}_conda")
	endif()
	set(BUILDNAME_BASE "${BUILDNAME_BASE};${TESTING_COMPILER}")
	set(BUILDNAME_BASE "${BUILDNAME_BASE};${CMAKE_BUILD_TYPE}")
endif()
if(DEFINED CDASH_OVERRIDE_BUILDNAME_PREPEND)
	set(BUILDNAME_BASE "${CDASH_OVERRIDE_BUILDNAME_PREPEND};${BUILDNAME_BASE}")
endif()
if(DEFINED CDASH_OVERRIDE_BUILDNAME_APPEND)
	set(BUILDNAME_BASE "${BUILDNAME_BASE};${CDASH_OVERRIDE_BUILDNAME_APPEND}")
endif()

set(BUILDNAME "${BUILDNAME_BASE}" CACHE STRING "Build name variable for CTest (set CDASH_OVERRIDE_BUILDNAME to override)" FORCE)

message(STATUS "Git Branch (set CDASH_OVERRIDE_GIT_BRANCH to force): ${GITBRANCH}")
message(STATUS "System ID: ${TESTING_SYSTEM_NAME}")
message(STATUS "Compilers: ${TESTING_COMPILER}")
message(STATUS "Build: ${BUILDNAME}")
message(STATUS "Site: ${SITE}")

configure_file("${CMAKE_CURRENT_SOURCE_DIR}/cmake/CTestCustom.ctest.in" ${CMAKE_BINARY_DIR}/CTestCustom.ctest)
