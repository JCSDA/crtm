#!/bin/sh

LINKFILE_LIST=$*
if [ -z "${LINKFILE_LIST}" ]; then
  exit 0
fi

for FILE in ${LINKFILE_LIST}; do
  if [ -h ${FILE} ]; then
    rm -f ${FILE}
  fi
done

