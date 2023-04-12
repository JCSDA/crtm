# after a new clone of the repository, the binary LFS-stored files are in gzip format (*.gz)
# this script traverses the fix/ directory and uncompresses all of the binary and netCDF files.
# If you're making change to binary files, please add them to LFS in gzipped format.

find ./fix/ -name "*.gz" -type f -exec gunzip -fv {} \;
