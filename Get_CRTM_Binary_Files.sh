#foldername="fix_REL-2.4.0" #rel 2.4.0 files
foldername="fix_REL-2.4.1_latest"

filename="${foldername}.tgz"
if test -f "$filename"; then
    if [ -d "fix/" ]; then #fix directory exists
        echo "fix/ already exists, doing nothing."
    else
        #untar the file and move directory to fix
				tar -zxvf $filename
				mv $foldername fix
				echo "fix/ directory created from existing $filename file."
    fi 
else
    #download, untar, move
		echo "downloading $filename, please wait about 5 minutes (3.3 GB tar file)"
    wget -q ftp://ftp.ssec.wisc.edu/pub/s4/CRTM/$filename # CRTM binary files
    tar -zxvf $filename
		mv $foldername fix
		echo "fix/ directory created from downloaded $filename."
fi
echo "Completed."
