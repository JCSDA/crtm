filename="fix_REL-2.4.0.tgz" #rel 2.4.0 files

if test -f "$filename"; then
    if [ -d "fix/" ]; then #fix directory exists
        echo "fix/ already exists, doing nothing."
    else
        #untar the file and move directory to fix
				tar -zxvf $filename
				mv fix_crtm-internal_develop fix
				echo "fix/ directory created from existing $filename file."
    fi 
else
    #download, untar, move
		echo "downloading $filename, please wait about 5 minutes (3.2 GB tar file)"
    wget -q ftp://ftp.ssec.wisc.edu/pub/s4/CRTM/$filename #jedi set of CRTM binary files
    tar -zxvf $filename
		mv fix_crtm-internal_develop fix
		echo "fix/ directory created from downloaded $filename."
fi
echo "Completed."
