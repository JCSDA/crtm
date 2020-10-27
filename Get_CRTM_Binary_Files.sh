filename="fix_crtm-internal_develop_2.tgz" #rel 2.3_jedi files

if test -f "$filename"; then
    if [ -d "fix/" ]; then #fix directory exists
        echo "fix/ already exists, doing nothing."
    else
        #untar the file and move directory to fix
				mkdir fix/
        tar -zxvf $filename --directory fix/
				echo "fix/ directory created from existing $filename file."
    fi 
else
    #download, untar, move
		echo "downloading $filename, please wait about 5 minutes, depending on download speed. (3.2 GB file)"
    wget -q ftp://ftp.ucar.edu/pub/cpaess/bjohns/$filename #jedi set of CRTM binary files
		mkdir fix/
    tar -zxvf $filename --directory fix/
		echo "fix/ directory created from downloaded $filename."
fi
echo "Completed."
