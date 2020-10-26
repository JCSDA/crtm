filename="crtm_coefficients.tar.gz"

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
		echo "downloading $filename, please wait about 1-2 minutes."
    wget -q ftp://ftp.ucar.edu/pub/cpaess/bjohns/$filename #jedi set of CRTM binary files
		mkdir fix/
    tar -zxvf $filename --directory fix/
		echo "fix/ directory created from downloaded $filename."
fi
echo "Completed."
