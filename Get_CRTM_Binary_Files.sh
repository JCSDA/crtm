if test -f "crtm_coefficients.tar.gz"; then
    if [ -d "fix/" ]; then #fix directory exists
        echo "fix/ exists, doing nothing."
    else
        #untar the file and move directory to fix
				mkdir fix/
        tar -zxvf crtm_coefficients.tar.gz --directory fix/
    fi 
else
    #download, untar, move
    wget -q ftp://ftp.ucar.edu/pub/cpaess/bjohns/crtm_coefficients.tar.gz #jedi set of CRTM binary files
		mkdir fix/
    tar -zxvf crtm_coefficients.tar.gz --directory fix/
fi
