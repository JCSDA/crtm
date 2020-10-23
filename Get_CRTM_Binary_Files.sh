

if [ -z $1 ]
then
		tarball="crtm_full"
elif [ -n $1 ]
then
		tarball="crtm_jedi"
fi

if [ -d "fix/" ]; then
		echo "fix/ directory exists"
else
		if test -f "crtm_coefficients.tar.gz"; then
				echo "crtm_coefficients.tar.gz exists."
		else
				echo 
				case $tarball in
						"crtm_jedi") wget https://jedi-test-files.s3.amazonaws.com/crtm/2.3.0/crtm_coefficients.tar.gz ;; 
						"crtm_full") wget ftp://ftp.ucar.edu/pub/cpaess/bjohns/crtm_coefficients.tar.gz ;;
						*) echo "Sorry, I can not get fixfiles for you!";;
				esac
		fi
		# fix directory does not exist but the tar file does:
		mkdir fix
		tar -zxvf crtm_coefficients.tar.gz fix/.
fi
