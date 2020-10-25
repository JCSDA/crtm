if test -f "fix_crtm-internal_develop_2.tgz"; then
		if [ -d "fix/" ]; then #fix directory exists
				echo "fix/ exists, doing nothing."
		else
				#untar the file and move directory to fix
				tar -zxvf fix_crtm-internal_develop_2.tgz
				mv fix_crtm-internal_develop fix
		fi 
else
    #download, untar, move
		wget -q ftp://ftp.ucar.edu/pub/cpaess/bjohns/fix_crtm-internal_develop_2.tgz #full set of CRTM binary files
		tar -zxvf fix_crtm-internal_develop_2.tgz
		mv fix_crtm-internal_develop fix
fi
