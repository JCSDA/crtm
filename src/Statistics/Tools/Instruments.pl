#!/usr/bin/perl

$front=10;
$back=-14;
$LBLdata="/u/wx20gd/src/CRTM/AtmOptics/Test/Performance_Results/48netCDFdata";
$fixdata="/


opendir (DIR, $LBLdata);

@TauProfile_files = grep(/.nc$/,readdir(DIR));

closedir(DIR);

foreach $file (@TauProfile_files){


$file_prefix = substr $file, $front, $back;

system("Generate_CRTM_Stats $file_prefix");



sleep 10;

}



