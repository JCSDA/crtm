#! /usr/bin/env ruby

dims=[{:name=>'PREDICTOR_DIMNAME'   ,:var=>'n_Predictors',:desc=>'Number of AirMass bias correction coefficients'},
      {:name=>'CHANNEL_DIMNAME'     ,:var=>'n_Channels'  ,:desc=>'Number of spectral channels'},
      {:name=>'FOV_DIMNAME'         ,:var=>'n_FOVs'      ,:desc=>'Number of sensor FOVs in scan position statistics'},
      {:name=>'TIME_DIMNAME'        ,:var=>'n_Times'     ,:desc=>'Number of observation times in time series statistics'},
      {:name=>'VARIABLENAME_DIMNAME',:var=>'N_VARIABLES' ,:desc=>'Number of variables for which statistics are accumulated'},
      {:name=>'VNSL_DIMNAME'        ,:var=>'VNSL'        ,:desc=>'String length for variable name'}]

dims.each do |d|
  str=<<-EOF
    ! ...#{d[:desc]}
    NF90_Status = NF90_DEF_DIM( FileID,#{d[:name]},#{d[:var]},#{d[:var]}_DimID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//#{d[:name]}//' dimension in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
  EOF
  puts str
end
