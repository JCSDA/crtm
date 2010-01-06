#! /usr/bin/env ruby

dims=[{:name=>'PREDICTOR_DIMNAME'   ,:var=>'n_Predictors'},
      {:name=>'CHANNEL_DIMNAME'     ,:var=>'n_Channels'  },
      {:name=>'FOV_DIMNAME'         ,:var=>'n_FOVs'      },
      {:name=>'TIME_DIMNAME'        ,:var=>'n_Times'     },
      {:name=>'VARIABLENAME_DIMNAME',:var=>'n_Variables' },
      {:name=>'VNSL_DIMNAME'        ,:var=>'vnsl'        }]

dims.each do |d|
  str=<<-EOF
    ! ...#{d[:var]} dimension 
    NF90_Status = NF90_INQ_DIMID( FileId,#{d[:name]},DimId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring dimension ID for '//#{d[:name]}//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=rds%#{d[:var]} )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error reading dimension value for '//#{d[:name]}//' - '// &
            TRIM(NF90_STRERROR( NF90_Status ))
      CALL Inquire_Cleanup(); RETURN
    END IF
  EOF
  puts str
end
