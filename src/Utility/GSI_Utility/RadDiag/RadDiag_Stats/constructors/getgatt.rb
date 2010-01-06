#! /usr/bin/env ruby

gatts=[{:name=>'VERSION_GATTNAME'         ,:var=>'Version'         ,:type=>"Integer"},
       {:name=>'SENSOR_ID_GATTNAME'       ,:var=>'Sensor_Id'       ,:type=>"String" },
       {:name=>'WMO_SATELLITE_ID_GATTNAME',:var=>'WMO_Satellite_Id',:type=>"Integer"},
       {:name=>'WMO_SENSOR_ID_GATTNAME'   ,:var=>'WMO_Sensor_Id'   ,:type=>"Integer"},
       {:name=>'TITLE_GATTNAME'           ,:var=>'title'           ,:type=>"String" },
       {:name=>'HISTORY_GATTNAME'         ,:var=>'history'         ,:type=>"String" },
       {:name=>'COMMENT_GATTNAME'         ,:var=>'comment'         ,:type=>"String" }]
       
gatts.each do |g|
  init  = ""
  clean = ""
  if g[:type] == "String"
    init = "; #{g[:var]} = ''"
    clean =<<-EOF
        
      CALL StrClean( GAttString )
      #{g[:var]} = GAttString(1:MIN(LEN(#{g[:var]}), LEN_TRIM(GAttString)))
    EOF
  end
  
  str=<<-EOF
    ! ...The #{g[:var]}
    IF ( PRESENT(#{g[:var]}) ) THEN
      GAttName = #{g[:name]}#{init}
      NF90_Status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),#{g[:var]} )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL ReadGAtts_Cleanup(); RETURN
      END IF#{clean.chomp}
    END IF
  EOF
  puts str
end
