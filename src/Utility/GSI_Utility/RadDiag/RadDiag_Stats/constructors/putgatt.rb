#! /usr/bin/env ruby

gatts=[{:name=>'SENSOR_ID_GATTNAME'       ,:var=>'Sensor_Id'       },
       {:name=>'WMO_SATELLITE_ID_GATTNAME',:var=>'WMO_Satellite_Id'},
       {:name=>'WMO_SENSOR_ID_GATTNAME'   ,:var=>'WMO_Sensor_Id'   },
       {:name=>'TITLE_GATTNAME'           ,:var=>'title'           },
       {:name=>'HISTORY_GATTNAME'         ,:var=>'history'         },
       {:name=>'COMMENT_GATTNAME'         ,:var=>'comment'         }]
       
gatts.each do |g|
  str=<<-EOF
    ! ...The #{g[:var]}
    IF ( PRESENT(#{g[:var]}) ) THEN
      GAttName = #{g[:name]}
      NF90_Status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(GAttName),#{g[:var]} )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        CALL WriteGAtts_Cleanup(); RETURN
      END IF
    END IF
  EOF
  puts str
end
