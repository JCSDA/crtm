#! /usr/bin/env ruby

vars=[{:name=>'CHANNEL_VARNAME'      ,:var=>'Channel'            },
      {:name=>'AIRMASSCOEFF_VARNAME' ,:var=>'AirMassCoefficients'},
      {:name=>'FOV_VARNAME'          ,:var=>'FOV'                },
      {:name=>'SCAN_DATA_VARNAME'    ,:var=>'scan_Data'          },
      {:name=>'SCAN_NSAMPLES_VARNAME',:var=>'scan_nSamples'      },
      {:name=>'DATETIME_VARNAME'     ,:var=>'DateTime'           },
      {:name=>'TIME_DATA_VARNAME'    ,:var=>'time_Data'          },
      {:name=>'TIME_NSAMPLES_VARNAME',:var=>'time_nSamples'      },
      {:name=>'VARIABLENAME_VARNAME' ,:var=>'VariableNames'      }]

vars.each do |v|
  str=<<-EOF
    ! ...#{v[:var]} variable 
    NF90_Status = NF90_INQ_VARID( FileId,#{v[:name]},VarId )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error inquiring '//TRIM(Filename)//' for '//#{v[:name]}//&
            ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
    NF90_Status = NF90_PUT_VAR( FileId,VarID,RadDiag_Stats%#{v[:var]} )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error writing '//#{v[:name]}//' to '//TRIM(Filename)//&
            ' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Write_Cleanup(); RETURN
    END IF
  EOF
  puts str
end
