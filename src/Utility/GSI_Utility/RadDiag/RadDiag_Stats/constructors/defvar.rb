#! /usr/bin/env ruby

vars=[{:name=>'CHANNEL'      ,:var=>'Channel'            ,:dims=>'n_Channels_DimID'                                   },
      {:name=>'AIRMASSCOEFF' ,:var=>'AirMassCoefficients',:dims=>'n_Predictors_DimID, n_Channels_DimID, n_Times_DimID'},
      {:name=>'FOV'          ,:var=>'FOV'                ,:dims=>'n_FOVs_DimID'                                       },
      {:name=>'SCAN_DATA'    ,:var=>'scan_Data'          ,:dims=>'n_Variables_DimID,n_Channels_DimID,n_FOVs_DimID'    },
      {:name=>'SCAN_NSAMPLES',:var=>'scan_nSamples'      ,:dims=>'n_Channels_DimID,n_FOVs_DimID'                      },
      {:name=>'DATETIME'     ,:var=>'DateTime'           ,:dims=>'n_Times_DimID'                                      },
      {:name=>'TIME_DATA'    ,:var=>'time_Data'          ,:dims=>'n_Variables_DimID,n_Channels_DimID,n_Times_DimID'   },
      {:name=>'TIME_NSAMPLES',:var=>'time_nSamples'      ,:dims=>'n_Channels_DimID,n_Times_DimID'                     },
      {:name=>'VARIABLENAME' ,:var=>'VariableNames'      ,:dims=>'vsnl_DimID, n_Variables_DimID'                      }]

vars.each do |v|
  str=<<-EOF
    ! ...#{v[:var]} variable
    NF90_Status = NF90_DEF_VAR( FileID, &
                                #{v[:name]}_VARNAME, &
                                #{v[:name]}_TYPE, &
                                dimIDs=(/#{v[:dims]}/), &
                                varID=VarID )
    IF ( NF90_Status /= NF90_NOERR ) THEN
      msg = 'Error defining '//#{v[:name]}_VARNAME//' variable in '//&
            TRIM(Filename)//' - '//TRIM(NF90_STRERROR( NF90_Status ))
      CALL Create_Cleanup(); RETURN
    END IF
    Put_Status(1) = NF90_PUT_ATT( FileID,VarID,LONGNAME_ATTNAME   ,#{v[:name]}_LONGNAME    )
    Put_Status(2) = NF90_PUT_ATT( FileID,VarID,DESCRIPTION_ATTNAME,#{v[:name]}_DESCRIPTION )
    Put_Status(3) = NF90_PUT_ATT( FileID,VarID,UNITS_ATTNAME      ,#{v[:name]}_UNITS       )
    Put_Status(4) = NF90_PUT_ATT( FileID,VarID,FILLVALUE_ATTNAME  ,#{v[:name]}_FILLVALUE   )
    IF ( ANY(Put_Status /= NF90_NOERR) ) THEN
      msg = 'Error writing '//#{v[:name]}_VARNAME//' variable attributes to '//TRIM(Filename)
      CALL Create_Cleanup(); RETURN
    END IF
  EOF
  puts str
end
