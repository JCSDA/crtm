#! /usr/bin/env ruby

DIMS=[{:name=>'FOV'    ,:var=>'n_FOVs'    ,:desc=>'Number of fields of view for the sensor'},
      {:name=>'CHANNEL',:var=>'n_Channels',:desc=>'Total number of channels for the sensor'}]

VARS=[{:name=>'SENSOR_CHANNEL',:var=>'Sensor_Channel',:dims=>'n_channels_dimid'             },
      {:name=>'A_EARTH'       ,:var=>'A_earth'       ,:dims=>'n_fovs_dimid,n_channels_dimid'},
      {:name=>'A_SPACE'       ,:var=>'A_space'       ,:dims=>'n_fovs_dimid,n_channels_dimid'},
      {:name=>'A_PLATFORM'    ,:var=>'A_platform'    ,:dims=>'n_fovs_dimid,n_channels_dimid'}]

GATTS=[{:name=>'RELEASE'         ,:var=>'Release'         ,:type=>"Integer"},
       {:name=>'VERSION'         ,:var=>'Version'         ,:type=>"Integer"},
       {:name=>'SENSOR_ID'       ,:var=>'Sensor_Id'       ,:type=>"String" },
       {:name=>'WMO_SATELLITE_ID',:var=>'WMO_Satellite_Id',:type=>"Integer"},
       {:name=>'WMO_SENSOR_ID'   ,:var=>'WMO_Sensor_Id'   ,:type=>"Integer"},
       {:name=>'TITLE'           ,:var=>'title'           ,:type=>"String" },
       {:name=>'HISTORY'         ,:var=>'history'         ,:type=>"String" },
       {:name=>'COMMENT'         ,:var=>'comment'         ,:type=>"String" }]

STRUCTNAME = "ACCoeff"



def defdims(dims)
  alldims = ''
  dims.each do |d|
    thisdim=<<-EOF
      ! ...#{d[:desc]}
      nf90_status = NF90_DEF_DIM( FileID,#{d[:name]}_DIMNAME,#{d[:var]},#{d[:var]}_dimid )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error defining '//#{d[:name]}_DIMNAME//' dimension in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Create_Cleanup(); RETURN
      END IF
    EOF
    alldims << thisdim
  end
  alldims
end


def getdims(dims, structname)
  alldims = ''
  dims.each do |d|
    thisdim=<<-EOF
      ! ...#{d[:var]} dimension
      NF90_Status = NF90_INQ_DIMID( FileId,#{d[:name]}_DIMNAME,DimId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error inquiring dimension ID for '//#{d[:name]}_DIMNAME//' - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_INQUIRE_DIMENSION( FileId,DimId,Len=#{structname.downcase}%#{d[:var]} )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error reading dimension value for '//#{d[:name]}_DIMNAME//' - '// &
              TRIM(NF90_STRERROR( NF90_Status ))
        CALL Inquire_Cleanup(); RETURN
      END IF
    EOF
    alldims << thisdim
  end
  alldims
end


def defvars(vars)
  allvars = ''
  vars.each do |v|
    thisvar=<<-EOF
      ! ...#{v[:var]} variable
      nf90_status = NF90_DEF_VAR( FileID, &
                                  #{v[:name]}_VARNAME, &
                                  #{v[:name]}_TYPE, &
                                  dimIDs=(/#{v[:dims]}/), &
                                  varID=variD )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error defining '//#{v[:name]}_VARNAME//' variable in '//&
              TRIM(Filename)//' - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Create_Cleanup(); RETURN
      END IF
      put_status(1) = NF90_PUT_ATT( FileID,varid,LONGNAME_ATTNAME   ,#{v[:name]}_LONGNAME    )
      put_status(2) = NF90_PUT_ATT( FileID,varid,DESCRIPTION_ATTNAME,#{v[:name]}_DESCRIPTION )
      put_status(3) = NF90_PUT_ATT( FileID,varid,UNITS_ATTNAME      ,#{v[:name]}_UNITS       )
      put_status(4) = NF90_PUT_ATT( FileID,varid,FILLVALUE_ATTNAME  ,#{v[:name]}_FILLVALUE   )
      IF ( ANY(put_status /= NF90_NOERR) ) THEN
        msg = 'Error writing '//#{v[:name]}_VARNAME//' variable attributes to '//TRIM(Filename)
        CALL Create_Cleanup(); RETURN
      END IF
    EOF
    allvars << thisvar
  end
  allvars
end


def getvars(vars, structname)
  allvars = ''
  vars.each do |v|
    thisvar=<<-EOF
      ! ...#{v[:var]} variable
      nf90_status = NF90_INQ_VARID( fileid,#{v[:name]}_VARNAME,varid )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//#{v[:name]}_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Read_Cleanup(); RETURN
      END IF
      nf90_status = NF90_GET_VAR( fileid,varid,#{structname}%#{v[:var]} )
      IF ( nf90_status /= NF90_NOERR ) THEN
        msg = 'Error reading '//#{v[:name]}_VARNAME//' from '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( nf90_status ))
        CALL Read_Cleanup(); RETURN
      END IF
    EOF
    allvars << thisvar
  end
  allvars
end


def putvars(vars, structname)
  allvars = ''
  vars.each do |v|
    thisvar=<<-EOF
      ! ...#{v[:var]} variable
      NF90_Status = NF90_INQ_VARID( FileId,#{v[:name]}_VARNAME,VarId )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error inquiring '//TRIM(Filename)//' for '//#{v[:name]}_VARNAME//&
              ' variable ID - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
      NF90_Status = NF90_PUT_VAR( FileId,VarID,#{structname}%#{v[:var]} )
      IF ( NF90_Status /= NF90_NOERR ) THEN
        msg = 'Error writing '//#{v[:name]}_VARNAME//' to '//TRIM(Filename)//&
              ' - '//TRIM(NF90_STRERROR( NF90_Status ))
        CALL Write_Cleanup(); RETURN
      END IF
    EOF
    allvars << thisvar
  end
  allvars
end


def getgatts(gatts)
  allgatts = ''
  gatts.each do |g|
    init  = ""
    clean = ""
    if g[:type] == "String"
      init = "; #{g[:var]} = ''"
      clean =<<-EOF
         
        CALL StrClean( gattstring )
        #{g[:var]} = gattstring(1:MIN(LEN(#{g[:var]}), LEN_TRIM(gattstring)))
      EOF
    end
   
    thisgatt=<<-EOF
      ! ...The #{g[:var]}
      IF ( PRESENT(#{g[:var]}) ) THEN
        gattname = #{g[:name]}_GATTNAME#{init}
        nf90_status = NF90_GET_ATT( FileID,NF90_GLOBAL,TRIM(gattname),#{g[:var]} )
        IF ( nf90_status /= NF90_NOERR ) THEN
          CALL ReadGAtts_Cleanup(); RETURN
        END IF#{clean.chomp}
      END IF
    EOF
    allgatts << thisgatt
  end
  allgatts
end


def putgatts(gatts)
  allgatts = ''
  gatts.each do |g|
    thisgatt=<<-EOF
      ! ...The #{g[:var]}
      IF ( PRESENT(#{g[:var]}) ) THEN
        gattname = #{g[:name]}_GATTNAME
        nf90_status = NF90_PUT_ATT( FileID,NF90_GLOBAL,TRIM(gattname),#{g[:var]} )
        IF ( nf90_status /= NF90_NOERR ) THEN
          CALL WriteGAtts_Cleanup(); RETURN
        END IF
      END IF
    EOF
    allgatts << thisgatt
  end
  allgatts
end


puts "this is the main bit"

File.open('defdims.txt' ,'w') { |f| f << defdims(DIMS) }
File.open('getdims.txt' ,'w') { |f| f << getdims(DIMS, STRUCTNAME) }
File.open('defvars.txt' ,'w') { |f| f << defvars(VARS) }
File.open('getvars.txt' ,'w') { |f| f << getvars(VARS, STRUCTNAME) }
File.open('putvars.txt' ,'w') { |f| f << putvars(VARS, STRUCTNAME) }
File.open('getgatts.txt','w') { |f| f << getgatts(GATTS) }
File.open('putgatts.txt','w') { |f| f << putgatts(GATTS) }

