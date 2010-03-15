#!/bin/sh

create_fdoc()
{
linkfiles.sh ${CRTM_SOURCE_ROOT} $*
create_fdoc.rb ${FILES}
rm *.html
unlinkfiles $*
}

# Root
FILES="CRTM_Module.f90 CRTM_LifeCycle.f90 CRTM_Adjoint_Module.f90 CRTM_Forward_Module.f90 CRTM_K_Matrix_Module.f90 CRTM_Tangent_Linear_Module.f90"
create_fdoc ${FILES}

# ChannelInfo
cd ChannelInfo
FILES="CRTM_ChannelInfo_Define.f90"
create_fdoc ${FILES}
cd ..

# Atmosphere
cd Atmosphere
FILES="CRTM_Atmosphere_Define.f90 CRTM_Atmosphere_IO.f90 CRTM_Cloud_Define.f90 CRTM_Cloud_IO.f90 CRTM_Aerosol_Define.f90 CRTM_Aerosol_IO.f90"
create_fdoc ${FILES}
cd ..

# Surface
cd Surface
FILES="CRTM_Surface_Define.f90 CRTM_Surface_IO.f90 CRTM_SensorData_Define.f90 CRTM_SensorData_IO.f90"
create_fdoc ${FILES}
cd ..

# Geometry
cd Geometry
FILES="CRTM_Geometry_Define.f90 CRTM_Geometry_IO.f90"
create_fdoc ${FILES}
cd ..

# RTSolution
cd RTSolution
FILES="CRTM_RTSolution_Define.f90 CRTM_RTSolution_IO.f90"
create_fdoc ${FILES}
cd ..

# Options
cd Options
FILES="CRTM_Options_Define.f90 SSU_Input_Define.f90 Zeeman_Input_Define.f90"
create_fdoc ${FILES}
cd ..

