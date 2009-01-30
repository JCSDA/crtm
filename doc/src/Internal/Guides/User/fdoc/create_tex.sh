#!/bin/sh

create_fdoc()
{
linkfiles ${CRTM_SOURCE_ROOT} $*
create_fdoc.rb ${FILES}
rm *.html
unlinkfiles $*
}

# Root
FILES="CRTM_LifeCycle.f90 CRTM_Adjoint_Module.f90 CRTM_Forward_Module.f90 CRTM_K_Matrix_Module.f90 CRTM_Tangent_Linear_Module.f90"
create_fdoc ${FILES}

# ChannelInfo
cd ChannelInfo
FILES="CRTM_ChannelInfo_Define.f90"
create_fdoc ${FILES}
cd ..

# Atmosphere
cd Atmosphere
FILES="CRTM_Atmosphere_Define.f90 CRTM_Atmosphere_Binary_IO.f90 CRTM_Cloud_Define.f90 CRTM_Cloud_Binary_IO.f90 CRTM_Aerosol_Define.f90 CRTM_Aerosol_Binary_IO.f90"
create_fdoc ${FILES}
cd ..

# Surface
cd Surface
FILES="CRTM_Surface_Define.f90 CRTM_Surface_Binary_IO.f90 CRTM_SensorData_Define.f90 "
create_fdoc ${FILES}
cd ..

# GeometryInfo
cd GeometryInfo
FILES="CRTM_GeometryInfo_Define.f90"
create_fdoc ${FILES}
cd ..

# RTSolution
cd RTSolution
FILES="CRTM_RTSolution_Define.f90 CRTM_RTSolution_Binary_IO.f90"
create_fdoc ${FILES}
cd ..

# Options
cd Options
FILES="CRTM_Options_Define.f90"
create_fdoc ${FILES}
cd ..

