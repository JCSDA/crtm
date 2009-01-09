#!/bin/sh

ln -sf ${CRTM_FIXFILE_ROOT}/AerosolCoeff/Big_Endian/AerosolCoeff.bin                   AerosolCoeff.bin                            
ln -sf ${CRTM_FIXFILE_ROOT}/CloudCoeff/Big_Endian/CloudCoeff.bin                       CloudCoeff.bin                              
ln -sf ${CRTM_FIXFILE_ROOT}/EmisCoeff/Big_Endian/EmisCoeff.bin                         EmisCoeff.bin                               
ln -sf ${CRTM_FIXFILE_ROOT}/SpcCoeff/Microwave/No_AC/Big_Endian/amsua_n18.SpcCoeff.bin amsua_n18.SpcCoeff.bin                      
ln -sf ${CRTM_FIXFILE_ROOT}/TauCoeff/Microwave/Liebe/Big_Endian/amsua_n18.TauCoeff.bin amsua_n18.TauCoeff.bin                      
ln -sf ${CRTM_FIXFILE_ROOT}/SpcCoeff/Infrared/Big_Endian/hirs4_n18.SpcCoeff.bin        hirs4_n18.SpcCoeff.bin                      
ln -sf ${CRTM_FIXFILE_ROOT}/TauCoeff/Infrared/ORD/Big_Endian/hirs4_n18.TauCoeff.bin    hirs4_n18.TauCoeff.bin                      
ln -sf ${CRTM_FIXFILE_ROOT}/SpcCoeff/Microwave/No_AC/Big_Endian/mhs_n18.SpcCoeff.bin   mhs_n18.SpcCoeff.bin                        
ln -sf ${CRTM_FIXFILE_ROOT}/TauCoeff/Microwave/Liebe/Big_Endian/mhs_n18.TauCoeff.bin   mhs_n18.TauCoeff.bin                        
