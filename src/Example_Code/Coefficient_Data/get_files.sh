#!/bin/sh

ENDIAN="Big_Endian"
CMD="cp"
echo "Getting files from ${CRTM_FIXFILE_ROOT}..."
${CMD} ${CRTM_FIXFILE_ROOT}/AerosolCoeff/${ENDIAN}/AerosolCoeff.bin                     AerosolCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/CloudCoeff/${ENDIAN}/CloudCoeff.bin                         CloudCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/EmisCoeff/Wu_Smith/${ENDIAN}/EmisCoeff.bin                  EmisCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Microwave/No_AC/${ENDIAN}/amsua_n18.SpcCoeff.bin   amsua_n18.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Microwave/${ENDIAN}/amsua_n18.TauCoeff.bin    amsua_n18.TauCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Infrared/${ENDIAN}/hirs4_n18.SpcCoeff.bin          hirs4_n18.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Infrared/ORD/${ENDIAN}/hirs4_n18.TauCoeff.bin hirs4_n18.TauCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Microwave/No_AC/${ENDIAN}/mhs_n18.SpcCoeff.bin     mhs_n18.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Microwave/${ENDIAN}/mhs_n18.TauCoeff.bin      mhs_n18.TauCoeff.bin
