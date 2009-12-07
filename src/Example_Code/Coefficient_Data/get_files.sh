#!/bin/sh

ENDIAN="Big_Endian"
CMD="cp"
echo "Getting files from ${CRTM_FIXFILE_ROOT}..."
${CMD} ${CRTM_FIXFILE_ROOT}/AerosolCoeff/${ENDIAN}/AerosolCoeff.bin                     AerosolCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/CloudCoeff/${ENDIAN}/CloudCoeff.bin                         CloudCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/EmisCoeff/Wu_Smith/${ENDIAN}/EmisCoeff.bin                  EmisCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Microwave/No_AC/${ENDIAN}/amsre_aqua.SpcCoeff.bin   amsre_aqua.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Microwave/${ENDIAN}/amsre_aqua.TauCoeff.bin    amsre_aqua.TauCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Infrared/${ENDIAN}/hirs3_n17.SpcCoeff.bin          hirs3_n17.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Infrared/ORD/${ENDIAN}/hirs3_n17.TauCoeff.bin hirs3_n17.TauCoeff.bin

${CMD} ${CRTM_FIXFILE_ROOT}/SpcCoeff/Visible/${ENDIAN}/v.seviri_m09.SpcCoeff.bin     v.seviri_m09.SpcCoeff.bin
${CMD} ${CRTM_FIXFILE_ROOT}/TauCoeff/ODAS/Visible/${ENDIAN}/v.seviri_m09.TauCoeff.bin  v.seviri_m09.TauCoeff.bin
